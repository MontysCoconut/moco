/*
 * moco, the Monty Compiler
 * Copyright (c) 2013-2014, Monty's Coconut, All rights reserved.
 *
 * This file is part of moco, the Monty Compiler.
 *
 * moco is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public
 * License as published by the Free Software Foundation; either
 * version 3.0 of the License, or (at your option) any later version.
 *
 * moco is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 *
 * Linking this program and/or its accompanying libraries statically or
 * dynamically with other modules is making a combined work based on this
 * program. Thus, the terms and conditions of the GNU General Public License
 * cover the whole combination.
 *
 * As a special exception, the copyright holders of moco give
 * you permission to link this programm and/or its accompanying libraries
 * with independent modules to produce an executable, regardless of the
 * license terms of these independent modules, and to copy and distribute the
 * resulting executable under terms of your choice, provided that you also meet,
 * for each linked independent module, the terms and conditions of the
 * license of that module.
 *
 * An independent module is a module which is not
 * derived from or based on this program and/or its accompanying libraries.
 * If you modify this library, you may extend this exception to your version of
 * the program or library, but you are not obliged to do so. If you do not wish
 * to do so, delete this exception statement from your version.
 *
 * You should have received a copy of the GNU General Public
 * License along with this library.
 */
package de.uni.bremen.monty.moco.visitor;

import de.uni.bremen.monty.moco.ast.*;
import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.ast.expression.*;
import de.uni.bremen.monty.moco.ast.expression.literal.*;
import de.uni.bremen.monty.moco.ast.statement.ReturnStatement;
import de.uni.bremen.monty.moco.ast.statement.Statement;
import de.uni.bremen.monty.moco.ast.statement.UnpackAssignment;
import de.uni.bremen.monty.moco.ast.statement.YieldStatement;
import de.uni.bremen.monty.moco.ast.types.*;
import de.uni.bremen.monty.moco.exception.*;
import de.uni.bremen.monty.moco.util.OverloadCandidate;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;
import java.util.Arrays;

/** This visitor must traverse the entire AST and resolve variables and types. */
public class ResolveVisitor extends VisitOnceVisitor {

	@Override
	protected int bit() {
		return 1;
	}

	/** Constructor. */
	public ResolveVisitor() {
		super();
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ClassDeclaration node) {
		ClassScope scope = node.getScope();
		List<PartialAppliedTypeInfo> superClasses = node.getSuperClassDeclarations();
		for (ResolvableIdentifier identifier : node.getSuperClassIdentifiers()) {
			PartialAppliedTypeInfo type = (PartialAppliedTypeInfo) scope.resolveType(identifier, this);
			superClasses.add(type);
			scope.addParentClassScope(type.getScope());
		}
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(TypeParameterDeclaration node) {
		ResolvableIdentifier upperTypeBound = node.getUpperTypeBoundIdentifier();
		Type type = node.getScope().resolveType(upperTypeBound, this);
		node.setUpperTypeBound(type);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(VariableDeclaration node) {
		super.visit(node);
		Scope scope = node.getScope();
		if(node.getType()== null) {
			Type type;
			if (node.typeMustBeInferred()) {
				Expression inferred = node.getExpressionToInferType();
				visitDoubleDispatched(inferred);
				type = inferred.getType();
			} else {
				type = scope.resolveType(node.getTypeIdentifier(), this);
			}
			node.setType(type);
		}

		// variable declarations in a generator must be registered
		ASTNode generatorFun = node.getParentNodeByType(FunctionDeclaration.class);
		if (generatorFun instanceof GeneratorFunctionDeclaration) {
			((GeneratorFunctionDeclaration) generatorFun).addVariableDeclaration(node);
		}
	}

	/** {@inheritDoc} */
	@Override
	public void visit(VariableAccess node) {
		super.visit(node);
		Declaration declaration = null;

		Scope scope = node.getScope();
		// if the variable access is casted into something else, right away...
		if (node.getParentNode() instanceof CastExpression) {
			visitDoubleDispatched(node.getParentNode());
			declaration = overloadResolutionForVariableAccess(node);
		}
		if (declaration == null) {
			declaration = scope.resolve(node.getIdentifier());
		}

		if (declaration instanceof VariableDeclaration) {
			VariableDeclaration variable = (VariableDeclaration) declaration;
			node.setDeclaration(variable);

			resolveClosureVariables(node, variable);

			visitDoubleDispatched(variable);
			node.setType(variable.getType().extend(scope.getContext()));
			if (!(scope instanceof ClassScope) && findEnclosingClass(node) == CoreClasses.voidType()) {
				if (node.getDeclaration() == null
				        || node.getDeclaration().getPosition().getLineNumber() > node.getPosition().getLineNumber()) {
					throw new UnknownIdentifierException(node.getIdentifier());
				}
			}
		}
	}

	protected void resolveClosureVariables(VariableAccess access, VariableDeclaration declaration) {
		// everything that is not an attribute (since attributes are always defined outside the current scope)
		if (declaration.getDeclarationType() != VariableDeclaration.DeclarationType.ATTRIBUTE) {
			Declaration declParent = (Declaration) declaration.getParentNodeByType(Declaration.class);
			Declaration accessParent = (Declaration) access.getParentNodeByType(Declaration.class);

			if (declParent != accessParent) {
				if (!(declParent instanceof ModuleDeclaration)) {
					FunctionDeclaration fn = ((FunctionDeclaration) accessParent);
					VariableDeclaration closureVarDecl = fn.addClosureVariable(declaration);
					access.setClosureVariable(true);
					access.setDeclaration(closureVarDecl);
				}
			}
		}
	}

	protected void resolveClosureVariablesForSelf(SelfExpression self) {
		FunctionDeclaration selfParent = (FunctionDeclaration) self.getParentNodeByType(FunctionDeclaration.class);
		// self expressions in unbound functions must be closure variables
		if (selfParent != null && selfParent.isUnbound()) {
			self.setInClosure();
			selfParent.addClosureVariable(new VariableDeclaration(self.getPosition(), new Identifier("self"),
			        self.getType(), VariableDeclaration.DeclarationType.VARIABLE));
		}
	}

	/** If a variable access is casted into a function, we can do overload resolution just like for function calls e.g.
	 * "Int -> Int myFunction := square as (Int -> Int)" will look for the implementation of square with the correct
	 * signature.
	 *
	 * @param node
	 * @return */
	protected Declaration overloadResolutionForVariableAccess(VariableAccess node) {
		CastExpression cast = (CastExpression) node.getParentNode();
		Type castedTo = cast.getType();

		if(castedTo.isFunction()){
			if(!(castedTo instanceof ConcreteType)){
				throw new RuntimeException();
			}
			ConcreteType functionType = (ConcreteType) castedTo;
			ConcreteType param = functionType.getConcreteGenericTypes().get(0);

			List<? extends Type> params;
			if(param.isTuple()){
				params = param.getConcreteGenericTypes();
			} else {
				params = Collections.singletonList(param);
			}

			List<FunctionType> availableFunctions = node.getScope().resolveFunction(node.getIdentifier(), this);

			FunctionType result = overloadResolution(params, availableFunctions);
			ConcreteVariableType variableType = (ConcreteVariableType) result.getWrapperFunctionObjectDeclaration();
			return variableType.getDeclaration();
		}
		return null;
	}

	/** {@inheritDoc} */
	@Override
	public void visit(SelfExpression node) {
		super.visit(node);
		node.setType(TypeFactory.from(findEnclosingClass(node), TypeContext.EMPTY));
		resolveClosureVariablesForSelf(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ParentExpression node) {
		super.visit(node);
		ResolvableIdentifier parentIdentifier = node.getParentIdentifier();
		node.setType(node.getScope().resolveType(parentIdentifier, this));
		node.setSelfType(TypeFactory.from(findEnclosingClass(node), TypeContext.EMPTY));
	}

	/** {@inheritDoc} */
	@Override
	public void visit(CastExpression node) {
		Type type;
		if (node.typeMustBeInferred()) {
			type = node.inferType(this);
		} else {
			type = node.getScope().resolveType(node.getCastIdentifier(), this);
		}
		node.setType(type);
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(IsExpression node) {
		super.visit(node);
		if (node.typeMustBeInferred()) {
			visitDoubleDispatched(node.getExpressionToInferFrom());
			node.inferType();
		} else {
			node.setToType(node.getScope().resolveType(node.getIsIdentifier(), this));
		}
		node.setType(Types.boolType());
	}

	/** {@inheritDoc} */
	@Override
	public void visit(MemberAccess node) {
		Expression leftExpr = node.getLeft();
		Expression rightExpr = node.getRight();
		visitDoubleDispatched(leftExpr);
		Type leftType = leftExpr.getType();

		rightExpr.setScope(leftType.getScope());
		visitDoubleDispatched(rightExpr);

		Type type = rightExpr.getType();
		node.setType(type);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ConditionalExpression node) {
		super.visit(node);
		node.setType(node.getThenExpression().getType());
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ZeroExpression node) {
		Scope scope = node.getScope();
		node.setType(scope.resolveType(node.getIdentifier(), this));
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(IntegerLiteral node) {
		node.setType(Types.intType());
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(FloatLiteral node) {
		node.setType(Types.floatType());
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(CharacterLiteral node) {
		node.setType(Types.charType());
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(StringLiteral node) {
		node.setType(Types.stringType());
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(BooleanLiteral node) {
		node.setType(Types.boolType());
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ArrayLiteral node) {
		super.visit(node);
		Type genericType;
		if(node.getEntries().isEmpty()){
			Type arrayType = Types.objectType(); //Should be the Bottom type
			genericType = arrayType;
		} else {
			Type firstType = node.getEntries().get(0).getType();
			for (Expression expression : node.getEntries()) {
				if (!expression.getType().matchesTypeExactly(firstType)) {
					throw new RuntimeException("All Types of an Array must be the same");
				}
			}
			genericType = firstType;
		}
		ResolvableIdentifier genericTypeIdentifier = ((ConcreteType)genericType).getResolvableIdentifier();
		ConcreteType arrayType = (ConcreteType) node.getScope().resolveType(new ResolvableIdentifier("Array", Arrays.asList(genericTypeIdentifier)), this);
		FunctionCall parentNode = (FunctionCall) node.getParentNode();
		parentNode.setType(arrayType);
		parentNode.getIdentifier().getGenericTypes().set(0,genericTypeIdentifier);
		node.setType(Types.arrayType());

	}

	/** {@inheritDoc} */
	@Override
	public void visit(UnpackAssignment node) {
		String typeSymbol = node.getTmpDecl().getType().getIdentifier().getSymbol();
		if (typeSymbol.startsWith("Tuple")) {
			int rhsLength = Integer.parseInt(typeSymbol.substring(5));
			int lhsLength = (node.getAssignments().size() - 1);
			if (lhsLength != rhsLength) {
				throw new TypeMismatchException(node, "can't unpack a " + rhsLength + "-tuple to " + lhsLength
				        + " variables");
			}
			super.visit(node);
		} else {
			throw new TypeMismatchException(node, "can't unpack something that is not a tuple");
		}
	}

	/** {@inheritDoc} */
	@Override
	public void visit(FunctionDeclaration node) {
		if (node.isReturnTypeToBeInferred()) {
			super.visit(node);
			Statement returnStatement = node.getBody().getStatements().get(node.getBody().getStatements().size() - 1);
			if (returnStatement instanceof ReturnStatement) {
				node.setInferredReturnType(((ReturnStatement) returnStatement).getParameter().getType(), this);
			} else {
				throw new MontyBaseException(node, "can not infer return type!");
			}
		} else {
			if (node.isFunction()) {
				Scope scope = node.getScope();
				Type returnType = scope.resolveType(node.getReturnTypeIdentifier(), this);
				node.setReturnType(returnType);
			} else {
				if (node.isMethod() && node.getIdentifier().getSymbol().equals("initializer")) {
					node.setDeclarationType(FunctionDeclaration.DeclarationType.INITIALIZER);
				}
			}
			super.visit(node);
		}
	}

	/** {@inheritDoc} */
	@Override
	public void visit(FunctionCall node) {
		super.visit(node);

		if (node instanceof TupleLiteral) {
			((TupleLiteral) node).setConcreteTupleType();
		}

		Scope scope = node.getScope();
		Optional<Type> maybeTypeDeclaration = scope.tryToResolveType(node.getIdentifier(), this);

		// constructor call
		if (maybeTypeDeclaration.isPresent()) {
			Type declaration = maybeTypeDeclaration.get();
			if(!(declaration instanceof PartialAppliedTypeInfo)){
				throw new InvalidExpressionException(node, "Type Variables can't be instantiated");
			}
			PartialAppliedTypeInfo classDecl = (PartialAppliedTypeInfo) declaration;
			ResolvableIdentifier identifier = node.getIdentifier();
			if (classDecl.isAbstract()) {
				throw new InvalidExpressionException(node, "The abstract class '" + identifier.toString()
				        + "' must not be instantiated");
			}
			if(node.getType() == null) {
				node.setType(classDecl);
			}
			FunctionType initializer = findMatchingInitializer(node, classDecl);
			initializer = (initializer != null) ? initializer : classDecl.getDefaultInitializer();
			node.setDeclaration(initializer);
		} else {
			List<Type> argTypes = new ArrayList<>(node.getArguments().size());
			for (Expression exp : node.getArguments()) {
				argTypes.add(exp.getType());
			}
			List<FunctionType> alternatives = scope.resolveFunction(node.getIdentifier(), this);
			FunctionType callableDeclaration = overloadResolution(argTypes, alternatives);

			// if the parameter is just one tuple, we might have to unpack it
			if (callableDeclaration == null) {
				if ((argTypes.size() == 1)
						&& (argTypes.get(0).getIdentifier().getSymbol().startsWith("Tuple"))) {
                    List<ConcreteType> argCls = ((ConcreteType)argTypes.get(0)).getConcreteGenericTypes();
                    argTypes = new ArrayList<>(argCls.size());
                    for (ConcreteType cls : argCls) {
                        argTypes.add(cls);
                    }
                    callableDeclaration = overloadResolution(argTypes, alternatives);
                } else {
					throw new TypeMismatchException(node, String.format("Could not find matching Function for Identifier \"%s\" with arguments: \"%s\"", node.getIdentifier(), argsAsString(argTypes)));
				}
			}

			if (callableDeclaration instanceof FunctionTypeDecl) {
				FunctionType functionType = callableDeclaration.extend(scope.getContext());
				node.setDeclaration(functionType);
				if (functionType.isFunction()) {
					node.setType(functionType.getReturnType());
				} else {
					node.setType(Types.voidType());
				}
			}
			else if (callableDeclaration instanceof FunctionTypeVariable) {
				callVariable(node, (FunctionTypeVariable) callableDeclaration);
			} else {
				List<Identifier> argsAsString = argsAsString(argTypes);
				List<List<String>> possibleParametersAsString = alternatives.stream().filter(FunctionDeclaration.class::isInstance)
						.map(FunctionDeclaration.class::cast).map((functionDeclaration) -> functionDeclaration.getParameters().stream().map(s -> s.getType().getIdentifier().toString()).collect(Collectors.toList())).collect(Collectors.toList());
				throw new TypeMismatchException(node, String.format("Arguments \"%s\" of function call \"%s\" do not match declaration.\nPossible parameters are: \"%s\"", argsAsString, node.getIdentifier(), possibleParametersAsString));
			}
		}
	}

	public List<Identifier> argsAsString(List<Type> argTypes) {
		return argTypes.stream().map(t -> {
			if(t instanceof ConcreteType && ((ConcreteType)t).isFunctionWrapper()){
				return (((ConcreteType)t).getWrappedFunction().getIdentifier());
			}
			return t.getIdentifier();
		}).collect(Collectors.toList());
	}

	/** this method does the wrapping logic for function objects
	 *
	 * @param node
	 * @param callableDeclaration */
	protected void callVariable(FunctionCall node, FunctionTypeVariable callableDeclaration) {
		VariableAccess varAccess =
		        new VariableAccess(node.getPosition(),
		                ResolvableIdentifier.convert(callableDeclaration.getIdentifier()));

		TupleLiteral tuple = null;
		// if there are more than one arguments, we have to pack them into a tuple...
		List<Expression> arguments = node.getArguments();
		if (arguments.size() != 1) {
			arguments = new ArrayList<>(1);
			tuple = new TupleLiteral(node.getPosition(), node.getArguments());
			arguments.add(tuple);
		}
		FunctionCall functionCall =
		        new FunctionCall(node.getPosition(), new ResolvableIdentifier("_apply_"), arguments);
		MemberAccess memberAccess = new MemberAccess(node.getPosition(), varAccess, functionCall);
		memberAccess.setScope(node.getScope());
		functionCall.setParentNode(memberAccess);
		varAccess.setParentNode(memberAccess);
		functionCall.setScope(node.getScope());
		varAccess.setScope(node.getScope());
		if (tuple != null) {
			tuple.setParentNode(functionCall);
			tuple.setScope(functionCall.getScope());
		}

		WrappedFunctionCall wrapper = (WrappedFunctionCall) node.getParentNode();
		wrapper.setMemberAccess(memberAccess);
		memberAccess.setParentNode(wrapper);
		wrapper.setFunctionCall(null);
		visit(wrapper);
	}

	/** Find an enclosing class of this node.
	 *
	 * If the search is not sucessfull this method returns CoreClasses.voidType(). */
	private ClassDeclaration findEnclosingClass(ASTNode node) {
		ASTNode parent = node.getParentNode();
		while (parent != null) {
			if (parent instanceof ClassDeclaration) {
				return (ClassDeclaration) parent;
			}
			parent = parent.getParentNode();
		}
		return CoreClasses.voidType();
	}

	/** Searches the given class declaration in order to find a initializer declaration that matches the signature of the
	 * given initializer node.
	 *
	 * @param node
	 *            a function call node representing a initializer
	 * @param classDeclaration
	 *            the class declaration searched for a matching initializer.
	 * @return the matching initializer if one is found for the given function call or null otherwise */
	private FunctionType findMatchingInitializer(FunctionCall node, PartialAppliedTypeInfo classDeclaration) {

		List<FunctionType> functions = classDeclaration.getInitializers(this);

		if (functions.isEmpty()) {
			return null;
		} else {
			List<Type> argTypes = new ArrayList<>(node.getArguments().size());
			for (Expression exp : node.getArguments()) {
				argTypes.add(exp.getType());
			}
			return overloadResolution(argTypes, functions);
		}
	}

//	/** this one performs the best-fit algorithm, described in the language specification
//	 *
//	 * @param arguments
//	 * @param variables
//	 * @return */
//	public Declaration overloadResolution(List<Type> arguments, List<VariableType> variables) {
//		List<OverloadCandidate> candidates = new ArrayList<>();
//		for (FunctionType declaration : variables) {
//			if (declaration instanceof FunctionDeclaration) {
//				handleOverloadedFunction(arguments, declaration, candidates);
//			} else if (declaration instanceof VariableDeclaration) {
//				handleOverloadedVariable(arguments, declaration, candidates);
//			} else {
//				throw new RuntimeException("Invalid declaration type for overload resolution: '"
//						+ declaration.getIdentifier() + "'!");
//			}
//		}
//		return overloadResolution_(candidates);
//	}

	/** this one performs the best-fit algorithm, described in the language specification
	 *
	 * @param arguments
	 * @param functions
	 * @return */
	public FunctionType overloadResolution(List<? extends Type> arguments, List<FunctionType> functions) {
		List<OverloadCandidate<FunctionType>> candidates = new ArrayList<>();
		for (FunctionType declaration : functions) {
			if (declaration instanceof FunctionTypeDecl) {
				handleOverloadedFunction(arguments, (FunctionTypeDecl) declaration, candidates);
			} else if (declaration instanceof FunctionTypeVariable) {
				handleOverloadedVariable(arguments, (FunctionTypeVariable) declaration, candidates);
			} else {
				throw new RuntimeException("Invalid declaration type for overload resolution: '"
				        + declaration.getIdentifier() + "'!");
			}
		}
		return overloadResolution_(candidates);
	}

	private <T extends MemberType> T overloadResolution_(List<OverloadCandidate<T>> candidates) {
		T result = null;
		// find out the minimum type distance
		int minScore = Integer.MAX_VALUE;
		for (OverloadCandidate<T> candidate : candidates) {
			if (candidate.getScore() < minScore) {
				minScore = candidate.getScore();
				result = candidate.getDeclaration();
			}
		}
		return result;
	}

	/** adds 'declaration' to 'candidates' if the signature of it matches 'arguments'
	 *
	 * @param arguments
	 * @param declaration
	 * @param candidates */
	protected void handleOverloadedFunction(List<? extends Type> arguments, FunctionTypeDecl declaration,
											List<OverloadCandidate<FunctionType>> candidates) {
		if (declaration.getParameter().size() == arguments.size()) {
			int score = 0;
			int argIndex = 0;
			for (VariableType param : declaration.getParameter()) {
				int typeDist = param.
						getType().
						getTypeDist(arguments.
								get(argIndex));
				if ((typeDist < Integer.MAX_VALUE) && (score < Integer.MAX_VALUE)) {
					score += typeDist;
					argIndex++;
				} else {
					score = Integer.MAX_VALUE;
					break;
				}
			}
			if (score != Integer.MAX_VALUE) {
				candidates.add(new OverloadCandidate<>(declaration, score));
			}
			// special case for functions without parameters
		} else if ((arguments.size() == 0) && (declaration.getParameter().size() == 1)
		        && (declaration.getParameter().get(0).getType() != null)
		        && (declaration.getParameter().get(0).getType().getIdentifier().getSymbol().equals("Tuple0"))) {
			candidates.add(new OverloadCandidate<>(declaration, 0));
		}
	}

	/** does the same as handleOverloadedFunction, but for function objects stored in variables
	 *
	 * @param arguments
	 * @param declaration
	 * @param candidates */
	protected void handleOverloadedVariable(List<? extends Type> arguments, FunctionTypeVariable declaration,
		List<OverloadCandidate<FunctionType>> candidates) {
		List<? extends Type> paramTypes = declaration.getParameterTypes();

		if (declaration.getParameterTypes().size() == arguments.size()) {
			int score = 0;
			int argIndex = 0;
			for (Type genericType : paramTypes) {
				int typeDist = genericType.getTypeDist(arguments.get(argIndex));
				if ((typeDist < Integer.MAX_VALUE) && (score < Integer.MAX_VALUE)) {
					score += typeDist;
					argIndex++;
				} else {
					score = Integer.MAX_VALUE;
					break;
				}
			}
			if (score != Integer.MAX_VALUE) {
				candidates.add(new OverloadCandidate<FunctionType>(declaration, score));
			}
		}
		// special case for functions without parameters
		else if ((arguments.size() == 0) && (paramTypes.size() == 1)
				&& (paramTypes.get(0).getIdentifier().getSymbol().equals("Tuple0"))) {
			candidates.add(new OverloadCandidate<FunctionType>(declaration, 0));
		}
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ReturnStatement node) {
		super.visit(node);

		if (node instanceof YieldStatement) {
			FunctionDeclaration parentNode = node.getParentNodeByType(FunctionDeclaration.class);
			if (parentNode instanceof GeneratorFunctionDeclaration) {
				((GeneratorFunctionDeclaration) parentNode).addYieldStatement((YieldStatement) node);
			}
		}
	}
}
