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
import de.uni.bremen.monty.moco.exception.*;
import de.uni.bremen.monty.moco.util.OverloadCandidate;
import de.uni.bremen.monty.moco.util.TupleDeclarationFactory;

import java.util.ArrayList;
import java.util.List;

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
		ClassScope scope = (ClassScope) node.getScope();
		List<TypeDeclaration> superClasses = node.getSuperClassDeclarations();
		for (ResolvableIdentifier identifier : node.getSuperClassIdentifiers()) {
			TypeDeclaration type = scope.resolveType(identifier);
			superClasses.add(type);
			if (type instanceof ClassDeclaration) {
				scope.addParentClassScope((ClassScope) type.getScope());
			}
		}
		super.visit(node);

		setVMT(node, superClasses);
	}

	private void setVMT(ClassDeclaration node, List<TypeDeclaration> superClasses) {
		int attributeIndex = 1;
		List<FunctionDeclaration> virtualMethodTable = node.getVirtualMethodTable();
		// This can only deal with single inheritance!
		if (!superClasses.isEmpty()) {
			TypeDeclaration type = superClasses.get(0);
			if (type instanceof ClassDeclaration) {
				ClassDeclaration clazz = (ClassDeclaration) type;
				attributeIndex = clazz.getLastAttributeIndex();
				virtualMethodTable.addAll(clazz.getVirtualMethodTable());
			}
		}

		// Make room for the ctable pointer
		int vmtIndex = virtualMethodTable.size() + 1;

		for (Declaration decl : node.getBlock().getDeclarations()) {
			if (decl instanceof VariableDeclaration) {
				VariableDeclaration varDecl = (VariableDeclaration) decl;
				varDecl.setAttributeIndex(attributeIndex++);
			} else if (decl instanceof FunctionDeclaration) {
				FunctionDeclaration procDecl = (FunctionDeclaration) decl;
				if (!procDecl.isInitializer()) {
					boolean foundEntry = false;
					for (int i = 0; !foundEntry && i < virtualMethodTable.size(); i++) {
						FunctionDeclaration vmtEntry = virtualMethodTable.get(i);
						if (procDecl.overridesFunction(vmtEntry)) {
							virtualMethodTable.set(i, procDecl);
							procDecl.setVMTIndex(vmtEntry.getVMTIndex());
							foundEntry = true;
						}
					}
					if (!foundEntry) {
						virtualMethodTable.add(procDecl);
						procDecl.setVMTIndex(vmtIndex++);
					}
				}
			}
		}
		node.setLastAttributeIndex(attributeIndex);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(VariableDeclaration node) {
		super.visit(node);
		Scope scope = node.getScope();
		TypeDeclaration type;
		if (node.typeMustBeInferred()) {
			Expression inferred = node.getExpressionToInferType();
			visitDoubleDispatched(inferred);
			type = inferred.getType();
		} else {
			type = scope.resolveType(node.getTypeIdentifier());
		}
		node.setType(type);

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
			node.setType(variable.getType());
			if (!(scope instanceof ClassScope) && findEnclosingClass(node) == CoreClasses.voidType()) {
				if (node.getDeclaration() == null
				        || node.getDeclaration().getPosition().getLineNumber() > node.getPosition().getLineNumber()) {
					throw new UnknownIdentifierException(node.getIdentifier());
				}
			}
		}
	}

	protected void resolveClosureVariables(VariableAccess access, VariableDeclaration declaration) {
		// everything that is not an attribute
		if (declaration.getDeclarationType() != VariableDeclaration.DeclarationType.ATTRIBUTE) {
			Declaration declParent = (Declaration) declaration.getParentNodeByType(Declaration.class);
			Declaration accessParent = (Declaration) access.getParentNodeByType(Declaration.class);

			if (declParent != accessParent) {
				if (!(declParent instanceof ModuleDeclaration)) {
					FunctionDeclaration fn = ((FunctionDeclaration) accessParent);
					// VariableDeclaration attr = fn.registerClosureVariable(declaration);
					// access.setDeclaration(attr);
					VariableDeclaration closureVarDecl = fn.addClosureVariable(declaration);
					access.setClosureVariable(true);
					access.setDeclaration(closureVarDecl);
				}
			}
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
		// if the cast type is inferred, we can not resolve this one
		if (cast.getCastIdentifier() == null) {
			return null;
		}

		TypeDeclaration castedTo = cast.getScope().resolveType(cast.getCastIdentifier());
		Declaration declaration = null;
		if (castedTo.matchesType(CoreClasses.functionType())) {

			ClassDeclaration paramType = ((ClassDeclarationVariation) castedTo).getConcreteGenericTypes().get(0);
			List<TypeDeclaration> params = new ArrayList<>();
			if (TupleDeclarationFactory.isTuple(paramType)) {
				params.addAll(((ClassDeclarationVariation) paramType).getConcreteGenericTypes());
			} else {
				params.add(paramType);
			}
			declaration = overloadResolution(params, node.getScope().resolveFunction(node.getIdentifier()));
			if (declaration instanceof FunctionDeclaration) {
				declaration = ((FunctionDeclaration) declaration).getWrapperFunctionObjectDeclaration();
			}
			if (declaration == null) {
				throw new UnknownIdentifierException(node, ResolvableIdentifier.convert(node.getIdentifier()));
			}
		}
		return declaration;
	}

	/** {@inheritDoc} */
	@Override
	public void visit(SelfExpression node) {
		super.visit(node);
		node.setType(findEnclosingClass(node));
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ParentExpression node) {
		super.visit(node);
		node.setType(node.getScope().resolveType(node.getParentIdentifier()));
		node.setSelfType(findEnclosingClass(node));
	}

	/** {@inheritDoc} */
	@Override
	public void visit(CastExpression node) {
		if (node.typeParameterMustBeInferred()) {
			node.inferTypeParameter();
		}
		if (node.typeMustBeInferred()) {
			visitDoubleDispatched(node.getExpressionToInferFrom());
			node.inferType();
		}
		super.visit(node);
		node.setType(node.getScope().resolveType(node.getCastIdentifier()));
	}

	/** {@inheritDoc} */
	@Override
	public void visit(IsExpression node) {
		super.visit(node);
		if (node.typeMustBeInferred()) {
			visitDoubleDispatched(node.getExpressionToInferFrom());
			node.inferType();
		} else {
			node.setToType(node.getScope().resolveType(node.getIsIdentifier()));
		}
		node.setType(CoreClasses.boolType());
	}

	/** {@inheritDoc} */
	@Override
	public void visit(MemberAccess node) {
		visitDoubleDispatched(node.getLeft());
		TypeDeclaration leftType = node.getLeft().getType();

		if (leftType instanceof ClassDeclaration) {
			node.getRight().setScope(leftType.getScope());
		}
		visitDoubleDispatched(node.getRight());

		TypeDeclaration type = node.getRight().getType();
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
		node.setType(scope.resolveType(node.getIdentifier()));
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(IntegerLiteral node) {
		node.setType(CoreClasses.intType());
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(FloatLiteral node) {
		node.setType(CoreClasses.floatType());
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(CharacterLiteral node) {
		node.setType(CoreClasses.charType());
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(StringLiteral node) {
		node.setType(CoreClasses.stringType());
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(BooleanLiteral node) {
		node.setType(CoreClasses.boolType());
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ArrayLiteral node) {
		node.setType(CoreClasses.arrayType());
		super.visit(node);
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
				node.setInferredReturnType(((ReturnStatement) returnStatement).getParameter().getType());
			} else {
				throw new MontyBaseException(node, "can not infer return type!");
			}
		} else {
			if (node.isFunction()) {
				Scope scope = node.getScope();
				TypeDeclaration returnType = scope.resolveType(node.getReturnTypeIdentifier());
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
		Declaration declaration = null;

		declaration = scope.tryToResolveType(node.getIdentifier());

		// constructor call
		if (declaration instanceof ClassDeclaration) {
			ClassDeclaration classDecl = (ClassDeclaration) declaration;
			ResolvableIdentifier identifier = node.getIdentifier();
			if (classDecl.isAbstract()) {
				throw new InvalidExpressionException(node, "The abstract class '" + identifier.toString()
				        + "' must not be instantiated");
			}
			node.setType(classDecl);
			FunctionDeclaration initializer = (FunctionDeclaration) findMatchingInitializer(node, classDecl);
			initializer = (initializer != null) ? initializer : classDecl.getDefaultInitializer();
			node.setDeclaration(initializer);
		} else {
			List<TypeDeclaration> argTypes = new ArrayList<>(node.getArguments().size());
			for (Expression exp : node.getArguments()) {
				argTypes.add(exp.getType());
			}
			Declaration callableDeclaration = overloadResolution(argTypes, scope.resolveFunction(node.getIdentifier()));

			if (callableDeclaration instanceof FunctionDeclaration) {
				FunctionDeclaration function = (FunctionDeclaration) callableDeclaration;
				node.setDeclaration(function);
				if (function.isFunction()) {
					visitDoubleDispatched(function);

					node.setType(function.getReturnType());
				} else {
					node.setType(CoreClasses.voidType());
				}
			} else if (callableDeclaration instanceof VariableDeclaration) {
				callVariable(node, callableDeclaration);
			} else {
				throw new TypeMismatchException(node, "Arguments of function call do not match declaration.");
			}
		}
	}

	/** this method does the wrapping logic for function objects
	 *
	 * @param node
	 * @param callableDeclaration */
	protected void callVariable(FunctionCall node, Declaration callableDeclaration) {
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
	private Declaration findMatchingInitializer(FunctionCall node, ClassDeclaration classDeclaration) {

		List<Declaration> functions = new ArrayList<>();

		// iterate through the declarations of the given class
		for (Declaration declaration : classDeclaration.getBlock().getDeclarations()) {
			// find a matching declaration
			if ("initializer".equals(declaration.getIdentifier().getSymbol())) {
				// and verify that it is a function...
				if (declaration instanceof FunctionDeclaration) {
					// without any return type
					if (!(((FunctionDeclaration) declaration).isFunction())) {
						functions.add(declaration);
					}
				}
			}
		}

		if (functions.isEmpty()) {
			return null;
		} else {
			List<TypeDeclaration> argTypes = new ArrayList<>(node.getArguments().size());
			for (Expression exp : node.getArguments()) {
				argTypes.add(exp.getType());
			}
			return overloadResolution(argTypes, functions);
		}
	}

	/** this one performs the best-fit algorithm, described in the language specification
	 *
	 * @param arguments
	 * @param functions
	 * @return */
	public Declaration overloadResolution(List<TypeDeclaration> arguments, List<Declaration> functions) {
		List<OverloadCandidate> candidates = new ArrayList<>();
		for (Declaration declaration : functions) {
			if (declaration instanceof FunctionDeclaration) {
				handleOverloadedFunction(arguments, declaration, candidates);
			} else if (declaration instanceof VariableDeclaration) {
				handleOverloadedVariable(arguments, declaration, candidates);
			} else {
				throw new RuntimeException("Invalid declaration type for overload resolution: '"
				        + declaration.getIdentifier() + "'!");
			}
		}
		Declaration result = null;
		// find out the minimum type distance
		int minScore = Integer.MAX_VALUE;
		for (OverloadCandidate candidate : candidates) {
			if (candidate.getScore() < minScore) {
				minScore = candidate.getScore();
				result = candidate.getDeclaration();
			}
		}
		return result;
	}

	/** this is a helper method which finds out whether a typeDeclaration is a classdeclarationvariation or a subtype of
	 * some. This is needed since function types are subtypes of Function<Param, Return>, but not
	 * ClassDeclarationVariations themself.
	 *
	 * @param type
	 * @return */
	protected ClassDeclarationVariation getClassDeclarationVariationFromTypeDecl(TypeDeclaration type) {
		if (type instanceof ClassDeclarationVariation) {
			return (ClassDeclarationVariation) type;
		} else if (type instanceof ClassDeclaration) {
			List<TypeDeclaration> superClasses = ((ClassDeclaration) type).getSuperClassDeclarations();
			for (TypeDeclaration decl : superClasses) {
				ClassDeclarationVariation result = getClassDeclarationVariationFromTypeDecl(decl);
				if (result != null) {
					return result;
				}
			}
		}
		return null;
	}

	/** adds 'declaration' to 'candidates' if the signature of it matches 'arguments'
	 *
	 * @param arguments
	 * @param declaration
	 * @param candidates */
	protected void handleOverloadedFunction(List<TypeDeclaration> arguments, Declaration declaration,
	        List<OverloadCandidate> candidates) {
		FunctionDeclaration funDecl = (FunctionDeclaration) declaration;
		if (funDecl.getParameters().size() == arguments.size()) {
			int score = 0;
			int argIndex = 0;
			for (VariableDeclaration param : funDecl.getParameters()) {
				visitDoubleDispatched(param);
				int typeDist = param.getType().getTypeDist(arguments.get(argIndex));
				if ((typeDist < Integer.MAX_VALUE) && (score < Integer.MAX_VALUE)) {
					score += typeDist;
					argIndex++;
				} else {
					score = Integer.MAX_VALUE;
					break;
				}
			}
			if (score != Integer.MAX_VALUE) {
				candidates.add(new OverloadCandidate(declaration, score));
			}
			// special case for functions without parameters
		} else if ((arguments.size() == 0) && (funDecl.getParameters().size() == 1)
		        && (funDecl.getParameters().get(0).getType() != null)
		        && (funDecl.getParameters().get(0).getType().getIdentifier().getSymbol().equals("Tuple0"))) {
			candidates.add(new OverloadCandidate(declaration, 0));
		}
	}

	/** does the same as handleOverloadedFunction, but for function objects stored in variables
	 *
	 * @param arguments
	 * @param declaration
	 * @param candidates */
	protected void handleOverloadedVariable(List<TypeDeclaration> arguments, Declaration declaration,
	        List<OverloadCandidate> candidates) {
		ClassDeclarationVariation typedecl =
		        getClassDeclarationVariationFromTypeDecl(((VariableDeclaration) declaration).getType());
		if (typedecl != null) {
			if (typedecl.getConcreteGenericTypes().size() == 2) {
				ClassDeclaration concreteParamType = typedecl.getConcreteGenericTypes().get(0);
				List<ClassDeclaration> paramTypes;
				if (TupleDeclarationFactory.isTuple(concreteParamType)) {
					paramTypes = ((ClassDeclarationVariation) concreteParamType).getConcreteGenericTypes();
				} else {
					paramTypes = new ArrayList<>(1);
					paramTypes.add(concreteParamType);
				}
				if (paramTypes.size() == arguments.size()) {
					int score = 0;
					int argIndex = 0;
					for (ClassDeclaration genericType : paramTypes) {
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
						candidates.add(new OverloadCandidate(declaration, score));
					}
				}
				// special case for functions without parameters
				else if ((arguments.size() == 0) && (paramTypes.size() == 1)
				        && (paramTypes.get(0).getIdentifier().getSymbol().equals("Tuple0"))) {
					candidates.add(new OverloadCandidate(declaration, 0));
				}
			}
		}
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ReturnStatement node) {
		super.visit(node);

		if (node instanceof YieldStatement) {
			FunctionDeclaration parentNode = (FunctionDeclaration) node.getParentNodeByType(FunctionDeclaration.class);
			if (parentNode instanceof GeneratorFunctionDeclaration) {
				((GeneratorFunctionDeclaration) parentNode).addYieldStatement((YieldStatement) node);
			}
		}
	}
}
