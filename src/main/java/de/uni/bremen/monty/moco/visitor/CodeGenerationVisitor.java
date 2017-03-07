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
import de.uni.bremen.monty.moco.ast.Package;
import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.ast.expression.*;
import de.uni.bremen.monty.moco.ast.expression.literal.*;
import de.uni.bremen.monty.moco.ast.statement.*;
import de.uni.bremen.monty.moco.ast.types.*;
import de.uni.bremen.monty.moco.codegeneration.CodeGenerator;
import de.uni.bremen.monty.moco.codegeneration.NameMangler;
import de.uni.bremen.monty.moco.codegeneration.context.CodeContext;
import de.uni.bremen.monty.moco.codegeneration.context.ContextUtils;
import de.uni.bremen.monty.moco.codegeneration.identifier.LLVMIdentifier;
import de.uni.bremen.monty.moco.codegeneration.identifier.LLVMIdentifierFactory;
import de.uni.bremen.monty.moco.codegeneration.types.*;

import java.util.*;

import static de.uni.bremen.monty.moco.codegeneration.types.LLVMTypeFactory.pointer;

/** The CodeGenerationVisitor has the following tasks:
 *
 * <p>
 * <ul>
 * <li>Process the AST</li>
 * <li>Delegates as much work as possible to the CodeGenerator</li>
 * <li>Tell the CodeGenerator in which {@link CodeContext} to write</li>
 * <li>Evaluated expression should be given Statements as Arguments, see {@link #stack}</li>
 * </ul>
 * </p> */
public class CodeGenerationVisitor extends BaseVisitor {

	private final LLVMIdentifierFactory llvmIdentifierFactory = new LLVMIdentifierFactory();
	private ContextUtils contextUtils = new ContextUtils();
	private final CodeGenerator codeGenerator;
	private final NameMangler nameMangler;

	private TypeContext currentContext = TypeContext.EMPTY;

	/** Each Expression pushes it's evaluated value onto the Stack. The value is represented by a LLVMIdentifier where
	 * the evaluated value is stored at runtime.
	 *
	 * Statements or complex Expressions can pop those values from the stack, which they use as parameters for further
	 * calculation.
	 *
	 * e.g. a := 3 is an Assignment having a VariableAccess and IntLiteral as children. VariableAccess and IntLiteral
	 * are expressions, thus pushing their values on the stack. An Assignment on the other hand is an Statement and
	 * return nothing, so doesn't push sth. on the stack, but instead it needs two Arguments. Those are popped from the
	 * Stack and yield the the evaluated VariableAccess and IntLiteral.
	 *
	 * Of course this only works, if the Assignment first process the children and afterwards popping from the stack. */
	private Stack<LLVMIdentifier<LLVMType>> stack = new Stack<>();

	public CodeGenerationVisitor() {
		nameMangler = new NameMangler();
		TypeConverter typeConverter = new TypeConverter(llvmIdentifierFactory, contextUtils.constant(), nameMangler);
		this.codeGenerator = new CodeGenerator(typeConverter, llvmIdentifierFactory, nameMangler);
	}

	public void writeLLVMCode(StringBuffer llvmOutput) {
		contextUtils.writeLLVMCode(llvmOutput);
	}

	private void openNewFunctionScope() {
		contextUtils.addNewContext();
		llvmIdentifierFactory.openScope();
	}

	private void closeFunctionContext() {
		contextUtils.active().close();
		contextUtils.closeContext();
		llvmIdentifierFactory.closeScope();
	}

	private List<LLVMIdentifier<? extends LLVMType>> buildLLVMParameter(ConcreteFunctionType node) {
		List<LLVMIdentifier<? extends LLVMType>> llvmParameter = new ArrayList<>();

		if (node.isMethod() || node.isInitializer()) {
			LLVMType selfType = codeGenerator.mapToLLVMType(node.getDefiningClass());
			LLVMIdentifier<LLVMType> selfReference = llvmIdentifierFactory.newLocal("self", selfType, false);
			llvmParameter.add(selfReference);
		}

		if (node.isClosure()) {
			ConcreteType typeDeclaration = node.getWrapperClass();
			LLVMType contextType = codeGenerator.mapToLLVMType(typeDeclaration);
			LLVMIdentifier<LLVMType> ctxReference = llvmIdentifierFactory.newLocal("..ctx..", contextType, false);
			llvmParameter.add(ctxReference);
		}

		for (ConcreteVariableType param : node.getParameter()) {
			LLVMType llvmType = codeGenerator.mapToLLVMType(param);
			llvmType = llvmType instanceof LLVMStructType ? pointer(llvmType) : llvmType;
			boolean resolvable = llvmType instanceof LLVMStructType;
			LLVMIdentifier<LLVMType> e =
			        llvmIdentifierFactory.newLocal(nameMangler.mangleVariable(param), llvmType, resolvable);

			llvmParameter.add(e);
		}
		return llvmParameter;
	}

	private void addFunction(ConcreteFunctionType node, ConcreteType returnType) {
		List<LLVMIdentifier<? extends LLVMType>> llvmParameter = buildLLVMParameter(node);
		String name = nameMangler.mangleFunction(node);
		codeGenerator.addFunction(contextUtils.active(), returnType, llvmParameter, name);

		if (node instanceof GeneratorFunctionType) {
			addGeneratorJumpHeader((ConcreteGeneratorFunctionType) node);
		}
	}

	private void addGeneratorJumpHeader(ConcreteGeneratorFunctionType node) {
		ConcreteType selfType = node.getDefiningClass();
		LLVMIdentifier<LLVMPointer<LLVMType>> self = codeGenerator.resolveLocalVarName("self", selfType, false);

		LLVMIdentifier<LLVMType> pointervar =
		        codeGenerator.accessGeneratorJumpPointer(contextUtils.active(), self, node.getDefiningClass(), 0, false); // no
		                                                                                                                  // LValue

		// get the actual attribute
		LLVMType jumpPrtType = LLVMTypeFactory.pointer(LLVMTypeFactory.int8());
		LLVMIdentifier<LLVMType> pointercontentvar = llvmIdentifierFactory.newLocal(jumpPrtType, false);
		contextUtils.active().load(llvmIdentifierFactory.pointerTo(pointervar), pointercontentvar);

		// get all possible target labels:
		String labels = "label %startGenerator";
		for (int i = 0; i < node.getYieldStatements().size(); i++) {
			labels += ", label %yield" + i;
		}
		// jump to the label which is stored in that attribute
		contextUtils.active().appendLine("indirectbr " + pointercontentvar + ", [ " + labels + " ]");
		contextUtils.active().label("startGenerator");
	}

	private void setGeneratorLabel(ConcreteType classDecl, String label) {
		LLVMIdentifier<LLVMPointer<LLVMType>> self = codeGenerator.resolveLocalVarName("self", classDecl, false);

		LLVMIdentifier<LLVMType> pointervar =
		        codeGenerator.accessGeneratorJumpPointer(contextUtils.active(), self, classDecl, 0, true); // used as
		                                                                                                   // lvalue
		ConcreteFunctionType inFunction = null;
		for (ConcreteFunctionType fun : classDecl.getMethods()) {
			if (fun.getIdentifier().getSymbol().equals("getNext")) {
				inFunction = fun;
				break;
			}
		}

		String funName = nameMangler.mangleFunction(inFunction);
		contextUtils.active().appendLine(
		        "store i8* blockaddress(@" + funName + ", %" + label + "), "
		                + llvmIdentifierFactory.pointerTo(pointervar));
	}

	private void addNativeFunction(ConcreteFunctionType node, ConcreteType returnType) {
		List<LLVMIdentifier<? extends LLVMType>> llvmParameter = buildLLVMParameter(node);
		String name = nameMangler.mangleFunction(node);
		codeGenerator.addNativeFunction(contextUtils.active(), returnType, llvmParameter, name);
	}

	@Override
	public void visit(Package node) {
		if (node.getParentNode() == null) {
			openNewFunctionScope();
			codeGenerator.addMain(contextUtils.active());

			super.visit(node);

			codeGenerator.returnMain(contextUtils.active());
			closeFunctionContext();
		} else {
			super.visit(node);
		}
	}

	@Override
	public void visit(Block node) {
		for (Declaration declaration : node.getDeclarations()) {
			visitDoubleDispatched(declaration);
		}
		for (Statement statement : node.getStatements()) {
			stack.clear();
			visitDoubleDispatched(statement);
		}
	}

	@Override
	public void visit(Assignment node) {
		super.visit(node);
		LLVMIdentifier<LLVMType> source = stack.pop();
		LLVMIdentifier<LLVMType> target = stack.pop();
		codeGenerator.assign(contextUtils.active(), target, source);

		if (node.belongsToFunctionWrapper()) {
			initializeClosureWrapper(node, source);
		}
	}

	protected void initializeClosureWrapper(Assignment node, LLVMIdentifier<?> source) {
		Position pos = node.getPosition();
		FunctionDeclaration function = node.getCorrespondingFunctionWrapper();
		if (function.isClosure()) {
			ClassDeclaration functionWrapper = function.getWrapperClass();
			for (VariableDeclaration var : function.getClosureVariableOriginalDeclarations()) {
				VariableDeclaration localVar = function.getClosureVariable(var);
				VariableAccess lvalue = new VariableAccess(pos, ResolvableIdentifier.convert(var.getIdentifier()));
				lvalue.setLValue();
				LLVMIdentifier<LLVMType> closureTarget =
				        codeGenerator.accessClosureContextMember(
				                contextUtils.active(),
								(TypeFactory.makeConcrete(functionWrapper, currentContext)),
				                localVar,
				                lvalue,
				                currentContext.makeConcrete(var.getType()),
				                (LLVMIdentifier<LLVMPointer<LLVMType>>) source);

				if (var.getIdentifier().getSymbol().equals("self")) {
					SelfExpression self = new SelfExpression(pos);
					self.setType(var.getType());
					self.setParentNode(node.getParentNode());
					visit(self);
				} else {
					VariableAccess varAccess =
					        new VariableAccess(pos, ResolvableIdentifier.convert(var.getIdentifier()));
					varAccess.setType(var.getType());
					varAccess.setDeclaration(var);
					varAccess.setParentNode(node.getParentNode());
					varAccess.setScope(node.getScope());
					visit(varAccess);
				}
				LLVMIdentifier<LLVMType> closureSource = stack.pop();
				codeGenerator.assign(contextUtils.active(), closureTarget, closureSource);
			}
		}
	}

	@Override
	public void visit(ClassDeclaration node) {
		//Do nothing
	}

	public void visitAllClasses() {
		for (ConcreteType variation : ConcreteType.getAllTypes()) {
            currentContext = variation.getContext();
            if (!variation.equals(Types.voidType())) {
                openNewFunctionScope();
                codeGenerator.buildConstructor(contextUtils.active(), variation);
                closeFunctionContext();
            }
            visitDoubleDispatched(variation.getBlock());
		}
	}

	@Override
	public void visit(VariableDeclaration node) {
		ConcreteVariableType variableType = TypeFactory.makeConcrete(node, currentContext);
		super.visit(node);
		if (!node.isAttribute()) {
			ConcreteType type = currentContext.makeConcrete(node.getType());
			if (node.getIsGlobal()) {
				codeGenerator.declareGlobalVariable(
				        contextUtils.constant(),
				        nameMangler.mangleVariable(variableType),
						type);
			} else if (!(node.getParentNodeByType(FunctionDeclaration.class) instanceof GeneratorFunctionDeclaration)) {
				// only do sth. if the enclosing function is not a generator function.
				// if it is, the declaration is already made somewhere else...
				codeGenerator.declareLocalVariable(
				        contextUtils.active(),
				        nameMangler.mangleVariable(variableType),
						type);
			}
		}
	}

	@Override
	public void visit(VariableAccess node) {
		super.visit(node);

		VariableDeclaration varDeclaration = (VariableDeclaration) node.getDeclaration();

		ConcreteVariableType variableType = TypeFactory.makeConcrete(varDeclaration, currentContext.extend(node.getScope().getContext()));

		ConcreteType type = currentContext.makeConcrete(node.getType());

		FunctionDeclaration functionParent =
				varDeclaration.getParentNodeByType(FunctionDeclaration.class);
		GeneratorFunctionDeclaration generatorParent = null;
		if (functionParent instanceof GeneratorFunctionDeclaration) {
			generatorParent =
					varDeclaration.getParentNodeByType(GeneratorFunctionDeclaration.class);
		}

		LLVMIdentifier<LLVMType> llvmIdentifier;
		if (varDeclaration.getIsGlobal()) {
			llvmIdentifier = codeGenerator.resolveGlobalVarName(nameMangler.mangleVariable(variableType), type);
		} else if (generatorParent != null) {
			llvmIdentifier =
			        codeGenerator.accessContextMember(
			                contextUtils.active(),
							TypeFactory.makeConcrete(generatorParent.getDefiningClass(), currentContext),
			                varDeclaration,
			                node,
			                type);
		} else if (node.isClosureVariable()) {
			ClassDeclaration containingClass =
			        node.getParentNodeByType(FunctionDeclaration.class).getWrapperClass();
			llvmIdentifier =
			        codeGenerator.accessClosureContextMember(
			                contextUtils.active(),
							(TypeFactory.makeConcrete(containingClass, currentContext)),
			                varDeclaration,
			                node,
			                type);
		} else if (varDeclaration.isAttribute()) {
			LLVMIdentifier<?> leftIdentifier = stack.pop();
			llvmIdentifier =
			        codeGenerator.accessMember(
			                contextUtils.active(),
			                (LLVMIdentifier<LLVMPointer<LLVMType>>) leftIdentifier,
			                varDeclaration.getAttributeIndex(),
			                type,
			                !node.getLValue());
		} else {
			llvmIdentifier =
			        codeGenerator.resolveLocalVarName(
			                nameMangler.mangleVariable(variableType),
			                type,
			                !varDeclaration.isParameter());
		}
		stack.push(llvmIdentifier);
	}

	@SuppressWarnings("unchecked")
	@Override
	public void visit(SelfExpression node) {
		Type type = node.getType();
		ConcreteType concreteType = currentContext.makeConcrete(type);
		// if 'self' is used inside a closure, we can not use the usual 'self'
		if (node.isInClosure()) {
			FunctionDeclaration funDecl = (FunctionDeclaration) node.getParentNodeByType(FunctionDeclaration.class);
			ClassDeclaration containingClass = funDecl.getWrapperClass();
			VariableAccess lvalue = new VariableAccess(new Position(), new ResolvableIdentifier("self"));

			stack.push(codeGenerator.accessClosureContextMember(
			        contextUtils.active(),
					(TypeFactory.makeConcrete(containingClass, currentContext)),
			        funDecl.getClosureVariable(new VariableDeclaration(new Position(), new Identifier("self"), concreteType,
			                VariableDeclaration.DeclarationType.VARIABLE)),
			        lvalue,
			        concreteType));
		} else {
			LLVMIdentifier<LLVMType> self = codeGenerator.resolveLocalVarName("self", concreteType, false);
			stack.push(self);
		}
	}

	@SuppressWarnings("unchecked")
	@Override
	public void visit(ParentExpression node) {
		ConcreteType selfType = currentContext.makeConcrete(node.getSelfType());
		ConcreteType resultType = currentContext.makeConcrete(node.getType());

		LLVMIdentifier<?> self = codeGenerator.resolveLocalVarName("self", selfType, false);
		LLVMIdentifier<?> result =
		        codeGenerator.castClass(
		                contextUtils.active(),
		                (LLVMIdentifier<LLVMPointer<LLVMType>>) self,
		                selfType,
						resultType,
		                codeGenerator.createLabelPrefix("cast", node));
		stack.push((LLVMIdentifier<LLVMType>) result);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(CastExpression node) {
		super.visit(node);
		LLVMIdentifier<?> object = stack.pop();
		LLVMIdentifier<?> result;
		ConcreteType resultType = currentContext.makeConcrete(node.getType());
		ConcreteType sourceType = currentContext.makeConcrete(node.getExpression().getType());
		if (node.isUnchecked()) {
			result =
			        codeGenerator.castClassUnchecked(
			                contextUtils.active(),
			                (LLVMIdentifier<LLVMPointer<LLVMType>>) object,
							resultType);
		} else {
			result =
			        codeGenerator.castClass(
			                contextUtils.active(),
			                (LLVMIdentifier<LLVMPointer<LLVMType>>) object,
							sourceType,
							resultType,
			                codeGenerator.createLabelPrefix("cast", node));
		}
		stack.push((LLVMIdentifier<LLVMType>) result);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(IsExpression node) {
		super.visit(node);
		LLVMIdentifier<?> object = stack.pop();
		ConcreteType resultType = currentContext.makeConcrete(node.getToType());
		ConcreteType sourceType = currentContext.makeConcrete(node.getExpression().getType());
		LLVMIdentifier<?> result =
		        codeGenerator.isClass(
		                contextUtils.active(),
		                (LLVMIdentifier<LLVMPointer<LLVMType>>) object,
						sourceType,
						resultType);
		LLVMIdentifier<LLVMType> boxedResult =
		        codeGenerator.boxType(contextUtils.active(), (LLVMIdentifier<LLVMType>) result, Types.boolType());
		stack.push(boxedResult);
	}

	@Override
	public void visit(MemberAccess node) {
		super.visit(node);
		// If right is VariableAccess, everything is done in visit(VariableAccess)
		// If right is FunctionCall, everything is done in visit(FunctionCall)
	}

	@Override
	public void visit(ZeroExpression node) {
		super.visit(node);
		ConcreteType type = currentContext.makeConcrete(node.getType());
		LLVMPointer llvmType = codeGenerator.mapToLLVMType(type);
		stack.push(llvmIdentifierFactory.constantNull(llvmType));
	}

	@SuppressWarnings("unchecked")
	@Override
	public void visit(StringLiteral node) {
		super.visit(node);
		LLVMIdentifier<? extends LLVMType> addr =
		        codeGenerator.addConstantString(contextUtils.constant(), node.getValue());
		// Boxing
		CodeContext c = contextUtils.active();
		LLVMIdentifier<LLVMType> box = codeGenerator.boxType(c, (LLVMIdentifier<LLVMType>) addr, (ConcreteType) node.getType());
		stack.push(box);
	}

	@SuppressWarnings("unchecked")
	@Override
	public void visit(CharacterLiteral node) {
		super.visit(node);
		LLVMIdentifier<? extends LLVMType> addr = codeGenerator.loadChar(node.getValue());
		// Boxing
		CodeContext c = contextUtils.active();
		LLVMIdentifier<LLVMType> box = codeGenerator.boxType(c, (LLVMIdentifier<LLVMType>) addr, (ConcreteType) node.getType());
		stack.push(box);
	}

	@SuppressWarnings("unchecked")
	@Override
	public void visit(IntegerLiteral node) {
		super.visit(node);

		LLVMIdentifier<? extends LLVMType> addr = codeGenerator.loadInt(node.getValue());
		// Boxing
		CodeContext c = contextUtils.active();
		LLVMIdentifier<LLVMType> box = codeGenerator.boxType(c, (LLVMIdentifier<LLVMType>) addr, (ConcreteType) node.getType());
		stack.push(box);
	}

	@SuppressWarnings("unchecked")
	@Override
	public void visit(BooleanLiteral node) {
		super.visit(node);

		LLVMIdentifier<? extends LLVMType> addr = codeGenerator.loadBool(node.getValue());
		// Boxing
		CodeContext c = contextUtils.active();
		LLVMIdentifier<LLVMType> box = codeGenerator.boxType(c, (LLVMIdentifier<LLVMType>) addr, (ConcreteType) node.getType());
		stack.push(box);
	}

	@SuppressWarnings("unchecked")
	@Override
	public void visit(FloatLiteral node) {
		super.visit(node);

		LLVMIdentifier<? extends LLVMType> addr = codeGenerator.loadFloat(node.getValue());
		// Boxing
		CodeContext c = contextUtils.active();
		LLVMIdentifier<LLVMType> box = codeGenerator.boxType(c, (LLVMIdentifier<LLVMType>) addr, (ConcreteType) node.getType());
		stack.push(box);
	}

	@Override
	public void visit(ArrayLiteral node) {
		super.visit(node);

		List<LLVMIdentifier<?>> elements = new ArrayList<>(node.getEntries().size());
		for (int i = 0; i < node.getEntries().size(); i++) {
			elements.add(stack.pop());
		}
		Collections.reverse(elements);

		LLVMIdentifier<? extends LLVMType> array = codeGenerator.buildArray(contextUtils.active(), elements);

		// Boxing
		CodeContext c = contextUtils.active();
		LLVMIdentifier<LLVMType> box = codeGenerator.boxType(c, (LLVMIdentifier<LLVMType>) array, currentContext.makeConcrete(node.getType()));
		stack.push(box);
	}

	@Override
	public void visit(ConditionalExpression node) {

		String ifPre = codeGenerator.createLabelPrefix("ifexpr", node);
		String ifTrue = ifPre + ".true";
		String ifFalse = ifPre + ".false";
		String ifEnd = ifPre + ".end";

		visitDoubleDispatched(node.getCondition());

		LLVMIdentifier<LLVMType> condition = stack.pop();
		codeGenerator.branch(contextUtils.active(), condition, ifTrue, ifFalse);

		contextUtils.active().label(ifTrue);
		visitDoubleDispatched(node.getThenExpression());
		LLVMIdentifier<LLVMType> thenExpr = stack.pop();
		contextUtils.active().branch(ifEnd);

		contextUtils.active().label(ifFalse);
		visitDoubleDispatched(node.getElseExpression());
		LLVMIdentifier<LLVMType> elseExpr = stack.pop();
		contextUtils.active().branch(ifEnd);

		contextUtils.active().label(ifEnd);
		List<LLVMIdentifier<LLVMType>> identifiers = new ArrayList<>();
		identifiers.add(thenExpr);
		identifiers.add(elseExpr);
		List<String> labels = new ArrayList<>();
		labels.add(ifTrue);
		labels.add(ifFalse);
		stack.push(contextUtils.active().phi(
		        thenExpr.getType(),
		        thenExpr.needToBeResolved(),
		        identifiers,
		        llvmIdentifierFactory.newLocal(thenExpr.getType(), thenExpr.needToBeResolved()),
		        labels));
	}

	@SuppressWarnings("unchecked")
	@Override
	public void visit(FunctionCall node) {
		super.visit(node);

		List<ConcreteType> expectedParameters = new ArrayList<>();
		for (Type paramType : node.getDeclaration().getParameterTypes()) {
			expectedParameters.add(currentContext.makeConcrete(paramType));
		}
		List<LLVMIdentifier<?>> arguments = new ArrayList<>(node.getArguments().size());
		for (int i = 0; i < node.getArguments().size(); i++) {
			arguments.add(stack.pop());
		}
		Collections.reverse(arguments);

		ConcreteFunctionType declaration = TypeFactory.makeConcrete(node.getDeclaration(),currentContext);

		Type definingClass = declaration.getDefiningClass();

		ConcreteType concreteType = definingClass == null ? null : currentContext.makeConcrete(definingClass);
		List<Type> treatSpecial =
		        Arrays.asList(
		                Types.intType(),
		                Types.boolType(),
		                Types.floatType(),
		                Types.charType(),
		                Types.stringType(),
		                Types.arrayType());
		if (declaration.isInitializer() && treatSpecial.contains(definingClass)) {
			// Instead of calling the initializer of this boxed type with a boxed value as arguments just push the
			// argument on the stack and return.
			stack.push((LLVMIdentifier<LLVMType>) arguments.get(0));
			return;
		}

		if (declaration.isMethod() || declaration.isInitializer()) {
			expectedParameters.add(0, concreteType);
			if (declaration.isMethod()) {
				arguments.add(0, stack.pop());
			} else if (declaration.isInitializer()) {
				ASTNode parentNode = node.getParentNode();
				ASTNode rightMember = node;
				if (parentNode instanceof WrappedFunctionCall) {
					rightMember = parentNode;
					parentNode = parentNode.getParentNode();
				}
				if ((parentNode instanceof MemberAccess) && (((MemberAccess) parentNode).getRight() == rightMember)) {
					arguments.add(0, stack.pop());
				} else {
					LLVMIdentifier<LLVMType> selfReference =
					        codeGenerator.callConstructor(contextUtils.active(), concreteType);
					if (!declaration.isDefaultInitializer()) {
						codeGenerator.callVoid(
						        contextUtils.active(),
						        nameMangler.mangleFunction(declaration.getDefiningClass().getDefaultInitializer()),
						        Arrays.asList(selfReference),
						        Arrays.asList(concreteType));
					}
					arguments.add(0, selfReference);
				}
			}
		}

		if (declaration.isClosure()) {
			// the 'self' of a function wrapper implementation is passed as a context to the actual function
			ConcreteType wrapperClass = declaration.getWrapperClass();

			LLVMIdentifier<LLVMPointer<LLVMType>> ctx;
			// if we're inside the wrapper class' _apply_ method, use "self" as the function's context
			if (wrapperClass.isAssignableFrom(TypeFactory.from(node.getParentNodeByType(ClassDeclaration.class), wrapperClass.getContext()))) {
				ctx = codeGenerator.resolveLocalVarName("self", wrapperClass, false);
			} else {
				// if not, use the respective wrapper object
				ctx =
				        codeGenerator.resolveLocalVarName(
				                nameMangler.mangleVariable(declaration.getWrapperFunctionObjectDeclaration()),
				                wrapperClass,
				                true);
			}
			expectedParameters.add(0, wrapperClass);
			arguments.add(0, ctx);
		}

		if (declaration.isMethod() && !declaration.isInitializer()) {
			if (declaration.isFunction()) {
				stack.push((LLVMIdentifier<LLVMType>) codeGenerator.callMethod(
				        contextUtils.active(),
						declaration,
				        arguments,
				        expectedParameters));
			} else {
				codeGenerator.callVoidMethod(contextUtils.active(), declaration, arguments, expectedParameters);
			}
		} else {
			if (declaration.isFunction()) {
				stack.push((LLVMIdentifier<LLVMType>) codeGenerator.call(
				        contextUtils.active(),
				        nameMangler.mangleFunction(declaration),
				        currentContext.makeConcrete(node.getType()),
				        arguments,
				        expectedParameters));
			} else {
				if (declaration.isInitializer()) {
					stack.push((LLVMIdentifier<LLVMType>) arguments.get(0));
				}
				codeGenerator.callVoid(
				        contextUtils.active(),
				        nameMangler.mangleFunction(declaration),
				        arguments,
				        expectedParameters);
			}
		}
	}

	@Override
	public void visit(FunctionDeclaration node) {
		ConcreteType returnType = currentContext.makeConcrete(node.getReturnType());
		ConcreteFunctionType functionType = TypeFactory.makeConcrete(node, currentContext);
		if (node.isAbstract()) {
			openNewFunctionScope();
			if ((returnType == null) || (returnType.isVoid())) {
				addFunction(functionType, Types.voidType());
				codeGenerator.returnValue(
				        contextUtils.active(),
				        (LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) llvmIdentifierFactory.voidId(),
						Types.voidType());
			} else {
				addFunction(functionType, returnType);
				codeGenerator.returnValue(
				        contextUtils.active(),
				        (LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) llvmIdentifierFactory.constantNull((LLVMPointer) codeGenerator.mapToLLVMType(returnType)),
				        returnType);
			}
			closeFunctionContext();
		} else {
			if (node.isFunction()) {
				openNewFunctionScope();
				if (node.isNative()) {
					addNativeFunction(functionType, returnType);
				} else {
					addFunction(functionType, returnType);
					visitDoubleDispatched(node.getBody());
				}
				closeFunctionContext();
			} else {
				openNewFunctionScope();
				if (node.isNative() && !node.isInitializer()) {
					addNativeFunction(functionType, returnType);
				} else {
					addFunction(functionType, returnType);

					visitDoubleDispatched(node.getBody());
					if (node.isInitializer()) {
						if (node.getDefiningClass().isGenerator()) {
							setGeneratorLabel((TypeFactory.makeConcrete(node.getDefiningClass(), currentContext)) , "startGenerator");
						}
						codeGenerator.returnValue(
						        contextUtils.active(),
						        (LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) llvmIdentifierFactory.voidId(),
								Types.voidType());
					}
				}
				closeFunctionContext();
			}
		}
	}

	@Override
	public void visit(ReturnStatement node) {
		super.visit(node);
		// if we have a yield statement here, we have to set the generator jump destination to this label
		if (node instanceof YieldStatement) {
			ClassDeclaration parentClass = node.getParentNodeByType(ClassDeclaration.class);
			setGeneratorLabel((TypeFactory.makeConcrete(parentClass, currentContext)), "yield"
			        + ((YieldStatement) node).getYieldStatementIndex());
		}

		if (node.getParameter() != null) {
			ASTNode parent = node.getParentNodeByType(FunctionDeclaration.class);
			ConcreteType returnType = currentContext.makeConcrete(((FunctionDeclaration) parent).getReturnType());

			LLVMIdentifier<LLVMType> returnValue = stack.pop();
			codeGenerator.returnValue(contextUtils.active(), returnValue, returnType);
		} else {
			codeGenerator.returnValue(
			        contextUtils.active(),
			        (LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) llvmIdentifierFactory.voidId(),
					Types.voidType());
		}
		// if we have a yield statement here, we have to add a label to jump to
		if (node instanceof YieldStatement) {
			String yieldLabel = "yield" + ((YieldStatement) node).getYieldStatementIndex();
			contextUtils.active().label(yieldLabel);
		}
	}

	@Override
	public void visit(ConditionalStatement node) {
		visitDoubleDispatched(node.getCondition());

		String ifPre = codeGenerator.createLabelPrefix("if", node);

		String ifTrue = ifPre + ".true";
		String ifFalse = ifPre + ".false";
		String ifEnd = ifPre + ".end";

		LLVMIdentifier<LLVMType> condition = stack.pop();
		codeGenerator.branch(contextUtils.active(), condition, ifTrue, ifFalse);

		contextUtils.active().label(ifTrue);
		visitDoubleDispatched(node.getThenBlock());
		contextUtils.active().branch(ifEnd);

		contextUtils.active().label(ifFalse);
		visitDoubleDispatched(node.getElseBlock());
		contextUtils.active().branch(ifEnd);

		contextUtils.active().label(ifEnd);
	}

	@Override
	public void visit(WhileLoop node) {

		String whlPre = codeGenerator.createLabelPrefix("while", node);
		String whileCond = whlPre + ".condition";
		String whileBlk = whlPre + ".block";
		String whileEnd = whlPre + ".end";

		contextUtils.active().branch(whileCond);
		contextUtils.active().label(whileCond);
		visitDoubleDispatched(node.getCondition());

		LLVMIdentifier<LLVMType> condition = stack.pop();
		codeGenerator.branch(contextUtils.active(), condition, whileBlk, whileEnd);

		contextUtils.active().label(whileBlk);
		visitDoubleDispatched(node.getBody());
		contextUtils.active().branch(whileCond);
		contextUtils.active().label(whileEnd);
	}

	@Override
	public void visit(SkipStatement node) {
		super.visit(node);
		String whlPre = codeGenerator.getLabelPrefix(node.getLoop());
		contextUtils.active().branch(whlPre + ".condition");
	}

	@Override
	public void visit(BreakStatement node) {
		super.visit(node);
		String whlPre = codeGenerator.getLabelPrefix(node.getLoop());
		contextUtils.active().branch(whlPre + ".end");
	}
}
