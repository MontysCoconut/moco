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

import de.uni.bremen.monty.moco.ast.ASTNode;
import de.uni.bremen.monty.moco.ast.Block;
import de.uni.bremen.monty.moco.ast.CoreClasses;
import de.uni.bremen.monty.moco.ast.Package;
import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.ast.expression.*;
import de.uni.bremen.monty.moco.ast.expression.literal.*;
import de.uni.bremen.monty.moco.ast.statement.*;
import de.uni.bremen.monty.moco.codegeneration.CodeGenerator;
import de.uni.bremen.monty.moco.codegeneration.CodeWriter;
import de.uni.bremen.monty.moco.codegeneration.context.CodeContext;
import de.uni.bremen.monty.moco.codegeneration.context.ContextUtils;
import de.uni.bremen.monty.moco.codegeneration.identifier.LLVMIdentifier;
import de.uni.bremen.monty.moco.codegeneration.identifier.LLVMIdentifierFactory;
import de.uni.bremen.monty.moco.codegeneration.types.LLVMStructType;
import de.uni.bremen.monty.moco.codegeneration.types.LLVMType;
import de.uni.bremen.monty.moco.codegeneration.types.LLVMPointer;
import de.uni.bremen.monty.moco.codegeneration.types.LLVMTypeFactory;
import de.uni.bremen.monty.moco.codegeneration.types.TypeConverter;
import de.uni.bremen.monty.moco.util.Params;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Stack;

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
	private final CodeWriter codeWriter;

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

	/** Only Expressions push to a Stack. So this is a Stack of Stacks so every Statement has its own stack.
	 * 
	 * e.g. the FunctionCall as a statement would leave behind a non-empty stack. */
	private Stack<Stack<LLVMIdentifier<LLVMType>>> stackOfStacks = new Stack<>();

	public CodeGenerationVisitor(Params params) throws IOException {
		TypeConverter typeConverter = new TypeConverter(llvmIdentifierFactory, contextUtils.constant());
		this.codeWriter = new CodeWriter(params);
		this.codeGenerator = new CodeGenerator(typeConverter, llvmIdentifierFactory);
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

	private List<LLVMIdentifier<? extends LLVMType>> buildLLVMParameter(ProcedureDeclaration node) {
		List<LLVMIdentifier<? extends LLVMType>> llvmParameter = new ArrayList<>();

		if (node.isMethod() || node.isInitializer()) {
			LLVMType selfType = codeGenerator.mapToLLVMType(node.getDefiningClass());
			LLVMIdentifier<LLVMType> selfReference = llvmIdentifierFactory.newLocal("self", selfType, false);
			llvmParameter.add(selfReference);
		}

		for (VariableDeclaration param : node.getParameter()) {
			LLVMType llvmType = codeGenerator.mapToLLVMType(param.getType());
			llvmType = llvmType instanceof LLVMStructType ? pointer(llvmType) : llvmType;
			boolean resolvable = llvmType instanceof LLVMStructType;
			LLVMIdentifier<LLVMType> e =
			        llvmIdentifierFactory.newLocal(param.getMangledIdentifier().getSymbol(), llvmType, resolvable);

			llvmParameter.add(e);
		}
		return llvmParameter;
	}

	private void addFunction(ProcedureDeclaration node, TypeDeclaration returnType) {
		List<LLVMIdentifier<? extends LLVMType>> llvmParameter = buildLLVMParameter(node);
		String name = node.getMangledIdentifier().getSymbol();
		codeGenerator.addFunction(contextUtils.active(), returnType, llvmParameter, name);
	}

	private void addNativeFunction(ProcedureDeclaration node, TypeDeclaration returnType) {
		List<LLVMIdentifier<? extends LLVMType>> llvmParameter = buildLLVMParameter(node);
		String name = node.getMangledIdentifier().getSymbol();
		codeGenerator.addNativeFunction(contextUtils.active(), returnType, llvmParameter, name);
	}

	private boolean isNative(ASTNode node) {
		while (node.getParentNode() != null) {
			node = node.getParentNode();
			if (node instanceof Package) {
				if (((Package) node).isNativePackage()) {
					return true;
				}
			}
		}
		return false;
	}

	protected void writeData() throws IOException {
		codeWriter.write(contextUtils.getData());
	}

	@Override
	protected void onEnterEachNode(ASTNode node) {
		contextUtils.setNode(node);
	}

	@Override
	protected void onExitChildrenEachNode(ASTNode node) {
		contextUtils.setNode(node);
	}

	@Override
	public void visit(Package node) {
		contextUtils.setNode(node);
		if (node.getParentNode() == null) {
			openNewFunctionScope();
			codeGenerator.addMain(contextUtils.active());

			super.visit(node);

			codeGenerator.returnMain(contextUtils.active());
			closeFunctionContext();

			try {
				writeData();
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
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
			stackOfStacks.push(stack);
			stack = new Stack<>();
			visitDoubleDispatched(statement);
			stack = stackOfStacks.pop();
		}
	}

	@Override
	public void visit(Assignment node) {
		super.visit(node);
		LLVMIdentifier<LLVMType> source = stack.pop();
		LLVMIdentifier<LLVMType> target = stack.pop();
		codeGenerator.assign(contextUtils.active(), target, source);
	}

	@Override
	public void visit(ClassDeclaration node) {
		// These are not boxed yet. So they cant inherit from object and cant have initializers.
		List<ClassDeclaration> treatSpecial =
		        Arrays.asList(CoreClasses.stringType(), CoreClasses.arrayType(), CoreClasses.voidType());
		if (!treatSpecial.contains(node)) {
			openNewFunctionScope();
			codeGenerator.buildConstructor(contextUtils.active(), node);
			closeFunctionContext();
		}
		super.visit(node);
	}

	@Override
	public void visit(VariableDeclaration node) {
		super.visit(node);
		if (!node.isAttribute()) {
			if (node.getIsGlobal()) {
				codeGenerator.declareGlobalVariable(
				        contextUtils.constant(),
				        node.getMangledIdentifier().getSymbol(),
				        node.getType());
			} else {
				codeGenerator.declareLocalVariable(
				        contextUtils.active(),
				        node.getMangledIdentifier().getSymbol(),
				        node.getType());
			}
		}
	}

	@Override
	public void visit(VariableAccess node) {
		super.visit(node);

		VariableDeclaration varDeclaration = (VariableDeclaration) node.getDeclaration();

		LLVMIdentifier<LLVMType> llvmIdentifier;
		if (varDeclaration.getIsGlobal()) {
			llvmIdentifier =
			        codeGenerator.resolveGlobalVarName(node.getMangledIdentifier().getSymbol(), node.getType());
		} else if (varDeclaration.isAttribute()) {
			LLVMIdentifier<?> leftIdentifier = stack.pop();
			llvmIdentifier =
			        codeGenerator.accessMember(
			                contextUtils.active(),
			                (LLVMIdentifier<LLVMPointer<LLVMType>>) leftIdentifier,
			                varDeclaration.getAttributeIndex(),
			                node.getType(),
			                !node.getLValue());
		} else {
			llvmIdentifier =
			        codeGenerator.resolveLocalVarName(
			                node.getMangledIdentifier().getSymbol(),
			                node.getType(),
			                !varDeclaration.isParameter());
		}
		stack.push(llvmIdentifier);
	}

	@SuppressWarnings("unchecked")
	@Override
	public void visit(SelfExpression node) {
		stack.push(codeGenerator.resolveLocalVarName("self", node.getType(), false));
	}

	@SuppressWarnings("unchecked")
	@Override
	public void visit(ParentExpression node) {
		LLVMIdentifier<?> self = codeGenerator.resolveLocalVarName("self", node.getSelfType(), false);
		LLVMIdentifier<?> result =
		        codeGenerator.castClass(
		                contextUtils.active(),
		                (LLVMIdentifier<LLVMPointer<LLVMType>>) self,
		                node.getSelfType(),
		                (ClassDeclaration) node.getType(),
		                codeGenerator.createLabelPrefix("cast", node));
		stack.push((LLVMIdentifier<LLVMType>) result);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(CastExpression node) {
		super.visit(node);
		LLVMIdentifier<?> object = stack.pop();
		LLVMIdentifier<?> result =
		        codeGenerator.castClass(
		                contextUtils.active(),
		                (LLVMIdentifier<LLVMPointer<LLVMType>>) object,
		                (ClassDeclaration) node.getExpression().getType(),
		                (ClassDeclaration) node.getType(),
		                codeGenerator.createLabelPrefix("cast", node));
		stack.push((LLVMIdentifier<LLVMType>) result);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(IsExpression node) {
		super.visit(node);
		LLVMIdentifier<?> object = stack.pop();
		LLVMIdentifier<?> result =
		        codeGenerator.isClass(
		                contextUtils.active(),
		                (LLVMIdentifier<LLVMPointer<LLVMType>>) object,
		                (ClassDeclaration) node.getExpression().getType(),
		                (ClassDeclaration) node.getToType());
		LLVMIdentifier<LLVMType> boxedResult =
		        codeGenerator.boxType(contextUtils.active(), (LLVMIdentifier<LLVMType>) result, CoreClasses.boolType());
		stack.push(boxedResult);
	}

	@Override
	public void visit(MemberAccess node) {
		super.visit(node);
		// If right is VariableAccess, everything is done in visit(VariableAccess)
		// If right is FunctionCall, everything is done in visit(FunctionCall)
	}

	@SuppressWarnings("unchecked")
	@Override
	public void visit(StringLiteral node) {
		super.visit(node);

		LLVMIdentifier<? extends LLVMType> addr =
		        codeGenerator.addConstantString(contextUtils.constant(), node.getValue());
		stack.push((LLVMIdentifier<LLVMType>) addr);
	}

	@SuppressWarnings("unchecked")
	@Override
	public void visit(CharacterLiteral node) {
		super.visit(node);
		LLVMIdentifier<? extends LLVMType> addr = codeGenerator.loadChar(node.getValue());
		// Boxing
		CodeContext c = contextUtils.active();
		LLVMIdentifier<LLVMType> box = codeGenerator.boxType(c, (LLVMIdentifier<LLVMType>) addr, node.getType());
		stack.push(box);
	}

	@SuppressWarnings("unchecked")
	@Override
	public void visit(IntegerLiteral node) {
		super.visit(node);

		LLVMIdentifier<? extends LLVMType> addr = codeGenerator.loadInt(node.getValue());
		// Boxing
		CodeContext c = contextUtils.active();
		LLVMIdentifier<LLVMType> box = codeGenerator.boxType(c, (LLVMIdentifier<LLVMType>) addr, node.getType());
		stack.push(box);
	}

	@SuppressWarnings("unchecked")
	@Override
	public void visit(BooleanLiteral node) {
		super.visit(node);

		LLVMIdentifier<? extends LLVMType> addr = codeGenerator.loadBool(node.getValue());
		// Boxing
		CodeContext c = contextUtils.active();
		LLVMIdentifier<LLVMType> box = codeGenerator.boxType(c, (LLVMIdentifier<LLVMType>) addr, node.getType());
		stack.push(box);
	}

	@SuppressWarnings("unchecked")
	@Override
	public void visit(FloatLiteral node) {
		super.visit(node);

		LLVMIdentifier<? extends LLVMType> addr = codeGenerator.loadFloat(node.getValue());
		// Boxing
		CodeContext c = contextUtils.active();
		LLVMIdentifier<LLVMType> box = codeGenerator.boxType(c, (LLVMIdentifier<LLVMType>) addr, node.getType());
		stack.push(box);
	}

	@Override
	public void visit(ArrayLiteral node) {
		super.visit(node);

		ClassDeclaration type = (ClassDeclaration) node.getType();
		LLVMIdentifier<LLVMPointer<LLVMStructType>> array =
		        codeGenerator.addArray(contextUtils.active(), node.getEntries().size(), type);
		for (int i = node.getEntries().size() - 1; i >= 0; i--) {
			codeGenerator.setArrayElement(contextUtils.active(), array, i, stack.pop());
		}

		stack.push((LLVMIdentifier) array);
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

		List<TypeDeclaration> expectedParameters = new ArrayList<>();
		for (VariableDeclaration varDeclaration : node.getDeclaration().getParameter()) {
			expectedParameters.add(varDeclaration.getType());
		}
		List<LLVMIdentifier<?>> arguments = new ArrayList<>(node.getArguments().size());
		for (int i = 0; i < node.getArguments().size(); i++) {
			arguments.add(stack.pop());
		}
		Collections.reverse(arguments);

		ProcedureDeclaration declaration = node.getDeclaration();
		ClassDeclaration definingClass = declaration.getDefiningClass();

		List<ClassDeclaration> treatSpecial =
		        Arrays.asList(
		                CoreClasses.intType(),
		                CoreClasses.boolType(),
		                CoreClasses.floatType(),
		                CoreClasses.charType());
		if (declaration.isInitializer() && treatSpecial.contains(definingClass)) {
			// Instead of calling the initializer of this boxed type with a boxed value as arguments just push the
			// argument on the stack and return.
			stack.push((LLVMIdentifier<LLVMType>) arguments.get(0));
			return;
		}

		if (declaration.isMethod() || declaration.isInitializer()) {
			expectedParameters.add(0, definingClass);
			if (declaration.isMethod()
			        || (declaration.isInitializer() && (node.getParentNode() instanceof MemberAccess))) {
				arguments.add(0, stack.pop());
			} else if (declaration.isInitializer()) {
				LLVMIdentifier<LLVMType> selfReference =
				        codeGenerator.callConstructor(contextUtils.active(), definingClass);
				codeGenerator.callVoid(
				        contextUtils.active(),
				        definingClass.getDefaultInitializer().getMangledIdentifier().getSymbol(),
				        Arrays.<LLVMIdentifier<?>> asList(selfReference),
				        Arrays.<TypeDeclaration> asList(definingClass));
				arguments.add(0, selfReference);
			}
		}

		if (declaration.isMethod() && !declaration.isInitializer()) {
			if (declaration instanceof FunctionDeclaration) {
				stack.push((LLVMIdentifier<LLVMType>) codeGenerator.callMethod(
				        contextUtils.active(),
				        (FunctionDeclaration) declaration,
				        arguments,
				        expectedParameters));
			} else {
				codeGenerator.callVoidMethod(contextUtils.active(), declaration, arguments, expectedParameters);
			}
		} else {
			if (declaration instanceof FunctionDeclaration) {
				stack.push((LLVMIdentifier<LLVMType>) codeGenerator.call(
				        contextUtils.active(),
				        declaration.getMangledIdentifier().getSymbol(),
				        node.getType(),
				        arguments,
				        expectedParameters));
			} else {
				if (declaration.isInitializer()) {
					stack.push((LLVMIdentifier<LLVMType>) arguments.get(0));
				}
				codeGenerator.callVoid(
				        contextUtils.active(),
				        declaration.getMangledIdentifier().getSymbol(),
				        arguments,
				        expectedParameters);
			}
		}
	}

	@Override
	public void visit(FunctionDeclaration node) {
		openNewFunctionScope();
		if (isNative(node)) {
			addNativeFunction(node, node.getReturnType());
		} else {
			addFunction(node, node.getReturnType());
			visitDoubleDispatched(node.getBody());
		}
		closeFunctionContext();
	}

	@Override
	public void visit(ProcedureDeclaration node) {
		openNewFunctionScope();

		if (isNative(node) && !node.isInitializer()) {
			addNativeFunction(node, CoreClasses.voidType());
		} else {
			addFunction(node, CoreClasses.voidType());

			visitDoubleDispatched(node.getBody());
			if (node.isInitializer()) {
				codeGenerator.returnValue(
				        contextUtils.active(),
				        (LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) llvmIdentifierFactory.voidId(),
				        CoreClasses.voidType());
			}
		}
		closeFunctionContext();
	}

	@Override
	public void visit(ReturnStatement node) {
		super.visit(node);
		if (node.getParameter() != null) {
			ASTNode parent = node;
			while (!(parent instanceof FunctionDeclaration)) {
				parent = parent.getParentNode();
			}
			LLVMIdentifier<LLVMType> returnValue = stack.pop();
			codeGenerator.returnValue(
			        contextUtils.active(),
			        returnValue,
			        ((FunctionDeclaration) parent).getReturnType());
		} else {
			codeGenerator.returnValue(
			        contextUtils.active(),
			        (LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) llvmIdentifierFactory.voidId(),
			        CoreClasses.voidType());
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
