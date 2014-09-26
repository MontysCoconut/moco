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
package de.uni.bremen.monty.moco.ast;

import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.ast.expression.Expression;
import de.uni.bremen.monty.moco.ast.expression.FunctionCall;
import de.uni.bremen.monty.moco.ast.expression.MemberAccess;
import de.uni.bremen.monty.moco.ast.expression.VariableAccess;
import de.uni.bremen.monty.moco.ast.expression.literal.BooleanLiteral;
import de.uni.bremen.monty.moco.ast.expression.literal.StringLiteral;
import de.uni.bremen.monty.moco.ast.statement.*;
import de.uni.bremen.monty.moco.exception.InvalidPlaceToDeclareException;
import de.uni.bremen.monty.moco.exception.RedeclarationException;
import de.uni.bremen.monty.moco.visitor.DeclarationVisitor;
import de.uni.bremen.monty.moco.visitor.SetParentVisitor;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.*;

/** Test the DeclarationVisitor.
 * <p>
 * Note the following limitations! This does not test: <br>
 * - Overriding <br>
 * - Overloading <br>
 * - Inheritance with multiple super-classes <br>
 * - Access-modifiers */
public class DeclarationTest {

	private SetParentVisitor setParentVisitor;
	private DeclarationVisitor declarationVisitor;
	private int lineCounter = 0;

	// module
	private Import moduleImport01;
	private Import moduleImport02;
	private List<Import> moduleImports;
	private Block moduleBlock;
	private Package aPackage;
	private ModuleDeclaration moduleDeclaration;

	// module class
	private Block classBlock;
	private ClassDeclaration classDeclaration;
	private VariableDeclaration classVariableDeclaration;

	private Block classFunctionBlock;
	private List<VariableDeclaration> classFunctionParams;
	private FunctionDeclaration classFunctionDeclaration;

	private Block classProcedureBlock;
	private List<VariableDeclaration> classProcedureParams;
	private ProcedureDeclaration classProcedureDeclaration;

	// module class extends
	private Block extendedClassBlock;
	private ClassDeclaration extendedClassDeclaration;

	// module function
	private Block moduleFunctionBlock;
	private VariableDeclaration moduleFunctionParamsVariable01;
	private VariableDeclaration moduleFunctionParamsVariable02;
	private List<VariableDeclaration> moduleFunctionParams;
	private FunctionDeclaration moduleFunctionDeclaration;
	private VariableDeclaration moduleFunctionVariableDeclaration;
	private Block moduleFunctionFunctionBlock;
	private List<VariableDeclaration> moduleFunctionFunctionParams;
	private FunctionDeclaration moduleFunctionFunctionDeclaration;
	private Block moduleFunctionProcedureBlock;
	private List<VariableDeclaration> moduleFunctionProcedureParams;
	private ProcedureDeclaration moduleFunctionProcedureDeclaration;

	// module procedure
	private Block moduleProcedureBlock;
	private VariableDeclaration moduleProcedureParamsVariable01;
	private VariableDeclaration moduleProcedureParamsVariable02;
	private List<VariableDeclaration> moduleProcedureParams;
	private ProcedureDeclaration moduleProcedureDeclaration;
	private VariableDeclaration moduleProcedureVariableDeclaration;
	private Block moduleProcedureFunctionBlock;
	private List<VariableDeclaration> moduleProcedureFunctionParams;
	private FunctionDeclaration moduleProcedureFunctionDeclaration;
	private Block moduleProcedureProcedureBlock;
	private List<VariableDeclaration> moduleProcedureProcedureParams;
	private ProcedureDeclaration moduleProcedureProcedureDeclaration;

	// module variable
	private VariableDeclaration moduleVariableDeclaration;

	// module loop
	private Expression moduleLoopCondition;
	private Block moduleLoopBlock;
	private WhileLoop moduleLoop;
	private ContinueStatement moduleLoopContinueStatement;
	private BreakStatement moduleLoopBreakStatement;

	// module conditional
	private Expression moduleConditionalStatementCondition;
	private ConditionalStatement moduleConditionalStatement;
	private Block moduleConditionalStatementThenBlock;
	private Block moduleConditionalStatementElseBlock;

	// module function return statement
	private Expression moduleReturnStatementParameter;
	private ReturnStatement moduleReturnStatement;

	// module procedure call
	private Expression moduleProcedureCallFunctionCallParameter01;
	private Expression moduleProcedureCallFunctionCallParameter02;
	private List<Expression> moduleProcedureCallFunctionCallParameters;
	private FunctionCall moduleProcedureCall;

	// module assignment with function call as left side
	private VariableDeclaration moduleAssignmentMemberAccessVariableDeclaration;
	private VariableAccess moduleAssignmentMemberAccessVariableAccessClass;
	private VariableAccess moduleAssignmentMemberAccessVariableAccessVariable;
	private MemberAccess moduleAssignmentMemberAccess;
	private Expression moduleAssignmentFunctionCallParameter01;
	private Expression moduleAssignmentFunctionCallParameter02;
	private List<Expression> moduleAssignmentFunctionCallParameters;
	private FunctionCall moduleAssignmentFunctionCall;
	private Assignment moduleAssignment;

	// invalid modul
	private Import invalidModuleImport01;
	private Import invalidModuleImport02;
	private List<Import> invalidModuleImports;
	private Block invalidModuleBlock;
	private ModuleDeclaration invalidModuleDeclaration;

	// invalid class
	private List<ResolvableIdentifier> invalidClassSuperClasses;
	private Block invalidClassBlock;
	private ClassDeclaration invalidClassDeclaration;

	private Position buildPosition() {
		return new Position("declarationTest", lineCounter++, 0);
	}

	@Before
	public void setUpAST() {
		lineCounter = 0;
		setParentVisitor = new SetParentVisitor();
		setParentVisitor.setStopOnFirstError(true);
		declarationVisitor = new DeclarationVisitor();
		declarationVisitor.setStopOnFirstError(true);

		// module
		moduleImport01 = new Import(buildPosition(), new ResolvableIdentifier("Dummy"));
		moduleImport02 = new Import(buildPosition(), new ResolvableIdentifier("Dummy"));
		moduleImports = new ArrayList<Import>();
		moduleBlock = new Block(buildPosition());
		aPackage = new Package(new Identifier(""));
		moduleDeclaration =
		        new ModuleDeclaration(buildPosition(), new Identifier("moduleDeclaration"), moduleBlock, moduleImports);
		aPackage.addModule(moduleDeclaration);

		// module class
		classBlock = new Block(buildPosition());
		List<ResolvableIdentifier> classDeclSuperClasses = new ArrayList<ResolvableIdentifier>();
		classDeclaration =
		        new ClassDeclaration(buildPosition(), new Identifier("classDeclaration"), classDeclSuperClasses,
		                classBlock);
		classVariableDeclaration =
		        new VariableDeclaration(buildPosition(), new Identifier("classVariableDeclaration"),
		                new ResolvableIdentifier("String"), VariableDeclaration.DeclarationType.ATTRIBUTE);

		classFunctionBlock = new Block(buildPosition());
		classFunctionParams = new ArrayList<VariableDeclaration>();
		classFunctionDeclaration =
		        new FunctionDeclaration(buildPosition(), new Identifier("classFunctionDeclaration"),
		                classFunctionBlock, classFunctionParams, new ResolvableIdentifier("String"));

		classProcedureBlock = new Block(buildPosition());
		classProcedureParams = new ArrayList<VariableDeclaration>();
		classProcedureDeclaration =
		        new ProcedureDeclaration(buildPosition(), new Identifier("classProcedureDeclaration"),
		                classProcedureBlock, classProcedureParams);

		// module class extended
		extendedClassBlock = new Block(buildPosition());
		List<ResolvableIdentifier> extendedClassDeclSuperClasses = new ArrayList<ResolvableIdentifier>();
		extendedClassDeclSuperClasses.add(new ResolvableIdentifier("classDeclaration"));
		extendedClassDeclaration =
		        new ClassDeclaration(buildPosition(), new Identifier("extendedClassDeclaration"),
		                extendedClassDeclSuperClasses, extendedClassBlock);

		// module function
		moduleFunctionBlock = new Block(buildPosition());
		moduleFunctionParamsVariable01 =
		        new VariableDeclaration(buildPosition(), new Identifier("moduleFunctionParamsVariable01"),
		                new ResolvableIdentifier("String"), VariableDeclaration.DeclarationType.PARAMETER);
		moduleFunctionParamsVariable02 =
		        new VariableDeclaration(buildPosition(), new Identifier("moduleFunctionParamsVariable02"),
		                new ResolvableIdentifier("String"), VariableDeclaration.DeclarationType.PARAMETER);
		moduleFunctionParams = new ArrayList<VariableDeclaration>();
		moduleFunctionDeclaration =
		        new FunctionDeclaration(buildPosition(), new Identifier("moduleFunctionDeclaration"),
		                moduleFunctionBlock, moduleFunctionParams, new ResolvableIdentifier("String"));
		moduleFunctionVariableDeclaration =
		        new VariableDeclaration(buildPosition(), new Identifier("moduleFunctionVariableDeclaration"),
		                new ResolvableIdentifier("String"), VariableDeclaration.DeclarationType.VARIABLE);
		moduleFunctionFunctionBlock = new Block(buildPosition());
		moduleFunctionFunctionParams = new ArrayList<VariableDeclaration>();
		moduleFunctionFunctionDeclaration =
		        new FunctionDeclaration(buildPosition(), new Identifier("moduleFunctionFunctionDeclaration"),
		                moduleFunctionFunctionBlock, moduleFunctionFunctionParams, new ResolvableIdentifier("String"));
		moduleFunctionProcedureBlock = new Block(buildPosition());
		moduleFunctionProcedureParams = new ArrayList<VariableDeclaration>();
		moduleFunctionProcedureDeclaration =
		        new ProcedureDeclaration(buildPosition(), new Identifier("moduleFunctionProcedureDeclaration"),
		                moduleFunctionProcedureBlock, moduleFunctionProcedureParams);

		// module procedure
		moduleProcedureBlock = new Block(buildPosition());
		moduleProcedureParamsVariable01 =
		        new VariableDeclaration(buildPosition(), new Identifier("moduleProcedureParamsVariable01"),
		                new ResolvableIdentifier("String"), VariableDeclaration.DeclarationType.PARAMETER);
		moduleProcedureParamsVariable02 =
		        new VariableDeclaration(buildPosition(), new Identifier("moduleProcedureParamsVariable02"),
		                new ResolvableIdentifier("String"), VariableDeclaration.DeclarationType.PARAMETER);
		moduleProcedureParams = new ArrayList<VariableDeclaration>();
		moduleProcedureDeclaration =
		        new ProcedureDeclaration(buildPosition(), new Identifier("moduleProcedureDeclaration"),
		                moduleProcedureBlock, moduleProcedureParams);
		moduleProcedureVariableDeclaration =
		        new VariableDeclaration(buildPosition(), new Identifier("moduleProcedureVariableDeclaration"),
		                new ResolvableIdentifier("String"), VariableDeclaration.DeclarationType.VARIABLE);
		moduleProcedureFunctionBlock = new Block(buildPosition());
		moduleProcedureFunctionParams = new ArrayList<VariableDeclaration>();
		moduleProcedureFunctionDeclaration =
		        new FunctionDeclaration(buildPosition(), new Identifier("moduleProcedureFunctionDeclaration"),
		                moduleProcedureFunctionBlock, moduleProcedureFunctionParams, new ResolvableIdentifier("String"));
		moduleProcedureProcedureBlock = new Block(buildPosition());
		moduleProcedureProcedureParams = new ArrayList<VariableDeclaration>();
		moduleProcedureProcedureDeclaration =
		        new ProcedureDeclaration(buildPosition(), new Identifier("moduleProcedureProcedureDeclaration"),
		                moduleProcedureProcedureBlock, moduleProcedureProcedureParams);

		// module variable
		moduleVariableDeclaration =
		        new VariableDeclaration(buildPosition(), new Identifier("moduleVariableDeclaration"),
		                new ResolvableIdentifier("String"), VariableDeclaration.DeclarationType.VARIABLE);

		// module loop
		moduleLoopCondition = new BooleanLiteral(buildPosition(), true);
		moduleLoopBlock = new Block(buildPosition());
		moduleLoop = new WhileLoop(buildPosition(), moduleLoopCondition, moduleLoopBlock);
		moduleLoopContinueStatement = new ContinueStatement(buildPosition());
		moduleLoopBreakStatement = new BreakStatement(buildPosition());

		// module conditional
		moduleConditionalStatementCondition = new BooleanLiteral(buildPosition(), true);
		moduleConditionalStatementThenBlock = new Block(buildPosition());
		moduleConditionalStatementElseBlock = new Block(buildPosition());
		moduleConditionalStatement =
		        new ConditionalStatement(buildPosition(), moduleConditionalStatementCondition,
		                moduleConditionalStatementThenBlock, moduleConditionalStatementElseBlock);

		// module function return statement
		moduleReturnStatementParameter = new StringLiteral(buildPosition(), "42");
		moduleReturnStatement = new ReturnStatement(buildPosition(), moduleReturnStatementParameter);

		// module procedure call
		moduleProcedureCallFunctionCallParameter01 = new StringLiteral(buildPosition(), "42");
		moduleProcedureCallFunctionCallParameter02 = new StringLiteral(buildPosition(), "42");
		moduleProcedureCallFunctionCallParameters = new ArrayList<Expression>();

		moduleProcedureCall =
		        new FunctionCall(buildPosition(), new ResolvableIdentifier("moduleProcedureDeclaration"),
		                moduleProcedureCallFunctionCallParameters);

		// module assignment with function call as left side
		moduleAssignmentMemberAccessVariableDeclaration =
		        new VariableDeclaration(buildPosition(), new Identifier(
		                "moduleAssignmentMemberAccessVariableDeclaration"),
		                new ResolvableIdentifier("classDeclaration"), VariableDeclaration.DeclarationType.VARIABLE);
		moduleAssignmentMemberAccessVariableAccessClass =
		        new VariableAccess(buildPosition(), new ResolvableIdentifier(
		                "moduleAssignmentMemberAccessVariableDeclaration"));
		moduleAssignmentMemberAccessVariableAccessVariable =
		        new VariableAccess(buildPosition(), new ResolvableIdentifier("classVariableDeclaration"));
		moduleAssignmentMemberAccess =
		        new MemberAccess(buildPosition(), moduleAssignmentMemberAccessVariableAccessClass,
		                moduleAssignmentMemberAccessVariableAccessVariable);
		moduleAssignmentFunctionCallParameter01 = new StringLiteral(buildPosition(), "42");
		moduleAssignmentFunctionCallParameter02 = new StringLiteral(buildPosition(), "42");
		moduleAssignmentFunctionCallParameters = new ArrayList<Expression>();
		moduleAssignmentFunctionCall =
		        new FunctionCall(buildPosition(), new ResolvableIdentifier("moduleFunctionDeclaration"),
		                moduleAssignmentFunctionCallParameters);
		moduleAssignment = new Assignment(buildPosition(), moduleAssignmentMemberAccess, moduleAssignmentFunctionCall);

		// invalid modul
		invalidModuleImport01 = new Import(buildPosition(), new ResolvableIdentifier("Dummy"));
		invalidModuleImport02 = new Import(buildPosition(), new ResolvableIdentifier("Dummy"));
		invalidModuleImports = new ArrayList<Import>();
		invalidModuleBlock = new Block(buildPosition());
		invalidModuleDeclaration =
		        new ModuleDeclaration(buildPosition(), new Identifier("invalidModuleDeclaration"), invalidModuleBlock,
		                invalidModuleImports);

		// invalid class
		invalidClassBlock = new Block(buildPosition());
		invalidClassSuperClasses = new ArrayList<ResolvableIdentifier>();
		invalidClassDeclaration =
		        new ClassDeclaration(buildPosition(), new Identifier("invalidClass"), invalidClassSuperClasses,
		                invalidClassBlock);

	}

	private void fillASTModule() {
		moduleImports.add(moduleImport01);
		moduleImports.add(moduleImport02);
	}

	private void fillASTClass() {
		moduleBlock.addDeclaration(classDeclaration);
	}

	private void fillASTClassVariable() {
		classBlock.addDeclaration(classVariableDeclaration);
	}

	private void fillASTClassFunction() {
		classBlock.addDeclaration(classFunctionDeclaration);
	}

	private void fillASTClassProcedure() {
		classBlock.addDeclaration(classProcedureDeclaration);
	}

	private void fillASTExtendedClass() {
		moduleBlock.addDeclaration(extendedClassDeclaration);
	}

	private void fillASTModuleFunction() {
		moduleBlock.addDeclaration(moduleFunctionDeclaration);
	}

	private void fillASTModuleFunctionVariable() {
		moduleFunctionParams.add(moduleFunctionParamsVariable01);
		moduleFunctionParams.add(moduleFunctionParamsVariable02);
		moduleFunctionBlock.addDeclaration(moduleFunctionVariableDeclaration);
	}

	private void fillASTModuleFunctionFunction() {
		moduleFunctionBlock.addDeclaration(moduleFunctionFunctionDeclaration);
	}

	private void fillASTModuleFunctionProcedure() {
		moduleFunctionBlock.addDeclaration(moduleFunctionProcedureDeclaration);
	}

	private void fillASTModuleProcedure() {
		moduleBlock.addDeclaration(moduleProcedureDeclaration);
	}

	private void fillASTModuleProcedureVariable() {
		moduleProcedureParams.add(moduleProcedureParamsVariable01);
		moduleProcedureParams.add(moduleProcedureParamsVariable02);
		moduleProcedureBlock.addDeclaration(moduleProcedureVariableDeclaration);
	}

	private void fillASTModuleProcedureFunction() {
		moduleProcedureBlock.addDeclaration(moduleProcedureFunctionDeclaration);
	}

	private void fillASTModuleProcedureProcedure() {
		moduleProcedureBlock.addDeclaration(moduleProcedureProcedureDeclaration);
	}

	private void fillASTModuleVariable() {
		moduleBlock.addDeclaration(moduleVariableDeclaration);
	}

	private void fillASTModuleStatements() {
		moduleLoopBlock.addStatement(moduleLoopContinueStatement);
		moduleLoopBlock.addStatement(moduleLoopBreakStatement);

		moduleBlock.addDeclaration(moduleAssignmentMemberAccessVariableDeclaration);
		moduleBlock.addStatement(moduleLoop);
		moduleBlock.addStatement(moduleConditionalStatement);
		moduleBlock.addStatement(moduleReturnStatement);

		moduleProcedureCallFunctionCallParameters.add(moduleProcedureCallFunctionCallParameter01);
		moduleProcedureCallFunctionCallParameters.add(moduleProcedureCallFunctionCallParameter02);
		moduleBlock.addStatement(moduleProcedureCall);

		moduleAssignmentFunctionCallParameters.add(moduleAssignmentFunctionCallParameter01);
		moduleAssignmentFunctionCallParameters.add(moduleAssignmentFunctionCallParameter02);
		moduleBlock.addStatement(moduleAssignment);
	}

	private void fillASTInvalidModule() {
		invalidModuleImports.add(invalidModuleImport01);
		invalidModuleImports.add(invalidModuleImport02);
	}

	@Test
	public void setUpASTTest() {
		assertNotNull("setParentVisitor is null", setParentVisitor);
		assertNotNull("declarationVisitor is null", declarationVisitor);

		assertNotNull("moduleImport01 is null", moduleImport01);
		assertNotNull("moduleImport02 is null", moduleImport02);
		assertNotNull("moduleImports is null", moduleImports);
		assertNotNull("moduleBlock is null", moduleBlock);
		assertNotNull("moduleDeclaration is null", moduleDeclaration);
		assertNotNull("package is null", aPackage);

		assertNotNull("classDeclarationDeclarations is null", classBlock);
		assertNotNull("classDeclaration is null", classDeclaration);
		assertNotNull("classVariableDeclaration is null", classVariableDeclaration);

		assertNotNull("classFunctionBlock is null", classFunctionBlock);
		assertNotNull("classFunctionParams is null", classFunctionParams);
		assertNotNull("classFunctionDeclaration is null", classFunctionDeclaration);

		assertNotNull("classProcedureBlock is null", classProcedureBlock);
		assertNotNull("classProcedureParams is null", classProcedureParams);
		assertNotNull("classProcedureDeclaration is null", classProcedureDeclaration);

		assertNotNull("extendedClassDeclarationDeclarations is null", extendedClassBlock);
		assertNotNull("extendedClassDeclaration is null", extendedClassDeclaration);

		assertNotNull("moduleFunctionBlock is null", moduleFunctionBlock);
		assertNotNull("moduleFunctionParamsVariable01 is null", moduleFunctionParamsVariable01);
		assertNotNull("moduleFunctionParamsVariable02 is null", moduleFunctionParamsVariable02);
		assertNotNull("moduleFunctionParams is null", moduleFunctionParams);
		assertNotNull("moduleFunctionDeclaration is null", moduleFunctionDeclaration);
		assertNotNull("moduleFunctionVariableDeclaration is null", moduleFunctionVariableDeclaration);
		assertNotNull("moduleFunctionFunctionBlock is null", moduleFunctionFunctionBlock);
		assertNotNull("moduleFunctionFunctionParams is null", moduleFunctionFunctionParams);
		assertNotNull("moduleFunctionFunctionDeclaration is null", moduleFunctionFunctionDeclaration);
		assertNotNull("moduleFunctionProcedureBlock is null", moduleFunctionProcedureBlock);
		assertNotNull("moduleFunctionProcedureParams is null", moduleFunctionProcedureParams);
		assertNotNull("moduleFunctionProcedureDeclaration is null", moduleFunctionProcedureDeclaration);

		assertNotNull("moduleProcedureBlock is null", moduleProcedureBlock);
		assertNotNull("moduleProcedureParamsVariable01 is null", moduleProcedureParamsVariable01);
		assertNotNull("moduleProcedureParamsVariable02 is null", moduleProcedureParamsVariable02);
		assertNotNull("moduleProcedureParams is null", moduleProcedureParams);
		assertNotNull("moduleProcedureDeclaration is null", moduleProcedureDeclaration);
		assertNotNull("moduleProcedureVariableDeclaration is null", moduleProcedureVariableDeclaration);
		assertNotNull("moduleProcedureFunctionBlock is null", moduleProcedureFunctionBlock);
		assertNotNull("moduleProcedureFunctionParams is null", moduleProcedureFunctionParams);
		assertNotNull("moduleProcedureFunctionDeclaration is null", moduleProcedureFunctionDeclaration);
		assertNotNull("moduleProcedureProcedureBlock is null", moduleProcedureProcedureBlock);
		assertNotNull("moduleProcedureProcedureParams is null", moduleProcedureProcedureParams);
		assertNotNull("moduleProcedureProcedureDeclaration is null", moduleProcedureProcedureDeclaration);

		assertNotNull("moduleVariableDeclaration is null", moduleVariableDeclaration);

		assertNotNull("moduleLoopCondition is null", moduleLoopCondition);
		assertNotNull("moduleLoopBlock is null", moduleLoopBlock);
		assertNotNull("moduleLoop is null", moduleLoop);
		assertNotNull("moduleLoopContinueStatement is null", moduleLoopContinueStatement);
		assertNotNull("moduleLoopBreakStatement is null", moduleLoopBreakStatement);

		assertNotNull("moduleConditionalStatementCondition is null", moduleConditionalStatementCondition);
		assertNotNull("moduleConditionalStatement is null", moduleConditionalStatement);
		assertNotNull("moduleConditionalStatementThenBlock is null", moduleConditionalStatementThenBlock);
		assertNotNull("moduleConditionalStatementElseBlock is null", moduleConditionalStatementElseBlock);

		assertNotNull("moduleReturnStatementParameter is null", moduleReturnStatementParameter);
		assertNotNull("moduleReturnStatement is null", moduleReturnStatement);

		assertNotNull("moduleProcedureCallFunctionCallParameter01 is null", moduleProcedureCallFunctionCallParameter01);
		assertNotNull("moduleProcedureCallFunctionCallParameter02 is null", moduleProcedureCallFunctionCallParameter02);
		assertNotNull("moduleProcedureCallFunctionCallParameters is null", moduleProcedureCallFunctionCallParameters);
		assertNotNull("moduleProcedureCall is null", moduleProcedureCall);

		assertNotNull(
		        "moduleAssignmentMemberAccessVariableDeclaration is null",
		        moduleAssignmentMemberAccessVariableDeclaration);
		assertNotNull(
		        "moduleAssignmentMemberAccessVariableAccessClass is null",
		        moduleAssignmentMemberAccessVariableAccessClass);
		assertNotNull(
		        "moduleAssignmentMemberAccessVariableAccessVariable is null",
		        moduleAssignmentMemberAccessVariableAccessVariable);
		assertNotNull("moduleAssignmentMemberAccess is null", moduleAssignmentMemberAccess);
		assertNotNull("moduleAssignmentFunctionCallParameter01 is null", moduleAssignmentFunctionCallParameter01);
		assertNotNull("moduleAssignmentFunctionCallParameter02 is null", moduleAssignmentFunctionCallParameter02);
		assertNotNull("moduleAssignmentFunctionCallParameters is null", moduleAssignmentFunctionCallParameters);
		assertNotNull("moduleAssignmentFunctionCall is null", moduleAssignmentFunctionCall);
		assertNotNull("moduleAssignment is null", moduleAssignment);

		assertNotNull("invalidModuleImport01 is null", invalidModuleImport01);
		assertNotNull("invalidModuleImport02 is null", invalidModuleImport02);
		assertNotNull("invalidModuleImports is null", invalidModuleImports);
		assertNotNull("invalidModuleBlock is null", invalidModuleBlock);
		assertNotNull("invalidModuleDeclaration is null", invalidModuleDeclaration);

		assertNotNull("invalidClassSuperClasses is null", invalidClassSuperClasses);
		assertNotNull("invalidClassDeclarations is null", invalidClassBlock);
		assertNotNull("invalidClassDeclaration is null", invalidClassDeclaration);
	}

	// DECLARATION AND SCOPE

	// ONE CASE TO TEST 'EM ALL

	@Test
	public void nodeInteractionTest() {
		// Es muss einen Test geben, in dem alle AST-Nodes vorkommen. Nach
		// Durchlauf des DeclarationVisitors müssen alle AST-Nodes einen Scope
		// haben. Jede AST-Node, die mehr als ein Kind haben kann, muss für
		// jeden möglichen Kind-Typ mehr als 1 Kind besitzen. (z.B. Modulblock
		// mit 2 Statements und 2 Declarations)

		fillASTModule();
		fillASTExtendedClass();
		fillASTClass();
		fillASTClassVariable();
		fillASTClassFunction();
		fillASTClassProcedure();
		fillASTModuleFunction();
		fillASTModuleFunctionVariable();
		fillASTModuleFunctionFunction();
		fillASTModuleFunctionProcedure();
		fillASTModuleProcedure();
		fillASTModuleProcedureVariable();
		fillASTModuleProcedureFunction();
		fillASTModuleProcedureProcedure();
		fillASTModuleVariable();
		fillASTModuleStatements();

		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);

		ASTNode[] allASTNodes =
		        new ASTNode[] { moduleImport01, moduleImport02, moduleBlock, aPackage, classDeclaration,
		                classVariableDeclaration, classFunctionBlock, classFunctionDeclaration, classProcedureBlock,
		                classProcedureDeclaration, extendedClassDeclaration, moduleFunctionBlock,
		                moduleFunctionParamsVariable01, moduleFunctionParamsVariable02, moduleFunctionDeclaration,
		                moduleFunctionVariableDeclaration, moduleFunctionFunctionBlock,
		                moduleFunctionFunctionDeclaration, moduleFunctionProcedureBlock,
		                moduleFunctionProcedureDeclaration, moduleProcedureBlock, moduleProcedureParamsVariable01,
		                moduleProcedureParamsVariable02, moduleProcedureDeclaration,
		                moduleProcedureVariableDeclaration, moduleProcedureFunctionBlock,
		                moduleProcedureFunctionDeclaration, moduleProcedureProcedureBlock,
		                moduleProcedureProcedureDeclaration, moduleVariableDeclaration, moduleLoopCondition,
		                moduleLoopBlock, moduleLoop, moduleLoopContinueStatement, moduleLoopBreakStatement,
		                moduleConditionalStatementCondition, moduleConditionalStatement,
		                moduleConditionalStatementThenBlock, moduleConditionalStatementElseBlock,
		                moduleReturnStatementParameter, moduleReturnStatement,
		                moduleProcedureCallFunctionCallParameter01, moduleProcedureCallFunctionCallParameter02,
		                moduleProcedureCall, moduleAssignmentMemberAccessVariableDeclaration,
		                moduleAssignmentMemberAccessVariableAccessClass,
		                moduleAssignmentMemberAccessVariableAccessVariable, moduleAssignmentMemberAccess,
		                moduleAssignmentFunctionCallParameter01, moduleAssignmentFunctionCallParameter02,
		                moduleAssignmentFunctionCall, moduleAssignment, moduleDeclaration };

		for (ASTNode node : allASTNodes) {
			assertNotNull("Scope not set for class: " + node.getClass().getSimpleName(), node.getScope());

			if (node instanceof ClassDeclaration) {
				assertTrue(
				        "Scope for ClassDeclaration is no ClassScope: " + node.getClass().getSimpleName(),
				        node.getScope() instanceof ClassScope);
			}

			if (!(node instanceof Package)) {
				assertNotNull("Parent not set for class: " + node.getClass().getSimpleName(), node.getParentNode());
			} else {
				assertNull(
				        "Parent set for class (but should be null): " + node.getClass().getSimpleName(),
				        node.getParentNode());
			}
		}
	}

	// Module Declaration Tests

	@Test
	public void moduleTestModuleDeclaration() {
		fillASTModule();

		// there is no way of validating the correct declaration but to fail
		// if an exception occurs.
		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
	}

	@Test(expected = InvalidPlaceToDeclareException.class)
	public void moduleTestModuleDeclarationInModule() {
		fillASTModule();
		fillASTInvalidModule();

		moduleBlock.addDeclaration(invalidModuleDeclaration);
		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
	}

	@Test(expected = InvalidPlaceToDeclareException.class)
	public void moduleTestModuleDeclarationInClass() {
		fillASTModule();
		fillASTClass();
		fillASTInvalidModule();

		classBlock.addDeclaration(invalidModuleDeclaration);
		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
	}

	@Test(expected = InvalidPlaceToDeclareException.class)
	public void moduleTestModuleDeclarationInFunction() {
		fillASTModule();
		fillASTModuleFunction();
		fillASTInvalidModule();

		moduleFunctionBlock.addDeclaration(invalidModuleDeclaration);
		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
	}

	@Test(expected = InvalidPlaceToDeclareException.class)
	public void moduleTestModuleDeclarationInProcedure() {
		fillASTModule();
		fillASTModuleProcedure();
		fillASTInvalidModule();

		moduleProcedureBlock.addDeclaration(invalidModuleDeclaration);
		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
	}

	// Class Declaration Tests

	@Test
	public void classTestClassDeclarationInModule() {
		fillASTModule();
		fillASTClass();

		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
		assertSame(classDeclaration, moduleDeclaration.getScope().resolve(null, new ResolvableIdentifier("classDeclaration")));
	}

	@Test(expected = InvalidPlaceToDeclareException.class)
	public void classTestClassDeclarationInClass() {
		fillASTModule();
		fillASTClass();

		classBlock.addDeclaration(invalidClassDeclaration);
		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
	}

	@Test(expected = InvalidPlaceToDeclareException.class)
	public void classTestClassDeclarationInFunction() {
		fillASTModule();
		fillASTModuleFunction();

		moduleFunctionBlock.addDeclaration(invalidClassDeclaration);
		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
	}

	@Test(expected = InvalidPlaceToDeclareException.class)
	public void classTestClassDeclarationInProcedure() {
		fillASTModule();
		fillASTModuleProcedure();

		moduleProcedureBlock.addDeclaration(invalidClassDeclaration);
		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
	}

	// Function Declaration Tests

	@Test
	public void functionTestFunctionDeclarationInModule() {
		fillASTModule();
		fillASTModuleFunction();

		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
		assertTrue(moduleDeclaration.getScope().resolveProcedure(null, new ResolvableIdentifier("moduleFunctionDeclaration")).contains(
		        moduleFunctionDeclaration));
	}

	@Test
	public void functionTestFunctionDeclarationInClass() {
		fillASTModule();
		fillASTClass();
		fillASTClassFunction();

		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
		assertTrue(classDeclaration.getScope().resolveProcedure(null, new ResolvableIdentifier("classFunctionDeclaration")).contains(
		        classFunctionDeclaration));
	}

	@Test
	public void functionTestFunctionDeclarationInFunction() {
		fillASTModule();
		fillASTModuleFunction();
		fillASTModuleFunctionFunction();

		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
		assertTrue(moduleFunctionDeclaration.getScope().resolveProcedure(null,
		        new ResolvableIdentifier("moduleFunctionFunctionDeclaration")).contains(
		        moduleFunctionFunctionDeclaration));
	}

	@Test
	public void functionTestFunctionDeclarationInProcedure() {
		fillASTModule();
		fillASTModuleProcedure();
		fillASTModuleProcedureFunction();

		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
		assertTrue(moduleProcedureDeclaration.getScope().resolveProcedure(null,
		        new ResolvableIdentifier("moduleProcedureFunctionDeclaration")).contains(
		        moduleProcedureFunctionDeclaration));
	}

	// Procedure Declaration Tests

	@Test
	public void procedureTestProcedureDeclarationInModule() {
		fillASTModule();
		fillASTModuleProcedure();

		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
		assertTrue(moduleDeclaration.getScope().resolveProcedure(null, new ResolvableIdentifier("moduleProcedureDeclaration")).contains(
		        moduleProcedureDeclaration));
	}

	@Test
	public void procedureTestProcedureDeclarationInClass() {
		fillASTModule();
		fillASTClass();
		fillASTClassProcedure();

		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
		assertTrue(classDeclaration.getScope().resolveProcedure(null, new ResolvableIdentifier("classProcedureDeclaration")).contains(
		        classProcedureDeclaration));
	}

	@Test
	public void procedureTestProcedureDeclarationInFunction() {
		fillASTModule();
		fillASTModuleFunction();
		fillASTModuleFunctionProcedure();

		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
		assertTrue(moduleFunctionDeclaration.getScope().resolveProcedure(null,
		        new ResolvableIdentifier("moduleFunctionProcedureDeclaration")).contains(
		        moduleFunctionProcedureDeclaration));
	}

	@Test
	public void procedureTestProcedureDeclarationInProcedure() {
		fillASTModule();
		fillASTModuleProcedure();
		fillASTModuleProcedureProcedure();

		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
		assertTrue(moduleProcedureDeclaration.getScope().resolveProcedure(null,
		        new ResolvableIdentifier("moduleProcedureProcedureDeclaration")).contains(
		        moduleProcedureProcedureDeclaration));
	}

	// Variable Declaration Tests

	@Test
	public void variableTestVariableDeclarationInModule() {
		fillASTModule();
		fillASTModuleVariable();

		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
		assertSame(
		        moduleVariableDeclaration,
		        moduleDeclaration.getScope().resolve(null, new ResolvableIdentifier("moduleVariableDeclaration")));
	}

	@Test
	public void variableTestVariableDeclarationInClass() {
		fillASTModule();
		fillASTClass();
		fillASTClassVariable();

		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
		assertSame(
		        classVariableDeclaration,
		        classDeclaration.getScope().resolve(null, new ResolvableIdentifier("classVariableDeclaration")));
	}

	@Test
	public void variableTestVariableDeclarationInFunction() {
		fillASTModule();
		fillASTModuleFunction();
		fillASTModuleFunctionVariable();

		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
		Scope scope = moduleFunctionDeclaration.getScope();

		assertSame(
		        moduleFunctionParamsVariable01,
		        scope.resolve(null, new ResolvableIdentifier("moduleFunctionParamsVariable01")));
		assertSame(
		        moduleFunctionParamsVariable02,
		        scope.resolve(null, new ResolvableIdentifier("moduleFunctionParamsVariable02")));
		assertSame(
		        moduleFunctionVariableDeclaration,
		        scope.resolve(null, new ResolvableIdentifier("moduleFunctionVariableDeclaration")));
	}

	@Test
	public void variableTestVariableDeclarationInProcedure() {
		fillASTModule();
		fillASTModuleProcedure();
		fillASTModuleProcedureVariable();

		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
		Scope scope = moduleProcedureDeclaration.getScope();

		assertSame(
		        moduleProcedureParamsVariable01,
		        scope.resolve(null, new ResolvableIdentifier("moduleProcedureParamsVariable01")));
		assertSame(
		        moduleProcedureParamsVariable02,
		        scope.resolve(null, new ResolvableIdentifier("moduleProcedureParamsVariable02")));
		assertSame(
		        moduleProcedureVariableDeclaration,
		        scope.resolve(null, new ResolvableIdentifier("moduleProcedureVariableDeclaration")));
	}

	@Test
	public void variableTestResolveVariableInNestedScope() {
		fillASTModule();
		fillASTModuleVariable();
		fillASTClass();
		fillASTClassVariable();
		fillASTClassFunction();

		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
		Scope scope = classFunctionDeclaration.getScope();
		assertSame(classVariableDeclaration, scope.resolve(null, new ResolvableIdentifier("classVariableDeclaration")));
		assertSame(moduleVariableDeclaration, scope.resolve(null, new ResolvableIdentifier("moduleVariableDeclaration")));
	}

	// REDECLARATION

	@Test(expected = RedeclarationException.class)
	public void redeclarationTestRedeclarationInSameScopeTest() {
		fillASTModule();
		fillASTModuleVariable();

		VariableDeclaration variableDeclaration =
		        new VariableDeclaration(buildPosition(), new Identifier("moduleVariableDeclaration"),
		                new ResolvableIdentifier("String"), VariableDeclaration.DeclarationType.VARIABLE);
		moduleBlock.addDeclaration(variableDeclaration);

		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
	}

	@Test(expected = RedeclarationException.class)
	public void redeclarationTestRedeclarationInSameClassScopeTest() {
		fillASTModule();
		fillASTClass();
		fillASTClassVariable();

		VariableDeclaration variableDeclaration =
		        new VariableDeclaration(buildPosition(), new Identifier("classVariableDeclaration"),
		                new ResolvableIdentifier("String"), VariableDeclaration.DeclarationType.ATTRIBUTE);
		classBlock.addDeclaration(variableDeclaration);

		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
	}

	@Test
	public void redeclarationTestRedeclarationInDifferentScopes() {
		fillASTModule();
		fillASTModuleVariable();
		fillASTModuleFunction();

		VariableDeclaration variableDeclaration =
		        new VariableDeclaration(buildPosition(), new Identifier("moduleVariableDeclaration"),
		                new ResolvableIdentifier("String"), VariableDeclaration.DeclarationType.VARIABLE);
		moduleFunctionBlock.addDeclaration(variableDeclaration);

		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
		assertSame(
		        variableDeclaration,
		        moduleFunctionDeclaration.getScope().resolve(null, new ResolvableIdentifier("moduleVariableDeclaration")));
		assertNotSame(
		        variableDeclaration,
		        moduleDeclaration.getScope().resolve(null, new ResolvableIdentifier("moduleVariableDeclaration")));
	}

	@Test
	public void redeclarationTestRedeclarationInExtendedClassScope() {
		fillASTModule();
		fillASTExtendedClass();
		fillASTClass();
		fillASTClassVariable();

		VariableDeclaration variableDeclaration =
		        new VariableDeclaration(buildPosition(), new Identifier("classVariableDeclaration"),
		                new ResolvableIdentifier("String"), VariableDeclaration.DeclarationType.ATTRIBUTE);
		extendedClassBlock.addDeclaration(variableDeclaration);

		setParentVisitor.visit(aPackage);
		declarationVisitor.visit(aPackage);
		assertSame(
		        variableDeclaration,
		        extendedClassDeclaration.getScope().resolve(null, new ResolvableIdentifier("classVariableDeclaration")));
		assertNotSame(
		        variableDeclaration,
		        classDeclaration.getScope().resolve(null, new ResolvableIdentifier("classVariableDeclaration")));
	}
}
