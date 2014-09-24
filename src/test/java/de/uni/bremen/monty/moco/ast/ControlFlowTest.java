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

import de.uni.bremen.monty.moco.ast.declaration.FunctionDeclaration;
import de.uni.bremen.monty.moco.ast.declaration.ModuleDeclaration;
import de.uni.bremen.monty.moco.ast.declaration.ProcedureDeclaration;
import de.uni.bremen.monty.moco.ast.declaration.VariableDeclaration;
import de.uni.bremen.monty.moco.ast.expression.Expression;
import de.uni.bremen.monty.moco.ast.expression.literal.BooleanLiteral;
import de.uni.bremen.monty.moco.ast.expression.literal.StringLiteral;
import de.uni.bremen.monty.moco.ast.statement.*;
import de.uni.bremen.monty.moco.exception.InvalidControlFlowException;
import de.uni.bremen.monty.moco.visitor.ControlFlowVisitor;
import de.uni.bremen.monty.moco.visitor.DeclarationVisitor;
import de.uni.bremen.monty.moco.visitor.SetParentVisitor;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;

public class ControlFlowTest {

	private SetParentVisitor setParentVisitor;
	private ControlFlowVisitor controlFlowVisitor;

	// MODULE
	private Block moduleBlock;
	private List<Import> moduleImports;
	private Package aPackage;
	private ModuleDeclaration moduleDeclaration;

	// MODULE LOOP
	private Expression moduleLoopCondition;
	private Block moduleLoopBlock;
	private WhileLoop moduleLoop;
	private ContinueStatement moduleLoopContinueStatement;
	private BreakStatement moduleLoopBreakStatement;
	private Expression moduleLoopLoopCondition;
	private Block moduleLoopLoopBlock;

	// MODULE LOOP LOOP
	private WhileLoop moduleLoopLoop;
	private ContinueStatement moduleLoopLoopContinueStatement;
	private BreakStatement moduleLoopLoopBreakStatement;
	private Expression moduleLoopLoopConditionalStatementCondition;
	private Block moduleLoopLoopConditionalStatementThenBlock;
	private Block moduleLoopLoopConditionalStatementElseBlock;
	private ConditionalStatement moduleLoopLoopConditionalStatement;
	private Expression moduleLoopLoopThenConditionalStatementCondition;
	private Block moduleLoopLoopThenConditionalStatementThenBlock;
	private Block moduleLoopLoopThenConditionalStatementElseBlock;
	private ConditionalStatement moduleLoopLoopThenConditionalStatement;
	private Expression moduleLoopLoopElseConditionalStatementCondition;
	private Block moduleLoopLoopElseConditionalStatementThenBlock;
	private Block moduleLoopLoopElseConditionalStatementElseBlock;
	private ConditionalStatement moduleLoopLoopElseConditionalStatement;

	// MODULE LOOP CONDITIONAL STATEMENT
	private Expression moduleLoopConditionalStatementCondition;
	private Block moduleLoopConditionalStatementThenBlock;
	private Block moduleLoopConditionalStatementElseBlock;
	private ConditionalStatement moduleLoopConditionalStatement;
	private ContinueStatement moduleLoopConditionalContinueStatement;
	private BreakStatement moduleLoopConditionalBreakStatement;

	// MODULE CONDITIONAL STATEMENT
	private Expression moduleConditionalStatementCondition;
	private Block moduleConditionalStatementThenBlock;
	private Block moduleConditionalStatementElseBlock;
	private ConditionalStatement moduleConditionalStatement;

	// MODULE STATETEMENTS
	private ContinueStatement moduleContinueStatement;
	private BreakStatement moduleBreakStatement;
	private Expression moduleReturnStatementParameter;
	private ReturnStatement moduleReturnStatement;

	// MODULE FUNCTION
	private Block moduleFunctionBlock;
	private List<VariableDeclaration> moduleFunctionParameter;
	private FunctionDeclaration moduleFunctionDeclaration;
	private Expression moduleFunctionReturnStatementParameter;
	private ReturnStatement moduleFunctionReturnStatement;

	// MODULE FUNCTION PROCEDURE
	private Block moduleFunctionProcedureBlock;
	private List<VariableDeclaration> moduleFunctionProcedureParameter;
	private ProcedureDeclaration moduleFunctionProcedureDeclaration;
	private Expression moduleFunctionProcedureReturnStatementParameter;
	private ReturnStatement moduleFunctionProcedureReturnStatement;

	// MODULE FUNCTION CONDITIONAL STATEMENT
	private Expression moduleFunctionConditionalStatementCondition;
	private Block moduleFunctionConditionalStatementThenBlock;
	private Block moduleFunctionConditionalStatementElseBlock;
	private ConditionalStatement moduleFunctionConditionalStatement;
	private Expression moduleFunctionConditionalReturnThenStatementParameter;
	private ReturnStatement moduleFunctionConditionalReturnThenStatement;
	private Expression moduleFunctionConditionalReturnElseStatementParameter;
	private ReturnStatement moduleFunctionConditionalReturnElseStatement;
	private Expression moduleFunctionConditionalConditionalStatementCondition;
	private Block moduleFunctionConditionalConditionalStatementThenBlock;
	private Block moduleFunctionConditionalConditionalStatementElseBlock;
	private ConditionalStatement moduleFunctionConditionalConditionalStatement;
	private Expression moduleFunctionConditionalConditionalReturnStatementParameter;
	private ReturnStatement moduleFunctionConditionalConditionalReturnStatement;
	private Expression moduleFunctionConditionalConditionalConditionalStatementCondition;
	private Block moduleFunctionConditionalConditionalConditionalStatementThenBlock;
	private Block moduleFunctionConditionalConditionalConditionalStatementElseBlock;
	private ConditionalStatement moduleFunctionConditionalConditionalConditionalStatement;
	private Expression moduleFunctionConditionalConditionalConditionalThenReturnStatementParameter;
	private ReturnStatement moduleFunctionConditionalConditionalConditionalThenReturnStatement;
	private Expression moduleFunctionConditionalConditionalConditionalElseReturnStatementParameter;
	private ReturnStatement moduleFunctionConditionalConditionalConditionalElseReturnStatement;

	// MODULE FUNCTION LOOP
	private Expression moduleFunctionLoopCondition;
	private Block moduleFunctionLoopBlock;
	private WhileLoop moduleFunctionLoop;
	private Expression moduleFunctionLoopReturnStatementParameter;
	private ReturnStatement moduleFunctionLoopReturnStatement;
	private Expression moduleFunctionLoopLoopCondition;
	private Block moduleFunctionLoopLoopBlock;
	private WhileLoop moduleFunctionLoopLoop;
	private Expression moduleFunctionLoopLoopLoopCondition;
	private Block moduleFunctionLoopLoopLoopBlock;
	private WhileLoop moduleFunctionLoopLoopLoop;
	private Expression moduleFunctionLoopLoopLoopReturnStatementParameter;
	private ReturnStatement moduleFunctionLoopLoopLoopReturnStatement;

	// helper
	private int counter = 0;

	public Position nextPosition() {
		return new Position("TestFile", counter++, 1);
	}

	@Before
	public void setUpAST() {

		// reset counter
		counter = 0;

		// VISITORS ---------------------------
		setParentVisitor = new SetParentVisitor();
		setParentVisitor.setStopOnFirstError(true);
		controlFlowVisitor = new ControlFlowVisitor();
		controlFlowVisitor.setStopOnFirstError(true);

		// AST --------------------------------

		// MODULE
		moduleBlock = new Block(nextPosition());
		moduleImports = new ArrayList<>(); // empty list is fine here ...
		moduleDeclaration =
		        new ModuleDeclaration(nextPosition(), new Identifier("TestModule"), moduleBlock, moduleImports);

		aPackage = new Package(new Identifier(""));
		aPackage.addModule(moduleDeclaration);

		// MODULE LOOP
		moduleLoopCondition = new BooleanLiteral(nextPosition(), true);
		moduleLoopBlock = new Block(nextPosition());
		moduleLoop = new WhileLoop(nextPosition(), moduleLoopCondition, moduleLoopBlock);
		moduleLoopContinueStatement = new ContinueStatement(nextPosition());
		moduleLoopBreakStatement = new BreakStatement(nextPosition());
		// add module loop to module
		moduleBlock.addStatement(moduleLoop);

		// MODULE LOOP LOOP
		moduleLoopLoopCondition = new BooleanLiteral(nextPosition(), true);
		moduleLoopLoopBlock = new Block(nextPosition());
		moduleLoopLoop = new WhileLoop(nextPosition(), moduleLoopLoopCondition, moduleLoopLoopBlock);
		moduleLoopLoopContinueStatement = new ContinueStatement(nextPosition());
		moduleLoopLoopBreakStatement = new BreakStatement(nextPosition());
		// add module loop loop to module loop
		moduleLoopBlock.addStatement(moduleLoopLoop);
		moduleLoopLoopConditionalStatementCondition = new BooleanLiteral(nextPosition(), true);
		moduleLoopLoopConditionalStatementThenBlock = new Block(nextPosition());
		moduleLoopLoopConditionalStatementElseBlock = new Block(nextPosition());
		moduleLoopLoopConditionalStatement =
		        new ConditionalStatement(nextPosition(), moduleLoopLoopConditionalStatementCondition,
		                moduleLoopLoopConditionalStatementThenBlock, moduleLoopLoopConditionalStatementElseBlock);
		moduleLoopLoopThenConditionalStatementCondition = new BooleanLiteral(nextPosition(), true);
		moduleLoopLoopThenConditionalStatementThenBlock = new Block(nextPosition());
		moduleLoopLoopThenConditionalStatementElseBlock = new Block(nextPosition());
		moduleLoopLoopThenConditionalStatement =
		        new ConditionalStatement(nextPosition(), moduleLoopLoopThenConditionalStatementCondition,
		                moduleLoopLoopThenConditionalStatementThenBlock,
		                moduleLoopLoopThenConditionalStatementElseBlock);
		moduleLoopLoopElseConditionalStatementCondition = new BooleanLiteral(nextPosition(), true);
		moduleLoopLoopElseConditionalStatementThenBlock = new Block(nextPosition());
		moduleLoopLoopElseConditionalStatementElseBlock = new Block(nextPosition());
		moduleLoopLoopElseConditionalStatement =
		        new ConditionalStatement(nextPosition(), moduleLoopLoopElseConditionalStatementCondition,
		                moduleLoopLoopElseConditionalStatementThenBlock,
		                moduleLoopLoopElseConditionalStatementElseBlock);

		// MODULE LOOP CONDITIONAL STATEMENT
		moduleLoopConditionalStatementCondition = new BooleanLiteral(nextPosition(), true);
		moduleLoopConditionalStatementThenBlock = new Block(nextPosition());
		moduleLoopConditionalStatementElseBlock = new Block(nextPosition());
		moduleLoopConditionalStatement =
		        new ConditionalStatement(nextPosition(), moduleLoopConditionalStatementCondition,
		                moduleLoopConditionalStatementThenBlock, moduleLoopConditionalStatementElseBlock);
		moduleLoopConditionalContinueStatement = new ContinueStatement(nextPosition());
		moduleLoopConditionalBreakStatement = new BreakStatement(nextPosition());
		// add module loop conditional statement to module loop
		moduleLoopBlock.addStatement(moduleLoopConditionalStatement);

		// MODULE CONDITIONAL STATEMENT
		moduleConditionalStatementCondition = new BooleanLiteral(nextPosition(), true);
		moduleConditionalStatementThenBlock = new Block(nextPosition());
		moduleConditionalStatementElseBlock = new Block(nextPosition());
		moduleConditionalStatement =
		        new ConditionalStatement(nextPosition(), moduleConditionalStatementCondition,
		                moduleConditionalStatementThenBlock, moduleConditionalStatementElseBlock);
		// add statements to module conditional statement (then)
		// add module conditional statement to module
		moduleBlock.addStatement(moduleConditionalStatement);

		// MODULE STATETEMENTS
		moduleContinueStatement = new ContinueStatement(nextPosition());
		moduleBreakStatement = new BreakStatement(nextPosition());
		moduleReturnStatementParameter = new StringLiteral(nextPosition(), "return");
		moduleReturnStatement = new ReturnStatement(nextPosition(), moduleReturnStatementParameter);
		// add statements to module
		// moduleBlock.addStatement(moduleContinueStatement);
		// moduleBlock.addStatement(moduleBreakStatement);
		// moduleBlock.addStatement(moduleReturnStatement);

		// MODULE FUNCTION
		moduleFunctionBlock = new Block(nextPosition());
		moduleFunctionParameter = new ArrayList<>();
		moduleFunctionDeclaration =
		        new FunctionDeclaration(nextPosition(), new Identifier("ModuleFunction"), moduleFunctionBlock,
		                moduleFunctionParameter, new ResolvableIdentifier("String"));
		moduleFunctionReturnStatementParameter = new StringLiteral(nextPosition(), "return");
		moduleFunctionReturnStatement = new ReturnStatement(nextPosition(), moduleFunctionReturnStatementParameter);

		// MODULE FUNCTION PROCEDURE
		moduleFunctionProcedureBlock = new Block(nextPosition());
		moduleFunctionProcedureParameter = new ArrayList<>();
		moduleFunctionProcedureDeclaration =
		        new ProcedureDeclaration(nextPosition(), new Identifier("ModuleFunctionProcedure"),
		                moduleFunctionProcedureBlock, moduleFunctionProcedureParameter);
		moduleFunctionProcedureReturnStatementParameter = new StringLiteral(nextPosition(), "return");
		moduleFunctionProcedureReturnStatement =
		        new ReturnStatement(nextPosition(), moduleFunctionProcedureReturnStatementParameter);
		// add module function procedure to module function
		// moduleFunctionBlock.addDeclaration(moduleFunctionProcedureDeclaration);

		// MODULE FUNCTION CONDITIONAL STATEMENT
		moduleFunctionConditionalStatementCondition = new BooleanLiteral(nextPosition(), false);
		moduleFunctionConditionalStatementThenBlock = new Block(nextPosition());
		moduleFunctionConditionalStatementElseBlock = new Block(nextPosition());
		moduleFunctionConditionalStatement =
		        new ConditionalStatement(nextPosition(), moduleFunctionConditionalStatementCondition,
		                moduleFunctionConditionalStatementThenBlock, moduleFunctionConditionalStatementElseBlock);
		// add module function conditional statement to module function
		moduleFunctionBlock.addStatement(moduleFunctionConditionalStatement);
		moduleFunctionConditionalReturnThenStatementParameter = new StringLiteral(nextPosition(), "return");
		moduleFunctionConditionalReturnThenStatement =
		        new ReturnStatement(nextPosition(), moduleFunctionConditionalReturnThenStatementParameter);
		moduleFunctionConditionalReturnElseStatementParameter = new StringLiteral(nextPosition(), "return");
		moduleFunctionConditionalReturnElseStatement =
		        new ReturnStatement(nextPosition(), moduleFunctionConditionalReturnElseStatementParameter);
		moduleFunctionConditionalConditionalStatementCondition = new BooleanLiteral(nextPosition(), true);
		moduleFunctionConditionalConditionalStatementThenBlock = new Block(nextPosition());
		moduleFunctionConditionalConditionalStatementElseBlock = new Block(nextPosition());
		moduleFunctionConditionalConditionalStatement =
		        new ConditionalStatement(nextPosition(), moduleFunctionConditionalConditionalStatementCondition,
		                moduleFunctionConditionalConditionalStatementThenBlock,
		                moduleFunctionConditionalConditionalStatementElseBlock);
		// add module function conditional conditional statement to module
		// function conditional else block
		moduleFunctionConditionalStatementElseBlock.addStatement(moduleFunctionConditionalConditionalStatement);
		moduleFunctionConditionalConditionalReturnStatementParameter = new StringLiteral(nextPosition(), "return");
		moduleFunctionConditionalConditionalReturnStatement =
		        new ReturnStatement(nextPosition(), moduleFunctionConditionalConditionalReturnStatementParameter);
		moduleFunctionConditionalConditionalConditionalStatementCondition =
		        new BooleanLiteral(new Position("TestFile", 37, 1), false);
		moduleFunctionConditionalConditionalConditionalStatementThenBlock = new Block(nextPosition());
		moduleFunctionConditionalConditionalConditionalStatementElseBlock = new Block(nextPosition());
		moduleFunctionConditionalConditionalConditionalStatement =
		        new ConditionalStatement(nextPosition(),
		                moduleFunctionConditionalConditionalConditionalStatementCondition,
		                moduleFunctionConditionalConditionalConditionalStatementThenBlock,
		                moduleFunctionConditionalConditionalConditionalStatementElseBlock);
		// add module function conditional conditional conditional statement to
		// module function conditional conditional then block
		moduleFunctionConditionalConditionalStatementThenBlock.addStatement(moduleFunctionConditionalConditionalConditionalStatement);
		moduleFunctionConditionalConditionalConditionalThenReturnStatementParameter =
		        new StringLiteral(nextPosition(), "return");
		moduleFunctionConditionalConditionalConditionalThenReturnStatement =
		        new ReturnStatement(nextPosition(),
		                moduleFunctionConditionalConditionalConditionalThenReturnStatementParameter);
		moduleFunctionConditionalConditionalConditionalElseReturnStatementParameter =
		        new StringLiteral(nextPosition(), "return");
		moduleFunctionConditionalConditionalConditionalElseReturnStatement =
		        new ReturnStatement(nextPosition(),
		                moduleFunctionConditionalConditionalConditionalElseReturnStatementParameter);

		// MODULE FUNCTION LOOP
		moduleFunctionLoopCondition = new BooleanLiteral(nextPosition(), true);
		moduleFunctionLoopBlock = new Block(nextPosition());
		moduleFunctionLoop = new WhileLoop(nextPosition(), moduleFunctionLoopCondition, moduleFunctionLoopBlock);
		// add module function loop to module function
		moduleFunctionBlock.addStatement(moduleFunctionLoop);

		moduleFunctionLoopReturnStatementParameter = new StringLiteral(nextPosition(), "return");
		moduleFunctionLoopReturnStatement =
		        new ReturnStatement(nextPosition(), moduleFunctionLoopReturnStatementParameter);

		moduleFunctionLoopLoopCondition = new BooleanLiteral(nextPosition(), true);
		moduleFunctionLoopLoopBlock = new Block(nextPosition());
		moduleFunctionLoopLoop =
		        new WhileLoop(nextPosition(), moduleFunctionLoopLoopCondition, moduleFunctionLoopLoopBlock);
		// ass module function loop loop to module function loop block
		moduleFunctionLoopBlock.addStatement(moduleFunctionLoopLoop);

		moduleFunctionLoopLoopLoopCondition = new BooleanLiteral(nextPosition(), true);
		moduleFunctionLoopLoopLoopBlock = new Block(nextPosition());
		moduleFunctionLoopLoopLoop =
		        new WhileLoop(nextPosition(), moduleFunctionLoopLoopLoopCondition, moduleFunctionLoopLoopLoopBlock);
		// add module function loop loop loop to module function loop loop block
		moduleFunctionLoopLoopBlock.addStatement(moduleFunctionLoopLoopLoop);

		moduleFunctionLoopLoopLoopReturnStatementParameter = new StringLiteral(nextPosition(), "return");
		moduleFunctionLoopLoopLoopReturnStatement =
		        new ReturnStatement(nextPosition(), moduleFunctionLoopLoopLoopReturnStatementParameter);

	}

	@Test
	public void setUpASTTest() {
		assertNotNull("setParentVisitor is null", setParentVisitor);
		assertNotNull("controlFlowVisitor is null", controlFlowVisitor);

		// MODULE
		assertNotNull("moduleDeclaration is null", moduleDeclaration);
		assertNotNull("moduleBlock is null", moduleBlock);
		assertNotNull("moduleImports is null", moduleImports);
		assertNotNull("package is null", aPackage);

		// MODULE LOOP
		assertNotNull("moduleLoopCondition is null", moduleLoopCondition);
		assertNotNull("moduleLoopBlock is null", moduleLoopBlock);
		assertNotNull("moduleLoop is null", moduleLoop);
		assertNotNull("moduleLoopContinueStatement is null", moduleLoopContinueStatement);
		assertNotNull("moduleLoopBreakStatement is null", moduleLoopBreakStatement);
		assertNotNull("moduleLoopLoopCondition is null", moduleLoopLoopCondition);
		assertNotNull("moduleLoopLoopBlock is null", moduleLoopLoopBlock);

		// MODULE LOOP LOOP
		assertNotNull("moduleLoopLoop is null", moduleLoopLoop);
		assertNotNull("moduleLoopLoopContinueStatement is null", moduleLoopLoopContinueStatement);
		assertNotNull("moduleLoopLoopBreakStatement is null", moduleLoopLoopBreakStatement);
		assertNotNull(
		        "moduleLoopLoopConditionalStatementCondition is null",
		        moduleLoopLoopConditionalStatementCondition);
		assertNotNull(
		        "moduleLoopLoopConditionalStatementThenBlock is null",
		        moduleLoopLoopConditionalStatementThenBlock);
		assertNotNull(
		        "moduleLoopLoopConditionalStatementElseBlock is null",
		        moduleLoopLoopConditionalStatementElseBlock);
		assertNotNull("moduleLoopLoopConditionalStatement is null", moduleLoopLoopConditionalStatement);
		assertNotNull(
		        "moduleLoopLoopThenConditionalStatementCondition is null",
		        moduleLoopLoopThenConditionalStatementCondition);
		assertNotNull(
		        "moduleLoopLoopThenConditionalStatementThenBlock is null",
		        moduleLoopLoopThenConditionalStatementThenBlock);
		assertNotNull(
		        "moduleLoopLoopThenConditionalStatementElseBlock is null",
		        moduleLoopLoopThenConditionalStatementElseBlock);
		assertNotNull("moduleLoopLoopThenConditionalStatement is null", moduleLoopLoopThenConditionalStatement);
		assertNotNull(
		        "moduleLoopLoopElseConditionalStatementCondition is null",
		        moduleLoopLoopElseConditionalStatementCondition);
		assertNotNull(
		        "moduleLoopLoopElseConditionalStatementThenBlock is null",
		        moduleLoopLoopElseConditionalStatementThenBlock);
		assertNotNull(
		        "moduleLoopLoopElseConditionalStatementElseBlock is null",
		        moduleLoopLoopElseConditionalStatementElseBlock);
		assertNotNull("moduleLoopLoopElseConditionalStatement is null", moduleLoopLoopElseConditionalStatement);

		// MODULE LOOP CONDITIONAL STATEMENT
		assertNotNull("moduleLoopConditionalStatementCondition is null", moduleLoopConditionalStatementCondition);
		assertNotNull("moduleLoopConditionalStatementThenBlock is null", moduleLoopConditionalStatementThenBlock);
		assertNotNull("moduleLoopConditionalStatementElseBlock is null", moduleLoopConditionalStatementElseBlock);
		assertNotNull("moduleLoopConditionalStatement is null", moduleLoopConditionalStatement);
		assertNotNull("moduleLoopConditionalContinueStatement is null", moduleLoopConditionalContinueStatement);
		assertNotNull("moduleLoopConditionalBreakStatement is null", moduleLoopConditionalBreakStatement);

		// MODULE CONDITIONAL STATEMENT
		assertNotNull("moduleConditionalStatementCondition is null", moduleConditionalStatementCondition);
		assertNotNull("moduleConditionalStatementThenBlock is null", moduleConditionalStatementThenBlock);
		assertNotNull("moduleConditionalStatementElseBlock is null", moduleConditionalStatementElseBlock);
		assertNotNull("moduleConditionalStatement is null", moduleConditionalStatement);

		// MODULE STATETEMENTS
		assertNotNull("moduleContinueStatement is null", moduleContinueStatement);
		assertNotNull("moduleBreakStatement is null", moduleBreakStatement);
		assertNotNull("moduleReturnStatementParameter is null", moduleReturnStatementParameter);
		assertNotNull("moduleReturnStatement is null", moduleReturnStatement);

		// MODULE FUNCTION
		assertNotNull("moduleFunctionBlock is null", moduleFunctionBlock);
		assertNotNull("moduleFunctionParameter is null", moduleFunctionParameter);
		assertNotNull("moduleFunctionDeclaration is null", moduleFunctionDeclaration);
		assertNotNull("moduleFunctionReturnStatementParameter is null", moduleFunctionReturnStatementParameter);
		assertNotNull("moduleFunctionReturnStatement is null", moduleFunctionReturnStatement);

		// MODULE FUNCTION PROCEDURE
		assertNotNull("moduleFunctionProcedureBlock is null", moduleFunctionProcedureBlock);
		assertNotNull("moduleFunctionProcedureParameter is null", moduleFunctionProcedureParameter);
		assertNotNull("moduleFunctionProcedureDeclaration is null", moduleFunctionProcedureDeclaration);
		assertNotNull(
		        "moduleFunctionProcedureReturnStatementParameter is null",
		        moduleFunctionProcedureReturnStatementParameter);
		assertNotNull("moduleFunctionProcedureReturnStatement is null", moduleFunctionProcedureReturnStatement);

		// MODULE FUNCTION CONDITIONAL STATEMENT
		assertNotNull(
		        "moduleFunctionConditionalStatementCondition is null",
		        moduleFunctionConditionalStatementCondition);
		assertNotNull(
		        "moduleFunctionConditionalStatementThenBlock is null",
		        moduleFunctionConditionalStatementThenBlock);
		assertNotNull(
		        "moduleFunctionConditionalStatementElseBlock is null",
		        moduleFunctionConditionalStatementElseBlock);
		assertNotNull("moduleFunctionConditionalStatement is null", moduleFunctionConditionalStatement);
		assertNotNull(
		        "moduleFunctionConditionalReturnThenStatementParameter is null",
		        moduleFunctionConditionalReturnThenStatementParameter);
		assertNotNull(
		        "moduleFunctionConditionalReturnThenStatement is null",
		        moduleFunctionConditionalReturnThenStatement);
		assertNotNull(
		        "moduleFunctionConditionalReturnElseStatementParameter is null",
		        moduleFunctionConditionalReturnElseStatementParameter);
		assertNotNull(
		        "moduleFunctionConditionalReturnElseStatement is null",
		        moduleFunctionConditionalReturnElseStatement);
		assertNotNull(
		        "moduleFunctionConditionalConditionalStatementCondition is null",
		        moduleFunctionConditionalConditionalStatementCondition);
		assertNotNull(
		        "moduleFunctionConditionalConditionalStatementThenBlock is null",
		        moduleFunctionConditionalConditionalStatementThenBlock);
		assertNotNull(
		        "moduleFunctionConditionalConditionalStatementElseBlock is null",
		        moduleFunctionConditionalConditionalStatementElseBlock);
		assertNotNull(
		        "moduleFunctionConditionalConditionalStatement is null",
		        moduleFunctionConditionalConditionalStatement);
		assertNotNull(
		        "moduleFunctionConditionalConditionalReturnStatementParameter is null",
		        moduleFunctionConditionalConditionalReturnStatementParameter);
		assertNotNull(
		        "moduleFunctionConditionalConditionalReturnStatement is null",
		        moduleFunctionConditionalConditionalReturnStatement);
		assertNotNull(
		        "moduleFunctionConditionalConditionalConditionalStatementCondition is null",
		        moduleFunctionConditionalConditionalConditionalStatementCondition);
		assertNotNull(
		        "moduleFunctionConditionalConditionalConditionalStatementThenBlock is null",
		        moduleFunctionConditionalConditionalConditionalStatementThenBlock);
		assertNotNull(
		        "moduleFunctionConditionalConditionalConditionalStatementElseBlock is null",
		        moduleFunctionConditionalConditionalConditionalStatementElseBlock);
		assertNotNull(
		        "moduleFunctionConditionalConditionalConditionalStatement is null",
		        moduleFunctionConditionalConditionalConditionalStatement);
		assertNotNull(
		        "moduleFunctionConditionalConditionalConditionalThenReturnStatementParameter is null",
		        moduleFunctionConditionalConditionalConditionalThenReturnStatementParameter);
		assertNotNull(
		        "moduleFunctionConditionalConditionalConditionalThenReturnStatement is null",
		        moduleFunctionConditionalConditionalConditionalThenReturnStatement);
		assertNotNull(
		        "moduleFunctionConditionalConditionalConditionalElseReturnStatementParameter is null",
		        moduleFunctionConditionalConditionalConditionalElseReturnStatementParameter);
		assertNotNull(
		        "moduleFunctionConditionalConditionalConditionalElseReturnStatement is null",
		        moduleFunctionConditionalConditionalConditionalElseReturnStatement);

		// MODULE FUNCTION LOOP
		assertNotNull("moduleFunctionLoopCondition is null", moduleFunctionLoopCondition);
		assertNotNull("moduleFunctionLoopBlock is null", moduleFunctionLoopBlock);
		assertNotNull("moduleFunctionLoop is null", moduleFunctionLoop);
		assertNotNull("moduleFunctionLoopReturnStatementParameter is null", moduleFunctionLoopReturnStatementParameter);
		assertNotNull("moduleFunctionLoopReturnStatement is null", moduleFunctionLoopReturnStatement);
		assertNotNull("moduleFunctionLoopLoopCondition is null", moduleFunctionLoopLoopCondition);
		assertNotNull("moduleFunctionLoopLoopBlock is null", moduleFunctionLoopLoopBlock);
		assertNotNull("moduleFunctionLoopLoop is null", moduleFunctionLoopLoop);
		assertNotNull("moduleFunctionLoopLoopLoopCondition is null", moduleFunctionLoopLoopLoopCondition);
		assertNotNull("moduleFunctionLoopLoopLoopBlock is null", moduleFunctionLoopLoopLoopBlock);
		assertNotNull("moduleFunctionLoopLoopLoop is null", moduleFunctionLoopLoopLoop);
		assertNotNull(
		        "moduleFunctionLoopLoopLoopReturnStatementParameter is null",
		        moduleFunctionLoopLoopLoopReturnStatementParameter);
		assertNotNull("moduleFunctionLoopLoopLoopReturnStatement is null", moduleFunctionLoopLoopLoopReturnStatement);
	}

	/** Creates a continue statement inside of a while loop. */
	@Test
	public void continueStatementTest01() {
		// add previously defined statement to module loop
		moduleLoopBlock.addStatement(moduleLoopContinueStatement);

		// control flow visitor should not complain about it, since the
		// structure is
		// valid
		setParentVisitor.visit(aPackage);
		controlFlowVisitor.visit(aPackage);
	}

	/** Creates a continue statement inside of a nested while loop. */
	@Test
	public void continueStatementTest02() {
		// add previously defined statement to module loop
		moduleLoopLoopBlock.addStatement(moduleLoopLoopContinueStatement);

		// control flow visitor should not complain about it, since the
		// structure is
		// valid
		setParentVisitor.visit(aPackage);
		controlFlowVisitor.visit(aPackage);
	}

	/** Creates a continue statement inside of a conditional statement within a while loop */
	@Test
	public void continueStatementTest03() {
		// add statements to module loop conditional statement (then)
		moduleLoopConditionalStatementThenBlock.addStatement(moduleLoopConditionalContinueStatement);

		// control flow visitor should not complain about it, since the
		// structure is
		// valid
		setParentVisitor.visit(aPackage);
		controlFlowVisitor.visit(aPackage);
	}

	/** Create continue statement outside of a while loop */
	@Test(expected = InvalidControlFlowException.class)
	public void continueStatementTest04() {
		moduleBlock.addStatement(moduleContinueStatement);

		// control flow visitor should throw an exception now, since the
		// structure is invalid
		setParentVisitor.visit(aPackage);
		controlFlowVisitor.visit(aPackage);
	}

	/** Create break statement outside of a while loop */
	@Test(expected = InvalidControlFlowException.class)
	public void breakStatementTest04() {
		moduleBlock.addStatement(moduleBreakStatement);

		// control flow visitor should throw an exception now, since the
		// structure is invalid
		setParentVisitor.visit(aPackage);
		controlFlowVisitor.visit(aPackage);
	}

	/** Creates return statement outside of a function block */
	@Test(expected = InvalidControlFlowException.class)
	public void returnStatementTest01() {
		moduleBlock.addStatement(moduleReturnStatement);

		// control flow visitor should throw an exception now, since the
		// structure is invalid
		setParentVisitor.visit(aPackage);
		controlFlowVisitor.visit(aPackage);
	}

	/** Creates a return statement inside of a function block */
	@Test
	public void returnStatementTest02() {
		// add function to module
		moduleBlock.addDeclaration(moduleFunctionDeclaration);
		// ad a return statement to module function block
		moduleFunctionBlock.addStatement(moduleFunctionReturnStatement);

		// control flow visitor should not complain about it, since the
		// structure is
		// valid
		setParentVisitor.visit(aPackage);
		controlFlowVisitor.visit(aPackage);
	}

	/** Creates a return statement inside of a nested procedure declaration within a function block */
	@Test
	public void returnStatementTest03() {
		// add function to module
		moduleBlock.addDeclaration(moduleFunctionDeclaration);
		// add return statement to procedure
		moduleFunctionProcedureBlock.addStatement(moduleFunctionProcedureReturnStatement);
		// add procedure to function
		moduleFunctionBlock.addDeclaration(moduleFunctionProcedureDeclaration);

		// start visitors and expect InvalidControlFlowException, since there's
		// no guarantee, that the function always executes a return statement
		boolean invalidControlFlow = false;
		try {
			setParentVisitor.visit(aPackage);
			controlFlowVisitor.visit(aPackage);
		} catch (InvalidControlFlowException icfe) {
			invalidControlFlow = true;
		}
		// there should have been an InvalidControlFlowException during the
		// visit ...
		assertTrue("ReturnStatements needed inside the moduleFunction", invalidControlFlow);

		// now, add another return statement to the surrounding function
		moduleFunctionBlock.addStatement(moduleFunctionReturnStatement);

		// control flow visitor should not complain anymore, since the
		// structure is now valid
		setParentVisitor.visit(aPackage);
		controlFlowVisitor.visit(aPackage);
	}

	/** Creates a return statement inside of a conditional statement within a function block */
	@Test
	public void returnStatementTest04() {
		// add function to module
		moduleBlock.addDeclaration(moduleFunctionDeclaration);
		// add module function conditional return statement to module function
		// conditional statement
		moduleFunctionConditionalStatementElseBlock.addStatement(moduleFunctionConditionalReturnElseStatement);
		// add module function conditional statement to module function block
		moduleFunctionBlock.addStatement(moduleFunctionConditionalStatement);

		// control flow visitor should definitely complain about it, since there
		// is only a return statement in the else block, but neither in the then
		// block, nor in the function itself.
		boolean invalidControlFlow = false;
		try {
			setParentVisitor.visit(aPackage);
			controlFlowVisitor.visit(aPackage);
		} catch (InvalidControlFlowException icfe) {
			invalidControlFlow = true;
		}
		// there should have been an InvalidControlFlowException during the
		// visit ...
		assertTrue("ReturnStatements does not need to be found inside both conditional branches", invalidControlFlow);

		// now, add another return statement to the surrounding function
		moduleFunctionBlock.addStatement(moduleFunctionReturnStatement);

		// run visitors again
		setParentVisitor.visit(aPackage);
		controlFlowVisitor.visit(aPackage);

		// The controlFlowVisitor should not complain about anything anymore,
		// since the structure is now valid.
	}

	/** Creates a return statement inside of a conditional statement within a function block again, but resolve errors by
	 * adding another conditional return statement */
	@Test
	public void returnStatementTest05() {
		// add function to module
		moduleBlock.addDeclaration(moduleFunctionDeclaration);
		// add module function conditional return statement to module function
		// conditional statement
		moduleFunctionConditionalStatementElseBlock.addStatement(moduleFunctionConditionalReturnElseStatement);
		// add module function conditional statement to module function block
		moduleFunctionBlock.addStatement(moduleFunctionConditionalStatement);

		// control flow visitor should definitely complain about it, since there
		// is only a return statement in the else block, but neither in the then
		// block, nor in the function itself.
		boolean invalidControlFlow = false;
		try {
			setParentVisitor.visit(aPackage);
			controlFlowVisitor.visit(aPackage);
		} catch (InvalidControlFlowException icfe) {
			invalidControlFlow = true;
		}
		// there should have been an InvalidControlFlowException during the
		// visit ...
		assertTrue("ReturnStatements does not need to be found inside both conditional branches", invalidControlFlow);

		// add return statement to conditional statements then block
		moduleFunctionConditionalStatementThenBlock.addStatement(moduleFunctionConditionalReturnThenStatement);

		// run visitors again
		setParentVisitor.visit(aPackage);
		controlFlowVisitor.visit(aPackage);

		// The controlFlowVisitor should not complain about anything, since the
		// structure is valid.
	}

	/** Creates return statement inside of a 3-times nested conditional statement within a function block */
	@Test
	public void returnStatementTest06() {
		// add function to module
		moduleBlock.addDeclaration(moduleFunctionDeclaration);
		// add module function conditional conditional conditional return
		// statement to module function conditional conditional conditional then
		// block
		moduleFunctionConditionalConditionalConditionalStatementElseBlock.addStatement(moduleFunctionConditionalConditionalConditionalElseReturnStatement);

		// control flow should be invalid now, since there's no global return
		// statement
		boolean invalidControlFlow = false;
		try {
			setParentVisitor.visit(aPackage);
			controlFlowVisitor.visit(aPackage);
		} catch (InvalidControlFlowException icfe) {
			invalidControlFlow = true;
		}
		// there should have been an InvalidControlFlowException during the
		// visit ...
		assertTrue("ReturnStatements does not need to be found inside both conditional branches", invalidControlFlow);

		// resolve these errors by adding another return statement to each
		// conditional branch
		moduleFunctionConditionalConditionalConditionalStatementThenBlock.addStatement(moduleFunctionConditionalConditionalConditionalThenReturnStatement);
		moduleFunctionConditionalConditionalStatementElseBlock.addStatement(moduleFunctionConditionalConditionalReturnStatement);
		moduleFunctionConditionalStatementThenBlock.addStatement(moduleFunctionConditionalReturnThenStatement);

		// everything should be just fine now
		setParentVisitor.visit(aPackage);
		controlFlowVisitor.visit(aPackage);
	}

	/** Creates return statement inside a while loop within a function block */
	@Test
	public void returnStatementTest07() {
		// add function to module
		moduleBlock.addDeclaration(moduleFunctionDeclaration);
		// add module function loop return statement to module function loop
		// block
		moduleFunctionLoopBlock.addStatement(moduleFunctionLoopReturnStatement);

		// control flow should be invalid now, since there's no global return
		// statement
		boolean invalidControlFlow = false;
		try {
			setParentVisitor.visit(aPackage);
			controlFlowVisitor.visit(aPackage);
		} catch (InvalidControlFlowException icfe) {
			invalidControlFlow = true;
		}
		// there should have been an InvalidControlFlowException during the
		// visit ...
		assertTrue("ReturnStatements does not need to be found inside both conditional branches", invalidControlFlow);

		// resolve errors by adding a return statement to module function
		moduleFunctionBlock.addStatement(moduleFunctionReturnStatement);

		// everything should be just fine now
		setParentVisitor.visit(aPackage);
		controlFlowVisitor.visit(aPackage);
	}

	/** Create return statement inside of a 3-times nested while loop within a function block */
	@Test
	public void returnStatementTest08() {
		// add function to module
		moduleBlock.addDeclaration(moduleFunctionDeclaration);
		// add module function loop loop loop return statement to module
		// function loop loop loop block
		moduleFunctionLoopLoopLoopBlock.addStatement(moduleFunctionLoopLoopLoopReturnStatement);

		// control flow should be invalid now, since there's no global return
		// statement
		boolean invalidControlFlow = false;
		try {
			setParentVisitor.visit(aPackage);
			controlFlowVisitor.visit(aPackage);
		} catch (InvalidControlFlowException icfe) {
			invalidControlFlow = true;
		}
		// there should have been an InvalidControlFlowException during the
		// visit ...
		assertTrue("ReturnStatements does not need to be found inside both conditional branches", invalidControlFlow);

		// resolve errors by adding a return statement to module function
		moduleFunctionBlock.addStatement(moduleFunctionReturnStatement);

		// everything should be just fine now
		setParentVisitor.visit(aPackage);
		controlFlowVisitor.visit(aPackage);
	}

	/** Creates a conditional statement inside a while loop */
	@Test
	public void conditionalStatementTest01() {
		moduleLoopBlock.addStatement(moduleLoopConditionalStatement);

		// control flow visitor should not complain about it, since the
		// structure is valid
		setParentVisitor.visit(aPackage);
		controlFlowVisitor.visit(aPackage);
	}

	/** Creates a conditional statement inside a conditional statements then block */
	@Test
	public void conditionalStatementTest02() {
		moduleConditionalStatementThenBlock.addStatement(moduleLoopConditionalStatement);

		// control flow visitor should not complain about it, since the
		// structure is valid
		setParentVisitor.visit(aPackage);
		controlFlowVisitor.visit(aPackage);
	}

	/** Creates a conditional statement inside a conditional statements else block */
	@Test
	public void conditionalStatementTest03() {
		moduleConditionalStatementElseBlock.addStatement(moduleLoopConditionalStatement);

		// control flow visitor should not complain about it, since the
		// structure is valid
		setParentVisitor.visit(aPackage);
		controlFlowVisitor.visit(aPackage);
	}

	/** Creates a conditional statement inside of a nested while loop */
	@Test
	public void conditionalStatementTest04() {
		moduleLoopLoopBlock.addStatement(moduleLoopLoopConditionalStatement);

		// everything should be just fine
		setParentVisitor.visit(aPackage);
		controlFlowVisitor.visit(aPackage);
	}

	/** Creates conditional statement inside of a nested conditional statement within a while loop */
	@Test
	public void conditionalStatementTest05() {
		// add a conditional statement inside a nested while loop
		moduleLoopLoopBlock.addStatement(moduleLoopLoopConditionalStatement);
		// add some more conditional statements to the one above
		moduleLoopLoopConditionalStatementThenBlock.addStatement(moduleLoopLoopThenConditionalStatement);
		moduleLoopLoopConditionalStatementElseBlock.addStatement(moduleLoopLoopElseConditionalStatement);

		// everything should be just fine
		setParentVisitor.visit(aPackage);
		controlFlowVisitor.visit(aPackage);
	}
}
