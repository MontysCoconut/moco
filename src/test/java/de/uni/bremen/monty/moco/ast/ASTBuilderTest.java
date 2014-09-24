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

import de.uni.bremen.monty.moco.CompileTestProgramsTest;
import de.uni.bremen.monty.moco.antlr.MontyLexer;
import de.uni.bremen.monty.moco.antlr.MontyParser;
import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.ast.expression.FunctionCall;
import de.uni.bremen.monty.moco.ast.expression.VariableAccess;
import de.uni.bremen.monty.moco.ast.expression.literal.IntegerLiteral;
import de.uni.bremen.monty.moco.ast.expression.literal.StringLiteral;
import de.uni.bremen.monty.moco.ast.statement.Assignment;
import de.uni.bremen.monty.moco.ast.statement.ConditionalStatement;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.apache.commons.io.FileUtils;
import org.junit.Ignore;
import org.junit.Test;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;

import static de.uni.bremen.monty.moco.ast.declaration.VariableDeclaration.DeclarationType;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.Assert.assertTrue;

public class ASTBuilderTest {

	@Test
	public void shouldConvertVariableInitialisation() throws Exception {
		ModuleDeclaration ast = buildASTfrom("varDecl");

		VariableDeclaration declaration = (VariableDeclaration) ast.getBlock().getDeclarations().get(0);

		Assignment statement = (Assignment) ast.getBlock().getStatements().get(0);
		VariableAccess left = (VariableAccess) statement.getLeft();
		StringLiteral right = (StringLiteral) statement.getRight();

		assertThat(declaration.getIdentifier().getSymbol(), is("s"));
		assertThat(left.getIdentifier().getSymbol(), is("s"));
		assertThat(right.getValue(), is("Hallo"));

	}

	@Test
	public void shouldConvertDeclarationType() throws Exception {
		ModuleDeclaration ast = buildASTfrom("declarationType");

		ClassDeclaration classDecl = (ClassDeclaration) ast.getBlock().getDeclarations().get(0);
		VariableDeclaration memberDecl = (VariableDeclaration) classDecl.getBlock().getDeclarations().get(0);
		VariableDeclaration memberInit = (VariableDeclaration) classDecl.getBlock().getDeclarations().get(1);
		ProcedureDeclaration memberProc = (ProcedureDeclaration) classDecl.getBlock().getDeclarations().get(2);
		FunctionDeclaration memberFun = (FunctionDeclaration) classDecl.getBlock().getDeclarations().get(3);

		VariableDeclaration varDecl = (VariableDeclaration) ast.getBlock().getDeclarations().get(1);
		VariableDeclaration varInit = (VariableDeclaration) ast.getBlock().getDeclarations().get(2);
		FunctionDeclaration funDecl = (FunctionDeclaration) ast.getBlock().getDeclarations().get(3);
		ProcedureDeclaration procDecl = (ProcedureDeclaration) ast.getBlock().getDeclarations().get(4);

		assertThat(varDecl.getDeclarationType(), is(VariableDeclaration.DeclarationType.VARIABLE));
		assertThat(varInit.getDeclarationType(), is(VariableDeclaration.DeclarationType.VARIABLE));
		assertThat(
		        funDecl.getParameter().get(0).getDeclarationType(),
		        is(VariableDeclaration.DeclarationType.PARAMETER));
		assertThat(
		        procDecl.getParameter().get(0).getDeclarationType(),
		        is(VariableDeclaration.DeclarationType.PARAMETER));

		assertThat(memberDecl.getDeclarationType(), is(VariableDeclaration.DeclarationType.ATTRIBUTE));
		assertThat(memberInit.getDeclarationType(), is(VariableDeclaration.DeclarationType.ATTRIBUTE));
		assertThat(
		        memberProc.getParameter().get(0).getDeclarationType(),
		        is(VariableDeclaration.DeclarationType.PARAMETER));
		assertThat(
		        memberFun.getParameter().get(0).getDeclarationType(),
		        is(VariableDeclaration.DeclarationType.PARAMETER));

		assertThat(memberProc.getDeclarationType(), is(ProcedureDeclaration.DeclarationType.METHOD));
		assertThat(memberFun.getDeclarationType(), is(ProcedureDeclaration.DeclarationType.METHOD));
		assertThat(funDecl.getDeclarationType(), is(ProcedureDeclaration.DeclarationType.UNBOUND));
		assertThat(procDecl.getDeclarationType(), is(ProcedureDeclaration.DeclarationType.UNBOUND));

	}

	@Test
	public void shouldConvertIf() throws Exception {
		ModuleDeclaration ast = buildASTfrom("ifElse");

		ConditionalStatement condStmt = (ConditionalStatement) ast.getBlock().getStatements().get(0);

		VariableAccess condition = (VariableAccess) condStmt.getCondition();
		FunctionCall thenBlock = (FunctionCall) condStmt.getThenBlock().getStatements().get(0);
		Block elseBlock = condStmt.getElseBlock();

		assertThat(condition.getIdentifier().getSymbol(), is("a"));

		assertThat(getValue(thenBlock), is(1));
		assertTrue(elseBlock.isEmpty());
	}

	@Test
	public void shouldConvertIfElse() throws Exception {
		ModuleDeclaration ast = buildASTfrom("ifElse");

		ConditionalStatement condStmt = (ConditionalStatement) ast.getBlock().getStatements().get(1);

		VariableAccess condition = (VariableAccess) condStmt.getCondition();
		FunctionCall thenBlock = (FunctionCall) condStmt.getThenBlock().getStatements().get(0);
		FunctionCall elseBlock = (FunctionCall) condStmt.getElseBlock().getStatements().get(0);

		assertThat(condition.getIdentifier().getSymbol(), is("a"));

		assertThat(getValue(thenBlock), is(1));
		assertThat(getValue(elseBlock), is(2));
	}

	@Test
	public void shouldConvertIfWithElseAndElseIf() throws Exception {
		ModuleDeclaration ast = buildASTfrom("ifElse");

		ConditionalStatement condStmt = (ConditionalStatement) ast.getBlock().getStatements().get(2);

		VariableAccess firstCondition = (VariableAccess) condStmt.getCondition();
		FunctionCall firstThenBlock = (FunctionCall) condStmt.getThenBlock().getStatements().get(0);
		ConditionalStatement firstElseBlock = (ConditionalStatement) condStmt.getElseBlock().getStatements().get(0);

		VariableAccess secondCondition = (VariableAccess) firstElseBlock.getCondition();
		FunctionCall secondThenBlock = (FunctionCall) firstElseBlock.getThenBlock().getStatements().get(0);
		FunctionCall secondElseBlock = (FunctionCall) firstElseBlock.getElseBlock().getStatements().get(0);

		assertThat(firstCondition.getIdentifier().getSymbol(), is("a"));
		assertThat(secondCondition.getIdentifier().getSymbol(), is("b"));

		assertThat(getValue(firstThenBlock), is(1));
		assertThat(getValue(secondThenBlock), is(2));
		assertThat(getValue(secondElseBlock), is(3));

	}

	@Test
	public void shouldConvertIfWithElseAnd2ElseIf() throws Exception {
		ModuleDeclaration ast = buildASTfrom("ifElse");

		ConditionalStatement condStmt = (ConditionalStatement) ast.getBlock().getStatements().get(3);

		VariableAccess firstCondition = (VariableAccess) condStmt.getCondition();
		FunctionCall firstThenBlock = (FunctionCall) condStmt.getThenBlock().getStatements().get(0);
		ConditionalStatement firstElseBlock = (ConditionalStatement) condStmt.getElseBlock().getStatements().get(0);

		VariableAccess secondCondition = (VariableAccess) firstElseBlock.getCondition();
		FunctionCall secondThenBlock = (FunctionCall) firstElseBlock.getThenBlock().getStatements().get(0);
		ConditionalStatement secondElseBlock =
		        (ConditionalStatement) firstElseBlock.getElseBlock().getStatements().get(0);

		VariableAccess thirdCondition = (VariableAccess) secondElseBlock.getCondition();
		FunctionCall thirdThenBlock = (FunctionCall) secondElseBlock.getThenBlock().getStatements().get(0);
		FunctionCall thirdElseBlock = (FunctionCall) secondElseBlock.getElseBlock().getStatements().get(0);

		assertThat(firstCondition.getIdentifier().getSymbol(), is("a"));
		assertThat(secondCondition.getIdentifier().getSymbol(), is("b"));
		assertThat(thirdCondition.getIdentifier().getSymbol(), is("c"));

		assertThat(getValue(firstThenBlock), is(1));
		assertThat(getValue(secondThenBlock), is(2));
		assertThat(getValue(thirdThenBlock), is(3));
		assertThat(getValue(thirdElseBlock), is(4));

	}

	@Test
	public void shouldConvertIfWith2ElseIf() throws Exception {
		ModuleDeclaration ast = buildASTfrom("ifElse");

		ConditionalStatement condStmt = (ConditionalStatement) ast.getBlock().getStatements().get(4);

		VariableAccess firstCondition = (VariableAccess) condStmt.getCondition();
		FunctionCall firstThenBlock = (FunctionCall) condStmt.getThenBlock().getStatements().get(0);
		ConditionalStatement firstElseBlock = (ConditionalStatement) condStmt.getElseBlock().getStatements().get(0);

		VariableAccess secondCondition = (VariableAccess) firstElseBlock.getCondition();
		FunctionCall secondThenBlock = (FunctionCall) firstElseBlock.getThenBlock().getStatements().get(0);
		ConditionalStatement secondElseBlock =
		        (ConditionalStatement) firstElseBlock.getElseBlock().getStatements().get(0);

		VariableAccess thirdCondition = (VariableAccess) secondElseBlock.getCondition();
		FunctionCall thirdThenBlock = (FunctionCall) secondElseBlock.getThenBlock().getStatements().get(0);

		assertThat(firstCondition.getIdentifier().getSymbol(), is("a"));
		assertThat(secondCondition.getIdentifier().getSymbol(), is("b"));
		assertThat(thirdCondition.getIdentifier().getSymbol(), is("c"));

		assertThat(getValue(firstThenBlock), is(1));
		assertThat(getValue(secondThenBlock), is(2));
		assertThat(getValue(thirdThenBlock), is(3));

		assertTrue(secondElseBlock.getElseBlock().isEmpty());

	}

	private int getValue(FunctionCall print) {
		return ((IntegerLiteral) print.getArguments().get(0)).getValue();
	}

	private ModuleDeclaration buildASTfrom(String varDecl) throws IOException, URISyntaxException {
		String fileName = "testAstBuilder/" + varDecl + ".monty";

		String montyProgram = getFileContent(fileName);
		MontyParser parser = createMontyParser(montyProgram);
		return buildAST(fileName, parser);
	}

	private ModuleDeclaration buildAST(String fileName, MontyParser parser) {
		ASTBuilder astBuilder = new ASTBuilder(fileName);
		ASTNode rootNode = astBuilder.visitModuleDeclaration(parser.moduleDeclaration());
		return (ModuleDeclaration) rootNode;
	}

	private MontyParser createMontyParser(String montyProgram) {
		ANTLRInputStream input = new ANTLRInputStream(montyProgram + "\n");
		MontyLexer lexer = new MontyLexer(input);
		CommonTokenStream tokens = new CommonTokenStream(lexer);
		return new MontyParser(tokens);
	}

	private String getFileContent(String fileName) throws URISyntaxException, IOException {
		Class<CompileTestProgramsTest> aClass = CompileTestProgramsTest.class;
		ClassLoader classLoader = aClass.getClassLoader();
		File file = new File(classLoader.getResource(fileName).toURI());
		return FileUtils.readFileToString(file);
	}
}
