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
import de.uni.bremen.monty.moco.ast.declaration.VariableDeclaration.DeclarationType;
import de.uni.bremen.monty.moco.ast.expression.*;
import de.uni.bremen.monty.moco.ast.expression.literal.BooleanLiteral;
import de.uni.bremen.monty.moco.ast.expression.literal.FloatLiteral;
import de.uni.bremen.monty.moco.ast.expression.literal.IntegerLiteral;
import de.uni.bremen.monty.moco.ast.expression.literal.StringLiteral;
import de.uni.bremen.monty.moco.ast.statement.*;
import de.uni.bremen.monty.moco.exception.InvalidExpressionException;
import de.uni.bremen.monty.moco.exception.TypeMismatchException;
import de.uni.bremen.monty.moco.exception.UnknownIdentifierException;
import de.uni.bremen.monty.moco.visitor.DeclarationVisitor;
import de.uni.bremen.monty.moco.visitor.ResolveVisitor;
import de.uni.bremen.monty.moco.visitor.SetParentVisitor;
import de.uni.bremen.monty.moco.visitor.TypeCheckVisitor;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import static org.junit.Assert.*;

public class TypeCheckTest {

	private SetParentVisitor SPV;
	private DeclarationVisitor DV;
	private TypeCheckVisitor TCV;
	private ResolveVisitor RV;

	private ModuleDeclaration moduleDeclaration;
	private Package aPackage;
	private Block moduleBlock;

	private ClassDeclaration object;
	private ClassDeclaration classDeclaration;

	private VariableDeclaration intVariable;
	private VariableDeclaration floatVariable;
	private VariableDeclaration strVariable;
	private VariableDeclaration booleanVariable;

	private VariableAccess intVariableAccess;
	private VariableAccess floatVariableAccess;
	private VariableAccess booleanVariableAccess;
	private VariableAccess strVariableAccess;

	private IntegerLiteral intLiteral;
	private FloatLiteral floatLiteral;
	private StringLiteral strLiteral;
	private BooleanLiteral booleanLiteral;

	private FunctionDeclaration functionDeclarationIntReturnInt;
	private FunctionDeclaration functionDeclarationStringReturnString;

	private ClassDeclaration classPerson;
	private ClassDeclaration classStudent;

	private VariableDeclaration classPersonVarDecl;
	private VariableDeclaration classStudentVarDecl;

	private ConditionalExpression ConditionStringString;
	private ConditionalExpression ConditionIntString;
	private ConditionalExpression ConditionIntInt;

	private int lineCounter = 0;

	private Position buildPosition() {
		return new Position("TypeCheckTest", lineCounter++, 0);
	}

	// helper
	private int counter = 0;

	public Position nextPosition() {
		return new Position("TestFile", counter++, 1);
	}

	@Rule
	public ExpectedException exception = ExpectedException.none();

	@Before
	public void setUpAST() {
		// reset counter
		counter = 0;

		SPV = new SetParentVisitor();
		SPV.setStopOnFirstError(true);

		DV = new DeclarationVisitor();
		DV.setStopOnFirstError(true);

		RV = new ResolveVisitor();
		RV.setStopOnFirstError(true);

		TCV = new TypeCheckVisitor();
		TCV.setStopOnFirstError(true);

		Block classDeclarationBlock = new Block(buildPosition());
		classDeclaration =
		        new ClassDeclaration(buildPosition(), new Identifier("classDeclaration"),
		                new ArrayList<ResolvableIdentifier>(), classDeclarationBlock);
		classDeclarationBlock.addDeclaration(new VariableDeclaration(buildPosition(), new Identifier(
		        "classVariableDeclaration"), new ResolvableIdentifier("String"),
		        VariableDeclaration.DeclarationType.ATTRIBUTE));
		classDeclarationBlock.addDeclaration(new ProcedureDeclaration(buildPosition(), new Identifier(
		        "classProcedureDeclaration"), new Block(buildPosition()), new ArrayList<VariableDeclaration>()));

		intVariable =
		        new VariableDeclaration(buildPosition(), new Identifier("intVariable"),
		                new ResolvableIdentifier("Int"), VariableDeclaration.DeclarationType.VARIABLE);
		floatVariable =
		        new VariableDeclaration(buildPosition(), new Identifier("floatVariable"), new ResolvableIdentifier(
		                "Float"), VariableDeclaration.DeclarationType.VARIABLE);
		booleanVariable =
		        new VariableDeclaration(buildPosition(), new Identifier("booleanVariable"), new ResolvableIdentifier(
		                "Bool"), VariableDeclaration.DeclarationType.VARIABLE);
		strVariable =
		        new VariableDeclaration(buildPosition(), new Identifier("strVariable"), new ResolvableIdentifier(
		                "String"), VariableDeclaration.DeclarationType.VARIABLE);

		intVariableAccess = new VariableAccess(buildPosition(), new ResolvableIdentifier("intVariable"));
		floatVariableAccess = new VariableAccess(buildPosition(), new ResolvableIdentifier("floatVariable"));
		booleanVariableAccess = new VariableAccess(buildPosition(), new ResolvableIdentifier("booleanVariable"));
		strVariableAccess = new VariableAccess(buildPosition(), new ResolvableIdentifier("strVariable"));

		intLiteral = new IntegerLiteral(buildPosition(), 1);
		floatLiteral = new FloatLiteral(buildPosition(), 0f);
		strLiteral = new StringLiteral(buildPosition(), "42");
		booleanLiteral = new BooleanLiteral(buildPosition(), false);

		// set up test AST
		moduleBlock = new Block(new Position("TESTFILE", 2, 10));
		moduleDeclaration =
		        new ModuleDeclaration(new Position("TESTFILE", 1, 10), new Identifier("Main"), moduleBlock,
		                new ArrayList<Import>());
		aPackage = new Package(new Identifier(""));
		aPackage.addModule(moduleDeclaration);
		Package corePackage = new Package(new Identifier("core"));
		Block block = new Block(new Position());
		for (ClassDeclaration classDeclaration : CoreClasses.getAllCoreClasses()) {
			block.addDeclaration(classDeclaration);
		}

		corePackage.addModule(new ModuleDeclaration(new Position(), new Identifier("CoreClasses"), block,
		        Collections.<Import> emptyList()));
		aPackage.addSubPackage(corePackage);

		// Function Block Declaration
		Block functionblock = new Block(buildPosition());
		functionblock.addStatement(new ReturnStatement(buildPosition(), new StringLiteral(buildPosition(),
		        "StringReturnString Result")));
		ArrayList<VariableDeclaration> params = new ArrayList<VariableDeclaration>();
		params.add(new VariableDeclaration(buildPosition(), new Identifier("a"), new ResolvableIdentifier("String"),
		        DeclarationType.PARAMETER));
		functionDeclarationStringReturnString =
		        new FunctionDeclaration(buildPosition(), new Identifier("StringReturnString"), functionblock, params,
		                new ResolvableIdentifier("String"));

		Block functionblock2 = new Block(buildPosition());
		functionblock2.addStatement(new ReturnStatement(buildPosition(), new IntegerLiteral(buildPosition(), 1337)));
		ArrayList<VariableDeclaration> params2 = new ArrayList<VariableDeclaration>();
		params2.add(new VariableDeclaration(buildPosition(), new Identifier("a"), new ResolvableIdentifier("Int"),
		        DeclarationType.PARAMETER));
		functionDeclarationIntReturnInt =
		        new FunctionDeclaration(buildPosition(), new Identifier("IntReturnInt"), functionblock2, params2,
		                new ResolvableIdentifier("Int"));

		// Class Declaration Person
		Block declList = new Block(buildPosition());
		declList.addDeclaration(new VariableDeclaration(buildPosition(), new Identifier("name"),
		        new ResolvableIdentifier("String"), DeclarationType.ATTRIBUTE));
		declList.addDeclaration(new VariableDeclaration(buildPosition(), new Identifier("age"),
		        new ResolvableIdentifier("String"), DeclarationType.ATTRIBUTE));
		classPerson =
		        new ClassDeclaration(buildPosition(), new Identifier("Person"), new ArrayList<ResolvableIdentifier>(),
		                declList);

		// Class Declaration Person
		Block declList2 = new Block(buildPosition());
		declList2.addDeclaration(new VariableDeclaration(buildPosition(), new Identifier("name"),
		        new ResolvableIdentifier("String"), DeclarationType.ATTRIBUTE));
		declList2.addDeclaration(new VariableDeclaration(buildPosition(), new Identifier("age"),
		        new ResolvableIdentifier("String"), DeclarationType.ATTRIBUTE));
		classStudent =
		        new ClassDeclaration(buildPosition(), new Identifier("Student"), new ArrayList<ResolvableIdentifier>(),
		                declList2);

		classPersonVarDecl =
		        new VariableDeclaration(buildPosition(), new Identifier("myPerson"),
		                new ResolvableIdentifier("Person"), DeclarationType.VARIABLE);
		classStudentVarDecl =
		        new VariableDeclaration(buildPosition(), new Identifier("myStudent"), new ResolvableIdentifier(
		                "Student"), DeclarationType.VARIABLE);

		ConditionStringString = new ConditionalExpression(buildPosition(), booleanLiteral, strLiteral, strLiteral);

		ConditionIntString = new ConditionalExpression(buildPosition(), booleanLiteral, intLiteral, strLiteral);

		ConditionIntInt = new ConditionalExpression(buildPosition(), booleanLiteral, intLiteral, intLiteral);

	}

	@Test
	public void setUpASTTest() {
		assertNotNull("SPV is null", SPV);
		assertNotNull("DV is null", DV);
		assertNotNull("RV is null", RV);
		assertNotNull("TCV is null", TCV);

		assertNotNull("package is null", aPackage);
		assertNotNull("moduleDeclaration is null", moduleDeclaration);
		assertNotNull("moduleBlock is null", moduleBlock);

		assertNotNull("intVariable is null", intVariable);
		assertNotNull("floatVariable is null", floatVariable);
		assertNotNull("strVariable is null", strVariable);
		assertNotNull("booleanVariable is null", booleanVariable);

		assertNotNull("intVariableAccess is null", intVariableAccess);
		assertNotNull("floatVariableAccess is null", floatVariableAccess);
		assertNotNull("booleanVariableAccess is null", booleanVariableAccess);
		assertNotNull("strVariableAccess is null", strVariableAccess);

		assertNotNull("intLiteral is null", intLiteral);
		assertNotNull("floatLiteral is null", floatLiteral);
		assertNotNull("strLiteral is null", strLiteral);
		assertNotNull("booleanLiteral is null", booleanLiteral);
	}

	// TYPE COMPATIBILITY

	@Test
	public void typeEqualityTest() {
		for (TypeDeclaration type1 : CoreClasses.getAllCoreClasses()) {
			for (TypeDeclaration type2 : CoreClasses.getAllCoreClasses()) {
				if (type2 != CoreClasses.objectType()) {
					if (type1 == type2 || type2 == CoreClasses.objectType()) {
						assertTrue(String.format(
						        "%s does not match %s",
						        type1.getIdentifier().getSymbol(),
						        type2.getIdentifier().getSymbol()), type1.matchesType(type2));
					} else {
						assertFalse(String.format(
						        "%s does match %s",
						        type1.getIdentifier().getSymbol(),
						        type2.getIdentifier().getSymbol()), type1.matchesType(type2));
					}
				}
			}
		}
	}

	// LITERAL COMPATIBILITY

	@Test
	public void literalCompatibilityTest() {
		Assignment intAssignment = new Assignment(buildPosition(), intVariableAccess, intLiteral);
		Assignment floatAssignment = new Assignment(buildPosition(), floatVariableAccess, floatLiteral);
		Assignment booleanAssignment = new Assignment(buildPosition(), booleanVariableAccess, booleanLiteral);
		Assignment strAssignment = new Assignment(buildPosition(), strVariableAccess, strLiteral);

		moduleBlock.addDeclaration(intVariable);
		moduleBlock.addDeclaration(floatVariable);
		moduleBlock.addDeclaration(booleanVariable);
		moduleBlock.addDeclaration(strVariable);
		moduleBlock.addStatement(intAssignment);
		moduleBlock.addStatement(floatAssignment);
		moduleBlock.addStatement(booleanAssignment);
		moduleBlock.addStatement(strAssignment);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage); // no exception here
	}

	// LITERAL INCOMPATIBILITY

	@Test(expected = TypeMismatchException.class)
	public void literalIncompatibilityTestIntFloat() {
		Assignment intAssignment = new Assignment(buildPosition(), intVariableAccess, floatLiteral);

		moduleBlock.addDeclaration(intVariable);
		moduleBlock.addStatement(intAssignment);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage); // TypeMismatchException
	}

	@Test(expected = TypeMismatchException.class)
	public void literalIncompatibilityTestIntBool() {
		Assignment intAssignment = new Assignment(buildPosition(), intVariableAccess, booleanLiteral);

		moduleBlock.addDeclaration(intVariable);
		moduleBlock.addStatement(intAssignment);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage); // TypeMismatchException
	}

	@Test(expected = TypeMismatchException.class)
	public void literalIncompatibilityTestIntStr() {
		Assignment intAssignment = new Assignment(buildPosition(), intVariableAccess, strLiteral);

		moduleBlock.addDeclaration(intVariable);
		moduleBlock.addStatement(intAssignment);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage); // TypeMismatchException
	}

	@Test(expected = TypeMismatchException.class)
	public void literalIncompatibilityTestFloatInt() {
		Assignment floatAssignment = new Assignment(buildPosition(), floatVariableAccess, intLiteral);

		moduleBlock.addDeclaration(floatVariable);
		moduleBlock.addStatement(floatAssignment);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage); // TypeMismatchException
	}

	@Test(expected = TypeMismatchException.class)
	public void literalIncompatibilityTestFloatBool() {
		Assignment floatAssignment = new Assignment(buildPosition(), floatVariableAccess, booleanLiteral);

		moduleBlock.addDeclaration(floatVariable);
		moduleBlock.addStatement(floatAssignment);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage); // TypeMismatchException
	}

	@Test(expected = TypeMismatchException.class)
	public void literalIncompatibilityTestFloatStr() {
		Assignment floatAssignment = new Assignment(buildPosition(), floatVariableAccess, strLiteral);

		moduleBlock.addDeclaration(floatVariable);
		moduleBlock.addStatement(floatAssignment);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage); // TypeMismatchException
	}

	@Test(expected = TypeMismatchException.class)
	public void literalIncompatibilityTestBoolInt() {
		Assignment booleanAssignment = new Assignment(buildPosition(), booleanVariableAccess, intLiteral);

		moduleBlock.addDeclaration(booleanVariable);
		moduleBlock.addStatement(booleanAssignment);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage); // TypeMismatchException
	}

	@Test(expected = TypeMismatchException.class)
	public void literalIncompatibilityTestBoolFloat() {
		Assignment booleanAssignment = new Assignment(buildPosition(), booleanVariableAccess, floatLiteral);

		moduleBlock.addDeclaration(booleanVariable);
		moduleBlock.addStatement(booleanAssignment);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage); // TypeMismatchException
	}

	@Test(expected = TypeMismatchException.class)
	public void literalIncompatibilityTestBoolStr() {
		Assignment booleanAssignment = new Assignment(buildPosition(), booleanVariableAccess, strLiteral);

		moduleBlock.addDeclaration(booleanVariable);
		moduleBlock.addStatement(booleanAssignment);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage); // TypeMismatchException
	}

	@Test(expected = TypeMismatchException.class)
	public void literalIncompatibilityTestStrInt() {
		Assignment strAssignment = new Assignment(buildPosition(), strVariableAccess, intLiteral);

		moduleBlock.addDeclaration(strVariable);
		moduleBlock.addStatement(strAssignment);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage); // TypeMismatchException
	}

	@Test(expected = TypeMismatchException.class)
	public void literalIncompatibilityTestStrFloat() {
		Assignment strAssignment = new Assignment(buildPosition(), strVariableAccess, floatLiteral);

		moduleBlock.addDeclaration(strVariable);
		moduleBlock.addStatement(strAssignment);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage); // TypeMismatchException
	}

	@Test(expected = TypeMismatchException.class)
	public void literalIncompatibilityTestStrBool() {
		Assignment strAssignment = new Assignment(buildPosition(), strVariableAccess, booleanLiteral);

		moduleBlock.addDeclaration(strVariable);
		moduleBlock.addStatement(strAssignment);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage); // TypeMismatchException
	}

	// ASSIGNMENTS
	// => Each testcase has to include a well-typed scenario (with a correctly
	// typed left and right side), as well as an ill-typed scenario.

	private Object[] makeVarAccessVarAccessAssignment(String LeftResolvableSymbol, String RightResolvableSymbol) {
		/*
		 * Makes a the declarations and VariableAccess objects needed for an variable access = variable access
		 * AssignmentTest01 and appends it to the Block
		 */
		VariableDeclaration v1 =
		        new VariableDeclaration(buildPosition(), new Identifier("var1"), new ResolvableIdentifier(
		                LeftResolvableSymbol), DeclarationType.VARIABLE);
		moduleBlock.addDeclaration(v1);
		VariableDeclaration v2 =
		        new VariableDeclaration(buildPosition(), new Identifier("var2"), new ResolvableIdentifier(
		                RightResolvableSymbol), DeclarationType.VARIABLE);
		moduleBlock.addDeclaration(v2);
		VariableAccess val = new VariableAccess(buildPosition(), new ResolvableIdentifier("var1"));

		VariableAccess var = new VariableAccess(buildPosition(), new ResolvableIdentifier("var2"));

		Assignment a = new Assignment(buildPosition(), val, var);
		moduleBlock.addStatement(a);
		Object[] result = { v1, v2, a };
		return result;
	}

	@Test
	public void assignmentTest01_1() {
		// variable access = variable access

		Object[] generated = makeVarAccessVarAccessAssignment("String", "String");
		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);

		Scope bs = moduleBlock.getScope();
		assertNotNull("Scope of module is null, but should be anything else!", moduleDeclaration.getScope());
		assertNotNull("Scope of moduleBlock is null, but should be anything else!", bs);

		assertEquals(
		        "Declaration doesn't match with the orginal one.",
		        moduleBlock.getDeclarations().get(0),
		        (VariableDeclaration) generated[0]);
		assertEquals(
		        "Declaration doesn't match with the orginal one.",
		        moduleBlock.getDeclarations().get(1),
		        (VariableDeclaration) generated[1]);

		for (Declaration dc : moduleBlock.getDeclarations()) {
			Identifier obj_id = dc.getIdentifier();
			Scope obj_s = dc.getScope();
			assertNotNull("Identifiert shouldn't be null.", obj_id);
			assertNotNull("Scope shouldn't be null.", obj_s);
			assertEquals("One var declaration is not in the scope of the moduleBlock.", obj_s, bs);
		}

		ArrayList<Statement> statements = (ArrayList<Statement>) moduleBlock.getStatements();

		Assignment assignment = null;
		if (statements.get(0) instanceof Assignment) {
			assignment = (Assignment) statements.get(0);
		} else
			fail("Statement is not an AssignmentStatement.");

		assertEquals("Statement doesn't match with the orginal one.", statements.get(0), (Assignment) generated[2]);

		if (!(assignment.getLeft() instanceof VariableAccess) && !(assignment.getRight() instanceof VariableAccess)) {
			fail("Left or Right of assignment are not from type VariableAccess");
		}
		VariableAccess left = (VariableAccess) assignment.getLeft();
		VariableAccess right = (VariableAccess) assignment.getRight();

		assertEquals("Assignment is not in the moduleBlock scope", assignment.getScope(), bs);
		assertNotNull("Type of left Expression is null, but should be string.", left.getType());
		assertNotNull("Type of right Expression is null, but should be string.", right.getType());
		assertEquals("Type of left Expression is not String", left.getType(), CoreClasses.stringType());
		assertEquals("Type of right Expression is not String", right.getType(), CoreClasses.stringType());
	}

	@Test
	public void assignmentTest01_2() {
		// variable access = variable access
		// Ill-Type Test #1
		exception.expect(TypeMismatchException.class);
		makeVarAccessVarAccessAssignment("String", "Int");
		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test
	public void assignmentTest01_3() {
		// variable access = variable access
		// Ill-Type Test #2
		exception.expect(TypeMismatchException.class);
		makeVarAccessVarAccessAssignment("Float", "String");
		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test
	public void assignmentTest01_4() {
		// variable access = variable access
		// Ill-Type Test #3
		exception.expect(TypeMismatchException.class);
		makeVarAccessVarAccessAssignment("Int", "Float");
		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	private void makeVarAccessMemberAccessAssignment(String LeftResolvableSymbol, boolean otherWay) {
		/*
		 * Makes a the declarations and Access objects needed for an variable access = member access AssignmentTest02
		 * and appends it to the Blockclass
		 */

		// Declaration of a class with a member as attribute.

		moduleBlock.addDeclaration(classPerson);

		// Declaration of a variable
		VariableDeclaration v1 =
		        new VariableDeclaration(buildPosition(), new Identifier("var1"), new ResolvableIdentifier(
		                LeftResolvableSymbol), DeclarationType.VARIABLE);
		moduleBlock.addDeclaration(v1);

		VariableAccess val = new VariableAccess(buildPosition(), new ResolvableIdentifier("var1"));
		VariableAccess classvar = new VariableAccess(buildPosition(), new ResolvableIdentifier("Person"));
		VariableAccess classaccess = new VariableAccess(buildPosition(), new ResolvableIdentifier("name"));

		MemberAccess mar = new MemberAccess(buildPosition(), classvar, classaccess);
		Assignment a;
		if (otherWay)
			a = new Assignment(buildPosition(), mar, val);
		else
			a = new Assignment(buildPosition(), val, mar);
		moduleBlock.addStatement(a);
	}

	@Test
	public void assignmentTest02_2() {
		// variable access = member access
		// Ill-Type Test #1
		exception.expect(TypeMismatchException.class);
		makeVarAccessMemberAccessAssignment("Float", false);
		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test
	public void assignmentTest02_3() {
		// variable access = member access
		// Ill-Type Test #2
		exception.expect(TypeMismatchException.class);
		makeVarAccessMemberAccessAssignment("Int", false);
		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test
	public void assignmentTest02_4() {
		// variable access = member access
		// Ill-Type Test #3
		exception.expect(TypeMismatchException.class);
		makeVarAccessMemberAccessAssignment("Bool", false);
		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	private void variableAccessConditionalExpression_helper(boolean otherway) {
		moduleBlock.addDeclaration(strVariable);
		Assignment a =
		        (otherway) ? new Assignment(buildPosition(), strVariableAccess, ConditionStringString) : new Assignment(
		                buildPosition(), ConditionStringString, strVariableAccess);
		moduleBlock.addStatement(a);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);

		Statement s = moduleBlock.getStatements().get(0);

		assertEquals(
		        "Statement doesn't match with the orginal one!",
		        moduleBlock.getStatements().get(0),
		        (Assignment) s);

		Scope mbs = moduleBlock.getScope();
		assertEquals(
		        "ConditionalExpression scope doesn't match with block scope!",
		        mbs,
		        ConditionStringString.getScope());
		assertEquals("Assignment scope doesn't match with block scope!", mbs, a.getScope());
		assertEquals("booleanLiteral scope doesn't match with block scope!", mbs, booleanLiteral.getScope());
		assertEquals("strLiteral scope doesn't match with block scope!", mbs, strLiteral.getScope());
		assertEquals("strVariableAccess scope doesn't match with block scope!", mbs, strVariableAccess.getScope());
		assertEquals("strVariable scope doesn't match with block scope!", mbs, strVariable.getScope());
		VariableAccess v = (VariableAccess) a.getLeft();
		assertTrue("VariableAccess Left Expression is not marked as L-Value", v.getLValue());
		assertTrue(
		        "Type of Left Expression from VariableAccess is not string!",
		        v.getType() == CoreClasses.stringType());

	}

	@Test
	public void assignmentTest03_1() {
		// variable access = conditional expression
		variableAccessConditionalExpression_helper(true);
	}

	@Test
	public void assignmentTest03_2() {
		// variable access = conditional expression
		// Ill-Type Test #1
		exception.expect(TypeMismatchException.class);
		moduleBlock.addDeclaration(strVariable);
		Assignment a = new Assignment(buildPosition(), strVariableAccess, ConditionIntInt);
		moduleBlock.addStatement(a);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test
	public void assignmentTest03_3() {
		// variable access = conditional expression
		// Ill-Type Test #2
		exception.expect(TypeMismatchException.class);
		ConditionalExpression CE =
		        new ConditionalExpression(buildPosition(), booleanLiteral, floatLiteral, floatLiteral);
		moduleBlock.addDeclaration(strVariable);
		Assignment a = new Assignment(buildPosition(), strVariableAccess, CE);
		moduleBlock.addStatement(a);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test
	public void assignmentTest03_4() {
		// conditional expression = variable access (is not allowed)
		exception.expect(InvalidExpressionException.class);
		variableAccessConditionalExpression_helper(false);
	}

	private FunctionCall VariableAccessFunctionCallHelper() {
		// build function with two parameters ( both int) and return type int:
		Block functionblock = new Block(buildPosition());
		functionblock.addStatement(new ReturnStatement(buildPosition(), new IntegerLiteral(buildPosition(), 1337)));
		ArrayList<VariableDeclaration> params = new ArrayList<VariableDeclaration>();
		params.add(new VariableDeclaration(buildPosition(), new Identifier("a"), new ResolvableIdentifier("Int"),
		        DeclarationType.PARAMETER));
		params.add(new VariableDeclaration(buildPosition(), new Identifier("b"), new ResolvableIdentifier("Int"),
		        DeclarationType.PARAMETER));
		FunctionDeclaration fd =
		        new FunctionDeclaration(buildPosition(), new Identifier("testfunction"), functionblock, params,
		                new ResolvableIdentifier("Int"));
		moduleBlock.addDeclaration(fd);
		// Make a list with given parameters:
		ArrayList<Expression> values = new ArrayList<Expression>();
		values.add(intLiteral);
		values.add(intLiteral);
		return new FunctionCall(buildPosition(), new ResolvableIdentifier("testfunction"), values);
	}

	@Test
	public void assignmentTest04_1() {
		// variable access = function call
		moduleBlock.addDeclaration(intVariable);
		Assignment a = new Assignment(buildPosition(), intVariableAccess, VariableAccessFunctionCallHelper());
		moduleBlock.addStatement(a);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);

	}

	@Test
	public void assignmentTest04_2() {
		// variable access = function call
		// Ill-Type Test #1 (Int return on a string variable)
		exception.expect(TypeMismatchException.class);
		moduleBlock.addDeclaration(strVariable);
		Assignment a = new Assignment(buildPosition(), strVariableAccess, VariableAccessFunctionCallHelper());
		moduleBlock.addStatement(a);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);

	}

	@Test
	public void assignmentTest04_3() {
		// variable access = function call
		// (Int return on a float variable) is not allowed
		exception.expect(TypeMismatchException.class);
		moduleBlock.addDeclaration(floatVariable);
		Assignment a = new Assignment(buildPosition(), floatVariableAccess, VariableAccessFunctionCallHelper());
		moduleBlock.addStatement(a);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);

	}

	@Test
	public void assignmentTest04_4() {
		// variable access = function call
		// Ill-Type Test #3(Int return on a boolean variable)
		exception.expect(TypeMismatchException.class);
		moduleBlock.addDeclaration(booleanVariable);
		Assignment a = new Assignment(buildPosition(), booleanVariableAccess, VariableAccessFunctionCallHelper());
		moduleBlock.addStatement(a);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);

	}

	@Test
	public void assignmentTest05_2() {
		// member access = variable access
		// Ill-Type Test #1
		exception.expect(TypeMismatchException.class);
		makeVarAccessMemberAccessAssignment("Float", true);
		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test
	public void assignmentTest05_3() {
		// member access = variable access
		// Ill-Type Test #2
		exception.expect(TypeMismatchException.class);
		makeVarAccessMemberAccessAssignment("Int", true);
		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test
	public void assignmentTest05_4() {
		// member access = variable access
		// Ill-Type Test #3
		exception.expect(TypeMismatchException.class);
		makeVarAccessMemberAccessAssignment("Bool", true);
		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	private void makeMemberAccessMemberAccessAssignment() {
		/*
		 * Makes a the declarations and Access objects needed for an member access = member access AssignmentTest06 and
		 * appends it to the Block
		 */

		moduleBlock.addDeclaration(classStudent);
		moduleBlock.addDeclaration(classPerson);

		// Declaration of variables myPerson and myStudent
		moduleBlock.addDeclaration(classPersonVarDecl);
		moduleBlock.addDeclaration(classStudentVarDecl);

		// Create VariableAccess objects.
		VariableAccess classvar = new VariableAccess(buildPosition(), new ResolvableIdentifier("myPerson"));
		VariableAccess classvar2 = new VariableAccess(buildPosition(), new ResolvableIdentifier("myStudent"));

		// Make the Assignment with from two MemberAccesses.
		MemberAccess ma1 =
		        new MemberAccess(buildPosition(), classvar, new VariableAccess(buildPosition(),
		                new ResolvableIdentifier("name")));
		MemberAccess ma2 =
		        new MemberAccess(buildPosition(), classvar2, new VariableAccess(buildPosition(),
		                new ResolvableIdentifier("name")));
		Assignment a = new Assignment(buildPosition(), ma1, ma2);
		moduleBlock.addStatement(a);

	}

	@Test
	public void assignmentTest06() {
		// member access = member access
		makeMemberAccessMemberAccessAssignment();

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test
	public void assignmentTest07() {
		// member access = conditional expression
		moduleBlock.addDeclaration(classStudent);
		moduleBlock.addDeclaration(classStudentVarDecl);

		VariableAccess classvar = new VariableAccess(buildPosition(), new ResolvableIdentifier("myStudent"));

		MemberAccess ma =
		        new MemberAccess(buildPosition(), classvar, new VariableAccess(buildPosition(),
		                new ResolvableIdentifier("name")));
		Assignment a = new Assignment(buildPosition(), ma, ConditionStringString);
		moduleBlock.addStatement(a);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);

		Scope sc = a.getScope();
		assertNotNull(sc);
		assertEquals(sc, moduleBlock.getScope());
		assertTrue(a.getRight() instanceof ConditionalExpression);
		assertTrue(a.getLeft() instanceof MemberAccess);
		assertTrue(a.getLeft().getType().equals(a.getRight().getType()));
	}

	@Test
	public void assignmentTest08() {
		// member access = function call

		moduleBlock.addDeclaration(classStudent);
		moduleBlock.addDeclaration(classStudentVarDecl);
		moduleBlock.addDeclaration(functionDeclarationStringReturnString);

		VariableAccess classvar = new VariableAccess(buildPosition(), new ResolvableIdentifier("myStudent"));

		MemberAccess ma =
		        new MemberAccess(buildPosition(), classvar, new VariableAccess(buildPosition(),
		                new ResolvableIdentifier("name")));

		ArrayList<Expression> values = new ArrayList<Expression>();
		values.add(strLiteral);
		FunctionCall fc = new FunctionCall(buildPosition(), new ResolvableIdentifier("StringReturnString"), values);

		Assignment a = new Assignment(buildPosition(), ma, fc);
		moduleBlock.addStatement(a);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);

		Scope sc = a.getScope();
		assertNotNull(sc);
		assertEquals(sc, moduleBlock.getScope());
		assertTrue(a.getLeft() instanceof MemberAccess);
		assertTrue(a.getRight() instanceof FunctionCall);
		assertTrue(a.getLeft().getType().equals(a.getRight().getType()));
	}

	@Test
	public void assignmentTest09() {
		// function call = variable access
		exception.expect(InvalidExpressionException.class);
		moduleBlock.addDeclaration(intVariable);
		Assignment a = new Assignment(buildPosition(), VariableAccessFunctionCallHelper(), intVariableAccess);
		moduleBlock.addStatement(a);
		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	// MEMBER ACCESS

	@Test(expected = UnknownIdentifierException.class)
	public void memberAccessTest01() {
		// x 1: left ist keine classDeclaration
		FunctionDeclaration functionDeclaration =
		        new FunctionDeclaration(buildPosition(), new Identifier("functionDeclaration"), new Block(
		                buildPosition()), new ArrayList<VariableDeclaration>(), new ResolvableIdentifier("String"));

		VariableAccess varAccess = new VariableAccess(buildPosition(), new ResolvableIdentifier("functionDeclaration"));

		MemberAccess memberAccess = new MemberAccess(buildPosition(), varAccess, intVariableAccess);

		WhileLoop loop = new WhileLoop(buildPosition(), memberAccess, new Block(buildPosition()));

		moduleBlock.addDeclaration(intVariable);
		moduleBlock.addDeclaration(functionDeclaration);
		moduleBlock.addStatement(loop);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = TypeMismatchException.class)
	public void memberAccessTest02() {
		// x 2: left löst nach classDeclaration auf, right ist conditional
		ConditionalExpression conditionalExpression =
		        new ConditionalExpression(buildPosition(), booleanLiteral, strLiteral, strLiteral);

		VariableAccess varAccess = new VariableAccess(buildPosition(), new ResolvableIdentifier("classDeclaration"));

		MemberAccess memberAccess = new MemberAccess(buildPosition(), varAccess, conditionalExpression);

		WhileLoop loop = new WhileLoop(buildPosition(), memberAccess, new Block(buildPosition()));

		moduleBlock.addDeclaration(classDeclaration);
		moduleBlock.addStatement(loop);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = TypeMismatchException.class)
	public void memberAccessTest03() {
		// x 3: left löst nach classDeclaration auf, right ist boolean
		VariableAccess varAccess = new VariableAccess(buildPosition(), new ResolvableIdentifier("classDeclaration"));

		MemberAccess memberAccess = new MemberAccess(buildPosition(), varAccess, booleanLiteral);

		WhileLoop loop = new WhileLoop(buildPosition(), memberAccess, new Block(buildPosition()));

		moduleBlock.addDeclaration(classDeclaration);
		moduleBlock.addStatement(loop);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = TypeMismatchException.class)
	public void memberAccessTest04() {
		// x 4: left löst nach classDeclaration auf, right ist string
		VariableAccess varAccess = new VariableAccess(buildPosition(), new ResolvableIdentifier("classDeclaration"));

		MemberAccess memberAccess = new MemberAccess(buildPosition(), varAccess, strLiteral);

		WhileLoop loop = new WhileLoop(buildPosition(), memberAccess, new Block(buildPosition()));

		moduleBlock.addDeclaration(classDeclaration);
		moduleBlock.addStatement(loop);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = TypeMismatchException.class)
	public void memberAccessTest05() {
		// x 5: left löst nach classDeclaration auf, right ist float
		VariableAccess varAccess = new VariableAccess(buildPosition(), new ResolvableIdentifier("classDeclaration"));

		MemberAccess memberAccess = new MemberAccess(buildPosition(), varAccess, floatLiteral);

		WhileLoop loop = new WhileLoop(buildPosition(), memberAccess, new Block(buildPosition()));

		moduleBlock.addDeclaration(classDeclaration);
		moduleBlock.addStatement(loop);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = TypeMismatchException.class)
	public void memberAccessTest06() {
		// x 6: left löst nach classDeclaration auf, right ist int
		VariableAccess varAccess = new VariableAccess(buildPosition(), new ResolvableIdentifier("classDeclaration"));

		MemberAccess memberAccess = new MemberAccess(buildPosition(), varAccess, intLiteral);

		WhileLoop loop = new WhileLoop(buildPosition(), memberAccess, new Block(buildPosition()));

		moduleBlock.addDeclaration(classDeclaration);
		moduleBlock.addStatement(loop);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = UnknownIdentifierException.class)
	public void memberAccessTest07() {
		// x 7: left löst nach classDeclaration auf, right ist nicht vorhandener
		// ProcedureCall
		VariableAccess varAccess = new VariableAccess(buildPosition(), new ResolvableIdentifier("classDeclaration"));
		FunctionCall functionCall =
		        new FunctionCall(buildPosition(), new ResolvableIdentifier("invalid"), new ArrayList<Expression>());

		MemberAccess memberAccess = new MemberAccess(buildPosition(), varAccess, functionCall);

		WhileLoop loop = new WhileLoop(buildPosition(), memberAccess, new Block(buildPosition()));

		moduleBlock.addDeclaration(classDeclaration);
		moduleBlock.addStatement(loop);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = UnknownIdentifierException.class)
	public void memberAccessTest08() {
		// x 8: left löst nach classDeclaration auf, right ist nicht vorhandener
		// variableAccess
		VariableAccess varAccess = new VariableAccess(buildPosition(), new ResolvableIdentifier("classDeclaration"));
		VariableAccess rightVarAccess = new VariableAccess(buildPosition(), new ResolvableIdentifier("invalid"));

		MemberAccess memberAccess = new MemberAccess(buildPosition(), varAccess, rightVarAccess);

		WhileLoop loop = new WhileLoop(buildPosition(), memberAccess, new Block(buildPosition()));

		moduleBlock.addDeclaration(classDeclaration);
		moduleBlock.addStatement(loop);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = TypeMismatchException.class)
	public void memberAccessTest09() {
		// y 9: left löst nach classDeclaration auf, right ist vorhandener
		// ProcedureCall
		VariableAccess varAccess = new VariableAccess(buildPosition(), new ResolvableIdentifier("classDeclaration"));
		FunctionCall functionCall =
		        new FunctionCall(buildPosition(), new ResolvableIdentifier("classProcedureDeclaration"),
		                new ArrayList<Expression>());

		MemberAccess memberAccess = new MemberAccess(buildPosition(), varAccess, functionCall);

		WhileLoop loop = new WhileLoop(buildPosition(), memberAccess, new Block(buildPosition()));

		moduleBlock.addDeclaration(classDeclaration);
		moduleBlock.addStatement(loop);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = TypeMismatchException.class)
	public void memberAccessTest10() {
		// y 10: left löst nach classDeclaration auf, right ist vorhandener
		// variableAccess
		VariableAccess varAccess = new VariableAccess(buildPosition(), new ResolvableIdentifier("classDeclaration"));
		VariableAccess rightVarAccess =
		        new VariableAccess(buildPosition(), new ResolvableIdentifier("classVariableDeclaration"));

		MemberAccess memberAccess = new MemberAccess(buildPosition(), varAccess, rightVarAccess);

		WhileLoop loop = new WhileLoop(buildPosition(), memberAccess, new Block(buildPosition()));

		moduleBlock.addDeclaration(classDeclaration);
		moduleBlock.addStatement(loop);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	// FUNCTION CALL
	@Test
	public void functionCallTest() {
		// FunctionCall with float returnType
		Block functionBlock = new Block(nextPosition());
		functionBlock.addStatement(new ReturnStatement(nextPosition(), floatLiteral));
		FunctionDeclaration functionDeclaration =
		        new FunctionDeclaration(nextPosition(), new Identifier("functionWithReturn"), functionBlock,
		                new ArrayList<VariableDeclaration>(), new ResolvableIdentifier("Float"));
		FunctionCall procedureCall =
		        new FunctionCall(nextPosition(), new ResolvableIdentifier("functionWithReturn"),
		                new ArrayList<Expression>());

		moduleBlock.addDeclaration(functionDeclaration);
		moduleBlock.addStatement(procedureCall);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test
	public void functionCallWithParamsTest() {
		// Functioncall with int Returntype and matching int Paramters
		List<VariableDeclaration> variables = new ArrayList<VariableDeclaration>();
		variables.add(intVariable);
		List<Expression> parameters = new ArrayList<Expression>();
		parameters.add(intLiteral);
		Block functionBlock = new Block(nextPosition());
		functionBlock.addStatement(new ReturnStatement(nextPosition(), intLiteral));
		FunctionDeclaration functionDeclaration =
		        new FunctionDeclaration(nextPosition(), new Identifier("functionWithParameters"), functionBlock,
		                variables, new ResolvableIdentifier("Int"));
		FunctionCall procedureCall =
		        new FunctionCall(nextPosition(), new ResolvableIdentifier("functionWithParameters"), parameters);

		moduleBlock.addDeclaration(functionDeclaration);
		moduleBlock.addStatement(procedureCall);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = TypeMismatchException.class)
	public void functionCallWithParamsTest02() {
		// Parameters from Declaration and Call doesn't match
		List<VariableDeclaration> variables = new ArrayList<VariableDeclaration>();
		variables.add(intVariable);
		List<Expression> parameters = new ArrayList<Expression>();
		parameters.add(strLiteral);
		Block functionBlock = new Block(nextPosition());
		functionBlock.addStatement(new ReturnStatement(nextPosition(), intLiteral));

		FunctionDeclaration functionDeclaration =
		        new FunctionDeclaration(nextPosition(), new Identifier("functionWithParameters02"), functionBlock,
		                variables, new ResolvableIdentifier("Int"));
		FunctionCall procedureCall =
		        new FunctionCall(nextPosition(), new ResolvableIdentifier("functionWithParameters02"), parameters);

		moduleBlock.addDeclaration(functionDeclaration);
		moduleBlock.addStatement(procedureCall);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = TypeMismatchException.class)
	public void functionCallWithParamsTest03() {
		// Count of paramters in the FunctionCall does not match the declaration
		List<VariableDeclaration> variables = new ArrayList<VariableDeclaration>();
		variables.add(intVariable);
		variables.add(floatVariable);
		List<Expression> parameters = new ArrayList<Expression>();
		parameters.add(intLiteral);
		Block functionBlock = new Block(nextPosition());
		functionBlock.addStatement(new ReturnStatement(nextPosition(), intLiteral));

		FunctionDeclaration functionDeclaration =
		        new FunctionDeclaration(nextPosition(), new Identifier("functionWithParameters03"), functionBlock,
		                variables, new ResolvableIdentifier("Int"));
		FunctionCall procedureCall =
		        new FunctionCall(nextPosition(), new ResolvableIdentifier("functionWithParameters03"), parameters);

		moduleBlock.addDeclaration(functionDeclaration);
		moduleBlock.addStatement(procedureCall);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = TypeMismatchException.class)
	public void functionCallWithParamsTest04() {
		// Count of paramters in the FunctionCall does not match the declaration
		List<VariableDeclaration> variables = new ArrayList<VariableDeclaration>();
		variables.add(floatVariable);
		List<Expression> parameters = new ArrayList<Expression>();
		parameters.add(floatLiteral);
		parameters.add(strLiteral);
		Block functionBlock = new Block(nextPosition());
		functionBlock.addStatement(new ReturnStatement(nextPosition(), intLiteral));

		FunctionDeclaration functionDeclaration =
		        new FunctionDeclaration(nextPosition(), new Identifier("functionWithParameters04"), functionBlock,
		                variables, new ResolvableIdentifier("Int"));
		FunctionCall procedureCall =
		        new FunctionCall(nextPosition(), new ResolvableIdentifier("functionWithParameters04"), parameters);

		moduleBlock.addDeclaration(functionDeclaration);
		moduleBlock.addStatement(procedureCall);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = TypeMismatchException.class)
	public void functionCallWithParamsTest05() {
		// Count of paramters in the FunctionCall does not match the declaration
		List<VariableDeclaration> variables = new ArrayList<VariableDeclaration>();
		List<Expression> parameters = new ArrayList<Expression>();
		parameters.add(floatLiteral);
		parameters.add(strLiteral);
		Block functionBlock = new Block(nextPosition());
		functionBlock.addStatement(new ReturnStatement(nextPosition(), floatLiteral));

		FunctionDeclaration functionDeclaration =
		        new FunctionDeclaration(nextPosition(), new Identifier("functionWithParameters05"), functionBlock,
		                variables, new ResolvableIdentifier("Float"));
		FunctionCall procedureCall =
		        new FunctionCall(nextPosition(), new ResolvableIdentifier("functionWithParameters05"), parameters);

		moduleBlock.addDeclaration(functionDeclaration);
		moduleBlock.addStatement(procedureCall);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = TypeMismatchException.class)
	public void functionCallWithParamsTest06() {
		// Count of paramters in the FunctionCall does not match the declaration
		List<VariableDeclaration> variables = new ArrayList<VariableDeclaration>();
		List<Expression> parameters = new ArrayList<Expression>();
		parameters.add(floatLiteral);
		parameters.add(strLiteral);
		Block functionBlock = new Block(nextPosition());
		functionBlock.addStatement(new ReturnStatement(nextPosition(), strLiteral));

		FunctionDeclaration functionDeclaration =
		        new FunctionDeclaration(nextPosition(), new Identifier("functionWithParameters06"), functionBlock,
		                variables, new ResolvableIdentifier("String"));
		FunctionCall procedureCall =
		        new FunctionCall(nextPosition(), new ResolvableIdentifier("functionWithParameters06"), parameters);

		moduleBlock.addDeclaration(functionDeclaration);
		moduleBlock.addStatement(procedureCall);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = TypeMismatchException.class)
	public void functionCallWithParamsTest07() {
		// ReturnStatement type doesn't match Return Type in Declaration
		Block functionBlock = new Block(nextPosition());
		functionBlock.addStatement(new ReturnStatement(nextPosition(), strLiteral));

		FunctionDeclaration functionDeclaration =
		        new FunctionDeclaration(nextPosition(), new Identifier("functionWithParameters06"), functionBlock,
		                new ArrayList<VariableDeclaration>(), new ResolvableIdentifier("Int"));
		FunctionCall procedureCall =
		        new FunctionCall(nextPosition(), new ResolvableIdentifier("functionWithParameters06"),
		                new ArrayList<Expression>());

		moduleBlock.addDeclaration(functionDeclaration);
		moduleBlock.addStatement(procedureCall);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	// PROCEDURE CALL

	@Test
	public void procedureCallTest() {
		// ProcedureCall with matching Declaration
		ProcedureDeclaration procedureDeclaration =
		        new ProcedureDeclaration(nextPosition(), new Identifier("procedure"), new Block(nextPosition()),
		                new ArrayList<VariableDeclaration>());
		FunctionCall procedureCall =
		        new FunctionCall(nextPosition(), new ResolvableIdentifier("procedure"), new ArrayList<Expression>());

		moduleBlock.addDeclaration(procedureDeclaration);
		moduleBlock.addStatement(procedureCall);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test
	public void procedureCallWithParamsTest() {
		// ProcedureCall and Declaration with matching types
		List<VariableDeclaration> variables = new ArrayList<VariableDeclaration>();
		variables.add(strVariable);
		List<Expression> parameters = new ArrayList<Expression>();
		parameters.add(strLiteral);

		ProcedureDeclaration procedureDeclaration =
		        new ProcedureDeclaration(nextPosition(), new Identifier("procedureWithParams"), new Block(
		                nextPosition()), variables);
		FunctionCall procedureCall =
		        new FunctionCall(nextPosition(), new ResolvableIdentifier("procedureWithParams"), parameters);

		moduleBlock.addDeclaration(procedureDeclaration);
		moduleBlock.addStatement(procedureCall);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = TypeMismatchException.class)
	public void procedureCallWithParamsTest02() {
		// Parameters from Declaration and Call doesn't match
		List<VariableDeclaration> variables = new ArrayList<VariableDeclaration>();
		variables.add(strVariable);
		List<Expression> parameters = new ArrayList<Expression>();
		parameters.add(floatLiteral);

		ProcedureDeclaration procedureDeclaration =
		        new ProcedureDeclaration(nextPosition(), new Identifier("procedureWithParams02"), new Block(
		                nextPosition()), variables);
		FunctionCall procedureCall =
		        new FunctionCall(nextPosition(), new ResolvableIdentifier("procedureWithParams02"), parameters);

		moduleBlock.addDeclaration(procedureDeclaration);
		moduleBlock.addStatement(procedureCall);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = TypeMismatchException.class)
	public void procedureCallWithParamsTest03() {
		// Count of paramters in the ProcedureCall does not match the
		// declaration
		List<VariableDeclaration> variables = new ArrayList<VariableDeclaration>();
		variables.add(strVariable);
		variables.add(intVariable);
		List<Expression> parameters = new ArrayList<Expression>();
		parameters.add(strLiteral);

		ProcedureDeclaration procedureDeclaration =
		        new ProcedureDeclaration(nextPosition(), new Identifier("procedureWithParams03"), new Block(
		                nextPosition()), variables);
		FunctionCall procedureCall =
		        new FunctionCall(nextPosition(), new ResolvableIdentifier("procedureWithParams03"), parameters);

		moduleBlock.addDeclaration(procedureDeclaration);
		moduleBlock.addStatement(procedureCall);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = TypeMismatchException.class)
	public void procedureCallWithParamsTest04() {
		// Count of paramters in the ProcedureCall does not match the
		// declaration
		List<VariableDeclaration> variables = new ArrayList<VariableDeclaration>();
		variables.add(strVariable);
		List<Expression> parameters = new ArrayList<Expression>();
		parameters.add(strLiteral);
		parameters.add(intLiteral);

		ProcedureDeclaration procedureDeclaration =
		        new ProcedureDeclaration(nextPosition(), new Identifier("procedureWithParams04"), new Block(
		                nextPosition()), variables);
		FunctionCall procedureCall =
		        new FunctionCall(nextPosition(), new ResolvableIdentifier("procedureWithParams04"), parameters);

		moduleBlock.addDeclaration(procedureDeclaration);
		moduleBlock.addStatement(procedureCall);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = TypeMismatchException.class)
	public void procedureCallWithParamsTest05() {
		// Count of paramters in the ProcedureCall does not match the
		// declaration
		List<VariableDeclaration> variables = new ArrayList<VariableDeclaration>();
		variables.add(strVariable);
		List<Expression> parameters = new ArrayList<Expression>();

		ProcedureDeclaration procedureDeclaration =
		        new ProcedureDeclaration(nextPosition(), new Identifier("procedureWithParams05"), new Block(
		                nextPosition()), variables);
		FunctionCall procedureCall =
		        new FunctionCall(nextPosition(), new ResolvableIdentifier("procedureWithParams05"), parameters);

		moduleBlock.addDeclaration(procedureDeclaration);
		moduleBlock.addStatement(procedureCall);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = TypeMismatchException.class)
	public void procedureCallWithParamsTest06() {
		// Count of paramters in the ProcedureCall does not match the
		// declaration
		List<VariableDeclaration> variables = new ArrayList<VariableDeclaration>();
		List<Expression> parameters = new ArrayList<Expression>();
		parameters.add(intLiteral);

		ProcedureDeclaration procedureDeclaration =
		        new ProcedureDeclaration(nextPosition(), new Identifier("procedureWithParams06"), new Block(
		                nextPosition()), variables);
		FunctionCall procedureCall =
		        new FunctionCall(nextPosition(), new ResolvableIdentifier("procedureWithParams06"), parameters);

		moduleBlock.addDeclaration(procedureDeclaration);
		moduleBlock.addStatement(procedureCall);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	// CONDITIONAL STATEMENT
	@Test
	public void conditionalTestBooleanCondition() {
		ConditionalStatement conditionalStatement =
		        new ConditionalStatement(buildPosition(), booleanLiteral, new Block(buildPosition()), new Block(
		                buildPosition()));

		moduleBlock.addStatement(conditionalStatement);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = TypeMismatchException.class)
	public void conditionalTestNotBooleanCondition() {
		ConditionalStatement conditionalStatement =
		        new ConditionalStatement(buildPosition(), strLiteral, new Block(buildPosition()), new Block(
		                buildPosition()));

		moduleBlock.addStatement(conditionalStatement);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	// CONDITIONAL EXPRESSION

	@Test
	public void conditionalExpressionTestCorrectConditionalExpression() {
		// thenExpression matches elseExpression
		ConditionalExpression conditionalExpression =
		        new ConditionalExpression(buildPosition(), booleanLiteral, strLiteral, strLiteral);
		Assignment conditionalAssignment = new Assignment(buildPosition(), strVariableAccess, conditionalExpression);

		moduleBlock.addDeclaration(strVariable);
		moduleBlock.addStatement(conditionalAssignment);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = TypeMismatchException.class)
	public void conditionalExpressionTestNotBooleanCondition() {
		ConditionalExpression conditionalExpression =
		        new ConditionalExpression(buildPosition(), intLiteral, strLiteral, strLiteral);
		Assignment conditionalAssignment = new Assignment(buildPosition(), strVariableAccess, conditionalExpression);

		moduleBlock.addDeclaration(strVariable);
		moduleBlock.addStatement(conditionalAssignment);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}

	@Test(expected = TypeMismatchException.class)
	public void conditionalExpressionTestThenElseMismatches() {
		// thenExpression mismatches elseExpression
		ConditionalExpression conditionalExpression =
		        new ConditionalExpression(buildPosition(), booleanLiteral, strLiteral, intLiteral);
		Assignment conditionalAssignment = new Assignment(buildPosition(), strVariableAccess, conditionalExpression);

		moduleBlock.addDeclaration(strVariable);
		moduleBlock.addStatement(conditionalAssignment);

		SPV.visit(aPackage);
		DV.visit(aPackage);
		RV.visit(aPackage);
		TCV.visit(aPackage);
	}
}
