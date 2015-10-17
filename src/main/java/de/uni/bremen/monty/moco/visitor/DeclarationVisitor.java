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
import de.uni.bremen.monty.moco.ast.statement.*;
import de.uni.bremen.monty.moco.ast.expression.*;
import de.uni.bremen.monty.moco.ast.expression.literal.*;
import de.uni.bremen.monty.moco.ast.Package;
import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.ast.expression.Expression;
import de.uni.bremen.monty.moco.ast.expression.FunctionCall;
import de.uni.bremen.monty.moco.ast.expression.MemberAccess;
import de.uni.bremen.monty.moco.ast.expression.SelfExpression;
import de.uni.bremen.monty.moco.ast.statement.Statement;
import de.uni.bremen.monty.moco.exception.InvalidPlaceToDeclareException;
import sun.reflect.generics.tree.ReturnType;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/** This visitor must traverse the entire AST, set up scopes and define declarations.
 * <p>
 * For every node that opens a new scope this scope must be created and assigned:
 *
 * <pre>
 * currentScope = node.setScope(new Scope(currentScope));
 * </pre>
 *
 * For every other node the associated scope must be set:
 *
 * <pre>
 * node.setScope(currentScope);
 * </pre>
 *
 * Every declaration must be defined using the currentScope. */
public class DeclarationVisitor extends BaseVisitor {

	/** The current scope for the ast node. */
	private Scope currentScope = new Scope(null);

	// Declaration

	/** {@inheritDoc} */
	@Override
	public void visit(ModuleDeclaration node) {
		if (!(node.getParentNode() instanceof Package)) {
			throw new InvalidPlaceToDeclareException(node, "A module must be the child of an package. Here: "
			        + getNodeInformation(node) + " parent: " + getNodeInformation(node.getParentNode()));
		}
		super.visit(node);
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ClassDeclaration node) {
		Block classBlock = node.getBlock();

		currentScope.define(node);
		currentScope = new ClassScope(currentScope);

		for (AbstractGenericType abstractGenericType : node.getAbstractGenericTypes()) {
			currentScope.define(abstractGenericType);
		}

		if (node != CoreClasses.voidType()) {
			if (node != CoreClasses.objectType() && node.getSuperClassIdentifiers().isEmpty()) {
				node.getSuperClassIdentifiers().add(new ResolvableIdentifier("Object"));
			}

			FunctionDeclaration defaultInitializer = buildDefaultInitializer(node);
			node.setDefaultInitializer(defaultInitializer);
			classBlock.addDeclaration(defaultInitializer);
			// The default initializer contains these statements so they should no longer be inside the class-block.
			classBlock.getStatements().clear();
		}

		super.visit(node);

		node.setScope(classBlock.getScope());
		currentScope = currentScope.getParentScope();
	}

	/** {@inheritDoc} */
	@Override
	public void visit(FunctionDeclaration node) {
		currentScope.define(node);
		currentScope = new Scope(currentScope);
		super.visit(node);
		node.setScope(node.getBody().getScope());
		currentScope = currentScope.getParentScope();
	}

	/** {@inheritDoc} */
	@Override
	public void visit(VariableDeclaration node) {
		// the parent is the Block of the ModuleDeclaration
		if (node.getParentNode().getParentNode() instanceof ModuleDeclaration) {
			node.setIsGlobal(true);
		}
		currentScope.define(node.getIdentifier(), node);
		super.visit(node);
	}

	// Other

	/** {@inheritDoc} */
	@Override
	public void visit(Block node) {
		boolean backToParentScope = false;

		if (node.getParentNode() instanceof ClassDeclaration) {
			currentScope = new ClassScope(currentScope);
			backToParentScope = true;
		} else if (!(node.getParentNode() instanceof ModuleDeclaration)) {
			currentScope = new Scope(currentScope);
			backToParentScope = true;
		}

		super.visit(node);

		if (backToParentScope) {
			currentScope = currentScope.getParentScope();
		}
	}

	@Override
	protected void onEnterChildrenEachNode(ASTNode node) {
		node.setScope(currentScope);
	}

	private FunctionDeclaration buildDefaultInitializer(ClassDeclaration node) {

		FunctionDeclaration initializer =
		        new FunctionDeclaration(node.getPosition(), new Identifier(node.getIdentifier().getSymbol()
		                + "_definit"), new Block(node.getPosition()), new ArrayList<VariableDeclaration>(),
		                FunctionDeclaration.DeclarationType.DEFAULT_INITIALIZER, (TypeDeclaration) null, false);
		initializer.setParentNode(node.getBlock());
		Block initializerBlock = initializer.getBody();
		initializerBlock.setParentNode(initializer);

		for (ResolvableIdentifier superclass : node.getSuperClassIdentifiers()) {
			SelfExpression self = new SelfExpression(node.getPosition());
			FunctionCall call =
			        new FunctionCall(node.getPosition(), new ResolvableIdentifier(superclass.getSymbol() + "_definit"),
			                new ArrayList<Expression>());
			MemberAccess defaultInitializerCall = new MemberAccess(node.getPosition(), self, call);

			self.setParentNode(defaultInitializerCall);
			call.setParentNode(defaultInitializerCall);
			defaultInitializerCall.setParentNode(initializerBlock);
			initializerBlock.addStatement(defaultInitializerCall);
		}

		for (Declaration declaration : node.getBlock().getDeclarations()) {
			if (declaration instanceof VariableDeclaration) {
				VariableDeclaration variable = (VariableDeclaration) declaration;
				SelfExpression self = new SelfExpression(node.getPosition());
				VariableAccess varAccess =
				        new VariableAccess(node.getPosition(), ResolvableIdentifier.convert(variable.getIdentifier()));
				MemberAccess access = new MemberAccess(node.getPosition(), self, varAccess);
				ZeroExpression zero = new ZeroExpression(node.getPosition(), variable.getTypeIdentifier());
				Assignment assignment = new Assignment(node.getPosition(), access, zero);

				self.setParentNode(access);
				varAccess.setParentNode(access);
				access.setParentNode(assignment);
				zero.setParentNode(assignment);
				assignment.setParentNode(initializerBlock);
				initializerBlock.addStatement(assignment);
			}
		}

		for (Statement stm : node.getBlock().getStatements()) {
			initializerBlock.addStatement(stm);
		}

		return initializer;
	}
}
