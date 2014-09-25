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
import de.uni.bremen.monty.moco.ast.Package;
import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.exception.InvalidPlaceToDeclareException;

import java.util.List;
import java.util.ArrayList;
import java.util.Arrays;

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
		if (!(node.getParentNode().getParentNode() instanceof ModuleDeclaration)) {
			throw new InvalidPlaceToDeclareException(node, "A class may only be declared in a module.");
		}
		Block classBlock = node.getBlock();

		currentScope.define(node);
		currentScope = new ClassScope(currentScope);

		// These are not boxed yet. So they cant inherit from object and cant have initializers.
		List<ClassDeclaration> treatSpecial =
		        Arrays.asList(CoreClasses.stringType(), CoreClasses.arrayType(), CoreClasses.voidType());

		if (!treatSpecial.contains(node)) {
			if (node != CoreClasses.objectType() && node.getSuperClassIdentifiers().isEmpty()) {
				node.getSuperClassIdentifiers().add(new ResolvableIdentifier("Object"));
			}

			ProcedureDeclaration defaultInitializer = buildDefaultInitializer(node);
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
	public void visit(ProcedureDeclaration node) {
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

	private ProcedureDeclaration buildDefaultInitializer(ClassDeclaration node) {

		ProcedureDeclaration initializer =
		        new ProcedureDeclaration(node.getPosition(), new Identifier(node.getIdentifier().getSymbol()
		                + "_definit"), new Block(node.getPosition()), new ArrayList<VariableDeclaration>(),
		                ProcedureDeclaration.DeclarationType.INITIALIZER);
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

		for (Statement stm : node.getBlock().getStatements()) {
			initializerBlock.addStatement(stm);
		}

		return initializer;
	}
}
