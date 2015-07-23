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
import de.uni.bremen.monty.moco.ast.Package;
import de.uni.bremen.monty.moco.ast.declaration.Declaration;
import de.uni.bremen.monty.moco.ast.declaration.FunctionDeclaration;
import de.uni.bremen.monty.moco.ast.declaration.ProcedureDeclaration;
import de.uni.bremen.monty.moco.ast.statement.BreakStatement;
import de.uni.bremen.monty.moco.ast.statement.ConditionalStatement;
import de.uni.bremen.monty.moco.ast.statement.ContinueStatement;
import de.uni.bremen.monty.moco.ast.statement.ReturnStatement;
import de.uni.bremen.monty.moco.ast.statement.SkipStatement;
import de.uni.bremen.monty.moco.ast.statement.Statement;
import de.uni.bremen.monty.moco.ast.statement.WhileLoop;
import de.uni.bremen.monty.moco.exception.InvalidControlFlowException;

/** This visitor must traverse the entire AST to validate the intended AST-structure. */
public class ControlFlowVisitor extends BaseVisitor {

	/** Set to `true` if the current statement-list needs to contain a ReturnStatement */
	private boolean needsReturnStatement;

	@Override
	public void visit(Package node) {
		if (!node.isNativePackage()) {
			super.visit(node);
		}
	}

	/** {@inheritDoc} */
	@Override
	public void visit(FunctionDeclaration node) {
		needsReturnStatement = true;
		super.visit(node);
		if (needsReturnStatement) {
			throw new InvalidControlFlowException(node, "ReturnStatement needed.");
		}
	}

	/** {@inheritDoc} */
	@Override
	public void visit(Block node) {
		boolean needsReturnStatementCopy = needsReturnStatement;
		for (Declaration declaration : node.getDeclarations()) {
			visitDoubleDispatched(declaration);
		}
		needsReturnStatement = needsReturnStatementCopy;
		for (Statement statement : node.getStatements()) {
			visitDoubleDispatched(statement);
		}
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ConditionalStatement node) {
		visitDoubleDispatched(node.getCondition());
		boolean needsReturnStatementCopy = needsReturnStatement;
		visitDoubleDispatched(node.getThenBlock());
		boolean needsReturnStatementCopyThen = needsReturnStatement;
		this.needsReturnStatement = needsReturnStatementCopy;
		visitDoubleDispatched(node.getElseBlock());
		boolean needsReturnStatementCopyElse = needsReturnStatement;

		if (needsReturnStatementCopy && !needsReturnStatementCopyThen && !needsReturnStatementCopyElse) {
			needsReturnStatement = false;
		} else {
			needsReturnStatement = needsReturnStatementCopy;
		}
	}

	/** {@inheritDoc} */
	@Override
	public void visit(WhileLoop node) {
		boolean needsReturnStatementCopy = needsReturnStatement;
		super.visit(node);
		needsReturnStatement = needsReturnStatementCopy;
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ContinueStatement node) {
		super.visit(node);
		for (ASTNode currentNode = node; currentNode != null; currentNode = currentNode.getParentNode()) {
			if (currentNode instanceof WhileLoop) {
				return;
			}
		}
		throw new InvalidControlFlowException(node, "Unable to find enclosing WhileLoop.");
	}

	/** {@inheritDoc} */
	@Override
	public void visit(BreakStatement node) {
		super.visit(node);
		for (ASTNode currentNode = node; currentNode != null; currentNode = currentNode.getParentNode()) {
			if (currentNode instanceof WhileLoop) {
				node.setLoop((WhileLoop) currentNode);
				return;
			}
		}
		throw new InvalidControlFlowException(node, "Unable to find enclosing While-Loop for BreakStatement.");
	}

	/** {@inheritDoc} */
	@Override
	public void visit(SkipStatement node) {
		super.visit(node);
		for (ASTNode currentNode = node; currentNode != null; currentNode = currentNode.getParentNode()) {
			if (currentNode instanceof WhileLoop) {
				node.setLoop((WhileLoop) currentNode);
				return;
			}
		}
		throw new InvalidControlFlowException(node, "Unable to find enclosing While-Loop for SkipStatement.");
	}

	/** {@inheritDoc} */
	@Override
	public void visit(ReturnStatement node) {
		super.visit(node);
		needsReturnStatement = false;
		for (ASTNode currentNode = node; currentNode != null; currentNode = currentNode.getParentNode()) {
			if (currentNode instanceof ProcedureDeclaration) {
				return;
			}
		}

		throw new InvalidControlFlowException(node, "Unable to find enclosing Function-/ProcedureDeclaration.");
	}
}
