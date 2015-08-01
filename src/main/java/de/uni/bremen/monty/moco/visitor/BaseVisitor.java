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
import de.uni.bremen.monty.moco.ast.Import;
import de.uni.bremen.monty.moco.ast.Package;
import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.ast.expression.*;
import de.uni.bremen.monty.moco.ast.expression.literal.*;
import de.uni.bremen.monty.moco.ast.statement.*;
import de.uni.bremen.monty.moco.exception.MontyBaseException;

/** This is the base-visitor to be subclassed by all visitors.
 *
 * It implements methods for all node-types that call node.visitChildren() (so the node must implement this method).
 * Override if necessary.
 *
 * If you want to visit a node in the implementation of a visit()-method just use visit(node). */
public class BaseVisitor {

	/** Count the exceptions caught in visit(ASTNode) **/
	private int errorCounter;

	/** Stop on the first error that is encountered **/
	private boolean stopOnFirstError;

	/** Comfort method to visit a node via double-dispatch.
	 *
	 * @param node
	 *            the node to visit */
	public void visitDoubleDispatched(ASTNode node) {
		try {
			onEnterEachNode(node);
			node.visit(this);
			onExitEachNode(node);
		} catch (RuntimeException exception) {
			errorCounter += 1;
			if (stopOnFirstError) {
				throw exception;
			} else {
				logError(exception);
			}
		}
	}

	/** Returns information used in the exceptions that includes the ASTNodes name (optional additional information) and
	 * (if available) the position.
	 *
	 * @return the node information */
	public String getNodeInformation(ASTNode node) {
		if (node == null) {
			return "null";
		} else if (node.getPosition() != null) {
			return String.format("%s at %s", node.toString(), node.getPosition().toString());
		}
		return node.toString();
	}

	/** Log an exception.
	 * <p>
	 * This method logs the exception or prints it if it's not a Monty exception. */
	public void logError(RuntimeException exception) {
		if (exception instanceof MontyBaseException) {
			MontyBaseException exc = (MontyBaseException) exception;
			System.err.println(String.format(
			        "%s caught error in %s: %s",
			        getClass().getSimpleName(),
			        getNodeInformation(exc.getNode()),
			        exc.getMessage()));
		} else {
			exception.printStackTrace();
		}
	}

	/** Was there an error during the execution of this visitor?
	 *
	 * @return true if no error was caught, false otherwise */
	public boolean foundError() {
		return errorCounter != 0;
	}

	// Declaration

	/** Visitor method to visit a ModuleDeclaration.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(ModuleDeclaration node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a ClassDeclaration.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(ClassDeclaration node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a AbstractGenericType.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(AbstractGenericType node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a ProcedureDeclaration.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(ProcedureDeclaration node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a VariableDeclaration.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(VariableDeclaration node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	// Expression

	/** Visitor method to visit a ConditionalExpression.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(ConditionalExpression node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a SelfExpression.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(SelfExpression node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a ParentExpression.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(ParentExpression node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a CastExpression.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(CastExpression node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a IsExpression.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(IsExpression node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a FunctionCall.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(FunctionCall node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a MemberAccess.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(MemberAccess node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a VariableAccess.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(VariableAccess node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	// Literal
	/** Visitor method to visit a ZeroExpression.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(ZeroExpression node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a BooleanLiteral.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(BooleanLiteral node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a FloatLiteral.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(FloatLiteral node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a IntegerLiteral.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(IntegerLiteral node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a StringLiteral.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(StringLiteral node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit an ArrayLiteral.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(ArrayLiteral node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a TupleLiteral.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(TupleLiteral node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a CharacterLiteral.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(CharacterLiteral node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	// Statements

	/** Visitor method to visit a Assignment.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(Assignment node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a BreakStatement.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(BreakStatement node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a SkipStatement.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(SkipStatement node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a ConditionalStatement.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(ConditionalStatement node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a ContinueStatement.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(ContinueStatement node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a ReturnStatement.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(ReturnStatement node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a WhileLoop.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(WhileLoop node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a TryStatement.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(TryStatement node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	// Other

	/** Visitor method to visit a Block.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(Block node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a Package.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(Package node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	/** Visitor method to visit a Import.
	 *
	 * @param node
	 *            the node to visit */
	public void visit(Import node) {
		onEnterChildrenEachNode(node);
		node.visitChildren(this);
		onExitChildrenEachNode(node);
	}

	protected void onEnterEachNode(ASTNode node) {

	}

	protected void onExitEachNode(ASTNode node) {

	}

	protected void onEnterChildrenEachNode(ASTNode node) {

	}

	protected void onExitChildrenEachNode(ASTNode node) {

	}

	public void setStopOnFirstError(boolean stopOnFirstError) {
		this.stopOnFirstError = stopOnFirstError;
	}
}
