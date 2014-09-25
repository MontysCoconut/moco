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

import java.util.*;

import de.uni.bremen.monty.moco.ast.declaration.Declaration;
import de.uni.bremen.monty.moco.ast.statement.Statement;
import de.uni.bremen.monty.moco.visitor.BaseVisitor;

/** This node represents a block.
 * 
 * A block contains declarations and statements. The declarations must be processed first. */
public class Block extends BasicASTNode {

	/** The statements. */
	private final List<Statement> statements;

	/** The declarations. */
	private final List<Declaration> declarations;

	/** Constructor.
	 * 
	 * @param position
	 *            Position of this node */
	public Block(Position position) {
		super(position);
		statements = new ArrayList<Statement>();
		declarations = new ArrayList<Declaration>();
	}

	/** @return true if the block does not have any statements or declarations. */
	public boolean isEmpty() {
		return statements.isEmpty() && declarations.isEmpty();
	}

	/** Add a statement to this block.
	 * 
	 * @param statement
	 *            the statement to add */
	public void addStatement(Statement statement) {
		statements.add(statement);
	}

	/** Add a declaration to this block.
	 * 
	 * @param declaration
	 *            the declaration to add */
	public void addDeclaration(Declaration declaration) {
		declarations.add(declaration);
	}

	/** Get the statements of this block.
	 * 
	 * @return the statements */
	public List<Statement> getStatements() {
		return statements;
	}

	/** Get the declarations of this block.
	 * 
	 * @return the declarations */
	public List<Declaration> getDeclarations() {
		return declarations;
	}

	/** {@inheritDoc} */
	@Override
	public void visit(BaseVisitor visitor) {
		visitor.visit(this);
	}

	/** {@inheritDoc} */
	@Override
	public void visitChildren(BaseVisitor visitor) {
		for (Declaration declaration : declarations) {
			visitor.visitDoubleDispatched(declaration);
		}
		for (Statement statement : statements) {
			visitor.visitDoubleDispatched(statement);
		}
	}
}
