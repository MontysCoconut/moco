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
package de.uni.bremen.monty.moco.ast.expression;

import de.uni.bremen.monty.moco.ast.*;
import de.uni.bremen.monty.moco.ast.declaration.Declaration;
import de.uni.bremen.monty.moco.visitor.BaseVisitor;

/** VariableAccess is an expression that references a local variable or a member of an object. */
public class VariableAccess extends Expression {

	/** Identifier of the variable to access. */
	private final ResolvableIdentifier identifier;
	private Declaration declaration;

	/** Is this a L-value? */
	private boolean lValue = false;

	public VariableAccess(Position position, ResolvableIdentifier identifier) {
		super(position);
		this.identifier = identifier;
	}

	/** Get the identifier of the variable to access.
	 * 
	 * @return the identifier */
	public ResolvableIdentifier getIdentifier() {
		return identifier;
	}

	/** {@inheritDoc} */
	@Override
	public void visit(BaseVisitor visitor) {
		visitor.visit(this);
	}

	/** {@inheritDoc} */
	@Override
	public void visitChildren(BaseVisitor visitor) {
	}

	/** Mark this VariableAccess as a L-value. */
	public void setLValue() {
		lValue = true;
	}

	/** Is this a L-value?
	 * 
	 * @return if L-value */
	public boolean getLValue() {
		return lValue;
	}

	/** @return the declaration */
	public Declaration getDeclaration() {
		return declaration;
	}

	/** @param declaration
	 *            the declaration to set */
	public void setDeclaration(Declaration declaration) {
		this.declaration = declaration;
	}

	/** Get mangled identifier
	 * 
	 * @return the mangled identifier */
	public Identifier getMangledIdentifier() {
		return declaration.getMangledIdentifier();
	}
}
