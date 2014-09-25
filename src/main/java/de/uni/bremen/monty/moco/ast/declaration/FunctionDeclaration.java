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
package de.uni.bremen.monty.moco.ast.declaration;

import de.uni.bremen.monty.moco.ast.*;
import de.uni.bremen.monty.moco.visitor.BaseVisitor;

import java.util.List;

/** A FunctionDeclaration represents the declaration of a function in the AST.
 * <p>
 * It can be used as a returnType. */
public class FunctionDeclaration extends ProcedureDeclaration {

	/** The return returnType. */
	private ResolvableIdentifier returnTypeIdentifier;
	private TypeDeclaration returnType;

	/** Constructor.
	 * 
	 * @param position
	 *            Position of this node
	 * @param identifier
	 *            the identifier
	 * @param body
	 *            the body of this function
	 * @param parameter
	 *            the parameter of this function
	 * @param returnTypeIdentifier
	 *            the return returnType */
	public FunctionDeclaration(Position position, Identifier identifier, Block body,
	        List<VariableDeclaration> parameter, ProcedureDeclaration.DeclarationType declarationType,
	        ResolvableIdentifier returnTypeIdentifier) {
		super(position, identifier, body, parameter, declarationType);
		this.returnTypeIdentifier = returnTypeIdentifier;
	}

	public FunctionDeclaration(Position position, Identifier identifier, Block body,
	        List<VariableDeclaration> parameter, ResolvableIdentifier returnTypeIdentifier) {
		this(position, identifier, body, parameter, ProcedureDeclaration.DeclarationType.UNBOUND, returnTypeIdentifier);
	}

	/** Constructor
	 * 
	 * @param position
	 *            * Position of this node
	 * @param identifier
	 *            the identifier
	 * @param body
	 *            the body of this function
	 * @param parameter
	 *            the parameter of this function
	 * @param returnType
	 *            the return returnType */
	public FunctionDeclaration(Position position, Identifier identifier, Block body,
	        List<VariableDeclaration> parameter, ClassDeclaration returnType) {
		super(position, identifier, body, parameter);
		this.returnType = returnType;
		this.returnTypeIdentifier = ResolvableIdentifier.convert(returnType.getIdentifier());
	}

	/** get the return returnType.
	 * 
	 * @return the return returnType */
	public ResolvableIdentifier getReturnTypeIdentifier() {
		return returnTypeIdentifier;
	}

	/** {@inheritDoc} */
	@Override
	public void visit(BaseVisitor visitor) {
		visitor.visit(this);
	}

	/** {@inheritDoc} */
	@Override
	public void visitChildren(BaseVisitor visitor) {
		super.visitChildren(visitor);
	}

	/** get the returnType.
	 * 
	 * @return the returnType */
	public TypeDeclaration getReturnType() {
		return returnType;
	}

	/** set the returnType
	 * 
	 * @param returnType */
	public void setReturnType(TypeDeclaration returnType) {
		this.returnType = returnType;
	}

	/** Check equality of two types taking into account the AST object hierachy.
	 * <p>
	 * 
	 * @param other
	 *            the other TypeDeclaration to check against
	 * @return if equal */
	@Override
	public boolean matchesType(TypeDeclaration other) {
		if (!super.matchesType(other)) {
			return false;
		}
		if (!(other instanceof FunctionDeclaration)) {
			return true;
		}
		FunctionDeclaration function = (FunctionDeclaration) other;
		return returnType.matchesType(function.getReturnType());
	}
}
