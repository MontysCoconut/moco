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

public class VariableDeclaration extends Declaration {
	public enum DeclarationType {
		VARIABLE, PARAMETER, ATTRIBUTE
	}

	private ResolvableIdentifier typeIdentifier;
	private TypeDeclaration type;
	private DeclarationType declarationType;
	private boolean isGlobal;

	/* Index of the variable if it is an attribute in the class struct */
	private int attributeIndex;

	public VariableDeclaration(Position position, Identifier identifier, ResolvableIdentifier typeIdentifier,
	        DeclarationType declarationType) {
		this(position, identifier, typeIdentifier);
		this.declarationType = declarationType;
	}

	public VariableDeclaration(Position position, Identifier identifier, ResolvableIdentifier typeIdentifier) {
		super(position, identifier);
		this.typeIdentifier = typeIdentifier;
		this.declarationType = DeclarationType.VARIABLE;
		attributeIndex = -1;
	}

	/** set the declaration type */
	public void setDeclarationType(DeclarationType type) {
		this.declarationType = type;
	}

	/** get the declaration type
	 * 
	 * @return the declaration type */
	public DeclarationType getDeclarationType() {
		return declarationType;
	}

	public boolean isVariable() {
		return declarationType == DeclarationType.VARIABLE;
	}

	public boolean isParameter() {
		return declarationType == DeclarationType.PARAMETER;
	}

	public boolean isAttribute() {
		return declarationType == DeclarationType.ATTRIBUTE;
	}

	/** get the identifier of the type.
	 * 
	 * @return the type identifier */
	public ResolvableIdentifier getTypeIdentifier() {
		return typeIdentifier;
	}

	/** get the type.
	 * 
	 * @return the type */
	public TypeDeclaration getType() {
		return type;
	}

	/** set the type
	 * 
	 * @param type */
	public void setType(TypeDeclaration type) {
		this.type = type;
	}

	/** get if this variable is global.
	 * 
	 * @return if global */
	public boolean getIsGlobal() {
		return isGlobal;
	}

	/** set if this variable is global.
	 * 
	 * @param isGlobal
	 *            if global */
	public void setIsGlobal(boolean isGlobal) {
		this.isGlobal = isGlobal;
	}

	/** Get the attributeIndex. */
	public int getAttributeIndex() {
		return attributeIndex;
	}

	/** Set the attributeIndex. */
	public void setAttributeIndex(int attributeIndex) {
		this.attributeIndex = attributeIndex;
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

}
