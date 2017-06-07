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
import de.uni.bremen.monty.moco.ast.types.Type;
import de.uni.bremen.monty.moco.visitor.BaseVisitor;

import java.util.Optional;


public class TypeParameterDeclaration extends TypeDeclaration {

	private Type upperTypeBound;
	private final ResolvableIdentifier upperTypeBoundIdentifier;

	public TypeParameterDeclaration(Position position, Identifier identifier, Optional<ResolvableIdentifier> typeBound) {
		super(position, identifier);
		this.upperTypeBoundIdentifier = typeBound.orElse(new ResolvableIdentifier(CoreClasses.OBJECT_SYMBOL));
	}

    @Override
	public void visit(BaseVisitor visitor) {
		visitor.visit(this);
	}

	@Override
	public void visitChildren(BaseVisitor visitor) {}

	public boolean equals(Object other) {
		if (other instanceof TypeParameterDeclaration) {
			return ((TypeParameterDeclaration) other).getIdentifier().equals(getIdentifier());
		}
		return false;
	}

	public Type getUpperTypeBound() {
		return upperTypeBound;
	}

	@Override
	public String toString() {
		return getIdentifier().getSymbol();
	}

	public ResolvableIdentifier getUpperTypeBoundIdentifier() {
		return upperTypeBoundIdentifier;
	}

	public void setUpperTypeBound(Type upperTypeBound) {
		this.upperTypeBound = upperTypeBound;
	}
}
