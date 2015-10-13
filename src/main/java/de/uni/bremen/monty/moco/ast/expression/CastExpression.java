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
import de.uni.bremen.monty.moco.visitor.BaseVisitor;

public class CastExpression extends Expression {

	private Expression expression;
	private ResolvableIdentifier castIdentifier;
	private Expression inferTypeParameterFrom;
	private Expression inferTypeFrom;
	private boolean unchecked = false;

	public CastExpression(Position position, Expression expression, ResolvableIdentifier castIdentifier) {
		super(position);
		this.expression = expression;
		this.castIdentifier = castIdentifier;
		this.inferTypeParameterFrom = null;
	}

	public CastExpression(Position position, Expression expression, ResolvableIdentifier castIdentifier,
	        Expression inferTypeParameterFrom) {
		super(position);
		this.expression = expression;
		this.castIdentifier = castIdentifier;
		this.inferTypeParameterFrom = inferTypeParameterFrom;
	}

	public CastExpression(Position position, Expression expression, Expression inferTypeFrom) {
		super(position);
		this.expression = expression;
		this.inferTypeFrom = inferTypeFrom;
	}

	public CastExpression(Position position, Expression expression, Expression inferTypeFrom, boolean unchecked) {
		super(position);
		this.expression = expression;
		this.inferTypeFrom = inferTypeFrom;
		this.unchecked = unchecked;
	}

	public ResolvableIdentifier getCastIdentifier() {
		return castIdentifier;
	}

	public Expression getExpression() {
		return expression;
	}

	public boolean typeParameterMustBeInferred() {
		return inferTypeParameterFrom != null;
	}

	public void inferTypeParameter() {
		if (inferTypeParameterFrom.getType().getIdentifier() instanceof ResolvableIdentifier) {
			ResolvableIdentifier otherIdent = (ResolvableIdentifier) inferTypeParameterFrom.getType().getIdentifier();
			int max = Math.min(otherIdent.getGenericTypes().size(), castIdentifier.getGenericTypes().size());
			for (int i = 0; i < max; i++) {
				castIdentifier.getGenericTypes().set(i, otherIdent.getGenericTypes().get(i));
			}
		}
	}

	public boolean typeMustBeInferred() {
		return inferTypeFrom != null;
	}

	public Expression getExpressionToInferFrom() {
		return inferTypeFrom;
	}

	public void inferType() {
		setType(inferTypeFrom.getType());
		castIdentifier = ResolvableIdentifier.convert(getType().getIdentifier());
	}

	@Override
	public void visit(BaseVisitor visitor) {
		visitor.visit(this);
	}

	@Override
	public void visitChildren(BaseVisitor visitor) {
		visitor.visitDoubleDispatched(expression);
	}

	public boolean isUnchecked() {
		return unchecked;
	}
}
