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

package de.uni.bremen.monty.moco.ast.expression.literal;

import de.uni.bremen.monty.moco.ast.CoreClasses;
import de.uni.bremen.monty.moco.ast.Position;
import de.uni.bremen.monty.moco.visitor.BaseVisitor;

public class CharacterLiteral extends LiteralExpression<Character> {
	public CharacterLiteral(Position position, Character value) {
		super(position, value);
	}

	public CharacterLiteral(Position position, String value) {
		this(position, analyzeStringInitialization(value));
	}

	@Override
	public void visit(BaseVisitor visitor) {
		visitor.visit(this);
	}

	/** This method is used when a Char literal is initialized with the token text (not with the character value
	 * directly. It parses escape sequences and replaces them by the actual characters.
	 *
	 * @param value
	 * @return A Java Character which corresponds to the Monty Char literal */
	protected static Character analyzeStringInitialization(String value) {
		// 'value' includes the single quotes, hence we have to consider this in our index counting...
		// if there is an escape sequence in the char literal, we have to process it...
		if ((value.length() > 3) && (value.charAt(1) == '\\')) {
			switch (value.charAt(2)) {
			case 't':
				return '\t';
			case 'b':
				return '\b';
			case 'n':
				return '\n';
			case 'r':
				return '\r';
			case 'f':
				return '\f';
			case '\'':
				return '\'';
			case '"':
				return '\"';
			case '\\':
				return '\\';
			}
		}
		// if nothing else worked, we just return the first char in the literal (i.e. the x in 'x')
		return value.charAt(1);
	}
}
