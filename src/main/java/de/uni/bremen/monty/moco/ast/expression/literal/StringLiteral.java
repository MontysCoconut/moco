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
import org.apache.commons.lang3.StringUtils;

public class StringLiteral extends LiteralExpression<String> {
	public StringLiteral(Position position, String value) {
		super(position, prepareStrLiteral(value));
	}

	@Override
	public void visit(BaseVisitor visitor) {
		visitor.visit(this);
	}

	/** Removes leading and trailing quotes of string literals
	 *
	 * @param value
	 *            the string to prepare
	 * @return value without leading and trailing quotes */
	protected static String prepareStrLiteral(String value) {
		if ((value.startsWith("\"")) && (value.endsWith("\""))) {
			value = value.substring(1, value.length() - 1);
		}
		return value;
	}

	/** @param value
	 * @return */
	public static int getStrLiteralLength(String value) {
		// every backslash is followed by two numbers, but those three characters
		// represent only one char in the IR string literal
		return value.length() - (StringUtils.countMatches(value, "\\") * 2);
	}

	/** replaces escape sequences of string literals by the LLVM IR equivalents
	 *
	 * @param value
	 *            the string which should be escaped
	 * @return an escaped string */
	public static String replaceEscapeSequences(String value) {
		value = value.replace("\\t", "\\09"); // horizontal tab
		value = value.replace("\\b", "\\08"); // backspace
		value = value.replace("\\n", "\\0A"); // line feed
		value = value.replace("\\r", "\\0D"); // carriage return
		value = value.replace("\\f", "\\0C"); // formfeed
		value = value.replace("\\'", "\\27"); // single quote
		value = value.replace("\\\"", "\\22"); // double quote
		value = value.replace("\\\\", "\\5C"); // backslash

		return value;
	}
}
