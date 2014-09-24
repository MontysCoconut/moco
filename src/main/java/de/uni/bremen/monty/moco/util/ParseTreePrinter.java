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
package de.uni.bremen.monty.moco.util;

import org.antlr.v4.runtime.Parser;
import org.antlr.v4.runtime.ParserRuleContext;
import org.antlr.v4.runtime.misc.Utils;
import org.antlr.v4.runtime.tree.ErrorNode;
import org.antlr.v4.runtime.tree.ParseTreeListener;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.antlr.v4.runtime.tree.Trees;

import java.util.Arrays;
import java.util.List;

public class ParseTreePrinter implements ParseTreeListener {
	private final List<String> ruleNames;
	private final StringBuilder builder = new StringBuilder();
	private int intend = 0;
	private ParserRuleContext lastOne;
	private boolean intended;

	public ParseTreePrinter(Parser parser) {
		this.ruleNames = Arrays.asList(parser.getRuleNames());
	}

	@Override
	public void visitTerminal(TerminalNode node) {
		if (builder.length() > 0) {
			builder.append(' ');
		}

		append(Utils.escapeWhitespace(Trees.getNodeText(node, ruleNames), false));
	}

	@Override
	public void visitErrorNode(ErrorNode node) {
		if (builder.length() > 0) {
			builder.append(' ');
		}

		append(Utils.escapeWhitespace(Trees.getNodeText(node, ruleNames), false));
	}

	@Override
	public void enterEveryRule(ParserRuleContext ctx) {
		builder.append("\n");
		intended = false;

		if (builder.length() > 0) {
			builder.append(' ');
		}

		if (!ctx.equals(lastOne)) {
			intend++;
		}

		int ruleIndex = ctx.getRuleIndex();
		String ruleName;
		if (ruleIndex >= 0 && ruleIndex < ruleNames.size()) {
			ruleName = ruleNames.get(ruleIndex);
		} else {
			ruleName = Integer.toString(ruleIndex);
		}

		append(ruleName);
		lastOne = ctx;
	}

	private void append(String ruleName) {
		if (!intended) {
			for (int i = 0; i < intend; i++) {
				builder.append(' ');
			}
			intended = true;
		}
		builder.append(ruleName);
	}

	@Override
	public void exitEveryRule(ParserRuleContext ctx) {
		if (ctx.getChildCount() > 0) {
			intend--;
		}
	}

	@Override
	public String toString() {
		return builder.toString();
	}
}
