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
package de.uni.bremen.monty.moco.codegeneration.context;

import org.apache.commons.lang3.StringUtils;

import java.util.ArrayList;
import java.util.List;

/** An .ll File is composed by a Tree of Context. This Tree is realized in this class with a recursive Structure that is
 * created with the attribute {@link #innerContexts}
 * 
 * The leaves are lines holding a single Instruction represented as {@link StringData}. Those leaves can be composed
 * together in a Context. For e.g. a function in llvm is a context because it is a composition of statements. One could
 * further subdivide a function into multiple parts. All functions inside a .ll file are further composed into a
 * context, which is then the root of the .ll file. */
public class Context implements ContextData {

	/** Recursive Structure creating a Tree */
	private List<ContextData> innerContexts;
	private CommentAppender commentAppender;
	private int indentation = 0;

	public Context(CommentAppender commentAppender) {
		this.commentAppender = commentAppender;
		innerContexts = new ArrayList<>();
	}

	/** Adds an LLVM-Instruction as a Leaf to the Tree.
	 * 
	 * @param data */
	public void append(String data) {
		data = StringUtils.repeat(" ", indentation * 4) + data;
		// data = commentAppender.addComment(data);
		innerContexts.add(new StringData(data + "\n"));
	}

	/** Appends a Context to the Tree. This increases the depth of the Tree...
	 * 
	 * @param c */
	public void append(Context c) {
		innerContexts.add(c);
	}

	/** Converts this into a List of instruction as a String. Each Instruction is a new Line in the String. This is done
	 * by flattening the Tree by concatenating.
	 * 
	 * @return All the children in a String */
	public String getData() {
		StringBuilder s = new StringBuilder();
		for (ContextData contextData : innerContexts) {
			s.append(contextData.getData());
		}
		return s.toString();
	}

	protected void indent() {
		indentation++;
	}

	protected void dedent() {
		indentation--;
	}

	protected void emptyLine() {
		innerContexts.add(new StringData("\n"));
	}
}
