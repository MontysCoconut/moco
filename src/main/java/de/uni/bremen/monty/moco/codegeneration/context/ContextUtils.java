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

import de.uni.bremen.monty.moco.ast.ASTNode;

import java.util.Stack;

/** Utility class for accessing different {@link Context}
 * 
 * Holds the {@link #baseContext} which is the Context that represent a whole .ll file. */
public class ContextUtils {

	private final CommentAppender commentAppender;

	/** represents a whole .ll file */
	private final CodeContext baseContext;

	/** on top of each .ll file there is an area where all constant declaration should be located. */
	private final CodeContext constantsContext;

	/** each function should have it's own CodeContext. This is where those are stored. The first Context in the Stack is
	 * the container of all functions in the .ll file and not the container for all statements in a function.
	 * 
	 * A Stack is used because monty allows to have functions inside functions.(Despite having closures) LLVM doesn't
	 * support that feature. So while processing a function you may find another function, which you have to process.
	 * You start processing the new one, while saving the state of the old one in the stack */
	private Stack<CodeContext> activeContexts;

	/** Creates a {@link #baseContext} and a {@link #constantsContext} and makes the constantContext part of the
	 * baseContext. */
	public ContextUtils() {
		commentAppender = new CommentAppender();

		baseContext = new CodeContext(commentAppender);
		activeContexts = new Stack<>();
		activeContexts.push(baseContext);

		constantsContext = new CodeContext(commentAppender);
		baseContext.append(constantsContext);
	}

	/** Sets the current Node to the {@link #commentAppender}
	 * 
	 * @param node
	 *            ast node */
	public void setNode(ASTNode node) {
		commentAppender.setNode(node);
	}

	/** Returns the active Context. That is the Context in which the next instruction for LLVM from an 'normal' monty
	 * Statement should be saved.
	 * 
	 * @return the active Context. */
	public CodeContext active() {
		return activeContexts.peek();
	}

	/** Returns the constant Context. That is the Context in which the constant LLVM-declarations should be located. Like
	 * string literals.
	 * 
	 * @return the constant Context. */
	public CodeContext constant() {
		return constantsContext;
	}

	/** Converts the {@link #baseContext} into a string. The baseContext represents a whole .ll File. So the data is the
	 * content of a .ll file.
	 * 
	 * @return the converted {@link #baseContext} */
	public String getData() {
		return baseContext.getData();
	}

	/** Adds a new Context to the {@link #activeContexts}. This should be used in the beginning of the processing of a
	 * monty function. So that the llvm instructions of the monty statements stored into this new Context */
	public void addNewContext() {
		CodeContext context = new CodeContext(commentAppender);
		activeContexts.push(context);
		baseContext.append(context);
	}

	/** Closes a Context opened with {@link #addNewContext()}. This should be done at the end of processing a function. */
	public void closeContext() {
		activeContexts.pop();
	}
}
