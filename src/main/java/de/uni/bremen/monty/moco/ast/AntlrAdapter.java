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

package de.uni.bremen.monty.moco.ast;

import de.uni.bremen.monty.moco.antlr.MontyLexer;
import de.uni.bremen.monty.moco.antlr.MontyParser;
import de.uni.bremen.monty.moco.ast.declaration.ModuleDeclaration;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.SequenceInputStream;

public class AntlrAdapter {

	public MontyParser createParser(final InputStream file) throws IOException {

		InputStream in = createInputStream(file);

		// the additional line-break is needed because of our indentation rule
		// and the fact that a statement should be terminated by a line break
		ANTLRInputStream input = new ANTLRInputStream(in);
		MontyLexer lexer = new MontyLexer(input);
		CommonTokenStream tokens = new CommonTokenStream(lexer);
		return new MontyParser(tokens);
	}

	private InputStream createInputStream(InputStream file) {
		return new SequenceInputStream(file, new ByteArrayInputStream("\n".getBytes()));
	}

	public ModuleDeclaration parse(InputStream file, String fileName) throws IOException {
		MontyParser parser = createParser(file);

		ASTBuilder astBuilder = new ASTBuilder(fileName);
		ASTNode moduleNode = astBuilder.visit(parser.compilationUnit());
		return (ModuleDeclaration) moduleNode;
	}
}
