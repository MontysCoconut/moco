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

import de.uni.bremen.monty.moco.ast.Block;
import de.uni.bremen.monty.moco.ast.Identifier;
import de.uni.bremen.monty.moco.ast.Position;
import de.uni.bremen.monty.moco.ast.ResolvableIdentifier;
import de.uni.bremen.monty.moco.ast.expression.Expression;
import de.uni.bremen.monty.moco.ast.expression.FunctionCall;
import de.uni.bremen.monty.moco.ast.statement.ReturnStatement;
import de.uni.bremen.monty.moco.ast.statement.YieldStatement;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class GeneratorFunctionDeclaration extends FunctionDeclaration {
	private List<YieldStatement> yieldStatements = new ArrayList<>();
	private List<VariableDeclaration> varDecls = new ArrayList<>();
	private int attributeIndex = 0;

	public GeneratorFunctionDeclaration(Position position, Identifier identifier, Block body,
	        List<VariableDeclaration> parameters, ResolvableIdentifier returnTypeIdentifier) {
		super(position, identifier, body, parameters, DeclarationType.METHOD, new ResolvableIdentifier("Maybe",
		        Arrays.asList(returnTypeIdentifier)));
		// the last statement inside a generator is always "return Nothing<Type>()"
		body.addStatement(new ReturnStatement(position, new FunctionCall(position, new ResolvableIdentifier("Nothing",
		        Arrays.asList(returnTypeIdentifier)), new ArrayList<Expression>())));
	}

	public List<YieldStatement> getYieldStatements() {
		return yieldStatements;
	}

	public void addYieldStatement(YieldStatement yield) {
		if (!yieldStatements.contains(yield)) {
			yieldStatements.add(yield);
		}
	}

	public List<VariableDeclaration> getVariableDeclarations() {
		return varDecls;
	}

	public void addVariableDeclaration(VariableDeclaration varDecl) {
		if (!varDecls.contains(varDecl)) {
			varDecls.add(varDecl);
			attributeIndex++;
			varDecl.setAttributeIndex(attributeIndex);
		}
	}
}
