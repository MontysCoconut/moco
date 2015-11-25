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

import de.uni.bremen.monty.moco.ast.*;
import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.ast.expression.*;
import de.uni.bremen.monty.moco.ast.statement.Assignment;
import de.uni.bremen.monty.moco.ast.statement.ReturnStatement;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class GeneratorClassFactory {
	public static ClassDeclaration generateGeneratorClass(Position pos, ResolvableIdentifier className,
	        ResolvableIdentifier iteratorName, List<VariableDeclaration> params,
	        List<VariableDeclaration> defaultParams, List<Expression> defaultValues, ResolvableIdentifier returnType) {
		Block body = new Block(pos);
		Block initBody = new Block(pos);
		Block getIterBody = new Block(pos);

		addInitializers(pos, body, params, defaultParams, defaultValues);

		params.addAll(defaultParams);
		addParamsToClass(params, body, initBody);

		// init method
		FunctionDeclaration initializer =
		        new FunctionDeclaration(pos, new Identifier("initializer"), initBody, params,
		                FunctionDeclaration.DeclarationType.INITIALIZER, null);

		// getIterator method
		FunctionDeclaration getIterator =
		        new FunctionDeclaration(pos, new Identifier("getIterator"), getIterBody,
		                new ArrayList<VariableDeclaration>(), FunctionDeclaration.DeclarationType.METHOD,
		                new ResolvableIdentifier("Iterator", Arrays.asList(returnType)));
		createGetIterBody(pos, iteratorName, params, getIterBody);

		body.addDeclaration(initializer);
		body.addDeclaration(getIterator);

		return new ClassDeclaration(pos, className, Arrays.asList(new ResolvableIdentifier("Iterable",
		        Arrays.asList(returnType))), body);
	}

	public static ClassDeclaration generateGeneratorIteratorClass(Position pos, List<VariableDeclaration> params,
	        Block funBody, ResolvableIdentifier returnType) {
		Block body = new Block(pos);
		Block initBody = new Block(pos);
		ResolvableIdentifier iterName = TmpIdentifierFactory.getUniqueIdentifier();

		// init method
		FunctionDeclaration initializer =
		        new FunctionDeclaration(pos, new Identifier("initializer"), initBody, params,
		                FunctionDeclaration.DeclarationType.METHOD, null);
		addParamsToClass(params, body, initBody);

		// getNext method
		addAttributesAsLocalVars(body, funBody);
		GeneratorFunctionDeclaration getNext =
		        new GeneratorFunctionDeclaration(pos, new Identifier("getNext"), funBody,
		                new ArrayList<VariableDeclaration>(), returnType);

		body.addDeclaration(initializer);
		body.addDeclaration(getNext);
		ClassDeclaration it =
		        new ClassDeclaration(pos, iterName, Arrays.asList(new ResolvableIdentifier("Iterator",
		                Arrays.asList(returnType))), body);
		it.setGenerator(true);
		return it;
	}

	/** adds the parameters as attributes to the class and the initializer, also assigns the values in init body
	 *
	 * @param params
	 * @param classBody
	 * @param initBody */
	private static void addParamsToClass(List<VariableDeclaration> params, Block classBody, Block initBody) {
		for (VariableDeclaration param : params) {
			Position lPos = param.getPosition();
			// add as an attribute
			VariableDeclaration attribute =
			        new VariableDeclaration(lPos, param.getIdentifier(), param.getTypeIdentifier(),
			                VariableDeclaration.DeclarationType.ATTRIBUTE);
			attribute.setAccessModifier(AccessModifier.PUBLIC);
			classBody.addDeclaration(attribute);

			// add an assignment
			initBody.addStatement(new Assignment(lPos, new MemberAccess(lPos, new SelfExpression(lPos),
			        new VariableAccess(lPos, ResolvableIdentifier.convert(param.getIdentifier()))), new VariableAccess(
			        lPos, ResolvableIdentifier.convert(param.getIdentifier()))));
		}
	}

	/** creates the body for the getIterator method
	 *
	 * @param pos
	 * @param iterName
	 * @param params
	 * @param funBody */
	private static void createGetIterBody(Position pos, ResolvableIdentifier iterName,
	        List<VariableDeclaration> params, Block funBody) {
		List<Expression> arguments = new ArrayList<>();
		FunctionCall call = new FunctionCall(pos, iterName, arguments);
		for (VariableDeclaration param : params) {
			Position lPos = param.getPosition();
			arguments.add(new MemberAccess(lPos, new SelfExpression(lPos), new VariableAccess(lPos,
			        ResolvableIdentifier.convert(param.getIdentifier()))));
		}
		funBody.addStatement(new ReturnStatement(pos, call));
	}

	private static void addAttributesAsLocalVars(Block classBody, Block funBody) {
		for (Declaration decl : classBody.getDeclarations()) {
			if (decl instanceof VariableDeclaration) {
				VariableDeclaration attrDecl = (VariableDeclaration) decl;
				Position lPos = attrDecl.getPosition();

				// add as a local variable
				VariableDeclaration varDecl =
				        new VariableDeclaration(lPos, attrDecl.getIdentifier(), attrDecl.getTypeIdentifier(),
				                VariableDeclaration.DeclarationType.VARIABLE);
				funBody.addDeclaration(varDecl);

				// add an assignment
				funBody.getStatements().add(
				        0,
				        new Assignment(lPos, new VariableAccess(lPos,
				                ResolvableIdentifier.convert(attrDecl.getIdentifier())), new MemberAccess(lPos,
				                new SelfExpression(lPos), new VariableAccess(lPos,
				                        ResolvableIdentifier.convert(attrDecl.getIdentifier())))));
			}
		}
	}

	private static void addInitializers(Position pos, Block classBody, List<VariableDeclaration> params,
	        List<VariableDeclaration> defaultParams, List<Expression> defaultValues) {

		List<Expression> paramAccesses = new ArrayList<>(params.size());
		for (VariableDeclaration var : params) {
			paramAccesses.add(new VariableAccess(var.getPosition(), ResolvableIdentifier.convert(var.getIdentifier())));
		}
		for (int i = 0; i < defaultParams.size(); i++) {
			Block body = new Block(pos);
			List<VariableDeclaration> pars = new ArrayList<>();
			for (VariableDeclaration par : params) {
				pars.add(new VariableDeclaration(par.getPosition(), par.getIdentifier(), par.getTypeIdentifier(),
				        par.getDeclarationType()));
			}
			List<Expression> arguments = new ArrayList<>(i + paramAccesses.size());
			arguments.addAll(paramAccesses);

			for (int j = 0; j < i; j++) {
				VariableDeclaration var = defaultParams.get(j);
				pars.add(new VariableDeclaration(var.getPosition(), var.getIdentifier(), var.getTypeIdentifier(),
				        var.getDeclarationType()));
				arguments.add(new VariableAccess(var.getPosition(), ResolvableIdentifier.convert(var.getIdentifier())));
			}
			for (int j = i; j < defaultParams.size(); j++) {
				arguments.add(defaultValues.get(j));
			}

			FunctionDeclaration initializer =
			        new FunctionDeclaration(pos, new Identifier("initializer"), body, pars,
			                FunctionDeclaration.DeclarationType.INITIALIZER, null);
			body.addStatement(new MemberAccess(pos, new SelfExpression(pos), new FunctionCall(pos,
			        new ResolvableIdentifier("initializer"), arguments)));
			classBody.addDeclaration(initializer);
		}
	}
}
