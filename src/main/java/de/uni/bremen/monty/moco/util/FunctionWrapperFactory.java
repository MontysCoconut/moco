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

import de.uni.bremen.monty.moco.ast.Block;
import de.uni.bremen.monty.moco.ast.Identifier;
import de.uni.bremen.monty.moco.ast.Position;
import de.uni.bremen.monty.moco.ast.ResolvableIdentifier;
import de.uni.bremen.monty.moco.ast.declaration.ClassDeclaration;
import de.uni.bremen.monty.moco.ast.declaration.FunctionDeclaration;
import de.uni.bremen.monty.moco.ast.declaration.VariableDeclaration;
import de.uni.bremen.monty.moco.ast.expression.Expression;
import de.uni.bremen.monty.moco.ast.expression.FunctionCall;
import de.uni.bremen.monty.moco.ast.expression.VariableAccess;
import de.uni.bremen.monty.moco.ast.statement.Assignment;
import de.uni.bremen.monty.moco.ast.statement.ReturnStatement;
import de.uni.bremen.monty.moco.ast.statement.UnpackAssignment;

import java.util.ArrayList;
import java.util.List;

public class FunctionWrapperFactory {
	public static void generateWrapperClass(FunctionDeclaration function, TupleDeclarationFactory tupleFactory) {
		// we need unique identifiers for both, the wrapper-class and its instance
		ResolvableIdentifier classIdentifier = TmpIdentifierFactory.getUniqueIdentifier();
		ResolvableIdentifier varIdentifier = TmpIdentifierFactory.getUniqueIdentifier();

		Position pos = function.getPosition();

		// create the wrapper class
		Block body = new Block(pos);
		FunctionDeclaration applyMethod = createApplyMethod(function, tupleFactory);
		body.addDeclaration(applyMethod);
		List<ResolvableIdentifier> baseClasses = new ArrayList<>(1);
		List<ResolvableIdentifier> genericParams = new ArrayList<>(2);
		genericParams.add(applyMethod.getParameters().get(0).getTypeIdentifier());
		genericParams.add(applyMethod.getReturnTypeIdentifier());
		baseClasses.add(new ResolvableIdentifier("Function", genericParams));
		ClassDeclaration wrapperClass = new ClassDeclaration(pos, classIdentifier, baseClasses, body);
		wrapperClass.setWrappedFunction(function);

		// create a variable and store an instance of the wrapper in it
		VariableDeclaration functionObjectDeclaration =
		        new VariableDeclaration(pos, varIdentifier, wrapperClass, VariableDeclaration.DeclarationType.VARIABLE);
		Assignment functionObjectAssignment =
		        new Assignment(pos, new VariableAccess(pos,
		                ResolvableIdentifier.convert(functionObjectDeclaration.getIdentifier())), new FunctionCall(pos,
		                classIdentifier, new ArrayList<Expression>()));

		// store the generated declarations inside the function declaration AST-node
		function.setWrapperClass(wrapperClass);
		function.setWrapperFunctionObjectDeclaration(functionObjectDeclaration);
		function.setWrapperFunctionAssignment(functionObjectAssignment);
		functionObjectAssignment.setCorrespondingFunctionWrapper(function);
	}

	protected static FunctionDeclaration createApplyMethod(FunctionDeclaration function,
	        TupleDeclarationFactory tupleFactory) {
		Position pos = function.getPosition();
		Block body = new Block(pos);

		ResolvableIdentifier paramType;
		List<Expression> localVariables;
		// if the function receives more than one parameter, we pack them into a tuple
		if (function.getParameters().size() > 1) {
			// instead of a list of arguments, the apply-method expects a tuple containing them
			List<ResolvableIdentifier> parameters = new ArrayList<>(function.getParameters().size());
			localVariables = new ArrayList<>(function.getParameters().size());

			for (VariableDeclaration parameter : function.getParameters()) {
				parameters.add(parameter.getTypeIdentifier());
				body.addDeclaration(new VariableDeclaration(pos, parameter.getIdentifier(),
				        parameter.getTypeIdentifier(), VariableDeclaration.DeclarationType.VARIABLE));
				localVariables.add(new VariableAccess(pos, ResolvableIdentifier.convert(parameter.getIdentifier())));
			}
			paramType = tupleFactory.getTupleIdentifier(parameters);

			// in order to call the actual function, we need to unpack the parameter tuple
			UnpackAssignment unpack =
			        new UnpackAssignment(pos, localVariables,
			                new VariableAccess(pos, new ResolvableIdentifier("param")));
			body.addDeclaration(unpack.getTmpDecl());
			body.addStatement(unpack);
		}
		// if there is only one parameter, we don't need to pack it
		else if (function.getParameters().size() == 1) {
			paramType = function.getParameters().get(0).getTypeIdentifier();
			localVariables = new ArrayList<>(1);
			localVariables.add(new VariableAccess(pos, new ResolvableIdentifier("param")));
		}
		// if there is no parameter, we just use an empty Tuple
		else {
			paramType = new ResolvableIdentifier("Tuple0");
			localVariables = new ArrayList<>();
		}

		// either return the real return value (if function has a return value or if the return type must be inferred)
		if ((function.isFunction()) || (function.isReturnTypeToBeInferred())) {
			body.addStatement(new ReturnStatement(pos, new FunctionCall(pos,
			        ResolvableIdentifier.convert(function.getIdentifier()), localVariables)));
		}
		// or return an empty tuple (if the function is a procedure)
		else {
			body.addStatement(new FunctionCall(pos, ResolvableIdentifier.convert(function.getIdentifier()),
			        localVariables));
			body.addStatement(new ReturnStatement(pos, new FunctionCall(pos, new ResolvableIdentifier("Tuple0"),
			        new ArrayList<Expression>())));
		}

		// the return type of the method is either the return type of the original function,
		// or empty tuple if it was a procedure.
		ResolvableIdentifier returnTypeIdentifier =
		        function.getReturnTypeIdentifier() != null ? function.getReturnTypeIdentifier() : tupleFactory.getTupleIdentifier(new ArrayList<ResolvableIdentifier>());

		// create a list containing the only parameter of the method
		List<VariableDeclaration> methodParams = new ArrayList<>(1);
		methodParams.add(new VariableDeclaration(pos, new Identifier("param"), paramType,
		        VariableDeclaration.DeclarationType.PARAMETER));

		// compose the actual method
		FunctionDeclaration applyMethod =
		        new FunctionDeclaration(pos, new Identifier("_apply_"), body, methodParams,
		                FunctionDeclaration.DeclarationType.METHOD, returnTypeIdentifier, false);
		return applyMethod;
	}
}
