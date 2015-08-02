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
import de.uni.bremen.monty.moco.ast.expression.MemberAccess;
import de.uni.bremen.monty.moco.ast.expression.SelfExpression;
import de.uni.bremen.monty.moco.ast.expression.VariableAccess;
import de.uni.bremen.monty.moco.ast.statement.Assignment;
import de.uni.bremen.monty.moco.ast.statement.ReturnStatement;

import java.util.*;

/** A ClassDeclaration represents the declaration of a class in the AST.
 * <p>
 * A ClassDeclaration has a list of superclasses and a list of nested declarations. It can be used as a type. */
public class TupleClassDeclaration extends ClassDeclaration {

	private static Map<Integer, TupleClassDeclaration> tupleTypes = new HashMap<>();

	private TupleClassDeclaration(Position position, Identifier identifier,
	        ArrayList<ResolvableIdentifier> superClasses, Block classContent, boolean isAbstract,
	        List<AbstractGenericType> genericTypes) {
		super(position, identifier, superClasses, classContent, isAbstract, genericTypes);
	}

	/** This method checks whether a tuple type with the correct length is already defined. If not, a new tuple type is
	 * generated.
	 *
	 * @param n
	 *            the size of the tuple type (TupleN)
	 * @return a TupleN ClassDeclaration (either a reference to an old one, or a new one) */
	public static TupleClassDeclaration getInstance(int n) {
		// lookup
		TupleClassDeclaration tupleType = tupleTypes.get(n);
		// generate a new tuple type
		if (tupleType == null) {
			tupleType = newInstance(n);
			tupleTypes.put(n, tupleType);
		}
		return tupleType;
	}

	/** This method checks whether a tuple type with the correct length is already defined. If not, a new tuple type is
	 * generated.
	 *
	 * @param n
	 *            the size of the tuple type (TupleN)
	 * @return a new TupleN ClassDeclaration or null, if that one is already defined */
	public static TupleClassDeclaration getNewInstance(int n) {
		// lookup
		TupleClassDeclaration tupleType = tupleTypes.get(n);
		// generate a new tuple type
		if (tupleType == null) {
			tupleType = newInstance(n);
			tupleTypes.put(n, tupleType);
			return tupleType;
		}
		// return null in order to signalize that no new class declaration has been created
		return null;
	}

	/** This method generates a new TupleType
	 *
	 * @param n
	 *            the size of the tuple type (TupleN)
	 * @return a new TupleN ClassDeclaration */
	private static TupleClassDeclaration newInstance(int n) {
		// components of the class declaration
		List<AbstractGenericType> genericTypes = new ArrayList<>();
		Block classContent = new Block(new Position());
		TupleClassDeclaration tupleType =
		        new TupleClassDeclaration(new Position(), new Identifier("Tuple" + n),
		                new ArrayList<ResolvableIdentifier>(), classContent, false, genericTypes);

		// components of the initializer
		List<VariableDeclaration> initializerParameters = new ArrayList<>();
		Block initializerBody = new Block(new Position());

		// process the generic type parameters
		for (int i = 0; i < n; i++) {
			// add the type parameter to the class
			AbstractGenericType t = new AbstractGenericType(tupleType, new Position(), new Identifier("T" + i));
			genericTypes.add(t);

			// add an attribute with that type to the class
			VariableDeclaration attr =
			        new VariableDeclaration(new Position(), new Identifier("_" + (i + 1)), t,
			                VariableDeclaration.DeclarationType.ATTRIBUTE);
			classContent.addDeclaration(attr);

			// add a parameter with that type to the initializer parameter list
			VariableDeclaration param =
			        new VariableDeclaration(new Position(), new Identifier("p" + i), t,
			                VariableDeclaration.DeclarationType.PARAMETER);
			initializerParameters.add(param);

			// add an assignment to the initializer body: "self._1 := p1"
			initializerBody.addStatement(new Assignment(new Position(), new MemberAccess(new Position(),
			        new SelfExpression(new Position()), new VariableAccess(new Position(),
			                ResolvableIdentifier.convert(attr.getIdentifier()))), new VariableAccess(new Position(),
			        ResolvableIdentifier.convert(param.getIdentifier()))));
		}
		initializerBody.addStatement(new ReturnStatement(new Position(), null));
		// generate default initializer with every attribute being a parameter
		ProcedureDeclaration initializer =
		        new ProcedureDeclaration(new Position(), new Identifier("initializer"), initializerBody,
		                initializerParameters, ProcedureDeclaration.DeclarationType.INITIALIZER, (TypeDeclaration) null);
		classContent.addDeclaration(initializer);

		return tupleType;
	}
}
