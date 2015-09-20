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
import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.ast.expression.MemberAccess;
import de.uni.bremen.monty.moco.ast.expression.SelfExpression;
import de.uni.bremen.monty.moco.ast.expression.VariableAccess;
import de.uni.bremen.monty.moco.ast.statement.Assignment;
import de.uni.bremen.monty.moco.ast.statement.ReturnStatement;

import java.util.*;

/** This class bundles utilities for the automatic generation of types at compile-time */
public class TupleDeclarationFactory {
	private Set<Integer> tupleTypes = new HashSet<>();

	public void checkTupleType(int n) {
		tupleTypes.add(n);
	}

	/** This method checks whether the given Identifier is a tuple type. If yes, a new tuple type is introduced, given
	 * that it was not introduced before.
	 *
	 * @param type */
	public void checkTupleType(ResolvableIdentifier type) {
		if (type == null) {
			return;
		}
		String str = type.getSymbol();
		if (str.startsWith("Tuple")) {
			String number = str.substring(5);
			try {
				checkTupleType(Integer.parseInt(number));
			} catch (Exception e) {
				// if the rest is not a number, we don't need to create a tuple type
			}
		}
	}

	public ResolvableIdentifier getTupleIdentifier(List<ResolvableIdentifier> types) {
		checkTupleType(types.size());
		return new ResolvableIdentifier("Tuple" + types.size(), types);
	}

	/** This method generates a new TupleType
	 *
	 * @param n
	 *            the size of the tuple type (TupleN)
	 * @return a new TupleN ClassDeclaration */
	protected ClassDeclaration createTupleType(int n) {
		// components of the class declaration
		ClassDeclaration tupleType =
		        new ClassDeclaration(new Position(), new Identifier("Tuple" + n),
		                new ArrayList<ResolvableIdentifier>(), new Block(new Position()), false,
		                new ArrayList<AbstractGenericType>());

		// generate the initializer
		FunctionDeclaration initializer =
		        new FunctionDeclaration(new Position(), new Identifier("initializer"), new Block(new Position()),
		                new ArrayList<VariableDeclaration>(), FunctionDeclaration.DeclarationType.INITIALIZER,
		                (TypeDeclaration) null, false);

		// process the generic type parameters
		for (int i = 0; i < n; i++) {
			// add the type parameter to the class
			AbstractGenericType t = new AbstractGenericType(tupleType, new Position(), new Identifier("T" + i));
			tupleType.getAbstractGenericTypes().add(t);

			// add an attribute with that type to the class
			addTupleAttribute(tupleType, i, t);

			// add a parameter to the initializer to initialize the attribute
			addTupleInitializerParameter(initializer, i, t);
		}
		// return nothing
		initializer.getBody().addStatement(new ReturnStatement(new Position(), null));
		// add the initializer to the class declaration
		tupleType.getBlock().addDeclaration(initializer);

		return tupleType;
	}

	protected void addTupleAttribute(ClassDeclaration tupleType, int i, AbstractGenericType t) {
		VariableDeclaration attr =
		        new VariableDeclaration(new Position(), new Identifier("_" + (i + 1)), t,
		                VariableDeclaration.DeclarationType.ATTRIBUTE);
		tupleType.getBlock().addDeclaration(attr);
	}

	protected void addTupleInitializerParameter(FunctionDeclaration initializer, int i, AbstractGenericType t) {
		// add a parameter with that type to the initializer parameter list
		VariableDeclaration param =
		        new VariableDeclaration(new Position(), new Identifier("p" + (i + 1)), t,
		                VariableDeclaration.DeclarationType.PARAMETER);
		initializer.getParameters().add(param);

		// add an assignment to the initializer body: "self._1 := p1"
		initializer.getBody().addStatement(
		        new Assignment(new Position(),
		                new MemberAccess(new Position(), new SelfExpression(new Position()), new VariableAccess(
		                        new Position(), ResolvableIdentifier.convert(new Identifier("_" + (i + 1))))),
		                new VariableAccess(new Position(), ResolvableIdentifier.convert(param.getIdentifier()))));
	}

	public Collection<ClassDeclaration> generateNecessaryTupleTypes() {
		Collection<ClassDeclaration> tupleClasses = new ArrayList<>(tupleTypes.size());
		for (int n : tupleTypes) {
			tupleClasses.add(createTupleType(n));
		}
		return tupleClasses;
	}

	public static boolean isTuple(ClassDeclaration type) {
		String strIdent = type.getIdentifier().getSymbol();
		if (strIdent.startsWith("Tuple")) {
			int n;
			try {
				n = Integer.parseInt(strIdent.substring(5));
			} catch (Exception e) {
				return false;
			}
			if (type instanceof ClassDeclarationVariation) {
				if (((ClassDeclarationVariation) type).getConcreteGenericTypes().size() == n) {
					return true;
				}
			}
		}
		return false;
	}
}
