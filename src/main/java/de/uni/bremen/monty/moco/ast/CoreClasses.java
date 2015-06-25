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

import de.uni.bremen.monty.moco.ast.declaration.ClassDeclaration;

import java.util.Map;
import java.util.HashMap;
import java.util.Collection;
import java.util.Collections;

public class CoreClasses {

	private static Map<String, ClassDeclaration> coreClasses = new HashMap<String, ClassDeclaration>();

	static {
		// TODO find name for void that is not a valid identifier
		String[] classNames = new String[] { "Object", "Char", "String", "Int", "Float", "Bool", "Array", "__void" };
		for (String name : classNames) {
			CoreClasses.setCoreClass(name, new ClassDeclaration(new Position("Dummy_" + name, 0, 0), new Identifier(
			        name), Collections.<ResolvableIdentifier> emptyList(), new Block(
			        new Position("Dummy_" + name, 1, 0))));
		}
	}

	public static void setCoreClass(String name, ClassDeclaration classDeclaration) {
		coreClasses.put(name, classDeclaration);
	}

	public static ClassDeclaration objectType() {
		return coreClasses.get("Object");
	}

	public static ClassDeclaration charType() {
		return coreClasses.get("Char");
	}

	public static ClassDeclaration stringType() {
		return coreClasses.get("String");
	}

	public static ClassDeclaration intType() {
		return coreClasses.get("Int");
	}

	public static ClassDeclaration floatType() {
		return coreClasses.get("Float");
	}

	public static ClassDeclaration boolType() {
		return coreClasses.get("Bool");
	}

	public static ClassDeclaration arrayType() {
		return coreClasses.get("Array");
	}

	public static ClassDeclaration voidType() {
		return coreClasses.get("__void");
	}
}
