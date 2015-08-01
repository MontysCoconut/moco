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

package de.uni.bremen.monty.moco.codegeneration;

import de.uni.bremen.monty.moco.ast.ASTNode;
import de.uni.bremen.monty.moco.ast.Identifier;
import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.ast.statement.ConditionalStatement;
import de.uni.bremen.monty.moco.ast.statement.WhileLoop;

import java.util.EnumMap;

enum Mangled {
	MODULE("M."), CLASS(".C."), FUNC(".F."), PROC(".P."), BLOCK(".B."), VAR(".V."), TYPE("$"), IF("IF."), ELSE("ELSE."), WHILE(
	        "WHILE.");

	private final String symbol;

	public String toString() {
		return symbol;
	}

	Mangled(String symbol) {
		this.symbol = symbol;
	}
}

/** The NameManglingVisitor uses the following pattern to mangle the names:
 *
 * _ : '.';
 *
 * $ : '$';
 *
 *
 * module : 'module'_name;
 *
 * class : 'class'_name;
 *
 * func : 'func'_name$type($type)*;
 *
 * proc : 'proc'_name($type)*;
 *
 * var : 'var'_name$type;
 *
 * type : 'type'_module(_class)?((_block|_proc|_func)*(_proc|_func))?;
 *
 * block : 'block'_(IF|TRY|WHILE|HANDLE|ELSE)_number;
 *
 * mangled : packet_module(_class)?((_block|_proc|_func)*(_proc|_func|_var))?; * */
public class NameMangler {

	private String mangleProcedureDeclaration(ProcedureDeclaration node, String midTerm) {
		String parameter = "";
		for (final VariableDeclaration variableDeclaration : node.getParameter()) {
			if (escapeForLLVM(variableDeclaration.getIdentifier()).equals("self")) {
			} else {
				TypeDeclaration type = variableDeclaration.getType();
				ClassDeclaration classDeclaration = getConcreteClass(node, type);
				parameter += Mangled.TYPE + mangleClass(classDeclaration);
			}
		}

		String base;
		if (node.getDefiningClass() != null) {
			base = mangleClass(node.getDefiningClass());
		} else {
			base = mangleBlock(node);
		}

		return base + midTerm + parameter;
	}

	private ClassDeclaration getConcreteClass(ASTNode node, TypeDeclaration type) {
		if (type instanceof AbstractGenericType) {
			ClassDeclarationVariation variation =
			        (ClassDeclarationVariation) ((AbstractGenericType) type).getDefinedIn().getParentNode();
			type = variation.mapAbstractToConcrete((AbstractGenericType) type);
		}
		return (ClassDeclaration) type;
	}

	public String mangleProcedure(ProcedureDeclaration node) {
		if (node.isFunction()) {
			String funcName = Mangled.FUNC + escapeForLLVM(node.getIdentifier());
			funcName += Mangled.TYPE + mangleClass(getConcreteClass(node, node.getReturnType()));

			return mangleProcedureDeclaration(node, funcName);

		} else {
			final String procName = Mangled.PROC + escapeForLLVM(node.getIdentifier());

			return mangleProcedureDeclaration(node, procName);
		}
	}

	public String mangleVariable(VariableDeclaration node) {
		String base = mangleBlock(node);

		String var =
		        Mangled.VAR + escapeForLLVM(node.getIdentifier()) + Mangled.TYPE
		                + mangleClass(getConcreteClass(node, node.getType()));

		return base + var;
	}

	private String mangleBlock(ASTNode node) {
		ASTNode parent = node.getParentNode();
		if (parent instanceof ModuleDeclaration) {
			return mangleModule((ModuleDeclaration) parent);
		} else if (parent instanceof ClassDeclaration) {
			return mangleClass((ClassDeclaration) parent);
		} else if (parent instanceof ProcedureDeclaration) {
			return mangleProcedure((ProcedureDeclaration) parent);
		} else if (parent instanceof ConditionalStatement || parent instanceof WhileLoop) {
			return Mangled.BLOCK + Integer.toHexString(System.identityHashCode(parent)) + mangleBlock(parent);
		} else {
			return mangleBlock(parent);
		}
	}

	public String mangleClass(ClassDeclaration node) {
		ASTNode n = node;
		while (!(n instanceof ModuleDeclaration)) {
			n = n.getParentNode();
		}
		ModuleDeclaration module = (ModuleDeclaration) n;
		String base = mangleModule(module);

		String className = Mangled.CLASS + escapeForLLVM(node.getIdentifier());
		if (node instanceof ClassDeclarationVariation) {
			className += Mangled.TYPE;
			for (ClassDeclaration concreteGenericType : ((ClassDeclarationVariation) node).getConcreteGenericTypes()) {
				className += Mangled.TYPE + mangleClass(concreteGenericType);
			}
		}

		return base + className;
	}

	private String mangleModule(ModuleDeclaration node) {
		String moduleName = node.getIdentifier().getSymbol();
		moduleName = Mangled.MODULE + moduleName;
		return moduleName;
	}

	private String escapeForLLVM(Identifier identifier) {
		String string = identifier.getSymbol();
		string = string.replaceAll("%", "_rem");
		string = string.replaceAll("\\*", "_mult");
		string = string.replaceAll("/", "_div");
		string = string.replaceAll("\\+", "_plus");
		string = string.replaceAll("-", "_minus");
		string = string.replaceAll("<=", "_lesser_equal");
		string = string.replaceAll(">=", "_greater_equal");
		string = string.replaceAll("!=", "_not_equal");
		string = string.replaceAll("=", "_equal");
		string = string.replaceAll("<", "_lesser");
		string = string.replaceAll(">", "_greater");
		string = string.replaceAll("%", "_rem");
		return string;
	}

}
