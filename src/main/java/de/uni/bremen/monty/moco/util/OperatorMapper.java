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

public class OperatorMapper {
	public static String methodFromBinaryOperator(String operator) {
		switch (operator) {
		case "+":
			return "_add_";
		case "-":
			return "_sub_";
		case "*":
			return "_mul_";
		case "/":
			return "_div_";
		case "%":
			return "_mod_";
		case "^":
			return "_pow_";
		case "=":
			return "_eq_";
		case "!=":
			return "_neq_";
		case "<":
			return "_lt_";
		case ">":
			return "_gt_";
		case "<=":
			return "_leq_";
		case ">=":
			return "_geq_";
		case "in":
			return "_contains_";
		case "and":
			return "_and_";
		case "or":
			return "_or_";
		case "xor":
			return "_xor_";
		}
		return null;
	}

	public static String methodFromUnaryOperator(String operator) {
		switch (operator) {
		case "-":
			return "_neg_";
		case "not":
			return "_not_";
		}
		return null;
	}

	public static String operatorFromMethod(String method) {
		switch (method) {
		case "_add_":
			return "+";
		case "_sub_":
			return "-";
		case "_mul_":
			return "*";
		case "_div_":
			return "/";
		case "_mod_":
			return "%";
		case "_pow_":
			return "^";
		case "_eq_":
			return "=";
		case "_neq_":
			return "!=";
		case "_lt_":
			return "<";
		case "_gt_":
			return ">";
		case "_leq_":
			return "<=";
		case "_geq_":
			return ">=";
		case "_contains_":
			return "in";
		case "_neg_":
			return "-";
		case "_not_":
			return "not";
		}
		return null;
	}
}
