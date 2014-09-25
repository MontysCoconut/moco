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
package de.uni.bremen.monty.moco.codegeneration.types;

/** An LLVMType represents a type for LLVM-IR. This can be a LLVMSimpleType like 'i64' or an composed Type like 'i8*'.
 * SubTypes like LLVMPointer or LLVMArrayType are designed that they can be composed freely.
 * 
 * If you want an Pointer to an Array with Pointers to boolean values you can write:
 * LLVMPointer<LLVMArrayType<LLVMPointer<LLVMBool>>> type = pointer(array(pointer(int1()),2)) using the factory Methods
 * in LLVMTypeFactory.
 * 
 * {@link #toString()} is overwritten in such a way, that it results in the String that represent this type in LLVM-IR
 * e.g. type.toString() == "[2 x i1*]*"
 * 
 * Only use {@link LLVMTypeFactory} to instantiate LLVMTypes. */
public abstract class LLVMType {

	public abstract String toString();

	@Override
	public abstract boolean equals(Object o);

	@Override
	public abstract int hashCode();
}
