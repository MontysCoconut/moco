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
package de.uni.bremen.monty.moco.codegeneration.identifier;

import de.uni.bremen.monty.moco.codegeneration.types.LLVMType;

/** LLVMIdentifier is the simple composition of an LLVMType and a name. In LLVM-IR you often have to write s.th. like
 * 'i64 %0' as a part of an instruction. This is an LLVMIdentifier. And if you use {@link #toString()} it gives you the
 * representation for LLVM-IR.
 * 
 * The Type can be composed, see {@link LLVMType}. And the name can be more complex too. e.g. ' getelementptr inbounds
 * (i8* %0, i32 0, i32 0) ' is currently modeled as a simple name for an identifier.
 * 
 * Instances should only be created with the {@link LLVMIdentifierFactory}.
 * 
 * @param <T>
 *            The Type that is encapsulated in the Identifier. If you have e.g. 'i64* %2' the Type will be
 *            LLVMIdentifier<LLVMPointer<LLVMInt64>> */
public class LLVMIdentifier<T extends LLVMType> {
	protected final String name;
	protected final T type;

	/** This is indicates if this value is a concrete value, or a pointer to a value, that needs to be resolved or
	 * dereference to gets its value. */
	private boolean resolvable;

	/** Don't use this. Only {@link LLVMIdentifierFactory} should create instances.
	 * 
	 * @param type
	 * @param name
	 * @param resolvable */
	LLVMIdentifier(T type, String name, boolean resolvable) {
		this.name = name;
		this.type = type;
		this.resolvable = resolvable;
	}

	public String getName() {
		return name;
	}

	public T getType() {
		return type;
	}

	/** @returns the LLVM-IR representation. e.g. new LLVMIdentifier(pointer(int64(),"%foobar").toString() ==
	 *          "i64* %foobar" */
	@Override
	public String toString() {
		return type + " " + name;
	}

	/** Is it a value or a pointer to value
	 * 
	 * @return true if its a pointer to a value */
	public boolean needToBeResolved() {
		return resolvable;
	}

}
