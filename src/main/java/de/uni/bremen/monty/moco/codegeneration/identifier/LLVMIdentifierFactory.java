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

import de.uni.bremen.monty.moco.codegeneration.types.LLVMArrayType;
import de.uni.bremen.monty.moco.codegeneration.types.LLVMPointer;
import de.uni.bremen.monty.moco.codegeneration.types.LLVMStructType;
import de.uni.bremen.monty.moco.codegeneration.types.LLVMType;

import java.util.List;
import java.util.Stack;
import java.util.concurrent.atomic.AtomicInteger;

import static de.uni.bremen.monty.moco.codegeneration.types.LLVMTypeFactory.*;

/** This Factory creates LLVMIdentifier. It has various Methods allowing you to create global or local Identifier, with a
 * given name or a generated name, resolvable or not, and even elementPointer and functionSignatures
 * 
 * LLVM use prefixes to differ local from global values. '@' is global '%' is local
 * 
 * Abstract on LLVM Name Policy for local values: If you don't care about the name of a value you can just use a number.
 * Like '%3' BUT this number must start with 0 for the first value in the scope and have to increment, without omitting
 * one. This counter is only valid in a local scope.
 * 
 * This is where the {@link #scope} comes into play. */
public class LLVMIdentifierFactory {

	/** Global counter for global values. */
	private int strIndex = 0;

	/**
     *
     */
	private Stack<AtomicInteger> scope = new Stack<>();

	public LLVMIdentifierFactory() {
		scope.push(new AtomicInteger());
	}

	public <T extends LLVMType> LLVMIdentifier<T> newLocal(String symbol, T type, boolean resolvable) {
		return new LLVMIdentifier<>(type, "%" + symbol, resolvable);
	}

	public <T extends LLVMType> LLVMIdentifier<T> newLocal(T type) {
		return newLocal(type, true);
	}

	private String newName() {
		return "_unnamed_" + scope.peek().getAndIncrement();
	}

	private String newGlobalName() {
		return "." + strIndex++;
	}

	public void openScope() {
		scope.push(new AtomicInteger());
	}

	public void closeScope() {
		scope.pop();
	}

	public <T extends LLVMType> LLVMIdentifier<T> newGlobal(String symbol, T type) {
		return new LLVMIdentifier<>(type, "@" + symbol, true);
	}

	public <T extends LLVMType> LLVMIdentifier<T> newGlobal(T type) {
		return new LLVMIdentifier<>(type, "@" + newGlobalName(), true);
	}

	public <T extends LLVMType> LLVMIdentifier<LLVMPointer<T>> pointerTo(LLVMIdentifier<T> right) {
		return new LLVMIdentifier<>(pointer(right.getType()), right.getName(), false);
	}

	public <T extends LLVMType> LLVMIdentifier<T> constant(T llvmType, int value) {
		return new LLVMIdentifier<>(llvmType, value + "", false);
	}

	public <T extends LLVMType> LLVMIdentifier<T> constant(T llvmType, float value) {
		return new LLVMIdentifier<>(llvmType, value + "", false);
	}

	public <T extends LLVMType> LLVMIdentifier<T> constant(T llvmType, boolean value) {
		return new LLVMIdentifier<>(llvmType, value + "", false);
	}

	public <T extends LLVMType> LLVMIdentifier<LLVMPointer<T>> constantNull(LLVMPointer<T> llvmType) {
		return new LLVMIdentifier<>(llvmType, "null", false);
	}

	public StructConstant constant(LLVMStructType llvmType, List<LLVMIdentifier<LLVMType>> arguments) {
		return new StructConstant(llvmType, arguments);
	}

	public ArrayConstant constant(LLVMArrayType llvmType, List<LLVMIdentifier<LLVMType>> arguments) {
		return new ArrayConstant(llvmType, arguments);
	}

	public <T extends LLVMType> LLVMIdentifier<LLVMPointer<T>> elementPointerTo(
	        LLVMIdentifier<LLVMArrayType<T>> identifier) {
		LLVMArrayType<T> arrayType = identifier.getType();
		T typeInArray = arrayType.getInternalType();
		LLVMPointer<T> pointer = pointer(typeInArray);
		return new LLVMIdentifier<>(pointer, "getelementptr inbounds (" + arrayType + "* " + identifier.getName()
		        + ", i32 0, i32 0)", false);
	}

	public <T extends LLVMType> FunctionSignature<T> newFunction(T type, String s,
	        List<LLVMIdentifier<? extends LLVMType>> llvmTypes) {
		return new FunctionSignature<>(type, "@" + s, llvmTypes);
	}

	public LLVMIdentifier<LLVMType> bitcast(LLVMIdentifier<LLVMType> identifier, LLVMType toType) {
		return new LLVMIdentifier<>(toType, "bitcast (" + identifier + " to " + toType + ")", false);
	}

	public <T extends LLVMType> LLVMIdentifier<T> newLocal(T type, boolean resolvable) {
		return new LLVMIdentifier<>(type, "%" + newName(), resolvable);
	}

	public <T extends LLVMType> LLVMIdentifier<T> nameless(T type) {
		return new LLVMIdentifier<>(type, "", false);
	}

	public LLVMIdentifier<LLVMVoidType> voidId() {
		return nameless(voidType());
	}
}
