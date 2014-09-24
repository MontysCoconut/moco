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

import java.util.List;
import org.apache.commons.lang3.StringUtils;

public class LLVMTypeFactory {

	public static LLVMInt64 int64() {
		return new LLVMInt64();
	}

	public static LLVMInt8 int8() {
		return new LLVMInt8();
	}

	public static LLVMInt32 int32() {
		return new LLVMInt32();
	}

	public static LLVMBool int1() {
		return new LLVMBool();
	}

	public static LLVMFloat float32() {
		return new LLVMFloat();
	}

	public static LLVMDouble double64() {
		return new LLVMDouble();
	}

	public static <T extends LLVMType> LLVMPointer<T> pointer(T type) {
		return new LLVMPointer<>(type);
	}

	public static <T extends LLVMType> LLVMArrayType<T> array(T type, int size) {
		return new LLVMArrayType<>(type, size);
	}

	public static LLVMNotDefinedType notDefined() {
		return new LLVMNotDefinedType();
	}

	public static LLVMVoidType voidType() {
		return new LLVMVoidType();
	}

	public static interface LLVMIntType {
	}

	public static class LLVMVoidType extends LLVMSimpleType {
		public LLVMVoidType() {
			super("void");
		}
	}

	public static class LLVMNotDefinedType extends LLVMSimpleType {
		public LLVMNotDefinedType() {
			super("...");
		}
	}

	public abstract static class LLVMInt extends LLVMSimpleType {
		LLVMInt(String type) {
			super(type);
		}
	}

	public static class LLVMInt64 extends LLVMInt {
		public LLVMInt64() {
			super("i64");
		}
	}

	public static class LLVMInt8 extends LLVMInt {
		public LLVMInt8() {
			super("i8");
		}
	}

	public static class LLVMInt32 extends LLVMInt {
		public LLVMInt32() {
			super("i32");
		}
	}

	public static class LLVMBool extends LLVMSimpleType implements LLVMIntType {
		public LLVMBool() {
			super("i1");
		}
	}

	public static class LLVMFloat extends LLVMSimpleType {
		public LLVMFloat() {
			super("float");
		}
	}

	public static class LLVMDouble extends LLVMSimpleType {
		public LLVMDouble() {
			super("double");
		}
	}

	public static LLVMStructType struct(String name) {
		return new LLVMStructType("%" + name);
	}

	public static LLVMStructType struct(List<LLVMType> types) {
		return new LLVMStructType(" { " + StringUtils.join(types, ", ") + " }");
	}

	public static LLVMFunctionType function(LLVMType returnType, List<LLVMType> parameter) {
		return new LLVMFunctionType(returnType, parameter);
	}
}
