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

package de.uni.bremen.monty.moco.codegeneration.context;

import static de.uni.bremen.monty.moco.codegeneration.types.LLVMTypeFactory.double64;
import static de.uni.bremen.monty.moco.codegeneration.types.LLVMTypeFactory.int1;
import static de.uni.bremen.monty.moco.codegeneration.types.LLVMTypeFactory.int32;
import static de.uni.bremen.monty.moco.codegeneration.types.LLVMTypeFactory.int64;
import static de.uni.bremen.monty.moco.codegeneration.types.LLVMTypeFactory.int8;
import static de.uni.bremen.monty.moco.codegeneration.types.LLVMTypeFactory.pointer;

import java.util.Arrays;

import de.uni.bremen.monty.moco.ast.CoreClasses;
import de.uni.bremen.monty.moco.codegeneration.Native;
import de.uni.bremen.monty.moco.codegeneration.CodeGenerator;
import de.uni.bremen.monty.moco.codegeneration.context.CodeContext.FcmpOperand;
import de.uni.bremen.monty.moco.codegeneration.context.CodeContext.IcmpOperand;
import de.uni.bremen.monty.moco.codegeneration.identifier.LLVMIdentifier;
import de.uni.bremen.monty.moco.codegeneration.identifier.LLVMIdentifierFactory;
import de.uni.bremen.monty.moco.codegeneration.types.*;
import de.uni.bremen.monty.moco.codegeneration.types.LLVMTypeFactory.*;

import static de.uni.bremen.monty.moco.codegeneration.types.LLVMTypeFactory.*;

public class Operations {

	private CodeGenerator codeGenerator;
	private LLVMIdentifierFactory llvmIdentifierFactory;

	private LLVMIdentifier<LLVMPointer<LLVMInt8>> stringFormat;
	private LLVMIdentifier<LLVMPointer<LLVMInt8>> intFormat;
	private LLVMIdentifier<LLVMPointer<LLVMInt8>> floatFormat;
	private LLVMIdentifier<LLVMPointer<LLVMInt8>> charFormat;

	public Operations(CodeGenerator codeGenerator, LLVMIdentifierFactory llvmIdentifierFactory) {
		this.codeGenerator = codeGenerator;
		this.llvmIdentifierFactory = llvmIdentifierFactory;
	}

	@Native("M.Print.P.print$M.Char.C.Char")
	public void printChar(CodeContext c, LLVMIdentifier<LLVMType> addr) {
		LLVMType type = addr.getType();
		LLVMIdentifier<LLVMTypeFactory.LLVMInt32> signature = llvmIdentifierFactory.newGlobal("printf", int32());
		c.call(
		        (LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) signature,
		        llvmIdentifierFactory.newLocal(signature.getType(), false),
		        Arrays.<LLVMIdentifier<?>> asList(charFormat, addr),
		        "(i8*, ...)*");
	}

	@Native("M.Print.P.print$M.String.C.String")
	public void printString(CodeContext c, LLVMIdentifier<LLVMType> addr) {
		LLVMType type = addr.getType();
		LLVMIdentifier<LLVMTypeFactory.LLVMInt32> signature = llvmIdentifierFactory.newGlobal("printf", int32());
		c.call(
		        (LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) signature,
		        llvmIdentifierFactory.newLocal(signature.getType(), false),
		        Arrays.<LLVMIdentifier<?>> asList(stringFormat, addr),
		        "(i8*, ...)*");
	}

	@Native("M.Print.P.print$M.Int.C.Int")
	public void printInt(CodeContext c, LLVMIdentifier<LLVMType> addr) {
		LLVMType type = addr.getType();
		LLVMIdentifier<LLVMTypeFactory.LLVMInt32> signature = llvmIdentifierFactory.newGlobal("printf", int32());
		c.call(
		        (LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) signature,
		        llvmIdentifierFactory.newLocal(signature.getType(), false),
		        Arrays.<LLVMIdentifier<?>> asList(intFormat, addr),
		        "(i8*, ...)*");
	}

	@Native("M.Print.P.print$M.Bool.C.Bool")
	public void printBool(CodeContext c, LLVMIdentifier<LLVMType> addr) {
		LLVMType type = addr.getType();
		LLVMIdentifier<LLVMTypeFactory.LLVMInt32> signature = llvmIdentifierFactory.newGlobal("printf", int32());
		c.call(
		        (LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) signature,
		        llvmIdentifierFactory.newLocal(signature.getType(), false),
		        Arrays.<LLVMIdentifier<?>> asList(intFormat, addr),
		        "(i8*, ...)*");
	}

	@Native("M.Print.P.print$M.Float.C.Float")
	public void printFloat(CodeContext c, LLVMIdentifier<LLVMType> addr) {
		LLVMType type = addr.getType();
		LLVMIdentifier<LLVMTypeFactory.LLVMInt32> signature = llvmIdentifierFactory.newGlobal("printf", int32());
		c.call(
		        (LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) signature,
		        llvmIdentifierFactory.newLocal(signature.getType(), false),
		        Arrays.<LLVMIdentifier<?>> asList(floatFormat, addr),
		        "(i8*, ...)*");
	}

	@Native("M.Int.C.Int.F.operator_plus$M.Int.C.Int$M.Int.C.Int")
	public LLVMIdentifier<LLVMInt> add(CodeContext c, LLVMIdentifier<LLVMInt> arg1, LLVMIdentifier<LLVMInt> arg2) {
		return c.binaryOperation("add", arg1, arg2, llvmIdentifierFactory.newLocal(arg1.getType(), false));
	}

	@Native("M.Float.C.Float.F.operator_plus$M.Float.C.Float$M.Float.C.Float")
	public LLVMIdentifier<LLVMDouble> fadd(CodeContext c, LLVMIdentifier<LLVMDouble> arg1,
	        LLVMIdentifier<LLVMDouble> arg2) {
		return c.binaryOperation("fadd", arg1, arg2, llvmIdentifierFactory.newLocal(arg1.getType(), false));
	}

	@Native("M.Int.C.Int.F.operator_minus$M.Int.C.Int$M.Int.C.Int")
	public LLVMIdentifier<LLVMInt> sub(CodeContext c, LLVMIdentifier<LLVMInt> arg1, LLVMIdentifier<LLVMInt> arg2) {
		return c.binaryOperation("sub", arg1, arg2, llvmIdentifierFactory.newLocal(arg1.getType(), false));
	}

	@Native("M.Int.C.Int.F.operator_minus$M.Int.C.Int")
	public LLVMIdentifier<LLVMInt> sub(CodeContext c, LLVMIdentifier<LLVMInt> arg1) {
		return c.binaryOperation(
		        "sub",
		        llvmIdentifierFactory.constant(int64(), 0),
		        arg1,
		        llvmIdentifierFactory.newLocal(arg1.getType(), false));
	}

	@Native("M.Float.C.Float.F.operator_minus$M.Float.C.Float")
	public LLVMIdentifier<LLVMDouble> fsub(CodeContext c, LLVMIdentifier<LLVMDouble> arg1) {
		return c.binaryOperation(
		        "fsub",
		        llvmIdentifierFactory.constant(double64(), 0.0f),
		        arg1,
		        llvmIdentifierFactory.newLocal(arg1.getType(), false));
	}

	@Native("M.Float.C.Float.F.operator_minus$M.Float.C.Float$M.Float.C.Float")
	public LLVMIdentifier<LLVMDouble> fsub(CodeContext c, LLVMIdentifier<LLVMDouble> arg1,
	        LLVMIdentifier<LLVMDouble> arg2) {
		return c.binaryOperation("fsub", arg1, arg2, llvmIdentifierFactory.newLocal(arg1.getType(), false));
	}

	@Native("M.Int.C.Int.F.operator_mult$M.Int.C.Int$M.Int.C.Int")
	public LLVMIdentifier<LLVMInt> mul(CodeContext c, LLVMIdentifier<LLVMInt> arg1, LLVMIdentifier<LLVMInt> arg2) {
		return c.binaryOperation("mul", arg1, arg2, llvmIdentifierFactory.newLocal(arg1.getType(), false));
	}

	@Native("M.Float.C.Float.F.operator_mult$M.Float.C.Float$M.Float.C.Float")
	public LLVMIdentifier<LLVMDouble> fmul(CodeContext c, LLVMIdentifier<LLVMDouble> arg1,
	        LLVMIdentifier<LLVMDouble> arg2) {
		return c.binaryOperation("fmul", arg1, arg2, llvmIdentifierFactory.newLocal(arg1.getType(), false));
	}

	@Native("M.Int.C.Int.F.operator_div$M.Int.C.Int$M.Int.C.Int")
	public LLVMIdentifier<LLVMInt> sdiv(CodeContext c, LLVMIdentifier<LLVMInt> arg1, LLVMIdentifier<LLVMInt> arg2) {
		return c.binaryOperation("sdiv", arg1, arg2, llvmIdentifierFactory.newLocal(arg1.getType(), false));
	}

	@Native("M.Float.C.Float.F.operator_div$M.Float.C.Float$M.Float.C.Float")
	public LLVMIdentifier<LLVMDouble> fdiv(CodeContext c, LLVMIdentifier<LLVMDouble> arg1,
	        LLVMIdentifier<LLVMDouble> arg2) {
		return c.binaryOperation("fdiv", arg1, arg2, llvmIdentifierFactory.newLocal(arg1.getType(), false));
	}

	@Native("M.Int.C.Int.F.operator_equal$M.Bool.C.Bool$M.Int.C.Int")
	public LLVMIdentifier<LLVMBool> intEq(CodeContext c, LLVMIdentifier<LLVMInt> arg1, LLVMIdentifier<LLVMInt> arg2) {
		return c.icmp(IcmpOperand.eq, arg1, arg2, llvmIdentifierFactory.newLocal(int1(), false));
	}

	@Native("M.Bool.C.Bool.F.operator_equal$M.Bool.C.Bool$M.Bool.C.Bool")
	public LLVMIdentifier<LLVMBool> boolEq(CodeContext c, LLVMIdentifier<LLVMBool> arg1, LLVMIdentifier<LLVMBool> arg2) {
		return c.icmp(IcmpOperand.eq, arg1, arg2, llvmIdentifierFactory.newLocal(int1(), false));
	}

	@Native("M.Float.C.Float.F.operator_equal$M.Bool.C.Bool$M.Float.C.Float")
	public LLVMIdentifier<LLVMBool> floatEq(CodeContext c, LLVMIdentifier<LLVMDouble> arg1,
	        LLVMIdentifier<LLVMDouble> arg2) {
		return c.fcmp(FcmpOperand.oeq, arg1, arg2, llvmIdentifierFactory.newLocal(int1(), false));
	}

	@Native("M.Int.C.Int.F.operator_not_equal$M.Bool.C.Bool$M.Int.C.Int")
	public LLVMIdentifier<LLVMBool> intNe(CodeContext c, LLVMIdentifier<LLVMInt> arg1, LLVMIdentifier<LLVMInt> arg2) {
		return c.icmp(IcmpOperand.ne, arg1, arg2, llvmIdentifierFactory.newLocal(int1(), false));
	}

	@Native("M.Bool.C.Bool.F.operator_not_equal$M.Bool.C.Bool$M.Bool.C.Bool")
	public LLVMIdentifier<LLVMBool> boolNe(CodeContext c, LLVMIdentifier<LLVMBool> arg1, LLVMIdentifier<LLVMBool> arg2) {
		return c.icmp(IcmpOperand.ne, arg1, arg2, llvmIdentifierFactory.newLocal(int1(), false));
	}

	@Native("M.Float.C.Float.F.operator_not_equal$M.Bool.C.Bool$M.Float.C.Float")
	public LLVMIdentifier<LLVMBool> floatNe(CodeContext c, LLVMIdentifier<LLVMDouble> arg1,
	        LLVMIdentifier<LLVMDouble> arg2) {
		return c.fcmp(FcmpOperand.one, arg1, arg2, llvmIdentifierFactory.newLocal(int1(), false));
	}

	@Native("M.Int.C.Int.F.operator_lesser$M.Bool.C.Bool$M.Int.C.Int")
	public LLVMIdentifier<LLVMBool> intSlt(CodeContext c, LLVMIdentifier<LLVMInt> arg1, LLVMIdentifier<LLVMInt> arg2) {
		return c.icmp(IcmpOperand.slt, arg1, arg2, llvmIdentifierFactory.newLocal(int1(), false));
	}

	@Native("M.Float.C.Float.F.operator_lesser$M.Bool.C.Bool$M.Float.C.Float")
	public LLVMIdentifier<LLVMBool> floatSlt(CodeContext c, LLVMIdentifier<LLVMDouble> arg1,
	        LLVMIdentifier<LLVMDouble> arg2) {
		return c.fcmp(FcmpOperand.olt, arg1, arg2, llvmIdentifierFactory.newLocal(int1(), false));
	}

	@Native("M.Int.C.Int.F.operator_lesser_equal$M.Bool.C.Bool$M.Int.C.Int")
	public LLVMIdentifier<LLVMBool> intSle(CodeContext c, LLVMIdentifier<LLVMInt> arg1, LLVMIdentifier<LLVMInt> arg2) {
		return c.icmp(IcmpOperand.sle, arg1, arg2, llvmIdentifierFactory.newLocal(int1(), false));
	}

	@Native("M.Float.C.Float.F.operator_lesser_equal$M.Bool.C.Bool$M.Float.C.Float")
	public LLVMIdentifier<LLVMBool> floatSle(CodeContext c, LLVMIdentifier<LLVMDouble> arg1,
	        LLVMIdentifier<LLVMDouble> arg2) {
		return c.fcmp(FcmpOperand.ole, arg1, arg2, llvmIdentifierFactory.newLocal(int1(), false));
	}

	@Native("M.Int.C.Int.F.operator_greater$M.Bool.C.Bool$M.Int.C.Int")
	public LLVMIdentifier<LLVMBool> intSgt(CodeContext c, LLVMIdentifier<LLVMInt> arg1, LLVMIdentifier<LLVMInt> arg2) {
		return c.icmp(IcmpOperand.sgt, arg1, arg2, llvmIdentifierFactory.newLocal(int1(), false));
	}

	@Native("M.Float.C.Float.F.operator_greater$M.Bool.C.Bool$M.Float.C.Float")
	public LLVMIdentifier<LLVMBool> floatSgt(CodeContext c, LLVMIdentifier<LLVMDouble> arg1,
	        LLVMIdentifier<LLVMDouble> arg2) {
		return c.fcmp(FcmpOperand.ogt, arg1, arg2, llvmIdentifierFactory.newLocal(int1(), false));
	}

	@Native("M.Int.C.Int.F.operator_greater_equal$M.Bool.C.Bool$M.Int.C.Int")
	public LLVMIdentifier<LLVMBool> intSge(CodeContext c, LLVMIdentifier<LLVMInt> arg1, LLVMIdentifier<LLVMInt> arg2) {
		return c.icmp(IcmpOperand.sge, arg1, arg2, llvmIdentifierFactory.newLocal(int1(), false));
	}

	@Native("M.Float.C.Float.F.operator_greater_equal$M.Bool.C.Bool$M.Float.C.Float")
	public LLVMIdentifier<LLVMBool> floatSge(CodeContext c, LLVMIdentifier<LLVMDouble> arg1,
	        LLVMIdentifier<LLVMDouble> arg2) {
		return c.fcmp(FcmpOperand.oge, arg1, arg2, llvmIdentifierFactory.newLocal(int1(), false));
	}

	@Native("M.Bool.C.Bool.F.operator_and$M.Bool.C.Bool$M.Bool.C.Bool")
	public LLVMIdentifier<LLVMBool> and(CodeContext c, LLVMIdentifier<LLVMBool> arg1, LLVMIdentifier<LLVMBool> arg2) {
		return c.binaryOperation("and", arg1, arg2, llvmIdentifierFactory.newLocal(arg1.getType(), false));
	}

	@Native("M.Bool.C.Bool.F.operator_or$M.Bool.C.Bool$M.Bool.C.Bool")
	public LLVMIdentifier<LLVMBool> or(CodeContext c, LLVMIdentifier<LLVMBool> arg1, LLVMIdentifier<LLVMBool> arg2) {
		return c.binaryOperation("or", arg1, arg2, llvmIdentifierFactory.newLocal(arg1.getType(), false));
	}

	@Native("M.Bool.C.Bool.F.operator_xor$M.Bool.C.Bool$M.Bool.C.Bool")
	public LLVMIdentifier<LLVMBool> xor(CodeContext c, LLVMIdentifier<LLVMBool> arg1, LLVMIdentifier<LLVMBool> arg2) {
		return c.binaryOperation("xor", arg1, arg2, llvmIdentifierFactory.newLocal(arg1.getType(), false));
	}

	@Native("M.Bool.C.Bool.F.operator_not$M.Bool.C.Bool")
	public LLVMIdentifier<LLVMBool> not(CodeContext c, LLVMIdentifier<LLVMBool> arg1) {
		return c.binaryOperation(
		        "xor",
		        arg1,
		        llvmIdentifierFactory.constant(int1(), 1),
		        llvmIdentifierFactory.newLocal(arg1.getType(), false));
	}

	@Native("M.Int.C.Int.F.operator_rem$M.Int.C.Int$M.Int.C.Int")
	public LLVMIdentifier<LLVMInt> srem(CodeContext c, LLVMIdentifier<LLVMInt> arg1, LLVMIdentifier<LLVMInt> arg2) {
		return c.binaryOperation("srem", arg1, arg2, llvmIdentifierFactory.newLocal(arg1.getType(), false));
	}

	@Native("M.Array.F.operator_array_access$M.Int.C.Int$M.std.C.Array$M.Int.C.Int")
	public LLVMIdentifier<LLVMType> arrayAccess(CodeContext c, LLVMIdentifier<LLVMPointer> arrayPointer,
	        LLVMIdentifier<LLVMInt> index) {

		LLVMIdentifier<LLVMPointer<LLVMStructType>> arrayStructPointer =
		        (LLVMIdentifier<LLVMPointer<LLVMStructType>>) (LLVMIdentifier<?>) arrayPointer;

		codeGenerator.checkArrayBounds(c, arrayStructPointer, index);
		LLVMIdentifier<LLVMType> result =
		        llvmIdentifierFactory.newLocal(codeGenerator.mapToLLVMType(CoreClasses.intType()));
		c.getelementptr(
		        result,
		        arrayStructPointer,
		        llvmIdentifierFactory.constant(int32(), 0),
		        llvmIdentifierFactory.constant(int32(), 1),
		        index);
		return result;
	}

	public void setStringFormat(LLVMIdentifier<LLVMPointer<LLVMInt8>> stringFormat) {
		this.stringFormat = stringFormat;
	}

	public void setIntFormat(LLVMIdentifier<LLVMPointer<LLVMInt8>> intFormat) {
		this.intFormat = intFormat;
	}

	public void setFloatFormat(LLVMIdentifier<LLVMPointer<LLVMInt8>> floatFormat) {
		this.floatFormat = floatFormat;
	}

	public void setCharFormat(LLVMIdentifier<LLVMPointer<LLVMInt8>> charformat) {
		this.charFormat = charformat;
	}
}
