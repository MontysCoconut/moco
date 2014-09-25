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

import static de.uni.bremen.monty.moco.codegeneration.types.LLVMTypeFactory.pointer;

import java.util.List;
import java.util.Arrays;
import org.apache.commons.lang3.StringUtils;

import de.uni.bremen.monty.moco.codegeneration.identifier.FunctionSignature;
import de.uni.bremen.monty.moco.codegeneration.identifier.LLVMIdentifier;
import de.uni.bremen.monty.moco.codegeneration.types.LLVMPointer;
import de.uni.bremen.monty.moco.codegeneration.types.LLVMStructType;
import de.uni.bremen.monty.moco.codegeneration.types.LLVMType;
import de.uni.bremen.monty.moco.codegeneration.types.LLVMTypeFactory.LLVMBool;
import de.uni.bremen.monty.moco.codegeneration.types.LLVMTypeFactory.LLVMIntType;

/** The CodeContext provides methods representing LLVM-Instruction.
 * 
 * In LLVM-IR is e.g. the load instruction, so there is a load Method in CodeContext.
 * 
 * For convenience CodeContext extends from Context. From the perspective of it's users you have only one instance
 * knowing where to write (Context) and how/what to write (CodeContext). */
public class CodeContext extends Context {

	/** @param commentAppender
	 *            the comment appender to be used */
	public CodeContext(CommentAppender commentAppender) {
		super(commentAppender);
	}

	// --------------Memory Access and Addressing Operations--------------------

	/** Allocates space for a variable
	 * 
	 * @param identifierOfLocalVar
	 *            identifier of the local variable
	 * @param llvmType
	 *            type of the local variable
	 * @return identifier of the local variable */
	public <T extends LLVMType> LLVMIdentifier<T> alloca(LLVMIdentifier<T> identifierOfLocalVar, LLVMType llvmType) {
		append(identifierOfLocalVar.getName() + " = alloca " + llvmType);
		return identifierOfLocalVar;
	}

	/** Dereferences a pointer
	 * 
	 * @param sourcePointer
	 *            the pointer to dereferences
	 * @param <T>
	 *            Type encapsulated from the Pointer
	 * @return Address of the dereferenced value */
	public <T extends LLVMType> LLVMIdentifier<T> load(LLVMIdentifier<LLVMPointer<T>> sourcePointer,
	        LLVMIdentifier<T> identifier) {
		append(identifier.getName() + " = load " + sourcePointer);
		return identifier;
	}

	/** Stores a value
	 * 
	 * @param source
	 *            the value
	 * @param targetPointer
	 *            target address
	 * @param <T>
	 *            the type of the value */
	public <T extends LLVMType> void store(LLVMIdentifier<T> source, LLVMIdentifier<LLVMPointer<T>> targetPointer) {
		append("store " + source + ", " + targetPointer);
	}

	/** Gets an element pointer
	 * 
	 * @param varIdentifier
	 *            the LLVMIdentifier
	 * @param pointer
	 *            the pointer
	 * @param offsets
	 *            the offset added to pointer */

	@SafeVarargs
	public final void getelementptr(LLVMIdentifier<? extends LLVMType> varIdentifier,
	        LLVMIdentifier<? extends LLVMType> pointer, LLVMIdentifier<? extends LLVMType>... offsets) {
		append(varIdentifier.getName() + " = getelementptr inbounds " + pointer + ", "

		+ StringUtils.join(offsets, ','));
	}

	// ---------------- Other Operations ---------------------------------------

	/** Compares two Integers
	 * 
	 * @param icmpOperand
	 *            How to compare
	 * @param arg1
	 *            First argument. Should be an Integer.
	 * @param arg2
	 *            Second argument. Should be an Integer.
	 * @return Address of the calculated result */
	public LLVMIdentifier<LLVMBool> icmp(IcmpOperand icmpOperand, LLVMIdentifier<?> arg1, LLVMIdentifier<?> arg2,
	        LLVMIdentifier<LLVMBool> product) {

		append(product.getName() + " = icmp " + icmpOperand.name() + " " + arg1 + "," + arg2.getName());
		return product;
	}

	/** Compares two Integers
	 * 
	 * @param fcmpOperand
	 *            How to compare
	 * @param arg1
	 *            First argument. Should be an Integer.
	 * @param arg2
	 *            Second argument. Should be an Integer.
	 * @return Address of the calculated result */
	public LLVMIdentifier<LLVMBool> fcmp(FcmpOperand fcmpOperand, LLVMIdentifier<?> arg1, LLVMIdentifier<?> arg2,
	        LLVMIdentifier<LLVMBool> product) {

		append(product.getName() + " = fcmp " + fcmpOperand.name() + " " + arg1 + "," + arg2.getName());
		return product;
	}

	/** The phi instruction.
	 * 
	 * This must be the first instruction in a basic block. identifier and label must be nonempty and of the same
	 * length.
	 * 
	 * @param type
	 *            the result type
	 * @param resolvable
	 *            if the result needs to be resolved
	 * @param identifiers
	 *            list of identifier
	 * @param labels
	 *            list of labels
	 * @param <T>
	 *            Return Type
	 * @return Identifier for result */

	public <T extends LLVMType> LLVMIdentifier<T> phi(T type, boolean resolvable, List<LLVMIdentifier<T>> identifiers,
	        LLVMIdentifier<T> identifier, List<String> labels) {

		String typeStr = resolvable ? pointer(type).toString() : type.toString();
		StringBuilder sb = new StringBuilder();
		sb.append(String.format("%s = phi %s ", identifier.getName(), typeStr));
		for (int i = 0; i < identifiers.size(); i++) {
			sb.append(String.format("[%s, %%%s]", identifiers.get(i).getName(), labels.get(i)));

			if (i != (identifiers.size() - 1)) {
				sb.append(", ");
			}
		}
		append(sb.toString());
		return identifier;
	}

	/** @param signature
	 *            Name and return type of the function
	 * @param arguments
	 *            List of Arguments
	 * @param <T>
	 *            Return Type
	 * @return Identifier for result */
	public <T extends LLVMType> LLVMIdentifier<T> call(LLVMIdentifier<LLVMType> signature,
	        LLVMIdentifier<T> identifier, LLVMIdentifier<? extends LLVMType>... arguments) {
		return call(signature, identifier, Arrays.asList(arguments));
	}

	/** @param signature
	 *            Name and return type of the function
	 * @param arguments
	 *            List of Arguments
	 * @param <T>
	 *            Return Type
	 * @return Identifier for result */
	public <T extends LLVMType> LLVMIdentifier<T> call(LLVMIdentifier<LLVMType> signature,
	        LLVMIdentifier<T> identifier, List<LLVMIdentifier<? extends LLVMType>> arguments) {
		return call(signature, identifier, arguments, "");
	}

	/** Calls a function
	 * 
	 * @param signature
	 *            Name and return type of the function
	 * @param arguments
	 *            List of Arguments
	 * @param overloadArgs
	 *            Special argument that is used for overloaded methods.
	 * @param <T>
	 *            Return Type
	 * @return Identifier for result */
	public <T extends LLVMType> LLVMIdentifier<T> call(LLVMIdentifier<LLVMType> signature,
	        LLVMIdentifier<T> identifier, List<LLVMIdentifier<? extends LLVMType>> arguments, String overloadArgs) {
		append(identifier.getName() + " = call " + signature.getType() + " " + overloadArgs + " " + signature.getName()
		        + "(" + StringUtils.join(arguments, ',') + ")");
		return identifier;
	}

	/** Calls a Procedure
	 * 
	 * @param signature
	 *            Name and return type of the function
	 * @param arguments
	 *            List of Arguments */
	public void callVoid(LLVMIdentifier<LLVMType> signature, LLVMIdentifier<?>... arguments) {
		callVoid(signature, Arrays.asList(arguments));
	}

	/** Calls a Procedure
	 * 
	 * @param signature
	 *            Name and return type of the function
	 * @param arguments
	 *            List of Arguments */
	public void callVoid(LLVMIdentifier<LLVMType> signature, List<LLVMIdentifier<?>> arguments) {
		append("call " + signature + "(" + StringUtils.join(arguments, ',') + ")");
	}

	/** Defines a function. Appends the function signature and opens a new scope. Instructions called after this will be
	 * inside this new scope until {@link #close()} is used.
	 * 
	 * @param fNAttr
	 *            LLVM-Attributes like 'ssp'
	 * @param functionSignature
	 *            function Signature: name, return type and parameter */
	public void define(List<LLVMFunctionAttribute> fNAttr, FunctionSignature<?> functionSignature) {
		emptyLine();
		append("define " + functionSignature + " " + StringUtils.join(fNAttr, ' ') + " {");
		indent();
	}

	public enum LLVMFunctionAttribute {
		ssp, nounwind
	}

	/** Appends a label
	 * 
	 * A label starts a basic block. Because a basic block must end with a terminator instruction the last instruction
	 * before this label must be a terminator instruction. `br` (branch instruction) is one of those.
	 * 
	 * @param label
	 *            name of the label */
	public void label(String label) {
		append(label + ":");
	}

	/** Append a global variable.
	 * 
	 * Dont use this method unless you know why, use global(Linkage, LLVMIdentifier<LLVMType>, boolean,
	 * LLVMIdentifier<LLVMType>) or global(Linkage, LLVMIdentifier<LLVMType>, boolean) instead. */
	public LLVMIdentifier<LLVMType> global(Linkage linkage, LLVMIdentifier<LLVMType> target, boolean isConstant,
	        String initializer) {
		String globalOrConstant = isConstant ? " constant " : " global ";
		append(target.getName() + " = " + linkage + globalOrConstant + target.getType() + initializer);
		return target;
	}

	/** Append a global variable.
	 * 
	 * initializer should be a StructConstant or an ArrayConstant. */
	public LLVMIdentifier<LLVMType> global(Linkage linkage, LLVMIdentifier<LLVMType> target, boolean isConstant,
	        LLVMIdentifier initializer) {
		return global(linkage, target, isConstant, initializer.getName());
	}

	/** Append a global variable with a zeroinitializer. */
	public LLVMIdentifier<LLVMType> global(Linkage linkage, LLVMIdentifier<LLVMType> target, boolean isConstant) {
		return global(linkage, target, isConstant, " zeroinitializer");
	}

	public enum Linkage {

		priv("private"), internal;

		private String name;

		private Linkage(String name) {
			this.name = name;
		}

		private Linkage() {

		}

		public String toString() {
			if (name == null) {
				return name();
			} else {
				return name;
			}
		}
	}

	/** Return instruction for functions
	 * 
	 * @param llvmIdentifier
	 *            Value to return */
	public void ret(LLVMIdentifier<?> llvmIdentifier) {
		append("ret " + llvmIdentifier);
	}

	/** Declares a function. Declare means the implementation of the function is somewhere else.
	 * 
	 * @param functionSignature
	 *            Name and return type of the function */
	public void declare(FunctionSignature<LLVMType> functionSignature) {
		append("declare " + functionSignature);
	}

	/** Closes the scope opened from a {@link #define(List, FunctionSignature)}. Last instruction for a function
	 * definition */
	public void close() {
		dedent();
		append("}");
		emptyLine();
	}

	/** Unconditional branch. */
	public void branch(String label) {
		append("br label %" + label);
	}

	/** Conditional branch. */
	public void branch(LLVMIdentifier<LLVMBool> value, String trueLabel, String falseLabel) {
		append("br " + value + ", label %" + trueLabel + ", label %" + falseLabel);
	}

	/** Adds a type declaration Compares two Integers
	 * 
	 * @param type
	 *            name and structur of the new type */
	public void type(LLVMStructType type, List<LLVMType> list) {
		String variableType = " = type { ";
		if (!list.isEmpty()) {
			variableType += list.get(0).toString();
			for (int i = 1; i < list.size(); i++) {
				variableType += ", " + list.get(i).toString();
			}
		}

		append(type + variableType + " }");
	}

	@SuppressWarnings("unchecked")
	public <T extends LLVMType> LLVMIdentifier<T> binaryOperation(String operator, LLVMIdentifier<?> arg1,
	        LLVMIdentifier<?> arg2, LLVMIdentifier<?> product) {
		append(product.getName() + " = " + operator + " " + arg1 + "," + arg2.getName());
		return (LLVMIdentifier<T>) product;
	}

	/** Converts a pointer to an integer */
	public void ptrtoint(LLVMIdentifier<LLVMType> target, LLVMIdentifier<LLVMPointer<LLVMType>> source) {
		append(target.getName() + " = ptrtoint " + source + " to " + target.getType());
	}

	/** Casts a pointer to a pointer of a different type */
	public <T extends LLVMType> void bitcast(LLVMIdentifier<LLVMPointer<T>> target,
	        LLVMIdentifier<LLVMPointer<T>> source) {
		append(target.getName() + " = bitcast " + source + " to " + target.getType());
	}

	/** The sext instruction.
	 * 
	 * Casts an integer of small bitsize to an integer of larger bitsize.
	 * 
	 * @param result
	 *            variable for the result
	 * @param toCast
	 *            variable to cast
	 * @param toType
	 *            type to cast to */
	public LLVMIdentifier<LLVMType> sext(LLVMIdentifier<LLVMType> toCast, LLVMIntType toType,
	        LLVMIdentifier<LLVMType> result) {
		append(result.getName() + " = sext " + toCast + " to " + toType);
		return result;
	}

	/** Compare modi for {@link #icmp(IcmpOperand, LLVMIdentifier, LLVMIdentifier, LLVMIdentifier)} */
	public enum IcmpOperand {
		/** equals */
		eq,
		/** not equals */
		ne,
		/** unsigned greater than */
		ugt,
		/** unsigned greater or equal */
		uge,
		/** unsigned less than */
		ult,
		/** unsigned less or equal */
		ule,
		/** signed greater than */
		sgt,
		/** signed greater or equal */
		sge,
		/** signed less than */
		slt,
		/** signed less or equal */
		sle;
	}

	public enum FcmpOperand {
		/** ordered and equal */
		oeq,
		/** ordered and not equal */
		one,
		/** ordered and greater than */
		ogt,
		/** ordered and greater than or equal */
		oge,
		/** ordered and less than */
		olt,
		/** ordered and less than or equal */
		ole
	}
}
