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
import de.uni.bremen.monty.moco.ast.CoreClasses;
import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.codegeneration.context.CodeContext;
import de.uni.bremen.monty.moco.codegeneration.context.CodeContext.LLVMFunctionAttribute;
import de.uni.bremen.monty.moco.codegeneration.context.CodeContext.Linkage;
import de.uni.bremen.monty.moco.codegeneration.context.Operations;
import de.uni.bremen.monty.moco.codegeneration.identifier.FunctionSignature;
import de.uni.bremen.monty.moco.codegeneration.identifier.LLVMIdentifier;
import de.uni.bremen.monty.moco.codegeneration.identifier.LLVMIdentifierFactory;
import de.uni.bremen.monty.moco.codegeneration.types.*;
import de.uni.bremen.monty.moco.codegeneration.voodoo.BlackMagic;

import java.util.*;

import static de.uni.bremen.monty.moco.codegeneration.types.LLVMTypeFactory.*;

/** This class should contain most of the logic for the CodeGeneration. Mainly methods are called from the
 * CodeGenerationVisitor and have a Parameter 'CodeContext c'. On this argument LLVM instructions can be executed.
 * 
 * It is an mapping layer between the CodeGenerationVisitor on one side and CodeContext on the other. The
 * CodeGenerationVisitor is mainly influenced by the Monty-AST and the CodeContext only know LLVM instructions. So the
 * CodeGenerator is the where most parts of the mapping between those two languages exists.
 * 
 * An simple task of mapping is e.g. map from monty-Types (TypeDeclaration) to LLVM-Types(LLVMType) */
public class CodeGenerator {
	private final Operations operations;
	private final BlackMagic blackMagic;
	private final TypeConverter typeConverter;

	/*
	 * Map an ASTNode to a label prefix.
	 */
	protected HashMap<ASTNode, String> node2label = new HashMap<>();

	/*
	 * Map each label to its number of occurrences. We use this information to create unique labels in the resulting
	 * LLVM code.
	 */
	protected HashMap<String, Integer> label2occurrences = new HashMap<>();

	private LLVMIdentifierFactory llvmIdentifierFactory;

	public CodeGenerator(TypeConverter typeConverter, LLVMIdentifierFactory llvmIdentifierFactory) {
		this.typeConverter = typeConverter;
		this.llvmIdentifierFactory = llvmIdentifierFactory;
		operations = new Operations(this, llvmIdentifierFactory);
		blackMagic = new BlackMagic(operations);
		initFormatStrings();
	}

	public void initFormatStrings() {
		LLVMArrayType<LLVMInt8> stringType = array(int8(), 3);
		LLVMIdentifier<LLVMArrayType<LLVMInt8>> stringFormatIdent =
		        llvmIdentifierFactory.newGlobal(".stringFormat", stringType);
		operations.setStringFormat(llvmIdentifierFactory.elementPointerTo(stringFormatIdent));
		LLVMIdentifier<LLVMArrayType<LLVMInt8>> intFormatIdent =
		        llvmIdentifierFactory.newGlobal(".intFormat", stringType);
		operations.setIntFormat(llvmIdentifierFactory.elementPointerTo(intFormatIdent));
		LLVMIdentifier<LLVMArrayType<LLVMInt8>> floatFormatIdent =
		        llvmIdentifierFactory.newGlobal(".floatFormat", stringType);
		operations.setFloatFormat(llvmIdentifierFactory.elementPointerTo(floatFormatIdent));
		LLVMIdentifier<LLVMArrayType<LLVMInt8>> charFormatIdent =
		        llvmIdentifierFactory.newGlobal(".charFormat", stringType);
		operations.setCharFormat(llvmIdentifierFactory.elementPointerTo(charFormatIdent));
	}

	private <T extends LLVMType> LLVMIdentifier<LLVMPointer<T>> castIfNeeded(CodeContext c,
	        LLVMIdentifier<LLVMPointer<T>> variable, LLVMPointer<T> toType) {
		if (!variable.getType().equals(toType)) {
			LLVMIdentifier<LLVMPointer<T>> castedVariable =
			        llvmIdentifierFactory.newLocal(toType, variable.needToBeResolved());
			c.bitcast(castedVariable, variable);
			return castedVariable;
		}
		return variable;
	}

	private <T extends LLVMType> LLVMIdentifier<T> castIfNeeded(CodeContext c, LLVMIdentifier<T> variable, T toType) {
		if ((variable.getType() instanceof LLVMPointer) && (toType instanceof LLVMPointer)) {
			return (LLVMIdentifier<T>) (LLVMIdentifier<?>) castIfNeeded(
			        c,
			        (LLVMIdentifier<LLVMPointer<LLVMType>>) (LLVMIdentifier<?>) variable,
			        (LLVMPointer<LLVMType>) toType);
		}
		return variable;
	}

	private <T extends LLVMType> LLVMIdentifier<T> resolveIfNeeded(CodeContext c, LLVMIdentifier<T> addr) {
		if (addr.needToBeResolved()) {
			LLVMIdentifier<LLVMPointer<T>> sourcePointer = llvmIdentifierFactory.pointerTo(addr);
			LLVMIdentifier<T> targetPointer = llvmIdentifierFactory.newLocal(addr.getType(), false);
			return c.load(sourcePointer, targetPointer);
		} else {
			return addr;
		}
	}

	private List<LLVMIdentifier<? extends LLVMType>> resolveArgumentsIfNeeded(CodeContext c,
	        List<LLVMIdentifier<?>> arguments, List<TypeDeclaration> parameters) {
		List<LLVMIdentifier<? extends LLVMType>> resolvedArguments = new ArrayList<>(arguments.size());
		for (int i = 0; i < arguments.size(); i++) {
			LLVMIdentifier<LLVMType> resolvedArgument = resolveIfNeeded(c, (LLVMIdentifier<LLVMType>) arguments.get(i));
			LLVMType expectedType = mapToLLVMType(parameters.get(i));
			resolvedArguments.add(castIfNeeded(c, resolvedArgument, expectedType));
		}
		return resolvedArguments;
	}

	private List<LLVMIdentifier<? extends LLVMType>> unboxArgumentsIfNeeded(CodeContext c,
	        List<LLVMIdentifier<?>> arguments) {
		List<LLVMIdentifier<? extends LLVMType>> unboxedArguments = new ArrayList<>(arguments.size());
		for (LLVMIdentifier<?> llvmIdentifier : arguments) {
			if (llvmIdentifier.getType().equals(mapToLLVMType(CoreClasses.intType()))) {
				unboxedArguments.add(unboxType(c, (LLVMIdentifier<LLVMType>) llvmIdentifier, int64()));
			} else if (llvmIdentifier.getType().equals(mapToLLVMType(CoreClasses.boolType()))) {
				unboxedArguments.add(unboxType(c, (LLVMIdentifier<LLVMType>) llvmIdentifier, int1()));
			} else if (llvmIdentifier.getType().equals(mapToLLVMType(CoreClasses.floatType()))) {
				unboxedArguments.add(unboxType(c, (LLVMIdentifier<LLVMType>) llvmIdentifier, double64()));
			} else if (llvmIdentifier.getType().equals(mapToLLVMType(CoreClasses.charType()))) {
				unboxedArguments.add(unboxType(c, (LLVMIdentifier<LLVMType>) llvmIdentifier, int8()));
			} else {
				unboxedArguments.add(llvmIdentifier);
			}
		}
		return unboxedArguments;
	}

	private LLVMIdentifier<LLVMArrayType<LLVMInt8>> addStringToDataField(CodeContext c, String value) {
		int length = value.length() + 1;

		LLVMArrayType<LLVMInt8> type = array(int8(), length);
		LLVMIdentifier<LLVMArrayType<LLVMInt8>> identifier = llvmIdentifierFactory.newGlobal(type);
		String internValue = "c\"" + value + "\\00\";";
		c.global(Linkage.priv, (LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) identifier, true, internValue);
		return identifier;
	}

	private LLVMIdentifier<LLVMPointer<LLVMType>> getVMTPointer(CodeContext c,
	        LLVMIdentifier<LLVMPointer<LLVMType>> selfReference, ClassDeclaration classDeclaration) {
		LLVMPointer<LLVMType> vmtType =
		        pointer((LLVMType) struct(classDeclaration.getMangledIdentifier().getSymbol() + "_vmt_type"));
		LLVMIdentifier<LLVMPointer<LLVMType>> vmtPointer = llvmIdentifierFactory.newLocal(vmtType, true);
		c.getelementptr(
		        (LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) vmtPointer,
		        selfReference,
		        llvmIdentifierFactory.constant(int32(), 0),
		        llvmIdentifierFactory.constant(int32(), 0));
		return vmtPointer;
	}

	private LLVMIdentifier<LLVMPointer<LLVMFunctionType>> getFunctionPointer(CodeContext c,
	        LLVMIdentifier<LLVMPointer<LLVMType>> selfReference, ProcedureDeclaration declaration) {
		LLVMIdentifier<LLVMPointer<LLVMType>> vmtPointer =
		        getVMTPointer(c, selfReference, (ClassDeclaration) declaration.getParentNode().getParentNode());

		LLVMPointer<LLVMFunctionType> functionType = mapToLLVMType(declaration);
		LLVMIdentifier<LLVMPointer<LLVMFunctionType>> functionPointer = llvmIdentifierFactory.newLocal(functionType);
		c.getelementptr(
		        functionPointer,
		        resolveIfNeeded(c, vmtPointer),
		        llvmIdentifierFactory.constant(int32(), 0),
		        llvmIdentifierFactory.constant(int32(), declaration.getVMTIndex()));
		return functionPointer;
	}

	public void buildConstructor(CodeContext c, ClassDeclaration classDeclaration) {
		List<LLVMIdentifier<? extends LLVMType>> llvmParameter = new ArrayList<>();
		String constructorName = classDeclaration.getMangledIdentifier().getSymbol() + "_constructor";
		addFunction(c, classDeclaration, llvmParameter, constructorName);

		LLVMPointer<LLVMType> selfType = mapToLLVMType(classDeclaration);
		LLVMIdentifier<LLVMPointer<LLVMType>> selfReference = llvmIdentifierFactory.newLocal(selfType, false);
		malloc(c, selfReference);

		LLVMIdentifier<LLVMType> vmtPointer =
		        (LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) getVMTPointer(c, selfReference, classDeclaration);
		LLVMIdentifier<LLVMType> vmtData =
		        llvmIdentifierFactory.newGlobal(
		                classDeclaration.getMangledIdentifier().getSymbol() + "_vmt_data",
		                vmtPointer.getType());
		c.store(vmtData, llvmIdentifierFactory.pointerTo(vmtPointer));

		returnValue(c, (LLVMIdentifier) selfReference, classDeclaration);
	}

	public LLVMIdentifier<LLVMType> callConstructor(CodeContext c, ClassDeclaration classDeclaration) {
		LLVMIdentifier<LLVMType> result = llvmIdentifierFactory.newLocal(mapToLLVMType(classDeclaration), false);
		LLVMIdentifier<LLVMType> signature =
		        llvmIdentifierFactory.newGlobal(
		                classDeclaration.getMangledIdentifier().getSymbol() + "_constructor",
		                result.getType());
		c.call(signature, result);
		return result;
	}

	/** Create a unique label prefix and store it under an association with the given node. */
	public String createLabelPrefix(String name, ASTNode node) {
		if (!label2occurrences.containsKey(name)) {
			label2occurrences.put(name, 0);
		}
		int id = label2occurrences.get(name);
		String label = name + id;
		label2occurrences.put(name, id + 1);
		node2label.put(node, label);
		return label;
	}

	/** Get the unique label prefix associated with the given node.
	 * 
	 * This is a Map lookup so the error will be thrown if node does not exist as key */
	public String getLabelPrefix(ASTNode node) {
		return node2label.get(node);
	}

	public LLVMIdentifier<LLVMType> declareGlobalVariable(CodeContext c, String name, TypeDeclaration type) {
		LLVMType llvmType = mapToLLVMType(type);
		LLVMIdentifier<LLVMType> variable = llvmIdentifierFactory.newGlobal(name, llvmType);
		c.global(Linkage.priv, variable, false);
		return variable;
	}

	public LLVMIdentifier<LLVMType> declareLocalVariable(CodeContext c, String name, TypeDeclaration type) {
		LLVMType llvmType = mapToLLVMType(type);

		LLVMIdentifier<LLVMType> variable = llvmIdentifierFactory.newLocal(name, llvmType, true);
		c.alloca(variable, llvmType);
		return variable;
	}

	public <T extends LLVMType> LLVMIdentifier<T> resolveLocalVarName(String name, TypeDeclaration type,
	        boolean resolvable) {
		T llvmType = mapToLLVMType(type);
		return llvmIdentifierFactory.newLocal(name, llvmType, resolvable);
	}

	public <T extends LLVMType> LLVMIdentifier<T> resolveGlobalVarName(String name, TypeDeclaration type) {
		T llvmType = mapToLLVMType(type);
		return llvmIdentifierFactory.newGlobal(name, llvmType);
	}

	public void addMain(CodeContext active) {

		FunctionSignature<?> mainFunction =
		        llvmIdentifierFactory.newFunction(
		                int32(),
		                "main",
		                Collections.<LLVMIdentifier<? extends LLVMType>> emptyList());

		active.define(Arrays.asList(LLVMFunctionAttribute.ssp), mainFunction);
		active.label("entry");
	}

	public void addFunction(CodeContext c, TypeDeclaration returnType,
	        List<LLVMIdentifier<? extends LLVMType>> llvmParameter, String name) {
		LLVMType llvmReturnType = mapToLLVMType(returnType);
		c.define(
		        new ArrayList<LLVMFunctionAttribute>(),
		        llvmIdentifierFactory.newFunction(llvmReturnType, name, llvmParameter));
		c.label("entry");
	}

	public void addNativeFunction(CodeContext c, TypeDeclaration returnType,
	        List<LLVMIdentifier<? extends LLVMType>> llvmParameter, String name) {
		addFunction(c, returnType, llvmParameter, name);

		List<LLVMIdentifier<?>> unboxedParameter = unboxArgumentsIfNeeded(c, llvmParameter);

		LLVMIdentifier<LLVMType> result =
		        (LLVMIdentifier<LLVMType>) blackMagic.generateNativeFunction(c, name, unboxedParameter);
		if (result != null) {
			if (result.getType() instanceof LLVMPointer) {
				returnValue(c, result, returnType);
			} else {
				returnValue(c, boxType(c, result, returnType), returnType);
			}
		} else {
			returnValue(
			        c,
			        (LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) llvmIdentifierFactory.voidId(),
			        CoreClasses.voidType());
		}
	}

	public void returnMain(CodeContext c) {
		c.ret(llvmIdentifierFactory.constant(int32(), 0));
	}

	public void returnValue(CodeContext c, LLVMIdentifier<LLVMType> returnValue, TypeDeclaration expectedType) {
		LLVMIdentifier<LLVMType> resolved = resolveIfNeeded(c, returnValue);
		LLVMIdentifier<LLVMType> casted = castIfNeeded(c, resolved, mapToLLVMType(expectedType));
		c.ret(casted);
	}

	public LLVMIdentifier<LLVMPointer<LLVMInt8>> addConstantString(CodeContext constant, String value) {
		LLVMIdentifier<LLVMArrayType<LLVMInt8>> nameOfDataField = addStringToDataField(constant, value);
		LLVMIdentifier<LLVMPointer<LLVMInt8>> stringAsCharPointer =
		        llvmIdentifierFactory.elementPointerTo(nameOfDataField);
		return stringAsCharPointer;
	}

	public void exit(CodeContext c, int statusCode) {
		LLVMIdentifier<LLVMType> signature = llvmIdentifierFactory.newGlobal("exit", (LLVMType) voidType());
		c.callVoid(signature, llvmIdentifierFactory.constant(int32(), statusCode));
	}

	/** Allocates heap memory for the given type and return a typed pointer. */
	public <T extends LLVMType> LLVMIdentifier<LLVMPointer<T>> malloc(CodeContext c,
	        LLVMIdentifier<LLVMPointer<T>> result) {
		return malloc(c, result, (LLVMPointer<LLVMType>) result.getType());
	}

	/** Allocates heap memory for the given type and return a typed pointer. */
	public <T extends LLVMType> LLVMIdentifier<LLVMPointer<T>> malloc(CodeContext c,
	        LLVMIdentifier<LLVMPointer<T>> result, LLVMPointer<LLVMType> inputType) {

		LLVMIdentifier<LLVMPointer<LLVMType>> sizePtr = llvmIdentifierFactory.newLocal(inputType);
		c.getelementptr(
		        sizePtr,
		        llvmIdentifierFactory.constantNull(inputType),
		        llvmIdentifierFactory.constant(int32(), 1));
		LLVMIdentifier<LLVMType> sizeInt = llvmIdentifierFactory.newLocal((LLVMType) int32());
		c.ptrtoint(sizeInt, (LLVMIdentifier) sizePtr);

		LLVMIdentifier<LLVMPointer<LLVMInt8>> s = llvmIdentifierFactory.newGlobal("malloc", pointer(int8()));
		LLVMIdentifier<LLVMPointer<LLVMInt8>> mallocPtr = llvmIdentifierFactory.newLocal(s.getType());
		c.call((LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) s, mallocPtr, sizeInt);
		c.bitcast((LLVMIdentifier) result, (LLVMIdentifier) mallocPtr);
		return result;
	}

	public <T extends LLVMType> void assign(CodeContext c, LLVMIdentifier<T> target, LLVMIdentifier<T> source) {
		source = resolveIfNeeded(c, source);
		source = castIfNeeded(c, source, target.getType());
		LLVMIdentifier<LLVMPointer<T>> targetPointer = llvmIdentifierFactory.pointerTo(target);
		c.store(source, targetPointer);
	}

	public LLVMIdentifier<LLVMType> accessMember(CodeContext c, LLVMIdentifier<LLVMPointer<LLVMType>> pointer,
	        int attributeOffset, TypeDeclaration type, boolean load) {

		LLVMIdentifier<LLVMType> result = llvmIdentifierFactory.newLocal(mapToLLVMType(type), load);
		c.getelementptr(
		        result,
		        resolveIfNeeded(c, pointer),
		        llvmIdentifierFactory.constant(int32(), 0),
		        llvmIdentifierFactory.constant(int32(), attributeOffset));
		return result;
	}

	public <T extends LLVMType> T mapToLLVMType(TypeDeclaration type) {
		return typeConverter.mapToLLVMType(type);
	}

	public LLVMIdentifier<LLVMPointer<LLVMType>> castClass(CodeContext c,
	        LLVMIdentifier<LLVMPointer<LLVMType>> pointer, ClassDeclaration sourceType, ClassDeclaration resultType,
	        String labelPrefix) {

		String successLabel = labelPrefix + ".success";
		String failureLabel = labelPrefix + ".failure";

		pointer = resolveIfNeeded(c, pointer);
		LLVMIdentifier<LLVMBool> isaCmpResult = isClass(c, pointer, sourceType, resultType);

		c.branch(isaCmpResult, successLabel, failureLabel);
		c.label(failureLabel);
		exit(c, 1);
		c.branch(successLabel);
		c.label(successLabel);
		return castIfNeeded(c, pointer, (LLVMPointer<LLVMType>) mapToLLVMType(resultType));
	}

	public LLVMIdentifier<LLVMBool> isClass(CodeContext c, LLVMIdentifier<LLVMPointer<LLVMType>> pointer,
	        ClassDeclaration sourceType, ClassDeclaration resultType) {

		pointer = resolveIfNeeded(c, pointer);
		LLVMIdentifier<LLVMPointer<LLVMType>> vmt = getVMTPointer(c, pointer, sourceType);
		vmt = resolveIfNeeded(c, vmt);

		LLVMType ctType = array(pointer(int8()), sourceType.getSuperClassDeclarationsRecursive().size() + 1);
		LLVMIdentifier<LLVMPointer<LLVMType>> ct = llvmIdentifierFactory.newLocal(pointer(ctType), true);
		c.getelementptr(ct, vmt, llvmIdentifierFactory.constant(int32(), 0), llvmIdentifierFactory.constant(int32(), 0));
		ct = resolveIfNeeded(c, ct);

		LLVMType pointerArrayType = array(pointer(int8()), 0);
		LLVMIdentifier<LLVMPointer<LLVMType>> pointerArray = llvmIdentifierFactory.newLocal(pointer(pointerArrayType));
		c.bitcast(pointerArray, ct);

		LLVMType resultVMTType = struct(resultType.getMangledIdentifier().getSymbol() + "_vmt_type");
		LLVMIdentifier<LLVMPointer<LLVMType>> resultVMT =
		        llvmIdentifierFactory.newGlobal(
		                resultType.getMangledIdentifier().getSymbol() + "_vmt_data",
		                pointer(resultVMTType));
		LLVMIdentifier<LLVMPointer<LLVMType>> resultPointer =
		        llvmIdentifierFactory.newLocal(pointer((LLVMType) int8()));
		c.bitcast(resultPointer, resultVMT);

		LLVMIdentifier<LLVMBool> isaCmpResult = llvmIdentifierFactory.newLocal(int1(), false);
		LLVMIdentifier<LLVMBool> isaSignature = llvmIdentifierFactory.newGlobal("vmt_isa_class", int1());
		c.call((LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) isaSignature, isaCmpResult, pointerArray, resultPointer);
		return isaCmpResult;
	}

	public void checkArrayBounds(CodeContext c, LLVMIdentifier<LLVMPointer<LLVMStructType>> array,
	        LLVMIdentifier<LLVMInt> index) {

		List<LLVMType> arrayTypeList = Arrays.asList(int64(), array(pointer(int8()), 0));
		array = castIfNeeded(c, array, pointer(struct(arrayTypeList)));
		LLVMIdentifier<LLVMVoidType> boundsCheckSignature =
		        llvmIdentifierFactory.newGlobal("array_bounds_check", voidType());
		c.callVoid((LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) boundsCheckSignature, array, index);
	}

	public LLVMIdentifier<?> call(CodeContext c, String functionName, TypeDeclaration returnType,
	        List<LLVMIdentifier<?>> arguments, List<TypeDeclaration> parameters) {
		LLVMType llvmReturnType = mapToLLVMType(returnType);
		LLVMIdentifier<LLVMType> functionSignature = llvmIdentifierFactory.newGlobal(functionName, llvmReturnType);
		List<LLVMIdentifier<? extends LLVMType>> resolvedArguments = resolveArgumentsIfNeeded(c, arguments, parameters);
		return c.call(
		        functionSignature,
		        llvmIdentifierFactory.newLocal(functionSignature.getType(), false),
		        resolvedArguments);
	}

	public void callVoid(CodeContext c, String functionName, List<LLVMIdentifier<?>> arguments,
	        List<TypeDeclaration> parameters) {
		List<LLVMIdentifier<?>> resolvedArguments = resolveArgumentsIfNeeded(c, arguments, parameters);
		c.callVoid(llvmIdentifierFactory.newGlobal(functionName, (LLVMType) voidType()), resolvedArguments);
	}

	public LLVMIdentifier<?> callMethod(CodeContext c, FunctionDeclaration declaration,
	        List<LLVMIdentifier<?>> arguments, List<TypeDeclaration> parameters) {
		List<LLVMIdentifier<?>> resolvedArguments = resolveArgumentsIfNeeded(c, arguments, parameters);

		LLVMIdentifier<LLVMPointer<LLVMFunctionType>> functionPointer =
		        getFunctionPointer(c, (LLVMIdentifier<LLVMPointer<LLVMType>>) resolvedArguments.get(0), declaration);
		return c.call(
		        (LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) resolveIfNeeded(c, functionPointer),
		        llvmIdentifierFactory.newLocal(mapToLLVMType(declaration.getReturnType()), false),
		        resolvedArguments);
	}

	public void callVoidMethod(CodeContext c, ProcedureDeclaration declaration, List<LLVMIdentifier<?>> arguments,
	        List<TypeDeclaration> parameters) {
		List<LLVMIdentifier<?>> resolvedArguments = resolveArgumentsIfNeeded(c, arguments, parameters);

		LLVMIdentifier<LLVMPointer<LLVMFunctionType>> functionPointer =
		        getFunctionPointer(c, (LLVMIdentifier<LLVMPointer<LLVMType>>) resolvedArguments.get(0), declaration);
		c.callVoid(
		        (LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) resolveIfNeeded(c, functionPointer),
		        resolvedArguments);
	}

	public void branch(CodeContext c, LLVMIdentifier<LLVMType> expression, String trueLabel, String falseLabel) {
		LLVMIdentifier<LLVMType> boxedValue = resolveIfNeeded(c, expression);
		LLVMIdentifier<LLVMBool> value = unboxType(c, boxedValue, int1());
		c.branch(value, trueLabel, falseLabel);
	}

	public LLVMIdentifier<LLVMInt64> loadInt(Integer value) {
		return llvmIdentifierFactory.constant(int64(), value);
	}

	public LLVMIdentifier<LLVMDouble> loadFloat(Float value) {
		return llvmIdentifierFactory.constant(double64(), value);
	}

	public LLVMIdentifier<LLVMBool> loadBool(Boolean value) {
		return llvmIdentifierFactory.constant(int1(), value);
	}

	public LLVMIdentifier<LLVMInt8> loadChar(Character value) {
		return llvmIdentifierFactory.constant(int8(), value);
	}

	public LLVMIdentifier<LLVMPointer<LLVMStructType>> addArray(CodeContext c, int size, ClassDeclaration type) {

		LLVMPointer<LLVMStructType> array = mapToLLVMType(type);
		LLVMIdentifier<LLVMPointer<LLVMStructType>> var = llvmIdentifierFactory.newLocal(array, false);

		// Temporary until object or generic
		LLVMType arrayType = mapToLLVMType((TypeDeclaration) CoreClasses.intType());
		List<LLVMType> mallocList = Arrays.asList(int64(), array(arrayType, size));
		malloc(c, var, pointer((LLVMType) struct(mallocList)));

		LLVMIdentifier<LLVMType> arraySize =
		        (LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) llvmIdentifierFactory.constant(int64(), size);
		LLVMIdentifier<LLVMPointer<LLVMType>> sizeField = llvmIdentifierFactory.newLocal(pointer(arraySize.getType()));
		c.getelementptr(
		        sizeField,
		        var,
		        llvmIdentifierFactory.constant(int32(), 0),
		        llvmIdentifierFactory.constant(int32(), 0));
		c.store(arraySize, sizeField);

		return var;
	}

	public void setArrayElement(CodeContext c, LLVMIdentifier<LLVMPointer<LLVMStructType>> array, int index,
	        LLVMIdentifier<LLVMType> value) {

		// Temporary until object or generic
		LLVMType internalType = mapToLLVMType((TypeDeclaration) CoreClasses.intType());
		LLVMIdentifier<LLVMPointer<LLVMType>> element = llvmIdentifierFactory.newLocal(pointer(internalType));

		c.getelementptr(
		        element,
		        array,
		        llvmIdentifierFactory.constant(int32(), 0),
		        llvmIdentifierFactory.constant(int32(), 1),
		        llvmIdentifierFactory.constant(int32(), index));
		c.store(value, element);
	}

	public LLVMIdentifier<LLVMType> boxType(CodeContext c, LLVMIdentifier<LLVMType> toBox, TypeDeclaration type) {

		LLVMIdentifier<LLVMType> boxedValue = callConstructor(c, (ClassDeclaration) type);
		LLVMIdentifier<LLVMType> boxedValueField = llvmIdentifierFactory.newLocal(toBox.getType());
		c.getelementptr(
		        boxedValueField,
		        boxedValue,
		        llvmIdentifierFactory.constant(int32(), 0),
		        llvmIdentifierFactory.constant(int32(), 1));
		LLVMIdentifier<LLVMPointer<LLVMType>> targetPointer = llvmIdentifierFactory.pointerTo(boxedValueField);
		c.store(toBox, targetPointer);
		return boxedValue;
	}

	public <T extends LLVMType> LLVMIdentifier<T> unboxType(CodeContext c, LLVMIdentifier<LLVMType> toUnbox, T llvmtype) {
		toUnbox = resolveIfNeeded(c, toUnbox);
		LLVMIdentifier<T> unboxedValue = llvmIdentifierFactory.newLocal(llvmtype);

		c.getelementptr(
		        unboxedValue,
		        toUnbox,
		        llvmIdentifierFactory.constant(int32(), 0),
		        llvmIdentifierFactory.constant(int32(), 1));
		LLVMIdentifier<LLVMPointer<T>> sourcePointer = llvmIdentifierFactory.pointerTo(unboxedValue);
		LLVMIdentifier<T> targetPointer = llvmIdentifierFactory.newLocal(unboxedValue.getType(), false);

		c.load(sourcePointer, targetPointer);
		return targetPointer;
	}
}
