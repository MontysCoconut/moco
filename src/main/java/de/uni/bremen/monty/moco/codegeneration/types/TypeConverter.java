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

import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.ast.types.*;
import de.uni.bremen.monty.moco.codegeneration.NameMangler;
import de.uni.bremen.monty.moco.codegeneration.context.CodeContext;
import de.uni.bremen.monty.moco.codegeneration.context.CodeContext.Linkage;
import de.uni.bremen.monty.moco.codegeneration.identifier.LLVMIdentifier;
import de.uni.bremen.monty.moco.codegeneration.identifier.LLVMIdentifierFactory;

import java.util.*;

import static de.uni.bremen.monty.moco.codegeneration.types.LLVMTypeFactory.*;

public class TypeConverter {
	private Map<ConcreteType, LLVMType> typeMap = new HashMap<>();
	private LLVMIdentifierFactory llvmIdentifierFactory;
	private CodeContext constantContext;
	private NameMangler nameMangler;

	public TypeConverter(LLVMIdentifierFactory llvmIdentifierFactory, CodeContext constantContext,
	        NameMangler nameMangler) {
		this.llvmIdentifierFactory = llvmIdentifierFactory;
		this.constantContext = constantContext;
		this.nameMangler = nameMangler;
		initPreDefinedTypes();
	}

	private void initPreDefinedTypes() {
		typeMap.put(Types.voidType(), voidType());
	}

	private LLVMPointer<LLVMFunctionType> convertType(ConcreteFunctionType type) {
		List<LLVMType> parameter = new ArrayList<>();
		if (type.getDeclarationType().equals(FunctionDeclaration.DeclarationType.METHOD)) {
			parameter.add(mapToLLVMType(type.getDefiningClass()));
		}
		for (ConcreteVariableType varDecl : type.getParameter()) {
			parameter.add(mapToLLVMType(varDecl.getType()));
		}
		if (type.isFunction()) {
			return pointer(function(mapToLLVMType(type.getReturnType()), parameter));
		}
		return pointer(function(voidType(), parameter));
	}

	private <T extends LLVMType> T convertType(ConcreteType type) {
		return (T)pointer(struct(nameMangler.mangleClass(type)));
	}

	private void addType(ConcreteType concreteType) {
		String mangledNodeName = nameMangler.mangleClass(concreteType);
		LLVMStructType llvmClassType = struct(mangledNodeName);
		List<LLVMType> llvmClassTypeDeclarations = new ArrayList<>();

		LLVMStructType llvmVMTType = struct(mangledNodeName + "_vmt_type");
		List<LLVMType> llvmVMTTypeDeclarations = new ArrayList<>();
		llvmClassTypeDeclarations.add(pointer(llvmVMTType));

		LLVMIdentifier<LLVMType> llvmVMTDataIdentifier =
		        llvmIdentifierFactory.newGlobal(mangledNodeName + "_vmt_data", llvmVMTType);
		List<LLVMIdentifier<LLVMType>> llvmVMTDataInitializer = new ArrayList<>();

		List<ConcreteType> recursiveSuperClassDeclarations = concreteType.getSuperClassDeclarationsRecursive();
		LLVMArrayType llvmCTDataType = array(pointer(int8()), recursiveSuperClassDeclarations.size() + 1);
		LLVMIdentifier<LLVMType> llvmCTDataIdentifier =
		        llvmIdentifierFactory.newGlobal(mangledNodeName + "_ct_data", llvmCTDataType);
		List<LLVMIdentifier<LLVMType>> llvmCTDataInitializer = new ArrayList<>();

		llvmVMTTypeDeclarations.add(pointer(llvmCTDataType));
		llvmVMTDataInitializer.add((LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) llvmIdentifierFactory.pointerTo(llvmCTDataIdentifier));

		for (ConcreteType classDeclaration : recursiveSuperClassDeclarations) {
			// Ensure that addType() was called for this classDeclaration so that a VMT/CT was generated.
			mapToLLVMType(classDeclaration);
			String mangledClass = nameMangler.mangleClass(classDeclaration);
			LLVMIdentifier<LLVMType> vmtDataIdent =
			        llvmIdentifierFactory.newGlobal(mangledClass + "_vmt_data", pointer(struct(mangledClass
			                + "_vmt_type")));
			llvmCTDataInitializer.add(llvmIdentifierFactory.bitcast(vmtDataIdent, pointer(int8())));
		}
		llvmCTDataInitializer.add((LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) llvmIdentifierFactory.constantNull(pointer(int8())));

		if (concreteType.equals(Types.intType())) {
			llvmClassTypeDeclarations.add(LLVMTypeFactory.int64());
		} else if (concreteType.equals(Types.boolType())) {
			llvmClassTypeDeclarations.add(LLVMTypeFactory.int1());
		} else if (concreteType.equals(Types.floatType())) {
			llvmClassTypeDeclarations.add(LLVMTypeFactory.double64());
		} else if (concreteType.equals(Types.charType())) {
			llvmClassTypeDeclarations.add(LLVMTypeFactory.int8());
		} else if (concreteType.equals(Types.stringType())) {
			llvmClassTypeDeclarations.add(LLVMTypeFactory.pointer(LLVMTypeFactory.int8()));
		} else if (concreteType.equals(Types.arrayType())) {
			LLVMType llvmType = mapToLLVMType(Types.objectType());
			LLVMType array = struct(Arrays.asList(LLVMTypeFactory.int64(), LLVMTypeFactory.array(llvmType, 0)));
			llvmClassTypeDeclarations.add(LLVMTypeFactory.pointer(array));
		}

		for (ConcreteType classDeclaration : recursiveSuperClassDeclarations) {
			for (ConcreteVariableType decl : classDeclaration.getVariables()) {
				llvmClassTypeDeclarations.add(mapToLLVMType(decl.getType()));
			}
		}

		// generator functions
		if (concreteType.isGenerator()) {
			// create the structure
			LLVMType contextStruct = addGeneratorContext(concreteType, mangledNodeName);
			// add context struct as the last index to the class declaration
			llvmClassTypeDeclarations.add(contextStruct);
		}

		// closures
		if (concreteType.hasWrappedFunction()) {
			ConcreteFunctionType wrappedFunction = concreteType.getWrappedFunction();
			if (wrappedFunction.isClosure()) {
                // create the structure
                LLVMType closureContextStruct =
                        addClosureContext(concreteType, wrappedFunction.getClosureVariables(), mangledNodeName);
                // add context struct as the last index to the class declaration
                llvmClassTypeDeclarations.add(closureContextStruct);
            }
		}

		for (ConcreteFunctionType function : concreteType.getVirtualMethods()) {
			if (!function.isInitializer()) {
				LLVMType signature = mapToLLVMType(function);
				llvmVMTTypeDeclarations.add(signature);
				llvmVMTDataInitializer.add(llvmIdentifierFactory.newGlobal(
				        nameMangler.mangleFunction(function),
				        signature));
			}
		}
		constantContext.type(llvmVMTType, llvmVMTTypeDeclarations);
		constantContext.type(llvmClassType, llvmClassTypeDeclarations);
		constantContext.global(
		        Linkage.priv,
		        llvmCTDataIdentifier,
		        true,
		        llvmIdentifierFactory.constant(llvmCTDataType, llvmCTDataInitializer));
		constantContext.global(
		        Linkage.priv,
		        llvmVMTDataIdentifier,
		        true,
		        llvmIdentifierFactory.constant(llvmVMTType, llvmVMTDataInitializer));
	}

	protected LLVMType addClosureContext(ConcreteType classDecl, Collection<ConcreteVariableType> variables,
										 String mangledNodeName) {
		// declaration types inside the struct
		List<LLVMType> llvmStructTypeDeclarations = new ArrayList<>();

		// add state i8 pointer to context structure
		llvmStructTypeDeclarations.add(LLVMTypeFactory.pointer(LLVMTypeFactory.int8()));
		// add the variable declarations inside the generator to the struct
		int attributeIndex = 1;
		for (ConcreteVariableType decl : variables) {
			llvmStructTypeDeclarations.add(mapToLLVMType(decl.getType()));
			decl.setAttributeIndex(attributeIndex);
			attributeIndex += 1;
		}

		LLVMStructType llvmStructType = struct(mangledNodeName + "_closure_context");
		constantContext.type(llvmStructType, llvmStructTypeDeclarations);

		return llvmStructType;
	}

	protected LLVMType addGeneratorContext(ConcreteType classDecl, String mangledNodeName) {
		ConcreteGeneratorFunctionType inFunction = null;

		for (ConcreteFunctionType fun : classDecl.getMethods()) {
			if (fun.getIdentifier().getSymbol().equals("getNext")) {
				inFunction = (ConcreteGeneratorFunctionType) fun;
				break;
			}
		}

		// declaration types inside the struct
		List<LLVMType> llvmStructTypeDeclarations = new ArrayList<>();

		// add state i8 pointer to context structure
		llvmStructTypeDeclarations.add(LLVMTypeFactory.pointer(LLVMTypeFactory.int8()));
		// add the variable declarations inside the generator to the struct
		for (ConcreteVariableType decl : inFunction.getVariableDeclarations()) {
			llvmStructTypeDeclarations.add(mapToLLVMType(decl.getType()));
		}

		LLVMStructType llvmStructType = struct(mangledNodeName + "_context");
		constantContext.type(llvmStructType, llvmStructTypeDeclarations);

		return llvmStructType;
	}

	public LLVMPointer<LLVMFunctionType> mapToLLVMType(ConcreteFunctionType type) {
		return convertType(type);
	}

	public <T extends LLVMType> T mapToLLVMType(ConcreteType type) {
		T llvmType = (T) typeMap.get(type);
		if (llvmType == null) {
			llvmType = convertType(type);
			typeMap.put(type, llvmType);
			addType(type);
		}
		return llvmType;
	}
}
