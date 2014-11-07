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

import de.uni.bremen.monty.moco.ast.ASTNode;
import de.uni.bremen.monty.moco.ast.CoreClasses;
import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.codegeneration.identifier.LLVMIdentifier;
import de.uni.bremen.monty.moco.codegeneration.identifier.LLVMIdentifierFactory;
import de.uni.bremen.monty.moco.codegeneration.context.CodeContext;
import de.uni.bremen.monty.moco.codegeneration.context.CodeContext.Linkage;

import java.util.*;

import static de.uni.bremen.monty.moco.codegeneration.types.LLVMTypeFactory.*;

public class TypeConverter {
	private Map<TypeDeclaration, LLVMType> typeMap = new HashMap<>();
	private LLVMIdentifierFactory llvmIdentifierFactory;
	private CodeContext constantContext;

	public TypeConverter(LLVMIdentifierFactory llvmIdentifierFactory, CodeContext constantContext) {
		this.llvmIdentifierFactory = llvmIdentifierFactory;
		this.constantContext = constantContext;
		initPreDefinedTypes();
	}

	private void initPreDefinedTypes() {
		typeMap.put(CoreClasses.voidType(), voidType());
	}

	private LLVMPointer<LLVMFunctionType> convertType(ProcedureDeclaration type) {
		List<LLVMType> parameter = new ArrayList<>();
		final ASTNode grandFatherNode = type.getParentNode().getParentNode();
		if (grandFatherNode instanceof ClassDeclaration) {
			ClassDeclaration typeDeclaration = (ClassDeclaration) grandFatherNode;
			parameter.add(mapToLLVMType(typeDeclaration));
		}
		for (VariableDeclaration varDecl : type.getParameter()) {
			parameter.add(mapToLLVMType(varDecl.getType()));
		}
		if (type instanceof FunctionDeclaration) {
			FunctionDeclaration func = (FunctionDeclaration) type;
			return pointer(function(mapToLLVMType(func.getReturnType()), parameter));
		}
		return pointer(function(voidType(), parameter));
	}

	private LLVMPointer<LLVMStructType> convertType(ClassDeclaration type) {
		return pointer(struct(type.getMangledIdentifier().getSymbol()));
	}

	public TypeDeclaration mapToBoxedType(LLVMType type) {
		if (type instanceof LLVMBool) {
			return CoreClasses.boolType();
		} else if (type instanceof LLVMInt) {
			return CoreClasses.intType();
		} else if (type instanceof LLVMDouble) {
			return CoreClasses.floatType();
		} else if (type instanceof LLVMInt8) {
			return CoreClasses.charType();
		}
		return null;
	}

	private <T extends LLVMType> T convertType(TypeDeclaration type) {
		if (type instanceof ProcedureDeclaration) {
			return (T) convertType((ProcedureDeclaration) type);
		}
		return (T) convertType((ClassDeclaration) type);
	}

	private void addType(TypeDeclaration typeDecl) {
		if (typeDecl == CoreClasses.arrayType()) {
			addArray(typeDecl);
		} else if (typeDecl instanceof ClassDeclaration) {
			addClass((ClassDeclaration) typeDecl);
		}
	}

	private void addClass(ClassDeclaration classDecl) {
		String mangledNodeName = classDecl.getMangledIdentifier().getSymbol();
		LLVMStructType llvmClassType = struct(classDecl.getMangledIdentifier().getSymbol());
		List<LLVMType> llvmClassTypeDeclarations = new ArrayList<>();

		LLVMStructType llvmVMTType = struct(mangledNodeName + "_vmt_type");
		List<LLVMType> llvmVMTTypeDeclarations = new ArrayList<>();
		llvmClassTypeDeclarations.add(pointer(llvmVMTType));

		LLVMIdentifier<LLVMType> llvmVMTDataIdentifier =
		        llvmIdentifierFactory.newGlobal(mangledNodeName + "_vmt_data", (LLVMType) llvmVMTType);
		List<LLVMIdentifier<LLVMType>> llvmVMTDataInitializer = new ArrayList<>();

		List<ClassDeclaration> recursiveSuperClassDeclarations = classDecl.getSuperClassDeclarationsRecursive();
		LLVMArrayType llvmCTDataType = array(pointer(int8()), recursiveSuperClassDeclarations.size() + 1);
		LLVMIdentifier<LLVMType> llvmCTDataIdentifier =
		        llvmIdentifierFactory.newGlobal(mangledNodeName + "_ct_data", (LLVMType) llvmCTDataType);
		List<LLVMIdentifier<LLVMType>> llvmCTDataInitializer = new ArrayList<>();

		llvmVMTTypeDeclarations.add(pointer(llvmCTDataType));
		llvmVMTDataInitializer.add((LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) llvmIdentifierFactory.pointerTo(llvmCTDataIdentifier));

		for (ClassDeclaration classDeclaration : recursiveSuperClassDeclarations) {
			// Ensure that addType() was called for this classDeclaration so that a VMT/CT was generated.
			mapToLLVMType(classDeclaration);
			LLVMIdentifier<LLVMType> vmtDataIdent =
			        llvmIdentifierFactory.newGlobal(
			                classDeclaration.getMangledIdentifier().getSymbol() + "_vmt_data",
			                (LLVMType) pointer(struct(classDeclaration.getMangledIdentifier().getSymbol() + "_vmt_type")));
			llvmCTDataInitializer.add(llvmIdentifierFactory.bitcast(vmtDataIdent, pointer(int8())));
		}
		llvmCTDataInitializer.add((LLVMIdentifier<LLVMType>) (LLVMIdentifier<?>) llvmIdentifierFactory.constantNull(pointer(int8())));

		if (classDecl == CoreClasses.intType()) {
			llvmClassTypeDeclarations.add(LLVMTypeFactory.int64());
		} else if (classDecl == CoreClasses.boolType()) {
			llvmClassTypeDeclarations.add(LLVMTypeFactory.int1());
		} else if (classDecl == CoreClasses.floatType()) {
			llvmClassTypeDeclarations.add(LLVMTypeFactory.double64());
		} else if (classDecl == CoreClasses.charType()) {
			llvmClassTypeDeclarations.add(LLVMTypeFactory.int8());
		} else if (classDecl == CoreClasses.stringType()) {
			llvmClassTypeDeclarations.add(LLVMTypeFactory.pointer(LLVMTypeFactory.int8()));
		}

		for (ClassDeclaration classDeclaration : recursiveSuperClassDeclarations) {
			for (Declaration decl : classDeclaration.getBlock().getDeclarations()) {
				if (decl instanceof VariableDeclaration) {
					llvmClassTypeDeclarations.add(mapToLLVMType(((VariableDeclaration) decl).getType()));
				}
			}
		}
		for (ProcedureDeclaration procedure : classDecl.getVirtualMethodTable()) {
			if (!procedure.isInitializer()) {
				LLVMType signature = mapToLLVMType(procedure);
				llvmVMTTypeDeclarations.add(signature);
				llvmVMTDataInitializer.add(llvmIdentifierFactory.newGlobal(
				        procedure.getMangledIdentifier().getSymbol(),
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

	private void addArray(TypeDeclaration typeDecl) {
		// Temporary until object or generic
		LLVMType llvmType = mapToLLVMType((TypeDeclaration) CoreClasses.intType());
		List<LLVMType> list = Arrays.asList(int64(), array(llvmType, 0));
		LLVMStructType type = struct(typeDecl.getMangledIdentifier().getSymbol());
		constantContext.type(type, list);
	}

	public LLVMPointer<LLVMStructType> mapToLLVMType(ClassDeclaration type) {
		return (LLVMPointer<LLVMStructType>) mapToLLVMType((TypeDeclaration) type);
	}

	public LLVMPointer<LLVMFunctionType> mapToLLVMType(ProcedureDeclaration type) {
		return (LLVMPointer<LLVMFunctionType>) mapToLLVMType((TypeDeclaration) type);
	}

	public <T extends LLVMType> T mapToLLVMType(TypeDeclaration type) {
		T llvmType = (T) typeMap.get(type);
		if (llvmType == null) {
			llvmType = convertType(type);
			typeMap.put(type, llvmType);
			addType(type);
		}
		return llvmType;
	}
}
