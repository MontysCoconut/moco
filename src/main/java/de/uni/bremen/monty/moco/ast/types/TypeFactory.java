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
package de.uni.bremen.monty.moco.ast.types;

import de.uni.bremen.monty.moco.ast.declaration.*;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class TypeFactory {

	public static PartialAppliedTypeInfo from(ClassDeclaration declaration, TypeContext context){
        List<Type> types = new ArrayList<>();
        boolean concrete = true;
        for (TypeParameterDeclaration typeParameterDeclaration : declaration.getTypeParameterDeclarations()) {
            Type type = context.resolve(typeParameterDeclaration);
            types.add(type);
            if(!(type instanceof ConcreteType)) {
                concrete = false;
            }
        }
        if(concrete){
            return new ConcreteType(declaration, (List) types, context);
        }
        return new PartialAppliedTypeInfo(declaration, types, context);
    }

	public static VariableType from(VariableDeclaration declaration, TypeContext context) {
		Type type = declaration.getType();
		Type returnType = context.resolve(type);
		if (returnType instanceof ConcreteType) {
			return new ConcreteVariableType(declaration, (ConcreteType) returnType, context);
		}
		return new VariableType(declaration, returnType);
	}

	public static FunctionType from(FunctionDeclaration declaration, TypeContext context){
        Type type = declaration.getReturnType();
        Type returnType = context.resolve(type);

        List<? extends VariableType> parameter = declaration.getParameters().stream().map(p -> from(p, context)).collect(Collectors.toList());
        List<? extends VariableType> closureVariables = declaration.getClosureVariables().stream().map(d -> from(d, context)).collect(Collectors.toList());
        PartialAppliedTypeInfo wrapperClass;
        VariableType wrapperObject;
        PartialAppliedTypeInfo definingClass;
        boolean isUnbound = declaration.getDeclarationType() == FunctionDeclaration.DeclarationType.UNBOUND;
        if(isUnbound){
            definingClass = null;
        } else {
            definingClass = from(declaration.getDefiningClass(), context);
        }
        if (declaration.getWrapperClass() != null) {
            wrapperClass = from(declaration.getWrapperClass(), context);
            wrapperObject = TypeFactory.from(declaration.getWrapperFunctionObjectDeclaration(), context);
        } else {
            wrapperClass = null;
            wrapperObject = null;
        }

        if((!declaration.isReturnTypeToBeInferred() || declaration.getReturnType()!=null) && returnType instanceof ConcreteType && parameter.stream().allMatch(t -> t instanceof ConcreteVariableType) && (isUnbound || definingClass instanceof ConcreteType)) {
            if(declaration instanceof GeneratorFunctionDeclaration){
                return new ConcreteGeneratorFunctionType((GeneratorFunctionDeclaration) declaration, context, (List<ConcreteVariableType>) parameter, (ConcreteType)returnType,(ConcreteType) definingClass, (ConcreteType) wrapperClass, (ConcreteVariableType) wrapperObject, (List<ConcreteVariableType>) closureVariables);
            }
            return new ConcreteFunctionTypeDecl(declaration, context, (List<ConcreteVariableType>) parameter, (ConcreteType)returnType, (ConcreteType) definingClass, (ConcreteType) wrapperClass, (ConcreteVariableType) wrapperObject, (List<ConcreteVariableType>) closureVariables);
        }
        if(declaration instanceof GeneratorFunctionDeclaration){
            return new GeneratorFunctionTypeImpl((GeneratorFunctionDeclaration) declaration, context, parameter, returnType, definingClass, wrapperClass, wrapperObject, closureVariables);
        }
        return new FunctionTypeDecl(declaration, context, parameter, returnType, definingClass, wrapperClass, wrapperObject, closureVariables);
    }

	public static ConcreteType makeConcrete(ClassDeclaration declaration, TypeContext context) {
		return (ConcreteType) from(declaration, context);
	}

	public static ConcreteFunctionType makeConcrete(FunctionDeclaration declaration, TypeContext context) {
		FunctionType from = from(declaration, context);
		return (ConcreteFunctionType) from;
	}

	public static ConcreteFunctionType makeConcrete(FunctionType declaration, TypeContext context) {
		FunctionType extend = declaration.extend(context);
		return (ConcreteFunctionType) extend;
	}

	public static ConcreteVariableType makeConcrete(VariableDeclaration declaration, TypeContext context) {
		return (ConcreteVariableType) from(declaration, context);
	}

	public static FunctionType createFunction(VariableDeclaration declaration) {
		return new FunctionTypeVariable(declaration);
	}
}
