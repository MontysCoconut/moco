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

import de.uni.bremen.monty.moco.ast.CoreClasses;
import de.uni.bremen.monty.moco.ast.declaration.TypeParameterDeclaration;
import de.uni.bremen.monty.moco.ast.declaration.ClassDeclaration;

import java.util.*;
import java.util.stream.Collectors;


public class TypeContextImpl extends TypeContext {

    private ClassDeclaration declaration;
    private final List<? extends Type> appliedTypes;
    private final TypeContext parent;

    protected TypeContextImpl(ClassDeclaration declaration, List<? extends Type> appliedTypes) {
        this.declaration = declaration;
        this.appliedTypes = appliedTypes;
        parent = EMPTY;
    }

    protected TypeContextImpl(ClassDeclaration declaration, List<? extends Type> appliedTypes, TypeContext parent) {
        this.declaration = declaration;
        this.appliedTypes = appliedTypes;
        this.parent = parent;
    }

    private List<TypeContext> superContext() {
        return declaration.getSuperClassDeclarations().stream().map( s -> s.context).collect(Collectors.toList());
    }

    @Override
    public TypeContext extend(TypeContext context) {
        return new TypeContextImpl(declaration, appliedTypes, context);
    }

    @Override
    public Type resolve(Type type) {
        if(type instanceof TypeVariable) {
            return resolve(((TypeVariable) type).declaration);
        } else {
            return type.extend(this);
        }
    }

    @Override
    public Type resolve(TypeParameterDeclaration typeVariable) {
        Type type = tryToResolve(typeVariable);
        if(type == null){
            return new TypeVariable(typeVariable);
        }
        return type;
    }

    @Override
    protected Type tryToResolve(TypeParameterDeclaration typeVariable) {
        Type type = resolveOwnTypes(typeVariable);
        if (type != null) {
            return type;
        }
        type = resolveClassParentTypes(typeVariable);
        if (type != null) return type;

        type = resolveParentTypes(typeVariable);
        if (type != null) return type;

        return null;
    }

    private Type resolveFunctionTypes(TypeContext typeContext, TypeParameterDeclaration typeVariable) {
        if(typeContext instanceof TypeContextImpl && ((TypeContextImpl) typeContext).declaration.equals(CoreClasses.functionType())){
            Type tuple = ((TypeContextImpl) typeContext).appliedTypes.get(0);
            if (tuple instanceof PartialAppliedTypeInfo){
                PartialAppliedTypeInfo concreteTuple = (ConcreteType) tuple;
                return concreteTuple.context.tryToResolve(typeVariable);
            }
        }
        return null;
    }

    private Type resolveParentTypes(TypeParameterDeclaration typeVariable) {
        return parent.tryToResolve(typeVariable);
    }

    private Type resolveClassParentTypes(TypeParameterDeclaration typeVariable) {
        Type type;
        for (TypeContext typeContext : superContext()) {
            type = typeContext.tryToResolve(typeVariable);
            if(type != null) {
                return type;
            }
            type = resolveFunctionTypes(typeContext, typeVariable);
            if (type != null) {
                return type;
            }
        }
        return null;
    }

    private Type resolveOwnTypes(TypeParameterDeclaration typeVariable) {
        Type type = mappings().get(typeVariable);
        if(type instanceof TypeVariable) {
            Type otherType = tryToResolve(((TypeVariable) type).declaration);
            return otherType != null ? otherType : type;
        }
        return type;
    }

    private HashMap<TypeParameterDeclaration, Type> mappings() {
        HashMap<TypeParameterDeclaration, Type> map = new HashMap<>();
        List<TypeParameterDeclaration> typeParameterDeclarations = declaration.getTypeParameterDeclarations();
        for (int i = 0; i < typeParameterDeclarations.size(); i++) {
            TypeParameterDeclaration key = typeParameterDeclarations.get(i);
            Type value = appliedTypes.get(i);
            if (!(value instanceof TypeVariable && ((TypeVariable) value).declaration.equals(key))) {
                map.put(key, value);
            }
        }
        return map;
    }
}
