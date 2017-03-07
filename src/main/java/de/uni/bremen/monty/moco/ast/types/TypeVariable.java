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

import de.uni.bremen.monty.moco.ast.Identifier;
import de.uni.bremen.monty.moco.ast.Scope;
import de.uni.bremen.monty.moco.ast.declaration.TypeParameterDeclaration;
import de.uni.bremen.monty.moco.ast.declaration.ClassDeclaration;

public class TypeVariable extends Type {

    TypeParameterDeclaration declaration;

    public TypeVariable(TypeParameterDeclaration declaration) {
        this.declaration = declaration;
    }

    @Override
    public boolean isAssignableFrom(Type other) {
        if(other instanceof TypeVariable){
            return this.declaration.equals(((TypeVariable) other).declaration);
        }
        return declaration.getUpperTypeBound().isAssignableFrom(other);
    }

    @Override
    protected boolean matchesDeclaration(ClassDeclaration declaration) {
        return false;
    }

    @Override
    public boolean matchesTypeExactly(Type type) {
        if (type instanceof TypeVariable) {
            if (((TypeVariable) type).declaration.equals(declaration)) {
                return true;
            }
        }
        return false;
    }

    @Override
    public Identifier getIdentifier() {
        return declaration.getIdentifier();
    }

    @Override
    public Scope getScope() {
        return declaration.getUpperTypeBound().getScope();
    }

    public Type extend(TypeContext context) {
        return context.resolve(this);
    }

    @Override
    public boolean isTuple(int n) {
        return false;
    }

    @Override
    public boolean isTuple() {
        return false;
    }

    @Override
    public boolean isFunction() {
        return false;
    }

    public Type getUpperTypeBound() {
        return declaration.getUpperTypeBound();
    }

    @Override
    public int getTypeDist(Type other) {
        if(other instanceof TypeVariable){
            if(this.declaration == ((TypeVariable) other).declaration){
                return 0;
            }
            return Integer.MAX_VALUE;
        }
        throw new RuntimeException();
    }

    @Override
    protected int getTypeDist(Type other, int dist) {
        throw new RuntimeException();
    }

    @Override
    public String toString() {
        return declaration.getIdentifier().getSymbol();
    }
}
