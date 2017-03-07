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
import de.uni.bremen.monty.moco.ast.Identifier;
import de.uni.bremen.monty.moco.ast.Scope;
import de.uni.bremen.monty.moco.ast.declaration.ClassDeclaration;

/**
 * Is a Class or a TypeVariable or something between.
 * Not FunctionDecl or Mudule
 */
public abstract class Type {

    public abstract boolean isAssignableFrom(Type other);

    public boolean isVoid() {
        return this.matchesDeclaration(CoreClasses.voidType());
    }

    protected abstract boolean matchesDeclaration(ClassDeclaration declaration);

    public abstract boolean matchesTypeExactly(Type type);

    public abstract Identifier getIdentifier();

    /** Calculates the distance of this and other in the inheritance graph. Only calculates positive distances, i.e. if
     * this is a parameter and other is an argument, other must be equal to or a subtype of this. otherwise the result
     * will be Integer.MAX_VALUE.
     *
     * @param other
     * @return a distance 0 <= dist <= Integer.MAX_VALUE */
    public int getTypeDist(Type other) {
        return getTypeDist(other, 0);
    }

    /** a helper method for "public int getTypeDist(TypeDeclaration other)"
     *
     * @param other
     * @param dist
     * @return */
    protected int getTypeDist(Type other, int dist) {
        if (dist == Integer.MAX_VALUE) {
            return dist;
        }
        if (matchesTypeExactly(other)) {
            return dist;
        } else if ((other instanceof PartialAppliedTypeInfo)
                && (!((PartialAppliedTypeInfo) other).getSuperClassDeclarations().isEmpty())) {
            int parentDist = Integer.MAX_VALUE;
            for (Type superClass : ((PartialAppliedTypeInfo) other).getSuperClassDeclarations()) {
                int superDist = getTypeDist(superClass, dist + 1);
                if (superDist < parentDist) {
                    parentDist = superDist;
                }
            }
            return parentDist;
        } else if (other instanceof TypeVariable) {
            return getTypeDist(((TypeVariable) other).getUpperTypeBound());
        }
        return Integer.MAX_VALUE;
    }

    @Override
    public boolean equals(Object o) {
        throw new UnsupportedOperationException();
    }

    @Override
    public int hashCode() {
        throw new UnsupportedOperationException();
    }

    public abstract Scope getScope();

    public abstract Type extend(TypeContext context);
//    abstract ConcreteTypeInfo turnConcrete(TypeContext context);

    public abstract boolean isTuple(int n);

    public abstract boolean isTuple();

    public abstract boolean isFunction();
}
