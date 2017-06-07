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
package de.uni.bremen.monty.moco.util;


import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

/**
 * iterates over the given Set. If Elements are added during the iteration they will be processed too.
 * The default iterator of a Set would throw a ConcurrentModificationException in that case.
 *
 * This is not Thread safe!
 */
public class AllIterator<T> implements Iterator<T> {

    private final Set<T> visited = new HashSet<>();
    private final Set<T> original;
    private Iterator<T> iterator;

    public AllIterator(Set<T> allTypes) {
        original = allTypes;
        iterator = copy(original).iterator();
    }

    @Override
    public boolean hasNext() {
        boolean hasNext = iterator.hasNext();
        if (!hasNext) {
            Set<T> copy = copy(original);
            copy.removeAll(visited);
            if (copy.isEmpty()) {
                return false;
            } else {
                iterator = copy.iterator();
                return true;
            }
        }
        return true;
    }

        @Override
        public T next() {
        T ele = iterator.next();
        visited.add(ele);
        return ele;
    }

    private <T> Set<T> copy(Set<T> from) {
        HashSet<T> newSet = new HashSet<>(from.size());
        newSet.addAll(from);
        return newSet;
    }
}
