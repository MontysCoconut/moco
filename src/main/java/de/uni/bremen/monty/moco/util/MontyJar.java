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

import org.apache.commons.lang3.StringUtils;

import java.io.IOException;
import java.io.InputStream;
import java.net.JarURLConnection;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.jar.JarEntry;
import java.util.jar.JarFile;

public class MontyJar implements MontyResource {

	private final JarFile jarFile;
	private final JarEntry jarEntry;
	private final static Comparator<MontyResource> comparator = createComparator();

	public MontyJar(URL url) throws IOException {
		JarURLConnection urlConnection = (JarURLConnection) url.openConnection();
		jarFile = urlConnection.getJarFile();
		jarEntry = urlConnection.getJarEntry();
	}

	public MontyJar(JarFile jarFile, JarEntry jarEntry) {
		this.jarFile = jarFile;
		this.jarEntry = jarEntry;
	}

	@Override
	public MontyResource[] listSubPackages() {
		return getSubResources(true);
	}

	@Override
	public InputStream toInputStream() throws IOException {
		return jarFile.getInputStream(jarEntry);
	}

	@Override
	public String getName() {
		return jarEntry.getName();
	}

	@Override
	public MontyResource[] listSubModules() throws IOException {
		return getSubResources(false);
	}

	private MontyResource[] getSubResources(boolean showFiles) {
		ArrayList<MontyResource> list = new ArrayList<>();

		Enumeration<JarEntry> entries = jarFile.entries();
		while (entries.hasMoreElements()) {
			JarEntry x = entries.nextElement();

			if (x.getName().startsWith(jarEntry.getName())) {
				String substring = x.getName().substring(jarEntry.getName().length());

				if (!substring.equals("")) {
					if (showFiles == substring.contains("/")) {
						if (!showFiles || StringUtils.countMatches(substring, "/") == 1
						        && substring.indexOf("/") == substring.length() - 1) {
							list.add(new MontyJar(jarFile, x));
						}
					}
				}

			}
		}
		MontyResource[] montyResources = list.toArray(new MontyResource[list.size()]);
		Arrays.sort(montyResources, comparator);
		return montyResources;
	}

	private static Comparator<MontyResource> createComparator() {
		return new Comparator<MontyResource>() {
			@Override
			public int compare(MontyResource o1, MontyResource o2) {
				return o1.getName().compareTo(o2.getName());
			}
		};
	}
}
