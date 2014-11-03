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

import org.apache.commons.io.filefilter.DirectoryFileFilter;
import org.apache.commons.io.filefilter.SuffixFileFilter;

import java.io.*;
import java.net.URI;
import java.util.Arrays;

public class MontyFile extends File implements MontyResource {

	public MontyFile(URI uri) {
		super(uri);
	}

	public MontyFile(String pathname) {
		super(pathname);
	}

	@Override
	public MontyResource[] listSubPackages() {
		File[] files = listFiles((FileFilter) DirectoryFileFilter.DIRECTORY);
		MontyResource[] montyResources = convertAllFiles(files);
		Arrays.sort(montyResources);
		return montyResources;
	}

	private MontyResource[] convertAllFiles(File[] files) {
		MontyResource[] resources = new MontyResource[files.length];
		for (int i = 0; i < files.length; i++) {
			resources[i] = new MontyFile(files[i].toURI());
		}
		return resources;
	}

	@Override
	public InputStream toInputStream() throws FileNotFoundException {
		return new FileInputStream(this);
	}

	@Override
	public MontyResource[] listSubModules() {
		MontyResource[] montyResources = convertAllFiles(listFiles((FilenameFilter) new SuffixFileFilter(".monty")));
		Arrays.sort(montyResources);
		return montyResources;
	}

}
