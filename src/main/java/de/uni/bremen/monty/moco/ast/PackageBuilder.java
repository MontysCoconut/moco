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

package de.uni.bremen.monty.moco.ast;

import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.util.*;

import java.io.InputStream;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Collections;

public class PackageBuilder {
	private final AntlrAdapter antlrAdapter = new AntlrAdapter();

	public Package buildPackage(InputStream inputStream) {
		return buildPackage(new MontyInputStream(inputStream));
	}

	public Package buildPackage(String inputFile) {
		return buildPackage(new MontyFile(inputFile));
	}

	private Package buildPackage(MontyResource inputFile) {
		Package basePackage = new Package(new Identifier(""));
		Package corePackage = addCoreLib(basePackage);
		Package subPackage = (inputFile.isDirectory()) ? createPackage(inputFile) : createPackageFromFile(inputFile);
		addGeneratedTupleTypes(corePackage);
		basePackage.addSubPackage(subPackage);
		return basePackage;
	}

	private Package addCoreLib(Package basePackage) {
		Package corePackage = createPackage(getCoreLibFolder());
		corePackage.setNativePackage(true);
		basePackage.addSubPackage(corePackage);

		Block block = new Block(new Position());
		ModuleDeclaration module =
		        new ModuleDeclaration(new Position(), new Identifier("CoreClasses"), block,
		                Collections.<Import> emptyList());
		block.addDeclaration(CoreClasses.voidType());
		corePackage.addModule(module);
		setCoreClasses(corePackage);
		return corePackage;
	}

	private void addGeneratedTupleTypes(Package corePackage) {
		ModuleDeclaration tupleModule = null;
		for (ModuleDeclaration tmodule : corePackage.getModules()) {
			if (tmodule.getIdentifier().getSymbol().equals("Tuple")) {
				tupleModule = tmodule;
				break;
			}
		}
		if (tupleModule == null) {
			throw new RuntimeException("Error, no Tuple module in core library!");
		}

		for (ClassDeclaration tupleDecl : antlrAdapter.getTupleDeclarationFactory().generateNecessaryTupleTypes()) {
			tupleModule.getBlock().addDeclaration(tupleDecl);
		}
	}

	private void setCoreClasses(Package corePackage) {
		for (ModuleDeclaration module : corePackage.getModulesRecursive()) {
			for (Declaration declaration : module.getBlock().getDeclarations()) {
				if (declaration instanceof ClassDeclaration) {
					ClassDeclaration classDeclaration = (ClassDeclaration) declaration;
					CoreClasses.setCoreClass(classDeclaration.getIdentifier().getSymbol(), classDeclaration);
				}
			}
		}
	}

	private MontyResource getCoreLibFolder() {
		try {
			Class<?> aClass = PackageBuilder.class;
			ClassLoader classLoader = aClass.getClassLoader();
			URL testProgramFolder = classLoader.getResource("corelib/");
			if (testProgramFolder.toString().startsWith("jar:")) {
				return new MontyJar(testProgramFolder);
			} else {
				return new MontyFile(testProgramFolder.toURI());
			}
		} catch (URISyntaxException e) {
			throw new RuntimeException(e);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	private Package createPackageFromFile(MontyResource inputFile) {
		Package mainPackage = new Package(new Identifier(""));
		addModules(new MontyResource[] { inputFile }, mainPackage);
		return mainPackage;
	}

	private Package createPackage(MontyResource inputFolder) {
		MontyResource[] montyFiles = inputFolder.listSubModules();
		return createPackage(inputFolder, montyFiles);
	}

	private Package createPackage(MontyResource inputFolder, MontyResource[] montyFiles) {
		Package mainPackage = new Package(new Identifier(inputFolder.getName()));
		addModules(montyFiles, mainPackage);
		addSubPackages(inputFolder, mainPackage);
		return mainPackage;
	}

	private void addModules(MontyResource[] montyFiles, Package aPackage) {
		for (MontyResource file : montyFiles) {
			try {
				ModuleDeclaration module = antlrAdapter.parse(file.toInputStream(), file.getName());
				aPackage.addModule(module);
			} catch (IOException e) {
				throw new RuntimeException(e);
			}
		}
	}

	private void addSubPackages(MontyResource inputFolder, Package mainPackage) {
		MontyResource[] subPackages = inputFolder.listSubPackages();
		for (MontyResource subPackage : subPackages) {
			mainPackage.addSubPackage(createPackage(subPackage));
		}
	}
}
