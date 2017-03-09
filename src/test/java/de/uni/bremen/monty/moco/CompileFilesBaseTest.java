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
package de.uni.bremen.monty.moco;

import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.StringUtils;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.PrintStream;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.Collection;

import static de.uni.bremen.monty.moco.IntegrationTestUtils.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.isEmptyString;

public class CompileFilesBaseTest {
	protected File file;
	protected String fileName;

	public CompileFilesBaseTest(File file, String fileName) {
		this.file = file;
		this.fileName = fileName;
	}

	protected static Collection<Object[]> buildParameterObject(Collection<File> programFiles) {
		Object[][] a = new Object[programFiles.size()][1];
		int i = 0;
		for (File programFile : programFiles) {
			a[i++] = new Object[] { programFile, programFile.getName() };
		}
		return Arrays.asList(a);
	}

	protected static Collection<File> getFiles4Filter(String folderPath, String[] filter) throws URISyntaxException {
		Class aClass = CompileTestProgramsTest.class;
		ClassLoader classLoader = aClass.getClassLoader();
		File testprogramFolder = new File(classLoader.getResource(folderPath).toURI());
		return FileUtils.listFiles(testprogramFolder, filter, true);
	}

	protected static Collection<File> getAllMontyFiles(String folderPath) throws URISyntaxException {
		return getFiles4Filter(folderPath, new String[] { "monty" });
	}

	protected static File getLastModifiedMontyFile(String folderPath) throws URISyntaxException {

		final File[] montyFilesInDir =
		        getFiles4Filter(folderPath, new String[] { "monty", "ignore" }).toArray(new File[0]);
		File latestModifiedMontyProgram = montyFilesInDir[0];

		for (File file : montyFilesInDir) {
			if (file.lastModified() > latestModifiedMontyProgram.lastModified()) {
				latestModifiedMontyProgram = file;
			}
		}

		return latestModifiedMontyProgram;
	}

	protected void runTest() throws IOException, InterruptedException {
		final PrintStream bufferOut = System.out;
		final PrintStream bufferErr = System.err;
		final ByteArrayOutputStream outStream = setStdout();
		final ByteArrayOutputStream errorStream = setStdErr(file);

		if (inputFileExists(file)) {
			System.setProperty("testrun.readFromFile", changeFileExtension(file, ".input"));
		}
		Main.main(new String[] { "-e", file.getAbsolutePath() });

		if (outputFileExists(file)) {
			assertThat(getOutput(errorStream), is(isEmptyString()));
			assertThat(getOutput(outStream), is(expectedResultFromFile(file)));
		} else {
			// chop the last char to not contain /n in the string
			assertThat(StringUtils.chop(getOutput(errorStream)), is(expectedErrorFromFile(file)));
			assertThat(getOutput(outStream), is(isEmptyString()));
		}
		System.clearProperty("testrun.readFromFile");
		System.setOut(bufferOut);
		System.setErr(bufferErr);
	}

}
