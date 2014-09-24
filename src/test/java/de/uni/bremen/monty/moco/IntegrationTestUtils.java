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

import static de.uni.bremen.monty.moco.IntegrationTestUtils.outputFileExists;
import de.uni.bremen.monty.moco.util.MultiOutputStream;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

public class IntegrationTestUtils {
	protected static String getOutput(ByteArrayOutputStream outStream) {
		return new String(outStream.toByteArray());

	}

	protected static ByteArrayOutputStream setStdout() {
		ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
		System.setOut(new PrintStream(byteArrayOutputStream));
		return byteArrayOutputStream;
	}

	protected static ByteArrayOutputStream setStdErr(File file) {
		ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();

		if (outputFileExists(file)) {
			OutputStream errStream = new MultiOutputStream(byteArrayOutputStream, System.err);
			System.setErr(new PrintStream(errStream));
		} else {

			System.setErr(new PrintStream(byteArrayOutputStream));
		}

		return byteArrayOutputStream;
	}

	protected static ByteArrayOutputStream setStdErr() {
		ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
		OutputStream errStream = new MultiOutputStream(byteArrayOutputStream, System.err);
		System.setErr(new PrintStream(errStream));
		return byteArrayOutputStream;
	}

	protected static String getExpectedResultFromFile(String outputFile) throws IOException {
		byte[] encoded = Files.readAllBytes(Paths.get(outputFile));
		return new String(encoded, StandardCharsets.UTF_8);
	}

	protected static String expectedResultFromFile(File file) throws IOException {
		String outputFile = changeFileExtension(file, ".output");
		return IntegrationTestUtils.getExpectedResultFromFile(outputFile);
	}

	private static String changeFileExtension(File file, String newExtension) {
		return file.getAbsolutePath().substring(0, file.getAbsolutePath().lastIndexOf('.')) + newExtension;
	}

	protected static String expectedErrorFromFile(File file) throws IOException {
		String outputFile = changeFileExtension(file, ".error");
		return IntegrationTestUtils.getExpectedResultFromFile(outputFile);
	}

	protected static boolean outputFileExists(File file) {
		String outputFile = changeFileExtension(file, ".output");
		return new File(outputFile).exists();
	}
}
