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

import de.uni.bremen.monty.moco.antlr.MontyParser;
import de.uni.bremen.monty.moco.ast.AntlrAdapter;
import de.uni.bremen.monty.moco.ast.Package;
import de.uni.bremen.monty.moco.ast.PackageBuilder;
import de.uni.bremen.monty.moco.util.*;
import de.uni.bremen.monty.moco.visitor.*;

import org.apache.commons.io.IOUtils;
import org.apache.commons.io.FilenameUtils;

import net.sourceforge.argparse4j.ArgumentParsers;
import net.sourceforge.argparse4j.impl.Arguments;
import net.sourceforge.argparse4j.inf.ArgumentParserException;
import net.sourceforge.argparse4j.inf.ArgumentParser;
import net.sourceforge.argparse4j.inf.Namespace;

import java.io.*;

public class Main {

	private static Namespace parseArgs(String[] args) {
		ArgumentParser parser =
		        ArgumentParsers.newArgumentParser("moco", false).description("The Monty to LLVM compiler.");

		parser.addArgument("--help").action(Arguments.help()).help("Print this help and exit.");
		parser.addArgument("-ll", "--generate-only").action(Arguments.storeTrue()).dest("generateOnly").help(
		        "Only generate the LLVM output without running it.");
		parser.addArgument("-e", "--stop-on-first-error").action(Arguments.storeTrue()).dest("stopOnFirstError").help(
		        "Stop the compilation on the first encountered error.");
		parser.addArgument("-p", "--print-ast").action(Arguments.storeTrue()).dest("printAST").help("Print the AST.");
		parser.addArgument("-d", "--debug-parsetree").action(Arguments.storeTrue()).dest("debugParseTree").help(
		        "Debug the parsetree without running anything.");
		parser.addArgument("-o").metavar("<file>").dest("outputFile").help("Write output to <file>.");
		parser.addArgument("file").nargs("?").help("Monty file to run.");

		Namespace ns = null;
		try {
			ns = parser.parseArgs(args);
		} catch (ArgumentParserException ape) {
			parser.handleError(ape);
		}
		return ns;
	}

	private static void debugParseTree(String inputFile) throws IOException {
		AntlrAdapter antlrAdapter = new AntlrAdapter();
		MontyParser parser = null;

		if (inputFile == null) {
			parser = antlrAdapter.createParser(System.in);
		} else {
			File file = new File(inputFile);
			parser = antlrAdapter.createParser(new FileInputStream(file));
		}

		ParseTreePrinter parseTreePrinter = new ParseTreePrinter(parser);
		parser.addParseListener(parseTreePrinter);
		parser.compilationUnit();
		System.out.print(parseTreePrinter.toString());
	}

	private static Package buildPackage(String inputFile) {
		PackageBuilder packageBuilder = new PackageBuilder();
		if (inputFile == null) {
			return packageBuilder.buildPackage(System.in);
		} else {
			return packageBuilder.buildPackage(inputFile);
		}
	}

	private static boolean visitVisitors(Package ast, boolean stopOnFirstError, StringBuffer output) {

		CodeGenerationVisitor cgv = new CodeGenerationVisitor();
		BaseVisitor[] visitors =
		        new BaseVisitor[] { new SetParentVisitor(), new DeclarationVisitor(), new ResolveVisitor(),
		                new TypeCheckVisitor(), new ControlFlowVisitor(), new NameManglingVisitor(), cgv };

		boolean everyThingIsAwesome = true;

		for (BaseVisitor visitor : visitors) {
			visitor.setStopOnFirstError(stopOnFirstError);

			try {
				visitor.visitDoubleDispatched(ast);
			} catch (RuntimeException exception) {
				visitor.logError(exception);
				everyThingIsAwesome = false;
				break;
			}

			if (visitor.foundError()) {
				everyThingIsAwesome = false;
				break;
			}
		}

		cgv.writeLLVMCode(output);
		return everyThingIsAwesome;
	}

	private static File createOutputFile(String name) throws IOException {
		if (name == null) {
			File llvmCodeFile = File.createTempFile("monty", ".ll", null);
			llvmCodeFile.deleteOnExit();
			return llvmCodeFile;
		}
		return new File(name);
	}

	private static void writeOutput(String llvmCode, File outputFile) throws FileNotFoundException {
		PrintStream resultStream = new PrintStream(outputFile);
		resultStream.print(llvmCode);
		resultStream.close();
	}

	private static void runCode(File llvmCode) throws IOException {
		ProcessBuilder processBuilder = new ProcessBuilder("lli", llvmCode.getAbsolutePath());
		String readFromFile = System.getProperty("testrun.readFromFile");
		if (readFromFile == null) {
			processBuilder.redirectInput(ProcessBuilder.Redirect.INHERIT);
		} else {
			processBuilder.redirectInput(new File(readFromFile));
		}
		Process process = processBuilder.start();

		System.err.print(IOUtils.toString(process.getErrorStream()));
		System.out.print(IOUtils.toString(process.getInputStream()));
	}

	private static File buildExecutable(String outputFileName, String inputFileName, boolean compileOnly,
	        String llvmCode) throws IOException, InterruptedException {
		File outputFile = null;
		if (outputFileName != null) {
			outputFile = new File(outputFileName);
		} else if (inputFileName != null) {
			outputFile = new File(FilenameUtils.removeExtension(inputFileName));
		} else if (compileOnly) {
			outputFile = File.createTempFile("output", null, null);
			outputFile.deleteOnExit();
		} else {
			outputFile = new File("output");
		}

		ProcessBuilder llcProcessBuilder = new ProcessBuilder("llc");
		Process llcProcess = llcProcessBuilder.start();
		PrintStream llcInput = new PrintStream(llcProcess.getOutputStream());
		llcInput.print(llvmCode);
		llcInput.close();

		ProcessBuilder ccProcessBuilder =
		        new ProcessBuilder("cc", "-x", "assembler", "-o", outputFile.getAbsolutePath(), "-");
		Process ccProcess = ccProcessBuilder.start();
		IOUtils.copy(llcProcess.getInputStream(), ccProcess.getOutputStream());
		ccProcess.getOutputStream().close();

		System.err.print(IOUtils.toString(llcProcess.getErrorStream()));
		System.err.print(IOUtils.toString(ccProcess.getErrorStream()));
		return outputFile;
	}

	private static void runExecutable(File executable) throws IOException, InterruptedException {
		ProcessBuilder processBuilder = new ProcessBuilder(executable.getAbsolutePath());

		String readFromFile = System.getProperty("testrun.readFromFile");
		if (readFromFile != null) {
			processBuilder.redirectInput(new File(readFromFile));
		} else {
			processBuilder.redirectInput(ProcessBuilder.Redirect.INHERIT);
		}
		Process process = processBuilder.start();
		System.err.print(IOUtils.toString(process.getErrorStream()));
		System.out.print(IOUtils.toString(process.getInputStream()));
	}

	public static void main(String[] args) throws IOException, InterruptedException {
		Namespace ns = parseArgs(args);

		if (ns == null) {
			return;
		}

		String inputFileName = ns.get("file");
		String outputFileName = ns.get("outputFile");
		boolean generateOnly = ns.get("generateOnly");
		boolean stopOnFirstError = ns.get("stopOnFirstError");
		boolean printAST = ns.get("printAST");
		boolean debugParseTree = ns.get("debugParseTree");

		if (debugParseTree) {
			debugParseTree(inputFileName);
			return;
		}

		StringWriter writer = new StringWriter();
		IOUtils.copy(Main.class.getResourceAsStream("/std_llvm_include.ll"), writer);

		Package ast = buildPackage(inputFileName);
		if (!visitVisitors(ast, stopOnFirstError, writer.getBuffer())) {
			return;
		}

		if (printAST) {
			(new PrintVisitor()).visitDoubleDispatched(ast);
		}

		File executable = buildExecutable(outputFileName, inputFileName, false, writer.toString());
		runExecutable(executable);
	}
}
