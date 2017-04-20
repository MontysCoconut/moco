
Moco [![Build Status](https://travis-ci.org/MontysCoconut/moco.svg?branch=master)](https://travis-ci.org/MontysCoconut/moco)
====

`moco` is a [Monty](http://montyscoconut.github.io/) to
[LLVM](http://llvm.org/) compiler built using
[Java](https://www.java.com/en/), [ANTLR4](http://www.antlr.org/) and
[LLVM](http://llvm.org/).

Please read the [language
specification](http://montyscoconut.github.io/downloads.html) for
details on the `Monty` programming language and refer to the [feature
overview](FEATURES.md) for a list of implemented features.

Installing Dependencies
=======================

Linux (Ubuntu)
--------------

Install [LLVM](http://llvm.org/) and a JRE 8.

    sudo apt-get install llvm
    sudo apt-get install default-jre

If you want to build `moco` yourself you'll need to install the following
dependencies and a JDK 8.

    sudo apt-get install git
    sudo apt-get install maven
    sudo apt-get install default-jdk

You can also install Graphviz to generate class diagrams. This is optional.

    sudo apt-get install graphviz

Mac OS X
--------

> **Note:** Please make sure you have [homebrew](http://brew.sh/) and a JRE 8
installed.

Install [LLVM](http://llvm.org/) using homebrew:

    brew install llvm

If you want to build `moco` yourself you'll need to install the following
dependencies and a JDK 8.

    brew install git
    brew install maven

Please make sure to set the `JAVA_HOME` environment variable for `maven`.

    export JAVA_HOME=$(/usr/libexec/java_home)

You can also install Graphviz to generate class diagrams. This is optional.

    brew install graphviz

Windows
-------

Currently Windows is not supported.

Building
========

> **Note:** Instead of building your own version you can always get a
precompiled .jar file at the
[releases](https://github.com/MontysCoconut/moco/releases) page.

For building your own version based on the most recent commit clone the git
repository:

    git clone https://github.com/MontysCoconut/moco.git

After that you can change into the directory and build the executable
.jar file. Now you can find the executable jar in the `target/` directory.

    cd moco
    mvn package

If you want to generate the class documentation make sure to have
[graphviz](http://www.graphviz.org/) installed and run the following command.
After that you can find the generated HTML-files in the `target/site`
directory.

    mvn site

For some background information and a guide to submit merge requests please
read [hacking](HACKING.md).

Usage
=====

To use `moco` you'll either need to compile your own version or fetch a
precompiled .jar file at the
[releases](https://github.com/MontysCoconut/moco/releases) page.

You can just pass `moco` a Monty-file and it will be compiled and executed.

    ➤ cat hello.monty
    print("Hello World!")
    ➤ java -jar moco-0.6.jar hello.monty
    Hello World!

Please see the help text for information about the command-line switches.
    usage: moco [--help] [-S] [-c] [-e] [-p] [-d] [-o <file>] [file]

    The Monty compiler.

    positional arguments:
    file                   Monty file to run.

    optional arguments:
    --help                 Print this help and exit.
    -S, --emit-assembly    Emit the LLVM assembly and stop.
    -c, --compile-only     Only compile the executable without running it.
    -e, --stop-on-first-error
                            Stop the compilation on the first encountered error.
    -p, --print-ast        Print the AST.
    -d, --debug-parsetree  Debug the parsetree without running anything.
    -o <file>              Write output to <file>.

    Without -S or -c the program is compiled and directly executed.
