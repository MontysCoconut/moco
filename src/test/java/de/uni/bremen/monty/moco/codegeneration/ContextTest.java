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
package de.uni.bremen.monty.moco.codegeneration;

import de.uni.bremen.monty.moco.codegeneration.context.CommentAppender;
import de.uni.bremen.monty.moco.codegeneration.context.Context;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.Mock;
import org.mockito.runners.MockitoJUnitRunner;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.startsWith;
import static org.mockito.AdditionalAnswers.returnsFirstArg;
import static org.mockito.Matchers.anyString;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class ContextTest {

	@Mock
	CommentAppender commentAppender;

	@Before
	public void setUp() throws Exception {
		when(commentAppender.addComment(anyString())).then(returnsFirstArg());
	}

	@Test
	public void shouldContainSimpleString() throws Exception {
		String testString = "print xxx";

		Context context = new Context(commentAppender);
		context.append(testString);

		assertThat(context.getData(), startsWith(testString));
	}

	@Test
	public void shouldContainAnotherContext() throws Exception {
		String testString = "call xxx";

		Context context1 = new Context(commentAppender);
		Context context2 = new Context(commentAppender);
		context1.append(context2);
		context2.append(testString);

		assertThat(context1.getData(), startsWith(testString));
	}

	@Test
	public void shouldContainMultpleChildren() throws Exception {

		Context context1 = new Context(commentAppender);
		Context context2 = new Context(commentAppender);
		Context context3 = new Context(commentAppender);
		Context context4 = new Context(commentAppender);
		Context context5 = new Context(commentAppender);

		context1.append(context2);
		context1.append(context3);
		context1.append("World");
		context1.append(context5);
		context3.append(context4);

		context2.append("!");
		context4.append("Hello ");
		context3.append("pretty ");

		context5.append("!");

		assertThat(context1.getData(), is("!\nHello \npretty \nWorld\n!\n"));
	}

}
