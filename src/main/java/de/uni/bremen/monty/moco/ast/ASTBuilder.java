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

import de.uni.bremen.monty.moco.antlr.MontyBaseVisitor;
import de.uni.bremen.monty.moco.antlr.MontyParser;
import de.uni.bremen.monty.moco.antlr.MontyParser.*;
import de.uni.bremen.monty.moco.ast.declaration.*;
import de.uni.bremen.monty.moco.ast.declaration.FunctionDeclaration.DeclarationType;
import de.uni.bremen.monty.moco.ast.expression.*;
import de.uni.bremen.monty.moco.ast.expression.literal.*;
import de.uni.bremen.monty.moco.ast.statement.*;
import de.uni.bremen.monty.moco.util.*;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.apache.commons.io.FilenameUtils;

import java.util.*;

public class ASTBuilder extends MontyBaseVisitor<ASTNode> {
	private final String fileName;
	private Stack<Block> currentBlocks;
	// functions push null, generators push their return type
	private Stack<ResolvableIdentifier> currentGeneratorReturnType;
	private VariableDeclaration.DeclarationType currentVariableContext;
	private FunctionDeclaration.DeclarationType currentFunctionContext;
	private TupleDeclarationFactory tupleDeclarationFactory;
	private PatternMatchingContext patternMatchingContext = new PatternMatchingContext();

	private static final Map<String, String> binaryOperatorMapping = new HashMap<String, String>() {
		{
			put("+", "_add_");
			put("-", "_sub_");
			put("*", "_mul_");
			put("/", "_div_");
			put("%", "_mod_");
			put("^", "_pow_");
			put("=", "_eq_");
			put("!=", "_neq_");
			put("<", "_lt_");
			put(">", "_gt_");
			put("<=", "_leq_");
			put(">=", "_geq_");
			put("in", "_contains_");
		}
	};

	private static final Map<String, String> unaryOperatorMapping = new HashMap<String, String>() {
		{
			put("-", "_neg_");
			put("not", "_not_");
		}
	};

	public ASTBuilder(String fileName, TupleDeclarationFactory tupleDeclarationFactory) {
		this.fileName = fileName;
		currentBlocks = new Stack<>();
		currentGeneratorReturnType = new Stack<>();
		currentGeneratorReturnType.push(null); // initially, we are not inside a generator
		this.tupleDeclarationFactory = tupleDeclarationFactory;
	}

	private Position position(Token idSymbol) {
		return new Position(fileName, idSymbol.getLine(), idSymbol.getCharPositionInLine());
	}

	private String getText(TerminalNode identifier) {
		return identifier.getSymbol().getText();
	}

	@Override
	public ASTNode visitModuleDeclaration(@NotNull MontyParser.ModuleDeclarationContext ctx) {
		Block block = new Block(position(ctx.getStart()));
		ModuleDeclaration module =
		        new ModuleDeclaration(position(ctx.getStart()), new Identifier(FilenameUtils.getBaseName(fileName)),
		                block, new ArrayList<Import>());
		currentBlocks.push(block);

		for (MontyParser.ImportLineContext imp : ctx.importLine()) {

			module.getImports().add(
			        new Import(position(imp.getStart()), new ResolvableIdentifier(getText(imp.Identifier()))));
		}

		for (ClassDeclarationContext classDeclarationContext : ctx.classDeclaration()) {
			ClassDeclaration classDecl = (ClassDeclaration) visit(classDeclarationContext);
			block.addDeclaration(classDecl);
		}
		addStatementsToBlock(block, ctx.statement());
		currentBlocks.pop();
		return module;
	}

	@Override
	public ASTNode visitUnpackAssignment(@NotNull MontyParser.UnpackAssignmentContext ctx) {
		Expression right = (Expression) visit(ctx.right);
		List<Expression> left = new ArrayList<>(ctx.left.unpackable().size());

		for (UnpackableContext target : ctx.left.unpackable()) {
			if (target.variableDeclaration() != null) {
				VariableDeclaration decl = (VariableDeclaration) visitVariableDeclaration(target.variableDeclaration());
				currentBlocks.peek().addDeclaration(decl);
				left.add(new VariableAccess(position(target.getStart()),
				        ResolvableIdentifier.convert(decl.getIdentifier())));
			} else {
				left.add((Expression) visitExpression(target.expression()));
			}
		}
		UnpackAssignment assignment = new UnpackAssignment(position(ctx.getStart()), left, right);
		currentBlocks.peek().addDeclaration(assignment.getTmpDecl());
		return assignment;
	}

	@Override
	public ASTNode visitAssignment(@NotNull AssignmentContext ctx) {
		Assignment assignment =
		        new Assignment(position(ctx.getStart()), (Expression) visit(ctx.left), (Expression) visit(ctx.right));
		return assignment;
	}

	@Override
	public ASTNode visitCompoundAssignment(CompoundAssignmentContext ctx) {
		Expression expr =
		        binaryExpression(
		                position(ctx.getStart()),
		                ctx.compoundSymbol().operator.getText().substring(0, 1),
		                ctx.left,
		                ctx.right);

		return new Assignment(position(ctx.getStart()), (Expression) visit(ctx.left), expr);
	}

	@Override
	public ASTNode visitVariableDeclaration(@NotNull VariableDeclarationContext ctx) {
		ResolvableIdentifier type = convertResolvableIdentifier(ctx.type());
		tupleDeclarationFactory.checkTupleType(type);
		return new VariableDeclaration(position(ctx.getStart()), new Identifier(getText(ctx.Identifier())), type,
		        currentVariableContext);
	}

	private ResolvableIdentifier convertResolvableIdentifier(TypeContext type) {
		String typeName;
		List<TypeContext> typeParameters = null;
		// if there is no class identifier, we have to handle syntactic sugar here
		if (type.ClassIdentifier() == null) {
			if (type.arrow() != null) {
				typeParameters = type.type();
				typeName = "Function";
			} else {
				// a tuple
				typeParameters = type.type();
				int n = typeParameters != null ? typeParameters.size() : 0;
				typeName = "Tuple" + n;
			}
		} else {
			typeName = type.ClassIdentifier().toString();
			if (type.typeList() != null) {
				typeParameters = type.typeList().type();
			}
		}

		// handle generic type parameters
		ArrayList<ResolvableIdentifier> genericTypes = new ArrayList<>();
		if (typeParameters != null) {
			for (TypeContext typeContext : typeParameters) {
				genericTypes.add(convertResolvableIdentifier(typeContext));
			}
		}
		return new ResolvableIdentifier(typeName, genericTypes);
	}

	@Override
	public ASTNode visitFunctionCall(FunctionCallContext ctx) {
		ArrayList<Expression> arguments = new ArrayList<>();
		ResolvableIdentifier identifier;
		if (ctx.Identifier() == null) {
			identifier = convertResolvableIdentifier(ctx.type());
		} else {
			identifier = new ResolvableIdentifier(ctx.Identifier().getText());
		}
		tupleDeclarationFactory.checkTupleType(identifier); // since constructor calls are also function calls
		FunctionCall func = new FunctionCall(position(ctx.getStart()), identifier, arguments);
		if (ctx.expressionList() != null) {
			for (ExpressionContext exprC : ctx.expressionList().expression()) {

				ASTNode expr = visit(exprC);
				if (expr instanceof Expression) {

					arguments.add((Expression) expr);
				}
			}
		}
		return new WrappedFunctionCall(position(ctx.getStart()), func);
	}

	private void buildDefaultFunctions(boolean isFunction, List<DefaultParameterContext> defaultParameter,
	        List<VariableDeclaration> allVariableDeclarations, List<VariableDeclaration> params,
	        List<Expression> defaultExpression, List<VariableDeclaration> defaultVariableDeclaration,
	        Identifier identifier, Token token, TypeContext typeContext, DeclarationType declarationTypeCopy) {

		for (int defaultParameterIdx = 0; defaultParameterIdx < defaultParameter.size(); defaultParameterIdx++) {
			Block block = new Block(position(token));
			List<Expression> l = new ArrayList<>();
			for (int variableDeclarationIdy = 0; variableDeclarationIdy < allVariableDeclarations.size(); variableDeclarationIdy++) {
				if (variableDeclarationIdy >= params.size() + defaultParameterIdx) {
					l.add(defaultExpression.get(variableDeclarationIdy - params.size()));
				} else if (variableDeclarationIdy < params.size()) {
					l.add(new VariableAccess(position(token), new ResolvableIdentifier(params.get(
					        variableDeclarationIdy).getIdentifier().getSymbol())));
				} else {
					VariableDeclaration variableDeclaration =
					        defaultVariableDeclaration.get(variableDeclarationIdy - params.size());
					l.add(new VariableAccess(position(token), new ResolvableIdentifier(
					        variableDeclaration.getIdentifier().getSymbol())));
				}
			}

			List<VariableDeclaration> subParams =
			        allVariableDeclarations.subList(0, params.size() + defaultParameterIdx);

			Expression expression =
			        new FunctionCall(position(token), new ResolvableIdentifier(identifier.getSymbol()), l);

			if (declarationTypeCopy == FunctionDeclaration.DeclarationType.METHOD) {
				expression = new MemberAccess(position(token), new SelfExpression(position(token)), expression);
			}

			ResolvableIdentifier returnTypeIdent = null;
			if (isFunction) {
				block.addStatement(new ReturnStatement(new Position(), expression));
				returnTypeIdent = convertResolvableIdentifier(typeContext);
				tupleDeclarationFactory.checkTupleType(returnTypeIdent);
			} else {
				block.addStatement((Statement) expression);
				block.addStatement(new ReturnStatement(new Position(), null));
			}

			FunctionDeclaration funDecl =
			        new FunctionDeclaration(position(token), identifier, block, subParams, declarationTypeCopy,
			                returnTypeIdent);

			currentBlocks.peek().addDeclaration(funDecl);
		}
	}

	private FunctionDeclaration buildFunctions(boolean isFunction, ParameterListContext parameterListContext,
	        Token token, TypeContext typeContext, StatementBlockContext statementBlockContext, Identifier identifier) {

		FunctionDeclaration.DeclarationType declarationTypeCopy = currentFunctionContext;
		List<VariableDeclaration> params = parameterListToVarDeclList(parameterListContext);
		List<DefaultParameterContext> defaultParameter = defaultParameterListToVarDeclList(parameterListContext);

		List<VariableDeclaration> defaultVariableDeclaration = new ArrayList<>();
		List<Expression> defaultExpression = new ArrayList<>();
		for (DefaultParameterContext context : defaultParameter) {
			defaultVariableDeclaration.add((VariableDeclaration) visit(context.variableDeclaration()));
			defaultExpression.add((Expression) visit(context.expression()));
		}

		List<VariableDeclaration> allVariableDeclarations = new ArrayList<>();
		allVariableDeclarations.addAll(params);
		allVariableDeclarations.addAll(defaultVariableDeclaration);

		buildDefaultFunctions(
		        isFunction,
		        defaultParameter,
		        allVariableDeclarations,
		        params,
		        defaultExpression,
		        defaultVariableDeclaration,
		        identifier,
		        token,
		        typeContext,
		        declarationTypeCopy);

		FunctionDeclaration funDecl;

		ResolvableIdentifier returnTypeIdent = null;
		if (isFunction) {
			returnTypeIdent = convertResolvableIdentifier(typeContext);
			tupleDeclarationFactory.checkTupleType(returnTypeIdent);
		}
		funDecl =
		        new FunctionDeclaration(position(token), identifier, (Block) visit(statementBlockContext),
		                allVariableDeclarations, declarationTypeCopy, returnTypeIdent);
		if (funDecl.isUnbound()) {
			FunctionWrapperFactory.generateWrapperClass(funDecl, tupleDeclarationFactory);
			currentBlocks.peek().addDeclaration(funDecl.getWrapperClass());
			currentBlocks.peek().addDeclaration(funDecl.getWrapperFunctionObjectDeclaration());
			currentBlocks.peek().addStatement(funDecl.getWrapperFunctionAssignment());
		}

		return funDecl;
	}

	private FunctionDeclaration buildAbstractMethod(boolean functionDeclaration,
	        ParameterListContext parameterListContext, Token token, TypeContext typeContext, Identifier identifier) {

		List<VariableDeclaration> params = parameterListToVarDeclList(parameterListContext);

		ResolvableIdentifier typeIdent = null;
		if (typeContext != null) {
			typeIdent = convertResolvableIdentifier(typeContext);
			tupleDeclarationFactory.checkTupleType(typeIdent);
		}

		FunctionDeclaration procDecl =
		        new FunctionDeclaration(position(token), identifier, new Block(position(token)), params,
		                currentFunctionContext, typeIdent, true);
		return procDecl;
	}

	@Override
	public ASTNode visitFunctionDeclaration(FunctionDeclarationContext ctx) {
		currentGeneratorReturnType.push(null);
		FunctionDeclaration proc =
		        buildFunctions(
		                ctx.type() != null,
		                ctx.parameterList(),
		                ctx.getStart(),
		                ctx.type(),
		                ctx.statementBlock(),
		                new Identifier(getText(ctx.Identifier())));
		// if the function does not have any return type, we have to add a return statement
		if (ctx.type() == null) {
			List<Statement> list = proc.getBody().getStatements();
			if ((list.isEmpty()) || !(list.get(list.size() - 1) instanceof ReturnStatement)) {
				list.add(new ReturnStatement(new Position(), null));
			}
		}
		currentGeneratorReturnType.pop();
		return proc;
	}

	@Override
	public ASTNode visitGeneratorDeclaration(GeneratorDeclarationContext ctx) {
		Position pos = position(ctx.getStart());
		ResolvableIdentifier returnType = convertResolvableIdentifier(ctx.type());
		ResolvableIdentifier className = new ResolvableIdentifier(getText(ctx.ClassIdentifier()));
		tupleDeclarationFactory.checkTupleType(returnType);
		currentGeneratorReturnType.push(returnType);

		List<VariableDeclaration> params = new ArrayList<>();
		List<VariableDeclaration> defaultParams = new ArrayList<>();
		List<Expression> defaultValues = new ArrayList<>();
		parameterListToVarDeclListIncludingDefaults(ctx.parameterList(), params, defaultParams, defaultValues);

		Block funBody = (Block) visit(ctx.statementBlock());
		currentGeneratorReturnType.pop();

		return createGenerator(pos, className, returnType, params, defaultParams, defaultValues, funBody);
	}

	private ASTNode createGenerator(Position pos, ResolvableIdentifier className, ResolvableIdentifier returnType,
	        List<VariableDeclaration> params, List<VariableDeclaration> defaultParams, List<Expression> defaultValues,
	        Block body) {
		tupleDeclarationFactory.checkTupleType(returnType);
		currentGeneratorReturnType.push(returnType);

		List<VariableDeclaration> allParams = new ArrayList<>();
		allParams.addAll(params);
		allParams.addAll(defaultParams);

		ClassDeclaration iterator =
		        GeneratorClassFactory.generateGeneratorIteratorClass(pos, allParams, body, returnType);
		currentBlocks.peek().addDeclaration(iterator);

		ClassDeclaration generator =
		        GeneratorClassFactory.generateGeneratorClass(
		                pos,
		                className,
		                ResolvableIdentifier.convert(iterator.getIdentifier()),
		                params,
		                defaultParams,
		                defaultValues,
		                returnType);
		currentGeneratorReturnType.pop();
		return generator;
	}

	protected List<VariableDeclaration> createParameterListWithoutDefaults(ParameterListWithoutDefaultsContext ctx) {
		ArrayList<VariableDeclaration> parameterList = new ArrayList<>();
		ParameterListWithoutDefaultsContext plist = ctx;
		if (plist != null) {
			currentVariableContext = VariableDeclaration.DeclarationType.PARAMETER;
			for (VariableDeclarationContext var : plist.variableDeclaration()) {
				parameterList.add((VariableDeclaration) visit(var));
			}
		}
		return parameterList;
	}

	@Override
	public ASTNode visitFunctionExpression(FunctionExpressionContext ctx) {
		List<VariableDeclaration> parameterList =
		        createParameterListWithoutDefaults(ctx.parameterListWithoutDefaults());

		Block body = new Block(position(ctx.expression().getStart()));
		body.addStatement(new ReturnStatement(body.getPosition(), (Expression) visit(ctx.expression())));
		FunctionDeclaration funDecl =
		        new FunctionDeclaration(position(ctx.getStart()), TmpIdentifierFactory.getUniqueIdentifier(), body,
		                parameterList);

		FunctionWrapperFactory.generateWrapperClass(funDecl, tupleDeclarationFactory);
		currentBlocks.peek().addDeclaration(funDecl);
		currentBlocks.peek().addDeclaration(funDecl.getWrapperClass());
		Declaration wrapperObject = funDecl.getWrapperFunctionObjectDeclaration();
		currentBlocks.peek().addDeclaration(wrapperObject);
		currentBlocks.peek().addStatement(funDecl.getWrapperFunctionAssignment());

		return new VariableAccess(funDecl.getPosition(), ResolvableIdentifier.convert(wrapperObject.getIdentifier()));
	}

	@Override
	public ASTNode visitClassDeclaration(ClassDeclarationContext ctx) {
		// if there is an 'abstract' keyword, the class is abstract
		return createClass(
		        position(ctx.getStart()),
		        ctx.type(),
		        ctx.typeList(),
		        ctx.getTokens(MontyParser.AbstractKeyword).size() > 0,
		        ctx.memberDeclaration());
	}

	protected ClassDeclaration createClass(Position pos, TypeContext className, TypeListContext inheritsFrom,
	        boolean isAbstract, List<MemberDeclarationContext> members) {
		List<ResolvableIdentifier> superClasses = new ArrayList<>();
		if (inheritsFrom != null) {
			for (TypeContext typeContext : inheritsFrom.type()) {
				ResolvableIdentifier type = convertResolvableIdentifier(typeContext);
				superClasses.add(type);
				tupleDeclarationFactory.checkTupleType(type);
			}
		}

		ArrayList<AbstractGenericType> genericTypes = new ArrayList<>();
		ClassDeclaration cl =
		        new ClassDeclaration(pos, convertResolvableIdentifier(className), superClasses, new Block(pos),
		                isAbstract, genericTypes);

		TypeContext type = className;
		if (type.typeList() != null) {
			for (TypeContext typeContext1 : type.typeList().type()) {
				genericTypes.add(new AbstractGenericType(cl, position(typeContext1.getStart()),
				        convertResolvableIdentifier(typeContext1)));
			}
		}

		if (members == null) {
			members = new ArrayList<>();
		}
		currentBlocks.push(cl.getBlock());
		for (MemberDeclarationContext member : members) {
			currentVariableContext = VariableDeclaration.DeclarationType.ATTRIBUTE;
			currentFunctionContext = FunctionDeclaration.DeclarationType.METHOD;
			ASTNode astNode = visit(member);
			if (astNode instanceof Declaration) {

				Declaration decl = (Declaration) astNode;
				AccessModifierContext modifierCtx = member.accessModifier();

				// access modifiers are optional
				if (modifierCtx != null) {
					decl.setAccessModifier(AccessModifier.stringToAccess(modifierCtx.modifier.getText()));
				}
				// if none is given, the default accessibility is "package"
				else {
					decl.setAccessModifier(AccessModifier.PACKAGE);
				}

				cl.getBlock().addDeclaration(decl);
			} else if (astNode instanceof Assignment) {

				Assignment asgnmnt =
				        new Assignment(astNode.getPosition(), new MemberAccess(astNode.getPosition(),
				                new SelfExpression(new Position()), ((Assignment) astNode).getLeft()),
				                ((Assignment) astNode).getRight());
				cl.getBlock().addStatement(asgnmnt);
			}
		}
		currentBlocks.pop();
		return cl;
	}

	@Override
	public ASTNode visitAbstractMethodDeclaration(AbstractMethodDeclarationContext ctx) {
		return buildAbstractMethod(
		        true,
		        ctx.parameterList(),
		        ctx.getStart(),
		        ctx.type(),
		        new Identifier(getText(ctx.Identifier())));
	}

	private List<VariableDeclaration> parameterListToVarDeclList(ParameterListContext parameter) {
		if (parameter == null) {
			return new ArrayList<>();
		}
		ArrayList<VariableDeclaration> parameterList = new ArrayList<>();
		currentVariableContext = VariableDeclaration.DeclarationType.PARAMETER;
		for (VariableDeclarationContext var : parameter.variableDeclaration()) {
			parameterList.add((VariableDeclaration) visit(var));
		}
		return parameterList;
	}

	private void parameterListToVarDeclListIncludingDefaults(ParameterListContext context,
	        List<VariableDeclaration> params, List<VariableDeclaration> defaultParams, List<Expression> defaultValues) {

		if (context == null) {
			return;
		}
		currentVariableContext = VariableDeclaration.DeclarationType.PARAMETER;
		if (context.variableDeclaration() != null) {
			for (VariableDeclarationContext var : context.variableDeclaration()) {
				params.add((VariableDeclaration) visit(var));
			}
		}

		if (context.defaultParameter() != null) {
			for (DefaultParameterContext var : context.defaultParameter()) {
				defaultParams.add((VariableDeclaration) visit(var.variableDeclaration()));
				defaultValues.add((Expression) visit(var.expression()));
			}
		}

	}

	private List<DefaultParameterContext> defaultParameterListToVarDeclList(ParameterListContext parameter) {
		if (parameter == null) {
			return new ArrayList<>();
		}
		currentVariableContext = VariableDeclaration.DeclarationType.PARAMETER;
		return parameter.defaultParameter();
	}

	@Override
	public ASTNode visitForStatement(ForStatementContext ctx) {
		ResolvableIdentifier ident = new ResolvableIdentifier(getText(ctx.Identifier()));
		Position pos = position(ctx.getStart());
		Expression iterableExpression =
		        new MemberAccess(pos, (Expression) visit(ctx.expression()), new FunctionCall(pos,
		                new ResolvableIdentifier("getIterator"), new ArrayList<Expression>()));
		return createForLoop(pos, ident, iterableExpression, (Block) visit(ctx.statementBlock()));
	}

	private ASTNode createForLoop(Position pos, ResolvableIdentifier indexVar, Expression iterableExpression, Block body) {
		// Iterator<Int> r := MyRange(10).getIterator()
		// while true:
		// ....Maybe<Int> _i := r.getNext()
		// ....if _i.hasValue():
		// ........Int i := (_i as Just<Int>).getValue()
		// ........// body
		// ....else:
		// ........break

		// ## MONTY: ## Iterator<Int> r := MyRange(10).getIterator()
		// get the iterator object
		ResolvableIdentifier iterableIdentifier = TmpIdentifierFactory.getUniqueIdentifier();
		VariableDeclaration iterableDeclaration = new VariableDeclaration(pos, iterableIdentifier, iterableExpression);
		Assignment iterableAssignment =
		        new Assignment(pos, new VariableAccess(pos, iterableIdentifier), iterableExpression);
		// add the declaration and the assignment to the current block
		currentBlocks.peek().addDeclaration(iterableDeclaration);
		currentBlocks.peek().addStatement(iterableAssignment);

		// ## MONTY: ## while true:
		Block whileBlock = new Block(pos);
		WhileLoop loop = new WhileLoop(pos, new BooleanLiteral(pos, true), whileBlock);

		// ## MONTY: ## Maybe<Int> _i := r.getNext()
		// add the maybe value containing the indexing variable to the loop's body
		ResolvableIdentifier maybeIdent = TmpIdentifierFactory.getUniqueIdentifier();
		Expression callGetNext =
		        new MemberAccess(pos, new VariableAccess(pos, iterableIdentifier), new FunctionCall(pos,
		                new ResolvableIdentifier("getNext"), new ArrayList<Expression>()));
		whileBlock.addDeclaration(new VariableDeclaration(pos, maybeIdent, callGetNext));
		whileBlock.getStatements().add(
		        0,
		        new Assignment(pos, new VariableAccess(pos, ResolvableIdentifier.convert(maybeIdent)), callGetNext));

		// ## MONTY: ## if _i.hasValue():
		// check whether we have a Just<T> or a Nothing<T> here...
		Block thenBlock = body;
		Block elseBlock = new Block(pos);
		Statement ifstm =
		        new ConditionalStatement(pos, new MemberAccess(pos, new VariableAccess(pos, maybeIdent),
		                new FunctionCall(pos, new ResolvableIdentifier("hasValue"), new ArrayList<Expression>())),
		                thenBlock, elseBlock);

		// ## MONTY: ## Int i := (_i as Just<Int>).getValue()
		ResolvableIdentifier iteratorType =
		        new ResolvableIdentifier("Just", Arrays.asList((ResolvableIdentifier) null));
		Expression getValueExpr =
		        new MemberAccess(pos, new CastExpression(pos, new VariableAccess(pos, maybeIdent), iteratorType,
		                callGetNext), new FunctionCall(pos, new ResolvableIdentifier("getValue"),
		                new ArrayList<Expression>()));
		VariableDeclaration itDecl = new VariableDeclaration(pos, indexVar, getValueExpr);
		Assignment itAss = new Assignment(pos, new VariableAccess(pos, indexVar), getValueExpr);
		thenBlock.addDeclaration(itDecl);
		thenBlock.getStatements().add(0, itAss);

		whileBlock.addStatement(ifstm);

		// ## MONTY: ## else: break
		elseBlock.addStatement(new BreakStatement(pos));
		return loop;
	}

	@Override
	public ASTNode visitWhileStatement(WhileStatementContext ctx) {
		ASTNode expr = visit(ctx.expression());
		WhileLoop loop =
		        new WhileLoop(position(ctx.getStart()), (Expression) expr, (Block) visit(ctx.statementBlock()));

		return loop;
	}

	@Override
	public ASTNode visitIfStatement(IfStatementContext ctx) {

		Block leastElseBlock = new Block(new Position());
		if (ctx.elseBlock != null) {
			leastElseBlock = (Block) visit(ctx.elseBlock);
		}
		Block firstElseBlock;

		if (ctx.elif().isEmpty()) {
			firstElseBlock = leastElseBlock;
		} else {

			Block lastElseBlock = new Block(position(ctx.getStart()));
			firstElseBlock = lastElseBlock;
			Block currentElseBlock;

			for (int i = 0; i < ctx.elif().size(); i++) {
				ElifContext currentCtx = ctx.elif(i);

				if (i == ctx.elif().size() - 1) {
					currentElseBlock = leastElseBlock;
				} else {
					currentElseBlock = new Block(position(currentCtx.getStart()));
				}

				lastElseBlock.addStatement(new ConditionalStatement(position(ctx.elif().get(i).getStart()),
				        (Expression) visit(currentCtx.elifCondition), (Block) visit(currentCtx.elifBlock),
				        currentElseBlock));
				lastElseBlock = currentElseBlock;

			}
		}
		return new ConditionalStatement(position(ctx.getStart()), (Expression) visit(ctx.ifCondition),
		        (Block) visit(ctx.thenBlock), firstElseBlock);

	}

	@Override
	public ASTNode visitTryStatement(TryStatementContext ctx) {
		ASTNode decl = visit(ctx.variableDeclaration().get(0));
		TryStatement tryStm =
		        new TryStatement(position(ctx.getStart()), (VariableDeclaration) decl, new Block(
		                position(ctx.getStart())), new Block(position(ctx.getStart())));
		addStatementsToBlock(tryStm.getTryBlock(), ctx.tryBlock.statement());
		addStatementsToBlock(tryStm.getHandleBlock(), ctx.handleBlock.statement());
		return tryStm;
	}

	public void addStatementsToBlock(Block block, List<StatementContext> statements) {
		for (StatementContext stm : statements) {
			currentVariableContext = VariableDeclaration.DeclarationType.VARIABLE;
			currentFunctionContext = FunctionDeclaration.DeclarationType.UNBOUND;
			ASTNode node = visit(stm);
			if (node instanceof Statement) {
				block.addStatement((Statement) node);
			} else {
				block.addDeclaration((Declaration) node);
			}
		}
	}

	@Override
	public ASTNode visitIndependentDeclaration(IndependentDeclarationContext ctx) {
		ASTNode node;
		if (ctx.functionDeclaration() != null) {
			node = visit(ctx.functionDeclaration());
		} else {
			node = visit(ctx.variableDeclaration());
			if (ctx.expression() != null) {
				currentBlocks.peek().addDeclaration((Declaration) node);
				return new Assignment(position(ctx.getStart()), new VariableAccess(position(ctx.getStart()),
				        ResolvableIdentifier.convert(((VariableDeclaration) node).getIdentifier())),
				        (Expression) visit(ctx.expression()));
			}
		}
		return node;
	}

	@Override
	public ASTNode visitStatementBlock(StatementBlockContext ctx) {

		Block block = new Block(position(ctx.getStart()));
		currentBlocks.push(block);
		addStatementsToBlock(block, ctx.statement());
		currentBlocks.pop();
		return block;
	}

	@Override
	public ASTNode visitReturnStm(ReturnStmContext ctx) {
		if (currentGeneratorReturnType.peek() != null) {
			// if we are inside a generator, automatically return "Nothing<>"
			return new ReturnStatement(position(ctx.getStart()), new FunctionCall(position(ctx.getStop()),
			        new ResolvableIdentifier("Nothing", Arrays.asList(currentGeneratorReturnType.peek())),
			        new ArrayList<Expression>()));
		} else {
			ASTNode expr = null;
			if (ctx.expression() != null) {
				expr = visit(ctx.expression());
			}

			return new ReturnStatement(position(ctx.getStart()), (Expression) expr);
		}
	}

	@Override
	public ASTNode visitYieldStm(YieldStmContext ctx) {
		ASTNode expr = visit(ctx.expression());
		return createYieldStatement(position(ctx.getStart()), (Expression) expr);
	}

	protected Statement createYieldStatement(Position pos, Expression expr) {
		return new YieldStatement(pos, new FunctionCall(expr.getPosition(), new ResolvableIdentifier("Just",
		        Arrays.asList(currentGeneratorReturnType.peek())), Arrays.asList(expr)));
	}

	@Override
	public ASTNode visitRaiseStm(RaiseStmContext ctx) {
		ASTNode expr = null;
		if (ctx.expression() != null) {

			expr = visit(ctx.expression());
		}
		return new RaiseStatement(position(ctx.getStart()), (Expression) expr);
	}

	@Override
	public ASTNode visitBreakStm(BreakStmContext ctx) {

		return new BreakStatement(position(ctx.getStart()));
	}

	@Override
	public ASTNode visitSkipStm(SkipStmContext ctx) {

		return new SkipStatement(position(ctx.getStart()));
	}

	@Override
	public ASTNode visitExpression(ExpressionContext ctx) {

		if (ctx.primary() != null) {
			return visit(ctx.primary());
		} else if (ctx.ifExpCondition != null && ctx.ifExprElse != null && ctx.ifExprThen != null) {
			return visitTernary(ctx);
		} else if (ctx.functionCall() != null) {
			return visit(ctx.functionCall());
		} else if (ctx.functionExpression() != null) {
			return visit(ctx.functionExpression());
		} else if (ctx.accessOperator() != null) {
			return visitMemberAccessExpr(ctx);
		} else if (ctx.plusMinusOperator() != null && ctx.singleExpression != null) {

			return unaryExpression(
			        position(ctx.getStart()),
			        ctx.plusMinusOperator().operator.getText(),
			        ctx.singleExpression);
		} else if (ctx.notOperator() != null) {

			return unaryExpression(position(ctx.getStart()), ctx.notOperator().operator.getText(), ctx.singleExpression);
		} else if (ctx.powerOperator() != null) {

			return binaryExpression(position(ctx.getStart()), ctx.powerOperator().getText(), ctx.left, ctx.right);
		} else if (ctx.dotOperator() != null) {

			return binaryExpression(position(ctx.getStart()), ctx.dotOperator().getText(), ctx.left, ctx.right);
		} else if (ctx.plusMinusOperator() != null) {

			return binaryExpression(position(ctx.getStart()), ctx.plusMinusOperator().getText(), ctx.left, ctx.right);
		} else if (ctx.compareOperator() != null) {

			return binaryExpression(position(ctx.getStart()), ctx.compareOperator().getText(), ctx.left, ctx.right);
		} else if (ctx.eqOperator() != null) {

			return binaryExpression(position(ctx.getStart()), ctx.eqOperator().getText(), ctx.left, ctx.right);
		} else if (ctx.inOperator() != null) {

			return binaryExpression(position(ctx.getStart()), ctx.inOperator().getText(), ctx.left, ctx.right);
		} else if (ctx.andOperator() != null) {

			return booleanExpression(position(ctx.getStart()), ctx.andOperator().getText(), ctx.left, ctx.right);
		} else if (ctx.orOperator() != null) {

			return booleanExpression(position(ctx.getStart()), ctx.orOperator().getText(), ctx.left, ctx.right);
		} else if (ctx.asOperator() != null) {
			return visitCastExpression(ctx);
		} else if (ctx.isOperator() != null) {
			return visitIsExpression(ctx);
		} else if (ctx.listComprehension() != null) {
			return visitListComprehension(ctx.listComprehension());
		}
		return null;
	}

	@Override
	public ASTNode visitPrimary(PrimaryContext ctx) {
		if (ctx.singleExpression != null) {

			return visit(ctx.singleExpression);
		} else if (ctx.literal() != null) {

			return visit(ctx.literal());
		} else if (ctx.parent != null) {

			return visitParent(ctx);
		} else if (ctx.Identifier() != null) {

			return visitIdentifier(ctx);
		} else {

			return visitSelf(ctx);
		}
	}

	@Override
	public ASTNode visitLiteral(LiteralContext ctx) {

		if (ctx.IntegerLiteral() != null) {

			return new IntegerLiteral(position(ctx.getStart()),
			        Integer.parseInt(ctx.IntegerLiteral().getSymbol().getText()));
		} else if (ctx.RealLiteral() != null) {

			return new FloatLiteral(position(ctx.getStart()), Float.parseFloat(ctx.RealLiteral().getSymbol().getText()));
		} else if (ctx.CharacterLiteral() != null) {

			return new CharacterLiteral(position(ctx.getStart()), ctx.CharacterLiteral().getSymbol().getText());
		} else if (ctx.StringLiteral() != null) {

			return new StringLiteral(position(ctx.getStart()), ctx.StringLiteral().getSymbol().getText());
		} else if (ctx.arrayLiteral() != null) {
			ArrayList<Expression> elements = new ArrayList<>();
			for (ExpressionContext eContext : ctx.arrayLiteral().expression()) {
				elements.add((Expression) visit(eContext));
			}
			return new ArrayLiteral(position(ctx.getStart()), elements);
		} else if (ctx.rangeLiteral() != null) {
			ArrayList<Expression> parameters = new ArrayList<>();
			for (ExpressionContext eContext : ctx.rangeLiteral().expression()) {
				parameters.add((Expression) visit(eContext));
			}
			return new FunctionCall(position(ctx.getStart()), new ResolvableIdentifier("Range"), parameters);
		} else if (ctx.tupleLiteral() != null) {
			ArrayList<Expression> elements = new ArrayList<>();
			for (ExpressionContext eContext : ctx.tupleLiteral().expression()) {
				elements.add((Expression) visit(eContext));
			}
			// generate a new tuple type if necessary
			tupleDeclarationFactory.checkTupleType(elements.size());
			TupleLiteral tuple = new TupleLiteral(position(ctx.getStart()), elements);
			return tuple;
		} else {
			return new BooleanLiteral(position(ctx.getStart()), Boolean.parseBoolean(ctx.BooleanLiteral().toString()));
		}
	}

	public ASTNode visitIdentifier(PrimaryContext ctx) {

		return new VariableAccess(position(ctx.getStart()), new ResolvableIdentifier(getText(ctx.Identifier())));
	}

	public ASTNode visitSelf(PrimaryContext ctx) {

		return new SelfExpression(position(ctx.getStart()));
	}

	public ParentExpression visitParent(PrimaryContext ctx) {
		return new ParentExpression(position(ctx.getStart()), convertResolvableIdentifier(ctx.type()));
	}

	public ASTNode visitTernary(ExpressionContext ctx) {
		ASTNode condition = visit(ctx.ifExpCondition);
		ASTNode thenExpr = visit(ctx.ifExprThen);
		ASTNode elseExpr = visit(ctx.ifExprElse);
		return new ConditionalExpression(position(ctx.getStart()), (Expression) condition, (Expression) thenExpr,
		        (Expression) elseExpr);
	}

	@Override
	public ASTNode visitMemberAccessStmt(@NotNull MemberAccessStmtContext ctx) {
		ASTNode left = visit(ctx.left);
		ASTNode right = visit(ctx.right);
		return new MemberAccess(position(ctx.getStart()), (Expression) left, (Expression) right);
	}

	public ASTNode visitMemberAccessExpr(ExpressionContext ctx) {
		ASTNode left = visit(ctx.left);
		ASTNode right = visit(ctx.right);
		return new MemberAccess(position(ctx.getStart()), (Expression) left, (Expression) right);
	}

	private MemberAccess unaryExpression(Position position, String operator, ExpressionContext expr) {
		Expression self = (Expression) visit(expr);
		FunctionCall operatorCall =
		        new FunctionCall(position, new ResolvableIdentifier(unaryOperatorMapping.get(operator)),
		                new ArrayList<Expression>());
		return new MemberAccess(position, self, operatorCall);
	}

	private MemberAccess binaryExpression(Position position, String operator, ExpressionContext left,
	        ExpressionContext right) {

		Expression self = (Expression) visit(left);
		String methodName = binaryOperatorMapping.get(operator);
		FunctionCall operatorCall;
		// we have a special case for "a in x", which translates to "x._contains_(a)"
		if (methodName.equals("_contains_")) {
			Expression rightExpr = (Expression) visit(right);
			operatorCall = new FunctionCall(position, new ResolvableIdentifier(methodName), Arrays.asList(self));

			return new MemberAccess(position, rightExpr, operatorCall);
		} else {
			operatorCall =
			        new FunctionCall(position, new ResolvableIdentifier(methodName),
			                Arrays.asList((Expression) visit(right)));
			return new MemberAccess(position, self, operatorCall);
		}

	}

	private Expression booleanExpression(Position position, String operator, ExpressionContext left,
	        ExpressionContext right) {
		Expression leftExp = (Expression) visit(left);
		Expression rightExp = (Expression) visit(right);
		if (operator.equals("and")) {
			return createAndExpression(position, leftExp, rightExp);
		} else {
			return createOrExpression(position, leftExp, rightExp);
		}
	}

	private ConditionalExpression createAndExpression(Position position, Expression left, Expression right) {
		// (x and y) ==> y if x else false
		return new ConditionalExpression(position, left, right, new BooleanLiteral(position, false));
	}

	private ConditionalExpression createOrExpression(Position position, Expression left, Expression right) {
		// (x or y) ==> true if x else y
		return new ConditionalExpression(position, left, new BooleanLiteral(position, true), right);
	}

	private CastExpression visitCastExpression(ExpressionContext ctx) {
		ResolvableIdentifier type = convertResolvableIdentifier(ctx.type());
		tupleDeclarationFactory.checkTupleType(type);
		return new CastExpression(position(ctx.getStart()), (Expression) visit(ctx.expr), type);
	}

	private IsExpression visitIsExpression(ExpressionContext ctx) {
		ResolvableIdentifier type = new ResolvableIdentifier(getText(ctx.ClassIdentifier()));
		tupleDeclarationFactory.checkTupleType(type);
		return new IsExpression(position(ctx.getStart()), (Expression) visit(ctx.expr), type);
	}

	protected ASTNode aggregateResult(ASTNode aggregate, ASTNode nextResult) {
		return nextResult == null ? aggregate : nextResult;
	}

	@Override
	public ASTNode visitListComprehension(ListComprehensionContext ctx) {
		Expression target = (Expression) visit(ctx.expression());
		Position pos = position(ctx.getStart());
		Stack<ResolvableIdentifier> identifiers = new Stack<>();
		Stack<Expression> sources = new Stack<>();
		Stack<Expression> filters = new Stack<>();
		ResolvableIdentifier type = convertResolvableIdentifier(ctx.type());
		tupleDeclarationFactory.checkTupleType(type);

		for (ListGeneratorContext genCtx : ctx.listGenerator()) {
			identifiers.push(new ResolvableIdentifier(getText(genCtx.Identifier())));
			sources.push((Expression) visit(genCtx.expression()));
			if (genCtx.listFilter() != null) {
				filters.push((Expression) visit(genCtx.listFilter().expression()));
			} else {
				filters.push(null);
			}
		}

		// we start with the innermost block
		Block currentBlock = new Block(position(ctx.expression().getStart()));
		currentBlocks.push(currentBlock);
		currentGeneratorReturnType.push(type);
		currentBlock.addStatement(createYieldStatement(position(ctx.expression().getStart()), target));
		currentGeneratorReturnType.pop();
		for (int i = identifiers.size(); i > 0; i--) {
			Expression filter = filters.pop();
			Expression source = sources.pop();
			source =
			        new MemberAccess(source.getPosition(), source, new FunctionCall(source.getPosition(),
			                new ResolvableIdentifier("getIterator"), new ArrayList<Expression>()));
			ResolvableIdentifier ident = identifiers.pop();
			if (filter != null) {
				Position filterPos = filter.getPosition();
				Statement ifStm = new ConditionalStatement(filterPos, filter, currentBlock, new Block(filterPos));
				currentBlocks.pop();
				currentBlock = new Block(filterPos);
				currentBlocks.push(currentBlock);
				currentBlock.addStatement(ifStm);
			}
			currentBlocks.pop();
			Block forBlock = new Block(source.getPosition()); // the block containing the for stm
			currentBlocks.push(forBlock);
			Statement forStm = (Statement) createForLoop(source.getPosition(), ident, source, currentBlock);
			currentBlock = forBlock;
			currentBlock.addStatement(forStm);
		}
		currentBlocks.pop();
		// the current block is now the outermost for loop
		ClassDeclaration generator =
		        (ClassDeclaration) createGenerator(pos, TmpIdentifierFactory.getUniqueIdentifier(), type,
		        // new ResolvableIdentifier("Object"),
		                new ArrayList<VariableDeclaration>(),
		                new ArrayList<VariableDeclaration>(),
		                new ArrayList<Expression>(),
		                currentBlock);
		currentBlocks.peek().addDeclaration(generator);
		// return new instance
		return new WrappedFunctionCall(pos, new FunctionCall(pos,
		        ResolvableIdentifier.convert(generator.getIdentifier()), new ArrayList<Expression>()));
	}

	/* ---- Pattern matching ---- */

	@Override
	public ASTNode visitCaseStatement(CaseStatementContext ctx) {
		Position pos = position(ctx.getStart());
		// extract the expression and make a new temporary variable for it...
		Expression subject = (Expression) visit(ctx.expression());
		ResolvableIdentifier subjectIdent = TmpIdentifierFactory.getUniqueIdentifier();
		VariableDeclaration subjectVar = new VariableDeclaration(subject.getPosition(), subjectIdent, subject);
		Assignment subjectAss =
		        new Assignment(subject.getPosition(), new VariableAccess(subject.getPosition(), subjectIdent), subject);
		currentBlocks.peek().addDeclaration(subjectVar);
		currentBlocks.peek().addStatement(subjectAss);

		// add a boolean variable in order to indicate, whether a match was successful (more flexible than nesting else)
		ResolvableIdentifier alreadyMatchedIdent = TmpIdentifierFactory.getUniqueIdentifier();
		VariableDeclaration alreadyMatchedDecl =
		        new VariableDeclaration(pos, alreadyMatchedIdent, new ResolvableIdentifier("Bool"));
		currentBlocks.peek().addDeclaration(alreadyMatchedDecl);
		currentBlocks.peek().addStatement(setAlreadyMatched(pos, alreadyMatchedIdent, false));

		// convert the different cases into an if-else-orgy
		List<PatternContext> patterns = ctx.caseBlock().pattern();
		List<StatementBlockContext> blocks = ctx.caseBlock().statementBlock();
		return createPatternIfElifs(patterns, blocks, new VariableAccess(pos, subjectIdent), alreadyMatchedIdent);
	}

	public Statement createPatternIfElifs(List<PatternContext> patterns, List<StatementBlockContext> blocks,
	        Expression subject, ResolvableIdentifier alreadyMatchedIdent) {
		Block outermostBlock = new Block(new Position());
		Block currentBlock = outermostBlock;
		int i = 0;
		for (PatternContext pattern : patterns) {
			patternMatchingContext.clearBindings();
			Position patternPos = position(pattern.getStart());
			Block patternBlock = (Block) visit(blocks.get(i));
			// at the end of every case block, we have to set the alreadyMatched variable to true
			patternBlock.addStatement(setAlreadyMatched(patternPos, alreadyMatchedIdent, true));

			currentBlock.addStatement(handlePattern(pattern, subject, patternBlock));
			Block insteadOfElseBlock = new Block(patternPos);
			currentBlock.addStatement(ifNotAlreadyMatched(
			        patternPos,
			        alreadyMatchedIdent,
			        insteadOfElseBlock,
			        new Block(patternPos)));
			currentBlock = insteadOfElseBlock;
			i++;
		}
		currentBlocks.peek().addStatement(outermostBlock.getStatements().get(0));
		return outermostBlock.getStatements().get(1);
	}

	public ConditionalStatement handlePattern(PatternContext ctx, Expression subject, Block patternBlock) {
		if (ctx.compoundPattern() != null) {
			return handleCompoundPattern(ctx.compoundPattern(), subject, patternBlock);
		} else if (ctx.expression() != null) {
			return handleExpressionPattern(ctx, subject, patternBlock);
		} else if (ctx.typedPattern() != null) {
			return handleTypedPattern(ctx, subject, patternBlock);
		} else {
			return handleWildcardPattern(ctx, subject, patternBlock);
		}
	}

	protected ConditionalStatement handleCompoundPattern(CompoundPatternContext ctx, Expression subject,
	        Block patternBlock) {
		Position pos = position(ctx.getStart());
		int patternCount = ctx.patternList() != null ? ctx.patternList().pattern().size() : 0;
		// if there was no type specified, the pattern matches tuples, thus we assume that the value is already
		// a tuple and there is no need for type checks or type casts. Further, tuples don't have to be decomposed.
		if (ctx.type() != null) {

			ResolvableIdentifier type = convertResolvableIdentifier(ctx.type());
			tupleDeclarationFactory.checkTupleType(type);
			Expression castedArg = new CastExpression(pos, subject, type, true);
			ResolvableIdentifier decomposedIdent = TmpIdentifierFactory.getUniqueIdentifier();
			Block thenBlock = ctx.patternList() != null ? new Block(pos) : patternBlock;

			ConditionalStatement stm =
			        new ConditionalStatement(pos, new IsExpression(pos, subject, type), thenBlock, new Block(pos));

			if (ctx.patternList() != null) {
				Expression decomposeExpr =
				        new FunctionCall(pos, new ResolvableIdentifier("decompose"), Arrays.asList(castedArg));
				thenBlock.addDeclaration(new VariableDeclaration(pos, decomposedIdent, decomposeExpr));
				thenBlock.addStatement(new Assignment(pos, new VariableAccess(pos, decomposedIdent), decomposeExpr));
				subject = new VariableAccess(pos, decomposedIdent);
				if (patternCount == 1) {
					thenBlock.addStatement(handlePattern(ctx.patternList().pattern().get(0), subject, patternBlock));
				} else {
					thenBlock.addStatement(processNestedCompoundPattern(
					        ctx.patternList().pattern(),
					        subject,
					        1,
					        patternBlock));
				}
			}
			return stm;
		}
		if (patternCount == 1) {
			return handlePattern(ctx.patternList().pattern().get(0), subject, patternBlock);
		}
		return processNestedCompoundPattern(ctx.patternList().pattern(), subject, 1, patternBlock);
	}

	protected ConditionalStatement processNestedCompoundPattern(List<PatternContext> patterns, Expression subject,
	        int i, Block patternBlock) {
		PatternContext pattern = patterns.get(0);
		patterns.remove(0);
		Position subPatternPos = position(pattern.getStart());

		Expression subjectPart =
		        new MemberAccess(subPatternPos, subject, new VariableAccess(subPatternPos, new ResolvableIdentifier("_"
		                + i)));

		Block thenBlock;
		if (patterns.size() > 0) {
			thenBlock = new Block(subPatternPos);
			thenBlock.addStatement(processNestedCompoundPattern(patterns, subject, i + 1, patternBlock));
		} else {
			thenBlock = patternBlock;
		}
		return handlePattern(pattern, subjectPart, thenBlock);
	}

	protected ConditionalStatement handleWildcardPattern(PatternContext ctx, Expression subject, Block patternBlock) {
		// _ matches everything, but binds nothing
		Position pos = position(ctx.getStart());
		return new ConditionalStatement(pos, new BooleanLiteral(pos, true), patternBlock, new Block(pos));
	}

	protected ConditionalStatement handleTypedPattern(PatternContext ctx, Expression subject, Block patternBlock) {
		Position pos = position(ctx.getStart());
		Expression condition;
		// a typed pattern matches everything that is an instance of "type"
		ResolvableIdentifier type = convertResolvableIdentifier(ctx.typedPattern().type());
		tupleDeclarationFactory.checkTupleType(type);
		condition = new IsExpression(position(ctx.getStart()), subject, type);
		// if the identifier is not the wildcard '_', the value is bound to that identifier
		if (ctx.typedPattern().Identifier() != null) {
			ResolvableIdentifier localDecl = new ResolvableIdentifier(getText(ctx.typedPattern().Identifier()));
			VariableDeclaration localVarDecl = new VariableDeclaration(pos, localDecl, type);
			patternBlock.addDeclaration(localVarDecl);
			patternMatchingContext.addBinding(localVarDecl);
			patternBlock.getStatements().add(
			        0,
			        new Assignment(pos, new VariableAccess(pos, localDecl), new CastExpression(pos, subject, type)));
		}
		return new ConditionalStatement(pos, condition, patternBlock, new Block(pos));
	}

	protected ConditionalStatement handleExpressionPattern(PatternContext ctx, Expression subject, Block patternBlock) {
		Position pos = position(ctx.getStart());
		// if the pattern is an expression, we simply check the subject and the pattern expression for equality
		Expression expression = (Expression) visitExpression(ctx.expression());
		return new ConditionalStatement(pos, createAndExpression(
		        pos,
		        new IsExpression(pos, subject, expression),
		        new MemberAccess(pos, new CastExpression(pos, subject, expression, true), new FunctionCall(pos,
		                new ResolvableIdentifier("_eq_"), Arrays.asList(expression)))), patternBlock, new Block(pos));
	}

	protected Statement setAlreadyMatched(Position pos, ResolvableIdentifier alreadyMatchedIdent, boolean value) {
		return new Assignment(pos, new VariableAccess(pos, alreadyMatchedIdent), new BooleanLiteral(pos, value));
	}

	protected ConditionalStatement ifNotAlreadyMatched(Position pos, ResolvableIdentifier alreadyMatchedIdent,
	        Block thenBlock, Block elseBlock) {
		return new ConditionalStatement(pos, new MemberAccess(pos, new VariableAccess(pos, alreadyMatchedIdent),
		        new FunctionCall(pos, new ResolvableIdentifier("_not_"), new ArrayList<Expression>())), thenBlock,
		        elseBlock);
	}

	/** {@inheritDoc} */
	@Override
	public ASTNode visitCaseClassDeclaration(CaseClassDeclarationContext ctx) {
		Position pos = position(ctx.getStart());
		ClassDeclaration classDecl = createClass(pos, ctx.type(), ctx.typeList(), false, ctx.memberDeclaration());

		List<VariableDeclaration> parameterList =
		        createParameterListWithoutDefaults(ctx.parameterListWithoutDefaults());
		Block classBlock = classDecl.getBlock();
		Block initBody = new Block(pos);

		for (VariableDeclaration var : parameterList) {
			Position vp = var.getPosition();
			ResolvableIdentifier resId = ResolvableIdentifier.convert(var.getIdentifier());
			VariableDeclaration attr =
			        new VariableDeclaration(vp, resId, var.getTypeIdentifier(),
			                VariableDeclaration.DeclarationType.ATTRIBUTE);
			attr.setAccessModifier(AccessModifier.PACKAGE);
			classBlock.addDeclaration(attr);

			initBody.addStatement(new Assignment(pos, new MemberAccess(vp, new SelfExpression(vp), new VariableAccess(
			        vp, resId)), new VariableAccess(vp, resId)));
		}

		FunctionDeclaration caseInit =
		        new FunctionDeclaration(pos, new Identifier("initializer"), initBody, parameterList,
		                DeclarationType.INITIALIZER, null);
		classDecl.getBlock().addDeclaration(caseInit);
		classDecl.getBlock().addDeclaration(createCaseEqMethod(pos, parameterList, classDecl));
		currentBlocks.peek().addDeclaration(classDecl);

		// create the decompose function
		return createDecomposerFunction(pos, parameterList, classDecl);
	}

	protected FunctionDeclaration createDecomposerFunction(Position pos, List<VariableDeclaration> parameterList,
	        ClassDeclaration classDecl) {
		int n = parameterList.size();
		Block decomposeBody = new Block(pos);

		ResolvableIdentifier decompReturnType;
		ResolvableIdentifier decompParam = new ResolvableIdentifier("obj");
		if (n != 1) {
			List<ResolvableIdentifier> decomposeTypes = new ArrayList<>(n);
			List<Expression> decompExp = new ArrayList<>(n);

			for (VariableDeclaration var : parameterList) {
				ResolvableIdentifier varType = var.getTypeIdentifier();
				tupleDeclarationFactory.checkTupleType(varType);
				decomposeTypes.add(varType);
				decompExp.add(new MemberAccess(pos, new VariableAccess(pos, decompParam), new VariableAccess(pos,
				        ResolvableIdentifier.convert(var.getIdentifier()))));
			}
			decompReturnType = new ResolvableIdentifier("Tuple" + n, decomposeTypes);
			tupleDeclarationFactory.checkTupleType(decompReturnType);
			decomposeBody.addStatement(new ReturnStatement(pos, new FunctionCall(pos, decompReturnType, decompExp)));
		} else {
			VariableDeclaration value = parameterList.get(0);
			decompReturnType = value.getTypeIdentifier();
			decomposeBody.addStatement(new ReturnStatement(pos, new MemberAccess(pos, new VariableAccess(pos,
			        decompParam), new VariableAccess(pos, ResolvableIdentifier.convert(value.getIdentifier())))));
		}

		return new FunctionDeclaration(pos, new Identifier("decompose"), decomposeBody,
		        Arrays.asList(new VariableDeclaration(pos, decompParam,
		                ResolvableIdentifier.convert(classDecl.getIdentifier()),
		                VariableDeclaration.DeclarationType.PARAMETER)), DeclarationType.UNBOUND, decompReturnType);
	}

	protected FunctionDeclaration createCaseEqMethod(Position pos, List<VariableDeclaration> attributes,
	        ClassDeclaration classDecl) {
		Block body = new Block(pos);
		ResolvableIdentifier paramIdent = new ResolvableIdentifier("other");
		ResolvableIdentifier classIdentifier = ResolvableIdentifier.convert(classDecl.getIdentifier());

		Expression returnExpression;

		if (attributes.size() > 0) {
			List<Expression> returnExpressions = new ArrayList<>(attributes.size());

			for (VariableDeclaration attr : attributes) {
				ResolvableIdentifier attrId = ResolvableIdentifier.convert(attr.getIdentifier());
				Expression selfAttr = new MemberAccess(pos, new SelfExpression(pos), new VariableAccess(pos, attrId));
				Expression otherAttr =
				        new MemberAccess(pos, new VariableAccess(pos, paramIdent), new VariableAccess(pos, attrId));
				returnExpressions.add(new MemberAccess(pos, selfAttr, new FunctionCall(pos, new ResolvableIdentifier(
				        "_eq_"), Arrays.asList(otherAttr))));
			}
			returnExpression = returnExpressions.get(0);
			returnExpressions.remove(0);
			while (returnExpressions.size() > 0) {
				returnExpression = createAndExpression(pos, returnExpression, returnExpressions.get(0));
				returnExpressions.remove(0);
			}
		} else {
			returnExpression = new BooleanLiteral(pos, true);
		}

		body.addStatement(new ReturnStatement(pos, returnExpression));

		return new FunctionDeclaration(pos, new ResolvableIdentifier("_eq_"), body,
		        Arrays.asList(new VariableDeclaration(pos, paramIdent, classIdentifier,
		                VariableDeclaration.DeclarationType.PARAMETER)), DeclarationType.METHOD,
		        ResolvableIdentifier.convert(CoreClasses.boolType().getIdentifier()));
	}
}
