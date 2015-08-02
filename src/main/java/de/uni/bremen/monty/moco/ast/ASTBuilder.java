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
import de.uni.bremen.monty.moco.ast.declaration.ProcedureDeclaration.DeclarationType;
import de.uni.bremen.monty.moco.ast.expression.*;
import de.uni.bremen.monty.moco.ast.expression.literal.*;
import de.uni.bremen.monty.moco.ast.statement.*;
import org.antlr.v4.runtime.Token;
import org.antlr.v4.runtime.misc.NotNull;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.apache.commons.io.FilenameUtils;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Stack;

public class ASTBuilder extends MontyBaseVisitor<ASTNode> {
	private final String fileName;
	private Stack<Block> currentBlocks;
	private VariableDeclaration.DeclarationType currentVariableContext;
	private ProcedureDeclaration.DeclarationType currentProcedureContext;

	public ASTBuilder(String fileName) {
		this.fileName = fileName;
		currentBlocks = new Stack<>();
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
		checkTupleType(type);
		return new VariableDeclaration(position(ctx.getStart()), new Identifier(getText(ctx.Identifier())), type,
		        currentVariableContext);
	}

	private ResolvableIdentifier convertResolvableIdentifier(TypeContext type) {
		String typeName;
		List<TypeContext> typeParameters = null;
		// if there is no class identifier, we have to handle syntactic sugar here
		if (type.ClassIdentifier() == null) {
			// a tuple
			typeParameters = type.type();
			int n = typeParameters != null ? typeParameters.size() : 0;
			typeName = "Tuple" + n;
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
		checkTupleType(identifier); // since constructor calls are also function calls
		FunctionCall func = new FunctionCall(position(ctx.getStart()), identifier, arguments);
		if (ctx.expressionList() != null) {
			for (ExpressionContext exprC : ctx.expressionList().expression()) {

				ASTNode expr = visit(exprC);
				if (expr instanceof Expression) {

					arguments.add((Expression) expr);
				}
			}
		}
		return func;
	}

	private void buildDefaultProcedures(boolean functionDeclaration, List<DefaultParameterContext> defaultParameter,
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

			if (declarationTypeCopy == ProcedureDeclaration.DeclarationType.METHOD) {
				expression = new MemberAccess(position(token), new SelfExpression(position(token)), expression);
			}

			ProcedureDeclaration procDecl1;
			if (functionDeclaration) {
				block.addStatement(new ReturnStatement(new Position(), expression));
				ResolvableIdentifier returnTypeIdent = convertResolvableIdentifier(typeContext);
				checkTupleType(returnTypeIdent);
				procDecl1 =
				        new ProcedureDeclaration(position(token), identifier, block, subParams, declarationTypeCopy,
				                returnTypeIdent);
			} else {
				block.addStatement((Statement) expression);
				block.addStatement(new ReturnStatement(new Position(), null));
				procDecl1 =
				        new ProcedureDeclaration(position(token), identifier, block, subParams, declarationTypeCopy,
				                (TypeDeclaration) null);
			}
			currentBlocks.peek().addDeclaration(procDecl1);
		}
	}

	private ProcedureDeclaration buildProcedures(boolean functionDeclaration,
	        ParameterListContext parameterListContext, Token token, TypeContext typeContext,
	        StatementBlockContext statementBlockContext, Identifier identifier) {

		ProcedureDeclaration.DeclarationType declarationTypeCopy = currentProcedureContext;
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

		buildDefaultProcedures(
		        functionDeclaration,
		        defaultParameter,
		        allVariableDeclarations,
		        params,
		        defaultExpression,
		        defaultVariableDeclaration,
		        identifier,
		        token,
		        typeContext,
		        declarationTypeCopy);

		ProcedureDeclaration procDecl2;

		if (functionDeclaration) {
			ResolvableIdentifier returnTypeIdent = convertResolvableIdentifier(typeContext);
			checkTupleType(returnTypeIdent);
			procDecl2 =
			        new ProcedureDeclaration(position(token), identifier, (Block) visit(statementBlockContext),
			                allVariableDeclarations, declarationTypeCopy, returnTypeIdent);
		} else {
			procDecl2 =
			        new ProcedureDeclaration(position(token), identifier, (Block) visit(statementBlockContext),
			                allVariableDeclarations, declarationTypeCopy, (TypeDeclaration) null);
		}
		return procDecl2;
	}

	private ProcedureDeclaration buildAbstractMethod(boolean functionDeclaration,
	        ParameterListContext parameterListContext, Token token, TypeContext typeContext, Identifier identifier) {

		List<VariableDeclaration> params = parameterListToVarDeclList(parameterListContext);

		ResolvableIdentifier typeIdent = null;
		if (typeContext != null) {
			typeIdent = convertResolvableIdentifier(typeContext);
			checkTupleType(typeIdent);
		}

		ProcedureDeclaration procDecl =
		        new ProcedureDeclaration(position(token), identifier, new Block(position(token)), params,
		                currentProcedureContext, typeIdent, true);
		return procDecl;
	}

	@Override
	public ASTNode visitFunctionDeclaration(FunctionDeclarationContext ctx) {
		Identifier identifier;
		if (ctx.binaryOperation() == null) {
			identifier = new Identifier(getText(ctx.Identifier()));
		} else {
			identifier = new Identifier("operator" + ctx.binaryOperation().getText());
		}

		return buildProcedures(true, ctx.parameterList(), ctx.getStart(), ctx.type(), ctx.statementBlock(), identifier);
	}

	@Override
	public ASTNode visitClassDeclaration(ClassDeclarationContext ctx) {
		List<ResolvableIdentifier> superClasses = new ArrayList<>();
		if (ctx.typeList() != null) {
			for (TypeContext typeContext : ctx.typeList().type()) {
				ResolvableIdentifier type = convertResolvableIdentifier(typeContext);
				superClasses.add(type);
				checkTupleType(type);
			}
		}

		ArrayList<AbstractGenericType> genericTypes = new ArrayList<>();
		// if there is an 'abstract' keyword, the class is abstract
		boolean isAbstract = ctx.getTokens(MontyParser.AbstractKeyword).size() > 0;

		ClassDeclaration cl =
		        new ClassDeclaration(position(ctx.getStart()), convertResolvableIdentifier(ctx.type()), superClasses,
		                new Block(position(ctx.getStart())), isAbstract, genericTypes);

		TypeContext type = ctx.type();
		if (type.typeList() != null) {
			for (TypeContext typeContext1 : type.typeList().type()) {
				genericTypes.add(new AbstractGenericType(cl, position(typeContext1.getStart()),
				        convertResolvableIdentifier(typeContext1)));
			}
		}

		currentBlocks.push(cl.getBlock());
		for (MemberDeclarationContext member : ctx.memberDeclaration()) {
			currentVariableContext = VariableDeclaration.DeclarationType.ATTRIBUTE;
			currentProcedureContext = ProcedureDeclaration.DeclarationType.METHOD;
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

	@Override
	public ASTNode visitProcedureDeclaration(ProcedureDeclarationContext ctx) {
		ProcedureDeclaration proc =
		        buildProcedures(false, ctx.parameterList(), ctx.start, null, ctx.statementBlock(), new Identifier(
		                getText(ctx.Identifier())));

		List<Statement> list = proc.getBody().getStatements();
		if ((list.isEmpty()) || !(list.get(list.size() - 1) instanceof ReturnStatement)) {
			list.add(new ReturnStatement(new Position(), null));
		}
		return proc;
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

	private List<DefaultParameterContext> defaultParameterListToVarDeclList(ParameterListContext parameter) {
		if (parameter == null) {
			return new ArrayList<>();
		}
		currentVariableContext = VariableDeclaration.DeclarationType.PARAMETER;
		return parameter.defaultParameter();
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
			currentProcedureContext = ProcedureDeclaration.DeclarationType.UNBOUND;
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
		} else if (ctx.procedureDeclaration() != null) {
			node = visit(ctx.procedureDeclaration());
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
		ASTNode expr = null;
		if (ctx.expression() != null) {

			expr = visit(ctx.expression());
		}

		return new ReturnStatement(position(ctx.getStart()), (Expression) expr);
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

			return binaryExpression(position(ctx.getStart()), ctx.andOperator().getText(), ctx.left, ctx.right);
		} else if (ctx.orOperator() != null) {

			return binaryExpression(position(ctx.getStart()), ctx.orOperator().getText(), ctx.left, ctx.right);
		} else if (ctx.asOperator() != null) {
			return visitCastExpression(ctx);
		} else if (ctx.isOperator() != null) {
			return visitIsExpression(ctx);
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
		} else if (ctx.tupleLiteral() != null) {
			ArrayList<Expression> elements = new ArrayList<>();
			for (ExpressionContext eContext : ctx.tupleLiteral().expression()) {
				elements.add((Expression) visit(eContext));
			}
			TupleLiteral tuple = new TupleLiteral(position(ctx.getStart()), elements);
			// generate a new tuple type if necessary
			addTupleType(elements.size());
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
		String underscore = "";
		if (operator.equals("not")) {
			underscore = "_";
		}
		Expression self = (Expression) visit(expr);
		FunctionCall operatorCall =
		        new FunctionCall(position, new ResolvableIdentifier("operator" + underscore + operator),
		                new ArrayList<Expression>());
		return new MemberAccess(position, self, operatorCall);
	}

	private MemberAccess binaryExpression(Position position, String operator, ExpressionContext left,
	        ExpressionContext right) {
		String underscore = "";
		List<String> needsUnderscore = Arrays.asList("and", "or", "xor", "in");
		if (needsUnderscore.contains(operator)) {
			underscore = "_";
		}
		Expression self = (Expression) visit(left);
		FunctionCall operatorCall =
		        new FunctionCall(position, new ResolvableIdentifier("operator" + underscore + operator),
		                Arrays.asList((Expression) visit(right)));
		return new MemberAccess(position, self, operatorCall);
	}

	private CastExpression visitCastExpression(ExpressionContext ctx) {
		ResolvableIdentifier type = new ResolvableIdentifier(getText(ctx.ClassIdentifier()));
		checkTupleType(type);
		return new CastExpression(position(ctx.getStart()), (Expression) visit(ctx.expr), type);
	}

	private IsExpression visitIsExpression(ExpressionContext ctx) {
		ResolvableIdentifier type = new ResolvableIdentifier(getText(ctx.ClassIdentifier()));
		checkTupleType(type);
		return new IsExpression(position(ctx.getStart()), (Expression) visit(ctx.expr), type);
	}

	protected ASTNode aggregateResult(ASTNode aggregate, ASTNode nextResult) {
		return nextResult == null ? aggregate : nextResult;
	}

	protected void addTupleType(int n) {
		ClassDeclaration tupleType = TupleClassDeclaration.getNewInstance(n);
		// if a new type was generated, it has to be attached to the module
		if (tupleType != null) {
			currentBlocks.get(0).addDeclaration(tupleType);
		}
	}

	protected void checkTupleType(ResolvableIdentifier type) {
		String str = type.toString();
		if (str.startsWith("Tuple")) {
			String number = str.substring(5);
			try {
				addTupleType(Integer.parseInt(number));
			} catch (Exception e) {
				// if the rest is not a number, we don't need to create a tuple type
			}
		}
	}
}
