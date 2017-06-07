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

 lexer grammar lex;

@lexer::header {
    import java.util.Stack;
    import java.util.LinkedList;
    import java.util.Queue;
}

@lexer::members {
// we need a stack to store the indentation levels
Stack<Integer> indentationStack = new Stack<Integer>();
// the queue provides us the possibility to add more than one token per lexer rule
Queue<Token> tokenQueue = new LinkedList<Token>();
// we have to count opening an closing brackets for implicit line skips
int nesting = 0;

// we have to store the 'imaginary tokens' because they only appear in the parser class
public static final int Indent = MontyParser.Indent;
public static final int Dedent = MontyParser.Dedent;

// initialize the indentation stack with level 0 indent
{
    indentationStack.push(0);
}

// add every token to the queue
@Override
public void emit(Token token) {
    _token = token;
    tokenQueue.offer(token);
}

// return tokens from the queue until it is empty, then return EOF
@Override
public Token nextToken() {
    super.nextToken();
    if(tokenQueue.peek() == null)
        return new CommonToken(Token.EOF);
    return tokenQueue.poll();
}
}

// forward declarations of 'imaginary tokens'
tokens { Indent, Dedent }

// line skip with \
ExplicitLineSkip : '\\' ('\u000C')? '\r'? '\n' -> skip;
// line skip within brackets
// ImplicitLineSkip : ('\u000C')? '\r'? '\n' {nesting>0}? -> skip;

// check indentation, treat multiple newlines as one
EndOfLine :
        //_________line breaks____  _whitespaces__  __comments__     ___last line break______   ___indent__
        ((('\u000C')?('\r')? '\n' ) | '\t' | ' ' | ('//' (~'\n')*))* (('\u000C')?('\r')? '\n' ) (' ' | '\t' )*

        {
            if(nesting>0)
            {
                skip();
                return;
            }

            String tokenText = getText();
            int indentLength = tokenText.length()-(tokenText.lastIndexOf("\n")+1);
            // generate a newline token
            tokenQueue.offer(new CommonToken(EndOfLine, "\n"));

            // if the indentation is deeper than the last one, add an Indent token
            if(indentLength > indentationStack.peek())
            {
                indentationStack.push(indentLength);
                tokenQueue.offer(new CommonToken(Indent, "Indent"));
            }
            // if the indentation is less deep than the last line,
            // add as many Dedent tokens as required
            else if(indentLength < indentationStack.peek())
            {
                while(indentationStack.peek() > indentLength)
                {
                    indentationStack.pop();
                    tokenQueue.offer(new CommonToken(Dedent, "Dedent"));
                }
                // if the new indentation does not match any outer level,
                // we have a problem:
                if(indentationStack.peek() != indentLength)
                {
                    throw new RuntimeException("Monty says: Your indentation sucks!");
                }
            }

            skip(); // do not automatically emit an EndOfLine token,
                    // since we already did it manually to ensure
                    // that it appears before the Indent or Dedent tokens
        };

Lparenthesis
 : '(' {nesting++;} ;
Rparenthesis
 : ')' {nesting--;} ;
Lbracket
 : '[' {nesting++;} ;
Rbracket
 : ']' {nesting--;} ;
Lcurly
 : '{' {nesting++;} ;
Rcurly
 : '}' {nesting--;} ;


/* See http://www.antlr.org/wiki/display/ANTLR4/Grammar+Lexicon */
fragment IdentifierStart
  : Letter
  | '\u00C0' .. '\u00D6'
  | '\u00D8' .. '\u00F6'
  | '\u00F8' .. '\u02FF'
  | '\u0370' .. '\u037D'
  | '\u037F' .. '\u1FFF'
  | '\u200C' .. '\u200D'
  | '\u2070' .. '\u218F'
  | '\u2C00' .. '\u2FEF'
  | '\u3001' .. '\uD7FF'
  | '\uF900' .. '\uFDCF'
  | '\uFDF0' .. '\uFFFD'
  ;

fragment IdentifierCharacter
  : IdentifierStart
  | Digit
  | '_'
  | '\u00B7'
  | '\u0300' .. '\u036F'
  | '\u203F' .. '\u2040'
  ;

BooleanLiteral
  : ('true'  | 'false')
  ;

AbstractKeyword
  : 'abstract'
  ;

/* Unicode identifiers. */

ClassIdentifier
  : UppercaseLetter (LowercaseLetter | UppercaseLetter | Digit)* LowercaseLetter (LowercaseLetter | UppercaseLetter | Digit)*
  ;

Identifier
  : '_'* LowercaseLetter (LowercaseLetter | UppercaseLetter | Digit | '_')*
  | '_'+ (LowercaseLetter | UppercaseLetter | Digit | '_')+
  ;

ConstantIdentifier
  : '_'* UppercaseLetter (UppercaseLetter | Digit | '_')*
  ;

UppercaseLetter
  : [A-Z]
  ;

LowercaseLetter
  : [a-z]
  ;

IntegerLiteral
  : (Digit | Letter)+ Exponent? Base?
  ;

RealLiteral
  : Digit+ '.' Digit+ Exponent?
  ;

Digit
  : [0-9]
  ;

Letter
  : [a-zA-Z]
  ;

operation
 : unaryOperation
 | binaryOperation
 ;

unaryOperation
 : 'not'
 | '-'
 ;

binaryOperation
 : '+'
 | '-'
 | '*'
 | '/'
 | '%'
 | '^'

 | '='
 | '!='
 | '<'
 | '>'
 | '<='
 | '>='

 | 'and'
 | 'or'
 | 'xor'

 | Lbracket Rbracket

 | 'in'
 ;

fragment Base
  : '_' Digit+
  ;

fragment Exponent
  : 'e' ('+' | '-')? Digit+
  ;

CharacterLiteral
  : '\'' (StringEscapeSequence | ~('\\' | '\'' | '\n' | '\r')) '\''
  ;

StringLiteral
  : 'raw'? '"' (StringEscapeSequence | ~('\\' | '"' | '\n' | '\r'))* '"'
  ;

fragment StringEscapeSequence
  : '\\' ('t' | 'b' | 'n' | 'r' | 'f' | '\'' | '\"' | '\\' 
  | ('u' HexDigit HexDigit HexDigit HexDigit ))
  ;

fragment HexDigit
  : [0-9A-Fa-f]
  ;

/* Toss out whitespaces. */
Whitespaces
  : ' '+ -> skip
  ;

/* Treat one or more tabs as a token representing a sequence of indentation
 * characters.
 */
SpaceChars
  : '\t'+ -> skip
  ;

Semicolon : ';' ;
