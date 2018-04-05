/********************************************************************************
*
* File: spl.lex
* The SPL scanner
*
********************************************************************************/

package edu.uta.spl;

import java_cup.runtime.Symbol;

%%
%class SplLex
%public
%line
%column
%cup

DIGIT=[0-9]
ID=[a-zA-Z][a-zA-Z0-9_]*

%{

  private Symbol symbol ( int type ) {
    return new Symbol(type, yyline+1, yycolumn+1);
  }

  private Symbol symbol ( int type, Object value ) {
    return new Symbol(type, yyline+1, yycolumn+1, value);
  }

  public void lexical_error ( String message, Object value) {
    System.err.println("*** Lexical Error: " + message + " (line: " + (yyline+1)
                       + ", position: " + (yycolumn+1) + ", value: " + value + ")");
    System.exit(1);
  }
%}

%%

"var"					{ return symbol(sym.VAR); }
"int"					{ return symbol(sym.INT); }
"bool"					{ return symbol(sym.BOOLEAN); }
"int"					{ return symbol(sym.INT); }
"float"					{ return symbol(sym.FLOAT); }
"string"				{ return symbol(sym.STRING); }
"to"					{ return symbol(sym.TO); }
"by"					{ return symbol(sym.BY); }
"def"					{ return symbol(sym.DEF); }
"type"					{ return symbol(sym.TYPE); }
"while"					{ return symbol(sym.WHILE); }
"for"					{ return symbol(sym.FOR); }
"loop"					{ return symbol(sym.LOOP); }
"to"					{ return symbol(sym.TO); }
"if"					{ return symbol(sym.IF); }
"else"					{ return symbol(sym.ELSE); }
"true"					{ return symbol(sym.TRUE); }
"false"					{ return symbol(sym.FALSE); }
"array"         		{ return symbol(sym.ARRAY); }
"print"					{ return symbol(sym.PRINT); }
"return"				{ return symbol(sym.RETURN); }
"exit"					{ return symbol(sym.EXIT); }
"read"					{ return symbol(sym.READ); }
"+"						{ return symbol(sym.PLUS); }
"-"						{ return symbol(sym.MINUS); }
"*"						{ return symbol(sym.TIMES); }
"/"						{ return symbol(sym.DIV); }
"%"						{ return symbol(sym.MOD); }
"."						{ return symbol(sym.DOT); }
"("						{ return new Symbol(sym.LP); }
")"						{ return new Symbol(sym.RP); }
"{"						{ return new Symbol(sym.LB); }
"}"						{ return new Symbol(sym.RB); }
"#"						{ return new Symbol(sym.SHARP); }
"&&"					{ return new Symbol(sym.AND); }
";"						{ return new Symbol(sym.SEMI); }
[ \t\r\n\f]				{ /* ignore white spaces. */ }
"/*".*"*/"				{ /* ignore comments. */ }
"="						{ return new Symbol(sym.EQUAL); }
"!"     		      	{ return new Symbol(sym.NOT); }
"||"					{ return new Symbol(sym.OR); }
"=="					{ return new Symbol(sym.EQ); }
"<"						{ return new Symbol(sym.LT); }
"<="					{ return new Symbol(sym.LEQ); }
">"						{ return new Symbol(sym.GT); }
">="					{ return new Symbol(sym.GEQ); }
"<>"					{ return new Symbol(sym.NEQ); }
":"						{ return new Symbol(sym.COLON); }
"["						{ return new Symbol(sym.LSB); }
"]"						{ return new Symbol(sym.RSB); }
","						{ return new Symbol(sym.COMMA); }
\"[^\"]*\"				{ return new Symbol(sym.STRING_LITERAL, yytext().substring(1,yytext().length()-1)); }
{DIGIT}+        		{ return new Symbol(sym.INTEGER_LITERAL, Integer.parseInt(yytext())); }
{DIGIT}+"."{DIGIT}+  	{ return new Symbol(sym.FLOAT_LITERAL, Float.parseFloat(yytext())); }
{ID}		            { return new Symbol(sym.ID, yytext()); }
.           		    { lexical_error("Illegal character", yytext()); }
