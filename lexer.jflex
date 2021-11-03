package cup.example;
import java_cup.runtime.ComplexSymbolFactory;
import java_cup.runtime.ComplexSymbolFactory.Location;
import java_cup.runtime.Symbol;
import java.lang.*;
import java.io.InputStreamReader;

%%

%class Lexer
%implements sym
%public
%unicode
%line
%column
%cup
%char
%{
	

    public Lexer(ComplexSymbolFactory sf, java.io.InputStream is){
		this(is);
        symbolFactory = sf;
    }
	public Lexer(ComplexSymbolFactory sf, java.io.Reader reader){
		this(reader);
        symbolFactory = sf;
    }
    
    private StringBuffer sb;
    private ComplexSymbolFactory symbolFactory;
    private int csline,cscolumn;

    public Symbol symbol(String name, int code){
		return symbolFactory.newSymbol(name, code,
						new Location(yyline+1,yycolumn+1, yychar), // -yylength()
						new Location(yyline+1,yycolumn+yylength(), yychar+yylength())
				);
    }
    public Symbol symbol(String name, int code, String lexem){
	return symbolFactory.newSymbol(name, code, 
						new Location(yyline+1, yycolumn +1, yychar), 
						new Location(yyline+1,yycolumn+yylength(), yychar+yylength()), lexem);
    }
    
    protected void emit_warning(String message){
    	System.out.println("scanner warning: " + message + " at : 2 "+ 
    			(yyline+1) + " " + (yycolumn+1) + " " + yychar);
    }
    
    protected void emit_error(String message){
    	System.out.println("scanner error: " + message + " at : 2" + 
    			(yyline+1) + " " + (yycolumn+1) + " " + yychar);
    }
%}

LineEnd 	= [\r\n]|\r\n
Character	= [^\r\n]
Whitespace 	= {LineEnd} | [ \t\f]
LineComment = "//" {Character}*{LineEnd} 

Identifier 	= [a-zA-Z][a-zA-Z0-9_]*
Num 		= [0-9]+
StringChar	= [^\r\n\"\\]
String		= "\""[^(\"|\\t|\\n)]*[\\\"]*"\""
FloatLiteral  = ({FLit1}|{FLit2}|{FLit3})
FLit1    = [0-9]+ \. [0-9]*
FLit2    = \. [0-9]+ 
FLit3    = [0-9]+ 

%eofval{
    return symbolFactory.newSymbol("EOF",sym.EOF);
%eofval}

%state CODESEG

%%  

<YYINITIAL> {

  {Whitespace} {                              } 
  
  /* Declares */
  "string"		{return symbolFactory.newSymbol("STRING",STRING);}
  "int"			{return symbolFactory.newSymbol("INT",INT);}
  "real"		{return symbolFactory.newSymbol("REAL",REAL);}
  
  /* Program structure */
  "program"		{return symbolFactory.newSymbol("PROGRAM", PROGRAM);}
  "endprogram"	{return symbolFactory.newSymbol("ENDPROGRAM", ENDPROGRAM);}
  "function"	{return symbolFactory.newSymbol("FUNCTION", FUNCTION);}
  "endfunction"	{return symbolFactory.newSymbol("ENDFUNCTION", ENDFUNCTION);}
  
  /* Statements */
  	/* A.Assignment Statement */
  ":="			{return symbolFactory.newSymbol("ASSIGNMENT", ASSIGNMENT);}
  
  	/* B.Control Statements */
  "if"			{return symbolFactory.newSymbol("IF", IF);}
  "else"		{return symbolFactory.newSymbol("ELSE", ELSE);}
  "endif"		{return symbolFactory.newSymbol("ENDIF", ENDIF);}
  "while"		{return symbolFactory.newSymbol("WHILE", WHILE);}
  "do"			{return symbolFactory.newSymbol("DO", DO);}
  "enddo"		{return symbolFactory.newSymbol("ENDDO", ENDDO);}
  "for"			{return symbolFactory.newSymbol("FOR", FOR);}
  "to"			{return symbolFactory.newSymbol("TO", TO);}
  "endfor"		{return symbolFactory.newSymbol("ENDFOR", ENDFOR);}
  "return"		{return symbolFactory.newSymbol("RETURN", RETURN);}

  
  /* Expressions */
  "+"          	{return symbolFactory.newSymbol("ADD", ADD);}
  "-"          	{return symbolFactory.newSymbol("SUB", SUB);}
  "*"			{return symbolFactory.newSymbol("MULT", MULT);}
  "/"			{return symbolFactory.newSymbol("DIV", DIV);}
  "**"			{return symbolFactory.newSymbol("POW", POW);}
  "mod"			{return symbolFactory.newSymbol("MOD", MOD);} 
  
  /* Operators */
  "and"			{return symbolFactory.newSymbol("AND", AND);}
  "or"			{return symbolFactory.newSymbol("OR", OR);}
  "not"			{return symbolFactory.newSymbol("NOT", NOT);}
  "="			{return symbolFactory.newSymbol("EQUAL", EQUAL);}
  "<>"			{return symbolFactory.newSymbol("NEQUAL", NEQUAL);}
  "<"			{return symbolFactory.newSymbol("LT", LT);}
  ">"			{return symbolFactory.newSymbol("GT", GT);}
  "<="			{return symbolFactory.newSymbol("LE", LE);}
  ">="			{return symbolFactory.newSymbol("GE", GE);}
                                                
  /* IO */                                          
  "read"		{return symbolFactory.newSymbol("READ", READ);}
  "clear"		{return symbolFactory.newSymbol("CLEAR", CLEAR);}
  "draw"		{return symbolFactory.newSymbol("DRAW", DRAW);}
  "write"		{return symbolFactory.newSymbol("WRITE", WRITE);}
  "set color"	{return symbolFactory.newSymbol("SETCOLOR", SETCOLOR);}
  "set line"	{return symbolFactory.newSymbol("SETLINE", SETLINE);}
                                                
  /* OTHER SEPARATORS */                            
  "("			{return symbolFactory.newSymbol("LPARENT", LPARENT);}
  ")"			{return symbolFactory.newSymbol("RPARENT", RPARENT);}
  ","			{return symbolFactory.newSymbol("COMMA", COMMA);}
  ";"			{return symbolFactory.newSymbol("SEMICOLON", SEMICOLON);}
  /*should { } [ ] be included? */               
  
  {LineComment}	{						}
  {Identifier}	{return symbolFactory.newSymbol("IDENTIFIER", IDENTIFIER, yytext());}
  {String}		{return symbolFactory.newSymbol("STR", STR);}
  {Num}			{return symbolFactory.newSymbol("NUM", NUM, Integer.parseInt(yytext()));}
  {FloatLiteral} {return symbolFactory.newSymbol("FLOATING_POINT_LITERAL", FLOATING_POINT_LITERAL, new Float(yytext().substring(0,yylength()-1)));}   
  {Whitespace}	{ /* ignore */ }                       
}
// error fallback
.|\n          { emit_warning("Unrecognized character '" +yytext()+"' -- ignored"); }

