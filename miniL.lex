/* Include Stuff */
%{
  #include "miniL-parser.h"  
  int currLine = 1, currPos = 0;
%}

/* Define Patterns */
DIGIT [0-9]
LETTER [a-zA-Z]
NLU [0-9a-zA-Z_]          
NUMBER_LETTER [0-9a-zA-Z]
TAB [\t ]
NEWLINE [\n]

/* NLU = NUMBER_LETTER_UNDERSCORE */
/* Define Rules */
%%
"function"     return FUNCTION; currPos += yyleng;
"beginparams"  return BEGIN_PARAMS; currPos += yyleng;
"endparams"    return END_PARAMS;  currPos += yyleng;
"beginlocals"  return BEGIN_LOCALS; currPos += yyleng;
"endlocals"    return END_LOCALS; currPos += yyleng;
"beginbody"    return BEGIN_BODY; currPos += yyleng;
"endbody"      return END_BODY; currPos += yyleng;
"integer"      return INTEGER; currPos += yyleng;
"array"        return ARRAY; currPos += yyleng;
"of"           return OF; currPos += yyleng;
"if"           return IF; currPos += yyleng;
"then"         return THEN; currPos += yyleng;
"endif"        return ENDIF; currPos += yyleng;
"else"         return ELSE; currPos += yyleng;
"of"	       return OF; currPos += yyleng;
"while"        return WHILE; currPos += yyleng;
"do"           return DO; currPos += yyleng;
"beginloop"    return BEGINLOOP; currPos += yyleng;
"endloop"      return ENDLOOP; currPos += yyleng;
"enum"         return ENUM; currPos += yyleng;
"continue"     return CONTINUE; currPos += yyleng;
"read"         return READ; currPos += yyleng;
"write"        return WRITE; currPos += yyleng;
"and"          return AND; currPos += yyleng;
"or"           return OR; currPos += yyleng;
"not"          return NOT; currPos += yyleng;
"true"         return TRUE; currPos += yyleng;
"false"        return FALSE; currPos += yyleng;
"return"       return RETURN; currPos += yyleng;

"-"       return SUB; currPos += yyleng;
"+"       return ADD; currPos += yyleng;
"*"       return MULT; currPos += yyleng;
"/"       return DIV; currPos += yyleng;
"%"       return MOD; currPos += yyleng;

"=="      return EQ; currPos += yyleng;
"<>"      return NEQ; currPos += yyleng;
"<"       return LT; currPos += yyleng;
">"       return GT; currPos += yyleng;
"<="      return LTE; currPos += yyleng;
">="      return GTE; currPos += yyleng;

";"       return SEMICOLON; currPos += yyleng;
":"       return COLON; currPos += yyleng;
","       return COMMA; currPos += yyleng;
"("       return L_PAREN; currPos += yyleng;
")"       return R_PAREN; currPos += yyleng;
"["       return L_SQUARE_BRACKET; currPos += yyleng;
"]"       return R_SQUARE_BRACKET; currPos += yyleng;
":="      return ASSIGN; currPos += yyleng;

{LETTER}({NLU}*{NUMBER_LETTER}+)? { yylval.identifier = yytext; return IDENT; currPos += yyleng; }

{DIGIT}+ { yylval.number = atoi(yytext); return NUMBER; currPos += yyleng;}

({DIGIT}+{NLU}+)|("_"{NLU}+) {
        printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter.\n", currLine, currPos, yytext);
        exit(0);
}

{LETTER}({NLU}*{NUMBER_LETTER}+)?"_" {
        printf("Error at line %d, column %d: identifier \"%s\" cannot end with an underscore.\n", currLine, currPos, yytext);
        exit(0);
}

"##".*{NEWLINE} { currPos = 0; currLine++; }

{TAB}+   {currPos += yyleng;}
{NEWLINE}+      {currLine ++; currPos = 0;}

%%

int main(int argc, char** argv) {
  if (argc >= 2) {
	yyin = fopen(argv[1], "r");
	if (yyin == NULL) {
      		yyin = stdin;
    	}
  }
  else {
    yyin = stdin;
  }

  yyparse();
  
  return 0;
}
