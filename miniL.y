%{
#include <stdio.h>
#include <stdlib.h>
void yyerror(const char* s);
%}

%union{
  char* identifier;
  int number;
}

%error-verbose
%start prog

%token <identifier> IDENT
%token <number> NUMBER

%token FUNCTION
%token BEGIN_PARAMS
%token END_PARAMS
%token BEGIN_LOCALS
%token END_LOCALS
%token BEGIN_BODY
%token END_BODY
%token INTEGER
%token ARRAY
%token ENUM
%token OF
%token IF
%token THEN
%token ENDIF
%token ELSE
%token FOR
%token WHILE
%token DO
%token BEGINLOOP
%token ENDLOOP
%token CONTINUE
%token READ
%token WRITE
%left AND
%left OR
%right NOT
%token TRUE
%token FALSE
%token RETURN

%left SUB
%left ADD
%left MULT
%left DIV
%left MOD

%left EQ
%left NEQ
%left LT
%left GT
%left LTE
%left GTE

%token SEMICOLON
%token COLON
%token COMMA
%token L_PAREN
%token R_PAREN
%token L_SQUARE_BRACKET
%token R_SQUARE_BRACKET
%right ASSIGN


%%  /*  Grammar rules and actions follow  */

prog: funcs {printf("prog -> funcs\n");}	
;

funcs: func funcs {printf("funcs -> func funcs\n");}
      |%empty {printf("funcs -> epsilon\n");}
;

func:	FUNCTION IDENT SEMICOLON BEGIN_PARAMS declaration_cycle END_PARAMS BEGIN_LOCALS declaration_cycle END_LOCALS BEGIN_BODY statement_cycle END_BODY
{printf("func -> FUNCTION IDENT SEMICOLON BEGIN_PARAMS declaration_cycle END_PARAMS BEGIN_LOCALS declaration_cycle END_LOCALS BEGIN_BODY statement_cycle END_BODY\n");}
;

declaration_cycle: declaration SEMICOLON declaration_cycle {printf("declaration_cycle -> declaration SEMICOLON declaration_cycle\n");}
		  |%empty {printf("declaration_cycle -> epsilon\n");}
;

declaration: identifier_cycle COLON ENUM L_PAREN identifier_cycle R_PAREN {printf("declaration -> identifier_cycle COLON ENUM L_PAREN identifier_cycle R_PAREN\n");} 
       |identifier_cycle COLON INTEGER {printf("declaration -> identifier_cycle COLON INTEGER\n");}
       |identifier_cycle COLON ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER {printf("declaration -> identifier_cycle COLON ARRAY L_SQUARE_BRACKET NUMBER %d R_SQUARE_BRACKET OF INTEGER\n", $5);}
;

identifier_cycle: IDENT {printf("identifier_cycle -> IDENT %s \n", $1);}
                 |IDENT COMMA identifier_cycle {printf("identifier_cycle -> IDENT %s COMMA identifier_cycle\n", $1);}

statement_cycle: statement SEMICOLON {printf("statement_cycle -> statement SEMICOLON\n");}
                |statement SEMICOLON statement_cycle {printf("statement_cycle -> statement SEMICOLON statement_cycle\n");}
;

statement: var ASSIGN expression {printf("statement -> var ASSIGN expression\n");}
         | IF boolexp THEN statement_cycle else ENDIF {printf("statement -> IF boolexp THEN statement_cycle else ENDIF\n");}		 
         | WHILE boolexp BEGINLOOP statement_cycle ENDLOOP {printf("statement -> WHILE boolexp BEGINLOOP statement_cycle ENDLOOP\n");}
         | DO BEGINLOOP statement_cycle ENDLOOP WHILE boolexp {printf("statement -> DO BEGINLOOP statement_cycle ENDLOOP WHILE boolexp\n");}
         | READ var_cycle {printf("statement -> READ var_cycle\n");}
         | WRITE var_cycle {printf("statement -> WRITE var_cycle\n");}
         | CONTINUE {printf("statement -> CONTINUE\n");}
         | RETURN expression {printf("statement -> RETURN expression\n");}
;

else: ELSE statement_cycle {printf("else -> ELSE statement_cycle\n");}
     |%empty {printf("else -> epsilon\n");}
;

var_cycle: var COMMA var_cycle {printf("var_cycle -> var COMMA var_cycle\n");}
	  |var {printf("var_cycle -> var\n");}
;

boolexp: relation_and_exp {printf("boolexp -> relation_and_exp\n");}
       | relation_and_exp OR boolexp {printf("boolexp -> relation_and_exp OR boolexp\n");}
;

relation_and_exp: relation_exp {printf("relation_and_exp -> relation_exp\n");}
      		 |relation_exp AND relation_and_exp {printf("relation_and_exp -> relation_exp AND relation_and_exp\n");}
;

relation_exp: NOT relation_exp_cases {printf("relation_exp -> NOT relation_exp_cases\n");}
             |relation_exp_cases {printf("relation_exp -> relation_exp_cases\n");}

;

relation_exp_cases: expression comp expression {printf("relation_exp -> expression comp expression\n");}
                   |TRUE {printf("relation_exp -> TRUE\n");}
                   |FALSE {printf("relation_exp -> FALSE\n");}
                   |L_PAREN boolexp R_PAREN {printf("relation_exp -> L_PAREN boolexp R_PAREN\n");}
;

comp: EQ {printf("comp -> EQ\n");}
     |NEQ {printf("comp -> NEQ\n");}
     |LT {printf("comp -> LT\n");}
     |GT {printf("comp -> GT\n");}
     |LTE {printf("comp -> LTE\n");}
     |GTE {printf("comp -> GTE\n");}
;

expression: mult_exp {printf("expression -> mult_exp\n");}
           |mult_exp ADD expression {printf("expression -> mult_exp ADD expression\n");}
           |mult_exp SUB expression {printf("expression -> mult_exp SUB expression\n");}
;

mult_exp: term {printf("mult_exp -> term\n");}
         |term MULT mult_exp {printf("mult_exp -> term MULT mult_exp\n");}
         |term DIV mult_exp {printf("mult_exp -> term DIV mult_exp\n");}
         |term MOD mult_exp {printf("mult_exp -> term MOD mult_exp\n");}
;

term: SUB var {printf("term -> SUB var\n");}
     |SUB NUMBER {printf("term -> SUB NUMBER %d\n", $2);}
     |SUB L_PAREN expression R_PAREN {printf("term -> SUB L_PAREN expression R_PAREN\n");}
     |var {printf("term -> var\n");}
     |NUMBER {printf("term -> NUMBER %d\n", $1);}
     |L_PAREN expression R_PAREN {printf("term -> L_PAREN expression R_PAREN\n");}
     |IDENT L_PAREN expression_cycle R_PAREN {printf("term -> IDENT %s L_PAREN expression_cycle R_PAREN\n", $1);}
;

expression_cycle: expression COMMA expression_cycle {printf("expression_cycle -> expression COMMA expression_cycle\n");}
		 |expression {printf("expression_cycle -> expression\n");}
;

var: IDENT {printf("var -> IDENT %s\n", $1);}
    |IDENT L_SQUARE_BRACKET expression R_SQUARE_BRACKET {printf("var -> IDENT %s L_SQUARE_BRACKET expression R_SQUARE_BRACKET\n", $1);}
;

%%

		 
void yyerror(const char* s) {
  extern int currLine;
  extern char* yytext;

  printf("%s at line %d before \"%s\"\n", s, currLine, yytext);
  exit(0);
}
