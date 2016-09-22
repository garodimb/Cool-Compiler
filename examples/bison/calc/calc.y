/* Simple infix calculator parser */

%{
	#include <stdio.h>
	#include <math.h>
	#include <ctype.h>
	int yylex(void);
	void yyerror(char const *);	
%}

%define api.value.type{double}
%token NUM
%left '+' '-'
%left '*' '/'
%precedence NEG /* Unnary minus operator */
%right '^'

%%

/* Grammar rules for infix calculator */

input:
	%empty
	| input line
	;

line:
	'\n'
	| exp '\n' { printf("Ans: %.10g\n",$1); }
	;

exp:
	NUM
	| exp '+' exp { $$ = $1 + $3; }
	| exp '-' exp { $$ = $1 - $3; }
	| exp '*' exp { $$ = $1 * $3; }
	| exp '/' exp { $$ = $1 / $3; }
	| exp '^' exp { $$ = pow($1,$3); }
	| '-' exp %prec NEG { $$ = -$2; }
	| '(' exp ')'	{ $$ = $2; }
	;

%%

int yylex (void)
{  int c;

  /* Skip white space.  */
  while ((c = getchar ()) == ' ' || c == '\t')
    continue;
  /* Process numbers.  */
  if (c == '.' || isdigit (c))
    {
      ungetc (c, stdin);
      scanf ("%lf", &yylval);
      return NUM;
    }
  /* Return end-of-input.  */
  if (c == EOF)
    return 0;
  /* Return a single char.  */
  return c;
}

void yyerror (char const *s)
{
  fprintf (stderr, "%s\n", s);
}

int main()
 {
 	yyparse();
 }