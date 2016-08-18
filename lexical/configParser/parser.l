%{
	/* Header file included */
	#include <stdio.h>
	#include <parser.h>
%}

%%
 /* Keywords */;
"db_type" 				return TYPE;
"db_name"				return NAME;
"db_table_prefix"		return TABLE_PREFIX;
"db_port"				return PORT;

 /* Colon as seperator */;
":"						return COLON;

 /* Indetifier */;
[a-zA-Z]+[_a-zA-Z0-9]*	return IDENTIFIER;

 /* Integer */;
[1-9]+[0-9]*			return INTEGER;
[ \t\n]					;

 /* Unexpected indetifier */;
.						printf("Unexpected indetifier!\n");
%%

int yywrap(void)
{
	return 1;
}