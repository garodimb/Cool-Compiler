/* Simple lexical analyzer for config.in */

#include <stdio.h>
#include <parser.h>

extern int yylex(void);
extern int yylineno;
extern char* yytext;

int main(int argc, char const *argv[])
{
	int ntoken, vtoken;
	char *names[] = {NULL,"db_name","db_time","db_table_prefix","db_port"};
	ntoken = yylex();
	
	while(ntoken){
		if(yylex()!=COLON){
			printf("[Line: %d] Expected ':', found %s\n",yylineno,yytext);
		}

		vtoken = yylex();
		switch(ntoken){
			case TYPE:
			case NAME:
			case TABLE_PREFIX:
						if(vtoken!=IDENTIFIER){
							printf("[Line: %d] Expected IDENTIFIER, found %s\n",yylineno,yytext);
							return 1;
						}
						printf("%s value assigned to %s\n",yytext,names[ntoken]);
						break;

			case PORT:
						if(vtoken!=INTEGER){
							printf("[Line: %d] Expected INTEGER, found %s\n",yylineno,yytext);
							return 1;
						}
						break;
						printf("%s value assigned to %s\n",yytext,names[ntoken]);
			default:
						printf("[Line: %d] Unexpected identifier %s\n", yylineno,yytext);
		}
		ntoken = yylex();
	}
	return 0;
}