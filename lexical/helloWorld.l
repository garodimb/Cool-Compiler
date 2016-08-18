
%%
[+]?[0-9]+		printf("Its' Number!\n");
"hello"			printf("This is hello\n");
"exit"|"quit"|"q"	exit(0);
.				printf("This is not valid identifier!\n");
%%