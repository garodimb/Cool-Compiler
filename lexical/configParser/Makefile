all: main

lex.yy.c:	parser.l
			lex -t parser.l > parser.c

main.o:		main.c parser.h
			gcc -c main.c -I.

parser.o:	parser.c parser.h
			gcc -c parser.c -I.

main: 		parser.o main.o parser.h
			gcc $(CFLAGS) parser.o main.o -o main -I.

clean:	
			rm -rf parser.c lex.yy.c *.o main