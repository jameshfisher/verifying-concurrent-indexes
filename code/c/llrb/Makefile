analyze.o: analyze.c analyze.h Node.h
	gcc -c analyze.c

main.o: main.c analyze.h Node.h
	gcc -c main.c

main: main.o analyze.o Node.o
	gcc -o main main.o analyze.o Node.o