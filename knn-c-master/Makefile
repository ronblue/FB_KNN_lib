#Sourced: http://www.cs.colby.edu/maxwell/courses/tutorials/maketutor/
CC = gcc
CFLAGS = -Wall -I.
# Ultra debug mode  -Q -v -da -g -O0
#from: https://gcc.gnu.org/bugs/segfault.html
all: test

knn: knn.c
	$(CC) knn.c terminal_user_input.c $(CFLAGS) -DNDEBUG -o knn

test: knn.c
	$(CC) terminal_user_input.c tests.c $(CFLAGS) -Wextra -o test_knn
	test_knn.exe | contrib/greenest
