#include <stdio.h>
#include <ctype.h>
#include <stdbool.h>
#include "file_input_output.h"

void write_lines(FILE *file_ptr) {

  fprintf(file_ptr, "%d", 10);

// probably you want to change this to write multiple lines

  fprintf(file_ptr, "%s", "this is my test file");
}

void read_lines_to_terminal(FILE *file_ptr) {
  my_string string;
  int my_int;

  while (!feof(file_ptr)) {
    fscanf(file_ptr, "%d", &my_int);
    fscanf(file_ptr, "%[^\n]\n", string.str);

    printf("Line read is: %s, Integer is: %d\n", string.str, my_int);
  }
}

void main()
{
  FILE *file_ptr;

// write to the file
  if ((file_ptr = fopen("mytestfile.dat", "w")) == NULL)
    printf("File could not be opened");
  else
    write_lines(file_ptr);

  fclose(file_ptr);

// open the file and read from it

  if ((file_ptr = fopen("mytestfile.dat", "r+")) == NULL)
    printf("File could not be opened");
  else
    read_lines_to_terminal(file_ptr);

 fclose(file_ptr);
}
