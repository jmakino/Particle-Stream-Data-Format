/* input_output.c: Example of PSDF parsing and printing for a very basic
   body structure in C. 

   Author: Will M. Farr <w-farr@northwestern.edu>

   To compile on my machine:

   gcc -o c-example c-example.c -I/path/to/yaml.h/include -L/path/to/libyaml -lyaml

   The program reads a PSDF stream from stdin, reports on the bodies
   it has read to stderr, and writes the corresponding PSDF stream on
   stdout.  Note that elements of the particle mapping that do not
   correspond to the id, t, m, r, v, tags that are used are ignored.
   The program will quit with an error if the input stream contains
   bodies that do not contain at least one each of the id, t, m, r, v
   tags.  Generalization to your preferred body structure should be
   easy.

 */

#include <yaml.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include "psdf-body.h"

int main() {
  yaml_parser_t parser;
  body_t *b;

  yaml_parser_initialize(&parser);
  yaml_parser_set_input_file(&parser, stdin);

  do {
    b = read_body(&parser);
    fprintf(stderr, "Read body with id = %d, t = %g, m = %g, r = {%g, %g, %g}, v = {%g, %g, %g}.\n",
            b->id, b->t, b->m, b->r[0], b->r[1], b->r[2], b->v[0], b->v[1], b->v[2]);
    write_body(stdout, b);
    free(b);
  } while(1);

  return 0;
}
