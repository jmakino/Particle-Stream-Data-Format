#ifndef __PSDF_BODY_H__
#define __PSDF_BODY_H__ 1

#include<yaml.h>

/* A basic body type. */
typedef struct {
  int id;
  double t;
  double m;
  double r[3];
  double v[3];
} body_t;

/* Reads from the YAML stream of the given parser, ignoring the
   output, until a particular YAML event occurs. */
void
read_until(yaml_parser_t *parser, yaml_event_type_t type);

/* Drops the current node (an alias, a scalar, a sequence, or a
   mapping) from the YAML stream of the given parser. */
void
drop_node(yaml_parser_t *parser);

/* Read a double from the given parser. */
double
read_double(yaml_parser_t *parser);

/* Reads an int from the YAML stream of the given parser. */
int
read_int(yaml_parser_t *parser);

/* Reads a three-vector from the given YAML stream (a sequence with
   three double-precision components), and stores the result in v. */
void
read_three_vector(yaml_parser_t *parser, double v[3]);

/* Reads a body from the next YAML document that occurs in the stream
   represented by parser, returning a pointer to a corresponding
   body_t.  The returned pointer points to freshly allocated memory
   (as with malloc), and it is the responsibility of the receiver to
   deallocate it (with free) when it is no longer needed. */
body_t *
read_body(yaml_parser_t *parser);

/* Writes a body to the given file stream, in PSDF format. */
void
write_body(FILE *file, body_t *b);


#endif /* __PSDF_BODY_H__ */
