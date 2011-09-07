/* c-example.c: */

#include <yaml.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>

typedef struct {
  int id;
  double t;
  double m;
  double r[3];
  double v[3];
} body_t;

static void
read_until(yaml_parser_t *parser, yaml_event_type_t type) {
  yaml_event_t event;

  yaml_parser_parse(parser, &event);
  if (event.type == YAML_STREAM_END_EVENT) {
    fprintf(stderr, "Stream ended!\n");
    exit(0);
  }

  while (event.type != type) {
    yaml_event_delete(&event);
    yaml_parser_parse(parser, &event);
    if (event.type == YAML_STREAM_END_EVENT) {
      fprintf(stderr, "Stream ended!\n");
      exit(0);
    }
  }

  yaml_event_delete(&event);
}

static void
drop_node(yaml_parser_t *parser) {
  yaml_event_t event;

  yaml_parser_parse(parser, &event);

  switch (event.type) {
  case YAML_ALIAS_EVENT:
    /* Done. */
    break;

  case YAML_SCALAR_EVENT:
    /* Done. */
    break;

  case YAML_SEQUENCE_START_EVENT:
    read_until(parser, YAML_SEQUENCE_END_EVENT);
    break;

  case YAML_MAPPING_START_EVENT:
    read_until(parser, YAML_MAPPING_END_EVENT);
    break;

  default:
    fprintf(stderr, "Did not recognize node event type: %d!\n", event.type);
    assert(0);
  }

  yaml_event_delete(&event);
}

static double
read_double(yaml_parser_t *parser) {
  yaml_event_t event;
  double val;

  yaml_parser_parse(parser, &event);

  if (event.type == YAML_SCALAR_EVENT) {
    val = atof(event.data.scalar.value);
    yaml_event_delete(&event);
    return val;
  } else {
    fprintf(stderr, "Trying to read double from stream, but failed.\n");
    assert(0);
    return 0.0/0.0;
  }
}

static int
read_int(yaml_parser_t *parser) {
  yaml_event_t event;
  int val;

  yaml_parser_parse(parser, &event);

  if (event.type == YAML_SCALAR_EVENT) {
    val = atoi(event.data.scalar.value);
    yaml_event_delete(&event);
    return val;
  } else {
    fprintf(stderr, "Trying to read int from stream, but failed: event type = %d.\n", event.type);
    assert(0);
    return 0;
  }
}

static void
read_three_vector(yaml_parser_t *parser, double v[3]) {
  yaml_event_t event;

  yaml_parser_parse(parser, &event);

  if (event.type == YAML_SEQUENCE_START_EVENT) {
    v[0] = read_double(parser);
    v[1] = read_double(parser);
    v[2] = read_double(parser);
  } else {
    fprintf(stderr, "Trying to read three-vector, but failed!\n");
    assert(0);
  }

  read_until(parser, YAML_SEQUENCE_END_EVENT);

  yaml_event_delete(&event);
}

static int
event_value_string_matches(yaml_event_t *event, const char *str, size_t n) {
  return (event->data.scalar.length >= n) && (strncmp(event->data.scalar.value, str, n) == 0);
}

/* Reads and returns a single body from the YAML stream associated
   with the given parser. */
static body_t *
read_body(yaml_parser_t *parser) {
  yaml_event_t event;
  int read_id = 0;
  int read_t = 0;
  int read_m = 0;
  int read_r = 0;
  int read_v = 0;
  body_t *b = malloc(sizeof(body_t));

  assert(b);

  /* Read up to the first mapping of the next document. */
  read_until(parser, YAML_DOCUMENT_START_EVENT);
  read_until(parser, YAML_MAPPING_START_EVENT);

  /* Now extract values from the mapping. */
  yaml_parser_parse(parser, &event);

  while (!read_id || !read_t || !read_m || !read_r || !read_v) {
    switch (event.type) {
    case YAML_ALIAS_EVENT: 
      /* Ignore alias as mapping source, junk associated target. */
      drop_node(parser);
      break;

    case YAML_SEQUENCE_START_EVENT:
      /* Read to sequence end, drop associated target node. */
      read_until(parser, YAML_SEQUENCE_END_EVENT);
      drop_node(parser);
      break;

    case YAML_MAPPING_START_EVENT:
      /* Read to mapping end, drop associated target node. */
      read_until(parser, YAML_MAPPING_END_EVENT);
      drop_node(parser);
      break;

    case YAML_SCALAR_EVENT:
      /* We will have a scalar event for our names: "m", "r", "v", "id", etc. */
      assert(event.data.scalar.length > 0);
      switch (event.data.scalar.value[0]) {
      case 'm':
        if (event_value_string_matches(&event, "m", 1)) {
          b->m = read_double(parser);
          read_m = 1;
        } else {
          drop_node(parser);
        }
        break;
      case 'r':
        if (event_value_string_matches(&event, "r", 1)) {
          /* Position */
          read_three_vector(parser, b->r);
          read_r = 1;
        } else {
          drop_node(parser);
        }
        break;

      case 'v':
        if (event_value_string_matches(&event, "v", 1)) {
          /* Velocity. */
          read_three_vector(parser, b->v);
          read_v = 1;
        } else {
          drop_node(parser);
        }
        break;

      case 'i':
        if (event_value_string_matches(&event, "id", 2)) {
          b->id = read_int(parser);
          read_id = 1;
        } else {
          /* Ignore any other tags beginning with i. */
          drop_node(parser);
        }
        break;

      case 't':
        if (event_value_string_matches(&event, "t", 1)) {
          b->t = read_double(parser);
          read_t = 1;
        } else {
          drop_node(parser);
        }
        break;

      default:
        /* Drop the other node in the mapping. */
        drop_node(parser);
        break;
      }
      
    case YAML_MAPPING_END_EVENT:
      /* Ended before we finished inputting body. */
      fprintf(stderr, "Cannot construct body without all of (id, m, t, r, v) map elements!\n");
      assert(0);
    }

    yaml_event_delete(&event);
    yaml_parser_parse(parser, &event);
  }

  /* Now we have read our body, so parse to the end of the document, and stop. */
  read_until(parser, YAML_DOCUMENT_END_EVENT);
  
  yaml_event_delete(&event);

  return b;
}

/* Note that we may want to give more digits of precision than the
   default in %g, depending on the purpose of the output. */
static void 
write_body(body_t *b) {
  fprintf(stdout, "--- !!Particle\n");
  fprintf(stdout, "id: %d\n", b->id);
  fprintf(stdout, "m: %g\n", b->m);
  fprintf(stdout, "t: %g\n", b->t);
  fprintf(stdout, "r:\n  - %g\n  - %g\n  - %g\n", b->r[0], b->r[1], b->r[2]);
  fprintf(stdout, "v:\n  - %g\n  - %g\n  - %g\n", b->v[0], b->v[1], b->v[2]);
}

int main() {
  yaml_parser_t parser;
  body_t *b;

  yaml_parser_initialize(&parser);
  yaml_parser_set_input_file(&parser, stdin);

  do {
    b = read_body(&parser);
    fprintf(stderr, "Read body with id = %d, t = %g, m = %g, r = {%g, %g, %g}, v = {%g, %g, %g}.\n",
            b->id, b->t, b->m, b->r[0], b->r[1], b->r[2], b->v[0], b->v[1], b->v[2]);
    write_body(b);
    free(b);
  } while(1);

  return 0;
}
