// psdf-body-fint.c
// Fortran interface of psdf-body.c
//  Time-stamp: <11/12/18 00:43:53 makino>
//   
//   Author: Jun Makino 

#include <yaml.h>
#include <assert.h>
#include <string.h>
#include <stdio.h>
#include "psdf-body.h"

static yaml_parser_t parser;

int init_psdf_parser__()
{
  yaml_parser_initialize(&parser);
  yaml_parser_set_input_file(&parser, stdin);
  return 0;
}
int final_psdf_parser__()
{
    return 0;
}

int read_body__(int * id, double * t, double * m,
	      double r[3], double v[3])
{
    body_t *b;
    b = read_body(&parser);
    *id = b->id;
    *t = b->t;
    *m = b->m;
    int k;
    for(k=0;k<3;k++)r[k] = b->r[k];
    for(k=0;k<3;k++)v[k] = b->v[k];
    return 0;
}

int write_body__(int * id, double * t, double * m,
	       double r[3], double v[3])
{
    body_t b;
    b.id = *id;
    b.t = *t;
    b.m = *m;
    int k;
    for(k=0;k<3;k++)b.r[k] = r[k];
    for(k=0;k<3;k++)b.v[k] = v[k];
    write_body(stdout, &b);
    return 0;
}
