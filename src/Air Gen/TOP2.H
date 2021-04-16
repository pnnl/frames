#ifndef TOP2_H
#define TOP2_H

#include <stdio.h>

int tconv(int v);
typedef float tgroup[16];

typedef struct
  {
    int top,chanl;
    tgroup t[13];
    FILE *f;
  } TOPFILE;

TOPFILE *opentop(char *fn,char *m);
void closetop(TOPFILE *tf);
void readtop(TOPFILE *tf);
void writetop(TOPFILE *tf);
#endif