#ifndef JFD2_H
#define JFD2_H

#include <stdio.h>

typedef struct
  {
    int wil;
    char format[30];
  }jgroup1;

typedef struct
  {
    float rf[7][6][16];
  }jgroup2;

typedef struct
  {
    float calms[7];
  }jgroup3;


typedef struct
  {
    FILE *f;
    jgroup1 j1;
    jgroup2 j2;
    jgroup3 j3;
  }JFDFILE;

JFDFILE *openjfd(char *s,char *m);
void closejfd(JFDFILE *j);
int readjfd(JFDFILE *jf);
int writejfd(JFDFILE *jf);

#endif
