#include "switch.h"
#ifndef MET2_H
#define MET2_H 0
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "memhand.h"

typedef struct
  {
    char sname[80];
  }mgroup1;

typedef struct
  {
    float hmstab,hmunst,dprecip,tsnum,annrain;
  }mgroup2;

typedef struct
  {
    float u[6];
  }mgroup3;

typedef struct
  {
    float anhgt,zos;
  }mgroup4;

typedef struct
  {
    char file[60];
  }mgroup5;

typedef struct
  {
    FILE *f;
    mgroup1 m1;
    mgroup2 m2;
    mgroup3 m3;
    mgroup4 m4;
    mgroup5 m5;
  }METFILE;

METFILE *openmet(char *s,char *mo);
int readmet(METFILE *m);
int writemet(METFILE *m);
void closemet(METFILE *m);
void namemet(METFILE *m,char *s);

#endif
