/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef gidH
#define gidH

#include "fcsv.h"

// structures for GID file
typedef struct __glyphrec
{
  char name[SMALLSTRING];
  int cnt;
} glyphrec;

typedef struct __paramrec
{
  bool quote;
  int cnt1;
  int cnt2;
  int cnt3;
  int cnt4;
  int cnt5;
  int cnt6;
  int ref;
  char name[SMALLSTRING];
  char uunit[SMALLSTRING];
  char punit[SMALLSTRING];
  char valu[LARGESTRING];
  char fmt[SMALLSTRING];
} paramrec;

//The following allows __element to point to the parent __param
typedef struct __parameter __param;

typedef struct __element
{
  int idx1;
  int idx2;
  int idx3;
  int idx4;
  int idx5;
  int idx6;
  int ref;
  char valu[LARGESTRING];
  struct __element *next;
  __param *parent;
}element;

typedef struct __parameter
{
  char name[SMALLSTRING];
  char uunit[SMALLSTRING];
  char punit[SMALLSTRING];
  element *entries;
  struct __parameter *next;
} parameter;

typedef struct __GIDFILE
{
  fcsv f;
  parameter *cache;
} GIDFILE;

// used with read and write routines
extern paramrec val;
extern glyphrec glyph;

char *getvalu(element *e, int c1=0, int c2=0, int c3=0, int c4=0, int c5=0, int c6=0);
int findidx(element *e, char *valu, int idx);

void copyparamrec(paramrec *d, paramrec *s);
int paramequal(paramrec *f, paramrec *s);
void Put_Value(GIDFILE *f, paramrec *p);
char *Get_Value(GIDFILE *f, paramrec *p);

int Get_Element_Ref(element *e, paramrec *p);
int Get_Element_Ref(parameter *pm, paramrec *p);

char *Get_Element_Value(element *e, paramrec *p);
char *Get_Element_Value(parameter *pm, paramrec *p);

element *Get_Element(GIDFILE *f, char *pname);
parameter *Get_Parameter(GIDFILE *f, char *pname);

GIDFILE *Open_GID(char *s);
void Close_GID(GIDFILE *f);
int readglyph(GIDFILE *f);
int writeglyph(GIDFILE *f);
paramrec *readparamrec(GIDFILE *f);
void writeparamrec(GIDFILE *f, paramrec *p);

int Load_GID(GIDFILE *f, int idx, char *model);
int Load_GIDSection(GIDFILE *f, char *model);

char *info(GIDFILE *f,char *s,int c1=0,int c2=0,int c3=0,int c4=0,int c5=0,int c6=0);
int readStr    (GIDFILE *f, char *pname, char **val, int idx1=0, int idx2=0,int idx3=0,int idx4=0,int idx5=0,int idx6=0);
int readFloat  (GIDFILE *f, char *pname, float *val, int idx1=0, int idx2=0,int idx3=0,int idx4=0,int idx5=0,int idx6=0);
int readInt    (GIDFILE *f, char *pname, int *val, int idx1=0, int idx2=0,int idx3=0,int idx4=0,int idx5=0,int idx6=0);
int readDouble (GIDFILE *f, char *pname, double *val, int idx1=0, int idx2=0,int idx3=0,int idx4=0,int idx5=0,int idx6=0);

int Copy_Update_GID(char *sname, char *dname, int count,
                    char **gnames, paramrec *p, paramrec *fui);

#endif
