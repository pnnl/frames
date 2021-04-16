/*______________________________________________________________________________

   Date:       February 2001
   Programmer: Karl Castleton, Mitch Pelton
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef fcsvH
#define fcsvH

#include "robust.h"

const int READ=0;
const int WRITE=1;
const int APPEND=2;

class fcsv
{
  int  pos,len,mde;
  char line[MAXSTRING],del;
  FILE *fptr;
  char next();
  void find(int *start,int *size);
  public:
  bool newline;
  bool wasQuoted;
  fcsv();
  int open(char *name,int mode,char delim=',');
  int rewind();
  void close();
  void flush();
  int eof();
  int eol();
  void read(int *val);
  void read(float *val);
  void read(double *val);
  void read(char *val);
  void readchar(char *val);
  void readln(char *val);
  void readln();

  void write(int val,char *format="%d");
  void write(long int val,char *format="%d");
  void write(float val,char *format="%.10g");
  void write(double val,char *format="%.10lg");
  void write(char *val);
  void write(char *val, bool quoted);
  void write(fpos_t pos, long int val);
  void writeln();

  fpos_t getfpos();
  void setfpos(fpos_t);
};

#endif
