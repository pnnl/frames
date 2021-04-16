#include "switch.h"
#ifndef FILEIO_H
#define FILEIO_H 0


#ifdef MS_DOS
  int copy(char *d,char *s);
  int exist(char *s);
  char **FindFiles(char *search,int *i);
  char **FindPRMFiles(int *i);
  void FreeFiles(int i,char **names);
  void filenameparse(char *line,char *name,char *directory,char *drive);
  char *pathfile(char *s1,char *s2);
  char *drvpathfile(char *s1,char *s2,char *s3);
  int  changeto(char *filepath,char *drive);
  void current(char *filepath,char *drive);
#else
  int copy();
  int exist();
  char **FindFiles();
  char **FindPRMFiles();
  void filenameparse();
  char *pathfile();
  char *drvpathfile();
  int  changeto();
  void current();
#endif

#endif