#ifndef NEWINFO_H
#define NEWINFO_H

#include "gid.h"

extern FILE *mlog;
extern int ecount;

void LogStart(char *fname);
void LogStop(void);
char *infol(GIDFILE *p,char *s,int c1=0,int c2=0,int c3=0,int c4=0,int c5=0,int c6=0);

#endif
