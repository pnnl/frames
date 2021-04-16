#ifndef ERROR_H
#define ERROR_H

#include <io.h>
#include <conio.h>
#include <process.h>

void ErrOpen(char *fname);
void Error(char *s1,char *s2="",char *s3="");
void ErrClose();
void WrnOpen(char *fname);
void Warning(char *s1,char *s2="",char *s3="");
void WrnClose();

extern int siteidx,modidx,numcon;

#endif
