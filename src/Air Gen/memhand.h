#include "switch.h"
#ifndef MEMHAND_H
#define MEMHAND_H

typedef void handlerproc(void);
#ifdef MS_DOS
void defaultoutmem();
void defaultnullmem();
void readyhandler(handlerproc *o,handlerproc *n);
void starthandler();
void far *getmem(long int size);
void putmem(void *point);
char far *memdup(char far *s);
void testmem(void *point);
#else
void defaultoutmem();
void defaultnullmem();
void readyhandler();
void starthandler();
void *getmem();
void putmem();
char *memdup(char *s);
void testmem();
#endif

#endif

