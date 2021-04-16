#ifndef MEMHAND_H
#define MEMHAND_H

typedef void handlerproc();

void defaultoutmem();
void defaultnullmem();
void readyhandler(handlerproc *o,handlerproc *n);
void starthandler();
void *regetmem(void *oldblock, long int size);
void *getmem(long int size);
void putmem(void *point);
char *memdup(char *s);
void testmem(void *point);

#endif

