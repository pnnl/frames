#include "memhand.h"
#include <string.h>
#include <alloc.h>
#include <process.h>
#include <stdio.h>

typedef void handlerproc();
static handlerproc *outmem, *nullmem;
int handlerready = 0;


void defaultoutmem()
{
  printf("Out of Memory\n");
}

void defaultnullmem()
{
  printf("Null Pointer\n");
}

void readyhandler(handlerproc *o,handlerproc *n)
{
  outmem  = o;
  nullmem = n;
  handlerready = 1;
}

void starthandler()
{
  outmem  = defaultoutmem;
  nullmem = defaultnullmem;
  handlerready = 1;
}

void *getmem(long int size)
{
  void *p;
//  printf("Before farmalloc %ld\n",farcoreleft());
  p = farmalloc(size);
//  printf("After farmalloc %ld\n",farcoreleft());
  if (p == NULL && size>0 && handlerready)
  {
    outmem();
    exit(0);
  }
  return p;
}

void *regetmem(void *oldblock, long int size)
{
  void *p;
//  printf("Before farmalloc %ld\n",farcoreleft());
  p = farrealloc(oldblock,size);
//  printf("After farmalloc %ld\n",farcoreleft());
  if (p == NULL && size>0 && handlerready)
  {
    outmem();
    exit(0);
  }
  return p;
}

void putmem(void *p)
{
  if (p != NULL)
  {
//    printf("Before farfree %ld\n",farcoreleft());
    farfree(p);
//    printf("After farfree %ld\n",farcoreleft());
  }
}

char *memdup(char *s)
{
  char *p;
  p = (char *)getmem((long)(strlen(s)+1));
  strcpy(p,s);
  return p;
}

void testmem(void *point)
{
  if (point == NULL && handlerready)
  {
    nullmem();
    exit(0);
  }
}
