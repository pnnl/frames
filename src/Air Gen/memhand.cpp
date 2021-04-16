#include "memhand.h"
#include <string.h>
#include <alloc.h>
#include <process.h>
#include <stdio.h>

typedef void handlerproc(void);
static handlerproc *outmem,*nullmem;
int    handlerready=0;

//extern FILE *err;

void defaultoutmem(void)
  {
    printf("Out of Memory\n");
  }

void defaultnullmem(void)
  {
    printf("Null Pointer\n");
  }

void readyhandler(handlerproc *o,handlerproc *n)
  {
    outmem=o;
    nullmem=n;
    handlerready=1;
  }

void starthandler()
  {
	 outmem=defaultoutmem;
	 nullmem=defaultnullmem;
	 handlerready=1;
  }

void far *getmem(long int size)
  {
	 void far *p;
//	 if (size>0)
//		fprintf(err,"Before farmalloc %ld\n",farcoreleft());
	 p=farmalloc(size);
//	 if (size>0)
//		fprintf(err,"After farmalloc %ld\n",farcoreleft());
	 if (p==NULL && size>0 && handlerready)
		{
		  outmem();
		  exit(1);
		}
//	 fflush(err);
	 return p;
  }

void putmem(void *p)
  {
	 if (p!=NULL)
		{
//		  fprintf(err,"Before farfree %ld\n",farcoreleft());
		  farfree(p);
//		  fprintf(err,"After farfree %ld\n",farcoreleft());
		}
//	 fflush(err);
  }

char far *memdup(char far *s)
  {
    char far *p;
    p=(char far *)getmem((long)(strlen(s)+1));
    strcpy(p,s);
    return p;
  }

void testmem(void *point)
  {
	 if (point==NULL && handlerready)
		{
		  nullmem();
		  exit(1);
		}
  }
