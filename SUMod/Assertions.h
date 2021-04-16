#ifndef ASSERTIONS_H
#define ASSERTIONS_H

#define pre(cond) _assert(cond,#cond,__FILE__,__LINE__)
#define post(cond) _assert(cond,#cond,__FILE__,__LINE__)
#define assert(cond) _assert(cond,#cond,__FILE__,__LINE__)

void _assert(bool check,char *strCheck,char *file,int line);

#endif
