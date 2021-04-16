/*______________________________________________________________________________

  Date:       1993 - 2004
  Company:    Pacific Northwest National Laboratories
  Battelle Corporation
  ________________________________________________________________________________
  __Modifiication  History________________________________________________________
  
    DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef robustH
#define robustH

#pragma warning(disable:4786)
#pragma warning(disable:4503)

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>
#include <math.h>
#include <time.h>

#define eps  0.00000001            // used for comparisons
#define _eps 0.00001               // used for delta adder in series

#define MAXSTRING 8192
#define LARGESTRING 1024
#define MEDSTRING 256
#define SMALLSTRING 64

#define FULLNAME  0x20

#ifdef BORLAND //if using Borland compiler
  #include <vector.h>
  #include <map.h>
  #include <dir.h>
  #include <values.h>
  #include <io.h>       //neeeded for file 'access' call

  #define mMAXFLOAT     MAXFLOAT
  #define mMINFLOAT     MINFLOAT
  #define mMAXDOUBLE    MAXDOUBLE // -- Note:  atof returns 1.NAN on a string representing this value
  //  #define mMAXDOUBLE    MAXDOUBLE * 1.0e-1  // this works OK with atof and is 'close enough'
  #define mMINDOUBLE    MINDOUBLE // -- Note:  DBL_MIN is the miminum POSITIVE value
  //  #define mMINDOUBLE    MAXDOUBLE * -1.0e-1

  #define MY_strcmpi strcmpi
  #define MY_strncmpi strncmpi
  #define MY_fstruct ffblk

#else//if using Microsoft compiler
  #include <io.h>
  #include <float.h>
  #include <vector>
  #include <map>
  using namespace std;

  #define mMAXFLOAT     FLT_MAX
  #define mMINFLOAT     FLT_MAX * -1.0e-1
//#define mMAXDOUBLE    DBL_MAX -- Note:  atof returns 1.NAN on a string representing this value
  #define mMAXDOUBLE    DBL_MAX * 1.0e-1  // this works OK with atof and is 'close enough'
//#define mMINDOUBLE    DBL_MIN -- Note:  DBL_MIN is the miminum POSITIVE value
  #define mMINDOUBLE    DBL_MAX * -1.0e-1

  #define MAXPATH _MAX_PATH
  #define MAXDRIVE _MAX_DRIVE
  #define MAXDIR _MAX_DIR
  #define MAXFILE _MAX_FNAME
  #define MAXEXT _MAX_EXT

  #define MY_strcmpi _stricmp
  #define MY_strncmpi _strnicmp
  #define MY_fstruct _finddata_t
  #define findfirst _findfirst
  #define findnext _findnext
  #define findclose _findclose
  #define strlwr _strlwr
  #define strupr _strupr

  #define WILDCARDS 0x01
  #define EXTENSION 0x02
  #define FILENAME  0x04
  #define DIRECTORY 0x08
  #define DRIVE     0x10
#endif

typedef std::vector<char *> _LIST;
extern _LIST rList;

// variables for fnsplit
extern int fnflags;
extern char fnpath[MAXPATH];
extern char fndrive[MAXDRIVE];
extern char fndir[MAXDIR];
extern char fnfile[MAXFILE];
extern char fnext[MAXEXT];

// work variables
extern char *blank;
extern char dummy[MAXSTRING];

// used for epf,rif,hif readers
typedef struct __expRoute
{
  char route[SMALLSTRING];
  char path[SMALLSTRING];
  char measure[SMALLSTRING];
  char unit[SMALLSTRING];
} expRoute;

typedef std::map<std::string, expRoute *>      _EXPLIST;
typedef _EXPLIST::iterator                     _EXPLIST_IT;

extern char MSGName[MAXPATH];
extern char WRNName[MAXPATH];
extern char ERRName[MAXPATH];

void writeMsg(char *s, char *l2=NULL, char *l3=NULL);
void writeWarn(char *s, char *l2=NULL, char *l3=NULL);
void writeError(char *s, char *l2=NULL, char *l3=NULL);

//robust time and date strings
char *Time();
char *Date();

//robust string list operations
_LIST splitList(char *strToSplit);

//robust path string operations
char *AddExtension(char *file, char *ext);
char *concat(char *prefix, char *pname);
int fnSetPath(char *path);
int rfnsplit(char *path, char *mDrv, char *mDir, char *mFle, char *mExt);
void getpath(char *fname);

//robust string operations
int isnumeric(char *s);
int isboolean(char *s);
int ratoi(char *s);
long ratol(char *s);
float ratof(char *s);
double ratod(char *s);

char *trim(char *s);
char *rstrcat(char *s1, const char *s2=NULL, const char *s3=NULL, const char *s4=NULL, const char *s5=NULL, const char *s6=NULL);
char *rstrcpy(char *s1, const char *s2=NULL, const char *s3=NULL, const char *s4=NULL, const char *s5=NULL, const char *s6=NULL);
char *rstrstr(char *s1, char *s2);

int rstrcmp(const char *s1,const char *s2);
int rstrcmpi(const char *s1,const char *s2);
int rstrncmp(const char *s1,const char *s2,const int len);
int rstrncmpi(const char *s1,const char *s2,const int len);

//misc. routines
int GCD(int n, int m);
void sort(int n, int *idx, float *val, float *mean, float *min, float *max, float *median, float *sd);
void rsort(int n, int *idx, double *val, double *mean, double *min, double *max, double *median, double *sd);
int Copy(char *s, char *d);
int isEqual(float x, float y, float e=eps);
int isEqual(double x, double y, double e=eps);

/*
* 3d interpolate the value at x, y using the values at (listX, listY) and return
* the result.  pointCount should specify the size of the listX, listY, and
* values lists (other words: how many points will be used to interpolate x,y)
* the algorithm used by this function is a x-point inverse squared distance
* weighted interpolation.  this means:
* distance1 = (x1, y1) - (x, y)
* distance2 = (x2, y2) - (x, y)
* ...
* weight1 = 1/(distance1^2)
* weight2 = 1/(distance2^2)
* ...
* weightvalue1 = weight1 * values[1]
* weightvalue2 = weight2 * values[2]
* ...
* pointvalue = sum(weightvaluesAll) / sum(weight)
*/
float interpolate(float x, float y, float *listX, float *listY, float *values, int pointCount=3);
double interpolate(double x, double y, double *listX, double *listY, double *values, int pointCount=3);
// estimate the y value at x coordinate targetX, according to the
// line defined by the points (x1, y1) - (x2, y2)
float interpolate(float x1, float y1, float x2, float y2, float targetX);
double interpolate(double x1, double y1, double x2, double y2, double targetX);

extern _LIST cList;
extern _LIST nList;
extern _LIST d1List;
extern _LIST d2List;
extern _LIST d3List;

// ???List are comma seperated and should be one to one with cas as primary key
// all degradation products must be listed as parents if not they are ignored
void OpenDecayChain(char *casList, char *nameList, char *degList, char *secList, int branching);
void CloseDecayChain();
void SetDecayAllowDup(bool flag);
void ClearDecayFlags();
void GetDecayChain(int p);

int Date2JulHours(int mn, int dy, int yr, int hr);
void JulHours2Date(int jhr, int yr, int *mn, int *dy, int *hr);

#endif
