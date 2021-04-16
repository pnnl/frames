/*______________________________________________________________________________

   Date:       2001
   Programmer: Mitch Pelton
   Company:    Pacific Northwest National Laboratories
               Run by Battelle Memorial Institute
               For Department of Energy
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef messageH
#define messageH

#ifdef _WIN32
#pragma warning(disable:4786)
#endif

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include <process.h>
#include <limits.h>
#include "Test.h"
#include "robust.h"

// Platform switches
/*____________________________________________________________________________*/
#ifdef BORLAND
  #include <values.h>
  #include <dir.h>
  #include <vcl.h>
  
  #define INT           int
#else
  #include <windows.h>
  #include <winbase.h>
  #include <float.h>

  #define INT           short
#endif

//  General purpose defines
/*____________________________________________________________________________*/
#define MMF_MAX_DIM 12


#define EPSILON              1.0E-4
#define MININTEGER           INT_MIN // <limits.h>
#define MAXINTEGER           INT_MAX // <limits.h>
#define MINSTRING            512

#define dsFLOAT                   0
#define dsINTEGER                 1
#define dsLOGICAL                 2
#define dsSTRING                  3
#define dsINVALID                 4

#define DESCENDING               -1
#define NOTORDERED                0
#define ASCENDING                 1

#define LOCKED                    0
#define UNLOCKED                  1

#define MODULEUNKNOWN             0 // no module selected
#define MODULESELECTED            1 // module selected
#define MODULEINPUTOK             2 // module input successful
#define MODULERUNOK               3 // module run successful


// Error and warning message codes
/*____________________________________________________________________________*/

#define SUCCESS               0
#define FAILURE              -1
#define DATATYPEREAD         -2
#define DATATYPEWRITE        -3

// Error codes
/*____________________________________________________________________________*/

#define VARIABLENULL          -1000
#define VARIABLENAME          -1001
#define VARIABLEDESCRIPTION   -1002
#define VARIABLEDATATYPE      -1003
#define VARIABLERANGE         -1004
#define VARIABLECONVERT       -1005
#define VARIABLESTOCHASTIC    -1006
#define VARIABLEPREPOSITION   -1007
#define VARIABLEDIMENSION     -1008
#define VARIABLEINDEX         -1009
#define VARIABLEELEMENTS      -1010
#define VARIABLENOTFOUND      -1011
#define VARIABLEDUPLICATE     -1012
#define VARIABLEINDEXNAME     -1013
#define VARIABLESCALAR        -1014
#define VARIABLEREFCOUNT      -1015
#define VARIABLESELFINDEXED   -1016
#define VARIABLEPRIMARYKEY    -1017
#define VARIABLENAMECHAR      -1018
#define REQUIRESATOZ		      -1019
#define VARIABLEDECLARATION   -1020

#define DICTIONARYFILE        -2000
#define DICTIONARYPATH        -2001
#define DICTIONARYNAME        -2002
#define DICTIONARYFORMAT      -2003
#define DICTIONARYEMPTY       -2004
#define DICTIONARYREF         -2005
#define REFDICINVALID         -2006
#define REFVARINVALID         -2007

#define DATASETFILE           -3000
#define DATASETPATH           -3001
#define DATASETNAME           -3002
#define DATASETFORMAT         -3003
#define DATASETEMPTY          -3004
#define DATASETDESCRIPTION    -3005
#define DATASETDICTIONARY     -3006
#define DATASETMODE           -3007
#define DATASETCOUNT          -3008
#define DATASETNOTFOUND       -3009
#define VALUENOTFOUND         -3010

#define DOMAINNOTFOUND        -4000
#define CLASSNOTFOUND         -4001
#define GROUPNOTFOUND         -4002
#define SUBGROUPNOTFOUND      -4003
#define MODULENOTFOUND        -4004
#define ICONNOTFOUND          -4005
#define SCHEMENAME            -4006
#define SCHEMENOTFOUND        -4007

#define CONVERTFILE           -5000
#define UNITNULL              -5001
#define UNITABBR              -5002
#define UNITNAME              -5003
#define MEASUREINDEX          -5004
#define MEASURENAME           -5005

#define ICONMODELNOTOK        -6000
#define ICONUINOTOK           -6001
#define UIDICNOTDEFINED       -6002
#define ICONMODUNKNOWN        -6003
#define ICONMODFAILED         -6004 
#define NOFLOWCYCLES          -6005

#define LISTEMPTY             -7000
#define LISTITEMNULL          -7001
#define LISTKEYNULL           -7002
#define LISTINVALIDITEM       -7003
#define LISTINVALIDKEY        -7004
#define LISTINVALIDINDEX      -7005
#define LISTDUPLICATE         -7006
#define LISTSORTED            -7007
#define LISTORDER             -7008

#define OUTOFMEMORY          -10000
#define BADPROCESSID         -10001
#define NOERRORFILE          -10002
#define NOWARNFILE           -10003
#define NODATASETS           -10004
#define NODICTIONARIES       -10005
#define NOMODULES            -10006
#define NOSIMULATIONS        -10007
#define NOSTARTUPDIC         -10008
#define NOCONVERSIONDIC      -10009
#define NOSIMULATIONDIC      -10010
#define NOMODULEDIC          -10011
#define NODELETESYSDIC       -10012
#define LAUNCHWAIT           -10013
#define LAUNCHERROR          -10014
#define OPENIO               -10015
#define SIMNOTAVAILABLE      -10016
#define INVALIDINIPATH       -10017
#define INVALIDPATH          -10018
#define OPENFILEERROR        -10019
#define WRITEFILEERROR       -10020

#define UDEFERROR            -49998
#define UDEFWARNING          -49999

#define LASTERROR            -50000

//#define GETDIMENSION             -1
//#define GETMIN                   -1
//#define GETMAX                   -1
//#define GETSTOCHASTIC            -1
//#define GETINDEXINGKEY           -1
//#define NOARGUMENT           -10004

//extern char *__Msg1000[];
//extern char *__Msg2000[];
//extern char *__Msg3000[];
//extern char *__Msg4000[];
//extern char *__Msg10000[];
extern char  **EList[];
extern char *blank;

void cstr2fstr(char *s, int len);
void fstr2cstr(char *s, int len);
char *UnquotedString(char *str);
char *GetMsgText(int code);
char *GetPathPart(char *path, int part);

int RegisterError(int code);
int ReadError(int code, char *_msg);
int ReadWarning(int code, char *_msg);

int *SetIdx(int _1=0, int _2=0, int _3=0, int _4=0, int _5=0, int _6=0,
            int _7=0, int _8=0, int _9=0, int _10=0, int _11=0, int _12=0);

extern int _Idx[12];

#endif



