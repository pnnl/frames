/*___________________________________________________________________________

   Date:       2001
   Programmer: Mitch Pelton
   Company:    Pacific Northwest National Laboratories
               Run by Battelle Memorial Institute
               For Department of Energy
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#include "Message.h"

char *__Msg1000[] = {
  "Variable is NULL",                  //  #define VARIABLENULL          -1000  //
  "Invalid variable name in dictionary",        //  #define VARIABLENAME          -1001  //
  "Invalid variable description in dictionary",      //  #define VARIABLEDESCRIPTION   -1002  //
  "Invalid variable data type in dictionary",      //  #define VARIABLEDATATYPE      -1003  //
  "Invalid variable range in dictionary",        //  #define VARIABLERANGE         -1004  //
  "Variable unit conversion not supported",        //  #define VARIABLECONVERT       -1005  //
  "Invalid variable stochastic flag in dictionary",    //  #define VARIABLESTOCHASTIC    -1006  //
  "Invalid variable preposition in dictionary",      //  #define VARIABLEPREPOSITION   -1007  //
  "Invalid variable dimension in dictionary",      //  #define VARIABLEDIMENSION     -1008  //
  "Invalid variable indices in dictionary",        //  #define VARIABLEINDEX         -1009  //
  "Variable value has not been defined",        //  #define VARIABLEELEMENTS      -1010  //
  "Variable not found in dictionary",          //  #define VARIABLENOTFOUND      -1011  //
  "Variable duplicate found in dictionary",        //  #define VARIABLEDUPLICATE     -1012  //
  "Invalid variable index name in dictionary",      //  #define VARIABLEINDEXNAME     -1013  //
  "Invalid variable scalar in dictionary",        //  #define VARIABLESCALAR        -1014  //
  "Invalid variable reference count in dictionary",    //  #define VARIABLEREFCOUNT      -1015  //
  "Invalid variable self index in dictionary",      //  #define VARIABLESELFINDEXED   -1016  //
  "Invalid variable primary key in dictionary",      //  #define VARIABLEPRIMARYKEY    -1017  //
  "First character of variable name must be a/A to z/Z",  // #define VARIABLENAMECHAR -1018
  "First character must be a/A to z/Z",					  // #define REQUIRESATOZ -1019
  "Variable declaration has changed",             // #define VARIABLEDECLARATION -1020 
};
char *__Msg2000[] = {
  "Unable to find or open dictionary",          //  #define DICTIONARYFILE        -2000  //
  "Dictionary path is NULL",              //  #define DICTIONARYPATH        -2001  //
  "Dictionary name is NULL",              //  #define DICTIONARYNAME        -2002  //
  "Dictionary format not recognized",          //  #define DICTIONARYFORMAT      -2003  //
  "No variables have been defined for dictionary",   //  #define DICTIONARYEMPTY       -2004  //
  "Dictionary is a referenced dictionary",
  "Index refers to unknown dictionary",
  "Index refers to unknown dictionary variable"
};
char *__Msg3000[] = {
  "Unable to find or open dataset",            //  #define DATASETFILE           -3000  //
  "Dataset path is NULL",                //  #define DATASETPATH           -3001  //
  "Dataset name is NULL",                //  #define DATASETNAME           -3002  //
  "Dataset format not recognized",            //  #define DATASETFORMAT         -3003  //
  "No parameters have been defined for dataset",    //  #define DATASETEMPTY          -3004  //
  "Dataset internal description does not match named dictionary",          //  #define DATASETDESCRIPTION    -3005  //
  "Dataset internal dictionary name does not match named dictionary",        //  #define DATASETDICTIONARY     -3006  //
  "Invalid I/O mode specified for dataset",        //  #define DATASETMODE           -3007  //
  "Dataset count exceeds dictionary count",        //  #define DATASETCOUNT          -3008  //
  "Dataset name not found",                //  #define DATASETNOTFOUND       -3009  //
  "Value not found in variable"              //  #define VALUENOTFOUND         -3010 //
};
char *__Msg4000[] = {
  "Domain name not found",                //  #define DOMAINNOTFOUND        -4000  //
  "Class name not found",                //  #define CLASSNOTFOUND         -4001  //
  "Group name not found",                //  #define GROUPNOTFOUND         -4002  //
  "Sub-group name not found",              //  #define SUBGROUPNOTFOUND      -4003  //
  "Module name not found",                //  #define MODULENOTFOUND        -4004  //
  "Module Icon ID not found",              //  #define ICONNOTFOUND          -4005  //
  "Scheme name is NULL",                //  #define SCHEMENAME            -4006  //
  "Scheme name not found",              //  #define SCHEMENOTFOUND        -4007
};
char *__Msg5000[] = {
  "Unable to find or open conversion file",        //  #define CONVERTFILE           -5000  //
  "Unit is NULL",                    //  #define UNITNULL              -5001  //
  "Unit abbreviation is NULL",              //  #define UNITABBR              -5002  //
  "Unit descriptive name is NULL",            //  #define UNITNAME              -5003  //
  "Measure index is out of range",            //  #define MEASUREINDEX          -5004  //
  "Measure name not found"                //  #define MEASURENAME           -5005  //
};
char *__Msg6000[] = {
  "IconActive is not OK",                //  #define ICONPASSIVENOTOK      -6000  //
  "IconPassive is not OK",                //  #define ICONACTIVENOTOK       -6001  //
  "No module UI dictionary defined",            //  #define UIDICNOTDEFINED       -6002  //
  "Icon module has not been selected",          //  #define ICONMODUNKNOWN -6003
  "Module Failed", // #define ICONMODFAILED -6004
  "Cycles are not permitted", // #define NOFLOWCYCLES -6005
};
char *__Msg7000[] = {
  "List is empty",                    //  #define LISTEMPTY             -7000  //
  "List item is NULL",                  //  #define LISTITEMNULL          -7001  //
  "List key is NULL",                  //  #define LISTKEYNULL           -7002  //
  "List item not found",                //  #define LISTINVALIDITEM       -7003  //
  "List key not found",                  //  #define LISTINVALIDKEY        -7004  //
  "List index is out of range",              //  #define LISTINVALIDINDEX      -7005  //
  "List does not allow duplicates,  duplicate found",    //  #define LISTDUPLICATE         -7006  //
  "List is sorted, can't promote or demote item",       //  #define LISTSORTED            -7007  //
  "List is ascending or descending ordered, value not in order"    //  #define LISTORDER
};
char *__Msg8000[] = {
  "Undefined"
};
char *__Msg9000[] = {
  "Undefined"
};
char *__Msg10000[] = {
  "Out of memory",                    //  #define OUTOFMEMORY          -10000  //
  "Bad process ID",                    //  #define BADPROCESSID         -10001  //
  "No error file",                    //  #define NOERRORFILE          -10002  //
  "No warning file",                  //  #define NOWARNFILE           -10003  //
  "No datasets",                    //  #define NODATASETS           -10004  //
  "No dictionaries",                  //  #define NODICTIONARIES       -10005  //
  "No modules",                      //  #define NOMODULES            -10006  //
  "No simulations",                    //  #define NOSIMULATIONS        -10007  //
  "Unable to find or open STARTUP dictionary",      //  #define NOSTARTUPDIC         -10008  //
  "Unable to find or open CONVERSION dictionary",    //  #define NOCONVERSIONDIC      -10009  //
  "Unable to find or open SIMULATION dictionary",    //  #define NOSIMULATIONDIC      -10010  //
  "Unable to find or open MODULE dictionary",      //  #define NOMODULEDIC          -10011  //
  "System dictionaries cannot be deleted",        //  #define NODELETESYSDIC       -10012  //
  "Failed wait for launched executable",         // #define LAUNCHWAIT      -10013
  "Failed to launch executable",         // #define LAUNCHERROR      -10014
  "Failed to call OpenIO or OpenINI",         // #define LAUNCHERROR      -10015
  "Simulation not available",         // #define SIMNOTAVAILABLE      -10016
  "OpenINI path does not match environment path", // #define INVALIDINIPATH -10017
  "File does not exist", // #define INVALIDPATH -10018
  "Error opening file",  // #define OPENFILEERROR -10019
  "Error writing to file",  // #define WRITEFILEERROR -10020
};


char  **EList[] = { __Msg1000,__Msg2000,__Msg3000,__Msg4000,__Msg5000,
                    __Msg6000,__Msg7000,__Msg8000,__Msg9000,__Msg10000 };

//char *blank = "";

int _Idx[12]  =  {0,0,0,0,0,0,0,0,0,0,0,0};

int *SetIdx(int _1,int _2, int _3, int _4, int _5, int _6, int _7, int _8, int _9, int _10, int _11, int _12)
{
  _Idx[0]  = _1;
  _Idx[1]  = _2;
  _Idx[2]  = _3;
  _Idx[3]  = _4;
  _Idx[4]  = _5;
  _Idx[5]  = _6;
  _Idx[6]  = _7;
  _Idx[7]  = _8;
  _Idx[8]  = _9;
  _Idx[9]  = _10;
  _Idx[10]  = _11;
  _Idx[11]  = _12;
  return _Idx;
}
//-----------------------------------------------------------------------

char *GetMsgText(int code)
{
  for (int i=-10000, j=10; i<0; i+=1000, j--)
  {
    if (code <= i)
      return (char *)(EList[j-1][-(code-i)]);
  }
  return NULL;
}
//-----------------------------------------------------------------------


char *UnquotedString(char in[])
{
  unsigned int i=0;
  char *str = (char *)in;

  if (str == NULL) return str;
  for (i=strlen(str)-1; i>=0; i--)
  {
    if ((str[i]=='\"') || (str[i]=='\''))
      str[i]='\0';
    else
      break;
  }
  for (i=0;i<strlen(str);i++)
  {
    if ((str[0]=='\"') || (str[0]=='\''))
      str = str++;
    else
      break;
  }
  return str;
}

//  String conversion routines to convert C to FORTRAN and vise versa
/*____________________________________________________________________________*/
void cstr2fstr(char *s, int len)
{
#ifdef DEBUG
  *warn << "Starting conversion C to Fortan" << NewLn;
#endif
  for (int i=strlen(s); i<len; i++)    s[i] = ' ';
#ifdef DEBUG
  *warn << "Ending conversion C to Fortan" << NewLn;
#endif
}
//-----------------------------------------------------------------------

void fstr2cstr(char *s, int len)
{
#ifdef DEBUG
  *warn << "Starting conversion Fortan to C" << NewLn;
#endif

  s[len] = ' ';
  while (s[len] == ' ' && len >= 0)    s[len--] = '\0';

#ifdef DEBUG
  *warn << "Ending conversion Fortan to C" << NewLn;
#endif

}
//-----------------------------------------------------------------------


//List *ERR = new List();

int RegisterError(int code)
{
//  for (int i=-10000, j=10; i<0; i+=1000, j--)
//    if (code < i) ERR->Add((void *)(EList[j][-code-i]));
  return code;
}
//-----------------------------------------------------------------------

int ReadError(int code, char *_msg)
{
  rstrcpy(_msg,blank);
  for (int i=-10000, j=10; i<0; i+=1000, j--)
  {
    if (code <= i)
    {
      rstrcpy(_msg,(char *)(EList[j-1][i-code]));
      return SUCCESS;
    }
  }
  return FAILURE;
}

int ReadWarning(int code, char *_msg)
{
  return ReadError(code, _msg);
}





