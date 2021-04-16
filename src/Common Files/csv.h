/*______________________________________________________________________________

   Date:       February 2001
   Programmer: Mitch Pelton
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#ifndef csvH
#define csvH

#include "robust.h"

extern int sections;
typedef enum {NewLn}LineCommand;
typedef enum {_READ_,_CREATE_,_APPEND_}CSVMode;

class icsv
{
  char sdelim,delim;
  public:
  FILE *fptr;
  long recordsCount;
  long recordsRead;
  char temp[MAXSTRING];

  icsv(char *name,char sdel='"',char del=',');
  ~icsv();
  int ok();
  int eof();
  long size();
  long getpos();
  int setpos(long pos);
  icsv& operator >> (int &i);
  icsv& operator >> (long &i);
  icsv& operator >> (float &f);
  icsv& operator >> (double &d);
  icsv& operator >> (char *s);
  icsv& operator >> (LineCommand lc);
  icsv& NextLine();
  int Skip(int numlines);
  int SeekSection(char *name);
  int SeekSection(char *name, long *filepos);
};

class ocsv
{
private:
  FILE *AppendOpen(char *name);
public:

  char temp[MAXSTRING];
  bool always;
  public:
  FILE *fptr;
  char sdelim,delim;
  int lnct;

  ocsv(char *name,char sdel='"',char del=',',CSVMode mode=_CREATE_);
  ~ocsv();
  int ok();
  int HasComma(char *s);
  ocsv& operator << (int i);
  ocsv& operator << (long int i);
  ocsv& operator << (float f);
  ocsv& operator << (double d);
  ocsv& operator << (char *s);
  ocsv& operator << (LineCommand lc);
  ocsv& NextLine();
  void Flush();
  long getpos();
  int setpos(long pos);
  void alwaysQuote();
  void smartQuote();
};

#endif
