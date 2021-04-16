/*______________________________________________________________________________

  Date:       February 2001
  Programmer: Mitch Pelton, Karl Castleton, Bonnie Hoopes
  Company:    Pacific Northwest National Laboratories
  Battelle Corporation
  ________________________________________________________________________________
  __Modifiication  History________________________________________________________
  
    DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#include "frames.h"
#include "csv.h"

int sections=true;


//______________________________________________________________________________

void  FRAMES_API HasSections(int val)
{
  sections = val;
}

/*______________________________________________________________________________

  class icsv
  char temp[MAXSTRING];
  char sdelim,delim;
  FILE *fptr;
*/

icsv::icsv(char *name,char sdel,char del)
{
  fptr = fopen(name,"r");
  sdelim = sdel;
  delim = del;
  recordsRead = 0;
}

icsv::~icsv()
{
  fclose(fptr);
}

int icsv::ok()
{
  if (fptr==NULL) return 0;
  return 1;
}

int icsv::eof()
{
  return feof(fptr);
}

long icsv::size()
{
  long pos = ftell(fptr);
  fseek(fptr,0,SEEK_END);
  long len = ftell(fptr);
  fseek(fptr,pos,SEEK_SET);
  return len;
}

long icsv::getpos()
{
  return ftell(fptr);
}

int icsv::setpos(long pos)
{
  return fseek(fptr,pos,SEEK_SET);
}

icsv& icsv::operator >> (int &i)
{
  *this >> temp;
  i=atoi(temp);
  return *this;
}

icsv& icsv::operator >> (long &i)
{
  *this >> temp;
  i=ratol(temp);
  return *this;
}

icsv& icsv::operator >> (float &f)
{
  *this >> temp;
  f=ratof(temp);
  return *this;
}

icsv& icsv::operator >> (double &d)
{
  *this >> temp;
  d=ratod(temp);
  return *this;
}

icsv& icsv::operator >> (char *s)
{
  char *t = s;
  int c = ' ';
  int q = 0;  // q = 0 no quote q = 1 quote mode

  while (c==' ' && c!=EOF) c = fgetc(fptr);
  ungetc(c,fptr);
  do
  {
    c = fgetc(fptr);
    if (c==sdelim && q==0)
    {
      q = 1;
      c = fgetc(fptr);
    }
    if (c==sdelim && q==1)
    {
      q = 0;
      c = fgetc(fptr);
      // Scan the rest of the line until a delim.
      // Another option might be to consider the closing
      // quote as a delimeter and start a new field.
      // That's probably not the way to go.
      while (c!=delim && c!='\n' && c!=EOF)
        c = fgetc(fptr);
      break;
    }
    // The \n should probably be taken out for multiline strings
    if ((c!=delim && c!='\n')|| q!=0)
    {
      *s =(char)c;
      s++;
    }
  }
  while (((c!=delim && c!='\n') || q!=0) && c!=EOF);
  if (c=='\n')
    ungetc(c,fptr);
  *s = '\0';
  s = trim(t);
  return *this;
}

icsv& icsv::operator >>(LineCommand lc)
{
  if (lc==NewLn) return NextLine();
  else return *this;
}

icsv& icsv::NextLine()
{
  fgets(temp,MAXSTRING-1,fptr);
  recordsRead++;
  return *this;
}

int icsv::Skip(int numlines)
{
  for (int i=0; i<numlines; i++) NextLine();
  return feof(fptr);
}

int icsv::SeekSection(char *name)
{
  char _name[SMALLSTRING];
  int i,n;
  
  recordsCount=0;
  if (sections)
    do
    {
      *this >> _name >> recordsCount >> NewLn;
      if (rstrcmpi(name,_name)!=0)
        for (i=0;i<recordsCount;i++) NextLine();
    }
    while (rstrcmpi(name,_name)!=0 && !feof(fptr));
    recordsRead=0;
    
    //Scan header lines off
    if (!feof(fptr))
    {
      *this >> n >> NewLn;
      for (i=0;i<n;i++) NextLine();
    }
    return feof(fptr);
}


int icsv::SeekSection(char *name, long *filepos)
{
  char _name[SMALLSTRING];
  int i,n;
  
  recordsCount=0;
  if (sections)
    do
    {
      *this >> _name >> recordsCount >> NewLn;
      if (strcmpi(name,_name)!=0)
        for (i=0;i<recordsCount;i++) NextLine();
    }
    while (strcmpi(name,_name)!=0 && !feof(fptr));
    recordsRead=0;
    
    //Scan header lines off
    if (!feof(fptr))
    {
      *filepos = getpos(); // fptr->tellg();
      *this >> n >> NewLn;
      for (i=0;i<n;i++) NextLine();
    }
    return feof(fptr);
}


/*______________________________________________________________________________

  class ocsv
  char temp[MAXFIELD];
  char sdelim,delim;
  FILE *fptr;
*/

void ocsv::smartQuote() {
  always=false;
}
void ocsv::alwaysQuote() {
  always=true;
}

ocsv::ocsv(char *name,char sdel,char del,CSVMode mode)
{
  smartQuote();
  if (mode==_APPEND_)
    fptr = AppendOpen(name);
  else
    fptr = fopen(name,"w+");
  sdelim = sdel;
  delim = del;
  lnct=0;
}

ocsv::~ocsv()
{
  fclose(fptr);
}

int ocsv::ok()
{
  if (fptr==NULL) return 0;
  return 1;
}

ocsv& ocsv::operator << (int i)
{
  fprintf(fptr,"%d%c",i,delim);
  return *this;
}

ocsv& ocsv::operator << (long int i)
{
  fprintf(fptr,"%ld%c",i,delim);
  return *this;
}

ocsv& ocsv::operator << (float f)
{
  fprintf(fptr,"%.15g%c",f,delim);
  return *this;
}

ocsv& ocsv::operator << (double d)
{
  fprintf(fptr,"%.15lg%c",d,delim);
  return *this;
}

int ocsv::HasComma(char *s)
{
  if (always) return true;
  int i = 0;
  char *t = s;
  while (t[i] != '\0' )
    if (t[i++] == ',')
      return 1;
    return 0;
}

ocsv& ocsv::operator << (char  *s)
{
  if (HasComma(s))
    fprintf(fptr,"%c%s%c%c",sdelim,s,sdelim,delim);
  else
    fprintf(fptr,"%s%c",s,delim);
  return *this;
}

ocsv& ocsv::operator << (LineCommand lc)
{
  if (lc==NewLn) return NextLine();
  else return *this;
}

ocsv& ocsv::NextLine()
{
  fseek(fptr,-1,SEEK_CUR);
  fprintf(fptr,"\n");
  fflush(fptr);
  lnct++;
  return *this;
}

void ocsv::Flush()
{
  fflush(fptr);
}

long ocsv::getpos()
{
  return ftell(fptr);
}

int ocsv::setpos(long pos)
{
  return fseek(fptr,pos,SEEK_SET);
}

//------------------------------------------------------------------------------
FILE *ocsv::AppendOpen(char *name)
{
  char fname[MAXPATH];

  rename(name,rstrcpy(fname, name,"~copy~"));
  icsv *inf = new icsv(fname, '"', ',');
  lnct=0;
  fptr = fopen(name,"w+");
  while (true)
  {
    inf->NextLine();
    if(!inf->eof())
    {
      fprintf(fptr,"%s",inf->temp);
    }
    else
      break;
  }
  fflush(fptr);
  delete inf;
  unlink(fname);
  return fptr;
}
