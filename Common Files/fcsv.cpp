/*______________________________________________________________________________

  Date:       February 2001
  Programmer: Mitch Pelton, Karl Castleton, Bonnie Hoopes
  Company:    Pacific Northwest National Laboratories
  Battelle Corporation
  ________________________________________________________________________________
  __Modifiication  History________________________________________________________
  
    DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#include "fcsv.h"

/*
class fcsv
{
bool wasQuoted;
int  pos,len,mde,type;
char line[MAXSTRING],del;
FILE *fptr;

*/

void fcsv::find(int *start,int *size)
{
  char n;
  *start=pos;
  *size=0;
  wasQuoted = false;
  do
  n=next();
  while (n==' ' || n=='\t');      // ignore leading white space
  if (n=='"')                     // see if it starts with a '"'
  {
    wasQuoted = true;
    *start=pos;
    *size=0;
    while (next()!='"')
      (*size)++;
    do
    n=next();
    while (n!=del || ((n!='\t' && n!=' ') && del==' '));
  }
  else if (n!=del)
  {
    *start=pos-1;
    *size=1;
    n=next();
    while (n!=del || ((n!='\t' && n!=' ') && del==' '))
    {
      (*size)++;
      n=next();
    }
  }
  /*
  if (*size > 0 && line[*start+*size-1]==' ') // ignore ending spaces
  {
  while (line[*start+*size-1]==' ')
  (*size)--;
  }
  */
}

char fcsv::next()
{
  char retval;
  
  if (pos<len)
  {
    retval =line[pos];
    pos++;
    if ( retval=='\t' )
      retval = ' ';
  }
  else
    retval = del;
  return retval;
}

fcsv::fcsv()
{
  fptr=NULL;
  newline = true;
  wasQuoted = false;
}

int  fcsv::open(char *name,int mode,char delim)
{
  switch(mode)
  {
  case READ:
    fptr=fopen(name,"rt");
    break;
  case WRITE:
    fptr=fopen(name,"wt");
    break;
  case APPEND:
    fptr=fopen(name,"at");
    break;
  default: return 0;
  }
  mde=mode;
  del=delim;
  return rewind();
}

int fcsv::rewind()
{
  pos=0;
  len=0;
  newline = true;
  wasQuoted = false;
  if (fptr!=NULL && mde==READ)
  {
    fseek(fptr,0l,SEEK_SET);
    fgets(line,MAXSTRING,fptr);
    len=strlen(line)-1;
    line[len]='\0';
    return 1;
  }
  else if (fptr!=NULL && mde==WRITE)
  {
    line[0]='\0';
    return 1;
  }
  else
  {
    rstrcpy(line,"");
    return 0;
  }
}

void fcsv::close()
{
  newline = true;
  if (fptr==NULL) return;
  fclose(fptr);
}

void fcsv::flush()
{
  if (fptr) fflush(fptr);
}

int fcsv::eof()
{
  if ((feof(fptr) && pos==len) || fptr==NULL)
    return 1;
  else
    return 0;
}

int fcsv::eol()
{
  if (feof(fptr) || pos==len || fptr==NULL)
    return 1;
  else
    return 0;
}

void fcsv::read(int *val)
{
  int start,size;
  if (fptr==NULL)
  {
    *val=0;
    return;
  }
  find(&start,&size);
  *val=ratoi(&line[start]);
}

void fcsv::read(double *val)
{
  int start,size;
  if (fptr==NULL)
  {
    *val=0.0;
    return;
  }
  find(&start,&size);
  *val=ratof(&line[start]);
}

void fcsv::read(float *val)
{
  int start,size;
  if (fptr==NULL)
  {
    *val=0.0;
    return;
  }
  find(&start,&size);
  *val=ratof(&line[start]);
}

void fcsv::read(char *val)
{
  int start,size;
  if (fptr==NULL)
  {
    rstrcpy(line,"");
    rstrcpy(val,"");
    return;
  }
  find(&start,&size);
  strncpy(val,&line[start],size);
  val[size]='\0';
  val = trim(val);
}

void fcsv::readchar(char *val)
{
  int start,size;
  if (fptr==NULL)
  {
    *val='\0';
    return;
  }
  find(&start,&size);
  *val=line[start];
}

void fcsv::readln(char *val)
{
  if (fptr==NULL)
  {
    rstrcpy(line,"");
    rstrcpy(val,"");
    return;
  }
  rstrcpy(val,&line[pos]);
  readln();
}

void fcsv::readln()
{
  if (fgets(line,MAXSTRING,fptr)==NULL)
  {
    pos=0;
    len=0;
    line[0]='\0';
  }
  else
  {
    pos=0;
    len=strlen(line)-1;
    line[len]='\0';
  }
}

void fcsv::write(int val,char *format)
{
  if (fptr==NULL) return;
  if (newline)
  {
    newline = false;
    fprintf(fptr,format,val);
  }
  else
  {
    fprintf(fptr,"%c",del);
    fprintf(fptr,format,val);
  }
}
void fcsv::write(long int val,char *format)
{
  if (fptr==NULL) return;
  if (newline)
  {
    newline = false;
    fprintf(fptr,format,val);
  }
  else
  {
    fprintf(fptr,"%c",del);
    fprintf(fptr,format,val);
  }
}

void fcsv::write(float val,char *format)
{
  if (fptr==NULL) return;
  if (newline)
  {
    newline = false;
    fprintf(fptr,format,val);
  }
  else
  {
    fprintf(fptr,"%c",del);
    fprintf(fptr,format,val);
  }
}

void fcsv::write(double val,char *format)
{
  if (fptr==NULL) return;
  if (newline)
  {
    newline = false;
    fprintf(fptr,format,val);
  }
  else
  {
    fprintf(fptr,"%c",del);
    fprintf(fptr,format,val);
  }
}

void fcsv::write(char *val)
{
  if (fptr==NULL) return;
  if (newline)
  {
    newline = false;
    fprintf(fptr,"\"%s\"",val);
  }
  else
    fprintf(fptr,"%c\"%s\"",del,val);
}

void fcsv::write(char *val, bool quote)
{
  if (fptr==NULL) return;
  if (newline)
  {
    newline = false;
    if (quote)
      fprintf(fptr,"\"%s\"",val);
    else
      fprintf(fptr,"%s",val);
  }
  else
    if (quote)
      fprintf(fptr,"%c\"%s\"",del,val);
    else
      fprintf(fptr,"%c%s",del,val);
}

void fcsv::write(fpos_t pos, long int val)
{
  if (fsetpos(fptr,&pos)==0)
    write(val);
}

void fcsv::writeln()
{
  if (fptr==NULL) return;
  newline = true;
  fprintf(fptr,"\n");
}

fpos_t fcsv::getfpos()
{
  fpos_t pos;
  fgetpos(fptr,&pos);
  return(pos);
}

void fcsv::setfpos(fpos_t pos)
{
  fsetpos(fptr,&pos);
}


