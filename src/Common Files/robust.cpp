/*______________________________________________________________________________

  Date:       February 2001
  Company:    Pacific Northwest National Laboratories
  Battelle Corporation
  ________________________________________________________________________________
  __Modifiication  History________________________________________________________
  
    DATE     WHO  DESCRIPTION
    
  ________________________________________________________________________________
      
    Notes on path strinmg functions and copy functions
    A file's full path name (path) as a string in the form X:\DIR\SUBDIR\NAME.EXT
    and splits path into its four components.  Those components in the strings
    pointed to by drive, dir, name, and ext.

    The maximum sizes for these strings are given by the constants
    MAXDRIVE, MAXDIR, MAXPATH, MAXFILE, and MAXEXT (defined in dir.h)
    and each size includes space for the null-terminator.

    Constant  Max 32-bit  String
    ==============================
    MAXPATH     256 path
    MAXDRIVE    3 drive; includes colon (:)
    MAXDIR      260 dir; includes leading and trailing backslashes (\)
    MAXFILE     256 name
    MAXEXT      256 ext; includes leading dot (.)

    fnsplit assumes that there is enough space to store each non-null component.

    When fnsplit splits path it treats the punctuation as follows:
      drive includes the colon (C:, A:, and so on)
      dir   includes the leading and trailing backslashes (\BC\include\, \source\ ,and so on)
      name  includes the file name
      ext   includes the dot preceding the extension (.C, .EXE, and so on).

    fnmerge and fnsplit are invertible; if you split a given path with fnsplit then merge the resultant components with fnmerge you end up with path.

    Return Value

    fnflags an integer (composed of five flags defined in dir.h)
    indicating which of the full path name components were present in path.
    These flags and the components they represent are

    EXTENSION An extension
    FILENAME  A file name
    DIRECTORY A directory (and possibly subdirectories)
    DRIVE     A drive specification (see dir.h)
    WILDCARDS Wildcards (* or ?)
______________________________________________________________________________*/

#include "robust.h"
#include "frames.h"
/*____________________________________________________________________________*/

// used by *split
int fnflags;
char fnpath[MAXPATH];
char fndrive[MAXDRIVE];
char fndir[MAXDIR];
char fnfile[MAXFILE];
char fnext[MAXEXT];

char *blank= "";
char dummy[MAXSTRING];

// used for writeMsg, writeWarn and writeError
char MSGName[MAXPATH];
char WRNName[MAXPATH];
char ERRName[MAXPATH];


//------------------------------------------------------------------------------
void writeMsg(char *l1,char *l2,char *l3)
{
  FILE *f;
  f=fopen(MSGName,"at");
  if (f == NULL) return;
  if (l1!=NULL) fprintf(f,"%s",l1);
  if (l2!=NULL) fprintf(f," %s",l2);
  if (l3!=NULL) fprintf(f," %s",l3);
  fprintf(f,"\n");
  fflush(f);
  fclose(f);
}
//------------------------------------------------------------------------------
void writeWarn(char *l1,char *l2,char *l3)
{
  FILE *f;
  f=fopen(WRNName,"at");
  if (f == NULL) return;
  if (l1!=NULL) fprintf(f,"%s",l1);
  if (l2!=NULL) fprintf(f," %s",l2);
  if (l3!=NULL) fprintf(f," %s",l3);
  fprintf(f,"\n");
  fflush(f);
  fclose(f);
}
//------------------------------------------------------------------------------
void writeError(char *l1,char *l2,char *l3)
{
  FILE *f;
  f=fopen(ERRName,"at");
  if (f == NULL) return;
  if (l1!=NULL) fprintf(f,"%s",l1);
  if (l2!=NULL) fprintf(f," %s",l2);
  if (l3!=NULL) fprintf(f," %s",l3);
  fprintf(f,"\n");
  fflush(f);
  fclose(f);
}
//------------------------------------------------------------------------------
static char timestring[9];
char *Time()
{
  time_t ti;
  struct tm *t;
  ti=time(NULL);
  t=localtime(&ti);
  sprintf(timestring,"%02d:%02d:%02d",t->tm_hour,t->tm_min,t->tm_sec);
  return timestring;
}
//------------------------------------------------------------------------------
static char daystring[11]="  /  /    ";
char *Date()
{
  time_t ti;
  struct tm *t;
  ti=time(NULL);
  t=localtime(&ti);
  sprintf(daystring,"%02d/%02d/%04d",t->tm_mon+1,t->tm_mday,t->tm_year+1900);
  return daystring;
}
//------------------------------------------------------------------------------
static char result[MAXPATH];
char *AddExtension(char *file, char *ext)
{
  sprintf(result,"%s.%s",file,ext);
  return result;
}

char *concat(char *prefix, char *pname)
{
  sprintf(result,"%s%s",prefix,pname);
  return result;
}
//--------------------------------------------------------------------
_LIST rList;
_LIST splitList(char *strToSplit)
{
  char *tempToken;
  
  rList.clear();
  tempToken = strtok(strToSplit, ",");
  if(tempToken)
    rList.push_back(tempToken);
  while((tempToken = strtok(NULL, ",")) != NULL)
    rList.push_back(tempToken);
  return rList;
}
//--------------------------------------------------------------------
int rfnsplit(char *path,char *mDrv,char *mDir,char *mFle,char *mExt)
{
  strcpy(fnpath,path);
#ifdef Borland
  fnflags = fnsplit(path,mDrv,mDir,mFle,mExt);//int return
#else
  _splitpath(path,mDrv,mDir,mFle,mExt);//void return
  
  if (strlen(mDrv)!=0) fnflags |= DRIVE;
  if (strlen(mDir)!=0) fnflags |= DIRECTORY;
  if (strlen(mFle)!=0) fnflags |= FILENAME;
  if (strlen(mExt)!=0) fnflags |= EXTENSION;
#endif
  return fnflags;
}

//------------------------------------------------------------------------------
int fnSetPath(char *path)
{
  strcpy(fnpath,path);
  fnflags = rfnsplit(fnpath,fndrive,fndir,fnfile,fnext);
  return fnflags;
}
//--------------------------------------------------------------------------------
char *GetPathPart(char *path, int part)
{
  fnflags = rfnsplit(path,fndrive,fndir, fnfile,fnext);
  switch (part)
  {
  case DRIVE:     if (fnflags | DRIVE)      return fndrive;
  case DIRECTORY: if (fnflags | DIRECTORY)  return fndir;
  case FILENAME:  if (fnflags | FILENAME)   return fnfile;
  case EXTENSION: if (fnflags | EXTENSION)  return fnext;
  case FULLNAME:
    if (fnflags & FILENAME)
    {
      if (fnflags & EXTENSION)
        return rstrcat(fnfile,fnext);
      else
        return fnfile;
    }
    break;

  }
  return "";
}
//-------------------------------------------------------------------------------
void getpath(char *fname)
{
  rfnsplit(fname,fndrive,fndir,fnfile,fnext);
  strcpy(fnpath,fndrive);
  strcat(fnpath,fndir);
}

//------------------------------------------------------------------------------
int isnumeric(char *s)
{
  int i=0,l;
  l = strlen(s);
  while (' '==s[i])
    i++;
  if (isalpha(s[i])) return 0;
  while (isdigit(s[i]) || s[i]=='.' ||
    s[i]=='e' || s[i]=='E' ||
    s[i]=='+' || s[i]=='-')
    i++;
  if (i==l) return 1;
  return 0;
}
//------------------------------------------------------------------------------
int isboolean(char *s)
{
  int i=0;
  while (' '==s[i])
    i++;
  if (!rstrcmpi(s+i,"true") ||
      !rstrcmpi(s+i,"false"))
    return 1;
  else
    return 0;
}
//------------------------------------------------------------------------------
int ratoi(char *s)
{
  if (s==NULL) return 0;
  else         return atoi(s);
}
//------------------------------------------------------------------------------
long ratol(char *s)
{
  if (s==NULL) return 0;
  else         return atol(s);
}
//------------------------------------------------------------------------------
float ratof(char *s)
{
  if (s==NULL) return 0.0;
  else         return (float)atof(s);
}
//------------------------------------------------------------------------------
double ratod(char *s)
{
  if (s==NULL) return 0.0;
  else         return atof(s);
}
//------------------------------------------------------------------------------
//_itoa( i, buffer, 10 );
/*char *r_itoa(int i, char *str, int base)
{
_itoa(i,str,base);

}*/
//------------------------------------------------------------------------------
char *trim(char *src)
{
  char *temp;
  ///copy the string to working string
  temp = strdup(src);
  ///set the left and right pointers
  char *left = temp;
  char *right = temp + strlen(temp)-1;
  if (right<left) 
  { 
    delete temp;
    return src;
  }
   ///move the left pointer up to the first non-whitespace or null character
  while(isspace(left[0]) && left[0] != '\0')        left++;
  ///move the right pointer down to the the first non-whitespace or beginning of string
  while(isspace(right[0]) && right != temp)         right--;
  ///cut off all the whitespace on the right side of string
  right[1] = '\0';
  ///copy trimmed string back to source and delete work space
  rstrcpy(src, left);
  delete temp;
  ///return the trimed string
  return src;
}
//------------------------------------------------------------------------------
//append to s1 chars s2..s6
char *rstrcat(char *s1, const char *s2, const char *s3, const char *s4, const char *s5, const char *s6)
{
  if (s1==NULL) return NULL;
  else
  {
    if (s2!=NULL)
      strcat(s1,s2);
    if (s3!=NULL)
      strcat(s1,s3);
    if (s4!=NULL)
      strcat(s1,s4);
    if (s5!=NULL)
      strcat(s1,s5);
    if (s6!=NULL)
      strcat(s1,s6);
  }
  return s1;
}
//------------------------------------------------------------------------------
//copy s2 to s1 and append chars s3..s6
char *rstrcpy(char *s1, const char *s2, const char *s3, const char *s4, const char *s5, const char *s6)
{
  if (s1==NULL) return NULL;
  strcpy(s1,"");
  if (s2!=NULL)
    strcat(s1,s2);
  if (s3!=NULL)
    strcat(s1,s3);
  if (s4!=NULL)
    strcat(s1,s4);
  if (s5!=NULL)
    strcat(s1,s5);
  if (s6!=NULL)
    strcat(s1,s6);
  return s1;
}
//------------------------------------------------------------------------------
char *rstrstr(char *s1, char *s2)
{
  int idx;

  if (s1==NULL || s2==NULL)       
    return NULL;
  char *t1 = strlwr(strdup(s1));
  char *t2 = strlwr(strdup(s2));
  char *result = strstr(t1,t2);
  if (result)                      
    idx = result-t1;
  delete t1;
  delete t2;
  if (result)
    return &s1[idx];
  return NULL;
}
//------------------------------------------------------------------------------
int rstrcmp(const char *s1,const char *s2)
{
  if (s1==NULL && s2!=NULL)       return -1;
  else if (s2==NULL && s1!=NULL)  return 1;
  else if (s1==NULL && s2==NULL)  return 0;
  else return strcmp(s1,s2);
}
//------------------------------------------------------------------------------
int rstrcmpi(const char *s1,const char *s2)
{
  if (s1==NULL && s2!=NULL)       return -1;
  else if (s2==NULL && s1!=NULL)  return 1;
  else if (s1==NULL && s2==NULL)  return 0;
  else return MY_strcmpi(s1,s2);
}
//------------------------------------------------------------------------------
int rstrncmp(const char *s1,const char *s2,const int len)
{
  if (s1==NULL && s2!=NULL)       return -1;
  else if (s2==NULL && s1!=NULL)  return 1;
  else if (s1==NULL && s2==NULL)  return 0;
  else return strncmp(s1,s2,len);
}
//------------------------------------------------------------------------------
int rstrncmpi(const char *s1,const char *s2,const int len)
{
  if (s1==NULL && s2!=NULL)       return -1;
  else if (s2==NULL && s1!=NULL)  return 1;
  else if (s1==NULL && s2==NULL)  return 0;
  else return MY_strncmpi(s1,s2,len);
}
//------------------------------------------------------------------------------
int Copy(char *s,char *d)
{
  long int BLOCK=32768;
  char *line;
  int t;

  line=new char[BLOCK];
  FILE *in,*out;
  in=fopen(s,"rb");
  if (in==NULL) return 0;
  out=fopen(d,"wb");
  if (out==NULL)
  {
    fclose(in);
    return 0;
  }
  do
  {
    t=fread(line,sizeof(char),BLOCK,in);
    fwrite(line,sizeof(char),t,out);
  }
  while (t==BLOCK);
  delete[] line;
  fclose(in);
  fclose(out);
  return 1;
}
//------------------------------------------------------------------------------
// GCD finds the greatest common divisor
int getGCD(int n,int m)
{
  if (n>0)    return getGCD(m % n, n);
  else        return m;
}

int GCD(int n, int m)
{
  if (m<n)
    return getGCD(m,n);
  else
    return getGCD(n,m);
}
//------------------------------------------------------------------------------
// sort *float and also finds the mean min, max, median and standard deviation
void sort(int n,int *idx,float *val,float *mean,float *min,float *max,float *median,float *sd)
{
  float sum,sums,t;
  int i,j,temp;
  if (n==0) return;
  *min=val[0];
  *max=val[0];
  sum=val[0];
  sums=val[0]*val[0];
  for (i=1;i<n;i++)
  {
    if (val[i]<*min) *min=val[i];
    if (val[i]>*max) *max=val[i];
    sum+=val[i];
    sums+=val[i]*val[i];
  }
  *mean=sum/(float) n;
  if (n>2)
  {
    t = (sums-sum*sum/(float)n)/((float)n-1.0);
    if (t<0)
      *sd = 0.0;
    else
      *sd=(float)sqrt(t);
  }
  else
    *sd=-1.0;
  for (i=0;i<n-1;i++)
    for (j=i+1;j<n;j++)
      if (val[idx[i]]>=val[idx[j]])
      {
        temp=idx[i];
        idx[i]=idx[j];
        idx[j]=temp;
      }
      if (n % 2 !=0)
        *median=val[idx[n/2]];
      else
        *median=(val[idx[n/2-1]]+val[idx[n/2]])/(float)2.0;
      return;
}
//------------------------------------------------------------------------------
// sort *double and also finds the mean min, max, median and standard deviation
void FRAMES_API StatSort(int n,int *idx,double *val,double *mean,double *min,double *max,double *median, double *sd)
{
  double sum,sums,t;
  int i,j,temp;
  if (n==0) return;
  *min=val[0];
  *max=val[0];
  sum=val[0];
  sums=val[0]*val[0];
  for (i=1;i<n;i++)
  {
    if (val[i]<*min) *min=val[i];
    if (val[i]>*max) *max=val[i];
    sum+=val[i];
    sums+=val[i]*val[i];
  }
  *mean=sum/(double) n;
  if (n>2)
  {
    t = (sums-sum*sum/(double)n)/((double)n-1.0);
    if (t < 0)
      *sd = 0.0;
    else
      *sd=sqrt(t);
  }
  else
    *sd=-1.0;
  for (i=0;i<n-1;i++)
    for (j=i+1;j<n;j++)
      if (val[idx[i]]>=val[idx[j]])
      {
        temp=idx[i];
        idx[i]=idx[j];
        idx[j]=temp;
      }
      if (n % 2 !=0)
        *median=val[idx[n/2]];
      else
        *median=(val[idx[n/2-1]]+val[idx[n/2]])/2.0;
      return;
}
//-----------------------------------------------------------------------------
int isEqual(float x, float y, float e)
{
  double d = fabs(x-y);
  if (d <= (e*fabs(x)) && d <= (e*fabs(y)))
    return 1;
  else
    return 0;
}

bool AlmostEqual2sComplement(float A, float B, int maxUlps)
// bool isEqual(float A, float B, int maxUlps)
{
    // Make sure maxUlps is non-negative and small enough that the
    // default NAN won't compare as equal to anything.
//    assert(maxUlps > 0 && maxUlps < 4 * 1024 * 1024);
    // Make aInt lexicographically ordered as a twos-complement int
    int aInt = *(int*)&A;
    if (aInt < 0)
        aInt = 0x80000000 - aInt;
    // Make bInt lexicographically ordered as a twos-complement int
    int bInt = *(int*)&B;
    if (bInt < 0)
        bInt = 0x80000000 - bInt;
    // Get Ulp count
    int intDiff = abs(aInt - bInt);
    if (intDiff <= maxUlps)
        return true;
    return false;
}

//-----------------------------------------------------------------------------
int isEqual(double x, double y, double e)
{
  double d = fabs(x-y);
  if (d <= (e*fabs(x)) && d <= (e*fabs(y)))
    return 1;
  else
    return 0;
}
/*
bool AlmostEqual2sComplement(float A, float B, int maxUlps)
{
    // Make sure maxUlps is non-negative and small enough that the
    // default NAN won't compare as equal to anything.
    assert(maxUlps > 0 && maxUlps < 4 * 1024 * 1024);
    // Make aInt lexicographically ordered as a twos-complement int
    int aInt = *(int*)&A;
    if (aInt < 0)
        aInt = 0x80000000 - aInt;
    // Make bInt lexicographically ordered as a twos-complement int
    int bInt = *(int*)&B;
    if (bInt < 0)
        bInt = 0x80000000 - bInt;
    // Get Ulp count
    int intDiff = abs(aInt - bInt);
    if (intDiff <= maxUlps)
        return true;
    return false;
}
*/
//------------------------------------------------------------------------------
float interpolate(float x1, float y1, float x2, float y2, float targetX)
{
  if (isEqual(x1,x2))
    if (isEqual(x1,targetX))      return (y1+y2)*0.5;
    else                          return mMAXFLOAT;
  if (isEqual(y1,y2))             return y1;
  return y1+(y2-y1)*(targetX-x1)/(x2-x1);
}
//------------------------------------------------------------------------------
double interpolate(double x1, double y1, double x2, double y2, double targetX)
{
  if (isEqual(x1,x2))
    if (isEqual(x1,targetX))      return (y1+y2)*0.5;
    else                          return mMAXDOUBLE;
  if (isEqual(y1,y2))             return y1;
  return y1+(y2-y1)*(targetX-x1)/(x2-x1);
}
//------------------------------------------------------------------------------
///interpolate the value at x, y using the values at (listX, listY) return the result
float interpolate(float x, float y, float *listX, float *listY, float *values, int pointCount)
{
  int i;
  float distance;
  float weight;
  float weightValue;
  float totalWeightValue = 0.0;
  float totalWeight = 0.0;

  if(pointCount < 1) return 0.0;
  for(i=0; i < pointCount; i++)
  {
    ///now currX and currY are in cartesian, find difference from x,y
    distance = (float)sqrt(pow(listX[i]-x,2.0) + pow(listY[i]-y,2.0));
    if (distance == 0) return values[i];
    weight = (float)1.0 / ((float)pow(distance, 2.0));
    weightValue = weight * values[i];
    totalWeight += weight;
    totalWeightValue += weightValue;
  }
  return totalWeightValue / totalWeight;
}
//-------------------------------------------------------------------------------------------
double interpolate(double x, double y, double *listX, double *listY, double *values, int pointCount)
{
  int i;
  double distance;
  double weight;
  double weightValue;
  double totalWeightValue = 0.0;
  double totalWeight = 0.0;

  if(pointCount < 1) return 0.0;
  for(i=0; i < pointCount; i++)
  {
    ///now currX and currY are in cartesian, find difference from x,y
    distance = sqrt(pow(listX[i]-x,2.0) + pow(listY[i]-y,2.0));
    if (distance == 0) return values[i];
    weight = 1.0 / (pow(distance, 2.0));
    weightValue = weight * values[i];
    totalWeight += weight;
    totalWeightValue += weightValue;
  }
  return totalWeightValue / totalWeight;
}

// Decay chain processor variables
int *dupFlags;
bool allowDup;
int allowBranch;
char *clist;
char *nlist;
char *d1list;
char *d2list;
_LIST cList;
_LIST nList;
_LIST d1List;
_LIST d2List;
_LIST d3List;

// ???List are comma seperated and should be one to one with cas as primary key
// all degradation products must be listed as parents if not they are ignored
void OpenDecayChain(char *casList, char *nameList, char *degList, char *secList, int branching)
{
  clist = strdup(casList);
  nlist = strdup(nameList);
  d1list = strdup(degList);
  d2list = strdup(secList);
  splitList(clist);
  cList = rList;
  splitList(nlist);
  nList = rList;
  splitList(d1list);
  d1List = rList;
  splitList(d2list);
  d2List = rList;
  dupFlags = new int [(long)cList.size()];
  d3List.clear();
  allowDup = false;
  allowBranch = branching;
}

void CloseDecayChain()
{
  cList.clear();
  nList.clear();
  d1List.clear();
  d2List.clear();
  d3List.clear();
  delete[] dupFlags;
  delete d2list;
  delete d1list;
  delete nlist;
  delete clist;
}

void SetDecayAllowDup(bool flag)
{
  allowDup = flag;
}

void ClearDecayFlags()
{
  for(int i=0; i< (long)cList.size(); i++) dupFlags[i] = 0;
  d3List.clear();
  rstrcpy(dummy,"");
}

void GetDecayChain(int p)
{
  int i;
  if (p >= (long)cList.size()) return;
  dupFlags[p] = 1;

  for (i=0; i< (long)cList.size(); i++)
    if (!rstrcmpi(cList[i], d1List[p]))
    {
      if (!dupFlags[i] || allowDup)
        d3List.push_back(d1List[p]);
      GetDecayChain(i);
      rstrcat(dummy,d1List[p],",");
      break;
    }

  if (allowBranch)
    for (i=0; i< (long)cList.size(); i++)
      if (!rstrcmpi(cList[i], d2List[p]))
      {
        if (!dupFlags[i] || allowDup)
          d3List.push_back(d2List[p]);
        GetDecayChain(i);
        rstrcat(dummy,d1List[p],",");
        break;
      }
}

bool CheckLeap(int year)
{
  if ((year % 4 == 0) && ((year % 100 != 0) || (year % 400 == 0)))
    return true;
  else
    return false;
}

int Date2JulHours(int mn, int dy, int yr, int hr)
{
  int Numdays;
  int feb;

  if (CheckLeap(yr))
    feb = 29;
  else
    feb = 28;

  Numdays = dy - 1;
  if (mn > 1)  Numdays = Numdays + 31;
  if (mn > 2)  Numdays = Numdays + feb;
  if (mn > 3)  Numdays = Numdays + 31;
  if (mn > 4)  Numdays = Numdays + 30;
  if (mn > 5)  Numdays = Numdays + 31;
  if (mn > 6)  Numdays = Numdays + 30;
  if (mn > 7)  Numdays = Numdays + 31;
  if (mn > 8)  Numdays = Numdays + 31;
  if (mn > 9)  Numdays = Numdays + 30;
  if (mn > 10) Numdays = Numdays + 31;
  if (mn > 11) Numdays = Numdays + 30;
  return (Numdays * 24) + hr;
}


int Use(int jhour, int days, int *month)
{
  if (days * 24 < jhour)
  {
    *month = *month + 1;
    return jhour - (days * 24);
  }
  else
    return jhour;
}

void JulHours2Date(int jhr, int yr, int *mn, int *dy, int *hr)
{
  int feb;

  if (CheckLeap(yr))
    feb = 29;
  else
    feb = 28;
  *mn = 1;
  *hr = jhr - 1;
  *hr = Use(*hr, 31, mn);
  *hr = Use(*hr, feb, mn);
  *hr = Use(*hr, 31, mn);
  *hr = Use(*hr, 30, mn);
  *hr = Use(*hr, 31, mn);
  *hr = Use(*hr, 30, mn);
  *hr = Use(*hr, 31, mn);
  *hr = Use(*hr, 31, mn);
  *hr = Use(*hr, 30, mn);
  *hr = Use(*hr, 31, mn);
  *hr = Use(*hr, 30, mn);
  *dy = 1;
  while (*hr >= 24)
  {
    *dy = *dy + 1;
    *hr = *hr - 24;
  }
}
