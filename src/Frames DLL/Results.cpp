/*______________________________________________________________________________

   Date:       1993 - 2004
   Company:    Pacific Northwest National Laboratories
               Battelle Corporation
________________________________________________________________________________
__Modifiication  History________________________________________________________

  DATE     WHO  DESCRIPTION
______________________________________________________________________________*/

#include "results.h"
#include "series.h"

//------------------------------------------------------------------------------
int Result::read(GIDFILE *g,int count)
{
  int i;
  char temp[LARGESTRING];

  firstIteration = true;
  if (!readStr(g,"suOutAlias",&Alias,count)) return 0;
  if (!readStr(g,"suOutDes",&Des,count)) return 0;
  if (!readStr(g,"suOutExt",&ext,count)) return 0;
  if (!readStr(g,"suOutType",&Type,count)) return 0;
  if (!readStr(g,"suOutTime",&Time,count)) return 0;
  if (!readStr(g,"suOutSourceID",&Source,count)) return 0;
  if (!readStr(g,"suOutCASID",&CASID,count)) return 0;
  if (!readStr(g,"suOutParentID",&ParentID,count)) return 0;
  if (!strlen(ParentID))
  {
    delete[] ParentID;
    ParentID = strdup(CASID);
  }
  if (!readStr(g,"suOutOrgID",&OrgID,count)) return 0;
  if (!readInt(g,"suOutTimePtCnt",&NumYears,count)) return 0;
  if (NumYears>0) Years = new double[NumYears];
  for (i = 0; i<NumYears; i++)
    if (!readDouble(g,"suOutTimePt",&(Years[i]),count,i+1)) return 0;

  NumValues = 0;
  if (!rstrcmpi(Time,"peak"))
  {
    NumValues = 2;
    Labels = new char*[NumValues];
    sprintf(temp,"%s peak time",Type);
    Labels[0] = strdup(temp);
    sprintf(temp,"%s peak value",Type);
    Labels[1] = strdup(temp);
  }
  else if (!rstrcmpi(Time,"average years # to #"))
  {
    NumValues = 1;
    Labels = new char*[NumValues];
    sprintf(temp,"%s average value over time interval %.2lf to %.2lf",Type,Years[0],Years[1]);
    Labels[0] = strdup(temp);
  }
  else if (!rstrcmpi(Time,"at year(s) #(,#...)"))
  {
    NumValues = NumYears;
    Labels = new char*[NumValues];
    for (i = 0; i<NumYears; i++)
    {
      sprintf(temp,"%s value at time %lf",Type,Years[i]);
      Labels[i] = strdup(temp);
    }
  }
  if (NumValues > 0) Values = new double[NumValues];

  if (!rstrcmpi(ext,"twi")) kind = 6;
  else if (!rstrcmpi(ext,"ato")) kind = 5;
  else if (!rstrcmpi(ext,"epf")) kind = 4;
  else if (!rstrcmpi(ext,"rif")) kind = 3;
  else if (!rstrcmpi(ext,"hif")) kind = 2;
  else if (!rstrcmpi(ext,"bbf")) kind = 1;
  else if (!rstrcmpi(ext,"aff")) kind = 1;
  else if (!rstrcmpi(ext,"wff")) kind = 1;
  else if (!rstrcmpi(ext,"wcf")) kind = 1;
  else if (!rstrcmpi(ext,"scf")) kind = 1;
  else return 0;
  return 1;
}
//------------------------------------------------------------------------------
void Result::reset()
{
  //all of these items are read each iteration
  if (next) next->reset();
  if (qual) delete qual;
  if (consumer) delete consumer;
  if (producer) delete producer;
  qual = NULL;
  consumer = NULL;
  producer = NULL;

  if (dtime) delete[] dtime;
  if (dvalue) delete[] dvalue;
  if (xunit) delete xunit;
  if (yunit) delete yunit;

  num = 0;
  dtime = NULL;
  dvalue = NULL;
  xunit = NULL;
  yunit = NULL;
}
//------------------------------------------------------------------------------
Result::Result()
{
  Alias = NULL;
  Des = NULL;
  Type = NULL;
  Time = NULL;
  Source = NULL;
  CASID = NULL;
  ParentID = NULL;
  OrgID = NULL;
  NumYears = 0;
  Years = NULL;

  NumValues = 0;
  Values = NULL;
  Labels = NULL;

  ext = NULL;
  qual = NULL;
  consumer = NULL;

  num = 0;
  dtime = NULL;
  dvalue = NULL;
  xunit = NULL;
  yunit = NULL;

  home = this;
  next = NULL;
}
//------------------------------------------------------------------------------
Result::Result(Result *head)
{
  Des = strdup(head->Des);
  Alias = head->Alias;
  Type = head->Type;
  Time = head->Time;
  Source = head->Source;
  CASID = head->CASID;
  ParentID = head->ParentID;
  OrgID = head->OrgID;
  NumYears = head->NumYears;
  Years = head->Years;

  ext = head->ext;
  qual = NULL;
  consumer = NULL;
  producer = NULL;

  num = 0;
  dtime = NULL;
  dvalue = NULL;
  xunit = NULL;
  yunit = NULL;

  home = head;
  next = NULL;
  NumValues = head->NumValues;
  try  {  Values = new double[NumValues];  }
  catch(...)  {  printf("Could not create result class values array!");   }
  try
  {
    Labels = new char *[NumValues];
    for (int i=0; i<NumValues; i++)
      Labels[i] = strdup(head->Labels[i]);
  }
  catch(...)  {  printf("Could not create result class labels array!");   }
}
//------------------------------------------------------------------------------
Result::~Result()
{
  int i;
  if (next) delete next;

  if (this == home)
  {
    //only first instance has the memory allocated
    // all other in the next list point back to home
    if (Alias)    delete[] Alias;
    if (ext)      delete[] ext;
    if (Type)     delete[] Type;
    if (Time)     delete[] Time;
    if (Source)   delete[] Source;
    if (CASID)    delete[] CASID;
    if (ParentID) delete[] ParentID;
    if (OrgID)    delete[] OrgID; //Orgin or Organism
    if (Years)    delete[] Years;
  }
  if (Des)      delete[] Des;
  if (qual)     delete[] qual;
  if (consumer) delete[] consumer;
  if (producer) delete[] producer;
  if (dtime)    delete[] dtime;
  if (dvalue)   delete[] dvalue;
  if (xunit)    delete[] xunit;
  if (yunit)    delete[] yunit;
  if (Values)   delete[] Values;
  for (i = 0; i<NumValues; i++)
    delete Labels[i];
  if (Labels)   delete[] Labels;

  Alias = NULL;
  Des = NULL;
  Type = NULL;
  Time = NULL;
  Source = NULL;
  CASID = NULL;
  ParentID = NULL;
  OrgID = NULL;
  NumYears = 0;
  Years = NULL;
  NumValues = 0;
  Labels = NULL;
  Values = NULL;
  ext = NULL;
  qual = NULL;
  consumer = NULL;
  producer = NULL;
  num = 0;
  dtime = NULL;
  dvalue = NULL;
  xunit = NULL;
  yunit = NULL;

  home = NULL;
  next = NULL;
}
//------------------------------------------------------------------------------
void Result::writeSUFLabels(fcsv *f)
{
  int i;
  char temp[MAXSTRING];

  for (i = 0; i<NumValues; i++)
  {
    if (consumer != NULL)
      sprintf(temp,"%s.%s.%s %s",Alias,consumer,qual,Labels[i]);
    else
      sprintf(temp,"%s.%s.%s %s",Alias,producer,qual,Labels[i]);
    f->write(temp);
  }
  if (next)
    next->writeSUFLabels(f);
}
//------------------------------------------------------------------------------
void Result::writeSUFValues(fcsv *f)
{
  int i;
  for (i = 0; i<NumValues; i++)
    f->write(Values[i]);
  if (next)
    next->writeSUFValues(f);
}
//------------------------------------------------------------------------------
void Result::computeResults()
{
  int j;
  Series *S1 = new Series();
  S1->count = num;
  S1->xValues = dtime;
  S1->yValues = dvalue;
  if (!rstrcmpi(Time, "peak"))
  {
    S1->MaxYwithX(&Values[0], &Values[1]);
  }
  else if (!rstrcmpi(Time,"average years # to #"))
  {
    Values[0] = S1->Average(Years[0], Years[1]);
  }
  else               // series of values
    for (j = 0; j<NumYears; j++)
    {
      Values[j] = S1->InterpY(Years[j]);
    }
  firstIteration = false;
  S1->count = 0;
  S1->xValues = NULL;
  S1->yValues = NULL;
  delete S1;
}
//------------------------------------------------------------------------------
int Result::ProcessPDCF(char *fuiname)
{
  int i,numSet;
  bool ok;
  char temp1[LARGESTRING];
  Result *res;
  Result *curr;
  icsv *inf;
  GIDFILE *GID;

  // already found value must reset to get new values
  if (num>0) return num;

  sprintf(temp1,"%s.%s",fuiname,ext);
  if (kind == 6)
  {
    GID = Open_GID(temp1);
    if (GID != NULL)
    {
      if (Load_GIDSection(GID, Source))
        if (!readInt(GID, "numMedia", &numSet,1,1,1))
        {
          writeError("Error reading",temp1);
          delete GID;
          return 0;
        }
    }
    else
    {
      writeError("Error opening",temp1);
      return 0;
    }
  }
  else
  {
    inf = new icsv(temp1,'\"');
    if (!inf->ok())
    {
      writeError("Error opening",temp1);
      delete inf;
      return 0;
    }
    if (0 != inf->SeekSection(Source))
    {
      writeError(Source,"data not found in",temp1);
      delete inf;
      return 0;
    }
    *inf >> numSet >> NewLn;
  }

  curr = this;
  for (i = 0; i<numSet; i++)
  {
    sprintf(temp1,"Read Error: \n"
                  "  Type = \"%s\""
                  "  Time = \"%s\""
                  "  Source = \"%s\""
                  "  CASID = \"%s\""
                  "  ParentID = \"%s\""
                  "  OrgID = \"%s\"", Type,Time,Source,CASID,ParentID,OrgID);
    if (i != 0)
    {
      try
      { if (curr->next == NULL) curr->next = new Result(home); }
      catch(...)
      { writeError("Unable to create new result!"); }
      curr = curr->next;
    }

    ok = false;
    switch (kind)
    {
      case 1: ok = curr->CFReadSet(inf); break;
      case 2: res = curr->HIFReadSet(inf);
              if (res) { curr = res;  ok = true; }
              break;
      case 3: res = curr->RIFReadSet(inf);
              if (res) { curr = res;  ok = true; }
              break;
      case 4: ok = curr->EPFReadSet(inf); break;
      case 5: ok = curr->ATOReadSet(inf); break;
      case 6: ok = curr->TWIReadSet(GID,i+1); break;
    }
    if (!ok)  writeError(temp1);
  }

  if (kind == 6)
    Close_GID(GID);
  else
    delete inf;

  return num;
}
//------------------------------------------------------------------------------

