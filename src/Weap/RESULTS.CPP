#include "results.h"

/*
class Results
{
 private:
  float RegionA,RegionB,RegionC;
  float	RegionD,RegionE,RegionF;
  TimeSeries CvsD->;
  void Regions(TimeSeries *S,ToxData *T);
 public:
  Results();
  void BreakLC50(TimeSeries *TS,ToxData *TX);
  void CalcRegions(ConData *tox,ConSeries *CS,char *Name,int ConNum);
  void WriteProb(fcsv resout);
  void WriteRegions(fcsv resout,char *Id,char *Name);
};
*/


void Results::SumRegions(int cnt, double y, Point **lclist, ToxData *T, double *A, double *B, double *C, double *D )
{
  int above;
  double d1,d2;

  if (lclist[0]->y > y)
    above = 1;
  else
    above = 0;

  for (int i=1; i<cnt; i++)
  {
    if (lclist[i-1]->x < T->AcuteTime && T->AcuteTime <= lclist[i]->x)
    {
      d1 = T->AcuteTime - lclist[i-1]->x;             // some acute
      d2 = lclist[i]->x - T->AcuteTime;               // some chronic
    }
    else
    {
      if (lclist[i]->x < T->AcuteTime)
      {
        d1 = lclist[i]->x - lclist[i-1]->x;          // all acute
        d2 = 0;                                      // no chronic
      }
      else if (lclist[i]->x > T->AcuteTime)
      {
        d1 = 0;                                      // no acute
        d2 = lclist[i]->x - lclist[i-1]->x;          // all chronic
      }
    }
    if (above)
    {
      *A += d1;
      *B += d2;
    }
    else
    {
      *C += d1;
      *D += d2;
    }
    above = !above;
  }
}

void Results::Regions(Series *S,ToxData *T)
{
  int i,j,cnt;
  double conc,step, C, D;
  Point *b, *cc, **lclist;
  Segment *seg, *cccseg, **curve;

  cc = NULL;

  step = S->MaxY() / resolution;
  DurationTotal = S->MaxX() - S->MinX();

  conc = 0;
  if (CvsD != NULL) delete CvsD;
  CvsD = new Series();
  CvsD->Init(resolution+1);
  for (i=0; i<=resolution; i++, conc+=step)                                     // create concentration Vs duration curve
  {
    CvsD->xValues[resolution-i] = S->ExceedDelta(conc,0,S->count-1);
    CvsD->yValues[resolution-i] = conc;
  }

  CvsD->PrintSeries();
  S->PrintSeries();
  lclist = new Point *[T->NumLC+2];
  curve = new Segment *[T->NumLC-1];
  for (j=0; j < T->NumLC-1; j++)
    curve[j] = new Segment(T->LCdur[j],T->LCconc[j],T->LCdur[j+1],T->LCconc[j+1]);



  cnt=0;
  lclist[cnt] = new Point(0,S->MaxY());
  cnt++;
  for (i=0; i<resolution-1; i++)
  {
    seg = new Segment(CvsD->xValues[i],CvsD->yValues[i],CvsD->xValues[i+1],CvsD->yValues[i+1]);
    for (j=0; j < T->NumLC-1; j++)
    {
      b = seg->Intersect(curve[j]);                                             // find if LC intersects segment
      if (b!=NULL && lclist[cnt-1]->x < b->x)                                   // keeps from getting duplicate
      {
        lclist[cnt] = new Point(b->x,b->y);
        cnt++;
      }
    }
    delete seg;
    seg = NULL;
  }
  lclist[cnt] = new Point(S->MaxX(),0);
  cnt++;
  SumRegions(cnt,T->LCconc[0],lclist,T,&RegionA,&RegionB,&RegionC,&RegionD);
  for (i=0; i<cnt; i++)  delete lclist[i];

  cccseg = new Segment(0,T->CCCvalue,DurationTotal,T->CCCvalue);
  cnt=0;
  lclist[cnt] = new Point(0,S->MaxY());
  cnt++;
  for (i=0; i<resolution-1; i++)
  {
    seg = new Segment(CvsD->xValues[i],CvsD->yValues[i],CvsD->xValues[i+1],CvsD->yValues[i+1]);
    b = seg->Intersect(cccseg);                                                 // find if CCC intersects segment
    if (b!=NULL && lclist[cnt-1]->x < b->x)                                     // keeps from getting duplicate
    {
      lclist[cnt] = new Point(b->x,b->y);
      cnt++;
    }
    delete seg;
    seg = NULL;
  }
  lclist[cnt] = new Point(S->MaxX(),0);
  cnt++;
  SumRegions(cnt,T->CCCvalue,lclist,T,&C,&D,&RegionE,&RegionF);
  RegionC -= RegionE;
  RegionD -= RegionF;

  for (i=0; i<cnt; i++)  delete lclist[i];

  RegionA=RegionA*100/DurationTotal;
  RegionB=RegionB*100/DurationTotal;
  RegionC=RegionC*100/DurationTotal;
  RegionD=RegionD*100/DurationTotal;
  RegionE=RegionE*100/DurationTotal;
  RegionF=RegionF*100/DurationTotal;

  for (i=0; i<T->NumLC-1; i++)
    delete curve[i];

  delete cc;
  delete cccseg;
  delete[] curve;
  delete[] lclist;
}

Results::~Results()
{
  if (CvsD != NULL) delete CvsD;
}

Results::Results()
{
  DurationTotal = 0;
  RegionA = 0;  RegionB = 0;  RegionC = 0;
  RegionD = 0;  RegionE = 0;  RegionF = 0;
  CvsD = NULL;
}

void Results::CalcRegions(ConData *ctox,ConSeries *CS,char *IDName,int ConNum)
{
  int Si;

  DurationTotal = 0;
  RegionA = 0;  RegionB = 0;  RegionC = 0;
  RegionD = 0;  RegionE = 0;  RegionF = 0;
  Si = CS->GetIndex(IDName);
  if (Si>=0)
  {
    Regions(&(CS->Con[Si]),&(ctox->Con[ConNum]));
  }
  else
  {
   /* what to do if con data does not have this constituent */
  }
}

void Results::WriteProb(fcsv *resout)
{
  resout->write(resolution+1);
  resout->write("g/ml");
  resout->write("%");

//  resout->write("Concentration in g/ml");
//  resout->write("Probability of Equaling or Exceeding a Concentration, based on Exposure Duration (%)");
//  resout->write(resolution+1);

  resout->writeln();
  for (int i=resolution; i>=0; i--)
  {
    resout->write(CvsD->yValues[i]);
    resout->write(CvsD->xValues[i]/DurationTotal*100,"%3.1f");
    resout->writeln();
  }
}

void Results::WriteRegions(fcsv *resout,char *Id,char *Name, char *effect)
{
  char temp[1024];

  resout->write(Id);
  resout->write(Name);
  resout->write(6);
  resout->writeln();
  resout->write(RegionA);
  sprintf(temp,"%s Time Exceeding %s for Acute Exposure", "%",  effect);
  resout->write(temp);
  resout->writeln();
  resout->write(RegionB);
  sprintf(temp,"%s Time Exceeding %s for Chronic Exposure", "%", effect);
  resout->write(temp);
  resout->writeln();
  resout->write(RegionC);
  sprintf(temp,"%s Time Exceeding Acceptable Limit but Below %s for Acute Exposure", "%", effect);
  resout->write(temp);
  resout->writeln();
  resout->write(RegionD);
  sprintf(temp,"%s Time Exceeding Acceptable Limit but Below %s for Chronic Exposure", "%", effect);
  resout->write(temp);
  resout->writeln();
  resout->write(RegionE);
  resout->write("% Time for No Adverse Acute Impact");
  resout->write(temp);
  resout->writeln();
  resout->write(RegionF);
  resout->write("% Time for No Adverse Chronic Impact");
  resout->writeln();
}
