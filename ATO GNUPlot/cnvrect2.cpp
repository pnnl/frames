#include "cnvrect2.h"

int Memory::on=1;

  void newHandler() throw(xalloc)
  {  mem.Dump("Error:");  }

  void setHandler()
  { set_new_handler(newHandler);  }

Memory mem(NULL,NULL,NULL,__FILE__,__LINE__);  // dummy first element

/*______________________________________________________________________________
class Memory::
  Memory *Pred,*Succ;
  void *Pointer;
  char File[20];
  int  Line;
  static int on;
*/
  void Memory::Dump(char *title)
  {
    if (on)
    {
      Memory *current=this;
      ocsv f("memory.dmp");
      f << title << NewLn;
      while (current!=NULL)
      {
        f << current->File << current->Line << NewLn;
        current=current->Succ;
      }
    }
  }

  void Memory::Off()
  { on=0;  }

  void Memory::On()
  { on=1;  }

  Memory::Memory(void *ref,Memory *p,Memory *s,char *f,int l)
  {
    Pred=p;
    Succ=s;
    Pointer=ref;
    strncpy(File,f,19); File[19]='\0';
    Line=l;
  }

  void Memory::New(void far *p,char *file,int line)
  {
    if (on)
    {
      Memory *temp;
      temp=new Memory(p,this,Succ,file,line);
      if (Succ!=NULL) Succ->Pred=temp;
      Succ=temp;
    }
  }

  void Memory::Deleting(void far *p)
  {
    if (on)
    {
      Memory *current;
      current=Succ;
      while (current!=NULL && current->Pointer!=p)
        current=current->Succ;
      if (current!=NULL && current->Pointer==p)
      {
        if (current->Pred!=NULL) current->Pred->Succ=current->Succ;
        if (current->Succ!=NULL) current->Succ->Pred=current->Pred;
        delete current;
      }
    }
  }

/*______________________________________________________________________________
class ConvertData::
  double CenterX,CenterY,TimeSpan,TimeInterval;
  char Source[30];
  char fname[128];
*/
  ConvertData::ConvertData(char *GIDFilepath,char *Glyph)
  {

    long i,numlines;
    char pname[40];
    icsv *f;

    strcpy(Source,Glyph);

    try
    {
      sprintf(fname,"%s%s.set",GIDFilepath,Glyph);
      f=new icsv(fname,'"',',');
      if (f->ok())
      {
        mem.Newed(f);
        *f >> Source >> numlines >> NewLn;
        if (strcmpi(Source,Glyph)==0)
          for (i=0;i<numlines;i++)
          {
            *f >> pname;
            if (strcmpi(pname,"CenterX")==0)            *f >> CenterX;
            if (strcmpi(pname,"CenterY")==0)            *f >> CenterY;
            if (strcmpi(pname,"TimeSpan")==0)           *f >> TimeSpan;
            if (strcmpi(pname,"TimeInterval")==0)       *f >> TimeInterval;
            *f >> NewLn;
          }
        mem.Deleting(f);
        delete f;
        return;
      }
    }
    catch(...)
    {
      Source[0] = 0;
      fname[0] = 0;
    }

    CenterX = 0.0;
    CenterY = 0.0;
    TimeSpan = 100.0;
    TimeInterval = 10.0;
  }

  ConvertData::~ConvertData()
  {
    ocsv *f;
    try
    {
      f=new ocsv(fname,'"',',');
      *f << Source << 4 << NewLn;
      *f << "CenterX" << CenterX << NewLn;
      *f << "CenterY" << CenterY << NewLn;
      *f << "TimeSpan" << TimeSpan << NewLn;
      *f << "TimeInterval" << TimeInterval << NewLn;
      *f << NewLn;
    }
    catch (...)
    {}
  }
/*______________________________________________________________________________
class ATOContaminant::
  char Type[30],Particle[20],ConName[50],ConId[20],Units[20];
  int NumX,NumY;
  double Time,*X,*Y,**Conc; // Concentration and Deposition Rates are stored in Conc
*/
  ATOContaminant::ATOContaminant()
  {
    NumX=0;
    NumY=0;
    X=NULL;
    Y=NULL;
    Conc=NULL;
    strcpy(ConName,"");
    strcpy(ConId,"");
  }

  void ATOContaminant::Destroy()
  {
    int i;
    if (NumX>0 && NumY>0)
    {
      if (X!=NULL)
      {
        mem.Deleting(X);
        delete[] X;
        X=NULL;
      }
      if (Y!=NULL)
      {
        mem.Deleting(Y);
        delete[] Y;
        Y=NULL;
      }
      if (Conc!=NULL)
      {
        for (i=0;i<NumX;i++)
          if (Conc[i]!=NULL)
          {
            mem.Deleting(Conc[i]);
            delete[] Conc[i];
            Conc[i]=NULL;
          }
        mem.Deleting(Conc); delete[] Conc;
        Conc=NULL;
      }
      NumX=0;
      NumY=0;
    }
  }

  ATOContaminant::~ATOContaminant()
  { Destroy();  }

  long ATOContaminant::Read(icsv &f,char *name,char *id,double time)
  {
    char dummy[20];
    int i,j;

    Destroy();
    Time=time;
    f >> Type >> Particle >> dummy >> Units >> NumX >> dummy >> NumY >> NewLn;
    strcpy(ConName,name);
    strcpy(ConId,id);
    X=new double[NumX]; mem.Newed(X);
    Y=new double[NumY]; mem.Newed(Y);
    Conc=new double*[NumX]; mem.Newed(Conc);
    for (i=0; i<NumX;i++)
    {
      f >> X[i];
      Conc[i]=new double[NumY]; mem.Newed(Conc[i]);
    }
    f >> NewLn;
    for (j=0;j<NumY;j++)
    {
      f >> Y[j];
      for (i=0;i<NumX;i++)
        f >> Conc[i][j];
      f >> NewLn;
    }
    return 1;
  }

  double ATOContaminant::CalcX(double X,double Y,double CenterX,long polar) // km
  {
    if (!polar) return X/1000.0+CenterX;
    else return CenterX+sin(Y*M_PI/180.0)*X/1000.0;
  }

  double ATOContaminant::CalcY(double X,double Y,double CenterY,long polar) // km
  {
    if (!polar) return Y/1000.0+CenterY;
    else return CenterY+cos(Y*M_PI/180.0)*X/1000.0;
  }

  int ATOContaminant::Rad()
  {
    if (strncmpi(Units,"Bq",2)==0) return 1;
    else return 0;
  }

  double ATOContaminant::ChmConc(int i,int j,double min)
  {
    double c=Conc[i][j]*1e6; // Kg/? > mg/?
    if (min==0.0) return c;
    if (c<min) return min;
    return c;
  }

  double ATOContaminant::RadConc(int i,int j,double min)
  {
    double c=Conc[i][j]*1e12/3.7e10; // Bq/? > pCi/?
    if (min==0.0) return c;
    if (c<min) return min;
    return c;
  }

  void ATOContaminant::Write(ocsv &f,int numtimes,ATOContaminant *air,ATOContaminant *dep,
             double CenterX,double CenterY,long polar,int AirConc)
  {
    int i,j,t;
    static double ftperkm=3281.0;
    static char *dunit="ft";
    static char *RadAirUnit="pCi/m^3";
    static char *RadDepUnit="pCi/m^2/yr";
    static char *ChmAirUnit="mg/m^3";
    static char *ChmDepUnit="mg/m^2/yr";
    char AirLabel[60],DepLabel[60];
    f << "Time" << "yrs";
    for (t=0;t<numtimes;t++)
      f << air[t].Time ;
    f << NewLn;
    f << "X" << "Y";
    for (t=0;t<numtimes;t++)
    {
      sprintf(AirLabel,"Air Concentration at t=%.3g yrs",air[t].Time);
      sprintf(DepLabel,"Deposition Rate at t=%.3g yrs",dep[t].Time);
      if (AirConc)
        f << AirLabel;
      else
        f << DepLabel;
    }
    f << NewLn;
    f << dunit << dunit ;
    for (t=0;t<numtimes;t++)
      if (Rad())
        if (AirConc)
          f << RadAirUnit;
        else
          f << RadDepUnit;
      else
        if (AirConc)
          f << ChmAirUnit;
        else
      f << ChmDepUnit;
    f << NewLn;
    for (i=0;i<NumX;i++)
      for (j=0;j<NumY;j++)
      {
        f << CalcX(X[i],Y[j],CenterX,polar)*ftperkm
          << CalcY(X[i],Y[j],CenterY,polar)*ftperkm;
        for (t=0;t<numtimes;t++)
        {
          if (Rad())
            if (AirConc)
              f << air[t].RadConc(i,j);
            else
              f << dep[t].RadConc(i,j);
          else
            if (AirConc)
              f << air[t].ChmConc(i,j);
            else
              f << dep[t].ChmConc(i,j);
        }
      f << NewLn;
    }
  }

  void ATOContaminant::XExtrema(double &min,double &max)
  {
    int i;
    min=X[0];
    max=X[0];
    for (i=1;i<NumX;i++)
    {
      if (X[i]<min) min=X[i];
      if (X[i]>max) max=X[i];
    }
  }

  void ATOContaminant::YExtrema(double &min,double &max)
  {
    int j;
    min=Y[0];
    max=Y[0];
    for (j=1;j<NumY;j++)
    {
      if (Y[j]<min) min=Y[j];
      if (Y[j]>max) max=Y[j];
    }
  }

  void ATOContaminant::ConcExtrema(double &min,double &max)
  {
    int i,j;
    min=Conc[0][0];
    max=Conc[0][0];
    for (i=0;i<NumX;i++)
    for (j=0;j<NumY;j++)
    {
      if (Conc[i][j]<min) min=Conc[i][j];
      if (Conc[i][j]>max) max=Conc[i][j];
    }
  }

  void ATOContaminant::ConcSeriesExtrema(int numtimes,ATOContaminant *a,double &Min,double &Max)
  {
    int t;
    double tmin,tmax;
    for (t=0;t<numtimes;t++)
    {
      a[t].ConcExtrema(tmin,tmax);
      if (t==0)
      {
        Min=tmin;
        Max=tmax;
      }
      else
      {
        if (tmin<Min) Min=tmin;
        if (tmax>Max) Max=tmax;
      }
    }
  }

  void ATOContaminant::WriteGNU(ofstream &d,char *dname,ofstream &o,int numtimes,ATOContaminant *air,ATOContaminant *dep,
                double CenterX,double CenterY,long polar)
  {
    static int count=0;
    int i,j,t;
    static double ftperkm=3281.0;
    static char *RadAirUnit="pCi/m^3";
    static char *RadDepUnit="pCi/m^2/y";
    static char *ChmAirUnit="mg/m^3";
    static char *ChmDepUnit="mg/m^2/y";
    double Min,Max;

    if (NumX==0 || NumY==0) return;
    XExtrema(Min,Max);
    if (polar)
    {
      Min=(CenterX-Max/1000.0)*ftperkm;
      Max=(CenterX+Max/1000.0)*ftperkm;
    }
    else
    {
      Min=(CenterX+Min/1000.0)*ftperkm;
      Max=(CenterX+Max/1000.0)*ftperkm;
    }
    o << "#set xrange [" ;
    o.precision(10);
    o << Min <<":" ;
    o.precision(10);
    o << Max <<"]\n";
    if (polar)
    {
      Min=(CenterY-Max/1000.0)*ftperkm;
      Max=(CenterY+Max/1000.0)*ftperkm;
    }
    else
    {
      YExtrema(Min,Max);
      Min=(CenterY+Min/1000.0)*ftperkm;
      Max=(CenterY+Max/1000.0)*ftperkm;
    }
    o << "#set yrange [" ;
    o.precision(10);
    o << Min <<":" ;
    o.precision(10);
    o << Max <<"]\n";
    // Output air for all time
    //        ConcSeriesExtrema(numtimes,air,Min,Max);
    //        if (Min < 1e-30) Min=1e-30;
    //        o << "set zrange [" << Min <<":"<< Max <<"]\n";
    o << "set zlabel \"Concentration (";
    if (Rad())
      o << RadAirUnit;
    else
      o << ChmAirUnit;
    o << ")\"\n";
    for (t=0;t<numtimes;t++)
    {
      air[t].ConcExtrema(Min,Max);
//      if ((Min>0.0 || Min<0.0) && (Max>0.0 || Max<0.0))
      if (Min!=Max)
      {
        o << "set title \" Air Concentrations at t=" << air[t].Time
          << " for " << ConName << "\"\n";
        o << "set output \'plot" << count << ".gif\'\n";
        o << "splot \"" << dname << "\" index " << count << " notitle\n";
        o << "pause -1 \"Press Enter for next plot\"\n";
        for (i=0;i<NumX;i++)
          for (j=0;j<NumY;j++)
          {
            d << CalcX(X[i],Y[j],CenterX,polar) << " "
              << CalcY(X[i],Y[j],CenterY,polar) << " ";
            if (Rad())
              d << air[t].RadConc(i,j,1e-30);   // Bq/m3 > pCi/m3
            else
              d << air[t].ChmConc(i,j,1e-30);   // Kg/m3 > mg/m3
            d << "\n";
          }
        d << "\n\n";
        count++;
      }
    }

    // Output deposition for all time
    //        ConcSeriesExtrema(numtimes,dep,Min,Max);
    //        if (Min < 1e-30) Min=1e-30;
    //        o << "set zrange [" << Min <<":"<< Max <<"]\n";
    o << "set zlabel \"Dep. Rate (";
    if (Rad())
      o << RadDepUnit;
    else
      o << ChmDepUnit;
    o << ")\"\n";
    for (t=0;t<numtimes;t++)
    {
      dep[t].ConcExtrema(Min,Max);
//      if ((Min>0.0 || Min<0.0) && (Max>0.0 || Max<0.0))
      if (Min!=Max)
      {
        o << "set title \"Deposition Rates at t=" << air[t].Time
          << " for " << ConName << "\"\n";
        o << "splot \"" << dname << "\" index " << count << " notitle\n";
        o << "pause -1 \"Press Enter for next plot\"\n";
        for (i=0;i<NumX;i++)
          for (j=0;j<NumY;j++)
          {
            d << CalcX(X[i],Y[j],CenterX,polar) << " "
              << CalcY(X[i],Y[j],CenterY,polar) << " ";
            if (Rad())
              d << dep[t].RadConc(i,j,1e-30);   // Bq/m3 > pCi/m3
            else
              d << dep[t].ChmConc(i,j,1e-30);   // Kg/m3 > mg/m3
            d << "\n";
          }
        d << "\n\n";
        count++;
      }
    }
  }

  long ATOContaminant::operator ==(ATOContaminant &a)
  {
    if (strcmp(a.Type,Type)==0) return 1;
    else return 0;
  }

  ATOContaminant & ATOContaminant::operator +=(ATOContaminant &a)
  {
    int i,j;
    if ((*this)==a)
      for (i=0;i<NumX;i++)
        for (j=0;j<NumY;j++)
          Conc[i][j]+=a.Conc[i][j];
    return *this;
  }

  ATOContaminant & ATOContaminant::operator /=(double divisor)
  {
    int i,j;
    if (divisor!=0.0)
    {
      for (i=0;i<NumX;i++)
        for (j=0;j<NumY;j++)
          Conc[i][j]/=divisor;
    }
    return *this;
  }

  ATOContaminant & ATOContaminant::operator =(ATOContaminant &a)
  {
    int i,j;
    Destroy();
    strcpy(Type,a.Type);
    strcpy(Particle,a.Particle);
    strcpy(ConName,a.ConName);
    strcpy(ConId,a.ConId);
    strcpy(Units,a.Units);
    NumX=a.NumX;
    NumY=a.NumY;
    Time=a.Time;
    X=new double[NumX]; mem.Newed(X);
    Y=new double[NumY]; mem.Newed(Y);
    Conc=new double*[NumX]; mem.Newed(Conc);

    for (i=0; i<NumX;i++)
    {
      X[i]=a.X[i];
      Conc[i]=new double[NumY]; mem.Newed(Conc[i]);
    }
    for (j=0;j<NumY;j++)
      Y[j]=a.Y[j];
    for (i=0;i<NumX;i++)
      for (j=0;j<NumY;j++)
        Conc[i][j]=a.Conc[i][j];
    return *this;
  }

  long ATOContaminant::Empty()
  {
    if (NumX==0 || NumY==0) return 1;
    else return 0;
  }

  long ATOContaminant::AirConc()
  { return (strcmp(Type,"Air Concentration")==0);  }

  void ATOContaminant::Integrate(ATOContaminant &a)
  {
    int i,j;
    for (i=0;i<NumX;i++)
      for (j=0;j<NumY;j++)
        Conc[i][j]=Time*Conc[i][j];
  }

  void ATOContaminant::Integrate(ATOContaminant &total,ATOContaminant &a,ATOContaminant &b)
  {
    int i,j;
    for (i=0;i<NumX;i++)
      for (j=0;j<NumY;j++)
        Conc[i][j]=total.Conc[i][j]+
                   0.5*(b.Time-a.Time)*(b.Conc[i][j]+a.Conc[i][j]);
  }

  double ATOContaminant::getTime()
  { return Time;  }

  double ATOContaminant::Interp(double y1,double y2,double x1,double x2,double x)
  {
    if (x2==x1) return (y1+y2)/2.0;
    return y1+(y2-y1)*(x-x1)/(x2-x1);
  }

  void ATOContaminant::Average(double start,double end,int numtimes,ATOContaminant *values)
  {
    int t,i,j;
    double Cs,Ce;

    Destroy();
    if (numtimes==0) return;
    strcpy(Type,values[0].Type);
    strcpy(Particle,values[0].Particle);
    strcpy(ConName,values[0].ConName);
    strcpy(ConId,values[0].ConId);
    strcpy(Units,values[0].Units);
    NumX=values[0].NumX;
    NumY=values[0].NumY;
    Time=(start+end)/2.0;
    X=new double[NumX]; mem.Newed(X);
    Y=new double[NumY]; mem.Newed(Y);
    Conc=new double*[NumX]; mem.Newed(Conc);
    for (i=0; i<NumX;i++)
    {
      X[i]=values[0].X[i];
      Conc[i]=new double[NumY]; mem.Newed(Conc[i]);
      for (j=0;j<NumY;j++)
        Conc[i][j]=0.0;
    }
    for (j=0;j<NumY;j++)
      Y[j]=values[0].Y[j];
    if (numtimes==1) return; // can't integrate a single point
    for (t=1;t<numtimes;t++)
      for (i=0;i<NumX;i++)
        for (j=0;j<NumY;j++)
          if (start <= values[t-1].Time && values[t].Time <= end)
          // entire period needs to be integrated
              Conc[i][j]+=0.5*(values[t-1].Conc[i][j]+values[t].Conc[i][j])*
                              (values[t].Time-values[t-1].Time);
          else if (values[t-1].Time <= start && end <= values[t].Time)
          // time interval completely in this period
              {
                Cs=Interp(values[t-1].Conc[i][j],values[t].Conc[i][j],
                          values[t-1].Time,      values[t].Time, start);
                Ce=Interp(values[t-1].Conc[i][j],values[t].Conc[i][j],
                          values[t-1].Time,      values[t].Time, end);
                Conc[i][j]+=0.5*(Cs+Ce)*(end-start);
              }
          else if (values[t-1].Time <= start && start <= values[t].Time)
          // start time is in this period
              {
                Cs=Interp(values[t-1].Conc[i][j],values[t].Conc[i][j],
                          values[t-1].Time,      values[t].Time, start);
                Conc[i][j]+=0.5*(Cs+values[t].Conc[i][j])*(values[t].Time-start);
              }
          else if (values[t-1].Time <= end && end <= values[t].Time)
          // end time is in this period
              {
                Ce=Interp(values[t-1].Conc[i][j],values[t].Conc[i][j],
                          values[t-1].Time,      values[t].Time, end);
                Conc[i][j]+=0.5*(values[t-1].Conc[i][j]+Ce)*(end-values[t-1].Time);
              }
     this->operator /=(end-start);
  }

/*______________________________________________________________________________
class ATOEntry::
private:
  int NumTimes;
  ATOContaminant **result;
public:
  char PConName[50];
  char PConId[20];
  char ConName[50];
  char ConId[20];
*/

  ATOEntry::ATOEntry()
  {
    NumTimes=0;
    result=NULL;
  }

  void ATOEntry::Destroy()
  {
    if (result!=NULL)
    {
      mem.Deleting(result[0]); delete[] result[0];
      mem.Deleting(result[1]);delete[] result[1];
      mem.Deleting(result);delete[] result;
    }
  }

  ATOEntry::~ATOEntry()
  { Destroy();  }

  void ATOEntry::Read(icsv &f,char *pname,char *pid,char *name,char *id,int numtimes)
  {
    static int MaxTimes=20;
    int i,j,numresult;
    double time;
    ATOContaminant temp;
    char dummy[20];
    Destroy();
    NumTimes=numtimes;
    if (numtimes>MaxTimes) NumTimes=MaxTimes;
    strcpy(PConName,pname);
    strcpy(PConId,pid);
    strcpy(ConName,name);
    strcpy(ConId,id);
    result=new ATOContaminant*[2]; mem.Newed(result);
    result[0]=new ATOContaminant[NumTimes]; mem.Newed(result[0]); // Air Concentrations
    result[1]=new ATOContaminant[NumTimes]; mem.Newed(result[1]);// Deposition Concentrations
    for (i=0;i<numtimes;i++)
    {
      f >> time >> dummy >> numresult >> NewLn;
      for (j=0;j<numresult;j++)
      {
        temp.Read(f,ConName,ConId,time);
        if (i<NumTimes)
        {
          if (temp.AirConc() && result[0][i].Empty())
            result[0][i]=temp;
          else if (temp.AirConc())
            result[0][i]+=temp;
          else if (result[1][i].Empty()) // must be deposition
            result[1][i]=temp;
          else
            result[1][i]+=temp;
        }
      }
    }
  }

  void ATOEntry::IntegrateDeposition()
  {
    int i;
    ATOContaminant *temp;
    temp=new ATOContaminant[NumTimes]; mem.Newed(temp);
    temp[0]=result[1][0];
    temp[0].Integrate(result[1][0]);  // Multiply by Time
    for (i=1;i<NumTimes;i++)
    {
      temp[i]=result[1][i];
      temp[i].Integrate(temp[i-1],result[1][i-1],result[1][i]);
      result[1][i-1]=temp[i-1];  // Copy integrated value into results
    }
    result[1][i-1]=temp[i-1];  // Copy Last value
    mem.Deleting(temp); delete[] temp;
  }

  void ATOEntry::Write(ocsv &air,ocsv &dep,double cx,double cy,long gridtype)
  {
    air << PConName << PConId << ConName << ConId << NumTimes << NewLn;
    dep << PConName << PConId << ConName << ConId << NumTimes << NewLn;
    result[0][0].Write(air,NumTimes,result[0],result[1],cx,cy,gridtype,1);
    result[0][0].Write(dep,NumTimes,result[0],result[1],cx,cy,gridtype,0);
  }

  void ATOEntry::WriteGNU(ofstream &data,char *dname,ofstream &script,double cx,double cy,long gridtype)
  {
    result[0][0].WriteGNU(data,dname,script,NumTimes,result[0],result[1],cx,cy,gridtype);
  }

  void ATOEntry::Average(double timespan,double timeinterval,ATOEntry *instantaneous)
  {
    double start,end,adder;
    int t;
    NumTimes=(int)timespan/timeinterval+1;
    strcpy(PConName,instantaneous->PConName);
    strcpy(PConId,instantaneous->PConId);
    strcpy(ConName,instantaneous->ConName);
    strcpy(ConId,instantaneous->ConId);
    result=new ATOContaminant*[2]; mem.Newed(result);
    result[0]=new ATOContaminant[NumTimes]; mem.Newed(result[0]); // Air Concentrations
    result[1]=new ATOContaminant[NumTimes]; mem.Newed(result[1]);// Deposition Concentrations
    for (t=0;t<NumTimes;t++)
    {
      if (t==0) adder=1.0;  // 1 to 71 for first time
      else      adder=0.0;
      start=(double)t*timeinterval;
      end=start+timeinterval;
      //Atmospheric Concentration
      result[0][t].Average(start+adder,end+adder,
                           instantaneous->NumTimes,
                           instantaneous->result[0]);
      //Deposited Concentration
      result[1][t].Average(start+adder,end+adder,
                           instantaneous->NumTimes,
                           instantaneous->result[1]);
    }
  }

/*______________________________________________________________________________
class Grid::
double CX,CY;
int Type;
char DName[128];
ocsv *air,*dep;
ofstream *d,*s;
*/

  Grid::Grid(char *GridFilePath,double cx,double cy,char *gridtype)
  {
    air=new ocsv("AirConc.csv"); mem.Newed(air);
    dep=new ocsv("Deposit.csv"); mem.Newed(dep);
    Type=(strcmpi(gridtype,"polar")==0);
    CX=cx;
    CY=cy;
    sprintf(DName,"gnuplot.scp");
    s=new ofstream(DName); mem.Newed(s);
    *s << "# GNU Plot script file\n";
    *s << "set terminal windows\n";
    *s << "set zero 1e-30\n";
    *s << "set parametric\n";
    *s << "set data style lines\n";
    *s << "set dgrid3d 32,32,8\n";
    *s << "set surface\n";
    *s << "set nohidden\n";
    *s << "set contour\n";
    *s << "set logscale z 10\n";
    *s << "set output\n";
    *s << "set xlabel \"X (km)\"\n";
    *s << "set ylabel \"Y (km)\"\n";
    *s << "#set zrange[:]\n";
    sprintf(DName,"gnuplot.dat");
    d=new ofstream(DName); mem.Newed(d);
  }

  Grid::~Grid()
  {
    s->close();
    d->close();
    mem.Deleting(air); delete air;
    mem.Deleting(dep); delete dep;
    mem.Deleting(s); delete s;
    mem.Deleting(d); delete d;
  }

  void Grid::Write(ATOEntry &ae)
  { ae.Write(*air,*dep,CX,CY,Type);  }

  void Grid::WriteGNU(ATOEntry &ae)
  { ae.WriteGNU(*d,DName,*s,CX,CY,Type);  }

/*______________________________________________________________________________
class ATO::
private:
  int NumDataSets,NumCon,NumProg,NumTimes,CurrentCon,CurrentProg,Ok;
  double CurrentTime;
  char ResultType[7],CoordinateType[9];
  icsv *f;
public:
  char PConName[50];
  char PConId[20];
*/


  ATO::ATO(char *ATOFilepath,char *Glyph)
  {
    int i,numparticles,numheader;
    long numlines;
    char temp[128],tglyph[20],dummy[20];
    Ok=0;
    sprintf(temp,"%s.ato",ATOFilepath);
    f = new icsv(temp,'"',',');
    if (f->ok())
    {
      mem.Newed(f);
      do
      {
        *f >> tglyph >> numlines >> NewLn;
        if (strcmpi(tglyph,Glyph)!=0)
        {
          for (i=0;i<numlines;i++)
            *f >> NewLn;
        }
      }
      while (strcmpi(tglyph,Glyph)!=0  && !f->eof());
      if (strcmpi(tglyph,Glyph)==0)
      {
        *f >> numheader >> NewLn;
        for (i=0;i<numheader;i++)
        *f >> NewLn;
        *f >> NumDataSets >> NewLn;
        *f >> numparticles >> NewLn;
        for (i=0;i<numparticles;i++)
          *f >> NewLn;
        *f >> ResultType >> CoordinateType >> dummy >> NumCon >> NewLn;
        CurrentCon=0;
        CurrentProg=0;
        Ok=1;
      }
    }  
  }

  ATO::~ATO()
  { mem.Deleting(f); delete f;  }

  long ATO::Read(ATOEntry &ae)
  {
    char conname[50],conid[20];
    if (!Ok) return 0;
    if (f->eof()) return 0;
    if (CurrentCon==NumCon && CurrentProg==0) return 0;
    if (CurrentProg==0)
    {
      *f >> PConName >> PConId >> NumTimes >> NumProg >> NewLn;
      strcpy(conname,PConName);
      strcpy(conid,PConId);
    }
    else
      *f >> conname >> conid >> NumTimes >> NewLn;
    ae.Read(*f,PConName,PConId,conname,conid,NumTimes);
    if (CurrentProg<NumProg) CurrentProg++;
    else  { CurrentCon++; CurrentProg=0;}
    return 1;
  }

  char *ATO::GridType()
  {
    if (!Ok) return 0;
    return CoordinateType;
  }

  int ATO::NumberConstituent()
  { return NumCon;  }

  int ATO::CurrentConstituent()
  { return CurrentCon;  }

