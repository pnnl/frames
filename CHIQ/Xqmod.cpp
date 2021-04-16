#pragma hdrstop
#include <conio.h>
#include "..\Common Files\frames.h"
#include "..\Common Files\gid.h"
#include "..\Common Files\robust.h"
#include "error.h"
#include <except.h>
#include <condefs.h>

//---------------------------------------------------------------------------
USEUNIT("..\Common Files\gid.cpp");
USEUNIT("..\Common Files\Fcsv.cpp");
USEUNIT("..\Common Files\robust.cpp");
USEUNIT("..\Common Files\ERROR.CPP");
USELIB("..\Common Files\frames.lib");
USERES("Xqmod.res");
//---------------------------------------------------------------------------
#pragma argsused

int siteidx,modidx,numcon;
struct ffblk ff;
GIDFILE *G=NULL;

element *ChemId;/// = Get_Element(G,"FSCASID");
element *ChemName;/// = Get_Element(G,"FSCNAME");
element *Clk;/// = Get_Element(G,"CLKTYPE");

fcsv *xQ;
int numxQ;
fpos_t *fpos;

double GetChiQ(int s, int r, char *prod, char *ftyp)
{
  int p, ss, rr;
  double res;
  char product[SMALLSTRING];

    for (p=0; p<numxQ; p++)
    {
      xQ->setfpos(fpos[p]);
      xQ->readln();
      xQ->read(product);
      if (!rstrcmpi(product,concat(prod,ftyp)))
        break;
    }
    if (p==numxQ) return 0;
    for (ss=0; ss<=s; ss++)
      xQ->readln();
    for (rr=0; rr<=r; rr++)
      xQ->read(&res);
    return res;
}

int main(int argc,char **argv)
{
  char tmp[MAXSTRING];
  char des[MAXSTRING];
  char srcid[SMALLSTRING];
  char cName[SMALLSTRING];
  char cId[SMALLSTRING];
  char tunits[SMALLSTRING];
  char vunits[SMALLSTRING];

  char dirunt[SMALLSTRING];
  char dstunt[SMALLSTRING];
  char name[SMALLSTRING];
  char product[SMALLSTRING];

  char mode[SMALLSTRING];
  char gtyp[SMALLSTRING];
  char ftyp[SMALLSTRING];
  char sunt[SMALLSTRING];
  char dunt[SMALLSTRING];
  double size;
  double density;

  int nc;
  int fcnt;
  int conIdx;
  int S_cnt;
  int F_cnt;
  int numdir;
  int numdst;
  double *dst;
  double *dir;

  int numTimes;
  double s1;
  double s2;
  double gs1;
  double sum;
  double gsum;
  double avg;
  double gavg;
  double *times=NULL;
  double *values=NULL;
  double conversion;
  bool gtype[4];
  int convert;


  if (argc<6)
  {
    printf("USAGE: <FUIFile> <RunFile> <SiteIndex> <ModelIndex> <ModelID>\n");
  }
  else
  {
    siteidx = ratoi(argv[3]);
    modidx = ratoi(argv[4]);

    ErrOpen(argv[2]);
    WrnOpen(argv[2]);
    G=Open_GID(AddExtension(argv[1],"gid"));
    if (G==NULL)
      Error("GID file not found");
    Load_GID(G,siteidx,argv[5]);

    int cnt1 = ratoi(info(G,"nummod",siteidx));
    element *modid = Get_Element(G,"ModID");
    element *modnumsrc = Get_Element(G,"modsrcnum");
    element *modsrcid = Get_Element(G,"modsrcid");
    element *modsrcqual = Get_Element(G,"modsrcqual");
    for (int i=1; i<=cnt1; i++)
    {
      if (!rstrcmpi(argv[5],getvalu(modid,siteidx,i)))
      {
        int cnt2 = ratoi(getvalu(modnumsrc,siteidx,i));
        for (int j=1; j<=cnt2; j++)
        {
          rstrcpy(des,getvalu(modsrcqual,siteidx,i,j));
          if (!rstrcmpi("Air",des))
            rstrcpy(srcid, getvalu(modsrcid,siteidx,i,j));
        }
        break;
      }
    }

    ChemId = Get_Element(G,"FSCASID");
    ChemName = Get_Element(G,"FSCNAME");
    Clk = Get_Element(G,"CLKTYPE");
    numcon = ratoi(info(G,"numcon",1));

    xQ = new fcsv();
    rstrcpy(tmp,info(G,"filename"));

    if (xQ->open(tmp,READ))
    {
      xQ->read(tmp);
      xQ->read(&numxQ);
      fpos = new fpos_t[numxQ+1];
      xQ->read(mode);
      xQ->readln();
      xQ->read(tmp);
      xQ->read(&numdst);
      xQ->read(dstunt);
      xQ->readln();
      dst = new double[numdst];
      xQ->read(tmp);
      for (int i = 0; i< numdst; i++)
        xQ->read(&dst[i]);
      xQ->readln();

      xQ->read(tmp);
      xQ->read(&numdir);
      xQ->read(dirunt);
      xQ->readln();
      fpos[0] = xQ->getfpos();
      dir = new double[numdir];
      xQ->read(tmp);
      for (int i = 0; i< numdir; i++)
        xQ->read(&dir[i]);
      xQ->readln();


      for (int i=0; i<4; i++)
        gtype[i] = false;

      for (int i=1; i<= numxQ; i++)
      {
        xQ->read(gtyp);
        if (rstrstr(gtyp,"Concentration"))  gtype[0] = true;
        if (rstrstr(gtyp,"Total Dep"))      gtype[1] = true;
        if (rstrstr(gtyp,"Dry Dep"))        gtype[2] = true;
        if (rstrstr(gtyp,"Wet Dep"))        gtype[3] = true;
        for (int j=0; j<numdir; j++)
          xQ->readln();
        fpos[i] = xQ->getfpos();
        xQ->readln();
      }

      fcsv *fle = new fcsv();
      fle->open(AddExtension(argv[2],"ato"),WRITE);
      fle->write(1);
      fle->writeln();
      fle->write("x/Q Model");
      fle->writeln();

      // use frames DLL calls to read SCF,WCF,ATO files frames.h for declarations
      affOpen(argv[1], srcid);
      S_cnt = affGetNumSets();
      fle->write(S_cnt);
      fle->writeln();

      for (int i = 1; i<= S_cnt; i++)
      {
        F_cnt = affGetSetInfo(i,name,tmp);
        F_cnt = affGetNumFluxTypes(i);


  //    only one flux type output may need two gas amd particle
  /*
        fle->write(F_cnt);
        fle->write(name);
        fle->writeln();
        for (int j = 1; j<= F_cnt; j++)
        {
          affGetFluxType(i,j,ftyp,&size,sunt,&density,dunt);
          fle->write(ftyp);
          fle->write(size);
          fle->write(sunt);
          fle->write(density);
          fle->write(dunt);
          fle->writeln();
        }
  */
  // above was replaced by the following
        fle->write(1);
        fle->write(name);
        fle->writeln();
        int fct = 0;
        s1 = 0;
        s2 = 0;
        for (int j = 1; j<= F_cnt; j++)
        {
          affGetFluxType(i,j,ftyp,&size,sunt,&density,dunt);
          if (!rstrcmpi(sunt,"um"))
          {
            s1 += size;
            if (!rstrcmpi(dunt,"g/cm^3"))
            {
              s2 += density;
              fct++;
            }
          }
        }
        fle->write("all");
        fle->write(s1/F_cnt);
        fle->write("um");
        fle->write(s2/F_cnt);
        fle->write("g/cm^3");
        fle->writeln();
  // end of replacement

        fle->write(mode);
        fle->write("polar");
        fle->write("grid");
        nc = affGetNumContam(i);
        fle->write(nc);

        int hr,day,mn,yr;
        if (!rstrcmpi("acute",mode))
        {
          yr = ratoi(info(G,"arstartyear"));
          JulHours2Date((int)ratof(info(G,"jhour")),yr,&mn,&day,&hr);
          fle->write(yr);
          fle->write(mn);
          fle->write(day);
          fle->write(hr);
        }
        fle->writeln();

        for (int j=1; j<=numcon; j++)
        {
          for (conIdx=1; conIdx<=nc; conIdx++)
          {
            affGetContamName(i,conIdx,0,cName,cId);
            if (!strcmpi(cId,getvalu(ChemId,siteidx,j)))
              break;
          }
          convert = ratoi(getvalu(Clk,siteidx,j));
          if (!rstrcmpi("acute",mode))
            if (convert == 0)          conversion = 1;                          // no conversion
            else                       conversion = 0.03704;                    // convert from pCi to Bq
          else
            if (convert == 0)          conversion = 1 / 31556926.0;             // no conversion
            else                       conversion = 0.03704 / 31556926.0;       // convert from pCi to Bq

          numTimes = affGetSeriesProperties(i,conIdx,0,vunits,tunits);
          fle->write(cName);
          fle->write(cId);

  //        only one time as we average the or integrate the flux
          fle->write(1);
  //        fle->write(numTimes);

          fle->write(0);
          fle->writeln();

          if (numTimes <= 0) continue;

          if (times) delete[] times;
          if (values) delete[] values;
          times = new double[numTimes];
          values = new double[numTimes];
          fcnt = 0;
          for (int m=1; m<=F_cnt; m++)
          {
            sum = 0.0;
            gsum = 0.0;
            affGetFluxType(i,m,ftyp,&size,sunt,&density,dunt);
            affGetSeriesValues(i,conIdx,0,m,numTimes,times,values);
            if (!rstrcmpi(sunt,"um") && !rstrcmpi(dunt,"g/cm^3"))
              for (int k=0; k<numTimes; k++)
                sum+=values[k];
            if (!rstrcmpi(sunt,"fraction") && (size > 0))
              for (int k=0; k<numTimes; k++)
                gsum+=values[k];
            if (gsum > 0.0) fcnt++;
            if (sum > 0.0) fcnt++;
          }

  //        only one time should be 0, 1 for now
  //        as we average the or integrate the flux
            fle->write(1);
  //        for (int k=0; k<numTimes; k++)
  //        {
  //          fle->write(times[k]);

            fle->write(tunits);


            int cnt = 0;
            for (int x=0; x<4; x++)   // needs to changes to 4 types
              if (gtype[x])
                if (fcnt>0) cnt++;
//                cnt += fcnt;
            fle->write(cnt);
            fle->writeln();

  //          for (int x=0; x<numxQ; x++)   // needs to changes to 4 types
            for (int x=0; x<4; x++)   // needs to changes to 4 types
            {
              if (!gtype[x]) continue;
              if (gtype[x] && x==0) rstrcpy(product,"Concentration");
              if (gtype[x] && x==1) rstrcpy(product,"Total Deposition");
              if (gtype[x] && x==2) rstrcpy(product,"Dry Deposition");
              if (gtype[x] && x==3) rstrcpy(product,"Wet Deposition");

              s1 = 0;
              sum = 0.0;
              gs1 = 0;
              gsum = 0.0;
              for (int m=1; m<=F_cnt; m++)
              {
                affGetFluxType(i,m,ftyp,&size,sunt,&density,dunt);
                affGetSeriesValues(i,conIdx,0,m,numTimes,times,values);
                if (!rstrcmpi(sunt,"um") && !rstrcmpi(dunt,"g/cm^3"))
                  for (int n=0; n<numTimes; n++)
                  {
                    sum+=values[n];
                    if (n>0)
                      s1 += (values[n-1] + values[n]) / 2 * (times[n] - times[n-1]);
                  }
                // check if gas and is depositing
                if (!rstrcmpi(sunt,"fraction") && (size > 0))
                  for (int n=0; n<numTimes; n++)
                  {
                    gsum+=values[n];
                    if (n>0)
                      gs1 += (values[n-1] + values[n]) / 2 * (times[n] - times[n-1]);
                  }
  //          ending brace because we average instead of step
              }
                if (!rstrcmpi("acute",mode))  { avg = s1; gavg = gs1; }
                if (!rstrcmpi("chronic",mode)) { avg = sum/numTimes; gavg = gsum/numTimes; }

  //              xQ->setfpos(fpos[x]);
  //              xQ->readln();
  //              xQ->read(product);
  //              xQ->readln();

  //            if ((!rstrstr(product,"(Gas)") && sum > 0.0) || (rstrstr(product,"(Gas)") && gsum > 0.0))
                if ((sum > 0.0) || (gsum > 0.0))
                {
  //                was needed for the name
  //                affGetFluxType(i,m, name, &size, sunt, &density, dunt);

                  if (!rstrncmpi(product,"Con",3))
                  {
                    if (!rstrcmpi("acute",mode))
                    {
                      fle->write("Air Exposure");
  //                    fle->write(name);
                      fle->write("all");
                      fle->write("");
                      if (convert == 0)
                        fle->write("(kg-s)/m^3");
                      else
                        fle->write("(Bq-s)/m^3");
                    }
                    else
                    {
                      fle->write("Air Concentration");
  //                    fle->write(name);
                      fle->write("all");
                      fle->write("");
                      if (convert == 0)
                        fle->write("kg/m^3");
                      else
                        fle->write("Bq/m^3");
                    }
                  }

                  else
                  {
                    if (!rstrcmpi("acute",mode))
                       fle->write("Total Deposition");
                    else
                       fle->write("Deposition Rate");

  //                    fle->write(name);
                    fle->write("all");
                    if (!rstrncmpi(product,"Dry",3))
                      fle->write("Dry");
                    if (!rstrncmpi(product,"Wet",3))
                      fle->write("Wet");
                    if (!rstrncmpi(product,"Tot",3))
                      fle->write("Total");
                    if (!rstrcmpi("acute",mode))
                    {
                      if (convert == 0)
                        fle->write("kg/m^2");
                      else
                        fle->write("Bq/m^2");
                    }
                    else
                    {
                      if (convert == 0)
                        fle->write("(kg/m^2)/s");
                      else
                        fle->write("(Bq/m^2)/s");
                    }
                  }
                  fle->write(numdst);
                  fle->write(dstunt);
                  fle->write(numdir);
                  fle->write(dirunt);
                  fle->writeln();

                  for (int r=0; r<numdst; r++)
                    fle->write(dst[r]);
                  fle->writeln();
                  for (int s=0; s<numdir; s++)
                  {
                    fle->write(dir[s]);
                    for (int r=0; r<numdst; r++)
                    {
                      sum = GetChiQ(s,r,product,"");
                      gsum = GetChiQ(s,r,product," (Gas)");

  //                    xQ->read(&sum);
  //                    xQ->read(&gsum); // need this for the

                      sum = sum*avg*conversion;
                      gsum = gsum*gavg*conversion;
                      fle->write(sum+gsum,"%.3lg");

                    }
                    fle->writeln();
                    xQ->readln();
                  }
                }
  //            }
            }
  //        }
        }
      }
      findclose(&ff);
      affClose();
      WrnClose();
      Close_GID(G);
      fle->close();
      xQ->close();
      ErrClose();
      if (values) delete[] values;
      if (times) delete[] times;
      delete fle;
      delete[] dst;
      delete[] dir;
      delete[] fpos;
      delete xQ;
    }
    else
    {
      findclose(&ff);
      WrnClose();
      Close_GID(G);
      delete xQ;
      Error("Unable to find or open X/Q file");
    }
  }
  return 0;
}












