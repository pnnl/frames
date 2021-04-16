#pragma hdrstop
#include <conio.h>
#include "..\Common Files\frames.h"
#include "..\Common Files\gid.h"
#include "..\Common Files\csv.h"
#include "..\Common Files\robust.h"
#include "..\Common Files\F2ModuleDevC.h"
#include "..\Common Files\F2SystemDevC.h"
#include "..\Common Files\F2ErrorC.h"
#include "error.h"
#include <except.h>
#include <condefs.h>

//---------------------------------------------------------------------------
USEUNIT("..\Common Files\gid.cpp");
USEUNIT("..\Common Files\Fcsv.cpp");
USEUNIT("..\Common Files\robust.cpp");
USEUNIT("..\Common Files\Error.cpp");
USEUNIT("..\Common Files\csv.cpp");
USELIB("..\Common Files\frames.lib");
USERES("copysrc.res");
USELIB("..\Common Files\omfSystemIO.lib");
//---------------------------------------------------------------------------
#pragma argsused

int siteidx,modidx,numlife,numcon;
struct ffblk ff;
GIDFILE *G=NULL;

element *LifeForm;
element *LifeName;
element *ChemId;/// = Get_Element(G,"FSCASID");
element *ChemName;/// = Get_Element(G,"FSCNAME");


char *AddExten(char *f,char *e)
{
  static char fname[MAXSTRING];
  sprintf(fname,"%s.%s",f,e);
  return fname;
}

void lookup(char *name, double &lat, double &lon)
{
  if (strstr(name,"S01"))  { lat = 45.917069264294;  lon = -119.3779585293;}
  if (strstr(name,"S02"))  { lat = 45.918437986474;  lon = -119.3683992576;}
  if (strstr(name,"S03"))  { lat = 45.920055454462;  lon = -119.3589596835;}
  if (strstr(name,"S04"))  { lat = 45.92403022403;   lon = -119.3495249787;}
  if (strstr(name,"S05"))  { lat = 45.926850155793;  lon = -119.3388679356;}
  if (strstr(name,"S06"))  { lat = 45.92238794523;   lon = -119.3753988708;}
  if (strstr(name,"S07"))  { lat = 45.924077408304;  lon = -119.3659876892;}
  if (strstr(name,"S08"))  { lat = 45.926523403184;  lon = -119.3578242530;}
  if (strstr(name,"S09"))  { lat = 45.92992044261;   lon = -119.3462770035;}
  if (strstr(name,"S10"))  { lat = 45.931671574347;  lon = -119.3363735263;}
  if (strstr(name,"S11"))  { lat = 45.929114381408;  lon = -119.3302541662;}
  if (strstr(name,"RS1"))   { lat = 45.920527552332;  lon = -119.3578912459;}
  if (strstr(name,"RS2"))   { lat = 45.921498059719;  lon = -119.3557863827;}
  if (strstr(name,"RS3"))   { lat = 45.922400852661;  lon = -119.3539081452;}
  if (strstr(name,"RS4"))   { lat = 45.92347055669;   lon = -119.3513533598;}
  if (strstr(name,"RS5"))   { lat = 45.923657828875;  lon = -119.3588583707;}
  if (strstr(name,"RS6"))   { lat = 45.92469175997;   lon = -119.3554250877;}
}

int main(int argc,char **argv)
{
  char filename[MAXSTRING];
  char outname[MAXSTRING];
  char outpath[MAXSTRING];
  char name[MAXSTRING];
  char kind[MAXSTRING];
  char des[SMALLSTRING];
  char cas[SMALLSTRING];
  char cname[SMALLSTRING];
  char tUnits[SMALLSTRING];
  char vUnits[SMALLSTRING];
  char rb[LARGESTRING];
  double lat;
  double lon;

//  chdir("c:\frames1.6");

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
      Error("GID file not found!");
    Load_GID(G,siteidx,argv[5]);


    getpath(argv[0]);
    element *location = Get_Element(G,"Location");
    element *direction = Get_Element(G,"Direction");
    element *reachback = Get_Element(G,"Reachback");
    if (direction!=NULL)                 // sensor module
    {
      element *endtime = Get_Element(G,"EndTime");
      element *threshold = Get_Element(G,"Threshold");

        // copy source files
      rstrcpy(filename,fnpath,getvalu(direction),"Source.wcf");
      rstrcpy(outname,argv[2],".wcf");
      if (!Copy(filename,outname))
        Error("Copy1 file failed!");

        // copy source graphs
      rstrcpy(filename,fnpath,getvalu(direction),"Source.xls");
      rstrcpy(outname,fnpath,"source.xls");
      if (!Copy(filename,outname))
        Error("Copy2 file failed!");


      int idx;
      wcfOpen(getvalu(direction),"riv1");
      int numset = wcfGetNumSets();
      if (strstr(getvalu(direction),"West"))
        idx = 97;
      else
        idx = 97;
      bool first = true;
      for (int j=1; j<=numset; j++)
      {
        if (wcfGetSetInfo(j,name,kind))
        {
          int numcon = wcfGetNumContam(j);
          name[3] = 0;
          for (int k=1; k<=numcon; k++)
          {
            wcfGetContamName(j, k, 0, cname, cas);
            int cnt = wcfGetSeriesProperties(j, k, 0, vUnits, tUnits);
            double *times = new double[cnt+1];
            double *values = new double[cnt+1];
            wcfGetSeriesValues(j, k, 0, cnt+1, times, values);
            lookup(name, lat, lon);
            for (int i=0; i<4; i++)     // four time periods being simulated
            {
              ocsv *fout;
              sprintf(outpath,"Sensor_Findings_T%d.txt",i+1);
              printf("%s\n",outpath);
              if (first)
                unlink(outpath);
              if(0 == access(outpath, 0))
                fout = new ocsv(outpath, '"', ',', _APPEND_);
              else
              {
                fout = new ocsv(outpath, '"', ',', _CREATE_);
                *fout << "ValidFemisUserName" << "PointName";
                *fout << "ClassName" << "SubclassName" << "Notes";
                *fout << "AttrName1" << "AttrValue1";
                *fout << "AttrName2" << "AttrValue2";
                *fout << "AttrName3" << "AttrValue3";
                *fout << "AttrName4" << "AttrValue4";
                *fout << "AttrName5" << "AttrValue5";
                *fout << "Lat" << "Long" << NewLn;;
              }
              *fout << "millard" << name << "Water_Sensor";
              double limit = ratof(getvalu(threshold));
              char mylimit[SMALLSTRING];
              char myreachback[SMALLSTRING];
              if (limit > values[idx+(i*8)])
              {
                *fout << "Active_Normal" << "";
                rstrcpy(mylimit,"FALSE");
              }
              else
              {
                sprintf(myreachback,"%s Time %f", getvalu(reachback),times[idx+(i*8)]);
                *fout << "Above Threshold" << getvalu(reachback);
                rstrcpy(mylimit,"TRUE");
              }

              *fout << "Threshold_Limit" << mylimit ;
              *fout << "Chemical" << cas;
              *fout << "Concentration" << values[idx+(i*8)];
              *fout << "Concent_Units" << vUnits;
              *fout << "" << "";
              *fout << lat << lon << NewLn;
              delete fout;
            }
            first = false;
            delete[] times;
            delete[] values;
          }
        }
      }
    }
    if (location!=NULL)      // WCF Hard Wire Model Results
    {
      int cnt1 = ratoi(info(G,"nummod",siteidx));
      element *modid = Get_Element(G,"ModID");
      element *modnumsrc = Get_Element(G,"modsrcnum");
      element *modsrcid = Get_Element(G,"modsrcid");
      element *modsrcqual = Get_Element(G,"modsrcqual");
      for (int i=1; i<=cnt1; i++)
        if (!strcmpi(argv[5],getvalu(modid,siteidx,i)))
        {
          int cnt2 = ratoi(getvalu(modnumsrc,siteidx,i));
          for (int j=1; j<=cnt2; j++)
          {
            strcpy(des,getvalu(modsrcqual,siteidx,i,j));
            if (!rstrcmpi("Surface Water",des))
            {
              strcpy(des,getvalu(modsrcid,siteidx,i,j));
              break;
            }
          }
          break;
        }

      wcfOpen(argv[1],des);
      if (wcfGetNumSets())
        if (wcfGetSetInfo(1,name,kind))
        {
          if (strstr(name,"West") != NULL)
            rstrcpy(filename,fnpath,"westgrd",getvalu(location),".wcf");
          if (strstr(name,"East") != NULL)
            rstrcpy(filename,fnpath,"eastgrd",getvalu(location),".wcf");
          if (strlen(filename))
          {
            rstrcpy(outname,argv[2],".wcf");
            if (!Copy(filename,outname))
              Error("Copy3 file failed!");

            rstrcpy(filename,fnpath,"source.xls");
            if (!Copy(filename,getvalu(reachback)))
              Error("Copy4 file failed!");

            ocsv *fout;
            rstrcpy(outpath,"Source_Location.txt");
            fout = new ocsv(outpath, '"', ',', _CREATE_);
            // header
            *fout << "ValidFemisUserName" << "PointName";
            *fout << "ClassName" << "SubclassName" << "Notes";
            *fout << "AttrName1" << "AttrValue1";
            *fout << "AttrName2" << "AttrValue2";
            *fout << "AttrName3" << "AttrValue3";
            *fout << "AttrName4" << "AttrValue4";
            *fout << "AttrName5" << "AttrValue5";
            *fout << "Lat" << "Long" << NewLn;
            // source info
            *fout << "millard" << getvalu(location) << "Potential_Source";
            getpath(getvalu(reachback));
            if (strstr(name,"West") != NULL)
            {
              rstrcpy(rb, fnpath, "\wplume_",getvalu(location),".avi; ",getvalu(reachback));
              if (!Copy("R:\\Demo\\W_TA_PAR_Mess.bat","C:\\Program Files\\FramesV2\\RRA\\PAR.bat"))
                Error("Copy5 file failed!");
            }
            else
            {
              rstrcpy(rb, fnpath, "\eplume_",getvalu(location),".avi; ",getvalu(reachback));
              if (!Copy("R:\\Demo\\E_TA_PAR_Mess.bat","C:\\Program Files\\FramesV2\\RRA\\PAR.bat"))
                Error("Copy5 file failed!");
            }

            *fout << "Estimated" << rb;
            *fout << "Chemical" << "SR90";
            *fout << "Concentration" << "100";
            *fout << "Concent_Units" << "mg/L";
            if (strstr(name,"West") != NULL)
              *fout << "Exp_Release_Time" << "1999.703912827";
            else
              *fout << "Exp_Release_Time" << "1999.714836";

            *fout << "" << "";
            lookup(getvalu(location), lat, lon);
            *fout << lat << lon << NewLn;
            delete fout;

          }
          else
            Error("Valid Direction not found!");
        }
        else
          Error("Valid Direction not found in WCF!");
      else
        Error("Module ID not found: ",des);
    }

    WrnClose();
    ErrClose();
    Close_GID(G);
  }
//  findclose(&ff);
  return 0;
}

