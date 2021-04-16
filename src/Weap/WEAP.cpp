#pragma hdrstop
#include <conio.h>
#include <except.h>
#include <condefs.h>
#include "..\Common Files\robust.h"
#include "..\Common Files\gid.h"
#include "..\Common Files\StringParser.h"
#include "results.h"
#include "error.h"

#define MAXMEDIA 100

//---------------------------------------------------------------------------
USEUNIT("..\Common Files\robust.cpp");
USEUNIT("..\Common Files\fcsv.cpp");
USEUNIT("..\Common Files\gid.cpp");
USEUNIT("..\Common Files\series.cpp");
USEUNIT("..\Common Files\Line.cpp");
USEUNIT("..\Common Files\StringParser.cpp");
USEUNIT("ERROR.CPP");
USEUNIT("TOXDATA.CPP");
USEUNIT("CSERIES.CPP");
USEUNIT("RESULTS.CPP");
USEUNIT("ESERIES.CPP");
USERES("WEAP.res");
//---------------------------------------------------------------------------
#pragma argsused

typedef std::map<std::string, int>      _idxPos_MAP;
typedef _idxPos_MAP::iterator           _idxPos_MAP_ITR;

typedef std::map<std::string, _idxPos_MAP *>      _idxPos_MAP_PTR_MAP;
typedef _idxPos_MAP_PTR_MAP::iterator             _idxPos_MAP_PTR_MAP_ITR;

_idxPos_MAP_PTR_MAP mytrv;
int siteidx,modidx,numcon;
int numsrc,numlife,numlocs,numecos,numscfs,numtwis;
int bbfcheck;
char Title[MAXSTRING];
char WEAPUI_INI[MAXSTRING];

struct ffblk ff;
Series *T;
Series *frq;
EcoSeries *twi=NULL;
EcoSeries *scf=NULL;
EcoSeries *bbf=NULL;
EcoSeries *loc=NULL;
ConSeries *con=NULL;
ConData *tox=NULL;
Results *res=NULL;
fcsv *resout=NULL;
GIDFILE *G=NULL;

element *LifeForm;
element *LifeName;
element *ChemId;   /// = Get_Element(G,"FSCASID");
element *ChemName; /// = Get_Element(G,"FSCNAME");


char *AddExten(char *f,char *e)
{
  static char fname[MAXSTRING];
  sprintf(fname,"%s.%s",f,e);
  return fname;
}

bool ReadIniLong(char *AppName, char *KeyName, int Default, int *value)
{
  int i;
  char pszVal[MEDSTRING];

  *value = Default;
  for (i=0; i<MEDSTRING; i++) pszVal[i] = 0;
  i = GetPrivateProfileString(AppName, KeyName, "", pszVal, MEDSTRING, WEAPUI_INI);
  if (i > 0)
  {
    *value = ratoi(pszVal);
    return true;
  }
  return false;
}

bool ReadIniString(char *AppName, char *KeyName, char *Default, char *value)
{
  int i;
  char pszVal[MEDSTRING];

  rstrcpy(value,Default);
  for (i=0; i<MEDSTRING; i++) pszVal[i] = 0;
  i = GetPrivateProfileString(AppName, KeyName, "", pszVal, MEDSTRING, WEAPUI_INI);
  if (i > 0)
  {
    rstrcpy(value,pszVal);
    return true;
  }
  return false;
}

int ReadIni(char *section)
{
  int i;
  int j;
  int Cnt;
  bool sval;
  char temp[MEDSTRING];
  char value[MEDSTRING];
  CStringParser cmdln;
  _idxPos_MAP *ip;

  Cnt = 0;
  sval = ReadIniLong(section, "keyCount", 0, &Cnt);
  for(i=1; i<=Cnt; i++)
  {
    sprintf(temp,"key%d",i);
    ReadIniString(section, temp, "", value);
    cmdln.Parse(value, CParseOptions(','));

    for (j=2; j<cmdln.GetCount(); j+=2)
    {
      ip = new _idxPos_MAP;
      rstrcpy(temp,cmdln.GetAt(0));
      (*ip)[temp] = atoi(cmdln.GetAt(j+1));
      rstrcpy(temp,cmdln.GetAt(j));
      mytrv[temp] = ip;
    }
  }

  Cnt = 0;
  sval = ReadIniLong(section, "desCount", 0, &Cnt);
  for(i=1; i<=Cnt; i++)
  {
    sprintf(temp,"des%d",i);
    ReadIniString(section, temp, "", value);
    cmdln.Parse(value, CParseOptions(','));

    for (j=1; j<cmdln.GetCount(); j+=2)
    {
      rstrcpy(temp,cmdln.GetAt(j));
      _idxPos_MAP_PTR_MAP_ITR it = mytrv.find(temp);
      if (it==mytrv.end())
      {
        ip = new _idxPos_MAP;
        mytrv[temp] = ip;
      }
      else
        ip = (*it).second;
      rstrcpy(temp,cmdln.GetAt(0));
      (*ip)[temp] = atoi(cmdln.GetAt(j+1));
    }
  }

  Cnt = 0;
  sval = ReadIniLong(section, "idxCount", 0, &Cnt);
  for(i=1; i<=Cnt; i++)
  {
    sprintf(temp,"idx%d",i);
    ReadIniString(section, temp, "", value);
    cmdln.Parse(value, CParseOptions(','));

    for (j=2; j<cmdln.GetCount(); j+=2)
    {
      rstrcpy(temp,cmdln.GetAt(j));
      _idxPos_MAP_PTR_MAP_ITR it = mytrv.find(temp);
      if (it==mytrv.end())
      {
        ip = new _idxPos_MAP;
        mytrv[temp] = ip;
      }
      else
        ip = (*it).second;
      rstrcpy(temp,cmdln.GetAt(0));
      (*ip)[temp] = atoi(cmdln.GetAt(j+1));
    }
  }
  return 1;
}

int DoTRV(EcoSeries *bbf, int i1, int j1, int i, int j, fcsv *resout, char *trv, _idxPos_MAP *ip, char *vary=NULL, char *uncr=NULL)
//int DoTRV(EcoSeries *bbf, int i1, int j1, int i, int j, fcsv *resout, char *trv, int desc, int body, int effect, int jurisdiction)
{
  _idxPos_MAP_ITR it;
  char temp[MAXSTRING];
  int ci;
  int ssl = 0;
  int cct = 0;
  int user = 0;
  int count = 0;
  int descnt = 1;
  int bdycnt = 1;
  int effcnt = 1;
  int jurcnt = 1;
  int cnt;
  char *hq_c;
  char *du;
  char *clsname;
  float hq;
  element *n1 = NULL;
  element *des_l = NULL;
  element *bdy_l = NULL;
  element *eff_l = NULL;
  element *jur_l = NULL;
  element *mfx_l = NULL;
  element *tfx_l = NULL;
  element *des_u = NULL;                     //use variable
  element *bdy_u = NULL;                     //use variable
  element *eff_u = NULL;                     //use variable
  element *jur_u = NULL;                     //use variable
  element *hq_l = NULL;
  element *dur_l = NULL;
  element *clss = NULL;
  element *cur = NULL;

  if (!strcmpi(trv,"SSL")) ssl = 1;
  if (!strcmpi(trv,"TRV")) cct = 1;
  if (!strcmpi(trv,"user")) user = 1;

  int desc = 0;
  sprintf(temp,"%sDescript",trv);
  it = ip->find(temp);
  if (it !=  ip->end()) desc = 1;

  if (desc || user)
  {
    if (!user)
    {
      sprintf(temp,"num%sdescript",trv);
      n1 = Get_Element(G,temp);
      if (n1 == NULL) return 0;
      descnt = ratoi(info(G,temp));
    }
    sprintf(temp,"%sdescript",trv);
    des_l = Get_Element(G,temp);
    if (des_l == NULL) return 0;
    sprintf(temp,"%schem",trv);
    des_u = Get_Element(G,temp);
    if (des_u == NULL) return 0;
  }

  int body = 0;
  it = ip->find("BodyPart");
  if (it != ip->end())
  {
    body = 1;
    n1 = Get_Element(G,"numbodypart");
    if (n1 == NULL) return 0;
    bdycnt = ratoi(info(G,"numbodypart"));
    bdy_l = Get_Element(G,"bodypart");
    if (bdy_l == NULL) return 0;
    sprintf(temp,"bodypart%schem",trv);
    bdy_u = Get_Element(G,temp);
    if (bdy_u == NULL) return 0;
  }

  int effect = 0;
  it = ip->find("Effect");
  if (it != ip->end())
  {
    effect = 1;
    n1 = Get_Element(G,"numeffect");
    if (n1 == NULL) return 0;
    effcnt = ratoi(info(G,"numeffect"));
    eff_l = Get_Element(G,"effect");
    if (eff_l == NULL) return 0;
    sprintf(temp,"effect%schem",trv);
    eff_u = Get_Element(G,temp);
    if (eff_u == NULL) return 0;
  }

  int jurisdiction = 0;
  it = ip->find("Jurisdiction");
  if (it != ip->end())
  {
    jurisdiction=1;           // SSL and TRV indice
    jur_l = Get_Element(G,"jurisdiction");
    if (jur_l == NULL) return 0;
    for (jurcnt=0,cur=jur_l; cur!=NULL; jurcnt++, cur = cur->next);

    mfx_l = Get_Element(G,"derivation");
    if (mfx_l == NULL) return 0;
    tfx_l = Get_Element(G,"effect");
    if (tfx_l == NULL) return 0;

    sprintf(temp,"jurisdiction%s",trv);
    jur_u = Get_Element(G, temp);
//    if (cct)
//      jur_u = Get_Element(G,"cct");
//    else
//      jur_u = Get_Element(G, trv));
    if (jur_u == NULL) return 0;
  }

  if (ssl || cct)
  {
    sprintf(temp,"num%s",trv);     //ssl variables
    n1 = Get_Element(G,temp);
    hq_l = Get_Element(G,trv);
    if (cct)
    {
      // use dur_l for the unique class
      dur_l = Get_Element(G,"UniqueClass");
      clss = Get_Element(G,"Class");
    }
    else
      // dur_l not used but can't be null for error check
      dur_l = Get_Element(G,trv);
  }
  else
  {
    sprintf(temp,"num%schem",trv);     //ebf variables
    n1 = Get_Element(G,temp);
    sprintf(temp,"%schem",trv);
    hq_l = Get_Element(G,temp);
    sprintf(temp,"%schemdur",trv);
    dur_l = Get_Element(G,temp);
  }

  bbfcheck = 0;
  if (!(hq_l == NULL || dur_l == NULL || n1 == NULL) || clss == NULL)
    bbfcheck = 1;
  if (bbfcheck)
  {
    for (int q=1; q<= jurcnt; q++)
    {
      if (!strncmpi(getvalu(jur_u,q),"T",1) || !jurisdiction || user)
      {
        for (int k=1; k<= descnt; k++)
        {
          if (!strncmpi(getvalu(des_u,k),"T",1) || !desc || user)
          {
            for (int m=1; m<= bdycnt; m++)
            {
              if (!strncmpi(getvalu(bdy_u,m),"T",1) || !body || user)
              {
                for (int n=1; n<= effcnt; n++)
                {
                  if (!strncmpi(getvalu(eff_u,n),"T",1) || !effect || user)
                  {
                    if (ssl)
                    {
                      sprintf(temp,"num%s",trv);
                      cnt = ratoi(info(G,temp,j1,q));
                    }
                    else if (cct)
                    {
                      cnt = 0;
                      clsname = getvalu(clss,i1);
                      for (ci=1, cur=dur_l; cur!=NULL; ci++, cur=cur->next)
                      {
                        if (!rstrcmpi(clsname, getvalu(dur_l,ci)))
                        {
                          sprintf(temp,"num%s",trv);
                          cnt = ratoi(info(G,temp,j1,ci,q));
                          break;
                        }
                      }
                    }
                    else
                    {
                      sprintf(temp,"num%schem",trv);
                      if (desc)
                        if (body && effect)
                          cnt = ratoi(info(G,temp,j1,i1,k,m,n));
                        else
                          cnt = ratoi(info(G,temp,j1,i1,k));
                      else
                        if (body && effect)
                          cnt = ratoi(info(G,temp,j1,i1,m,n));
                        else
                          cnt = ratoi(info(G,temp,j1,i1));
                    }

                    for (int p=1; p<= cnt; p++)
                    {
                      if (ssl)
                      {
                        hq_c = getvalu(hq_l,j1,q,p);
                      }
                      else if (cct)
                      {
                        hq_c = getvalu(hq_l,j1,ci,q,p);
                      }
                      else
                      {
                        if (desc)
                          if (body && effect)
                          {
                            hq_c = getvalu(hq_l,j1,i1,k,m,n,p);
                            du = getvalu(dur_l,j1,i1,k,m,n,p);
                          }
                          else
                          {
                            hq_c = getvalu(hq_l,j1,i1,k,p);
                            du = getvalu(dur_l,j1,i1,k,p);
                          }
                        else
                          if (body && effect)
                          {
                            hq_c = getvalu(hq_l,j1,i1,m,n,p);
                            du = getvalu(dur_l,j1,i1,m,n,p);
                          }
                          else
                          {
                            hq_c = getvalu(hq_l,j1,i1,p);
                            du = getvalu(dur_l,j1,i1,p);
                          }
                       }

                      hq = ratof(hq_c);
                      if (hq <= 0.0)
                        continue;

                      rstrcpy(Title,"");
//                      rstrcpy(Title,bbf->Eco[i].Type);
//                      strcat(Title," exposed to ");
//                      strcat(Title,bbf->Eco[i].Con[j].Id);
//                      strcat(Title,"(");
//                      strcat(Title,bbf->Eco[i].Con[j].Name);
//                      strcat(Title,")");
//                      strcat(Title," by ");
//                      strcat(Title,bbf->Eco[i].Name);
//                      strcat(Title," using ");
                      if (desc)
                        strcat(Title,getvalu(des_l,k));
                      else if (user)
                        strcat(Title,getvalu(des_l,j1,i1,p));
                      else if (ssl || cct)
                      {
                        strcat(Title,getvalu(jur_l,q));
                        strcat(Title,"~");
                        strcat(Title,getvalu(mfx_l,j1,ci,q,p));
                        strcat(Title,"~");
                        strcat(Title,getvalu(tfx_l,j1,ci,q,p));
                        strcat(Title,"~");
                      }
                      else
                        strcat(Title,trv);
/*
                      if (cct)
                        strcat(Title," intake rate of ");
                      else
                        strcat(Title," concentration of ");
*/
                      strcat(Title," ");
                      strcat(Title,hq_c);
                      if (ssl)
                        strcat(Title," mg/kg");
                      else if (cct)
                        strcat(Title," mg/kg/day");
                      else
                      {
                        strcat(Title," mg/kg for ");
                        strcat(Title,du);
                        strcat(Title," days");
                      }
                      if (effect)
                      {
                        strcat(Title," resulting in ");
                        strcat(Title,getvalu(eff_l,n));
                        strcat(Title," effects");
                      }
                      if (body)
                      {
                        strcat(Title," for the ");
                        strcat(Title,getvalu(bdy_l,m));
                      }
                      if (rstrncmpi(vary,"Disc",4) && vary!=NULL)
                      {
                        strcat(Title," with uncertainty of ");
                        strcat(Title,uncr);
                        strcat(Title," and variabilty of ");
                        strcat(Title,vary);
                      }
                      resout->write(Title);
                      resout->writeln();
            // EHQ
/*
                      resout->write("EHQ");
                      resout->write("");
                      resout->write("Time");
                      resout->write("yr");
                      //resout->writeln();
*/
                      T = new Series();
                      bbf->Eco[i].Con[j].CopyTo(T);
                      rstrcpy(T->yUnits,"HQ");
                      T->Div(hq);
                      T->WriteSeries(resout);

/*             this now handeled by viewers
            // EHQ Rankorder
                      resout->write("EHQ");
                      resout->write("");
                      resout->write("Probability of Equaling or Exceeding EHQ");
                      resout->write("%");
                      Series *R = T->MakeExceed(1,resolution);
                      if (R!=NULL)
                        R->WriteSeries(resout);
                      else
                      {
                        Warning("Unbale to convert to EHQ exceedence for ", bbf->Eco[i].Type, bbf->Eco[i].Con[j].Id);
                        resout->write(0);
                        resout->writeln();
                      }
                      delete R;
*/
                      delete T;
/*
            // Intake / Bodyburden Rankorder
                      if (cct)
                      {
                        resout->write("Body Intake Rate");
                        resout->write("mg/kg/day");
                        resout->write("Probability of Equaling or Exceeding Body Intake Rate");
                      }
                      else if (ssl)
                      {
                        resout->write("Soil Concentration");
                        resout->write("mg/kg");
                        resout->write("Probability of Equaling or Exceeding Soil Concentration");
                      }
                      else
                      {
                        resout->write("Body Burden");
                        resout->write("mg/kg");
                        resout->write("Probability of Equaling or Exceeding Body Burden");
                      }
                      resout->write("%");
                      //resout->writeln();
                      R = bbf->Eco[i].Con[j].MakeExceed(1,resolution);
                      if (R!=NULL)
                        R->WriteSeries(resout);
                      else
                      {
                        Warning("Unbale to convert to concentration exceedence for ", bbf->Eco[i].Type, bbf->Eco[i].Con[j].Id);
                        resout->write(0);
                        resout->writeln();
                      }
                      delete R;
*/
                      count++;
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  else
  {
    Error("Missing GID file entries");
    return 0;
  }
  return count;
}



int main(int argc,char **argv)
{
  int i1;
  int j1;
  int pos1;
  int pos2;
  char des[MAXSTRING];
  char cId[MAXSTRING];
  char cName[MAXSTRING];
  char id[MAXSTRING];
  char name[MAXSTRING];
  char twiSrcId[MAXSTRING];
  char temp[MEDSTRING];

  if (argc<6)
  {
    printf("USAGE: <FUIFile> <RunFile> <SiteIndex> <ModelIndex> <ModelID>\n");
  }
  else
  {
    fnsplit(argv[0],fndrive,fndir,fnfile,fnext);
    fnmerge(WEAPUI_INI,fndrive,fndir,"WeapUI","ini");

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
    element *deslc = Get_Element(G,"deslc");
    for (int i=1; i<=cnt1; i++)
    {
      if (!strcmpi(argv[5],getvalu(modid,siteidx,i)))
      {
        int cnt2 = ratoi(getvalu(modnumsrc,siteidx,i));
        for (int j=1; j<=cnt2; j++)
        {
          rstrcpy(des,getvalu(modsrcqual,siteidx,i,j));
          if (!rstrcmpi("Eco Aquatic Benchmarks",des) ||
              !rstrcmpi("Aquatic Organism",des) ||
              !rstrcmpi("Aquatic Benchmarks",des) ||
              !rstrcmpi("Terrestrial Organism",des) ||
              !rstrcmpi("Chemical Terrestrial TRVs",des) ||
              !rstrcmpi("Chemical SSLs",des))
            Load_GIDSection(G,getvalu(modsrcid,siteidx,i,j));
          if (!rstrcmpi("Terrestrial Wildlife Intake",des))
            rstrcpy(twiSrcId,getvalu(modsrcid,siteidx,i,j));
        }
        break;
      }
    }
    LifeForm = Get_Element(G,"LifeFormSci");
    LifeName = Get_Element(G,"LifeFormName");
    numlife = ratoi(info(G,"numlife"));
    if (LifeForm == NULL)
    {
      LifeForm = Get_Element(G,"ScientificName");
      LifeName = Get_Element(G,"CommonName");
      if (numlife == 0) numlife = ratoi(info(G,"numScientificName"));
    }
    numecos = ratoi(info(G,"numecos"));
    numlocs = ratoi(info(G,"numlocs"));
    numscfs = ratoi(info(G,"numscfs"));
    numtwis = ratoi(info(G,"numtwis"));
    numcon = ratoi(info(G,"numcon",siteidx));

    try
    {      resout = new fcsv;    }
    catch(...)
    {     Error("Can't allocate memory for output file"); }

    // begin output
    if (numlocs)
    {  if (!resout->open(AddExten(argv[2],"exf"),WRITE))
        Error("Can't open output file!"); }
    else
    {  if (!resout->open(AddExten(argv[2],"hqf"),WRITE))
        Error("Can't open output file!"); }
    resout->write(1);
    resout->writeln();
    resout->write("WEAP Wildlife Ecological Assessment Program");
    resout->writeln();

    // ====================================================
    // Terrestrail CCT TRV calcs
    if (numtwis && !findfirst(AddExten(argv[1],"twi"),&ff,0))
    {
      // loop for multiple twi
      try
      {
        twi = new EcoSeries;
      }
      catch(...)
      {
        Error("Out of memory in main");
      }

      twi->InitConc(MAXMEDIA);

      GIDFILE *TG = Open_GID(AddExtension(argv[1],"twi"));
      if (TG==NULL)  Error("TWI file not found");

      Load_GIDSection(TG,twiSrcId);

      ChemId = Get_Element(G,"ChemCAS");
      ChemName = Get_Element(G,"ChemName");
//      ChemId = Get_Element(TG,"ChemicalCASID");
//      ChemName = Get_Element(TG,"ChemicalName");

      if (!(ChemId == NULL))
        bbfcheck = twi->ReadIntakeTWI(TG);
      if (bbfcheck)
      {
        // find all media names
        int nummed=0;
        char *med[MAXMEDIA];
        int loccnt[MAXMEDIA];
        for (int m=0; m<twi->NumEco && twi->Eco[m].Con!=NULL; m++)
        {
          int n;
          for (n=0; n<nummed; n++)
            if (!strcmpi(twi->Eco[m].Name,med[n]))
              break;
          if (n==nummed)
          {
            nummed++;
            med[n] = twi->Eco[m].Name;
            loccnt[n] = 1;
          }
          else
          {
            loccnt[n]++;
          }
        }

        ReadIni("CCT");

        resout->write(nummed);
        resout->writeln();
        for (int m=0; m<nummed; m++)
        {
          resout->write("Terrestrial Organism Intake HQ");
          resout->write(med[m]);
          resout->write(numlife);
          resout->writeln();
          for (int i=1; i<=numlife; i++)
          {
            rstrcpy(id,getvalu(LifeForm,i));
            rstrcpy(name,getvalu(LifeName,i));
            resout->write(name);
            resout->write(id);

            // see if there are any of this medium and and lifeform
            for (i1=0; i1<twi->NumEco; i1++)
            {  if (!strcmpi(id,twi->Eco[i1].Type) && !strcmpi(med[m],twi->Eco[i1].Name)) break; }
            if (i1 < twi->NumEco)
            {
              resout->write(numcon);
              resout->writeln();
            }
            else
            {
              Warning("No information found for ",name);
              resout->write(0);
              resout->writeln();
            }

            // do output for all chemical all outputs in list
            for (int j=1; j<=numcon && i1 < twi->NumEco; j++)
            {
              rstrcpy(cId,getvalu(ChemId,j));
              rstrcpy(cName,getvalu(ChemName,j));
//              rstrcpy(cId,getvalu(ChemId,1,i1,j));
//              rstrcpy(cName,getvalu(ChemName,1,i1,j1));
              resout->write(cName);
              resout->write(cId);
              for (j1=0; j1<twi->Eco[i1].NumCon; j1++)
              {  if (!strcmpi(twi->Eco[i1].Con[j1].Name,cId)) break;  }
              if (j1 < twi->Eco[i1].NumCon)
              {
                int cnt3 = 0;
                pos1 = resout->getfpos();
                resout->write(0,"%-4d");
                resout->writeln();

//              if (ratoi(info(G,"cct")))  cnt3 += DoTRV(twi,i,j,i1,j1,resout,"TRV",0,0,0,1);
// replaced by
                for (_idxPos_MAP_PTR_MAP_ITR it = mytrv.begin(); it != mytrv.end(); it++)
                {
                   rstrcpy(temp,(*it).first.c_str());
                   cnt3 += DoTRV(twi,i,j,i1,j1,resout,temp,(*it).second);
                }

                pos2 = resout->getfpos();
                bool nl = resout->newline;
                resout->setfpos(pos1+1);
                resout->write(cnt3,"%-4d");
                resout->setfpos(pos2);
                resout->newline = nl;;
              }
              else
              {
                Warning(name, " has no chemcial information found for ", cName);
                resout->write(0);
                resout->writeln();
              }
            }
          }
        }
        Close_GID(TG);
      }
      else
        Error("Missing GID file entries");
      delete twi;
    }

    // ====================================================
    // Aquatic TRV calcs
    if (numecos && !findfirst(AddExten(argv[1],"bbf"),&ff,0))
    {
      try
      {
        bbf = new EcoSeries;
      }
      catch(...)
      {
        Error("Out of memory in main");
      }

      bbf->InitConc(numlife*MAXMEDIA);
      ChemId = Get_Element(G,"ChemID");
      ChemName = Get_Element(G,"ChemName");
      if (ChemId == NULL)
      {
        ChemId = Get_Element(G,"myChemCasId");
        ChemName = Get_Element(G,"myChemName");
      }
      if (ChemId != NULL)
        bbfcheck = bbf->ReadConcBBF(G,AddExten(argv[1],"bbf"));
      if (bbfcheck)
      {
        // find all media names
        int nummed=0;
        char *med[MAXMEDIA];
        for (int m=0; m<bbf->NumEco && bbf->Eco[m].Con!=NULL; m++)
        {
          int n;
          for (n=0; n<nummed; n++)
            if (!strcmpi(bbf->Eco[m].Name,med[n]))
              break;
          if (n==nummed)
          {
            nummed++;
            med[n] = bbf->Eco[m].Name;
          }
        }

        ReadIni("EBF");

        // begin output
        resout->write(nummed);
        resout->writeln();
        for (int m=0; m<nummed; m++)
        {
          resout->write("Aquatic Organism HQ");
          resout->write(med[m]);
          resout->write(numlife);
          resout->writeln();
          for (int i=1; i<=numlife; i++)
          {
            rstrcpy(id,getvalu(LifeForm,i));
            rstrcpy(name,getvalu(LifeName,i));
            resout->write(name);
            resout->write(id);

            // see if there are anyt of this medium and and lifeform
            for (i1=0; i1<bbf->NumEco; i1++)
            {  if (!strcmpi(id,bbf->Eco[i1].Type) && !strcmpi(med[m],bbf->Eco[i1].Name)) break; }
            if (i1 < bbf->NumEco)
            {
              resout->write(numcon);
              resout->writeln();
            }
            else
            {
              Warning("No information found for ",name);
              resout->write(0);
              resout->writeln();
            }
            // do output for all chemical all outputs in list
            for (int j=1; j<=numcon && i1 < bbf->NumEco; j++)
            {
              rstrcpy(cId,getvalu(ChemId,j));
              rstrcpy(cName,getvalu(ChemName,j));
              resout->write(cName);
              resout->write(cId);
              pos1 = resout->getfpos();
              resout->write(0,"%-4d");
              resout->writeln();

              int v1 = 0;
              int u1 = -1;
              int cnt3 = 0;
              CStringParser vry;
              CStringParser unc;
              char vtemp[SMALLSTRING];
              char utemp[SMALLSTRING];
              vry.Parse(bbf->vary, CParseOptions(','));
              unc.Parse(bbf->uncr, CParseOptions(','));

              for (j1=0; j1<bbf->Eco[i1].NumCon; j1++)
              {
                if (!strcmpi(bbf->Eco[i1].Con[j1].Name,cId))
                {

                  if (u1 == unc.GetCount()-1) v1++;
                  if (u1 >= unc.GetCount()-1) u1=0;
                  else u1++;
                  rstrcpy(vtemp,vry.GetAt(v1));
                  rstrcpy(utemp,unc.GetAt(u1));

                  for (_idxPos_MAP_PTR_MAP_ITR it = mytrv.begin(); it != mytrv.end(); it++)
                  {
                    rstrcpy(temp,(*it).first.c_str());
                    cnt3 += DoTRV(bbf,i,j,i1,j1,resout,temp,(*it).second,vtemp,utemp);
                  }

                }
              }
              if (cnt3)
              {
                pos2 = resout->getfpos();
                bool nl = resout->newline;
                resout->newline = false;
                resout->setfpos(pos1);
                resout->write(cnt3,"%-4d");
                resout->setfpos(pos2);
                resout->newline = nl;
              }
              else
              {
                Warning(name, " has no chemcial information found for ", cName);
              }
            }
          }
        }
      }
      else
        Error("Missing GID file entries");
      delete bbf;
    }


    // ====================================================
    // Terrestrail SSL TRV calcs
    if (numscfs && !findfirst(AddExten(argv[1],"scf"),&ff,0))
    {
      try
      {
        scf = new EcoSeries;
      }
      catch(...)
      {
        Error("Out of memory in main");
      }

      scf->InitConc(MAXMEDIA);
      ChemId = Get_Element(G,"ChemCAS");
      ChemName = Get_Element(G,"ChemName");

      if (!(ChemId == NULL))
        bbfcheck = scf->ReadConcSCF(G,AddExten(argv[1],"scf"),argv[5]);
      if (bbfcheck)
      {
        // find all media names
        int nummed=0;
        char *med[MAXMEDIA];
        int loccnt[MAXMEDIA];

        for (int m=0; m<scf->NumEco && scf->Eco[m].Con!=NULL; m++)
        {
          int n;
          for (n=0; n<nummed; n++)
            if (!strcmpi(scf->Eco[m].Name,med[n]))
              break;
          if (n==nummed)
          {
            nummed++;
            med[n] = scf->Eco[m].Name; //producing glyphname
            loccnt[n] = 1;
          }
          else
          {
            loccnt[n]++;
          }
        }

        ReadIni("SSL");

        resout->write(nummed);
        resout->writeln();

        for (int m=0; m<nummed; m++)
        {
          resout->write("Terrestrial HQ");
          resout->write(med[m]);
          resout->write(loccnt[m]);
          resout->writeln();
          for (int i=1; i<=loccnt[m]; i++)
          {
            sprintf(id,"Location#%d",i);
            resout->write(med[m]);
            resout->write(id);

            // see if there are any of this medium and location
            for (i1=0; i1<scf->NumEco; i1++)
              if (!strcmpi(id,scf->Eco[i1].Type) && !strcmpi(med[m],scf->Eco[i1].Name))
                break;
            if (i1 < scf->NumEco)
            {
              resout->write(numcon);
              resout->writeln();
            }
            else
            {
              Warning("No information found for ",name);
              resout->write(0);
              resout->writeln();
            }

            // do output for all chemical all outputs in list
            for (int j=1; j<=numcon && i1 < scf->NumEco; j++)
            {
              rstrcpy(cId,getvalu(ChemId,j));
              rstrcpy(cName,getvalu(ChemName,j));
              resout->write(cName);
              resout->write(cId);
              for (j1=0; j1<scf->Eco[i1].NumCon; j1++)
              {  if (!strcmpi(scf->Eco[i1].Con[j1].Name,cId)) break;  }
              if (j1 < scf->Eco[i1].NumCon)
              {
                int cnt3 = 0;
                pos1 = resout->getfpos();
                resout->write(0,"%-4d");
                resout->writeln();

//                if (ratoi(info(G,"ssl")))  cnt3 += DoTRV(scf,i,j,i1,j1,resout,"SSL",0,0,0,1);
// replaced by
                for (_idxPos_MAP_PTR_MAP_ITR it = mytrv.begin(); it != mytrv.end(); it++)
                {
                   rstrcpy(temp,(*it).first.c_str());
                   cnt3 += DoTRV(scf,i,j,i1,j1,resout,temp,(*it).second);
                }

                pos2 = resout->getfpos();
                bool nl = resout->newline;
                resout->setfpos(pos1+1);
                resout->write(cnt3,"%-4d");
                resout->setfpos(pos2);
                resout->newline = nl;;
              }
              else
              {
                Warning(name, " has no chemcial information found for ", cName);
                resout->write(0);
                resout->writeln();
              }
            }
          }
        }
      }
      else
        Error("Missing GID file entries");
      delete scf;
    }

    // ====================================================
    // Aquatic frequency calcs
    if (numlocs && !findfirst(AddExten(argv[1],"wcf"),&ff,0))
    {
      resout->write(1);
      resout->writeln();
      resout->write("Aquatic Organism Effects");
      resout->write("Area of Concern");
      resout->write(numlife);
      resout->writeln();
      for (int i=1; i<=numlife; i++)
      {
        try
        {
          loc = new EcoSeries;
          con = new ConSeries;
          frq = new Series;
        }
        catch(...)
        {
          Error("Can't allocate memory for location and contaminate data");
        }

        frq->ReadFreqGID(G,i);
        loc->InitConc(numlocs);
        loc->ReadConcWCF(G,AddExten(argv[1],"wcf"),argv[5]);
        loc->Combine(con,frq);

        try
        {
          res = new Results();
          tox = new ConData();
        }
        catch(...)
        {
          Error("Out of memory in main");
        }

        tox->ReadGID(G,siteidx,i);
        resout->write(getvalu(LifeName,i));
        resout->write(getvalu(LifeForm,i));
        resout->write(tox->NumCon);
        resout->writeln();

        for (int j=0;j<tox->NumCon;j++)
        {
          res->CalcRegions(tox,con,con->Con[j].Id,j);
          res->WriteRegions(resout,con->Con[j].Id,con->Con[j].Name, getvalu(deslc,i,j+1));
          res->WriteProb(resout);
        }
        delete frq;
        delete loc;
        delete con;
        delete tox;
        delete res;
      }
    }
    resout->close();
    WrnClose();
    ErrClose();
    Close_GID(G);
    delete resout;
  }
  findclose(&ff);
  return 0;
}


