//---------------------------------------------------------------------------
#pragma hdrstop

#include "conClass.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)

  CON::CON()
  {
    parent=this;
    numProgeny=0;
    type=0;
    prog=NULL;
    TS=NULL;
  }

  CON::CON(CON *pcon)
  {
    numProgeny=0;
    type=0;
    prog=NULL;
    parent = pcon;
    TS=NULL;
  }

  CON::~CON()
  {
    if (prog != NULL) delete[] prog;
    if (TS != NULL) delete TS;
    TS=NULL;
    prog=NULL; numProgeny=0;
  }

  void CON::Init(int siteidx, int ncon, int nprog,
         element *fscname, element *fscasid,
         element *nds, element *ktype)
  {
    strcpy(name,getvalu(fscname,siteidx,ncon,nprog));
    strcpy(cas,getvalu(fscasid,siteidx,ncon,nprog));
    type = atoi(getvalu(ktype,siteidx,ncon,nprog));
//  parent=NULL;
    if (nprog==0)
    {
      numProgeny = atoi(getvalu(nds,siteidx,ncon,nprog));
      if (numProgeny > 0)
      {
        prog = new CON*[numProgeny];
        for (int i=1;i<=numProgeny;i++)
          {
          prog[i-1] = new CON(this);
          prog[i-1]->Init(siteidx,ncon,i,fscname,fscasid,nds,ktype);
          }
      }
    }
    TS = new Series(0);
  }

  void CON::AddSeries(char *units, char *timeunits, Series *addTS)
  {
    TS->Add(addTS);
    strcpy(TS->Id,cas);
    strcpy(TS->Name,name);
    strcpy(TS->yUnits,units);
    strcpy(TS->xUnits,timeunits);
  }

  void CON::Write(fcsv *fout)
  {
    fout->write(name);
    fout->write(cas);
    fout->write(numProgeny);
    TS->WriteSeries(fout);
    for (int nprog=1; nprog<=numProgeny; nprog++)
      prog[nprog-1]->Write(fout);
  }

  void CON::Read(fcsv *fin)
  {
    fin->read(name);
    fin->read(cas);
    fin->read(&numProgeny);
    TS = new Series();
    TS->ReadSeries(fin);
    if (numProgeny>0)
    {
      prog = new CON*[numProgeny];
      for (int i=1;i<=numProgeny;i++)
      {
        prog[i-1] = new CON(this);
        prog[i-1]->Read(fin);
      }
    }
  }

