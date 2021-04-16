#include "scfPlus.h"
#include "plusop.h"

bool doScfPlus(GIDfile &gid)
{
    int i = 0;
    char input[SMALLSTRING];

    SCF outscf;
    rstrcpy(input, " ");
    while(strcmp(input, "") != 0)
    {
      i++;
      rstrcpy(input, getInput(gid, modelName, i).name);
      if(rstrcmp(input, "") != 0)
      {
        SCF nextscf(readPath, input);
        if (!outscf.add(&nextscf, modelName))
          return false;
      }
    }

    outscf.setName("all");
    outscf.scfWrite(writePath);
    return true;
}

bool doScfSync(GIDfile &gid, char *media)
{
    int k,j,i,size,year;
    long idx[6] = {0,0,0,0,0,0};
    double offX,interval,error;
    char input[SMALLSTRING];
    char msg[MEDSTRING];
    Parameter *param;

    SCF escf;

    Section *csm = gid.GetSection(modelName);
    param = csm->GetParameter("year");
    if (param!=NULL)      year = ratof(param->GetEntry(idx)->value);
    param = csm->GetParameter("interval");
    if (param!=NULL)      interval = ratof(param->GetEntry(idx)->value);
    param = csm->GetParameter("size");
    if (param!=NULL)      size = ratoi(param->GetEntry(idx)->value);
    param = csm->GetParameter("error");
    if (param!=NULL)      error = ratof(param->GetEntry(idx)->value)/100.0;

    k=1;
    idx[0] = k;
    rstrcpy(input, getInput(gid, modelName, k).name);
    param = csm->GetParameter("starttime");
    while(rstrcmp(input, ""))
    {
      if (param!=NULL)      offX = ratof(param->GetEntry(idx)->value);
      SCF oldcf(readPath, input);
      for (i=0; i<oldcf.numSet; i++)
        if (!rstrcmpi(media,oldcf.set[i]->type))
        {
          for(j=0; j<oldcf.set[i]->numCon; j++)
          {
            Series *s1 = oldcf.set[i]->pCon[j]->xy->MakeInterval(year,offX,interval,size,error);
            Series *s2 = oldcf.set[i]->pCon[j]->xy;
            if (s1)
            {
              oldcf.set[i]->pCon[j]->xy = s1;
              delete s2;
            }
            else
            {
              rstrcpy(msg,"Failed to meet syncronized criteria for module: ", input, ", chemical: ", oldcf.set[i]->pCon[j]->name);
              writeError(msg);
              return false;
            }
          }
          SCFSet *newset = oldcf.set[i]->copy();
          escf.append(newset);
        }
      k++;
      idx[0] = k;
      rstrcpy(input, getInput(gid, modelName, k).name);
    }
    escf.scfWrite(writePath);
    return true;
}
