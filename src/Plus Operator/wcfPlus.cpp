#include "wcfPlus.h"
#include "plusop.h"

bool doWcfPlus(GIDfile &gid)
{
    int i = 0;
    char input[SMALLSTRING];

    WCF outwcf;
    rstrcpy(outwcf.name,modelName);
    rstrcpy(input, " ");
    while(strcmp(input, "") != 0)
    {
      i++;
      rstrcpy(input, getInput(gid, modelName, i).name);
      if(rstrcmp(input, "") != 0)
      {
        WCF nextwcf(readPath, input);
        if (!outwcf.add(&nextwcf, modelName))
          return false;
      }
    }

    outwcf.setName("all");
    outwcf.wcfWrite(writePath);
    return true;
}

bool doWcfSync(GIDfile &gid, char *media)
{
    int k,j,i,size,year;
    long idx[6] = {0,0,0,0,0,0};
    double offX,interval,error;
    char input[SMALLSTRING];
    char msg[MEDSTRING];
    Parameter *param;
    Parameter *pname;

    WCF ewcf;

    Section *csm = gid.GetSection(modelName);
    param = csm->GetParameter("year");
    if (param!=NULL)      year = ratof(param->GetEntry(idx)->value);
    param = csm->GetParameter("interval");
    if (param!=NULL)      interval = ratof(param->GetEntry(idx)->value);
    param = csm->GetParameter("size");
    if (param!=NULL)      size = ratoi(param->GetEntry(idx)->value);
    param = csm->GetParameter("error");
    if (param!=NULL)      error = ratof(param->GetEntry(idx)->value);
    pname = csm->GetParameter("name");

    k=1;       // model index
    idx[0] = k;
    rstrcpy(input, getInput(gid, modelName, k).name);
    param = csm->GetParameter("starttime");
    while(rstrcmp(input, ""))
    {
      Entry *e = pname->FindEntry(input);   // find sync index of source
      if (e != NULL)
      {
        idx[0] = e->idx[0];    // assign it when found
        if (param!=NULL)    offX = ratof(param->GetEntry(idx)->value);
        WCF oldcf(readPath, input);
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
            WCFSet *newset = oldcf.set[i]->copy();
            ewcf.append(newset);
          }
      }
      k++;
      idx[0] = k;
      rstrcpy(input, getInput(gid, modelName, k).name);
    }
    ewcf.wcfWrite(writePath);
    return true;
}
