#include "wffPlus.h"
#include "plusop.h"

bool doWffPlus(GIDfile &gid)
{
    int i = 0;
    char input[SMALLSTRING];

    WFF outwff;
    rstrcpy(outwff.name,modelName);
    rstrcpy(input, " ");
    while(strcmp(input, "") != 0)
    {
      i++;
      rstrcpy(input, getInput(gid, modelName, i).name);
      if(rstrcmp(input, "") != 0)
      {
        WFF nextwff(readPath, input);
        if (!outwff.add(&nextwff, modelName))
          return false;
      }
    }

    outwff.setName("all");
    outwff.wffWrite(writePath);
    return true;
}

bool doWffSync(GIDfile &gid, char *media)
{
    int k,j,i,size,year;
    long idx[6] = {0,0,0,0,0,0};
    double offX,interval,error;
    char input[SMALLSTRING];
    char msg[MEDSTRING];
    Parameter *param;

    WFF ewff;

    Section *csm = gid.GetSection(modelName);
    param = csm->GetParameter("year");
    if (param!=NULL)      year = ratof(param->GetEntry(idx)->value);
    param = csm->GetParameter("interval");
    if (param!=NULL)      interval = ratof(param->GetEntry(idx)->value);
    param = csm->GetParameter("size");
    if (param!=NULL)      size = ratoi(param->GetEntry(idx)->value);
    param = csm->GetParameter("error");
    if (param!=NULL)      error = ratof(param->GetEntry(idx)->value);

    k=1;
    idx[0] = k;
    rstrcpy(input, getInput(gid, modelName, k).name);
    param = csm->GetParameter("starttime");
    while(rstrcmp(input, ""))
    {
      if (param!=NULL)      offX = ratof(param->GetEntry(idx)->value);
      WFF oldcf(readPath, input);
      for (i=0; i<oldcf.numSet; i++)
        if (!rstrcmpi(media,oldcf.set[i]->type))
        {
          for(j=0; j<oldcf.set[i]->numCon; j++)
          {
            Series *s1 = oldcf.set[i]->pCon[j]->xy->MakeInterval(year, offX,interval,size,error);
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
          WFFSet *newset = oldcf.set[i]->copy();
          ewff.append(newset);
        }

      k++;
      idx[0] = k;
      rstrcpy(input, getInput(gid, modelName, k).name);
    }

    ewff.wffWrite(writePath);
    return true;
}

bool doWffSA(GIDfile &gid) {
    int k,fluxCntVal;
    long idx[6] = {0,0,0,0,0,0};
    double fluxVal;
    char input[SMALLSTRING];
    char* usrName;
    Parameter *flux;
    Parameter *fluxCnt;
    Parameter *fluxUsr;
    double xMax = 0;
    double xTemp = 0;
    double yTemp = 0;

    WFF ewff;
    rstrcpy(ewff.name,modelName);

    Section *csm = gid.GetSection(modelName);

    k=1;
    rstrcpy(input, getInput(gid, modelName, k).name);
    flux = csm->GetParameter("flux");
    fluxCnt = csm->GetParameter("fluxCount");
    fluxUsr = csm->GetParameter("name");
    if (flux==NULL || fluxCnt==NULL || fluxUsr==NULL) {
      return false;
    }
    idx[0] = 0;
    fluxCntVal = ratoi(fluxCnt->GetEntry(idx)->value);
    k=1;
    rstrcpy(input, getInput(gid, modelName, k).name);
    while(rstrcmp(input, ""))
    {
      WFF wffIn(readPath, input);
      fluxVal = 0;
      for(int num = 1; num <= fluxCntVal; num++){
        idx[0] = num;

        usrName = fluxUsr->GetEntry(idx)->value;
        if (!rstrcmpi(input, usrName)){
          fluxVal = ratod(flux->GetEntry(idx)->value);
        }
      }
      for (int i=0; i<wffIn.numSet; i++){
        if (!rstrcmpi(wffIn.set[i]->type, "aquifer")){
          if (fluxVal <= 0)
            return false;
          for(int n=0; n<wffIn.set[i]->numCon; n++){
            wffIn.set[i]->flux->InsertXpts(wffIn.set[i]->pCon[n]->xy);
            //wffIn.set[i]->pCon[n]->xy->InsertXpts(wffIn.set[i]->flux);

            wffIn.set[i]->pCon[n]->xy2 = wffIn.set[i]->pCon[n]->xy;
            wffIn.set[i]->pCon[n]->xy2->count = wffIn.set[i]->pCon[n]->xy->count;
            wffIn.set[i]->pCon[n]->xy = new Series(wffIn.set[i]->pCon[n]->xy->count);
            wffIn.set[i]->pCon[n]->xy->xInterval = wffIn.set[i]->pCon[n]->xy2->xInterval;
            for(int m=0; m<wffIn.set[i]->pCon[n]->xy->count; m++){
              yTemp =  wffIn.set[i]->flux->DiscreteInterpY(wffIn.set[i]->pCon[n]->xy2->xValues[m]);  //get yValue
              if (yTemp > 0)
                wffIn.set[i]->pCon[n]->xy2->yValues[m] /= yTemp;                                     //divide flux by quantity
              else
                wffIn.set[i]->pCon[n]->xy2->yValues[m] = 0;                                          //no water flux, no movement
              wffIn.set[i]->pCon[n]->xy2->yValues[m] *= fluxVal;                                     //multiply result by rate
              wffIn.set[i]->pCon[n]->xy->xValues[m] = wffIn.set[i]->pCon[n]->xy2->xValues[m];
              wffIn.set[i]->pCon[n]->xy->yValues[m] = 0;
            }
            wffIn.set[i]->pCon[n]->numXYSeries = 2;
          }
          rstrcpy(wffIn.set[i]->type, "Surface Water");
          Series *myflux = new Series(2);
          wffIn.set[i]->numFlux = 2;
          myflux->xValues[0] = wffIn.set[i]->flux->xValues[0];
          myflux->xValues[1] = wffIn.set[i]->flux->xValues[wffIn.set[i]->flux->count - 1];
          for (int n=0; n<myflux->count; n++){
            myflux->yValues[n] = fluxVal;
          }
          delete wffIn.set[i]->flux;
          wffIn.set[i]->flux = myflux;
        }
      }
      if (!ewff.add(&wffIn, modelName))
          return false;
      k++;
      rstrcpy(input, getInput(gid, modelName, k).name);
    }

    ewff.setName("all");
    for (int i=0; i<ewff.numSet; i++){
      for (int j=0; j<ewff.set[i]->numCon; j++){
        xTemp = ewff.set[i]->pCon[j]->xy->MaxX();
        if (xTemp > xMax){
           xMax = xTemp;
        }
      }
      xTemp = ewff.set[i]->flux->MaxX();
      if (xTemp > xMax){
         Series *myflux = new Series();
         int k;
         for(k=0; k<ewff.set[i]->flux->count && ewff.set[i]->flux->xValues[k] < xMax; k++){
           myflux->AppendEntry(ewff.set[i]->flux->xValues[k], ewff.set[i]->flux->yValues[k]);
         }
         myflux->AppendEntry(xMax, ewff.set[i]->flux->InterpY(xMax));
         delete ewff.set[i]->flux;
         ewff.set[i]->flux = myflux;
      }

    }
    ewff.wffWrite(writePath);
    return true;
}

bool doWffSAp(GIDfile &gid) {
    int k,fluxCntVal;
    long idx[6] = {0,0,0,0,0,0};
    double fluxVal;
    char input[SMALLSTRING];
    char* usrName;
    Parameter *flux;
    Parameter *fluxCnt;
    Parameter *fluxUsr;
    double xMax = 0;
    double xTemp = 0;
    double yTemp = 0;

    WFF ewff;
    rstrcpy(ewff.name,modelName);

    Section *csm = gid.GetSection(modelName);

    k=1;
    rstrcpy(input, getInput(gid, modelName, k).name);
    flux = csm->GetParameter("percentflux");
    fluxCnt = csm->GetParameter("fluxCount");
    fluxUsr = csm->GetParameter("name");
    if (flux==NULL || fluxCnt==NULL || fluxUsr==NULL) {
      return false;
    }
    idx[0] = 0;
    fluxCntVal = ratoi(fluxCnt->GetEntry(idx)->value);
    k=1;
    rstrcpy(input, getInput(gid, modelName, k).name);
    while(rstrcmp(input, ""))
    {
      WFF wffIn(readPath, input);
      fluxVal = 0;
      for(int num = 1; num <= fluxCntVal; num++){
        idx[0] = num;

        usrName = fluxUsr->GetEntry(idx)->value;
        if (!rstrcmpi(input, usrName)){
          fluxVal = ratod(flux->GetEntry(idx)->value);
        }
      }
      for (int i=0; i<wffIn.numSet; i++){
        if (!rstrcmpi(wffIn.set[i]->type, "aquifer")){
          if (fluxVal <= 0)
            return false;
          else
            fluxVal = fluxVal * wffIn.set[i]->flux->MaxY();
          for(int n=0; n<wffIn.set[i]->numCon; n++){
            wffIn.set[i]->flux->InsertXpts(wffIn.set[i]->pCon[n]->xy);
            //wffIn.set[i]->pCon[n]->xy->InsertXpts(wffIn.set[i]->flux);

            wffIn.set[i]->pCon[n]->xy2 = wffIn.set[i]->pCon[n]->xy;
            wffIn.set[i]->pCon[n]->xy2->count = wffIn.set[i]->pCon[n]->xy->count;
            wffIn.set[i]->pCon[n]->xy = new Series(wffIn.set[i]->pCon[n]->xy->count);
            wffIn.set[i]->pCon[n]->xy->xInterval = wffIn.set[i]->pCon[n]->xy2->xInterval;
            for(int m=0; m<wffIn.set[i]->pCon[n]->xy->count; m++){
              yTemp =  wffIn.set[i]->flux->DiscreteInterpY(wffIn.set[i]->pCon[n]->xy2->xValues[m]);  //get yValue
              if (yTemp > 0)
                wffIn.set[i]->pCon[n]->xy2->yValues[m] /= yTemp;                                     //divide flux by quantity
              else
                wffIn.set[i]->pCon[n]->xy2->yValues[m] = 0;                                          //no water flux, no movement
              wffIn.set[i]->pCon[n]->xy2->yValues[m] *= fluxVal;                                     //multiply result by rate
              wffIn.set[i]->pCon[n]->xy->xValues[m] = wffIn.set[i]->pCon[n]->xy2->xValues[m];
              wffIn.set[i]->pCon[n]->xy->yValues[m] = 0;
            }
            wffIn.set[i]->pCon[n]->numXYSeries = 2;
          }
          rstrcpy(wffIn.set[i]->type, "Surface Water");
          Series *myflux = new Series(2);
          wffIn.set[i]->numFlux = 2;
          myflux->xValues[0] = wffIn.set[i]->flux->xValues[0];
          myflux->xValues[1] = wffIn.set[i]->flux->xValues[wffIn.set[i]->flux->count - 1];
          for (int n=0; n<myflux->count; n++){
            myflux->yValues[n] = fluxVal;
          }
          delete wffIn.set[i]->flux;
          wffIn.set[i]->flux = myflux;
        }
      }
      if (!ewff.add(&wffIn, modelName))
          return false;
      k++;
      rstrcpy(input, getInput(gid, modelName, k).name);
    }

    ewff.setName("all");
    for (int i=0; i<ewff.numSet; i++){
      for (int j=0; j<ewff.set[i]->numCon; j++){
        xTemp = ewff.set[i]->pCon[j]->xy->MaxX();
        if (xTemp > xMax){
           xMax = xTemp;
        }
      }
      xTemp = ewff.set[i]->flux->MaxX();
      if (xTemp > xMax){
         Series *myflux = new Series();
         int k;
         for(k=0; k<ewff.set[i]->flux->count && ewff.set[i]->flux->xValues[k] < xMax; k++){
           myflux->AppendEntry(ewff.set[i]->flux->xValues[k], ewff.set[i]->flux->yValues[k]);
         }
         myflux->AppendEntry(xMax, ewff.set[i]->flux->InterpY(xMax));
         delete ewff.set[i]->flux;
         ewff.set[i]->flux = myflux;
      }
    }
    ewff.wffWrite(writePath);
    return true;
}