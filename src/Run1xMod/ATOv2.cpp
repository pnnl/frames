/*______________________________________________________________________________

  Date:       1993 - 2004
  Company:    Pacific Northwest National Laboratories
  Battelle Corporation

    ** ATOv2.cpp **
    ________________________________________________________________________________
    __Modifiication  History________________________________________________________

      DATE     WHO  DESCRIPTION
______________________________________________________________________________*/


#include "ATOv2.h"

bool savegrid;

//-----------------------------------------------------------------------------------
/*  Pre: t has values: outprodunit, moist, releasetype, outprodname

  Post: Construct setname(dictionary name) for t
  Return: "ChemAirWetDepChronic" Note: NOT ".dic"
*/
char *ATOGrid_v2::getDictionaryName(ATOState *t)
{
  char *prodname;
  static char dictionary[SMALLSTRING];

  if(!rstrncmpi(t->outprodname, AIR, 3))
    prodname = CONC;
  else
    if(!rstrncmpi(t->outprodname, "External", 3))
      prodname = "ExternalDose";
    else
      prodname = DEP;
  rstrcpy(dictionary, t->prefix, MEDIUM, t->moist, prodname);
  return dictionary;
}
//-----------------------------------------------------------------------------------
/*  Pre: t has values: outprodunit, fluxtype, outprodname

  Post: construct variable name ex. "ChemSolidDep"
  Return: ex: "ChemSolidDep"
*/
char* ATOGrid_v2::getVariableName(ATOState *t)
{
  char *flux;
  char *prodname;
  static char variablename[SMALLSTRING];

  flux = blank;
  if(!rstrncmpi(t->fluxtype, GAS, 3))
    flux = GAS;
  else if(!rstrncmpi(t->fluxtype, LIQUID, 3))
    flux = LIQUID;
  else if(!rstrncmpi(t->fluxtype, "Particle", 8))
    flux = SOLID;

  if(!rstrncmpi(t->outprodname, AIR, 3))             // 'Air Exposure' Concentration  |  'Air Concentration'
    prodname = CONC;
  else
    if(!rstrncmpi(t->outprodname, "External", 3))    // 'External Dose'
      prodname = "ExternalDose";
    else                                             // 'Total Deposition' | 'Deposition'
      prodname = DEP;

  if (!rstrncmpi(t->releasetype, ACUTE, 5) && rstrncmpi(t->outprodname, "External", 3))
    return rstrcpy(variablename, flux, "Int", prodname);
  else
    return rstrcpy(variablename, flux, prodname);
}
//-----------------------------------------------------------------------------------
int ATOGrid_v2::WriteDataset(long pid, icsv *inf, Glyph *g, ATOState *t)
{
  int i, j, pt;
  int ErrorCode;
  double dumFloat;
  double dumFloat1;
  double dumFloat2;
  char ptname[SMALLSTRING];
  char varset[SMALLSTRING];
  char varname[SMALLSTRING];

  *inf >> t->outprodname >> t->fluxtype >> t->moist >> t->vunits;
  *inf >> axis1num >> axis1units >> axis2num >> axis2units >> NewLn;

  // fix case sensitivity
  if(!rstrncmpi(t->moist, WET, 3)) rstrcpy(t->moist, WET);
  else if(!rstrncmpi(t->moist, DRY, 3)) rstrcpy(t->moist, DRY);
  else if(!rstrncmpi(t->moist, TOTAL, 5)) rstrcpy(t->moist, TOTAL);

  //Getflux index
  t->fluxidx = 0;
  if(!rstrncmpi(t->fluxtype, GAS, 3))
    t->fluxidx = ratoi(&t->fluxtype[3]);
  else if(!rstrncmpi(t->fluxtype, "Particle", 8))
    t->fluxidx = ratoi(&t->fluxtype[8]);
  else if(!rstrncmpi(t->fluxtype, ALL, 3))
    t->fluxidx = 0;

  GetSetName(getDictionaryName(t), varset, g->ID2x, g->osetlist);    //return ex "mod1.AcuteChemAirConc"
  rstrcpy(varname, getVariableName(t));                        //return ex "SolidConc"

  GetSetName(concat(MEDIUM,PTS), setname, g->ID2x, g->osetlist);
  if(!rstrcmpi(t->spatialtype, PTS))
  {
    if(savegrid)
    {
      ErrorCode = PutString(pid, setname, SETATTRIBUTEDES, blank, SetIdx(1), "Coordinate Type");
      ErrorCode = PutString(pid, setname, SETATTRIBUTEDES, blank, SetIdx(2), "Point alignment");
      ErrorCode = PutString(pid, setname, SETATTRIBUTEDES, blank, SetIdx(3), "Number of 1st axis");
      ErrorCode = PutString(pid, setname, SETATTRIBUTEDES, blank, SetIdx(4), "Number of 2nd axis");

      ErrorCode = PutString(pid, setname, SETATTRIBUTES, blank, SetIdx(1), t->coordtype);
      ErrorCode = PutString(pid, setname, SETATTRIBUTES, blank, SetIdx(2), t->spatialtype);
      sprintf(dummy,"%d",axis1num);      ErrorCode = PutString(pid, setname, SETATTRIBUTES, blank, SetIdx(3), dummy);
      sprintf(dummy,"0");                ErrorCode = PutString(pid, setname, SETATTRIBUTES, blank, SetIdx(4), dummy);

      for(i = 0; i<axis1num; i++)      {
        *inf >> ptname;
        ErrorCode = PutString(pid, setname, FEATURE, blank, SetIdx(i+1), ptname);
      }
      *inf >> NewLn;
      for(i = 0; i<axis1num; i++)      {
        *inf >> dumFloat;
        ErrorCode = PutFloat(pid, setname, FEATUREPTS, axis1units, SetIdx(i+1, 1, 1), dumFloat);
      }
      *inf >> NewLn;
      for(i = 0; i<axis1num; i++)      {
        *inf >> dumFloat;
        ErrorCode = PutFloat(pid, setname, FEATUREPTS, axis1units, SetIdx(i+1, 2, 1), dumFloat);
      }
      *inf >> NewLn;
      for(i = 0; i<axis1num; i++)
        ErrorCode = PutFloat(pid, setname, FEATUREPTS, axis1units, SetIdx(i+1, 3, 1), 2.0);
      savegrid = false;
    }
    else
      *inf >> NewLn >> NewLn >> NewLn;

    *inf >> dumFloat;  // scan off -99
    for(i = 0; i<axis1num; i++)
    {
      *inf >> dumFloat;
      ErrorCode = PutFloat(pid, varset, TIMEPTS, t->tunits, SetIdx(i+1, t->conidx, t->timeidx), t->time);
      if (t->fluxidx)
        ErrorCode = PutFloat(pid, varset, varname, t->vunits, SetIdx(i+1, t->conidx, t->fluxidx, t->timeidx), dumFloat);
      else
        ErrorCode = PutFloat(pid, varset, varname, t->vunits, SetIdx(i+1, t->conidx, t->timeidx), dumFloat);
    }
    *inf >> NewLn;
  }
  else
  {
    pt = 0;
    axis1values = new double[axis1num];  TrackLocation(axis1values);
    for(i = 0; i < axis1num; i++)
      *inf >> axis1values[i];
    *inf >> NewLn;
    if(savegrid)
    {
      ErrorCode = PutString(pid, setname, SETATTRIBUTEDES, blank, SetIdx(1), "Coordinate Type");
      ErrorCode = PutString(pid, setname, SETATTRIBUTEDES, blank, SetIdx(2), "Point alignment");
      ErrorCode = PutString(pid, setname, SETATTRIBUTEDES, blank, SetIdx(3), "Number of 1st axis");
      ErrorCode = PutString(pid, setname, SETATTRIBUTEDES, blank, SetIdx(4), "Number of 2nd axis");

      ErrorCode = PutString(pid, setname, SETATTRIBUTES, blank, SetIdx(1), t->coordtype);
      ErrorCode = PutString(pid, setname, SETATTRIBUTES, blank, SetIdx(2), t->spatialtype);
      sprintf(dummy,"%d",axis1num);      ErrorCode = PutString(pid, setname, SETATTRIBUTES, blank, SetIdx(3), dummy);
      sprintf(dummy,"%d",axis2num);      ErrorCode = PutString(pid, setname, SETATTRIBUTES, blank, SetIdx(4), dummy);

      for(j = 0; j<axis2num; j++)
      {
        *inf >> dumFloat;
        for(i = 0; i<axis1num; i++)
        {
          pt++;
          dumFloat1 = dumFloat;
          sprintf(ptname,"%s#%d",g->ID1x,pt);
          t->dist = axis1values[i];
          if(!strcmpi(t->coordtype,POLAR))
            convertFromCompass(t->dist, dumFloat1);
          ErrorCode = PutString(pid, setname, FEATURE, blank, SetIdx(pt), ptname);
          ErrorCode = PutFloat(pid, setname, FEATUREPTS, axis1units, SetIdx(pt, 1, 1), t->dist);
          ErrorCode = PutFloat(pid, setname, FEATUREPTS, axis1units, SetIdx(pt, 2, 1), dumFloat1);
          ErrorCode = PutFloat(pid, setname, FEATUREPTS, axis1units, SetIdx(pt, 3, 1), 2.0);
          *inf >> dumFloat2;
          ErrorCode = PutFloat(pid, varset, TIMEPTS, t->tunits, SetIdx(pt, t->conidx, t->timeidx), t->time);
          if (t->fluxidx)
            ErrorCode = PutFloat(pid, varset, varname, t->vunits, SetIdx(pt, t->conidx, t->fluxidx, t->timeidx), dumFloat2);
          else // Value is total particle/vapor/gas therfore no flux index
            ErrorCode = PutFloat(pid, varset, varname, t->vunits, SetIdx(pt, t->conidx, t->timeidx), dumFloat2);
        }
        *inf >> NewLn;
      }
    }
    else
      for(j = 0; j<axis2num; j++)
      {
        *inf >> dumFloat;
        for(i = 0; i<axis1num; i++)
        {
          pt++;
          *inf >> dumFloat2;
          ErrorCode = PutFloat(pid, varset, TIMEPTS, t->tunits, SetIdx(pt, t->conidx, t->timeidx), t->time);
          if (t->fluxidx)
            ErrorCode = PutFloat(pid, varset, varname, t->vunits, SetIdx(pt, t->conidx, t->fluxidx, t->timeidx), dumFloat2);
          else // Value is total particle/vapor/gas therfore no flux index
            ErrorCode = PutFloat(pid, varset, varname, t->vunits, SetIdx(pt, t->conidx, t->timeidx), dumFloat2);
        }
        *inf >> NewLn;
      }
    TrackDelete(axis1values);
    savegrid = false;
  }
  return 0;
}
//-----------------------------------------------------------------------------------
int ATOTimePeriod_v2::WriteDataset(long pid, icsv *inf, Glyph *g, ATOState *t)
{
  int i;
  int outputnum;

  *inf >> t->time >> t->tunits >> outputnum >> NewLn;
  for(i = 0; i < outputnum; i++)
  {
    ATOGrid_v2 *p = new ATOGrid_v2();   TrackLocation(p);
    p->WriteDataset(pid, inf, g, t);
    TrackDelete(p);
  }
  return 0;
}
//-----------------------------------------------------------------------------------
int ATOCon_v2::WriteDataset(long pid, icsv *inf, Glyph *g, ATOState *t, STR_VEC_PTR_VEC *conlist)
{
  int i;
  int ErrorCode;
  int idx[MMF_MAX_DIM];

  *inf >> t->chemname >> t->casid >> numtimes >> numprogs >> NewLn;

  for(STR_VEC_PTR_VEC_ITR ic = conlist->begin(); ic!= conlist->end(); ++ic)
  {
    STR_VEC_ITR iv = (*ic)->begin();
    rstrcpy(t->prefix, iv->c_str());
    ++iv;
    rstrcpy(dummy, iv->c_str());
    if(0 == rstrcmpi(t->casid, dummy)) break;
  }
  for(i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;
  GetSetName(concat(t->prefix, LIST), setname, blank, g->isetlist);
  ErrorCode = VarLookUp(pid, setname, CASID, t->casid, idx);
  t->conidx = idx[0];

  for(i = 1; i <= numtimes; i++)
  {
    t->timeidx = i;
    ATOTimePeriod_v2 *p = new ATOTimePeriod_v2();   TrackLocation(p);
    p->WriteDataset(pid, inf, g, t);
    TrackDelete(p);
  }
  return 0;
}

//-----------------------------------------------------------------------------------
int ATOFluxType_v2::WriteDataset(long pid, icsv *inf, Glyph *g, int &ngas, int &nliq, int &nsol)
{
  int ErrorCode = 0;
  *inf >> fluxname >> radius >> radunits >> density >> denunits >> NewLn;

  if(rstrstr(fluxname, GAS))
  {
    ngas++;
    GetSetName("SuspendedGasesDef", setname, g->ID2x, g->osetlist);
    if(strlen(setname))
    {
      ErrorCode += PutString(pid, setname, NAME, blank, SetIdx(ngas), fluxname);
      ErrorCode += PutFloat(pid, setname, REACTIVE, radunits, SetIdx(ngas), radius);
      ErrorCode += PutFloat(pid, setname, DENSITY, denunits, SetIdx(ngas), density);
    }
  }
  else if(rstrstr(fluxname, LIQUID))
  {
    nliq++;
    GetSetName("SuspendedLiquidsDef", setname, g->ID2x, g->osetlist);
    if(strlen(setname))
    {
      ErrorCode += PutString(pid, setname, NAME, blank, SetIdx(nliq), fluxname);
      ErrorCode += PutFloat(pid, setname, MIDSIZE, radunits, SetIdx(nliq), radius);
    }
  }
    // "All" is assumed to be all states, we will assume "particle"
    // for storage of the average particle size and density
  else if(rstrstr(fluxname, "Particle") || rstrstr(fluxname, ALL))
  {
    nsol++;
    GetSetName("SuspendedSolidsDef", setname, g->ID2x, g->osetlist);
    if(strlen(setname))
    {
      ErrorCode += PutString(pid, setname, NAME, blank, SetIdx(nsol), fluxname);
      ErrorCode += PutFloat(pid, setname, MIDSIZE, radunits, SetIdx(nsol), radius);
      ErrorCode += PutFloat(pid, setname, DENSITY, denunits, SetIdx(nsol), density);
    }
  }
  return ErrorCode;
}
//--------------------------------------------------------------------------------
int ATOSet_v2::WriteDataset(long pid, icsv *inf, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int i;
  int ngas = 0;
  int nsol = 0;
  int nliq = 0;
  int ErrorCode;

  *inf >> fluxnum >> modname >> NewLn;
  for(i = 0; i < fluxnum; i++)
  {
    ATOFluxType_v2 *q = new ATOFluxType_v2();      TrackLocation(q);
    q->WriteDataset(pid, inf, g, ngas, nsol, nliq);
    TrackDelete(q);
  }

  ATOState *t = new ATOState();  TrackLocation(t);
  *inf >> t->releasetype >> t->coordtype >> t->spatialtype >> numcons;
  if(!rstrcmpi(t->releasetype, ACUTE))
  {
    GetSetName(ACUTE, setname, g->ID2x, g->osetlist);
    ErrorCode = PutString(pid, setname, FEATURE, blank, SetIdx(1), "Start Date");

    ErrorCode += PutString(pid, setname, ATTRIBUTEDES, blank, SetIdx(1), "Year");
    ErrorCode += PutString(pid, setname, ATTRIBUTEDES, blank, SetIdx(2), "Month");
    ErrorCode += PutString(pid, setname, ATTRIBUTEDES, blank, SetIdx(3), "Day");
    ErrorCode += PutString(pid, setname, ATTRIBUTEDES, blank, SetIdx(4), "Hour");

    *inf >> dummy;    ErrorCode += PutString(pid, setname, ATTRIBUTES, blank, SetIdx(1,1), dummy);
    *inf >> dummy;    ErrorCode += PutString(pid, setname, ATTRIBUTES, blank, SetIdx(1,2), dummy);
    *inf >> dummy;    ErrorCode += PutString(pid, setname, ATTRIBUTES, blank, SetIdx(1,3), dummy);
    *inf >> dummy;    ErrorCode += PutString(pid, setname, ATTRIBUTES, blank, SetIdx(1,4), dummy);
    TIME=ACUTE;
  }
  else
    TIME=blank;
  *inf >> NewLn;

  for(i = 1; i <= numcons; i++)
  {
    ATOCon_v2 *p = new ATOCon_v2();    TrackLocation(p);
    p->WriteDataset(pid, inf, g, t, conlist);
    TrackDelete(p);
  }
  TrackDelete(t);

  return 0;
}
//-----------------------------------------------------------------------------------
int ATO_v2::WriteDataset(long pid, char *fuiname, Glyph *g, STR_VEC_PTR_VEC *conlist)
{
  int i;
  int numInfo;

  Log("ATO_v2::WriteDataset");
  MEDIUM = AIR;

  rstrcpy(dummy, fuiname, _DOT, _ATO);
  inf = new icsv(dummy, '\"');  TrackLocation(inf);
  if(inf->ok())
  {
    *inf >> numInfo >> NewLn;
    for(i = 0; i < numInfo; i++)
      *inf >> dummy >> NewLn;

    *inf >> numSet >> NewLn;
    for(i = 0; i < numSet; i++)
    {
      savegrid = true;
      ATOSet_v2 *p = new ATOSet_v2(); TrackLocation(p);
      p->WriteDataset(pid, inf, g, conlist);
      TrackDelete(p);
    }
  }
  TrackDelete(inf);
  return 0;
}

//-----------------------------------------------------------------------------------------
int ATOGrid_v2::WriteFile(long pid, ocsv *fp, Glyph *g, ATOState *t, int fIdx)
{
  int i, j;
  int ErrorCode;
  int pt = 0;
  int nrec = 0;
  double dumFloat;

  *fp << t->prodname << t->fluxtype << t->moist << t->vunits;
  *fp << axis1num << axis1units << axis2num << axis2units << NewLn; nrec++;

  for(j=0;j<axis2num;j++)
    for(i=0;i<axis1num;i++)
    {
      pt++;
      // we may want to assure we are at the right time
      // right now we will assume that all series have the same times so we'll use the index
      if (fIdx)
        ErrorCode = GetFloat(pid, setname, t->name, t->vunits, SetIdx(pt, t->conidx, fIdx, t->timeidx), &dumFloat);
      else  //
        ErrorCode = GetFloat(pid, setname, t->name, t->vunits, SetIdx(pt, t->conidx, t->timeidx), &dumFloat);
      values[i][j] = dumFloat;
    }

  for(i = 0; i < axis1num; i++)
    *fp << axis1values[i];
  *fp << NewLn; nrec++;

  for(j = 0; j < axis2num; j++)
  {
    *fp << axis2values[j];
    for(i = 0; i < axis1num; i++)
      *fp << values[i][j];
    *fp << NewLn; nrec++;
  }
  return nrec;
}
//-----------------------------------------------------------------------------------------
int ATOTimePeriod_v2::WritePoints(long pid, ocsv *fp, Glyph *g, ATOState *t, int fIdx, char *ID)
{
  int i;
  int numLoc;
  int nrec = 0;
  int ErrorCode;
  double dumFloat;
  char name[SMALLSTRING];
  char sname[SMALLSTRING];

  rstrcpy(sname,setname);

  numLoc=0;
  GetSetName(concat(MEDIUM,PTS), setname, ID, g->isetlist);
  ErrorCode = GetVarDimSize(pid, setname, FEATURE, SetIdx(), &numLoc);

  *fp << t->prodname << t->fluxtype << t->moist << t->vunits;
  *fp << numLoc << NewLn; nrec++;

  for(i = 0; i < numLoc; i++)
  {
    ErrorCode = GetString(pid, setname, FEATURE, blank, SetIdx(i+1), name);
    *fp << name;
  }
  *fp << NewLn; nrec++;

  for(i = 0; i < numLoc; i++)
  {
    ErrorCode = GetFloat(pid, setname, FEATUREPTS, M, SetIdx(i+1, 1, 1), &dumFloat);
    *fp << dumFloat;
  }
  *fp << NewLn; nrec++;

  for(i = 0; i < numLoc; i++)
  {
    ErrorCode = GetFloat(pid, setname, FEATUREPTS, M, SetIdx(i+1, 2, 1), &dumFloat);
    *fp << dumFloat;
  }
  *fp << NewLn; nrec++;
  *fp << -99;

  for(i = 0; i < numLoc; i++)
  {
    ErrorCode = GetFloat(pid, sname, t->name, t->vunits, SetIdx(i+1, t->conidx, fIdx, t->timeidx), &dumFloat);
    *fp << dumFloat;
  }
  *fp << NewLn; nrec++;

  return nrec;
}
//-----------------------------------------------------------------------------------------
int ATOTimePeriod_v2::forEachPhase(long pid, ocsv *fp, Glyph *g, ATOState *t, int i, char *ID)
{
  int nrec = 0;
  if(t->grid != NULL)
    nrec += t->grid->WriteFile(pid, fp, g, t, i);
  else// a list of points
    nrec += WritePoints(pid, fp, g, t, i, ID);
  return nrec;
}
//----------------------------------------------------------------------------------
int ATOTimePeriod_v2::forEachOutProd(long pid, ocsv *fp, Glyph *g, ATOState *t, char *ID)
{
  int i;
  int n;
  int ErrorCode;
  int nrec = 0;

  rstrcpy(t->phase, GAS);
  rstrcpy(t->name, t->phase, t->outprodname); //GasConc or GasDep
  for(i=1; i<=t->ngas; i++)
  {
    n=0;
    sprintf(t->fluxtype,"%s %d", t->phase, i);
    ErrorCode = GetVarDimSize(pid, setname, t->name, SetIdx(1,t->conidx,i), &n);
    if(n > 0) nrec += forEachPhase(pid, fp, g, t, i, ID);
  }

  rstrcpy(t->phase, LIQUID);
  rstrcpy(t->name, t->phase, t->outprodname); //LiquidConc or LiquidDep
  for(i=1; i<=t->nliq; i++)
  {
    n=0;
    sprintf(t->fluxtype,"%s %d", t->phase, i);
    ErrorCode = GetVarDimSize(pid, setname, t->name, SetIdx(1,t->conidx,i), &n);
    if(n > 0) nrec += forEachPhase(pid, fp, g, t, i, ID);
  }

  rstrcpy(t->phase, SOLID);
  rstrcpy(t->name, t->phase, t->outprodname); //SolidConc or SolidDep
  for(i=1; i<=t->nsol; i++)
  {
    n=0;
    sprintf(t->fluxtype,"Particle %d", i);
    ErrorCode = GetVarDimSize(pid, setname, t->name, SetIdx(1,t->conidx,i), &n);
    if(n > 0) nrec += forEachPhase(pid, fp, g, t, i, ID);
  }

  rstrcpy(t->phase, ALL);
  if (TIME==ACUTE)
    rstrcpy(t->name, "Int", t->outprodname); //IntConc or IntDep
  else
    rstrcpy(t->name, t->outprodname); //Conc or Dep

  n=0;
  rstrcpy(t->fluxtype, ALL, " 1");
  ErrorCode = GetVarDimSize(pid, setname, t->name, SetIdx(1,t->conidx), &n);
  if(n > 0) nrec += forEachPhase(pid, fp, g, t, 0, ID);
  ClearErrors(pid);
  return nrec;
}
//----------------------------------------------------------------------------------
int ATOTimePeriod_v2::WriteFile(long pid, ocsv *fp, Glyph *g, char *modId, ATOState *t, char *ID)
{
  int n;
  int len;
  int ErrorCode;
  int nrec = 0;
  char dictName[SMALLSTRING];

  rstrcpy(dictName, &modId[3], _DOT, t->prefix, AIR); //create "13.AcuteChemAir" or "mod1.ChronicRadAir"
  len = strlen(dictName);
  for(STR_VEC_MAP_ITR it = g->isetlist.begin(); it != g->isetlist.end(); ++it)
  {
    char tempp[SMALLSTRING];
    rstrcpy(tempp,(*it).first.c_str());
    STR_VEC sv = (*it).second;
    for(STR_VEC_ITR is = sv.begin(); is != sv.end(); ++is)
    {
      rstrcpy(setname,(*is).c_str());
      if(rstrstr(setname, dictName))
      {
        if(rstrstr(setname, "AirExternalDose"))
        {
          rstrcpy(t->vunits,"Sv");
          rstrcpy(t->prodname, "External Dose");
          rstrcpy(t->outprodname, "ExternalDose");
          rstrcpy(t->moist, blank);
          rstrcpy(t->name, t->outprodname);
          n=0;
          ErrorCode = GetVarDimSize(pid, setname, t->name, SetIdx(1,t->conidx), &n);
          if(n > 0) nrec += forEachPhase(pid, fp, g, t, 0, ID);
        }
        else if(rstrstr(setname, "AirConc"))
        {
          if(TIME==ACUTE)
          {
            if(!rstrcmpi(t->prefix,RAD)) rstrcpy(t->vunits, "(Bq-s)/m^3");
            else rstrcpy(t->vunits, "(kg-s)/m^3");
            rstrcpy(t->prodname, "Air Exposure");
          }
          else
          {
            if(!rstrcmpi(t->prefix,RAD)) rstrcpy(t->vunits, "Bq/m^3");
            else rstrcpy(t->vunits, "kg/m^3");
            rstrcpy(t->prodname, "Air Concentration");
          }
          rstrcpy(t->outprodname, CONC);
          rstrcpy(t->moist, blank);
          nrec += forEachOutProd(pid, fp, g, t, ID);
        }
        else
        {
          if(TIME==ACUTE)
          {
            if(!rstrcmpi(t->prefix,RAD)) rstrcpy(t->vunits, "Bq/m^2");
            else  rstrcpy(t->vunits, "kg/m^2");
            rstrcpy(t->prodname, "Total Deposition");
          }
          else
          {
            if(!rstrcmpi(t->prefix,RAD)) rstrcpy(t->vunits, "(Bq/m^2)/s");
            else rstrcpy(t->vunits, "kg/m^2/yr");
            rstrcpy(t->prodname, "Deposition Rate");
          }
          rstrcpy(t->outprodname, DEP);
          if(rstrstr(setname, "AirWetDep"))
          {
            rstrcpy(t->moist, WET);
            nrec += forEachOutProd(pid, fp, g, t, ID);
          }
          else if(rstrstr(setname, "AirDryDep"))
          {
            rstrcpy(t->moist, DRY);
            nrec += forEachOutProd(pid, fp, g, t, ID);
          }
          else if(rstrstr(setname, "AirTotalDep"))
          {
            rstrcpy(t->moist, TOTAL);
            nrec += forEachOutProd(pid, fp, g, t, ID);
          }
        }
      }
    }
  }
  return nrec;
}

int ATOCon_v2::WriteFile(long pid, ocsv *fp, Glyph *g, char *modId, ATOState *t, STR_VEC *conlist, char *ID)
{
  int i;
  int len;
  int pcnt;
  int n;
  int nrec;
  int ErrorCode;
  int idx[MMF_MAX_DIM];
  char *product;
  char dictName[SMALLSTRING];
  char variable[SMALLSTRING];
  STR_VEC_MAP_ITR it;
  Series_v2 *series = NULL;

  Log("ATOCon_v2::WriteFile");
  nrec = 0;
  STR_VEC_ITR iv = conlist->begin();
  rstrcpy(t->prefix, iv->c_str());
  ++iv;
  rstrcpy(t->casid, iv->c_str());
  for(i = 0; i<MMF_MAX_DIM; i++) idx[i] = 0;
  GetSetName(concat(t->prefix, LIST), setname, blank, g->isetlist);
  ErrorCode = VarLookUp(pid, setname, CASID, t->casid, idx);
  ErrorCode = GetString(pid, setname, NAME, blank, idx, t->chemname);
  t->conidx = idx[0];
  idx[0] = 1;
  idx[1] = t->conidx;
  idx[2] = 1;

  //reflect all time pts into series
  rstrcpy(t->tunits,YEAR);

  rstrcpy(dictName, &modId[3], _DOT, t->prefix, AIR); //create "13.AcuteChemAir" or "mod1.ChronicRadAir"
  len = strlen(dictName);
  pcnt = 0;

  for(it = g->isetlist.begin(); it != g->isetlist.end(); ++it)
  {
    char tempp[SMALLSTRING];
    rstrcpy(tempp,(*it).first.c_str());
    STR_VEC sv = (*it).second;
    for(STR_VEC_ITR is = sv.begin(); is != sv.end(); ++is)
    {
      rstrcpy(setname,(*is).c_str());
      if(rstrstr(setname, dictName))
      {
        product = blank;
        if(rstrstr(setname, "AirConc"))                 { product = CONC;}
        else if(rstrstr(setname, "AirWetDep"))          { product = DEP;}
        else if(rstrstr(setname, "AirDryDep"))          { product = DEP;}
        else if(rstrstr(setname, "AirTotalDep"))        { product = DEP;}
        if(rstrstr(setname, "ExternalDose"))
        {
          n=0;
          GetVarDimSize(pid, setname, TIMEPTS, SetIdx(1,t->conidx), &n);
          if(n > 0) pcnt++;
        }
        else
        {
          if (TIME==ACUTE)  rstrcpy(dummy,"Int");
          else rstrcpy(dummy,blank);

          rstrcpy(variable, dummy, GAS, product);
          for(i=1; i<=t->ngas; i++)
          {
            n=0;
            GetVarDimSize(pid, setname, variable, SetIdx(1,t->conidx,i), &n);
            if(n > 0) pcnt++;
          }

          rstrcpy(variable, dummy, LIQUID, product);
          for(i=1; i<=t->nliq; i++)
          {
            n=0;
            GetVarDimSize(pid, setname, variable, SetIdx(1,t->conidx,i), &n);
            if(n > 0) pcnt++;
          }

          rstrcpy(variable, dummy, SOLID, product);
          for(i=1; i<=t->nsol; i++)
          {
            n=0;
            GetVarDimSize(pid, setname, variable, SetIdx(1,t->conidx,i), &n);
            if(n > 0) pcnt++;
          }

          rstrcpy(variable, dummy, product);
          n=0;
          GetVarDimSize(pid, setname, variable, SetIdx(1,t->conidx), &n);
          if(n > 0) pcnt++;
        }

        if(series)
        {
          Series_v2 *tempseries = new Series_v2(YEAR, blank);  TrackLocation(tempseries);
          if (tempseries->ReadTimes(pid, setname, TIMEPTS, SetIdx(1,t->conidx), 3))
            series->InsertXpts(tempseries);
          TrackDelete(tempseries);
        }
        else
        {
          series = new Series_v2(YEAR, blank);  TrackLocation(series);
          series->ReadTimes(pid, setname, TIMEPTS, SetIdx(1,t->conidx), 3);
        }
      }
    }
  }

  //all times should now be reflected onto series
  *fp << t->chemname << t->casid << series->count << 0 << NewLn;
  nrec+= 1;
  if(series->count < 1)
  {
    TrackDelete(series);
    return nrec;
  }

  for(i = 0; i < series->count; i++)
  {
    *fp << series->xValues[i] << t->tunits << pcnt << NewLn;
    nrec+= 1;
    ATOTimePeriod_v2 *p = new ATOTimePeriod_v2();  TrackLocation(p);
    p->time = series->xValues[i];
    t->timeidx = i+1;
    rstrcpy(p->unit, YEAR);
    nrec += p->WriteFile(pid, fp, g, modId, t, ID);
    TrackDelete(p);
  }
  TrackDelete(series);
  return nrec;
}

//----------------------------------------------------------------------------------
int ATOSet_v2::WriteFile(long pid, ocsv *fp, Glyph *g, char *modId, STR_VEC_PTR_VEC *conlist, char *ID)
{
  int i;
  int j;
  int pt;
  int ErrorCode;
  int nrec = 0;
  int numFluxTypes;
  int axis1num;
  int axis2num;
  double dumFloat;
  char temp[SMALLSTRING];
  ATOState t;


  //look up coordtype and set up grid for writing if used
  GetSetName(concat(MEDIUM,PTS),setname, ID, g->isetlist ); // my id or my sources id
  ErrorCode = GetString(pid, setname, SETATTRIBUTES, blank, SetIdx(1), t.coordtype);
  ErrorCode += GetString(pid, setname, SETATTRIBUTES, blank, SetIdx(2), t.spatialtype);
  ErrorCode += GetString(pid, setname, SETATTRIBUTES, blank, SetIdx(3), temp);
  axis1num = ratoi(temp);
  ErrorCode += GetString(pid, setname, SETATTRIBUTES, blank, SetIdx(4), temp);
  axis2num = ratoi(temp);
  t.ngas = 0;
  t.nsol = 0;
  t.nliq = 0;
  GetSetName("SuspendedGasesDef", setname, ID, g->isetlist);
  ErrorCode = GetVarDimSize(pid, setname, NAME, SetIdx(), &t.ngas);
  GetSetName("SuspendedSolidsDef", setname, ID, g->isetlist);
  ErrorCode = GetVarDimSize(pid, setname, NAME, SetIdx(), &t.nsol);
  GetSetName("SuspendedLiquidsDef", setname, ID, g->isetlist);
  ErrorCode = GetVarDimSize(pid, setname, NAME, SetIdx(), &t.nliq);
  numFluxTypes = t.nliq+t.nsol+t.ngas;

  *fp << numFluxTypes << modId << NewLn;
  nrec++;

  for(i = 0; i<t.ngas; i++)
  {
    GetSetName("SuspendedGasesDef", setname, ID, g->isetlist);
    ErrorCode = GetString(pid, setname, NAME, blank, SetIdx(i+1), dummy);
    *fp << dummy;
    ErrorCode = GetFloat(pid, setname, REACTIVE, FRACTION, SetIdx(i+1), &dumFloat);
    *fp << dumFloat << FRACTION;
    ErrorCode = GetFloat(pid, setname, DENSITY, GCM3, SetIdx(i+1), &dumFloat);
    *fp << dumFloat << GCM3 << NewLn;
    nrec++;
  }
  for(i = 0; i<t.nliq; i++)
  {
    GetSetName("SuspendedLiquidsDef", setname, ID, g->isetlist);
    ErrorCode = GetString(pid, setname, NAME, blank, SetIdx(i+1), dummy);
    *fp << dummy;
    ErrorCode = GetFloat(pid, setname, MIDSIZE, UM, SetIdx(i+1), &dumFloat);
    *fp << dumFloat << UM << NewLn;
    nrec++;
  }
  for(i = 0; i<t.nsol; i++)
  {
    GetSetName("SuspendedSolidsDef", setname, ID, g->isetlist);
    ErrorCode = GetString(pid, setname, NAME, blank, SetIdx(i+1), dummy);
    *fp << dummy;
    ErrorCode = GetFloat(pid, setname, MIDSIZE, UM, SetIdx(i+1), &dumFloat);
    *fp << dumFloat << UM;
    ErrorCode = GetFloat(pid, setname, DENSITY, GCM3, SetIdx(i+1), &dumFloat);
    *fp << dumFloat << GCM3 << NewLn;
    nrec++;
  }

  t.grid = NULL;
  ATOGrid_v2 *grid = new ATOGrid_v2();  TrackLocation(grid);
  GetSetName(concat(MEDIUM,PTS),setname, ID, g->isetlist ); // my id or my sources id
  if(!rstrcmpi(t.spatialtype,GRID) && axis2num > 0)
  {
    t.grid = grid;
    grid->axis1num = axis1num;
    grid->axis2num = axis2num;
    rstrcpy(grid->axis1units, M);
    rstrcpy(grid->axis2units, M);

    grid->axis1values = new double[grid->axis1num];
    grid->axis2values = new double[grid->axis2num];

    double x;
    double y;
    bool ispolar = false;
    if(!rstrcmpi(t.coordtype, POLAR)) ispolar = true;
    pt = 0;
    for(j=0;j<grid->axis2num;j++)
    {
      for(i=0;i<grid->axis1num;i++)
      {
        pt++;
        ErrorCode = GetFloat(pid, setname, FEATUREPTS, M, SetIdx(pt,1,1), &x);
        ErrorCode = GetFloat(pid, setname, FEATUREPTS, M, SetIdx(pt,2,1), &y);
        if (ispolar)          convertToCompass(x,y);
        if(j==0)              grid->axis1values[i] = x;
      }
      grid->axis2values[j] = y;
    }
    if(ispolar) rstrcpy(grid->axis2units, "deg");

    grid->values = new double*[grid->axis1num];
    for(i=0;i<grid->axis1num;i++)
      grid->values[i] = new double[grid->axis2num];
  }
  else
  {
  // grid is null and just points are written with location names}
  }

  numcons = conlist->size();

  string rtype = TIME;
  transform(rtype.begin(),rtype.end(),rtype.begin(),(int(*)(int))tolower);

  if (TIME==ACUTE)
  {
    *fp << (char *)rtype.c_str() << t.coordtype << t.spatialtype << numcons;
    GetSetName(ACUTE, setname, ID, g->isetlist);
    ErrorCode = GetString(pid, setname, ATTRIBUTES, blank, SetIdx(1,1), dummy);
    *fp << ratoi(dummy);  //year
    ErrorCode = GetString(pid, setname, ATTRIBUTES, blank, SetIdx(1,2), dummy);
    *fp << ratoi(dummy);  //year
    ErrorCode += GetString(pid, setname, ATTRIBUTES, blank, SetIdx(1,3), dummy);
    *fp << ratoi(dummy);  //month
    ErrorCode += GetString(pid, setname, ATTRIBUTES, blank, SetIdx(1,4), dummy);
    *fp << ratoi(dummy);  //day
    ErrorCode += GetString(pid, setname, ATTRIBUTES, blank, SetIdx(1,5), dummy);
    *fp << ratoi(dummy) << NewLn;  //hour
  }
  else
    *fp << (char *)rtype.c_str() << t.coordtype << t.spatialtype << numcons << NewLn;
  nrec++;

  if(numcons>0)
    for(int nc = 0; nc<numcons; nc++)
    {
      ATOCon_v2 *p = new ATOCon_v2();  TrackLocation(p);
      nrec += p->WriteFile(pid, fp, g, modId, &t, (*conlist)[nc], ID);
      TrackDelete(p);
    }
  TrackDelete(grid);
  numcons = 0;
  return nrec;
}
//----------------------------------------------------------------------------------------------------
int ATO_v2::WriteFile(long pid, char *outpath, Glyph *g, STR_VEC_PTR_VEC *conlist, char *fqual, char *ID, char *ID2x)
{
  int ErrorCode, numpts;
  long nrec = 0, posbeg = 0;

  Log("ATO_v2::WriteFile");
  MEDIUM = AIR;
  if(rstrstr(fqual,ACUTE))     TIME=ACUTE;
  else                         TIME=CHRONIC;
  // check whether to create _ATO or append existing _ATO
  if(0 == access(AddExtension(outpath, _ATO), 0))
    outf = new ocsv(AddExtension(outpath, _ATO), '"', ',', _APPEND_);
  else
    outf = new ocsv(AddExtension(outpath, _ATO), '"', ',', _CREATE_);
  if(!outf->ok()) return 0;

  posbeg = outf->getpos();
  outf->smartQuote();
  *outf << ID << "         " << NewLn;
  outf->alwaysQuote();

  numpts = 0;
  GetSetName(concat(MEDIUM,PTS), setname, ID2x, g->isetlist);
  ErrorCode = GetVarDimSize(pid, setname, FEATURE, SetIdx(), &numpts);

  // we will always assume on dataset for 1x models
  // a dataset in 1x held multiple locations
  *outf << 1 << NewLn; nrec++;
  *outf << "_ATO generated by 1X Wrapper in Frames V2" << NewLn; nrec++;
  *outf << 1 << NewLn; nrec++;
  if(numpts>0)
  {
    ATOSet_v2 *p = new ATOSet_v2(); TrackLocation(p);
    nrec += p->WriteFile(pid, outf, g, ID, conlist, ID2x);
    TrackDelete(p);
  }

  outf->setpos(posbeg);
  *outf << ID;
  fprintf(outf->fptr,"%ld",nrec);
  delete outf;
  outf = NULL;
  return 0;
}
