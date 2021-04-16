#include "sumdata.h"

bool Result::TWIReadSet(GIDFILE *GID, int setidx)
{
  int i;
  int numChem;
  int numSciNames;
  int ErrorCode;
  int ORGidx = 0;
  int CASidx = 0;
  char *genus;
  char *species;
  char *cas;
  char *media;
  char temp[SMALLSTRING];
  parameter *tempp;

  //get index of Organism
  i = 1;
  if (!readInt (GID, "NumOrganism", &numSciNames,1))
  {
    writeError("Unable to read number of organisms");
    return false;
  }
  while(i <= numSciNames)
  {
    ErrorCode = readStr(GID, "OrganismSPECIES", &species, 1, i);
    ErrorCode += readStr(GID, "OrganismGENUS", &genus, 1, i);
    if (ErrorCode != 2) writeWarn("Unable to read organism");
    rstrcpy(temp, genus, " ", species);
    if (species != NULL) { delete species; species = NULL; }
    if (genus != NULL)   { delete genus; genus = NULL; }
    if (!rstrcmpi(OrgID, temp))
    {
      ORGidx = i; //ORGidx has index of organism
      break;
    }
    i++;
  }
  if (i > numSciNames)
  {
    writeError("Unable to read organism");
    return false;
  }

  //get index of CASID
  i = 1;
  if (!readInt (GID, "NumChemical", &numChem, 1, ORGidx))
  {
    writeError("Unable to read number of chemicals");
    return false;
  }
  while(i <= numChem)
  {
    ErrorCode = readStr(GID, "ChemicalCASID", &cas, 1, ORGidx, i);
    if (!ErrorCode) writeWarn("Unable to read chemical");
    rstrcpy(temp,cas);
    if (cas != NULL) { delete cas; cas = NULL; }
    if (!rstrcmpi(CASID, temp))
    {
      CASidx = i; //CASidx has index of chemical
      break;
    }
    i++;
  }
  if (i > numChem)
  {
    writeError("Unable to read chemical");
    return false;
  }
  //  Labels
  if (producer != NULL) delete producer;
  producer = strdup(Source);
  if (qual != NULL) delete qual;
  ErrorCode = readStr(GID, "MEDIANAME", &media, 1, ORGidx, CASidx, setidx);
  if (!ErrorCode) writeWarn("Unable to read media name");
  rstrcpy(temp, media);
  if (media != NULL) { delete media; media = NULL; }
  qual = strdup(temp);

  // get number of points from GID; store in int num
  if (!readInt(GID,"NumTimePoints",&num, 1, ORGidx, CASidx, setidx))
  {
    writeError("Unable to read number of time points");
    return false;
  }
  // redim dtime, dvalue to num
  try
  {
    if (dtime != NULL) delete[] dtime;
    dtime = new double[num];
    if (dvalue != NULL) delete[] dvalue;
    dvalue = new double[num];
  }
  catch(...)
  {
    writeError("Unable to create intake series");
    return false;
  }
  for (i = 0; i<num; i++)
  {
    dvalue[i] = 0.0;
    dtime[i] = 0.0;
  }

  // read timepoints into dtime[] and values into dvalue[]
  ErrorCode = 0;
  for (i = 1; i<= num; i++)
  {
    ErrorCode += readDouble(GID, "TIMEPOINT", &dtime[i-1], 1, ORGidx, CASidx, setidx, i);
    ErrorCode -= readDouble(GID, "DOSE", &dvalue[i-1], 1, ORGidx, CASidx, setidx, i);
  }
  if (ErrorCode)
  {
    writeError("Unable to correctly read dose time series!");
    return false;
  }

  if (xunit != NULL) delete xunit;
  if (yunit != NULL) delete yunit;

  tempp = Get_Parameter(GID, "TIMEPOINT");
  if (tempp != NULL)
    {if (tempp->punit != NULL)
      xunit = strdup(tempp->punit); }
  else
    xunit = NULL;

  tempp = Get_Parameter(GID, "DOSE");
  if (tempp != NULL)
  {
    if (tempp->punit != NULL)
      yunit = strdup(tempp->punit);
  }
  else
    yunit = NULL;

  computeResults();

  return true;
}
