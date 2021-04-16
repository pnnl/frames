#pragma hdrstop
#include <condefs.h>
#include "wffPlus.h"
#include "wcfplus.h"
#include "scfplus.h"
#include "atoplus.h"
#include "plusop.h"

//---------------------------------------------------------------------------
USEUNIT("..\Common Files\robust.cpp");
USEUNIT("..\Common Files\csv.cpp");
USEUNIT("..\Common Files\fcsv.cpp");
USEUNIT("..\Common Files\series.cpp");
USEUNIT("..\Frames DLL\wffClass.cpp");
USEUNIT("..\Frames DLL\wcfClass.cpp");
USEUNIT("..\Frames DLL\atoClass.cpp");
USEUNIT("..\Frames DLL\scfClass.cpp");
USEUNIT("gid.cpp");
USEUNIT("wffPlus.cpp");
USEUNIT("wcfPlus.cpp");
USEUNIT("atoPlus.cpp");
USEUNIT("scfPlus.cpp");
USEUNIT("plusop.cpp");
USERES("plus.res");
//---------------------------------------------------------------------------
#pragma argsused


int main (int argc, char **argv)
{
  bool error = false;

  if(!parseArguments(argc, argv))
  {
    writeError("Could not parse arguments");
    error = true;
  }
  else
  {    ///assemble gid file path name
    char gidFile[MAXPATH];
    rstrcpy(gidFile, readPath);
    strcat(gidFile, ".gid");

    ///create gid object, open up the gid file, and read
    GIDfile gid(gidFile);
    gid.Read();

    if(stricmp(modelType, "/ato") == SAME)
        error=!doAtoPlus(gid);
    else if(rstrcmpi(modelType, "/wcf") == SAME)
        error=!doWcfPlus(gid);
    else if(rstrcmpi(modelType, "/scf") == SAME)
        error=!doScfPlus(gid);
    else if(rstrcmpi(modelType, "/wff") == SAME)
        error=!doWffPlus(gid);

    else if(rstrcmpi(modelType, "/wffs") == SAME)
        error=!doWffSync(gid, "Surface Water");
    else if(rstrcmpi(modelType, "/wffa") == SAME)
        error=!doWffSync(gid, "Aquifer");
    else if(rstrcmpi(modelType, "/wffv") == SAME)
        error=!doWffSync(gid, "Vadose");

    else if(rstrcmpi(modelType, "/sawff") == SAME)
        error=!doWffSA(gid);
    else if(rstrcmpi(modelType, "/sawffp") == SAME)
        error=!doWffSAp(gid);

    else if(rstrcmpi(modelType, "/wcfs") == SAME)
        error=!doWcfSync(gid, "Surface Water");
    else if(rstrcmpi(modelType, "/wcfa") == SAME)
        error=!doWcfSync(gid, "Aquifer");
    else if(rstrcmpi(modelType, "/wcfst") == SAME)
        error=!doWcfSync(gid, "Surface Water-Total");
    else if(rstrcmpi(modelType, "/wcfat") == SAME)
        error=!doWcfSync(gid, "Aquifer-Total");

    else if(rstrcmpi(modelType, "/scfd") == SAME)
        error=!doScfSync(gid, "Sediment");
    else if(rstrcmpi(modelType, "/scfl") == SAME)
        error=!doScfSync(gid, "Soil");
    else if(rstrcmpi(modelType, "/scfdd") == SAME)
        error=!doScfSync(gid, "Sediment-Dissolved");
    else if(rstrcmpi(modelType, "/scfld") == SAME)
        error=!doScfSync(gid, "Soil-Dissolved");
    else
    {
      writeError("The correct input type could not be found");
      error = true;
    }
  }
  if (!error) remove(ERRName);
  return 0;
}

