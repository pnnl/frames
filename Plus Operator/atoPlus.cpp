#include "atoPlus.h"
#include "plusop.h"

bool doAtoPlus(GIDfile &gid)
{
    ATO firstato("cartesian");
    int i = 1;

    char readFile[MAXPATH];
    rstrcpy(readFile, readPath);

    char writeFile[MAXPATH];
    rstrcpy(writeFile, writePath);
    strcat(writeFile, ".ato");

    model mod = getInput(gid, modelName, i);
    i++;
    ///read in the first ato, use it to store the results
    if(rstrcmpi(mod.name, "") != 0)
      firstato.Read(readFile, mod.name, mod.x, mod.y, mod.z);
    else
      return false;

    mod = getInput(gid, modelName, i);
    ///add all other ato's to this one
    while(rstrcmpi(mod.name, "") != 0)
    {
      ATO ato("cartesian");
      ato.Read(readFile, mod.name, mod.x, mod.y, mod.z);
      firstato.add(&ato);
      i++;
      mod = getInput(gid, modelName, i);
    }

    firstato.ChangeName(0,modelName);
    firstato.Write(writeFile, false);

    return true;
}
