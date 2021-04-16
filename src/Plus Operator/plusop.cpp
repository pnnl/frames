#include "plusop.h"

char readPath[MAXPATH];
char writePath[MAXPATH];
char modelName[SMALLSTRING];
char modelType[SMALLSTRING];
int siteIndex = 0;
int modelIndex = 0;

char temp_getInput[MAXSTRING];

void outputMessage(const char *message)
{
    OUT << "MESSAGE: " << message << endl;
}

void outputArguments(int argc, char ** argv)
{
    outputMessage("Here are the program arguments.");
    for(int i = 0; i < argc; i++)
        OUT << (i+1) << ". " << argv[i] << endl;
    OUT << endl;
}

bool parseArguments(int argc, char **argv)
{
  rstrcpy(writePath, "");
  ///1 for program path, 1 for switch, 5 for frames variables
  if(argc < 7)
      return false;

  rstrcpy(modelName,argv[argc-1]);
  modelIndex = atoi(argv[argc-2]);
  siteIndex = atoi(argv[argc-3]);
  rstrcpy(writePath, argv[argc-4]);
  rstrcpy(readPath, argv[argc-5]);
  rstrcpy(modelType, argv[argc-6]);

  rstrcpy(ERRName,writePath,".err");
  rstrcpy(WRNName,writePath,".wrn");

  return true;
}

/**
  * get a property of a model/module.  user modelName to specify which model
  * you want the property from, propertyName to specify the property you want
  * and the index of that property (in the case that the property is an array).  index
  * can be left alone or specified with -1 to assume that the property is non-array
  * (if it is array, it just returns the first value).  index should not be zero based.
  * the return value is the property value, obviously in the form of a string.  a value
  * of "" is returned if an error occurs, or "" just happens to be the value of the
  * property.
  * do not delete the returned string, it will be taken care of upon program
  * termination. the inputType is a filter to only return a property value if the model
  * is of type inputType.
  */
char *getModelProperty(GIDfile &gid, char* modelName, char* propertyName, int index = -1)
{
    rstrcpy(temp_getInput, "");

    ///locate the mod id
    Section *csm = gid.GetSection("CSM");
    Parameter *param = csm->GetParameter("ModId");

    ///return empty when ModId section can't be found
    if(!param)
        return temp_getInput;

    Entry *mod = param->FindEntry(modelName);
    param = csm->GetParameter(propertyName);

    ///return empty when model can't be found
    if(!param || !mod)
        return temp_getInput;

    Entry *source;

    int tempIndex = 0;
    if(index >= 1)
    {
      ///pretend there is a source model index i, and set it
      tempIndex = mod->idx[2];
      mod->idx[2] = index;
    }

    ///get entry for x,y,index,0,0,0 (should be source model,
    ///if source returns null, then that source model at
    ///index does not exist)
    source = param->GetEntry(mod->idx);

    if(index >= 1)
      ///return entry back to previous value
      mod->idx[2] = tempIndex;

    ///exit if a source did not return, or the input type doesn't match
    ///(if specified)
    if(source)
      rstrcpy(temp_getInput, source->value);

    return temp_getInput;

}

model getInput(GIDfile &gid, const char *modelName, int index, const char *inputType)
{
    model inputmod;
    if(!inputType || stricmp(getModelProperty(gid, (char*)modelName, "ModSrcType", index), inputType) == SAME)
    {
      rstrcpy(inputmod.name, getModelProperty(gid, (char*)modelName, "ModSrcId", index));
      inputmod.x = atoi(getModelProperty(gid, inputmod.name, "ModLocX", 0))*1000;
      inputmod.y = atoi(getModelProperty(gid, inputmod.name, "ModLocY", 0))*1000;
      inputmod.z = atoi(getModelProperty(gid, inputmod.name, "ModLocZ", 0))*1000;
    }
    return inputmod;
}

void pause()
{
  char c;
  cin >> c;
}
