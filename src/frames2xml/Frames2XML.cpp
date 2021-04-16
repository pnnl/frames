#include "Frames2XML.h"
#include "..\Common Files\ReferencesC.h"

#define C() c_str()

///for outputting module metadata and passing to outputMetadataHelper.
const int STRING = 0;
const int DOUBLE = 1;
const int BOOLEAN = 2;
const int INTEGER = 3;

char dummy[255];
char* s(int number, int radix=10)
{
    itoa(number, dummy, radix);
    return dummy;
}

char* s(double number)
{
    sprintf(dummy, "%.6g", number);
    return dummy;
}


Frames2XML::Frames2XML(ostream *stream, const char* xsl)
{
  DEBUG_FUNCTION0("Frames2XML")
    this->stream = stream;
    outputFailed = false;

    _arguments = 0;
    _arguments_length = 0;

    PID = 0;
    dimensionSize = 0;
    dimensionIndexArray = 0;

    strcpy(_simulationPath, "");
    strcpy(_simulationFile, "");
    strcpy(_simulationModule, "");
    strcpy(_simulationDataset, "");
    strcpy(_dictionary, "");
    strcpy(_dataset, "");
    strcpy(_variable, "");

    ///output header for xml file
	(*stream) << "<?xml version=\"1.0\"?>" << endl;
    (*stream) << "<?xml-stylesheet type=\"text/xsl\" href=\"";
    if(xsl)
        (*stream) << xsl << "\"?>" << endl;
    else
        (*stream) << "\"?>" << endl;

    ///begin the document with the root tag
    xml.opennl("frames");
    (*stream) << xml.output();
    xml.clear();
    outputClosing = false;
}

Frames2XML::~Frames2XML()
{
  DEBUG_FUNCTION0("~FRAMES2XML")
    ///close tag is smart enough to know if the tag has already
    ///been output
    closeTag();
    CloseIO(PID, 0);
}

void Frames2XML::closeTag()
{
  DEBUG_FUNCTION0("closeTag")
    ///output the close tag if it has not been output yet
    if(!outputClosing)
    {
        xml.clear();
        xml.closenl("frames");
        (*stream) << xml.output();
    }
    outputClosing = true;
}

int Frames2XML::getPID()
{
  DEBUG_FUNCTION0("getPID")
    return PID;
}

bool Frames2XML::hasErrors()
{
//  DEBUG_FUNCTION0("hasErrors")
    if(PID < 0)
    {
	    outputMessage("error", "Invalid PID");
	    return true;
    }
    if(!strcmp(_simulationDataset, ""))
    {
	    outputMessage("error", "Simulation dataset has not been set or is invalid");
	    return true;
    }

    return false;
}

bool Frames2XML::outputHeader()
{
  DEBUG_FUNCTION0("outputHeader")
    xml.clear();
    xml.opennl("date");
    time_t timet;
    time(&timet);
    xml.putValuenl(ctime(&timet));
    xml.closenl("date");

    (*stream) << xml.output();
    return true;
}

bool Frames2XML::outputArguments()
{
  DEBUG_FUNCTION0("outputArguments")
    xml.clear();

    ///open up arguments and output count
    xml.opennl("arguments");
    xml.open("count");
    xml.putValue(s(_arguments_length));
    xml.closenl("count");

    ///output each argument on it's own line
    for(int i = 0; i < _arguments_length; i++)
    {
	    xml.open("argument");
	    xml.putValue(_arguments[i]);
	    xml.closenl("argument");
    }

    xml.closenl("arguments");

    (*stream) <<  xml.output();

    char parameters[] = "Read the documentation for information on parameters.";
    outputMessage("message", parameters);

    return true;
}

bool Frames2XML::outputMessage(char *tag, char *message)
{
  DEBUG_FUNCTION2("outputMessage", tag, message)
    xml.clear();

    xml.opennl(tag);
    xml.putValuenl(message);
    xml.closenl(tag);

    (*stream) <<  xml.output();

    return true;
}

bool Frames2XML::openSystem(char **arguments, int count)
{
  DEBUG_FUNCTION2("openSystem", "arguments", count)
//    cout << "openSystem" << endl;

    _arguments = arguments;
    _arguments_length = count;
    if(count < 3)
    {
	    outputMessage("error", "Invalid arguments and/or could not open FRAMES");
	    outputArguments();

	    return false;
    }

    ///the last three parameters, if passed right from frames, will always
    ///be these parameters in this order
    strcpy(_simulationPath, arguments[count-3]);
    strcpy(_simulationFile, arguments[count-2]);
    strcpy(_simulationModule, arguments[count-1]);

    ///attempt to open up the frames system
    /*
     * this is bad.  the simulation is not always in the same path
     * as the startup.ini, but until the tools menu is fixed in frames
     * they will have to have the same path for this to work.
     */
//    PID = OpenINI(_simulationPath);
//    cout << OpenSim(PID, _simulationPath, _simulationFile) << endl;
    PID = OpenIO(_simulationPath, _simulationFile, _simulationModule);


    if(PID < 1)
    {
	    outputMessage("error", "Could not open FRAMES IO (Invalid PID returned)");
        outputMessage("error", s(PID));
	    return false;
    }

    char path[MAX];
    GetSim(PID, path, _simulationDataset);

    return true;
}

bool Frames2XML::outputSimulation()
{
  DEBUG_FUNCTION0("outputSimulation")
//    cout << "outputSimulation" << endl;
    if(hasErrors())
	    return false;

    int size = getNumberModules();

    ///loop through each module and output it
    for(int i = 0; i < size; i++)
	    outputModule(i+1);

    return true;
}

int Frames2XML::getNumberModules()
{
  DEBUG_FUNCTION0("getNumberModules")
    if(hasErrors())
	    return -1;

    int size = -1;

    ///get the size (count) of all modules in the simulation
    int indices[] = {1};
    GetVarDimSize(PID, _simulationDataset, "modid", indices, &size);

      return size;
}

void Frames2XML::outputModuleHeader(int index)
{
  DEBUG_FUNCTION1("outputModuleHeader", index)
    xml.clear();
    atts.clear();

    char data[MAX];
    char moduleId[MAX];
    char moduleName[MAX];

    ///the first two modules read need to have different attribute names
    ///than the names stored in the simulation
    ///read the module ID
    ReadString1(PID, _simulationDataset, "modid", "", index, data);
    atts.add("name", data);
    ///save the module id for outputting the uiio and schemes
    strcpy(moduleId, data);

    ///read the module type
    ReadString1(PID, _simulationDataset, "modname", "", index, data);
    atts.add("type", data);

    ///save the module type (actually the name) for outputting the metadata
    strcpy(moduleName, data);

    ///the other attributes can be wrapped in a loop
    string names[] = {"label", "domain", "class", "group", "subgroup", "scheme"};
    int names_length = 6;

    ///loop through the attribute names and grab their values from the
    ///simulation
    char temp[MAX];
    for(int i = 0; i < names_length; i++)
    {
        strcpy(temp, "mod");
        strcat(temp, names[i].c_str());
	    ///module label
	    ReadString1(PID, _simulationDataset, temp, "", index, data);
        strcpy(temp, names[i].c_str());
	    if(strcmp(data, ""))
	        atts.add(temp, data);
    }

    ///output the module open tag and all of it's attributes
    xml.opennl("module", &atts);
    (*stream) <<  xml.output();
}

void Frames2XML::outputModuleFooter()
{
  DEBUG_FUNCTION0("outputModuleFooter")
    xml.clear();
    xml.closenl("module");
    (*stream) <<  xml.output();
}

bool Frames2XML::outputModule(int index)
{
  DEBUG_FUNCTION1("outputModule", index)
//    cout << "openModule " << index << endl;
    if(hasErrors())
	    return false;

    char moduleName[MAX];
    char moduleId[MAX];

    ///read the module name
    ReadString1(PID, _simulationDataset, "modname", "", index, moduleName);
    ///read the module id
    ReadString1(PID, _simulationDataset, "modid", "", index, moduleId);

    outputModuleHeader(index);

    outputModuleUIIO(moduleId);
    outputModuleScheme(moduleId);
    outputModuleMetadata(moduleName);

    outputModuleFooter();

    return true;
}

bool Frames2XML::outputModule(char *moduleId)
{
  DEBUG_FUNCTION1("outputModule", moduleId)
    if(hasErrors())
	    return false;

    int i = getIndexFromId(moduleId);
    outputModule(i);

    return true;
}

void Frames2XML::getNameFromId(char *moduleId, char *moduleName)
{
  DEBUG_FUNCTION2("getNameFromId", moduleId, moduleName)
    int size = getNumberModules();
    char mod[MAX];
    for(int i = 1; i <= size; i++)
    {
	    ///read module name at index i from simulation module list.
	    ReadString1(PID, _simulationDataset, "modid", "", i, mod);
	    if(!stricmp(moduleId,mod))
	    {
        ReadString1(PID, _simulationDataset, "modname", "", i, moduleName);
	      return;
	    }
    }

    strcpy(moduleName, "");
}

int Frames2XML::getIndexFromId(char *moduleId)
{
  DEBUG_FUNCTION1("getIndexFromId", moduleId)
    int size = getNumberModules();
    char mod[MAX];
    for(int i = 1; i <= size; i++)
    {
	    ///read module name at index i from simulation module list.
	    ReadString1(PID, _simulationDataset, "modid", "", i, mod);
	    if(!stricmp(moduleId,mod))
	        return i;
    }

    return -1;
}

bool Frames2XML::outputModuleUIIO(char *moduleId)
{
  DEBUG_FUNCTION1("outputModuleUIIO", moduleId)
//    cout << "outputModuleUIIO " << moduleId << endl;
    if(hasErrors())
	    return false;

    char dictionary[MAX];
    char dataset[MAX];

    GetIconUIDic(PID, moduleId, dictionary);
    GetIconUISet(PID, moduleId, dataset);

    ///if the module has a uiio dic, output it and it's dataset
    if(strcmp(dictionary,""))
    {
	    xml.clear();
	    xml.opennl("uiio");
	    (*stream) <<  xml.output();

	    outputDictionary(dictionary, dataset);
 
	    xml.clear();
	    xml.closenl("uiio");
	    (*stream) <<  xml.output();
    }

    return true;
}

bool Frames2XML::outputModuleScheme(char *moduleId)
{
  DEBUG_FUNCTION1("outputModuleScheme", moduleId)
//    cout << "outputModuleScheme" << moduleId << endl;
    if(hasErrors())
	    return false;

    xml.clear();
    xml.opennl("scheme");
    (*stream) <<  xml.output();

    outputModuleIO(moduleId,true);
    outputModuleIO(moduleId,false);

    xml.clear();
    xml.closenl("scheme");
    (*stream) <<  xml.output();

    return true;
}

bool Frames2XML::outputModuleIO(char *moduleId, bool inputs)
{
  DEBUG_FUNCTION2("outputModuleIO", moduleId, inputs)
    if(hasErrors())
	    return false;

    char list[MAX];
    char dataset[MAX];
    char dictionary[MAX];
    int size = 0;

    ///create the scheme tag header
    xml.clear();
    if(inputs)
    {
	    xml.opennl("inputs");
	    GetIModList(PID, moduleId, ",", list);
    }
    else
    {
	    xml.opennl("outputs");
	    GetOModList(PID, moduleId, ",", list);
    }

    ///if there are not inputs/outputs, exit
    if(!strcmp(list, ""))
    {
	    ///put the tab width back by closing the outputs
	    xml.closenl("outputs");
	    ///now clear cause we don't want to output
	    xml.clear();

	    return true;
    }

    ///write the xml header
    (*stream) <<  xml.output();

    char *c = list;
    char *module = list;
    ///loop through all the input/output modules
    while(c)
    {
        if(module > list + MAX)
            break;
        
        ///turn the list temporarily into one string by replacing the
        /// comma with a null
        c = strchr(module, ',');
        if(c)
            strcpy(c, "\0");

	    if(inputs)
	        NumIModSet(PID, moduleId, module, &size);
	    else
	        NumOModSet(PID, moduleId, module, &size);

	    ///loop through each dataset, outputting it
	    for(int i = 1; i <= size; i++)
	    {
	        if(inputs)
	        {
		        GetIDataSet(PID, moduleId, module, i, dataset);
		        GetIDictionary(PID, moduleId, module, i, dictionary);
	        }
	        else
	        {
		        GetODataSet(PID, moduleId, module, i, dataset);
		        GetODictionary(PID, moduleId, module, i, dictionary);
	        }

	        outputDictionary(dictionary, dataset);
	    }

        ///move pointer for finding next string
        if(c)
            module = c+1;

    }    

    ///output tag tail for inputs/outputs
    xml.clear();
    if(inputs)
	    xml.closenl("inputs");
    else
	    xml.closenl("outputs");

    (*stream) <<  xml.output();

    return true;
}

bool Frames2XML::outputModuleMetadata(char *moduleName)
{
  DEBUG_FUNCTION1("ouptutModuleMetatdata", moduleName)
    if(hasErrors())
	    return false;

    xml.clear();
    xml.opennl("metadata");

    ///holds list of all the variables in the metadata of
    ///a module
    string strings[] = {
          "class",
			    "icon",
			    "login",
			    "modelcmdline",
			    "modelexe",
			    "modelurl",
			    "name",
			    "operatingsystem",
			    "pocaddress1",
			    "pocaddress2",
			    "poccity",
			    "poccompany",
			    "poccontact",
			    "poccounrty",
			    "pocemail",
			    "pocfax",
			    "pocperson",
			    "pocphone",
			    "pocstate",
			    "pocurl",
			    "poczip",
			    "processor",
			    "systemversion",
			    "uicmdline",
			    "uiexe",
			    "version",
			    };

    int strings_length = 26;

    string integers[] = {"diskspace",
			     "ram",
			     "systemupdate",
			     "tool"};

    int integers_length = 4;

    int indices[] = {1};
    int count = 0;

    GetModInteger(PID, moduleName, "DescriptionCount", "", indices, &count);
    if(count > 0)
    {
	    ///wrap each description in a parent "descriptions" tag
	    xml.opennl("descriptions");

	    ///output descriptions
	    outputModuleMetadataHelper(moduleName, "description", STRING, count);
            xml.closenl("descriptions");
    }

    ///find the reference count
    GetModInteger(PID, moduleName, "ReferenceCount", "", indices, &count);
    ///output references
    if(count > 0)
    {
	    ///wrap each description in a parent "descriptions" tag
	    xml.opennl("references");

	    ///output descriptions
	    outputModuleMetadataHelper(moduleName, "reference", STRING, count);
        xml.closenl("references");
    }

    ///output strings
    char str[MAX];
    for(int i = 0; i < strings_length; i++)
    {
        strcpy(str, strings[i].c_str());
	    outputModuleMetadataHelper(moduleName, str, STRING,1);
    }

    ///output integers
    for(i = 0; i < integers_length; i++)
    {
        strcpy(str, integers[i].c_str());
	    outputModuleMetadataHelper(moduleName, str, INTEGER,1);
    }


    xml.closenl("metadata");
    (*stream) <<  xml.output();

    return true;
}

bool Frames2XML::outputModuleMetadataHelper(char *moduleName, char *variableName, int type, int count)
{
  DEBUG_FUNCTION3("outputModuleMetadataHelper", moduleName, variableName, count)
    if(hasErrors())
	    return false;

    int index[] = {1};
    for(int i = 0; i < count; i++)
    {
	    char metadata[MAX];
	    ///for each type, retrieve, store in metadata, and output
	    if(type == STRING)
	    {
	        GetModString(PID, moduleName, variableName, "", index, metadata);
	    }
	    else if(type == DOUBLE)
	    {
            double data = 0;
	        GetModDouble(PID, moduleName, variableName, "", index, &data);
	        strcpy(metadata, s(data));
	    }
	    else if(type == BOOLEAN || type == INTEGER)
	    {
	        int data = 0;
            GetModInteger(PID, moduleName, variableName, "", index, &data);
	        strcpy(metadata, s(data));
	    }
	    
	    ///output string if it contains data
	    if(strcmp(metadata, ""))
	    {
	        xml.open(variableName);
	        xml.putValue(metadata);
	        xml.closenl(variableName);
	    }
	    index[0]++;
    }

    return true;
}

bool Frames2XML::outputDictionary(char *dictionary, char *dataset)
{
  DEBUG_FUNCTION2("outputDictionary", dictionary, dataset)
    if(hasErrors() || dictionary == "")
	    return false;

    xml.clear();
    atts.clear();

    ///add dictionary and dataset names as attributes
    atts.add("name", dictionary);
    atts.add("dataset", dataset);

    ///create the open dictionary tag with attributes
    xml.opennl("dictionary", &atts);

    ///write the open tag to the file
    (*stream) <<  xml.output();

    ///output variables and values
    int number = 0;
    char variableName[MAX];
    GetVarCount(PID, dictionary, &number);

    for(int i = 1; i <= number; i++)
    {
	    GetVarName(PID, dictionary, i, variableName);
	    outputVariable(dictionary, dataset, variableName);
    }

    xml.clear();
    atts.clear();
    ///write close dictionary tag to output
    xml.closenl("dictionary");
    (*stream) <<  xml.output();

    return true;
}

bool Frames2XML::outputVariable(char *dictionary, char *dataset, char *variable)
{
  DEBUG_FUNCTION3("outputVariable", dictionary, dataset, variable)
    if(hasErrors())
	    return false;

    xml.clear();
    atts.clear();

    ///add dictionary and dataset names as attributes
    atts.add("name", variable);

    ///create the open dictionary tag with attributes
    xml.opennl("variable", &atts);

    ///output variable attributes
    int intValue = 0;
    char strValue[MAX];
    double doubleValue = 0.0;

    ///primary key
    GetVarPrimaryKey(PID, dictionary, variable, &intValue);
    xml.open("primarykey");
    xml.putValue (s(intValue));
    xml.closenl("primarykey");

    ///description
    GetVarDescription(PID, dictionary, variable, strValue);
    if(strcmp(strValue, ""))
    {
	    xml.open("description");
	    xml.putValue(strValue);
	    xml.closenl("description");
    }

    ///datatype
    GetVarType(PID, dictionary, variable, strValue);
    xml.open("type");
    xml.putValue(strValue);
    xml.closenl("type");

    ///dimension, scalar, and indexes
    char indices[MAX];
    VarIndexList(PID, dictionary, variable, ",", indices);
    _indexCount = 0;
    if(strcmp(indices, ""))
    {
	    xml.opennl("indices");

        char *c = indices;
        char *index = indices;
	    while(c)
	    {
            c = strchr(index, ',');
            if(c)
                strcpy(c, "\0");

	        xml.open("index");
	        xml.putValue(index);
	        xml.closenl("index");
            strcpy(_indices[_indexCount], index);

            if(c)
                index = c+1;

            ///count indicies
            _indexCount++;
	    }
	    xml.closenl("indices");
    }

    GetVarScalar(PID, dictionary, variable, &intValue);
    xml.open("scalar");
    xml.putValue(s(intValue));
    xml.closenl("scalar");
    _vector = !intValue;

    ///var max and min
    GetVarMax(PID, dictionary, variable, &doubleValue);
    xml.open("maximum");
    xml.putValue(s(doubleValue));
    xml.closenl("maximum");
    GetVarMin(PID, dictionary, variable, &doubleValue);
    xml.open("minimum");
    xml.putValue(s(doubleValue));
    xml.closenl("minimum");

    ///measure and unit
    GetVarMeasure(PID, dictionary, variable, strValue);
    if(strcmp(strValue, ""))
    {
	    xml.open("measure");
	    xml.putValue(strValue);
	    xml.closenl("measure");
    }
    GetVarUnit(PID, dictionary, variable, strValue);
    if(strcmp(strValue, ""))
    {
	    xml.open("unit");
	    xml.putValue(strValue);
	    xml.closenl("unit");
    }

    ///stochastic
    GetVarStochastic(PID, dictionary, variable, &intValue);
    xml.open("stochastic");
    xml.putValue(s(intValue));
    xml.closenl("stochastic");

    ///preposition
    GetVarPreposition(PID, dictionary, variable, strValue);
    if(strcmp(strValue, ""))
    {
	    xml.open("preposition");
	    xml.putValue(strValue);
	    xml.closenl("preposition");
    }

    (*stream) <<  xml.output();
    if(dataset != NULL)
	    outputValues(dictionary, dataset, variable);

    xml.clear();

    ///write close dictionary tag to output
    xml.closenl("variable");
    (*stream) <<  xml.output();

    return true;
}

bool Frames2XML::outputValues(char *dictionary, char *dataset, char *variable)
{
  DEBUG_FUNCTION3("outputValues", dictionary, dataset, variable)
    if(hasErrors())
	    return false;

    xml.clear();
    xml.opennl("values");
    (*stream) <<  xml.output();

    outputReferencesForVariable(dataset, variable);

    int size = 0;
    ///add one for the current variable (just assume it's vector,
    ///recurseDimentsion will take care of it)
    dimensionSize = _indexCount + 1;

    ///initialize the dimension index array to all 0's
    dimensionIndexArray = new int[8];//dimensionSize];
    for(int i = 0; i < 8; i++)
	    dimensionIndexArray[i] = 0;

    strcpy(_dictionary, dictionary);
    strcpy(_dataset, dataset);
    strcpy(_variable, variable);

    ///start off the recursive output of dimensions at the first dimension
    recurseDimension(0);

    xml.clear();
    xml.closenl("values");
    (*stream) <<  xml.output();

    return true;
}
/*
void convertToString(int _1, int _2, int _3, int _4, int _5, int _6, char *_string)
{
    char buffer[25];
    strcpy(_string, "");
    strcat(_string, itoa(_1, buffer, 10));
    strcat(_string, ",");
    strcat(_string, itoa(_2, buffer, 10));
    strcat(_string, ",");
    strcat(_string, itoa(_3, buffer, 10));
    strcat(_string, ",");
    strcat(_string, itoa(_4, buffer, 10));
    strcat(_string, ",");
    strcat(_string, itoa(_5, buffer, 10));
    strcat(_string, ",");
    strcat(_string, itoa(_6, buffer, 10));
}
*/
void Frames2XML::recurseDimension(int dimensionIndex)
{
  DEBUG_FUNCTION1("recurseDimension", dimensionIndex)
    int size = 0;
    char value[MAX];

    if((dimensionIndex >= dimensionSize - 1) && !_vector)
        size = 1;
    else
        GetVarDimSize(PID, _dataset, _variable, dimensionIndexArray, &size);

	for(int i = 1; i <= size; i++)
	{
        if(dimensionIndex < dimensionSize - 1)
        {        
            dimensionIndexArray[dimensionIndex] = i;
            xml.clear();
            atts.clear();
            int val = i;//= dimensionIndex + 1;
            //atts.add("name", s(val));
            //val = i;//dimensionIndexArray[dimensionIndex]+1;
            getStringAtVariableIndex(dimensionIndex, i, value);
            atts.add("value", value);
//            atts.add("value", s(val));
            xml.opennl("index", &atts);
            (*stream) <<  xml.output();

	        recurseDimension(dimensionIndex+1);
            xml.clear();
            xml.closenl("index");
            (*stream) << xml.output();
        }
        else
        {

	        dimensionIndexArray[dimensionIndex] = i;

            xml.clear();
            xml.open("v");

            char value[MAX];
            strcpy(value, "");
            getStringAtDimensionIndex(_dictionary, _dataset, _variable,
                                       dimensionIndexArray, value);
            xml.putValue(value);

	        xml.closenl("v");

            (*stream) <<  xml.output();

            outputReferencesForVariable(_dataset, _variable, dimensionIndexArray[0],
                                                             dimensionIndexArray[1],
                                                             dimensionIndexArray[2],
                                                             dimensionIndexArray[3],
                                                             dimensionIndexArray[4],
                                                             dimensionIndexArray[5]);

        }

    }
    dimensionIndexArray[dimensionIndex] = 0;

}

void Frames2XML::outputReferencesForVariable(char * dataset, char* variable,
                                             int _1, int _2, int _3, int _4,
                                             int _5, int _6)
{
  DEBUG_FUNCTION5("outputReferencesForVariable", dataset, variable, _1, _2, _3)
    xml.clear();

    char value[MAX];
    int count = 0;
    GetVarRefCount(PID, dataset, variable, _1, _2, _3, _4, _5, _6, &count);
/*
    convertToString(dimensionIndexArray[0],
                     dimensionIndexArray[1],
                     dimensionIndexArray[2],
                     dimensionIndexArray[3],
                     dimensionIndexArray[4],
                     dimensionIndexArray[5],
                     value);
    xml.opennl("message");
    xml.putValuenl(_dataset);
    xml.putValuenl(_variable);
    xml.putValue(value);
    xml.closenl("message");
*/
    for(int i = 1; i <= count; i++)
    {
        xml.open("r");

        GetVarRef(PID, dataset, variable, _1, _2, _3, _4, _5, _6, i, value);
        xml.putValue(value);

	    xml.closenl("r");
    }
    (*stream) << xml.output();
}

void Frames2XML::getStringAtVariableIndex(int dimIndex, int index, char* value)
{
  DEBUG_FUNCTION3("getStringAtVariableIndex", dimIndex, index, value)
    char dicvar[MAX];
    char dic[MAX];
    char var[MAX];

    strcpy(dicvar, _indices[dimIndex]);
    char *c = dicvar;
    if(strcmp(c, ""))
    {
        ///find the separator (.)
        c = strchr(dicvar, '.');
        if(c)
            strcpy(c, "\0");

        ///copy the dictionary name into dic
        strcpy(dic, dicvar);

        ///and the rest, variable name into var
        if(c)
        {
            c++;
            strcpy(var, c);
        }

    }
/*
    if(!stricmp(_variable, "AppDicVars"))
    {
        int bla[7] = {1,1,0,0,0,0,0};

        char value[MAX];
		GetString(PID, _dataset, var, "", bla, value);
        cout << "value: " << value << endl;
        cout << "_var: " << var << endl;
    }

    int indices[7] = {1,0,0,0,0,0,0};
    indices[0] = index;
*/
    strcpy(value, "");
    if(!stricmp(dic, _dictionary))
        getStringAtDimensionIndex(dic, _dataset, var, dimensionIndexArray, value);
    else
    {
        ///get dataset
        ///call func
        strcpy(value, "");
    }
}

void Frames2XML::getStringAtDimensionIndex(char* dic, char* dataset, char* var,
                                           int* indices, char* stringValue)
{
  DEBUG_FUNCTION4("getStringAtDimensionIndex", dic, dataset, var, "indices")
	///output values
    char type[MAX];
    char unit[MAX];

    GetVarType(PID, dic, var, type);
	GetVarUnit(PID, dic, var, unit);

    if(!stricmp(type, "STRING"))
	{
        char value[MAX];
		GetString(PID, dataset, var, unit, indices, value);
        strcpy(stringValue, value);
	}
	else if(!stricmp(type, "FLOAT"))
	{
        double value = 0.0;
		GetFloat(PID, dataset, var, unit, indices, &value);
        strcpy(stringValue, s(value));
	}
	else if(!stricmp(type, "LOGICAL"))
	{
        int value = 0;
		GetInteger(PID, dataset, var, unit, indices, &value);
        strcpy(stringValue, s(value));
	}
	else if(!stricmp(type, "INTEGER"))
	{
        int value = 0;
		GetLogical(PID, dataset, var, unit, indices, &value);
        strcpy(stringValue, s(value));
	}
    else
        strcpy(stringValue, "");
}

void Frames2XML::outputReferences()
{
  DEBUG_FUNCTION0("outputReferences")
    int count = 0;

    GetRefCount(PID, &count);

//    cout << "count: " << count << endl;
    ///are there references?
    if(count > 0)
    {
        xml.clear();

        xml.opennl("references");

        char refid[MAX];
        char ref[MAX];

        ///get each ref and output
        for(int i = 1; i <= count; i++)
        {
          strcpy(refid, "");
          strcpy(ref, "");
            GetRefByIndex(PID, i, refid, ref);

            atts.clear();
            atts.add("name", refid);

            xml.opennl("reference", &atts);
            xml.putValuenl(ref);
            xml.closenl("reference");
        }
        
        xml.closenl("references");
        (*stream) <<  xml.output();
    }
}