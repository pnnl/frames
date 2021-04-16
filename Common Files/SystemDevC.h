
#ifndef SystemDev_H
#define SystemDev_H
#ifndef BUILD_DLL
  #define FRAMES_API __declspec(dllimport) __stdcall
#else
  #define FRAMES_API __declspec(dllexport) __stdcall
#endif
// API Name: FRAMES System
  #ifdef __cplusplus
    extern "C" {
  #else
    #define bool int
  #endif

   /*
   Date:       2001
   Programmer: Mitch Pelton, Karl Castleton, and Bonnie Hoopes
   Company:    Pacific Northwest National Laboratories
               Operated by Battelle Memorial Institute
               For Department of Energy
               
   This collection of functions is intented for FRAMES 2.0 system developers.
   Most will not do anything unless the caller has a Process Id (PID) that was
   received by calling OpenINI. Model/module developers should restrict their 
   calls to the DataSet API and the Conversion API. All functions will return
   an error code unless otherwise stated. An error code of zero is to be 
   considered a success.
   
   The domains and their pallette are a way to arrange the different types of
   modules (functions, procedure) available to the system. 
   
   All system I/O is saved memory and is only saved to disk when a save API
   call is made. 
   
   The following terminology is applicable to this documentation

       Fully Qualified Path     example: "C:\FramesV2\Simulations\GWtest\test.sim"
       Full Path                example: "\FramesV2\Simulations\GWtest\test.sim"
       Path                     example: "\FramesV2\Simulations\Gwtest"
       File                     example: "test.sim"

   */
     
   /*-------------------------------------------------------------------------*/
   /* Documentation for: OpenINI
    Open the INI and returns a PID. If the Startup.ini file does not exist path is the location where it will be created.
    The OpenINI function call is the first call made; if OpenINI is never called, 
    all API calls are ignored. All changes to INI are persistent. 
    The OpenINI function call is made with the file path as input (from command line), opens 
    the INI, and returns the process id.
  
   */

       int FRAMES_API _OpenINI (
       char *  StartupFile);
       #ifndef BUILD_DLL
         #define OpenINI _OpenINI
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: CloseINI
    Closes the entire frames session. If cancel is 0 all Startup.ini changes will be written. If cancel is 1 then no changes are written.
    The CloseINI function call is made with a process id as input and closes the entire FRAMES session. 
  
   */

       int FRAMES_API _CloseINI (
       int PID, int cancel);
       #ifndef BUILD_DLL
         #define CloseINI _CloseINI
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddNewDictionary
    Create and add dictionary in registry,  dictionary name will be set to the parameter dictionary.
    The dictionary file is expected to be empty. The AddNewDictionary function call is made with a 
    process id and path string as input, and an empty dic string to output the dictionary name. This function will 
    create and add a dictionary. 
  
   */

       int FRAMES_API _AddNewDictionary (
       int PID, char *  dictionaryFile, char *  dictionaryName);
       #ifndef BUILD_DLL
         #define AddNewDictionary _AddNewDictionary
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddOpenDictionary
    Open and add dictionary in registry, dictionary returns the dictionary name. The dictionary file is expected 
    to be complete for this function to reach success (return a 0). The AddOpenDictionary function call is made with a process id and path string as input, 
    and an empty dic string to output the dictionary name. This function will add a dictionary to the registry. 
  
   */

       int FRAMES_API _AddOpenDictionary (
       int PID, char *  dictionaryFile, char *  dictionary);
       #ifndef BUILD_DLL
         #define AddOpenDictionary _AddOpenDictionary
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DelDictionary
    Delete a dictionary from FRAMES 2.0 Startup.ini. The DelDictionary function call is made with a process id and a dictionary name string as input to delete a 
    dictionary from the registry.
  
   */

       int FRAMES_API _DelDictionary (
       int PID, char *  dictionaryName);
       #ifndef BUILD_DLL
         #define DelDictionary _DelDictionary
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SaveDictionary
    Saves the contents for a dictionary to disk. The path is associated with the name when the dictionary was created
    or opened. The SaveDictionary function call is made with a process id and a dictionary name string as input to save a dictionary to the registry. 
  
   */

       int FRAMES_API _SaveDictionary (
       int PID, char *  dictionaryName);
       #ifndef BUILD_DLL
         #define SaveDictionary _SaveDictionary
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SaveDictionaryAs
    Save as dictionary to disk but does not included it in the registry. The SaveDictionaryAs 
    function call is made with a process id, a dictionary name string, a path string, and a 
    new dictionary name string as input to save a dictionary not included in the registry.
  
   */

       int FRAMES_API _SaveDictionaryAs (
       int PID, char *  dictionaryName, char *  dictionaryFile, char *  dictionaryNewName);
       #ifndef BUILD_DLL
         #define SaveDictionaryAs _SaveDictionaryAs
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DictionaryCount
    Get the count of all dictionaries in registry. The DictionaryCount function call is made with a process id and a dictionary name string as input, and a count integer to output a 
    count of all dictionaries in the registry. 
  
   */

       int FRAMES_API _DictionaryCount (
       int PID, int * count);
       #ifndef BUILD_DLL
         #define DictionaryCount _DictionaryCount
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DictionaryList
    Get the list of all dictionaries in registry in a single delimited string. The DictionaryList function call is made with a process id and a delimiter string as input, and an empty 
    list string to output a string of dictionary names separated by commas. 
  
   */

       int FRAMES_API _DictionaryList (
       int PID, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define DictionaryList _DictionaryList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetDicDescription
    Get the description of a dictionary.
  
   */

       int FRAMES_API _GetDicDescription (
       int PID, char *  dictionaryName, char *  description);
       #ifndef BUILD_DLL
         #define GetDicDescription _GetDicDescription
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetDicDescription
    Set the description of a dictionary.
  
   */

       int FRAMES_API _SetDicDescription (
       int PID, char *  dictionaryName, char *  description);
       #ifndef BUILD_DLL
         #define SetDicDescription _SetDicDescription
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetDicPrivilege
    Get the privilege of a dictionary. The privilege is either 0=System UI, 1=System Boundary Condition, 2=Module UI, 
    or 3=Module Boundary Condition. This represents the level of review a dictionary has received. A system dictionary
    should have a number of individuals agree that it is complete, useful and mutually exclusive of other system dictionaries.
    Module level privileges are meant for people to develop their own standards that may become system dictionaries
    in the future.
  
   */

       int FRAMES_API _GetDicPrivilege (
       int PID, char *  dictionaryName, int * privilege);
       #ifndef BUILD_DLL
         #define GetDicPrivilege _GetDicPrivilege
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetDicPrivilege
    Set the privilege of a dictionary.
  
   */

       int FRAMES_API _SetDicPrivilege (
       int PID, char *  dictionaryName, int privilege);
       #ifndef BUILD_DLL
         #define SetDicPrivilege _SetDicPrivilege
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetDicVersion
    Get the version of a dictionary. 
  
   */

       int FRAMES_API _GetDicVersion (
       int PID, char *  dictionaryName, int * version);
       #ifndef BUILD_DLL
         #define GetDicVersion _GetDicVersion
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetDicVersion
    Set the version of a dictionary.
  
   */

       int FRAMES_API _SetDicVersion (
       int PID, char *  dictionaryName, int version);
       #ifndef BUILD_DLL
         #define SetDicVersion _SetDicVersion
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetDicPath
    Get the path associated with a dictionary. To change the path you need to 1) SaveDictionaryAs changing the path, 
    2) DelDictionary, and 3) AddOpenDictionary from it new location.
  
   */

       int FRAMES_API _GetDicPath (
       int PID, char *  dictionaryName, char *  dictionaryFile);
       #ifndef BUILD_DLL
         #define GetDicPath _GetDicPath
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetDicUpdate
    If the state of a dictionary has changed then update flag will be non-zero. 
  
   */

       int FRAMES_API _GetDicUpdate (
       int PID, char *  dictionaryName, int * update);
       #ifndef BUILD_DLL
         #define GetDicUpdate _GetDicUpdate
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddVariable
    The AddVariable function call is made with a process id, a dictionary name string, and a 
    variable string as input to add a variable to a dictionary. The function returns 
    success/fail flag. This function must be called to allocate new space for a 
    parameter. The parameter will be added to the dictionary or dataset by _dicId with 
    the given _name. The parameter will have the default settings of:  dimension = 0; 
    description = ""; type = integer; stochastic = false; min = 0; max = 0.
  
   */

       int FRAMES_API _AddVariable (
       int PID, char *  dictionaryName, char *  variable);
       #ifndef BUILD_DLL
         #define AddVariable _AddVariable
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DelVariable
    Delete a variable from a dictionary. Warning - Deleting a variable, dictionary, or a dataset can have some
    serious side effects therefore the software will not allow the use any these delete functions when more than one 
    handle to a simulation exists. (i.e. A module and the system are active, or two system level users are active
    on a single instance of FRAMES. The DelVariable function call is made with a process id, a 
    dictionary name string, and a variable string as input to delete a variable from a dictionary. The function 
    returns success/fail flag. 
  
   */

       int FRAMES_API _DelVariable (
       int PID, char *  dictionaryName, char *  variable);
       #ifndef BUILD_DLL
         #define DelVariable _DelVariable
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetVarCount
    Gets the varible count of a dictionary. The GetVarCount function call is made with a process id, a 
    dictionary name string, and a count integer to output a count of all variables in the dictionary. The function 
    returns success/fail flag. 
  
   */

       int FRAMES_API _GetVarCount (
       int PID, char *  dictionaryName, int * count);
       #ifndef BUILD_DLL
         #define GetVarCount _GetVarCount
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetVarName
    Gets the name of the nth (index) variable in the dictionary. 
  
   */

       int FRAMES_API _GetVarName (
       int PID, char *  dictionaryName, int index, char *  variable);
       #ifndef BUILD_DLL
         #define GetVarName _GetVarName
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetVarList
    Gets the delimited list of all variable names in a dictionary. The function returns success/fail flag. 
  
   */

       int FRAMES_API _GetVarList (
       int PID, char *  dictionaryName, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define GetVarList _GetVarList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetVarName
    Set the variable name (rename).The SetVarName function call is made with a process id, a 
    dictionary name string, an existing variable name string, and a variable 
    name string as input to change the variable name. 
  
   */

       int FRAMES_API _SetVarName (
       int PID, char *  dictionaryName, char *  variableName, char *  variableNewName);
       #ifndef BUILD_DLL
         #define SetVarName _SetVarName
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetVarDescription
    Set the variable's description. The SetVarDescription function call is made with a process id, a dictionary name 
    string, a variable name string, and a description string as input to change the description of 
    the variable.
  
   */

       int FRAMES_API _SetVarDescription (
       int PID, char *  dictionaryName, char *  variableName, char *  description);
       #ifndef BUILD_DLL
         #define SetVarDescription _SetVarDescription
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetVarType
    Set the variable's type. Type can be either "String", "Logical", "Real", or "Integer".
    The SetVarType function call is made with a process id, a dictionary name string, a variable 
    name string, and a type string as input to change the type parameter of the variable.
  
   */

       int FRAMES_API _SetVarType (
       int PID, char *  dictionaryName, char *  variableName, char *  type);
       #ifndef BUILD_DLL
         #define SetVarType _SetVarType
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetVarScalar
    Set the variable's scalar flag. If the flag is 0 then a set of values is stored. (i.e. the dimensionality will be increased by 1).
    If the flag=1 a single value is stored.
  
   */

       int FRAMES_API _SetVarScalar (
       int PID, char *  dictionaryName, char *  variableName, int flag);
       #ifndef BUILD_DLL
         #define SetVarScalar _SetVarScalar
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetVarMin
    Set the variable's minimum value. Setting both minimum and maximum equal to each other 
    has the effect of having the variable NOT range checked.
  
   */

       int FRAMES_API _SetVarMin (
       int PID, char *  dictionaryName, char *  variableName, double min);
       #ifndef BUILD_DLL
         #define SetVarMin _SetVarMin
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetVarMax
    Set the variable's maximum value. Setting both minimum and maximum equal to each other 
    has the effect of having the variable NOT range checked.
  
   */

       int FRAMES_API _SetVarMax (
       int PID, char *  dictionaryName, char *  variableName, double max);
       #ifndef BUILD_DLL
         #define SetVarMax _SetVarMax
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetVarMeasure
    Set the variable's measure. 
  
   */

       int FRAMES_API _SetVarMeasure (
       int PID, char *  dictionaryName, char *  variableName, char *  measureName);
       #ifndef BUILD_DLL
         #define SetVarMeasure _SetVarMeasure
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetVarUnit
    Set the variable's unit for its given measure. 
  
   */

       int FRAMES_API _SetVarUnit (
       int PID, char *  dictionaryName, char *  variableName, char *  unitName);
       #ifndef BUILD_DLL
         #define SetVarUnit _SetVarUnit
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetVarStochastic
   Set the variable's stochastic flag. This flag signifies if the parameter can be modified by an another 
   program and not invalidate any calibration. This allows other programs such and Sensativity/Uncertainty or 
   Parameter estimation programs to modify these parameters.
  
   */

       int FRAMES_API _SetVarStochastic (
       int PID, char *  dictionaryName, char *  variableName, int flag);
       #ifndef BUILD_DLL
         #define SetVarStochastic _SetVarStochastic
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetVarPreposition
    Set the preposition, this helps in describing parameters in a human readable form. This is the word that would best be used to write a description of this 
   parameters as an index for another. For example the chemical parameter would use "for" as a preposition to 
   make statements such as Concentration for benzene. The preposition greatly facilitates writing descriptive
   text associated with results. The SetVarPreposition function call is made with a process id, a dictionary name string, a variable name string, 
   and a prep string as input to change the preposition parameter of the variable. 
  
   */

       int FRAMES_API _SetVarPreposition (
       int PID, char *  dictionaryName, char *  variableName, char *  prepositionName);
       #ifndef BUILD_DLL
         #define SetVarPreposition _SetVarPreposition
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetVarPrimaryKey
   Set the variable primarykey flag. Is this parameter a key to information on other parameters. This is 
   used in both databases and in the representation of the data. For example a variable being used to index 
   other variables frequently (like chemical) would indicate that chemical should be a primary key. The typical
   meaning of primary key from Database design is not incorrect when used here.
  
   */

       int FRAMES_API _SetVarPrimaryKey (
       int PID, char *  dictionaryName, char *  variableName, int flag);
       #ifndef BUILD_DLL
         #define SetVarPrimaryKey _SetVarPrimaryKey
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddVarIndex
   Add a variable index or label index to a variable. The AddVarIndex function call is made 
   with a process id, a dictionary name string, a variable name string, a dictionary 
   reference string, and an index string as input to add a variable index or label 
   index to a variable.
  
   */

       int FRAMES_API _AddVarIndex (
       int PID, char *  dictionaryName, char *  variableName, char *  reference, char *  index);
       #ifndef BUILD_DLL
         #define AddVarIndex _AddVarIndex
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DelVarIndex
   Delete an indice from a variable. The DelVarIndex function call is made with a process id, a dictionary 
   name string, a variable name string, and an index string as input to delete a variable index or label 
   index to a variable.
  
   */

       int FRAMES_API _DelVarIndex (
       int PID, char *  dictionaryName, char *  variableName, char *  reference, char *  index);
       #ifndef BUILD_DLL
         #define DelVarIndex _DelVarIndex
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetVarIndexCount
   Get the count of indices for a variable. The GetVarIndexCount function call is made with a process id, a 
   dictionary name string, and a variable name string as input, and an empty count integer to output a 
   count of indices for a variable. 
  
   */

       int FRAMES_API _GetVarIndexCount (
       int PID, char *  dictionaryName, char *  variableName, int * count);
       #ifndef BUILD_DLL
         #define GetVarIndexCount _GetVarIndexCount
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: VarIndexList
   Get the list of all indices for a variable. The VarIndexList function call is made with a process 
   id, a dictionary name string, a variable name string, and a delimiter string as input, and an 
   empty list string to output a list of indices for a variable. 
  
   */

       int FRAMES_API _VarIndexList (
       int PID, char *  dictionaryName, char *  variableName, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define VarIndexList _VarIndexList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: PromoteVarIndex
    Move and index up one level in the order of referred indices. 
  
   */

       int FRAMES_API _PromoteVarIndex (
       int PID, char *  dictionaryName, char *  variableName, char *  reference, char *  index);
       #ifndef BUILD_DLL
         #define PromoteVarIndex _PromoteVarIndex
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DemoteVarIndex
    Move and index down one level in the order of referred indices. 
  
   */

       int FRAMES_API _DemoteVarIndex (
       int PID, char *  dictionaryName, char *  variableName, char *  reference, char *  index);
       #ifndef BUILD_DLL
         #define DemoteVarIndex _DemoteVarIndex
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddNewModule
   Create and add a module description in registry module name passed in. The AddNewModule function call is 
  made with a process id, a path string, and a module name string as input to create and add a 
  module description in the registry. It is expected that .mod file does not exist or is empty.
  
   */

       int FRAMES_API _AddNewModule (
       int PID, char *  moduleFile, char *  moduleName);
       #ifndef BUILD_DLL
         #define AddNewModule _AddNewModule
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddOpenModule
   Open and add a module description in registry module name passed out. The AddOpenModule function 
   call is made with a process id and a path string as input to open and add a module description 
   in the registry, and a module name string to output the module name. 
  
   */

       int FRAMES_API _AddOpenModule (
       int PID, char *  moduleFile, char *  module);
       #ifndef BUILD_DLL
         #define AddOpenModule _AddOpenModule
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DelModule
   Delete a module description dataset from registry. The DelModule function call is made with a process 
   id and a module name string as input to delete a module description dataset from the registry. 
  
   */

       int FRAMES_API _DelModule (
       int PID, char *  moduleName);
       #ifndef BUILD_DLL
         #define DelModule _DelModule
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SaveModule
   Save the module description dataset to registry. The SaveModule function call is made with a 
   process id and a module name string as input to save a module description dataset in the registry. 
  
   */

       int FRAMES_API _SaveModule (
       int PID, char *  moduleName);
       #ifndef BUILD_DLL
         #define SaveModule _SaveModule
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SaveModuleAs
   Save the module description as another dataset to registry. The SaveModuleAs function call is made with 
   a process id,  an old module name string, a path string, and a new module name string as input to save 
   the module description as another dataset in the registry. The old module name continues to be the 
   open module. 
  
   */

       int FRAMES_API _SaveModuleAs (
       int PID, char *  moduleName, char *  moduleFile, char *  moduleNewName);
       #ifndef BUILD_DLL
         #define SaveModuleAs _SaveModuleAs
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ModuleCount
   Get the count of all modules in registry. The SaveModuleAs function call is made with 
   a process id,  an old module name string, a path string, and a new module name string as input to save 
   the module description as another dataset in the registry. The old module name continues to be the 
   open module. 
  
   */

       int FRAMES_API _ModuleCount (
       int PID, int * count);
       #ifndef BUILD_DLL
         #define ModuleCount _ModuleCount
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ModuleList
   Get the list of all module desriptions in registry. The ModuleList function call is made with a 
   process id, a mod string, and a delimiter string as input, and an empty list string to output a 
   comma-separated list of all module descriptions in the registry. 
  
   */

       int FRAMES_API _ModuleList (
       int PID, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define ModuleList _ModuleList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetModPath
   Get module path associated with a module. 
  
   */

       int FRAMES_API _GetModPath (
       int PID, char *  moduleName, char *  moduleFile);
       #ifndef BUILD_DLL
         #define GetModPath _GetModPath
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddScheme
   Add a scheme to a module. 
  
   */

       int FRAMES_API _AddScheme (
       int PID, char *  moduleName, char *  schemeName);
       #ifndef BUILD_DLL
         #define AddScheme _AddScheme
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DelScheme
   Delete a scheme from a module. 
  
   */

       int FRAMES_API _DelScheme (
       int PID, char *  moduleName, char *  schemeName);
       #ifndef BUILD_DLL
         #define DelScheme _DelScheme
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SchemeCount
   Get the number of the schemes for a module. 
  
   */

       int FRAMES_API _SchemeCount (
       int PID, char *  moduleName, int * count);
       #ifndef BUILD_DLL
         #define SchemeCount _SchemeCount
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SchemeList
   Get the number of the schemes for a module. 
  
   */

       int FRAMES_API _SchemeList (
       int PID, char *  moduleName, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define SchemeList _SchemeList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddSchemeIDic
   Add a dictionary to a scheme for a module. 
  
   */

       int FRAMES_API _AddSchemeIDic (
       int PID, char *  moduleName, char *  schemeName, char *  dictionaryName);
       #ifndef BUILD_DLL
         #define AddSchemeIDic _AddSchemeIDic
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DelSchemeIDic
   Delete a dictionary from a scheme for a module. 
  
   */

       int FRAMES_API _DelSchemeIDic (
       int PID, char *  moduleName, char *  schemeName, char *  dictionaryName);
       #ifndef BUILD_DLL
         #define DelSchemeIDic _DelSchemeIDic
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SchemeIDicCount
   Get the number of input dictionaries associated with a scheme for a module. 
  
   */

       int FRAMES_API _SchemeIDicCount (
       int PID, char *  moduleName, char *  schemeName, int * count);
       #ifndef BUILD_DLL
         #define SchemeIDicCount _SchemeIDicCount
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SchemeIDicList
   Get a single string that contains all the names of the input dictionaries associated with a scheme for a module. 
  
   */

       int FRAMES_API _SchemeIDicList (
       int PID, char *  moduleName, char *  schemeName, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define SchemeIDicList _SchemeIDicList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddSchemeODic
   Add an output dictionary to a scheme for a module. 
  
   */

       int FRAMES_API _AddSchemeODic (
       int PID, char *  moduleName, char *  schemeName, char *  dictionaryName);
       #ifndef BUILD_DLL
         #define AddSchemeODic _AddSchemeODic
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DelSchemeODic
   Delete an output dictionary from a scheme for a module. 
  
   */

       int FRAMES_API _DelSchemeODic (
       int PID, char *  moduleName, char *  schemeName, char *  dictionaryName);
       #ifndef BUILD_DLL
         #define DelSchemeODic _DelSchemeODic
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SchemeODicCount
   Get the number of output dictionaries associated with a scheme for a module. 
  
   */

       int FRAMES_API _SchemeODicCount (
       int PID, char *  moduleName, char *  schemeName, int * count);
       #ifndef BUILD_DLL
         #define SchemeODicCount _SchemeODicCount
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SchemeODicList
   Get a single string that contains all the names of the output dictionaries associated with a scheme for a module. 
  
   */

       int FRAMES_API _SchemeODicList (
       int PID, char *  moduleName, char *  schemeName, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define SchemeODicList _SchemeODicList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetModDimSize
    Get the dimension size for the dataset information about modules.
  
   */

       int FRAMES_API _GetModDimSize (
       int PID, char *  iconName, char *  variableName, int * indices, int * count);
       #ifndef BUILD_DLL
         #define GetModDimSize _GetModDimSize
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetModInteger
    Get an integer from meta data about a module.
  
   */

       int FRAMES_API _GetModInteger (
       int PID, char *  iconName, char *  variableName, char *  unitName, int * indices, int * value);
       #ifndef BUILD_DLL
         #define GetModInteger _GetModInteger
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetModDouble
    Get an integer from meta data about a module.
  
   */

       int FRAMES_API _GetModDouble (
       int PID, char *  iconName, char *  variableName, char *  unitName, int * indices, double * value);
       #ifndef BUILD_DLL
         #define GetModDouble _GetModDouble
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetModLogical
    Get an integer from meta data about a module.
  
   */

       int FRAMES_API _GetModLogical (
       int PID, char *  iconName, char *  variableName, char *  unitName, int * indices, int * value);
       #ifndef BUILD_DLL
         #define GetModLogical _GetModLogical
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetModString
    Get an integer from meta data about a module.
  
   */

       int FRAMES_API _GetModString (
       int PID, char *  iconName, char *  variableName, char *  unitName, int * indices, char *  value);
       #ifndef BUILD_DLL
         #define GetModString _GetModString
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: PutModInteger
    Put an integer of meta data about a module.
  
   */

       int FRAMES_API _PutModInteger (
       int PID, char *  iconName, char *  variableName, char *  unitName, int * indices, int value);
       #ifndef BUILD_DLL
         #define PutModInteger _PutModInteger
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: PutModDouble
    Put a double of meta data about a module.
  
   */

       int FRAMES_API _PutModDouble (
       int PID, char *  iconName, char *  variableName, char *  unitName, int * indices, double value);
       #ifndef BUILD_DLL
         #define PutModDouble _PutModDouble
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: PutModLogical
    Put an logical of meta data about a module.
  
   */

       int FRAMES_API _PutModLogical (
       int PID, char *  iconName, char *  variableName, char *  unitName, int * indices, int value);
       #ifndef BUILD_DLL
         #define PutModLogical _PutModLogical
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: PutModString
    Put a string of meta data about a module.
  
   */

       int FRAMES_API _PutModString (
       int PID, char *  iconName, char *  variableName, char *  unitName, int * indices, char *  value);
       #ifndef BUILD_DLL
         #define PutModString _PutModString
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddDomain
    Automatically adds the four class types to the domain. The AddDomain function call is made with a process id and a domain name string as input to add a domain; automatically adds the four class types to the domain. 
  
   */

       int FRAMES_API _AddDomain (
       int PID, char *  domainName);
       #ifndef BUILD_DLL
         #define AddDomain _AddDomain
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DelDomain
    Deletes the domain and it tree. The DelDomain function call is made with a process id and a domain name string as input to delete a domain and its tree, deleting its classes, groups, subgroups, and modules. 
  
   */

       int FRAMES_API _DelDomain (
       int PID, char *  domainName);
       #ifndef BUILD_DLL
         #define DelDomain _DelDomain
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetDomainIcon
    Get the icon associated with a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels.
  
   */

       int FRAMES_API _GetDomainIcon (
       int PID, char *  domainName, char *  domainIconFile);
       #ifndef BUILD_DLL
         #define GetDomainIcon _GetDomainIcon
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetDomainIcon
    Set the icon associated with a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels.
  
   */

       int FRAMES_API _SetDomainIcon (
       int PID, char *  domainName, char *  domainIconFile);
       #ifndef BUILD_DLL
         #define SetDomainIcon _SetDomainIcon
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddClass
    For future use - not presently active. The AddClass function call is made with a process id, a domain name string, and a class name string as input to add a class. 
  
   */

       int FRAMES_API _AddClass (
       int PID, char *  domainName, char *  className);
       #ifndef BUILD_DLL
         #define AddClass _AddClass
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DelClass
    For future use - not presently active. The DelClass function call is made with a process id, a domain name string, and a class name string as input to delete a class from a domain, including its groups, subgroups, and modules. 
  
   */

       int FRAMES_API _DelClass (
       int PID, char *  domainName, char *  className);
       #ifndef BUILD_DLL
         #define DelClass _DelClass
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetClassIcon
    Get the icon associated with a class in a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels.
  
   */

       int FRAMES_API _GetClassIcon (
       int PID, char *  domainName, char *  className, char *  classIconFile);
       #ifndef BUILD_DLL
         #define GetClassIcon _GetClassIcon
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetClassIcon
    Set the icon associated with a class in a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels.
  
   */

       int FRAMES_API _SetClassIcon (
       int PID, char *  domainName, char *  className, char *  classIconFile);
       #ifndef BUILD_DLL
         #define SetClassIcon _SetClassIcon
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddGroup
    The AddGroup function call is made with a process id, a domain name string,  a class name string, and a group name string as input to add a group to a domain and class. 
  
   */

       int FRAMES_API _AddGroup (
       int PID, char *  domainName, char *  className, char *  groupName);
       #ifndef BUILD_DLL
         #define AddGroup _AddGroup
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DelGroup
    The DelGroup function call is made with a process id, a domain name string,  a class name string, and a group name string as input to delete a group from a domain and class, including its subgroups and modules. 
  
   */

       int FRAMES_API _DelGroup (
       int PID, char *  domainName, char *  className, char *  groupName);
       #ifndef BUILD_DLL
         #define DelGroup _DelGroup
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetGroupIcon
    Get the icon associated with a class in a domain. Icon are either .bmp, .gif files for image. They should be small 32x2 pixels.
  
   */

       int FRAMES_API _GetGroupIcon (
       int PID, char *  domainName, char *  className, char *  groupName, char *  groupIconFile);
       #ifndef BUILD_DLL
         #define GetGroupIcon _GetGroupIcon
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetGroupIcon
    Set the icon associated with a class in a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels.
  
   */

       int FRAMES_API _SetGroupIcon (
       int PID, char *  domainName, char *  className, char *  groupName, char *  groupIconFile);
       #ifndef BUILD_DLL
         #define SetGroupIcon _SetGroupIcon
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddSubGroup
    The AddSubGroup function call is made with a process id, a domain name string, a class name string, a group name string, and a subgroup name string as input to add a subgroup to a domain, class, and group. 
  
   */

       int FRAMES_API _AddSubGroup (
       int PID, char *  domainName, char *  className, char *  groupName, char *  subgroupName);
       #ifndef BUILD_DLL
         #define AddSubGroup _AddSubGroup
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DelSubGroup
    The DelSubGroup function call is made with a process id, a domain name string, a class name string, a group name string, and a subgroup name string as input to delete a subgroup from a domain, class, and group, including its modules. 
  
   */

       int FRAMES_API _DelSubGroup (
       int PID, char *  domainName, char *  className, char *  groupName, char *  subgroupName);
       #ifndef BUILD_DLL
         #define DelSubGroup _DelSubGroup
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetSubGrpIcon
    Get the icon associated with a class in a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels.
  
   */

       int FRAMES_API _GetSubGrpIcon (
       int PID, char *  domainName, char *  className, char *  groupName, char *  subgroupName, char *  subgroupIconFile);
       #ifndef BUILD_DLL
         #define GetSubGrpIcon _GetSubGrpIcon
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetSubGrpIcon
    Set the icon associated with a class in a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels.
  
   */

       int FRAMES_API _SetSubGrpIcon (
       int PID, char *  domainName, char *  className, char *  groupName, char *  subgroupName, char *  subgroupIconFile);
       #ifndef BUILD_DLL
         #define SetSubGrpIcon _SetSubGrpIcon
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddGroupModule
    Add a module at the group level. The AddGroupModule function call is made with a process id, a domain name string, a class name string, a group name string, and a module name string as input to add a module to a domain, class, and group. 
  
   */

       int FRAMES_API _AddGroupModule (
       int PID, char *  domainName, char *  className, char *  groupName, char *  moduleName);
       #ifndef BUILD_DLL
         #define AddGroupModule _AddGroupModule
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DelGroupModule
    Remove a module at the group level. The DelGroupModule function call is made with a process id, a domain name string, a class name string, a group name string, and a module name string as input to delete a module from a domain, class, and group. 
  
   */

       int FRAMES_API _DelGroupModule (
       int PID, char *  domainName, char *  className, char *  groupName, char *  moduleName);
       #ifndef BUILD_DLL
         #define DelGroupModule _DelGroupModule
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddSubGrpModule
    Add a module at the subgroup level. The AddSubGrpModule function call is made with a process id, a domain name string, a class name string, a group name string, a subgroup name string, and a module name string as input to add a module to a domain, class, group, and subgroup. 
  
   */

       int FRAMES_API _AddSubGrpModule (
       int PID, char *  domainName, char *  className, char *  groupName, char *  subgroupName, char *  moduleName);
       #ifndef BUILD_DLL
         #define AddSubGrpModule _AddSubGrpModule
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DelSubGrpModule
    Remove a module at the subgroup level. The DelSubGrpModule function call is made with a process id, a domain name string, a class name string, a group name string, a subgroup name string, and module name string as input to delete a module from a domain, class, group, and subgroup. 
  
   */

       int FRAMES_API _DelSubGrpModule (
       int PID, char *  domainName, char *  className, char *  groupName, char *  subgroupName, char *  moduleName);
       #ifndef BUILD_DLL
         #define DelSubGrpModule _DelSubGrpModule
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetDomainList
    The GetDomainList function call is made with a process id and a delimiter string as input, and an empty list string to output a comma-separated list of all domains in the registry. 
  
   */

       int FRAMES_API _GetDomainList (
       int PID, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define GetDomainList _GetDomainList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetClassList
    The GetClassList function call is made with a process id, a domain name string, and a delimiter string as input, and an empty list string to output a comma-separated list of all classes in a domain. 
  
   */

       int FRAMES_API _GetClassList (
       int PID, char *  domainName, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define GetClassList _GetClassList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetGroupList
    The GetGroupList function call is made with a process id, a domain name string, a class name string, and a delimiter string as input, and an empty list string to output a comma-separated list of all groups in a domain and class. 
  
   */

       int FRAMES_API _GetGroupList (
       int PID, char *  domainName, char *  className, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define GetGroupList _GetGroupList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetSubGrpList
    The GetSubGrpList function call is made with a process id, a domain name string, a class name string, a group name string, and a delimiter string as input, and an empty list string to output a comma-separated list of all subgroups in a domain, class, and group. 
  
   */

       int FRAMES_API _GetSubGrpList (
       int PID, char *  domainName, char *  className, char *  groupName, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define GetSubGrpList _GetSubGrpList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetGroupModList
    The GetGroupModList function call is made with a process id, a domain name string, a class name string, a group name string, and a delimiter string as input, and an empty list string to output a comma-separated list of all subgroups in a domain, class, and group. 
  
   */

       int FRAMES_API _GetGroupModList (
       int PID, char *  domainName, char *  className, char *  groupName, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define GetGroupModList _GetGroupModList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetSubGrpModList
    The GetSubGrpModList function call is made with a process id, a domain name string, a class name string, a group name string, a subgroup name string, and a delimiter string as input, and an empty list string to output a comma-separated list of all subgroups in a domain, class, and group. 
  
   */

       int FRAMES_API _GetSubGrpModList (
       int PID, char *  domainName, char *  className, char *  groupName, char *  subgroupName, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define GetSubGrpModList _GetSubGrpModList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetDomainCount
    Get the number of domains in the registery. The GetDomainCount function call is made with a process id as input, and an empty count integer to output a count of all domains in the registry. 
  
   */

       int FRAMES_API _GetDomainCount (
       int PID, int * count);
       #ifndef BUILD_DLL
         #define GetDomainCount _GetDomainCount
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetClassCount
    Get the number of classes in a domain. The GetClassCount function call is made with a process id and a domain name string as input, and an empty count integer to output a count of all classes in a domain. 
  
   */

       int FRAMES_API _GetClassCount (
       int PID, char *  domainName, int * count);
       #ifndef BUILD_DLL
         #define GetClassCount _GetClassCount
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetGroupCount
    Get the number of groups in a class, and domain. The GetGroupCount function call is made with a process id, a domain name string, and a class name string as input, and an empty count integer to output a count of all groups in a domain and class. 
  
   */

       int FRAMES_API _GetGroupCount (
       int PID, char *  domainName, char *  className, int * count);
       #ifndef BUILD_DLL
         #define GetGroupCount _GetGroupCount
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetSubGrpCount
    Get the number of subgroups in a group, class and domain.The GetSubGrpCount function call is made with a process id, a domain name string, a class name string, and a group name string as input, and an empty count integer to output a count of all subgroups in a domain, class, and group.
  
   */

       int FRAMES_API _GetSubGrpCount (
       int PID, char *  domainName, char *  className, char *  groupName, int * count);
       #ifndef BUILD_DLL
         #define GetSubGrpCount _GetSubGrpCount
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetGroupModCount
    Get the number of modules at the group level for a group, class, and domain. The GetGroupModCount function call is made with a process id, a domain name string, a class name string, and a group name string as input, and an empty count integer to output a count of all modules in a domain, class, and group. 
  
   */

       int FRAMES_API _GetGroupModCount (
       int PID, char *  domainName, char *  className, char *  groupName, int * count);
       #ifndef BUILD_DLL
         #define GetGroupModCount _GetGroupModCount
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetSubGrpModCount
    Get the number of modules at the subgroup level for a subgroup, group, class, and domain. The GetSubGrpModCount function call is made with a process id, a domain name string, a class name string, a group name string, and a subgroup name string as input, and an empty count integer to output a count of all modules in a domain, class, group, and subgroup.
  
   */

       int FRAMES_API _GetSubGrpModCount (
       int PID, char *  domainName, char *  className, char *  groupName, char *  subgroupName, int * count);
       #ifndef BUILD_DLL
         #define GetSubGrpModCount _GetSubGrpModCount
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: NewSim
    Open a new simulation return a simulation dataset name. The NewSim function call is made with a process id and a filename string as input to open a new simulation. Only one simulation is allowed to be open at a time in a FRAMES session, however, multiple FRAMES sessions may be open at the same time. 
  
   */

       int FRAMES_API _NewSim (
       int PID, char *  simulationFile, char *  simulationName);
       #ifndef BUILD_DLL
         #define NewSim _NewSim
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: OpenSim
    Open an existing simulation return a simulation dataset name. The OpenSim function call is made with a process id and a filename string as input to open an existing simulation. 
  
   */

       int FRAMES_API _OpenSim (
       int PID, char *  simulationFile, char *  simulation);
       #ifndef BUILD_DLL
         #define OpenSim _OpenSim
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SaveSim
    Save the simulation present state. The SaveSim function call is made with a process id as input to save the simulation's present state.
  
   */

       int FRAMES_API _SaveSim (
       int PID);
       #ifndef BUILD_DLL
         #define SaveSim _SaveSim
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SaveSimAs
    Open an exsisting simulation return a simId. The SaveSimAs function call is made with a process id and a filename string (new name) as input to save a simulation (SDE) as another name, but continue to work in same simulation name. 
  
   */

       int FRAMES_API _SaveSimAs (
       int PID, char *  simulationFile, char *  simulationNewName);
       #ifndef BUILD_DLL
         #define SaveSimAs _SaveSimAs
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: CloseSim
    Close the simulation without saving. The CloseSim function call is made with a process id as input to close the simulation without saving. 
  
   */

       int FRAMES_API _CloseSim (
       int PID);
       #ifndef BUILD_DLL
         #define CloseSim _CloseSim
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetSim
    Get current simulation name for which all simulation functions will use for this PID.
  
   */

       int FRAMES_API _GetSim (
       int PID, char *  simulationFile, char *  simulation);
       #ifndef BUILD_DLL
         #define GetSim _GetSim
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetSim
    Set current simulation name for which all simulation functions will use for this PID.
  
   */

       int FRAMES_API _SetSim (
       int PID, char *  simulationName);
       #ifndef BUILD_DLL
         #define SetSim _SetSim
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetSimList
    Get the list of all simulations loaded in the environment. 
  
   */

       int FRAMES_API _GetSimList (
       int PID, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define GetSimList _GetSimList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddNewDataSet
    Create a new dataset using a registered dictionary. The AddNewDataSet function call is made with a process id, a dictionary name string, a path name string, and a set string as input to create a new dataset using a registered dictionary.
  
   */

       int FRAMES_API _AddNewDataSet (
       int PID, char *  dictionaryName, char *  datasetFile, char *  datasetName);
       #ifndef BUILD_DLL
         #define AddNewDataSet _AddNewDataSet
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddOpenDataSet
    Open a dataset, dataset file must exist or an error will occur. The AddOpenDataSet function call is made with a process id, a path name string, and a set string as input to open an existing dataset to the registry. 
  
   */

       int FRAMES_API _AddOpenDataSet (
       int PID, char *  dictionaryName, char *  datasetFile, char *  dset);
       #ifndef BUILD_DLL
         #define AddOpenDataSet _AddOpenDataSet
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DelDataSet
    Delete a dataset. The DelDataSet function call is made with a process id and a set string as input to delete a dataset. Does not write the dataset before deletion.
  
   */

       int FRAMES_API _DelDataSet (
       int PID, char *  datasetName);
       #ifndef BUILD_DLL
         #define DelDataSet _DelDataSet
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SaveDataSet
    Save the dataset. The SaveDataSet function call is made with a process id and a set string as input to save a dataset.
  
   */

       int FRAMES_API _SaveDataSet (
       int PID, char *  datasetName);
       #ifndef BUILD_DLL
         #define SaveDataSet _SaveDataSet
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SaveDataSetAs
    Save the dataset with a different name. The SaveDataSetAs function call is made with a process id, an oldSet string, a path string, and a new dataset string as input to save a dataset under a new name.
  
   */

       int FRAMES_API _SaveDataSetAs (
       int PID, char *  datasetName, char *  datasetFile, char *  datasetNewName);
       #ifndef BUILD_DLL
         #define SaveDataSetAs _SaveDataSetAs
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetDataSetList
    Get the list of all the datasets for simulation. 
  
   */

       int FRAMES_API _GetDataSetList (
       int PID, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define GetDataSetList _GetDataSetList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetDataSetPath
    Get the path associated with a dictionary. To change the path you need to 1) SaveDictionaryAs changing the path, 2) DelDictionary, and 3) AddOpenDictionary from it new location.
  
   */

       int FRAMES_API _GetDataSetPath (
       int PID, char *  datasetName, char *  datasetFile);
       #ifndef BUILD_DLL
         #define GetDataSetPath _GetDataSetPath
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddLink
    Returns SUCCESS if the connection is made in the simulation. The AddLink function call is made with a process id, a modIdFrom string, and a modIdTo string as input to connect two modules, and returns a success/fail flag. The arrow of the connection will point from  -> to.
  
   */

       int FRAMES_API _AddLink (
       int PID, char *  idfrom, char *  idto);
       #ifndef BUILD_DLL
         #define AddLink _AddLink
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DelLink
    Returns SUCCESS if the connection is dropped in the simulation.
    The DelLink function call is made with a process id, a modIdFrom string, and a modIdTo string as input to 
    delete the connection between two modules, and returns a success/fail flag. 
  
   */

       int FRAMES_API _DelLink (
       int PID, char *  fromid, char *  toid);
       #ifndef BUILD_DLL
         #define DelLink _DelLink
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddIcon
    Returns the module ID added to the simulation.
    if scope is 0 then the icon is added to the global diagram. if scope is non zero it is added to the local diagram.
  
   */

       int FRAMES_API _AddIcon (
       int PID, int scope, char *  domainName, char *  className, char *  groupName, char *  subgroupName, char *  id);
       #ifndef BUILD_DLL
         #define AddIcon _AddIcon
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DelIcon
    Deletes the module ID from the simulation.
    The DelIcon function call is made with a process id and a id string as input to delete the module ID from the simulation. 
  
   */

       int FRAMES_API _DelIcon (
       int PID, char *  iconName);
       #ifndef BUILD_DLL
         #define DelIcon _DelIcon
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetIconUIDic
    Returns the dic for the module icon returns "" when no set exists.
  
   */

       int FRAMES_API _GetIconUIDic (
       int PID, char *  iconName, char *  dictionary);
       #ifndef BUILD_DLL
         #define GetIconUIDic _GetIconUIDic
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetIconUISet
    Returns the set for the module icon returns "" when no set exists.
  
   */

       int FRAMES_API _GetIconUISet (
       int PID, char *  iconName, char *  dataset);
       #ifndef BUILD_DLL
         #define GetIconUISet _GetIconUISet
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetIconMod
    Sets module and scheme associated with an icon. A subset of the datasets the icon consumes is given in the list parameter.
  
   */

       int FRAMES_API _SetIconMod (
       int PID, char *  iconName, char *  moduleName, char *  schemeName, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define SetIconMod _SetIconMod
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetIconMod
    Sets module and scheme associated with an icon. A list of datasets is returned in the list parameter.
  
   */

       int FRAMES_API _GetIconMod (
       int PID, char *  iconName, char *  module, char *  scheme);
       #ifndef BUILD_DLL
         #define GetIconMod _GetIconMod
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetIconUIArgs
    Get the commandline arguments for a given id.
    The active part of the icon is the user interface that requires the user to be active during its execution.
  
   */

       int FRAMES_API _GetIconUIArgs (
       int PID, char *  iconName, char *  uiExecutableFile, char *  cmdLine);
       #ifndef BUILD_DLL
         #define GetIconUIArgs _GetIconUIArgs
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: RunIconUI
    The active part of the icon is the user interface that requires the user to be active during its execution.
    The RunIconActive function call is made with a process id and a modId string as input to run the active module. 
    Returns a success/fail flag. See GetIconActiveArgs for the cmdline parameter.
  
   */

       int FRAMES_API _RunIconUI (
       int PID, char *  iconName, char *  uiExecutableFile, char *  cmdLine, char *  warningFile, char *  errorFile);
       #ifndef BUILD_DLL
         #define RunIconUI _RunIconUI
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetIconModelArgs
    The passive part of the icon is the model itself that requires NO user to be active during its execution.
  
   */

       int FRAMES_API _GetIconModelArgs (
       int PID, char *  iconName, char *  modelExecutableFile, char *  cmdLine);
       #ifndef BUILD_DLL
         #define GetIconModelArgs _GetIconModelArgs
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: RunIconModel
    The passive part of the icon is the model itself that requires NO user to be active during its execution.
    The RunModPassive function call is made with a process id and a modId string as input to run a passive module.
    Returns a success/fail flag. 
  
   */

       int FRAMES_API _RunIconModel (
       int PID, char *  iconName, char *  modelExecutableFile, char *  cmdLine, char *  warningFile, char *  errorFile);
       #ifndef BUILD_DLL
         #define RunIconModel _RunIconModel
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: IsIconUIOk
    Returns SUCCESS if no error during the execution of the user interface.
  
   */

       int FRAMES_API _IsIconUIOk (
       int PID, char *  iconName);
       #ifndef BUILD_DLL
         #define IsIconUIOk _IsIconUIOk
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: IsIconModelOk
    Returns SUCCESS if no error during the execution of the model.
  
   */

       int FRAMES_API _IsIconModelOk (
       int PID, char *  iconName);
       #ifndef BUILD_DLL
         #define IsIconModelOk _IsIconModelOk
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: IsIconInfoOk
    Returns SUCCESS if no error if the information for the module is complete (i.e. module and scheme have been chosen).
  
   */

       int FRAMES_API _IsIconInfoOk (
       int PID, char *  iconName);
       #ifndef BUILD_DLL
         #define IsIconInfoOk _IsIconInfoOk
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: RunBetweenIcons
    Returns SUCCESS if no error.
  
   */

       int FRAMES_API _RunBetweenIcons (
       int PID, char *  idBegin, char *  idEnd);
       #ifndef BUILD_DLL
         #define RunBetweenIcons _RunBetweenIcons
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetState
    Abitrarily set the state of an icon.
  
   */

       int FRAMES_API _SetState (
       int PID, char *  iconName, int state, int downstate);
       #ifndef BUILD_DLL
         #define SetState _SetState
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: LaunchTool
    Launches the specified module.
  
   */

       int FRAMES_API _LaunchTool (
       int PID, char *  moduleName, int isSystemTool, char *  iconId);
       #ifndef BUILD_DLL
         #define LaunchTool _LaunchTool
       #endif

  
  #ifdef __cplusplus
  }
  #endif
#endif
