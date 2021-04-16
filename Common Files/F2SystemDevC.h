#ifndef FRAMES2API_F2SystemDev_H
#define FRAMES2API_F2SystemDev_H

#ifndef BUILD_DLL
  #define FRAMES2API __declspec(dllimport) __stdcall
#else
  #define FRAMES2API __declspec(dllexport) __stdcall
#endif

#ifdef __cplusplus
  extern "C" {
#else
  #define bool int
#endif

  // API Name: FRAMES System API
  /* 
       Date:       2001    Programmer: Mitch Pelton, Karl Castleton, and 
       Bonnie Hoopes    Company:    Pacific Northwest National Laboratories  
                     Operated by Battelle Memorial Institute                
       For Department of Energy                    This collection of 
       functions is intented for FRAMES 2.0 system developers.    Most will 
       not do anything unless the caller has a Process Id (PID) that was    
       received by calling OpenINI. Model/module developers should restrict 
       their     calls to the DataSet API and the Conversion API. All 
       functions will return    an error code unless otherwise stated. An 
       error code of zero is to be     considered a success.        The 
       domains and their pallette are a way to arrange the different types 
       of    modules (functions, procedure) available to the system.         
       All system I/O is saved memory and is only saved to disk when a save 
       API    call is made.         The following terminology is applicable 
       to this documentation         Fully Qualified Path     example: 
       "C:\FramesV2\Simulations\GWtest\test.sim"        Full Path            
           example: "\FramesV2\Simulations\GWtest\test.sim"        Path      
                      example: "\FramesV2\Simulations\Gwtest"        File    
                        example: "test.sim"
  */
  /*============================================================================
     Documentation for: F2OpenINI
    ============================================================================
       Open the INI and returns your PID. If the Startup.ini file does not 
       exist path is the location where it will be created.    The OpenINI 
       function call is the first call made; if OpenINI is never called,     
       all API calls are ignored. All changes to INI are persistent.     The 
       OpenINI function call is made with the file path as input (from 
       command line), opens     the INI, and returns the process id.
  */
  int  FRAMES2API _F2OpenINI(char* _StartupFile);
    #ifndef BUILD_DLL
      #define F2OpenINI _F2OpenINI
    #endif

  /*============================================================================
     Documentation for: F2CloseINI
    ============================================================================
       Closes the entire frames session. If cancel is 0 all Startup.ini 
       changes will be written. If cancel is 1 then no changes are written.  
          The CloseINI function call is made with a process id as input and 
       closes the entire FRAMES session.
  */
  void  FRAMES2API _F2CloseINI(int _PID,int _cancel);
    #ifndef BUILD_DLL
      #define F2CloseINI _F2CloseINI
    #endif

  /*============================================================================
     Documentation for: F2NewDictionary
    ============================================================================
       Create and add dictionary in registry,  dictionary name will be set 
       to the parameter dictionary. The dictionary file is expected to be 
       empty. The NewDictionary function call is made with a process id and 
       path string as input, and an empty dic string to output the 
       dictionary name. This function will create and add a dictionary.
  */
  long  FRAMES2API _F2NewDictionary(int _PID,char* _dictionaryFile,char* _dictionaryName);
    #ifndef BUILD_DLL
      #define F2NewDictionary _F2NewDictionary
    #endif

  /*============================================================================
     Documentation for: F2OpenDictionary
    ============================================================================
       Open and add dictionary in registry, dictionary returns the 
       dictionary name. The dictionary file is expected to be complete for 
       this function to reach success (return a 0). The AddOpenDictionary 
       function call is made with a process id and path string as input, and 
       an empty dic string to output the dictionary name. This function will 
       add a dictionary to the registry.
  */
  long  FRAMES2API _F2OpenDictionary(int _PID,char* _dictionaryFile);
    #ifndef BUILD_DLL
      #define F2OpenDictionary _F2OpenDictionary
    #endif

  /*============================================================================
     Documentation for: F2DeleteDictionary
    ============================================================================
       Delete a dictionary from FRAMES 2.0 Startup.ini. The DelDictionary 
       function call is made with a process id and a dictionary name string 
       as input to delete a dictionary from the registry.
  */
  void  FRAMES2API _F2DeleteDictionary(int _PID,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define F2DeleteDictionary _F2DeleteDictionary
    #endif

  /*============================================================================
     Documentation for: F2SaveDictionary
    ============================================================================
       Saves the contents for a dictionary to disk. The path is associated 
       with the name when the dictionary was created or opened. The 
       SaveDictionary function call is made with a process id and a 
       dictionary name string as input to save a dictionary to the registry.
  */
  void  FRAMES2API _F2SaveDictionary(int _PID,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define F2SaveDictionary _F2SaveDictionary
    #endif

  /*============================================================================
     Documentation for: F2SaveDictionaryAs
    ============================================================================
       Save as dictionary to disk but does not included it in the registry. 
       The SaveDictionaryAs function call is made with a process id, a 
       dictionary handle, a file string, and a new dictionary name string as 
       input to save a dictionary not included in the registry.
  */
  void  FRAMES2API _F2SaveDictionaryAs(int _PID,long _dictionaryHandle,char* _dictionaryFile,char* _dictionaryNewName);
    #ifndef BUILD_DLL
      #define F2SaveDictionaryAs _F2SaveDictionaryAs
    #endif

  /*============================================================================
     Documentation for: F2GetDictionaryCount
    ============================================================================
       Get the count of all dictionaries in registry. The DictionaryCount 
       function call is made with a process id and a dictionary name string 
       as input, and a count integer to output a count of all dictionaries 
       in the registry.
  */
  int  FRAMES2API _F2GetDictionaryCount(int _PID);
    #ifndef BUILD_DLL
      #define F2GetDictionaryCount _F2GetDictionaryCount
    #endif

  /*============================================================================
     Documentation for: F2GetDictionaryName
    ============================================================================
       Get the name of a dictionary given the dictionary handle.
  */
  int  FRAMES2API _F2GetDictionaryName(int _PID,long _dictionaryHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetDictionaryName _F2GetDictionaryName
    #endif

  /*============================================================================
     Documentation for: F2GetDictionaryDescription
    ============================================================================
       Get the description of a dictionary given the dictionary handle.
  */
  int  FRAMES2API _F2GetDictionaryDescription(int _PID,long _dictionaryHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetDictionaryDescription _F2GetDictionaryDescription
    #endif

  /*============================================================================
     Documentation for: F2SetDictionaryDescription
    ============================================================================
       Set the description of a dictionary given the dictionary handle.
  */
  void  FRAMES2API _F2SetDictionaryDescription(int _PID,long _dictionaryHandle,char* _description);
    #ifndef BUILD_DLL
      #define F2SetDictionaryDescription _F2SetDictionaryDescription
    #endif

  /*============================================================================
     Documentation for: F2GetDictionaryPrivilege
    ============================================================================
       Get the privilege of a dictionary given the dictionary handle. The 
       privilege is either 0=Module UI, or 1=Boundary Condition. This 
       represents the level of review a dictionary has received. A system 
       dictionary should have a number of individuals agree that it is 
       complete, useful and mutually exclusive of other system dictionaries. 
       Returns the privilege of the dictionary
  */
  int  FRAMES2API _F2GetDictionaryPrivilege(int _PID,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define F2GetDictionaryPrivilege _F2GetDictionaryPrivilege
    #endif

  /*============================================================================
     Documentation for: F2SetDictionaryPrivilege
    ============================================================================
       Set the privilege of a dictionary given the dictionary handle.
  */
  void  FRAMES2API _F2SetDictionaryPrivilege(int _PID,long _dictionaryHandle,int _privilege);
    #ifndef BUILD_DLL
      #define F2SetDictionaryPrivilege _F2SetDictionaryPrivilege
    #endif

  /*============================================================================
     Documentation for: F2GetDictionaryVersion
    ============================================================================
       Get the version of a dictionary given the dictionary handle. Returns 
       the version of the dictionary
  */
  int  FRAMES2API _F2GetDictionaryVersion(int _PID,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define F2GetDictionaryVersion _F2GetDictionaryVersion
    #endif

  /*============================================================================
     Documentation for: F2SetDictionaryVersion
    ============================================================================
       Set the version of a dictionary given the dictionary handle.
  */
  void  FRAMES2API _F2SetDictionaryVersion(int _PID,long _dictionaryHandle,int _version);
    #ifndef BUILD_DLL
      #define F2SetDictionaryVersion _F2SetDictionaryVersion
    #endif

  /*============================================================================
     Documentation for: F2GetDictionaryPath
    ============================================================================
       Get the path associated with a dictionary given the dictionary 
       handle. To change the path you need to 1) SaveDictionaryAs changing 
       the path, 2) DelDictionary, and 3) AddOpenDictionary from it new 
       location. Returns the path to the dictionary.
  */
  int  FRAMES2API _F2GetDictionaryPath(int _PID,long _dictionaryHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetDictionaryPath _F2GetDictionaryPath
    #endif

  /*============================================================================
     Documentation for: F2GetDictionaryUpdate
    ============================================================================
       If the state of a dictionary has changed then update flag will be 
       non-zero. Returns the update flag of the dictionary.
  */
  int  FRAMES2API _F2GetDictionaryUpdate(int _PID,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define F2GetDictionaryUpdate _F2GetDictionaryUpdate
    #endif

  /*============================================================================
     Documentation for: F2AddVariable
    ============================================================================
       The AddVariable function call is made with a process id, a dictionary 
       name string, and a variable string as input to add a variable to a 
       dictionary. The function returns success/fail flag. This function 
       must be called to allocate new space for a parameter. The parameter 
       will be added to the dictionary or dataset by _dicId with the given 
       _name. The parameter will have the default settings of:  dimension = 
       0; description = ""; type = integer; stochastic = false; min = 0; max 
       = 0.
  */
  long  FRAMES2API _F2AddVariable(int _PID,long _dictionaryHandle,char* _variableName);
    #ifndef BUILD_DLL
      #define F2AddVariable _F2AddVariable
    #endif

  /*============================================================================
     Documentation for: F2DeleteVariable
    ============================================================================
       Delete a variable from a dictionary. Warning - Deleting a variable, 
       dictionary, or a dataset can have some serious side effects therefore 
       the software will not allow the use any these delete functions when 
       more than one handle to a simulation exists. (i.e. A module and the 
       system are active, or two system level users are active on a single 
       instance of FRAMES. The DelVariable function call is made with a 
       process id, a dictionary name string, and a variable string as input 
       to delete a variable from a dictionary. The function returns 
       success/fail flag.
  */
  void  FRAMES2API _F2DeleteVariable(int _PID,long _dictionaryHandle,long _variableHandle);
    #ifndef BUILD_DLL
      #define F2DeleteVariable _F2DeleteVariable
    #endif

  /*============================================================================
     Documentation for: F2GetVariableCount
    ============================================================================
       Gets the varible count of a dictionary given the dictionary handle. 
       The GetVarCount function call is made with a process id, a dictionary 
       name string, and a count integer to output a count of all variables 
       in the dictionary. Returns the count of the variables in the indexed 
       dictionary.
  */
  int  FRAMES2API _F2GetVariableCount(int _PID,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define F2GetVariableCount _F2GetVariableCount
    #endif

  /*============================================================================
     Documentation for: F2GetVariableName
    ============================================================================
       Gets the name of the nth (index) variable in the dictionary. Returns 
       the string variable name for the given index.
  */
  int  FRAMES2API _F2GetVariableName(int _PID,long _dictionaryHandle,int _variableIndex,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetVariableName _F2GetVariableName
    #endif

  /*============================================================================
     Documentation for: F2SetVariableName
    ============================================================================
       Set the variable name (rename).The SetVarName function call is made 
       with a process id, a dictionary name string, an existing variable 
       name string, and a variable name string as input to change the 
       variable name.
  */
  void  FRAMES2API _F2SetVariableName(int _PID,long _dictionaryHandle,long _variableHandle,char* _variableNewName);
    #ifndef BUILD_DLL
      #define F2SetVariableName _F2SetVariableName
    #endif

  /*============================================================================
     Documentation for: F2SetVariableDescription
    ============================================================================
       Set the variable's description. The SetVarDescription function call 
       is made with a process id, a dictionary name string, a variable name 
       string, and a description string as input to change the description 
       of the variable.
  */
  void  FRAMES2API _F2SetVariableDescription(int _PID,long _dictionaryHandle,long _variableHandle,char* _description);
    #ifndef BUILD_DLL
      #define F2SetVariableDescription _F2SetVariableDescription
    #endif

  /*============================================================================
     Documentation for: F2SetVariableType
    ============================================================================
       Set the variable's type. Type can be either "String", "Logical", 
       "Real", or "Integer". The SetVarType function call is made with a 
       process id, a dictionary name string, a variable name string, and a 
       type string as input to change the type parameter of the variable.
  */
  void  FRAMES2API _F2SetVariableType(int _PID,long _dictionaryHandle,long _variableHandle,char* _type);
    #ifndef BUILD_DLL
      #define F2SetVariableType _F2SetVariableType
    #endif

  /*============================================================================
     Documentation for: F2SetVariableScalar
    ============================================================================
       Set the variable's scalar flag. If the flag is 0 then a set of values 
       is stored. (i.e. the dimensionality will be increased by 1). If the 
       flag=1 a single value is stored.
  */
  void  FRAMES2API _F2SetVariableScalar(int _PID,long _dictionaryHandle,long _variableHandle,int _flag);
    #ifndef BUILD_DLL
      #define F2SetVariableScalar _F2SetVariableScalar
    #endif

  /*============================================================================
     Documentation for: F2SetVariableMinimum
    ============================================================================
       Set the variable's minimum value. Setting both minimum and maximum 
       equal to each other has the effect of having the variable NOT range 
       checked.
  */
  void  FRAMES2API _F2SetVariableMinimum(int _PID,long _dictionaryHandle,long _variableHandle,double _min);
    #ifndef BUILD_DLL
      #define F2SetVariableMinimum _F2SetVariableMinimum
    #endif

  /*============================================================================
     Documentation for: F2SetVariableMaximum
    ============================================================================
       Set the variable's maximum value. Setting both minimum and maximum 
       equal to each other has the effect of having the variable NOT range 
       checked.
  */
  void  FRAMES2API _F2SetVariableMaximum(int _PID,long _dictionaryHandle,long _variableHandle,double _max);
    #ifndef BUILD_DLL
      #define F2SetVariableMaximum _F2SetVariableMaximum
    #endif

  /*============================================================================
     Documentation for: F2SetVariableMeasure
    ============================================================================
       Set the variable's measure.
  */
  void  FRAMES2API _F2SetVariableMeasure(int _PID,long _dictionaryHandle,long _variableHandle,char* _measureName);
    #ifndef BUILD_DLL
      #define F2SetVariableMeasure _F2SetVariableMeasure
    #endif

  /*============================================================================
     Documentation for: F2SetVariableUnit
    ============================================================================
       Set the variable's unit for its given measure.
  */
  void  FRAMES2API _F2SetVariableUnit(int _PID,long _dictionaryHandle,long _variableHandle,char* _unitName);
    #ifndef BUILD_DLL
      #define F2SetVariableUnit _F2SetVariableUnit
    #endif

  /*============================================================================
     Documentation for: F2SetVariableStochastic
    ============================================================================
       Set the variable's stochastic flag. This flag signifies if the 
       parameter can be modified by an another program and not invalidate 
       any calibration. This allows other programs such and 
       Sensativity/Uncertainty or Parameter estimation programs to modify 
       these parameters.
  */
  void  FRAMES2API _F2SetVariableStochastic(int _PID,long _dictionaryHandle,long _variableHandle,int _flag);
    #ifndef BUILD_DLL
      #define F2SetVariableStochastic _F2SetVariableStochastic
    #endif

  /*============================================================================
     Documentation for: F2SetVariablePreposition
    ============================================================================
       Set the preposition, this helps in describing parameters in a human 
       readable form. This is the word that would best be used to write a 
       description of this parameters as an index for another. For example 
       the chemical parameter would use "for" as a preposition to make 
       statements such as Concentration for benzene. The preposition greatly 
       facilitates writing descriptive text associated with results. The 
       SetVarPreposition function call is made with a process id, a 
       dictionary name string, a variable name string, and a prep string as 
       input to change the preposition parameter of the variable.
  */
  void  FRAMES2API _F2SetVariablePreposition(int _PID,long _dictionaryHandle,long _variableHandle,char* _prepositionName);
    #ifndef BUILD_DLL
      #define F2SetVariablePreposition _F2SetVariablePreposition
    #endif

  /*============================================================================
     Documentation for: F2SetVariablePrimaryKey
    ============================================================================
       Set the variable primarykey flag. Is this parameter a key to 
       information on other parameters. This is used in both databases and 
       in the representation of the data. For example a variable being used 
       to index other variables frequently (like chemical) would indicate 
       that chemical should be a primary key. The typical meaning of primary 
       key from Database design is not incorrect when used here.
  */
  void  FRAMES2API _F2SetVariablePrimaryKey(int _PID,long _dictionaryHandle,long _variableHandle,int _flag);
    #ifndef BUILD_DLL
      #define F2SetVariablePrimaryKey _F2SetVariablePrimaryKey
    #endif

  /*============================================================================
     Documentation for: F2AddVariableIndex
    ============================================================================
       Add a variable index or label index to a variable. The AddVarIndex 
       function call is made with a process id, a dictionary name string, a 
       variable name string, a dictionary reference string, and an index 
       string as input to add a variable index or label index to a variable.
  */
  void  FRAMES2API _F2AddVariableIndex(int _PID,long _dictionaryHandle,long _variableHandle,char* _dictionaryName,char* _variableName);
    #ifndef BUILD_DLL
      #define F2AddVariableIndex _F2AddVariableIndex
    #endif

  /*============================================================================
     Documentation for: F2DeleteVariableIndex
    ============================================================================
       Delete an index from a variable. The DelVarIndex function call is 
       made with a process id, a dictionary name string, a variable name 
       string, and an index string as input to delete a variable index or 
       label index to a variable.
  */
  void  FRAMES2API _F2DeleteVariableIndex(int _PID,long _dictionaryHandle,long _variableHandle,int _indexIndex);
    #ifndef BUILD_DLL
      #define F2DeleteVariableIndex _F2DeleteVariableIndex
    #endif

  /*============================================================================
     Documentation for: F2GetVariableIndexCount
    ============================================================================
       Get the count of indices for a variable. The GetVarIndexCount 
       function call is made with a process id, a dictionary name string, 
       and a variable name string as input, and an empty count integer to 
       output a count of indices for a variable. Returns the count of the 
       indexes for the variable.
  */
  int  FRAMES2API _F2GetVariableIndexCount(int _PID,long _dictionaryHandle,long _variableHandle);
    #ifndef BUILD_DLL
      #define F2GetVariableIndexCount _F2GetVariableIndexCount
    #endif

  /*============================================================================
     Documentation for: F2GetVariableIndexIndex
    ============================================================================
       Get the index number for a index of a variable. It returns the index 
       of a particular dictionary and variable.
  */
  int  FRAMES2API _F2GetVariableIndexIndex(int _PID,long _dictionaryHandle,long _variableHandle,char* _dictionaryName,char* _variableName);
    #ifndef BUILD_DLL
      #define F2GetVariableIndexIndex _F2GetVariableIndexIndex
    #endif

  /*============================================================================
     Documentation for: F2GetVariableIndexDictionary
    ============================================================================
       Get the index dictionary name of indices for a variable.
  */
  int  FRAMES2API _F2GetVariableIndexDictionary(int _PID,long _dictionaryHandle,long _variableHandle,int _indexIndex,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetVariableIndexDictionary _F2GetVariableIndexDictionary
    #endif

  /*============================================================================
     Documentation for: F2GetVariableIndexVariable
    ============================================================================
       Get the index variable name of indices for a variable.
  */
  int  FRAMES2API _F2GetVariableIndexVariable(int _PID,long _dictionaryHandle,long _variableHandle,int _indexIndex,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetVariableIndexVariable _F2GetVariableIndexVariable
    #endif

  /*============================================================================
     Documentation for: F2PromoteVariableIndex
    ============================================================================
       Move and index up one level in the order of referred indices.
  */
  void  FRAMES2API _F2PromoteVariableIndex(int _PID,long _dictionaryHandle,long _variableHandle,int _indexIndex);
    #ifndef BUILD_DLL
      #define F2PromoteVariableIndex _F2PromoteVariableIndex
    #endif

  /*============================================================================
     Documentation for: F2DemoteVariableIndex
    ============================================================================
       Move and index down one level in the order of referred indices.
  */
  void  FRAMES2API _F2DemoteVariableIndex(int _PID,long _dictionaryHandle,long _variableHandle,int _indexIndex);
    #ifndef BUILD_DLL
      #define F2DemoteVariableIndex _F2DemoteVariableIndex
    #endif

  /*============================================================================
     Documentation for: F2NewModule
    ============================================================================
       Create and add a module description in registry module name passed 
       in. The AddNewModule function call is made with a process id, a fully 
       qualified file string, and a module name string as input to create 
       and add a module description in the registry. It is expected that 
       .mod file does not exist or is empty. Returns the index of the new 
       module.
  */
  long  FRAMES2API _F2NewModule(int _PID,char* _moduleFile,char* _moduleName);
    #ifndef BUILD_DLL
      #define F2NewModule _F2NewModule
    #endif

  /*============================================================================
     Documentation for: F2OpenModule
    ============================================================================
       Open and add a module description in registry module name passed out. 
       The AddOpenModule function call is made with a process id and a fully 
       qualified file string as input to open and add a module description 
       in the registry, and a module name string to output the module name. 
       Returns the index of the registered module.
  */
  long  FRAMES2API _F2OpenModule(int _PID,char* _moduleFile);
    #ifndef BUILD_DLL
      #define F2OpenModule _F2OpenModule
    #endif

  /*============================================================================
     Documentation for: F2GetModuleName
    ============================================================================
       Returns name of module.
  */
  int  FRAMES2API _F2GetModuleName(int _PID,long _moduleHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetModuleName _F2GetModuleName
    #endif

  /*============================================================================
     Documentation for: F2DeleteModule
    ============================================================================
       Delete a module description dataset from registry. The DelModule 
       function call is made with a process id and a module name string as 
       input to delete a module description dataset from the registry.
  */
  void  FRAMES2API _F2DeleteModule(int _PID,long _moduleHandle);
    #ifndef BUILD_DLL
      #define F2DeleteModule _F2DeleteModule
    #endif

  /*============================================================================
     Documentation for: F2SaveModule
    ============================================================================
       Save the module description dataset to registry. The SaveModule 
       function call is made with a process id and a module name string as 
       input to save a module description dataset in the registry.
  */
  void  FRAMES2API _F2SaveModule(int _PID,long _moduleHandle);
    #ifndef BUILD_DLL
      #define F2SaveModule _F2SaveModule
    #endif

  /*============================================================================
     Documentation for: F2SaveModuleAs
    ============================================================================
       Save the module description as another dataset to registry. The 
       SaveModuleAs function call is made with a process id, a handle 
       module, a fully qualified file string, and a new module name string 
       as input to save the module description as another module dataset in 
       the registry. The old module handle continues to be the open module.
  */
  void  FRAMES2API _F2SaveModuleAs(int _PID,long _moduleHandle,char* _moduleFile,char* _moduleNewName);
    #ifndef BUILD_DLL
      #define F2SaveModuleAs _F2SaveModuleAs
    #endif

  /*============================================================================
     Documentation for: F2GetModuleCount
    ============================================================================
       Get the count of all modules in registry. Returns the number of 
       registered modules.
  */
  int  FRAMES2API _F2GetModuleCount(int _PID);
    #ifndef BUILD_DLL
      #define F2GetModuleCount _F2GetModuleCount
    #endif

  /*============================================================================
     Documentation for: F2GetModulePath
    ============================================================================
       Get module path associated with a module. Returns the fully qualified 
       file of the module.
  */
  int  FRAMES2API _F2GetModulePath(int _PID,long _moduleHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetModulePath _F2GetModulePath
    #endif

  /*============================================================================
     Documentation for: F2AddScheme
    ============================================================================
       Add a scheme to a module. Returns the index of the new scheme.
  */
  long  FRAMES2API _F2AddScheme(int _PID,long _moduleHandle,char* _schemeName);
    #ifndef BUILD_DLL
      #define F2AddScheme _F2AddScheme
    #endif

  /*============================================================================
     Documentation for: F2DeleteScheme
    ============================================================================
       Delete a scheme from a module.
  */
  void  FRAMES2API _F2DeleteScheme(int _PID,long _moduleHandle,long _schemeHandle);
    #ifndef BUILD_DLL
      #define F2DeleteScheme _F2DeleteScheme
    #endif

  /*============================================================================
     Documentation for: F2GetSchemeCount
    ============================================================================
       Get the number of the schemes for a module. Returns number of schemes 
       for a module.
  */
  int  FRAMES2API _F2GetSchemeCount(int _PID,long _moduleHandle);
    #ifndef BUILD_DLL
      #define F2GetSchemeCount _F2GetSchemeCount
    #endif

  /*============================================================================
     Documentation for: F2GetSchemeName
    ============================================================================
       Gets the name of the nth (index) scheme of the module. Returns the 
       string scheme name for the given index.
  */
  int  FRAMES2API _F2GetSchemeName(int _PID,long _moduleHandle,long _schemeHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetSchemeName _F2GetSchemeName
    #endif

  /*============================================================================
     Documentation for: F2AddSchemeInputDictionary
    ============================================================================
       Add a dictionary to a scheme for a module. Returns the index of the 
       newly added dictionary to the scheme.
  */
  void  FRAMES2API _F2AddSchemeInputDictionary(int _PID,long _moduleHandle,long _schemeHandle,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define F2AddSchemeInputDictionary _F2AddSchemeInputDictionary
    #endif

  /*============================================================================
     Documentation for: F2DeleteSchemeInputDictionary
    ============================================================================
       Delete a dictionary from a scheme for a module.
  */
  void  FRAMES2API _F2DeleteSchemeInputDictionary(int _PID,long _moduleHandle,long _schemeHandle,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define F2DeleteSchemeInputDictionary _F2DeleteSchemeInputDictionary
    #endif

  /*============================================================================
     Documentation for: F2AddSchemeOutputDictionary
    ============================================================================
       Add an output dictionary to a scheme for a module. Returns the index 
       of the newly added output dictionary.
  */
  void  FRAMES2API _F2AddSchemeOutputDictionary(int _PID,long _moduleHandle,long _schemeHandle,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define F2AddSchemeOutputDictionary _F2AddSchemeOutputDictionary
    #endif

  /*============================================================================
     Documentation for: F2DeleteSchemeOutputDictionary
    ============================================================================
       Delete an output dictionary from a scheme for a module.
  */
  void  FRAMES2API _F2DeleteSchemeOutputDictionary(int _PID,long _moduleHandle,long _schemeHandle,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define F2DeleteSchemeOutputDictionary _F2DeleteSchemeOutputDictionary
    #endif

  /*============================================================================
     Documentation for: F2GetModuleVariableCount
    ============================================================================
       Get the number of variables in a modueles dataset..
  */
  int  FRAMES2API _F2GetModuleVariableCount(int _PID,long _moduleHandle);
    #ifndef BUILD_DLL
      #define F2GetModuleVariableCount _F2GetModuleVariableCount
    #endif

  /*============================================================================
     Documentation for: F2GetModuleVariableInteger
    ============================================================================
       Get an integer from modules variable data. Returns the integer that 
       was retrieved.
  */
  int  FRAMES2API _F2GetModuleVariableInteger(int _PID,long _moduleHandle,long _variableHandle,char* _unitName,int _indices[]);
    #ifndef BUILD_DLL
      #define F2GetModuleVariableInteger _F2GetModuleVariableInteger
    #endif

  /*============================================================================
     Documentation for: F2GetModuleVariableDouble
    ============================================================================
       Get an real from modules variable data. Returns the real that was 
       retrieved.
  */
  double  FRAMES2API _F2GetModuleVariableDouble(int _PID,long _moduleHandle,long _variableHandle,char* _unitName,int _indices[]);
    #ifndef BUILD_DLL
      #define F2GetModuleVariableDouble _F2GetModuleVariableDouble
    #endif

  /*============================================================================
     Documentation for: F2GetModuleVariableLogical
    ============================================================================
       Get an logical from modules variable data. Returns the logical that 
       was retrieved.
  */
  int  FRAMES2API _F2GetModuleVariableLogical(int _PID,long _moduleHandle,long _variableHandle,char* _unitName,int _indices[]);
    #ifndef BUILD_DLL
      #define F2GetModuleVariableLogical _F2GetModuleVariableLogical
    #endif

  /*============================================================================
     Documentation for: F2GetModuleVariableString
    ============================================================================
       Get an string from modules variable data. Returns the string that was 
       retrieved.
  */
  int  FRAMES2API _F2GetModuleVariableString(int _PID,long _moduleHandle,long _variableHandle,char* _unitName,int _indices[],char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetModuleVariableString _F2GetModuleVariableString
    #endif

  /*============================================================================
     Documentation for: F2SetModuleVariableInteger
    ============================================================================
       Put an integer of meta data about a module.
  */
  void  FRAMES2API _F2SetModuleVariableInteger(int _PID,long _moduleHandle,long _variableHandle,char* _unitName,int _indices[],int _value);
    #ifndef BUILD_DLL
      #define F2SetModuleVariableInteger _F2SetModuleVariableInteger
    #endif

  /*============================================================================
     Documentation for: F2SetModuleVariableDouble
    ============================================================================
       Put a double of meta data about a module.
  */
  void  FRAMES2API _F2SetModuleVariableDouble(int _PID,long _moduleHandle,long _variableHandle,char* _unitName,int _indices[],double _value);
    #ifndef BUILD_DLL
      #define F2SetModuleVariableDouble _F2SetModuleVariableDouble
    #endif

  /*============================================================================
     Documentation for: F2SetModuleVariableLogical
    ============================================================================
       Put an logical of meta data about a module.
  */
  void  FRAMES2API _F2SetModuleVariableLogical(int _PID,long _moduleHandle,long _variableHandle,char* _unitName,int _indices[],int _value);
    #ifndef BUILD_DLL
      #define F2SetModuleVariableLogical _F2SetModuleVariableLogical
    #endif

  /*============================================================================
     Documentation for: F2SetModuleVariableString
    ============================================================================
       Put a string of meta data about a module.
  */
  void  FRAMES2API _F2SetModuleVariableString(int _PID,long _moduleHandle,long _variableHandle,char* _unitName,int _indices[],char* _value);
    #ifndef BUILD_DLL
      #define F2SetModuleVariableString _F2SetModuleVariableString
    #endif

  /*============================================================================
     Documentation for: F2AddDomain
    ============================================================================
       Automatically adds the four class types to the domain. The AddDomain 
       function call is made with a process id and a domain name string as 
       input to add a domain; automatically adds the four class types to the 
       domain. Returns the index of the added Domain.
  */
  long  FRAMES2API _F2AddDomain(int _PID,char* _domainName);
    #ifndef BUILD_DLL
      #define F2AddDomain _F2AddDomain
    #endif

  /*============================================================================
     Documentation for: F2DeleteDomain
    ============================================================================
       Deletes the domain and it tree. The DelDomain function call is made 
       with a process id and a domain name string as input to delete a 
       domain and its tree, deleting its classes, groups, subgroups, and 
       modules.
  */
  void  FRAMES2API _F2DeleteDomain(int _PID,long _domainHandle);
    #ifndef BUILD_DLL
      #define F2DeleteDomain _F2DeleteDomain
    #endif

  /*============================================================================
     Documentation for: F2GetDomainIcon
    ============================================================================
       Get the icon associated with a domain. Icon are either .bmp, .gif 
       files for image. They should be small 32x32 pixels. Return the fully 
       qualified file for a module icon.
  */
  int  FRAMES2API _F2GetDomainIcon(int _PID,long _domainHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetDomainIcon _F2GetDomainIcon
    #endif

  /*============================================================================
     Documentation for: F2SetDomainIcon
    ============================================================================
       Set the icon associated with a domain. Icon are either .bmp, .gif 
       files for image. They should be small 32x32 pixels.
  */
  void  FRAMES2API _F2SetDomainIcon(int _PID,long _domainHandle,char* _domainIconFile);
    #ifndef BUILD_DLL
      #define F2SetDomainIcon _F2SetDomainIcon
    #endif

  /*============================================================================
     Documentation for: F2AddClass
    ============================================================================
       For future use - not presently active. The AddClass function call is 
       made with a process id, a domain name string, and a class name string 
       as input to add a class. Returns the index of the newly added class.
  */
  long  FRAMES2API _F2AddClass(int _PID,long _domainHandle,char* _className);
    #ifndef BUILD_DLL
      #define F2AddClass _F2AddClass
    #endif

  /*============================================================================
     Documentation for: F2DeleteClass
    ============================================================================
       For future use - not presently active. The DelClass function call is 
       made with a process id, a domain name string, and a class name string 
       as input to delete a class from a domain, including its groups, 
       subgroups, and modules.
  */
  void  FRAMES2API _F2DeleteClass(int _PID,long _domainHandle,long _classHandle);
    #ifndef BUILD_DLL
      #define F2DeleteClass _F2DeleteClass
    #endif

  /*============================================================================
     Documentation for: F2GetClassIcon
    ============================================================================
       Get the icon associated with a class in a domain. Icon are either 
       .bmp, .gif files for image. They should be small 32x32 pixels. 
       Returns the fully qualified file to the class icon.
  */
  int  FRAMES2API _F2GetClassIcon(int _PID,long _domainHandle,long _classHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetClassIcon _F2GetClassIcon
    #endif

  /*============================================================================
     Documentation for: F2SetClassIcon
    ============================================================================
       Set the icon associated with a class in a domain. Icon are either 
       .bmp, .gif files for image. They should be small 32x32 pixels.
  */
  void  FRAMES2API _F2SetClassIcon(int _PID,long _domainHandle,long _classHandle,char* _classIconFile);
    #ifndef BUILD_DLL
      #define F2SetClassIcon _F2SetClassIcon
    #endif

  /*============================================================================
     Documentation for: F2AddGroup
    ============================================================================
       The AddGroup function call is made with a process id, a domain name 
       string,  a class name string, and a group name string as input to add 
       a group to a domain and class. Returns the index of the newly added 
       group.
  */
  long  FRAMES2API _F2AddGroup(int _PID,long _domainHandle,long _classHandle,char* _groupName);
    #ifndef BUILD_DLL
      #define F2AddGroup _F2AddGroup
    #endif

  /*============================================================================
     Documentation for: F2DeleteGroup
    ============================================================================
       The DelGroup function call is made with a process id, a domain name 
       string,  a class name string, and a group name string as input to 
       delete a group from a domain and class, including its subgroups and 
       modules.
  */
  void  FRAMES2API _F2DeleteGroup(int _PID,long _domainHandle,long _classHandle,long _groupHandle);
    #ifndef BUILD_DLL
      #define F2DeleteGroup _F2DeleteGroup
    #endif

  /*============================================================================
     Documentation for: F2GetGroupIcon
    ============================================================================
       Get the icon associated with a class in a domain. Icon are either 
       .bmp, .gif files for image. They should be small 32x32 pixels. 
       Returns the fully qualified file to the group icon.
  */
  int  FRAMES2API _F2GetGroupIcon(int _PID,long _domainHandle,long _classHandle,long _groupHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetGroupIcon _F2GetGroupIcon
    #endif

  /*============================================================================
     Documentation for: F2SetGroupIcon
    ============================================================================
       Set the icon associated with a class in a domain. Icon are either 
       .bmp, .gif files for image. They should be small 32x32 pixels.
  */
  void  FRAMES2API _F2SetGroupIcon(int _PID,long _domainHandle,long _classHandle,long _groupHandle,char* _groupIconFile);
    #ifndef BUILD_DLL
      #define F2SetGroupIcon _F2SetGroupIcon
    #endif

  /*============================================================================
     Documentation for: F2AddSubGroup
    ============================================================================
       The AddSubGroup function call is made with a process id, a domain 
       name string, a class name string, a group name string, and a subgroup 
       name string as input to add a subgroup to a domain, class, and group. 
       Return the index of the newly added subgroup.
  */
  long  FRAMES2API _F2AddSubGroup(int _PID,long _domainHandle,long _classHandle,long _groupHandle,char* _subgroupName);
    #ifndef BUILD_DLL
      #define F2AddSubGroup _F2AddSubGroup
    #endif

  /*============================================================================
     Documentation for: F2DeleteSubGroup
    ============================================================================
       The DelSubGroup function call is made with a process id, a domain 
       name string, a class name string, a group name string, and a subgroup 
       name string as input to delete a subgroup from a domain, class, and 
       group, including its modules.
  */
  void  FRAMES2API _F2DeleteSubGroup(int _PID,long _domainHandle,long _classHandle,long _groupHandle,long _subgroupHandle);
    #ifndef BUILD_DLL
      #define F2DeleteSubGroup _F2DeleteSubGroup
    #endif

  /*============================================================================
     Documentation for: F2GetSubGroupIcon
    ============================================================================
       Get the icon associated with a class in a domain. Icon are either 
       .bmp, .gif files for image. They should be small 32x32 pixels. 
       Returns the fully qualified file to the subgroup icon.
  */
  int  FRAMES2API _F2GetSubGroupIcon(int _PID,long _domainHandle,long _classHandle,long _groupHandle,long _subgroupHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetSubGroupIcon _F2GetSubGroupIcon
    #endif

  /*============================================================================
     Documentation for: F2SetSubGroupIcon
    ============================================================================
       Set the icon associated with a class in a domain. Icon are either 
       .bmp, .gif files for image. They should be small 32x32 pixels.
  */
  void  FRAMES2API _F2SetSubGroupIcon(int _PID,long _domainHandle,long _classHandle,long _groupHandle,long _subgroupHandle,char* _subgroupIconFile);
    #ifndef BUILD_DLL
      #define F2SetSubGroupIcon _F2SetSubGroupIcon
    #endif

  /*============================================================================
     Documentation for: F2AddGroupModule
    ============================================================================
       Add a module at the group level. The AddGroupModule function call is 
       made with a process id, a domain name string, a class name string, a 
       group name string, and a module name string as input to add a module 
       to a domain, class, and group. Returns the index of the newly added 
       module.
  */
  void  FRAMES2API _F2AddGroupModule(int _PID,long _domainHandle,long _classHandle,long _groupHandle,long _moduleHandle);
    #ifndef BUILD_DLL
      #define F2AddGroupModule _F2AddGroupModule
    #endif

  /*============================================================================
     Documentation for: F2DeleteGroupModule
    ============================================================================
       Remove a module at the group level. The DelGroupModule function call 
       is made with a process id, a domain name string, a class name string, 
       a group name string, and a module name string as input to delete a 
       module from a domain, class, and group.
  */
  void  FRAMES2API _F2DeleteGroupModule(int _PID,long _domainHandle,long _classHandle,long _groupHandle,long _moduleHandle);
    #ifndef BUILD_DLL
      #define F2DeleteGroupModule _F2DeleteGroupModule
    #endif

  /*============================================================================
     Documentation for: F2AddSubGroupModule
    ============================================================================
       Add a module at the subgroup level. The AddSubGrpModule function call 
       is made with a process id, a domain name string, a class name string, 
       a group name string, a subgroup name string, and a module name string 
       as input to add a module to a domain, class, group, and subgroup. 
       Returns the index of the newly added subgroup module.
  */
  void  FRAMES2API _F2AddSubGroupModule(int _PID,long _domainHandle,long _classHandle,long _groupHandle,long _subgroupHandle,long _moduleHandle);
    #ifndef BUILD_DLL
      #define F2AddSubGroupModule _F2AddSubGroupModule
    #endif

  /*============================================================================
     Documentation for: F2DeleteSubGroupModule
    ============================================================================
       Remove a module at the subgroup level. The DelSubGrpModule function 
       call is made with a process id, a domain name string, a class name 
       string, a group name string, a subgroup name string, and module name 
       string as input to delete a module from a domain, class, group, and 
       subgroup.
  */
  void  FRAMES2API _F2DeleteSubGroupModule(int _PID,long _domainHandle,long _classHandle,long _groupHandle,long _subgroupHandle,long _moduleHandle);
    #ifndef BUILD_DLL
      #define F2DeleteSubGroupModule _F2DeleteSubGroupModule
    #endif

  /*============================================================================
     Documentation for: F2GetDomainName
    ============================================================================
       Gets the domain name.
  */
  int  FRAMES2API _F2GetDomainName(int _PID,long _domainHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetDomainName _F2GetDomainName
    #endif

  /*============================================================================
     Documentation for: F2GetClassName
    ============================================================================
       Gets the class name.
  */
  int  FRAMES2API _F2GetClassName(int _PID,long _domainHandle,long _classHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetClassName _F2GetClassName
    #endif

  /*============================================================================
     Documentation for: F2GetGroupName
    ============================================================================
       Gets the group name.
  */
  int  FRAMES2API _F2GetGroupName(int _PID,long _domainHandle,long _classHandle,long _groupHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetGroupName _F2GetGroupName
    #endif

  /*============================================================================
     Documentation for: F2GetSubGroupName
    ============================================================================
       Gets the sub group name.
  */
  int  FRAMES2API _F2GetSubGroupName(int _PID,long _domainHandle,long _classHandle,long _groupHandle,long _subgroupHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetSubGroupName _F2GetSubGroupName
    #endif

  /*============================================================================
     Documentation for: F2GetDomainCount
    ============================================================================
       Get the number of domains in the registery. The GetDomainCount 
       function call is made with a process id as input, and an empty count 
       integer to output a count of all domains in the registry. Returns the 
       number of the domains that are registered.
  */
  int  FRAMES2API _F2GetDomainCount(int _PID);
    #ifndef BUILD_DLL
      #define F2GetDomainCount _F2GetDomainCount
    #endif

  /*============================================================================
     Documentation for: F2GetClassCount
    ============================================================================
       Get the number of classes in a domain. The GetClassCount function 
       call is made with a process id and a domain name string as input, and 
       an empty count integer to output a count of all classes in a domain. 
       Returns the number of classes in a domain.
  */
  int  FRAMES2API _F2GetClassCount(int _PID,long _domainHandle);
    #ifndef BUILD_DLL
      #define F2GetClassCount _F2GetClassCount
    #endif

  /*============================================================================
     Documentation for: F2GetGroupCount
    ============================================================================
       Get the number of groups in a class, and domain. The GetGroupCount 
       function call is made with a process id, a domain name string, and a 
       class name string as input, and an empty count integer to output a 
       count of all groups in a domain and class. Returns the number of 
       groups in a class in a domain.
  */
  int  FRAMES2API _F2GetGroupCount(int _PID,long _domainHandle,long _classHandle);
    #ifndef BUILD_DLL
      #define F2GetGroupCount _F2GetGroupCount
    #endif

  /*============================================================================
     Documentation for: F2GetSubGroupCount
    ============================================================================
       Get the number of subgroups in a group, class and domain.The 
       GetSubGrpCount function call is made with a process id, a domain name 
       string, a class name string, and a group name string as input, and an 
       empty count integer to output a count of all subgroups in a domain, 
       class, and group. Returns the number of subgroups in a group in a 
       class in a domain.
  */
  int  FRAMES2API _F2GetSubGroupCount(int _PID,long _domainHandle,long _classHandle,long _groupHandle);
    #ifndef BUILD_DLL
      #define F2GetSubGroupCount _F2GetSubGroupCount
    #endif

  /*============================================================================
     Documentation for: F2GetGroupModuleCount
    ============================================================================
       Get the number of modules at the group level for a group, class, and 
       domain. The GetGroupModCount function call is made with a process id, 
       a domain name string, a class name string, and a group name string as 
       input, and an empty count integer to output a count of all modules in 
       a domain, class, and group. Returns the number of modules in a group.
  */
  int  FRAMES2API _F2GetGroupModuleCount(int _PID,long _domainHandle,long _classHandle,long _groupHandle);
    #ifndef BUILD_DLL
      #define F2GetGroupModuleCount _F2GetGroupModuleCount
    #endif

  /*============================================================================
     Documentation for: F2GetSubGroupModuleCount
    ============================================================================
       Get the number of modules at the subgroup level for a subgroup, 
       group, class, and domain. The GetSubGrpModCount function call is made 
       with a process id, a domain name string, a class name string, a group 
       name string, and a subgroup name string as input, and an empty count 
       integer to output a count of all modules in a domain, class, group, 
       and subgroup. Returns the number of modules in a subgroup.
  */
  int  FRAMES2API _F2GetSubGroupModuleCount(int _PID,long _domainHandle,long _classHandle,long _groupHandle,long _subgroupHandle);
    #ifndef BUILD_DLL
      #define F2GetSubGroupModuleCount _F2GetSubGroupModuleCount
    #endif

  /*============================================================================
     Documentation for: F2NewSimulation
    ============================================================================
       Open a new simulation return a simulation dataset name. The NewSim 
       function call is made with a process id and a filename string as 
       input to open a new simulation. Only one simulation is allowed to be 
       open at a time in a FRAMES session, however, multiple FRAMES sessions 
       may be open at the same time. Returns the index of the newly created 
       simulation file.
  */
  long  FRAMES2API _F2NewSimulation(int _PID,char* _simulationFile,char* _simulationName);
    #ifndef BUILD_DLL
      #define F2NewSimulation _F2NewSimulation
    #endif

  /*============================================================================
     Documentation for: F2OpenSimulation
    ============================================================================
       Open an existing simulation return a simulation dataset name. The 
       OpenSim function call is made with a process id and a filename string 
       as input to open an existing simulation. Returns the index of the 
       opened simulation file.
  */
  long  FRAMES2API _F2OpenSimulation(int _PID,char* _simulationFile);
    #ifndef BUILD_DLL
      #define F2OpenSimulation _F2OpenSimulation
    #endif

  /*============================================================================
     Documentation for: F2SaveSimulation
    ============================================================================
       Save the simulation present state. The SaveSim function call is made 
       with a process id as input to save the simulation's present state.
  */
  void  FRAMES2API _F2SaveSimulation(int _PID);
    #ifndef BUILD_DLL
      #define F2SaveSimulation _F2SaveSimulation
    #endif

  /*============================================================================
     Documentation for: F2SaveSimulationAs
    ============================================================================
       Open an exsisting simulation return a simId. The SaveSimAs function 
       call is made with a process id, a fully quailfied path and a filename 
       string (new name) as input to save a simulation (SDE) as another 
       name, but continue to work in same simulation name.
  */
  void  FRAMES2API _F2SaveSimulationAs(int _PID,char* _simulationFile,char* _simulationNewName);
    #ifndef BUILD_DLL
      #define F2SaveSimulationAs _F2SaveSimulationAs
    #endif

  /*============================================================================
     Documentation for: F2CloseSimulation
    ============================================================================
       Close the simulation without saving. The CloseSim function call is 
       made with a process id as input to close the simulation without 
       saving.
  */
  void  FRAMES2API _F2CloseSimulation(int _PID);
    #ifndef BUILD_DLL
      #define F2CloseSimulation _F2CloseSimulation
    #endif

  /*============================================================================
     Documentation for: F2GetSimulation
    ============================================================================
       Get current simulation name for which all simulation functions will 
       use for this PID. Returns the current simulation name.
  */
  int  FRAMES2API _F2GetSimulation(int _PID,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetSimulation _F2GetSimulation
    #endif

  /*============================================================================
     Documentation for: F2SetSimulation
    ============================================================================
       Set current simulation name for which all simulation functions will 
       use for this PID.
  */
  void  FRAMES2API _F2SetSimulation(int _PID,char* _simulationName);
    #ifndef BUILD_DLL
      #define F2SetSimulation _F2SetSimulation
    #endif

  /*============================================================================
     Documentation for: F2AddNewDataSet
    ============================================================================
       Create a new dataset using a registered dictionary. The AddNewDataSet 
       function call is made with a process id, a dictionary handle, a fully 
       qualified file string, and a set string as input to create a new 
       dataset using a registered dictionary. Returns the index of the new 
       dataset.
  */
  long  FRAMES2API _F2AddNewDataSet(int _PID,long _dictionaryHandle,char* _datasetFile,char* _datasetName);
    #ifndef BUILD_DLL
      #define F2AddNewDataSet _F2AddNewDataSet
    #endif

  /*============================================================================
     Documentation for: F2AddOpenDataSet
    ============================================================================
       Open a dataset, dataset file must exist or an error will occur. The 
       AddOpenDataSet function call is made with a process id, a fully 
       qualified file name string, and a set string as input to open an 
       existing dataset to the registry. Returns the index of the new 
       dataset.
  */
  long  FRAMES2API _F2AddOpenDataSet(int _PID,char* _datasetFile);
    #ifndef BUILD_DLL
      #define F2AddOpenDataSet _F2AddOpenDataSet
    #endif

  /*============================================================================
     Documentation for: F2DeleteDataSet
    ============================================================================
       Delete a dataset. The function call is made with a process id and a 
       set string as input to delete a dataset. Does not write the dataset 
       before deletion.
  */
  void  FRAMES2API _F2DeleteDataSet(int _PID,long _datasetHandle);
    #ifndef BUILD_DLL
      #define F2DeleteDataSet _F2DeleteDataSet
    #endif

  /*============================================================================
     Documentation for: F2SaveDataSet
    ============================================================================
       Save the dataset. The function call is made with a process id and a 
       set string as input to save a dataset.
  */
  void  FRAMES2API _F2SaveDataSet(int _PID,long _datasetHandle);
    #ifndef BUILD_DLL
      #define F2SaveDataSet _F2SaveDataSet
    #endif

  /*============================================================================
     Documentation for: F2SaveDataSetAs
    ============================================================================
       Save the dataset with a different name. The SaveDataSetAs function 
       call is made with a process id, a dataset handle, a fully qualified 
       file name string, and a new Set string as input to save a dataset 
       under a new name.
  */
  void  FRAMES2API _F2SaveDataSetAs(int _PID,long _datasetHandle,char* _datasetFile,char* _datasetNewName);
    #ifndef BUILD_DLL
      #define F2SaveDataSetAs _F2SaveDataSetAs
    #endif

  /*============================================================================
     Documentation for: F2GetDataSetName
    ============================================================================
       Get dataset name.
  */
  int  FRAMES2API _F2GetDataSetName(int _PID,long _datasetHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetDataSetName _F2GetDataSetName
    #endif

  /*============================================================================
     Documentation for: F2GetDataSetPath
    ============================================================================
       Get the path associated with a dictionary. To change the path you 
       need to 1) SaveDictionaryAs changing the path, 2) DelDictionary, and 
       3) AddOpenDictionary from it new location. Returns the fully 
       qualified file.
  */
  int  FRAMES2API _F2GetDataSetPath(int _PID,long _datasetHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetDataSetPath _F2GetDataSetPath
    #endif

  /*============================================================================
     Documentation for: F2AddLink
    ============================================================================
       The AddLink function call is made with a process id, a modIdFrom 
       string, and a modIdTo string as input to connect two modules, and 
       returns a success/fail flag. The arrow of the connection will point 
       from  -> to. Returns the link index of the new connection.
  */
  void  FRAMES2API _F2AddLink(int _PID,long _fromIconHandle,long _toIconHandle);
    #ifndef BUILD_DLL
      #define F2AddLink _F2AddLink
    #endif

  /*============================================================================
     Documentation for: F2DeleteLink
    ============================================================================
       The DelLink function call is made with a process id, a modIdFrom 
       string, and a modIdTo string as input to delete the connection 
       between two modules, and returns a success/fail flag.
  */
  void  FRAMES2API _F2DeleteLink(int _PID,long _fromIconHandle,long _toIconHandle);
    #ifndef BUILD_DLL
      #define F2DeleteLink _F2DeleteLink
    #endif

  /*============================================================================
     Documentation for: F2AddIcon
    ============================================================================
       Returns the module ID added to the simulation. If scope is 0 then the 
       icon is added to the global diagram. If scope is non zero it is added 
       to the local diagram. Returns the module index for the added module 
       instance (icon in the drawing space).
  */
  long  FRAMES2API _F2AddIcon(int _PID,int _scope,long _domainHandle,long _classHandle,long _groupHandle,long _subgroupHandle);
    #ifndef BUILD_DLL
      #define F2AddIcon _F2AddIcon
    #endif

  /*============================================================================
     Documentation for: F2DeleteIcon
    ============================================================================
       Deletes the module ID from the simulation. The function call is made 
       with a process id and a id string as input to delete the module ID 
       from the simulation.
  */
  void  FRAMES2API _F2DeleteIcon(int _PID,long _iconHandle);
    #ifndef BUILD_DLL
      #define F2DeleteIcon _F2DeleteIcon
    #endif

  /*============================================================================
     Documentation for: F2GetIconName
    ============================================================================
       Get icon name.
  */
  int  FRAMES2API _F2GetIconName(int _PID,long _iconHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetIconName _F2GetIconName
    #endif

  /*============================================================================
     Documentation for: F2GetIconUIDictionary
    ============================================================================
       Returns the dictionary index for the module icon returns.
  */
  int  FRAMES2API _F2GetIconUIDictionary(int _PID,long _iconHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetIconUIDictionary _F2GetIconUIDictionary
    #endif

  /*============================================================================
     Documentation for: F2GetIconUIDataSet
    ============================================================================
       Returns the dataset index for the module icon.
  */
  int  FRAMES2API _F2GetIconUIDataSet(int _PID,long _iconHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetIconUIDataSet _F2GetIconUIDataSet
    #endif

  /*============================================================================
     Documentation for: F2GetIconModule
    ============================================================================
       Gets module and scheme associated with an icon. A list of datasets is 
       returned in the list parameter. Returns the module for an icon.
  */
  int  FRAMES2API _F2GetIconModule(int _PID,long _iconHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetIconModule _F2GetIconModule
    #endif

  /*============================================================================
     Documentation for: F2GetIconScheme
    ============================================================================
       Gets module and scheme associated with an icon. A list of datasets is 
       returned in the list parameter. Returns the scheme for an icon.
  */
  int  FRAMES2API _F2GetIconScheme(int _PID,long _iconHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetIconScheme _F2GetIconScheme
    #endif

  /*============================================================================
     Documentation for: F2SetIconModule
    ============================================================================
       Sets the dataset names for the module and scheme associated with the 
       indicated icon. A subset of the datasets the icon consumes is given 
       in the list parameter.
  */
  void  FRAMES2API _F2SetIconModule(int _PID,long _iconHandle,long _moduleHandle,long _schemeHandle,char* _delimiter,char* _list);
    #ifndef BUILD_DLL
      #define F2SetIconModule _F2SetIconModule
    #endif

  /*============================================================================
     Documentation for: F2GetDictionaryHandle
    ============================================================================
       Get the number of the schemes for a module. Returns number of schemes 
       for a module.
  */
  long  FRAMES2API _F2GetDictionaryHandle(int _PID,char* _dictionaryName);
    #ifndef BUILD_DLL
      #define F2GetDictionaryHandle _F2GetDictionaryHandle
    #endif

  /*============================================================================
     Documentation for: F2GetVariableHandle
    ============================================================================
       Finds the index of a variable in a dictionary.
  */
  long  FRAMES2API _F2GetVariableHandle(int _PID,long _dictionaryHandle,char* _variableName);
    #ifndef BUILD_DLL
      #define F2GetVariableHandle _F2GetVariableHandle
    #endif

  /*============================================================================
     Documentation for: F2GetModuleHandle
    ============================================================================
       Finds the index of a currently registered module.
  */
  long  FRAMES2API _F2GetModuleHandle(int _PID,char* _moduleName);
    #ifndef BUILD_DLL
      #define F2GetModuleHandle _F2GetModuleHandle
    #endif

  /*============================================================================
     Documentation for: F2GetSchemeHandle
    ============================================================================
       Get the number of the schemes for a module. Returns number of schemes 
       for a module.
  */
  long  FRAMES2API _F2GetSchemeHandle(int _PID,long _moduleHandle,char* _schemeName);
    #ifndef BUILD_DLL
      #define F2GetSchemeHandle _F2GetSchemeHandle
    #endif

  /*============================================================================
     Documentation for: F2GetModuleVariableHandle
    ============================================================================
       Get the for index a variable name associate with the module meta 
       data. Returns the index of the variable name.
  */
  long  FRAMES2API _F2GetModuleVariableHandle(int _PID,long _moduleHandle,char* _variableName);
    #ifndef BUILD_DLL
      #define F2GetModuleVariableHandle _F2GetModuleVariableHandle
    #endif

  /*============================================================================
     Documentation for: F2GetClassModuleHandle
    ============================================================================
       Gets a modules index at the class level. Returns the index to the 
       module.
  */
  long  FRAMES2API _F2GetClassModuleHandle(int _PID,long _domainHandle,long _classHandle,char* _moduleName);
    #ifndef BUILD_DLL
      #define F2GetClassModuleHandle _F2GetClassModuleHandle
    #endif

  /*============================================================================
     Documentation for: F2GetDomainHandle
    ============================================================================
       Returns the index of the given domain name.
  */
  long  FRAMES2API _F2GetDomainHandle(int _PID,char* _domainName);
    #ifndef BUILD_DLL
      #define F2GetDomainHandle _F2GetDomainHandle
    #endif

  /*============================================================================
     Documentation for: F2GetClassHandle
    ============================================================================
       Returns the index of the given class name.
  */
  long  FRAMES2API _F2GetClassHandle(int _PID,long _domainHandle,char* _className);
    #ifndef BUILD_DLL
      #define F2GetClassHandle _F2GetClassHandle
    #endif

  /*============================================================================
     Documentation for: F2GetGroupHandle
    ============================================================================
       Returns the index of the given group.
  */
  long  FRAMES2API _F2GetGroupHandle(int _PID,long _domainHandle,long _classHandle,char* _groupName);
    #ifndef BUILD_DLL
      #define F2GetGroupHandle _F2GetGroupHandle
    #endif

  /*============================================================================
     Documentation for: F2GetSubGroupHandle
    ============================================================================
       Returns the index of the given subgroup.
  */
  long  FRAMES2API _F2GetSubGroupHandle(int _PID,long _domainHandle,long _classHandle,long _groupHandle,char* _subgroupName);
    #ifndef BUILD_DLL
      #define F2GetSubGroupHandle _F2GetSubGroupHandle
    #endif

  /*============================================================================
     Documentation for: F2GetGroupModuleHandle
    ============================================================================
       Returns the index of the given module for a group.
  */
  long  FRAMES2API _F2GetGroupModuleHandle(int _PID,long _domainHandle,long _classHandle,long _groupHandle,char* _moduleName);
    #ifndef BUILD_DLL
      #define F2GetGroupModuleHandle _F2GetGroupModuleHandle
    #endif

  /*============================================================================
     Documentation for: F2GetSubGroupModuleHandle
    ============================================================================
       Returns the index of the given module for a subgroup.
  */
  long  FRAMES2API _F2GetSubGroupModuleHandle(int _PID,long _domainHandle,long _classHandle,long _groupHandle,long _subgroupHandle,char* _moduleName);
    #ifndef BUILD_DLL
      #define F2GetSubGroupModuleHandle _F2GetSubGroupModuleHandle
    #endif

  /*============================================================================
     Documentation for: F2GetSimulationHandle
    ============================================================================
       Get the index for the given simulation name.
  */
  long  FRAMES2API _F2GetSimulationHandle(int _PID,char* _simulationName);
    #ifndef BUILD_DLL
      #define F2GetSimulationHandle _F2GetSimulationHandle
    #endif

  /*============================================================================
     Documentation for: F2GetDataSetHandle
    ============================================================================
       Returns the index of each dataset.
  */
  long  FRAMES2API _F2GetDataSetHandle(int _PID,char* _datasetName);
    #ifndef BUILD_DLL
      #define F2GetDataSetHandle _F2GetDataSetHandle
    #endif

  /*============================================================================
     Documentation for: F2GetIconHandle
    ============================================================================
       Get dataset name.
  */
  long  FRAMES2API _F2GetIconHandle(int _PID,char* _iconName);
    #ifndef BUILD_DLL
      #define F2GetIconHandle _F2GetIconHandle
    #endif

  /*============================================================================
     Documentation for: F2CreateProcessId
    ============================================================================
       Get process id for launching process
  */
  int  FRAMES2API _F2CreateProcessId(int _PID,char* _iconName);
    #ifndef BUILD_DLL
      #define F2CreateProcessId _F2CreateProcessId
    #endif

  /*============================================================================
     Documentation for: F2DeleteProcessId
    ============================================================================
       Delete pid generated by call to F2CreateProcessId.
  */
  int  FRAMES2API _F2DeleteProcessId(int _PID,int _newpid);
    #ifndef BUILD_DLL
      #define F2DeleteProcessId _F2DeleteProcessId
    #endif

  /*============================================================================
     Documentation for: F2CountDataSetsChanged
    ============================================================================
       Count datasets changed due to UI or Model runs.
  */
  int  FRAMES2API _F2CountDataSetsChanged(int _PID,long _iconHandle,int _mode);
    #ifndef BUILD_DLL
      #define F2CountDataSetsChanged _F2CountDataSetsChanged
    #endif

  /*============================================================================
     Documentation for: SystemOpen
    ============================================================================
       Returns a Process IDentification (PID). If the PID returned is a 
       negative number an error has occured. Gives system developer level 
       access to FRAMES by taking in the fully qualified path to the FRAMES 
       direcory, and then opening "Startup.ini" and returning a PID. The PID 
       is used in all subsequent calls. If the Startup.ini file does not 
       exist in the path provided, it will be created. This function must be 
       the first API call and if it is never called, all API calls will be 
       ignored by FRAMES.
  */
  int  FRAMES2API _SystemOpen(char* _StartupFile);
    #ifndef BUILD_DLL
      #define SystemOpen _SystemOpen
    #endif

  /*============================================================================
     Documentation for: SystemClose
    ============================================================================
       Close the system developer link to the FRAMES system. If the cancel 
       flag is 0 then the updated datasets are saved. Otherwise the updated 
       datasets are thrown away.
  */
  void  FRAMES2API _SystemClose(int _PID,int _cancel);
    #ifndef BUILD_DLL
      #define SystemClose _SystemClose
    #endif

  /*============================================================================
     Documentation for: SystemSave
    ============================================================================
       Save system objects to file.
  */
  void  FRAMES2API _SystemSave(int _PID);
    #ifndef BUILD_DLL
      #define SystemSave _SystemSave
    #endif

  /*============================================================================
     Documentation for: SystemSaveToDB
    ============================================================================
       Save system objects to database.
  */
  void  FRAMES2API _SystemSaveToDB(int _PID);
    #ifndef BUILD_DLL
      #define SystemSaveToDB _SystemSaveToDB
    #endif

  /*============================================================================
     Documentation for: SystemSaveToFile
    ============================================================================
       Save system objects to file.
  */
  void  FRAMES2API _SystemSaveToFile(int _PID);
    #ifndef BUILD_DLL
      #define SystemSaveToFile _SystemSaveToFile
    #endif

  /*============================================================================
     Documentation for: SystemAddDictionary
    ============================================================================
       Returns 0 if successful and a negative number if an error has 
       occured. Creates and adds a dictionary into the FRAMES system 
       registry. The dictionary file is expected to be empty. The call is 
       made with a fully qualified path to where the dictionary is to be 
       saved and the name of the dictionary.
  */
  long  FRAMES2API _SystemAddDictionary(int _PID,char* _dictionaryFile,char* _dictionaryName);
    #ifndef BUILD_DLL
      #define SystemAddDictionary _SystemAddDictionary
    #endif

  /*============================================================================
     Documentation for: SystemOpenDictionary
    ============================================================================
       Open a dictionary file and add it to the FRAMES system registry. If a 
       negative value is returned, an error as occured. The dictionay file 
       is expected to be a complete dictionary file.
  */
  long  FRAMES2API _SystemOpenDictionary(int _PID,char* _dictionaryFile);
    #ifndef BUILD_DLL
      #define SystemOpenDictionary _SystemOpenDictionary
    #endif

  /*============================================================================
     Documentation for: SystemCloseDictionary
    ============================================================================
       Closes an open dictionay. The dictionary file is not deleted, but the 
       dictionary is unregistered from the FRAMES system.
  */
  void  FRAMES2API _SystemCloseDictionary(int _PID,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define SystemCloseDictionary _SystemCloseDictionary
    #endif

  /*============================================================================
     Documentation for: SystemSaveDictionary
    ============================================================================
       Saves the contents for a dictionary to disk. The dictionary file is 
       saved to the fully qualified path associated with the dictionary when 
       it was created or opened.
  */
  void  FRAMES2API _SystemSaveDictionary(int _PID,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define SystemSaveDictionary _SystemSaveDictionary
    #endif

  /*============================================================================
     Documentation for: SystemSaveDictionaryToFile
    ============================================================================
       Saves the contents for a dictionary to disk. The dictionary file is 
       saved to the fully qualified path associated with the dictionary when 
       it was created or opened.
  */
  void  FRAMES2API _SystemSaveDictionaryToFile(int _PID,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define SystemSaveDictionaryToFile _SystemSaveDictionaryToFile
    #endif

  /*============================================================================
     Documentation for: SystemSaveDictionaryToDB
    ============================================================================
       Saves the contents for a dictionary to disk. The dictionary file is 
       saved to the fully qualified path associated with the dictionary when 
       it was created or opened.
  */
  void  FRAMES2API _SystemSaveDictionaryToDB(int _PID,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define SystemSaveDictionaryToDB _SystemSaveDictionaryToDB
    #endif

  /*============================================================================
     Documentation for: SystemSaveDictionaryAs
    ============================================================================
       Save the contents of a dictionary to disk in a different location 
       than specified in the creation or opening of the dictionary. Does not 
       register the new file with the FRAMES system. The dictionary must 
       first be closed and then reopened for the name change to take place.
  */
  void  FRAMES2API _SystemSaveDictionaryAs(int _PID,long _dictionaryHandle,char* _dictionaryFile,char* _dictionaryNewName);
    #ifndef BUILD_DLL
      #define SystemSaveDictionaryAs _SystemSaveDictionaryAs
    #endif

  /*============================================================================
     Documentation for: SystemDictionaryCount
    ============================================================================
       Returns the number of dictionaries in the FRAMES system.
  */
  int  FRAMES2API _SystemDictionaryCount(int _PID);
    #ifndef BUILD_DLL
      #define SystemDictionaryCount _SystemDictionaryCount
    #endif

  /*============================================================================
     Documentation for: SystemAddModule
    ============================================================================
       Returns the index of the new module. Creates and adds a module 
       description into the FRAMES system registry. It is expected that the 
       .mod file does not exist or is empty.
  */
  long  FRAMES2API _SystemAddModule(int _PID,char* _moduleFile,char* _moduleName);
    #ifndef BUILD_DLL
      #define SystemAddModule _SystemAddModule
    #endif

  /*============================================================================
     Documentation for: SystemOpenModule
    ============================================================================
       Returns the index of the open module. Opens and adds a module 
       description into the FRAMES system registry. It is expected that the 
       .mod file exists.
  */
  long  FRAMES2API _SystemOpenModule(int _PID,char* _moduleFile);
    #ifndef BUILD_DLL
      #define SystemOpenModule _SystemOpenModule
    #endif

  /*============================================================================
     Documentation for: SystemCloseModule
    ============================================================================
       Closes a module file and removes the module reference from the FRAMES 
       system registry. Does not delete the module file if the file was 
       saved to disk.
  */
  void  FRAMES2API _SystemCloseModule(int _PID,long _moduleHandle);
    #ifndef BUILD_DLL
      #define SystemCloseModule _SystemCloseModule
    #endif

  /*============================================================================
     Documentation for: SystemSaveModule
    ============================================================================
       Saves the module to the FRAMES system registry.
  */
  void  FRAMES2API _SystemSaveModule(int _PID,long _moduleHandle);
    #ifndef BUILD_DLL
      #define SystemSaveModule _SystemSaveModule
    #endif

  /*============================================================================
     Documentation for: SystemSaveModuleToFile
    ============================================================================
       Saves the module to the FRAMES system registry.
  */
  void  FRAMES2API _SystemSaveModuleToFile(int _PID,long _moduleHandle);
    #ifndef BUILD_DLL
      #define SystemSaveModuleToFile _SystemSaveModuleToFile
    #endif

  /*============================================================================
     Documentation for: SystemSaveModuleToDB
    ============================================================================
       Saves the module to the FRAMES system registry.
  */
  void  FRAMES2API _SystemSaveModuleToDB(int _PID,long _moduleHandle);
    #ifndef BUILD_DLL
      #define SystemSaveModuleToDB _SystemSaveModuleToDB
    #endif

  /*============================================================================
     Documentation for: SystemSaveModuleAs
    ============================================================================
       Saves the module to the FRAMES system registry as a different module 
       name. Does not register the new file with the FRAMES system. The 
       module must first be closed and then reopened for the name change to 
       take place.
  */
  void  FRAMES2API _SystemSaveModuleAs(int _PID,long _moduleHandle,char* _moduleFile,char* _moduleNewName);
    #ifndef BUILD_DLL
      #define SystemSaveModuleAs _SystemSaveModuleAs
    #endif

  /*============================================================================
     Documentation for: SystemModuleCount
    ============================================================================
       Get the count of all modules in registry. Returns the number of 
       registered modules.
  */
  int  FRAMES2API _SystemModuleCount(int _PID);
    #ifndef BUILD_DLL
      #define SystemModuleCount _SystemModuleCount
    #endif

  /*============================================================================
     Documentation for: DictionarySetDescription
    ============================================================================
       Set the dictionary description specified by the provided dictionary 
       handle.
  */
  void  FRAMES2API _DictionarySetDescription(int _PID,long _dictionaryHandle,char* _description);
    #ifndef BUILD_DLL
      #define DictionarySetDescription _DictionarySetDescription
    #endif

  /*============================================================================
     Documentation for: DictionarySetPrivilege
    ============================================================================
       Set the privilege of a dictionary specified by dictionary handle.
  */
  void  FRAMES2API _DictionarySetPrivilege(int _PID,long _dictionaryHandle,int _privilege);
    #ifndef BUILD_DLL
      #define DictionarySetPrivilege _DictionarySetPrivilege
    #endif

  /*============================================================================
     Documentation for: DictionarySetVersion
    ============================================================================
       Set the version of a dictionary.
  */
  void  FRAMES2API _DictionarySetVersion(int _PID,long _dictionaryHandle,int _version);
    #ifndef BUILD_DLL
      #define DictionarySetVersion _DictionarySetVersion
    #endif

  /*============================================================================
     Documentation for: DictionaryAddVariable
    ============================================================================
       Adds a variable to a dictionary. Returns the variable handle if 
       successful, a negative value for failure. This function must be 
       called to allocate new space for a variable. The variable will be 
       added to the dictionary with the given handle. The variable will have 
       the default settings of:  dimension = 0; description = ""; type = 
       integer; stochastic = false; min = 0; max = 0.
  */
  long  FRAMES2API _DictionaryAddVariable(int _PID,long _dictionaryHandle,char* _variableName);
    #ifndef BUILD_DLL
      #define DictionaryAddVariable _DictionaryAddVariable
    #endif

  /*============================================================================
     Documentation for: DictionaryDeleteVariable
    ============================================================================
       Delete a variable from a dictionary specified by the dictionary 
       handle and the variable handle. Warning - Deleting a variable, 
       dictionary, or a dataset can have some serious side effects therefore 
       the software will not allow the use any these delete functions when 
       more than one handle to a simulation exists. (i.e. A module and the 
       system are active, or two system level users are active on a single 
       instance of FRAMES.
  */
  void  FRAMES2API _DictionaryDeleteVariable(int _PID,long _dictionaryHandle,long _variableHandle);
    #ifndef BUILD_DLL
      #define DictionaryDeleteVariable _DictionaryDeleteVariable
    #endif

  /*============================================================================
     Documentation for: DictionaryVariableCount
    ============================================================================
       Returns the number of variables in the dictionary specified by the 
       dictionary handle.
  */
  int  FRAMES2API _DictionaryVariableCount(int _PID,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define DictionaryVariableCount _DictionaryVariableCount
    #endif

  /*============================================================================
     Documentation for: VariableSetName
    ============================================================================
       Set a variable's name. If referenced by another variable as an index 
       an error occurs.
  */
  void  FRAMES2API _VariableSetName(int _PID,long _dictionaryHandle,long _variableHandle,char* _variableNewName);
    #ifndef BUILD_DLL
      #define VariableSetName _VariableSetName
    #endif

  /*============================================================================
     Documentation for: VariableSetDescription
    ============================================================================
       Set a variable's description.
  */
  void  FRAMES2API _VariableSetDescription(int _PID,long _dictionaryHandle,long _variableHandle,char* _description);
    #ifndef BUILD_DLL
      #define VariableSetDescription _VariableSetDescription
    #endif

  /*============================================================================
     Documentation for: VariableSetType
    ============================================================================
       Sets a variable's type. Type can be either "String", "Logical", 
       "Float", or "Integer".
  */
  void  FRAMES2API _VariableSetType(int _PID,long _dictionaryHandle,long _variableHandle,char* _type);
    #ifndef BUILD_DLL
      #define VariableSetType _VariableSetType
    #endif

  /*============================================================================
     Documentation for: VariableSetScalar
    ============================================================================
       Set the variable's scalar flag. If the flag is 0 then a set of values 
       is stored. (i.e. the variable is a vector and the dimensionality is 
       increased by 1)  If the flag = 1 a single value is stored.
  */
  void  FRAMES2API _VariableSetScalar(int _PID,long _dictionaryHandle,long _variableHandle,int _flag);
    #ifndef BUILD_DLL
      #define VariableSetScalar _VariableSetScalar
    #endif

  /*============================================================================
     Documentation for: VariableSetMinimum
    ============================================================================
       Set the variable's minimum value. Setting both minimum and maximum 
       equal to each other has the effect of having the variable NOT range 
       checked.
  */
  void  FRAMES2API _VariableSetMinimum(int _PID,long _dictionaryHandle,long _variableHandle,double _min);
    #ifndef BUILD_DLL
      #define VariableSetMinimum _VariableSetMinimum
    #endif

  /*============================================================================
     Documentation for: VariableSetMaximum
    ============================================================================
       Set the variable's maximum value. Setting both minimum and maximum 
       equal to each other has the effect of having the variable NOT range 
       checked.
  */
  void  FRAMES2API _VariableSetMaximum(int _PID,long _dictionaryHandle,long _variableHandle,double _max);
    #ifndef BUILD_DLL
      #define VariableSetMaximum _VariableSetMaximum
    #endif

  /*============================================================================
     Documentation for: VariableSetMeasure
    ============================================================================
       Set the variable's measure.
  */
  void  FRAMES2API _VariableSetMeasure(int _PID,long _dictionaryHandle,long _variableHandle,char* _measureName);
    #ifndef BUILD_DLL
      #define VariableSetMeasure _VariableSetMeasure
    #endif

  /*============================================================================
     Documentation for: VariableSetUnit
    ============================================================================
       Set the variable's unit for its given measure.
  */
  void  FRAMES2API _VariableSetUnit(int _PID,long _dictionaryHandle,long _variableHandle,char* _unitName);
    #ifndef BUILD_DLL
      #define VariableSetUnit _VariableSetUnit
    #endif

  /*============================================================================
     Documentation for: VariableSetStochastic
    ============================================================================
       Set the variable's stochastic flag. This flag signifies if the 
       parameter can be modified by an another program and not invalidate 
       any calibration. This allows other programs such and 
       Sensativity/Uncertainty or  parameter estimation programs to modify 
       these parameters.
  */
  void  FRAMES2API _VariableSetStochastic(int _PID,long _dictionaryHandle,long _variableHandle,int _flag);
    #ifndef BUILD_DLL
      #define VariableSetStochastic _VariableSetStochastic
    #endif

  /*============================================================================
     Documentation for: VariableSetPreposition
    ============================================================================
       Set a variable's preposition word. This helps in describing 
       parameters in a human readable form. This is the word that would best 
       be used to write a description of this parameters as an index for 
       another. For example, the chemical parameter would use "for" as a 
       preposition to make statements such as Concentration for benzene. The 
       preposition greatly facilitates writing descriptive text associated 
       with results.
  */
  void  FRAMES2API _VariableSetPreposition(int _PID,long _dictionaryHandle,long _variableHandle,char* _prepositionName);
    #ifndef BUILD_DLL
      #define VariableSetPreposition _VariableSetPreposition
    #endif

  /*============================================================================
     Documentation for: VariableSetPrimaryKey
    ============================================================================
       Sets the variable's primarykey flag. Is this parameter a key to 
       information on other parameters. This is used in both databases and 
       in the representation of the data. For example a variable being used 
       to name other variables frequently (like chemical) would indicate 
       that chemical should be a primary key, similar to the typical meaning 
       of primary key from database design.
  */
  void  FRAMES2API _VariableSetPrimaryKey(int _PID,long _dictionaryHandle,long _variableHandle,int _flag);
    #ifndef BUILD_DLL
      #define VariableSetPrimaryKey _VariableSetPrimaryKey
    #endif

  /*============================================================================
     Documentation for: VariableAddIndex
    ============================================================================
       Add a variable index to a variable.
  */
  void  FRAMES2API _VariableAddIndex(int _PID,long _dictionaryHandle,long _variableHandle,char* _dictionaryName,char* _variableName);
    #ifndef BUILD_DLL
      #define VariableAddIndex _VariableAddIndex
    #endif

  /*============================================================================
     Documentation for: VariableDeleteIndex
    ============================================================================
       Delete an index from a variable.
  */
  void  FRAMES2API _VariableDeleteIndex(int _PID,long _dictionaryHandle,long _variableHandle,int _indexIndex);
    #ifndef BUILD_DLL
      #define VariableDeleteIndex _VariableDeleteIndex
    #endif

  /*============================================================================
     Documentation for: VariableIndexCount
    ============================================================================
       Returns the number of indices for a variable.
  */
  int  FRAMES2API _VariableIndexCount(int _PID,long _dictionaryHandle,long _variableHandle);
    #ifndef BUILD_DLL
      #define VariableIndexCount _VariableIndexCount
    #endif

  /*============================================================================
     Documentation for: IndexGetIndex
    ============================================================================
       Returns the index position for an index of a variable specified by 
       the dictionary and variable.
  */
  int  FRAMES2API _IndexGetIndex(int _PID,long _dictionaryHandle,long _variableHandle,char* _dictionaryName,char* _variableName);
    #ifndef BUILD_DLL
      #define IndexGetIndex _IndexGetIndex
    #endif

  /*============================================================================
     Documentation for: IndexGetDictionaryName
    ============================================================================
       Returns the index dictionary name of the specified index in a 
       variable.
  */
  int  FRAMES2API _IndexGetDictionaryName(int _PID,long _dictionaryHandle,long _variableHandle,int _indexIndex,char* _retvalue);
    #ifndef BUILD_DLL
      #define IndexGetDictionaryName _IndexGetDictionaryName
    #endif

  /*============================================================================
     Documentation for: IndexGetVariableName
    ============================================================================
       Returns the index variable name of the specified index in a variable.
  */
  int  FRAMES2API _IndexGetVariableName(int _PID,long _dictionaryHandle,long _variableHandle,int _indexIndex,char* _retvalue);
    #ifndef BUILD_DLL
      #define IndexGetVariableName _IndexGetVariableName
    #endif

  /*============================================================================
     Documentation for: IndexPromote
    ============================================================================
       Move one level in the order of referred indices.
  */
  void  FRAMES2API _IndexPromote(int _PID,long _dictionaryHandle,long _variableHandle,int _referredIndex);
    #ifndef BUILD_DLL
      #define IndexPromote _IndexPromote
    #endif

  /*============================================================================
     Documentation for: IndexDemote
    ============================================================================
       Move down one level in the order of referred indices.
  */
  void  FRAMES2API _IndexDemote(int _PID,long _dictionaryHandle,long _variableHandle,int _referredIndex);
    #ifndef BUILD_DLL
      #define IndexDemote _IndexDemote
    #endif

  /*============================================================================
     Documentation for: ModuleGetPath
    ============================================================================
       Get module path associated with a module. Returns the file of the 
       module.
  */
  int  FRAMES2API _ModuleGetPath(int _PID,long _moduleHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define ModuleGetPath _ModuleGetPath
    #endif

  /*============================================================================
     Documentation for: ModuleAddFile
    ============================================================================
       Add module file and associated path with a module path for the 
       installed domain. Index of 0 uses the absolute directory. Index of 1 
       uses the relative directory.
  */
  void  FRAMES2API _ModuleAddFile(int _PID,long _moduleHandle,char* _moduleAddFile,int _directoryIndex);
    #ifndef BUILD_DLL
      #define ModuleAddFile _ModuleAddFile
    #endif

  /*============================================================================
     Documentation for: ModuleRemoveFile
    ============================================================================
       Remove module file and associated path with a module path for the 
       installed domain. Index of 0 uses the absolute directory. Index of 1 
       uses the relative directory.
  */
  void  FRAMES2API _ModuleRemoveFile(int _PID,long _moduleHandle,char* _moduleRemovedFile,int _directoryIndex);
    #ifndef BUILD_DLL
      #define ModuleRemoveFile _ModuleRemoveFile
    #endif

  /*============================================================================
     Documentation for: ModuleAddDictionary
    ============================================================================
       
  */
  void  FRAMES2API _ModuleAddDictionary(int _PID,long _moduleHandle,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define ModuleAddDictionary _ModuleAddDictionary
    #endif

  /*============================================================================
     Documentation for: ModuleDeleteDictionary
    ============================================================================
       
  */
  void  FRAMES2API _ModuleDeleteDictionary(int _PID,long _moduleHandle,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define ModuleDeleteDictionary _ModuleDeleteDictionary
    #endif

  /*============================================================================
     Documentation for: ModuleDictionaryCount
    ============================================================================
       
  */
  int  FRAMES2API _ModuleDictionaryCount(int _PID,long _moduleHandle);
    #ifndef BUILD_DLL
      #define ModuleDictionaryCount _ModuleDictionaryCount
    #endif

  /*============================================================================
     Documentation for: ModuleAddScheme
    ============================================================================
       Returns the index of the new scheme. Adds a scheme to a module.
  */
  long  FRAMES2API _ModuleAddScheme(int _PID,long _moduleHandle,char* _schemeName);
    #ifndef BUILD_DLL
      #define ModuleAddScheme _ModuleAddScheme
    #endif

  /*============================================================================
     Documentation for: ModuleDeleteScheme
    ============================================================================
       Deletes a scheme from a module.
  */
  void  FRAMES2API _ModuleDeleteScheme(int _PID,long _moduleHandle,long _schemeHandle);
    #ifndef BUILD_DLL
      #define ModuleDeleteScheme _ModuleDeleteScheme
    #endif

  /*============================================================================
     Documentation for: ModuleSchemeCount
    ============================================================================
       Returns the number of schemes in a module.
  */
  int  FRAMES2API _ModuleSchemeCount(int _PID,long _moduleHandle);
    #ifndef BUILD_DLL
      #define ModuleSchemeCount _ModuleSchemeCount
    #endif

  /*============================================================================
     Documentation for: SchemeGetName
    ============================================================================
       Returns the name the scheme with the specified handle.
  */
  int  FRAMES2API _SchemeGetName(int _PID,long _moduleHandle,long _schemeHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define SchemeGetName _SchemeGetName
    #endif

  /*============================================================================
     Documentation for: SchemeAddInputDictionary
    ============================================================================
       Adds an input dictionary to a scheme.
  */
  void  FRAMES2API _SchemeAddInputDictionary(int _PID,long _moduleHandle,long _schemeHandle,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define SchemeAddInputDictionary _SchemeAddInputDictionary
    #endif

  /*============================================================================
     Documentation for: SchemeDeleteInputDictionary
    ============================================================================
       Deletes an input dictionary from a scheme.
  */
  void  FRAMES2API _SchemeDeleteInputDictionary(int _PID,long _moduleHandle,long _schemeHandle,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define SchemeDeleteInputDictionary _SchemeDeleteInputDictionary
    #endif

  /*============================================================================
     Documentation for: SchemeInputDictionaryCount
    ============================================================================
       Returns the number of input dictionaries in a scheme.
  */
  int  FRAMES2API _SchemeInputDictionaryCount(int _PID,long _moduleHandle,long _schemeHandle);
    #ifndef BUILD_DLL
      #define SchemeInputDictionaryCount _SchemeInputDictionaryCount
    #endif

  /*============================================================================
     Documentation for: SchemeAddOutputDictionary
    ============================================================================
       Adds an output dictionary to a scheme.
  */
  void  FRAMES2API _SchemeAddOutputDictionary(int _PID,long _moduleHandle,long _schemeHandle,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define SchemeAddOutputDictionary _SchemeAddOutputDictionary
    #endif

  /*============================================================================
     Documentation for: SchemeDeleteOutputDictionary
    ============================================================================
       Deletes an output dictionary from a scheme.
  */
  void  FRAMES2API _SchemeDeleteOutputDictionary(int _PID,long _moduleHandle,long _schemeHandle,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define SchemeDeleteOutputDictionary _SchemeDeleteOutputDictionary
    #endif

  /*============================================================================
     Documentation for: SchemeOutputDictionaryCount
    ============================================================================
       Returns the number of output dictionaries in a scheme
  */
  int  FRAMES2API _SchemeOutputDictionaryCount(int _PID,long _moduleHandle,long _schemeHandle);
    #ifndef BUILD_DLL
      #define SchemeOutputDictionaryCount _SchemeOutputDictionaryCount
    #endif

  /*============================================================================
     Documentation for: ModuleVariableGetScalar
    ============================================================================
       Returns the variable's scalar flag. 1 = scalar and 0 = vector (a 
       collection of values)
  */
  int  FRAMES2API _ModuleVariableGetScalar(int _PID,long _moduleHandle,long _variableHandle);
    #ifndef BUILD_DLL
      #define ModuleVariableGetScalar _ModuleVariableGetScalar
    #endif

  /*============================================================================
     Documentation for: ModuleVariableCount
    ============================================================================
       Returns the number of variables in the module specified by the module 
       handle.
  */
  int  FRAMES2API _ModuleVariableCount(int _PID,long _moduleHandle);
    #ifndef BUILD_DLL
      #define ModuleVariableCount _ModuleVariableCount
    #endif

  /*============================================================================
     Documentation for: ModuleVariableDimensionCount
    ============================================================================
       Gets the size of the specified dimensions. The indices parameters 
       define an array of integers that define the index values that are to 
       be used.
  */
  int  FRAMES2API _ModuleVariableDimensionCount(int _PID,long _moduleHandle,long _variableHandle,int _indices[]);
    #ifndef BUILD_DLL
      #define ModuleVariableDimensionCount _ModuleVariableDimensionCount
    #endif

  /*============================================================================
     Documentation for: ModuleVariableGetInteger
    ============================================================================
       Returns an integer from meta data about a module specified by the 
       given indices.
  */
  int  FRAMES2API _ModuleVariableGetInteger(int _PID,long _moduleHandle,long _variableHandle,char* _unitName,int _indices[]);
    #ifndef BUILD_DLL
      #define ModuleVariableGetInteger _ModuleVariableGetInteger
    #endif

  /*============================================================================
     Documentation for: ModuleVariableGetFloat
    ============================================================================
       Returns a float from meta data about a module specified by the given 
       indices.
  */
  double  FRAMES2API _ModuleVariableGetFloat(int _PID,long _moduleHandle,long _variableHandle,char* _unitName,int _indices[]);
    #ifndef BUILD_DLL
      #define ModuleVariableGetFloat _ModuleVariableGetFloat
    #endif

  /*============================================================================
     Documentation for: ModuleVariableGetLogical
    ============================================================================
       Returns a logical from meta data about a module specified by the 
       given indices.
  */
  int  FRAMES2API _ModuleVariableGetLogical(int _PID,long _moduleHandle,long _variableHandle,char* _unitName,int _indices[]);
    #ifndef BUILD_DLL
      #define ModuleVariableGetLogical _ModuleVariableGetLogical
    #endif

  /*============================================================================
     Documentation for: ModuleVariableGetString
    ============================================================================
       Returns a string from meta data about a module specified by the given 
       indices.
  */
  int  FRAMES2API _ModuleVariableGetString(int _PID,long _moduleHandle,long _variableHandle,char* _unitName,int _indices[],char* _retvalue);
    #ifndef BUILD_DLL
      #define ModuleVariableGetString _ModuleVariableGetString
    #endif

  /*============================================================================
     Documentation for: ModuleVariableSetInteger
    ============================================================================
       Sets an integer from meta data about a module specified by the given 
       indices.
  */
  void  FRAMES2API _ModuleVariableSetInteger(int _PID,long _moduleHandle,long _variableHandle,char* _unitName,int _indices[],int _value);
    #ifndef BUILD_DLL
      #define ModuleVariableSetInteger _ModuleVariableSetInteger
    #endif

  /*============================================================================
     Documentation for: ModuleVariableSetFloat
    ============================================================================
       Sets a float from meta data about a module specified by the given 
       indices.
  */
  void  FRAMES2API _ModuleVariableSetFloat(int _PID,long _moduleHandle,long _variableHandle,char* _unitName,int _indices[],double _value);
    #ifndef BUILD_DLL
      #define ModuleVariableSetFloat _ModuleVariableSetFloat
    #endif

  /*============================================================================
     Documentation for: ModuleVariableSetLogical
    ============================================================================
       Sets a logical from meta data about a module specified by the given 
       indices.
  */
  void  FRAMES2API _ModuleVariableSetLogical(int _PID,long _moduleHandle,long _variableHandle,char* _unitName,int _indices[],int _value);
    #ifndef BUILD_DLL
      #define ModuleVariableSetLogical _ModuleVariableSetLogical
    #endif

  /*============================================================================
     Documentation for: ModuleVariableSetString
    ============================================================================
       Sets a string from meta data about a module specified by the given 
       indices.
  */
  void  FRAMES2API _ModuleVariableSetString(int _PID,long _moduleHandle,long _variableHandle,char* _unitName,int _indices[],char* _value);
    #ifndef BUILD_DLL
      #define ModuleVariableSetString _ModuleVariableSetString
    #endif

  /*============================================================================
     Documentation for: ModuleLookUp
    ============================================================================
       Returns the index for the specified value in a set of values. The set 
       is determined by which index is given a 0 value in the indice array. 
       A negative number indicates the element could not be found.
  */
  int  FRAMES2API _ModuleLookUp(int _PID,long _moduleHandle,char* _variableName,char* _value,int _indices[]);
    #ifndef BUILD_DLL
      #define ModuleLookUp _ModuleLookUp
    #endif

  /*============================================================================
     Documentation for: ModuleDimensionCount
    ============================================================================
       Gets the size of the specified dimensions. The indices parameters 
       define an array of integers that define the index values that are to 
       be used.
  */
  int  FRAMES2API _ModuleDimensionCount(int _PID,long _moduleHandle,char* _variableName,int _indices[]);
    #ifndef BUILD_DLL
      #define ModuleDimensionCount _ModuleDimensionCount
    #endif

  /*============================================================================
     Documentation for: ModuleReadInt
    ============================================================================
       Returns an integer from meta data about a module specified by the 
       given indices.
  */
  int  FRAMES2API _ModuleReadInt(int _PID,long _moduleHandle,char* _variableName,char* _unitName,int _indices[]);
    #ifndef BUILD_DLL
      #define ModuleReadInt _ModuleReadInt
    #endif

  /*============================================================================
     Documentation for: ModuleReadReal
    ============================================================================
       Returns a float from meta data about a module specified by the given 
       indices.
  */
  double  FRAMES2API _ModuleReadReal(int _PID,long _moduleHandle,char* _variableName,char* _unitName,int _indices[]);
    #ifndef BUILD_DLL
      #define ModuleReadReal _ModuleReadReal
    #endif

  /*============================================================================
     Documentation for: ModuleReadLog
    ============================================================================
       Returns a logical from meta data about a module specified by the 
       given indices.
  */
  int  FRAMES2API _ModuleReadLog(int _PID,long _moduleHandle,char* _variableName,char* _unitName,int _indices[]);
    #ifndef BUILD_DLL
      #define ModuleReadLog _ModuleReadLog
    #endif

  /*============================================================================
     Documentation for: ModuleReadString
    ============================================================================
       Returns a string from meta data about a module specified by the given 
       indices.
  */
  int  FRAMES2API _ModuleReadString(int _PID,long _moduleHandle,char* _variableName,char* _unitName,int _indices[],char* _retvalue);
    #ifndef BUILD_DLL
      #define ModuleReadString _ModuleReadString
    #endif

  /*============================================================================
     Documentation for: ModuleWriteInt
    ============================================================================
       Sets an integer from meta data about a module specified by the given 
       indices.
  */
  void  FRAMES2API _ModuleWriteInt(int _PID,long _moduleHandle,char* _variableName,char* _unitName,int _indices[],int _value);
    #ifndef BUILD_DLL
      #define ModuleWriteInt _ModuleWriteInt
    #endif

  /*============================================================================
     Documentation for: ModuleWriteReal
    ============================================================================
       Sets a float from meta data about a module specified by the given 
       indices.
  */
  void  FRAMES2API _ModuleWriteReal(int _PID,long _moduleHandle,char* _variableName,char* _unitName,int _indices[],double _value);
    #ifndef BUILD_DLL
      #define ModuleWriteReal _ModuleWriteReal
    #endif

  /*============================================================================
     Documentation for: ModuleWriteLog
    ============================================================================
       Sets a logical from meta data about a module specified by the given 
       indices.
  */
  void  FRAMES2API _ModuleWriteLog(int _PID,long _moduleHandle,char* _variableName,char* _unitName,int _indices[],int _value);
    #ifndef BUILD_DLL
      #define ModuleWriteLog _ModuleWriteLog
    #endif

  /*============================================================================
     Documentation for: ModuleWriteString
    ============================================================================
       Sets a string from meta data about a module specified by the given 
       indices.
  */
  void  FRAMES2API _ModuleWriteString(int _PID,long _moduleHandle,char* _variableName,char* _unitName,int _indices[],char* _value);
    #ifndef BUILD_DLL
      #define ModuleWriteString _ModuleWriteString
    #endif

  /*============================================================================
     Documentation for: SystemAddDomain
    ============================================================================
       Returns the handle of the added domain. Automatically adds the four 
       class types to the domain.
  */
  long  FRAMES2API _SystemAddDomain(int _PID,char* _domainName);
    #ifndef BUILD_DLL
      #define SystemAddDomain _SystemAddDomain
    #endif

  /*============================================================================
     Documentation for: SystemDeleteDomain
    ============================================================================
       Delets a domain and its entire tree of Classes, Groups, and SubGroups.
  */
  void  FRAMES2API _SystemDeleteDomain(int _PID,long _domainHandle);
    #ifndef BUILD_DLL
      #define SystemDeleteDomain _SystemDeleteDomain
    #endif

  /*============================================================================
     Documentation for: DomainGetIcon
    ============================================================================
       Returns the fully qualified path for the domain icon. Icons are 
       either .bmp, .gif files and 32x32 pixels in size.
  */
  int  FRAMES2API _DomainGetIcon(int _PID,long _domainHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define DomainGetIcon _DomainGetIcon
    #endif

  /*============================================================================
     Documentation for: DomainSetIcon
    ============================================================================
       Sets the fully qualified path for the domain icon. Icons are either 
       .bmp, .gif files and 32x32 pixels in size.
  */
  void  FRAMES2API _DomainSetIcon(int _PID,long _domainHandle,char* _domainIconFile);
    #ifndef BUILD_DLL
      #define DomainSetIcon _DomainSetIcon
    #endif

  /*============================================================================
     Documentation for: DomainAddClass
    ============================================================================
       Returns the index of the new class. Adds a class to a domain.
  */
  long  FRAMES2API _DomainAddClass(int _PID,long _domainHandle,char* _className);
    #ifndef BUILD_DLL
      #define DomainAddClass _DomainAddClass
    #endif

  /*============================================================================
     Documentation for: DomainDeleteClass
    ============================================================================
       Deletes a class from a domain. Also deletes modules and anything else 
       below the group in the tree.
  */
  void  FRAMES2API _DomainDeleteClass(int _PID,long _domainHandle,long _classHandle);
    #ifndef BUILD_DLL
      #define DomainDeleteClass _DomainDeleteClass
    #endif

  /*============================================================================
     Documentation for: DomainAddDictionary
    ============================================================================
       
  */
  void  FRAMES2API _DomainAddDictionary(int _PID,long _domainHandle,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define DomainAddDictionary _DomainAddDictionary
    #endif

  /*============================================================================
     Documentation for: DomainDeleteDictionary
    ============================================================================
       
  */
  void  FRAMES2API _DomainDeleteDictionary(int _PID,long _domainHandle,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define DomainDeleteDictionary _DomainDeleteDictionary
    #endif

  /*============================================================================
     Documentation for: DomainDictionaryCount
    ============================================================================
       
  */
  int  FRAMES2API _DomainDictionaryCount(int _PID,long _domainHandle);
    #ifndef BUILD_DLL
      #define DomainDictionaryCount _DomainDictionaryCount
    #endif

  /*============================================================================
     Documentation for: ClassGetIcon
    ============================================================================
       Returns the fully qualified path for the module class icon. Icons are 
       either .bmp, .gif files and 32x32 pixels in size.
  */
  int  FRAMES2API _ClassGetIcon(int _PID,long _domainHandle,long _classHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define ClassGetIcon _ClassGetIcon
    #endif

  /*============================================================================
     Documentation for: ClassSetIcon
    ============================================================================
       Sets the fully qualified path for the module class icon. Icons are 
       either .bmp, .gif files and 32x32 pixels in size.
  */
  void  FRAMES2API _ClassSetIcon(int _PID,long _domainHandle,long _classHandle,char* _classIconFile);
    #ifndef BUILD_DLL
      #define ClassSetIcon _ClassSetIcon
    #endif

  /*============================================================================
     Documentation for: ClassAddGroup
    ============================================================================
       Returns the index of the new group. Adds a group to a module class.
  */
  long  FRAMES2API _ClassAddGroup(int _PID,long _domainHandle,long _classHandle,char* _groupName);
    #ifndef BUILD_DLL
      #define ClassAddGroup _ClassAddGroup
    #endif

  /*============================================================================
     Documentation for: ClassDeleteGroup
    ============================================================================
       Deletes a group from a module class. Also deletes modules and 
       anything else below the group in the tree.
  */
  void  FRAMES2API _ClassDeleteGroup(int _PID,long _domainHandle,long _classHandle,long _groupHandle);
    #ifndef BUILD_DLL
      #define ClassDeleteGroup _ClassDeleteGroup
    #endif

  /*============================================================================
     Documentation for: GroupGetIcon
    ============================================================================
       Returns the fully qualified path for the group icon. Icons are either 
       .bmp, .gif files and 32x32 pixels in size.
  */
  int  FRAMES2API _GroupGetIcon(int _PID,long _domainHandle,long _classHandle,long _groupHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define GroupGetIcon _GroupGetIcon
    #endif

  /*============================================================================
     Documentation for: GroupSetIcon
    ============================================================================
       Sets the fully qualified path for the group icon. Icons are either 
       .bmp, .gif files and 32x32 pixels in size.
  */
  void  FRAMES2API _GroupSetIcon(int _PID,long _domainHandle,long _classHandle,long _groupHandle,char* _groupIconFile);
    #ifndef BUILD_DLL
      #define GroupSetIcon _GroupSetIcon
    #endif

  /*============================================================================
     Documentation for: GroupAddSubGroup
    ============================================================================
       Returns the index of the new subGroup. Adds a subGroup to a group.
  */
  long  FRAMES2API _GroupAddSubGroup(int _PID,long _domainHandle,long _classHandle,long _groupHandle,char* _subgroupName);
    #ifndef BUILD_DLL
      #define GroupAddSubGroup _GroupAddSubGroup
    #endif

  /*============================================================================
     Documentation for: GroupDeleteSubGroup
    ============================================================================
       Deletes a subGroup and its modules from a group.
  */
  void  FRAMES2API _GroupDeleteSubGroup(int _PID,long _domainHandle,long _classHandle,long _groupHandle,long _subgroupHandle);
    #ifndef BUILD_DLL
      #define GroupDeleteSubGroup _GroupDeleteSubGroup
    #endif

  /*============================================================================
     Documentation for: SubGroupGetIcon
    ============================================================================
       Returns the fully qualified path for the subGroup icon. Icons are 
       either .bmp, .gif files and 32x32 pixels in size.
  */
  int  FRAMES2API _SubGroupGetIcon(int _PID,long _domainHandle,long _classHandle,long _groupHandle,long _subgroupHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define SubGroupGetIcon _SubGroupGetIcon
    #endif

  /*============================================================================
     Documentation for: SubGroupSetIcon
    ============================================================================
       Sets the fully qualified path for the subGroup icon. Icons are either 
       .bmp, .gif files and 32x32 pixels in size.
  */
  void  FRAMES2API _SubGroupSetIcon(int _PID,long _domainHandle,long _classHandle,long _groupHandle,long _subgroupHandle,char* _subgroupIconFile);
    #ifndef BUILD_DLL
      #define SubGroupSetIcon _SubGroupSetIcon
    #endif

  /*============================================================================
     Documentation for: DomainNodeAddModule
    ============================================================================
       Add module to any valid domain node.
  */
  void  FRAMES2API _DomainNodeAddModule(int _PID,long _nodeHandle,long _moduleHandle);
    #ifndef BUILD_DLL
      #define DomainNodeAddModule _DomainNodeAddModule
    #endif

  /*============================================================================
     Documentation for: DomainNodeDeleteModule
    ============================================================================
       Remove module from any valid domain node.
  */
  void  FRAMES2API _DomainNodeDeleteModule(int _PID,long _nodeHandle,long _moduleHandle);
    #ifndef BUILD_DLL
      #define DomainNodeDeleteModule _DomainNodeDeleteModule
    #endif

  /*============================================================================
     Documentation for: DomainAddModule
    ============================================================================
       Add module to any valid domain node.
  */
  void  FRAMES2API _DomainAddModule(int _PID,long _domainHandle,long _moduleHandle);
    #ifndef BUILD_DLL
      #define DomainAddModule _DomainAddModule
    #endif

  /*============================================================================
     Documentation for: DomainDeleteModule
    ============================================================================
       Remove module from any valid domain node.
  */
  void  FRAMES2API _DomainDeleteModule(int _PID,long _domainHandle,long _moduleHandle);
    #ifndef BUILD_DLL
      #define DomainDeleteModule _DomainDeleteModule
    #endif

  /*============================================================================
     Documentation for: GroupAddModule
    ============================================================================
       Returns the index of the newly added module. Adds a module to a group.
  */
  void  FRAMES2API _GroupAddModule(int _PID,long _domainHandle,long _classHandle,long _groupHandle,long _moduleHandle);
    #ifndef BUILD_DLL
      #define GroupAddModule _GroupAddModule
    #endif

  /*============================================================================
     Documentation for: GroupDeleteModule
    ============================================================================
       Deletes a module from a group.
  */
  void  FRAMES2API _GroupDeleteModule(int _PID,long _domainHandle,long _classHandle,long _groupHandle,long _moduleHandle);
    #ifndef BUILD_DLL
      #define GroupDeleteModule _GroupDeleteModule
    #endif

  /*============================================================================
     Documentation for: SubGroupAddModule
    ============================================================================
       Returns the index of the newly added module. Adds a module to a sub 
       group.
  */
  void  FRAMES2API _SubGroupAddModule(int _PID,long _domainHandle,long _classHandle,long _groupHandle,long _subgroupHandle,long _moduleHandle);
    #ifndef BUILD_DLL
      #define SubGroupAddModule _SubGroupAddModule
    #endif

  /*============================================================================
     Documentation for: SubGroupDeleteModule
    ============================================================================
       Deletes a module from a subGroup.
  */
  void  FRAMES2API _SubGroupDeleteModule(int _PID,long _domainHandle,long _classHandle,long _groupHandle,long _subgroupHandle,long _moduleHandle);
    #ifndef BUILD_DLL
      #define SubGroupDeleteModule _SubGroupDeleteModule
    #endif

  /*============================================================================
     Documentation for: DomainGetName
    ============================================================================
       Returns a domain name.
  */
  int  FRAMES2API _DomainGetName(int _PID,long _domainHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define DomainGetName _DomainGetName
    #endif

  /*============================================================================
     Documentation for: ClassGetName
    ============================================================================
       Returns a module class name.
  */
  int  FRAMES2API _ClassGetName(int _PID,long _domainHandle,long _classHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define ClassGetName _ClassGetName
    #endif

  /*============================================================================
     Documentation for: GroupGetName
    ============================================================================
       Returns a group name.
  */
  int  FRAMES2API _GroupGetName(int _PID,long _domainHandle,long _classHandle,long _groupHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define GroupGetName _GroupGetName
    #endif

  /*============================================================================
     Documentation for: SubGroupGetName
    ============================================================================
       Returns a subGroup name.
  */
  int  FRAMES2API _SubGroupGetName(int _PID,long _domainHandle,long _classHandle,long _groupHandle,long _subgroupHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define SubGroupGetName _SubGroupGetName
    #endif

  /*============================================================================
     Documentation for: SystemDomainCount
    ============================================================================
       Returns the number of domains.
  */
  int  FRAMES2API _SystemDomainCount(int _PID);
    #ifndef BUILD_DLL
      #define SystemDomainCount _SystemDomainCount
    #endif

  /*============================================================================
     Documentation for: DomainClassCount
    ============================================================================
       Returns the number of module classes in a domain.
  */
  int  FRAMES2API _DomainClassCount(int _PID,long _domainHandle);
    #ifndef BUILD_DLL
      #define DomainClassCount _DomainClassCount
    #endif

  /*============================================================================
     Documentation for: ClassGroupCount
    ============================================================================
       Returns the number of groups in a module class.
  */
  int  FRAMES2API _ClassGroupCount(int _PID,long _domainHandle,long _classHandle);
    #ifndef BUILD_DLL
      #define ClassGroupCount _ClassGroupCount
    #endif

  /*============================================================================
     Documentation for: GroupSubGroupCount
    ============================================================================
       Returns the number of sub groups in a group.
  */
  int  FRAMES2API _GroupSubGroupCount(int _PID,long _domainHandle,long _classHandle,long _groupHandle);
    #ifndef BUILD_DLL
      #define GroupSubGroupCount _GroupSubGroupCount
    #endif

  /*============================================================================
     Documentation for: GroupModuleCount
    ============================================================================
       Returns the number of modules in a group.
  */
  int  FRAMES2API _GroupModuleCount(int _PID,long _domainHandle,long _classHandle,long _groupHandle);
    #ifndef BUILD_DLL
      #define GroupModuleCount _GroupModuleCount
    #endif

  /*============================================================================
     Documentation for: SubGroupModuleCount
    ============================================================================
       Returns the number of modules in a subgroup.
  */
  int  FRAMES2API _SubGroupModuleCount(int _PID,long _domainHandle,long _classHandle,long _groupHandle,long _subgroupHandle);
    #ifndef BUILD_DLL
      #define SubGroupModuleCount _SubGroupModuleCount
    #endif

  /*============================================================================
     Documentation for: SystemAddSimulation
    ============================================================================
       Returns the handle of the newly created simulation file.
  */
  long  FRAMES2API _SystemAddSimulation(int _PID,char* _simulationFile,char* _simulationName);
    #ifndef BUILD_DLL
      #define SystemAddSimulation _SystemAddSimulation
    #endif

  /*============================================================================
     Documentation for: SystemOpenSimulation
    ============================================================================
       Returns the handle of the opened simulation file.
  */
  long  FRAMES2API _SystemOpenSimulation(int _PID,char* _simulationFile);
    #ifndef BUILD_DLL
      #define SystemOpenSimulation _SystemOpenSimulation
    #endif

  /*============================================================================
     Documentation for: SystemSaveSimulation
    ============================================================================
       Save the simulation present state to the file specified.
  */
  void  FRAMES2API _SystemSaveSimulation(int _PID);
    #ifndef BUILD_DLL
      #define SystemSaveSimulation _SystemSaveSimulation
    #endif

  /*============================================================================
     Documentation for: SystemSaveSimulationToFile
    ============================================================================
       Save the simulation present state to a file. The 
       SystemSaveSimulationToFile function call is made with a process id as 
       input to save the simulation's present state.
  */
  void  FRAMES2API _SystemSaveSimulationToFile(int _PID);
    #ifndef BUILD_DLL
      #define SystemSaveSimulationToFile _SystemSaveSimulationToFile
    #endif

  /*============================================================================
     Documentation for: SystemSaveSimulationToDB
    ============================================================================
       Save the simulation present state to a database. The 
       SystemSaveSimulationToDB function call is made with a process id as 
       input to save the simulation's present state.
  */
  void  FRAMES2API _SystemSaveSimulationToDB(int _PID);
    #ifndef BUILD_DLL
      #define SystemSaveSimulationToDB _SystemSaveSimulationToDB
    #endif

  /*============================================================================
     Documentation for: SystemSaveSimulationAs
    ============================================================================
       Save the simulation present state to a different file than specified. 
       Does not register the new file with the FRAMES system. The simulation 
       must first be closed and then reopened for the name change to take 
       place.
  */
  void  FRAMES2API _SystemSaveSimulationAs(int _PID,char* _simulationFile,char* _simulationNewName);
    #ifndef BUILD_DLL
      #define SystemSaveSimulationAs _SystemSaveSimulationAs
    #endif

  /*============================================================================
     Documentation for: SystemCloseSimulation
    ============================================================================
       Close the simulation without saving.
  */
  void  FRAMES2API _SystemCloseSimulation(int _PID);
    #ifndef BUILD_DLL
      #define SystemCloseSimulation _SystemCloseSimulation
    #endif

  /*============================================================================
     Documentation for: SystemAddDataSet
    ============================================================================
       Returns the handle to the new dataset. Creates a new dataset using a 
       dictionary handle.
  */
  long  FRAMES2API _SystemAddDataSet(int _PID,long _dictionaryHandle,char* _datasetFile,char* _datasetName);
    #ifndef BUILD_DLL
      #define SystemAddDataSet _SystemAddDataSet
    #endif

  /*============================================================================
     Documentation for: SystemOpenDataSet
    ============================================================================
       Returns the handle to the dataset. Opens a dataset. The dictionary 
       used by the dataset must be registered with the FRAMES system for the 
       dataset to function correctly.
  */
  long  FRAMES2API _SystemOpenDataSet(int _PID,char* _datasetFile);
    #ifndef BUILD_DLL
      #define SystemOpenDataSet _SystemOpenDataSet
    #endif

  /*============================================================================
     Documentation for: SystemCloseDataSet
    ============================================================================
       Close a dataset from the FRAMES system and remove its entry in the 
       system registry.
  */
  void  FRAMES2API _SystemCloseDataSet(int _PID,long _datasetHandle);
    #ifndef BUILD_DLL
      #define SystemCloseDataSet _SystemCloseDataSet
    #endif

  /*============================================================================
     Documentation for: SystemSaveDataSet
    ============================================================================
       Save a dataset to the file indicated when the dataset was created.
  */
  void  FRAMES2API _SystemSaveDataSet(int _PID,long _datasetHandle);
    #ifndef BUILD_DLL
      #define SystemSaveDataSet _SystemSaveDataSet
    #endif

  /*============================================================================
     Documentation for: SystemSaveDataSetToFile
    ============================================================================
       Save a dataset to the file indicated when the dataset was created.
  */
  void  FRAMES2API _SystemSaveDataSetToFile(int _PID,long _datasetHandle);
    #ifndef BUILD_DLL
      #define SystemSaveDataSetToFile _SystemSaveDataSetToFile
    #endif

  /*============================================================================
     Documentation for: SystemSaveDataSetToDB
    ============================================================================
       Save a dataset to the file indicated when the dataset was created.
  */
  void  FRAMES2API _SystemSaveDataSetToDB(int _PID,long _datasetHandle);
    #ifndef BUILD_DLL
      #define SystemSaveDataSetToDB _SystemSaveDataSetToDB
    #endif

  /*============================================================================
     Documentation for: SystemSaveDataSetAs
    ============================================================================
       Save the dataset with a different name to a different file. Does not 
       register the new file with the FRAMES system. The dataset must first 
       be closed and then reopened for the name change to take place.
  */
  void  FRAMES2API _SystemSaveDataSetAs(int _PID,long _datasetHandle,char* _datasetFile,char* _datasetNewName);
    #ifndef BUILD_DLL
      #define SystemSaveDataSetAs _SystemSaveDataSetAs
    #endif

  /*============================================================================
     Documentation for: SimulationAddLink
    ============================================================================
       Links two icons together inside a simulation. The arrow is drawn from 
       the "From Icon" and goes to the "To Icon."
  */
  void  FRAMES2API _SimulationAddLink(int _PID,long _fromIconHandle,long _toIconHandle);
    #ifndef BUILD_DLL
      #define SimulationAddLink _SimulationAddLink
    #endif

  /*============================================================================
     Documentation for: SimulationDeleteLink
    ============================================================================
       Deletes the link between two icons.
  */
  void  FRAMES2API _SimulationDeleteLink(int _PID,long _fromIconHandle,long _toIconHandle);
    #ifndef BUILD_DLL
      #define SimulationDeleteLink _SimulationDeleteLink
    #endif

  /*============================================================================
     Documentation for: SimulationAddIcon
    ============================================================================
       Returns the icon index for the added icon. Adds an icon into a 
       simulation.
  */
  long  FRAMES2API _SimulationAddIcon(int _PID,int _scope,long _domainHandle,long _classHandle,long _groupHandle,long _subgroupHandle);
    #ifndef BUILD_DLL
      #define SimulationAddIcon _SimulationAddIcon
    #endif

  /*============================================================================
     Documentation for: SimulationDeleteIcon
    ============================================================================
       Deletes an icon from the simulation.
  */
  void  FRAMES2API _SimulationDeleteIcon(int _PID,long _iconHandle);
    #ifndef BUILD_DLL
      #define SimulationDeleteIcon _SimulationDeleteIcon
    #endif

  /*============================================================================
     Documentation for: IconSetModule
    ============================================================================
       Sets the dataset names for the module and scheme associated with the 
       indicated icon. A subset of the datasets the icon consumes is given 
       in the list parameter.
  */
  void  FRAMES2API _IconSetModule(int _PID,long _iconHandle,long _moduleHandle,long _schemeHandle,char* _delimiter,char* _list);
    #ifndef BUILD_DLL
      #define IconSetModule _IconSetModule
    #endif

  /*============================================================================
     Documentation for: IconSetLabel
    ============================================================================
       Sets the user supplied label of an icon given the icon handle.
  */
  int  FRAMES2API _IconSetLabel(int _PID,long _iconHandle,char* _labe);
    #ifndef BUILD_DLL
      #define IconSetLabel _IconSetLabel
    #endif

  /*============================================================================
     Documentation for: IconSetNote
    ============================================================================
       Sets the user suppplied note of an icon given the icon handle.
  */
  int  FRAMES2API _IconSetNote(int _PID,long _iconHandle,char* _note);
    #ifndef BUILD_DLL
      #define IconSetNote _IconSetNote
    #endif

  /*============================================================================
     Documentation for: IconSetPosXY
    ============================================================================
       Sets the location of screen coordinates of an icon given the icon 
       handle.
  */
  int  FRAMES2API _IconSetPosXY(int _PID,long _iconHandle,int _x,int _y);
    #ifndef BUILD_DLL
      #define IconSetPosXY _IconSetPosXY
    #endif

  /*============================================================================
     Documentation for: IconGetUIPath
    ============================================================================
       Returns the executable/batch file for the user interface 
       (interactive) portion of an icon's module. The interactive portion of 
       a module requires human interaction.
  */
  int  FRAMES2API _IconGetUIPath(int _PID,long _iconHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define IconGetUIPath _IconGetUIPath
    #endif

  /*============================================================================
     Documentation for: IconGetUIArguments
    ============================================================================
       Returns the arguments for the user interface (interactive) portion of 
       an icon's module. The interactive portion of a module requires human 
       interaction.
  */
  int  FRAMES2API _IconGetUIArguments(int _PID,long _iconHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define IconGetUIArguments _IconGetUIArguments
    #endif

  /*============================================================================
     Documentation for: IconRunUI
    ============================================================================
       Execute the executable/batch file for the user interface 
       (interactive) portion of an icon's module. The interactive portion of 
       a module requires human interaction.
  */
  int  FRAMES2API _IconRunUI(int _PID,long _iconHandle,char* _uiExecutableFile,char* _cmdLine,char* _warningFile,char* _errorFile);
    #ifndef BUILD_DLL
      #define IconRunUI _IconRunUI
    #endif

  /*============================================================================
     Documentation for: IconGetModelPath
    ============================================================================
       Returns the fully qualified path to the executable/batch file to the 
       model (passive) part of a module. The passive portion of the model 
       requires no interaction.
  */
  int  FRAMES2API _IconGetModelPath(int _PID,long _iconHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define IconGetModelPath _IconGetModelPath
    #endif

  /*============================================================================
     Documentation for: IconGetModelArguments
    ============================================================================
       The passive part of the icon is the model itself that requires NO 
       user to be active during its execution. Returns the arguments or 
       commandline for the executable/batch file to the model (passive) part 
       of a module.
  */
  int  FRAMES2API _IconGetModelArguments(int _PID,long _iconHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define IconGetModelArguments _IconGetModelArguments
    #endif

  /*============================================================================
     Documentation for: IconRunModel
    ============================================================================
       The passive part of the icon is the model itself that requires NO 
       user to be active during its execution. The RunModPassive function 
       call is made with a process id and a modId string as input to run a 
       passive module. Returns a success/fail flag.
  */
  int  FRAMES2API _IconRunModel(int _PID,long _iconHandle,char* _modelExecutableFile,char* _cmdLine,char* _warningFile,char* _errorFile);
    #ifndef BUILD_DLL
      #define IconRunModel _IconRunModel
    #endif

  /*============================================================================
     Documentation for: IconRunUIStatus
    ============================================================================
       Returns the status of the process. This will indicate whether the 
       process interacted completely with the system by calling OpenIO, 
       CloseIO, has no errors and changed ui datasets
  */
  int  FRAMES2API _IconRunUIStatus(int _PID,long _iconHandle);
    #ifndef BUILD_DLL
      #define IconRunUIStatus _IconRunUIStatus
    #endif

  /*============================================================================
     Documentation for: IconRunModelStatus
    ============================================================================
       Returns the status of the process. This will indicate whether the 
       process interacted completely with the system by calling OpenIO, 
       CloseIO, has no errors and changed output datasets
  */
  int  FRAMES2API _IconRunModelStatus(int _PID,long _iconHandle);
    #ifndef BUILD_DLL
      #define IconRunModelStatus _IconRunModelStatus
    #endif

  /*============================================================================
     Documentation for: IconIsUIOk
    ============================================================================
       Indicates whether the icon state is sufficient to allow UI to execute.
  */
  int  FRAMES2API _IconIsUIOk(int _PID,long _iconHandle);
    #ifndef BUILD_DLL
      #define IconIsUIOk _IconIsUIOk
    #endif

  /*============================================================================
     Documentation for: IconIsModelOk
    ============================================================================
       Indicates whether the icon state is sufficient to allow model to 
       execute.
  */
  int  FRAMES2API _IconIsModelOk(int _PID,long _iconHandle);
    #ifndef BUILD_DLL
      #define IconIsModelOk _IconIsModelOk
    #endif

  /*============================================================================
     Documentation for: IconIsInformationOk
    ============================================================================
       Indicates whether the icon state is sufficient to allow input of 
       information.
  */
  int  FRAMES2API _IconIsInformationOk(int _PID,long _iconHandle);
    #ifndef BUILD_DLL
      #define IconIsInformationOk _IconIsInformationOk
    #endif

  /*============================================================================
     Documentation for: IconsRunBetween
    ============================================================================
       Attempts to advance the state from the begin indexed icon to the end 
       indexed icon.
  */
  int  FRAMES2API _IconsRunBetween(int _PID,long _iconBeginHandle,long _iconEndHandle);
    #ifndef BUILD_DLL
      #define IconsRunBetween _IconsRunBetween
    #endif

  /*============================================================================
     Documentation for: IconSetState
    ============================================================================
       Arbitrarily set the state of an icon given the icon handle.
  */
  void  FRAMES2API _IconSetState(int _PID,long _iconHandle,int _state,int _downstate);
    #ifndef BUILD_DLL
      #define IconSetState _IconSetState
    #endif

  /*============================================================================
     Documentation for: IconGetState
    ============================================================================
       Get the state of an icon given the icon handle.
  */
  int  FRAMES2API _IconGetState(int _PID,long _iconHandle);
    #ifndef BUILD_DLL
      #define IconGetState _IconGetState
    #endif

  /*============================================================================
     Documentation for: ToolLaunch
    ============================================================================
       Launches the specified module. The system flag indicates which set of 
       arguments to pass to module.
  */
  int  FRAMES2API _ToolLaunch(int _PID,long _moduleHandle,int _isSystemTool,long _iconHandle);
    #ifndef BUILD_DLL
      #define ToolLaunch _ToolLaunch
    #endif

  /*============================================================================
     Documentation for: Launcher
    ============================================================================
       Launches the specified module.
  */
  int  FRAMES2API _Launcher(char* _executableFile,char* _cmdLine,int _wait);
    #ifndef BUILD_DLL
      #define Launcher _Launcher
    #endif

  /*============================================================================
     Documentation for: DomainGetHandle
    ============================================================================
       Returns the handle of a domain.
  */
  long  FRAMES2API _DomainGetHandle(int _PID,char* _domainName);
    #ifndef BUILD_DLL
      #define DomainGetHandle _DomainGetHandle
    #endif

  /*============================================================================
     Documentation for: ClassGetHandle
    ============================================================================
       Returns the handle for a class.
  */
  long  FRAMES2API _ClassGetHandle(int _PID,long _domainHandle,char* _className);
    #ifndef BUILD_DLL
      #define ClassGetHandle _ClassGetHandle
    #endif

  /*============================================================================
     Documentation for: GroupGetHandle
    ============================================================================
       Returns the handle for a group.
  */
  long  FRAMES2API _GroupGetHandle(int _PID,long _domainHandle,long _classHandle,char* _groupName);
    #ifndef BUILD_DLL
      #define GroupGetHandle _GroupGetHandle
    #endif

  /*============================================================================
     Documentation for: SubGroupGetHandle
    ============================================================================
       Returns the handle for a subGroup.
  */
  long  FRAMES2API _SubGroupGetHandle(int _PID,long _domainHandle,long _classHandle,long _groupHandle,char* _subgroupName);
    #ifndef BUILD_DLL
      #define SubGroupGetHandle _SubGroupGetHandle
    #endif

  /*============================================================================
     Documentation for: HandleIsValid
    ============================================================================
       Return success if the handle is valid.
  */
  int  FRAMES2API _HandleIsValid(long _handleHandle);
    #ifndef BUILD_DLL
      #define HandleIsValid _HandleIsValid
    #endif

  /*============================================================================
     Documentation for: ModuleGetName
    ============================================================================
       Returns the name of the module given the module handle.
  */
  int  FRAMES2API _ModuleGetName(int _PID,long _moduleHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define ModuleGetName _ModuleGetName
    #endif

  /*============================================================================
     Documentation for: ModuleGetClass
    ============================================================================
       Returns the class type of the module given the module handle.
  */
  int  FRAMES2API _ModuleGetClass(int _PID,long _moduleHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define ModuleGetClass _ModuleGetClass
    #endif

  /*============================================================================
     Documentation for: SystemCreateProcessId
    ============================================================================
       Get process id for launching process
  */
  int  FRAMES2API _SystemCreateProcessId(int _PID,char* _iconName);
    #ifndef BUILD_DLL
      #define SystemCreateProcessId _SystemCreateProcessId
    #endif

  /*============================================================================
     Documentation for: SystemDeleteProcessId
    ============================================================================
       Delete pid generated by call to SystemCreateProcessId.
  */
  int  FRAMES2API _SystemDeleteProcessId(int _PID,int _newpid);
    #ifndef BUILD_DLL
      #define SystemDeleteProcessId _SystemDeleteProcessId
    #endif

  /*============================================================================
     Documentation for: ProcessGetSimulation
    ============================================================================
       Returns the name of the currently open simulation.
  */
  int  FRAMES2API _ProcessGetSimulation(int _PID,char* _retvalue);
    #ifndef BUILD_DLL
      #define ProcessGetSimulation _ProcessGetSimulation
    #endif

  /*============================================================================
     Documentation for: ProcessSetSimulation
    ============================================================================
       Sets the name of the simulation. The simulation name must already be 
       opened.
  */
  void  FRAMES2API _ProcessSetSimulation(int _PID,char* _simulationName);
    #ifndef BUILD_DLL
      #define ProcessSetSimulation _ProcessSetSimulation
    #endif

  /*============================================================================
     Documentation for: ProcessGetSimulationPath
    ============================================================================
       Returns the fully qualified file of the currently open simulation.
  */
  int  FRAMES2API _ProcessGetSimulationPath(int _PID,char* _retvalue);
    #ifndef BUILD_DLL
      #define ProcessGetSimulationPath _ProcessGetSimulationPath
    #endif

#ifdef __cplusplus
  }
#endif
#endif
