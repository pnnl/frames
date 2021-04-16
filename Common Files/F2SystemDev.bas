Attribute VB_Name = "F2SystemDevAPI"
Option Explicit
' API Name: FRAMES System API
'-------------------------------------------------------------------------
' Documentation for: F2OpenINI
' Open the INI and returns your PID. If the Startup.ini file does not exist path is the location where it will be created. The OpenINI function call is the first call made; if OpenINI is never called, all API calls are ignored. All changes to INI are persistent. The OpenINI function call is made with the file path as input (from command line), opens the INI, and returns the process id.
DECLARE Function F2OpenINI LIB "systemio.dll" Alias "__F2OpenINI@4" (  ByVal StartupFile as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2CloseINI
' Closes the entire frames session. If cancel is 0 all Startup.ini changes will be written. If cancel is 1 then no changes are written. The CloseINI function call is made with a process id as input and closes the entire FRAMES session.
DECLARE Sub F2CloseINI LIB "systemio.dll" Alias "__F2CloseINI@8" (  ByVal PID as long,  ByVal cancel as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2NewDictionary
' Create and add dictionary in registry, dictionary name will be set to the parameter dictionary. The dictionary file is expected to be empty. The NewDictionary function call is made with a process id and path string as input, and an empty dic string to output the dictionary name. This function will create and add a dictionary.
DECLARE Function F2NewDictionary LIB "systemio.dll" Alias "__F2NewDictionary@12" (  ByVal PID as long,  ByVal dictionaryFile as string,  ByVal dictionaryName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2OpenDictionary
' Open and add dictionary in registry, dictionary returns the dictionary name. The dictionary file is expected to be complete for this function to reach success (return a 0). The AddOpenDictionary function call is made with a process id and path string as input, and an empty dic string to output the dictionary name. This function will add a dictionary to the registry.
DECLARE Function F2OpenDictionary LIB "systemio.dll" Alias "__F2OpenDictionary@8" (  ByVal PID as long,  ByVal dictionaryFile as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2DeleteDictionary
' Delete a dictionary from FRAMES 2.0 Startup.ini. The DelDictionary function call is made with a process id and a dictionary name string as input to delete a dictionary from the registry.
DECLARE Sub F2DeleteDictionary LIB "systemio.dll" Alias "__F2DeleteDictionary@8" (  ByVal PID as long,  ByVal dictionaryHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2SaveDictionary
' Saves the contents for a dictionary to disk. The path is associated with the name when the dictionary was created or opened. The SaveDictionary function call is made with a process id and a dictionary name string as input to save a dictionary to the registry.
DECLARE Sub F2SaveDictionary LIB "systemio.dll" Alias "__F2SaveDictionary@8" (  ByVal PID as long,  ByVal dictionaryHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2SaveDictionaryAs
' Save as dictionary to disk but does not included it in the registry. The SaveDictionaryAs function call is made with a process id, a dictionary handle, a file string, and a new dictionary name string as input to save a dictionary not included in the registry.
DECLARE Sub F2SaveDictionaryAs LIB "systemio.dll" Alias "__F2SaveDictionaryAs@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal dictionaryFile as string,  ByVal dictionaryNewName as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetDictionaryCount
' Get the count of all dictionaries in registry. The DictionaryCount function call is made with a process id and a dictionary name string as input, and a count integer to output a count of all dictionaries in the registry.
DECLARE Function F2GetDictionaryCount LIB "systemio.dll" Alias "__F2GetDictionaryCount@4" (  ByVal PID as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetDictionaryName
' Get the name of a dictionary given the dictionary handle.
DECLARE Function DLLF2GetDictionaryName LIB "systemio.dll" Alias "__F2GetDictionaryName@12" (  ByVal PID as long,  ByVal dictionaryHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetDictionaryDescription
' Get the description of a dictionary given the dictionary handle.
DECLARE Function DLLF2GetDictionaryDescription LIB "systemio.dll" Alias "__F2GetDictionaryDescription@12" (  ByVal PID as long,  ByVal dictionaryHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2SetDictionaryDescription
' Set the description of a dictionary given the dictionary handle.
DECLARE Sub F2SetDictionaryDescription LIB "systemio.dll" Alias "__F2SetDictionaryDescription@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal description as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetDictionaryPrivilege
' Get the privilege of a dictionary given the dictionary handle. The privilege is either 0=Module UI, or 1=Boundary Condition. This represents the level of review a dictionary has received. A system dictionary should have a number of individuals agree that it is complete, useful and mutually exclusive of other system dictionaries. Returns the privilege of the dictionary
DECLARE Function F2GetDictionaryPrivilege LIB "systemio.dll" Alias "__F2GetDictionaryPrivilege@8" (  ByVal PID as long,  ByVal dictionaryHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2SetDictionaryPrivilege
' Set the privilege of a dictionary given the dictionary handle.
DECLARE Sub F2SetDictionaryPrivilege LIB "systemio.dll" Alias "__F2SetDictionaryPrivilege@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal privilege as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetDictionaryVersion
' Get the version of a dictionary given the dictionary handle. Returns the version of the dictionary
DECLARE Function F2GetDictionaryVersion LIB "systemio.dll" Alias "__F2GetDictionaryVersion@8" (  ByVal PID as long,  ByVal dictionaryHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2SetDictionaryVersion
' Set the version of a dictionary given the dictionary handle.
DECLARE Sub F2SetDictionaryVersion LIB "systemio.dll" Alias "__F2SetDictionaryVersion@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal version as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetDictionaryPath
' Get the path associated with a dictionary given the dictionary handle. To change the path you need to 1) SaveDictionaryAs changing the path, 2) DelDictionary, and 3) AddOpenDictionary from it new location. Returns the path to the dictionary.
DECLARE Function DLLF2GetDictionaryPath LIB "systemio.dll" Alias "__F2GetDictionaryPath@12" (  ByVal PID as long,  ByVal dictionaryHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetDictionaryUpdate
' If the state of a dictionary has changed then update flag will be non-zero. Returns the update flag of the dictionary.
DECLARE Function F2GetDictionaryUpdate LIB "systemio.dll" Alias "__F2GetDictionaryUpdate@8" (  ByVal PID as long,  ByVal dictionaryHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2AddVariable
' The AddVariable function call is made with a process id, a dictionary name string, and a variable string as input to add a variable to a dictionary. The function returns success/fail flag. This function must be called to allocate new space for a parameter. The parameter will be added to the dictionary or dataset by _dicId with the given _name. The parameter will have the default settings of: dimension = 0; description = ""; type = integer; stochastic = false; min = 0; max = 0.
DECLARE Function F2AddVariable LIB "systemio.dll" Alias "__F2AddVariable@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2DeleteVariable
' Delete a variable from a dictionary. Warning - Deleting a variable, dictionary, or a dataset can have some serious side effects therefore the software will not allow the use any these delete functions when more than one handle to a simulation exists. (i.e. A module and the system are active, or two system level users are active on a single instance of FRAMES. The DelVariable function call is made with a process id, a dictionary name string, and a variable string as input to delete a variable from a dictionary. The function returns success/fail flag.
DECLARE Sub F2DeleteVariable LIB "systemio.dll" Alias "__F2DeleteVariable@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetVariableCount
' Gets the varible count of a dictionary given the dictionary handle. The GetVarCount function call is made with a process id, a dictionary name string, and a count integer to output a count of all variables in the dictionary. Returns the count of the variables in the indexed dictionary.
DECLARE Function F2GetVariableCount LIB "systemio.dll" Alias "__F2GetVariableCount@8" (  ByVal PID as long,  ByVal dictionaryHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetVariableName
' Gets the name of the nth (index) variable in the dictionary. Returns the string variable name for the given index.
DECLARE Function DLLF2GetVariableName LIB "systemio.dll" Alias "__F2GetVariableName@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableIndex as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2SetVariableName
' Set the variable name (rename).The SetVarName function call is made with a process id, a dictionary name string, an existing variable name string, and a variable name string as input to change the variable name.
DECLARE Sub F2SetVariableName LIB "systemio.dll" Alias "__F2SetVariableName@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal variableNewName as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2SetVariableDescription
' Set the variable's description. The SetVarDescription function call is made with a process id, a dictionary name string, a variable name string, and a description string as input to change the description of the variable.
DECLARE Sub F2SetVariableDescription LIB "systemio.dll" Alias "__F2SetVariableDescription@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal description as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2SetVariableType
' Set the variable's type. Type can be either "String", "Logical", "Real", or "Integer". The SetVarType function call is made with a process id, a dictionary name string, a variable name string, and a type string as input to change the type parameter of the variable.
DECLARE Sub F2SetVariableType LIB "systemio.dll" Alias "__F2SetVariableType@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal idtype as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2SetVariableScalar
' Set the variable's scalar flag. If the flag is 0 then a set of values is stored. (i.e. the dimensionality will be increased by 1). If the flag=1 a single value is stored.
DECLARE Sub F2SetVariableScalar LIB "systemio.dll" Alias "__F2SetVariableScalar@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal flag as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2SetVariableMinimum
' Set the variable's minimum value. Setting both minimum and maximum equal to each other has the effect of having the variable NOT range checked.
DECLARE Sub F2SetVariableMinimum LIB "systemio.dll" Alias "__F2SetVariableMinimum@20" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal min as double ) 

'-------------------------------------------------------------------------
' Documentation for: F2SetVariableMaximum
' Set the variable's maximum value. Setting both minimum and maximum equal to each other has the effect of having the variable NOT range checked.
DECLARE Sub F2SetVariableMaximum LIB "systemio.dll" Alias "__F2SetVariableMaximum@20" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal max as double ) 

'-------------------------------------------------------------------------
' Documentation for: F2SetVariableMeasure
' Set the variable's measure.
DECLARE Sub F2SetVariableMeasure LIB "systemio.dll" Alias "__F2SetVariableMeasure@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal measureName as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2SetVariableUnit
' Set the variable's unit for its given measure.
DECLARE Sub F2SetVariableUnit LIB "systemio.dll" Alias "__F2SetVariableUnit@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal unitName as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2SetVariableStochastic
' Set the variable's stochastic flag. This flag signifies if the parameter can be modified by an another program and not invalidate any calibration. This allows other programs such and Sensativity/Uncertainty or Parameter estimation programs to modify these parameters.
DECLARE Sub F2SetVariableStochastic LIB "systemio.dll" Alias "__F2SetVariableStochastic@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal flag as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2SetVariablePreposition
' Set the preposition, this helps in describing parameters in a human readable form. This is the word that would best be used to write a description of this parameters as an index for another. For example the chemical parameter would use "for" as a preposition to make statements such as Concentration for benzene. The preposition greatly facilitates writing descriptive text associated with results. The SetVarPreposition function call is made with a process id, a dictionary name string, a variable name string, and a prep string as input to change the preposition parameter of the variable.
DECLARE Sub F2SetVariablePreposition LIB "systemio.dll" Alias "__F2SetVariablePreposition@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal prepositionName as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2SetVariablePrimaryKey
' Set the variable primarykey flag. Is this parameter a key to information on other parameters. This is used in both databases and in the representation of the data. For example a variable being used to index other variables frequently (like chemical) would indicate that chemical should be a primary key. The typical meaning of primary key from Database design is not incorrect when used here.
DECLARE Sub F2SetVariablePrimaryKey LIB "systemio.dll" Alias "__F2SetVariablePrimaryKey@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal flag as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2AddVariableIndex
' Add a variable index or label index to a variable. The AddVarIndex function call is made with a process id, a dictionary name string, a variable name string, a dictionary reference string, and an index string as input to add a variable index or label index to a variable.
DECLARE Sub F2AddVariableIndex LIB "systemio.dll" Alias "__F2AddVariableIndex@20" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal dictionaryName as string,  ByVal variableName as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2DeleteVariableIndex
' Delete an index from a variable. The DelVarIndex function call is made with a process id, a dictionary name string, a variable name string, and an index string as input to delete a variable index or label index to a variable.
DECLARE Sub F2DeleteVariableIndex LIB "systemio.dll" Alias "__F2DeleteVariableIndex@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal indexIndex as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetVariableIndexCount
' Get the count of indices for a variable. The GetVarIndexCount function call is made with a process id, a dictionary name string, and a variable name string as input, and an empty count integer to output a count of indices for a variable. Returns the count of the indexes for the variable.
DECLARE Function F2GetVariableIndexCount LIB "systemio.dll" Alias "__F2GetVariableIndexCount@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetVariableIndexIndex
' Get the index number for a index of a variable. It returns the index of a particular dictionary and variable.
DECLARE Function F2GetVariableIndexIndex LIB "systemio.dll" Alias "__F2GetVariableIndexIndex@20" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal dictionaryName as string,  ByVal variableName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetVariableIndexDictionary
' Get the index dictionary name of indices for a variable.
DECLARE Function DLLF2GetVariableIndexDictionary LIB "systemio.dll" Alias "__F2GetVariableIndexDictionary@20" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal indexIndex as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetVariableIndexVariable
' Get the index variable name of indices for a variable.
DECLARE Function DLLF2GetVariableIndexVariable LIB "systemio.dll" Alias "__F2GetVariableIndexVariable@20" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal indexIndex as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2PromoteVariableIndex
' Move and index up one level in the order of referred indices.
DECLARE Sub F2PromoteVariableIndex LIB "systemio.dll" Alias "__F2PromoteVariableIndex@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal indexIndex as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2DemoteVariableIndex
' Move and index down one level in the order of referred indices.
DECLARE Sub F2DemoteVariableIndex LIB "systemio.dll" Alias "__F2DemoteVariableIndex@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal indexIndex as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2NewModule
' Create and add a module description in registry module name passed in. The AddNewModule function call is made with a process id, a fully qualified file string, and a module name string as input to create and add a module description in the registry. It is expected that .mod file does not exist or is empty. Returns the index of the new module.
DECLARE Function F2NewModule LIB "systemio.dll" Alias "__F2NewModule@12" (  ByVal PID as long,  ByVal moduleFile as string,  ByVal moduleName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2OpenModule
' Open and add a module description in registry module name passed out. The AddOpenModule function call is made with a process id and a fully qualified file string as input to open and add a module description in the registry, and a module name string to output the module name. Returns the index of the registered module.
DECLARE Function F2OpenModule LIB "systemio.dll" Alias "__F2OpenModule@8" (  ByVal PID as long,  ByVal moduleFile as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetModuleName
' Returns name of module.
DECLARE Function DLLF2GetModuleName LIB "systemio.dll" Alias "__F2GetModuleName@12" (  ByVal PID as long,  ByVal moduleHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2DeleteModule
' Delete a module description dataset from registry. The DelModule function call is made with a process id and a module name string as input to delete a module description dataset from the registry.
DECLARE Sub F2DeleteModule LIB "systemio.dll" Alias "__F2DeleteModule@8" (  ByVal PID as long,  ByVal moduleHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2SaveModule
' Save the module description dataset to registry. The SaveModule function call is made with a process id and a module name string as input to save a module description dataset in the registry.
DECLARE Sub F2SaveModule LIB "systemio.dll" Alias "__F2SaveModule@8" (  ByVal PID as long,  ByVal moduleHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2SaveModuleAs
' Save the module description as another dataset to registry. The SaveModuleAs function call is made with a process id, a handle module, a fully qualified file string, and a new module name string as input to save the module description as another module dataset in the registry. The old module handle continues to be the open module.
DECLARE Sub F2SaveModuleAs LIB "systemio.dll" Alias "__F2SaveModuleAs@16" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal moduleFile as string,  ByVal moduleNewName as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetModuleCount
' Get the count of all modules in registry. Returns the number of registered modules.
DECLARE Function F2GetModuleCount LIB "systemio.dll" Alias "__F2GetModuleCount@4" (  ByVal PID as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetModulePath
' Get module path associated with a module. Returns the fully qualified file of the module.
DECLARE Function DLLF2GetModulePath LIB "systemio.dll" Alias "__F2GetModulePath@12" (  ByVal PID as long,  ByVal moduleHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2AddScheme
' Add a scheme to a module. Returns the index of the new scheme.
DECLARE Function F2AddScheme LIB "systemio.dll" Alias "__F2AddScheme@12" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal schemeName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2DeleteScheme
' Delete a scheme from a module.
DECLARE Sub F2DeleteScheme LIB "systemio.dll" Alias "__F2DeleteScheme@12" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal schemeHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetSchemeCount
' Get the number of the schemes for a module. Returns number of schemes for a module.
DECLARE Function F2GetSchemeCount LIB "systemio.dll" Alias "__F2GetSchemeCount@8" (  ByVal PID as long,  ByVal moduleHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetSchemeName
' Gets the name of the nth (index) scheme of the module. Returns the string scheme name for the given index.
DECLARE Function DLLF2GetSchemeName LIB "systemio.dll" Alias "__F2GetSchemeName@16" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal schemeHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2AddSchemeInputDictionary
' Add a dictionary to a scheme for a module. Returns the index of the newly added dictionary to the scheme.
DECLARE Sub F2AddSchemeInputDictionary LIB "systemio.dll" Alias "__F2AddSchemeInputDictionary@16" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal schemeHandle as long,  ByVal dictionaryHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2DeleteSchemeInputDictionary
' Delete a dictionary from a scheme for a module.
DECLARE Sub F2DeleteSchemeInputDictionary LIB "systemio.dll" Alias "__F2DeleteSchemeInputDictionary@16" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal schemeHandle as long,  ByVal dictionaryHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2AddSchemeOutputDictionary
' Add an output dictionary to a scheme for a module. Returns the index of the newly added output dictionary.
DECLARE Sub F2AddSchemeOutputDictionary LIB "systemio.dll" Alias "__F2AddSchemeOutputDictionary@16" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal schemeHandle as long,  ByVal dictionaryHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2DeleteSchemeOutputDictionary
' Delete an output dictionary from a scheme for a module.
DECLARE Sub F2DeleteSchemeOutputDictionary LIB "systemio.dll" Alias "__F2DeleteSchemeOutputDictionary@16" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal schemeHandle as long,  ByVal dictionaryHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetModuleVariableCount
' Get the number of variables in a modueles dataset..
DECLARE Function F2GetModuleVariableCount LIB "systemio.dll" Alias "__F2GetModuleVariableCount@8" (  ByVal PID as long,  ByVal moduleHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetModuleVariableInteger
' Get an integer from modules variable data. Returns the integer that was retrieved.
DECLARE Function F2GetModuleVariableInteger LIB "systemio.dll" Alias "__F2GetModuleVariableInteger@20" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetModuleVariableDouble
' Get an real from modules variable data. Returns the real that was retrieved.
DECLARE Function F2GetModuleVariableDouble LIB "systemio.dll" Alias "__F2GetModuleVariableDouble@20" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any )  as double 

'-------------------------------------------------------------------------
' Documentation for: F2GetModuleVariableLogical
' Get an logical from modules variable data. Returns the logical that was retrieved.
DECLARE Function F2GetModuleVariableLogical LIB "systemio.dll" Alias "__F2GetModuleVariableLogical@20" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetModuleVariableString
' Get an string from modules variable data. Returns the string that was retrieved.
DECLARE Function DLLF2GetModuleVariableString LIB "systemio.dll" Alias "__F2GetModuleVariableString@24" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2SetModuleVariableInteger
' Put an integer of meta data about a module.
DECLARE Sub F2SetModuleVariableInteger LIB "systemio.dll" Alias "__F2SetModuleVariableInteger@24" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2SetModuleVariableDouble
' Put a double of meta data about a module.
DECLARE Sub F2SetModuleVariableDouble LIB "systemio.dll" Alias "__F2SetModuleVariableDouble@28" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: F2SetModuleVariableLogical
' Put an logical of meta data about a module.
DECLARE Sub F2SetModuleVariableLogical LIB "systemio.dll" Alias "__F2SetModuleVariableLogical@24" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2SetModuleVariableString
' Put a string of meta data about a module.
DECLARE Sub F2SetModuleVariableString LIB "systemio.dll" Alias "__F2SetModuleVariableString@24" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2AddDomain
' Automatically adds the four class types to the domain. The AddDomain function call is made with a process id and a domain name string as input to add a domain; automatically adds the four class types to the domain. Returns the index of the added Domain.
DECLARE Function F2AddDomain LIB "systemio.dll" Alias "__F2AddDomain@8" (  ByVal PID as long,  ByVal domainName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2DeleteDomain
' Deletes the domain and it tree. The DelDomain function call is made with a process id and a domain name string as input to delete a domain and its tree, deleting its classes, groups, subgroups, and modules.
DECLARE Sub F2DeleteDomain LIB "systemio.dll" Alias "__F2DeleteDomain@8" (  ByVal PID as long,  ByVal domainHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetDomainIcon
' Get the icon associated with a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels. Return the fully qualified file for a module icon.
DECLARE Function DLLF2GetDomainIcon LIB "systemio.dll" Alias "__F2GetDomainIcon@12" (  ByVal PID as long,  ByVal domainHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2SetDomainIcon
' Set the icon associated with a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels.
DECLARE Sub F2SetDomainIcon LIB "systemio.dll" Alias "__F2SetDomainIcon@12" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal domainIconFile as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2AddClass
' For future use - not presently active. The AddClass function call is made with a process id, a domain name string, and a class name string as input to add a class. Returns the index of the newly added class.
DECLARE Function F2AddClass LIB "systemio.dll" Alias "__F2AddClass@12" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal className as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2DeleteClass
' For future use - not presently active. The DelClass function call is made with a process id, a domain name string, and a class name string as input to delete a class from a domain, including its groups, subgroups, and modules.
DECLARE Sub F2DeleteClass LIB "systemio.dll" Alias "__F2DeleteClass@12" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetClassIcon
' Get the icon associated with a class in a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels. Returns the fully qualified file to the class icon.
DECLARE Function DLLF2GetClassIcon LIB "systemio.dll" Alias "__F2GetClassIcon@16" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2SetClassIcon
' Set the icon associated with a class in a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels.
DECLARE Sub F2SetClassIcon LIB "systemio.dll" Alias "__F2SetClassIcon@16" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal classIconFile as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2AddGroup
' The AddGroup function call is made with a process id, a domain name string, a class name string, and a group name string as input to add a group to a domain and class. Returns the index of the newly added group.
DECLARE Function F2AddGroup LIB "systemio.dll" Alias "__F2AddGroup@16" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2DeleteGroup
' The DelGroup function call is made with a process id, a domain name string, a class name string, and a group name string as input to delete a group from a domain and class, including its subgroups and modules.
DECLARE Sub F2DeleteGroup LIB "systemio.dll" Alias "__F2DeleteGroup@16" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetGroupIcon
' Get the icon associated with a class in a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels. Returns the fully qualified file to the group icon.
DECLARE Function DLLF2GetGroupIcon LIB "systemio.dll" Alias "__F2GetGroupIcon@20" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2SetGroupIcon
' Set the icon associated with a class in a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels.
DECLARE Sub F2SetGroupIcon LIB "systemio.dll" Alias "__F2SetGroupIcon@20" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal groupIconFile as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2AddSubGroup
' The AddSubGroup function call is made with a process id, a domain name string, a class name string, a group name string, and a subgroup name string as input to add a subgroup to a domain, class, and group. Return the index of the newly added subgroup.
DECLARE Function F2AddSubGroup LIB "systemio.dll" Alias "__F2AddSubGroup@20" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2DeleteSubGroup
' The DelSubGroup function call is made with a process id, a domain name string, a class name string, a group name string, and a subgroup name string as input to delete a subgroup from a domain, class, and group, including its modules.
DECLARE Sub F2DeleteSubGroup LIB "systemio.dll" Alias "__F2DeleteSubGroup@20" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetSubGroupIcon
' Get the icon associated with a class in a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels. Returns the fully qualified file to the subgroup icon.
DECLARE Function DLLF2GetSubGroupIcon LIB "systemio.dll" Alias "__F2GetSubGroupIcon@24" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2SetSubGroupIcon
' Set the icon associated with a class in a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels.
DECLARE Sub F2SetSubGroupIcon LIB "systemio.dll" Alias "__F2SetSubGroupIcon@24" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupHandle as long,  ByVal subgroupIconFile as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2AddGroupModule
' Add a module at the group level. The AddGroupModule function call is made with a process id, a domain name string, a class name string, a group name string, and a module name string as input to add a module to a domain, class, and group. Returns the index of the newly added module.
DECLARE Sub F2AddGroupModule LIB "systemio.dll" Alias "__F2AddGroupModule@20" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal moduleHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2DeleteGroupModule
' Remove a module at the group level. The DelGroupModule function call is made with a process id, a domain name string, a class name string, a group name string, and a module name string as input to delete a module from a domain, class, and group.
DECLARE Sub F2DeleteGroupModule LIB "systemio.dll" Alias "__F2DeleteGroupModule@20" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal moduleHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2AddSubGroupModule
' Add a module at the subgroup level. The AddSubGrpModule function call is made with a process id, a domain name string, a class name string, a group name string, a subgroup name string, and a module name string as input to add a module to a domain, class, group, and subgroup. Returns the index of the newly added subgroup module.
DECLARE Sub F2AddSubGroupModule LIB "systemio.dll" Alias "__F2AddSubGroupModule@24" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupHandle as long,  ByVal moduleHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2DeleteSubGroupModule
' Remove a module at the subgroup level. The DelSubGrpModule function call is made with a process id, a domain name string, a class name string, a group name string, a subgroup name string, and module name string as input to delete a module from a domain, class, group, and subgroup.
DECLARE Sub F2DeleteSubGroupModule LIB "systemio.dll" Alias "__F2DeleteSubGroupModule@24" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupHandle as long,  ByVal moduleHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetDomainName
' Gets the domain name.
DECLARE Function DLLF2GetDomainName LIB "systemio.dll" Alias "__F2GetDomainName@12" (  ByVal PID as long,  ByVal domainHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetClassName
' Gets the class name.
DECLARE Function DLLF2GetClassName LIB "systemio.dll" Alias "__F2GetClassName@16" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetGroupName
' Gets the group name.
DECLARE Function DLLF2GetGroupName LIB "systemio.dll" Alias "__F2GetGroupName@20" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetSubGroupName
' Gets the sub group name.
DECLARE Function DLLF2GetSubGroupName LIB "systemio.dll" Alias "__F2GetSubGroupName@24" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetDomainCount
' Get the number of domains in the registery. The GetDomainCount function call is made with a process id as input, and an empty count integer to output a count of all domains in the registry. Returns the number of the domains that are registered.
DECLARE Function F2GetDomainCount LIB "systemio.dll" Alias "__F2GetDomainCount@4" (  ByVal PID as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetClassCount
' Get the number of classes in a domain. The GetClassCount function call is made with a process id and a domain name string as input, and an empty count integer to output a count of all classes in a domain. Returns the number of classes in a domain.
DECLARE Function F2GetClassCount LIB "systemio.dll" Alias "__F2GetClassCount@8" (  ByVal PID as long,  ByVal domainHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetGroupCount
' Get the number of groups in a class, and domain. The GetGroupCount function call is made with a process id, a domain name string, and a class name string as input, and an empty count integer to output a count of all groups in a domain and class. Returns the number of groups in a class in a domain.
DECLARE Function F2GetGroupCount LIB "systemio.dll" Alias "__F2GetGroupCount@12" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetSubGroupCount
' Get the number of subgroups in a group, class and domain.The GetSubGrpCount function call is made with a process id, a domain name string, a class name string, and a group name string as input, and an empty count integer to output a count of all subgroups in a domain, class, and group. Returns the number of subgroups in a group in a class in a domain.
DECLARE Function F2GetSubGroupCount LIB "systemio.dll" Alias "__F2GetSubGroupCount@16" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetGroupModuleCount
' Get the number of modules at the group level for a group, class, and domain. The GetGroupModCount function call is made with a process id, a domain name string, a class name string, and a group name string as input, and an empty count integer to output a count of all modules in a domain, class, and group. Returns the number of modules in a group.
DECLARE Function F2GetGroupModuleCount LIB "systemio.dll" Alias "__F2GetGroupModuleCount@16" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetSubGroupModuleCount
' Get the number of modules at the subgroup level for a subgroup, group, class, and domain. The GetSubGrpModCount function call is made with a process id, a domain name string, a class name string, a group name string, and a subgroup name string as input, and an empty count integer to output a count of all modules in a domain, class, group, and subgroup. Returns the number of modules in a subgroup.
DECLARE Function F2GetSubGroupModuleCount LIB "systemio.dll" Alias "__F2GetSubGroupModuleCount@20" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2NewSimulation
' Open a new simulation return a simulation dataset name. The NewSim function call is made with a process id and a filename string as input to open a new simulation. Only one simulation is allowed to be open at a time in a FRAMES session, however, multiple FRAMES sessions may be open at the same time. Returns the index of the newly created simulation file.
DECLARE Function F2NewSimulation LIB "systemio.dll" Alias "__F2NewSimulation@12" (  ByVal PID as long,  ByVal simulationFile as string,  ByVal simulationName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2OpenSimulation
' Open an existing simulation return a simulation dataset name. The OpenSim function call is made with a process id and a filename string as input to open an existing simulation. Returns the index of the opened simulation file.
DECLARE Function F2OpenSimulation LIB "systemio.dll" Alias "__F2OpenSimulation@8" (  ByVal PID as long,  ByVal simulationFile as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2SaveSimulation
' Save the simulation present state. The SaveSim function call is made with a process id as input to save the simulation's present state.
DECLARE Sub F2SaveSimulation LIB "systemio.dll" Alias "__F2SaveSimulation@4" (  ByVal PID as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2SaveSimulationAs
' Open an exsisting simulation return a simId. The SaveSimAs function call is made with a process id, a fully quailfied path and a filename string (new name) as input to save a simulation (SDE) as another name, but continue to work in same simulation name.
DECLARE Sub F2SaveSimulationAs LIB "systemio.dll" Alias "__F2SaveSimulationAs@12" (  ByVal PID as long,  ByVal simulationFile as string,  ByVal simulationNewName as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2CloseSimulation
' Close the simulation without saving. The CloseSim function call is made with a process id as input to close the simulation without saving.
DECLARE Sub F2CloseSimulation LIB "systemio.dll" Alias "__F2CloseSimulation@4" (  ByVal PID as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetSimulation
' Get current simulation name for which all simulation functions will use for this PID. Returns the current simulation name.
DECLARE Function DLLF2GetSimulation LIB "systemio.dll" Alias "__F2GetSimulation@8" (  ByVal PID as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2SetSimulation
' Set current simulation name for which all simulation functions will use for this PID.
DECLARE Sub F2SetSimulation LIB "systemio.dll" Alias "__F2SetSimulation@8" (  ByVal PID as long,  ByVal simulationName as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2AddNewDataSet
' Create a new dataset using a registered dictionary. The AddNewDataSet function call is made with a process id, a dictionary handle, a fully qualified file string, and a set string as input to create a new dataset using a registered dictionary. Returns the index of the new dataset.
DECLARE Function F2AddNewDataSet LIB "systemio.dll" Alias "__F2AddNewDataSet@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal datasetFile as string,  ByVal datasetName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2AddOpenDataSet
' Open a dataset, dataset file must exist or an error will occur. The AddOpenDataSet function call is made with a process id, a fully qualified file name string, and a set string as input to open an existing dataset to the registry. Returns the index of the new dataset.
DECLARE Function F2AddOpenDataSet LIB "systemio.dll" Alias "__F2AddOpenDataSet@8" (  ByVal PID as long,  ByVal datasetFile as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2DeleteDataSet
' Delete a dataset. The function call is made with a process id and a set string as input to delete a dataset. Does not write the dataset before deletion.
DECLARE Sub F2DeleteDataSet LIB "systemio.dll" Alias "__F2DeleteDataSet@8" (  ByVal PID as long,  ByVal datasetHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2SaveDataSet
' Save the dataset. The function call is made with a process id and a set string as input to save a dataset.
DECLARE Sub F2SaveDataSet LIB "systemio.dll" Alias "__F2SaveDataSet@8" (  ByVal PID as long,  ByVal datasetHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2SaveDataSetAs
' Save the dataset with a different name. The SaveDataSetAs function call is made with a process id, a dataset handle, a fully qualified file name string, and a new Set string as input to save a dataset under a new name.
DECLARE Sub F2SaveDataSetAs LIB "systemio.dll" Alias "__F2SaveDataSetAs@16" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal datasetFile as string,  ByVal datasetNewName as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetDataSetName
' Get dataset name.
DECLARE Function DLLF2GetDataSetName LIB "systemio.dll" Alias "__F2GetDataSetName@12" (  ByVal PID as long,  ByVal datasetHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetDataSetPath
' Get the path associated with a dictionary. To change the path you need to 1) SaveDictionaryAs changing the path, 2) DelDictionary, and 3) AddOpenDictionary from it new location. Returns the fully qualified file.
DECLARE Function DLLF2GetDataSetPath LIB "systemio.dll" Alias "__F2GetDataSetPath@12" (  ByVal PID as long,  ByVal datasetHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2AddLink
' The AddLink function call is made with a process id, a modIdFrom string, and a modIdTo string as input to connect two modules, and returns a success/fail flag. The arrow of the connection will point from -> to. Returns the link index of the new connection.
DECLARE Sub F2AddLink LIB "systemio.dll" Alias "__F2AddLink@12" (  ByVal PID as long,  ByVal fromIconHandle as long,  ByVal toIconHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2DeleteLink
' The DelLink function call is made with a process id, a modIdFrom string, and a modIdTo string as input to delete the connection between two modules, and returns a success/fail flag.
DECLARE Sub F2DeleteLink LIB "systemio.dll" Alias "__F2DeleteLink@12" (  ByVal PID as long,  ByVal fromIconHandle as long,  ByVal toIconHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2AddIcon
' Returns the module ID added to the simulation. If scope is 0 then the icon is added to the global diagram. If scope is non zero it is added to the local diagram. Returns the module index for the added module instance (icon in the drawing space).
DECLARE Function F2AddIcon LIB "systemio.dll" Alias "__F2AddIcon@24" (  ByVal PID as long,  ByVal scope as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupHandle as long )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2DeleteIcon
' Deletes the module ID from the simulation. The function call is made with a process id and a id string as input to delete the module ID from the simulation.
DECLARE Sub F2DeleteIcon LIB "systemio.dll" Alias "__F2DeleteIcon@8" (  ByVal PID as long,  ByVal iconHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetIconName
' Get icon name.
DECLARE Function DLLF2GetIconName LIB "systemio.dll" Alias "__F2GetIconName@12" (  ByVal PID as long,  ByVal iconHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetIconUIDictionary
' Returns the dictionary index for the module icon returns.
DECLARE Function DLLF2GetIconUIDictionary LIB "systemio.dll" Alias "__F2GetIconUIDictionary@12" (  ByVal PID as long,  ByVal iconHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetIconUIDataSet
' Returns the dataset index for the module icon.
DECLARE Function DLLF2GetIconUIDataSet LIB "systemio.dll" Alias "__F2GetIconUIDataSet@12" (  ByVal PID as long,  ByVal iconHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetIconModule
' Gets module and scheme associated with an icon. A list of datasets is returned in the list parameter. Returns the module for an icon.
DECLARE Function DLLF2GetIconModule LIB "systemio.dll" Alias "__F2GetIconModule@12" (  ByVal PID as long,  ByVal iconHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetIconScheme
' Gets module and scheme associated with an icon. A list of datasets is returned in the list parameter. Returns the scheme for an icon.
DECLARE Function DLLF2GetIconScheme LIB "systemio.dll" Alias "__F2GetIconScheme@12" (  ByVal PID as long,  ByVal iconHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2SetIconModule
' Sets the dataset names for the module and scheme associated with the indicated icon. A subset of the datasets the icon consumes is given in the list parameter.
DECLARE Sub F2SetIconModule LIB "systemio.dll" Alias "__F2SetIconModule@24" (  ByVal PID as long,  ByVal iconHandle as long,  ByVal moduleHandle as long,  ByVal schemeHandle as long,  ByVal delimiter as string,  ByVal list as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetDictionaryHandle
' Get the number of the schemes for a module. Returns number of schemes for a module.
DECLARE Function F2GetDictionaryHandle LIB "systemio.dll" Alias "__F2GetDictionaryHandle@8" (  ByVal PID as long,  ByVal dictionaryName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetVariableHandle
' Finds the index of a variable in a dictionary.
DECLARE Function F2GetVariableHandle LIB "systemio.dll" Alias "__F2GetVariableHandle@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetModuleHandle
' Finds the index of a currently registered module.
DECLARE Function F2GetModuleHandle LIB "systemio.dll" Alias "__F2GetModuleHandle@8" (  ByVal PID as long,  ByVal moduleName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetSchemeHandle
' Get the number of the schemes for a module. Returns number of schemes for a module.
DECLARE Function F2GetSchemeHandle LIB "systemio.dll" Alias "__F2GetSchemeHandle@12" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal schemeName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetModuleVariableHandle
' Get the for index a variable name associate with the module meta data. Returns the index of the variable name.
DECLARE Function F2GetModuleVariableHandle LIB "systemio.dll" Alias "__F2GetModuleVariableHandle@12" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetClassModuleHandle
' Gets a modules index at the class level. Returns the index to the module.
DECLARE Function F2GetClassModuleHandle LIB "systemio.dll" Alias "__F2GetClassModuleHandle@16" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal moduleName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetDomainHandle
' Returns the index of the given domain name.
DECLARE Function F2GetDomainHandle LIB "systemio.dll" Alias "__F2GetDomainHandle@8" (  ByVal PID as long,  ByVal domainName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetClassHandle
' Returns the index of the given class name.
DECLARE Function F2GetClassHandle LIB "systemio.dll" Alias "__F2GetClassHandle@12" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal className as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetGroupHandle
' Returns the index of the given group.
DECLARE Function F2GetGroupHandle LIB "systemio.dll" Alias "__F2GetGroupHandle@16" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetSubGroupHandle
' Returns the index of the given subgroup.
DECLARE Function F2GetSubGroupHandle LIB "systemio.dll" Alias "__F2GetSubGroupHandle@20" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetGroupModuleHandle
' Returns the index of the given module for a group.
DECLARE Function F2GetGroupModuleHandle LIB "systemio.dll" Alias "__F2GetGroupModuleHandle@20" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal moduleName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetSubGroupModuleHandle
' Returns the index of the given module for a subgroup.
DECLARE Function F2GetSubGroupModuleHandle LIB "systemio.dll" Alias "__F2GetSubGroupModuleHandle@24" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupHandle as long,  ByVal moduleName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetSimulationHandle
' Get the index for the given simulation name.
DECLARE Function F2GetSimulationHandle LIB "systemio.dll" Alias "__F2GetSimulationHandle@8" (  ByVal PID as long,  ByVal simulationName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetDataSetHandle
' Returns the index of each dataset.
DECLARE Function F2GetDataSetHandle LIB "systemio.dll" Alias "__F2GetDataSetHandle@8" (  ByVal PID as long,  ByVal datasetName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetIconHandle
' Get dataset name.
DECLARE Function F2GetIconHandle LIB "systemio.dll" Alias "__F2GetIconHandle@8" (  ByVal PID as long,  ByVal iconName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2CreateProcessId
' Get process id for launching process
DECLARE Function F2CreateProcessId LIB "systemio.dll" Alias "__F2CreateProcessId@8" (  ByVal PID as long,  ByVal iconName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2DeleteProcessId
' Delete pid generated by call to F2CreateProcessId.
DECLARE Function F2DeleteProcessId LIB "systemio.dll" Alias "__F2DeleteProcessId@8" (  ByVal PID as long,  ByVal newpid as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2CountDataSetsChanged
' Count datasets changed due to UI or Model runs.
DECLARE Function F2CountDataSetsChanged LIB "systemio.dll" Alias "__F2CountDataSetsChanged@12" (  ByVal PID as long,  ByVal iconHandle as long,  ByVal mode as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SystemOpen
' Returns a Process IDentification (PID). If the PID returned is a negative number an error has occured. Gives system developer level access to FRAMES by taking in the fully qualified path to the FRAMES direcory, and then opening "Startup.ini" and returning a PID. The PID is used in all subsequent calls. If the Startup.ini file does not exist in the path provided, it will be created. This function must be the first API call and if it is never called, all API calls will be ignored by FRAMES.
DECLARE Function SystemOpen LIB "systemio.dll" Alias "__SystemOpen@4" (  ByVal StartupFile as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SystemClose
' Close the system developer link to the FRAMES system. If the cancel flag is 0 then the updated datasets are saved. Otherwise the updated datasets are thrown away.
DECLARE Sub SystemClose LIB "systemio.dll" Alias "__SystemClose@8" (  ByVal PID as long,  ByVal cancel as long ) 

'-------------------------------------------------------------------------
' Documentation for: SystemSave
' Save system objects to file.
DECLARE Sub SystemSave LIB "systemio.dll" Alias "__SystemSave@4" (  ByVal PID as long ) 

'-------------------------------------------------------------------------
' Documentation for: SystemSaveToDB
' Save system objects to database.
DECLARE Sub SystemSaveToDB LIB "systemio.dll" Alias "__SystemSaveToDB@4" (  ByVal PID as long ) 

'-------------------------------------------------------------------------
' Documentation for: SystemSaveToFile
' Save system objects to file.
DECLARE Sub SystemSaveToFile LIB "systemio.dll" Alias "__SystemSaveToFile@4" (  ByVal PID as long ) 

'-------------------------------------------------------------------------
' Documentation for: SystemAddDictionary
' Returns 0 if successful and a negative number if an error has occured. Creates and adds a dictionary into the FRAMES system registry. The dictionary file is expected to be empty. The call is made with a fully qualified path to where the dictionary is to be saved and the name of the dictionary.
DECLARE Function SystemAddDictionary LIB "systemio.dll" Alias "__SystemAddDictionary@12" (  ByVal PID as long,  ByVal dictionaryFile as string,  ByVal dictionaryName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: SystemOpenDictionary
' Open a dictionary file and add it to the FRAMES system registry. If a negative value is returned, an error as occured. The dictionay file is expected to be a complete dictionary file.
DECLARE Function SystemOpenDictionary LIB "systemio.dll" Alias "__SystemOpenDictionary@8" (  ByVal PID as long,  ByVal dictionaryFile as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: SystemCloseDictionary
' Closes an open dictionay. The dictionary file is not deleted, but the dictionary is unregistered from the FRAMES system.
DECLARE Sub SystemCloseDictionary LIB "systemio.dll" Alias "__SystemCloseDictionary@8" (  ByVal PID as long,  ByVal dictionaryHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SystemSaveDictionary
' Saves the contents for a dictionary to disk. The dictionary file is saved to the fully qualified path associated with the dictionary when it was created or opened.
DECLARE Sub SystemSaveDictionary LIB "systemio.dll" Alias "__SystemSaveDictionary@8" (  ByVal PID as long,  ByVal dictionaryHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SystemSaveDictionaryToFile
' Saves the contents for a dictionary to disk. The dictionary file is saved to the fully qualified path associated with the dictionary when it was created or opened.
DECLARE Sub SystemSaveDictionaryToFile LIB "systemio.dll" Alias "__SystemSaveDictionaryToFile@8" (  ByVal PID as long,  ByVal dictionaryHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SystemSaveDictionaryToDB
' Saves the contents for a dictionary to disk. The dictionary file is saved to the fully qualified path associated with the dictionary when it was created or opened.
DECLARE Sub SystemSaveDictionaryToDB LIB "systemio.dll" Alias "__SystemSaveDictionaryToDB@8" (  ByVal PID as long,  ByVal dictionaryHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SystemSaveDictionaryAs
' Save the contents of a dictionary to disk in a different location than specified in the creation or opening of the dictionary. Does not register the new file with the FRAMES system. The dictionary must first be closed and then reopened for the name change to take place.
DECLARE Sub SystemSaveDictionaryAs LIB "systemio.dll" Alias "__SystemSaveDictionaryAs@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal dictionaryFile as string,  ByVal dictionaryNewName as string ) 

'-------------------------------------------------------------------------
' Documentation for: SystemDictionaryCount
' Returns the number of dictionaries in the FRAMES system.
DECLARE Function SystemDictionaryCount LIB "systemio.dll" Alias "__SystemDictionaryCount@4" (  ByVal PID as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SystemAddModule
' Returns the index of the new module. Creates and adds a module description into the FRAMES system registry. It is expected that the .mod file does not exist or is empty.
DECLARE Function SystemAddModule LIB "systemio.dll" Alias "__SystemAddModule@12" (  ByVal PID as long,  ByVal moduleFile as string,  ByVal moduleName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: SystemOpenModule
' Returns the index of the open module. Opens and adds a module description into the FRAMES system registry. It is expected that the .mod file exists.
DECLARE Function SystemOpenModule LIB "systemio.dll" Alias "__SystemOpenModule@8" (  ByVal PID as long,  ByVal moduleFile as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: SystemCloseModule
' Closes a module file and removes the module reference from the FRAMES system registry. Does not delete the module file if the file was saved to disk.
DECLARE Sub SystemCloseModule LIB "systemio.dll" Alias "__SystemCloseModule@8" (  ByVal PID as long,  ByVal moduleHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SystemSaveModule
' Saves the module to the FRAMES system registry.
DECLARE Sub SystemSaveModule LIB "systemio.dll" Alias "__SystemSaveModule@8" (  ByVal PID as long,  ByVal moduleHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SystemSaveModuleToFile
' Saves the module to the FRAMES system registry.
DECLARE Sub SystemSaveModuleToFile LIB "systemio.dll" Alias "__SystemSaveModuleToFile@8" (  ByVal PID as long,  ByVal moduleHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SystemSaveModuleToDB
' Saves the module to the FRAMES system registry.
DECLARE Sub SystemSaveModuleToDB LIB "systemio.dll" Alias "__SystemSaveModuleToDB@8" (  ByVal PID as long,  ByVal moduleHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SystemSaveModuleAs
' Saves the module to the FRAMES system registry as a different module name. Does not register the new file with the FRAMES system. The module must first be closed and then reopened for the name change to take place.
DECLARE Sub SystemSaveModuleAs LIB "systemio.dll" Alias "__SystemSaveModuleAs@16" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal moduleFile as string,  ByVal moduleNewName as string ) 

'-------------------------------------------------------------------------
' Documentation for: SystemModuleCount
' Get the count of all modules in registry. Returns the number of registered modules.
DECLARE Function SystemModuleCount LIB "systemio.dll" Alias "__SystemModuleCount@4" (  ByVal PID as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DictionarySetDescription
' Set the dictionary description specified by the provided dictionary handle.
DECLARE Sub DictionarySetDescription LIB "systemio.dll" Alias "__DictionarySetDescription@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal description as string ) 

'-------------------------------------------------------------------------
' Documentation for: DictionarySetPrivilege
' Set the privilege of a dictionary specified by dictionary handle.
DECLARE Sub DictionarySetPrivilege LIB "systemio.dll" Alias "__DictionarySetPrivilege@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal privilege as long ) 

'-------------------------------------------------------------------------
' Documentation for: DictionarySetVersion
' Set the version of a dictionary.
DECLARE Sub DictionarySetVersion LIB "systemio.dll" Alias "__DictionarySetVersion@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal version as long ) 

'-------------------------------------------------------------------------
' Documentation for: DictionaryAddVariable
' Adds a variable to a dictionary. Returns the variable handle if successful, a negative value for failure. This function must be called to allocate new space for a variable. The variable will be added to the dictionary with the given handle. The variable will have the default settings of: dimension = 0; description = ""; type = integer; stochastic = false; min = 0; max = 0.
DECLARE Function DictionaryAddVariable LIB "systemio.dll" Alias "__DictionaryAddVariable@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: DictionaryDeleteVariable
' Delete a variable from a dictionary specified by the dictionary handle and the variable handle. Warning - Deleting a variable, dictionary, or a dataset can have some serious side effects therefore the software will not allow the use any these delete functions when more than one handle to a simulation exists. (i.e. A module and the system are active, or two system level users are active on a single instance of FRAMES.
DECLARE Sub DictionaryDeleteVariable LIB "systemio.dll" Alias "__DictionaryDeleteVariable@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: DictionaryVariableCount
' Returns the number of variables in the dictionary specified by the dictionary handle.
DECLARE Function DictionaryVariableCount LIB "systemio.dll" Alias "__DictionaryVariableCount@8" (  ByVal PID as long,  ByVal dictionaryHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableSetName
' Set a variable's name. If referenced by another variable as an index an error occurs.
DECLARE Sub VariableSetName LIB "systemio.dll" Alias "__VariableSetName@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal variableNewName as string ) 

'-------------------------------------------------------------------------
' Documentation for: VariableSetDescription
' Set a variable's description.
DECLARE Sub VariableSetDescription LIB "systemio.dll" Alias "__VariableSetDescription@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal description as string ) 

'-------------------------------------------------------------------------
' Documentation for: VariableSetType
' Sets a variable's type. Type can be either "String", "Logical", "Float", or "Integer".
DECLARE Sub VariableSetType LIB "systemio.dll" Alias "__VariableSetType@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal idtype as string ) 

'-------------------------------------------------------------------------
' Documentation for: VariableSetScalar
' Set the variable's scalar flag. If the flag is 0 then a set of values is stored. (i.e. the variable is a vector and the dimensionality is increased by 1) If the flag = 1 a single value is stored.
DECLARE Sub VariableSetScalar LIB "systemio.dll" Alias "__VariableSetScalar@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal flag as long ) 

'-------------------------------------------------------------------------
' Documentation for: VariableSetMinimum
' Set the variable's minimum value. Setting both minimum and maximum equal to each other has the effect of having the variable NOT range checked.
DECLARE Sub VariableSetMinimum LIB "systemio.dll" Alias "__VariableSetMinimum@20" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal min as double ) 

'-------------------------------------------------------------------------
' Documentation for: VariableSetMaximum
' Set the variable's maximum value. Setting both minimum and maximum equal to each other has the effect of having the variable NOT range checked.
DECLARE Sub VariableSetMaximum LIB "systemio.dll" Alias "__VariableSetMaximum@20" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal max as double ) 

'-------------------------------------------------------------------------
' Documentation for: VariableSetMeasure
' Set the variable's measure.
DECLARE Sub VariableSetMeasure LIB "systemio.dll" Alias "__VariableSetMeasure@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal measureName as string ) 

'-------------------------------------------------------------------------
' Documentation for: VariableSetUnit
' Set the variable's unit for its given measure.
DECLARE Sub VariableSetUnit LIB "systemio.dll" Alias "__VariableSetUnit@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal unitName as string ) 

'-------------------------------------------------------------------------
' Documentation for: VariableSetStochastic
' Set the variable's stochastic flag. This flag signifies if the parameter can be modified by an another program and not invalidate any calibration. This allows other programs such and Sensativity/Uncertainty or parameter estimation programs to modify these parameters.
DECLARE Sub VariableSetStochastic LIB "systemio.dll" Alias "__VariableSetStochastic@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal flag as long ) 

'-------------------------------------------------------------------------
' Documentation for: VariableSetPreposition
' Set a variable's preposition word. This helps in describing parameters in a human readable form. This is the word that would best be used to write a description of this parameters as an index for another. For example, the chemical parameter would use "for" as a preposition to make statements such as Concentration for benzene. The preposition greatly facilitates writing descriptive text associated with results.
DECLARE Sub VariableSetPreposition LIB "systemio.dll" Alias "__VariableSetPreposition@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal prepositionName as string ) 

'-------------------------------------------------------------------------
' Documentation for: VariableSetPrimaryKey
' Sets the variable's primarykey flag. Is this parameter a key to information on other parameters. This is used in both databases and in the representation of the data. For example a variable being used to name other variables frequently (like chemical) would indicate that chemical should be a primary key, similar to the typical meaning of primary key from database design.
DECLARE Sub VariableSetPrimaryKey LIB "systemio.dll" Alias "__VariableSetPrimaryKey@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal flag as long ) 

'-------------------------------------------------------------------------
' Documentation for: VariableAddIndex
' Add a variable index to a variable.
DECLARE Sub VariableAddIndex LIB "systemio.dll" Alias "__VariableAddIndex@20" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal dictionaryName as string,  ByVal variableName as string ) 

'-------------------------------------------------------------------------
' Documentation for: VariableDeleteIndex
' Delete an index from a variable.
DECLARE Sub VariableDeleteIndex LIB "systemio.dll" Alias "__VariableDeleteIndex@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal indexIndex as long ) 

'-------------------------------------------------------------------------
' Documentation for: VariableIndexCount
' Returns the number of indices for a variable.
DECLARE Function VariableIndexCount LIB "systemio.dll" Alias "__VariableIndexCount@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IndexGetIndex
' Returns the index position for an index of a variable specified by the dictionary and variable.
DECLARE Function IndexGetIndex LIB "systemio.dll" Alias "__IndexGetIndex@20" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal dictionaryName as string,  ByVal variableName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IndexGetDictionaryName
' Returns the index dictionary name of the specified index in a variable.
DECLARE Function DLLIndexGetDictionaryName LIB "systemio.dll" Alias "__IndexGetDictionaryName@20" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal indexIndex as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: IndexGetVariableName
' Returns the index variable name of the specified index in a variable.
DECLARE Function DLLIndexGetVariableName LIB "systemio.dll" Alias "__IndexGetVariableName@20" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal indexIndex as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: IndexPromote
' Move one level in the order of referred indices.
DECLARE Sub IndexPromote LIB "systemio.dll" Alias "__IndexPromote@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal referredIndex as long ) 

'-------------------------------------------------------------------------
' Documentation for: IndexDemote
' Move down one level in the order of referred indices.
DECLARE Sub IndexDemote LIB "systemio.dll" Alias "__IndexDemote@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long,  ByVal referredIndex as long ) 

'-------------------------------------------------------------------------
' Documentation for: ModuleGetPath
' Get module path associated with a module. Returns the file of the module.
DECLARE Function DLLModuleGetPath LIB "systemio.dll" Alias "__ModuleGetPath@12" (  ByVal PID as long,  ByVal moduleHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: ModuleAddFile
' Add module file and associated path with a module path for the installed domain. Index of 0 uses the absolute directory. Index of 1 uses the relative directory.
DECLARE Sub ModuleAddFile LIB "systemio.dll" Alias "__ModuleAddFile@16" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal moduleAddFile as string,  ByVal directoryIndex as long ) 

'-------------------------------------------------------------------------
' Documentation for: ModuleRemoveFile
' Remove module file and associated path with a module path for the installed domain. Index of 0 uses the absolute directory. Index of 1 uses the relative directory.
DECLARE Sub ModuleRemoveFile LIB "systemio.dll" Alias "__ModuleRemoveFile@16" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal moduleRemovedFile as string,  ByVal directoryIndex as long ) 

'-------------------------------------------------------------------------
' Documentation for: ModuleAddDictionary
' 
DECLARE Sub ModuleAddDictionary LIB "systemio.dll" Alias "__ModuleAddDictionary@12" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal dictionaryHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: ModuleDeleteDictionary
' 
DECLARE Sub ModuleDeleteDictionary LIB "systemio.dll" Alias "__ModuleDeleteDictionary@12" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal dictionaryHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: ModuleDictionaryCount
' 
DECLARE Function ModuleDictionaryCount LIB "systemio.dll" Alias "__ModuleDictionaryCount@8" (  ByVal PID as long,  ByVal moduleHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ModuleAddScheme
' Returns the index of the new scheme. Adds a scheme to a module.
DECLARE Function ModuleAddScheme LIB "systemio.dll" Alias "__ModuleAddScheme@12" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal schemeName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: ModuleDeleteScheme
' Deletes a scheme from a module.
DECLARE Sub ModuleDeleteScheme LIB "systemio.dll" Alias "__ModuleDeleteScheme@12" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal schemeHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: ModuleSchemeCount
' Returns the number of schemes in a module.
DECLARE Function ModuleSchemeCount LIB "systemio.dll" Alias "__ModuleSchemeCount@8" (  ByVal PID as long,  ByVal moduleHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SchemeGetName
' Returns the name the scheme with the specified handle.
DECLARE Function DLLSchemeGetName LIB "systemio.dll" Alias "__SchemeGetName@16" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal schemeHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: SchemeAddInputDictionary
' Adds an input dictionary to a scheme.
DECLARE Sub SchemeAddInputDictionary LIB "systemio.dll" Alias "__SchemeAddInputDictionary@16" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal schemeHandle as long,  ByVal dictionaryHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SchemeDeleteInputDictionary
' Deletes an input dictionary from a scheme.
DECLARE Sub SchemeDeleteInputDictionary LIB "systemio.dll" Alias "__SchemeDeleteInputDictionary@16" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal schemeHandle as long,  ByVal dictionaryHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SchemeInputDictionaryCount
' Returns the number of input dictionaries in a scheme.
DECLARE Function SchemeInputDictionaryCount LIB "systemio.dll" Alias "__SchemeInputDictionaryCount@12" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal schemeHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SchemeAddOutputDictionary
' Adds an output dictionary to a scheme.
DECLARE Sub SchemeAddOutputDictionary LIB "systemio.dll" Alias "__SchemeAddOutputDictionary@16" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal schemeHandle as long,  ByVal dictionaryHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SchemeDeleteOutputDictionary
' Deletes an output dictionary from a scheme.
DECLARE Sub SchemeDeleteOutputDictionary LIB "systemio.dll" Alias "__SchemeDeleteOutputDictionary@16" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal schemeHandle as long,  ByVal dictionaryHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SchemeOutputDictionaryCount
' Returns the number of output dictionaries in a scheme
DECLARE Function SchemeOutputDictionaryCount LIB "systemio.dll" Alias "__SchemeOutputDictionaryCount@12" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal schemeHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ModuleVariableGetScalar
' Returns the variable's scalar flag. 1 = scalar and 0 = vector (a collection of values)
DECLARE Function ModuleVariableGetScalar LIB "systemio.dll" Alias "__ModuleVariableGetScalar@12" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ModuleVariableCount
' Returns the number of variables in the module specified by the module handle.
DECLARE Function ModuleVariableCount LIB "systemio.dll" Alias "__ModuleVariableCount@8" (  ByVal PID as long,  ByVal moduleHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ModuleVariableDimensionCount
' Gets the size of the specified dimensions. The indices parameters define an array of integers that define the index values that are to be used.
DECLARE Function ModuleVariableDimensionCount LIB "systemio.dll" Alias "__ModuleVariableDimensionCount@16" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableHandle as long, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ModuleVariableGetInteger
' Returns an integer from meta data about a module specified by the given indices.
DECLARE Function ModuleVariableGetInteger LIB "systemio.dll" Alias "__ModuleVariableGetInteger@20" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ModuleVariableGetFloat
' Returns a float from meta data about a module specified by the given indices.
DECLARE Function ModuleVariableGetFloat LIB "systemio.dll" Alias "__ModuleVariableGetFloat@20" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any )  as double 

'-------------------------------------------------------------------------
' Documentation for: ModuleVariableGetLogical
' Returns a logical from meta data about a module specified by the given indices.
DECLARE Function ModuleVariableGetLogical LIB "systemio.dll" Alias "__ModuleVariableGetLogical@20" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ModuleVariableGetString
' Returns a string from meta data about a module specified by the given indices.
DECLARE Function DLLModuleVariableGetString LIB "systemio.dll" Alias "__ModuleVariableGetString@24" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: ModuleVariableSetInteger
' Sets an integer from meta data about a module specified by the given indices.
DECLARE Sub ModuleVariableSetInteger LIB "systemio.dll" Alias "__ModuleVariableSetInteger@24" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: ModuleVariableSetFloat
' Sets a float from meta data about a module specified by the given indices.
DECLARE Sub ModuleVariableSetFloat LIB "systemio.dll" Alias "__ModuleVariableSetFloat@28" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: ModuleVariableSetLogical
' Sets a logical from meta data about a module specified by the given indices.
DECLARE Sub ModuleVariableSetLogical LIB "systemio.dll" Alias "__ModuleVariableSetLogical@24" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: ModuleVariableSetString
' Sets a string from meta data about a module specified by the given indices.
DECLARE Sub ModuleVariableSetString LIB "systemio.dll" Alias "__ModuleVariableSetString@24" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: ModuleLookUp
' Returns the index for the specified value in a set of values. The set is determined by which index is given a 0 value in the indice array. A negative number indicates the element could not be found.
DECLARE Function ModuleLookUp LIB "systemio.dll" Alias "__ModuleLookUp@20" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableName as string,  ByVal value as string, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ModuleDimensionCount
' Gets the size of the specified dimensions. The indices parameters define an array of integers that define the index values that are to be used.
DECLARE Function ModuleDimensionCount LIB "systemio.dll" Alias "__ModuleDimensionCount@16" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableName as string, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ModuleReadInt
' Returns an integer from meta data about a module specified by the given indices.
DECLARE Function ModuleReadInt LIB "systemio.dll" Alias "__ModuleReadInt@20" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableName as string,  ByVal unitName as string, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ModuleReadReal
' Returns a float from meta data about a module specified by the given indices.
DECLARE Function ModuleReadReal LIB "systemio.dll" Alias "__ModuleReadReal@20" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableName as string,  ByVal unitName as string, indices as Any )  as double 

'-------------------------------------------------------------------------
' Documentation for: ModuleReadLog
' Returns a logical from meta data about a module specified by the given indices.
DECLARE Function ModuleReadLog LIB "systemio.dll" Alias "__ModuleReadLog@20" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableName as string,  ByVal unitName as string, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ModuleReadString
' Returns a string from meta data about a module specified by the given indices.
DECLARE Function DLLModuleReadString LIB "systemio.dll" Alias "__ModuleReadString@24" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableName as string,  ByVal unitName as string, indices as Any, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: ModuleWriteInt
' Sets an integer from meta data about a module specified by the given indices.
DECLARE Sub ModuleWriteInt LIB "systemio.dll" Alias "__ModuleWriteInt@24" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableName as string,  ByVal unitName as string, indices as Any,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: ModuleWriteReal
' Sets a float from meta data about a module specified by the given indices.
DECLARE Sub ModuleWriteReal LIB "systemio.dll" Alias "__ModuleWriteReal@28" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableName as string,  ByVal unitName as string, indices as Any,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: ModuleWriteLog
' Sets a logical from meta data about a module specified by the given indices.
DECLARE Sub ModuleWriteLog LIB "systemio.dll" Alias "__ModuleWriteLog@24" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableName as string,  ByVal unitName as string, indices as Any,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: ModuleWriteString
' Sets a string from meta data about a module specified by the given indices.
DECLARE Sub ModuleWriteString LIB "systemio.dll" Alias "__ModuleWriteString@24" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableName as string,  ByVal unitName as string, indices as Any,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: SystemAddDomain
' Returns the handle of the added domain. Automatically adds the four class types to the domain.
DECLARE Function SystemAddDomain LIB "systemio.dll" Alias "__SystemAddDomain@8" (  ByVal PID as long,  ByVal domainName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: SystemDeleteDomain
' Delets a domain and its entire tree of Classes, Groups, and SubGroups.
DECLARE Sub SystemDeleteDomain LIB "systemio.dll" Alias "__SystemDeleteDomain@8" (  ByVal PID as long,  ByVal domainHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: DomainGetIcon
' Returns the fully qualified path for the domain icon. Icons are either .bmp, .gif files and 32x32 pixels in size.
DECLARE Function DLLDomainGetIcon LIB "systemio.dll" Alias "__DomainGetIcon@12" (  ByVal PID as long,  ByVal domainHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: DomainSetIcon
' Sets the fully qualified path for the domain icon. Icons are either .bmp, .gif files and 32x32 pixels in size.
DECLARE Sub DomainSetIcon LIB "systemio.dll" Alias "__DomainSetIcon@12" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal domainIconFile as string ) 

'-------------------------------------------------------------------------
' Documentation for: DomainAddClass
' Returns the index of the new class. Adds a class to a domain.
DECLARE Function DomainAddClass LIB "systemio.dll" Alias "__DomainAddClass@12" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal className as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: DomainDeleteClass
' Deletes a class from a domain. Also deletes modules and anything else below the group in the tree.
DECLARE Sub DomainDeleteClass LIB "systemio.dll" Alias "__DomainDeleteClass@12" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: DomainAddDictionary
' 
DECLARE Sub DomainAddDictionary LIB "systemio.dll" Alias "__DomainAddDictionary@12" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal dictionaryHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: DomainDeleteDictionary
' 
DECLARE Sub DomainDeleteDictionary LIB "systemio.dll" Alias "__DomainDeleteDictionary@12" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal dictionaryHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: DomainDictionaryCount
' 
DECLARE Function DomainDictionaryCount LIB "systemio.dll" Alias "__DomainDictionaryCount@8" (  ByVal PID as long,  ByVal domainHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ClassGetIcon
' Returns the fully qualified path for the module class icon. Icons are either .bmp, .gif files and 32x32 pixels in size.
DECLARE Function DLLClassGetIcon LIB "systemio.dll" Alias "__ClassGetIcon@16" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: ClassSetIcon
' Sets the fully qualified path for the module class icon. Icons are either .bmp, .gif files and 32x32 pixels in size.
DECLARE Sub ClassSetIcon LIB "systemio.dll" Alias "__ClassSetIcon@16" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal classIconFile as string ) 

'-------------------------------------------------------------------------
' Documentation for: ClassAddGroup
' Returns the index of the new group. Adds a group to a module class.
DECLARE Function ClassAddGroup LIB "systemio.dll" Alias "__ClassAddGroup@16" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: ClassDeleteGroup
' Deletes a group from a module class. Also deletes modules and anything else below the group in the tree.
DECLARE Sub ClassDeleteGroup LIB "systemio.dll" Alias "__ClassDeleteGroup@16" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: GroupGetIcon
' Returns the fully qualified path for the group icon. Icons are either .bmp, .gif files and 32x32 pixels in size.
DECLARE Function DLLGroupGetIcon LIB "systemio.dll" Alias "__GroupGetIcon@20" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: GroupSetIcon
' Sets the fully qualified path for the group icon. Icons are either .bmp, .gif files and 32x32 pixels in size.
DECLARE Sub GroupSetIcon LIB "systemio.dll" Alias "__GroupSetIcon@20" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal groupIconFile as string ) 

'-------------------------------------------------------------------------
' Documentation for: GroupAddSubGroup
' Returns the index of the new subGroup. Adds a subGroup to a group.
DECLARE Function GroupAddSubGroup LIB "systemio.dll" Alias "__GroupAddSubGroup@20" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: GroupDeleteSubGroup
' Deletes a subGroup and its modules from a group.
DECLARE Sub GroupDeleteSubGroup LIB "systemio.dll" Alias "__GroupDeleteSubGroup@20" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SubGroupGetIcon
' Returns the fully qualified path for the subGroup icon. Icons are either .bmp, .gif files and 32x32 pixels in size.
DECLARE Function DLLSubGroupGetIcon LIB "systemio.dll" Alias "__SubGroupGetIcon@24" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: SubGroupSetIcon
' Sets the fully qualified path for the subGroup icon. Icons are either .bmp, .gif files and 32x32 pixels in size.
DECLARE Sub SubGroupSetIcon LIB "systemio.dll" Alias "__SubGroupSetIcon@24" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupHandle as long,  ByVal subgroupIconFile as string ) 

'-------------------------------------------------------------------------
' Documentation for: DomainNodeAddModule
' Add module to any valid domain node.
DECLARE Sub DomainNodeAddModule LIB "systemio.dll" Alias "__DomainNodeAddModule@12" (  ByVal PID as long,  ByVal nodeHandle as long,  ByVal moduleHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: DomainNodeDeleteModule
' Remove module from any valid domain node.
DECLARE Sub DomainNodeDeleteModule LIB "systemio.dll" Alias "__DomainNodeDeleteModule@12" (  ByVal PID as long,  ByVal nodeHandle as long,  ByVal moduleHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: DomainAddModule
' Add module to any valid domain node.
DECLARE Sub DomainAddModule LIB "systemio.dll" Alias "__DomainAddModule@12" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal moduleHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: DomainDeleteModule
' Remove module from any valid domain node.
DECLARE Sub DomainDeleteModule LIB "systemio.dll" Alias "__DomainDeleteModule@12" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal moduleHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: GroupAddModule
' Returns the index of the newly added module. Adds a module to a group.
DECLARE Sub GroupAddModule LIB "systemio.dll" Alias "__GroupAddModule@20" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal moduleHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: GroupDeleteModule
' Deletes a module from a group.
DECLARE Sub GroupDeleteModule LIB "systemio.dll" Alias "__GroupDeleteModule@20" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal moduleHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SubGroupAddModule
' Returns the index of the newly added module. Adds a module to a sub group.
DECLARE Sub SubGroupAddModule LIB "systemio.dll" Alias "__SubGroupAddModule@24" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupHandle as long,  ByVal moduleHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SubGroupDeleteModule
' Deletes a module from a subGroup.
DECLARE Sub SubGroupDeleteModule LIB "systemio.dll" Alias "__SubGroupDeleteModule@24" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupHandle as long,  ByVal moduleHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: DomainGetName
' Returns a domain name.
DECLARE Function DLLDomainGetName LIB "systemio.dll" Alias "__DomainGetName@12" (  ByVal PID as long,  ByVal domainHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: ClassGetName
' Returns a module class name.
DECLARE Function DLLClassGetName LIB "systemio.dll" Alias "__ClassGetName@16" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: GroupGetName
' Returns a group name.
DECLARE Function DLLGroupGetName LIB "systemio.dll" Alias "__GroupGetName@20" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: SubGroupGetName
' Returns a subGroup name.
DECLARE Function DLLSubGroupGetName LIB "systemio.dll" Alias "__SubGroupGetName@24" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: SystemDomainCount
' Returns the number of domains.
DECLARE Function SystemDomainCount LIB "systemio.dll" Alias "__SystemDomainCount@4" (  ByVal PID as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DomainClassCount
' Returns the number of module classes in a domain.
DECLARE Function DomainClassCount LIB "systemio.dll" Alias "__DomainClassCount@8" (  ByVal PID as long,  ByVal domainHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ClassGroupCount
' Returns the number of groups in a module class.
DECLARE Function ClassGroupCount LIB "systemio.dll" Alias "__ClassGroupCount@12" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GroupSubGroupCount
' Returns the number of sub groups in a group.
DECLARE Function GroupSubGroupCount LIB "systemio.dll" Alias "__GroupSubGroupCount@16" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GroupModuleCount
' Returns the number of modules in a group.
DECLARE Function GroupModuleCount LIB "systemio.dll" Alias "__GroupModuleCount@16" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SubGroupModuleCount
' Returns the number of modules in a subgroup.
DECLARE Function SubGroupModuleCount LIB "systemio.dll" Alias "__SubGroupModuleCount@20" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SystemAddSimulation
' Returns the handle of the newly created simulation file.
DECLARE Function SystemAddSimulation LIB "systemio.dll" Alias "__SystemAddSimulation@12" (  ByVal PID as long,  ByVal simulationFile as string,  ByVal simulationName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: SystemOpenSimulation
' Returns the handle of the opened simulation file.
DECLARE Function SystemOpenSimulation LIB "systemio.dll" Alias "__SystemOpenSimulation@8" (  ByVal PID as long,  ByVal simulationFile as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: SystemSaveSimulation
' Save the simulation present state to the file specified.
DECLARE Sub SystemSaveSimulation LIB "systemio.dll" Alias "__SystemSaveSimulation@4" (  ByVal PID as long ) 

'-------------------------------------------------------------------------
' Documentation for: SystemSaveSimulationToFile
' Save the simulation present state to a file. The SystemSaveSimulationToFile function call is made with a process id as input to save the simulation's present state.
DECLARE Sub SystemSaveSimulationToFile LIB "systemio.dll" Alias "__SystemSaveSimulationToFile@4" (  ByVal PID as long ) 

'-------------------------------------------------------------------------
' Documentation for: SystemSaveSimulationToDB
' Save the simulation present state to a database. The SystemSaveSimulationToDB function call is made with a process id as input to save the simulation's present state.
DECLARE Sub SystemSaveSimulationToDB LIB "systemio.dll" Alias "__SystemSaveSimulationToDB@4" (  ByVal PID as long ) 

'-------------------------------------------------------------------------
' Documentation for: SystemSaveSimulationAs
' Save the simulation present state to a different file than specified. Does not register the new file with the FRAMES system. The simulation must first be closed and then reopened for the name change to take place.
DECLARE Sub SystemSaveSimulationAs LIB "systemio.dll" Alias "__SystemSaveSimulationAs@12" (  ByVal PID as long,  ByVal simulationFile as string,  ByVal simulationNewName as string ) 

'-------------------------------------------------------------------------
' Documentation for: SystemCloseSimulation
' Close the simulation without saving.
DECLARE Sub SystemCloseSimulation LIB "systemio.dll" Alias "__SystemCloseSimulation@4" (  ByVal PID as long ) 

'-------------------------------------------------------------------------
' Documentation for: SystemAddDataSet
' Returns the handle to the new dataset. Creates a new dataset using a dictionary handle.
DECLARE Function SystemAddDataSet LIB "systemio.dll" Alias "__SystemAddDataSet@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal datasetFile as string,  ByVal datasetName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: SystemOpenDataSet
' Returns the handle to the dataset. Opens a dataset. The dictionary used by the dataset must be registered with the FRAMES system for the dataset to function correctly.
DECLARE Function SystemOpenDataSet LIB "systemio.dll" Alias "__SystemOpenDataSet@8" (  ByVal PID as long,  ByVal datasetFile as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: SystemCloseDataSet
' Close a dataset from the FRAMES system and remove its entry in the system registry.
DECLARE Sub SystemCloseDataSet LIB "systemio.dll" Alias "__SystemCloseDataSet@8" (  ByVal PID as long,  ByVal datasetHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SystemSaveDataSet
' Save a dataset to the file indicated when the dataset was created.
DECLARE Sub SystemSaveDataSet LIB "systemio.dll" Alias "__SystemSaveDataSet@8" (  ByVal PID as long,  ByVal datasetHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SystemSaveDataSetToFile
' Save a dataset to the file indicated when the dataset was created.
DECLARE Sub SystemSaveDataSetToFile LIB "systemio.dll" Alias "__SystemSaveDataSetToFile@8" (  ByVal PID as long,  ByVal datasetHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SystemSaveDataSetToDB
' Save a dataset to the file indicated when the dataset was created.
DECLARE Sub SystemSaveDataSetToDB LIB "systemio.dll" Alias "__SystemSaveDataSetToDB@8" (  ByVal PID as long,  ByVal datasetHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SystemSaveDataSetAs
' Save the dataset with a different name to a different file. Does not register the new file with the FRAMES system. The dataset must first be closed and then reopened for the name change to take place.
DECLARE Sub SystemSaveDataSetAs LIB "systemio.dll" Alias "__SystemSaveDataSetAs@16" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal datasetFile as string,  ByVal datasetNewName as string ) 

'-------------------------------------------------------------------------
' Documentation for: SimulationAddLink
' Links two icons together inside a simulation. The arrow is drawn from the "From Icon" and goes to the "To Icon."
DECLARE Sub SimulationAddLink LIB "systemio.dll" Alias "__SimulationAddLink@12" (  ByVal PID as long,  ByVal fromIconHandle as long,  ByVal toIconHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SimulationDeleteLink
' Deletes the link between two icons.
DECLARE Sub SimulationDeleteLink LIB "systemio.dll" Alias "__SimulationDeleteLink@12" (  ByVal PID as long,  ByVal fromIconHandle as long,  ByVal toIconHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SimulationAddIcon
' Returns the icon index for the added icon. Adds an icon into a simulation.
DECLARE Function SimulationAddIcon LIB "systemio.dll" Alias "__SimulationAddIcon@24" (  ByVal PID as long,  ByVal scope as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupHandle as long )  as long 

'-------------------------------------------------------------------------
' Documentation for: SimulationDeleteIcon
' Deletes an icon from the simulation.
DECLARE Sub SimulationDeleteIcon LIB "systemio.dll" Alias "__SimulationDeleteIcon@8" (  ByVal PID as long,  ByVal iconHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: IconSetModule
' Sets the dataset names for the module and scheme associated with the indicated icon. A subset of the datasets the icon consumes is given in the list parameter.
DECLARE Sub IconSetModule LIB "systemio.dll" Alias "__IconSetModule@24" (  ByVal PID as long,  ByVal iconHandle as long,  ByVal moduleHandle as long,  ByVal schemeHandle as long,  ByVal delimiter as string,  ByVal list as string ) 

'-------------------------------------------------------------------------
' Documentation for: IconSetLabel
' Sets the user supplied label of an icon given the icon handle.
DECLARE Function IconSetLabel LIB "systemio.dll" Alias "__IconSetLabel@12" (  ByVal PID as long,  ByVal iconHandle as long,  ByVal labe as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IconSetNote
' Sets the user suppplied note of an icon given the icon handle.
DECLARE Function IconSetNote LIB "systemio.dll" Alias "__IconSetNote@12" (  ByVal PID as long,  ByVal iconHandle as long,  ByVal note as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IconSetPosXY
' Sets the location of screen coordinates of an icon given the icon handle.
DECLARE Function IconSetPosXY LIB "systemio.dll" Alias "__IconSetPosXY@16" (  ByVal PID as long,  ByVal iconHandle as long,  ByVal x as long,  ByVal y as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IconGetUIPath
' Returns the executable/batch file for the user interface (interactive) portion of an icon's module. The interactive portion of a module requires human interaction.
DECLARE Function DLLIconGetUIPath LIB "systemio.dll" Alias "__IconGetUIPath@12" (  ByVal PID as long,  ByVal iconHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: IconGetUIArguments
' Returns the arguments for the user interface (interactive) portion of an icon's module. The interactive portion of a module requires human interaction.
DECLARE Function DLLIconGetUIArguments LIB "systemio.dll" Alias "__IconGetUIArguments@12" (  ByVal PID as long,  ByVal iconHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: IconRunUI
' Execute the executable/batch file for the user interface (interactive) portion of an icon's module. The interactive portion of a module requires human interaction.
DECLARE Function IconRunUI LIB "systemio.dll" Alias "__IconRunUI@24" (  ByVal PID as long,  ByVal iconHandle as long,  ByVal uiExecutableFile as string,  ByVal cmdLine as string,  ByVal warningFile as string,  ByVal errorFile as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IconGetModelPath
' Returns the fully qualified path to the executable/batch file to the model (passive) part of a module. The passive portion of the model requires no interaction.
DECLARE Function DLLIconGetModelPath LIB "systemio.dll" Alias "__IconGetModelPath@12" (  ByVal PID as long,  ByVal iconHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: IconGetModelArguments
' The passive part of the icon is the model itself that requires NO user to be active during its execution. Returns the arguments or commandline for the executable/batch file to the model (passive) part of a module.
DECLARE Function DLLIconGetModelArguments LIB "systemio.dll" Alias "__IconGetModelArguments@12" (  ByVal PID as long,  ByVal iconHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: IconRunModel
' The passive part of the icon is the model itself that requires NO user to be active during its execution. The RunModPassive function call is made with a process id and a modId string as input to run a passive module. Returns a success/fail flag.
DECLARE Function IconRunModel LIB "systemio.dll" Alias "__IconRunModel@24" (  ByVal PID as long,  ByVal iconHandle as long,  ByVal modelExecutableFile as string,  ByVal cmdLine as string,  ByVal warningFile as string,  ByVal errorFile as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IconRunUIStatus
' Returns the status of the process. This will indicate whether the process interacted completely with the system by calling OpenIO, CloseIO, has no errors and changed ui datasets
DECLARE Function IconRunUIStatus LIB "systemio.dll" Alias "__IconRunUIStatus@8" (  ByVal PID as long,  ByVal iconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IconRunModelStatus
' Returns the status of the process. This will indicate whether the process interacted completely with the system by calling OpenIO, CloseIO, has no errors and changed output datasets
DECLARE Function IconRunModelStatus LIB "systemio.dll" Alias "__IconRunModelStatus@8" (  ByVal PID as long,  ByVal iconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IconIsUIOk
' Indicates whether the icon state is sufficient to allow UI to execute.
DECLARE Function IconIsUIOk LIB "systemio.dll" Alias "__IconIsUIOk@8" (  ByVal PID as long,  ByVal iconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IconIsModelOk
' Indicates whether the icon state is sufficient to allow model to execute.
DECLARE Function IconIsModelOk LIB "systemio.dll" Alias "__IconIsModelOk@8" (  ByVal PID as long,  ByVal iconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IconIsInformationOk
' Indicates whether the icon state is sufficient to allow input of information.
DECLARE Function IconIsInformationOk LIB "systemio.dll" Alias "__IconIsInformationOk@8" (  ByVal PID as long,  ByVal iconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IconsRunBetween
' Attempts to advance the state from the begin indexed icon to the end indexed icon.
DECLARE Function IconsRunBetween LIB "systemio.dll" Alias "__IconsRunBetween@12" (  ByVal PID as long,  ByVal iconBeginHandle as long,  ByVal iconEndHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IconSetState
' Arbitrarily set the state of an icon given the icon handle.
DECLARE Sub IconSetState LIB "systemio.dll" Alias "__IconSetState@16" (  ByVal PID as long,  ByVal iconHandle as long,  ByVal state as long,  ByVal downstate as long ) 

'-------------------------------------------------------------------------
' Documentation for: IconGetState
' Get the state of an icon given the icon handle.
DECLARE Function IconGetState LIB "systemio.dll" Alias "__IconGetState@8" (  ByVal PID as long,  ByVal iconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ToolLaunch
' Launches the specified module. The system flag indicates which set of arguments to pass to module.
DECLARE Function ToolLaunch LIB "systemio.dll" Alias "__ToolLaunch@16" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal isSystemTool as long,  ByVal iconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: Launcher
' Launches the specified module.
DECLARE Function Launcher LIB "systemio.dll" Alias "__Launcher@12" (  ByVal executableFile as string,  ByVal cmdLine as string,  ByVal wait as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DomainGetHandle
' Returns the handle of a domain.
DECLARE Function DomainGetHandle LIB "systemio.dll" Alias "__DomainGetHandle@8" (  ByVal PID as long,  ByVal domainName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: ClassGetHandle
' Returns the handle for a class.
DECLARE Function ClassGetHandle LIB "systemio.dll" Alias "__ClassGetHandle@12" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal className as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: GroupGetHandle
' Returns the handle for a group.
DECLARE Function GroupGetHandle LIB "systemio.dll" Alias "__GroupGetHandle@16" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: SubGroupGetHandle
' Returns the handle for a subGroup.
DECLARE Function SubGroupGetHandle LIB "systemio.dll" Alias "__SubGroupGetHandle@20" (  ByVal PID as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: HandleIsValid
' Return success if the handle is valid.
DECLARE Function HandleIsValid LIB "systemio.dll" Alias "__HandleIsValid@4" (  ByVal handleHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ModuleGetName
' Returns the name of the module given the module handle.
DECLARE Function DLLModuleGetName LIB "systemio.dll" Alias "__ModuleGetName@12" (  ByVal PID as long,  ByVal moduleHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: ModuleGetClass
' Returns the class type of the module given the module handle.
DECLARE Function DLLModuleGetClass LIB "systemio.dll" Alias "__ModuleGetClass@12" (  ByVal PID as long,  ByVal moduleHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: SystemCreateProcessId
' Get process id for launching process
DECLARE Function SystemCreateProcessId LIB "systemio.dll" Alias "__SystemCreateProcessId@8" (  ByVal PID as long,  ByVal iconName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SystemDeleteProcessId
' Delete pid generated by call to SystemCreateProcessId.
DECLARE Function SystemDeleteProcessId LIB "systemio.dll" Alias "__SystemDeleteProcessId@8" (  ByVal PID as long,  ByVal newpid as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ProcessGetSimulation
' Returns the name of the currently open simulation.
DECLARE Function DLLProcessGetSimulation LIB "systemio.dll" Alias "__ProcessGetSimulation@8" (  ByVal PID as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: ProcessSetSimulation
' Sets the name of the simulation. The simulation name must already be opened.
DECLARE Sub ProcessSetSimulation LIB "systemio.dll" Alias "__ProcessSetSimulation@8" (  ByVal PID as long,  ByVal simulationName as string ) 

'-------------------------------------------------------------------------
' Documentation for: ProcessGetSimulationPath
' Returns the fully qualified file of the currently open simulation.
DECLARE Function DLLProcessGetSimulationPath LIB "systemio.dll" Alias "__ProcessGetSimulationPath@8" (  ByVal PID as long, ByVal value as String )  as long 

Function F2GetDictionaryName (  ByVal PID as long,  ByVal dictionaryHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetDictionaryName(PID, dictionaryHandle, retStr3)
      F2GetDictionaryName=StripTerminator(retStr3)
End  Function 

Function F2GetDictionaryDescription (  ByVal PID as long,  ByVal dictionaryHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetDictionaryDescription(PID, dictionaryHandle, retStr3)
      F2GetDictionaryDescription=StripTerminator(retStr3)
End  Function 

Function F2GetDictionaryPath (  ByVal PID as long,  ByVal dictionaryHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetDictionaryPath(PID, dictionaryHandle, retStr3)
      F2GetDictionaryPath=StripTerminator(retStr3)
End  Function 

Function F2GetVariableName (  ByVal PID as long,  ByVal dictionaryHandle as long ,  ByVal variableIndex as long )  as string 
      Dim retStr4 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetVariableName(PID, dictionaryHandle, variableIndex, retStr4)
      F2GetVariableName=StripTerminator(retStr4)
End  Function 

Function F2GetVariableIndexDictionary (  ByVal PID as long,  ByVal dictionaryHandle as long ,  ByVal variableHandle as long ,  ByVal indexIndex as long )  as string 
      Dim retStr5 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetVariableIndexDictionary(PID, dictionaryHandle, variableHandle, indexIndex, retStr5)
      F2GetVariableIndexDictionary=StripTerminator(retStr5)
End  Function 

Function F2GetVariableIndexVariable (  ByVal PID as long,  ByVal dictionaryHandle as long ,  ByVal variableHandle as long ,  ByVal indexIndex as long )  as string 
      Dim retStr5 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetVariableIndexVariable(PID, dictionaryHandle, variableHandle, indexIndex, retStr5)
      F2GetVariableIndexVariable=StripTerminator(retStr5)
End  Function 

Function F2GetModuleName (  ByVal PID as long,  ByVal moduleHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetModuleName(PID, moduleHandle, retStr3)
      F2GetModuleName=StripTerminator(retStr3)
End  Function 

Function F2GetModulePath (  ByVal PID as long,  ByVal moduleHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetModulePath(PID, moduleHandle, retStr3)
      F2GetModulePath=StripTerminator(retStr3)
End  Function 

Function F2GetSchemeName (  ByVal PID as long,  ByVal moduleHandle as long ,  ByVal schemeHandle as long  )  as string 
      Dim retStr4 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetSchemeName(PID, moduleHandle, schemeHandle, retStr4)
      F2GetSchemeName=StripTerminator(retStr4)
End  Function 

Function F2GetModuleVariableString (  ByVal PID as long,  ByVal moduleHandle as long ,  ByVal variableHandle as long ,  ByVal unitName as string  , indices as tindex )  as string 
      Dim retStr6 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetModuleVariableString(PID, moduleHandle, variableHandle, unitName, indices, retStr6)
      F2GetModuleVariableString=StripTerminator(retStr6)
End  Function 

Function F2GetDomainIcon (  ByVal PID as long,  ByVal domainHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetDomainIcon(PID, domainHandle, retStr3)
      F2GetDomainIcon=StripTerminator(retStr3)
End  Function 

Function F2GetClassIcon (  ByVal PID as long,  ByVal domainHandle as long ,  ByVal classHandle as long  )  as string 
      Dim retStr4 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetClassIcon(PID, domainHandle, classHandle, retStr4)
      F2GetClassIcon=StripTerminator(retStr4)
End  Function 

Function F2GetGroupIcon (  ByVal PID as long,  ByVal domainHandle as long ,  ByVal classHandle as long ,  ByVal groupHandle as long  )  as string 
      Dim retStr5 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetGroupIcon(PID, domainHandle, classHandle, groupHandle, retStr5)
      F2GetGroupIcon=StripTerminator(retStr5)
End  Function 

Function F2GetSubGroupIcon (  ByVal PID as long,  ByVal domainHandle as long ,  ByVal classHandle as long ,  ByVal groupHandle as long ,  ByVal subgroupHandle as long  )  as string 
      Dim retStr6 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetSubGroupIcon(PID, domainHandle, classHandle, groupHandle, subgroupHandle, retStr6)
      F2GetSubGroupIcon=StripTerminator(retStr6)
End  Function 

Function F2GetDomainName (  ByVal PID as long,  ByVal domainHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetDomainName(PID, domainHandle, retStr3)
      F2GetDomainName=StripTerminator(retStr3)
End  Function 

Function F2GetClassName (  ByVal PID as long,  ByVal domainHandle as long ,  ByVal classHandle as long  )  as string 
      Dim retStr4 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetClassName(PID, domainHandle, classHandle, retStr4)
      F2GetClassName=StripTerminator(retStr4)
End  Function 

Function F2GetGroupName (  ByVal PID as long,  ByVal domainHandle as long ,  ByVal classHandle as long ,  ByVal groupHandle as long  )  as string 
      Dim retStr5 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetGroupName(PID, domainHandle, classHandle, groupHandle, retStr5)
      F2GetGroupName=StripTerminator(retStr5)
End  Function 

Function F2GetSubGroupName (  ByVal PID as long,  ByVal domainHandle as long ,  ByVal classHandle as long ,  ByVal groupHandle as long ,  ByVal subgroupHandle as long  )  as string 
      Dim retStr6 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetSubGroupName(PID, domainHandle, classHandle, groupHandle, subgroupHandle, retStr6)
      F2GetSubGroupName=StripTerminator(retStr6)
End  Function 

Function F2GetSimulation (  ByVal PID as long )  as string 
      Dim retStr2 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetSimulation(PID, retStr2)
      F2GetSimulation=StripTerminator(retStr2)
End  Function 

Function F2GetDataSetName (  ByVal PID as long,  ByVal datasetHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetDataSetName(PID, datasetHandle, retStr3)
      F2GetDataSetName=StripTerminator(retStr3)
End  Function 

Function F2GetDataSetPath (  ByVal PID as long,  ByVal datasetHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetDataSetPath(PID, datasetHandle, retStr3)
      F2GetDataSetPath=StripTerminator(retStr3)
End  Function 

Function F2GetIconName (  ByVal PID as long,  ByVal iconHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetIconName(PID, iconHandle, retStr3)
      F2GetIconName=StripTerminator(retStr3)
End  Function 

Function F2GetIconUIDictionary (  ByVal PID as long,  ByVal iconHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetIconUIDictionary(PID, iconHandle, retStr3)
      F2GetIconUIDictionary=StripTerminator(retStr3)
End  Function 

Function F2GetIconUIDataSet (  ByVal PID as long,  ByVal iconHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetIconUIDataSet(PID, iconHandle, retStr3)
      F2GetIconUIDataSet=StripTerminator(retStr3)
End  Function 

Function F2GetIconModule (  ByVal PID as long,  ByVal iconHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetIconModule(PID, iconHandle, retStr3)
      F2GetIconModule=StripTerminator(retStr3)
End  Function 

Function F2GetIconScheme (  ByVal PID as long,  ByVal iconHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetIconScheme(PID, iconHandle, retStr3)
      F2GetIconScheme=StripTerminator(retStr3)
End  Function 

Function IndexGetDictionaryName (  ByVal PID as long,  ByVal dictionaryHandle as long ,  ByVal variableHandle as long ,  ByVal indexIndex as long )  as string 
      Dim retStr5 as String * MAXFIELD
      Dim value as long
      value = DLLIndexGetDictionaryName(PID, dictionaryHandle, variableHandle, indexIndex, retStr5)
      IndexGetDictionaryName=StripTerminator(retStr5)
End  Function 

Function IndexGetVariableName (  ByVal PID as long,  ByVal dictionaryHandle as long ,  ByVal variableHandle as long ,  ByVal indexIndex as long )  as string 
      Dim retStr5 as String * MAXFIELD
      Dim value as long
      value = DLLIndexGetVariableName(PID, dictionaryHandle, variableHandle, indexIndex, retStr5)
      IndexGetVariableName=StripTerminator(retStr5)
End  Function 

Function ModuleGetPath (  ByVal PID as long,  ByVal moduleHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLModuleGetPath(PID, moduleHandle, retStr3)
      ModuleGetPath=StripTerminator(retStr3)
End  Function 

Function SchemeGetName (  ByVal PID as long,  ByVal moduleHandle as long ,  ByVal schemeHandle as long  )  as string 
      Dim retStr4 as String * MAXFIELD
      Dim value as long
      value = DLLSchemeGetName(PID, moduleHandle, schemeHandle, retStr4)
      SchemeGetName=StripTerminator(retStr4)
End  Function 

Function ModuleVariableGetString (  ByVal PID as long,  ByVal moduleHandle as long ,  ByVal variableHandle as long ,  ByVal unitName as string  , indices as tindex )  as string 
      Dim retStr6 as String * MAXFIELD
      Dim value as long
      value = DLLModuleVariableGetString(PID, moduleHandle, variableHandle, unitName, indices, retStr6)
      ModuleVariableGetString=StripTerminator(retStr6)
End  Function 

Function ModuleReadString (  ByVal PID as long,  ByVal moduleHandle as long ,  ByVal variableName as string  ,  ByVal unitName as string  , indices as tindex )  as string 
      Dim retStr6 as String * MAXFIELD
      Dim value as long
      value = DLLModuleReadString(PID, moduleHandle, variableName, unitName, indices, retStr6)
      ModuleReadString=StripTerminator(retStr6)
End  Function 

Function DomainGetIcon (  ByVal PID as long,  ByVal domainHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLDomainGetIcon(PID, domainHandle, retStr3)
      DomainGetIcon=StripTerminator(retStr3)
End  Function 

Function ClassGetIcon (  ByVal PID as long,  ByVal domainHandle as long ,  ByVal classHandle as long  )  as string 
      Dim retStr4 as String * MAXFIELD
      Dim value as long
      value = DLLClassGetIcon(PID, domainHandle, classHandle, retStr4)
      ClassGetIcon=StripTerminator(retStr4)
End  Function 

Function GroupGetIcon (  ByVal PID as long,  ByVal domainHandle as long ,  ByVal classHandle as long ,  ByVal groupHandle as long  )  as string 
      Dim retStr5 as String * MAXFIELD
      Dim value as long
      value = DLLGroupGetIcon(PID, domainHandle, classHandle, groupHandle, retStr5)
      GroupGetIcon=StripTerminator(retStr5)
End  Function 

Function SubGroupGetIcon (  ByVal PID as long,  ByVal domainHandle as long ,  ByVal classHandle as long ,  ByVal groupHandle as long ,  ByVal subgroupHandle as long  )  as string 
      Dim retStr6 as String * MAXFIELD
      Dim value as long
      value = DLLSubGroupGetIcon(PID, domainHandle, classHandle, groupHandle, subgroupHandle, retStr6)
      SubGroupGetIcon=StripTerminator(retStr6)
End  Function 

Function DomainGetName (  ByVal PID as long,  ByVal domainHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLDomainGetName(PID, domainHandle, retStr3)
      DomainGetName=StripTerminator(retStr3)
End  Function 

Function ClassGetName (  ByVal PID as long,  ByVal domainHandle as long ,  ByVal classHandle as long  )  as string 
      Dim retStr4 as String * MAXFIELD
      Dim value as long
      value = DLLClassGetName(PID, domainHandle, classHandle, retStr4)
      ClassGetName=StripTerminator(retStr4)
End  Function 

Function GroupGetName (  ByVal PID as long,  ByVal domainHandle as long ,  ByVal classHandle as long ,  ByVal groupHandle as long  )  as string 
      Dim retStr5 as String * MAXFIELD
      Dim value as long
      value = DLLGroupGetName(PID, domainHandle, classHandle, groupHandle, retStr5)
      GroupGetName=StripTerminator(retStr5)
End  Function 

Function SubGroupGetName (  ByVal PID as long,  ByVal domainHandle as long ,  ByVal classHandle as long ,  ByVal groupHandle as long ,  ByVal subgroupHandle as long  )  as string 
      Dim retStr6 as String * MAXFIELD
      Dim value as long
      value = DLLSubGroupGetName(PID, domainHandle, classHandle, groupHandle, subgroupHandle, retStr6)
      SubGroupGetName=StripTerminator(retStr6)
End  Function 

Function IconGetUIPath (  ByVal PID as long,  ByVal iconHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLIconGetUIPath(PID, iconHandle, retStr3)
      IconGetUIPath=StripTerminator(retStr3)
End  Function 

Function IconGetUIArguments (  ByVal PID as long,  ByVal iconHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLIconGetUIArguments(PID, iconHandle, retStr3)
      IconGetUIArguments=StripTerminator(retStr3)
End  Function 

Function IconGetModelPath (  ByVal PID as long,  ByVal iconHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLIconGetModelPath(PID, iconHandle, retStr3)
      IconGetModelPath=StripTerminator(retStr3)
End  Function 

Function IconGetModelArguments (  ByVal PID as long,  ByVal iconHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLIconGetModelArguments(PID, iconHandle, retStr3)
      IconGetModelArguments=StripTerminator(retStr3)
End  Function 

Function ModuleGetName (  ByVal PID as long,  ByVal moduleHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLModuleGetName(PID, moduleHandle, retStr3)
      ModuleGetName=StripTerminator(retStr3)
End  Function 

Function ModuleGetClass (  ByVal PID as long,  ByVal moduleHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLModuleGetClass(PID, moduleHandle, retStr3)
      ModuleGetClass=StripTerminator(retStr3)
End  Function 

Function ProcessGetSimulation (  ByVal PID as long )  as string 
      Dim retStr2 as String * MAXFIELD
      Dim value as long
      value = DLLProcessGetSimulation(PID, retStr2)
      ProcessGetSimulation=StripTerminator(retStr2)
End  Function 

Function ProcessGetSimulationPath (  ByVal PID as long )  as string 
      Dim retStr2 as String * MAXFIELD
      Dim value as long
      value = DLLProcessGetSimulationPath(PID, retStr2)
      ProcessGetSimulationPath=StripTerminator(retStr2)
End  Function 
