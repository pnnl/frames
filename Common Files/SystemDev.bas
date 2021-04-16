Attribute VB_Name = "SystemDevAPI"
Option Explicit
' API Name: FRAMES System
'-------------------------------------------------------------------------
' Documentation for: OpenINI
' Open the INI and returns a PID. If the Startup.ini file does not exist path is the location where it will be created. The OpenINI function call is the first call made; if OpenINI is never called, all API calls are ignored. All changes to INI are persistent. The OpenINI function call is made with the file path as input (from command line), opens the INI, and returns the process id.
DECLARE Function OpenINI LIB "systemio.dll" Alias "__OpenINI@4" (  ByVal StartupFile as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: CloseINI
' Closes the entire frames session. If cancel is 0 all Startup.ini changes will be written. If cancel is 1 then no changes are written. The CloseINI function call is made with a process id as input and closes the entire FRAMES session.
DECLARE Function CloseINI LIB "systemio.dll" Alias "__CloseINI@8" (  ByVal PID as long,  ByVal cancel as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddNewDictionary
' Create and add dictionary in registry, dictionary name will be set to the parameter dictionary. The dictionary file is expected to be empty. The AddNewDictionary function call is made with a process id and path string as input, and an empty dic string to output the dictionary name. This function will create and add a dictionary.
DECLARE Function AddNewDictionary LIB "systemio.dll" Alias "__AddNewDictionary@12" (  ByVal PID as long,  ByVal dictionaryFile as string,  ByVal dictionaryName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddOpenDictionary
' Open and add dictionary in registry, dictionary returns the dictionary name. The dictionary file is expected to be complete for this function to reach success (return a 0). The AddOpenDictionary function call is made with a process id and path string as input, and an empty dic string to output the dictionary name. This function will add a dictionary to the registry.
DECLARE Function DLLAddOpenDictionary LIB "systemio.dll" Alias "__AddOpenDictionary@12" (  ByVal PID as long,  ByVal dictionaryFile as string,  ByVal dictionary as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DelDictionary
' Delete a dictionary from FRAMES 2.0 Startup.ini. The DelDictionary function call is made with a process id and a dictionary name string as input to delete a dictionary from the registry.
DECLARE Function DelDictionary LIB "systemio.dll" Alias "__DelDictionary@8" (  ByVal PID as long,  ByVal dictionaryName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SaveDictionary
' Saves the contents for a dictionary to disk. The path is associated with the name when the dictionary was created or opened. The SaveDictionary function call is made with a process id and a dictionary name string as input to save a dictionary to the registry.
DECLARE Function SaveDictionary LIB "systemio.dll" Alias "__SaveDictionary@8" (  ByVal PID as long,  ByVal dictionaryName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SaveDictionaryAs
' Save as dictionary to disk but does not included it in the registry. The SaveDictionaryAs function call is made with a process id, a dictionary name string, a path string, and a new dictionary name string as input to save a dictionary not included in the registry.
DECLARE Function SaveDictionaryAs LIB "systemio.dll" Alias "__SaveDictionaryAs@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal dictionaryFile as string,  ByVal dictionaryNewName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DictionaryCount
' Get the count of all dictionaries in registry. The DictionaryCount function call is made with a process id and a dictionary name string as input, and a count integer to output a count of all dictionaries in the registry.
DECLARE Function DictionaryCount LIB "systemio.dll" Alias "__DictionaryCount@8" (  ByVal PID as long, count as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DictionaryList
' Get the list of all dictionaries in registry in a single delimited string. The DictionaryList function call is made with a process id and a delimiter string as input, and an empty list string to output a string of dictionary names separated by commas.
DECLARE Function DLLDictionaryList LIB "systemio.dll" Alias "__DictionaryList@12" (  ByVal PID as long,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetDicDescription
' Get the description of a dictionary.
DECLARE Function DLLGetDicDescription LIB "systemio.dll" Alias "__GetDicDescription@12" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal description as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetDicDescription
' Set the description of a dictionary.
DECLARE Function SetDicDescription LIB "systemio.dll" Alias "__SetDicDescription@12" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal description as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetDicPrivilege
' Get the privilege of a dictionary. The privilege is either 0=System UI, 1=System Boundary Condition, 2=Module UI, or 3=Module Boundary Condition. This represents the level of review a dictionary has received. A system dictionary should have a number of individuals agree that it is complete, useful and mutually exclusive of other system dictionaries. Module level privileges are meant for people to develop their own standards that may become system dictionaries in the future.
DECLARE Function GetDicPrivilege LIB "systemio.dll" Alias "__GetDicPrivilege@12" (  ByVal PID as long,  ByVal dictionaryName as string, privilege as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetDicPrivilege
' Set the privilege of a dictionary.
DECLARE Function SetDicPrivilege LIB "systemio.dll" Alias "__SetDicPrivilege@12" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal privilege as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetDicVersion
' Get the version of a dictionary.
DECLARE Function GetDicVersion LIB "systemio.dll" Alias "__GetDicVersion@12" (  ByVal PID as long,  ByVal dictionaryName as string, version as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetDicVersion
' Set the version of a dictionary.
DECLARE Function SetDicVersion LIB "systemio.dll" Alias "__SetDicVersion@12" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal version as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetDicPath
' Get the path associated with a dictionary. To change the path you need to 1) SaveDictionaryAs changing the path, 2) DelDictionary, and 3) AddOpenDictionary from it new location.
DECLARE Function DLLGetDicPath LIB "systemio.dll" Alias "__GetDicPath@12" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal dictionaryFile as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetDicUpdate
' If the state of a dictionary has changed then update flag will be non-zero.
DECLARE Function GetDicUpdate LIB "systemio.dll" Alias "__GetDicUpdate@12" (  ByVal PID as long,  ByVal dictionaryName as string, update as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddVariable
' The AddVariable function call is made with a process id, a dictionary name string, and a variable string as input to add a variable to a dictionary. The function returns success/fail flag. This function must be called to allocate new space for a parameter. The parameter will be added to the dictionary or dataset by _dicId with the given _name. The parameter will have the default settings of: dimension = 0; description = ""; type = integer; stochastic = false; min = 0; max = 0.
DECLARE Function AddVariable LIB "systemio.dll" Alias "__AddVariable@12" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variable as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DelVariable
' Delete a variable from a dictionary. Warning - Deleting a variable, dictionary, or a dataset can have some serious side effects therefore the software will not allow the use any these delete functions when more than one handle to a simulation exists. (i.e. A module and the system are active, or two system level users are active on a single instance of FRAMES. The DelVariable function call is made with a process id, a dictionary name string, and a variable string as input to delete a variable from a dictionary. The function returns success/fail flag.
DECLARE Function DelVariable LIB "systemio.dll" Alias "__DelVariable@12" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variable as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetVarCount
' Gets the varible count of a dictionary. The GetVarCount function call is made with a process id, a dictionary name string, and a count integer to output a count of all variables in the dictionary. The function returns success/fail flag.
DECLARE Function GetVarCount LIB "systemio.dll" Alias "__GetVarCount@12" (  ByVal PID as long,  ByVal dictionaryName as string, count as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetVarName
' Gets the name of the nth (index) variable in the dictionary.
DECLARE Function DLLGetVarName LIB "systemio.dll" Alias "__GetVarName@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal index as long,  ByVal variable as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetVarList
' Gets the delimited list of all variable names in a dictionary. The function returns success/fail flag.
DECLARE Function DLLGetVarList LIB "systemio.dll" Alias "__GetVarList@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetVarName
' Set the variable name (rename).The SetVarName function call is made with a process id, a dictionary name string, an existing variable name string, and a variable name string as input to change the variable name.
DECLARE Function SetVarName LIB "systemio.dll" Alias "__SetVarName@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string,  ByVal variableNewName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetVarDescription
' Set the variable's description. The SetVarDescription function call is made with a process id, a dictionary name string, a variable name string, and a description string as input to change the description of the variable.
DECLARE Function SetVarDescription LIB "systemio.dll" Alias "__SetVarDescription@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string,  ByVal description as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetVarType
' Set the variable's type. Type can be either "String", "Logical", "Real", or "Integer". The SetVarType function call is made with a process id, a dictionary name string, a variable name string, and a type string as input to change the type parameter of the variable.
DECLARE Function SetVarType LIB "systemio.dll" Alias "__SetVarType@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string,  ByVal idtype as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetVarScalar
' Set the variable's scalar flag. If the flag is 0 then a set of values is stored. (i.e. the dimensionality will be increased by 1). If the flag=1 a single value is stored.
DECLARE Function SetVarScalar LIB "systemio.dll" Alias "__SetVarScalar@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string,  ByVal flag as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetVarMin
' Set the variable's minimum value. Setting both minimum and maximum equal to each other has the effect of having the variable NOT range checked.
DECLARE Function SetVarMin LIB "systemio.dll" Alias "__SetVarMin@20" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string,  ByVal min as double )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetVarMax
' Set the variable's maximum value. Setting both minimum and maximum equal to each other has the effect of having the variable NOT range checked.
DECLARE Function SetVarMax LIB "systemio.dll" Alias "__SetVarMax@20" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string,  ByVal max as double )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetVarMeasure
' Set the variable's measure.
DECLARE Function SetVarMeasure LIB "systemio.dll" Alias "__SetVarMeasure@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string,  ByVal measureName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetVarUnit
' Set the variable's unit for its given measure.
DECLARE Function SetVarUnit LIB "systemio.dll" Alias "__SetVarUnit@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string,  ByVal unitName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetVarStochastic
' Set the variable's stochastic flag. This flag signifies if the parameter can be modified by an another program and not invalidate any calibration. This allows other programs such and Sensativity/Uncertainty or Parameter estimation programs to modify these parameters.
DECLARE Function SetVarStochastic LIB "systemio.dll" Alias "__SetVarStochastic@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string,  ByVal flag as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetVarPreposition
' Set the preposition, this helps in describing parameters in a human readable form. This is the word that would best be used to write a description of this parameters as an index for another. For example the chemical parameter would use "for" as a preposition to make statements such as Concentration for benzene. The preposition greatly facilitates writing descriptive text associated with results. The SetVarPreposition function call is made with a process id, a dictionary name string, a variable name string, and a prep string as input to change the preposition parameter of the variable.
DECLARE Function SetVarPreposition LIB "systemio.dll" Alias "__SetVarPreposition@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string,  ByVal prepositionName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetVarPrimaryKey
' Set the variable primarykey flag. Is this parameter a key to information on other parameters. This is used in both databases and in the representation of the data. For example a variable being used to index other variables frequently (like chemical) would indicate that chemical should be a primary key. The typical meaning of primary key from Database design is not incorrect when used here.
DECLARE Function SetVarPrimaryKey LIB "systemio.dll" Alias "__SetVarPrimaryKey@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string,  ByVal flag as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddVarIndex
' Add a variable index or label index to a variable. The AddVarIndex function call is made with a process id, a dictionary name string, a variable name string, a dictionary reference string, and an index string as input to add a variable index or label index to a variable.
DECLARE Function AddVarIndex LIB "systemio.dll" Alias "__AddVarIndex@20" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string,  ByVal reference as string,  ByVal index as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DelVarIndex
' Delete an indice from a variable. The DelVarIndex function call is made with a process id, a dictionary name string, a variable name string, and an index string as input to delete a variable index or label index to a variable.
DECLARE Function DelVarIndex LIB "systemio.dll" Alias "__DelVarIndex@20" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string,  ByVal reference as string,  ByVal index as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetVarIndexCount
' Get the count of indices for a variable. The GetVarIndexCount function call is made with a process id, a dictionary name string, and a variable name string as input, and an empty count integer to output a count of indices for a variable.
DECLARE Function GetVarIndexCount LIB "systemio.dll" Alias "__GetVarIndexCount@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string, count as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VarIndexList
' Get the list of all indices for a variable. The VarIndexList function call is made with a process id, a dictionary name string, a variable name string, and a delimiter string as input, and an empty list string to output a list of indices for a variable.
DECLARE Function DLLVarIndexList LIB "systemio.dll" Alias "__VarIndexList@20" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: PromoteVarIndex
' Move and index up one level in the order of referred indices.
DECLARE Function PromoteVarIndex LIB "systemio.dll" Alias "__PromoteVarIndex@20" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string,  ByVal reference as string,  ByVal index as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DemoteVarIndex
' Move and index down one level in the order of referred indices.
DECLARE Function DemoteVarIndex LIB "systemio.dll" Alias "__DemoteVarIndex@20" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string,  ByVal reference as string,  ByVal index as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddNewModule
' Create and add a module description in registry module name passed in. The AddNewModule function call is made with a process id, a path string, and a module name string as input to create and add a module description in the registry. It is expected that .mod file does not exist or is empty.
DECLARE Function AddNewModule LIB "systemio.dll" Alias "__AddNewModule@12" (  ByVal PID as long,  ByVal moduleFile as string,  ByVal moduleName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddOpenModule
' Open and add a module description in registry module name passed out. The AddOpenModule function call is made with a process id and a path string as input to open and add a module description in the registry, and a module name string to output the module name.
DECLARE Function DLLAddOpenModule LIB "systemio.dll" Alias "__AddOpenModule@12" (  ByVal PID as long,  ByVal moduleFile as string,  ByVal module as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DelModule
' Delete a module description dataset from registry. The DelModule function call is made with a process id and a module name string as input to delete a module description dataset from the registry.
DECLARE Function DelModule LIB "systemio.dll" Alias "__DelModule@8" (  ByVal PID as long,  ByVal moduleName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SaveModule
' Save the module description dataset to registry. The SaveModule function call is made with a process id and a module name string as input to save a module description dataset in the registry.
DECLARE Function SaveModule LIB "systemio.dll" Alias "__SaveModule@8" (  ByVal PID as long,  ByVal moduleName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SaveModuleAs
' Save the module description as another dataset to registry. The SaveModuleAs function call is made with a process id, an old module name string, a path string, and a new module name string as input to save the module description as another dataset in the registry. The old module name continues to be the open module.
DECLARE Function SaveModuleAs LIB "systemio.dll" Alias "__SaveModuleAs@16" (  ByVal PID as long,  ByVal moduleName as string,  ByVal moduleFile as string,  ByVal moduleNewName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ModuleCount
' Get the count of all modules in registry. The SaveModuleAs function call is made with a process id, an old module name string, a path string, and a new module name string as input to save the module description as another dataset in the registry. The old module name continues to be the open module.
DECLARE Function ModuleCount LIB "systemio.dll" Alias "__ModuleCount@8" (  ByVal PID as long, count as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ModuleList
' Get the list of all module desriptions in registry. The ModuleList function call is made with a process id, a mod string, and a delimiter string as input, and an empty list string to output a comma-separated list of all module descriptions in the registry.
DECLARE Function DLLModuleList LIB "systemio.dll" Alias "__ModuleList@12" (  ByVal PID as long,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetModPath
' Get module path associated with a module.
DECLARE Function DLLGetModPath LIB "systemio.dll" Alias "__GetModPath@12" (  ByVal PID as long,  ByVal moduleName as string,  ByVal moduleFile as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddScheme
' Add a scheme to a module.
DECLARE Function AddScheme LIB "systemio.dll" Alias "__AddScheme@12" (  ByVal PID as long,  ByVal moduleName as string,  ByVal schemeName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DelScheme
' Delete a scheme from a module.
DECLARE Function DelScheme LIB "systemio.dll" Alias "__DelScheme@12" (  ByVal PID as long,  ByVal moduleName as string,  ByVal schemeName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SchemeCount
' Get the number of the schemes for a module.
DECLARE Function SchemeCount LIB "systemio.dll" Alias "__SchemeCount@12" (  ByVal PID as long,  ByVal moduleName as string, count as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SchemeList
' Get the number of the schemes for a module.
DECLARE Function DLLSchemeList LIB "systemio.dll" Alias "__SchemeList@16" (  ByVal PID as long,  ByVal moduleName as string,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddSchemeIDic
' Add a dictionary to a scheme for a module.
DECLARE Function AddSchemeIDic LIB "systemio.dll" Alias "__AddSchemeIDic@16" (  ByVal PID as long,  ByVal moduleName as string,  ByVal schemeName as string,  ByVal dictionaryName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DelSchemeIDic
' Delete a dictionary from a scheme for a module.
DECLARE Function DelSchemeIDic LIB "systemio.dll" Alias "__DelSchemeIDic@16" (  ByVal PID as long,  ByVal moduleName as string,  ByVal schemeName as string,  ByVal dictionaryName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SchemeIDicCount
' Get the number of input dictionaries associated with a scheme for a module.
DECLARE Function SchemeIDicCount LIB "systemio.dll" Alias "__SchemeIDicCount@16" (  ByVal PID as long,  ByVal moduleName as string,  ByVal schemeName as string, count as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SchemeIDicList
' Get a single string that contains all the names of the input dictionaries associated with a scheme for a module.
DECLARE Function DLLSchemeIDicList LIB "systemio.dll" Alias "__SchemeIDicList@20" (  ByVal PID as long,  ByVal moduleName as string,  ByVal schemeName as string,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddSchemeODic
' Add an output dictionary to a scheme for a module.
DECLARE Function AddSchemeODic LIB "systemio.dll" Alias "__AddSchemeODic@16" (  ByVal PID as long,  ByVal moduleName as string,  ByVal schemeName as string,  ByVal dictionaryName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DelSchemeODic
' Delete an output dictionary from a scheme for a module.
DECLARE Function DelSchemeODic LIB "systemio.dll" Alias "__DelSchemeODic@16" (  ByVal PID as long,  ByVal moduleName as string,  ByVal schemeName as string,  ByVal dictionaryName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SchemeODicCount
' Get the number of output dictionaries associated with a scheme for a module.
DECLARE Function SchemeODicCount LIB "systemio.dll" Alias "__SchemeODicCount@16" (  ByVal PID as long,  ByVal moduleName as string,  ByVal schemeName as string, count as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SchemeODicList
' Get a single string that contains all the names of the output dictionaries associated with a scheme for a module.
DECLARE Function DLLSchemeODicList LIB "systemio.dll" Alias "__SchemeODicList@20" (  ByVal PID as long,  ByVal moduleName as string,  ByVal schemeName as string,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetModDimSize
' Get the dimension size for the dataset information about modules.
DECLARE Function GetModDimSize LIB "systemio.dll" Alias "__GetModDimSize@20" (  ByVal PID as long,  ByVal iconName as string,  ByVal variableName as string, indices as Any, count as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetModInteger
' Get an integer from meta data about a module.
DECLARE Function GetModInteger LIB "systemio.dll" Alias "__GetModInteger@24" (  ByVal PID as long,  ByVal iconName as string,  ByVal variableName as string,  ByVal unitName as string, indices as Any, value as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetModDouble
' Get an integer from meta data about a module.
DECLARE Function GetModDouble LIB "systemio.dll" Alias "__GetModDouble@24" (  ByVal PID as long,  ByVal iconName as string,  ByVal variableName as string,  ByVal unitName as string, indices as Any, value as double )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetModLogical
' Get an integer from meta data about a module.
DECLARE Function GetModLogical LIB "systemio.dll" Alias "__GetModLogical@24" (  ByVal PID as long,  ByVal iconName as string,  ByVal variableName as string,  ByVal unitName as string, indices as Any, value as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetModString
' Get an integer from meta data about a module.
DECLARE Function DLLGetModString LIB "systemio.dll" Alias "__GetModString@24" (  ByVal PID as long,  ByVal iconName as string,  ByVal variableName as string,  ByVal unitName as string, indices as Any,  ByVal value as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: PutModInteger
' Put an integer of meta data about a module.
DECLARE Function PutModInteger LIB "systemio.dll" Alias "__PutModInteger@24" (  ByVal PID as long,  ByVal iconName as string,  ByVal variableName as string,  ByVal unitName as string, indices as Any,  ByVal value as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: PutModDouble
' Put a double of meta data about a module.
DECLARE Function PutModDouble LIB "systemio.dll" Alias "__PutModDouble@28" (  ByVal PID as long,  ByVal iconName as string,  ByVal variableName as string,  ByVal unitName as string, indices as Any,  ByVal value as double )  as integer 

'-------------------------------------------------------------------------
' Documentation for: PutModLogical
' Put an logical of meta data about a module.
DECLARE Function PutModLogical LIB "systemio.dll" Alias "__PutModLogical@24" (  ByVal PID as long,  ByVal iconName as string,  ByVal variableName as string,  ByVal unitName as string, indices as Any,  ByVal value as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: PutModString
' Put a string of meta data about a module.
DECLARE Function PutModString LIB "systemio.dll" Alias "__PutModString@24" (  ByVal PID as long,  ByVal iconName as string,  ByVal variableName as string,  ByVal unitName as string, indices as Any,  ByVal value as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddDomain
' Automatically adds the four class types to the domain. The AddDomain function call is made with a process id and a domain name string as input to add a domain; automatically adds the four class types to the domain.
DECLARE Function AddDomain LIB "systemio.dll" Alias "__AddDomain@8" (  ByVal PID as long,  ByVal domainName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DelDomain
' Deletes the domain and it tree. The DelDomain function call is made with a process id and a domain name string as input to delete a domain and its tree, deleting its classes, groups, subgroups, and modules.
DECLARE Function DelDomain LIB "systemio.dll" Alias "__DelDomain@8" (  ByVal PID as long,  ByVal domainName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetDomainIcon
' Get the icon associated with a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels.
DECLARE Function DLLGetDomainIcon LIB "systemio.dll" Alias "__GetDomainIcon@12" (  ByVal PID as long,  ByVal domainName as string,  ByVal domainIconFile as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetDomainIcon
' Set the icon associated with a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels.
DECLARE Function SetDomainIcon LIB "systemio.dll" Alias "__SetDomainIcon@12" (  ByVal PID as long,  ByVal domainName as string,  ByVal domainIconFile as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddClass
' For future use - not presently active. The AddClass function call is made with a process id, a domain name string, and a class name string as input to add a class.
DECLARE Function AddClass LIB "systemio.dll" Alias "__AddClass@12" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DelClass
' For future use - not presently active. The DelClass function call is made with a process id, a domain name string, and a class name string as input to delete a class from a domain, including its groups, subgroups, and modules.
DECLARE Function DelClass LIB "systemio.dll" Alias "__DelClass@12" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetClassIcon
' Get the icon associated with a class in a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels.
DECLARE Function DLLGetClassIcon LIB "systemio.dll" Alias "__GetClassIcon@16" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string,  ByVal classIconFile as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetClassIcon
' Set the icon associated with a class in a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels.
DECLARE Function SetClassIcon LIB "systemio.dll" Alias "__SetClassIcon@16" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string,  ByVal classIconFile as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddGroup
' The AddGroup function call is made with a process id, a domain name string, a class name string, and a group name string as input to add a group to a domain and class.
DECLARE Function AddGroup LIB "systemio.dll" Alias "__AddGroup@16" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string,  ByVal groupName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DelGroup
' The DelGroup function call is made with a process id, a domain name string, a class name string, and a group name string as input to delete a group from a domain and class, including its subgroups and modules.
DECLARE Function DelGroup LIB "systemio.dll" Alias "__DelGroup@16" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string,  ByVal groupName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetGroupIcon
' Get the icon associated with a class in a domain. Icon are either .bmp, .gif files for image. They should be small 32x2 pixels.
DECLARE Function DLLGetGroupIcon LIB "systemio.dll" Alias "__GetGroupIcon@20" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string,  ByVal groupName as string,  ByVal groupIconFile as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetGroupIcon
' Set the icon associated with a class in a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels.
DECLARE Function SetGroupIcon LIB "systemio.dll" Alias "__SetGroupIcon@20" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string,  ByVal groupName as string,  ByVal groupIconFile as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddSubGroup
' The AddSubGroup function call is made with a process id, a domain name string, a class name string, a group name string, and a subgroup name string as input to add a subgroup to a domain, class, and group.
DECLARE Function AddSubGroup LIB "systemio.dll" Alias "__AddSubGroup@20" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string,  ByVal groupName as string,  ByVal subgroupName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DelSubGroup
' The DelSubGroup function call is made with a process id, a domain name string, a class name string, a group name string, and a subgroup name string as input to delete a subgroup from a domain, class, and group, including its modules.
DECLARE Function DelSubGroup LIB "systemio.dll" Alias "__DelSubGroup@20" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string,  ByVal groupName as string,  ByVal subgroupName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetSubGrpIcon
' Get the icon associated with a class in a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels.
DECLARE Function DLLGetSubGrpIcon LIB "systemio.dll" Alias "__GetSubGrpIcon@24" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string,  ByVal groupName as string,  ByVal subgroupName as string,  ByVal subgroupIconFile as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetSubGrpIcon
' Set the icon associated with a class in a domain. Icon are either .bmp, .gif files for image. They should be small 32x32 pixels.
DECLARE Function SetSubGrpIcon LIB "systemio.dll" Alias "__SetSubGrpIcon@24" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string,  ByVal groupName as string,  ByVal subgroupName as string,  ByVal subgroupIconFile as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddGroupModule
' Add a module at the group level. The AddGroupModule function call is made with a process id, a domain name string, a class name string, a group name string, and a module name string as input to add a module to a domain, class, and group.
DECLARE Function AddGroupModule LIB "systemio.dll" Alias "__AddGroupModule@20" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string,  ByVal groupName as string,  ByVal moduleName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DelGroupModule
' Remove a module at the group level. The DelGroupModule function call is made with a process id, a domain name string, a class name string, a group name string, and a module name string as input to delete a module from a domain, class, and group.
DECLARE Function DelGroupModule LIB "systemio.dll" Alias "__DelGroupModule@20" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string,  ByVal groupName as string,  ByVal moduleName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddSubGrpModule
' Add a module at the subgroup level. The AddSubGrpModule function call is made with a process id, a domain name string, a class name string, a group name string, a subgroup name string, and a module name string as input to add a module to a domain, class, group, and subgroup.
DECLARE Function AddSubGrpModule LIB "systemio.dll" Alias "__AddSubGrpModule@24" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string,  ByVal groupName as string,  ByVal subgroupName as string,  ByVal moduleName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DelSubGrpModule
' Remove a module at the subgroup level. The DelSubGrpModule function call is made with a process id, a domain name string, a class name string, a group name string, a subgroup name string, and module name string as input to delete a module from a domain, class, group, and subgroup.
DECLARE Function DelSubGrpModule LIB "systemio.dll" Alias "__DelSubGrpModule@24" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string,  ByVal groupName as string,  ByVal subgroupName as string,  ByVal moduleName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetDomainList
' The GetDomainList function call is made with a process id and a delimiter string as input, and an empty list string to output a comma-separated list of all domains in the registry.
DECLARE Function DLLGetDomainList LIB "systemio.dll" Alias "__GetDomainList@12" (  ByVal PID as long,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetClassList
' The GetClassList function call is made with a process id, a domain name string, and a delimiter string as input, and an empty list string to output a comma-separated list of all classes in a domain.
DECLARE Function DLLGetClassList LIB "systemio.dll" Alias "__GetClassList@16" (  ByVal PID as long,  ByVal domainName as string,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetGroupList
' The GetGroupList function call is made with a process id, a domain name string, a class name string, and a delimiter string as input, and an empty list string to output a comma-separated list of all groups in a domain and class.
DECLARE Function DLLGetGroupList LIB "systemio.dll" Alias "__GetGroupList@20" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetSubGrpList
' The GetSubGrpList function call is made with a process id, a domain name string, a class name string, a group name string, and a delimiter string as input, and an empty list string to output a comma-separated list of all subgroups in a domain, class, and group.
DECLARE Function DLLGetSubGrpList LIB "systemio.dll" Alias "__GetSubGrpList@24" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string,  ByVal groupName as string,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetGroupModList
' The GetGroupModList function call is made with a process id, a domain name string, a class name string, a group name string, and a delimiter string as input, and an empty list string to output a comma-separated list of all subgroups in a domain, class, and group.
DECLARE Function DLLGetGroupModList LIB "systemio.dll" Alias "__GetGroupModList@24" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string,  ByVal groupName as string,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetSubGrpModList
' The GetSubGrpModList function call is made with a process id, a domain name string, a class name string, a group name string, a subgroup name string, and a delimiter string as input, and an empty list string to output a comma-separated list of all subgroups in a domain, class, and group.
DECLARE Function DLLGetSubGrpModList LIB "systemio.dll" Alias "__GetSubGrpModList@28" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string,  ByVal groupName as string,  ByVal subgroupName as string,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetDomainCount
' Get the number of domains in the registery. The GetDomainCount function call is made with a process id as input, and an empty count integer to output a count of all domains in the registry.
DECLARE Function GetDomainCount LIB "systemio.dll" Alias "__GetDomainCount@8" (  ByVal PID as long, count as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetClassCount
' Get the number of classes in a domain. The GetClassCount function call is made with a process id and a domain name string as input, and an empty count integer to output a count of all classes in a domain.
DECLARE Function GetClassCount LIB "systemio.dll" Alias "__GetClassCount@12" (  ByVal PID as long,  ByVal domainName as string, count as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetGroupCount
' Get the number of groups in a class, and domain. The GetGroupCount function call is made with a process id, a domain name string, and a class name string as input, and an empty count integer to output a count of all groups in a domain and class.
DECLARE Function GetGroupCount LIB "systemio.dll" Alias "__GetGroupCount@16" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string, count as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetSubGrpCount
' Get the number of subgroups in a group, class and domain.The GetSubGrpCount function call is made with a process id, a domain name string, a class name string, and a group name string as input, and an empty count integer to output a count of all subgroups in a domain, class, and group.
DECLARE Function GetSubGrpCount LIB "systemio.dll" Alias "__GetSubGrpCount@20" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string,  ByVal groupName as string, count as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetGroupModCount
' Get the number of modules at the group level for a group, class, and domain. The GetGroupModCount function call is made with a process id, a domain name string, a class name string, and a group name string as input, and an empty count integer to output a count of all modules in a domain, class, and group.
DECLARE Function GetGroupModCount LIB "systemio.dll" Alias "__GetGroupModCount@20" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string,  ByVal groupName as string, count as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetSubGrpModCount
' Get the number of modules at the subgroup level for a subgroup, group, class, and domain. The GetSubGrpModCount function call is made with a process id, a domain name string, a class name string, a group name string, and a subgroup name string as input, and an empty count integer to output a count of all modules in a domain, class, group, and subgroup.
DECLARE Function GetSubGrpModCount LIB "systemio.dll" Alias "__GetSubGrpModCount@24" (  ByVal PID as long,  ByVal domainName as string,  ByVal className as string,  ByVal groupName as string,  ByVal subgroupName as string, count as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: NewSim
' Open a new simulation return a simulation dataset name. The NewSim function call is made with a process id and a filename string as input to open a new simulation. Only one simulation is allowed to be open at a time in a FRAMES session, however, multiple FRAMES sessions may be open at the same time.
DECLARE Function NewSim LIB "systemio.dll" Alias "__NewSim@12" (  ByVal PID as long,  ByVal simulationFile as string,  ByVal simulationName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: OpenSim
' Open an existing simulation return a simulation dataset name. The OpenSim function call is made with a process id and a filename string as input to open an existing simulation.
DECLARE Function DLLOpenSim LIB "systemio.dll" Alias "__OpenSim@12" (  ByVal PID as long,  ByVal simulationFile as string,  ByVal simulation as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SaveSim
' Save the simulation present state. The SaveSim function call is made with a process id as input to save the simulation's present state.
DECLARE Function SaveSim LIB "systemio.dll" Alias "__SaveSim@4" (  ByVal PID as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SaveSimAs
' Open an exsisting simulation return a simId. The SaveSimAs function call is made with a process id and a filename string (new name) as input to save a simulation (SDE) as another name, but continue to work in same simulation name.
DECLARE Function SaveSimAs LIB "systemio.dll" Alias "__SaveSimAs@12" (  ByVal PID as long,  ByVal simulationFile as string,  ByVal simulationNewName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: CloseSim
' Close the simulation without saving. The CloseSim function call is made with a process id as input to close the simulation without saving.
DECLARE Function CloseSim LIB "systemio.dll" Alias "__CloseSim@4" (  ByVal PID as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetSim
' Get current simulation name for which all simulation functions will use for this PID.
DECLARE Function DLLGetSim LIB "systemio.dll" Alias "__GetSim@12" (  ByVal PID as long,  ByVal simulationFile as string,  ByVal simulation as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetSim
' Set current simulation name for which all simulation functions will use for this PID.
DECLARE Function SetSim LIB "systemio.dll" Alias "__SetSim@8" (  ByVal PID as long,  ByVal simulationName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetSimList
' Get the list of all simulations loaded in the environment.
DECLARE Function DLLGetSimList LIB "systemio.dll" Alias "__GetSimList@12" (  ByVal PID as long,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddNewDataSet
' Create a new dataset using a registered dictionary. The AddNewDataSet function call is made with a process id, a dictionary name string, a path name string, and a set string as input to create a new dataset using a registered dictionary.
DECLARE Function AddNewDataSet LIB "systemio.dll" Alias "__AddNewDataSet@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal datasetFile as string,  ByVal datasetName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddOpenDataSet
' Open a dataset, dataset file must exist or an error will occur. The AddOpenDataSet function call is made with a process id, a path name string, and a set string as input to open an existing dataset to the registry.
DECLARE Function DLLAddOpenDataSet LIB "systemio.dll" Alias "__AddOpenDataSet@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal datasetFile as string,  ByVal dset as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DelDataSet
' Delete a dataset. The DelDataSet function call is made with a process id and a set string as input to delete a dataset. Does not write the dataset before deletion.
DECLARE Function DelDataSet LIB "systemio.dll" Alias "__DelDataSet@8" (  ByVal PID as long,  ByVal datasetName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SaveDataSet
' Save the dataset. The SaveDataSet function call is made with a process id and a set string as input to save a dataset.
DECLARE Function SaveDataSet LIB "systemio.dll" Alias "__SaveDataSet@8" (  ByVal PID as long,  ByVal datasetName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SaveDataSetAs
' Save the dataset with a different name. The SaveDataSetAs function call is made with a process id, an oldSet string, a path string, and a new dataset string as input to save a dataset under a new name.
DECLARE Function SaveDataSetAs LIB "systemio.dll" Alias "__SaveDataSetAs@16" (  ByVal PID as long,  ByVal datasetName as string,  ByVal datasetFile as string,  ByVal datasetNewName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetDataSetList
' Get the list of all the datasets for simulation.
DECLARE Function DLLGetDataSetList LIB "systemio.dll" Alias "__GetDataSetList@12" (  ByVal PID as long,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetDataSetPath
' Get the path associated with a dictionary. To change the path you need to 1) SaveDictionaryAs changing the path, 2) DelDictionary, and 3) AddOpenDictionary from it new location.
DECLARE Function DLLGetDataSetPath LIB "systemio.dll" Alias "__GetDataSetPath@12" (  ByVal PID as long,  ByVal datasetName as string,  ByVal datasetFile as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddLink
' Returns SUCCESS if the connection is made in the simulation. The AddLink function call is made with a process id, a modIdFrom string, and a modIdTo string as input to connect two modules, and returns a success/fail flag. The arrow of the connection will point from -> to.
DECLARE Function AddLink LIB "systemio.dll" Alias "__AddLink@12" (  ByVal PID as long,  ByVal idfrom as string,  ByVal idto as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DelLink
' Returns SUCCESS if the connection is dropped in the simulation. The DelLink function call is made with a process id, a modIdFrom string, and a modIdTo string as input to delete the connection between two modules, and returns a success/fail flag.
DECLARE Function DelLink LIB "systemio.dll" Alias "__DelLink@12" (  ByVal PID as long,  ByVal fromid as string,  ByVal toid as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddIcon
' Returns the module ID added to the simulation. if scope is 0 then the icon is added to the global diagram. if scope is non zero it is added to the local diagram.
DECLARE Function DLLAddIcon LIB "systemio.dll" Alias "__AddIcon@28" (  ByVal PID as long,  ByVal scope as long,  ByVal domainName as string,  ByVal className as string,  ByVal groupName as string,  ByVal subgroupName as string,  ByVal id as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DelIcon
' Deletes the module ID from the simulation. The DelIcon function call is made with a process id and a id string as input to delete the module ID from the simulation.
DECLARE Function DelIcon LIB "systemio.dll" Alias "__DelIcon@8" (  ByVal PID as long,  ByVal iconName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetIconUIDic
' Returns the dic for the module icon returns "" when no set exists.
DECLARE Function DLLGetIconUIDic LIB "systemio.dll" Alias "__GetIconUIDic@12" (  ByVal PID as long,  ByVal iconName as string,  ByVal dictionary as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetIconUISet
' Returns the set for the module icon returns "" when no set exists.
DECLARE Function DLLGetIconUISet LIB "systemio.dll" Alias "__GetIconUISet@12" (  ByVal PID as long,  ByVal iconName as string,  ByVal dataset as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetIconMod
' Sets module and scheme associated with an icon. A subset of the datasets the icon consumes is given in the list parameter.
DECLARE Function SetIconMod LIB "systemio.dll" Alias "__SetIconMod@24" (  ByVal PID as long,  ByVal iconName as string,  ByVal moduleName as string,  ByVal schemeName as string,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetIconMod
' Sets module and scheme associated with an icon. A list of datasets is returned in the list parameter.
DECLARE Function DLLGetIconMod LIB "systemio.dll" Alias "__GetIconMod@16" (  ByVal PID as long,  ByVal iconName as string,  ByVal module as string,  ByVal scheme as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetIconUIArgs
' Get the commandline arguments for a given id. The active part of the icon is the user interface that requires the user to be active during its execution.
DECLARE Function DLLGetIconUIArgs LIB "systemio.dll" Alias "__GetIconUIArgs@16" (  ByVal PID as long,  ByVal iconName as string,  ByVal uiExecutableFile as string,  ByVal cmdLine as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: RunIconUI
' The active part of the icon is the user interface that requires the user to be active during its execution. The RunIconActive function call is made with a process id and a modId string as input to run the active module. Returns a success/fail flag. See GetIconActiveArgs for the cmdline parameter.
DECLARE Function RunIconUI LIB "systemio.dll" Alias "__RunIconUI@24" (  ByVal PID as long,  ByVal iconName as string,  ByVal uiExecutableFile as string,  ByVal cmdLine as string,  ByVal warningFile as string,  ByVal errorFile as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetIconModelArgs
' The passive part of the icon is the model itself that requires NO user to be active during its execution.
DECLARE Function DLLGetIconModelArgs LIB "systemio.dll" Alias "__GetIconModelArgs@16" (  ByVal PID as long,  ByVal iconName as string,  ByVal modelExecutableFile as string,  ByVal cmdLine as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: RunIconModel
' The passive part of the icon is the model itself that requires NO user to be active during its execution. The RunModPassive function call is made with a process id and a modId string as input to run a passive module. Returns a success/fail flag.
DECLARE Function RunIconModel LIB "systemio.dll" Alias "__RunIconModel@24" (  ByVal PID as long,  ByVal iconName as string,  ByVal modelExecutableFile as string,  ByVal cmdLine as string,  ByVal warningFile as string,  ByVal errorFile as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IsIconUIOk
' Returns SUCCESS if no error during the execution of the user interface.
DECLARE Function IsIconUIOk LIB "systemio.dll" Alias "__IsIconUIOk@8" (  ByVal PID as long,  ByVal iconName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IsIconModelOk
' Returns SUCCESS if no error during the execution of the model.
DECLARE Function IsIconModelOk LIB "systemio.dll" Alias "__IsIconModelOk@8" (  ByVal PID as long,  ByVal iconName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IsIconInfoOk
' Returns SUCCESS if no error if the information for the module is complete (i.e. module and scheme have been chosen).
DECLARE Function IsIconInfoOk LIB "systemio.dll" Alias "__IsIconInfoOk@8" (  ByVal PID as long,  ByVal iconName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: RunBetweenIcons
' Returns SUCCESS if no error.
DECLARE Function RunBetweenIcons LIB "systemio.dll" Alias "__RunBetweenIcons@12" (  ByVal PID as long,  ByVal idBegin as string,  ByVal idEnd as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetState
' Abitrarily set the state of an icon.
DECLARE Function SetState LIB "systemio.dll" Alias "__SetState@16" (  ByVal PID as long,  ByVal iconName as string,  ByVal state as long,  ByVal downstate as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: LaunchTool
' Launches the specified module.
DECLARE Function LaunchTool LIB "systemio.dll" Alias "__LaunchTool@16" (  ByVal PID as long,  ByVal moduleName as string,  ByVal isSystemTool as long,  ByVal iconId as string )  as integer 

Function AddOpenDictionary (  ByVal PID as long,  ByVal dictionaryFile as string  , dictionary as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      AddOpenDictionary = DLLAddOpenDictionary(PID, dictionaryFile, retstr3)
      dictionary=StripTerminator(retStr3)
End  Function 

Function DictionaryList (  ByVal PID as long,  ByVal delimiter as string  , list as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      DictionaryList = DLLDictionaryList(PID, delimiter, retstr3)
      list=StripTerminator(retStr3)
End  Function 

Function GetDicDescription (  ByVal PID as long,  ByVal dictionaryName as string  , description as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      GetDicDescription = DLLGetDicDescription(PID, dictionaryName, retstr3)
      description=StripTerminator(retStr3)
End  Function 

Function GetDicPath (  ByVal PID as long,  ByVal dictionaryName as string  , dictionaryFile as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      GetDicPath = DLLGetDicPath(PID, dictionaryName, retstr3)
      dictionaryFile=StripTerminator(retStr3)
End  Function 

Function GetVarName (  ByVal PID as long,  ByVal dictionaryName as string  ,  ByVal index as long, variable as string   )  as integer 
      Dim retStr4 as String * MAXFIELD
      GetVarName = DLLGetVarName(PID, dictionaryName, index, retstr4)
      variable=StripTerminator(retStr4)
End  Function 

Function GetVarList (  ByVal PID as long,  ByVal dictionaryName as string  ,  ByVal delimiter as string  , list as string   )  as integer 
      Dim retStr4 as String * MAXFIELD
      GetVarList = DLLGetVarList(PID, dictionaryName, delimiter, retstr4)
      list=StripTerminator(retStr4)
End  Function 

Function VarIndexList (  ByVal PID as long,  ByVal dictionaryName as string  ,  ByVal variableName as string  ,  ByVal delimiter as string  , list as string   )  as integer 
      Dim retStr5 as String * MAXFIELD
      VarIndexList = DLLVarIndexList(PID, dictionaryName, variableName, delimiter, retstr5)
      list=StripTerminator(retStr5)
End  Function 

Function AddOpenModule (  ByVal PID as long,  ByVal moduleFile as string  , module as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      AddOpenModule = DLLAddOpenModule(PID, moduleFile, retstr3)
      module=StripTerminator(retStr3)
End  Function 

Function ModuleList (  ByVal PID as long,  ByVal delimiter as string  , list as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      ModuleList = DLLModuleList(PID, delimiter, retstr3)
      list=StripTerminator(retStr3)
End  Function 

Function GetModPath (  ByVal PID as long,  ByVal moduleName as string  , moduleFile as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      GetModPath = DLLGetModPath(PID, moduleName, retstr3)
      moduleFile=StripTerminator(retStr3)
End  Function 

Function SchemeList (  ByVal PID as long,  ByVal moduleName as string  ,  ByVal delimiter as string  , list as string   )  as integer 
      Dim retStr4 as String * MAXFIELD
      SchemeList = DLLSchemeList(PID, moduleName, delimiter, retstr4)
      list=StripTerminator(retStr4)
End  Function 

Function SchemeIDicList (  ByVal PID as long,  ByVal moduleName as string  ,  ByVal schemeName as string  ,  ByVal delimiter as string  , list as string   )  as integer 
      Dim retStr5 as String * MAXFIELD
      SchemeIDicList = DLLSchemeIDicList(PID, moduleName, schemeName, delimiter, retstr5)
      list=StripTerminator(retStr5)
End  Function 

Function SchemeODicList (  ByVal PID as long,  ByVal moduleName as string  ,  ByVal schemeName as string  ,  ByVal delimiter as string  , list as string   )  as integer 
      Dim retStr5 as String * MAXFIELD
      SchemeODicList = DLLSchemeODicList(PID, moduleName, schemeName, delimiter, retstr5)
      list=StripTerminator(retStr5)
End  Function 

Function GetModString (  ByVal PID as long,  ByVal iconName as string  ,  ByVal variableName as string  ,  ByVal unitName as string  , indices as tindex, value as string   )  as integer 
      Dim retStr6 as String * MAXFIELD
      GetModString = DLLGetModString(PID, iconName, variableName, unitName, indices, retstr6)
      value=StripTerminator(retStr6)
End  Function 

Function GetDomainIcon (  ByVal PID as long,  ByVal domainName as string  , domainIconFile as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      GetDomainIcon = DLLGetDomainIcon(PID, domainName, retstr3)
      domainIconFile=StripTerminator(retStr3)
End  Function 

Function GetClassIcon (  ByVal PID as long,  ByVal domainName as string  ,  ByVal className as string  , classIconFile as string   )  as integer 
      Dim retStr4 as String * MAXFIELD
      GetClassIcon = DLLGetClassIcon(PID, domainName, className, retstr4)
      classIconFile=StripTerminator(retStr4)
End  Function 

Function GetGroupIcon (  ByVal PID as long,  ByVal domainName as string  ,  ByVal className as string  ,  ByVal groupName as string  , groupIconFile as string   )  as integer 
      Dim retStr5 as String * MAXFIELD
      GetGroupIcon = DLLGetGroupIcon(PID, domainName, className, groupName, retstr5)
      groupIconFile=StripTerminator(retStr5)
End  Function 

Function GetSubGrpIcon (  ByVal PID as long,  ByVal domainName as string  ,  ByVal className as string  ,  ByVal groupName as string  ,  ByVal subgroupName as string  , subgroupIconFile as string   )  as integer 
      Dim retStr6 as String * MAXFIELD
      GetSubGrpIcon = DLLGetSubGrpIcon(PID, domainName, className, groupName, subgroupName, retstr6)
      subgroupIconFile=StripTerminator(retStr6)
End  Function 

Function GetDomainList (  ByVal PID as long,  ByVal delimiter as string  , list as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      GetDomainList = DLLGetDomainList(PID, delimiter, retstr3)
      list=StripTerminator(retStr3)
End  Function 

Function GetClassList (  ByVal PID as long,  ByVal domainName as string  ,  ByVal delimiter as string  , list as string   )  as integer 
      Dim retStr4 as String * MAXFIELD
      GetClassList = DLLGetClassList(PID, domainName, delimiter, retstr4)
      list=StripTerminator(retStr4)
End  Function 

Function GetGroupList (  ByVal PID as long,  ByVal domainName as string  ,  ByVal className as string  ,  ByVal delimiter as string  , list as string   )  as integer 
      Dim retStr5 as String * MAXFIELD
      GetGroupList = DLLGetGroupList(PID, domainName, className, delimiter, retstr5)
      list=StripTerminator(retStr5)
End  Function 

Function GetSubGrpList (  ByVal PID as long,  ByVal domainName as string  ,  ByVal className as string  ,  ByVal groupName as string  ,  ByVal delimiter as string  , list as string   )  as integer 
      Dim retStr6 as String * MAXFIELD
      GetSubGrpList = DLLGetSubGrpList(PID, domainName, className, groupName, delimiter, retstr6)
      list=StripTerminator(retStr6)
End  Function 

Function GetGroupModList (  ByVal PID as long,  ByVal domainName as string  ,  ByVal className as string  ,  ByVal groupName as string  ,  ByVal delimiter as string  , list as string   )  as integer 
      Dim retStr6 as String * MAXFIELD
      GetGroupModList = DLLGetGroupModList(PID, domainName, className, groupName, delimiter, retstr6)
      list=StripTerminator(retStr6)
End  Function 

Function GetSubGrpModList (  ByVal PID as long,  ByVal domainName as string  ,  ByVal className as string  ,  ByVal groupName as string  ,  ByVal subgroupName as string  ,  ByVal delimiter as string  , list as string   )  as integer 
      Dim retStr7 as String * MAXFIELD
      GetSubGrpModList = DLLGetSubGrpModList(PID, domainName, className, groupName, subgroupName, delimiter, retstr7)
      list=StripTerminator(retStr7)
End  Function 

Function OpenSim (  ByVal PID as long,  ByVal simulationFile as string  , simulation as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      OpenSim = DLLOpenSim(PID, simulationFile, retstr3)
      simulation=StripTerminator(retStr3)
End  Function 

Function GetSim (  ByVal PID as long, simulationFile as string  , simulation as string   )  as integer 
      Dim retStr2 as String * MAXFIELD
      Dim retStr3 as String * MAXFIELD
      GetSim = DLLGetSim(PID, retstr2, retstr3)
      simulationFile=StripTerminator(retStr2)
simulation=StripTerminator(retStr3)
End  Function 

Function GetSimList (  ByVal PID as long,  ByVal delimiter as string  , list as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      GetSimList = DLLGetSimList(PID, delimiter, retstr3)
      list=StripTerminator(retStr3)
End  Function 

Function AddOpenDataSet (  ByVal PID as long,  ByVal dictionaryName as string  ,  ByVal datasetFile as string  , dset as string   )  as integer 
      Dim retStr4 as String * MAXFIELD
      AddOpenDataSet = DLLAddOpenDataSet(PID, dictionaryName, datasetFile, retstr4)
      dset=StripTerminator(retStr4)
End  Function 

Function GetDataSetList (  ByVal PID as long,  ByVal delimiter as string  , list as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      GetDataSetList = DLLGetDataSetList(PID, delimiter, retstr3)
      list=StripTerminator(retStr3)
End  Function 

Function GetDataSetPath (  ByVal PID as long,  ByVal datasetName as string  , datasetFile as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      GetDataSetPath = DLLGetDataSetPath(PID, datasetName, retstr3)
      datasetFile=StripTerminator(retStr3)
End  Function 

Function AddIcon (  ByVal PID as long,  ByVal scope as long,  ByVal domainName as string  ,  ByVal className as string  ,  ByVal groupName as string  ,  ByVal subgroupName as string  , id as string   )  as integer 
      Dim retStr7 as String * MAXFIELD
      AddIcon = DLLAddIcon(PID, scope, domainName, className, groupName, subgroupName, retstr7)
      id=StripTerminator(retStr7)
End  Function 

Function GetIconUIDic (  ByVal PID as long,  ByVal iconName as string  , dictionary as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      GetIconUIDic = DLLGetIconUIDic(PID, iconName, retstr3)
      dictionary=StripTerminator(retStr3)
End  Function 

Function GetIconUISet (  ByVal PID as long,  ByVal iconName as string  , dataset as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      GetIconUISet = DLLGetIconUISet(PID, iconName, retstr3)
      dataset=StripTerminator(retStr3)
End  Function 

Function GetIconMod (  ByVal PID as long,  ByVal iconName as string  , module as string  , scheme as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      Dim retStr4 as String * MAXFIELD
      GetIconMod = DLLGetIconMod(PID, iconName, retstr3, retstr4)
      module=StripTerminator(retStr3)
scheme=StripTerminator(retStr4)
End  Function 

Function GetIconUIArgs (  ByVal PID as long,  ByVal iconName as string  , uiExecutableFile as string  , cmdLine as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      Dim retStr4 as String * MAXFIELD
      GetIconUIArgs = DLLGetIconUIArgs(PID, iconName, retstr3, retstr4)
      uiExecutableFile=StripTerminator(retStr3)
cmdLine=StripTerminator(retStr4)
End  Function 

Function GetIconModelArgs (  ByVal PID as long,  ByVal iconName as string  , modelExecutableFile as string  , cmdLine as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      Dim retStr4 as String * MAXFIELD
      GetIconModelArgs = DLLGetIconModelArgs(PID, iconName, retstr3, retstr4)
      modelExecutableFile=StripTerminator(retStr3)
cmdLine=StripTerminator(retStr4)
End  Function 
