Attribute VB_Name = "DataSetAPI"
Option Explicit
' API Name: FRAMES Data Set
'-------------------------------------------------------------------------
' Documentation for: IOOk
' This function checks to see if the status of the Input/Output system. A value of SUCCESS is returned if the system is "Ok".
DECLARE Function IOOk LIB "systemio.dll" Alias "__IOOk@4" (  ByVal PID as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: OpenIO
' OpenIO returns the modules Process IDentification (PID). If the returned pid is less than zero an error occurred. The path\name arguments are to the project file the PID, path, name and modId are given to the module via the command line. For nearly all functions in the API an integer value is returned. The integer value represents whether the function called succeeded or not. A value of SUCCESS indicates that the function did succeed. In error cases a negative value is returned that defines the error. There are functions available to discovery what the error code means.
DECLARE Function OpenIO LIB "systemio.dll" Alias "__OpenIO@12" (  ByVal simulationPath as string,  ByVal simulationName as string,  ByVal iconName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: CloseIO
' This functions closes the INI file and cleans all resources associated with SystemIO.dll. If cancel is 0 then the updated datasets are saved otherwise the updated datasets are thrown away.
DECLARE Sub CloseIO LIB "systemio.dll" Alias "__CloseIO@8" (  ByVal PID as long,  ByVal cancel as long ) 

'-------------------------------------------------------------------------
' Documentation for: NumIMod
' Get the number of modules this module is consuming
DECLARE Function NumIMod LIB "systemio.dll" Alias "__NumIMod@12" (  ByVal PID as long,  ByVal iconName as string, count as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: NumOMod
' Get the number of modules this module is producing to
DECLARE Function NumOMod LIB "systemio.dll" Alias "__NumOMod@12" (  ByVal PID as long,  ByVal iconName as string, count as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: NumIModSet
' Get the number of input datasets from _modId.
DECLARE Function NumIModSet LIB "systemio.dll" Alias "__NumIModSet@16" (  ByVal PID as long,  ByVal iconName as string,  ByVal fromIconName as string, count as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: NumOModSet
' Get the number of output datasets from _modId.
DECLARE Function NumOModSet LIB "systemio.dll" Alias "__NumOModSet@16" (  ByVal PID as long,  ByVal iconName as string,  ByVal toIconName as string, count as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: NumIDicDataSet
' Get the number of datasets related to this input dictionary.
DECLARE Function NumIDicDataSet LIB "systemio.dll" Alias "__NumIDicDataSet@16" (  ByVal PID as long,  ByVal iconName as string,  ByVal dictionaryName as string, count as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: NumODicDataSet
' Get the number of datasets related to this output dictionary.
DECLARE Function NumODicDataSet LIB "systemio.dll" Alias "__NumODicDataSet@16" (  ByVal PID as long,  ByVal iconName as string,  ByVal dictionaryName as string, count as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetIDicDataSet
' Get the input dataset name at _idx returned via _set.
DECLARE Function DLLGetIDicDataSet LIB "systemio.dll" Alias "__GetIDicDataSet@20" (  ByVal PID as long,  ByVal iconName as string,  ByVal dictionaryName as string,  ByVal index as long,  ByVal inputDataset as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetODicDataSet
' Get the output dataset name at _idx returned via _set.
DECLARE Function DLLGetODicDataSet LIB "systemio.dll" Alias "__GetODicDataSet@20" (  ByVal PID as long,  ByVal iconName as string,  ByVal dictionaryName as string,  ByVal index as long,  ByVal outputDataset as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetIModId
' Get an input modules ID at _idx returned via _modId, _idx from 1 to _NumIMod.
DECLARE Function DLLGetIModId LIB "systemio.dll" Alias "__GetIModId@16" (  ByVal PID as long,  ByVal iconName as string,  ByVal index as long,  ByVal inputModuleId as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetOModId
' Get an output modules ID at _idx returned via _modId, _idx from 1 to _NumOMod.
DECLARE Function DLLGetOModId LIB "systemio.dll" Alias "__GetOModId@16" (  ByVal PID as long,  ByVal iconName as string,  ByVal index as long,  ByVal outputModuleId as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetIDictionary
' Get an input dictionary name at _idx returned via _dic.
DECLARE Function DLLGetIDictionary LIB "systemio.dll" Alias "__GetIDictionary@20" (  ByVal PID as long,  ByVal iconName as string,  ByVal fromIconName as string,  ByVal index as long,  ByVal inputDictionary as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetODictionary
' Get an output dictionary name at _idx returned via _dic.
DECLARE Function DLLGetODictionary LIB "systemio.dll" Alias "__GetODictionary@20" (  ByVal PID as long,  ByVal iconName as string,  ByVal toIconName as string,  ByVal index as long,  ByVal outputDictionary as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetDicId
' Gets dictionary for specified dataset.
DECLARE Function DLLGetDicId LIB "systemio.dll" Alias "__GetDicId@12" (  ByVal PID as long,  ByVal datasetName as string,  ByVal dictionary as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetIDataSet
' Get the input dataset name at _idx returned via _set.
DECLARE Function DLLGetIDataSet LIB "systemio.dll" Alias "__GetIDataSet@20" (  ByVal PID as long,  ByVal iconName as string,  ByVal fromIconName as string,  ByVal index as long,  ByVal inputDataset as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetODataSet
' Get the output dataset name at _idx returned via _set.
DECLARE Function DLLGetODataSet LIB "systemio.dll" Alias "__GetODataSet@20" (  ByVal PID as long,  ByVal iconName as string,  ByVal toIconName as string,  ByVal index as long,  ByVal outputDataset as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetIModList
' List of input module names.
DECLARE Function DLLGetIModList LIB "systemio.dll" Alias "__GetIModList@16" (  ByVal PID as long,  ByVal iconName as string,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetOModList
' List of output module names.
DECLARE Function DLLGetOModList LIB "systemio.dll" Alias "__GetOModList@16" (  ByVal PID as long,  ByVal iconName as string,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetIDicList
' List of input module dictionaries.
DECLARE Function DLLGetIDicList LIB "systemio.dll" Alias "__GetIDicList@20" (  ByVal PID as long,  ByVal iconName as string,  ByVal fromIconName as string,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetODicList
' List of output module dictionaries.
DECLARE Function DLLGetODicList LIB "systemio.dll" Alias "__GetODicList@20" (  ByVal PID as long,  ByVal iconName as string,  ByVal toIconName as string,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetISetList
' List of input module dataset names.
DECLARE Function DLLGetISetList LIB "systemio.dll" Alias "__GetISetList@20" (  ByVal PID as long,  ByVal iconName as string,  ByVal fromIconName as string,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetOSetList
' List of output module dataset names.
DECLARE Function DLLGetOSetList LIB "systemio.dll" Alias "__GetOSetList@20" (  ByVal PID as long,  ByVal iconName as string,  ByVal toIconName as string,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetVarDescription
' Get the variable description.
DECLARE Function DLLGetVarDescription LIB "systemio.dll" Alias "__GetVarDescription@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string,  ByVal description as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetVarType
' Get the varible data type. The returned string will be "Integer","String","Real", or "Logical"
DECLARE Function DLLGetVarType LIB "systemio.dll" Alias "__GetVarType@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string,  ByVal vtype as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetVarScalar
' Get the variables scalar flag.
DECLARE Function GetVarScalar LIB "systemio.dll" Alias "__GetVarScalar@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string, flag as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetVarMin
' Get the min value for ints and doubles, length for strings, empty for logicals.
DECLARE Function GetVarMin LIB "systemio.dll" Alias "__GetVarMin@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string, min as double )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetVarMax
' Get the max value for ints and doubles, length for strings, empty for logicals.
DECLARE Function GetVarMax LIB "systemio.dll" Alias "__GetVarMax@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string, max as double )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetVarMeasure
' Get the variable measurement type.
DECLARE Function DLLGetVarMeasure LIB "systemio.dll" Alias "__GetVarMeasure@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string,  ByVal measure as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetVarUnit
' Get the variable unit of measure.
DECLARE Function DLLGetVarUnit LIB "systemio.dll" Alias "__GetVarUnit@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string,  ByVal unit as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetVarStochastic
' Get the variables stochastic flag. This flag signifies if the parameter can be modified by an another program and not invalidate any calibration. This allows other programs such and Sensativity/Uncertainty or Parameter estimation programs to modify these parameters.
DECLARE Function GetVarStochastic LIB "systemio.dll" Alias "__GetVarStochastic@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string, flag as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetVarPreposition
' Get the variables preposition. This is the word that would best be used to write a description of this parameters as an index for another. For example the chemical parameter would use "for" as a preposition to make statements such as Concentration for benzene. The preposition greatly facilitates writing descriptive text associated with results.
DECLARE Function DLLGetVarPreposition LIB "systemio.dll" Alias "__GetVarPreposition@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string,  ByVal preposition as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetVarPrimaryKey
' Get the variables primarykey flag. Is this parameter a key to information on other parameters. This is used in both databases and in the representation of the data. For example a variable being used to index other variables frequently (like chemical) would indicate that chemical should be a primary key. The typical meaning of primary key from Database design is not incorrect when used here.
DECLARE Function GetVarPrimaryKey LIB "systemio.dll" Alias "__GetVarPrimaryKey@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string, flag as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetVarDimension
' 
DECLARE Function GetVarDimension LIB "systemio.dll" Alias "__GetVarDimension@16" (  ByVal PID as long,  ByVal dictionaryName as string,  ByVal variableName as string, dimension as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetVarDimSize
' Gets the size of the specified dimensions. The indices parameters define an array of integers that define the index values that are to be used.
DECLARE Function GetVarDimSize LIB "systemio.dll" Alias "__GetVarDimSize@20" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string, indices as Any, count as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VarLookUp
' Lookup an index into the array. The indices parameters define an array of integers that define the index values that are to be used. The specified string value is searched in the variable for the given index values and the last index value is set. A negative number indicates the element could not be found.
DECLARE Function VarLookUp LIB "systemio.dll" Alias "__VarLookUp@20" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal value as string, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ClearVariable
' Deletes all values associated with a variable.
DECLARE Function ClearVariable LIB "systemio.dll" Alias "__ClearVariable@16" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetInteger
' Get an Integer from a dataset given the variable name, units and a set of indices.
DECLARE Function GetInteger LIB "systemio.dll" Alias "__GetInteger@24" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string, indices as Any, value as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetFloat
' Get an Float from a dataset given the variable name, units and a set of indices.
DECLARE Function GetFloat LIB "systemio.dll" Alias "__GetFloat@24" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string, indices as Any, value as double )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetLogical
' Get an Boolean(Logical) from a dataset given the variable name, units and a set of indices. Booleans are treated as integers with either a value of 1 or 0.
DECLARE Function GetLogical LIB "systemio.dll" Alias "__GetLogical@24" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string, indices as Any, value as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetString
' Get an String(Character(*)) from a dataset given the variable name, units and a set of indices.
DECLARE Function DLLGetString LIB "systemio.dll" Alias "__GetString@24" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string, indices as Any,  ByVal value as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: PutInteger
' Put an Integer in a dataset given the variable name, units and a set of indices.
DECLARE Function PutInteger LIB "systemio.dll" Alias "__PutInteger@24" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string, indices as Any,  ByVal value as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: PutFloat
' Put an Floating Point (Real) in a dataset given the variable name, units and a set of indices.
DECLARE Function PutFloat LIB "systemio.dll" Alias "__PutFloat@28" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string, indices as Any,  ByVal value as double )  as integer 

'-------------------------------------------------------------------------
' Documentation for: PutLogical
' Put an Boolean(Logical) in a dataset given the variable name, units and a set of indices. Booleans are treated as integers with either a value of 1 or 0.
DECLARE Function PutLogical LIB "systemio.dll" Alias "__PutLogical@24" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string, indices as Any,  ByVal value as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: PutString
' Put an String(Character(*)) in a dataset given the variable name, units and a set of indices.
DECLARE Function PutString LIB "systemio.dll" Alias "__PutString@24" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string, indices as Any,  ByVal value as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ReadInt
' Read an Integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Function ReadInt LIB "systemio.dll" Alias "__ReadInt@16" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ReadInt1
' Read an Integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Function ReadInt1 LIB "systemio.dll" Alias "__ReadInt1@20" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ReadInt2
' Read an Integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Function ReadInt2 LIB "systemio.dll" Alias "__ReadInt2@24" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ReadInt3
' Read an Integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Function ReadInt3 LIB "systemio.dll" Alias "__ReadInt3@28" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ReadInt4
' Read an Integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Function ReadInt4 LIB "systemio.dll" Alias "__ReadInt4@32" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ReadInt5
' Read an Integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Function ReadInt5 LIB "systemio.dll" Alias "__ReadInt5@36" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ReadInt6
' Read an Integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Function ReadInt6 LIB "systemio.dll" Alias "__ReadInt6@40" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ReadReal
' Read a real(float) in a dataset given the variable name, units. The return value is the real(float).
DECLARE Function ReadReal LIB "systemio.dll" Alias "__ReadReal@16" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string )  as double 

'-------------------------------------------------------------------------
' Documentation for: ReadReal1
' Read a real(float) in a dataset given the variable name, units. The return value is the real(float).
DECLARE Function ReadReal1 LIB "systemio.dll" Alias "__ReadReal1@20" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: ReadReal2
' Read a real(float) in a dataset given the variable name, units. The return value is the real(float).
DECLARE Function ReadReal2 LIB "systemio.dll" Alias "__ReadReal2@24" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: ReadReal3
' Read a real(float) in a dataset given the variable name, units. The return value is the real(float).
DECLARE Function ReadReal3 LIB "systemio.dll" Alias "__ReadReal3@28" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: ReadReal4
' Read a real(float) in a dataset given the variable name, units. The return value is the real(float).
DECLARE Function ReadReal4 LIB "systemio.dll" Alias "__ReadReal4@32" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: ReadReal5
' Read a real(float) in a dataset given the variable name, units. The return value is the real(float).
DECLARE Function ReadReal5 LIB "systemio.dll" Alias "__ReadReal5@36" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: ReadReal6
' Read a real(float) in a dataset given the variable name, units. The return value is the real(float).
DECLARE Function ReadReal6 LIB "systemio.dll" Alias "__ReadReal6@40" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: ReadLog
' Read an Logical in a dataset given the variable name, units. The return value is the integer.
DECLARE Function ReadLog LIB "systemio.dll" Alias "__ReadLog@16" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ReadLog1
' Read an Logical in a dataset given the variable name, units. The return value is the integer.
DECLARE Function ReadLog1 LIB "systemio.dll" Alias "__ReadLog1@20" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ReadLog2
' Read an Logical in a dataset given the variable name, units. The return value is the integer.
DECLARE Function ReadLog2 LIB "systemio.dll" Alias "__ReadLog2@24" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ReadLog3
' Read an Logical in a dataset given the variable name, units. The return value is the integer.
DECLARE Function ReadLog3 LIB "systemio.dll" Alias "__ReadLog3@28" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ReadLog4
' Read an Logical in a dataset given the variable name, units. The return value is the integer.
DECLARE Function ReadLog4 LIB "systemio.dll" Alias "__ReadLog4@32" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ReadLog5
' Read an Logical in a dataset given the variable name, units. The return value is the integer.
DECLARE Function ReadLog5 LIB "systemio.dll" Alias "__ReadLog5@36" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ReadLog6
' Read an Logical in a dataset given the variable name, units. The return value is the integer.
DECLARE Function ReadLog6 LIB "systemio.dll" Alias "__ReadLog6@40" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ReadString
' The space for strings (char *) is expected to be managed by the calling program. If dynamic allocation is used the calling program is responsible for deallocating that space. Read a string in a dataset given the variable name, units. The return value is the string.
DECLARE Sub DLLReadString LIB "systemio.dll" Alias "__ReadString@20" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: ReadString1
' Read a string in a dataset given the variable name, units. The return value is the string.
DECLARE Sub DLLReadString1 LIB "systemio.dll" Alias "__ReadString1@24" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: ReadString2
' Read a string in a dataset given the variable name, units. The return value is the string.
DECLARE Sub DLLReadString2 LIB "systemio.dll" Alias "__ReadString2@28" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: ReadString3
' Read a string in a dataset given the variable name, units. The return value is the string.
DECLARE Sub DLLReadString3 LIB "systemio.dll" Alias "__ReadString3@32" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: ReadString4
' Read a string in a dataset given the variable name, units. The return value is the string.
DECLARE Sub DLLReadString4 LIB "systemio.dll" Alias "__ReadString4@36" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: ReadString5
' Read a string in a dataset given the variable name, units. The return value is the string.
DECLARE Sub DLLReadString5 LIB "systemio.dll" Alias "__ReadString5@40" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: ReadString6
' Read a string in a dataset given the variable name, units. The return value is the string.
DECLARE Sub DLLReadString6 LIB "systemio.dll" Alias "__ReadString6@44" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: WriteInt
' Write an Integer in a dataset given the variable name, units.
DECLARE Sub WriteInt LIB "systemio.dll" Alias "__WriteInt@20" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: WriteInt1
' Write an Integer in a dataset given the variable name, units.
DECLARE Sub WriteInt1 LIB "systemio.dll" Alias "__WriteInt1@24" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: WriteInt2
' Write an Integer in a dataset given the variable name, units.
DECLARE Sub WriteInt2 LIB "systemio.dll" Alias "__WriteInt2@28" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: WriteInt3
' Write an Integer in a dataset given the variable name, units.
DECLARE Sub WriteInt3 LIB "systemio.dll" Alias "__WriteInt3@32" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: WriteInt4
' Write an Integer in a dataset given the variable name, units.
DECLARE Sub WriteInt4 LIB "systemio.dll" Alias "__WriteInt4@36" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: WriteInt5
' Write an Integer in a dataset given the variable name, units.
DECLARE Sub WriteInt5 LIB "systemio.dll" Alias "__WriteInt5@40" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: WriteInt6
' Write an Integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Sub WriteInt6 LIB "systemio.dll" Alias "__WriteInt6@44" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: WriteReal
' Write a real(float) to a dataset given the variable name, units.
DECLARE Sub WriteReal LIB "systemio.dll" Alias "__WriteReal@24" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: WriteReal1
' Write a real(float) to a dataset given the variable name, units.
DECLARE Sub WriteReal1 LIB "systemio.dll" Alias "__WriteReal1@28" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: WriteReal2
' Write a real(float) to a dataset given the variable name, units.
DECLARE Sub WriteReal2 LIB "systemio.dll" Alias "__WriteReal2@32" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: WriteReal3
' Write a real(float) to a dataset given the variable name, units.
DECLARE Sub WriteReal3 LIB "systemio.dll" Alias "__WriteReal3@36" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: WriteReal4
' Write a real(float) to a dataset given the variable name, units.
DECLARE Sub WriteReal4 LIB "systemio.dll" Alias "__WriteReal4@40" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: WriteReal5
' Write a real(float) to a dataset given the variable name, units.
DECLARE Sub WriteReal5 LIB "systemio.dll" Alias "__WriteReal5@44" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: WriteReal6
' Write a real(float) to a dataset given the variable name, units.
DECLARE Sub WriteReal6 LIB "systemio.dll" Alias "__WriteReal6@48" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: WriteLog
' Write an Logical in a dataset given the variable name, units.
DECLARE Sub WriteLog LIB "systemio.dll" Alias "__WriteLog@20" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: WriteLog1
' Write an Logical in a dataset given the variable name, units.
DECLARE Sub WriteLog1 LIB "systemio.dll" Alias "__WriteLog1@24" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: WriteLog2
' Write an Logical in a dataset given the variable name, units.
DECLARE Sub WriteLog2 LIB "systemio.dll" Alias "__WriteLog2@28" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: WriteLog3
' Write an Logical in a dataset given the variable name, units.
DECLARE Sub WriteLog3 LIB "systemio.dll" Alias "__WriteLog3@32" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: WriteLog4
' Write an Logical in a dataset given the variable name, units.
DECLARE Sub WriteLog4 LIB "systemio.dll" Alias "__WriteLog4@36" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: WriteLog5
' Write an Logical in a dataset given the variable name, units.
DECLARE Sub WriteLog5 LIB "systemio.dll" Alias "__WriteLog5@40" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: WriteLog6
' Write an Logical in a dataset given the variable name, units.
DECLARE Sub WriteLog6 LIB "systemio.dll" Alias "__WriteLog6@44" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: WriteString
' Write a string to a dataset given the variable name, units.
DECLARE Sub WriteString LIB "systemio.dll" Alias "__WriteString@20" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: WriteString1
' Write a string to a dataset given the variable name, units.
DECLARE Sub WriteString1 LIB "systemio.dll" Alias "__WriteString1@24" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: WriteString2
' Write a string to a dataset given the variable name, units.
DECLARE Sub WriteString2 LIB "systemio.dll" Alias "__WriteString2@28" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: WriteString3
' Write a string to a dataset given the variable name, units.
DECLARE Sub WriteString3 LIB "systemio.dll" Alias "__WriteString3@32" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: WriteString4
' Write a string to a dataset given the variable name, units.
DECLARE Sub WriteString4 LIB "systemio.dll" Alias "__WriteString4@36" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: WriteString5
' Write a string to a dataset given the variable name, units.
DECLARE Sub WriteString5 LIB "systemio.dll" Alias "__WriteString5@40" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: WriteString6
' Write a string to a dataset given the variable name, units.
DECLARE Sub WriteString6 LIB "systemio.dll" Alias "__WriteString6@44" (  ByVal PID as long,  ByVal datasetName as string,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long,  ByVal value as string ) 

Function GetIDicDataSet (  ByVal PID as long,  ByVal iconName as string  ,  ByVal dictionaryName as string  ,  ByVal index as long, inputDataset as string   )  as integer 
      Dim retStr5 as String * MAXFIELD
      GetIDicDataSet = DLLGetIDicDataSet(PID, iconName, dictionaryName, index, retstr5)
      inputDataset=StripTerminator(retStr5)
End  Function 

Function GetODicDataSet (  ByVal PID as long,  ByVal iconName as string  ,  ByVal dictionaryName as string  ,  ByVal index as long, outputDataset as string   )  as integer 
      Dim retStr5 as String * MAXFIELD
      GetODicDataSet = DLLGetODicDataSet(PID, iconName, dictionaryName, index, retstr5)
      outputDataset=StripTerminator(retStr5)
End  Function 

Function GetIModId (  ByVal PID as long,  ByVal iconName as string  ,  ByVal index as long, inputModuleId as string   )  as integer 
      Dim retStr4 as String * MAXFIELD
      GetIModId = DLLGetIModId(PID, iconName, index, retstr4)
      inputModuleId=StripTerminator(retStr4)
End  Function 

Function GetOModId (  ByVal PID as long,  ByVal iconName as string  ,  ByVal index as long, outputModuleId as string   )  as integer 
      Dim retStr4 as String * MAXFIELD
      GetOModId = DLLGetOModId(PID, iconName, index, retstr4)
      outputModuleId=StripTerminator(retStr4)
End  Function 

Function GetIDictionary (  ByVal PID as long,  ByVal iconName as string  ,  ByVal fromIconName as string  ,  ByVal index as long, inputDictionary as string   )  as integer 
      Dim retStr5 as String * MAXFIELD
      GetIDictionary = DLLGetIDictionary(PID, iconName, fromIconName, index, retstr5)
      inputDictionary=StripTerminator(retStr5)
End  Function 

Function GetODictionary (  ByVal PID as long,  ByVal iconName as string  ,  ByVal toIconName as string  ,  ByVal index as long, outputDictionary as string   )  as integer 
      Dim retStr5 as String * MAXFIELD
      GetODictionary = DLLGetODictionary(PID, iconName, toIconName, index, retstr5)
      outputDictionary=StripTerminator(retStr5)
End  Function 

Function GetDicId (  ByVal PID as long,  ByVal datasetName as string  , dictionary as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      GetDicId = DLLGetDicId(PID, datasetName, retstr3)
      dictionary=StripTerminator(retStr3)
End  Function 

Function GetIDataSet (  ByVal PID as long,  ByVal iconName as string  ,  ByVal fromIconName as string  ,  ByVal index as long, inputDataset as string   )  as integer 
      Dim retStr5 as String * MAXFIELD
      GetIDataSet = DLLGetIDataSet(PID, iconName, fromIconName, index, retstr5)
      inputDataset=StripTerminator(retStr5)
End  Function 

Function GetODataSet (  ByVal PID as long,  ByVal iconName as string  ,  ByVal toIconName as string  ,  ByVal index as long, outputDataset as string   )  as integer 
      Dim retStr5 as String * MAXFIELD
      GetODataSet = DLLGetODataSet(PID, iconName, toIconName, index, retstr5)
      outputDataset=StripTerminator(retStr5)
End  Function 

Function GetIModList (  ByVal PID as long,  ByVal iconName as string  ,  ByVal delimiter as string  , list as string   )  as integer 
      Dim retStr4 as String * MAXFIELD
      GetIModList = DLLGetIModList(PID, iconName, delimiter, retstr4)
      list=StripTerminator(retStr4)
End  Function 

Function GetOModList (  ByVal PID as long,  ByVal iconName as string  ,  ByVal delimiter as string  , list as string   )  as integer 
      Dim retStr4 as String * MAXFIELD
      GetOModList = DLLGetOModList(PID, iconName, delimiter, retstr4)
      list=StripTerminator(retStr4)
End  Function 

Function GetIDicList (  ByVal PID as long,  ByVal iconName as string  ,  ByVal fromIconName as string  ,  ByVal delimiter as string  , list as string   )  as integer 
      Dim retStr5 as String * MAXFIELD
      GetIDicList = DLLGetIDicList(PID, iconName, fromIconName, delimiter, retstr5)
      list=StripTerminator(retStr5)
End  Function 

Function GetODicList (  ByVal PID as long,  ByVal iconName as string  ,  ByVal toIconName as string  ,  ByVal delimiter as string  , list as string   )  as integer 
      Dim retStr5 as String * MAXFIELD
      GetODicList = DLLGetODicList(PID, iconName, toIconName, delimiter, retstr5)
      list=StripTerminator(retStr5)
End  Function 

Function GetISetList (  ByVal PID as long,  ByVal iconName as string  ,  ByVal fromIconName as string  ,  ByVal delimiter as string  , list as string   )  as integer 
      Dim retStr5 as String * MAXFIELD
      GetISetList = DLLGetISetList(PID, iconName, fromIconName, delimiter, retstr5)
      list=StripTerminator(retStr5)
End  Function 

Function GetOSetList (  ByVal PID as long,  ByVal iconName as string  ,  ByVal toIconName as string  ,  ByVal delimiter as string  , list as string   )  as integer 
      Dim retStr5 as String * MAXFIELD
      GetOSetList = DLLGetOSetList(PID, iconName, toIconName, delimiter, retstr5)
      list=StripTerminator(retStr5)
End  Function 

Function GetVarDescription (  ByVal PID as long,  ByVal dictionaryName as string  ,  ByVal variableName as string  , description as string   )  as integer 
      Dim retStr4 as String * MAXFIELD
      GetVarDescription = DLLGetVarDescription(PID, dictionaryName, variableName, retstr4)
      description=StripTerminator(retStr4)
End  Function 

Function GetVarType (  ByVal PID as long,  ByVal dictionaryName as string  ,  ByVal variableName as string  , vtype as string   )  as integer 
      Dim retStr4 as String * MAXFIELD
      GetVarType = DLLGetVarType(PID, dictionaryName, variableName, retstr4)
      vtype=StripTerminator(retStr4)
End  Function 

Function GetVarMeasure (  ByVal PID as long,  ByVal dictionaryName as string  ,  ByVal variableName as string  , measure as string   )  as integer 
      Dim retStr4 as String * MAXFIELD
      GetVarMeasure = DLLGetVarMeasure(PID, dictionaryName, variableName, retstr4)
      measure=StripTerminator(retStr4)
End  Function 

Function GetVarUnit (  ByVal PID as long,  ByVal dictionaryName as string  ,  ByVal variableName as string  , unit as string   )  as integer 
      Dim retStr4 as String * MAXFIELD
      GetVarUnit = DLLGetVarUnit(PID, dictionaryName, variableName, retstr4)
      unit=StripTerminator(retStr4)
End  Function 

Function GetVarPreposition (  ByVal PID as long,  ByVal dictionaryName as string  ,  ByVal variableName as string  , preposition as string   )  as integer 
      Dim retStr4 as String * MAXFIELD
      GetVarPreposition = DLLGetVarPreposition(PID, dictionaryName, variableName, retstr4)
      preposition=StripTerminator(retStr4)
End  Function 

Function GetString (  ByVal PID as long,  ByVal datasetName as string  ,  ByVal variableName as string  ,  ByVal unitName as string  , indices as tindex, value as string   )  as integer 
      Dim retStr6 as String * MAXFIELD
      GetString = DLLGetString(PID, datasetName, variableName, unitName, indices, retstr6)
      value=StripTerminator(retStr6)
End  Function 

Sub ReadString (  ByVal PID as long,  ByVal datasetName as string  ,  ByVal variableName as string  ,  ByVal unitName as string  , value as string   ) 
      Dim retStr5 as String * MAXFIELD
      call DLLReadString(PID, datasetName, variableName, unitName, retstr5)
      value=StripTerminator(retStr5)
End  Sub 

Sub ReadString1 (  ByVal PID as long,  ByVal datasetName as string  ,  ByVal variableName as string  ,  ByVal unitName as string  ,  ByVal index1 as long, value as string   ) 
      Dim retStr6 as String * MAXFIELD
      call DLLReadString1(PID, datasetName, variableName, unitName, index1, retstr6)
      value=StripTerminator(retStr6)
End  Sub 

Sub ReadString2 (  ByVal PID as long,  ByVal datasetName as string  ,  ByVal variableName as string  ,  ByVal unitName as string  ,  ByVal index1 as long,  ByVal index2 as long, value as string   ) 
      Dim retStr7 as String * MAXFIELD
      call DLLReadString2(PID, datasetName, variableName, unitName, index1, index2, retstr7)
      value=StripTerminator(retStr7)
End  Sub 

Sub ReadString3 (  ByVal PID as long,  ByVal datasetName as string  ,  ByVal variableName as string  ,  ByVal unitName as string  ,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long, value as string   ) 
      Dim retStr8 as String * MAXFIELD
      call DLLReadString3(PID, datasetName, variableName, unitName, index1, index2, index3, retstr8)
      value=StripTerminator(retStr8)
End  Sub 

Sub ReadString4 (  ByVal PID as long,  ByVal datasetName as string  ,  ByVal variableName as string  ,  ByVal unitName as string  ,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long, value as string   ) 
      Dim retStr9 as String * MAXFIELD
      call DLLReadString4(PID, datasetName, variableName, unitName, index1, index2, index3, index4, retstr9)
      value=StripTerminator(retStr9)
End  Sub 

Sub ReadString5 (  ByVal PID as long,  ByVal datasetName as string  ,  ByVal variableName as string  ,  ByVal unitName as string  ,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long, value as string   ) 
      Dim retStr10 as String * MAXFIELD
      call DLLReadString5(PID, datasetName, variableName, unitName, index1, index2, index3, index4, index5, retstr10)
      value=StripTerminator(retStr10)
End  Sub 

Sub ReadString6 (  ByVal PID as long,  ByVal datasetName as string  ,  ByVal variableName as string  ,  ByVal unitName as string  ,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long, value as string   ) 
      Dim retStr11 as String * MAXFIELD
      call DLLReadString6(PID, datasetName, variableName, unitName, index1, index2, index3, index4, index5, index6, retstr11)
      value=StripTerminator(retStr11)
End  Sub 
