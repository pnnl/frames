Attribute VB_Name = "F2ModuleDevAPI"
Option Explicit
' API Name: FRAMES Module API
'-------------------------------------------------------------------------
' Documentation for: ModuleDevOk
' Checks to see if the status of the module developer input/output (IO) system. A value of SUCCESS is returned if the system is "Ok".
DECLARE Function ModuleDevOk LIB "systemio.dll" Alias "__ModuleDevOk@4" (  ByVal PID as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ModuleDevOpen
' ModuleDevOpen returns the module link's Process IDentification (PID). The PID is used in all subsequent calls to identify each link into FRAMES. If the returned pid is less than zero an error occurred. The integer value represents whether the function called succeeded or not. A value of SUCCESS indicates that the function did succeed. In error cases a negative value is returned that defines the error. The Status API contains the functions to discover what an error code means.
DECLARE Function ModuleDevOpen LIB "systemio.dll" Alias "__ModuleDevOpen@12" (  ByVal simulationPath as string,  ByVal simulationName as string,  ByVal iconName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ModuleDevClose
' This functions closes the INI file and cleans all resources associated with SystemIO.dll. If the cancel flag is 0 then the updated datasets are saved. Otherwise the updated datasets are thrown away.
DECLARE Sub ModuleDevClose LIB "systemio.dll" Alias "__ModuleDevClose@8" (  ByVal PID as long,  ByVal cancel as long ) 

'-------------------------------------------------------------------------
' Documentation for: IconGetId
' Returns the icon id ("Mod#") of the icon given the icon handle.
DECLARE Function DLLIconGetId LIB "systemio.dll" Alias "__IconGetId@12" (  ByVal PID as long,  ByVal iconHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: IconGetName
' Returns the module name of the icon given the icon handle.
DECLARE Function DLLIconGetName LIB "systemio.dll" Alias "__IconGetName@12" (  ByVal PID as long,  ByVal iconHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: IconGetLabel
' Returns the user supplied label of the icon given the icon handle.
DECLARE Function DLLIconGetLabel LIB "systemio.dll" Alias "__IconGetLabel@12" (  ByVal PID as long,  ByVal iconHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: IconGetNote
' Returns the user suppplied note of the icon given the icon handle.
DECLARE Function DLLIconGetNote LIB "systemio.dll" Alias "__IconGetNote@12" (  ByVal PID as long,  ByVal iconHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: IconGetPosX
' Returns the x screen coordinate of the icon given the icon handle.
DECLARE Function IconGetPosX LIB "systemio.dll" Alias "__IconGetPosX@8" (  ByVal PID as long,  ByVal iconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IconGetPosY
' Returns the y screen coordinate of the icon given the icon handle.
DECLARE Function IconGetPosY LIB "systemio.dll" Alias "__IconGetPosY@8" (  ByVal PID as long,  ByVal iconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IconInputIconsCount
' Returns the number of input icons the icon, given the icon handle, is consuming.
DECLARE Function IconInputIconsCount LIB "systemio.dll" Alias "__IconInputIconsCount@8" (  ByVal PID as long,  ByVal iconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IconOutputIconsCount
' Returns the number of output icons the icon, given the icon handle, is producing.
DECLARE Function IconOutputIconsCount LIB "systemio.dll" Alias "__IconOutputIconsCount@8" (  ByVal PID as long,  ByVal iconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IconGetUIDictionaryByIndex
' Returns the user interface (UI) dictionary name of the icon given the icon handle and index.
DECLARE Function DLLIconGetUIDictionaryByIndex LIB "systemio.dll" Alias "__IconGetUIDictionaryByIndex@16" (  ByVal PID as long,  ByVal iconHandle as long,  ByVal dictionaryIndex as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: IconGetUIDataSetByIndex
' Returns the user interface (UI) dataset name of the icon given the icon handle and index.
DECLARE Function DLLIconGetUIDataSetByIndex LIB "systemio.dll" Alias "__IconGetUIDataSetByIndex@16" (  ByVal PID as long,  ByVal iconHandle as long,  ByVal datasetIndex as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: IconGetUIDictionaryCount
' Returns the number of module input dictionaries for the icon given the icon handle.
DECLARE Function IconGetUIDictionaryCount LIB "systemio.dll" Alias "__IconGetUIDictionaryCount@8" (  ByVal PID as long,  ByVal iconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IconGetScheme
' Returns the scheme name associated with the icon given the icon handle.
DECLARE Function DLLIconGetScheme LIB "systemio.dll" Alias "__IconGetScheme@12" (  ByVal PID as long,  ByVal iconHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: IconsInputDataSetsCount
' Return the number of input datasets between two linked icons.
DECLARE Function IconsInputDataSetsCount LIB "systemio.dll" Alias "__IconsInputDataSetsCount@12" (  ByVal PID as long,  ByVal fromIconHandle as long,  ByVal toIconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IconsOutputDataSetsCount
' Return the number of output datasets between two linked icons.
DECLARE Function IconsOutputDataSetsCount LIB "systemio.dll" Alias "__IconsOutputDataSetsCount@12" (  ByVal PID as long,  ByVal fromIconHandle as long,  ByVal toIconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IconsGetDataSet
' Get the dataset name at the datasetIndex.
DECLARE Function DLLIconsGetDataSet LIB "systemio.dll" Alias "__IconsGetDataSet@20" (  ByVal PID as long,  ByVal fromIconHandle as long,  ByVal toIconHandle as long,  ByVal datasetIndex as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: IconGetUIDataSet
' Returns the handle of the module input dataset given the named icon, named dictionary and index.
DECLARE Function IconGetUIDataSet LIB "systemio.dll" Alias "__IconGetUIDataSet@16" (  ByVal PID as long,  ByVal iconName as string,  ByVal dictionaryName as string,  ByVal datasetIndex as long )  as long 

'-------------------------------------------------------------------------
' Documentation for: IconGetInputDataSet
' Returns the handle of input boundary condition dataset given the named icon, named dictionary and index.
DECLARE Function IconGetInputDataSet LIB "systemio.dll" Alias "__IconGetInputDataSet@16" (  ByVal PID as long,  ByVal iconName as string,  ByVal dictionaryName as string,  ByVal datasetIndex as long )  as long 

'-------------------------------------------------------------------------
' Documentation for: IconGetOutputDataSet
' Returns the handle of output boundary condition dataset dataset given the named icon, named dictionary and index.
DECLARE Function IconGetOutputDataSet LIB "systemio.dll" Alias "__IconGetOutputDataSet@16" (  ByVal PID as long,  ByVal iconName as string,  ByVal dictionaryName as string,  ByVal datasetIndex as long )  as long 

'-------------------------------------------------------------------------
' Documentation for: DataSetGetDictionaryName
' Returns the name of the dataset's dictionary given the dataset handle.
DECLARE Function DLLDataSetGetDictionaryName LIB "systemio.dll" Alias "__DataSetGetDictionaryName@12" (  ByVal PID as long,  ByVal datasetHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: DataSetGetName
' Returns the name of the dataset given the dataset handle.
DECLARE Function DLLDataSetGetName LIB "systemio.dll" Alias "__DataSetGetName@12" (  ByVal PID as long,  ByVal datasetHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: DataSetGetPath
' Returns the fully qualified path of the dataset given the dataset handle. To change the path you need to 1) F2SystemDevSaveDataSetAs changing the path, 2) F2SystemDevCloseDataSet, and 3) F2SystemDevOpenDataSet from it new location.
DECLARE Function DLLDataSetGetPath LIB "systemio.dll" Alias "__DataSetGetPath@12" (  ByVal PID as long,  ByVal datasetHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: DataSetGetDictionary
' Returns the dictionary handle of the dataset given the dataset handle.
DECLARE Function DataSetGetDictionary LIB "systemio.dll" Alias "__DataSetGetDictionary@8" (  ByVal PID as long,  ByVal datasetHandle as long )  as long 

'-------------------------------------------------------------------------
' Documentation for: VariableGetName
' Returns the name of the varaible given the dictionary and variable handles.
DECLARE Function DLLVariableGetName LIB "systemio.dll" Alias "__VariableGetName@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: VariableGetDescription
' Returns the description of the varaible given the dictionary and variable handles.
DECLARE Function DLLVariableGetDescription LIB "systemio.dll" Alias "__VariableGetDescription@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: VariableGetType
' Returns the data type of the varaible given the dictionary and variable handles. The returned string will be one of "Integer", "String", "Float", or "Logical"
DECLARE Function DLLVariableGetType LIB "systemio.dll" Alias "__VariableGetType@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: VariableGetScalar
' Returns the scalar flag of the varaible given the dictionary and variable handles. The returned string will be one of 1 = scalar and 0 = vector (a collection of values)
DECLARE Function VariableGetScalar LIB "systemio.dll" Alias "__VariableGetScalar@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableGetMinimum
' Returns the minimum value of the varaible given the dictionary and variable handles, if the variable is "Float" or "Integer". If the variable is a "String", the length of the string is returned and if the variable is a "Logical," 0 is returned.
DECLARE Function VariableGetMinimum LIB "systemio.dll" Alias "__VariableGetMinimum@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: VariableGetMaximum
' Returns the maximum value of the varaible given the dictionary and variable handles, if the variable is "Float" or "Integer". If the variable is a "String", the length of the string is returned and if the variable is a "Logical," 1 is returned.
DECLARE Function VariableGetMaximum LIB "systemio.dll" Alias "__VariableGetMaximum@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: VariableGetMeasure
' Returns the measure name of the varaible given the dictionary and variable handles.
DECLARE Function DLLVariableGetMeasure LIB "systemio.dll" Alias "__VariableGetMeasure@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: VariableGetUnit
' Returns the unit name of the varaible given the dictionary and variable handles.
DECLARE Function DLLVariableGetUnit LIB "systemio.dll" Alias "__VariableGetUnit@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: VariableGetStochastic
' Returns the stochastic flag of the varaible given the dictionary and variable handles. This flag signifies if the variable can be modified by an another program and not invalidate any calibration. This allows other programs such and Sensativity/Uncertainty or Parameter estimation programs to modify these parameters.
DECLARE Function VariableGetStochastic LIB "systemio.dll" Alias "__VariableGetStochastic@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableGetPreposition
' Returns the preposition word of the varaible given the dictionary and variable handles. This is the word that would best be used to write a description of this parameters as an name for another. For example the chemical parameter would use "for" as a preposition to make statements such as Concentration for benzene. The preposition greatly facilitates writing descriptive text associated with results.
DECLARE Function DLLVariableGetPreposition LIB "systemio.dll" Alias "__VariableGetPreposition@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: VariableGetPrimaryKey
' Returns the primarykey flag of the varaible given the dictionary and variable handles. Is this parameter a key to information on other parameters. This is used in both databases and in the representation of the data. For example a variable being used to name other variables frequently (like chemical) would indicate that chemical should be a primary key, similar to the typical meaning of primary key from database design.
DECLARE Function VariableGetPrimaryKey LIB "systemio.dll" Alias "__VariableGetPrimaryKey@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableHasValues
' Returns non zero if the variable has values given the dictionary and variable handles. Otherwise it returns 0 when no values are present.
DECLARE Function VariableHasValues LIB "systemio.dll" Alias "__VariableHasValues@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableGetDimension
' Returns the total count of indices + 1 if the variable's scalar flag is set to false given the dictionary and variable handles. Otherwise it returns the count of thien indices.
DECLARE Function VariableGetDimension LIB "systemio.dll" Alias "__VariableGetDimension@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableDimensionCount
' Gets the size of the specified dimension. The _indices parameter define the array of integers that specify the postion for the dimension to be counted.
DECLARE Function VariableDimensionCount LIB "systemio.dll" Alias "__VariableDimensionCount@16" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableDimCount
' Gets the size of the specified dimension. The _[number] parameters define the array of integers that specify the postion for the dimension to be counted.
DECLARE Function VariableDimCount LIB "systemio.dll" Alias "__VariableDimCount@8" (  ByVal PID as long,  ByVal variableHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableDimCount1
' Gets the size of the specified dimension. The _[number] parameters define the array of integers that specify the postion for the dimension to be counted.
DECLARE Function VariableDimCount1 LIB "systemio.dll" Alias "__VariableDimCount1@12" (  ByVal PID as long,  ByVal variableHandle as long,  ByVal index1 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableDimCount2
' Gets the size of the specified dimension. The _[number] parameters define the array of integers that specify the postion for the dimension to be counted.
DECLARE Function VariableDimCount2 LIB "systemio.dll" Alias "__VariableDimCount2@16" (  ByVal PID as long,  ByVal variableHandle as long,  ByVal index1 as long,  ByVal index2 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableDimCount3
' Gets the size of the specified dimension. The _[number] parameters define the array of integers that specify the postion for the dimension to be counted.
DECLARE Function VariableDimCount3 LIB "systemio.dll" Alias "__VariableDimCount3@20" (  ByVal PID as long,  ByVal variableHandle as long,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableDimCount4
' Gets the size of the specified dimension. The _[number] parameters define the array of integers that specify the postion for the dimension to be counted.
DECLARE Function VariableDimCount4 LIB "systemio.dll" Alias "__VariableDimCount4@24" (  ByVal PID as long,  ByVal variableHandle as long,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableDimCount5
' Gets the size of the specified dimension. The _[number] parameters define the array of integers that specify the postion for the dimension to be counted.
DECLARE Function VariableDimCount5 LIB "systemio.dll" Alias "__VariableDimCount5@28" (  ByVal PID as long,  ByVal variableHandle as long,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableDimCount6
' Gets the size of the specified dimension. The _[number] parameters define the array of integers that specify the postion for the dimension to be counted.
DECLARE Function VariableDimCount6 LIB "systemio.dll" Alias "__VariableDimCount6@32" (  ByVal PID as long,  ByVal variableHandle as long,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableLookUp
' Returns the index for the specified value in a set of values. The set is determined by which index is given a 0 value in the indice array. A negative number indicates the element could not be found and the error that occured.
DECLARE Function VariableLookUp LIB "systemio.dll" Alias "__VariableLookUp@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal value as string, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableClear
' Deletes all values associated with a variable.
DECLARE Sub VariableClear LIB "systemio.dll" Alias "__VariableClear@16" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long, indices as Any ) 

'-------------------------------------------------------------------------
' Documentation for: VariableGetInteger
' Returns an integer from a dataset given the named variable, units and an array of indices. Deprecated. Use DataSetGetInteger or DataSetReadInt[/1/2/3/4/5/6]
DECLARE Function VariableGetInteger LIB "systemio.dll" Alias "__VariableGetInteger@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableGetFloat
' Returns a float from a dataset given the named variable, units and an array of indices. Deprecated. Use DataSetGetReal or DataSetReadReal[/1/2/3/4/5/6]
DECLARE Function VariableGetFloat LIB "systemio.dll" Alias "__VariableGetFloat@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any )  as double 

'-------------------------------------------------------------------------
' Documentation for: VariableGetLogical
' Returns a logical (boolean> from a dataset given the named variable, units and an array of indices. Booleans are treated as integers with either a value of 1=true or 0=false. Deprecated. Use DataSetGetLogical or DataSetReadLog[/1/2/3/4/5/6]
DECLARE Function VariableGetLogical LIB "systemio.dll" Alias "__VariableGetLogical@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableGetString
' Returns a string (Character(*)) from a dataset given the named variable, units and an array of indices. Deprecated. Use DataSetGetString or DataSetReadString[/1/2/3/4/5/6]
DECLARE Function DLLVariableGetString LIB "systemio.dll" Alias "__VariableGetString@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: VariableSetInteger
' Sets an integer in a dataset given the dataset and variable handles, named unit and an array of indices. Deprecated. Use DataSetSetInteger or DataSetWriteInt[/1/2/3/4/5/6]
DECLARE Sub VariableSetInteger LIB "systemio.dll" Alias "__VariableSetInteger@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: VariableSetFloat
' Sets a floating point in a dataset given the dataset and variable handles, named unit and an array of indices. Deprecated. Use DataSetSetReal or DataSetWriteReal[/1/2/3/4/5/6]
DECLARE Sub VariableSetFloat LIB "systemio.dll" Alias "__VariableSetFloat@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: VariableSetLogical
' Sets a logical in a dataset given the dataset and variable handles, named unit and an array of indices. Booleans are treated as integers with either a value of 1=true or 0=false. Deprecated. Use DataSetSetLogical or DataSetWriteLog[/1/2/3/4/5/6]
DECLARE Sub VariableSetLogical LIB "systemio.dll" Alias "__VariableSetLogical@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: VariableSetString
' Sets a string in a dataset given the dataset and variable handles, named unit and an array of indices. Deprecated. Use DataSetSetString or DataSetWriteString[/1/2/3/4/5/6]
DECLARE Sub VariableSetString LIB "systemio.dll" Alias "__VariableSetString@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetHasValues
' Returns non zero if the dataset has values in any variable given the dataset handle. Otherwise it returns 0 when no values are present.
DECLARE Function DataSetHasValues LIB "systemio.dll" Alias "__DataSetHasValues@8" (  ByVal PID as long,  ByVal datasetHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetClear
' Deletes all values for all variables in the dataset given the dataset handle.
DECLARE Sub DataSetClear LIB "systemio.dll" Alias "__DataSetClear@8" (  ByVal PID as long,  ByVal datasetHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetDimensionCount
' Gets the size of the specified dimension. The _indices parameter define the array of integers that specify the postion for the dimension to be counted.
DECLARE Function DataSetDimensionCount LIB "systemio.dll" Alias "__DataSetDimensionCount@16" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetDimCount
' Gets the size of the specified dimension. The _[number] parameters define the array of integers that specify the postion for the dimension to be counted.
DECLARE Function DataSetDimCount LIB "systemio.dll" Alias "__DataSetDimCount@12" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetDimCount1
' Gets the size of the specified dimension. The _[number] parameters define the array of integers that specify the postion for the dimension to be counted.
DECLARE Function DataSetDimCount1 LIB "systemio.dll" Alias "__DataSetDimCount1@16" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal index1 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetDimCount2
' Gets the size of the specified dimension. The _[number] parameters define the array of integers that specify the postion for the dimension to be counted.
DECLARE Function DataSetDimCount2 LIB "systemio.dll" Alias "__DataSetDimCount2@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal index1 as long,  ByVal index2 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetDimCount3
' Gets the size of the specified dimension. The _[number] parameters define the array of integers that specify the postion for the dimension to be counted.
DECLARE Function DataSetDimCount3 LIB "systemio.dll" Alias "__DataSetDimCount3@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetDimCount4
' Gets the size of the specified dimension. The _[number] parameters define the array of integers that specify the postion for the dimension to be counted.
DECLARE Function DataSetDimCount4 LIB "systemio.dll" Alias "__DataSetDimCount4@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetDimCount5
' Gets the size of the specified dimension. The _[number] parameters define the array of integers that specify the postion for the dimension to be counted.
DECLARE Function DataSetDimCount5 LIB "systemio.dll" Alias "__DataSetDimCount5@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetDimCount6
' Gets the size of the specified dimension. The _[number] parameters define the array of integers that specify the postion for the dimension to be counted.
DECLARE Function DataSetDimCount6 LIB "systemio.dll" Alias "__DataSetDimCount6@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetGetInteger
' Returns an integer from a dataset given the dataset handle, named variable, named unit and an array of indices.
DECLARE Function DataSetGetInteger LIB "systemio.dll" Alias "__DataSetGetInteger@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetGetReal
' Returns a float from a dataset given the dataset handle, named variable, named unit and an array of indices.
DECLARE Function DataSetGetReal LIB "systemio.dll" Alias "__DataSetGetReal@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string, indices as Any )  as double 

'-------------------------------------------------------------------------
' Documentation for: DataSetGetLogical
' Returns a logical from a dataset given the dataset handle, named variable, named unit and an array of indices. Booleans are treated as integers with either a value of 1=true or 0=false.
DECLARE Function DataSetGetLogical LIB "systemio.dll" Alias "__DataSetGetLogical@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetGetString
' Returns a string from a dataset given the dataset handle, named variable, named unit and an array of indices.
DECLARE Function DLLDataSetGetString LIB "systemio.dll" Alias "__DataSetGetString@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string, indices as Any, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: DataSetSetInteger
' Sets an integer in a dataset given the dataset handle, named variable, named unit and an array of indices.
DECLARE Sub DataSetSetInteger LIB "systemio.dll" Alias "__DataSetSetInteger@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string, indices as Any,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetSetReal
' Sets a float in a dataset given the dataset handle, named variable, named unit and an array of indices.
DECLARE Sub DataSetSetReal LIB "systemio.dll" Alias "__DataSetSetReal@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string, indices as Any,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetSetLogical
' Sets a logical in a dataset given the dataset handle, named variable, named unit and an array of indices. Booleans are treated as integers with either a value of 1=true or 0=false.
DECLARE Sub DataSetSetLogical LIB "systemio.dll" Alias "__DataSetSetLogical@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string, indices as Any,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetSetString
' Sets a string in a dataset given dataset handle, named variable, named unit and an array of indices.
DECLARE Sub DataSetSetString LIB "systemio.dll" Alias "__DataSetSetString@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string, indices as Any,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetLookUp
' Returns the index for the specified value in a set of values. The set is determined by which index is given a 0 value in the indice array. A negative number indicates the element could not be found.
DECLARE Function DataSetLookUp LIB "systemio.dll" Alias "__DataSetLookUp@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal value as string, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadInt
' Reads a integer in a dataset given the dataset handle, named variable, named unit and 0 indices and returns it.
DECLARE Function DataSetReadInt LIB "systemio.dll" Alias "__DataSetReadInt@16" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteInt
' Writes a integer in a dataset given the dataset handle, named variable, named unit and 0 indices.
DECLARE Sub DataSetWriteInt LIB "systemio.dll" Alias "__DataSetWriteInt@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadInt1
' Reads a integer in a dataset given the dataset handle, named variable, named unit and 1 indices and returns it.
DECLARE Function DataSetReadInt1 LIB "systemio.dll" Alias "__DataSetReadInt1@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteInt1
' Writes a integer in a dataset given the dataset handle, named variable, named unit and 1 indices.
DECLARE Sub DataSetWriteInt1 LIB "systemio.dll" Alias "__DataSetWriteInt1@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadInt2
' Reads a integer in a dataset given the dataset handle, named variable, named unit and 2 indices and returns it.
DECLARE Function DataSetReadInt2 LIB "systemio.dll" Alias "__DataSetReadInt2@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteInt2
' Writes a integer in a dataset given the dataset handle, named variable, named unit and 2 indices.
DECLARE Sub DataSetWriteInt2 LIB "systemio.dll" Alias "__DataSetWriteInt2@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadInt3
' Reads a integer in a dataset given the dataset handle, named variable, named unit and 3 indices and returns it.
DECLARE Function DataSetReadInt3 LIB "systemio.dll" Alias "__DataSetReadInt3@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteInt3
' Writes a integer in a dataset given the dataset handle, named variable, named unit and 3 indices.
DECLARE Sub DataSetWriteInt3 LIB "systemio.dll" Alias "__DataSetWriteInt3@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadInt4
' Reads a integer in a dataset given the dataset handle, named variable, named unit and 4 indices and returns it.
DECLARE Function DataSetReadInt4 LIB "systemio.dll" Alias "__DataSetReadInt4@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteInt4
' Writes a integer in a dataset given the dataset handle, named variable, named unit and 4 indices.
DECLARE Sub DataSetWriteInt4 LIB "systemio.dll" Alias "__DataSetWriteInt4@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadInt5
' Reads a integer in a dataset given the dataset handle, named variable, named unit and 5 indices and returns it.
DECLARE Function DataSetReadInt5 LIB "systemio.dll" Alias "__DataSetReadInt5@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteInt5
' Writes a integer in a dataset given the dataset handle, named variable, named unit and 5 indices.
DECLARE Sub DataSetWriteInt5 LIB "systemio.dll" Alias "__DataSetWriteInt5@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadInt6
' Reads a integer in a dataset given the dataset handle, named variable, named unit and 6 indices and returns it.
DECLARE Function DataSetReadInt6 LIB "systemio.dll" Alias "__DataSetReadInt6@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteInt6
' Writes a integer in a dataset given the dataset handle, named variable, named unit and 6 indices.
DECLARE Sub DataSetWriteInt6 LIB "systemio.dll" Alias "__DataSetWriteInt6@44" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadReal
' Reads a real in a dataset given the dataset handle, named variable, named unit and 0 indices and returns it.
DECLARE Function DataSetReadReal LIB "systemio.dll" Alias "__DataSetReadReal@16" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string )  as double 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteReal
' Writes a real in a dataset given the dataset handle, named variable, named unit and 0 indices.
DECLARE Sub DataSetWriteReal LIB "systemio.dll" Alias "__DataSetWriteReal@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadReal1
' Reads a real in a dataset given the dataset handle, named variable, named unit and 1 indices and returns it.
DECLARE Function DataSetReadReal1 LIB "systemio.dll" Alias "__DataSetReadReal1@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteReal1
' Writes a real in a dataset given the dataset handle, named variable, named unit and 1 indices.
DECLARE Sub DataSetWriteReal1 LIB "systemio.dll" Alias "__DataSetWriteReal1@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadReal2
' Reads a real in a dataset given the dataset handle, named variable, named unit and 2 indices and returns it.
DECLARE Function DataSetReadReal2 LIB "systemio.dll" Alias "__DataSetReadReal2@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteReal2
' Writes a real in a dataset given the dataset handle, named variable, named unit and 2 indices.
DECLARE Sub DataSetWriteReal2 LIB "systemio.dll" Alias "__DataSetWriteReal2@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadReal3
' Reads a real in a dataset given the dataset handle, named variable, named unit and 3 indices and returns it.
DECLARE Function DataSetReadReal3 LIB "systemio.dll" Alias "__DataSetReadReal3@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteReal3
' Writes a real in a dataset given the dataset handle, named variable, named unit and 3 indices.
DECLARE Sub DataSetWriteReal3 LIB "systemio.dll" Alias "__DataSetWriteReal3@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadReal4
' Reads a real in a dataset given the dataset handle, named variable, named unit and 4 indices and returns it.
DECLARE Function DataSetReadReal4 LIB "systemio.dll" Alias "__DataSetReadReal4@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteReal4
' Writes a real in a dataset given the dataset handle, named variable, named unit and 4 indices.
DECLARE Sub DataSetWriteReal4 LIB "systemio.dll" Alias "__DataSetWriteReal4@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadReal5
' Reads a real in a dataset given the dataset handle, named variable, named unit and 5 indices and returns it.
DECLARE Function DataSetReadReal5 LIB "systemio.dll" Alias "__DataSetReadReal5@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteReal5
' Writes a real in a dataset given the dataset handle, named variable, named unit and 5 indices.
DECLARE Sub DataSetWriteReal5 LIB "systemio.dll" Alias "__DataSetWriteReal5@44" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadReal6
' Reads a real in a dataset given the dataset handle, named variable, named unit and 6 indices and returns it.
DECLARE Function DataSetReadReal6 LIB "systemio.dll" Alias "__DataSetReadReal6@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteReal6
' Writes a real in a dataset given the dataset handle, named variable, named unit and 6 indices.
DECLARE Sub DataSetWriteReal6 LIB "systemio.dll" Alias "__DataSetWriteReal6@48" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadLog
' Reads a integer in a dataset given the dataset handle, named variable, named unit and 0 indices and returns it.
DECLARE Function DataSetReadLog LIB "systemio.dll" Alias "__DataSetReadLog@16" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteLog
' Writes a integer in a dataset given the dataset handle, named variable, named unit and 0 indices.
DECLARE Sub DataSetWriteLog LIB "systemio.dll" Alias "__DataSetWriteLog@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadLog1
' Reads a integer in a dataset given the dataset handle, named variable, named unit and 1 indices and returns it.
DECLARE Function DataSetReadLog1 LIB "systemio.dll" Alias "__DataSetReadLog1@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteLog1
' Writes a integer in a dataset given the dataset handle, named variable, named unit and 1 indices.
DECLARE Sub DataSetWriteLog1 LIB "systemio.dll" Alias "__DataSetWriteLog1@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadLog2
' Reads a integer in a dataset given the dataset handle, named variable, named unit and 2 indices and returns it.
DECLARE Function DataSetReadLog2 LIB "systemio.dll" Alias "__DataSetReadLog2@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteLog2
' Writes a integer in a dataset given the dataset handle, named variable, named unit and 2 indices.
DECLARE Sub DataSetWriteLog2 LIB "systemio.dll" Alias "__DataSetWriteLog2@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadLog3
' Reads a integer in a dataset given the dataset handle, named variable, named unit and 3 indices and returns it.
DECLARE Function DataSetReadLog3 LIB "systemio.dll" Alias "__DataSetReadLog3@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteLog3
' Writes a integer in a dataset given the dataset handle, named variable, named unit and 3 indices.
DECLARE Sub DataSetWriteLog3 LIB "systemio.dll" Alias "__DataSetWriteLog3@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadLog4
' Reads a integer in a dataset given the dataset handle, named variable, named unit and 4 indices and returns it.
DECLARE Function DataSetReadLog4 LIB "systemio.dll" Alias "__DataSetReadLog4@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteLog4
' Writes a integer in a dataset given the dataset handle, named variable, named unit and 4 indices.
DECLARE Sub DataSetWriteLog4 LIB "systemio.dll" Alias "__DataSetWriteLog4@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadLog5
' Reads a integer in a dataset given the dataset handle, named variable, named unit and 5 indices and returns it.
DECLARE Function DataSetReadLog5 LIB "systemio.dll" Alias "__DataSetReadLog5@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteLog5
' Writes a integer in a dataset given the dataset handle, named variable, named unit and 5 indices.
DECLARE Sub DataSetWriteLog5 LIB "systemio.dll" Alias "__DataSetWriteLog5@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadLog6
' Reads a integer in a dataset given the dataset handle, named variable, named unit and 6 indices and returns it.
DECLARE Function DataSetReadLog6 LIB "systemio.dll" Alias "__DataSetReadLog6@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteLog6
' Writes a integer in a dataset given the dataset handle, named variable, named unit and 6 indices.
DECLARE Sub DataSetWriteLog6 LIB "systemio.dll" Alias "__DataSetWriteLog6@44" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadString
' Reads a string in a dataset given the dataset handle, named variable, named unit and 0 indices and returns it.
DECLARE Function DLLDataSetReadString LIB "systemio.dll" Alias "__DataSetReadString@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteString
' Writes a string in a dataset given the dataset handle, named variable, named unit and 0 indices.
DECLARE Sub DataSetWriteString LIB "systemio.dll" Alias "__DataSetWriteString@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadString1
' Reads a string in a dataset given the dataset handle, named variable, named unit and 1 indices and returns it.
DECLARE Function DLLDataSetReadString1 LIB "systemio.dll" Alias "__DataSetReadString1@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteString1
' Writes a string in a dataset given the dataset handle, named variable, named unit and 1 indices.
DECLARE Sub DataSetWriteString1 LIB "systemio.dll" Alias "__DataSetWriteString1@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadString2
' Reads a string in a dataset given the dataset handle, named variable, named unit and 2 indices and returns it.
DECLARE Function DLLDataSetReadString2 LIB "systemio.dll" Alias "__DataSetReadString2@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteString2
' Writes a string in a dataset given the dataset handle, named variable, named unit and 2 indices.
DECLARE Sub DataSetWriteString2 LIB "systemio.dll" Alias "__DataSetWriteString2@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadString3
' Reads a string in a dataset given the dataset handle, named variable, named unit and 3 indices and returns it.
DECLARE Function DLLDataSetReadString3 LIB "systemio.dll" Alias "__DataSetReadString3@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteString3
' Writes a string in a dataset given the dataset handle, named variable, named unit and 3 indices.
DECLARE Sub DataSetWriteString3 LIB "systemio.dll" Alias "__DataSetWriteString3@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadString4
' Reads a string in a dataset given the dataset handle, named variable, named unit and 4 indices and returns it.
DECLARE Function DLLDataSetReadString4 LIB "systemio.dll" Alias "__DataSetReadString4@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteString4
' Writes a string in a dataset given the dataset handle, named variable, named unit and 4 indices.
DECLARE Sub DataSetWriteString4 LIB "systemio.dll" Alias "__DataSetWriteString4@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadString5
' Reads a string in a dataset given the dataset handle, named variable, named unit and 5 indices and returns it.
DECLARE Function DLLDataSetReadString5 LIB "systemio.dll" Alias "__DataSetReadString5@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteString5
' Writes a string in a dataset given the dataset handle, named variable, named unit and 5 indices.
DECLARE Sub DataSetWriteString5 LIB "systemio.dll" Alias "__DataSetWriteString5@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: DataSetReadString6
' Reads a string in a dataset given the dataset handle, named variable, named unit and 6 indices and returns it.
DECLARE Function DLLDataSetReadString6 LIB "systemio.dll" Alias "__DataSetReadString6@44" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: DataSetWriteString6
' Writes a string in a dataset given the dataset handle, named variable, named unit and 6 indices.
DECLARE Sub DataSetWriteString6 LIB "systemio.dll" Alias "__DataSetWriteString6@44" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadInt
' Reads a integer in a dataset given the dataset and variable handles, named unit and 0 indices and returns it.
DECLARE Function VariableReadInt LIB "systemio.dll" Alias "__VariableReadInt@16" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteInt
' Writes a integer in a dataset given the dataset and variable handles, named unit, and 0 indices.
DECLARE Sub VariableWriteInt LIB "systemio.dll" Alias "__VariableWriteInt@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadInt1
' Reads a integer in a dataset given the dataset and variable handles, named unit and 1 indices and returns it.
DECLARE Function VariableReadInt1 LIB "systemio.dll" Alias "__VariableReadInt1@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteInt1
' Writes a integer in a dataset given the dataset and variable handles, named unit, and 1 indices.
DECLARE Sub VariableWriteInt1 LIB "systemio.dll" Alias "__VariableWriteInt1@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadInt2
' Reads a integer in a dataset given the dataset and variable handles, named unit and 2 indices and returns it.
DECLARE Function VariableReadInt2 LIB "systemio.dll" Alias "__VariableReadInt2@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteInt2
' Writes a integer in a dataset given the dataset and variable handles, named unit, and 2 indices.
DECLARE Sub VariableWriteInt2 LIB "systemio.dll" Alias "__VariableWriteInt2@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadInt3
' Reads a integer in a dataset given the dataset and variable handles, named unit and 3 indices and returns it.
DECLARE Function VariableReadInt3 LIB "systemio.dll" Alias "__VariableReadInt3@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteInt3
' Writes a integer in a dataset given the dataset and variable handles, named unit, and 3 indices.
DECLARE Sub VariableWriteInt3 LIB "systemio.dll" Alias "__VariableWriteInt3@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadInt4
' Reads a integer in a dataset given the dataset and variable handles, named unit and 4 indices and returns it.
DECLARE Function VariableReadInt4 LIB "systemio.dll" Alias "__VariableReadInt4@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteInt4
' Writes a integer in a dataset given the dataset and variable handles, named unit, and 4 indices.
DECLARE Sub VariableWriteInt4 LIB "systemio.dll" Alias "__VariableWriteInt4@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadInt5
' Reads a integer in a dataset given the dataset and variable handles, named unit and 5 indices and returns it.
DECLARE Function VariableReadInt5 LIB "systemio.dll" Alias "__VariableReadInt5@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteInt5
' Writes a integer in a dataset given the dataset and variable handles, named unit, and 5 indices.
DECLARE Sub VariableWriteInt5 LIB "systemio.dll" Alias "__VariableWriteInt5@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadInt6
' Reads a integer in a dataset given the dataset and variable handles, named unit and 6 indices and returns it.
DECLARE Function VariableReadInt6 LIB "systemio.dll" Alias "__VariableReadInt6@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteInt6
' Writes a integer in a dataset given the dataset and variable handles, named unit, and 6 indices.
DECLARE Sub VariableWriteInt6 LIB "systemio.dll" Alias "__VariableWriteInt6@44" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadFloat
' Reads a real in a dataset given the dataset and variable handles, named unit and 0 indices and returns it.
DECLARE Function VariableReadFloat LIB "systemio.dll" Alias "__VariableReadFloat@16" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string )  as double 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteFloat
' Writes a real in a dataset given the dataset and variable handles, named unit, and 0 indices.
DECLARE Sub VariableWriteFloat LIB "systemio.dll" Alias "__VariableWriteFloat@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadFloat1
' Reads a real in a dataset given the dataset and variable handles, named unit and 1 indices and returns it.
DECLARE Function VariableReadFloat1 LIB "systemio.dll" Alias "__VariableReadFloat1@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteFloat1
' Writes a real in a dataset given the dataset and variable handles, named unit, and 1 indices.
DECLARE Sub VariableWriteFloat1 LIB "systemio.dll" Alias "__VariableWriteFloat1@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadFloat2
' Reads a real in a dataset given the dataset and variable handles, named unit and 2 indices and returns it.
DECLARE Function VariableReadFloat2 LIB "systemio.dll" Alias "__VariableReadFloat2@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteFloat2
' Writes a real in a dataset given the dataset and variable handles, named unit, and 2 indices.
DECLARE Sub VariableWriteFloat2 LIB "systemio.dll" Alias "__VariableWriteFloat2@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadFloat3
' Reads a real in a dataset given the dataset and variable handles, named unit and 3 indices and returns it.
DECLARE Function VariableReadFloat3 LIB "systemio.dll" Alias "__VariableReadFloat3@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteFloat3
' Writes a real in a dataset given the dataset and variable handles, named unit, and 3 indices.
DECLARE Sub VariableWriteFloat3 LIB "systemio.dll" Alias "__VariableWriteFloat3@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadFloat4
' Reads a real in a dataset given the dataset and variable handles, named unit and 4 indices and returns it.
DECLARE Function VariableReadFloat4 LIB "systemio.dll" Alias "__VariableReadFloat4@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteFloat4
' Writes a real in a dataset given the dataset and variable handles, named unit, and 4 indices.
DECLARE Sub VariableWriteFloat4 LIB "systemio.dll" Alias "__VariableWriteFloat4@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadFloat5
' Reads a real in a dataset given the dataset and variable handles, named unit and 5 indices and returns it.
DECLARE Function VariableReadFloat5 LIB "systemio.dll" Alias "__VariableReadFloat5@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteFloat5
' Writes a real in a dataset given the dataset and variable handles, named unit, and 5 indices.
DECLARE Sub VariableWriteFloat5 LIB "systemio.dll" Alias "__VariableWriteFloat5@44" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadFloat6
' Reads a real in a dataset given the dataset and variable handles, named unit and 6 indices and returns it.
DECLARE Function VariableReadFloat6 LIB "systemio.dll" Alias "__VariableReadFloat6@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteFloat6
' Writes a real in a dataset given the dataset and variable handles, named unit, and 6 indices.
DECLARE Sub VariableWriteFloat6 LIB "systemio.dll" Alias "__VariableWriteFloat6@48" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadLog
' Reads a integer in a dataset given the dataset and variable handles, named unit and 0 indices and returns it.
DECLARE Function VariableReadLog LIB "systemio.dll" Alias "__VariableReadLog@16" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteLog
' Writes a integer in a dataset given the dataset and variable handles, named unit, and 0 indices.
DECLARE Sub VariableWriteLog LIB "systemio.dll" Alias "__VariableWriteLog@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadLog1
' Reads a integer in a dataset given the dataset and variable handles, named unit and 1 indices and returns it.
DECLARE Function VariableReadLog1 LIB "systemio.dll" Alias "__VariableReadLog1@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteLog1
' Writes a integer in a dataset given the dataset and variable handles, named unit, and 1 indices.
DECLARE Sub VariableWriteLog1 LIB "systemio.dll" Alias "__VariableWriteLog1@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadLog2
' Reads a integer in a dataset given the dataset and variable handles, named unit and 2 indices and returns it.
DECLARE Function VariableReadLog2 LIB "systemio.dll" Alias "__VariableReadLog2@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteLog2
' Writes a integer in a dataset given the dataset and variable handles, named unit, and 2 indices.
DECLARE Sub VariableWriteLog2 LIB "systemio.dll" Alias "__VariableWriteLog2@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadLog3
' Reads a integer in a dataset given the dataset and variable handles, named unit and 3 indices and returns it.
DECLARE Function VariableReadLog3 LIB "systemio.dll" Alias "__VariableReadLog3@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteLog3
' Writes a integer in a dataset given the dataset and variable handles, named unit, and 3 indices.
DECLARE Sub VariableWriteLog3 LIB "systemio.dll" Alias "__VariableWriteLog3@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadLog4
' Reads a integer in a dataset given the dataset and variable handles, named unit and 4 indices and returns it.
DECLARE Function VariableReadLog4 LIB "systemio.dll" Alias "__VariableReadLog4@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteLog4
' Writes a integer in a dataset given the dataset and variable handles, named unit, and 4 indices.
DECLARE Sub VariableWriteLog4 LIB "systemio.dll" Alias "__VariableWriteLog4@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadLog5
' Reads a integer in a dataset given the dataset and variable handles, named unit and 5 indices and returns it.
DECLARE Function VariableReadLog5 LIB "systemio.dll" Alias "__VariableReadLog5@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteLog5
' Writes a integer in a dataset given the dataset and variable handles, named unit, and 5 indices.
DECLARE Sub VariableWriteLog5 LIB "systemio.dll" Alias "__VariableWriteLog5@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadLog6
' Reads a integer in a dataset given the dataset and variable handles, named unit and 6 indices and returns it.
DECLARE Function VariableReadLog6 LIB "systemio.dll" Alias "__VariableReadLog6@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteLog6
' Writes a integer in a dataset given the dataset and variable handles, named unit, and 6 indices.
DECLARE Sub VariableWriteLog6 LIB "systemio.dll" Alias "__VariableWriteLog6@44" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadString
' Reads a string in a dataset given the dataset and variable handles, named unit and 0 indices and returns it.
DECLARE Function DLLVariableReadString LIB "systemio.dll" Alias "__VariableReadString@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteString
' Writes a string in a dataset given the dataset and variable handles, named unit, and 0 indices.
DECLARE Sub VariableWriteString LIB "systemio.dll" Alias "__VariableWriteString@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadString1
' Reads a string in a dataset given the dataset and variable handles, named unit and 1 indices and returns it.
DECLARE Function DLLVariableReadString1 LIB "systemio.dll" Alias "__VariableReadString1@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteString1
' Writes a string in a dataset given the dataset and variable handles, named unit, and 1 indices.
DECLARE Sub VariableWriteString1 LIB "systemio.dll" Alias "__VariableWriteString1@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadString2
' Reads a string in a dataset given the dataset and variable handles, named unit and 2 indices and returns it.
DECLARE Function DLLVariableReadString2 LIB "systemio.dll" Alias "__VariableReadString2@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteString2
' Writes a string in a dataset given the dataset and variable handles, named unit, and 2 indices.
DECLARE Sub VariableWriteString2 LIB "systemio.dll" Alias "__VariableWriteString2@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadString3
' Reads a string in a dataset given the dataset and variable handles, named unit and 3 indices and returns it.
DECLARE Function DLLVariableReadString3 LIB "systemio.dll" Alias "__VariableReadString3@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteString3
' Writes a string in a dataset given the dataset and variable handles, named unit, and 3 indices.
DECLARE Sub VariableWriteString3 LIB "systemio.dll" Alias "__VariableWriteString3@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadString4
' Reads a string in a dataset given the dataset and variable handles, named unit and 4 indices and returns it.
DECLARE Function DLLVariableReadString4 LIB "systemio.dll" Alias "__VariableReadString4@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteString4
' Writes a string in a dataset given the dataset and variable handles, named unit, and 4 indices.
DECLARE Sub VariableWriteString4 LIB "systemio.dll" Alias "__VariableWriteString4@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadString5
' Reads a string in a dataset given the dataset and variable handles, named unit and 5 indices and returns it.
DECLARE Function DLLVariableReadString5 LIB "systemio.dll" Alias "__VariableReadString5@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteString5
' Writes a string in a dataset given the dataset and variable handles, named unit, and 5 indices.
DECLARE Sub VariableWriteString5 LIB "systemio.dll" Alias "__VariableWriteString5@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: VariableReadString6
' Reads a string in a dataset given the dataset and variable handles, named unit and 6 indices and returns it.
DECLARE Function DLLVariableReadString6 LIB "systemio.dll" Alias "__VariableReadString6@44" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: VariableWriteString6
' Writes a string in a dataset given the dataset and variable handles, named unit, and 6 indices.
DECLARE Sub VariableWriteString6 LIB "systemio.dll" Alias "__VariableWriteString6@44" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: VariableGetHandleByDataSet
' Returns the variable handle of the named dataset variable if it exists. If the handle is less than 0 an error has been returned.
DECLARE Function VariableGetHandleByDataSet LIB "systemio.dll" Alias "__VariableGetHandleByDataSet@12" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: VariableGetHandleByDictionary
' Returns the variable handle of the named dictionary variable if it exists. If the handle is less than 0 an error has been returned.
DECLARE Function VariableGetHandleByDictionary LIB "systemio.dll" Alias "__VariableGetHandleByDictionary@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: VariableGetHandleByModule
' Returns the variable handle of the named module variable if it exists. If the handle is less than 0 an error has been returned.
DECLARE Function VariableGetHandleByModule LIB "systemio.dll" Alias "__VariableGetHandleByModule@12" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal variableName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: DictionaryGetHandle
' Returns the dictionary handle of the named dictionary if it exists. If the handle is less than 0 an error has been returned.
DECLARE Function DictionaryGetHandle LIB "systemio.dll" Alias "__DictionaryGetHandle@8" (  ByVal PID as long,  ByVal dictionaryName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: DictionaryGetPath
' Returns the path of the dictionary given the dictionary handle. To change the path you need to 1) F2SystemSaveDictionaryAs changing the path, 2) F2SystemCloseDictionary, and 3) SystemOpenDictionary from it new location.
DECLARE Function DLLDictionaryGetPath LIB "systemio.dll" Alias "__DictionaryGetPath@12" (  ByVal PID as long,  ByVal dictionaryHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: DictionaryGetUpdate
' Returns the update flag of the dictionary given the dictionary handle. If the state of the dictionary has been updated or modified, this function will return a non-zero integer. 0 is returned if no updates have occurred.
DECLARE Function DictionaryGetUpdate LIB "systemio.dll" Alias "__DictionaryGetUpdate@8" (  ByVal PID as long,  ByVal dictionaryHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DictionaryGetName
' Returns the name of the dictionary given the dictionary handle.
DECLARE Function DLLDictionaryGetName LIB "systemio.dll" Alias "__DictionaryGetName@12" (  ByVal PID as long,  ByVal dictionaryHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: DictionaryGetDescription
' Returns the description of the dictionary given the dictionary handle.
DECLARE Function DLLDictionaryGetDescription LIB "systemio.dll" Alias "__DictionaryGetDescription@12" (  ByVal PID as long,  ByVal dictionaryHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: DictionaryGetVersion
' Returns the version number of the dictionary given the dictionary handle.
DECLARE Function DictionaryGetVersion LIB "systemio.dll" Alias "__DictionaryGetVersion@8" (  ByVal PID as long,  ByVal dictionaryHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DictionaryGetPrivilege
' Returns the priviledge of the dictionary by the provided dictionary handle. The privilege levels are: 0=Module UI, and 1=Boundary Condition. This represents the level of review a dictionary has received. A system dictionary should have a number of individuals agree that it is complete, useful and mutually exclusive of other system dictionaries.
DECLARE Function DictionaryGetPrivilege LIB "systemio.dll" Alias "__DictionaryGetPrivilege@8" (  ByVal PID as long,  ByVal dictionaryHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ModuleGetHandle
' Returns the handle to the named module if it exists. If the handle is less than 0 an error has been returned.
DECLARE Function ModuleGetHandle LIB "systemio.dll" Alias "__ModuleGetHandle@8" (  ByVal PID as long,  ByVal moduleName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: SchemeGetHandle
' Returns the handle to the named scheme if it exists. If the handle is less than 0 an error has been returned.
DECLARE Function SchemeGetHandle LIB "systemio.dll" Alias "__SchemeGetHandle@12" (  ByVal PID as long,  ByVal moduleHandle as long,  ByVal schemeName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: SimulationGetHandle
' Returns the handle to the named simulation if it exists. If the handle is less than 0 an error has been returned.
DECLARE Function SimulationGetHandle LIB "systemio.dll" Alias "__SimulationGetHandle@8" (  ByVal PID as long,  ByVal simulationName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: DataSetGetHandle
' Returns the handle to the named dataset if it exists. If the handle is less than 0 an error has been returned.
DECLARE Function DataSetGetHandle LIB "systemio.dll" Alias "__DataSetGetHandle@8" (  ByVal PID as long,  ByVal datasetName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: IconGetHandle
' Returns the handle to the named icon if it exists. If the handle is less than 0 an error has been returned.
DECLARE Function IconGetHandle LIB "systemio.dll" Alias "__IconGetHandle@8" (  ByVal PID as long,  ByVal iconName as string )  as long 

Function IconGetId (  ByVal PID as long,  ByVal iconHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLIconGetId(PID, iconHandle, retStr3)
      IconGetId=StripTerminator(retStr3)
End  Function 

Function IconGetName (  ByVal PID as long,  ByVal iconHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLIconGetName(PID, iconHandle, retStr3)
      IconGetName=StripTerminator(retStr3)
End  Function 

Function IconGetLabel (  ByVal PID as long,  ByVal iconHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLIconGetLabel(PID, iconHandle, retStr3)
      IconGetLabel=StripTerminator(retStr3)
End  Function 

Function IconGetNote (  ByVal PID as long,  ByVal iconHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLIconGetNote(PID, iconHandle, retStr3)
      IconGetNote=StripTerminator(retStr3)
End  Function 

Function IconGetUIDictionaryByIndex (  ByVal PID as long,  ByVal iconHandle as long ,  ByVal dictionaryIndex as long )  as string 
      Dim retStr4 as String * MAXFIELD
      Dim value as long
      value = DLLIconGetUIDictionaryByIndex(PID, iconHandle, dictionaryIndex, retStr4)
      IconGetUIDictionaryByIndex=StripTerminator(retStr4)
End  Function 

Function IconGetUIDataSetByIndex (  ByVal PID as long,  ByVal iconHandle as long ,  ByVal datasetIndex as long )  as string 
      Dim retStr4 as String * MAXFIELD
      Dim value as long
      value = DLLIconGetUIDataSetByIndex(PID, iconHandle, datasetIndex, retStr4)
      IconGetUIDataSetByIndex=StripTerminator(retStr4)
End  Function 

Function IconGetScheme (  ByVal PID as long,  ByVal iconHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLIconGetScheme(PID, iconHandle, retStr3)
      IconGetScheme=StripTerminator(retStr3)
End  Function 

Function IconsGetDataSet (  ByVal PID as long,  ByVal fromIconHandle as long ,  ByVal toIconHandle as long ,  ByVal datasetIndex as long )  as string 
      Dim retStr5 as String * MAXFIELD
      Dim value as long
      value = DLLIconsGetDataSet(PID, fromIconHandle, toIconHandle, datasetIndex, retStr5)
      IconsGetDataSet=StripTerminator(retStr5)
End  Function 

Function DataSetGetDictionaryName (  ByVal PID as long,  ByVal datasetHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLDataSetGetDictionaryName(PID, datasetHandle, retStr3)
      DataSetGetDictionaryName=StripTerminator(retStr3)
End  Function 

Function DataSetGetName (  ByVal PID as long,  ByVal datasetHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLDataSetGetName(PID, datasetHandle, retStr3)
      DataSetGetName=StripTerminator(retStr3)
End  Function 

Function DataSetGetPath (  ByVal PID as long,  ByVal datasetHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLDataSetGetPath(PID, datasetHandle, retStr3)
      DataSetGetPath=StripTerminator(retStr3)
End  Function 

Function VariableGetName (  ByVal PID as long,  ByVal dictionaryHandle as long ,  ByVal variableHandle as long  )  as string 
      Dim retStr4 as String * MAXFIELD
      Dim value as long
      value = DLLVariableGetName(PID, dictionaryHandle, variableHandle, retStr4)
      VariableGetName=StripTerminator(retStr4)
End  Function 

Function VariableGetDescription (  ByVal PID as long,  ByVal dictionaryHandle as long ,  ByVal variableHandle as long  )  as string 
      Dim retStr4 as String * MAXFIELD
      Dim value as long
      value = DLLVariableGetDescription(PID, dictionaryHandle, variableHandle, retStr4)
      VariableGetDescription=StripTerminator(retStr4)
End  Function 

Function VariableGetType (  ByVal PID as long,  ByVal dictionaryHandle as long ,  ByVal variableHandle as long  )  as string 
      Dim retStr4 as String * MAXFIELD
      Dim value as long
      value = DLLVariableGetType(PID, dictionaryHandle, variableHandle, retStr4)
      VariableGetType=StripTerminator(retStr4)
End  Function 

Function VariableGetMeasure (  ByVal PID as long,  ByVal dictionaryHandle as long ,  ByVal variableHandle as long  )  as string 
      Dim retStr4 as String * MAXFIELD
      Dim value as long
      value = DLLVariableGetMeasure(PID, dictionaryHandle, variableHandle, retStr4)
      VariableGetMeasure=StripTerminator(retStr4)
End  Function 

Function VariableGetUnit (  ByVal PID as long,  ByVal dictionaryHandle as long ,  ByVal variableHandle as long  )  as string 
      Dim retStr4 as String * MAXFIELD
      Dim value as long
      value = DLLVariableGetUnit(PID, dictionaryHandle, variableHandle, retStr4)
      VariableGetUnit=StripTerminator(retStr4)
End  Function 

Function VariableGetPreposition (  ByVal PID as long,  ByVal dictionaryHandle as long ,  ByVal variableHandle as long  )  as string 
      Dim retStr4 as String * MAXFIELD
      Dim value as long
      value = DLLVariableGetPreposition(PID, dictionaryHandle, variableHandle, retStr4)
      VariableGetPreposition=StripTerminator(retStr4)
End  Function 

Function VariableGetString (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableHandle as long ,  ByVal unitName as string  , indices as tindex )  as string 
      Dim retStr6 as String * MAXFIELD
      Dim value as long
      value = DLLVariableGetString(PID, datasetHandle, variableHandle, unitName, indices, retStr6)
      VariableGetString=StripTerminator(retStr6)
End  Function 

Function DataSetGetString (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableName as string  ,  ByVal unitName as string  , indices as tindex )  as string 
      Dim retStr6 as String * MAXFIELD
      Dim value as long
      value = DLLDataSetGetString(PID, datasetHandle, variableName, unitName, indices, retStr6)
      DataSetGetString=StripTerminator(retStr6)
End  Function 

Function DataSetReadString (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableName as string  ,  ByVal unitName as string   )  as string 
      Dim retStr5 as String * MAXFIELD
      Dim value as long
      value = DLLDataSetReadString(PID, datasetHandle, variableName, unitName, retStr5)
      DataSetReadString=StripTerminator(retStr5)
End  Function 

Function DataSetReadString1 (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableName as string  ,  ByVal unitName as string  ,  ByVal index1 as long )  as string 
      Dim retStr6 as String * MAXFIELD
      Dim value as long
      value = DLLDataSetReadString1(PID, datasetHandle, variableName, unitName, index1, retStr6)
      DataSetReadString1=StripTerminator(retStr6)
End  Function 

Function DataSetReadString2 (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableName as string  ,  ByVal unitName as string  ,  ByVal index1 as long,  ByVal index2 as long )  as string 
      Dim retStr7 as String * MAXFIELD
      Dim value as long
      value = DLLDataSetReadString2(PID, datasetHandle, variableName, unitName, index1, index2, retStr7)
      DataSetReadString2=StripTerminator(retStr7)
End  Function 

Function DataSetReadString3 (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableName as string  ,  ByVal unitName as string  ,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long )  as string 
      Dim retStr8 as String * MAXFIELD
      Dim value as long
      value = DLLDataSetReadString3(PID, datasetHandle, variableName, unitName, index1, index2, index3, retStr8)
      DataSetReadString3=StripTerminator(retStr8)
End  Function 

Function DataSetReadString4 (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableName as string  ,  ByVal unitName as string  ,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long )  as string 
      Dim retStr9 as String * MAXFIELD
      Dim value as long
      value = DLLDataSetReadString4(PID, datasetHandle, variableName, unitName, index1, index2, index3, index4, retStr9)
      DataSetReadString4=StripTerminator(retStr9)
End  Function 

Function DataSetReadString5 (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableName as string  ,  ByVal unitName as string  ,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long )  as string 
      Dim retStr10 as String * MAXFIELD
      Dim value as long
      value = DLLDataSetReadString5(PID, datasetHandle, variableName, unitName, index1, index2, index3, index4, index5, retStr10)
      DataSetReadString5=StripTerminator(retStr10)
End  Function 

Function DataSetReadString6 (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableName as string  ,  ByVal unitName as string  ,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long )  as string 
      Dim retStr11 as String * MAXFIELD
      Dim value as long
      value = DLLDataSetReadString6(PID, datasetHandle, variableName, unitName, index1, index2, index3, index4, index5, index6, retStr11)
      DataSetReadString6=StripTerminator(retStr11)
End  Function 

Function VariableReadString (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableHandle as long ,  ByVal unitName as string   )  as string 
      Dim retStr5 as String * MAXFIELD
      Dim value as long
      value = DLLVariableReadString(PID, datasetHandle, variableHandle, unitName, retStr5)
      VariableReadString=StripTerminator(retStr5)
End  Function 

Function VariableReadString1 (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableHandle as long ,  ByVal unitName as string  ,  ByVal index1 as long )  as string 
      Dim retStr6 as String * MAXFIELD
      Dim value as long
      value = DLLVariableReadString1(PID, datasetHandle, variableHandle, unitName, index1, retStr6)
      VariableReadString1=StripTerminator(retStr6)
End  Function 

Function VariableReadString2 (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableHandle as long ,  ByVal unitName as string  ,  ByVal index1 as long,  ByVal index2 as long )  as string 
      Dim retStr7 as String * MAXFIELD
      Dim value as long
      value = DLLVariableReadString2(PID, datasetHandle, variableHandle, unitName, index1, index2, retStr7)
      VariableReadString2=StripTerminator(retStr7)
End  Function 

Function VariableReadString3 (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableHandle as long ,  ByVal unitName as string  ,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long )  as string 
      Dim retStr8 as String * MAXFIELD
      Dim value as long
      value = DLLVariableReadString3(PID, datasetHandle, variableHandle, unitName, index1, index2, index3, retStr8)
      VariableReadString3=StripTerminator(retStr8)
End  Function 

Function VariableReadString4 (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableHandle as long ,  ByVal unitName as string  ,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long )  as string 
      Dim retStr9 as String * MAXFIELD
      Dim value as long
      value = DLLVariableReadString4(PID, datasetHandle, variableHandle, unitName, index1, index2, index3, index4, retStr9)
      VariableReadString4=StripTerminator(retStr9)
End  Function 

Function VariableReadString5 (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableHandle as long ,  ByVal unitName as string  ,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long )  as string 
      Dim retStr10 as String * MAXFIELD
      Dim value as long
      value = DLLVariableReadString5(PID, datasetHandle, variableHandle, unitName, index1, index2, index3, index4, index5, retStr10)
      VariableReadString5=StripTerminator(retStr10)
End  Function 

Function VariableReadString6 (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableHandle as long ,  ByVal unitName as string  ,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long )  as string 
      Dim retStr11 as String * MAXFIELD
      Dim value as long
      value = DLLVariableReadString6(PID, datasetHandle, variableHandle, unitName, index1, index2, index3, index4, index5, index6, retStr11)
      VariableReadString6=StripTerminator(retStr11)
End  Function 

Function DictionaryGetPath (  ByVal PID as long,  ByVal dictionaryHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLDictionaryGetPath(PID, dictionaryHandle, retStr3)
      DictionaryGetPath=StripTerminator(retStr3)
End  Function 

Function DictionaryGetName (  ByVal PID as long,  ByVal dictionaryHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLDictionaryGetName(PID, dictionaryHandle, retStr3)
      DictionaryGetName=StripTerminator(retStr3)
End  Function 

Function DictionaryGetDescription (  ByVal PID as long,  ByVal dictionaryHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLDictionaryGetDescription(PID, dictionaryHandle, retStr3)
      DictionaryGetDescription=StripTerminator(retStr3)
End  Function 
