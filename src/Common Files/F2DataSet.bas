Attribute VB_Name = "F2DataSetAPI"
Option Explicit
' API Name: FRAMES DataSet API
'-------------------------------------------------------------------------
' Documentation for: F2OpenIO
' OpenIO returns the modules Process IDentification (PID). If the returned pid is less than zero an error occurred. The path\name arguments are to the project file the PID, path, name and modId are given to the module via the command line. For nearly all functions in the API an integer value is returned. The integer value represents whether the function called succeeded or not. A value of SUCCESS indicates that the function did succeed. In error cases a negative value is returned that defines the error. There are functions available to discovery what the error code means.
DECLARE Function F2OpenIO LIB "systemio.dll" Alias "__F2OpenIO@12" (  ByVal simulationPath as string,  ByVal simulationName as string,  ByVal iconName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2CloseIO
' This functions closes the INI file and cleans all resources associated with SystemIO.dll. If cancel is 0 then the updated datasets are saved otherwise the updated datasets are thrown away.
DECLARE Sub F2CloseIO LIB "systemio.dll" Alias "__F2CloseIO@8" (  ByVal PID as long,  ByVal cancel as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2InputModuleCount
' Get the number of modules this module is consuming.
DECLARE Function F2InputModuleCount LIB "systemio.dll" Alias "__F2InputModuleCount@8" (  ByVal PID as long,  ByVal iconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2OutputModuleCount
' Get the number of modules this module is producing results for.
DECLARE Function F2OutputModuleCount LIB "systemio.dll" Alias "__F2OutputModuleCount@8" (  ByVal PID as long,  ByVal iconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2InputModuleDataSetCount
' Get the number of input datasets from _modId.
DECLARE Function F2InputModuleDataSetCount LIB "systemio.dll" Alias "__F2InputModuleDataSetCount@12" (  ByVal PID as long,  ByVal fromIconHandle as long,  ByVal toIconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2OutputModuleDataSetCount
' Get the number of output datasets from _modId.
DECLARE Function F2OutputModuleDataSetCount LIB "systemio.dll" Alias "__F2OutputModuleDataSetCount@12" (  ByVal PID as long,  ByVal fromIconHandle as long,  ByVal toIconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetDictionaryId
' Gets dictionary for specified dataset.
DECLARE Function DLLF2GetDictionaryId LIB "systemio.dll" Alias "__F2GetDictionaryId@12" (  ByVal PID as long,  ByVal datasetHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetInputDataSet
' Get the input dataset name at dataSetIndex and is returned.
DECLARE Function DLLF2GetInputDataSet LIB "systemio.dll" Alias "__F2GetInputDataSet@20" (  ByVal PID as long,  ByVal fromIconHandle as long,  ByVal toIconHandle as long,  ByVal datasetIndex as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetOutputDataSet
' Get the output dataset name at dataSetIndex and is returned.
DECLARE Function DLLF2GetOutputDataSet LIB "systemio.dll" Alias "__F2GetOutputDataSet@20" (  ByVal PID as long,  ByVal fromIconHandle as long,  ByVal toIconHandle as long,  ByVal datasetIndex as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetVariableDescription
' Get the variable description.
DECLARE Function DLLF2GetVariableDescription LIB "systemio.dll" Alias "__F2GetVariableDescription@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetVariableType
' Get the varible data type. The returned string will be "Integer","String","Real", or "Logical"
DECLARE Function DLLF2GetVariableType LIB "systemio.dll" Alias "__F2GetVariableType@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetVariableScalar
' Get the variables scalar flag. 1=scalar 0=a collection of values (vector)
DECLARE Function F2GetVariableScalar LIB "systemio.dll" Alias "__F2GetVariableScalar@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetVariableMinimum
' Get the minimum value for Integer and Float, length for Strings, zero for Logical.
DECLARE Function F2GetVariableMinimum LIB "systemio.dll" Alias "__F2GetVariableMinimum@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: F2GetVariableMaximum
' Get the maximum value for Integer and Float, length for Strings, zero for Logical.
DECLARE Function F2GetVariableMaximum LIB "systemio.dll" Alias "__F2GetVariableMaximum@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: F2GetVariableMeasure
' Get the variable measurement type.
DECLARE Function DLLF2GetVariableMeasure LIB "systemio.dll" Alias "__F2GetVariableMeasure@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetVariableUnit
' Get the variable unit of measure.
DECLARE Function DLLF2GetVariableUnit LIB "systemio.dll" Alias "__F2GetVariableUnit@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetVariableStochastic
' Get the variables stochastic flag. This flag signifies if the parameter can be modified by an another program and not invalidate any calibration. This allows other programs such and Sensativity/Uncertainty or Parameter estimation programs to modify these parameters.
DECLARE Function F2GetVariableStochastic LIB "systemio.dll" Alias "__F2GetVariableStochastic@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetVariablePreposition
' Get the variables preposition. This is the word that would best be used to write a description of this parameters as an index for another. For example the chemical parameter would use "for" as a preposition to make statements such as Concentration for benzene. The preposition greatly facilitates writing descriptive text associated with results.
DECLARE Function DLLF2GetVariablePreposition LIB "systemio.dll" Alias "__F2GetVariablePreposition@16" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetVariablePrimaryKey
' Get the variables primarykey flag. Is this parameter a key to information on other parameters. This is used in both databases and in the representation of the data. For example a variable being used to index other variables frequently (like chemical) would indicate that chemical should be a primary key. The typical meaning of primary key from Database design is not incorrect when used here.
DECLARE Function F2GetVariablePrimaryKey LIB "systemio.dll" Alias "__F2GetVariablePrimaryKey@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetVariableDimension
' Get the variables scalar flag.
DECLARE Function F2GetVariableDimension LIB "systemio.dll" Alias "__F2GetVariableDimension@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetVariableDimensionCount
' Gets the size of the specified dimensions. The indices parameters define an array of integers that define the index values that are to be used.
DECLARE Function F2GetVariableDimensionCount LIB "systemio.dll" Alias "__F2GetVariableDimensionCount@16" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2VariableLookUp
' Lookup an index into the array. The indices parameters define an array of integers that define the index values that are to be used. The specified string value is searched in the variable for the given index values and the last index value is set. A negative number indicates the element could not be found.
DECLARE Function F2VariableLookUp LIB "systemio.dll" Alias "__F2VariableLookUp@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal value as string, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2ClearVariable
' Deletes all values associated with a variable.
DECLARE Sub F2ClearVariable LIB "systemio.dll" Alias "__F2ClearVariable@16" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long, indices as Any ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetInteger
' Get an Integer from a dataset given the variable name, units and a set of indices.
DECLARE Function F2GetInteger LIB "systemio.dll" Alias "__F2GetInteger@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetFloat
' Get an Float from a dataset given the variable name, units and a set of indices.
DECLARE Function F2GetFloat LIB "systemio.dll" Alias "__F2GetFloat@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any )  as double 

'-------------------------------------------------------------------------
' Documentation for: F2GetLogical
' Get an Boolean from a dataset given the variable name, units and a set of indices. Booleans are treated as integers with either a value of 1 (true) or 0 (false).
DECLARE Function F2GetLogical LIB "systemio.dll" Alias "__F2GetLogical@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetString
' Get an String from a dataset given the variable name, units and a set of indices.
DECLARE Function DLLF2GetString LIB "systemio.dll" Alias "__F2GetString@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2SetInteger
' Put an Integer in a dataset given the variable name, units and a set of indices.
DECLARE Sub F2SetInteger LIB "systemio.dll" Alias "__F2SetInteger@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2SetFloat
' Put an Float in a dataset given the variable name, units and a set of indices.
DECLARE Sub F2SetFloat LIB "systemio.dll" Alias "__F2SetFloat@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: F2SetLogical
' Put an Boolean in a dataset given the variable name, units and a set of indices. Booleans are treated as integers with either a value of 1 (true) or 0 (false).
DECLARE Sub F2SetLogical LIB "systemio.dll" Alias "__F2SetLogical@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2SetString
' Put an String in a dataset given the variable name, units and a set of indices.
DECLARE Sub F2SetString LIB "systemio.dll" Alias "__F2SetString@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, indices as Any,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadInt
' Read an integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Function F2ReadInt LIB "systemio.dll" Alias "__F2ReadInt@16" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2WriteInt
' Write a/an integer in a dataset given the variable name, units.
DECLARE Sub F2WriteInt LIB "systemio.dll" Alias "__F2WriteInt@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadInt1
' Read an integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Function F2ReadInt1 LIB "systemio.dll" Alias "__F2ReadInt1@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2WriteInt1
' Write a/an integer in a dataset given the variable name, units.
DECLARE Sub F2WriteInt1 LIB "systemio.dll" Alias "__F2WriteInt1@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadInt2
' Read an integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Function F2ReadInt2 LIB "systemio.dll" Alias "__F2ReadInt2@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2WriteInt2
' Write a/an integer in a dataset given the variable name, units.
DECLARE Sub F2WriteInt2 LIB "systemio.dll" Alias "__F2WriteInt2@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadInt3
' Read an integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Function F2ReadInt3 LIB "systemio.dll" Alias "__F2ReadInt3@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2WriteInt3
' Write a/an integer in a dataset given the variable name, units.
DECLARE Sub F2WriteInt3 LIB "systemio.dll" Alias "__F2WriteInt3@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadInt4
' Read an integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Function F2ReadInt4 LIB "systemio.dll" Alias "__F2ReadInt4@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2WriteInt4
' Write a/an integer in a dataset given the variable name, units.
DECLARE Sub F2WriteInt4 LIB "systemio.dll" Alias "__F2WriteInt4@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadInt5
' Read an integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Function F2ReadInt5 LIB "systemio.dll" Alias "__F2ReadInt5@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2WriteInt5
' Write a/an integer in a dataset given the variable name, units.
DECLARE Sub F2WriteInt5 LIB "systemio.dll" Alias "__F2WriteInt5@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadInt6
' Read an integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Function F2ReadInt6 LIB "systemio.dll" Alias "__F2ReadInt6@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2WriteInt6
' Write a/an integer in a dataset given the variable name, units.
DECLARE Sub F2WriteInt6 LIB "systemio.dll" Alias "__F2WriteInt6@44" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadReal
' Read an real in a dataset given the variable name, units. The return value is the real.
DECLARE Function F2ReadReal LIB "systemio.dll" Alias "__F2ReadReal@16" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string )  as double 

'-------------------------------------------------------------------------
' Documentation for: F2WriteReal
' Write a/an real in a dataset given the variable name, units.
DECLARE Sub F2WriteReal LIB "systemio.dll" Alias "__F2WriteReal@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadReal1
' Read an real in a dataset given the variable name, units. The return value is the real.
DECLARE Function F2ReadReal1 LIB "systemio.dll" Alias "__F2ReadReal1@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: F2WriteReal1
' Write a/an real in a dataset given the variable name, units.
DECLARE Sub F2WriteReal1 LIB "systemio.dll" Alias "__F2WriteReal1@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadReal2
' Read an real in a dataset given the variable name, units. The return value is the real.
DECLARE Function F2ReadReal2 LIB "systemio.dll" Alias "__F2ReadReal2@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: F2WriteReal2
' Write a/an real in a dataset given the variable name, units.
DECLARE Sub F2WriteReal2 LIB "systemio.dll" Alias "__F2WriteReal2@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadReal3
' Read an real in a dataset given the variable name, units. The return value is the real.
DECLARE Function F2ReadReal3 LIB "systemio.dll" Alias "__F2ReadReal3@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: F2WriteReal3
' Write a/an real in a dataset given the variable name, units.
DECLARE Sub F2WriteReal3 LIB "systemio.dll" Alias "__F2WriteReal3@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadReal4
' Read an real in a dataset given the variable name, units. The return value is the real.
DECLARE Function F2ReadReal4 LIB "systemio.dll" Alias "__F2ReadReal4@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: F2WriteReal4
' Write a/an real in a dataset given the variable name, units.
DECLARE Sub F2WriteReal4 LIB "systemio.dll" Alias "__F2WriteReal4@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadReal5
' Read an real in a dataset given the variable name, units. The return value is the real.
DECLARE Function F2ReadReal5 LIB "systemio.dll" Alias "__F2ReadReal5@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: F2WriteReal5
' Write a/an real in a dataset given the variable name, units.
DECLARE Sub F2WriteReal5 LIB "systemio.dll" Alias "__F2WriteReal5@44" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadReal6
' Read an real in a dataset given the variable name, units. The return value is the real.
DECLARE Function F2ReadReal6 LIB "systemio.dll" Alias "__F2ReadReal6@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: F2WriteReal6
' Write a/an real in a dataset given the variable name, units.
DECLARE Sub F2WriteReal6 LIB "systemio.dll" Alias "__F2WriteReal6@48" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long,  ByVal value as double ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadLog
' Read an integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Function F2ReadLog LIB "systemio.dll" Alias "__F2ReadLog@16" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2WriteLog
' Write a/an integer in a dataset given the variable name, units.
DECLARE Sub F2WriteLog LIB "systemio.dll" Alias "__F2WriteLog@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadLog1
' Read an integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Function F2ReadLog1 LIB "systemio.dll" Alias "__F2ReadLog1@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2WriteLog1
' Write a/an integer in a dataset given the variable name, units.
DECLARE Sub F2WriteLog1 LIB "systemio.dll" Alias "__F2WriteLog1@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadLog2
' Read an integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Function F2ReadLog2 LIB "systemio.dll" Alias "__F2ReadLog2@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2WriteLog2
' Write a/an integer in a dataset given the variable name, units.
DECLARE Sub F2WriteLog2 LIB "systemio.dll" Alias "__F2WriteLog2@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadLog3
' Read an integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Function F2ReadLog3 LIB "systemio.dll" Alias "__F2ReadLog3@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2WriteLog3
' Write a/an integer in a dataset given the variable name, units.
DECLARE Sub F2WriteLog3 LIB "systemio.dll" Alias "__F2WriteLog3@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadLog4
' Read an integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Function F2ReadLog4 LIB "systemio.dll" Alias "__F2ReadLog4@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2WriteLog4
' Write a/an integer in a dataset given the variable name, units.
DECLARE Sub F2WriteLog4 LIB "systemio.dll" Alias "__F2WriteLog4@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadLog5
' Read an integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Function F2ReadLog5 LIB "systemio.dll" Alias "__F2ReadLog5@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2WriteLog5
' Write a/an integer in a dataset given the variable name, units.
DECLARE Sub F2WriteLog5 LIB "systemio.dll" Alias "__F2WriteLog5@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadLog6
' Read an integer in a dataset given the variable name, units. The return value is the integer.
DECLARE Function F2ReadLog6 LIB "systemio.dll" Alias "__F2ReadLog6@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2WriteLog6
' Write a/an integer in a dataset given the variable name, units.
DECLARE Sub F2WriteLog6 LIB "systemio.dll" Alias "__F2WriteLog6@44" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long,  ByVal value as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadString
' Read an string in a dataset given the variable name, units. The return value is the string.
DECLARE Function DLLF2ReadString LIB "systemio.dll" Alias "__F2ReadString@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2WriteString
' Write a/an string in a dataset given the variable name, units.
DECLARE Sub F2WriteString LIB "systemio.dll" Alias "__F2WriteString@20" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadString1
' Read an string in a dataset given the variable name, units. The return value is the string.
DECLARE Function DLLF2ReadString1 LIB "systemio.dll" Alias "__F2ReadString1@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2WriteString1
' Write a/an string in a dataset given the variable name, units.
DECLARE Sub F2WriteString1 LIB "systemio.dll" Alias "__F2WriteString1@24" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadString2
' Read an string in a dataset given the variable name, units. The return value is the string.
DECLARE Function DLLF2ReadString2 LIB "systemio.dll" Alias "__F2ReadString2@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2WriteString2
' Write a/an string in a dataset given the variable name, units.
DECLARE Sub F2WriteString2 LIB "systemio.dll" Alias "__F2WriteString2@28" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadString3
' Read an string in a dataset given the variable name, units. The return value is the string.
DECLARE Function DLLF2ReadString3 LIB "systemio.dll" Alias "__F2ReadString3@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2WriteString3
' Write a/an string in a dataset given the variable name, units.
DECLARE Sub F2WriteString3 LIB "systemio.dll" Alias "__F2WriteString3@32" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadString4
' Read an string in a dataset given the variable name, units. The return value is the string.
DECLARE Function DLLF2ReadString4 LIB "systemio.dll" Alias "__F2ReadString4@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2WriteString4
' Write a/an string in a dataset given the variable name, units.
DECLARE Sub F2WriteString4 LIB "systemio.dll" Alias "__F2WriteString4@36" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadString5
' Read an string in a dataset given the variable name, units. The return value is the string.
DECLARE Function DLLF2ReadString5 LIB "systemio.dll" Alias "__F2ReadString5@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2WriteString5
' Write a/an string in a dataset given the variable name, units.
DECLARE Sub F2WriteString5 LIB "systemio.dll" Alias "__F2WriteString5@40" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2ReadString6
' Read an string in a dataset given the variable name, units. The return value is the string.
DECLARE Function DLLF2ReadString6 LIB "systemio.dll" Alias "__F2ReadString6@44" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2WriteString6
' Write a/an string in a dataset given the variable name, units.
DECLARE Sub F2WriteString6 LIB "systemio.dll" Alias "__F2WriteString6@44" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableHandle as long,  ByVal unitName as string,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long,  ByVal value as string ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetDataSetVariableHandle
' Get the dataset variable handle.
DECLARE Function F2GetDataSetVariableHandle LIB "systemio.dll" Alias "__F2GetDataSetVariableHandle@12" (  ByVal PID as long,  ByVal datasetHandle as long,  ByVal variableName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetDictionaryVariableHandle
' Get the dictionary variable handle.
DECLARE Function F2GetDictionaryVariableHandle LIB "systemio.dll" Alias "__F2GetDictionaryVariableHandle@12" (  ByVal PID as long,  ByVal dictionaryHandle as long,  ByVal variableName as string )  as long 

Function F2GetDictionaryId (  ByVal PID as long,  ByVal datasetHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetDictionaryId(PID, datasetHandle, retStr3)
      F2GetDictionaryId=StripTerminator(retStr3)
End  Function 

Function F2GetInputDataSet (  ByVal PID as long,  ByVal fromIconHandle as long ,  ByVal toIconHandle as long ,  ByVal datasetIndex as long )  as string 
      Dim retStr5 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetInputDataSet(PID, fromIconHandle, toIconHandle, datasetIndex, retStr5)
      F2GetInputDataSet=StripTerminator(retStr5)
End  Function 

Function F2GetOutputDataSet (  ByVal PID as long,  ByVal fromIconHandle as long ,  ByVal toIconHandle as long ,  ByVal datasetIndex as long )  as string 
      Dim retStr5 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetOutputDataSet(PID, fromIconHandle, toIconHandle, datasetIndex, retStr5)
      F2GetOutputDataSet=StripTerminator(retStr5)
End  Function 

Function F2GetVariableDescription (  ByVal PID as long,  ByVal dictionaryHandle as long ,  ByVal variableHandle as long  )  as string 
      Dim retStr4 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetVariableDescription(PID, dictionaryHandle, variableHandle, retStr4)
      F2GetVariableDescription=StripTerminator(retStr4)
End  Function 

Function F2GetVariableType (  ByVal PID as long,  ByVal dictionaryHandle as long ,  ByVal variableHandle as long  )  as string 
      Dim retStr4 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetVariableType(PID, dictionaryHandle, variableHandle, retStr4)
      F2GetVariableType=StripTerminator(retStr4)
End  Function 

Function F2GetVariableMeasure (  ByVal PID as long,  ByVal dictionaryHandle as long ,  ByVal variableHandle as long  )  as string 
      Dim retStr4 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetVariableMeasure(PID, dictionaryHandle, variableHandle, retStr4)
      F2GetVariableMeasure=StripTerminator(retStr4)
End  Function 

Function F2GetVariableUnit (  ByVal PID as long,  ByVal dictionaryHandle as long ,  ByVal variableHandle as long  )  as string 
      Dim retStr4 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetVariableUnit(PID, dictionaryHandle, variableHandle, retStr4)
      F2GetVariableUnit=StripTerminator(retStr4)
End  Function 

Function F2GetVariablePreposition (  ByVal PID as long,  ByVal dictionaryHandle as long ,  ByVal variableHandle as long  )  as string 
      Dim retStr4 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetVariablePreposition(PID, dictionaryHandle, variableHandle, retStr4)
      F2GetVariablePreposition=StripTerminator(retStr4)
End  Function 

Function F2GetString (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableHandle as long ,  ByVal unitName as string  , indices as tindex )  as string 
      Dim retStr6 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetString(PID, datasetHandle, variableHandle, unitName, indices, retStr6)
      F2GetString=StripTerminator(retStr6)
End  Function 

Function F2ReadString (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableHandle as long ,  ByVal unitName as string   )  as string 
      Dim retStr5 as String * MAXFIELD
      Dim value as long
      value = DLLF2ReadString(PID, datasetHandle, variableHandle, unitName, retStr5)
      F2ReadString=StripTerminator(retStr5)
End  Function 

Function F2ReadString1 (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableHandle as long ,  ByVal unitName as string  ,  ByVal index1 as long )  as string 
      Dim retStr6 as String * MAXFIELD
      Dim value as long
      value = DLLF2ReadString1(PID, datasetHandle, variableHandle, unitName, index1, retStr6)
      F2ReadString1=StripTerminator(retStr6)
End  Function 

Function F2ReadString2 (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableHandle as long ,  ByVal unitName as string  ,  ByVal index1 as long,  ByVal index2 as long )  as string 
      Dim retStr7 as String * MAXFIELD
      Dim value as long
      value = DLLF2ReadString2(PID, datasetHandle, variableHandle, unitName, index1, index2, retStr7)
      F2ReadString2=StripTerminator(retStr7)
End  Function 

Function F2ReadString3 (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableHandle as long ,  ByVal unitName as string  ,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long )  as string 
      Dim retStr8 as String * MAXFIELD
      Dim value as long
      value = DLLF2ReadString3(PID, datasetHandle, variableHandle, unitName, index1, index2, index3, retStr8)
      F2ReadString3=StripTerminator(retStr8)
End  Function 

Function F2ReadString4 (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableHandle as long ,  ByVal unitName as string  ,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long )  as string 
      Dim retStr9 as String * MAXFIELD
      Dim value as long
      value = DLLF2ReadString4(PID, datasetHandle, variableHandle, unitName, index1, index2, index3, index4, retStr9)
      F2ReadString4=StripTerminator(retStr9)
End  Function 

Function F2ReadString5 (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableHandle as long ,  ByVal unitName as string  ,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long )  as string 
      Dim retStr10 as String * MAXFIELD
      Dim value as long
      value = DLLF2ReadString5(PID, datasetHandle, variableHandle, unitName, index1, index2, index3, index4, index5, retStr10)
      F2ReadString5=StripTerminator(retStr10)
End  Function 

Function F2ReadString6 (  ByVal PID as long,  ByVal datasetHandle as long ,  ByVal variableHandle as long ,  ByVal unitName as string  ,  ByVal index1 as long,  ByVal index2 as long,  ByVal index3 as long,  ByVal index4 as long,  ByVal index5 as long,  ByVal index6 as long )  as string 
      Dim retStr11 as String * MAXFIELD
      Dim value as long
      value = DLLF2ReadString6(PID, datasetHandle, variableHandle, unitName, index1, index2, index3, index4, index5, index6, retStr11)
      F2ReadString6=StripTerminator(retStr11)
End  Function 
