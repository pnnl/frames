#ifndef FRAMES2API_F2ModuleDev_H
#define FRAMES2API_F2ModuleDev_H

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

  // API Name: FRAMES Module API
  /* 
       Date:       2001    Programmer: Mitch Pelton, Karl Castleton, and 
       Bonnie Hoopes    Company:    Pacific Northwest National Laboratories  
                     Operated by Battelle Memorial Institute                
       For Department of Energy                    All functions will return 
       an error code unless otherwise stated.    An error code of zero is to 
       be considered a success.        The following terminology is 
       applicable to this documentation         Fully Qualified Path     
       example: "C:\FramesV2\Simulations\GWtest\test.sim"        Full Path   
                    example: "\FramesV2\Simulations\GWtest\test.sim"        
       Path                     example: "\FramesV2\Simulations\Gwtest"      
         File                     example: "test.sim"     Function Naming    
           The API functions have been named to aid in the understanding of 
       what each       function manipulates. Each function name has a prefix 
       that describes what       the function manipulates. The list below 
       describes each prefix used.         Dictionary - A single dictionary  
            Icon - A single icon       Icons - A pair of icons, usually 
       linked together       InputDataSet - A single dataset from an input 
       dictionary       InputDictionary - A single dictionary that an icon 
       consumes       InputModule - A module with a link into an icon       
       ModuleDev - The "module developer level access input/output link to 
       FRAMES       OutputDataSet - A single dataset from an output 
       dictionary       OutputDictionary - A single dictionary that an icon 
       produces       OutputModule - A module with a link out of an icon     
         Variable - A single variable
  */
  /*============================================================================
     Documentation for: ModuleDevOk
    ============================================================================
       Checks to see if the status of the module developer input/output (IO) 
       system. A value of SUCCESS is returned if the system is "Ok".
  */
  int  FRAMES2API _ModuleDevOk(int _PID);
    #ifndef BUILD_DLL
      #define ModuleDevOk _ModuleDevOk
    #endif

  /*============================================================================
     Documentation for: ModuleDevOpen
    ============================================================================
       ModuleDevOpen returns the module link's Process IDentification (PID). 
       The PID is used in all subsequent calls to identify each link into 
       FRAMES. If the returned pid is less than zero an error occurred. The 
       integer value represents whether the function called succeeded or 
       not. A value of SUCCESS indicates that the function did succeed. In 
       error cases a negative value is returned that defines the error. The 
       Status API contains the functions to discover what an error code 
       means.
  */
  int  FRAMES2API _ModuleDevOpen(char* _simulationPath,char* _simulationName,char* _iconName);
    #ifndef BUILD_DLL
      #define ModuleDevOpen _ModuleDevOpen
    #endif

  /*============================================================================
     Documentation for: ModuleDevClose
    ============================================================================
       This functions closes the INI file and cleans all resources 
       associated with SystemIO.dll. If the cancel flag is 0 then the 
       updated datasets are saved. Otherwise the updated datasets are thrown 
       away.
  */
  void  FRAMES2API _ModuleDevClose(int _PID,int _cancel);
    #ifndef BUILD_DLL
      #define ModuleDevClose _ModuleDevClose
    #endif

  /*============================================================================
     Documentation for: IconGetId
    ============================================================================
       Returns the icon id ("Mod#") of the icon given the icon handle.
  */
  int  FRAMES2API _IconGetId(int _PID,long _iconHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define IconGetId _IconGetId
    #endif

  /*============================================================================
     Documentation for: IconGetName
    ============================================================================
       Returns the module name of the icon given the icon handle.
  */
  int  FRAMES2API _IconGetName(int _PID,long _iconHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define IconGetName _IconGetName
    #endif

  /*============================================================================
     Documentation for: IconGetLabel
    ============================================================================
       Returns the user supplied label of the icon given the icon handle.
  */
  int  FRAMES2API _IconGetLabel(int _PID,long _iconHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define IconGetLabel _IconGetLabel
    #endif

  /*============================================================================
     Documentation for: IconGetNote
    ============================================================================
       Returns the user suppplied note of the icon given the icon handle.
  */
  int  FRAMES2API _IconGetNote(int _PID,long _iconHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define IconGetNote _IconGetNote
    #endif

  /*============================================================================
     Documentation for: IconGetPosX
    ============================================================================
       Returns the x screen coordinate of the icon given the icon handle.
  */
  int  FRAMES2API _IconGetPosX(int _PID,long _iconHandle);
    #ifndef BUILD_DLL
      #define IconGetPosX _IconGetPosX
    #endif

  /*============================================================================
     Documentation for: IconGetPosY
    ============================================================================
       Returns the y screen coordinate of the icon given the icon handle.
  */
  int  FRAMES2API _IconGetPosY(int _PID,long _iconHandle);
    #ifndef BUILD_DLL
      #define IconGetPosY _IconGetPosY
    #endif

  /*============================================================================
     Documentation for: IconInputIconsCount
    ============================================================================
       Returns the number of input icons the icon, given the icon handle, is 
       consuming.
  */
  int  FRAMES2API _IconInputIconsCount(int _PID,long _iconHandle);
    #ifndef BUILD_DLL
      #define IconInputIconsCount _IconInputIconsCount
    #endif

  /*============================================================================
     Documentation for: IconOutputIconsCount
    ============================================================================
       Returns the number of output icons the icon, given the icon handle, 
       is producing.
  */
  int  FRAMES2API _IconOutputIconsCount(int _PID,long _iconHandle);
    #ifndef BUILD_DLL
      #define IconOutputIconsCount _IconOutputIconsCount
    #endif

  /*============================================================================
     Documentation for: IconGetUIDictionaryByIndex
    ============================================================================
       Returns the user interface (UI) dictionary name of the icon given the 
       icon handle and index.
  */
  int  FRAMES2API _IconGetUIDictionaryByIndex(int _PID,long _iconHandle,int _dictionaryIndex,char* _retvalue);
    #ifndef BUILD_DLL
      #define IconGetUIDictionaryByIndex _IconGetUIDictionaryByIndex
    #endif

  /*============================================================================
     Documentation for: IconGetUIDataSetByIndex
    ============================================================================
       Returns the user interface (UI) dataset name of the icon given the 
       icon handle and index.
  */
  int  FRAMES2API _IconGetUIDataSetByIndex(int _PID,long _iconHandle,int _datasetIndex,char* _retvalue);
    #ifndef BUILD_DLL
      #define IconGetUIDataSetByIndex _IconGetUIDataSetByIndex
    #endif

  /*============================================================================
     Documentation for: IconGetUIDictionaryCount
    ============================================================================
       Returns the number of module input dictionaries for the icon given 
       the icon handle.
  */
  int  FRAMES2API _IconGetUIDictionaryCount(int _PID,long _iconHandle);
    #ifndef BUILD_DLL
      #define IconGetUIDictionaryCount _IconGetUIDictionaryCount
    #endif

  /*============================================================================
     Documentation for: IconGetScheme
    ============================================================================
       Returns the scheme name associated with the icon given the icon 
       handle.
  */
  int  FRAMES2API _IconGetScheme(int _PID,long _iconHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define IconGetScheme _IconGetScheme
    #endif

  /*============================================================================
     Documentation for: IconsInputDataSetsCount
    ============================================================================
       Return the number of input datasets between two linked icons.
  */
  int  FRAMES2API _IconsInputDataSetsCount(int _PID,long _fromIconHandle,long _toIconHandle);
    #ifndef BUILD_DLL
      #define IconsInputDataSetsCount _IconsInputDataSetsCount
    #endif

  /*============================================================================
     Documentation for: IconsOutputDataSetsCount
    ============================================================================
       Return the number of output datasets between two linked icons.
  */
  int  FRAMES2API _IconsOutputDataSetsCount(int _PID,long _fromIconHandle,long _toIconHandle);
    #ifndef BUILD_DLL
      #define IconsOutputDataSetsCount _IconsOutputDataSetsCount
    #endif

  /*============================================================================
     Documentation for: IconsGetDataSet
    ============================================================================
       Get the dataset name at the datasetIndex.
  */
  int  FRAMES2API _IconsGetDataSet(int _PID,long _fromIconHandle,long _toIconHandle,int _datasetIndex,char* _retvalue);
    #ifndef BUILD_DLL
      #define IconsGetDataSet _IconsGetDataSet
    #endif

  /*============================================================================
     Documentation for: IconGetUIDataSet
    ============================================================================
       Returns the handle of the module input dataset given the named icon, 
       named dictionary and index.
  */
  long  FRAMES2API _IconGetUIDataSet(int _PID,char* _iconName,char* _dictionaryName,int _datasetIndex);
    #ifndef BUILD_DLL
      #define IconGetUIDataSet _IconGetUIDataSet
    #endif

  /*============================================================================
     Documentation for: IconGetInputDataSet
    ============================================================================
       Returns the handle of input boundary condition dataset given the 
       named icon, named dictionary and index.
  */
  long  FRAMES2API _IconGetInputDataSet(int _PID,char* _iconName,char* _dictionaryName,int _datasetIndex);
    #ifndef BUILD_DLL
      #define IconGetInputDataSet _IconGetInputDataSet
    #endif

  /*============================================================================
     Documentation for: IconGetOutputDataSet
    ============================================================================
       Returns the handle of output boundary condition dataset dataset given 
       the named icon, named dictionary and index.
  */
  long  FRAMES2API _IconGetOutputDataSet(int _PID,char* _iconName,char* _dictionaryName,int _datasetIndex);
    #ifndef BUILD_DLL
      #define IconGetOutputDataSet _IconGetOutputDataSet
    #endif

  /*============================================================================
     Documentation for: DataSetGetDictionaryName
    ============================================================================
       Returns the name of the dataset's dictionary given the dataset handle.
  */
  int  FRAMES2API _DataSetGetDictionaryName(int _PID,long _datasetHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define DataSetGetDictionaryName _DataSetGetDictionaryName
    #endif

  /*============================================================================
     Documentation for: DataSetGetName
    ============================================================================
       Returns the name of the dataset given the dataset handle.
  */
  int  FRAMES2API _DataSetGetName(int _PID,long _datasetHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define DataSetGetName _DataSetGetName
    #endif

  /*============================================================================
     Documentation for: DataSetGetPath
    ============================================================================
       Returns the fully qualified path of the dataset given the dataset 
       handle. To change the path you need to 1) F2SystemDevSaveDataSetAs 
       changing the path, 2) F2SystemDevCloseDataSet, and 3) 
       F2SystemDevOpenDataSet from it new location.
  */
  int  FRAMES2API _DataSetGetPath(int _PID,long _datasetHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define DataSetGetPath _DataSetGetPath
    #endif

  /*============================================================================
     Documentation for: DataSetGetDictionary
    ============================================================================
       Returns the dictionary handle of the dataset given the dataset handle.
  */
  long  FRAMES2API _DataSetGetDictionary(int _PID,long _datasetHandle);
    #ifndef BUILD_DLL
      #define DataSetGetDictionary _DataSetGetDictionary
    #endif

  /*============================================================================
     Documentation for: VariableGetName
    ============================================================================
       Returns the name of the varaible given the dictionary and variable 
       handles.
  */
  int  FRAMES2API _VariableGetName(int _PID,long _dictionaryHandle,long _variableHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define VariableGetName _VariableGetName
    #endif

  /*============================================================================
     Documentation for: VariableGetDescription
    ============================================================================
       Returns the description of the varaible given the dictionary and 
       variable handles.
  */
  int  FRAMES2API _VariableGetDescription(int _PID,long _dictionaryHandle,long _variableHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define VariableGetDescription _VariableGetDescription
    #endif

  /*============================================================================
     Documentation for: VariableGetType
    ============================================================================
       Returns the data type of the varaible given the dictionary and 
       variable handles. The returned string will be one of "Integer", 
       "String", "Float", or "Logical"
  */
  int  FRAMES2API _VariableGetType(int _PID,long _dictionaryHandle,long _variableHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define VariableGetType _VariableGetType
    #endif

  /*============================================================================
     Documentation for: VariableGetScalar
    ============================================================================
       Returns the scalar flag of the varaible given the dictionary and 
       variable handles. The returned string will be one of 1 = scalar and 0 
       = vector (a collection of values)
  */
  int  FRAMES2API _VariableGetScalar(int _PID,long _dictionaryHandle,long _variableHandle);
    #ifndef BUILD_DLL
      #define VariableGetScalar _VariableGetScalar
    #endif

  /*============================================================================
     Documentation for: VariableGetMinimum
    ============================================================================
       Returns the minimum value of the varaible given the dictionary and 
       variable handles, if the variable is "Float" or "Integer". If the 
       variable is a "String", the length of the string is returned and if 
       the variable is a "Logical," 0 is returned.
  */
  double  FRAMES2API _VariableGetMinimum(int _PID,long _dictionaryHandle,long _variableHandle);
    #ifndef BUILD_DLL
      #define VariableGetMinimum _VariableGetMinimum
    #endif

  /*============================================================================
     Documentation for: VariableGetMaximum
    ============================================================================
       Returns the maximum value of the varaible given the dictionary and 
       variable handles, if the variable is  "Float" or "Integer". If the 
       variable is a "String", the length of the string is returned and if 
       the variable is a "Logical," 1 is returned.
  */
  double  FRAMES2API _VariableGetMaximum(int _PID,long _dictionaryHandle,long _variableHandle);
    #ifndef BUILD_DLL
      #define VariableGetMaximum _VariableGetMaximum
    #endif

  /*============================================================================
     Documentation for: VariableGetMeasure
    ============================================================================
       Returns the measure name of the varaible given the dictionary and 
       variable handles.
  */
  int  FRAMES2API _VariableGetMeasure(int _PID,long _dictionaryHandle,long _variableHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define VariableGetMeasure _VariableGetMeasure
    #endif

  /*============================================================================
     Documentation for: VariableGetUnit
    ============================================================================
       Returns the unit name of the varaible given the dictionary and 
       variable handles.
  */
  int  FRAMES2API _VariableGetUnit(int _PID,long _dictionaryHandle,long _variableHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define VariableGetUnit _VariableGetUnit
    #endif

  /*============================================================================
     Documentation for: VariableGetStochastic
    ============================================================================
       Returns the stochastic flag of the varaible given the dictionary and 
       variable handles. This flag signifies if the variable can be modified 
       by an another program and not invalidate any calibration. This allows 
       other programs such and Sensativity/Uncertainty or Parameter 
       estimation programs to modify these parameters.
  */
  int  FRAMES2API _VariableGetStochastic(int _PID,long _dictionaryHandle,long _variableHandle);
    #ifndef BUILD_DLL
      #define VariableGetStochastic _VariableGetStochastic
    #endif

  /*============================================================================
     Documentation for: VariableGetPreposition
    ============================================================================
       Returns the preposition word of the varaible given the dictionary and 
       variable handles. This is the word that would best be used to write a 
       description of this parameters as an name for another. For example 
       the chemical parameter would use "for" as a preposition to make 
       statements such as Concentration for benzene. The preposition greatly 
       facilitates writing descriptive text associated with results.
  */
  int  FRAMES2API _VariableGetPreposition(int _PID,long _dictionaryHandle,long _variableHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define VariableGetPreposition _VariableGetPreposition
    #endif

  /*============================================================================
     Documentation for: VariableGetPrimaryKey
    ============================================================================
       Returns the primarykey flag of the varaible given the dictionary and 
       variable handles. Is this parameter a key to information on other 
       parameters. This is used in both databases and in the representation 
       of the data. For example a variable being used to name other 
       variables frequently (like chemical) would indicate that chemical 
       should be a primary key, similar to the typical meaning of primary 
       key from database design.
  */
  int  FRAMES2API _VariableGetPrimaryKey(int _PID,long _dictionaryHandle,long _variableHandle);
    #ifndef BUILD_DLL
      #define VariableGetPrimaryKey _VariableGetPrimaryKey
    #endif

  /*============================================================================
     Documentation for: VariableHasValues
    ============================================================================
       Returns non zero if the variable has values given the dictionary and 
       variable handles. Otherwise it returns 0 when no values are present.
  */
  int  FRAMES2API _VariableHasValues(int _PID,long _dictionaryHandle,long _variableHandle);
    #ifndef BUILD_DLL
      #define VariableHasValues _VariableHasValues
    #endif

  /*============================================================================
     Documentation for: VariableGetDimension
    ============================================================================
       Returns the total count of indices + 1 if the variable's scalar flag 
       is set to false given the dictionary and variable handles. Otherwise 
       it returns the count of thien indices.
  */
  int  FRAMES2API _VariableGetDimension(int _PID,long _dictionaryHandle,long _variableHandle);
    #ifndef BUILD_DLL
      #define VariableGetDimension _VariableGetDimension
    #endif

  /*============================================================================
     Documentation for: VariableDimensionCount
    ============================================================================
       Gets the size of the specified dimension. The _indices parameter 
       define the array of integers that specify the postion for the 
       dimension to be counted.
  */
  int  FRAMES2API _VariableDimensionCount(int _PID,long _datasetHandle,long _variableHandle,int _indices[]);
    #ifndef BUILD_DLL
      #define VariableDimensionCount _VariableDimensionCount
    #endif

  /*============================================================================
     Documentation for: VariableDimCount
    ============================================================================
       Gets the size of the specified dimension. The _[number] parameters 
       define the array of integers that specify the postion for the 
       dimension to be counted.
  */
  int  FRAMES2API _VariableDimCount(int _PID,long _variableHandle);
    #ifndef BUILD_DLL
      #define VariableDimCount _VariableDimCount
    #endif

  /*============================================================================
     Documentation for: VariableDimCount1
    ============================================================================
       Gets the size of the specified dimension. The _[number] parameters 
       define the array of integers that specify the postion for the 
       dimension to be counted.
  */
  int  FRAMES2API _VariableDimCount1(int _PID,long _variableHandle,int _index1);
    #ifndef BUILD_DLL
      #define VariableDimCount1 _VariableDimCount1
    #endif

  /*============================================================================
     Documentation for: VariableDimCount2
    ============================================================================
       Gets the size of the specified dimension. The _[number] parameters 
       define the array of integers that specify the postion for the 
       dimension to be counted.
  */
  int  FRAMES2API _VariableDimCount2(int _PID,long _variableHandle,int _index1,int _index2);
    #ifndef BUILD_DLL
      #define VariableDimCount2 _VariableDimCount2
    #endif

  /*============================================================================
     Documentation for: VariableDimCount3
    ============================================================================
       Gets the size of the specified dimension. The _[number] parameters 
       define the array of integers that specify the postion for the 
       dimension to be counted.
  */
  int  FRAMES2API _VariableDimCount3(int _PID,long _variableHandle,int _index1,int _index2,int _index3);
    #ifndef BUILD_DLL
      #define VariableDimCount3 _VariableDimCount3
    #endif

  /*============================================================================
     Documentation for: VariableDimCount4
    ============================================================================
       Gets the size of the specified dimension. The _[number] parameters 
       define the array of integers that specify the postion for the 
       dimension to be counted.
  */
  int  FRAMES2API _VariableDimCount4(int _PID,long _variableHandle,int _index1,int _index2,int _index3,int _index4);
    #ifndef BUILD_DLL
      #define VariableDimCount4 _VariableDimCount4
    #endif

  /*============================================================================
     Documentation for: VariableDimCount5
    ============================================================================
       Gets the size of the specified dimension. The _[number] parameters 
       define the array of integers that specify the postion for the 
       dimension to be counted.
  */
  int  FRAMES2API _VariableDimCount5(int _PID,long _variableHandle,int _index1,int _index2,int _index3,int _index4,int _index5);
    #ifndef BUILD_DLL
      #define VariableDimCount5 _VariableDimCount5
    #endif

  /*============================================================================
     Documentation for: VariableDimCount6
    ============================================================================
       Gets the size of the specified dimension. The _[number] parameters 
       define the array of integers that specify the postion for the 
       dimension to be counted.
  */
  int  FRAMES2API _VariableDimCount6(int _PID,long _variableHandle,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6);
    #ifndef BUILD_DLL
      #define VariableDimCount6 _VariableDimCount6
    #endif

  /*============================================================================
     Documentation for: VariableLookUp
    ============================================================================
       Returns the index for the specified value in a set of values. The set 
       is determined by which index is given a 0 value in the indice array. 
       A negative number indicates the element could not be found and the 
       error that occured.
  */
  int  FRAMES2API _VariableLookUp(int _PID,long _datasetHandle,long _variableHandle,char* _value,int _indices[]);
    #ifndef BUILD_DLL
      #define VariableLookUp _VariableLookUp
    #endif

  /*============================================================================
     Documentation for: VariableClear
    ============================================================================
       Deletes all values associated with a variable.
  */
  void  FRAMES2API _VariableClear(int _PID,long _datasetHandle,long _variableHandle,int _indices[]);
    #ifndef BUILD_DLL
      #define VariableClear _VariableClear
    #endif

  /*============================================================================
     Documentation for: VariableGetInteger
    ============================================================================
       Returns an integer from a dataset given the named variable, units and 
       an array of indices.     Deprecated. Use DataSetGetInteger or 
       DataSetReadInt[/1/2/3/4/5/6]
  */
  int  FRAMES2API _VariableGetInteger(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _indices[]);
    #ifndef BUILD_DLL
      #define VariableGetInteger _VariableGetInteger
    #endif

  /*============================================================================
     Documentation for: VariableGetFloat
    ============================================================================
       Returns a float from a dataset given the named variable, units and an 
       array of indices.     Deprecated. Use DataSetGetReal or 
       DataSetReadReal[/1/2/3/4/5/6]
  */
  double  FRAMES2API _VariableGetFloat(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _indices[]);
    #ifndef BUILD_DLL
      #define VariableGetFloat _VariableGetFloat
    #endif

  /*============================================================================
     Documentation for: VariableGetLogical
    ============================================================================
       Returns a logical (boolean> from a dataset given the named variable, 
       units and an array of indices. Booleans are treated as integers with 
       either a value of 1=true or 0=false.     Deprecated. Use 
       DataSetGetLogical or DataSetReadLog[/1/2/3/4/5/6]
  */
  int  FRAMES2API _VariableGetLogical(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _indices[]);
    #ifndef BUILD_DLL
      #define VariableGetLogical _VariableGetLogical
    #endif

  /*============================================================================
     Documentation for: VariableGetString
    ============================================================================
       Returns a string (Character(*)) from a dataset given the named 
       variable, units and an array of indices.     Deprecated. Use 
       DataSetGetString or DataSetReadString[/1/2/3/4/5/6]
  */
  int  FRAMES2API _VariableGetString(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _indices[],char* _retvalue);
    #ifndef BUILD_DLL
      #define VariableGetString _VariableGetString
    #endif

  /*============================================================================
     Documentation for: VariableSetInteger
    ============================================================================
       Sets an integer in a dataset given the dataset and variable handles, 
       named unit and an array of indices.     Deprecated. Use 
       DataSetSetInteger or DataSetWriteInt[/1/2/3/4/5/6]
  */
  void  FRAMES2API _VariableSetInteger(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _indices[],int _value);
    #ifndef BUILD_DLL
      #define VariableSetInteger _VariableSetInteger
    #endif

  /*============================================================================
     Documentation for: VariableSetFloat
    ============================================================================
       Sets a floating point in a dataset given the dataset and variable 
       handles, named unit and an array of indices.     Deprecated. Use 
       DataSetSetReal or DataSetWriteReal[/1/2/3/4/5/6]
  */
  void  FRAMES2API _VariableSetFloat(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _indices[],double _value);
    #ifndef BUILD_DLL
      #define VariableSetFloat _VariableSetFloat
    #endif

  /*============================================================================
     Documentation for: VariableSetLogical
    ============================================================================
       Sets a logical in a dataset given the dataset and variable handles, 
       named unit and an array of indices. Booleans are treated as integers 
       with either a value of 1=true or 0=false.     Deprecated. Use 
       DataSetSetLogical or DataSetWriteLog[/1/2/3/4/5/6]
  */
  void  FRAMES2API _VariableSetLogical(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _indices[],int _value);
    #ifndef BUILD_DLL
      #define VariableSetLogical _VariableSetLogical
    #endif

  /*============================================================================
     Documentation for: VariableSetString
    ============================================================================
       Sets a string in a dataset given the dataset and variable handles, 
       named unit and an array of indices.     Deprecated. Use 
       DataSetSetString or DataSetWriteString[/1/2/3/4/5/6]
  */
  void  FRAMES2API _VariableSetString(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _indices[],char* _value);
    #ifndef BUILD_DLL
      #define VariableSetString _VariableSetString
    #endif

  /*============================================================================
     Documentation for: DataSetHasValues
    ============================================================================
       Returns non zero if the dataset has values in any variable given the 
       dataset handle. Otherwise it returns 0 when no values are present.
  */
  int  FRAMES2API _DataSetHasValues(int _PID,long _datasetHandle);
    #ifndef BUILD_DLL
      #define DataSetHasValues _DataSetHasValues
    #endif

  /*============================================================================
     Documentation for: DataSetClear
    ============================================================================
       Deletes all values for all variables in the dataset given the dataset 
       handle.
  */
  void  FRAMES2API _DataSetClear(int _PID,long _datasetHandle);
    #ifndef BUILD_DLL
      #define DataSetClear _DataSetClear
    #endif

  /*============================================================================
     Documentation for: DataSetDimensionCount
    ============================================================================
       Gets the size of the specified dimension. The _indices parameter 
       define the array of integers that specify the postion for the 
       dimension to be counted.
  */
  int  FRAMES2API _DataSetDimensionCount(int _PID,long _datasetHandle,char* _variableName,int _indices[]);
    #ifndef BUILD_DLL
      #define DataSetDimensionCount _DataSetDimensionCount
    #endif

  /*============================================================================
     Documentation for: DataSetDimCount
    ============================================================================
       Gets the size of the specified dimension. The _[number] parameters 
       define the array of integers that specify the postion for the 
       dimension to be counted.
  */
  int  FRAMES2API _DataSetDimCount(int _PID,long _datasetHandle,char* _variableName);
    #ifndef BUILD_DLL
      #define DataSetDimCount _DataSetDimCount
    #endif

  /*============================================================================
     Documentation for: DataSetDimCount1
    ============================================================================
       Gets the size of the specified dimension. The _[number] parameters 
       define the array of integers that specify the postion for the 
       dimension to be counted.
  */
  int  FRAMES2API _DataSetDimCount1(int _PID,long _datasetHandle,char* _variableName,int _index1);
    #ifndef BUILD_DLL
      #define DataSetDimCount1 _DataSetDimCount1
    #endif

  /*============================================================================
     Documentation for: DataSetDimCount2
    ============================================================================
       Gets the size of the specified dimension. The _[number] parameters 
       define the array of integers that specify the postion for the 
       dimension to be counted.
  */
  int  FRAMES2API _DataSetDimCount2(int _PID,long _datasetHandle,char* _variableName,int _index1,int _index2);
    #ifndef BUILD_DLL
      #define DataSetDimCount2 _DataSetDimCount2
    #endif

  /*============================================================================
     Documentation for: DataSetDimCount3
    ============================================================================
       Gets the size of the specified dimension. The _[number] parameters 
       define the array of integers that specify the postion for the 
       dimension to be counted.
  */
  int  FRAMES2API _DataSetDimCount3(int _PID,long _datasetHandle,char* _variableName,int _index1,int _index2,int _index3);
    #ifndef BUILD_DLL
      #define DataSetDimCount3 _DataSetDimCount3
    #endif

  /*============================================================================
     Documentation for: DataSetDimCount4
    ============================================================================
       Gets the size of the specified dimension. The _[number] parameters 
       define the array of integers that specify the postion for the 
       dimension to be counted.
  */
  int  FRAMES2API _DataSetDimCount4(int _PID,long _datasetHandle,char* _variableName,int _index1,int _index2,int _index3,int _index4);
    #ifndef BUILD_DLL
      #define DataSetDimCount4 _DataSetDimCount4
    #endif

  /*============================================================================
     Documentation for: DataSetDimCount5
    ============================================================================
       Gets the size of the specified dimension. The _[number] parameters 
       define the array of integers that specify the postion for the 
       dimension to be counted.
  */
  int  FRAMES2API _DataSetDimCount5(int _PID,long _datasetHandle,char* _variableName,int _index1,int _index2,int _index3,int _index4,int _index5);
    #ifndef BUILD_DLL
      #define DataSetDimCount5 _DataSetDimCount5
    #endif

  /*============================================================================
     Documentation for: DataSetDimCount6
    ============================================================================
       Gets the size of the specified dimension. The _[number] parameters 
       define the array of integers that specify the postion for the 
       dimension to be counted.
  */
  int  FRAMES2API _DataSetDimCount6(int _PID,long _datasetHandle,char* _variableName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6);
    #ifndef BUILD_DLL
      #define DataSetDimCount6 _DataSetDimCount6
    #endif

  /*============================================================================
     Documentation for: DataSetGetInteger
    ============================================================================
       Returns an integer from a dataset given the dataset handle, named 
       variable, named unit and an array of indices.
  */
  int  FRAMES2API _DataSetGetInteger(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _indices[]);
    #ifndef BUILD_DLL
      #define DataSetGetInteger _DataSetGetInteger
    #endif

  /*============================================================================
     Documentation for: DataSetGetReal
    ============================================================================
       Returns a float from a dataset given the dataset handle, named 
       variable, named unit and an array of indices.
  */
  double  FRAMES2API _DataSetGetReal(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _indices[]);
    #ifndef BUILD_DLL
      #define DataSetGetReal _DataSetGetReal
    #endif

  /*============================================================================
     Documentation for: DataSetGetLogical
    ============================================================================
       Returns a logical from a dataset given the dataset handle, named 
       variable, named unit and an array of indices. Booleans are treated as 
       integers with either a value of 1=true or 0=false.
  */
  int  FRAMES2API _DataSetGetLogical(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _indices[]);
    #ifndef BUILD_DLL
      #define DataSetGetLogical _DataSetGetLogical
    #endif

  /*============================================================================
     Documentation for: DataSetGetString
    ============================================================================
       Returns a string from a dataset given the dataset handle, named 
       variable, named unit and an array of indices.
  */
  int  FRAMES2API _DataSetGetString(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _indices[],char* _retvalue);
    #ifndef BUILD_DLL
      #define DataSetGetString _DataSetGetString
    #endif

  /*============================================================================
     Documentation for: DataSetSetInteger
    ============================================================================
       Sets an integer in a dataset given the dataset handle, named 
       variable, named unit and an array of indices.
  */
  void  FRAMES2API _DataSetSetInteger(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _indices[],int _value);
    #ifndef BUILD_DLL
      #define DataSetSetInteger _DataSetSetInteger
    #endif

  /*============================================================================
     Documentation for: DataSetSetReal
    ============================================================================
       Sets a float in a dataset given the dataset handle, named variable, 
       named unit and an array of indices.
  */
  void  FRAMES2API _DataSetSetReal(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _indices[],double _value);
    #ifndef BUILD_DLL
      #define DataSetSetReal _DataSetSetReal
    #endif

  /*============================================================================
     Documentation for: DataSetSetLogical
    ============================================================================
       Sets a logical in a dataset given the dataset handle, named variable, 
       named unit and an array of indices. Booleans are treated as integers 
       with either a value of 1=true or 0=false.
  */
  void  FRAMES2API _DataSetSetLogical(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _indices[],int _value);
    #ifndef BUILD_DLL
      #define DataSetSetLogical _DataSetSetLogical
    #endif

  /*============================================================================
     Documentation for: DataSetSetString
    ============================================================================
       Sets a string in a dataset given dataset handle, named variable, 
       named unit and an array of indices.
  */
  void  FRAMES2API _DataSetSetString(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _indices[],char* _value);
    #ifndef BUILD_DLL
      #define DataSetSetString _DataSetSetString
    #endif

  /*============================================================================
     Documentation for: DataSetLookUp
    ============================================================================
       Returns the index for the specified value in a set of values. The set 
       is determined by which index is given a 0 value in the indice array. 
       A negative number indicates the element could not be found.
  */
  int  FRAMES2API _DataSetLookUp(int _PID,long _datasetHandle,char* _variableName,char* _value,int _indices[]);
    #ifndef BUILD_DLL
      #define DataSetLookUp _DataSetLookUp
    #endif

  /*============================================================================
     Documentation for: DataSetReadInt
    ============================================================================
       Reads a integer in a dataset given the dataset handle, named 
       variable, named unit and 0 indices and returns it.
  */
  int  FRAMES2API _DataSetReadInt(int _PID,long _datasetHandle,char* _variableName,char* _unitName);
    #ifndef BUILD_DLL
      #define DataSetReadInt _DataSetReadInt
    #endif

  /*============================================================================
     Documentation for: DataSetWriteInt
    ============================================================================
       Writes a integer in a dataset given the dataset handle, named 
       variable, named unit and 0 indices.
  */
  void  FRAMES2API _DataSetWriteInt(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _value);
    #ifndef BUILD_DLL
      #define DataSetWriteInt _DataSetWriteInt
    #endif

  /*============================================================================
     Documentation for: DataSetReadInt1
    ============================================================================
       Reads a integer in a dataset given the dataset handle, named 
       variable, named unit and 1 indices and returns it.
  */
  int  FRAMES2API _DataSetReadInt1(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1);
    #ifndef BUILD_DLL
      #define DataSetReadInt1 _DataSetReadInt1
    #endif

  /*============================================================================
     Documentation for: DataSetWriteInt1
    ============================================================================
       Writes a integer in a dataset given the dataset handle, named 
       variable, named unit and 1 indices.
  */
  void  FRAMES2API _DataSetWriteInt1(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _value);
    #ifndef BUILD_DLL
      #define DataSetWriteInt1 _DataSetWriteInt1
    #endif

  /*============================================================================
     Documentation for: DataSetReadInt2
    ============================================================================
       Reads a integer in a dataset given the dataset handle, named 
       variable, named unit and 2 indices and returns it.
  */
  int  FRAMES2API _DataSetReadInt2(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2);
    #ifndef BUILD_DLL
      #define DataSetReadInt2 _DataSetReadInt2
    #endif

  /*============================================================================
     Documentation for: DataSetWriteInt2
    ============================================================================
       Writes a integer in a dataset given the dataset handle, named 
       variable, named unit and 2 indices.
  */
  void  FRAMES2API _DataSetWriteInt2(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _value);
    #ifndef BUILD_DLL
      #define DataSetWriteInt2 _DataSetWriteInt2
    #endif

  /*============================================================================
     Documentation for: DataSetReadInt3
    ============================================================================
       Reads a integer in a dataset given the dataset handle, named 
       variable, named unit and 3 indices and returns it.
  */
  int  FRAMES2API _DataSetReadInt3(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3);
    #ifndef BUILD_DLL
      #define DataSetReadInt3 _DataSetReadInt3
    #endif

  /*============================================================================
     Documentation for: DataSetWriteInt3
    ============================================================================
       Writes a integer in a dataset given the dataset handle, named 
       variable, named unit and 3 indices.
  */
  void  FRAMES2API _DataSetWriteInt3(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _value);
    #ifndef BUILD_DLL
      #define DataSetWriteInt3 _DataSetWriteInt3
    #endif

  /*============================================================================
     Documentation for: DataSetReadInt4
    ============================================================================
       Reads a integer in a dataset given the dataset handle, named 
       variable, named unit and 4 indices and returns it.
  */
  int  FRAMES2API _DataSetReadInt4(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4);
    #ifndef BUILD_DLL
      #define DataSetReadInt4 _DataSetReadInt4
    #endif

  /*============================================================================
     Documentation for: DataSetWriteInt4
    ============================================================================
       Writes a integer in a dataset given the dataset handle, named 
       variable, named unit and 4 indices.
  */
  void  FRAMES2API _DataSetWriteInt4(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _value);
    #ifndef BUILD_DLL
      #define DataSetWriteInt4 _DataSetWriteInt4
    #endif

  /*============================================================================
     Documentation for: DataSetReadInt5
    ============================================================================
       Reads a integer in a dataset given the dataset handle, named 
       variable, named unit and 5 indices and returns it.
  */
  int  FRAMES2API _DataSetReadInt5(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5);
    #ifndef BUILD_DLL
      #define DataSetReadInt5 _DataSetReadInt5
    #endif

  /*============================================================================
     Documentation for: DataSetWriteInt5
    ============================================================================
       Writes a integer in a dataset given the dataset handle, named 
       variable, named unit and 5 indices.
  */
  void  FRAMES2API _DataSetWriteInt5(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _value);
    #ifndef BUILD_DLL
      #define DataSetWriteInt5 _DataSetWriteInt5
    #endif

  /*============================================================================
     Documentation for: DataSetReadInt6
    ============================================================================
       Reads a integer in a dataset given the dataset handle, named 
       variable, named unit and 6 indices and returns it.
  */
  int  FRAMES2API _DataSetReadInt6(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6);
    #ifndef BUILD_DLL
      #define DataSetReadInt6 _DataSetReadInt6
    #endif

  /*============================================================================
     Documentation for: DataSetWriteInt6
    ============================================================================
       Writes a integer in a dataset given the dataset handle, named 
       variable, named unit and 6 indices.
  */
  void  FRAMES2API _DataSetWriteInt6(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6,int _value);
    #ifndef BUILD_DLL
      #define DataSetWriteInt6 _DataSetWriteInt6
    #endif

  /*============================================================================
     Documentation for: DataSetReadReal
    ============================================================================
       Reads a real in a dataset given the dataset handle, named variable, 
       named unit and 0 indices and returns it.
  */
  double  FRAMES2API _DataSetReadReal(int _PID,long _datasetHandle,char* _variableName,char* _unitName);
    #ifndef BUILD_DLL
      #define DataSetReadReal _DataSetReadReal
    #endif

  /*============================================================================
     Documentation for: DataSetWriteReal
    ============================================================================
       Writes a real in a dataset given the dataset handle, named variable, 
       named unit and 0 indices.
  */
  void  FRAMES2API _DataSetWriteReal(int _PID,long _datasetHandle,char* _variableName,char* _unitName,double _value);
    #ifndef BUILD_DLL
      #define DataSetWriteReal _DataSetWriteReal
    #endif

  /*============================================================================
     Documentation for: DataSetReadReal1
    ============================================================================
       Reads a real in a dataset given the dataset handle, named variable, 
       named unit and 1 indices and returns it.
  */
  double  FRAMES2API _DataSetReadReal1(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1);
    #ifndef BUILD_DLL
      #define DataSetReadReal1 _DataSetReadReal1
    #endif

  /*============================================================================
     Documentation for: DataSetWriteReal1
    ============================================================================
       Writes a real in a dataset given the dataset handle, named variable, 
       named unit and 1 indices.
  */
  void  FRAMES2API _DataSetWriteReal1(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,double _value);
    #ifndef BUILD_DLL
      #define DataSetWriteReal1 _DataSetWriteReal1
    #endif

  /*============================================================================
     Documentation for: DataSetReadReal2
    ============================================================================
       Reads a real in a dataset given the dataset handle, named variable, 
       named unit and 2 indices and returns it.
  */
  double  FRAMES2API _DataSetReadReal2(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2);
    #ifndef BUILD_DLL
      #define DataSetReadReal2 _DataSetReadReal2
    #endif

  /*============================================================================
     Documentation for: DataSetWriteReal2
    ============================================================================
       Writes a real in a dataset given the dataset handle, named variable, 
       named unit and 2 indices.
  */
  void  FRAMES2API _DataSetWriteReal2(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,double _value);
    #ifndef BUILD_DLL
      #define DataSetWriteReal2 _DataSetWriteReal2
    #endif

  /*============================================================================
     Documentation for: DataSetReadReal3
    ============================================================================
       Reads a real in a dataset given the dataset handle, named variable, 
       named unit and 3 indices and returns it.
  */
  double  FRAMES2API _DataSetReadReal3(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3);
    #ifndef BUILD_DLL
      #define DataSetReadReal3 _DataSetReadReal3
    #endif

  /*============================================================================
     Documentation for: DataSetWriteReal3
    ============================================================================
       Writes a real in a dataset given the dataset handle, named variable, 
       named unit and 3 indices.
  */
  void  FRAMES2API _DataSetWriteReal3(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,double _value);
    #ifndef BUILD_DLL
      #define DataSetWriteReal3 _DataSetWriteReal3
    #endif

  /*============================================================================
     Documentation for: DataSetReadReal4
    ============================================================================
       Reads a real in a dataset given the dataset handle, named variable, 
       named unit and 4 indices and returns it.
  */
  double  FRAMES2API _DataSetReadReal4(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4);
    #ifndef BUILD_DLL
      #define DataSetReadReal4 _DataSetReadReal4
    #endif

  /*============================================================================
     Documentation for: DataSetWriteReal4
    ============================================================================
       Writes a real in a dataset given the dataset handle, named variable, 
       named unit and 4 indices.
  */
  void  FRAMES2API _DataSetWriteReal4(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4,double _value);
    #ifndef BUILD_DLL
      #define DataSetWriteReal4 _DataSetWriteReal4
    #endif

  /*============================================================================
     Documentation for: DataSetReadReal5
    ============================================================================
       Reads a real in a dataset given the dataset handle, named variable, 
       named unit and 5 indices and returns it.
  */
  double  FRAMES2API _DataSetReadReal5(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5);
    #ifndef BUILD_DLL
      #define DataSetReadReal5 _DataSetReadReal5
    #endif

  /*============================================================================
     Documentation for: DataSetWriteReal5
    ============================================================================
       Writes a real in a dataset given the dataset handle, named variable, 
       named unit and 5 indices.
  */
  void  FRAMES2API _DataSetWriteReal5(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,double _value);
    #ifndef BUILD_DLL
      #define DataSetWriteReal5 _DataSetWriteReal5
    #endif

  /*============================================================================
     Documentation for: DataSetReadReal6
    ============================================================================
       Reads a real in a dataset given the dataset handle, named variable, 
       named unit and 6 indices and returns it.
  */
  double  FRAMES2API _DataSetReadReal6(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6);
    #ifndef BUILD_DLL
      #define DataSetReadReal6 _DataSetReadReal6
    #endif

  /*============================================================================
     Documentation for: DataSetWriteReal6
    ============================================================================
       Writes a real in a dataset given the dataset handle, named variable, 
       named unit and 6 indices.
  */
  void  FRAMES2API _DataSetWriteReal6(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6,double _value);
    #ifndef BUILD_DLL
      #define DataSetWriteReal6 _DataSetWriteReal6
    #endif

  /*============================================================================
     Documentation for: DataSetReadLog
    ============================================================================
       Reads a integer in a dataset given the dataset handle, named 
       variable, named unit and 0 indices and returns it.
  */
  int  FRAMES2API _DataSetReadLog(int _PID,long _datasetHandle,char* _variableName,char* _unitName);
    #ifndef BUILD_DLL
      #define DataSetReadLog _DataSetReadLog
    #endif

  /*============================================================================
     Documentation for: DataSetWriteLog
    ============================================================================
       Writes a integer in a dataset given the dataset handle, named 
       variable, named unit and 0 indices.
  */
  void  FRAMES2API _DataSetWriteLog(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _value);
    #ifndef BUILD_DLL
      #define DataSetWriteLog _DataSetWriteLog
    #endif

  /*============================================================================
     Documentation for: DataSetReadLog1
    ============================================================================
       Reads a integer in a dataset given the dataset handle, named 
       variable, named unit and 1 indices and returns it.
  */
  int  FRAMES2API _DataSetReadLog1(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1);
    #ifndef BUILD_DLL
      #define DataSetReadLog1 _DataSetReadLog1
    #endif

  /*============================================================================
     Documentation for: DataSetWriteLog1
    ============================================================================
       Writes a integer in a dataset given the dataset handle, named 
       variable, named unit and 1 indices.
  */
  void  FRAMES2API _DataSetWriteLog1(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _value);
    #ifndef BUILD_DLL
      #define DataSetWriteLog1 _DataSetWriteLog1
    #endif

  /*============================================================================
     Documentation for: DataSetReadLog2
    ============================================================================
       Reads a integer in a dataset given the dataset handle, named 
       variable, named unit and 2 indices and returns it.
  */
  int  FRAMES2API _DataSetReadLog2(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2);
    #ifndef BUILD_DLL
      #define DataSetReadLog2 _DataSetReadLog2
    #endif

  /*============================================================================
     Documentation for: DataSetWriteLog2
    ============================================================================
       Writes a integer in a dataset given the dataset handle, named 
       variable, named unit and 2 indices.
  */
  void  FRAMES2API _DataSetWriteLog2(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _value);
    #ifndef BUILD_DLL
      #define DataSetWriteLog2 _DataSetWriteLog2
    #endif

  /*============================================================================
     Documentation for: DataSetReadLog3
    ============================================================================
       Reads a integer in a dataset given the dataset handle, named 
       variable, named unit and 3 indices and returns it.
  */
  int  FRAMES2API _DataSetReadLog3(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3);
    #ifndef BUILD_DLL
      #define DataSetReadLog3 _DataSetReadLog3
    #endif

  /*============================================================================
     Documentation for: DataSetWriteLog3
    ============================================================================
       Writes a integer in a dataset given the dataset handle, named 
       variable, named unit and 3 indices.
  */
  void  FRAMES2API _DataSetWriteLog3(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _value);
    #ifndef BUILD_DLL
      #define DataSetWriteLog3 _DataSetWriteLog3
    #endif

  /*============================================================================
     Documentation for: DataSetReadLog4
    ============================================================================
       Reads a integer in a dataset given the dataset handle, named 
       variable, named unit and 4 indices and returns it.
  */
  int  FRAMES2API _DataSetReadLog4(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4);
    #ifndef BUILD_DLL
      #define DataSetReadLog4 _DataSetReadLog4
    #endif

  /*============================================================================
     Documentation for: DataSetWriteLog4
    ============================================================================
       Writes a integer in a dataset given the dataset handle, named 
       variable, named unit and 4 indices.
  */
  void  FRAMES2API _DataSetWriteLog4(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _value);
    #ifndef BUILD_DLL
      #define DataSetWriteLog4 _DataSetWriteLog4
    #endif

  /*============================================================================
     Documentation for: DataSetReadLog5
    ============================================================================
       Reads a integer in a dataset given the dataset handle, named 
       variable, named unit and 5 indices and returns it.
  */
  int  FRAMES2API _DataSetReadLog5(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5);
    #ifndef BUILD_DLL
      #define DataSetReadLog5 _DataSetReadLog5
    #endif

  /*============================================================================
     Documentation for: DataSetWriteLog5
    ============================================================================
       Writes a integer in a dataset given the dataset handle, named 
       variable, named unit and 5 indices.
  */
  void  FRAMES2API _DataSetWriteLog5(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _value);
    #ifndef BUILD_DLL
      #define DataSetWriteLog5 _DataSetWriteLog5
    #endif

  /*============================================================================
     Documentation for: DataSetReadLog6
    ============================================================================
       Reads a integer in a dataset given the dataset handle, named 
       variable, named unit and 6 indices and returns it.
  */
  int  FRAMES2API _DataSetReadLog6(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6);
    #ifndef BUILD_DLL
      #define DataSetReadLog6 _DataSetReadLog6
    #endif

  /*============================================================================
     Documentation for: DataSetWriteLog6
    ============================================================================
       Writes a integer in a dataset given the dataset handle, named 
       variable, named unit and 6 indices.
  */
  void  FRAMES2API _DataSetWriteLog6(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6,int _value);
    #ifndef BUILD_DLL
      #define DataSetWriteLog6 _DataSetWriteLog6
    #endif

  /*============================================================================
     Documentation for: DataSetReadString
    ============================================================================
       Reads a string in a dataset given the dataset handle, named variable, 
       named unit and 0 indices and returns it.
  */
  int  FRAMES2API _DataSetReadString(int _PID,long _datasetHandle,char* _variableName,char* _unitName,char* _retvalue);
    #ifndef BUILD_DLL
      #define DataSetReadString _DataSetReadString
    #endif

  /*============================================================================
     Documentation for: DataSetWriteString
    ============================================================================
       Writes a string in a dataset given the dataset handle, named 
       variable, named unit and 0 indices.
  */
  void  FRAMES2API _DataSetWriteString(int _PID,long _datasetHandle,char* _variableName,char* _unitName,char* _value);
    #ifndef BUILD_DLL
      #define DataSetWriteString _DataSetWriteString
    #endif

  /*============================================================================
     Documentation for: DataSetReadString1
    ============================================================================
       Reads a string in a dataset given the dataset handle, named variable, 
       named unit and 1 indices and returns it.
  */
  int  FRAMES2API _DataSetReadString1(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,char* _retvalue);
    #ifndef BUILD_DLL
      #define DataSetReadString1 _DataSetReadString1
    #endif

  /*============================================================================
     Documentation for: DataSetWriteString1
    ============================================================================
       Writes a string in a dataset given the dataset handle, named 
       variable, named unit and 1 indices.
  */
  void  FRAMES2API _DataSetWriteString1(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,char* _value);
    #ifndef BUILD_DLL
      #define DataSetWriteString1 _DataSetWriteString1
    #endif

  /*============================================================================
     Documentation for: DataSetReadString2
    ============================================================================
       Reads a string in a dataset given the dataset handle, named variable, 
       named unit and 2 indices and returns it.
  */
  int  FRAMES2API _DataSetReadString2(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,char* _retvalue);
    #ifndef BUILD_DLL
      #define DataSetReadString2 _DataSetReadString2
    #endif

  /*============================================================================
     Documentation for: DataSetWriteString2
    ============================================================================
       Writes a string in a dataset given the dataset handle, named 
       variable, named unit and 2 indices.
  */
  void  FRAMES2API _DataSetWriteString2(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,char* _value);
    #ifndef BUILD_DLL
      #define DataSetWriteString2 _DataSetWriteString2
    #endif

  /*============================================================================
     Documentation for: DataSetReadString3
    ============================================================================
       Reads a string in a dataset given the dataset handle, named variable, 
       named unit and 3 indices and returns it.
  */
  int  FRAMES2API _DataSetReadString3(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,char* _retvalue);
    #ifndef BUILD_DLL
      #define DataSetReadString3 _DataSetReadString3
    #endif

  /*============================================================================
     Documentation for: DataSetWriteString3
    ============================================================================
       Writes a string in a dataset given the dataset handle, named 
       variable, named unit and 3 indices.
  */
  void  FRAMES2API _DataSetWriteString3(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,char* _value);
    #ifndef BUILD_DLL
      #define DataSetWriteString3 _DataSetWriteString3
    #endif

  /*============================================================================
     Documentation for: DataSetReadString4
    ============================================================================
       Reads a string in a dataset given the dataset handle, named variable, 
       named unit and 4 indices and returns it.
  */
  int  FRAMES2API _DataSetReadString4(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4,char* _retvalue);
    #ifndef BUILD_DLL
      #define DataSetReadString4 _DataSetReadString4
    #endif

  /*============================================================================
     Documentation for: DataSetWriteString4
    ============================================================================
       Writes a string in a dataset given the dataset handle, named 
       variable, named unit and 4 indices.
  */
  void  FRAMES2API _DataSetWriteString4(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4,char* _value);
    #ifndef BUILD_DLL
      #define DataSetWriteString4 _DataSetWriteString4
    #endif

  /*============================================================================
     Documentation for: DataSetReadString5
    ============================================================================
       Reads a string in a dataset given the dataset handle, named variable, 
       named unit and 5 indices and returns it.
  */
  int  FRAMES2API _DataSetReadString5(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,char* _retvalue);
    #ifndef BUILD_DLL
      #define DataSetReadString5 _DataSetReadString5
    #endif

  /*============================================================================
     Documentation for: DataSetWriteString5
    ============================================================================
       Writes a string in a dataset given the dataset handle, named 
       variable, named unit and 5 indices.
  */
  void  FRAMES2API _DataSetWriteString5(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,char* _value);
    #ifndef BUILD_DLL
      #define DataSetWriteString5 _DataSetWriteString5
    #endif

  /*============================================================================
     Documentation for: DataSetReadString6
    ============================================================================
       Reads a string in a dataset given the dataset handle, named variable, 
       named unit and 6 indices and returns it.
  */
  int  FRAMES2API _DataSetReadString6(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6,char* _retvalue);
    #ifndef BUILD_DLL
      #define DataSetReadString6 _DataSetReadString6
    #endif

  /*============================================================================
     Documentation for: DataSetWriteString6
    ============================================================================
       Writes a string in a dataset given the dataset handle, named 
       variable, named unit and 6 indices.
  */
  void  FRAMES2API _DataSetWriteString6(int _PID,long _datasetHandle,char* _variableName,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6,char* _value);
    #ifndef BUILD_DLL
      #define DataSetWriteString6 _DataSetWriteString6
    #endif

  /*============================================================================
     Documentation for: VariableReadInt
    ============================================================================
       Reads a integer in a dataset given the dataset and variable handles, 
       named unit and 0 indices and returns it.
  */
  int  FRAMES2API _VariableReadInt(int _PID,long _datasetHandle,long _variableHandle,char* _unitName);
    #ifndef BUILD_DLL
      #define VariableReadInt _VariableReadInt
    #endif

  /*============================================================================
     Documentation for: VariableWriteInt
    ============================================================================
       Writes a integer in a dataset given the dataset and variable handles, 
       named unit, and 0 indices.
  */
  void  FRAMES2API _VariableWriteInt(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _value);
    #ifndef BUILD_DLL
      #define VariableWriteInt _VariableWriteInt
    #endif

  /*============================================================================
     Documentation for: VariableReadInt1
    ============================================================================
       Reads a integer in a dataset given the dataset and variable handles, 
       named unit and 1 indices and returns it.
  */
  int  FRAMES2API _VariableReadInt1(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1);
    #ifndef BUILD_DLL
      #define VariableReadInt1 _VariableReadInt1
    #endif

  /*============================================================================
     Documentation for: VariableWriteInt1
    ============================================================================
       Writes a integer in a dataset given the dataset and variable handles, 
       named unit, and 1 indices.
  */
  void  FRAMES2API _VariableWriteInt1(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _value);
    #ifndef BUILD_DLL
      #define VariableWriteInt1 _VariableWriteInt1
    #endif

  /*============================================================================
     Documentation for: VariableReadInt2
    ============================================================================
       Reads a integer in a dataset given the dataset and variable handles, 
       named unit and 2 indices and returns it.
  */
  int  FRAMES2API _VariableReadInt2(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2);
    #ifndef BUILD_DLL
      #define VariableReadInt2 _VariableReadInt2
    #endif

  /*============================================================================
     Documentation for: VariableWriteInt2
    ============================================================================
       Writes a integer in a dataset given the dataset and variable handles, 
       named unit, and 2 indices.
  */
  void  FRAMES2API _VariableWriteInt2(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _value);
    #ifndef BUILD_DLL
      #define VariableWriteInt2 _VariableWriteInt2
    #endif

  /*============================================================================
     Documentation for: VariableReadInt3
    ============================================================================
       Reads a integer in a dataset given the dataset and variable handles, 
       named unit and 3 indices and returns it.
  */
  int  FRAMES2API _VariableReadInt3(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3);
    #ifndef BUILD_DLL
      #define VariableReadInt3 _VariableReadInt3
    #endif

  /*============================================================================
     Documentation for: VariableWriteInt3
    ============================================================================
       Writes a integer in a dataset given the dataset and variable handles, 
       named unit, and 3 indices.
  */
  void  FRAMES2API _VariableWriteInt3(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _value);
    #ifndef BUILD_DLL
      #define VariableWriteInt3 _VariableWriteInt3
    #endif

  /*============================================================================
     Documentation for: VariableReadInt4
    ============================================================================
       Reads a integer in a dataset given the dataset and variable handles, 
       named unit and 4 indices and returns it.
  */
  int  FRAMES2API _VariableReadInt4(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4);
    #ifndef BUILD_DLL
      #define VariableReadInt4 _VariableReadInt4
    #endif

  /*============================================================================
     Documentation for: VariableWriteInt4
    ============================================================================
       Writes a integer in a dataset given the dataset and variable handles, 
       named unit, and 4 indices.
  */
  void  FRAMES2API _VariableWriteInt4(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _value);
    #ifndef BUILD_DLL
      #define VariableWriteInt4 _VariableWriteInt4
    #endif

  /*============================================================================
     Documentation for: VariableReadInt5
    ============================================================================
       Reads a integer in a dataset given the dataset and variable handles, 
       named unit and 5 indices and returns it.
  */
  int  FRAMES2API _VariableReadInt5(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5);
    #ifndef BUILD_DLL
      #define VariableReadInt5 _VariableReadInt5
    #endif

  /*============================================================================
     Documentation for: VariableWriteInt5
    ============================================================================
       Writes a integer in a dataset given the dataset and variable handles, 
       named unit, and 5 indices.
  */
  void  FRAMES2API _VariableWriteInt5(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _value);
    #ifndef BUILD_DLL
      #define VariableWriteInt5 _VariableWriteInt5
    #endif

  /*============================================================================
     Documentation for: VariableReadInt6
    ============================================================================
       Reads a integer in a dataset given the dataset and variable handles, 
       named unit and 6 indices and returns it.
  */
  int  FRAMES2API _VariableReadInt6(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6);
    #ifndef BUILD_DLL
      #define VariableReadInt6 _VariableReadInt6
    #endif

  /*============================================================================
     Documentation for: VariableWriteInt6
    ============================================================================
       Writes a integer in a dataset given the dataset and variable handles, 
       named unit, and 6 indices.
  */
  void  FRAMES2API _VariableWriteInt6(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6,int _value);
    #ifndef BUILD_DLL
      #define VariableWriteInt6 _VariableWriteInt6
    #endif

  /*============================================================================
     Documentation for: VariableReadFloat
    ============================================================================
       Reads a real in a dataset given the dataset and variable handles, 
       named unit and 0 indices and returns it.
  */
  double  FRAMES2API _VariableReadFloat(int _PID,long _datasetHandle,long _variableHandle,char* _unitName);
    #ifndef BUILD_DLL
      #define VariableReadFloat _VariableReadFloat
    #endif

  /*============================================================================
     Documentation for: VariableWriteFloat
    ============================================================================
       Writes a real in a dataset given the dataset and variable handles, 
       named unit, and 0 indices.
  */
  void  FRAMES2API _VariableWriteFloat(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,double _value);
    #ifndef BUILD_DLL
      #define VariableWriteFloat _VariableWriteFloat
    #endif

  /*============================================================================
     Documentation for: VariableReadFloat1
    ============================================================================
       Reads a real in a dataset given the dataset and variable handles, 
       named unit and 1 indices and returns it.
  */
  double  FRAMES2API _VariableReadFloat1(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1);
    #ifndef BUILD_DLL
      #define VariableReadFloat1 _VariableReadFloat1
    #endif

  /*============================================================================
     Documentation for: VariableWriteFloat1
    ============================================================================
       Writes a real in a dataset given the dataset and variable handles, 
       named unit, and 1 indices.
  */
  void  FRAMES2API _VariableWriteFloat1(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,double _value);
    #ifndef BUILD_DLL
      #define VariableWriteFloat1 _VariableWriteFloat1
    #endif

  /*============================================================================
     Documentation for: VariableReadFloat2
    ============================================================================
       Reads a real in a dataset given the dataset and variable handles, 
       named unit and 2 indices and returns it.
  */
  double  FRAMES2API _VariableReadFloat2(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2);
    #ifndef BUILD_DLL
      #define VariableReadFloat2 _VariableReadFloat2
    #endif

  /*============================================================================
     Documentation for: VariableWriteFloat2
    ============================================================================
       Writes a real in a dataset given the dataset and variable handles, 
       named unit, and 2 indices.
  */
  void  FRAMES2API _VariableWriteFloat2(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,double _value);
    #ifndef BUILD_DLL
      #define VariableWriteFloat2 _VariableWriteFloat2
    #endif

  /*============================================================================
     Documentation for: VariableReadFloat3
    ============================================================================
       Reads a real in a dataset given the dataset and variable handles, 
       named unit and 3 indices and returns it.
  */
  double  FRAMES2API _VariableReadFloat3(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3);
    #ifndef BUILD_DLL
      #define VariableReadFloat3 _VariableReadFloat3
    #endif

  /*============================================================================
     Documentation for: VariableWriteFloat3
    ============================================================================
       Writes a real in a dataset given the dataset and variable handles, 
       named unit, and 3 indices.
  */
  void  FRAMES2API _VariableWriteFloat3(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,double _value);
    #ifndef BUILD_DLL
      #define VariableWriteFloat3 _VariableWriteFloat3
    #endif

  /*============================================================================
     Documentation for: VariableReadFloat4
    ============================================================================
       Reads a real in a dataset given the dataset and variable handles, 
       named unit and 4 indices and returns it.
  */
  double  FRAMES2API _VariableReadFloat4(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4);
    #ifndef BUILD_DLL
      #define VariableReadFloat4 _VariableReadFloat4
    #endif

  /*============================================================================
     Documentation for: VariableWriteFloat4
    ============================================================================
       Writes a real in a dataset given the dataset and variable handles, 
       named unit, and 4 indices.
  */
  void  FRAMES2API _VariableWriteFloat4(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,double _value);
    #ifndef BUILD_DLL
      #define VariableWriteFloat4 _VariableWriteFloat4
    #endif

  /*============================================================================
     Documentation for: VariableReadFloat5
    ============================================================================
       Reads a real in a dataset given the dataset and variable handles, 
       named unit and 5 indices and returns it.
  */
  double  FRAMES2API _VariableReadFloat5(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5);
    #ifndef BUILD_DLL
      #define VariableReadFloat5 _VariableReadFloat5
    #endif

  /*============================================================================
     Documentation for: VariableWriteFloat5
    ============================================================================
       Writes a real in a dataset given the dataset and variable handles, 
       named unit, and 5 indices.
  */
  void  FRAMES2API _VariableWriteFloat5(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,double _value);
    #ifndef BUILD_DLL
      #define VariableWriteFloat5 _VariableWriteFloat5
    #endif

  /*============================================================================
     Documentation for: VariableReadFloat6
    ============================================================================
       Reads a real in a dataset given the dataset and variable handles, 
       named unit and 6 indices and returns it.
  */
  double  FRAMES2API _VariableReadFloat6(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6);
    #ifndef BUILD_DLL
      #define VariableReadFloat6 _VariableReadFloat6
    #endif

  /*============================================================================
     Documentation for: VariableWriteFloat6
    ============================================================================
       Writes a real in a dataset given the dataset and variable handles, 
       named unit, and 6 indices.
  */
  void  FRAMES2API _VariableWriteFloat6(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6,double _value);
    #ifndef BUILD_DLL
      #define VariableWriteFloat6 _VariableWriteFloat6
    #endif

  /*============================================================================
     Documentation for: VariableReadLog
    ============================================================================
       Reads a integer in a dataset given the dataset and variable handles, 
       named unit and 0 indices and returns it.
  */
  int  FRAMES2API _VariableReadLog(int _PID,long _datasetHandle,long _variableHandle,char* _unitName);
    #ifndef BUILD_DLL
      #define VariableReadLog _VariableReadLog
    #endif

  /*============================================================================
     Documentation for: VariableWriteLog
    ============================================================================
       Writes a integer in a dataset given the dataset and variable handles, 
       named unit, and 0 indices.
  */
  void  FRAMES2API _VariableWriteLog(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _value);
    #ifndef BUILD_DLL
      #define VariableWriteLog _VariableWriteLog
    #endif

  /*============================================================================
     Documentation for: VariableReadLog1
    ============================================================================
       Reads a integer in a dataset given the dataset and variable handles, 
       named unit and 1 indices and returns it.
  */
  int  FRAMES2API _VariableReadLog1(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1);
    #ifndef BUILD_DLL
      #define VariableReadLog1 _VariableReadLog1
    #endif

  /*============================================================================
     Documentation for: VariableWriteLog1
    ============================================================================
       Writes a integer in a dataset given the dataset and variable handles, 
       named unit, and 1 indices.
  */
  void  FRAMES2API _VariableWriteLog1(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _value);
    #ifndef BUILD_DLL
      #define VariableWriteLog1 _VariableWriteLog1
    #endif

  /*============================================================================
     Documentation for: VariableReadLog2
    ============================================================================
       Reads a integer in a dataset given the dataset and variable handles, 
       named unit and 2 indices and returns it.
  */
  int  FRAMES2API _VariableReadLog2(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2);
    #ifndef BUILD_DLL
      #define VariableReadLog2 _VariableReadLog2
    #endif

  /*============================================================================
     Documentation for: VariableWriteLog2
    ============================================================================
       Writes a integer in a dataset given the dataset and variable handles, 
       named unit, and 2 indices.
  */
  void  FRAMES2API _VariableWriteLog2(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _value);
    #ifndef BUILD_DLL
      #define VariableWriteLog2 _VariableWriteLog2
    #endif

  /*============================================================================
     Documentation for: VariableReadLog3
    ============================================================================
       Reads a integer in a dataset given the dataset and variable handles, 
       named unit and 3 indices and returns it.
  */
  int  FRAMES2API _VariableReadLog3(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3);
    #ifndef BUILD_DLL
      #define VariableReadLog3 _VariableReadLog3
    #endif

  /*============================================================================
     Documentation for: VariableWriteLog3
    ============================================================================
       Writes a integer in a dataset given the dataset and variable handles, 
       named unit, and 3 indices.
  */
  void  FRAMES2API _VariableWriteLog3(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _value);
    #ifndef BUILD_DLL
      #define VariableWriteLog3 _VariableWriteLog3
    #endif

  /*============================================================================
     Documentation for: VariableReadLog4
    ============================================================================
       Reads a integer in a dataset given the dataset and variable handles, 
       named unit and 4 indices and returns it.
  */
  int  FRAMES2API _VariableReadLog4(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4);
    #ifndef BUILD_DLL
      #define VariableReadLog4 _VariableReadLog4
    #endif

  /*============================================================================
     Documentation for: VariableWriteLog4
    ============================================================================
       Writes a integer in a dataset given the dataset and variable handles, 
       named unit, and 4 indices.
  */
  void  FRAMES2API _VariableWriteLog4(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _value);
    #ifndef BUILD_DLL
      #define VariableWriteLog4 _VariableWriteLog4
    #endif

  /*============================================================================
     Documentation for: VariableReadLog5
    ============================================================================
       Reads a integer in a dataset given the dataset and variable handles, 
       named unit and 5 indices and returns it.
  */
  int  FRAMES2API _VariableReadLog5(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5);
    #ifndef BUILD_DLL
      #define VariableReadLog5 _VariableReadLog5
    #endif

  /*============================================================================
     Documentation for: VariableWriteLog5
    ============================================================================
       Writes a integer in a dataset given the dataset and variable handles, 
       named unit, and 5 indices.
  */
  void  FRAMES2API _VariableWriteLog5(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _value);
    #ifndef BUILD_DLL
      #define VariableWriteLog5 _VariableWriteLog5
    #endif

  /*============================================================================
     Documentation for: VariableReadLog6
    ============================================================================
       Reads a integer in a dataset given the dataset and variable handles, 
       named unit and 6 indices and returns it.
  */
  int  FRAMES2API _VariableReadLog6(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6);
    #ifndef BUILD_DLL
      #define VariableReadLog6 _VariableReadLog6
    #endif

  /*============================================================================
     Documentation for: VariableWriteLog6
    ============================================================================
       Writes a integer in a dataset given the dataset and variable handles, 
       named unit, and 6 indices.
  */
  void  FRAMES2API _VariableWriteLog6(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6,int _value);
    #ifndef BUILD_DLL
      #define VariableWriteLog6 _VariableWriteLog6
    #endif

  /*============================================================================
     Documentation for: VariableReadString
    ============================================================================
       Reads a string in a dataset given the dataset and variable handles, 
       named unit and 0 indices and returns it.
  */
  int  FRAMES2API _VariableReadString(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,char* _retvalue);
    #ifndef BUILD_DLL
      #define VariableReadString _VariableReadString
    #endif

  /*============================================================================
     Documentation for: VariableWriteString
    ============================================================================
       Writes a string in a dataset given the dataset and variable handles, 
       named unit, and 0 indices.
  */
  void  FRAMES2API _VariableWriteString(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,char* _value);
    #ifndef BUILD_DLL
      #define VariableWriteString _VariableWriteString
    #endif

  /*============================================================================
     Documentation for: VariableReadString1
    ============================================================================
       Reads a string in a dataset given the dataset and variable handles, 
       named unit and 1 indices and returns it.
  */
  int  FRAMES2API _VariableReadString1(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,char* _retvalue);
    #ifndef BUILD_DLL
      #define VariableReadString1 _VariableReadString1
    #endif

  /*============================================================================
     Documentation for: VariableWriteString1
    ============================================================================
       Writes a string in a dataset given the dataset and variable handles, 
       named unit, and 1 indices.
  */
  void  FRAMES2API _VariableWriteString1(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,char* _value);
    #ifndef BUILD_DLL
      #define VariableWriteString1 _VariableWriteString1
    #endif

  /*============================================================================
     Documentation for: VariableReadString2
    ============================================================================
       Reads a string in a dataset given the dataset and variable handles, 
       named unit and 2 indices and returns it.
  */
  int  FRAMES2API _VariableReadString2(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,char* _retvalue);
    #ifndef BUILD_DLL
      #define VariableReadString2 _VariableReadString2
    #endif

  /*============================================================================
     Documentation for: VariableWriteString2
    ============================================================================
       Writes a string in a dataset given the dataset and variable handles, 
       named unit, and 2 indices.
  */
  void  FRAMES2API _VariableWriteString2(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,char* _value);
    #ifndef BUILD_DLL
      #define VariableWriteString2 _VariableWriteString2
    #endif

  /*============================================================================
     Documentation for: VariableReadString3
    ============================================================================
       Reads a string in a dataset given the dataset and variable handles, 
       named unit and 3 indices and returns it.
  */
  int  FRAMES2API _VariableReadString3(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,char* _retvalue);
    #ifndef BUILD_DLL
      #define VariableReadString3 _VariableReadString3
    #endif

  /*============================================================================
     Documentation for: VariableWriteString3
    ============================================================================
       Writes a string in a dataset given the dataset and variable handles, 
       named unit, and 3 indices.
  */
  void  FRAMES2API _VariableWriteString3(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,char* _value);
    #ifndef BUILD_DLL
      #define VariableWriteString3 _VariableWriteString3
    #endif

  /*============================================================================
     Documentation for: VariableReadString4
    ============================================================================
       Reads a string in a dataset given the dataset and variable handles, 
       named unit and 4 indices and returns it.
  */
  int  FRAMES2API _VariableReadString4(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,char* _retvalue);
    #ifndef BUILD_DLL
      #define VariableReadString4 _VariableReadString4
    #endif

  /*============================================================================
     Documentation for: VariableWriteString4
    ============================================================================
       Writes a string in a dataset given the dataset and variable handles, 
       named unit, and 4 indices.
  */
  void  FRAMES2API _VariableWriteString4(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,char* _value);
    #ifndef BUILD_DLL
      #define VariableWriteString4 _VariableWriteString4
    #endif

  /*============================================================================
     Documentation for: VariableReadString5
    ============================================================================
       Reads a string in a dataset given the dataset and variable handles, 
       named unit and 5 indices and returns it.
  */
  int  FRAMES2API _VariableReadString5(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,char* _retvalue);
    #ifndef BUILD_DLL
      #define VariableReadString5 _VariableReadString5
    #endif

  /*============================================================================
     Documentation for: VariableWriteString5
    ============================================================================
       Writes a string in a dataset given the dataset and variable handles, 
       named unit, and 5 indices.
  */
  void  FRAMES2API _VariableWriteString5(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,char* _value);
    #ifndef BUILD_DLL
      #define VariableWriteString5 _VariableWriteString5
    #endif

  /*============================================================================
     Documentation for: VariableReadString6
    ============================================================================
       Reads a string in a dataset given the dataset and variable handles, 
       named unit and 6 indices and returns it.
  */
  int  FRAMES2API _VariableReadString6(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6,char* _retvalue);
    #ifndef BUILD_DLL
      #define VariableReadString6 _VariableReadString6
    #endif

  /*============================================================================
     Documentation for: VariableWriteString6
    ============================================================================
       Writes a string in a dataset given the dataset and variable handles, 
       named unit, and 6 indices.
  */
  void  FRAMES2API _VariableWriteString6(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6,char* _value);
    #ifndef BUILD_DLL
      #define VariableWriteString6 _VariableWriteString6
    #endif

  /*============================================================================
     Documentation for: VariableGetHandleByDataSet
    ============================================================================
       Returns the variable handle of the named dataset variable if it 
       exists. If the handle is less than 0 an error has been returned.
  */
  long  FRAMES2API _VariableGetHandleByDataSet(int _PID,long _datasetHandle,char* _variableName);
    #ifndef BUILD_DLL
      #define VariableGetHandleByDataSet _VariableGetHandleByDataSet
    #endif

  /*============================================================================
     Documentation for: VariableGetHandleByDictionary
    ============================================================================
       Returns the variable handle of the named dictionary variable if it 
       exists. If the handle is less than 0 an error has been returned.
  */
  long  FRAMES2API _VariableGetHandleByDictionary(int _PID,long _dictionaryHandle,char* _variableName);
    #ifndef BUILD_DLL
      #define VariableGetHandleByDictionary _VariableGetHandleByDictionary
    #endif

  /*============================================================================
     Documentation for: VariableGetHandleByModule
    ============================================================================
       Returns the variable handle of the named module variable if it 
       exists. If the handle is less than 0 an error has been returned.
  */
  long  FRAMES2API _VariableGetHandleByModule(int _PID,long _moduleHandle,char* _variableName);
    #ifndef BUILD_DLL
      #define VariableGetHandleByModule _VariableGetHandleByModule
    #endif

  /*============================================================================
     Documentation for: DictionaryGetHandle
    ============================================================================
       Returns the dictionary handle of the named dictionary if it exists. 
       If the handle is less than 0 an error has been returned.
  */
  long  FRAMES2API _DictionaryGetHandle(int _PID,char* _dictionaryName);
    #ifndef BUILD_DLL
      #define DictionaryGetHandle _DictionaryGetHandle
    #endif

  /*============================================================================
     Documentation for: DictionaryGetPath
    ============================================================================
       Returns the path of the dictionary given the dictionary handle. To 
       change the path you need to 1) F2SystemSaveDictionaryAs changing the 
       path, 2) F2SystemCloseDictionary, and 3) SystemOpenDictionary from it 
       new location.
  */
  int  FRAMES2API _DictionaryGetPath(int _PID,long _dictionaryHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define DictionaryGetPath _DictionaryGetPath
    #endif

  /*============================================================================
     Documentation for: DictionaryGetUpdate
    ============================================================================
       Returns the update flag of the dictionary given the dictionary 
       handle. If the state of the dictionary has been updated or modified, 
       this function will return a non-zero integer. 0 is returned if no 
       updates have occurred.
  */
  int  FRAMES2API _DictionaryGetUpdate(int _PID,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define DictionaryGetUpdate _DictionaryGetUpdate
    #endif

  /*============================================================================
     Documentation for: DictionaryGetName
    ============================================================================
       Returns the name of the dictionary given the dictionary handle.
  */
  int  FRAMES2API _DictionaryGetName(int _PID,long _dictionaryHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define DictionaryGetName _DictionaryGetName
    #endif

  /*============================================================================
     Documentation for: DictionaryGetDescription
    ============================================================================
       Returns the description of the dictionary given the dictionary handle.
  */
  int  FRAMES2API _DictionaryGetDescription(int _PID,long _dictionaryHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define DictionaryGetDescription _DictionaryGetDescription
    #endif

  /*============================================================================
     Documentation for: DictionaryGetVersion
    ============================================================================
       Returns the version number of the dictionary given the dictionary 
       handle.
  */
  int  FRAMES2API _DictionaryGetVersion(int _PID,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define DictionaryGetVersion _DictionaryGetVersion
    #endif

  /*============================================================================
     Documentation for: DictionaryGetPrivilege
    ============================================================================
       Returns the priviledge of the dictionary by the provided dictionary 
       handle. The privilege levels are: 0=Module UI, and 1=Boundary 
       Condition. This represents the level of review a dictionary has 
       received. A system dictionary should have a number of individuals 
       agree that it is complete, useful and mutually exclusive of other 
       system dictionaries.
  */
  int  FRAMES2API _DictionaryGetPrivilege(int _PID,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define DictionaryGetPrivilege _DictionaryGetPrivilege
    #endif

  /*============================================================================
     Documentation for: ModuleGetHandle
    ============================================================================
       Returns the handle to the named module if it exists. If the handle is 
       less than 0 an error has been returned.
  */
  long  FRAMES2API _ModuleGetHandle(int _PID,char* _moduleName);
    #ifndef BUILD_DLL
      #define ModuleGetHandle _ModuleGetHandle
    #endif

  /*============================================================================
     Documentation for: SchemeGetHandle
    ============================================================================
       Returns the handle to the named scheme if it exists. If the handle is 
       less than 0 an error has been returned.
  */
  long  FRAMES2API _SchemeGetHandle(int _PID,long _moduleHandle,char* _schemeName);
    #ifndef BUILD_DLL
      #define SchemeGetHandle _SchemeGetHandle
    #endif

  /*============================================================================
     Documentation for: SimulationGetHandle
    ============================================================================
       Returns the handle to the named simulation if it exists. If the 
       handle is less than 0 an error has been returned.
  */
  long  FRAMES2API _SimulationGetHandle(int _PID,char* _simulationName);
    #ifndef BUILD_DLL
      #define SimulationGetHandle _SimulationGetHandle
    #endif

  /*============================================================================
     Documentation for: DataSetGetHandle
    ============================================================================
       Returns the handle to the named dataset if it exists. If the handle 
       is less than 0 an error has been returned.
  */
  long  FRAMES2API _DataSetGetHandle(int _PID,char* _datasetName);
    #ifndef BUILD_DLL
      #define DataSetGetHandle _DataSetGetHandle
    #endif

  /*============================================================================
     Documentation for: IconGetHandle
    ============================================================================
       Returns the handle to the named icon if it exists. If the handle is 
       less than 0 an error has been returned.
  */
  long  FRAMES2API _IconGetHandle(int _PID,char* _iconName);
    #ifndef BUILD_DLL
      #define IconGetHandle _IconGetHandle
    #endif

#ifdef __cplusplus
  }
#endif
#endif
