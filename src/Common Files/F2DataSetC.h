#ifndef FRAMES2API_F2DataSet_H
#define FRAMES2API_F2DataSet_H

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

  // API Name: FRAMES DataSet API
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
       to this documentation         Fully Qualified File     example: 
       "C:\FramesV2\Simulations\GWtest\test.sim"        Fully Qualified Path 
           example: "C:\FramesV2\Simulations\GWtest"        Full File        
               example: "\FramesV2\Simulations\GWtest\test.sim"        Full 
       Path                example: "\FramesV2\Simulations\Gwtest"        
       File                     example: "test.sim"
  */
  /*============================================================================
     Documentation for: F2OpenIO
    ============================================================================
       OpenIO returns the modules Process IDentification (PID). If the 
       returned pid is less than zero an error occurred. The path\name 
       arguments are to the project file the PID, path, name and modId are 
       given to the module via the command line.   For nearly all functions 
       in the API an integer value is returned. The integer value  
       represents whether the function called succeeded or not. A value of 
       SUCCESS indicates that the function did succeed. In error cases a 
       negative value is returned that defines the error. There are 
       functions available to discovery what the error code means.
  */
  int  FRAMES2API _F2OpenIO(char* _simulationPath,char* _simulationName,char* _iconName);
    #ifndef BUILD_DLL
      #define F2OpenIO _F2OpenIO
    #endif

  /*============================================================================
     Documentation for: F2CloseIO
    ============================================================================
       This functions closes the INI file and cleans all resources     
       associated with SystemIO.dll. If cancel is 0 then the updated 
       datasets are     saved otherwise the updated datasets are thrown away.
  */
  void  FRAMES2API _F2CloseIO(int _PID,int _cancel);
    #ifndef BUILD_DLL
      #define F2CloseIO _F2CloseIO
    #endif

  /*============================================================================
     Documentation for: F2InputModuleCount
    ============================================================================
       Get the number of modules this module is consuming.
  */
  int  FRAMES2API _F2InputModuleCount(int _PID,long _iconHandle);
    #ifndef BUILD_DLL
      #define F2InputModuleCount _F2InputModuleCount
    #endif

  /*============================================================================
     Documentation for: F2OutputModuleCount
    ============================================================================
       Get the number of modules this module is producing results for.
  */
  int  FRAMES2API _F2OutputModuleCount(int _PID,long _iconHandle);
    #ifndef BUILD_DLL
      #define F2OutputModuleCount _F2OutputModuleCount
    #endif

  /*============================================================================
     Documentation for: F2InputModuleDataSetCount
    ============================================================================
       Get the number of input datasets from _modId.
  */
  int  FRAMES2API _F2InputModuleDataSetCount(int _PID,long _fromIconHandle,long _toIconHandle);
    #ifndef BUILD_DLL
      #define F2InputModuleDataSetCount _F2InputModuleDataSetCount
    #endif

  /*============================================================================
     Documentation for: F2OutputModuleDataSetCount
    ============================================================================
       Get the number of output datasets from _modId.
  */
  int  FRAMES2API _F2OutputModuleDataSetCount(int _PID,long _fromIconHandle,long _toIconHandle);
    #ifndef BUILD_DLL
      #define F2OutputModuleDataSetCount _F2OutputModuleDataSetCount
    #endif

  /*============================================================================
     Documentation for: F2GetDictionaryId
    ============================================================================
       Gets dictionary for specified dataset.
  */
  int  FRAMES2API _F2GetDictionaryId(int _PID,long _datasetHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetDictionaryId _F2GetDictionaryId
    #endif

  /*============================================================================
     Documentation for: F2GetInputDataSet
    ============================================================================
       Get the input dataset name at dataSetIndex and is returned.
  */
  int  FRAMES2API _F2GetInputDataSet(int _PID,long _fromIconHandle,long _toIconHandle,int _datasetIndex,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetInputDataSet _F2GetInputDataSet
    #endif

  /*============================================================================
     Documentation for: F2GetOutputDataSet
    ============================================================================
       Get the output dataset name at dataSetIndex and is returned.
  */
  int  FRAMES2API _F2GetOutputDataSet(int _PID,long _fromIconHandle,long _toIconHandle,int _datasetIndex,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetOutputDataSet _F2GetOutputDataSet
    #endif

  /*============================================================================
     Documentation for: F2GetVariableDescription
    ============================================================================
       Get the variable description.
  */
  int  FRAMES2API _F2GetVariableDescription(int _PID,long _dictionaryHandle,long _variableHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetVariableDescription _F2GetVariableDescription
    #endif

  /*============================================================================
     Documentation for: F2GetVariableType
    ============================================================================
       Get the varible data type. The returned string will be 
       "Integer","String","Real", or "Logical"
  */
  int  FRAMES2API _F2GetVariableType(int _PID,long _dictionaryHandle,long _variableHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetVariableType _F2GetVariableType
    #endif

  /*============================================================================
     Documentation for: F2GetVariableScalar
    ============================================================================
       Get the variables scalar flag. 1=scalar 0=a collection of values 
       (vector)
  */
  int  FRAMES2API _F2GetVariableScalar(int _PID,long _dictionaryHandle,long _variableHandle);
    #ifndef BUILD_DLL
      #define F2GetVariableScalar _F2GetVariableScalar
    #endif

  /*============================================================================
     Documentation for: F2GetVariableMinimum
    ============================================================================
       Get the minimum value for Integer and Float, length for Strings, zero 
       for Logical.
  */
  double  FRAMES2API _F2GetVariableMinimum(int _PID,long _dictionaryHandle,long _variableHandle);
    #ifndef BUILD_DLL
      #define F2GetVariableMinimum _F2GetVariableMinimum
    #endif

  /*============================================================================
     Documentation for: F2GetVariableMaximum
    ============================================================================
       Get the maximum value for Integer and Float, length for Strings, zero 
       for Logical.
  */
  double  FRAMES2API _F2GetVariableMaximum(int _PID,long _dictionaryHandle,long _variableHandle);
    #ifndef BUILD_DLL
      #define F2GetVariableMaximum _F2GetVariableMaximum
    #endif

  /*============================================================================
     Documentation for: F2GetVariableMeasure
    ============================================================================
       Get the variable measurement type.
  */
  int  FRAMES2API _F2GetVariableMeasure(int _PID,long _dictionaryHandle,long _variableHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetVariableMeasure _F2GetVariableMeasure
    #endif

  /*============================================================================
     Documentation for: F2GetVariableUnit
    ============================================================================
       Get the variable unit of measure.
  */
  int  FRAMES2API _F2GetVariableUnit(int _PID,long _dictionaryHandle,long _variableHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetVariableUnit _F2GetVariableUnit
    #endif

  /*============================================================================
     Documentation for: F2GetVariableStochastic
    ============================================================================
       Get the variables stochastic flag. This flag signifies if the 
       parameter can be modified by an another      program and not 
       invalidate any calibration. This allows other programs such and 
       Sensativity/Uncertainty or      Parameter estimation programs to 
       modify these parameters.
  */
  int  FRAMES2API _F2GetVariableStochastic(int _PID,long _dictionaryHandle,long _variableHandle);
    #ifndef BUILD_DLL
      #define F2GetVariableStochastic _F2GetVariableStochastic
    #endif

  /*============================================================================
     Documentation for: F2GetVariablePreposition
    ============================================================================
       Get the variables preposition. This is the word that would best be 
       used to write a description of this      parameters as an index for 
       another. For example the chemical parameter would use "for" as a 
       preposition to      make statements such as Concentration for 
       benzene. The preposition greatly facilitates writing descriptive     
       text associated with results.
  */
  int  FRAMES2API _F2GetVariablePreposition(int _PID,long _dictionaryHandle,long _variableHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetVariablePreposition _F2GetVariablePreposition
    #endif

  /*============================================================================
     Documentation for: F2GetVariablePrimaryKey
    ============================================================================
       Get the variables primarykey flag. Is this parameter a key to 
       information on other parameters. This is      used in both databases 
       and in the representation of the data. For example a variable being 
       used to index      other variables frequently (like chemical) would 
       indicate that chemical should be a primary key. The typical     
       meaning of primary key from Database design is not incorrect when 
       used here.
  */
  int  FRAMES2API _F2GetVariablePrimaryKey(int _PID,long _dictionaryHandle,long _variableHandle);
    #ifndef BUILD_DLL
      #define F2GetVariablePrimaryKey _F2GetVariablePrimaryKey
    #endif

  /*============================================================================
     Documentation for: F2GetVariableDimension
    ============================================================================
       Get the variables scalar flag.
  */
  int  FRAMES2API _F2GetVariableDimension(int _PID,long _dictionaryHandle,long _variableHandle);
    #ifndef BUILD_DLL
      #define F2GetVariableDimension _F2GetVariableDimension
    #endif

  /*============================================================================
     Documentation for: F2GetVariableDimensionCount
    ============================================================================
       Gets the size of the specified dimensions. The indices parameters 
       define an array of integers that      define the index values that 
       are to be used.
  */
  int  FRAMES2API _F2GetVariableDimensionCount(int _PID,long _datasetHandle,long _variableHandle,int _indices[]);
    #ifndef BUILD_DLL
      #define F2GetVariableDimensionCount _F2GetVariableDimensionCount
    #endif

  /*============================================================================
     Documentation for: F2VariableLookUp
    ============================================================================
       Lookup an index into the array. The indices parameters define an 
       array of integers that define the index      values that are to be 
       used. The specified string value is searched in the variable for the 
       given index      values and the last index value is set. A negative 
       number indicates the element could not be found.
  */
  int  FRAMES2API _F2VariableLookUp(int _PID,long _datasetHandle,long _variableHandle,char* _value,int _indices[]);
    #ifndef BUILD_DLL
      #define F2VariableLookUp _F2VariableLookUp
    #endif

  /*============================================================================
     Documentation for: F2ClearVariable
    ============================================================================
       Deletes all values associated with a variable.
  */
  void  FRAMES2API _F2ClearVariable(int _PID,long _datasetHandle,long _variableHandle,int _indices[]);
    #ifndef BUILD_DLL
      #define F2ClearVariable _F2ClearVariable
    #endif

  /*============================================================================
     Documentation for: F2GetInteger
    ============================================================================
       Get an Integer from a dataset given the variable name, units and a 
       set of indices.
  */
  int  FRAMES2API _F2GetInteger(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _indices[]);
    #ifndef BUILD_DLL
      #define F2GetInteger _F2GetInteger
    #endif

  /*============================================================================
     Documentation for: F2GetFloat
    ============================================================================
       Get an Float from a dataset given the variable name, units and a set 
       of indices.
  */
  double  FRAMES2API _F2GetFloat(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _indices[]);
    #ifndef BUILD_DLL
      #define F2GetFloat _F2GetFloat
    #endif

  /*============================================================================
     Documentation for: F2GetLogical
    ============================================================================
       Get an Boolean from a dataset given the variable name, units and a 
       set of indices.     Booleans are treated as integers with either a 
       value of 1 (true) or 0 (false).
  */
  int  FRAMES2API _F2GetLogical(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _indices[]);
    #ifndef BUILD_DLL
      #define F2GetLogical _F2GetLogical
    #endif

  /*============================================================================
     Documentation for: F2GetString
    ============================================================================
       Get an String from a dataset given the variable name, units and a set 
       of indices.
  */
  int  FRAMES2API _F2GetString(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _indices[],char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetString _F2GetString
    #endif

  /*============================================================================
     Documentation for: F2SetInteger
    ============================================================================
       Put an Integer in a dataset given the variable name, units and a set 
       of indices.
  */
  void  FRAMES2API _F2SetInteger(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _indices[],int _value);
    #ifndef BUILD_DLL
      #define F2SetInteger _F2SetInteger
    #endif

  /*============================================================================
     Documentation for: F2SetFloat
    ============================================================================
       Put an Float in a dataset given the variable name, units and a set of 
       indices.
  */
  void  FRAMES2API _F2SetFloat(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _indices[],double _value);
    #ifndef BUILD_DLL
      #define F2SetFloat _F2SetFloat
    #endif

  /*============================================================================
     Documentation for: F2SetLogical
    ============================================================================
       Put an Boolean in a dataset given the variable name, units and a set 
       of indices.     Booleans are treated as integers with either a value 
       of 1 (true) or 0 (false).
  */
  void  FRAMES2API _F2SetLogical(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _indices[],int _value);
    #ifndef BUILD_DLL
      #define F2SetLogical _F2SetLogical
    #endif

  /*============================================================================
     Documentation for: F2SetString
    ============================================================================
       Put an String in a dataset given the variable name, units and a set 
       of indices.
  */
  void  FRAMES2API _F2SetString(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _indices[],char* _value);
    #ifndef BUILD_DLL
      #define F2SetString _F2SetString
    #endif

  /*============================================================================
     Documentation for: F2ReadInt
    ============================================================================
       Read an integer in a dataset given the variable name, units. The 
       return value is the integer.
  */
  int  FRAMES2API _F2ReadInt(int _PID,long _datasetHandle,long _variableHandle,char* _unitName);
    #ifndef BUILD_DLL
      #define F2ReadInt _F2ReadInt
    #endif

  /*============================================================================
     Documentation for: F2WriteInt
    ============================================================================
       Write a/an integer in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteInt(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _value);
    #ifndef BUILD_DLL
      #define F2WriteInt _F2WriteInt
    #endif

  /*============================================================================
     Documentation for: F2ReadInt1
    ============================================================================
       Read an integer in a dataset given the variable name, units. The 
       return value is the integer.
  */
  int  FRAMES2API _F2ReadInt1(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1);
    #ifndef BUILD_DLL
      #define F2ReadInt1 _F2ReadInt1
    #endif

  /*============================================================================
     Documentation for: F2WriteInt1
    ============================================================================
       Write a/an integer in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteInt1(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _value);
    #ifndef BUILD_DLL
      #define F2WriteInt1 _F2WriteInt1
    #endif

  /*============================================================================
     Documentation for: F2ReadInt2
    ============================================================================
       Read an integer in a dataset given the variable name, units. The 
       return value is the integer.
  */
  int  FRAMES2API _F2ReadInt2(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2);
    #ifndef BUILD_DLL
      #define F2ReadInt2 _F2ReadInt2
    #endif

  /*============================================================================
     Documentation for: F2WriteInt2
    ============================================================================
       Write a/an integer in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteInt2(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _value);
    #ifndef BUILD_DLL
      #define F2WriteInt2 _F2WriteInt2
    #endif

  /*============================================================================
     Documentation for: F2ReadInt3
    ============================================================================
       Read an integer in a dataset given the variable name, units. The 
       return value is the integer.
  */
  int  FRAMES2API _F2ReadInt3(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3);
    #ifndef BUILD_DLL
      #define F2ReadInt3 _F2ReadInt3
    #endif

  /*============================================================================
     Documentation for: F2WriteInt3
    ============================================================================
       Write a/an integer in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteInt3(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _value);
    #ifndef BUILD_DLL
      #define F2WriteInt3 _F2WriteInt3
    #endif

  /*============================================================================
     Documentation for: F2ReadInt4
    ============================================================================
       Read an integer in a dataset given the variable name, units. The 
       return value is the integer.
  */
  int  FRAMES2API _F2ReadInt4(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4);
    #ifndef BUILD_DLL
      #define F2ReadInt4 _F2ReadInt4
    #endif

  /*============================================================================
     Documentation for: F2WriteInt4
    ============================================================================
       Write a/an integer in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteInt4(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _value);
    #ifndef BUILD_DLL
      #define F2WriteInt4 _F2WriteInt4
    #endif

  /*============================================================================
     Documentation for: F2ReadInt5
    ============================================================================
       Read an integer in a dataset given the variable name, units. The 
       return value is the integer.
  */
  int  FRAMES2API _F2ReadInt5(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5);
    #ifndef BUILD_DLL
      #define F2ReadInt5 _F2ReadInt5
    #endif

  /*============================================================================
     Documentation for: F2WriteInt5
    ============================================================================
       Write a/an integer in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteInt5(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _value);
    #ifndef BUILD_DLL
      #define F2WriteInt5 _F2WriteInt5
    #endif

  /*============================================================================
     Documentation for: F2ReadInt6
    ============================================================================
       Read an integer in a dataset given the variable name, units. The 
       return value is the integer.
  */
  int  FRAMES2API _F2ReadInt6(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6);
    #ifndef BUILD_DLL
      #define F2ReadInt6 _F2ReadInt6
    #endif

  /*============================================================================
     Documentation for: F2WriteInt6
    ============================================================================
       Write a/an integer in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteInt6(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6,int _value);
    #ifndef BUILD_DLL
      #define F2WriteInt6 _F2WriteInt6
    #endif

  /*============================================================================
     Documentation for: F2ReadReal
    ============================================================================
       Read an real in a dataset given the variable name, units. The return 
       value is the real.
  */
  double  FRAMES2API _F2ReadReal(int _PID,long _datasetHandle,long _variableHandle,char* _unitName);
    #ifndef BUILD_DLL
      #define F2ReadReal _F2ReadReal
    #endif

  /*============================================================================
     Documentation for: F2WriteReal
    ============================================================================
       Write a/an real in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteReal(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,double _value);
    #ifndef BUILD_DLL
      #define F2WriteReal _F2WriteReal
    #endif

  /*============================================================================
     Documentation for: F2ReadReal1
    ============================================================================
       Read an real in a dataset given the variable name, units. The return 
       value is the real.
  */
  double  FRAMES2API _F2ReadReal1(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1);
    #ifndef BUILD_DLL
      #define F2ReadReal1 _F2ReadReal1
    #endif

  /*============================================================================
     Documentation for: F2WriteReal1
    ============================================================================
       Write a/an real in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteReal1(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,double _value);
    #ifndef BUILD_DLL
      #define F2WriteReal1 _F2WriteReal1
    #endif

  /*============================================================================
     Documentation for: F2ReadReal2
    ============================================================================
       Read an real in a dataset given the variable name, units. The return 
       value is the real.
  */
  double  FRAMES2API _F2ReadReal2(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2);
    #ifndef BUILD_DLL
      #define F2ReadReal2 _F2ReadReal2
    #endif

  /*============================================================================
     Documentation for: F2WriteReal2
    ============================================================================
       Write a/an real in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteReal2(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,double _value);
    #ifndef BUILD_DLL
      #define F2WriteReal2 _F2WriteReal2
    #endif

  /*============================================================================
     Documentation for: F2ReadReal3
    ============================================================================
       Read an real in a dataset given the variable name, units. The return 
       value is the real.
  */
  double  FRAMES2API _F2ReadReal3(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3);
    #ifndef BUILD_DLL
      #define F2ReadReal3 _F2ReadReal3
    #endif

  /*============================================================================
     Documentation for: F2WriteReal3
    ============================================================================
       Write a/an real in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteReal3(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,double _value);
    #ifndef BUILD_DLL
      #define F2WriteReal3 _F2WriteReal3
    #endif

  /*============================================================================
     Documentation for: F2ReadReal4
    ============================================================================
       Read an real in a dataset given the variable name, units. The return 
       value is the real.
  */
  double  FRAMES2API _F2ReadReal4(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4);
    #ifndef BUILD_DLL
      #define F2ReadReal4 _F2ReadReal4
    #endif

  /*============================================================================
     Documentation for: F2WriteReal4
    ============================================================================
       Write a/an real in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteReal4(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,double _value);
    #ifndef BUILD_DLL
      #define F2WriteReal4 _F2WriteReal4
    #endif

  /*============================================================================
     Documentation for: F2ReadReal5
    ============================================================================
       Read an real in a dataset given the variable name, units. The return 
       value is the real.
  */
  double  FRAMES2API _F2ReadReal5(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5);
    #ifndef BUILD_DLL
      #define F2ReadReal5 _F2ReadReal5
    #endif

  /*============================================================================
     Documentation for: F2WriteReal5
    ============================================================================
       Write a/an real in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteReal5(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,double _value);
    #ifndef BUILD_DLL
      #define F2WriteReal5 _F2WriteReal5
    #endif

  /*============================================================================
     Documentation for: F2ReadReal6
    ============================================================================
       Read an real in a dataset given the variable name, units. The return 
       value is the real.
  */
  double  FRAMES2API _F2ReadReal6(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6);
    #ifndef BUILD_DLL
      #define F2ReadReal6 _F2ReadReal6
    #endif

  /*============================================================================
     Documentation for: F2WriteReal6
    ============================================================================
       Write a/an real in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteReal6(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6,double _value);
    #ifndef BUILD_DLL
      #define F2WriteReal6 _F2WriteReal6
    #endif

  /*============================================================================
     Documentation for: F2ReadLog
    ============================================================================
       Read an integer in a dataset given the variable name, units. The 
       return value is the integer.
  */
  int  FRAMES2API _F2ReadLog(int _PID,long _datasetHandle,long _variableHandle,char* _unitName);
    #ifndef BUILD_DLL
      #define F2ReadLog _F2ReadLog
    #endif

  /*============================================================================
     Documentation for: F2WriteLog
    ============================================================================
       Write a/an integer in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteLog(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _value);
    #ifndef BUILD_DLL
      #define F2WriteLog _F2WriteLog
    #endif

  /*============================================================================
     Documentation for: F2ReadLog1
    ============================================================================
       Read an integer in a dataset given the variable name, units. The 
       return value is the integer.
  */
  int  FRAMES2API _F2ReadLog1(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1);
    #ifndef BUILD_DLL
      #define F2ReadLog1 _F2ReadLog1
    #endif

  /*============================================================================
     Documentation for: F2WriteLog1
    ============================================================================
       Write a/an integer in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteLog1(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _value);
    #ifndef BUILD_DLL
      #define F2WriteLog1 _F2WriteLog1
    #endif

  /*============================================================================
     Documentation for: F2ReadLog2
    ============================================================================
       Read an integer in a dataset given the variable name, units. The 
       return value is the integer.
  */
  int  FRAMES2API _F2ReadLog2(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2);
    #ifndef BUILD_DLL
      #define F2ReadLog2 _F2ReadLog2
    #endif

  /*============================================================================
     Documentation for: F2WriteLog2
    ============================================================================
       Write a/an integer in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteLog2(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _value);
    #ifndef BUILD_DLL
      #define F2WriteLog2 _F2WriteLog2
    #endif

  /*============================================================================
     Documentation for: F2ReadLog3
    ============================================================================
       Read an integer in a dataset given the variable name, units. The 
       return value is the integer.
  */
  int  FRAMES2API _F2ReadLog3(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3);
    #ifndef BUILD_DLL
      #define F2ReadLog3 _F2ReadLog3
    #endif

  /*============================================================================
     Documentation for: F2WriteLog3
    ============================================================================
       Write a/an integer in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteLog3(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _value);
    #ifndef BUILD_DLL
      #define F2WriteLog3 _F2WriteLog3
    #endif

  /*============================================================================
     Documentation for: F2ReadLog4
    ============================================================================
       Read an integer in a dataset given the variable name, units. The 
       return value is the integer.
  */
  int  FRAMES2API _F2ReadLog4(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4);
    #ifndef BUILD_DLL
      #define F2ReadLog4 _F2ReadLog4
    #endif

  /*============================================================================
     Documentation for: F2WriteLog4
    ============================================================================
       Write a/an integer in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteLog4(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _value);
    #ifndef BUILD_DLL
      #define F2WriteLog4 _F2WriteLog4
    #endif

  /*============================================================================
     Documentation for: F2ReadLog5
    ============================================================================
       Read an integer in a dataset given the variable name, units. The 
       return value is the integer.
  */
  int  FRAMES2API _F2ReadLog5(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5);
    #ifndef BUILD_DLL
      #define F2ReadLog5 _F2ReadLog5
    #endif

  /*============================================================================
     Documentation for: F2WriteLog5
    ============================================================================
       Write a/an integer in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteLog5(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _value);
    #ifndef BUILD_DLL
      #define F2WriteLog5 _F2WriteLog5
    #endif

  /*============================================================================
     Documentation for: F2ReadLog6
    ============================================================================
       Read an integer in a dataset given the variable name, units. The 
       return value is the integer.
  */
  int  FRAMES2API _F2ReadLog6(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6);
    #ifndef BUILD_DLL
      #define F2ReadLog6 _F2ReadLog6
    #endif

  /*============================================================================
     Documentation for: F2WriteLog6
    ============================================================================
       Write a/an integer in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteLog6(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6,int _value);
    #ifndef BUILD_DLL
      #define F2WriteLog6 _F2WriteLog6
    #endif

  /*============================================================================
     Documentation for: F2ReadString
    ============================================================================
       Read an string in a dataset given the variable name, units. The 
       return value is the string.
  */
  int  FRAMES2API _F2ReadString(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2ReadString _F2ReadString
    #endif

  /*============================================================================
     Documentation for: F2WriteString
    ============================================================================
       Write a/an string in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteString(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,char* _value);
    #ifndef BUILD_DLL
      #define F2WriteString _F2WriteString
    #endif

  /*============================================================================
     Documentation for: F2ReadString1
    ============================================================================
       Read an string in a dataset given the variable name, units. The 
       return value is the string.
  */
  int  FRAMES2API _F2ReadString1(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2ReadString1 _F2ReadString1
    #endif

  /*============================================================================
     Documentation for: F2WriteString1
    ============================================================================
       Write a/an string in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteString1(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,char* _value);
    #ifndef BUILD_DLL
      #define F2WriteString1 _F2WriteString1
    #endif

  /*============================================================================
     Documentation for: F2ReadString2
    ============================================================================
       Read an string in a dataset given the variable name, units. The 
       return value is the string.
  */
  int  FRAMES2API _F2ReadString2(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2ReadString2 _F2ReadString2
    #endif

  /*============================================================================
     Documentation for: F2WriteString2
    ============================================================================
       Write a/an string in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteString2(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,char* _value);
    #ifndef BUILD_DLL
      #define F2WriteString2 _F2WriteString2
    #endif

  /*============================================================================
     Documentation for: F2ReadString3
    ============================================================================
       Read an string in a dataset given the variable name, units. The 
       return value is the string.
  */
  int  FRAMES2API _F2ReadString3(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2ReadString3 _F2ReadString3
    #endif

  /*============================================================================
     Documentation for: F2WriteString3
    ============================================================================
       Write a/an string in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteString3(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,char* _value);
    #ifndef BUILD_DLL
      #define F2WriteString3 _F2WriteString3
    #endif

  /*============================================================================
     Documentation for: F2ReadString4
    ============================================================================
       Read an string in a dataset given the variable name, units. The 
       return value is the string.
  */
  int  FRAMES2API _F2ReadString4(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2ReadString4 _F2ReadString4
    #endif

  /*============================================================================
     Documentation for: F2WriteString4
    ============================================================================
       Write a/an string in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteString4(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,char* _value);
    #ifndef BUILD_DLL
      #define F2WriteString4 _F2WriteString4
    #endif

  /*============================================================================
     Documentation for: F2ReadString5
    ============================================================================
       Read an string in a dataset given the variable name, units. The 
       return value is the string.
  */
  int  FRAMES2API _F2ReadString5(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2ReadString5 _F2ReadString5
    #endif

  /*============================================================================
     Documentation for: F2WriteString5
    ============================================================================
       Write a/an string in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteString5(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,char* _value);
    #ifndef BUILD_DLL
      #define F2WriteString5 _F2WriteString5
    #endif

  /*============================================================================
     Documentation for: F2ReadString6
    ============================================================================
       Read an string in a dataset given the variable name, units. The 
       return value is the string.
  */
  int  FRAMES2API _F2ReadString6(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2ReadString6 _F2ReadString6
    #endif

  /*============================================================================
     Documentation for: F2WriteString6
    ============================================================================
       Write a/an string in a dataset given the variable name, units.
  */
  void  FRAMES2API _F2WriteString6(int _PID,long _datasetHandle,long _variableHandle,char* _unitName,int _index1,int _index2,int _index3,int _index4,int _index5,int _index6,char* _value);
    #ifndef BUILD_DLL
      #define F2WriteString6 _F2WriteString6
    #endif

  /*============================================================================
     Documentation for: F2GetDataSetVariableHandle
    ============================================================================
       Get the dataset variable handle.
  */
  long  FRAMES2API _F2GetDataSetVariableHandle(int _PID,long _datasetHandle,char* _variableName);
    #ifndef BUILD_DLL
      #define F2GetDataSetVariableHandle _F2GetDataSetVariableHandle
    #endif

  /*============================================================================
     Documentation for: F2GetDictionaryVariableHandle
    ============================================================================
       Get the dictionary variable handle.
  */
  long  FRAMES2API _F2GetDictionaryVariableHandle(int _PID,long _dictionaryHandle,char* _variableName);
    #ifndef BUILD_DLL
      #define F2GetDictionaryVariableHandle _F2GetDictionaryVariableHandle
    #endif

#ifdef __cplusplus
  }
#endif
#endif
