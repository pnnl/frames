
#ifndef DataSet_H
#define DataSet_H
#ifndef BUILD_DLL
  #define FRAMES_API __declspec(dllimport) __stdcall
#else
  #define FRAMES_API __declspec(dllexport) __stdcall
#endif
// API Name: FRAMES Data Set
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
   /* Documentation for: IOOk
    This function checks to see if the status of the Input/Output system. A value of SUCCESS is returned if the system is "Ok".
  
   */

       int FRAMES_API _IOOk (
       int PID);
       #ifndef BUILD_DLL
         #define IOOk _IOOk
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: OpenIO
OpenIO returns the modules Process IDentification (PID). If the returned
pid is less than zero an error occurred. The path\name arguments are to the project file
the PID, path, name and modId are given to the module via the command line. 

For nearly all functions in the API an integer value is returned. The integer value 
represents whether the function called succeeded or not. A value of SUCCESS indicates
that the function did succeed. In error cases a negative value is returned that defines
the error. There are functions available to discovery what the error code means. 

   */

       int FRAMES_API _OpenIO (
       char *  simulationPath, char *  simulationName, char *  iconName);
       #ifndef BUILD_DLL
         #define OpenIO _OpenIO
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: CloseIO
    This functions closes the INI file and cleans all resources associated with SystemIO.dll. If cancel is 0 then the updated datasets are saved otherwise the updated datasets are thrown away.
  
   */

       void FRAMES_API _CloseIO (
       int PID, int cancel);
       #ifndef BUILD_DLL
         #define CloseIO _CloseIO
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: NumIMod
    Get the number of modules this module is consuming
  
   */

       int FRAMES_API _NumIMod (
       int PID, char *  iconName, int * count);
       #ifndef BUILD_DLL
         #define NumIMod _NumIMod
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: NumOMod
    Get the number of modules this module is producing to

   */

       int FRAMES_API _NumOMod (
       int PID, char *  iconName, int * count);
       #ifndef BUILD_DLL
         #define NumOMod _NumOMod
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: NumIModSet
    Get the number of input datasets from _modId.
  
   */

       int FRAMES_API _NumIModSet (
       int PID, char *  iconName, char *  fromIconName, int * count);
       #ifndef BUILD_DLL
         #define NumIModSet _NumIModSet
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: NumOModSet
    Get the number of output datasets from _modId.
  
   */

       int FRAMES_API _NumOModSet (
       int PID, char *  iconName, char *  toIconName, int * count);
       #ifndef BUILD_DLL
         #define NumOModSet _NumOModSet
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: NumIDicDataSet
    Get the number of datasets related to this input dictionary.
  
   */

       int FRAMES_API _NumIDicDataSet (
       int PID, char *  iconName, char *  dictionaryName, int * count);
       #ifndef BUILD_DLL
         #define NumIDicDataSet _NumIDicDataSet
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: NumODicDataSet
    Get the number of datasets related to this output dictionary.
  
   */

       int FRAMES_API _NumODicDataSet (
       int PID, char *  iconName, char *  dictionaryName, int * count);
       #ifndef BUILD_DLL
         #define NumODicDataSet _NumODicDataSet
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetIDicDataSet
    Get the input dataset name at _idx returned via _set.
  
   */

       int FRAMES_API _GetIDicDataSet (
       int PID, char *  iconName, char *  dictionaryName, int index, char *  inputDataset);
       #ifndef BUILD_DLL
         #define GetIDicDataSet _GetIDicDataSet
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetODicDataSet
    Get the output dataset name at _idx returned via _set.
  
   */

       int FRAMES_API _GetODicDataSet (
       int PID, char *  iconName, char *  dictionaryName, int index, char *  outputDataset);
       #ifndef BUILD_DLL
         #define GetODicDataSet _GetODicDataSet
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetIModId
    Get an input modules ID at _idx returned via _modId, _idx from 1 to _NumIMod.
  
   */

       int FRAMES_API _GetIModId (
       int PID, char *  iconName, int index, char *  inputModuleId);
       #ifndef BUILD_DLL
         #define GetIModId _GetIModId
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetOModId
    Get an output modules ID at _idx returned via _modId, _idx from 1 to _NumOMod.
  
   */

       int FRAMES_API _GetOModId (
       int PID, char *  iconName, int index, char *  outputModuleId);
       #ifndef BUILD_DLL
         #define GetOModId _GetOModId
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetIDictionary
    Get an input dictionary name at _idx returned via _dic.
  
   */

       int FRAMES_API _GetIDictionary (
       int PID, char *  iconName, char *  fromIconName, int index, char *  inputDictionary);
       #ifndef BUILD_DLL
         #define GetIDictionary _GetIDictionary
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetODictionary
    Get an output dictionary name at _idx returned via _dic.
  
   */

       int FRAMES_API _GetODictionary (
       int PID, char *  iconName, char *  toIconName, int index, char *  outputDictionary);
       #ifndef BUILD_DLL
         #define GetODictionary _GetODictionary
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetDicId
    Gets dictionary for specified dataset.
  
   */

       int FRAMES_API _GetDicId (
       int PID, char *  datasetName, char *  dictionary);
       #ifndef BUILD_DLL
         #define GetDicId _GetDicId
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetIDataSet
    Get the input dataset name at _idx returned via _set.
  
   */

       int FRAMES_API _GetIDataSet (
       int PID, char *  iconName, char *  fromIconName, int index, char *  inputDataset);
       #ifndef BUILD_DLL
         #define GetIDataSet _GetIDataSet
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetODataSet
    Get the output dataset name at _idx returned via _set.
  
   */

       int FRAMES_API _GetODataSet (
       int PID, char *  iconName, char *  toIconName, int index, char *  outputDataset);
       #ifndef BUILD_DLL
         #define GetODataSet _GetODataSet
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetIModList
    List of input module names.
  
   */

       int FRAMES_API _GetIModList (
       int PID, char *  iconName, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define GetIModList _GetIModList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetOModList
    List of output module names.
  
   */

       int FRAMES_API _GetOModList (
       int PID, char *  iconName, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define GetOModList _GetOModList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetIDicList
    List of input module dictionaries.
  
   */

       int FRAMES_API _GetIDicList (
       int PID, char *  iconName, char *  fromIconName, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define GetIDicList _GetIDicList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetODicList
    List of output module dictionaries.
  
   */

       int FRAMES_API _GetODicList (
       int PID, char *  iconName, char *  toIconName, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define GetODicList _GetODicList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetISetList
    List of input module dataset names.
  
   */

       int FRAMES_API _GetISetList (
       int PID, char *  iconName, char *  fromIconName, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define GetISetList _GetISetList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetOSetList
    List of output module dataset names.
  
   */

       int FRAMES_API _GetOSetList (
       int PID, char *  iconName, char *  toIconName, char *  delimiter, char *  list);
       #ifndef BUILD_DLL
         #define GetOSetList _GetOSetList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetVarDescription
    Get the variable description.
  
   */

       int FRAMES_API _GetVarDescription (
       int PID, char *  dictionaryName, char *  variableName, char *  description);
       #ifndef BUILD_DLL
         #define GetVarDescription _GetVarDescription
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetVarType
    Get the varible data type. The returned string will be "Integer","String","Real", or "Logical"
  
   */

       int FRAMES_API _GetVarType (
       int PID, char *  dictionaryName, char *  variableName, char *  vtype);
       #ifndef BUILD_DLL
         #define GetVarType _GetVarType
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetVarScalar
    Get the variables scalar flag.
  
   */

       int FRAMES_API _GetVarScalar (
       int PID, char *  dictionaryName, char *  variableName, int * flag);
       #ifndef BUILD_DLL
         #define GetVarScalar _GetVarScalar
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetVarMin
    Get the min value for ints and doubles, length for strings, empty for logicals.
  
   */

       int FRAMES_API _GetVarMin (
       int PID, char *  dictionaryName, char *  variableName, double * min);
       #ifndef BUILD_DLL
         #define GetVarMin _GetVarMin
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetVarMax
    Get the max value for ints and doubles, length for strings, empty for logicals.
  
   */

       int FRAMES_API _GetVarMax (
       int PID, char *  dictionaryName, char *  variableName, double * max);
       #ifndef BUILD_DLL
         #define GetVarMax _GetVarMax
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetVarMeasure
    Get the variable measurement type.
  
   */

       int FRAMES_API _GetVarMeasure (
       int PID, char *  dictionaryName, char *  variableName, char *  measure);
       #ifndef BUILD_DLL
         #define GetVarMeasure _GetVarMeasure
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetVarUnit
    Get the variable unit of measure.
  
   */

       int FRAMES_API _GetVarUnit (
       int PID, char *  dictionaryName, char *  variableName, char *  unit);
       #ifndef BUILD_DLL
         #define GetVarUnit _GetVarUnit
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetVarStochastic
    Get the variables stochastic flag. This flag signifies if the parameter can be modified by an another program and not invalidate any calibration. This allows other programs such and Sensativity/Uncertainty or Parameter estimation programs to modify these parameters.
  
   */

       int FRAMES_API _GetVarStochastic (
       int PID, char *  dictionaryName, char *  variableName, int * flag);
       #ifndef BUILD_DLL
         #define GetVarStochastic _GetVarStochastic
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetVarPreposition
    Get the variables preposition. This is the word that would best be used to write a description of this parameters as an index for another. For example the chemical parameter would use "for" as a preposition to make statements such as Concentration for benzene. The preposition greatly facilitates writing descriptive text associated with results. 
  
   */

       int FRAMES_API _GetVarPreposition (
       int PID, char *  dictionaryName, char *  variableName, char *  preposition);
       #ifndef BUILD_DLL
         #define GetVarPreposition _GetVarPreposition
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetVarPrimaryKey
    Get the variables primarykey flag. Is this parameter a key to information on other parameters. This is used in both databases and in the representation of the data. For example a variable being used to index other variables frequently (like chemical) would indicate that chemical should be a primary key. The typical meaning of primary key from Database design is not incorrect when used here.
  
   */

       int FRAMES_API _GetVarPrimaryKey (
       int PID, char *  dictionaryName, char *  variableName, int * flag);
       #ifndef BUILD_DLL
         #define GetVarPrimaryKey _GetVarPrimaryKey
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetVarDimension
  
   */

       int FRAMES_API _GetVarDimension (
       int PID, char *  dictionaryName, char *  variableName, int * dimension);
       #ifndef BUILD_DLL
         #define GetVarDimension _GetVarDimension
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetVarDimSize
    Gets the size of the specified dimensions. The indices parameters define an array of integers that define the index values that are to be used.
  
   */

       int FRAMES_API _GetVarDimSize (
       int PID, char *  datasetName, char *  variableName, int * indices, int * count);
       #ifndef BUILD_DLL
         #define GetVarDimSize _GetVarDimSize
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: VarLookUp
    Lookup an index into the array. The indices parameters define an array of integers that define the index values that are to be used. The specified string value is searched in the variable for the given index values and the last index value is set. A negative number indicates the element could not be found.
  
   */

       int FRAMES_API _VarLookUp (
       int PID, char *  datasetName, char *  variableName, char *  value, int * indices);
       #ifndef BUILD_DLL
         #define VarLookUp _VarLookUp
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ClearVariable
    Deletes all values associated with a variable.
  
   */

       int FRAMES_API _ClearVariable (
       int PID, char *  datasetName, char *  variableName, int * indices);
       #ifndef BUILD_DLL
         #define ClearVariable _ClearVariable
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetInteger
    Get an Integer from a dataset given the variable name, units and a set of indices.
  
   */

       int FRAMES_API _GetInteger (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int * indices, int * value);
       #ifndef BUILD_DLL
         #define GetInteger _GetInteger
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetFloat
    Get an Float from a dataset given the variable name, units and a set of indices.
  
   */

       int FRAMES_API _GetFloat (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int * indices, double * value);
       #ifndef BUILD_DLL
         #define GetFloat _GetFloat
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetLogical
    Get an Boolean(Logical) from a dataset given the variable name, units and a set of indices. Booleans are treated as integers with either a value of 1 or 0.
  
   */

       int FRAMES_API _GetLogical (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int * indices, int * value);
       #ifndef BUILD_DLL
         #define GetLogical _GetLogical
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetString
    Get an String(Character(*)) from a dataset given the variable name, units and a set of indices.
  
   */

       int FRAMES_API _GetString (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int * indices, char *  value);
       #ifndef BUILD_DLL
         #define GetString _GetString
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: PutInteger
    Put an Integer in a dataset given the variable name, units and a set of indices.
  
   */

       int FRAMES_API _PutInteger (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int * indices, int value);
       #ifndef BUILD_DLL
         #define PutInteger _PutInteger
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: PutFloat
    Put an Floating Point (Real) in a dataset given the variable name, units and a set of indices.
  
   */

       int FRAMES_API _PutFloat (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int * indices, double value);
       #ifndef BUILD_DLL
         #define PutFloat _PutFloat
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: PutLogical
    Put an Boolean(Logical) in a dataset given the variable name, units and a set of indices. Booleans are treated as integers with either a value of 1 or 0.
  
   */

       int FRAMES_API _PutLogical (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int * indices, int value);
       #ifndef BUILD_DLL
         #define PutLogical _PutLogical
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: PutString
    Put an String(Character(*)) in a dataset given the variable name, units and a set of indices.
  
   */

       int FRAMES_API _PutString (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int * indices, char *  value);
       #ifndef BUILD_DLL
         #define PutString _PutString
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadInt
    Read an Integer in a dataset given the variable name, units. The return value is the integer.
  
   */

       int FRAMES_API _ReadInt (
       int PID, char *  datasetName, char *  variableName, char *  unitName);
       #ifndef BUILD_DLL
         #define ReadInt _ReadInt
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadInt1
    Read an Integer in a dataset given the variable name, units. The return value is the integer.
  
   */

       int FRAMES_API _ReadInt1 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1);
       #ifndef BUILD_DLL
         #define ReadInt1 _ReadInt1
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadInt2
    Read an Integer in a dataset given the variable name, units. The return value is the integer.
  
   */

       int FRAMES_API _ReadInt2 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2);
       #ifndef BUILD_DLL
         #define ReadInt2 _ReadInt2
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadInt3
    Read an Integer in a dataset given the variable name, units. The return value is the integer.
  
   */

       int FRAMES_API _ReadInt3 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3);
       #ifndef BUILD_DLL
         #define ReadInt3 _ReadInt3
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadInt4
    Read an Integer in a dataset given the variable name, units. The return value is the integer.
  
   */

       int FRAMES_API _ReadInt4 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4);
       #ifndef BUILD_DLL
         #define ReadInt4 _ReadInt4
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadInt5
    Read an Integer in a dataset given the variable name, units. The return value is the integer.
  
   */

       int FRAMES_API _ReadInt5 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4, int index5);
       #ifndef BUILD_DLL
         #define ReadInt5 _ReadInt5
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadInt6
    Read an Integer in a dataset given the variable name, units. The return value is the integer.
  
   */

       int FRAMES_API _ReadInt6 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4, int index5, int index6);
       #ifndef BUILD_DLL
         #define ReadInt6 _ReadInt6
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadReal
    Read a real(float) in a dataset given the variable name, units. The return value is the real(float).
  
   */

       double FRAMES_API _ReadReal (
       int PID, char *  datasetName, char *  variableName, char *  unitName);
       #ifndef BUILD_DLL
         #define ReadReal _ReadReal
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadReal1
    Read a real(float) in a dataset given the variable name, units. The return value is the real(float).
  
   */

       double FRAMES_API _ReadReal1 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1);
       #ifndef BUILD_DLL
         #define ReadReal1 _ReadReal1
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadReal2
    Read a real(float) in a dataset given the variable name, units. The return value is the real(float).
  
   */

       double FRAMES_API _ReadReal2 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2);
       #ifndef BUILD_DLL
         #define ReadReal2 _ReadReal2
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadReal3
    Read a real(float) in a dataset given the variable name, units. The return value is the real(float).
  
   */

       double FRAMES_API _ReadReal3 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3);
       #ifndef BUILD_DLL
         #define ReadReal3 _ReadReal3
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadReal4
    Read a real(float) in a dataset given the variable name, units. The return value is the real(float).
  
   */

       double FRAMES_API _ReadReal4 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4);
       #ifndef BUILD_DLL
         #define ReadReal4 _ReadReal4
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadReal5
    Read a real(float) in a dataset given the variable name, units. The return value is the real(float).
  
   */

       double FRAMES_API _ReadReal5 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4, int index5);
       #ifndef BUILD_DLL
         #define ReadReal5 _ReadReal5
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadReal6
    Read a real(float) in a dataset given the variable name, units. The return value is the real(float).
  
   */

       double FRAMES_API _ReadReal6 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4, int index5, int index6);
       #ifndef BUILD_DLL
         #define ReadReal6 _ReadReal6
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadLog
    Read an Logical in a dataset given the variable name, units. The return value is the integer.
  
   */

       int FRAMES_API _ReadLog (
       int PID, char *  datasetName, char *  variableName, char *  unitName);
       #ifndef BUILD_DLL
         #define ReadLog _ReadLog
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadLog1
    Read an Logical in a dataset given the variable name, units. The return value is the integer.
  
   */

       int FRAMES_API _ReadLog1 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1);
       #ifndef BUILD_DLL
         #define ReadLog1 _ReadLog1
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadLog2
    Read an Logical in a dataset given the variable name, units. The return value is the integer.
  
   */

       int FRAMES_API _ReadLog2 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2);
       #ifndef BUILD_DLL
         #define ReadLog2 _ReadLog2
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadLog3
    Read an Logical in a dataset given the variable name, units. The return value is the integer.
  
   */

       int FRAMES_API _ReadLog3 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3);
       #ifndef BUILD_DLL
         #define ReadLog3 _ReadLog3
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadLog4
    Read an Logical in a dataset given the variable name, units. The return value is the integer.
  
   */

       int FRAMES_API _ReadLog4 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4);
       #ifndef BUILD_DLL
         #define ReadLog4 _ReadLog4
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadLog5
    Read an Logical in a dataset given the variable name, units. The return value is the integer.
  
   */

       int FRAMES_API _ReadLog5 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4, int index5);
       #ifndef BUILD_DLL
         #define ReadLog5 _ReadLog5
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadLog6
    Read an Logical in a dataset given the variable name, units. The return value is the integer.
  
   */

       int FRAMES_API _ReadLog6 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4, int index5, int index6);
       #ifndef BUILD_DLL
         #define ReadLog6 _ReadLog6
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadString
    The space for strings (char *) is expected to be managed by the calling program. If dynamic allocation is used the calling program is responsible for deallocating that space. Read a string in a dataset given the variable name, units. The return value is the string.
  
   */

       void FRAMES_API _ReadString (
       int PID, char *  datasetName, char *  variableName, char *  unitName, char *  value);
       #ifndef BUILD_DLL
         #define ReadString _ReadString
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadString1
    Read a string in a dataset given the variable name, units. The return value is the string.
  
   */

       void FRAMES_API _ReadString1 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, char *  value);
       #ifndef BUILD_DLL
         #define ReadString1 _ReadString1
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadString2
    Read a string in a dataset given the variable name, units. The return value is the string.
  
   */

       void FRAMES_API _ReadString2 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, char *  value);
       #ifndef BUILD_DLL
         #define ReadString2 _ReadString2
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadString3
    Read a string in a dataset given the variable name, units. The return value is the string.
  
   */

       void FRAMES_API _ReadString3 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, char *  value);
       #ifndef BUILD_DLL
         #define ReadString3 _ReadString3
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadString4
    Read a string in a dataset given the variable name, units. The return value is the string.
  
   */

       void FRAMES_API _ReadString4 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4, char *  value);
       #ifndef BUILD_DLL
         #define ReadString4 _ReadString4
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadString5
    Read a string in a dataset given the variable name, units. The return value is the string.
  
   */

       void FRAMES_API _ReadString5 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4, int index5, char *  value);
       #ifndef BUILD_DLL
         #define ReadString5 _ReadString5
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ReadString6
    Read a string in a dataset given the variable name, units. The return value is the string.
  
   */

       void FRAMES_API _ReadString6 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4, int index5, int index6, char *  value);
       #ifndef BUILD_DLL
         #define ReadString6 _ReadString6
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteInt
    Write an Integer in a dataset given the variable name, units.
  
   */

       void FRAMES_API _WriteInt (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int value);
       #ifndef BUILD_DLL
         #define WriteInt _WriteInt
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteInt1
    Write an Integer in a dataset given the variable name, units.
  
   */

       void FRAMES_API _WriteInt1 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int value);
       #ifndef BUILD_DLL
         #define WriteInt1 _WriteInt1
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteInt2
    Write an Integer in a dataset given the variable name, units.
  
   */

       void FRAMES_API _WriteInt2 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int value);
       #ifndef BUILD_DLL
         #define WriteInt2 _WriteInt2
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteInt3
    Write an Integer in a dataset given the variable name, units.
  
   */

       void FRAMES_API _WriteInt3 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int value);
       #ifndef BUILD_DLL
         #define WriteInt3 _WriteInt3
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteInt4
    Write an Integer in a dataset given the variable name, units. 
  
   */

       void FRAMES_API _WriteInt4 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4, int value);
       #ifndef BUILD_DLL
         #define WriteInt4 _WriteInt4
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteInt5
    Write an Integer in a dataset given the variable name, units. 
  
   */

       void FRAMES_API _WriteInt5 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4, int index5, int value);
       #ifndef BUILD_DLL
         #define WriteInt5 _WriteInt5
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteInt6
    Write an Integer in a dataset given the variable name, units. The return value is the integer.
  
   */

       void FRAMES_API _WriteInt6 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4, int index5, int index6, int value);
       #ifndef BUILD_DLL
         #define WriteInt6 _WriteInt6
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteReal
    Write a real(float) to a dataset given the variable name, units.
  
   */

       void FRAMES_API _WriteReal (
       int PID, char *  datasetName, char *  variableName, char *  unitName, double value);
       #ifndef BUILD_DLL
         #define WriteReal _WriteReal
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteReal1
    Write a real(float) to a dataset given the variable name, units.
  
   */

       void FRAMES_API _WriteReal1 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, double value);
       #ifndef BUILD_DLL
         #define WriteReal1 _WriteReal1
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteReal2
    Write a real(float) to a dataset given the variable name, units.
  
   */

       void FRAMES_API _WriteReal2 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, double value);
       #ifndef BUILD_DLL
         #define WriteReal2 _WriteReal2
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteReal3
    Write a real(float) to a dataset given the variable name, units.
  
   */

       void FRAMES_API _WriteReal3 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, double value);
       #ifndef BUILD_DLL
         #define WriteReal3 _WriteReal3
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteReal4
    Write a real(float) to a dataset given the variable name, units.
  
   */

       void FRAMES_API _WriteReal4 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4, double value);
       #ifndef BUILD_DLL
         #define WriteReal4 _WriteReal4
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteReal5
    Write a real(float) to a dataset given the variable name, units.
  
   */

       void FRAMES_API _WriteReal5 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4, int index5, double value);
       #ifndef BUILD_DLL
         #define WriteReal5 _WriteReal5
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteReal6
    Write a real(float) to a dataset given the variable name, units.
  
   */

       void FRAMES_API _WriteReal6 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4, int index5, int index6, double value);
       #ifndef BUILD_DLL
         #define WriteReal6 _WriteReal6
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteLog
    Write an Logical in a dataset given the variable name, units. 
  
   */

       void FRAMES_API _WriteLog (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int value);
       #ifndef BUILD_DLL
         #define WriteLog _WriteLog
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteLog1
    Write an Logical in a dataset given the variable name, units. 
  
   */

       void FRAMES_API _WriteLog1 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int value);
       #ifndef BUILD_DLL
         #define WriteLog1 _WriteLog1
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteLog2
    Write an Logical in a dataset given the variable name, units. 
  
   */

       void FRAMES_API _WriteLog2 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int value);
       #ifndef BUILD_DLL
         #define WriteLog2 _WriteLog2
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteLog3
    Write an Logical in a dataset given the variable name, units. 
  
   */

       void FRAMES_API _WriteLog3 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int value);
       #ifndef BUILD_DLL
         #define WriteLog3 _WriteLog3
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteLog4
    Write an Logical in a dataset given the variable name, units. 
  
   */

       void FRAMES_API _WriteLog4 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4, int value);
       #ifndef BUILD_DLL
         #define WriteLog4 _WriteLog4
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteLog5
    Write an Logical in a dataset given the variable name, units. 
  
   */

       void FRAMES_API _WriteLog5 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4, int index5, int value);
       #ifndef BUILD_DLL
         #define WriteLog5 _WriteLog5
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteLog6
    Write an Logical in a dataset given the variable name, units. 
  
   */

       void FRAMES_API _WriteLog6 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4, int index5, int index6, int value);
       #ifndef BUILD_DLL
         #define WriteLog6 _WriteLog6
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteString
    Write a string to a dataset given the variable name, units.
  
   */

       void FRAMES_API _WriteString (
       int PID, char *  datasetName, char *  variableName, char *  unitName, char *  value);
       #ifndef BUILD_DLL
         #define WriteString _WriteString
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteString1
    Write a string to a dataset given the variable name, units.
  
   */

       void FRAMES_API _WriteString1 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, char *  value);
       #ifndef BUILD_DLL
         #define WriteString1 _WriteString1
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteString2
    Write a string to a dataset given the variable name, units.
  
   */

       void FRAMES_API _WriteString2 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, char *  value);
       #ifndef BUILD_DLL
         #define WriteString2 _WriteString2
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteString3
    Write a string to a dataset given the variable name, units.
  
   */

       void FRAMES_API _WriteString3 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, char *  value);
       #ifndef BUILD_DLL
         #define WriteString3 _WriteString3
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteString4
    Write a string to a dataset given the variable name, units.
  
   */

       void FRAMES_API _WriteString4 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4, char *  value);
       #ifndef BUILD_DLL
         #define WriteString4 _WriteString4
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteString5
    Write a string to a dataset given the variable name, units.
  
   */

       void FRAMES_API _WriteString5 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4, int index5, char *  value);
       #ifndef BUILD_DLL
         #define WriteString5 _WriteString5
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: WriteString6
    Write a string to a dataset given the variable name, units.
  
   */

       void FRAMES_API _WriteString6 (
       int PID, char *  datasetName, char *  variableName, char *  unitName, int index1, int index2, int index3, int index4, int index5, int index6, char *  value);
       #ifndef BUILD_DLL
         #define WriteString6 _WriteString6
       #endif

  
  #ifdef __cplusplus
  }
  #endif
#endif
