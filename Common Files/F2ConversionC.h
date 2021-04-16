#ifndef FRAMES2API_F2Conversion_H
#define FRAMES2API_F2Conversion_H

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

  // API Name: FRAMES Conversion API
  /* 
       This is the api that facilitates converting numbers from one unit to 
       another.    It does not use dimensional analysis because it is 
       possible to have two units    that would appear to cancel but do not. 
       (i.e. g/g)  Instead a list of     measures and associated units are 
       kept by this API. When a number is     converted the associated units 
       in the same measure are used for the conversion.       Function 
       Naming       The API functions have been named to aid in the 
       understanding of what each       function manipulates. Each function 
       name has a prefix that describes what       the function manipulates. 
       The list below describes each prefix used.          Measure - A 
       single measure         Measures - The list of measures         Unit - 
       A single unit     Deprecated APIs          All APIs with a "F2" 
       prefix are deprecated, unlisted, and will be removed      from the 
       product at the next release. The "F2" APIs were added after the       
       initial release of FramesV2, but as a result of design changes have 
       since      been superceded by a more object oriented naming 
       convention. It is       paramount for your ability to easily move 
       forward to future releases of       FramesV2 that obsolete APIs not 
       be referenced.
  */
  /*============================================================================
     Documentation for: F2MeasureCount
    ============================================================================
       Returns the number of measures currently in the system.
  */
  int  FRAMES2API _F2MeasureCount();
    #ifndef BUILD_DLL
      #define F2MeasureCount _F2MeasureCount
    #endif

  /*============================================================================
     Documentation for: F2GetMeasure
    ============================================================================
       Get the name of a measure from a particular index. This function 
       returns the measure name at the given index
  */
  int  FRAMES2API _F2GetMeasure(long _measureHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetMeasure _F2GetMeasure
    #endif

  /*============================================================================
     Documentation for: F2GetMeasureHandle
    ============================================================================
       Get the index of measure given its name. If the measure is not found 
       a negative value is returned.
  */
  long  FRAMES2API _F2GetMeasureHandle(char* _measureName);
    #ifndef BUILD_DLL
      #define F2GetMeasureHandle _F2GetMeasureHandle
    #endif

  /*============================================================================
     Documentation for: F2UnitsCount
    ============================================================================
       Get the number of units for a given measure index. If the measure 
       index is not valid a negative value is returned.
  */
  int  FRAMES2API _F2UnitsCount(long _measureHandle);
    #ifndef BUILD_DLL
      #define F2UnitsCount _F2UnitsCount
    #endif

  /*============================================================================
     Documentation for: F2GetUnit
    ============================================================================
       Get the unit associated with a measure and unit index.
  */
  int  FRAMES2API _F2GetUnit(long _measureHandle,long _unitHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetUnit _F2GetUnit
    #endif

  /*============================================================================
     Documentation for: F2GetUnitHandle
    ============================================================================
       Get the unit index associated with a measure and unit index.
  */
  long  FRAMES2API _F2GetUnitHandle(long _measureHandle,char* _unitName);
    #ifndef BUILD_DLL
      #define F2GetUnitHandle _F2GetUnitHandle
    #endif

  /*============================================================================
     Documentation for: F2GetConversion
    ============================================================================
       Convert a number from one unit in a measure to another unit.
  */
  double  FRAMES2API _F2GetConversion(long _measureHandle,double _InitValue,char* _UnitFrom,char* _UnitTo);
    #ifndef BUILD_DLL
      #define F2GetConversion _F2GetConversion
    #endif

  /*============================================================================
     Documentation for: F2AddMeasure
    ============================================================================
       Add a new measure to the set of measures. A negative value will be 
       returned if the measure cannot be added. This typically occurs 
       because the measure is already in the list. This will add the measure 
       for all FRAMES components.
  */
  long  FRAMES2API _F2AddMeasure(char* _measureName,char* _unitName);
    #ifndef BUILD_DLL
      #define F2AddMeasure _F2AddMeasure
    #endif

  /*============================================================================
     Documentation for: F2DeleteMeasure
    ============================================================================
       Delete a measure frpm the set of measures. A negative value will be 
       returned if the measure cannot be deleted. This typically occurs 
       because the measure is not in the list. This will delete the measure 
       for all FRAMES components.
  */
  void  FRAMES2API _F2DeleteMeasure(long _measureHandle);
    #ifndef BUILD_DLL
      #define F2DeleteMeasure _F2DeleteMeasure
    #endif

  /*============================================================================
     Documentation for: F2AddUnit
    ============================================================================
       Create a new Unit within a measure. A negative value will be returned 
       if the unit cannot be created. This typically occurs because the unit 
       already exists in the list. This will create the measure for all 
       FRAMES components.
  */
  long  FRAMES2API _F2AddUnit(long _measureHandle,char* _unitName);
    #ifndef BUILD_DLL
      #define F2AddUnit _F2AddUnit
    #endif

  /*============================================================================
     Documentation for: F2DeleteUnit
    ============================================================================
       Delete a unit from a measure. A negative value will be returned if 
       the unit cannot be deleted. This typically occurs because the unit is 
       not in the list. This will delete the unit for all FRAMES components.
  */
  void  FRAMES2API _F2DeleteUnit(long _measureHandle,long _unitHandle);
    #ifndef BUILD_DLL
      #define F2DeleteUnit _F2DeleteUnit
    #endif

  /*============================================================================
     Documentation for: F2GetUnitFactor
    ============================================================================
       Get the unit factor for a unit from a measure. A negative value will 
       be returned if the unit cannot be found. This typically occurs 
       because the unit is not in the list. Factor for conversion between 
       this unit and the base unit of the measure.
  */
  double  FRAMES2API _F2GetUnitFactor(long _measureHandle,long _unitHandle);
    #ifndef BUILD_DLL
      #define F2GetUnitFactor _F2GetUnitFactor
    #endif

  /*============================================================================
     Documentation for: F2GetUnitOffset
    ============================================================================
       Get the unit factor for a unit from a measure. A negative value will 
       be returned if the unit cannot be found. This typically occurs 
       because the unit is not in the list. Offset of conversion between 
       this unit and the base unit of the measure
  */
  double  FRAMES2API _F2GetUnitOffset(long _measureHandle,long _unitHandle);
    #ifndef BUILD_DLL
      #define F2GetUnitOffset _F2GetUnitOffset
    #endif

  /*============================================================================
     Documentation for: F2SetUnitFactor
    ============================================================================
       Set the unit factor for a unit from a measure. A negative value will 
       be returned if the unit cannot be found. This typically occurs 
       because the unit is not in the list. This will change the offset for 
       all FRAMES components.
  */
  void  FRAMES2API _F2SetUnitFactor(long _measureHandle,long _unitHandle,double _Factor);
    #ifndef BUILD_DLL
      #define F2SetUnitFactor _F2SetUnitFactor
    #endif

  /*============================================================================
     Documentation for: F2SetUnitOffset
    ============================================================================
       Set the unit factor for a unit from a measure. A negative value will 
       be returned if the unit cannot be found. This typically occurs 
       because the unit is not in the list. This will change the offset for 
       all FRAMES components.
  */
  void  FRAMES2API _F2SetUnitOffset(long _measureHandle,long _unitHandle,double _Offset);
    #ifndef BUILD_DLL
      #define F2SetUnitOffset _F2SetUnitOffset
    #endif

  /*============================================================================
     Documentation for: F2GetBaseUnit
    ============================================================================
       Get the base unit factor for a unit from a measure. A negative value 
       will be returned if the unit or measure cannot be found. This 
       typically occurs because the unit or measure is not in the list.
  */
  int  FRAMES2API _F2GetBaseUnit(long _measureHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2GetBaseUnit _F2GetBaseUnit
    #endif

  /*============================================================================
     Documentation for: MeasuresCount
    ============================================================================
       Returns the number of measures currently in the list of measures.
  */
  int  FRAMES2API _MeasuresCount();
    #ifndef BUILD_DLL
      #define MeasuresCount _MeasuresCount
    #endif

  /*============================================================================
     Documentation for: MeasureGetName
    ============================================================================
       Returns the name of a single measure specified by the given handle.
  */
  int  FRAMES2API _MeasureGetName(long _measureHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define MeasureGetName _MeasureGetName
    #endif

  /*============================================================================
     Documentation for: MeasureGetIndex
    ============================================================================
       Returns the index of a single measure.
  */
  int  FRAMES2API _MeasureGetIndex(char* _measureName);
    #ifndef BUILD_DLL
      #define MeasureGetIndex _MeasureGetIndex
    #endif

  /*============================================================================
     Documentation for: MeasureGetHandle
    ============================================================================
       Returns the handle of a single measure.
  */
  long  FRAMES2API _MeasureGetHandle(char* _measureName);
    #ifndef BUILD_DLL
      #define MeasureGetHandle _MeasureGetHandle
    #endif

  /*============================================================================
     Documentation for: MeasureUnitCount
    ============================================================================
       Returns the number of units in a single measure specified by the 
       given measure handle. If the measure handle is not valid, a negative 
       number is returned.
  */
  int  FRAMES2API _MeasureUnitCount(long _measureHandle);
    #ifndef BUILD_DLL
      #define MeasureUnitCount _MeasureUnitCount
    #endif

  /*============================================================================
     Documentation for: UnitGetName
    ============================================================================
       Returns the unit name.
  */
  int  FRAMES2API _UnitGetName(long _measureHandle,long _unitHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define UnitGetName _UnitGetName
    #endif

  /*============================================================================
     Documentation for: UnitGetIndex
    ============================================================================
       Returns the unit index with the specified name.
  */
  int  FRAMES2API _UnitGetIndex(long _measureHandle,char* _unitName);
    #ifndef BUILD_DLL
      #define UnitGetIndex _UnitGetIndex
    #endif

  /*============================================================================
     Documentation for: UnitGetHandle
    ============================================================================
       Returns the unit handle with the specified name.
  */
  long  FRAMES2API _UnitGetHandle(long _measureHandle,char* _unitName);
    #ifndef BUILD_DLL
      #define UnitGetHandle _UnitGetHandle
    #endif

  /*============================================================================
     Documentation for: UnitGetConversion
    ============================================================================
       Returns the result of the conversion. A value is converted from unit 
       in a measure to another unit in the same measure.
  */
  double  FRAMES2API _UnitGetConversion(long _measureHandle,double _InitValue,char* _UnitFrom,char* _UnitTo);
    #ifndef BUILD_DLL
      #define UnitGetConversion _UnitGetConversion
    #endif

  /*============================================================================
     Documentation for: MeasuresAdd
    ============================================================================
       Add a new measure to the set of measures. WARNING: This function does 
       NOT add the base unit into the new  measure's unit list!. A negative 
       value will be returned if the measure cannot be added. This typically 
       occurs because the measure is already in the list. This will add the 
       measure for all FRAMES components.
  */
  int  FRAMES2API _MeasuresAdd(char* _measureName,char* _unitName);
    #ifndef BUILD_DLL
      #define MeasuresAdd _MeasuresAdd
    #endif

  /*============================================================================
     Documentation for: MeasuresDelete
    ============================================================================
       Delete a measure from the set of measures. A negative value will be 
       returned if the measure cannot be deleted. This typically occurs 
       because the measure is not in the list. This will delete the measure 
       for all FRAMES components.
  */
  void  FRAMES2API _MeasuresDelete(long _measureHandle);
    #ifndef BUILD_DLL
      #define MeasuresDelete _MeasuresDelete
    #endif

  /*============================================================================
     Documentation for: MeasureAddUnit
    ============================================================================
       Add a new unit into a measure's unit list. A negative value will be 
       returned if the unit cannot be created. This typically occurs because 
       the unit already exists in the list. This will create the measure for 
       all FRAMES components.
  */
  int  FRAMES2API _MeasureAddUnit(long _measureHandle,char* _unitName);
    #ifndef BUILD_DLL
      #define MeasureAddUnit _MeasureAddUnit
    #endif

  /*============================================================================
     Documentation for: MeasureDeleteUnit
    ============================================================================
       Delete a unit from a measure's unit list. A negative value will be 
       returned if the unit cannot be deleted. This typically occurs because 
       the unit is not in the list. This will delete the unit for all FRAMES 
       components.
  */
  void  FRAMES2API _MeasureDeleteUnit(long _measureHandle,long _unitHandle);
    #ifndef BUILD_DLL
      #define MeasureDeleteUnit _MeasureDeleteUnit
    #endif

  /*============================================================================
     Documentation for: UnitGetFactor
    ============================================================================
       Returns the unit factor for a unit from a measure's unit list. The 
       unit is specified by the unit handle. A negative value will be 
       returned if the unit cannot be found. This typically occurs because 
       the unit is not in the list.
  */
  double  FRAMES2API _UnitGetFactor(long _measureHandle,long _unitHandle);
    #ifndef BUILD_DLL
      #define UnitGetFactor _UnitGetFactor
    #endif

  /*============================================================================
     Documentation for: UnitGetOffset
    ============================================================================
       Returns the unit offset for a unit from a measure's unit list. The 
       unit is specified by the unit handle. A negative value will be 
       returned if the unit cannot be found. This typically occurs because 
       the unit is not in the list.
  */
  double  FRAMES2API _UnitGetOffset(long _measureHandle,long _unitHandle);
    #ifndef BUILD_DLL
      #define UnitGetOffset _UnitGetOffset
    #endif

  /*============================================================================
     Documentation for: UnitSetFactor
    ============================================================================
       Sets the unit factor for a unit from a measure's unit list. The unit 
       is specified by the unit handle. This will change the offset for all 
       FRAMES components.
  */
  void  FRAMES2API _UnitSetFactor(long _measureHandle,long _unitHandle,double _Factor);
    #ifndef BUILD_DLL
      #define UnitSetFactor _UnitSetFactor
    #endif

  /*============================================================================
     Documentation for: UnitSetOffset
    ============================================================================
       Sets the unit offest for a unit from a measure's unit list. The unit 
       is specified by the unit handle. This will change the offset for all 
       FRAMES components.
  */
  void  FRAMES2API _UnitSetOffset(long _measureHandle,long _unitHandle,double _Offset);
    #ifndef BUILD_DLL
      #define UnitSetOffset _UnitSetOffset
    #endif

  /*============================================================================
     Documentation for: MeasureGetBaseUnit
    ============================================================================
       Returns a measure's base unit name. An empty string is returned if 
       the measure cannot be found. This typically occurs because the unit 
       or measure is not in the list.
  */
  int  FRAMES2API _MeasureGetBaseUnit(long _measureHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define MeasureGetBaseUnit _MeasureGetBaseUnit
    #endif

#ifdef __cplusplus
  }
#endif
#endif
