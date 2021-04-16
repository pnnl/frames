
#ifndef Conversion_H
#define Conversion_H
#ifndef BUILD_DLL
  #define FRAMES_API __declspec(dllimport) __stdcall
#else
  #define FRAMES_API __declspec(dllexport) __stdcall
#endif
// API Name: FRAMES Conversion
  #ifdef __cplusplus
    extern "C" {
  #else
    #define bool int
  #endif

   /*
   This is the api that facilitates converting numbers from one unit to another.
   It does not use dimensional analysis because it is possible to have two units
   that would appear to cancel but do not. (i.e. g/g)  Instead a list of 
   measures and associated units are kept by this API. When a number is
   converted the associated units in the same measure are used for the conversion.

   */
     
   /*-------------------------------------------------------------------------*/
   /* Documentation for: NumMeasures
    Returns the number of measures currently in the system.
  
   */

       int FRAMES_API _NumMeasures (
       );
       #ifndef BUILD_DLL
         #define NumMeasures _NumMeasures
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetMeasure
    Get the name of a measure from a particular index.
  
   */

       void FRAMES_API _GetMeasure (
       int index, char *  Measure);
       #ifndef BUILD_DLL
         #define GetMeasure _GetMeasure
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetMeasureIdx
    Get the index of measure given its name. If the measure is not found a negative value is returned.
  
   */

       int FRAMES_API _GetMeasureIdx (
       char *  Measure);
       #ifndef BUILD_DLL
         #define GetMeasureIdx _GetMeasureIdx
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetMeasureList
    Get a single string that contains all the measure names separated by a delimiter. If there are no measures a negative value is returned.
  
   */

       int FRAMES_API _GetMeasureList (
       char *  Separator, char *  MeasureList);
       #ifndef BUILD_DLL
         #define GetMeasureList _GetMeasureList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: NumUnits
    Get the number of units for a given measure index. If the measure index is not valid a negative value is returned.
  
   */

       int FRAMES_API _NumUnits (
       int index);
       #ifndef BUILD_DLL
         #define NumUnits _NumUnits
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: sNumUnits
    Get the number of units for a given measure name. If the measure name is not valid a negative value is returned.
  
   */

       int FRAMES_API _sNumUnits (
       char *  index);
       #ifndef BUILD_DLL
         #define sNumUnits _sNumUnits
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetUnit
    Get the unit associated with a measure and unit index. 
  
   */

       void FRAMES_API _GetUnit (
       int MeasureIdx, int UnitIdx, char *  Unit);
       #ifndef BUILD_DLL
         #define GetUnit _GetUnit
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetUnitAbbr
    Get the base unit abbreviation for a unit from a measure. A negative value will be returned if the unit or measure cannot be found. This
    typically occurs because the unit or measure is not in the list. 
  
   */

       void FRAMES_API _GetUnitAbbr (
       int MeasureIdx, int UnitIdx, char *  Unit);
       #ifndef BUILD_DLL
         #define GetUnitAbbr _GetUnitAbbr
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: sGetUnit
    Get the unit associated with a measure and unit index. 
  
   */

       void FRAMES_API _sGetUnit (
       char *  Measure, int UnitIdx, char *  Unit);
       #ifndef BUILD_DLL
         #define sGetUnit _sGetUnit
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: sGetUnitAbbr
    Get the base unit abbreviation for a unit from a measure. A negative value will be returned if the unit or measure cannot be found. This
    typically occurs because the unit or measure is not in the list. 
  
   */

       void FRAMES_API _sGetUnitAbbr (
       char *  Measure, int UnitIdx, char *  Unit);
       #ifndef BUILD_DLL
         #define sGetUnitAbbr _sGetUnitAbbr
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetUnitIdx
    Get the unit index associated with a measure index and unit name. A negative value is returned if the MeasureIdx is 
    invalid or the Unit is not found in the measure.
  
   */

       int FRAMES_API _GetUnitIdx (
       int MeasureIdx, char *  Unit);
       #ifndef BUILD_DLL
         #define GetUnitIdx _GetUnitIdx
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: sGetUnitIdx
    Get the unit index associated with a measure index and unit name. A negative value is returned if the Measure is 
    invalid or the Unit is not found in the measure.
  
   */

       int FRAMES_API _sGetUnitIdx (
       char *  Measure, char *  Unit);
       #ifndef BUILD_DLL
         #define sGetUnitIdx _sGetUnitIdx
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetUnitAbbrIdx
    Get the unit index associated with a measure index and unit Abbr. A negative value is returned if the MeasureIdx is 
    invalid or the Abbr is not found in the measure.
  
   */

       int FRAMES_API _GetUnitAbbrIdx (
       int MeasureIdx, char *  Abbr);
       #ifndef BUILD_DLL
         #define GetUnitAbbrIdx _GetUnitAbbrIdx
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: sGetUnitAbbrIdx
    Get the unit index associated with a measure index and unit abbr. A negative value is returned if the Measure is 
    invalid or the Abbr is not found in the measure.
  
   */

       int FRAMES_API _sGetUnitAbbrIdx (
       char *  Measure, char *  Abbr);
       #ifndef BUILD_DLL
         #define sGetUnitAbbrIdx _sGetUnitAbbrIdx
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetUnitList
    Get a single string that contains all the units in a measure separated by a delimiter. A negative value is returned if the MeasureIdx is 
    invalid or the Unit is not found in the measure.
  
   */

       int FRAMES_API _GetUnitList (
       int MeasureIdx, char *  Separator, char *  UnitList);
       #ifndef BUILD_DLL
         #define GetUnitList _GetUnitList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: sGetUnitList
    Get a single string that contains all the units in a measure separated by a delimiter. A negative value is returned if the MeasureIdx is 
    invalid or the Unit is not found in the measure.
  
   */

       int FRAMES_API _sGetUnitList (
       char *  Measure, char *  Separator, char *  UnitList);
       #ifndef BUILD_DLL
         #define sGetUnitList _sGetUnitList
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetConversion
    Convert a number from one unit in a measure to another unit.
  
   */

       double FRAMES_API _GetConversion (
       int MeasureIdx, double InitValue, char *  UnitFrom, char *  UnitTo);
       #ifndef BUILD_DLL
         #define GetConversion _GetConversion
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: sGetConversion
    Convert a number from one unit in a measure to another unit.
  
   */

       double FRAMES_API _sGetConversion (
       char *  Measure, double InitValue, char *  UnitFrom, char *  UnitTo);
       #ifndef BUILD_DLL
         #define sGetConversion _sGetConversion
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddMeasure
    Add a new measure to the set of measures. A negative value will be returned if the measure cannot be added. This
    typically occurs because the measure is already in the list. This will add the measure for all FRAMES components.
  
   */

       int FRAMES_API _AddMeasure (
       char *  Measure, char *  BaseUnit, char *  BaseUnitAbbr);
       #ifndef BUILD_DLL
         #define AddMeasure _AddMeasure
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DelMeasure
    Delete a measure frpm the set of measures. A negative value will be returned if the measure cannot be deleted. This
    typically occurs because the measure is not in the list. This will delete the measure for all FRAMES components.
  
   */

       int FRAMES_API _DelMeasure (
       char *  Measure);
       #ifndef BUILD_DLL
         #define DelMeasure _DelMeasure
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: NewUnit
    Create a new Unit within a measure. A negative value will be returned if the unit cannot be created. This
    typically occurs because the unit already exists in the list. This will create the measure for all FRAMES components.
  
   */

       int FRAMES_API _NewUnit (
       char *  Measure, char *  Unit, char *  UnitAbbr);
       #ifndef BUILD_DLL
         #define NewUnit _NewUnit
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddUnit
    Add a new Unit within a measure. A negative value will be returned if the unit cannot be added. This
    typically occurs because the unit already exists in the list. This will create the measure for all FRAMES components. The slope
    and the offset (intercept) for the line conversion can be given. This makes this API support conversion for measures
    like temperature where a multiplier (Factor) and an offset (intercept) are needed to define the conversion.
  
   */

       int FRAMES_API _AddUnit (
       char *  Measure, char *  Unit, char *  UnitAbbr, double Factor, double Offset);
       #ifndef BUILD_DLL
         #define AddUnit _AddUnit
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DelUnit
    Delete a unit from a measure. A negative value will be returned if the unit cannot be deleted. This
    typically occurs because the unit is not in the list. This will delete the unit for all FRAMES components.
  
   */

       int FRAMES_API _DelUnit (
       char *  Measure, char *  Unit);
       #ifndef BUILD_DLL
         #define DelUnit _DelUnit
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetUnitFactor
    Get the unit factor for a unit from a measure. A negative value will be returned if the unit cannot be found. This
    typically occurs because the unit is not in the list. 
  
   */

       int FRAMES_API _GetUnitFactor (
       char *  Measure, char *  Unit, double * Factor);
       #ifndef BUILD_DLL
         #define GetUnitFactor _GetUnitFactor
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetUnitOffset
    Get the unit factor for a unit from a measure. A negative value will be returned if the unit cannot be found. This
    typically occurs because the unit is not in the list. 
  
   */

       int FRAMES_API _GetUnitOffset (
       char *  Measure, char *  Unit, double * Offset);
       #ifndef BUILD_DLL
         #define GetUnitOffset _GetUnitOffset
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetUnitFactor
    Set the unit factor for a unit from a measure. A negative value will be returned if the unit cannot be found. This
    typically occurs because the unit is not in the list. This will change the offset for all FRAMES components.
  
   */

       int FRAMES_API _SetUnitFactor (
       char *  Measure, char *  Unit, double Factor);
       #ifndef BUILD_DLL
         #define SetUnitFactor _SetUnitFactor
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SetUnitOffset
    Set the unit factor for a unit from a measure. A negative value will be returned if the unit cannot be found. This
    typically occurs because the unit is not in the list. This will change the offset for all FRAMES components.
  
   */

       int FRAMES_API _SetUnitOffset (
       char *  Measure, char *  Unit, double Offset);
       #ifndef BUILD_DLL
         #define SetUnitOffset _SetUnitOffset
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetBaseUnit
    Get the base unit factor for a unit from a measure. A negative value will be returned if the unit or measure cannot be found. This
    typically occurs because the unit or measure is not in the list. 
  
   */

       int FRAMES_API _GetBaseUnit (
       char *  Measure, char *  Unit);
       #ifndef BUILD_DLL
         #define GetBaseUnit _GetBaseUnit
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetBaseUnitAbbr
    Get the base unit abbreviation for a unit from a measure. A negative value will be returned if the unit or measure cannot be found. This
    typically occurs because the unit or measure is not in the list. 
  
   */

       int FRAMES_API _GetBaseUnitAbbr (
       char *  Measure, char *  Abbr);
       #ifndef BUILD_DLL
         #define GetBaseUnitAbbr _GetBaseUnitAbbr
       #endif

  
  #ifdef __cplusplus
  }
  #endif
#endif
