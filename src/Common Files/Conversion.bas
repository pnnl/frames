Attribute VB_Name = "ConversionAPI"
Option Explicit
' API Name: FRAMES Conversion
'-------------------------------------------------------------------------
' Documentation for: NumMeasures
' Returns the number of measures currently in the system.
DECLARE Function NumMeasures LIB "systemio.dll" Alias "__NumMeasures@0" (  )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetMeasure
' Get the name of a measure from a particular index.
DECLARE Sub DLLGetMeasure LIB "systemio.dll" Alias "__GetMeasure@8" (  ByVal index as long,  ByVal Measure as string ) 

'-------------------------------------------------------------------------
' Documentation for: GetMeasureIdx
' Get the index of measure given its name. If the measure is not found a negative value is returned.
DECLARE Function GetMeasureIdx LIB "systemio.dll" Alias "__GetMeasureIdx@4" (  ByVal Measure as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetMeasureList
' Get a single string that contains all the measure names separated by a delimiter. If there are no measures a negative value is returned.
DECLARE Function DLLGetMeasureList LIB "systemio.dll" Alias "__GetMeasureList@8" (  ByVal Separator as string,  ByVal MeasureList as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: NumUnits
' Get the number of units for a given measure index. If the measure index is not valid a negative value is returned.
DECLARE Function NumUnits LIB "systemio.dll" Alias "__NumUnits@4" (  ByVal index as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: sNumUnits
' Get the number of units for a given measure name. If the measure name is not valid a negative value is returned.
DECLARE Function sNumUnits LIB "systemio.dll" Alias "__sNumUnits@4" (  ByVal index as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetUnit
' Get the unit associated with a measure and unit index.
DECLARE Sub DLLGetUnit LIB "systemio.dll" Alias "__GetUnit@12" (  ByVal MeasureIdx as long,  ByVal UnitIdx as long,  ByVal Unit as string ) 

'-------------------------------------------------------------------------
' Documentation for: GetUnitAbbr
' Get the base unit abbreviation for a unit from a measure. A negative value will be returned if the unit or measure cannot be found. This typically occurs because the unit or measure is not in the list.
DECLARE Sub DLLGetUnitAbbr LIB "systemio.dll" Alias "__GetUnitAbbr@12" (  ByVal MeasureIdx as long,  ByVal UnitIdx as long,  ByVal Unit as string ) 

'-------------------------------------------------------------------------
' Documentation for: sGetUnit
' Get the unit associated with a measure and unit index.
DECLARE Sub DLLsGetUnit LIB "systemio.dll" Alias "__sGetUnit@12" (  ByVal Measure as string,  ByVal UnitIdx as long,  ByVal Unit as string ) 

'-------------------------------------------------------------------------
' Documentation for: sGetUnitAbbr
' Get the base unit abbreviation for a unit from a measure. A negative value will be returned if the unit or measure cannot be found. This typically occurs because the unit or measure is not in the list.
DECLARE Sub DLLsGetUnitAbbr LIB "systemio.dll" Alias "__sGetUnitAbbr@12" (  ByVal Measure as string,  ByVal UnitIdx as long,  ByVal Unit as string ) 

'-------------------------------------------------------------------------
' Documentation for: GetUnitIdx
' Get the unit index associated with a measure index and unit name. A negative value is returned if the MeasureIdx is invalid or the Unit is not found in the measure.
DECLARE Function GetUnitIdx LIB "systemio.dll" Alias "__GetUnitIdx@8" (  ByVal MeasureIdx as long,  ByVal Unit as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: sGetUnitIdx
' Get the unit index associated with a measure index and unit name. A negative value is returned if the Measure is invalid or the Unit is not found in the measure.
DECLARE Function sGetUnitIdx LIB "systemio.dll" Alias "__sGetUnitIdx@8" (  ByVal Measure as string,  ByVal Unit as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetUnitAbbrIdx
' Get the unit index associated with a measure index and unit Abbr. A negative value is returned if the MeasureIdx is invalid or the Abbr is not found in the measure.
DECLARE Function GetUnitAbbrIdx LIB "systemio.dll" Alias "__GetUnitAbbrIdx@8" (  ByVal MeasureIdx as long,  ByVal Abbr as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: sGetUnitAbbrIdx
' Get the unit index associated with a measure index and unit abbr. A negative value is returned if the Measure is invalid or the Abbr is not found in the measure.
DECLARE Function sGetUnitAbbrIdx LIB "systemio.dll" Alias "__sGetUnitAbbrIdx@8" (  ByVal Measure as string,  ByVal Abbr as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetUnitList
' Get a single string that contains all the units in a measure separated by a delimiter. A negative value is returned if the MeasureIdx is invalid or the Unit is not found in the measure.
DECLARE Function DLLGetUnitList LIB "systemio.dll" Alias "__GetUnitList@12" (  ByVal MeasureIdx as long,  ByVal Separator as string,  ByVal UnitList as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: sGetUnitList
' Get a single string that contains all the units in a measure separated by a delimiter. A negative value is returned if the MeasureIdx is invalid or the Unit is not found in the measure.
DECLARE Function DLLsGetUnitList LIB "systemio.dll" Alias "__sGetUnitList@12" (  ByVal Measure as string,  ByVal Separator as string,  ByVal UnitList as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetConversion
' Convert a number from one unit in a measure to another unit.
DECLARE Function GetConversion LIB "systemio.dll" Alias "__GetConversion@20" (  ByVal MeasureIdx as long,  ByVal InitValue as double,  ByVal UnitFrom as string,  ByVal UnitTo as string )  as double 

'-------------------------------------------------------------------------
' Documentation for: sGetConversion
' Convert a number from one unit in a measure to another unit.
DECLARE Function sGetConversion LIB "systemio.dll" Alias "__sGetConversion@20" (  ByVal Measure as string,  ByVal InitValue as double,  ByVal UnitFrom as string,  ByVal UnitTo as string )  as double 

'-------------------------------------------------------------------------
' Documentation for: AddMeasure
' Add a new measure to the set of measures. A negative value will be returned if the measure cannot be added. This typically occurs because the measure is already in the list. This will add the measure for all FRAMES components.
DECLARE Function AddMeasure LIB "systemio.dll" Alias "__AddMeasure@12" (  ByVal Measure as string,  ByVal BaseUnit as string,  ByVal BaseUnitAbbr as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DelMeasure
' Delete a measure frpm the set of measures. A negative value will be returned if the measure cannot be deleted. This typically occurs because the measure is not in the list. This will delete the measure for all FRAMES components.
DECLARE Function DelMeasure LIB "systemio.dll" Alias "__DelMeasure@4" (  ByVal Measure as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: NewUnit
' Create a new Unit within a measure. A negative value will be returned if the unit cannot be created. This typically occurs because the unit already exists in the list. This will create the measure for all FRAMES components.
DECLARE Function NewUnit LIB "systemio.dll" Alias "__NewUnit@12" (  ByVal Measure as string,  ByVal Unit as string,  ByVal UnitAbbr as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddUnit
' Add a new Unit within a measure. A negative value will be returned if the unit cannot be added. This typically occurs because the unit already exists in the list. This will create the measure for all FRAMES components. The slope and the offset (intercept) for the line conversion can be given. This makes this API support conversion for measures like temperature where a multiplier (Factor) and an offset (intercept) are needed to define the conversion.
DECLARE Function AddUnit LIB "systemio.dll" Alias "__AddUnit@28" (  ByVal Measure as string,  ByVal Unit as string,  ByVal UnitAbbr as string,  ByVal Factor as double,  ByVal Offset as double )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DelUnit
' Delete a unit from a measure. A negative value will be returned if the unit cannot be deleted. This typically occurs because the unit is not in the list. This will delete the unit for all FRAMES components.
DECLARE Function DelUnit LIB "systemio.dll" Alias "__DelUnit@8" (  ByVal Measure as string,  ByVal Unit as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetUnitFactor
' Get the unit factor for a unit from a measure. A negative value will be returned if the unit cannot be found. This typically occurs because the unit is not in the list.
DECLARE Function GetUnitFactor LIB "systemio.dll" Alias "__GetUnitFactor@12" (  ByVal Measure as string,  ByVal Unit as string, Factor as double )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetUnitOffset
' Get the unit factor for a unit from a measure. A negative value will be returned if the unit cannot be found. This typically occurs because the unit is not in the list.
DECLARE Function GetUnitOffset LIB "systemio.dll" Alias "__GetUnitOffset@12" (  ByVal Measure as string,  ByVal Unit as string, Offset as double )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetUnitFactor
' Set the unit factor for a unit from a measure. A negative value will be returned if the unit cannot be found. This typically occurs because the unit is not in the list. This will change the offset for all FRAMES components.
DECLARE Function SetUnitFactor LIB "systemio.dll" Alias "__SetUnitFactor@16" (  ByVal Measure as string,  ByVal Unit as string,  ByVal Factor as double )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetUnitOffset
' Set the unit factor for a unit from a measure. A negative value will be returned if the unit cannot be found. This typically occurs because the unit is not in the list. This will change the offset for all FRAMES components.
DECLARE Function SetUnitOffset LIB "systemio.dll" Alias "__SetUnitOffset@16" (  ByVal Measure as string,  ByVal Unit as string,  ByVal Offset as double )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetBaseUnit
' Get the base unit factor for a unit from a measure. A negative value will be returned if the unit or measure cannot be found. This typically occurs because the unit or measure is not in the list.
DECLARE Function DLLGetBaseUnit LIB "systemio.dll" Alias "__GetBaseUnit@8" (  ByVal Measure as string,  ByVal Unit as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetBaseUnitAbbr
' Get the base unit abbreviation for a unit from a measure. A negative value will be returned if the unit or measure cannot be found. This typically occurs because the unit or measure is not in the list.
DECLARE Function DLLGetBaseUnitAbbr LIB "systemio.dll" Alias "__GetBaseUnitAbbr@8" (  ByVal Measure as string,  ByVal Abbr as string )  as integer 

Sub GetMeasure (  ByVal index as long, Measure as string   ) 
      Dim retStr2 as String * MAXFIELD
      call DLLGetMeasure(index, retstr2)
      Measure=StripTerminator(retStr2)
End  Sub 

Function GetMeasureList (  ByVal Separator as string  , MeasureList as string   )  as integer 
      Dim retStr2 as String * MAXFIELD
      GetMeasureList = DLLGetMeasureList(Separator, retstr2)
      MeasureList=StripTerminator(retStr2)
End  Function 

Sub GetUnit (  ByVal MeasureIdx as long,  ByVal UnitIdx as long, Unit as string   ) 
      Dim retStr3 as String * MAXFIELD
      call DLLGetUnit(MeasureIdx, UnitIdx, retstr3)
      Unit=StripTerminator(retStr3)
End  Sub 

Sub GetUnitAbbr (  ByVal MeasureIdx as long,  ByVal UnitIdx as long, Unit as string   ) 
      Dim retStr3 as String * MAXFIELD
      call DLLGetUnitAbbr(MeasureIdx, UnitIdx, retstr3)
      Unit=StripTerminator(retStr3)
End  Sub 

Sub sGetUnit (  ByVal Measure as string  ,  ByVal UnitIdx as long, Unit as string   ) 
      Dim retStr3 as String * MAXFIELD
      call DLLsGetUnit(Measure, UnitIdx, retstr3)
      Unit=StripTerminator(retStr3)
End  Sub 

Sub sGetUnitAbbr (  ByVal Measure as string  ,  ByVal UnitIdx as long, Unit as string   ) 
      Dim retStr3 as String * MAXFIELD
      call DLLsGetUnitAbbr(Measure, UnitIdx, retstr3)
      Unit=StripTerminator(retStr3)
End  Sub 

Function GetUnitList (  ByVal MeasureIdx as long,  ByVal Separator as string  , UnitList as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      GetUnitList = DLLGetUnitList(MeasureIdx, Separator, retstr3)
      UnitList=StripTerminator(retStr3)
End  Function 

Function sGetUnitList (  ByVal Measure as string  ,  ByVal Separator as string  , UnitList as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      sGetUnitList = DLLsGetUnitList(Measure, Separator, retstr3)
      UnitList=StripTerminator(retStr3)
End  Function 

Function GetBaseUnit (  ByVal Measure as string  , Unit as string   )  as integer 
      Dim retStr2 as String * MAXFIELD
      GetBaseUnit = DLLGetBaseUnit(Measure, retstr2)
      Unit=StripTerminator(retStr2)
End  Function 

Function GetBaseUnitAbbr (  ByVal Measure as string  , Abbr as string   )  as integer 
      Dim retStr2 as String * MAXFIELD
      GetBaseUnitAbbr = DLLGetBaseUnitAbbr(Measure, retstr2)
      Abbr=StripTerminator(retStr2)
End  Function 
