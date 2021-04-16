Attribute VB_Name = "F2ConversionAPI"
Option Explicit
' API Name: FRAMES Conversion API
'-------------------------------------------------------------------------
' Documentation for: F2MeasureCount
' Returns the number of measures currently in the system.
DECLARE Function F2MeasureCount LIB "systemio.dll" Alias "__F2MeasureCount@0" (  )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetMeasure
' Get the name of a measure from a particular index. This function returns the measure name at the given index
DECLARE Function DLLF2GetMeasure LIB "systemio.dll" Alias "__F2GetMeasure@8" (  ByVal measureHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetMeasureHandle
' Get the index of measure given its name. If the measure is not found a negative value is returned.
DECLARE Function F2GetMeasureHandle LIB "systemio.dll" Alias "__F2GetMeasureHandle@4" (  ByVal measureName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2UnitsCount
' Get the number of units for a given measure index. If the measure index is not valid a negative value is returned.
DECLARE Function F2UnitsCount LIB "systemio.dll" Alias "__F2UnitsCount@4" (  ByVal measureHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2GetUnit
' Get the unit associated with a measure and unit index.
DECLARE Function DLLF2GetUnit LIB "systemio.dll" Alias "__F2GetUnit@12" (  ByVal measureHandle as long,  ByVal unitHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetUnitHandle
' Get the unit index associated with a measure and unit index.
DECLARE Function F2GetUnitHandle LIB "systemio.dll" Alias "__F2GetUnitHandle@8" (  ByVal measureHandle as long,  ByVal unitName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2GetConversion
' Convert a number from one unit in a measure to another unit.
DECLARE Function F2GetConversion LIB "systemio.dll" Alias "__F2GetConversion@20" (  ByVal measureHandle as long,  ByVal InitValue as double,  ByVal UnitFrom as string,  ByVal UnitTo as string )  as double 

'-------------------------------------------------------------------------
' Documentation for: F2AddMeasure
' Add a new measure to the set of measures. A negative value will be returned if the measure cannot be added. This typically occurs because the measure is already in the list. This will add the measure for all FRAMES components.
DECLARE Function F2AddMeasure LIB "systemio.dll" Alias "__F2AddMeasure@8" (  ByVal measureName as string,  ByVal unitName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2DeleteMeasure
' Delete a measure frpm the set of measures. A negative value will be returned if the measure cannot be deleted. This typically occurs because the measure is not in the list. This will delete the measure for all FRAMES components.
DECLARE Sub F2DeleteMeasure LIB "systemio.dll" Alias "__F2DeleteMeasure@4" (  ByVal measureHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2AddUnit
' Create a new Unit within a measure. A negative value will be returned if the unit cannot be created. This typically occurs because the unit already exists in the list. This will create the measure for all FRAMES components.
DECLARE Function F2AddUnit LIB "systemio.dll" Alias "__F2AddUnit@8" (  ByVal measureHandle as long,  ByVal unitName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2DeleteUnit
' Delete a unit from a measure. A negative value will be returned if the unit cannot be deleted. This typically occurs because the unit is not in the list. This will delete the unit for all FRAMES components.
DECLARE Sub F2DeleteUnit LIB "systemio.dll" Alias "__F2DeleteUnit@8" (  ByVal measureHandle as long,  ByVal unitHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetUnitFactor
' Get the unit factor for a unit from a measure. A negative value will be returned if the unit cannot be found. This typically occurs because the unit is not in the list. Factor for conversion between this unit and the base unit of the measure.
DECLARE Function F2GetUnitFactor LIB "systemio.dll" Alias "__F2GetUnitFactor@8" (  ByVal measureHandle as long,  ByVal unitHandle as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: F2GetUnitOffset
' Get the unit factor for a unit from a measure. A negative value will be returned if the unit cannot be found. This typically occurs because the unit is not in the list. Offset of conversion between this unit and the base unit of the measure
DECLARE Function F2GetUnitOffset LIB "systemio.dll" Alias "__F2GetUnitOffset@8" (  ByVal measureHandle as long,  ByVal unitHandle as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: F2SetUnitFactor
' Set the unit factor for a unit from a measure. A negative value will be returned if the unit cannot be found. This typically occurs because the unit is not in the list. This will change the offset for all FRAMES components.
DECLARE Sub F2SetUnitFactor LIB "systemio.dll" Alias "__F2SetUnitFactor@16" (  ByVal measureHandle as long,  ByVal unitHandle as long,  ByVal Factor as double ) 

'-------------------------------------------------------------------------
' Documentation for: F2SetUnitOffset
' Set the unit factor for a unit from a measure. A negative value will be returned if the unit cannot be found. This typically occurs because the unit is not in the list. This will change the offset for all FRAMES components.
DECLARE Sub F2SetUnitOffset LIB "systemio.dll" Alias "__F2SetUnitOffset@16" (  ByVal measureHandle as long,  ByVal unitHandle as long,  ByVal Offset as double ) 

'-------------------------------------------------------------------------
' Documentation for: F2GetBaseUnit
' Get the base unit factor for a unit from a measure. A negative value will be returned if the unit or measure cannot be found. This typically occurs because the unit or measure is not in the list.
DECLARE Function DLLF2GetBaseUnit LIB "systemio.dll" Alias "__F2GetBaseUnit@8" (  ByVal measureHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: MeasuresCount
' Returns the number of measures currently in the list of measures.
DECLARE Function MeasuresCount LIB "systemio.dll" Alias "__MeasuresCount@0" (  )  as integer 

'-------------------------------------------------------------------------
' Documentation for: MeasureGetName
' Returns the name of a single measure specified by the given handle.
DECLARE Function DLLMeasureGetName LIB "systemio.dll" Alias "__MeasureGetName@8" (  ByVal measureHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: MeasureGetIndex
' Returns the index of a single measure.
DECLARE Function MeasureGetIndex LIB "systemio.dll" Alias "__MeasureGetIndex@4" (  ByVal measureName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: MeasureGetHandle
' Returns the handle of a single measure.
DECLARE Function MeasureGetHandle LIB "systemio.dll" Alias "__MeasureGetHandle@4" (  ByVal measureName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: MeasureUnitCount
' Returns the number of units in a single measure specified by the given measure handle. If the measure handle is not valid, a negative number is returned.
DECLARE Function MeasureUnitCount LIB "systemio.dll" Alias "__MeasureUnitCount@4" (  ByVal measureHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: UnitGetName
' Returns the unit name.
DECLARE Function DLLUnitGetName LIB "systemio.dll" Alias "__UnitGetName@12" (  ByVal measureHandle as long,  ByVal unitHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: UnitGetIndex
' Returns the unit index with the specified name.
DECLARE Function UnitGetIndex LIB "systemio.dll" Alias "__UnitGetIndex@8" (  ByVal measureHandle as long,  ByVal unitName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: UnitGetHandle
' Returns the unit handle with the specified name.
DECLARE Function UnitGetHandle LIB "systemio.dll" Alias "__UnitGetHandle@8" (  ByVal measureHandle as long,  ByVal unitName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: UnitGetConversion
' Returns the result of the conversion. A value is converted from unit in a measure to another unit in the same measure.
DECLARE Function UnitGetConversion LIB "systemio.dll" Alias "__UnitGetConversion@20" (  ByVal measureHandle as long,  ByVal InitValue as double,  ByVal UnitFrom as string,  ByVal UnitTo as string )  as double 

'-------------------------------------------------------------------------
' Documentation for: MeasuresAdd
' Add a new measure to the set of measures. WARNING: This function does NOT add the base unit into the new measure's unit list!. A negative value will be returned if the measure cannot be added. This typically occurs because the measure is already in the list. This will add the measure for all FRAMES components.
DECLARE Function MeasuresAdd LIB "systemio.dll" Alias "__MeasuresAdd@8" (  ByVal measureName as string,  ByVal unitName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: MeasuresDelete
' Delete a measure from the set of measures. A negative value will be returned if the measure cannot be deleted. This typically occurs because the measure is not in the list. This will delete the measure for all FRAMES components.
DECLARE Sub MeasuresDelete LIB "systemio.dll" Alias "__MeasuresDelete@4" (  ByVal measureHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: MeasureAddUnit
' Add a new unit into a measure's unit list. A negative value will be returned if the unit cannot be created. This typically occurs because the unit already exists in the list. This will create the measure for all FRAMES components.
DECLARE Function MeasureAddUnit LIB "systemio.dll" Alias "__MeasureAddUnit@8" (  ByVal measureHandle as long,  ByVal unitName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: MeasureDeleteUnit
' Delete a unit from a measure's unit list. A negative value will be returned if the unit cannot be deleted. This typically occurs because the unit is not in the list. This will delete the unit for all FRAMES components.
DECLARE Sub MeasureDeleteUnit LIB "systemio.dll" Alias "__MeasureDeleteUnit@8" (  ByVal measureHandle as long,  ByVal unitHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: UnitGetFactor
' Returns the unit factor for a unit from a measure's unit list. The unit is specified by the unit handle. A negative value will be returned if the unit cannot be found. This typically occurs because the unit is not in the list.
DECLARE Function UnitGetFactor LIB "systemio.dll" Alias "__UnitGetFactor@8" (  ByVal measureHandle as long,  ByVal unitHandle as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: UnitGetOffset
' Returns the unit offset for a unit from a measure's unit list. The unit is specified by the unit handle. A negative value will be returned if the unit cannot be found. This typically occurs because the unit is not in the list.
DECLARE Function UnitGetOffset LIB "systemio.dll" Alias "__UnitGetOffset@8" (  ByVal measureHandle as long,  ByVal unitHandle as long )  as double 

'-------------------------------------------------------------------------
' Documentation for: UnitSetFactor
' Sets the unit factor for a unit from a measure's unit list. The unit is specified by the unit handle. This will change the offset for all FRAMES components.
DECLARE Sub UnitSetFactor LIB "systemio.dll" Alias "__UnitSetFactor@16" (  ByVal measureHandle as long,  ByVal unitHandle as long,  ByVal Factor as double ) 

'-------------------------------------------------------------------------
' Documentation for: UnitSetOffset
' Sets the unit offest for a unit from a measure's unit list. The unit is specified by the unit handle. This will change the offset for all FRAMES components.
DECLARE Sub UnitSetOffset LIB "systemio.dll" Alias "__UnitSetOffset@16" (  ByVal measureHandle as long,  ByVal unitHandle as long,  ByVal Offset as double ) 

'-------------------------------------------------------------------------
' Documentation for: MeasureGetBaseUnit
' Returns a measure's base unit name. An empty string is returned if the measure cannot be found. This typically occurs because the unit or measure is not in the list.
DECLARE Function DLLMeasureGetBaseUnit LIB "systemio.dll" Alias "__MeasureGetBaseUnit@8" (  ByVal measureHandle as long, ByVal value as String )  as long 

Function F2GetMeasure (  ByVal measureHandle as long  )  as string 
      Dim retStr2 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetMeasure(measureHandle, retStr2)
      F2GetMeasure=StripTerminator(retStr2)
End  Function 

Function F2GetUnit (  ByVal measureHandle as long ,  ByVal unitHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetUnit(measureHandle, unitHandle, retStr3)
      F2GetUnit=StripTerminator(retStr3)
End  Function 

Function F2GetBaseUnit (  ByVal measureHandle as long  )  as string 
      Dim retStr2 as String * MAXFIELD
      Dim value as long
      value = DLLF2GetBaseUnit(measureHandle, retStr2)
      F2GetBaseUnit=StripTerminator(retStr2)
End  Function 

Function MeasureGetName (  ByVal measureHandle as long  )  as string 
      Dim retStr2 as String * MAXFIELD
      Dim value as long
      value = DLLMeasureGetName(measureHandle, retStr2)
      MeasureGetName=StripTerminator(retStr2)
End  Function 

Function UnitGetName (  ByVal measureHandle as long ,  ByVal unitHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLUnitGetName(measureHandle, unitHandle, retStr3)
      UnitGetName=StripTerminator(retStr3)
End  Function 

Function MeasureGetBaseUnit (  ByVal measureHandle as long  )  as string 
      Dim retStr2 as String * MAXFIELD
      Dim value as long
      value = DLLMeasureGetBaseUnit(measureHandle, retStr2)
      MeasureGetBaseUnit=StripTerminator(retStr2)
End  Function 
