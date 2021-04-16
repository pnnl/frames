Attribute VB_Name = "FramesAPI"
Option Explicit
Option Compare Text

Public Const MAXFIELD = 4096
'// the need for the DLL* calls is to make
'// sure the string passed has the space to write on
Dim retString As String * MAXFIELD
Dim retString1 As String * MAXFIELD
Dim retString2 As String * MAXFIELD

'//--------------------------------------------------------------------------------------
'//----------- MISC. calls ----------------------------------
'//--------------------------------------------------------------------------------------
Public Declare Sub AddSeries Lib "Frames" (ByVal Num1 As Integer, ByRef Times1 As Double, ByRef Values1 As Double, ByVal Num2 As Long, ByRef Times2 As Double, ByRef Values2 As Double, ByRef NumRes As Long, ByRef TimesRes As Double, ByRef ValuesRes As Double)
Public Declare Function GetProbability Lib "Frames" (ByVal x As Long, ByVal y As Long, ByVal count As Long, ByRef intimes As Double, ByRef invalues As Double, ByRef minval As Double, ByRef maxval As Double, ByRef begIdx As Long, ByRef endIdx As Long, ByRef sprob As Double, ByRef svalue As Double) As Long
Public Declare Sub HasSections Lib "Frames" (ByVal valu As Long)
Public Declare Sub StatSort Lib "Frames" (ByVal n As Long, Idx As Long, val As Double, mean As Double, min As Double, max As Double, median As Double, sd As Double)
'//--------------------------------------------------------------------------------------
'//----------- SUMMMM calls ---------------------------------
'//--------------------------------------------------------------------------------------
Public Declare Function Initialize Lib "Frames" (ByVal lp1 As String, ByVal lp2 As String, ByVal lp3 As String, ByVal lp4 As String, ByVal lp5 As String) As Long
Public Declare Function Iterate Lib "Frames" (ByVal Iteration As Integer) As Long
Public Declare Function Finalize Lib "Frames" () As Long
'//--------------------------------------------------------------------------------------
'//----------- SCF calls ------------------------------------
'//--------------------------------------------------------------------------------------
Public Declare Sub scfOpen Lib "Frames" (ByVal fname As String, ByVal Source As String)
Public Declare Sub scfClose Lib "Frames" ()
Public Declare Function scfGetNumRunInfo Lib "Frames" () As Long
Public Declare Function scfGetNumSets Lib "Frames" () As Long
Public Declare Function scfGetNumContam Lib "Frames" (ByVal dsIdx As Long) As Long
Public Declare Function scfGetNumProgeny Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long) As Long
Public Declare Function scfGetDimensions Lib "Frames" (ByVal dsIdx As Long, ByRef width As Double, ByRef length As Double, ByRef depth As Double) As Long
Public Declare Function scfGetSeriesValues Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdxx As Long, ByVal count As Long, ByRef times As Double, ByRef values As Double) As Long
Public Declare Function DLLscfGetRunInfo Lib "Frames" Alias "scfGetRunInfo" (ByVal Idx As Long, ByVal info As String) As Long
Public Declare Function DLLscfGetSetInfo Lib "Frames" Alias "scfGetSetInfo" (ByVal dsIdx As Long, ByVal dsname As String, ByVal dstype As String) As Long
Public Declare Function DLLscfGetContamName Lib "Frames" Alias "scfGetContamName" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal name As String, ByVal cas As String) As Long
Public Declare Function DLLscfGetSeriesProperties Lib "Frames" Alias "scfGetSeriesProperties" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal units As String, ByVal timeunits As String) As Long
Public Declare Sub scfAggregate Lib "Frames" (ByVal filename As String, ByVal casList As String)
Public Declare Sub scfInsert Lib "Frames" (ByVal filename As String, ByVal casList As String, ByVal nameList As String, ByVal degList As String, ByVal secList As String, ByVal branching As Long)
'//--------------------------------------------------------------------------------------
'//----------- AFF calls ------------------------------------
'//--------------------------------------------------------------------------------------
Public Declare Sub affOpen Lib "Frames" (ByVal fname As String, ByVal Source As String)
Public Declare Sub affClose Lib "Frames" ()
Public Declare Function affGetNumRunInfo Lib "Frames" () As Long
Public Declare Function affGetNumSets Lib "Frames" () As Long
Public Declare Function affGetNumFluxTypes Lib "Frames" (ByVal dsIdx As Long) As Long
Public Declare Function affGetNumContam Lib "Frames" (ByVal dsIdx As Long) As Long
Public Declare Function affGetNumProgeny Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long) As Long
Public Declare Function affGetDimensions Lib "Frames" (ByVal dsIdx As Long, ByRef area As Double, ByRef eht As Double, ByRef aht As Double, ByRef vel As Double, ByRef etmp As Double, ByRef atmp As Double) As Long
Public Declare Function affGetSeriesValues Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal fluxIdx As Long, ByVal count As Long, ByRef times As Double, ByRef values As Double) As Long
Public Declare Function DLLaffGetRunInfo Lib "Frames" Alias "affGetRunInfo" (ByVal Idx As Long, ByVal info As String) As Long
Public Declare Function DLLaffGetSetInfo Lib "Frames" Alias "affGetSetInfo" (ByVal dsIdx As Long, ByVal dsname As String, ByVal dstype As String) As Long
Public Declare Function DLLaffGetFluxType Lib "Frames" Alias "affGetFluxType" (ByVal dsIdx As Long, ByVal fluxIdx As Long, ByVal ftype As String, ByRef fsize As Double, ByVal suom As String, ByRef density As Double, ByVal duom As String) As Long
Public Declare Function DLLaffGetContamName Lib "Frames" Alias "affGetContamName" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal name As String, ByVal cas As String) As Long
Public Declare Function DLLaffGetSeriesProperties Lib "Frames" Alias "affGetSeriesProperties" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal units As String, ByVal timeunits As String) As Long
Public Declare Sub affAggregate Lib "Frames" (ByVal filename As String, ByVal casList As String)
Public Declare Sub affInsert Lib "Frames" (ByVal filename As String, ByVal casList As String, ByVal nameList As String, ByVal degList As String, ByVal secList As String, ByVal branching As Long)
'//--------------------------------------------------------------------------------------
'//----------- WFF calls ------------------------------------
'//--------------------------------------------------------------------------------------
Public Declare Sub wffOpen Lib "Frames" (ByVal fname As String, ByVal Source As String)
Public Declare Sub wffClose Lib "Frames" ()
Public Declare Function wffGetNumRunInfo Lib "Frames" () As Long
Public Declare Function wffGetNumSets Lib "Frames" () As Long
Public Declare Function wffGetNumContam Lib "Frames" (ByVal dsIdx As Long) As Long
Public Declare Function wffGetNumFluxTypes Lib "Frames" (ByVal dsIdx As Long) As Long
Public Declare Function wffGetNumProgeny Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long) As Long
Public Declare Function wffGetDimensions Lib "Frames" (ByVal dsIdx As Long, ByRef width As Double, ByRef length As Double, ByRef dist As Double, ByRef recharge As Double) As Long
Public Declare Function wffGetWaterFluxSeries Lib "Frames" (ByVal dsIdx As Long, ByVal count As Long, ByRef times As Long, ByRef values As Double) As Double
Public Declare Function wffGetSeriesValues Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal fluxIdx As Long, ByVal count As Long, ByRef times As Double, ByRef values As Double) As Long
Public Declare Function wffGetSeriesValuesSum Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal count As Long, ByRef times As Double, ByRef values As Double) As Long
Public Declare Function DLLwffGetRunInfo Lib "Frames" Alias "wffGetRunInfo" (ByVal Idx As Long, ByVal info As String) As Long
Public Declare Function DLLwffGetSetInfo Lib "Frames" Alias "wffGetSetInfo" (ByVal dsIdx As Long, ByVal dsname As String, ByVal dstype As String) As Long
Public Declare Function DLLwffGetContamName Lib "Frames" Alias "wffGetContamName" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal name As String, ByVal cas As String) As Long
Public Declare Function DLLwffGetSeriesProperties Lib "Frames" Alias "wffGetSeriesProperties" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal units As String, ByVal timeunits As String) As Long
Public Declare Sub wffAggregate Lib "Frames" (ByVal filename As String, ByVal casList As String)
Public Declare Sub wffInsert Lib "Frames" (ByVal filename As String, ByVal casList As String, ByVal nameList As String, ByVal degList As String, ByVal secList As String, ByVal branching As Long)
'//--------------------------------------------------------------------------------------
'//----------- WCF calls ------------------------------------
'//--------------------------------------------------------------------------------------
Public Declare Sub wcfOpen Lib "Frames" (ByVal fname As String, ByVal Source As String)
Public Declare Sub wcfClose Lib "Frames" ()
Public Declare Function wcfGetNumRunInfo Lib "Frames" () As Long
Public Declare Function wcfGetNumSets Lib "Frames" () As Long
Public Declare Function wcfGetNumContam Lib "Frames" (ByVal dsIdx As Long) As Long
Public Declare Function wcfGetNumProgeny Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long) As Long
Public Declare Function wcfGetLocation Lib "Frames" (ByVal dsIdx As Long, ByRef x As Double, ByRef y As Double, ByRef z As Double) As Long
Public Declare Function wcfGetSeriesValues Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdxx As Long, ByVal count As Long, ByRef times As Double, ByRef values As Double) As Long
Public Declare Function DLLwcfGetRunInfo Lib "Frames" Alias "wcfGetRunInfo" (ByVal Idx As Long, ByVal info As String) As Long
Public Declare Function DLLwcfGetSetInfo Lib "Frames" Alias "wcfGetSetInfo" (ByVal dsIdx As Long, ByVal dsname As String, ByVal dstype As String) As Long
Public Declare Function DLLwcfGetContamName Lib "Frames" Alias "wcfGetContamName" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal name As String, ByVal cas As String) As Long
Public Declare Function DLLwcfGetSeriesProperties Lib "Frames" Alias "wcfGetSeriesProperties" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal units As String, ByVal timeunits As String) As Long
Public Declare Sub wcfAggregate Lib "Frames" (ByVal filename As String, ByVal casList As String)
Public Declare Sub wcfInsert Lib "Frames" (ByVal filename As String, ByVal casList As String, ByVal nameList As String, ByVal degList As String, ByVal secList As String, ByVal branching As Long)
'//--------------------------------------------------------------------------------------
'//----------- ATO calls ------------------------------------
'//--------------------------------------------------------------------------------------
Public Declare Sub atoOpen Lib "Frames" (ByVal FUIName As String, ByVal Source As String)
Public Declare Sub atoClose Lib "Frames" ()
Public Declare Function atoGetDatasetCount Lib "Frames" () As Long
Public Declare Function atoGetConstituentCount Lib "Frames" (ByVal dsIdx As Long) As Long
Public Declare Function atoGetFluxCount Lib "Frames" (ByVal dsIdx As Long) As Long
Public Declare Sub DLLatoGetDatasetName Lib "Frames" Alias "atoGetDatasetName" (ByVal dsIdx As Long, ByVal name As String)
Public Declare Sub DLLatoGetDatasetModname Lib "Frames" Alias "atoGetDatasetModname" (ByVal dsIdx As Long, ByVal name As String)
Public Declare Sub DLLatoGetCoordinateType Lib "Frames" Alias "atoGetCoordinateType" (ByVal dsIdx As Long, ByVal name As String)
Public Declare Sub DLLatoGetReleaseType Lib "Frames" Alias "atoGetReleaseType" (ByVal dsIdx As Long, ByVal name As String)
Public Declare Sub DLLatoGetSpatialType Lib "Frames" Alias "atoGetSpatialType" (ByVal dsIdx As Long, ByVal name As String)
'//----------- ATO flux calls -------------------------------
Public Declare Sub DLLatoGetFluxType Lib "Frames" Alias "atoGetFluxType" (ByVal dsIdx As Long, ByVal fluxIndex As Long, ByVal name As String)
Public Declare Sub DLLatoGetFluxRadiusUnits Lib "Frames" Alias "atoGetFluxRadiusUnits" (ByVal dsIdx As Long, ByVal fluxIndex As Long, ByVal name As String)
Public Declare Sub DLLatoGetFluxDensityUnits Lib "Frames" Alias "atoGetFluxDensityUnits" (ByVal dsIdx As Long, ByVal fluxIndex As Long, ByVal name As String)
Public Declare Function atoGetFluxRadius Lib "Frames" (ByVal dsIdx As Long, ByVal fluxIndex As Long) As Double
'//----------- ATO constituent calls ------------------------
Public Declare Sub DLLatoGetCasID Lib "Frames" Alias "atoGetCasID" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal name As String)
Public Declare Sub DLLatoGetChemName Lib "Frames" Alias "atoGetChemName" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal name As String)
Public Declare Sub DLLatoGetParentCasID Lib "Frames" Alias "atoGetParentCasID" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal name As String)
Public Declare Sub DLLatoGetParentChemName Lib "Frames" Alias "atoGetParentChemName" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal name As String)
Public Declare Function atoGetProgenyCount Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long) As Long
'//----------- ATO time period calls ------------------------
Public Declare Sub DLLatoGetTimePeriodUnit Lib "Frames" Alias "atoGetTimePeriodUnit" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long, ByVal name As String)
Public Declare Function atoGetTimePeriodCount Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long) As Long
Public Declare Function atoGetTimePeriodOutputCount Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long) As Long
Public Declare Function atoGetTimePeriodTime Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long) As Double
'//----------- ATO grid calls -------------------------------
Public Declare Sub DLLatoGetGridAxis1Unit Lib "Frames" Alias "atoGetGridAxis1Unit" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long, ByVal gridID As Long, ByVal name As String)
Public Declare Sub DLLatoGetGridAxis2Unit Lib "Frames" Alias "atoGetGridAxis2Unit" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long, ByVal gridID As Long, ByVal name As String)
Public Declare Sub DLLatoGetGridDataUnit Lib "Frames" Alias "atoGetGridDataUnit" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long, ByVal gridID As Long, ByVal name As String)
Public Declare Function atoGetGridFluxIndex Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long, ByVal gridID As Long) As Long
Public Declare Sub DLLatoGetGridOutputType Lib "Frames" Alias "atoGetGridOutputType" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long, ByVal gridID As Long, ByVal name As String)
Public Declare Sub DLLatoGetGridMoist Lib "Frames" Alias "atoGetGridMoist" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long, ByVal gridID As Long, ByVal name As String)
Public Declare Function atoGetGridMax Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long, ByVal gridID As Long, ByRef xCoord As Double, ByRef yCoord As Double) As Double
Public Declare Function atoGetGridMin Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long, ByVal gridID As Long, ByRef xCoord As Double, ByRef yCoord As Double) As Double
Public Declare Function atoGetGridValue Lib "Frames" (ByVal dsID As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long, ByVal gridID As Long, ByVal axis1 As Long, ByVal axis2 As Long) As Double
Public Declare Function atoGetGridAxis1Count Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long, ByVal gridID As Long) As Long
Public Declare Function atoGetGridAxis2Count Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long, ByVal gridID As Long) As Long
Public Declare Function atoGetGridAxis1Value Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long, ByVal gridID As Long, ByVal Index As Long) As Double
Public Declare Function atoGetGridAxis2Value Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long, ByVal gridID As Long, ByVal Index As Long) As Double
'//----------- ATO timeseries calls -------------------------
Public Declare Function atoGetTimeSeries Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal output As String, ByVal flux As String, ByVal moist As String, ByVal xCoord As Double, ByVal yCoord As Double) As Long
Public Declare Function atoGetTimeSeriesTime Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal n As Long) As Double
Public Declare Function atoGetTimeSeriesValue Lib "Frames" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal n As Long) As Double
Public Declare Sub DLLatoGetTimeSeriesXAxisUnit Lib "Frames" Alias "atoGetTimeSeriesXAxisUnit" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal unit As String)
Public Declare Sub DLLatoGetTimeSeriesYAxisUnit Lib "Frames" Alias "atoGetTimeSeriesYAxisUnit" (ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal unit As String)
'//----------- ATO degradation chain coversion calls --------
Public Declare Sub atoAggregate Lib "Frames" (ByVal filename As String, ByVal casList As String)
Public Declare Sub atoInsert Lib "Frames" (ByVal filename As String, ByVal casList As String, ByVal nameList As String, ByVal degList As String, ByVal secList As String, ByVal branching As Long)
'//--------------------------------------------------------------------------------------
'//----------- BBF calls ------------------------------------
'//--------------------------------------------------------------------------------------
Public Declare Sub bbfOpen Lib "Frames" (ByVal FUIName As String, ByVal modId As String)
Public Declare Sub bbfClose Lib "Frames" ()
Public Declare Function bbfGetNumRunInfo Lib "Frames" () As Long
Public Declare Function bbfGetNumSets Lib "Frames" () As Long
Public Declare Function bbfGetNumVULevels Lib "Frames" (ByVal dsIdx As Long) As Long
Public Declare Function bbfGetNumOrganism Lib "Frames" (ByVal dsIdx As Long) As Long
Public Declare Function bbfGetNumContam Lib "Frames" (ByVal dsIdx As Long, ByVal orgIdx As Long) As Long
Public Declare Function bbfGetNumProgeny Lib "Frames" (ByVal dsIdx As Long, ByVal orgIdx As Long, ByVal conIdx As Long) As Long
Public Declare Function bbfGetSeriesValues Lib "Frames" (ByVal dsIdx As Long, ByVal orgIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal vuIdx As Long, ByVal count As Long, ByRef times As Double, ByRef values As Double) As Long
Public Declare Function DLLbbfGetRunInfo Lib "Frames" Alias "bbfGetRunInfo" (ByVal Idx As Long, ByVal info As String) As Long
Public Declare Function DLLbbfGetSetInfo Lib "Frames" Alias "bbfGetSetInfo" (ByVal dsIdx As Long, ByVal dsname As String, ByVal dstype As String) As Long
Public Declare Function DLLbbfGetVULevel Lib "Frames" Alias "bbfGetVULevel" (ByVal dsIdx As Long, ByVal vuIdx As Long, ByVal variability As String, ByVal uncertainty As String) As Long
Public Declare Function DLLbbfGetOrganismName Lib "Frames" Alias "bbfGetOrganismName" (ByVal dsIdx As Long, ByVal orgIdx As Long, ByVal name As String, ByVal id As String) As Long
Public Declare Function DLLbbfGetContamName Lib "Frames" Alias "bbfGetContamName" (ByVal dsIdx As Long, ByVal orgIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal name As String, ByVal cas As String) As Long
Public Declare Function DLLbbfGetSeriesProperties Lib "Frames" Alias "bbfGetSeriesProperties" (ByVal dsIdx As Long, ByVal orgIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal concunits As String, ByVal timeunits As String) As Long
'//--------------------------------------------------------------------------------------
'//----------- EPF calls ------------------------------------
'//--------------------------------------------------------------------------------------
Public Declare Function epChecksum Lib "Frames" () As Boolean
Public Declare Function epfOpen Lib "Frames" (ByVal FUIName As String, ByVal Source As String) As Long
'epfreaddatasets must be called before using the following functions
Public Declare Sub epfClose Lib "Frames" ()
Public Declare Sub epfAggregate Lib "Frames" (ByVal filename As String, ByVal casList As String)
Public Declare Function epfGetNumRunInfo Lib "Frames" () As Long
Public Declare Function epfGetRunInfo Lib "Frames" (ByVal Idx As Long, ByVal info As String) As Long
Public Declare Function epfGetSetInfo Lib "Frames" (ByVal dsIdx As Long, ByRef numLoc As Long, ByVal typ As String, ByVal med As String, ByVal exp As String) As Long
Public Declare Function epfGetExposurePoint Lib "Frames" (ByVal dsIdx As Long, ByVal locIdx As Long, ByRef xp As Double, ByRef yp As Double) As Long
Public Declare Function epfGetTimeCount Lib "Frames" (ByVal dsIdx As Long, ByVal parencas As String) As Long
Public Declare Function epfGetTime Lib "Frames" (ByVal dsIdx As Long, ByVal parentCAS As String, ByVal timeIdx As Long) As Double
Public Declare Function epfLoadRoutesAndPathwaysByTime Lib "Frames" (ByVal dsIdx As Long, ByVal casid As String, ByVal parentCAS As String, ByVal timeIdx As Long) As Long
Public Declare Sub epfGetRouteAndPathway Lib "Frames" (ByVal dsIdx As Long, ByVal expIdx As Long, ByVal Path As String, ByVal Route As String, ByVal unit As String)
Public Declare Function epfLoadTimeSeries Lib "Frames" (ByVal dsIdx As Long, ByVal locIdx As Long, ByVal casid As String, ByVal parentCAS As String, ByVal Path As String, ByVal Route As String) As Long
'epfloadtimeseries must be called before using the following functions
Public Declare Sub epfGetTimeSeries Lib "Frames" (ByVal dsIdx As Long, ByRef times As Double, ByRef values As Double)
Public Declare Sub epfGetTimeAndValue Lib "Frames" (ByVal dsIdx As Long, ByVal timeIdx As Long, ByRef times As Double, ByRef values As Double)
Public Declare Function epfGetDuration Lib "Frames" (ByVal dsIdx As Long) As Double
'//--------------------------------------------------------------------------------------
'//----------- RIF calls ------------------------------------
'//--------------------------------------------------------------------------------------
Public Declare Function rifChecksum Lib "Frames" () As Boolean
Public Declare Function rifOpen Lib "Frames" (ByVal FUIName As String, ByVal Source As String) As Long
'rifreaddatasets must be called before using the following functions
Public Declare Sub rifClose Lib "Frames" ()
Public Declare Sub rifAggregate Lib "Frames" (ByVal filename As String, ByVal casList As String)
Public Declare Function rifGetNumRunInfo Lib "Frames" () As Long
Public Declare Function rifGetRunInfo Lib "Frames" (ByVal Idx As Long, ByVal info As String) As Long
Public Declare Function rifGetSetInfo Lib "Frames" (ByVal dsIdx As Long, ByRef numLoc As Long, ByRef numAge As Long, ByVal typ As String, ByVal med As String, ByVal exp As String) As Long
Public Declare Function rifGetExposurePoint Lib "Frames" (ByVal dsIdx As Long, ByVal locIdx As Long, ByRef xp As Double, ByRef yp As Double) As Long
Public Declare Function rifGetTimeCount Lib "Frames" (ByVal dsIdx As Long, ByVal ageIdx As Long, ByVal parentCAS As String) As Long
Public Declare Function rifGetTime Lib "Frames" (ByVal dsIdx As Long, ByVal ageIdx As Long, ByVal parentCAS As String, ByVal timeIdx As Long) As Double
Public Declare Function rifGetAgeGroup Lib "Frames" (ByVal dsIdx As Long, ByVal ageIdx As Long, ByRef min As Double, ByRef max As Double) As Long
Public Declare Function rifLoadRoutesAndPathwaysByTime Lib "Frames" (ByVal dsIdx As Long, ByVal ageIdx As Long, ByVal casid As String, ByVal parentCAS As String, ByVal timeIdx As Long) As Long
Public Declare Sub rifGetRouteAndPathway Lib "Frames" (ByVal dsIdx As Long, ByVal expIdx As Long, ByVal Path As String, ByVal Route As String, ByVal measure As String, ByVal unit As String)
Public Declare Function rifLoadTimeSeries Lib "Frames" (ByVal dsIdx As Long, ByVal locIdx As Long, ByVal ageIdx As Long, ByVal casid As String, ByVal parentCAS As String, ByVal Path As String, ByVal Route As String, ByVal measure As String, ByVal unit As String) As Long
'rifloadtimeseries must be called before using the following functions
Public Declare Sub rifGetTimeSeries Lib "Frames" (ByVal dsIdx As Long, ByRef times As Double, ByRef values As Double)
Public Declare Sub rifGetTimeAndValue Lib "Frames" (ByVal dsIdx As Long, ByVal timeIdx As Long, ByRef times As Double, ByRef values As Double)
Public Declare Function rifGetDuration Lib "Frames" (ByVal dsIdx As Long) As Double
Public Declare Function rifGetPopulation Lib "Frames" (ByVal dsIdx As Long) As Double
'//--------------------------------------------------------------------------------------
'//----------- HIF calls ------------------------------------
'//--------------------------------------------------------------------------------------
Public Declare Function hifChecksum Lib "Frames" () As Boolean
Public Declare Function hifOpen Lib "Frames" (ByVal FUIName As String, ByVal Source As String) As Long
'hifreaddatasets must be called before using the following functions
Public Declare Sub hifClose Lib "Frames" ()
Public Declare Sub hifAggregate Lib "Frames" (ByVal filename As String, ByVal casList As String)
Public Declare Function hifGetNumRunInfo Lib "Frames" () As Long
Public Declare Function hifGetRunInfo Lib "Frames" (ByVal Idx As Long, ByVal info As String) As Long
Public Declare Function hifGetSetInfo Lib "Frames" (ByVal dsIdx As Long, ByRef numLoc As Long, ByRef numAge As Long, ByRef numOrgC As Long, ByRef numOrgD As Long, ByVal typ As String, ByVal med As String, ByVal exp As String) As Long
Public Declare Function hifGetExposurePoint Lib "Frames" (ByVal dsIdx As Long, ByVal locIdx As Long, ByRef xp As Double, ByRef yp As Double) As Long
Public Declare Function hifGetTimeCount Lib "Frames" (ByVal dsIdx As Long, ByVal ageIdx As Long, ByVal parentCAS As String) As Long
Public Declare Function hifGetTime Lib "Frames" (ByVal dsIdx As Long, ByVal ageIdx As Long, ByVal parentCAS As String, ByVal timeIdx As Long) As Double
Public Declare Function hifGetAgeGroup Lib "Frames" (ByVal dsIdx As Long, ByVal ageIdx As Long, ByRef min As Double, ByRef max As Double) As Long
Public Declare Sub hifGetCancerOrgan Lib "Frames" (ByVal dsIdx As Long, ByVal orgIdx As Long, ByVal lpStrings As String)
Public Declare Sub hifGetDoseOrgan Lib "Frames" (ByVal dsIdx As Long, ByVal orgIdx As Long, ByVal lpStrings As String)
Public Declare Function hifLoadRoutesAndPathwaysByTime Lib "Frames" (ByVal dsIdx As Long, ByVal ageIdx As Long, ByVal casid As String, ByVal parentCAS As String, ByVal timeIdx As Long) As Long
Public Declare Sub hifGetRouteAndPathway Lib "Frames" (ByVal dsIdx As Long, ByVal expIdx As Long, ByVal Path As String, ByVal Route As String, ByVal measure As String, ByVal unit As String)
Public Declare Function hifLoadTimeSeries Lib "Frames" (ByVal dsIdx As Long, ByVal locIdx As Long, ByVal ageIdx As Long, ByVal orgIdx As Long, ByVal cas As String, ByVal parentCAS As String, ByVal Path As String, ByVal Route As String, ByVal measure As String, ByVal unit As String) As Long
'hifloadtimeseries must be called before using the following functions
Public Declare Sub hifGetTimeSeries Lib "Frames" (ByVal dsIdx As Long, ByRef times As Double, ByRef values As Double)
Public Declare Sub hifGetTimeAndValue Lib "Frames" (ByVal dsIdx As Long, ByVal timeIdx As Long, ByRef times As Double, ByRef values As Double)
Public Declare Function hifGetDuration Lib "Frames" (ByVal dsIdx As Long) As Double
Public Declare Function hifGetPopulation Lib "Frames" (ByVal dsIdx As Long) As Double
'//--------------------------------------------------------------------------------------
'//----------- EXF calls ------------------------------------
'//--------------------------------------------------------------------------------------
Public Declare Sub exfOpen Lib "Frames" (ByVal FUIName As String, ByVal modId As String)
Public Declare Sub exfClose Lib "Frames" ()
Public Declare Function exfGetNumRunInfo Lib "Frames" () As Long
Public Declare Function exfGetNumSets Lib "Frames" () As Long
Public Declare Function exfGetNumLocation Lib "Frames" (ByVal dsIdx As Long) As Long
Public Declare Function exfGetNumOrganism Lib "Frames" (ByVal dsIdx As Long) As Long
Public Declare Function exfGetNumContam Lib "Frames" (ByVal dsIdx As Long, ByVal orgIdx As Long) As Long
Public Declare Function exfGetNumEffects Lib "Frames" (ByVal dsIdx As Long, ByVal orgIdx As Long, ByVal conIdx As Long) As Long
Public Declare Function exfGetSeriesValues Lib "Frames" (ByVal dsIdx As Long, ByVal orgIdx As Long, ByVal conIdx As Long, ByVal efxIdx As Long, ByVal count As Long, ByRef times As Double, ByRef values As Double) As Long
Public Declare Function DLLexfGetRunInfo Lib "Frames" Alias "exfGetRunInfo" (ByVal Idx As Long, ByVal info As String) As Long
Public Declare Function DLLexfGetSetInfo Lib "Frames" Alias "exfGetSetInfo" (ByVal dsIdx As Long, ByVal dsname As String, ByVal dstype As String) As Long
Public Declare Function DLLexfGetLocationName Lib "Frames" Alias "exfGetLocationName" (ByVal dsIdx As Long, ByVal locIdx As Long, ByVal name As String, ByVal id As String) As Long
Public Declare Function DLLexfGetOrganismName Lib "Frames" Alias "exfGetOrganismName" (ByVal dsIdx As Long, ByVal orgIdx As Long, ByVal name As String, ByVal id As String) As Long
Public Declare Function DLLexfGetContamName Lib "Frames" Alias "exfGetContamName" (ByVal dsIdx As Long, ByVal orgIdx As Long, ByVal conIdx As Long, ByVal name As String, ByVal cas As String) As Long
Public Declare Function DLLexfGetSeriesProperties Lib "Frames" Alias "exfGetSeriesProperties" (ByVal dsIdx As Long, ByVal orgIdx As Long, ByVal conIdx As Long, ByVal efxIdx As Long, ByVal efxDes As String, ByVal efxunits As String, ByVal timeunits As String) As Long

Public Function scfGetRunInfo(ByVal dsIdx As Long, ByRef info As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  scfGetRunInfo = DLLscfGetRunInfo(dsIdx, retString)
  info = StripTerminator(retString)
End Function

Public Function scfGetSetInfo(ByVal dsIdx As Long, ByRef dsname As String, ByRef dstype As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  scfGetSetInfo = DLLscfGetSetInfo(dsIdx, retString, retString1)
  dsname = StripTerminator(retString)
  dstype = StripTerminator(retString1)
End Function

Public Function scfGetContamName(ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByRef name As String, ByRef cas As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  scfGetContamName = DLLscfGetContamName(dsIdx, conIdx, progIdx, retString, retString1)
  name = StripTerminator(retString)
  cas = StripTerminator(retString1)
End Function

Public Function scfGetSeriesProperties(ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByRef units As String, ByRef timeunits As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  scfGetSeriesProperties = DLLscfGetSeriesProperties(dsIdx, conIdx, progIdx, retString, retString1)
  units = StripTerminator(retString)
  timeunits = StripTerminator(retString1)
End Function

Public Function affGetRunInfo(ByVal dsIdx As Long, ByRef info As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  affGetRunInfo = DLLaffGetRunInfo(dsIdx, retString)
  info = StripTerminator(retString)
End Function

Public Function affGetSetInfo(ByVal dsIdx As Long, ByRef dsname As String, ByRef dstype As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  affGetSetInfo = DLLaffGetSetInfo(dsIdx, retString, retString1)
  dsname = StripTerminator(retString)
  dstype = StripTerminator(retString1)
End Function

Public Function affGetFluxType(ByVal dsIdx As Long, ByVal nFlux As Long, ByRef name As String, ByRef size As Double, ByRef suom As String, ByRef density As Double, ByRef duom As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  retString2 = Space(MAXFIELD)
  affGetFluxType = DLLaffGetFluxType(dsIdx, nFlux, retString, size, retString1, density, retString2)
  name = StripTerminator(retString)
  suom = StripTerminator(retString1)
  duom = StripTerminator(retString2)
End Function

Public Function affGetContamName(ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByRef name As String, ByRef cas As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  affGetContamName = DLLaffGetContamName(dsIdx, conIdx, progIdx, retString, retString1)
  name = StripTerminator(retString)
  cas = StripTerminator(retString1)
End Function

Public Function affGetSeriesProperties(ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByRef units As String, ByRef timeunits As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  affGetSeriesProperties = DLLaffGetSeriesProperties(dsIdx, conIdx, progIdx, retString, retString1)
  units = StripTerminator(retString)
  timeunits = StripTerminator(retString1)
End Function

Public Function wffGetRunInfo(ByVal dsIdx As Long, ByRef info As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  wffGetRunInfo = DLLwffGetRunInfo(dsIdx, retString)
  info = StripTerminator(retString)
End Function

Public Function wffGetSetInfo(ByVal dsIdx As Long, ByRef dsname As String, ByRef dstype As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  wffGetSetInfo = DLLwffGetSetInfo(dsIdx, retString, retString1)
  dsname = StripTerminator(retString)
  dstype = StripTerminator(retString1)
End Function

Public Function wffGetContamName(ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByRef name As String, ByRef cas As String) As Long
  name = Space(MAXFIELD)
  cas = Space(MAXFIELD)
  wffGetContamName = DLLwffGetContamName(dsIdx, conIdx, progIdx, name, cas)
  name = StripTerminator(name)
  cas = StripTerminator(cas)
End Function

Public Function wffGetSeriesProperties(ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByRef units As String, ByRef timeunits As String) As Long
  units = Space(MAXFIELD)
  timeunits = Space(MAXFIELD)
  wffGetSeriesProperties = DLLwffGetSeriesProperties(dsIdx, conIdx, progIdx, units, timeunits)
  units = StripTerminator(units)
  timeunits = StripTerminator(timeunits)
End Function

Public Function wcfGetRunInfo(ByVal dsIdx As Long, ByRef info As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  wcfGetRunInfo = DLLwcfGetRunInfo(dsIdx, retString)
  info = StripTerminator(retString)
End Function

Public Function wcfGetSetInfo(ByVal dsIdx As Long, ByRef dsname As String, ByRef dstype As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  wcfGetSetInfo = DLLwcfGetSetInfo(dsIdx, retString, retString1)
  dsname = StripTerminator(retString)
  dstype = StripTerminator(retString1)
End Function

Public Function wcfGetContamName(ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByRef name As String, ByRef cas As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  wcfGetContamName = DLLwcfGetContamName(dsIdx, conIdx, progIdx, retString, retString1)
  name = StripTerminator(retString)
  cas = StripTerminator(retString1)
End Function

Public Function wcfGetSeriesProperties(ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByRef units As String, ByRef timeunits As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  wcfGetSeriesProperties = DLLwcfGetSeriesProperties(dsIdx, conIdx, progIdx, retString, retString1)
  units = StripTerminator(retString)
  timeunits = StripTerminator(retString1)
End Function

Public Sub atoGetDatasetName(ByVal dsIdx As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetDatasetName dsIdx, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetDatasetModname(ByVal dsIdx As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetDatasetModname dsIdx, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetCoordinateType(ByVal dsIdx As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetCoordinateType dsIdx, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetReleaseType(ByVal dsIdx As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetReleaseType dsIdx, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetSpatialType(ByVal dsIdx As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetSpatialType dsIdx, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetFluxType(ByVal dsIdx As Long, ByVal fluxIndex As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetFluxType dsIdx, fluxIndex, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetFluxRadiusUnits(ByVal dsIdx As Long, ByVal fluxIndex As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetFluxRadiusUnits dsIdx, fluxIndex, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetFluxDensityUnits(ByVal dsIdx As Long, ByVal fluxIndex As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetFluxDensityUnits dsIdx, fluxIndex, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetCasID(ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetCasID dsIdx, conIdx, progIdx, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetChemName(ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetChemName dsIdx, conIdx, progIdx, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetParentCasID(ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetParentCasID dsIdx, conIdx, progIdx, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetParentChemName(ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetParentChemName dsIdx, conIdx, progIdx, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetTimePeriodUnit(ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetTimePeriodUnit dsIdx, conIdx, progIdx, timeID, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetGridAxis1Unit(ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long, ByVal gridID As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetGridAxis1Unit dsIdx, conIdx, progIdx, timeID, gridID, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetGridAxis2Unit(ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long, ByVal gridID As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetGridAxis2Unit dsIdx, conIdx, progIdx, timeID, gridID, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetGridDataUnit(ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long, ByVal gridID As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetGridDataUnit dsIdx, conIdx, progIdx, timeID, gridID, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetGridMoist(ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long, ByVal gridID As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetGridMoist dsIdx, conIdx, progIdx, timeID, gridID, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetGridFluxType(ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long, ByVal gridID As Long, ByRef Index As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  Index = atoGetGridFluxIndex(dsIdx, conIdx, progIdx, timeID, gridID)
  atoGetFluxType dsIdx, Index, name
End Sub

Public Sub atoGetGridOutputType(ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByVal timeID As Long, ByVal gridID As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetGridOutputType dsIdx, conIdx, progIdx, timeID, gridID, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetTimeSeriesXAxisUnit(ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByRef unit As String)
  retString = Space(MAXFIELD)
  DLLatoGetTimeSeriesXAxisUnit dsIdx, conIdx, progIdx, retString
  unit = StripTerminator(retString)
End Sub

Public Sub atoGetTimeSeriesYAxisUnit(ByVal dsIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByRef unit As String)
  retString = Space(MAXFIELD)
  DLLatoGetTimeSeriesYAxisUnit dsIdx, conIdx, progIdx, retString
  unit = StripTerminator(retString)
End Sub

Public Function bbfGetRunInfo(ByVal dsIdx As Long, ByRef info As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  bbfGetRunInfo = DLLbbfGetRunInfo(dsIdx, retString)
  info = StripTerminator(retString)
End Function

Public Function bbfGetSetInfo(ByVal dsIdx As Long, ByRef dsname As String, ByRef dstype As String)
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  bbfGetSetInfo = DLLbbfGetSetInfo(dsIdx, retString, retString1)
  dsname = StripTerminator(retString)
  dstype = StripTerminator(retString1)
End Function

Public Function bbfGetVULevel(ByVal dsIdx As Long, ByVal vuIdx As Long, ByRef variability As String, ByRef uncertainty As String)
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  bbfGetVULevel = DLLbbfGetVULevel(dsIdx, vuIdx, retString, retString1)
  variability = StripTerminator(retString)
  uncertainty = StripTerminator(retString1)
End Function

Public Function bbfGetOrganismName(ByVal dsIdx As Long, ByVal orgIdx As Long, ByRef name As String, ByRef id As String)
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  bbfGetOrganismName = DLLbbfGetOrganismName(dsIdx, orgIdx, retString, retString1)
  name = StripTerminator(retString)
  id = StripTerminator(retString1)
End Function

Public Function bbfGetContamName(ByVal dsIdx As Long, ByVal orgIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByRef name As String, ByRef cas As String)
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  bbfGetContamName = DLLbbfGetContamName(dsIdx, orgIdx, conIdx, progIdx, retString, retString1)
  name = StripTerminator(retString)
  cas = StripTerminator(retString1)
End Function

Public Function bbfGetSeriesProperties(ByVal dsIdx As Long, ByVal orgIdx As Long, ByVal conIdx As Long, ByVal progIdx As Long, ByRef concunits As String, ByRef timeunits As String)
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  bbfGetSeriesProperties = DLLbbfGetSeriesProperties(dsIdx, orgIdx, conIdx, progIdx, retString, retString1)
  concunits = StripTerminator(retString)
  timeunits = StripTerminator(retString1)
End Function

Public Function exfGetRunInfo(ByVal dsIdx As Long, ByRef info As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  exfGetRunInfo = DLLexfGetRunInfo(dsIdx, retString)
  info = StripTerminator(retString)
End Function

Public Function exfGetSetInfo(ByVal dsIdx As Long, ByRef dsname As String, ByRef dstype As String)
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  exfGetSetInfo = DLLexfGetSetInfo(dsIdx, retString, retString1)
  dsname = StripTerminator(retString)
  dstype = StripTerminator(retString1)
End Function

Public Function exfGetLocationName(ByVal dsIdx As Long, ByVal locIdx As Long, ByRef name As String, ByRef id As String)
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  exfGetLocationName = DLLexfGetLocationName(dsIdx, locIdx, retString, retString1)
  name = StripTerminator(retString)
  id = StripTerminator(retString1)
End Function

Public Function exfGetOrganismName(ByVal dsIdx As Long, ByVal orgIdx As Long, ByRef name As String, ByRef id As String)
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  exfGetOrganismName = DLLexfGetOrganismName(dsIdx, orgIdx, retString, retString1)
  name = StripTerminator(retString)
  id = StripTerminator(retString1)
End Function

Public Function exfGetContamName(ByVal dsIdx As Long, ByVal orgIdx As Long, ByVal conIdx As Long, ByRef name As String, ByRef cas As String)
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  exfGetContamName = DLLexfGetContamName(dsIdx, orgIdx, conIdx, retString, retString1)
  name = StripTerminator(retString)
  cas = StripTerminator(retString1)
End Function

Public Function exfGetSeriesProperties(ByVal dsIdx As Long, ByVal orgIdx As Long, ByVal conIdx As Long, ByVal efxIdx As Long, ByRef efxDes As String, ByRef efxunits As String, ByRef timeunits As String)
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  exfGetSeriesProperties = DLLexfGetSeriesProperties(dsIdx, orgIdx, conIdx, efxIdx, retString, retString1, retString2)
  efxDes = StripTerminator(retString)
  efxunits = StripTerminator(retString1)
  timeunits = StripTerminator(retString2)
End Function
