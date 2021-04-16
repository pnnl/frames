Attribute VB_Name = "FramesAPI"
Option Explicit
Option Compare Text

Type ContamList
  cas As String
  name As String
  nprog As Integer
  pcas() As String
  pname() As String
  ktype As Long
End Type

Type AllContamList
  cas As String
  name As String
  pcas As String
  pname As String
End Type

Public Const MAXFIELD = 4096

'//-------------------------------------------
'//----------- MISC. calls -------------------
'//-------------------------------------------
Public Declare Sub AddSeries Lib "Frames" (ByVal Num1 As Integer, ByRef Times1 As Single, ByRef Values1 As Single, ByVal Num2 As Long, ByRef Times2 As Single, ByRef Values2 As Single, ByRef NumRes As Long, ByRef TimesRes As Single, ByRef ValuesRes As Single)
Public Declare Sub GetProbability Lib "Frames" (ByVal x As Long, ByVal y As Long, ByVal count As Long, ByRef intimes As Single, ByRef invalues As Single, ByRef minval As Single, ByRef maxval As Single, ByRef timestart As Single, ByRef timeend As Single, ByRef svalue As Single, ByRef sprob As Single)
Public Declare Sub GetProbabilityRAGS Lib "Frames" (ByVal zeroes As Long, ByVal count As Long, ByRef intimes As Single, ByRef invalues As Single, ByRef attrib As Single, ByRef sprob As Single, ByRef svalue As Single)
'//-------------------------------------------
'//----------- AFF calls ---------------------
'//-------------------------------------------
Public Declare Sub affOpen Lib "Frames" (ByVal fname As String, ByVal Source As String)
Public Declare Sub affClose Lib "Frames" ()
Public Declare Function affGetNumSets Lib "Frames" () As Long
Public Declare Function affGetNumFluxTypes Lib "Frames" (ByVal nset As Long) As Long
Public Declare Function affGetNumContam Lib "Frames" (ByVal nset As Long) As Long
Public Declare Function affGetNumProgeny Lib "Frames" (ByVal nset As Long, ByVal ncon As Long) As Long
Public Declare Function affGetFlux Lib "Frames" (ByVal nset As Long, ByVal cont As Long, ByVal pt As Long) As Double
Public Declare Function affGetSeriesValues Lib "Frames" (ByVal nset As Long, ByVal ncon As Long, ByVal nprog As Long, ByVal ntype As Long, ByVal count As Long, ByRef times As Single, ByRef values As Single) As Long
Public Declare Function DLLaffGetSetName Lib "Frames" Alias "affGetSetName" (ByVal nset As Long, ByVal name As String) As Long
Public Declare Function DLLaffGetFluxType Lib "Frames" Alias "affGetFluxType" (ByVal nset As Long, ByVal nFlux As Long, ByVal ftype As String, ByRef fsize As Single, ByVal uom As String) As Long
Public Declare Function DLLaffGetContamName Lib "Frames" Alias "affGetContamName" (ByVal nset As Long, ByVal ncon As Long, ByVal nprog As Long, ByVal name As String, ByVal cas As String) As Long
Public Declare Function DLLaffGetSeriesProperties Lib "Frames" Alias "affGetSeriesProperties" (ByVal nset As Long, ByVal ncon As Long, ByVal nprog As Long, ByVal ntype As Long, ByVal units As String, ByVal timeunits As String) As Long
'//-------------------------------------------
'//----------- WFF calls ---------------------
'//-------------------------------------------
Public Declare Sub wffOpen Lib "Frames" (ByVal fname As String, ByVal Source As String)
Public Declare Sub wffClose Lib "Frames" ()
Public Declare Function wffGetFlux Lib "Frames" (ByVal media As Long, ByVal cont As Long, ByVal pt As Long) As Double
Public Declare Function wffGetNumMedia Lib "Frames" () As Long
Public Declare Function wffGetNumContam Lib "Frames" (ByVal nMed As Long) As Long
Public Declare Function wffGetNumProgeny Lib "Frames" (ByVal nMed As Long, ByVal ncon As Long) As Long
Public Declare Function wffGetConc Lib "Frames" (ByVal loc As Long, ByVal cont As Long, ByVal pt As Long) As Double
Public Declare Function wffGetSeriesValues Lib "Frames" (ByVal nMed As Long, ByVal ncon As Long, ByVal nprog As Long, ByVal count As Long, ByRef times As Single, ByRef values As Single) As Long
Public Declare Function wffGetMediaDimensions Lib "Frames" (ByVal nMed As Long, ByRef width As Single, ByRef length As Single, ByRef dist As Single, ByRef recharge As Single) As Long
'Public Declare Function wffGetSeriesValuesSum Lib "Frames" (ByVal nmed as Long, ByVal ncon as Long, ByVal nprog as Long, ByVal count as Long, float *times, float *values) as Long
Public Declare Function DLLwffGetMedia Lib "Frames" Alias "wffGetMedia" (ByVal nMed As Long, ByVal name As String) As Long
Public Declare Function DLLwffGetContamName Lib "Frames" Alias "wffGetContamName" (ByVal nMed As Long, ByVal ncon As Long, ByVal nprog As Long, ByVal name As String, ByVal cas As String) As Long
Public Declare Function DLLwffGetSeriesProperties Lib "Frames" Alias "wffGetSeriesProperties" (ByVal nMed As Long, ByVal ncon As Long, ByVal nprog As Long, ByVal units As String, ByVal timeunits As String) As Long
'//-------------------------------------------
'//----------- WCF calls ---------------------
'//-------------------------------------------
Public Declare Sub wcfOpen Lib "Frames" (ByVal fname As String, ByVal Source As String)
Public Declare Sub wcfClose Lib "Frames" ()
'  void   _export _stdcall wcfInit();
'  void   _export _stdcall wcfDelete();
'  void   _export _stdcall wcfWrite(char *tmpname);
'  void   _export _stdcall wcfSetNumLoc(int nloc);
'  void   _export _stdcall wcfSetLocation(int nloc, char *lname, char *ltype);
'  void   _export _stdcall wcfSetNumCon(int nloc, int ncon);
'  void   _export _stdcall wcfSetContaminant(int nloc, int ncon, char *cname, char *cas, int nprog);
'  void   _export _stdcall wcfSetSeriesCount(int nloc, int ncon, int nc);
'  void   _export _stdcall wcfSetSeriesValue(int nloc, int ncon, int n, double time, double value);
Public Declare Function wcfGetNumLoc Lib "Frames" () As Long
Public Declare Function wcfGetNumContam Lib "Frames" (ByVal nLoc As Long) As Long
Public Declare Function wcfGetNumProgeny Lib "Frames" (ByVal nLoc As Long, ByVal ncon As Long) As Long
Public Declare Function wcfGetSeriesValues Lib "Frames" (ByVal nLoc As Long, ByVal ncon As Long, ByVal nprog As Long, ByVal count As Long, ByRef times As Single, ByRef values As Single) As Long
Public Declare Function DLLwcfGetLocName Lib "Frames" Alias "wcfGetLocName" (ByVal nLoc As Long, ByVal name As String, ByVal ltype As String) As Long
Public Declare Function DLLwcfGetContamName Lib "Frames" Alias "wcfGetContamName" (ByVal nLoc As Long, ByVal ncon As Long, ByVal nprog As Long, ByVal name As String, ByVal cas As String) As Long
Public Declare Function DLLwcfGetSeriesProperties Lib "Frames" Alias "wcfGetSeriesProperties" (ByVal nLoc As Long, ByVal ncon As Long, ByVal nprog As Long, ByVal units As String, ByVal timeunits As String) As Long
'//-------------------------------------------
'//----------- SCF calls ---------------------
'//-------------------------------------------
Public Declare Sub scfOpen Lib "Frames" (ByVal fname As String, ByVal Source As String)
Public Declare Sub scfClose Lib "Frames" ()
Public Declare Function scfGetNumSets Lib "Frames" () As Long
Public Declare Function scfGetNumLoc Lib "Frames" (ByVal nset As Long) As Long
Public Declare Function scfGetNumContam Lib "Frames" (ByVal nset As Long, ByVal nLoc As Long) As Long
Public Declare Function scfGetNumProgeny Lib "Frames" (ByVal nset As Long, ByVal nLoc As Long, ByVal ncon As Long) As Long
Public Declare Function DLLscfGetSetName Lib "Frames" Alias "scfGetSetName" (ByVal nset As Long, ByVal name As String, ByVal ltype As String) As Long
Public Declare Function DLLscfGetLocName Lib "Frames" Alias "scfGetLocName" (ByVal nset As Long, ByVal nLoc As Long, ByVal name As String) As Long
Public Declare Function DLLscfGetContamName Lib "Frames" Alias "scfGetContamName" (ByVal nset As Long, ByVal nLoc As Long, ByVal ncon As Long, ByVal nprog As Long, ByVal name As String, ByVal cas As String) As Long
Public Declare Function DLLscfGetSeriesProperties Lib "Frames" Alias "scfGetSeriesProperties" (ByVal nset As Long, ByVal nLoc As Long, ByVal ncon As Long, ByVal nprog As Long, ByVal units As String, ByVal timeunits As String) As Long
Public Declare Function scfGetSeriesValues Lib "Frames" (ByVal nset As Long, ByVal nLoc As Long, ByVal ncon As Long, ByVal nprog As Long, ByVal count As Long, ByRef times As Single, ByRef values As Single) As Long
'//-------------------------------------------
'//----------- ATO calls ---------------------
'//-------------------------------------------
Public Declare Sub atoOpen Lib "Frames" (ByVal FUIName As String, ByVal Source As String)
Public Declare Sub atoClose Lib "Frames" ()
Public Declare Function atoGetDatasetCount Lib "Frames" () As Long
Public Declare Function atoGetConstituentCount Lib "Frames" (ByVal dsIndex As Long) As Long
Public Declare Function atoGetFluxCount Lib "Frames" (ByVal dsIndex As Long) As Long
Public Declare Sub DLLatoGetDatasetName Lib "Frames" Alias "atoGetDatasetName" (ByVal dsIndex As Long, ByVal name As String)
Public Declare Sub DLLatoGetDatasetModname Lib "Frames" Alias "atoGetDatasetModname" (ByVal dsIndex As Long, ByVal name As String)
Public Declare Sub DLLatoGetCoordinateType Lib "Frames" Alias "atoGetCoordinateType" (ByVal dsIndex As Long, ByVal name As String)
Public Declare Sub DLLatoGetReleaseType Lib "Frames" Alias "atoGetReleaseType" (ByVal dsIndex As Long, ByVal name As String)
Public Declare Sub DLLatoGetSpatialType Lib "Frames" Alias "atoGetSpatialType" (ByVal dsIndex As Long, ByVal name As String)
'//----------- ATO flux calls ----------------
Public Declare Sub DLLatoGetFluxType Lib "Frames" Alias "atoGetFluxType" (ByVal dsIndex As Long, ByVal fluxIndex As Long, ByVal name As String)
Public Declare Sub DLLatoGetFluxRadiusUnits Lib "Frames" Alias "atoGetFluxRadiusUnits" (ByVal dsIndex As Long, ByVal fluxIndex As Long, ByVal name As String)
Public Declare Sub DLLatoGetFluxDensityUnits Lib "Frames" Alias "atoGetFluxDensityUnits" (ByVal dsIndex As Long, ByVal fluxIndex As Long, ByVal name As String)
Public Declare Function atoGetFluxRadius Lib "Frames" (ByVal dsIndex As Long, ByVal fluxIndex As Long) As Double
'//----------- ATO constituent calls ---------
Public Declare Sub DLLatoGetCasID Lib "Frames" Alias "atoGetCasID" (ByVal dsIndex As Long, ByVal constituentID As Long, ByVal progID As Long, ByVal name As String)
Public Declare Sub DLLatoGetChemName Lib "Frames" Alias "atoGetChemName" (ByVal dsIndex As Long, ByVal constituentID As Long, ByVal progID As Long, ByVal name As String)
Public Declare Sub DLLatoGetParentCasID Lib "Frames" Alias "atoGetParentCasID" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal name As String)
Public Declare Sub DLLatoGetParentChemName Lib "Frames" Alias "atoGetParentChemName" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal name As String)
Public Declare Function atoGetProgenyCount Lib "Frames" (ByVal dsIndex As Long, ByVal constituentID As Long) As Long
'//----------- ATO time period calls ----------
Public Declare Sub DLLatoGetTimePeriodUnit Lib "Frames" Alias "atoGetTimePeriodUnit" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long, ByVal name As String)
Public Declare Function atoGetTimePeriodCount Lib "Frames" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long) As Long
Public Declare Function atoGetTimePeriodOutputCount Lib "Frames" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long) As Long
Public Declare Function atoGetTimePeriodTime Lib "Frames" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long) As Double
'//----------- ATO grid calls ----------------
Public Declare Sub DLLatoGetGridAxis1Unit Lib "Frames" Alias "atoGetGridAxis1Unit" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long, ByVal gridID As Long, ByVal name As String)
Public Declare Sub DLLatoGetGridAxis2Unit Lib "Frames" Alias "atoGetGridAxis2Unit" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long, ByVal gridID As Long, ByVal name As String)
Public Declare Sub DLLatoGetGridDataUnit Lib "Frames" Alias "atoGetGridDataUnit" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long, ByVal gridID As Long, ByVal name As String)
Public Declare Sub DLLatoGetGridMoist Lib "Frames" Alias "atoGetGridMoist" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long, ByVal gridID As Long, ByVal name As String)
Public Declare Sub DLLatoGetGridFluxType Lib "Frames" Alias "atoGetGridFluxType" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long, ByVal gridID As Long, ByVal name As String)
Public Declare Sub DLLatoGetGridOutputType Lib "Frames" Alias "atoGetGridOutputType" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long, ByVal gridID As Long, ByVal name As String)
Public Declare Function atoGetGridMax Lib "Frames" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long, ByVal gridID As Long, ByRef xCoord As Double, ByRef yCoord As Double) As Double
Public Declare Function atoGetGridMin Lib "Frames" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long, ByVal gridID As Long, ByRef xCoord As Double, ByRef yCoord As Double) As Double
Public Declare Function atoGetGridValue Lib "Frames" (ByVal dsID As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long, ByVal gridID As Long, ByVal axis1 As Long, ByVal axis2 As Long) As Single
Public Declare Function atoGetGridAxis1Count Lib "Frames" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long, ByVal gridID As Long) As Long
Public Declare Function atoGetGridAxis2Count Lib "Frames" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long, ByVal gridID As Long) As Long
Public Declare Function atoGetGridAxis1Value Lib "Frames" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long, ByVal gridID As Long, ByVal Index As Long) As Double
Public Declare Function atoGetGridAxis2Value Lib "Frames" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long, ByVal gridID As Long, ByVal Index As Long) As Double
'//----------- ATO timeseries calls---------------
Public Declare Function atoGetTimeSeries Lib "Frames" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal output As String, ByVal flux As String, ByVal moist As String, ByVal xCoord As Double, ByVal yCoord As Double) As Long
Public Declare Function atoGetTimeSeriesTime Lib "Frames" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal n As Long) As Double
Public Declare Function atoGetTimeSeriesValue Lib "Frames" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal n As Long) As Double
Public Declare Sub DLLatoGetTimeSeriesXAxisUnit Lib "Frames" Alias "atoGetTimeSeriesXAxisUnit" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal unit As String)
Public Declare Sub DLLatoGetTimeSeriesYAxisUnit Lib "Frames" Alias "atoGetTimeSeriesYAxisUnit" (ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal unit As String)
'//-------------------------------------------
'//----------- EPF calls ---------------------
'//-------------------------------------------
Public Declare Sub epfCloseDataset Lib "Frames" ()
Public Declare Sub epfGetRoutesAndPathways Lib "Frames" (ByVal nd As Long, ByVal nrt As Long, ByVal Route As String, ByVal Path As String, ByVal unit As String)
Public Declare Sub epfGetTimeSeries Lib "Frames" (ByVal nd As Long, ByVal n As Long, ByRef times As Single, ByRef values As Single)
Public Declare Function epfReadRoutesAndPathwaysByTime Lib "Frames" (ByVal nd As Long, ByVal casid As String, ByVal parentcas As String, ByVal timeindex As Long) As Long
Public Declare Function epfGetTimeCount Lib "Frames" (ByVal nd As Long, ByVal parent As String) As Long
Public Declare Function readEPFDatasets Lib "Frames" (ByVal FUIName As String, ByVal Source As String) As Long
Public Declare Function epfGetExposurePoint Lib "Frames" (ByVal nd As Long, ByVal ep As Long, ByRef xp As Long, ByRef yp As Long) As Long
Public Declare Function epfGetDatasetCounts Lib "Frames" (ByVal nd As Long, ByRef np As Long, ByVal loc As String, ByVal med As String) As Long
Public Declare Function epfReadRoutesAndPathways Lib "Frames" (ByVal nd As Long, ByVal casid As String, ByVal pcasid As String) As Long
Public Declare Function epfReadTimeSeries Lib "Frames" (ByVal nd As Long, ByVal cas As String, ByVal npt As Long, ByVal rte As String, ByVal Path As String, ByVal prog As String) As Long
'//-------------------------------------------
'//----------- RIF calls ---------------------
'//-------------------------------------------
Public Declare Sub rifCloseDataset Lib "Frames" ()
Public Declare Sub rifGetRoutesAndPathways Lib "Frames" (ByVal nd As Long, ByVal nrt As Long, ByVal Route As String, ByVal Path As String, ByVal unit As String, ByVal name As String)
Public Declare Sub rifGetTimeSeries Lib "Frames" (ByVal nd As Long, ByVal n As Long, ByRef times As Single, ByRef values As Single)
Public Declare Function rifGetTimeCount Lib "Frames" (ByVal nd As Long, ByVal NA As Long, ByVal parent As String) As Long
Public Declare Function rifReadRoutesAndPathwaysByTime Lib "Frames" (ByVal nd As Long, ByVal NA As Long, ByVal casid As String, ByVal parentcas As String, ByVal timeindex As Long) As Long
Public Declare Function readRIFDatasets Lib "Frames" (ByVal FUIName As String, ByVal Source As String) As Long
Public Declare Function rifGetExposurePoint Lib "Frames" (ByVal nd As Long, ByVal ep As Long, ByRef xp As Long, ByRef yp As Long) As Long
Public Declare Function rifGetDatasetCounts Lib "Frames" (ByVal nd As Long, ByRef np As Long, ByRef NA As Long, ByVal loc As String, ByVal med As String, ByVal dstype As String) As Long
Public Declare Function rifReadRoutesAndPathways Lib "Frames" (ByVal nd As Long, ByVal NA As Long, ByVal casid As String, ByVal pcasid As String) As Long
Public Declare Function rifGetAgeGroup Lib "Frames" (ByVal nd As Long, ByVal NA As Long, ByRef mina As Single, ByRef maxa As Single) As Long
Public Declare Function rifReadTimeSeries Lib "Frames" (ByVal nd As Long, ByVal NA As Long, ByVal cas As String, ByVal npt As Long, ByVal endpt As String, ByVal uom As String, ByVal rte As String, ByVal Path As String, ByVal prog As String) As Long
'//-------------------------------------------
'//----------- HIF calls ---------------------
'//-------------------------------------------
Public Declare Sub hifCloseDataset Lib "Frames" ()
Public Declare Sub hifGetCancerSites Lib "Frames" (ByVal nd As Long, ByVal ns As Long, ByVal lpStrings As String)
Public Declare Sub hifGetOrganSites Lib "Frames" (ByVal nd As Long, ByVal ns As Long, ByVal lpStrings As String)
Public Declare Sub hifGetTimeSeries Lib "Frames" (ByVal nd As Long, ByVal n As Long, ByRef times As Single, ByRef values As Single)
Public Declare Sub hifGetRoutesAndPathways Lib "Frames" (ByVal nd As Long, ByVal nrt As Long, ByVal Route As String, ByVal Path As String, ByVal unit As String, ByVal name As String)
Public Declare Sub hifGetTimeSeriesArray Lib "Frames" (ByVal nd As Long, ByRef times As Single, ByRef values As Single)
Public Declare Function hifChecksum Lib "Frames" () As Boolean
Public Declare Function readHIFDatasets Lib "Frames" (ByVal FUIName As String, ByVal Source As String) As Long
Public Declare Function hifGetExposurePoint Lib "Frames" (ByVal nd As Long, ByVal ep As Long, ByRef xp As Long, ByRef yp As Long) As Long
Public Declare Function hifGetAgeGroup Lib "Frames" (ByVal nd As Long, ByVal NA As Long, ByRef mina As Single, ByRef maxa As Single) As Long
Public Declare Function hifGetDatasetCounts Lib "Frames" (ByVal nd As Long, ByRef np As Long, ByRef NA As Long, ByRef nc As Long, ByRef no As Long, ByVal loc As String, ByVal med As String, ByVal dstype As String) As Long
Public Declare Function hifReadTimeSeries Lib "Frames" (ByVal nd As Long, ByVal age As Long, ByVal lpcas As String, ByVal npt As Long, ByVal organ As Long, ByVal endpt As String, ByVal epunit As String, ByVal rte As String, ByVal Path As String, ByVal prog As String) As Long
Public Declare Function hifReadRoutesAndPathways Lib "Frames" (ByVal nd As Long, ByVal NA As Long, ByVal casid As String, ByVal timeindex As Long, ByVal pcas As String) As Long
Public Declare Function hifGetOption1Data Lib "Frames" (ByVal nd As Long, ByVal NA As Long, ByVal start As Single, ByVal casid As String, ByVal pcasid As String, ByVal pathname As String, ByVal exprte As String, ByVal pointnum As Long, ByVal endpt As String, ByVal organ As Long) As Double
Public Declare Function LoadStartTimes Lib "Frames" (ByVal nd As Long, ByVal NA As Long, ByVal casid As String, ByVal tslen As Long, ByVal timestring As String) As Long
Public Declare Function hifGetHealthImpactsByTime Lib "Frames" (ByVal nd As Long, ByVal NA As Long, ByVal np As Long, ByVal organ As Long, ByVal timeidx As Long, ByVal cas As String, ByVal pcas As String, ByVal rte As String, ByVal pw As String, ByVal metric As String, ByVal ep As String, ByVal expdur As String) As Double
'//-------------------------------------------
'//----------- SUMMMM calls ---------------------
'//-------------------------------------------
Public Declare Function Initialize Lib "Frames" (ByVal lp1 As String, ByVal lp2 As String, ByVal lp3 As String, ByVal lp4 As String, ByVal lp5 As String) As Long
Public Declare Function Iterate Lib "Frames" (ByVal Iteration As Integer) As Long
Public Declare Function Finalize Lib "Frames" () As Long

Public cont() As ContamList
Public allCont() As ContamList

Dim retString As String * MAXFIELD
Dim retString1 As String * MAXFIELD

'//-------------------------------------------
'//----------- AFF calls ---------------------
'//-------------------------------------------
Public Function affGetSetName(ByVal nset As Long, ByRef name As String) As Long
  retString = Space(MAXFIELD)
  affGetSetName = DLLaffGetSetName(nset, retString)
  name = StripTerminator(retString)
End Function

Public Function affGetFluxType(ByVal nset As Long, ByVal nFlux As Long, ByRef name As String, ByRef size As Single, ByRef uom As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  affGetFluxType = DLLaffGetFluxType(nset, nFlux, retString, size, retString1)
  name = StripTerminator(retString)
  uom = StripTerminator(retString1)
End Function

Public Function affGetContamName(ByVal nset As Long, ByVal ncon As Long, ByVal nprog As Long, ByRef name As String, ByRef cas As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  affGetContamName = DLLaffGetContamName(nset, ncon, nprog, retString, retString1)
  name = StripTerminator(retString)
  cas = StripTerminator(retString1)
End Function

Public Function affGetSeriesProperties(ByVal nset As Long, ByVal ncon As Long, ByVal nprog As Long, ByVal nFlux As Long, ByRef units As String, ByRef timeunits As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  affGetSeriesProperties = DLLaffGetSeriesProperties(nset, ncon, nprog, nFlux, retString, retString1)
  units = StripTerminator(retString)
  timeunits = StripTerminator(retString1)
End Function
'//-------------------------------------------
'//----------- WFF calls ---------------------
'//-------------------------------------------
Public Function wffGetMediaName(ByVal nMed As Long, ByRef name As String) As Long
  name = Space(MAXFIELD)
  wffGetMediaName = DLLwffGetMedia(nMed, name)
  name = StripTerminator(name)
End Function

Public Function wffGetContamName(ByVal nMed As Long, ByVal ncon As Long, ByVal nprog As Long, ByRef name As String, ByRef cas As String) As Long
  name = Space(MAXFIELD)
  cas = Space(MAXFIELD)
  wffGetContamName = DLLwffGetContamName(nMed, ncon, nprog, name, cas)
  name = StripTerminator(name)
  cas = StripTerminator(cas)
End Function

Public Function wffGetSeriesProperties(ByVal nMed As Long, ByVal ncon As Long, ByVal nprog As Long, ByRef units As String, ByRef timeunits As String) As Long
  units = Space(MAXFIELD)
  timeunits = Space(MAXFIELD)
  wffGetSeriesProperties = DLLwffGetSeriesProperties(nMed, ncon, nprog, units, timeunits)
  units = StripTerminator(units)
  timeunits = StripTerminator(timeunits)
End Function

'//-------------------------------------------
'//----------- WCF calls ---------------------
'//-------------------------------------------
Public Function wcfGetLocName(ByVal nLoc As Long, ByRef name As String, ByRef ltype As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  wcfGetLocName = DLLwcfGetLocName(nLoc, retString, retString1)
  name = StripTerminator(retString)
  ltype = StripTerminator(retString1)
End Function

Public Function wcfGetContamName(ByVal nLoc As Long, ByVal ncon As Long, ByVal nprog As Long, ByRef name As String, ByRef cas As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  wcfGetContamName = DLLwcfGetContamName(nLoc, ncon, nprog, retString, retString1)
  name = StripTerminator(retString)
  cas = StripTerminator(retString1)
End Function

Public Function wcfGetSeriesProperties(ByVal nLoc As Long, ByVal ncon As Long, ByVal nprog As Long, ByRef units As String, ByRef timeunits As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  wcfGetSeriesProperties = DLLwcfGetSeriesProperties(nLoc, ncon, nprog, retString, retString1)
  units = StripTerminator(retString)
  timeunits = StripTerminator(retString1)
End Function

'//-------------------------------------------
'//----------- SCF calls ---------------------
'//-------------------------------------------
Public Function scfGetSetName(ByVal nset As Long, ByRef name As String, ByRef ltype As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  scfGetSetName = DLLscfGetSetName(nset, retString, retString1)
  name = StripTerminator(retString)
  ltype = StripTerminator(retString1)
End Function

Public Function scfGetLocName(ByVal nset As Long, ByVal nLoc As Long, ByRef name As String) As Long
  retString = Space(MAXFIELD)
  scfGetLocName = DLLscfGetLocName(nset, nLoc, retString)
  name = StripTerminator(retString)
End Function

Public Function scfGetContamName(ByVal nset As Long, ByVal nLoc As Long, ByVal ncon As Long, ByVal nprog As Long, ByRef name As String, ByRef cas As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  scfGetContamName = DLLscfGetContamName(nset, nLoc, ncon, nprog, retString, retString1)
  name = StripTerminator(retString)
  cas = StripTerminator(retString1)
End Function

Public Function scfGetSeriesProperties(ByVal nset As Long, ByVal nLoc As Long, ByVal ncon As Long, ByVal nprog As Long, ByRef units As String, ByRef timeunits As String) As Long
  retString = Space(MAXFIELD)
  retString1 = Space(MAXFIELD)
  scfGetSeriesProperties = DLLscfGetSeriesProperties(nset, nLoc, ncon, nprog, retString, retString1)
  units = StripTerminator(retString)
  timeunits = StripTerminator(retString1)
End Function

'//-------------------------------------------
'//----------- ATO calls ---------------------
'//-------------------------------------------
Public Sub atoGetDatasetName(ByVal dsIndex As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetDatasetName dsIndex, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetDatasetModname(ByVal dsIndex As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetDatasetModname dsIndex, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetCoordinateType(ByVal dsIndex As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetCoordinateType dsIndex, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetReleaseType(ByVal dsIndex As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetReleaseType dsIndex, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetSpatialType(ByVal dsIndex As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetSpatialType dsIndex, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetFluxType(ByVal dsIndex As Long, ByVal fluxIndex As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetFluxType dsIndex, fluxIndex, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetFluxRadiusUnits(ByVal dsIndex As Long, ByVal fluxIndex As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetFluxRadiusUnits dsIndex, fluxIndex, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetFluxDensityUnits(ByVal dsIndex As Long, ByVal fluxIndex As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetFluxDensityUnits dsIndex, fluxIndex, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetCasID(ByVal dsIndex As Long, ByVal constituentID As Long, ByVal progID As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetCasID dsIndex, constituentID, progID, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetChemName(ByVal dsIndex As Long, ByVal constituentID As Long, ByVal progID As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetChemName dsIndex, constituentID, progID, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetParentCasID(ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetParentCasID dsIndex, constID, progID, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetParentChemName(ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetParentChemName dsIndex, constID, progID, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetTimePeriodUnit(ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetTimePeriodUnit dsIndex, constID, progID, timeID, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetGridAxis1Unit(ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long, ByVal gridID As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetGridAxis1Unit dsIndex, constID, progID, timeID, gridID, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetGridAxis2Unit(ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long, ByVal gridID As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetGridAxis2Unit dsIndex, constID, progID, timeID, gridID, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetGridDataUnit(ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long, ByVal gridID As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetGridDataUnit dsIndex, constID, progID, timeID, gridID, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetGridMoist(ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long, ByVal gridID As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetGridMoist dsIndex, constID, progID, timeID, gridID, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetGridFluxType(ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long, ByVal gridID As Long, ByRef Index As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetGridFluxType dsIndex, constID, progID, timeID, gridID, retString
  Index = Val(StripTerminator(retString))
  atoGetFluxType dsIndex, Index, name
End Sub

Public Sub atoGetGridOutputType(ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByVal timeID As Long, ByVal gridID As Long, ByRef name As String)
  retString = Space(MAXFIELD)
  DLLatoGetGridOutputType dsIndex, constID, progID, timeID, gridID, retString
  name = StripTerminator(retString)
End Sub

Public Sub atoGetTimeSeriesXAxisUnit(ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByRef unit As String)
  retString = Space(MAXFIELD)
  DLLatoGetTimeSeriesXAxisUnit dsIndex, constID, progID, retString
  unit = StripTerminator(retString)
End Sub

Public Sub atoGetTimeSeriesYAxisUnit(ByVal dsIndex As Long, ByVal constID As Long, ByVal progID As Long, ByRef unit As String)
  retString = Space(MAXFIELD)
  DLLatoGetTimeSeriesYAxisUnit dsIndex, constID, progID, retString
  unit = StripTerminator(retString)
End Sub

' A handy function to get the constituent (contaminant) list
Function getContamList(cont() As ContamList) As Integer
  Dim i As Integer, j As Integer, k As Integer, l As Integer, m As Integer
  Dim prm As String
  Dim prefix As String
  Dim fle As parmfile
  Dim sval As Boolean
  Dim numcon As Integer
  Dim np As Integer
  Dim nrec As Integer
  Dim pvalue As String
  Dim nds As Integer
  Dim n As Integer
  Dim remap() As Integer
  Dim sol As String
  Dim temp As parmrec
 
  numcon = 0
  
  ReDim cont(0)
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
'     If read_parmrec(fle, temp) Then
      read_parmrec fle, temp
        Select Case temp.pname
          Case "fui"
            nrec = temp.idx1
            For m = 1 To nrec
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  Select Case temp.pname
                    Case "fscasid"
                      If temp.idx3 = 0 Then
                        If temp.idx2 > numcon Then
                          numcon = temp.idx2
                          ReDim Preserve cont(numcon)
                        End If
                        cont(temp.idx2).cas = temp.pval
                      Else
                        If temp.idx3 > cont(temp.idx2).nprog Then
                          cont(temp.idx2).nprog = temp.idx3
                          ReDim Preserve cont(temp.idx2).pcas(temp.idx3)
                          ReDim Preserve cont(temp.idx2).pname(temp.idx3)
                        End If
                        cont(temp.idx2).pcas(temp.idx3) = temp.pval
                      End If
                    Case "fscname"
                      If temp.idx3 = 0 Then
                        If temp.idx2 > numcon Then
                          numcon = temp.idx2
                          ReDim Preserve cont(numcon)
                        End If
                        cont(temp.idx2).name = temp.pval
                      Else
                        If temp.idx3 > cont(temp.idx2).nprog Then
                          cont(temp.idx2).nprog = temp.idx3
                          ReDim Preserve cont(temp.idx2).pcas(temp.idx3)
                          ReDim Preserve cont(temp.idx2).pname(temp.idx3)
                        End If
                        cont(temp.idx2).pname(temp.idx3) = temp.pval
                      End If
                    Case "clktype"
                      If temp.idx3 = 0 Then
                        If temp.idx2 > numcon Then
                          numcon = temp.idx2
                          ReDim Preserve cont(numcon)
                        End If
                        cont(temp.idx2).ktype = Val(temp.pval)
                      End If
                  End Select
                End If
              End If
            Next
          Case Else
            nrec = temp.idx1
            For m = 1 To nrec
              get_line fle.file
            Next
        End Select
'     End If
    Loop
    close_parm fle
    
  Else
    PutError "Can't find or open file " & FUIName
'   put_val errfile, "Can't find or open file " & FuiName
'   put_line errfile
'   close_csv errfile
    End
  End If
  
  getContamList = UBound(cont)

End Function

'Each entry in the list must be followed by a ',' including the last entry
Public Sub SetComboFromString(box As ComboBox, listitems As String)
    Dim i As Integer
    Dim temp As String
    Dim csv As Variant
    
    box.Clear
    csv = Split(listitems, ",")
    If 0 > UBound(csv) Then Exit Sub
    For i = 0 To UBound(csv)
      If csv(i) <> "" Then box.AddItem csv(i)
    Next i
    Exit Sub
    
    temp = ""
    For i = 1 To Len(listitems)
        If Mid(listitems, i, 1) = "," Then
            box.AddItem temp
            temp = ""
        Else
            temp = temp + Mid(listitems, i, 1)
        End If
    Next
End Sub

