Attribute VB_Name = "ParamInfo"
Option Explicit
Option Compare Text


Type ParamInfoStruct
  sId As Integer
  pszName As String
  pszScreen As String
  sSeq As Integer
  pszDType As String
  pszCategory As String
  pszIntlUnit As String
  pszUserUnit As String
  pszPType As String
  ' defaults
  fOverride As Integer
  pszDefValue As String ' in internal units
' pszDefUnit As String
  sRow As Integer
' minRange As String
' maxRange As String
  StateVar As Integer
  range As String
End Type

Dim vptPInfo() As ParamInfoStruct
Type ListCatStruct
  category As String
  count As Integer
  list As String
End Type

Dim ListCat() As ListCatStruct

Function GetListCategoryValues(pszCategory$, catValue() As String, catText() As String) As Integer
Dim i As Integer
Dim j As Integer
Dim k As Integer
Dim StrArray() As String

  GetListCategoryValues = False

  For i = 1 To UBound(ListCat)
    If pszCategory = ListCat(i).category Then

      GetListCategoryValues = True
      
      ReDim catValue(ListCat(i).count)
      ReDim catText(ListCat(i).count)

      utlDecodeToList ListCat(i).list, k, StrArray()


      For j = 1 To ListCat(i).count
        k = 2 * (j - 1)
        catText(j) = StrArray(k)
        catValue(j) = StrArray(k + 1)
      Next
      Exit For
    End If
  Next
  
End Function
Function GetListCategoryCount(pszCategory$) As Integer
Dim i As Integer

  GetListCategoryCount = 0

  For i = 1 To UBound(ListCat)
    If pszCategory = ListCat(i).category Then
      GetListCategoryCount = ListCat(i).count
      Exit For
    End If
  Next
  
End Function

Sub utlDecodeToList(ByVal pszInp As String, sNum As Integer, rgpsz() As String)
'pszInp   - string to parse
'sNum     - output number of items (length of array)
'rgpsz    - output array of items
Dim sCount As Integer
Dim sPos As Integer      'position in string
Dim sNext As Integer     'next bar

  If Len(pszInp) = 0 Then
  'nothing in the string
  sNum = 0
  Exit Sub
  End If

  'count number of items
  sPos = 1
  sCount = 0
  Do While sPos > 0
  sPos = InStr(sPos, pszInp, "|")
  If (sPos <> 0) Then sPos = sPos + 1
  sCount = sCount + 1
  Loop

  'dimension the output array
  ReDim rgpsz(0 To sCount - 1)
  sNum = sCount

  'fill the array
  sPos = 1
  sNext = InStr(sPos, pszInp, "|")
  sCount = 0
  Do While sNext > 0
  rgpsz(sCount) = Mid(pszInp, sPos, sNext - sPos)
  sPos = sNext + 1
  sNext = InStr(sPos, pszInp, "|")
  sCount = sCount + 1
  Loop
  'put the last item in
  rgpsz(sCount) = Mid(pszInp, sPos)

End Sub

Sub utlDecodeToListv(ByVal pszDelim As String, ByVal pszInp As String, sNum As Integer, rgpsz() As String)
'pszInp   - string to parse
'sNum     - output number of items (length of array)
'rgpsz    - output array of items
Dim sCount As Integer
Dim sPos As Integer      'position in string
Dim sNext As Integer     'next bar

  If Len(pszInp) = 0 Then
  'nothing in the string
  sNum = 0
  Exit Sub
  End If

  'count number of items
  sPos = 1
  sCount = 0
  Do While sPos > 0
  sPos = InStr(sPos, pszInp, pszDelim)
  If (sPos <> 0) Then sPos = sPos + 1
  sCount = sCount + 1
  Loop

  'dimension the output array
  ReDim rgpsz(0 To sCount - 1)
  sNum = sCount

  'fill the array
  sPos = 1
  sNext = InStr(sPos, pszInp, pszDelim)
  sCount = 0
  Do While sNext > 0
  rgpsz(sCount) = Mid(pszInp, sPos, sNext - sPos)
  sPos = sNext + 1
  sNext = InStr(sPos, pszInp, pszDelim)
  sCount = sCount + 1
  Loop
  'put the last item in
  rgpsz(sCount) = Mid(pszInp, sPos)
  
End Sub

Sub InitListCatStructs()
Dim list As String
Dim sIndex As Integer

  ReDim ListCat(0)
  
  list = "Sand|1" & "|Loamy Sand|2" & "|Sandy Loam|3"
  list = list & "|Loam|4" & "|Silty Loam|5" & "|Silt|6"
  list = list & "|Sandy Clay Loam|7" & "|Clay Loam|8" & "|Silty Clay Loam|9"
  list = list & "|Sandy Clay|10" & "|Silty Clay|11" & "|Clay|12"
  UpdateListCategories "SoilClass", 12, list

  list = BOOL_YES & "|" & BOOL_YES & "|" & BOOL_NO & "|" & BOOL_NO
  UpdateListCategories "YesNo", 2, list
  
  list = BOOL_YES & "|" & "1" & "|" & BOOL_NO & "|" & "0"
  UpdateListCategories "YesNoBool", 2, list
  
  list = "none|none" & "|dirt|dirt" & "|paved|paved" & "|both|both"
  UpdateListCategories "Roads", 4, list
  
  UpdateListCategories "SourceMedia", 1, SourceMedia
  
  list = "Soil/AmendedSoil|soil"
  list = list & "|Glass|glass" & "|Cement (monolith)|grout"
  UpdateListCategories "WasteForm", 3, list
  
  list = "Type I|1" & "|Type II|2" & "|Type IA|3"
  UpdateListCategories "StormType", 3, list
  
  list = "0% <= x <= 1%|1" & "|1% < x <= 10%|2" & "|10 % < x <= 100%|3"
  UpdateListCategories "SurfaceCover", 3, list
  
  list = "Turn off loss route|0" & "|Known release rate(s)|" & OPT_REL
'FCR labeling change contaminant->constituent
'  list = list & "|Known contaminant flux|" & OPT_FLX & "|Compute loss route|3"
  list = list & "|Known constituent flux|" & OPT_FLX & "|Compute loss route|3"
  UpdateListCategories "OpModeInf", 4, list
  
  list = "Turn off loss route|0" & "|Known water erosion rate(s)|" & OPT_REL
'FCR labeling change contaminant->constituent
'  list = list & "|Known contaminant flux|" & OPT_FLX & "|Compute loss route|3"
  list = list & "|Known constituent flux|" & OPT_FLX & "|Compute loss route|3"
  UpdateListCategories "OpModeOvl", 4, list
  
  list = "Turn off loss route|0" & "|Known wind erosion rate(s)|" & OPT_REL
'FCR labeling change contaminant->constituent
'  list = list & "|Known contaminant flux|" & OPT_FLX & "|Compute loss route|3"
  list = list & "|Known constituent flux|" & OPT_FLX & "|Compute loss route|3"
  UpdateListCategories "OpModeSus", 4, list
  
'FCR labeling change contaminant->constituent
'  list = "Turn off loss route|0" & "|Known contaminant flux|" & OPT_FLX
  list = "Turn off loss route|0" & "|Known constituent flux|" & OPT_FLX
  list = list & "|Compute loss route|3"
  UpdateListCategories "OpModeVol", 3, list
  
'FCR labeling change contaminant->constituent
'  list = "Turn off pathway|0" & "|Known contaminant flux|" & OPT_FLX
  list = "Turn off pathway|0" & "|Known constituent flux|" & OPT_FLX
  UpdateListCategories "OpModeSrc", 2, list
  
  UpdateListCategories "StDecay", 2, "on|1|off|0"

  UpdateListCategories "AirType", 2, "area|area" & "|point|point"
  
End Sub

Function ListCategory(pszCategory$) As Integer
Dim i As Integer, pszList As String
  ListCategory = False
  For i = 1 To UBound(ListCat)
    If pszCategory = ListCat(i).category Then
      ListCategory = True
      Exit For
    End If
  Next
End Function

Function ListStructText(pszCategory$, pszValue$) As String
Dim i As Integer
Dim catValue() As String
Dim catText() As String

  ListStructText = ""
  If GetListCategoryValues(pszCategory, catValue(), catText()) Then
    For i = 1 To UBound(catValue)
      If catValue(i) = pszValue Then
        ListStructText = catText(i)
        Exit For
      End If
    Next
  End If
End Function

Function ListStructValue(pszCategory$, pszText$) As String
Dim i As Integer
Dim catValue() As String
Dim catText() As String

  ListStructValue = ""
  If GetListCategoryValues(pszCategory, catValue(), catText()) Then
    For i = 1 To UBound(catValue)
      If catText(i) = pszText Then
        ListStructValue = catValue(i)
        Exit For
      End If
    Next
  End If
End Function

Function ListStructValueForIndex(pszCategory$, Index%) As String
Dim catValue() As String
Dim catText() As String

  ListStructValueForIndex = ""
  If GetListCategoryValues(pszCategory, catValue(), catText()) Then
    If Index >= LBound(catValue) And Index <= UBound(catValue) Then
      ListStructValueForIndex = catValue(Index)
    End If
  End If
End Function

Sub UpdateListCategories(category$, count%, list$)
Dim i As Integer, Index As Integer

  Index = 1 + UBound(ListCat)
  
  For i = 1 To UBound(ListCat)
    If category = ListCat(i).category Then
      Index = i
    End If
  Next
  If Index > UBound(ListCat) Then ReDim Preserve ListCat(Index)
  
  ListCat(Index).category = category
  
  Dim scsv
  scsv = Split(list, "|")
  ListCat(Index).count = (1 + UBound(scsv)) / 2 ' count
  
  ListCat(Index).list = list
End Sub


Sub LoadParameterInfo()

  ' Options
  ConstructParamInfo "STMEDIA", "medium type for waste zone", "", "SourceMedia", "", "", "", "soil"
  ConstructParamInfo "STPOND", "ponded site", "bool", "YesNo", "", "", "", BOOL_NO
  ConstructParamInfo "STINF_OP", "leaching loss route", "int", "OpModeInf", "", "", "", "0"
  ConstructParamInfo "STOVL_OP", "overland runoff loss route", "int", "OpModeOvl", "", "", "", "0"
  ConstructParamInfo "STSUS_OP", "suspension loss route", "int", "OpModeSus", "", "", "", "0"
  ConstructParamInfo "STVOL_OP", "volatilization loss route", "int", "OpModeVol", "", "", "", "0"
  ConstructParamInfo "STSRC_OP", "known source/sink", "int", "OpModeSrc", "", "", "", "0"
' ConstructParamInfo "STREMSITE", "remediated site", "bool", "YesNo", "", "", "", BOOL_NO
' ConstructParamInfo "STWSTFRM", "form of the remediated waste", "", "WasteForm", "", "", "", "soil"
' ConstructParamInfo "STDEC_OP", "contaminant decay", "int", "StDecay", "", "", "", "1"
  ConstructParamInfo "STATYPE", "air release type", "int", "AirType", "", "", "", "area"
  ConstructParamInfo "STDELTA_T", "time interval for simulation", "float", "", "years", ".01 <= x <= 100", "", "1"
  ConstructParamInfo "STMAXTIME", "time period for simulation", "float", "", "years", "1 <= x <= 10000", "", "7000"
  ConstructParamInfo "STMINWST", "residual mass for simulation", "float", "", "fraction", "0.0001 < x <= 1", "", "0.01"
  ConstructParamInfo "STANNUAL", "output annual averages for length of run", "bool", "YesNoBool", "", "", "", BOOL_YES
    
  ' Monthly Climatology
  ConstructParamInfo "STTEMP", "Temperature measured at LCD", "float", "", "C", "-45 <= x <= 32", "", ""
  ConstructParamInfo "STMPRECIP", "monthly precipitation", "float", "distance (micro)", "cm", "0 <= x", "", ""
  ConstructParamInfo "STMNUMPRE", "number of monthly precipitation events", "int", "", "days", "0 <= x <= 31", "", ""
  ConstructParamInfo "STWINDV", "Mean monthly wind speed", "float", "velocity", "m/s", "0 <= x", "", ""
  ConstructParamInfo "STCLOUD", "average cloudiness or mean sky cover", "float", "ratio", "fraction", "0 <= x <= 1", "", ""
  ConstructParamInfo "STRHMAX", "maximum monthly relative humidity", "float", "", "%", "0 <= x <= 100", "", ""
  ConstructParamInfo "STRHMIN", "minimum monthly relative humidity", "float", "", "%", "0 <= x <= 100", "", ""
  
  ' Overland
  ConstructParamInfo "STOVLFLO", "discharge of water to overland", "float", "", "m^3/yr", "0 <= x", "", ""
  ConstructParamInfo "STSEDLOD", "sediment load to overland pathway", "float", "", "kg/yr", "0 <= x", "", ""
  ConstructParamInfo "STOVLFLX", "discharge as fraction of sw volume", "float", "ratio", "fraction/yr", "0 <= x <= 1", "", ""
  ConstructParamInfo "STSTORMI", "storm type index for location of waste site", "int", "StormType", "", "", "", "1"
  ConstructParamInfo "STPRECIP", "2-yr, 6-hr, precipitation event", "float", "distance (micro)", "cm", "0 <= x <= 12.7", "", ""
  ConstructParamInfo "STSLOPE", "slope of the waste site", "float", "", "percent", "0 <= x <= 100", "", ""
  ConstructParamInfo "STKFACTR", "soil erodibility factor", "float", "", "", ".02 <= x <= 1", "", ""
  ConstructParamInfo "STCFACTR", "vegetative cover factor", "float", "", "", ".01 <= x <= 1.5", "", ""
  ConstructParamInfo "STPFACTR", "erosion control practice factor", "float", "", "", "0.1 <= x <= 1.5", "", ""
  ConstructParamInfo "STSLENGTH", "length of site down slope", "float", "distance (macro)", "cm", "30.48 <= x <= 3048e+4", "", ""
  ConstructParamInfo "STSBULKD", "dry bulk density of surface soil", "float", "density", "g/cm^3", "1 <= x <= 3", "", "" ' also suspension
  
  ' Release site
  ConstructParamInfo "STCLEAN", "thickness of clean overburden", "float", "", "cm", "0 <= x <= 1000", "", ""
  ConstructParamInfo "STTHICK", "thickness", "float", "", "cm", "3.048 <= x <= 3048e+3", "", ""
  ConstructParamInfo "STLENGTH", "length", "float", "", "cm", "1 <= x <= 3048e+4", "", ""
  ConstructParamInfo "STWIDTH", "width", "float", "", "cm", "1 <= x <= 3048e+4", "", ""
  ConstructParamInfo "STZBULKD", "bulk density", "float", "", "g/cm^3", "1 <= x <= 3", "", ""
' ConstructParamInfo "STSSOL", "suspended solids concentration", "float", "", "g/cm^3", "1e-6 <= x", "", ""
  ConstructParamInfo "STSSOL", "suspended sediment", "float", "", "fraction", "0.0 <= x <= 0.1", "", "" ' 4/98 BLH
  ConstructParamInfo "STEFFPOR", "effective porosity", "float", "", "fraction", "0.001 <= x <= 1", "", ""
  ConstructParamInfo "STMOISTC", "moisture content", "float", "", "fraction", "0 <= x <= 1", "", ""
  ConstructParamInfo "STTOTPOR", "total porosity", "float", "", "fraction", ".001 <= x <= 1", "", ""
  ConstructParamInfo "STAIRSPC", "volumetric air content", "float", "", "fraction", "0 <= x <= 1", "", ""
  ConstructParamInfo "STAVTEMP", "average air temperature", "float", "temperature", "C", "-12 <= x <= 32", "", ""
  ConstructParamInfo "STAVWINDV", "mean annual wind speed", "float", "velocity", "m/s", ".45 <= x <= 22.35", "", ""
  ConstructParamInfo "STWINDHT", "height above ground of local wind measure", "float", "", "m", ".3048 <= x <= 152.4", "", ""
' ConstructParamInfo "STMIXDEP", "mixing layer depth for surface concentrations", "float", "", "m", ".001 <= x <= 5.0", "", ""

  ' suspension
  ConstructParamInfo "STRELHT", "source exit height", "float", "", "m", "0 <= x", "", ""
  ConstructParamInfo "STSTRUCHT", "adjacent structure height", "float", "", "m", "0 <= x", "", ""
  ConstructParamInfo "STEXVEL", "source exit velocity", "float", "", "m/s", "0 <= x", "", ""
  ConstructParamInfo "STEXTEMP", "source exit temperature", "float", "", "C", "0 <= x", "", ""
' ConstructParamInfo "STSBULKD", "dry bulk density of surface soil", "float", "density", "g/cm^3", "1 <= x <= 3", "", "" ' also overland
  ConstructParamInfo "STSAND", "sand in the surface soil", "float", "", "%", "0 <= x <= 100", "", ""
  ConstructParamInfo "STCORRSC", "fraction of surface cover", "float", "SurfaceCover", "", "", "", "1"
  ConstructParamInfo "STLOCSUR", "surface roughness length", "float", "Roughness", "m", "0 <= x <= 10", "", ""
  ConstructParamInfo "STVEGFR", "surface area covered with vegetation", "float", "", "fraction", "0 <= x <= 1", "", ""
  ConstructParamInfo "STCRUST", "surface area covered with a crust layer", "float", "", "fraction", "0 <= x <= 1", "", ""
  ConstructParamInfo "STNUMDIS", "Number of mechanical disturbances to site", "float", "", "#/month", "0 <= x <= 31", "", ""
  ConstructParamInfo "STMAXWIND", "maximum wind speed at site", "float", "velocity", "m/s", ".45 <= x <= 67", "", ""
  ConstructParamInfo "STPEI", "Thornwaite's Precipitation-Evaporation index", "int", "", "", "6 <= x <= 318", "", ""
  ConstructParamInfo "STNUMPRCP", "# of days/yr with >0.254mm precipitation", "float", "days", "", "0 <= x <= 365", "", "" ' also weather station
  ConstructParamInfo "STROADS", "Is there roadway travel at the site", "bool", "Roads", "", "", "", "none"
  ConstructParamInfo "STRTDIST", "Distance of roadway traveled", "float", "", "km", "0 <= x <= 10", "", ""
  ConstructParamInfo "STVSPEED", "Average speed of vehicle per trip", "float", "velocity", "km/hr", "0 <= x <= 999", "", ""
  ConstructParamInfo "STVWEIGH", "Average weight of vehicles", "float", "mass", "ton", "0 <= x <= 99", "", ""
  ConstructParamInfo "STRTNUM", "Number of round-trips per day", "float", "", "#/day", "0 <= x <= 9999", "", ""
  ConstructParamInfo "STSILT", "Percent of silt on road surface", "float", "", "%", "0 <= x <= 100", "", ""
  ConstructParamInfo "STWHEELS", "Average number of vehicle wheels", "float", "", "#", "0 <= x <= 20", "", ""
  
  ' weather station
  ConstructParamInfo "STLCDELEV", "elevation of LCD Station", "float", "distance (macro)", "m", "-15.24 <= x <= 3048", "", ""
  ConstructParamInfo "STLAT", "latitude of waste zone", "float", "", "deg", "0 <= x <= 90", "", ""
  ConstructParamInfo "STELEV", "elevation of waste zone", "float", "", "m", "-15.24 <= x <= 3048", "", ""
  ConstructParamInfo "STSCSCN", "SCS curve number", "integer", "", "", "0 <= x <= 100", "", ""
  ConstructParamInfo "STAVAILW", "Top soil water capacity", "Float", "distance (macro)", "cm", "0 <= x <= 76", "76", ""
  ConstructParamInfo "STNUMPRCP", "# of days/yr with >0.254mm precipitation", "float", "days", "", "0 <= x <= 365", "", "" ' also suspension
  
  ' contaminant
  ConstructParamInfo "STGHALF", "decay/degradation half life", "float", "", "day", "0 <= x", "", ""
  ConstructParamInfo "STSOL", "water solubility", "float", "", "mg/L", "0 <= x", "", ""
'FCR labeling change contaminant->constituent
'  ConstructParamInfo "STINVEN", "contaminant inventory", "float", "mass", "g", "0 <= x", "", ""
  ConstructParamInfo "STINVEN", "constituent inventory", "float", "mass", "g", "0 <= x", "", ""
  ConstructParamInfo "STVOLRAT", "fraction of volatilization release", "float", "ratio", "fraction", "0 <= x <= 1", "1", ""
  ConstructParamInfo "STKD", "equilibrium coefficient Kd", "float", "", "ml/g", "0 <= x", "", ""
  ConstructParamInfo "STKD_NUM", "number of kds", "int", "", "", "2 <= x <= 10", "", ""
  ConstructParamInfo "STKD_TIM", "time of kd", "float", "", "yr", "0 <= x", "", ""
  ConstructParamInfo "STCONC", "concentration", "float", "mass", "g/g", "0 <= x", "", ""
  ConstructParamInfo "STCONCTYPE", "concentration type", "int", "", "", "1 <= x <= 5", "", ""

  ' known flux contaminant
'FCR labeling change contaminant->constituent
'  ConstructParamInfo "STLEACH", "known contaminant leaching flux rates", "float", "", "g/yr or Ci/yr", "0 <= x", "", ""
  ConstructParamInfo "STLEACH", "known constituent leaching flux rates", "float", "", "g/yr or Ci/yr", "0 <= x", "", ""
  ConstructParamInfo "STLEACH_NUM", "number of leach rates", "int", "", "", "2 <= x <= 10", "", ""
  ConstructParamInfo "STLEACH_TIM", "time of leach rate", "float", "", "yr", "0 <= x", "", ""
  
'FCR labeling change contaminant->constituent
'  ConstructParamInfo "STOVL", "known contaminant overland flux rates", "float", "", "g/yr or Ci/yr", "0 <= x", "", ""
  ConstructParamInfo "STOVL", "known constituent overland flux rates", "float", "", "g/yr or Ci/yr", "0 <= x", "", ""
  ConstructParamInfo "STOVL_NUM", "number of overland rates", "int", "", "", "2 <= x <= 10", "", ""
  ConstructParamInfo "STOVL_TIM", "time of overland rate", "float", "", "yr", "0 <= x", "", ""
  
  ConstructParamInfo "STSUSP", "known wind suspension flux rates", "float", "", "g/yr", "0 <= x", "", ""
  ConstructParamInfo "STSUSP_NUM", "number of suspensionrates", "int", "", "", "2 <= x <= 10", "", ""
  ConstructParamInfo "STSUSP_TIM", "time of suspension rate", "float", "", "yr", "0 <= x", "", ""
  
'FCR labeling change contaminant->constituent
'  ConstructParamInfo "STVOLAT", "known contaminant volatilization flux rates", "float", "", "g/yr or Ci/yr", "0 <= x", "", ""
  ConstructParamInfo "STVOLAT", "known constituent volatilization flux rates", "float", "", "g/yr or Ci/yr", "0 <= x", "", ""
  ConstructParamInfo "STVOLAT_NUM", "number of volatilization rates", "int", "", "", "2 <= x <= 10", "", ""
  ConstructParamInfo "STVOLAT_TIM", "time of volatilization rate", "float", "", "yr", "0 <= x", "", ""
  
  ConstructParamInfo "STSOURCE", "known flux rates for a source/sink to the waste zone", "float", "", "g/yr or Ci/yr", "", "", ""
  ConstructParamInfo "STSOURCE_NUM", "number of flux rates", "int", "", "", "2 <= x <= 10", "", ""
  ConstructParamInfo "STSOURCE_TIM", "time of flux rate", "float", "", "yr", "0 <= x", "", ""
  
  ' known release
  ConstructParamInfo "STDARCY", "darcy infiltration rate", "float", "hydraulic conductivity", "cm/yr", "0 <= x", "", ""
  ConstructParamInfo "STDARCY_NUM", "number of darcy infiltration rates", "int", "", "", "2 <= x <= 10", "", ""
  ConstructParamInfo "STDARCY_TIM", "time of darcy infiltration rate", "float", "", "yr", "0 <= x", "", ""
  
' ConstructParamInfo "STWATR", "Water Erosion Rate", "float", "erosion rate", "cm/yr", "0 <= x", "", ""
  ConstructParamInfo "STWATR", "soil depth lost to water erosion", "float", "erosion rate", "cm/yr", "0 <= x", "", ""
  ConstructParamInfo "STWATR_NUM", "number of water erosion rates", "int", "", "", "2 <= x <= 10", "", ""
  ConstructParamInfo "STWATR_TIM", "time of water erosion rate", "float", "", "yr", "0 <= x", "", ""
  
  ConstructParamInfo "STWIND", "wind erosion rate", "float", "rate", "cm/yr", "0 <= x", "", ""
  ConstructParamInfo "STWIND_NUM", "number of wind erosion rates", "int", "", "", "2 <= x <= 10", "", ""
  ConstructParamInfo "STWIND_TIM", "time of wind erosion rate", "float", "", "yr", "0 <= x", "", ""
  
  ' for kd estimation
  ConstructParamInfo "KDCLASS", "soil-texture classification", "int", "SoilClass", "", "0 <= x", "", ""
  ConstructParamInfo "KDSAND", "% sand of soil", "float", "", "%", "0 <= x <= 100", "", ""
  ConstructParamInfo "KDSILT", "% silt of soil", "float", "", "%", "0 <= x <= 100", "", ""
  ConstructParamInfo "KDCLAY", "% clay of soil", "float", "", "%", "0 <= x <= 100", "", ""
  ConstructParamInfo "KDOMC", "% organic matter content of soil", "float", "", "%", "0 <= x <= 10", "", ""
  ConstructParamInfo "KDIRON", "% iron and aluminum in the soil", "float", "", "%", "0 <= x <= 99", "", ""
  ConstructParamInfo "KDPH", "pH of top soil", "float", "", "", ".1 <= x <= 14", "", ""

End Sub



Sub ConstructParamInfo(parameter$, prompt$, dtype$, category$, intlunits$, minvalue$, maxvalue$, typvalue$)
Dim CT As Integer
Dim ndx As Integer
Dim i As Integer
  ndx = 0
  For i = 1 To UBound(vptPInfo)
    If vptPInfo(i).pszName = parameter Then
      ndx = i
      Exit For
    End If
  Next
  If ndx = 0 Then
    ndx = 1 + UBound(vptPInfo)
    ReDim Preserve vptPInfo(ndx)
  End If
  
  vptPInfo(ndx).pszName = parameter
  vptPInfo(ndx).pszScreen = prompt
  vptPInfo(ndx).pszDType = dtype
  vptPInfo(ndx).pszCategory = category
  vptPInfo(ndx).pszIntlUnit = intlunits
  vptPInfo(ndx).pszUserUnit = intlunits
  vptPInfo(ndx).pszDefValue = typvalue
' vptPInfo(ndx).minRange = minvalue
' vptPInfo(ndx).maxRange = maxvalue
  vptPInfo(ndx).range = minvalue
 
End Sub



'Replace characters in a string with given characters
'It returns the new string
Function utlReplaceChar(pszInp$, pszFrom$, pszTo$) As String
'pszInp  -   string to replace characters in
'pszFrom -   character to replace
'pszTo   -   new character

Dim sPos As Integer     'position of character to replace
Dim pszNew As String    'new string

  'make sure something is in the string
  If (Len(pszInp) = 0) Then
  utlReplaceChar = ""
  Exit Function
  End If

  sPos = 1
  pszNew = pszInp
  sPos = InStr(sPos, pszNew, pszFrom)
  Do While sPos > 0
  Mid$(pszNew, sPos, 1) = pszTo
  sPos = InStr(sPos, pszNew, pszFrom)
  Loop
  utlReplaceChar = pszNew
End Function

Function GetParamInfoIndex(paramId$) As Integer
Dim i As Integer, sRc As Integer, pszIn As String, pszOut As String
Dim pType As Integer

  GetParamInfoIndex = 0
  
  For i = 1 To UBound(vptPInfo)
    If paramId = vptPInfo(i).pszName Then
      GetParamInfoIndex = i
      Exit Function
    End If
  Next

End Function

Function GetParamInfoStruct(pname$, pStruct As ParamInfoStruct) As Integer
Dim ndx As Integer
  ndx = GetParamInfoIndex(pname)
  If ndx = 0 Then
    MsgBox "Missing parameter info for " + pname
  End If
  pStruct = vptPInfo(ndx)
  GetParamInfoStruct = ndx
End Function

Sub InitParamInfo()
  ReDim vptPInfo(0)
End Sub

Sub InitParamInfoStruct()
  ReDim vptPInfo(0)
  vptPInfo(0).pszName = "Undefined"
  vptPInfo(0).pszScreen = "Missing Parameter Information..."
End Sub

Function ParamInfoCount() As Integer
  ParamInfoCount = UBound(vptPInfo)
End Function


Sub SetParamInfoElem(Index%, paramId$, item%, value)

  If Index > 0 And Index <= ParamInfoCount() Then
    If paramId <> vptPInfo(Index).pszName Then
      Index = GetParamInfoIndex(paramId)
    End If
  Else
    Index = GetParamInfoIndex(paramId)
  End If
  
  Select Case item
    Case E_NAME
      vptPInfo(Index).pszName = value
    Case E_SCREEN
      vptPInfo(Index).pszScreen = value
    Case E_SEQ
      vptPInfo(Index).sSeq = value
    Case E_DTYPE
      vptPInfo(Index).pszDType = value
    Case E_CATEGORY
      vptPInfo(Index).pszCategory = value
    Case E_INTLUNIT
      vptPInfo(Index).pszIntlUnit = value
    Case E_USERUNIT
      vptPInfo(Index).pszUserUnit = value
    Case E_PTYPE
      vptPInfo(Index).pszPType = value
    Case E_ORIDE
      vptPInfo(Index).fOverride = value
    Case E_DEFVAL
      vptPInfo(Index).pszDefValue = value
    Case E_SSROW
      vptPInfo(Index).sRow = value
    Case E_STATEVAR
      vptPInfo(Index).StateVar = value
    Case E_RANGE
      vptPInfo(Index).range = value
  End Select
  Exit Sub

End Sub




Function GetParamInfoElem(Index%, paramId$, item%) As Variant

  If Index > 0 And Index <= ParamInfoCount() Then
    If paramId <> vptPInfo(Index).pszName Then
      Index = GetParamInfoIndex(paramId)
    End If
  Else
    Index = GetParamInfoIndex(paramId)
  End If

  Select Case item
    Case E_NAME
      GetParamInfoElem = vptPInfo(Index).pszName
    Case E_SCREEN
      GetParamInfoElem = vptPInfo(Index).pszScreen
    Case E_SEQ
      GetParamInfoElem = vptPInfo(Index).sSeq
    Case E_DTYPE
      GetParamInfoElem = vptPInfo(Index).pszDType
    Case E_CATEGORY
      GetParamInfoElem = vptPInfo(Index).pszCategory
    Case E_INTLUNIT
      If vptPInfo(Index).pszIntlUnit = "unitless" Or vptPInfo(Index).pszIntlUnit = "n/a" Then
        GetParamInfoElem = ""
      Else
        GetParamInfoElem = vptPInfo(Index).pszIntlUnit
      End If
    Case E_USERUNIT
      If vptPInfo(Index).pszUserUnit = "" Then
        GetParamInfoElem = vptPInfo(Index).pszIntlUnit
      Else
        GetParamInfoElem = vptPInfo(Index).pszUserUnit
      End If
    Case E_PTYPE
      GetParamInfoElem = vptPInfo(Index).pszPType
    Case E_ORIDE
      GetParamInfoElem = vptPInfo(Index).fOverride
    Case E_DEFVAL
      GetParamInfoElem = vptPInfo(Index).pszDefValue
    Case E_SSROW
      GetParamInfoElem = vptPInfo(Index).sRow
    Case E_RANGE
      GetParamInfoElem = vptPInfo(Index).range
    Case E_STATEVAR
      GetParamInfoElem = vptPInfo(Index).StateVar
  End Select
  
End Function


Sub SetPathwayModeOptions(media$)
Dim kflx_c As Integer, kflx As String
Dim noop_c As Integer, noop As String
Dim cinf_c As Integer, cinf As String
Dim cdis_c As Integer, cdis As String
Dim comp_c As Integer, comp As String
Dim cero_c As Integer, cero As String
Dim cdar_c As Integer, cdar As String
Dim cswi_c As Integer, cswi As String


'FCR labeling change contaminant->constituent
'  kflx_c = 2: kflx = "Turn off pathway|0|Known contaminant flux|" & OPT_FLX
  kflx_c = 2: kflx = "Turn off pathway|0|Known constituent flux|" & OPT_FLX
  noop_c = 1: noop = "Turn off pathway|0"
  
  If ExecMode = COMPUTE_ONLY Then
    cswi_c = 2: cswi = "Turn off pathway|0|Known infiltration rate|" & OPT_REL
    cinf_c = 3: cinf = "Turn off pathway|0|Known infiltration rate|" & OPT_REL & "|Compute pathwayef|3"
    cdis_c = 2: cdis = "Turn off pathway|0|Known discharge to ovl|" & OPT_REL
    comp_c = 2: comp = "Turn off pathway|0|Compute pathway|3"
    cero_c = 3: cero = "Turn off pathway|0|Known erosion rate|" & OPT_REL & "|Compute pathway|3"
    cdar_c = 2: cdar = "Turn off pathway|0|Known darcy velocity|" & OPT_REL
  End If
  
  ' 5/98 BLH
  ' restore known contaminant flux as an option per Requirements for MEPAS 4.0
  '     however, check that all pathways are not know
  If ExecMode = COMBINED Then
'FCR labeling change contaminant->constituent
'    cswi_c = 3: cswi = "Turn off pathway|0|Known infiltration rate|" & OPT_REL _
'                          & "|Known contaminant flux|" & OPT_FLX
    cswi_c = 3: cswi = "Turn off pathway|0|Known infiltration rate|" & OPT_REL _
                          & "|Known constituent flux|" & OPT_FLX
'    cinf_c = 4: cinf = "Turn off pathway|0|Known infiltration rate|" & OPT_REL _
'                          & "|Known contaminant flux|" & OPT_FLX & "|Compute pathway|3"
    cinf_c = 4: cinf = "Turn off pathway|0|Known infiltration rate|" & OPT_REL _
                          & "|Known constituent flux|" & OPT_FLX & "|Compute pathway|3"
'    cdis_c = 3: cdis = "Turn off pathway|0|Known discharge to ovl|" & OPT_REL _
'                          & "|Known contaminant flux|" & OPT_FLX
    cdis_c = 3: cdis = "Turn off pathway|0|Known discharge to ovl|" & OPT_REL _
                          & "|Known constituent flux|" & OPT_FLX
'    comp_c = 3: comp = "Turn off pathway|0|" & "Known contaminant flux|" & OPT_FLX & "|Compute pathway|3"
    comp_c = 3: comp = "Turn off pathway|0|" & "Known constituent flux|" & OPT_FLX & "|Compute pathway|3"
'    cero_c = 4: cero = "Turn off pathway|0|Known erosion rate|" & OPT_REL _
'                          & "|Known contaminant flux|" & OPT_FLX & "|Compute pathway|3"
    cero_c = 4: cero = "Turn off pathway|0|Known erosion rate|" & OPT_REL _
                          & "|Known constituent flux|" & OPT_FLX & "|Compute pathway|3"
'    cdar_c = 3: cdar = "Turn off pathway|0|Known darcy velocity|" & OPT_REL _
'                          & "|Known contaminant flux|" & OPT_FLX
    cdar_c = 3: cdar = "Turn off pathway|0|Known darcy velocity|" & OPT_REL _
                          & "|Known constituent flux|" & OPT_FLX
  End If
  
  If InStr(media, "surface") Then
    Select Case ExecMode
      Case KNOWN_ONLY:
        UpdateListCategories "OpModeInf", kflx_c, kflx
        UpdateListCategories "OpModeOvl", kflx_c, kflx
        UpdateListCategories "OpModeSus", noop_c, noop
        UpdateListCategories "OpModeVol", kflx_c, kflx
        UpdateListCategories "OpModeSrc", kflx_c, kflx
      Case COMPUTE_ONLY:
        UpdateListCategories "OpModeInf", cswi_c, cswi
        UpdateListCategories "OpModeOvl", cdis_c, cdis
        UpdateListCategories "OpModeSus", noop_c, noop
        UpdateListCategories "OpModeVol", comp_c, comp
        UpdateListCategories "OpModeSrc", noop_c, noop
      Case COMBINED:
        UpdateListCategories "OpModeInf", cswi_c, cswi
        UpdateListCategories "OpModeOvl", cdis_c, cdis
        UpdateListCategories "OpModeSus", noop_c, noop
        UpdateListCategories "OpModeVol", comp_c, comp
        UpdateListCategories "OpModeSrc", kflx_c, kflx
    End Select
  End If
  If InStr(media, "soil") Then
    Select Case ExecMode
      Case KNOWN_ONLY:
        UpdateListCategories "OpModeInf", kflx_c, kflx
        UpdateListCategories "OpModeOvl", kflx_c, kflx
        UpdateListCategories "OpModeSus", kflx_c, kflx
        UpdateListCategories "OpModeVol", kflx_c, kflx
        UpdateListCategories "OpModeSrc", kflx_c, kflx
      Case COMPUTE_ONLY:
        UpdateListCategories "OpModeInf", cinf_c, cinf
        UpdateListCategories "OpModeOvl", cero_c, cero
        UpdateListCategories "OpModeSus", cero_c, cero
        UpdateListCategories "OpModeVol", comp_c, comp
        UpdateListCategories "OpModeSrc", noop_c, noop
      Case COMBINED:
        UpdateListCategories "OpModeInf", cinf_c, cinf
        UpdateListCategories "OpModeOvl", cero_c, cero
        UpdateListCategories "OpModeSus", cero_c, cero
        UpdateListCategories "OpModeVol", comp_c, comp
        UpdateListCategories "OpModeSrc", kflx_c, kflx
    End Select
  End If
  If InStr(media, "aquifer") Then
    Select Case ExecMode
      Case KNOWN_ONLY:
        UpdateListCategories "OpModeInf", kflx_c, kflx
        UpdateListCategories "OpModeOvl", noop_c, noop
        UpdateListCategories "OpModeSus", noop_c, noop
        UpdateListCategories "OpModeVol", noop_c, noop
        UpdateListCategories "OpModeSrc", kflx_c, kflx
      Case COMPUTE_ONLY:
        UpdateListCategories "OpModeInf", cdar_c, cdar
        UpdateListCategories "OpModeOvl", noop_c, noop
        UpdateListCategories "OpModeSus", noop_c, noop
        UpdateListCategories "OpModeVol", noop_c, noop
        UpdateListCategories "OpModeSrc", noop_c, noop
      Case COMBINED:
        UpdateListCategories "OpModeInf", cdar_c, cdar
        UpdateListCategories "OpModeOvl", noop_c, noop
        UpdateListCategories "OpModeSus", noop_c, noop
        UpdateListCategories "OpModeVol", noop_c, noop
        UpdateListCategories "OpModeSrc", kflx_c, kflx
    End Select
  End If
  If IsSecondarySource Then UpdateListCategories "OpModeSrc", 1, "Use Secondary Sources|" & OPT_EXT

End Sub
