Attribute VB_Name = "RunAPlume"
Option Explicit
Option Compare Text

Type desc
  name As String
  idx As Long
  Kind As Boolean
End Type

Type Confluxs
    name As String
    id As String
    Flux() As Double
    Chainindex As Long
End Type

Type Confluxset
    Parent As Confluxs
    NumProg As Long
    Prog() As Confluxs
End Type

Type Fluxset
    Kind As Long
    Radii As Double
    Dens As Double
End Type

Type AFFSet
    name As String
    numflux As Long
    Flux() As Fluxset
    Conflux() As Confluxset
    Numconc As Long
End Type

Type AFF
    numsets As Long
    AFFSet() As AFFSet
    SrcIndex As Long
End Type

Type NucVarbs
    name As String
    id As String
    indx As Long
    HalfLife As Double
    ParID1 As Long
    BrFct1 As Double
    ParID2 As Long
    BrFct2 As Double
End Type

Type Chain
    Parent As NucVarbs
    NumProg As Long
    Prog() As NucVarbs
End Type

Type Source
    name As String
    Kind As Long
    Rad As Double
    Ang As Double
    Area_Hgt As Double
    PlmFlg As Long
    St_Rad As Double
    St_Flow As Double
    ExTemp As Double
    DispFlg As Long
    IscFlg As Long
    IscWakeFlg As Long
    LowBoundFlg As Long
    BIDFlg As Long
    HghWndFlg As Long
    LowWndFlg As Long
    BldHgt(36) As Double
    BldWdth(36) As Double
    BldArea(36) As Double
End Type

Type RS_vals
    Numrad As Long
    Radius() As Double
    RecNum As Long
    RTX(2) As Double
    MinWind As Double
    SigParm As Long
    WindMin As Double
    ChgMod As Double
    CalmFlag As Long
    CalmDist(36) As Double
    St_Year As Long
    St_Month As Long
    St_Day As Long
    St_Hour As Long
    SectorFlag As Long
End Type

Type LstVals
    MetFile As String
    CShnFile As String
    RSFile As String
    OutFile As String
    NucDataFile As String
End Type

Type F_Source
    name As String
    Xval As Double
    Yval As Double
End Type

'Declare Function GetModuleUsage% Lib "Kernel" (ByVal hmodule%)

Global NumChain As Long
Global ChainData() As Chain
Global NumAFF As Long
Global AFFData() As AFF
Global NumSrc As Long
Global SourceData() As Source
Global RSData As RS_vals
Global LstData As LstVals

Global Const PcitoCi As Double = 3.1688E-20 'Convert PCi/yr to Ci/s

