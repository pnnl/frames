Attribute VB_Name = "RunAPuff"
Option Explicit
Option Compare Text

Type desc
  Name As String
  idx As Long
  Kind As Boolean
End Type

Type Confluxs
    Name As String
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
    Name As String
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
    Name As String
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
    Name As String
    Kind As Long
    X_val As Double
    Y_val As Double
    Area_Hgt As Double
    PlmFlg As Long
    St_Rad As Double
    St_Flow As Double
    ExTemp As Double
    SigR As Double
    SigZ As Double
End Type

Type RS_vals
    RecNum As Long
    RTX(2) As Double
    SigParm As Long
    WindMin As Double
    ChgMod As Double
    NumX As Long
    NumY As Long
    NodeDist As Double
    MaxPufRad As Double
    NumPufHr As Long
    SigRCoef As Double
    PufTimeStep As Long
    PufMergFlag As Long
    PufMergDist As Double
    TracRegFac As Double
    MinPufConc As Double
    St_Year As Long
    St_Month As Long
    St_Day As Long
    St_Hour As Long
End Type

Type GridVals
    GridFlag As Long
    GridVals(50, 50) As Double
End Type

Type LstVals
    MetFile As String
    CShnFile As String
    RSFile As String
    DomFile As String
    OutFile As String
    NucDataFile As String
End Type

Type F_Source
    Name As String
    Xval As Double
    Yval As Double
End Type

Global NumChain As Long
Global ChainData() As Chain
Global NumAFF As Long
Global AFFData() As AFF
Global NumSrc As Long
Global SourceData() As Source
Global RSData As RS_vals
Global LstData As LstVals
Global z0Grid As GridVals

Global Const PcitoCi As Double = 3.1688E-20 'Convert PCi/yr to Ci/s
