VERSION 5.00
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form frmConc 
   Caption         =   "Inventory from Concentrations Worksheet"
   ClientHeight    =   5712
   ClientLeft      =   6180
   ClientTop       =   5136
   ClientWidth     =   6972
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5712
   ScaleWidth      =   6972
   Begin VB.Frame Frame1 
      Caption         =   "User Defined"
      Height          =   555
      Left            =   225
      TabIndex        =   14
      Top             =   750
      Width           =   5160
      Begin VB.OptionButton optUDef 
         Caption         =   "Concentration"
         Height          =   225
         Index           =   1
         Left            =   2910
         TabIndex        =   2
         Top             =   195
         Width           =   1305
      End
      Begin VB.OptionButton optUDef 
         Caption         =   "Inventory"
         Height          =   225
         Index           =   0
         Left            =   1515
         TabIndex        =   1
         Top             =   195
         Width           =   1305
      End
   End
   Begin VB.CommandButton cmdCancel 
      Caption         =   "&Cancel"
      Height          =   375
      Left            =   5640
      TabIndex        =   10
      Top             =   1440
      Width           =   1215
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "&OK"
      Height          =   375
      Left            =   5655
      TabIndex        =   9
      Top             =   960
      Width           =   1215
   End
   Begin Threed.SSFrame SSFrame2 
      Height          =   3015
      Left            =   240
      TabIndex        =   13
      Top             =   2415
      Width           =   5175
      _Version        =   65536
      _ExtentX        =   9128
      _ExtentY        =   5318
      _StockProps     =   14
      Caption         =   "Concentration"
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Begin VB.ComboBox cboConc 
         Height          =   315
         Left            =   1080
         Style           =   2  'Dropdown List
         TabIndex        =   5
         Top             =   240
         Width           =   3900
      End
      Begin VB.ListBox List1 
         BackColor       =   &H80000004&
         Height          =   1392
         Left            =   240
         TabIndex        =   8
         Top             =   1320
         Width           =   4695
      End
      Begin VB.TextBox txtConc 
         Height          =   315
         Left            =   1080
         TabIndex        =   6
         Top             =   720
         Width           =   1800
      End
      Begin VB.ComboBox cboConcUom 
         Height          =   315
         ItemData        =   "CONCWS.frx":0000
         Left            =   3000
         List            =   "CONCWS.frx":000D
         Style           =   2  'Dropdown List
         TabIndex        =   7
         Top             =   720
         Width           =   1260
      End
   End
   Begin Threed.SSFrame SSFrame1 
      Height          =   975
      Left            =   240
      TabIndex        =   12
      Top             =   1335
      Width           =   5175
      _Version        =   65536
      _ExtentX        =   9128
      _ExtentY        =   1720
      _StockProps     =   14
      Caption         =   "Inventory"
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Begin VB.TextBox txtInven 
         Height          =   315
         Left            =   1080
         TabIndex        =   3
         Top             =   360
         Width           =   1800
      End
      Begin VB.ComboBox cboInvUom 
         Height          =   315
         ItemData        =   "CONCWS.frx":0040
         Left            =   3000
         List            =   "CONCWS.frx":004D
         Style           =   2  'Dropdown List
         TabIndex        =   4
         Top             =   360
         Width           =   1260
      End
   End
   Begin VB.ComboBox cboContam 
      Height          =   315
      Left            =   1320
      Style           =   2  'Dropdown List
      TabIndex        =   0
      Top             =   270
      Width           =   3900
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      Caption         =   "Constituent"
      Height          =   315
      Left            =   120
      TabIndex        =   11
      Top             =   300
      Width           =   1110
   End
End
Attribute VB_Name = "frmConc"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Compare Text
Option Explicit

Dim L As Double, W As Double, D As Double, BD As Double, p As Double, C As Double, M As Double, kd As Double
Dim MC As Double

Dim processInput As Boolean
Dim medium As String
Dim insufficientdata As Boolean

Private Type InventoryStruct
  invIntlVal As Double
  invIntlUom As String
  invUserVal As String
  invUserUom As String
  
  concIntlVal As Double
  concIntlUom As String
  concUserVal As String
  concUserUom As String
  
' invValue As Double
' invUom As String
' concValue As Double
' concUom As String
  concType As Integer
End Type

Private inv() As InventoryStruct

Private Sub ComputeInventoryFromConcentration()
  Dim ncont As Integer, valid As Boolean, CT As Double
  
  If insufficientdata Then Exit Sub
  If optUDef(0).value = 1 Then Exit Sub
  
  processInput = False
  txtInven = ""
  valid = IsNumeric(txtConc)
  If valid Then valid = CheckRange("STINVEN", txtConc, "0 <= X")
  If Not valid Then
    processInput = True
    Exit Sub
  End If
  
  ncont = cboContam.ItemData(cboContam.ListIndex)
  
  inv(ncont).concUserUom = cboConcUom
  inv(ncont).concUserVal = txtConc
  inv(ncont).concIntlVal = ConvertUOM(inv(ncont).concUserVal, "", inv(ncont).concUserUom, inv(ncont).concIntlUom)
  
  C = inv(ncont).concIntlVal
  
  Select Case inv(ncont).concType
    Case 1: ' wet
      M = C * L * W * D * (BD + MC)
    Case 2: ' dry
      M = C * L * W * D * BD
    Case 3: ' volume soil
      M = C * L * W * D
    Case 4: ' volume water
      M = C * L * W * D
    Case 5:
      CT = C * (p + BD * kd)
      M = CT * L * W * D
  End Select
    
  inv(ncont).invUserUom = cboInvUom
  inv(ncont).invIntlVal = M
  inv(ncont).invUserVal = ConvertUOM(CStr(M), "", inv(ncont).invIntlUom, inv(ncont).invUserUom)
  inv(ncont).invUserVal = Format(inv(ncont).invUserVal, "0.00###E+00")
  txtInven = inv(ncont).invUserVal
  processInput = True

End Sub
Private Sub ComputeConcentrationFromInventory()
  Dim ncont As Integer, pszValue As String, valid As Boolean
  Dim CT As Double
  
  If insufficientdata Then Exit Sub
  If optUDef(1).value = 1 Then Exit Sub
  
  processInput = False
  txtConc = ""
  valid = IsNumeric(txtInven)
  If valid Then valid = CheckRange("STINVEN", txtInven, "0 <= X")
  If Not valid Then
    processInput = True
    Exit Sub
  End If
  
  ncont = cboContam.ItemData(cboContam.ListIndex)
  
  inv(ncont).invUserUom = cboInvUom
  inv(ncont).invUserVal = txtInven
  inv(ncont).invIntlVal = ConvertUOM(inv(ncont).invUserVal, "", inv(ncont).invUserUom, inv(ncont).invIntlUom)
  
  M = inv(ncont).invIntlVal
  
  Select Case inv(ncont).concType
    Case 1: ' wet
'     M = C * L * W * D * (BD + P)
      C = M / (L * W * D * (BD + MC))
    Case 2: ' dry
'     M = C * L * W * D * BD
      C = M / (L * W * D * BD)
    Case 3: ' volume soil
'     M = C * L * W * D
      C = M / (L * W * D)
    Case 4: ' volume water
'     M = (C * L * W * D) / (P + BD * KD)
      C = M / (L * W * D)
    Case 5:
'     CT = C * (P + BD * KD)
'     M = CT * L * W * D
      CT = M / (L * W * D)
      C = CT / (p + BD * kd)
  End Select
    
  inv(ncont).concUserUom = cboConcUom
  inv(ncont).concIntlVal = C
  inv(ncont).concUserVal = ConvertUOM(CStr(C), "", inv(ncont).concIntlUom, inv(ncont).concUserUom)
  inv(ncont).concUserVal = Format(inv(ncont).concUserVal, "0.00###E+00")
  txtConc = inv(ncont).concUserVal

  processInput = True
End Sub

Private Sub cboConc_Click()
  Dim ncont As Integer, Unit As String, ctype As Integer, enable As Boolean
  Dim idx As Integer
  
  
  If Not processInput Then Exit Sub

  
  processInput = False
  
  ncont = cboContam.ItemData(cboContam.ListIndex)
  
  inv(ncont).concType = cboConc.ItemData(cboConc.ListIndex)
  
  ctype = inv(ncont).concType
  
  List1.Clear
  
  enable = True
  If GetParamElem(idx, "STLENGTH", 0, 0, E_INTLVAL, L) Then
    GetParamElem idx, "STLENGTH", 0, 0, E_INTLUNIT, Unit
    L = ConvertUOM(CStr(L), "", Unit, "cm")
  Else
    enable = False
    List1.AddItem "undefined STLENGTH: " & GetParamInfoElem(idx, "STLENGTH", E_SCREEN)
  End If
  If GetParamElem(idx, "STWIDTH", 0, 0, E_INTLVAL, W) Then
    GetParamElem idx, "STWIDTH", 0, 0, E_INTLUNIT, Unit
    W = ConvertUOM(CStr(W), "", Unit, "cm")
  Else
    enable = False
    List1.AddItem "undefined STWIDTH: " & GetParamInfoElem(idx, "STWIDTH", E_SCREEN)
  End If
  If GetParamElem(idx, "STTHICK", 0, 0, E_INTLVAL, D) Then
    GetParamElem idx, "STTHICK", 0, 0, E_INTLUNIT, Unit
    D = ConvertUOM(CStr(D), "", Unit, "cm")
  Else
    enable = False
    List1.AddItem "undefined STTHICK: " & GetParamInfoElem(idx, "STTHICK", E_SCREEN)
  End If
  If (1 = ctype Or 2 = ctype Or 5 = ctype) Then
    If GetParamElem(idx, "STZBULKD", 0, 0, E_INTLVAL, BD) Then
      GetParamElem idx, "STZBULKD", 0, 0, E_INTLUNIT, Unit
      BD = ConvertUOM(CStr(BD), "", Unit, "g/cm^3")
    Else
      enable = False
      List1.AddItem "undefined STZBULKD: " & GetParamInfoElem(idx, "STZBULKD", E_SCREEN)
    End If
  End If
  If (1 = ctype) Then
    If GetParamElem(idx, "STMOISTC", 0, 0, E_INTLVAL, MC) Then
      GetParamElem idx, "STMOISTC", 0, 0, E_INTLUNIT, Unit
      MC = ConvertUOM(CStr(MC), "", Unit, "fraction")
    Else
      enable = False
      List1.AddItem "undefined STMOISTC: " & GetParamInfoElem(idx, "STMOISTC", E_SCREEN)
    End If
  End If
  If (5 = ctype) Then
    If GetParamElem(idx, "STTOTPOR", 1, 0, E_INTLVAL, p) Then
      GetParamElem idx, "STTOTPOR", 1, 0, E_INTLUNIT, Unit
      p = ConvertUOM(CStr(p), "", Unit, "fraction")
    Else
      enable = False
      List1.AddItem "undefined STTOTPOR: " & GetParamInfoElem(idx, "STTOTPOR", E_SCREEN)
    End If
  End If
  If (5 = ctype) Then
    If GetParamElem3(idx, "STKD", ncont, 0, 1, E_INTLVAL, kd) Then
      GetParamElem3 idx, "STKD", ncont, 0, 1, E_INTLUNIT, Unit
'     KD = ConvertUOM(CStr(KD), "", unit, "mL/g")
    Else
      enable = False
      List1.AddItem "undefined STKD: " & GetParamInfoElem(idx, "STKD", E_SCREEN)
    End If
  End If
  
  If Not enable Then
    If Not insufficientdata Then
      MsgBox "Required site data is not defined."
      insufficientdata = True
      cmdOK.Enabled = False
    End If
    processInput = True
    Exit Sub
  End If
  
  If (1 = vptAllCont(ncont).type) Then Unit = "Ci" Else Unit = "g"
  
  List1.AddItem "STLENGTH (cm): " + CStr(L)
  List1.AddItem "STWIDTH (cm): " + CStr(W)
  List1.AddItem "STTHICK (cm): " + CStr(D)
  
  Select Case inv(ncont).concType
    Case 1: Unit = Unit + "/g"
      List1.AddItem "STZBULKD (g/cm^3): " + CStr(BD)
      List1.AddItem "STMOISTC (fraction): " + CStr(MC)
    Case 2: Unit = Unit + "/g"
      List1.AddItem "STZBULKD (g/cm^3): " + CStr(BD)
    Case 3: Unit = Unit + "/cm^3"
    Case 4: Unit = Unit + "/mL"
    Case 5: Unit = Unit + "/mL"
      List1.AddItem "STZBULKD (g/cm^3): " + CStr(BD)
      List1.AddItem "STTOTPOR (fraction): " + CStr(p)
      List1.AddItem "STKD (mL/g): " + CStr(kd)
  End Select
  
  cboConcUom.Clear
  get_conversion_items Unit, cboConcUom
  
  If (inv(ncont).concIntlUom = Unit) Then
    If Not inv(ncont).concUserUom = "" Then
      cboConcUom = inv(ncont).concUserUom
    End If
  End If
  inv(ncont).concIntlUom = Unit
  
  txtConc = inv(ncont).concUserVal
  
  If optUDef(0).value Then
    ComputeConcentrationFromInventory
  Else
    ComputeInventoryFromConcentration
  End If
  
  processInput = True

End Sub

Private Sub cboConcUom_Click()
  If processInput Then ComputeInventoryFromConcentration
End Sub

Private Sub cboContam_Click()
  Dim idx As Integer, ncont As Integer, i As Integer, recalc As Boolean
  Dim uservalue As String, userunit As String
  
  processInput = False
  
  ncont = cboContam.ItemData(cboContam.ListIndex)
  
  If (1 = vptAllCont(ncont).type) Then inv(ncont).invIntlUom = "Ci" Else inv(ncont).invIntlUom = "g"
  
  cboInvUom.Clear
  get_conversion_items inv(ncont).invUserUom, cboInvUom
  txtInven = inv(ncont).invUserVal
  
  If (0 = inv(ncont).concType) Then
    recalc = True
    uservalue = txtInven
    If InStr(medium, "Soil") Then
      inv(ncont).concType = 2
    Else
      If (InStr(medium, "surface")) Then
        inv(ncont).concType = 4
      Else
        inv(ncont).concType = 5
      End If
    End If
  End If
  For i = 0 To cboConc.ListCount - 1
    If inv(ncont).concType = cboConc.ItemData(i) Then
      cboConc.ListIndex = i
      Exit For
    End If
  Next i
  If recalc Then
    processInput = True
    cboConc_Click
    processInput = False
    txtInven = uservalue
'    ComputeConcentrationFromInventory
  End If
  
'  cboInvUom.AddItem userunit
'  cboInvUom.ListIndex = 0
  processInput = True
  If (cboConc.ListIndex >= 0) Then
    cboConc_Click
  Else
    cboConc.ListIndex = 0
  End If
    
'  GetParamElem idx, "STINVEN", ncont, 0, E_USERVAL, inv(ncont).invUserVal
  'GetParamElem idx, "STINVEN", ncont, 0, E_USERUNIT, inv(ncont).invUserUom
  
  
End Sub

Private Sub cboInvUom_Click()
  If processInput Then ComputeConcentrationFromInventory
End Sub

Private Sub cmdCancel_Click()
  Unload frmConc
End Sub

Private Sub cmdOK_Click()
  Dim ncont As Integer, idx As Integer
  
  For ncont = 1 To UBound(inv)
    If Not IsNumeric(inv(ncont).invUserVal) Then
      MsgBox ("Inventory is not defined for " + vptAllCont(ncont).name)
      Exit Sub
    End If
    If (0 >= Val(inv(ncont).invIntlVal)) Then
      MsgBox ("Inventory must be > 0.0 for " + vptAllCont(ncont).name)
      Exit Sub
    End If
  Next
  
  For ncont = 1 To UBound(vptCont)
    SetParamElem idx, "STINVEN", ncont, 0, E_INVALID, False
    SetParamElem idx, "STCONC", ncont, 0, E_INVALID, False
    SetParamElem idx, "STCONCTYPE", ncont, 0, E_INVALID, False
    
    SetParamElem idx, "STINVEN", ncont, 0, E_INTLVAL, inv(ncont).invIntlVal
    SetParamElem idx, "STINVEN", ncont, 0, E_INTLUNIT, inv(ncont).invIntlUom
    SetParamElem idx, "STCONC", ncont, 0, E_INTLVAL, inv(ncont).concIntlVal
    SetParamElem idx, "STCONC", ncont, 0, E_INTLUNIT, inv(ncont).concIntlUom
    
    SetParamElem idx, "STINVEN", ncont, 0, E_USERVAL, inv(ncont).invUserVal
    SetParamElem idx, "STINVEN", ncont, 0, E_USERUNIT, inv(ncont).invUserUom
    SetParamElem idx, "STCONC", ncont, 0, E_USERVAL, inv(ncont).concUserVal
    SetParamElem idx, "STCONC", ncont, 0, E_USERUNIT, inv(ncont).concUserUom
    SetParamElem idx, "STCONCTYPE", ncont, 0, E_USERVAL, inv(ncont).concType
    
    ' without the following 4 lines, STCONC and STCONCTYPE
    ' won't be saved to file when this spreadsheet content is saved
    If inv(ncont).concType > 0 Then
      SetParamElem idx, "STCONC", ncont, 0, E_SPR, TAB_CONT
      SetParamElem idx, "STCONC", ncont, 0, E_ACTIVE, True
      SetParamElem idx, "STCONCTYPE", ncont, 0, E_SPR, TAB_CONT
      SetParamElem idx, "STCONCTYPE", ncont, 0, E_ACTIVE, True
    End If
  Next
  Unload frmConc
End Sub

Private Sub Form_load()
  Dim spr As fpSpread
  Dim ncont As Integer, idx As Integer
  Dim uservalue As String, Unit As String
  Dim var As Variant
  Dim sRc As Integer, ndx As Integer
  
  insufficientdata = False ' initial assumption
  optUDef(0) = True
  
  Set spr = frmMain.ssTab(TAB_CONT)
  
  ReDim inv(UBound(vptCont))
  
  For ncont = 1 To UBound(vptCont)
    cboContam.AddItem vptAllCont(ncont).name
    
    GetParamElem idx, "STINVEN", ncont, 0, E_INTLVAL, inv(ncont).invIntlVal
    GetParamElem idx, "STINVEN", ncont, 0, E_INTLUNIT, inv(ncont).invIntlUom
    GetParamElem idx, "STCONC", ncont, 0, E_INTLVAL, inv(ncont).concIntlVal
    GetParamElem idx, "STCONC", ncont, 0, E_INTLUNIT, inv(ncont).concIntlUom
    
    GetParamElem idx, "STINVEN", ncont, 0, E_USERVAL, inv(ncont).invUserVal
    GetParamElem idx, "STINVEN", ncont, 0, E_USERUNIT, inv(ncont).invUserUom
    GetParamElem idx, "STCONC", ncont, 0, E_USERVAL, inv(ncont).concUserVal
    GetParamElem idx, "STCONC", ncont, 0, E_USERUNIT, inv(ncont).concUserUom
    GetParamElem idx, "STCONCTYPE", ncont, 0, E_USERVAL, inv(ncont).concType
    
    If Not IsNumeric(inv(ncont).invUserVal) Then
      If IsNumeric(inv(ncont).invIntlVal) Then
        inv(ncont).invUserVal = ConvertUOM(CStr(inv(ncont).invIntlVal), "", inv(ncont).invIntlUom, inv(ncont).invUserUom)
        inv(ncont).invUserVal = Format(inv(ncont).invUserVal, "0.00###E+00")
      End If
    End If
    If Not IsNumeric(inv(ncont).concUserVal) Then
      If IsNumeric(inv(ncont).concIntlVal) Then
        inv(ncont).concUserVal = ConvertUOM(CStr(inv(ncont).concIntlVal), "", inv(ncont).concIntlUom, inv(ncont).concUserUom)
        inv(ncont).concUserVal = Format(inv(ncont).concUserVal, "0.00###E+00")
      End If
    End If
    
    cboContam.ItemData(cboContam.NewIndex) = ncont
  Next ncont
  
  spr.GetText SS_C1, spr.Row, var
  ncont = CInt(var)
  
  cboConc.Clear
  sRc = GetParamElem(ndx, "STMEDIA", 0, 0, E_USERVAL, medium)
  
  If InStr(medium, "Soil") Then
'FCR Labeling change contaminant -> constituent
'    cboConc.AddItem "Mass contaminant/Mass soil Wet"
    cboConc.AddItem "Mass constituent/Mass soil Wet"
    cboConc.ItemData(cboConc.NewIndex) = 1
'FCR Labeling change contaminant -> constituent
'    cboConc.AddItem "Mass contaminant/Mass soil Dry"
    cboConc.AddItem "Mass constituent/Mass soil Dry"
    cboConc.ItemData(cboConc.NewIndex) = 2
'FCR Labeling change contaminant -> constituent
'    cboConc.AddItem "Mass contaminant/Volume soil"
    cboConc.AddItem "Mass constituent/Volume soil"
    cboConc.ItemData(cboConc.NewIndex) = 3
  Else
    If (InStr(medium, "surface")) Then
'FCR Labeling change contaminant -> constituent
'      cboConc.AddItem "Mass aqueous contaminant/Volume water"
      cboConc.AddItem "Mass aqueous constituent/Volume water"
      cboConc.ItemData(cboConc.NewIndex) = 4
    Else
'FCR Labeling change contaminant -> constituent
'      cboConc.AddItem "Mass aqueous contaminant/Volume water"
      cboConc.AddItem "Mass aqueous constituent/Volume water"
      cboConc.ItemData(cboConc.NewIndex) = 5
    End If
  End If
  cboContam.ListIndex = ncont - 1
  optUDef(0).value = True
End Sub

Private Sub optUDef_Click(Index As Integer)
  txtInven.Enabled = (Index = 0)
  txtConc.Enabled = (Index = 1)
  cboInvUom.Enabled = txtInven.Enabled
  cboConcUom.Enabled = txtConc.Enabled
End Sub

Private Sub txtConc_Change()
  If processInput Then ComputeInventoryFromConcentration
End Sub

Private Sub txtInven_Change()
  If processInput Then ComputeConcentrationFromInventory
End Sub
