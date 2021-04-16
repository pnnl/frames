VERSION 5.00
Object = "{F856EC8B-F03C-4515-BDC6-64CBD617566A}#7.0#0"; "FPSPR70.ocx"
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "comctl32.ocx"
Begin VB.Form frmEstimate 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Property Estimator"
   ClientHeight    =   8175
   ClientLeft      =   45
   ClientTop       =   615
   ClientWidth     =   9345
   ClipControls    =   0   'False
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   7.5
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form4"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   545
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   623
   ShowInTaskbar   =   0   'False
   StartUpPosition =   1  'CenterOwner
   Begin ComctlLib.StatusBar StatusBar1 
      Align           =   2  'Align Bottom
      Height          =   408
      Left            =   0
      TabIndex        =   29
      Top             =   7776
      Width           =   9348
      _ExtentX        =   16484
      _ExtentY        =   714
      SimpleText      =   ""
      _Version        =   327682
      BeginProperty Panels {0713E89E-850A-101B-AFC0-4210102A8DA7} 
         NumPanels       =   2
         BeginProperty Panel1 {0713E89F-850A-101B-AFC0-4210102A8DA7} 
            AutoSize        =   1
            Object.Width           =   8467
            MinWidth        =   5292
            Object.Tag             =   ""
         EndProperty
         BeginProperty Panel2 {0713E89F-850A-101B-AFC0-4210102A8DA7} 
            Object.Width           =   7937
            MinWidth        =   7937
            Object.Tag             =   ""
         EndProperty
      EndProperty
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Tahoma"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
   Begin VB.ComboBox cboProp 
      Height          =   288
      ItemData        =   "Estimate.frx":0000
      Left            =   1800
      List            =   "Estimate.frx":0002
      Style           =   2  'Dropdown List
      TabIndex        =   27
      Top             =   90
      Width           =   4932
   End
   Begin VB.Frame Frame4 
      Caption         =   "Estimations"
      ClipControls    =   0   'False
      Height          =   3510
      Left            =   120
      TabIndex        =   17
      Top             =   4200
      Width           =   9108
      Begin VB.CommandButton cmdClose 
         Caption         =   "&Close"
         Height          =   350
         Left            =   7680
         TabIndex        =   30
         Top             =   3000
         Width           =   1120
      End
      Begin FPSpreadADO.fpSpread vaSpread1 
         Height          =   2448
         Left            =   240
         TabIndex        =   26
         Top             =   336
         Width           =   8616
         _Version        =   458752
         _ExtentX        =   15187
         _ExtentY        =   4313
         _StockProps     =   64
         BackColorStyle  =   1
         ColHeaderDisplay=   0
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "Tahoma"
            Size            =   7.5
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         RowHeaderDisplay=   0
         ScrollBars      =   2
         ScrollBarShowMax=   0   'False
         SpreadDesigner  =   "Estimate.frx":0004
      End
      Begin VB.CommandButton cmdApply 
         Caption         =   "&Apply"
         Height          =   350
         Left            =   6360
         TabIndex        =   25
         Top             =   3000
         Width           =   1120
      End
   End
   Begin VB.Frame Frame1 
      Caption         =   "Input Parameters"
      ClipControls    =   0   'False
      Height          =   3615
      Left            =   120
      TabIndex        =   0
      Top             =   480
      Width           =   9072
      Begin VB.TextBox Text1 
         Alignment       =   1  'Right Justify
         Enabled         =   0   'False
         Height          =   300
         Index           =   0
         Left            =   5175
         TabIndex        =   21
         Text            =   "Text1"
         Top             =   285
         Width           =   1425
      End
      Begin VB.TextBox Text1 
         Alignment       =   1  'Right Justify
         Enabled         =   0   'False
         Height          =   300
         Index           =   5
         Left            =   5175
         TabIndex        =   18
         Text            =   "Text1"
         Top             =   2265
         Width           =   1425
      End
      Begin VB.Frame fraBase 
         Caption         =   "Based On..."
         ClipControls    =   0   'False
         Enabled         =   0   'False
         Height          =   765
         Left            =   3480
         TabIndex        =   7
         Top             =   2700
         Width           =   3960
         Begin VB.ComboBox cboBase 
            Height          =   288
            ItemData        =   "Estimate.frx":021F
            Left            =   330
            List            =   "Estimate.frx":022F
            Style           =   2  'Dropdown List
            TabIndex        =   8
            Top             =   300
            Width           =   3330
         End
      End
      Begin VB.Frame fraMethod 
         Caption         =   "Use..."
         ClipControls    =   0   'False
         Enabled         =   0   'False
         Height          =   765
         Left            =   360
         TabIndex        =   5
         Top             =   2700
         Width           =   3228
         Begin VB.CheckBox chkAcid 
            Caption         =   "Acid"
            Height          =   285
            Left            =   2205
            TabIndex        =   24
            Top             =   285
            Width           =   750
         End
         Begin VB.ComboBox cboMethod 
            Height          =   288
            ItemData        =   "Estimate.frx":025F
            Left            =   285
            List            =   "Estimate.frx":0269
            Style           =   2  'Dropdown List
            TabIndex        =   6
            Top             =   270
            Width           =   1740
         End
      End
      Begin VB.TextBox Text1 
         Alignment       =   1  'Right Justify
         Enabled         =   0   'False
         Height          =   300
         Index           =   4
         Left            =   5175
         TabIndex        =   4
         Text            =   "Text1"
         Top             =   1875
         Width           =   1425
      End
      Begin VB.TextBox Text1 
         Alignment       =   1  'Right Justify
         Enabled         =   0   'False
         Height          =   300
         Index           =   3
         Left            =   5175
         TabIndex        =   3
         Text            =   "Text1"
         Top             =   1470
         Width           =   1425
      End
      Begin VB.TextBox Text1 
         Alignment       =   1  'Right Justify
         Enabled         =   0   'False
         Height          =   300
         Index           =   2
         Left            =   5175
         TabIndex        =   2
         Text            =   "Text1"
         Top             =   1080
         Width           =   1425
      End
      Begin VB.TextBox Text1 
         Alignment       =   1  'Right Justify
         Enabled         =   0   'False
         Height          =   300
         Index           =   1
         Left            =   5175
         TabIndex        =   1
         Text            =   "Text1"
         Top             =   675
         Width           =   1425
      End
      Begin VB.Label Label1 
         Caption         =   "Label1"
         Enabled         =   0   'False
         Height          =   270
         Index           =   0
         Left            =   495
         TabIndex        =   23
         Top             =   300
         Width           =   3975
      End
      Begin VB.Label Label2 
         AutoSize        =   -1  'True
         Caption         =   "Label2"
         Enabled         =   0   'False
         Height          =   192
         Index           =   0
         Left            =   6792
         TabIndex        =   22
         Top             =   312
         Width           =   492
      End
      Begin VB.Label Label1 
         Caption         =   "Label1"
         Enabled         =   0   'False
         Height          =   270
         Index           =   5
         Left            =   495
         TabIndex        =   20
         Top             =   2325
         Width           =   3975
      End
      Begin VB.Label Label2 
         AutoSize        =   -1  'True
         Caption         =   "Label2"
         Enabled         =   0   'False
         Height          =   192
         Index           =   5
         Left            =   6792
         TabIndex        =   19
         Top             =   2340
         Width           =   492
      End
      Begin VB.Label Label2 
         AutoSize        =   -1  'True
         Caption         =   "Label2"
         Enabled         =   0   'False
         Height          =   192
         Index           =   4
         Left            =   6792
         TabIndex        =   16
         Top             =   1932
         Width           =   492
      End
      Begin VB.Label Label1 
         Caption         =   "Label1"
         Enabled         =   0   'False
         Height          =   270
         Index           =   4
         Left            =   495
         TabIndex        =   15
         Top             =   1920
         Width           =   3975
      End
      Begin VB.Label Label2 
         AutoSize        =   -1  'True
         Caption         =   "Label2"
         Enabled         =   0   'False
         Height          =   192
         Index           =   3
         Left            =   6792
         TabIndex        =   14
         Top             =   1536
         Width           =   492
      End
      Begin VB.Label Label1 
         Caption         =   "Label1"
         Enabled         =   0   'False
         Height          =   270
         Index           =   3
         Left            =   495
         TabIndex        =   13
         Top             =   1515
         Width           =   3975
      End
      Begin VB.Label Label2 
         AutoSize        =   -1  'True
         Caption         =   "Label2"
         Enabled         =   0   'False
         Height          =   192
         Index           =   2
         Left            =   6792
         TabIndex        =   12
         Top             =   1128
         Width           =   492
      End
      Begin VB.Label Label1 
         Caption         =   "Label1"
         Enabled         =   0   'False
         Height          =   270
         Index           =   2
         Left            =   495
         TabIndex        =   11
         Top             =   1110
         Width           =   3975
      End
      Begin VB.Label Label2 
         AutoSize        =   -1  'True
         Caption         =   "Label2"
         Enabled         =   0   'False
         Height          =   192
         Index           =   1
         Left            =   6792
         TabIndex        =   10
         Top             =   720
         Width           =   492
      End
      Begin VB.Label Label1 
         Caption         =   "Label1"
         Enabled         =   0   'False
         Height          =   270
         Index           =   1
         Left            =   495
         TabIndex        =   9
         Top             =   705
         Width           =   3975
      End
   End
   Begin VB.Label Label3 
      Alignment       =   1  'Right Justify
      AutoSize        =   -1  'True
      Caption         =   "Select Property:"
      Height          =   192
      Left            =   240
      TabIndex        =   28
      Top             =   120
      Width           =   1140
   End
   Begin VB.Menu mnuHelp 
      Caption         =   "&Help... (F1)"
   End
End
Attribute VB_Name = "frmEstimate"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Private Const kowIndex As Long = 0
Private Const solIndex As Long = 1
Private Const kocIndex As Long = 2
Private Const vpIndex As Long = 3
Private Const mwIndex As Long = 4
Private Const mpIndex As Long = 5

Private Const selSOL As Long = 1
Private Const selHLC As Long = 2
Private Const selKOC As Long = 3
Private Const selKP As Long = 4
Private Const selETF As Long = 5
Private Const selBFF As Long = 6

Private est As Variant
Private etf As Variant

Private Sub cboBase_Click()
  If cboProp.ListIndex = selBFF Then
    SetParam kowIndex, cboBase.ListIndex = 0
    SetParam solIndex, cboBase.ListIndex = 1
    SetParam kocIndex, cboBase.ListIndex = 2
  Else
    If InStr(cboMethod, "gems") And InStr(cboBase, "pest") Then
      SetParam kowIndex, False
      SetParam solIndex, True
    Else
      SetParam kowIndex, True
      SetParam solIndex, False
    End If
  End If
  Calculate
End Sub

Private Sub cmdApply_Click()
Dim r As Long
Dim nrow As Long
Dim value As Variant

  For r = 0 To 5
    frmCDBE.vaSpread1.GetText 3, Label1(r).tag, value
    If value <> Text1(r) Then
      frmCDBE.vaSpread1.SetText 3, Label1(r).tag, Text1(r)
    End If
  Next
  For r = 1 To vaSpread1.MaxRows
    vaSpread1.row = r
    vaSpread1.col = 4
    If 1 = vaSpread1.value Then
      nrow = vaSpread1.GetRowItemData(r)
      vaSpread1.GetText 3, r, value
      frmCDBE.vaSpread1.SetText 3, nrow, value
    End If
  Next
  Unload Me
End Sub

Private Sub cboMethod_Click()
Dim gems As Boolean

  If cboProp.ListIndex < 0 Then Exit Sub
  gems = InStr(cboMethod, "GEMS")
  If cboProp.ListIndex = selSOL Then
    chkAcid.Enabled = gems
    SetParam kowIndex, True
    SetParam mpIndex, (Not gems = 0)
  ElseIf cboProp.ListIndex = selKOC Then
    cboBase.Clear
    cboBase.AddItem "pesticides"
    If Not gems Then cboBase.AddItem "general"
    cboBase.AddItem "aromatics"
    If Not gems Then cboBase.AddItem "s-triazines"
    cboBase.ListIndex = 0
  ElseIf cboProp.ListIndex = selBFF Then
    If cboMethod.ListIndex = 0 Then
      cboBase.ListIndex = -1
      SetParam kowIndex, True
      SetParam solIndex, False
      SetParam kocIndex, False
      fraBase.Enabled = False
    Else
      fraBase.Enabled = True
      If cboBase.ListIndex < 0 Then cboBase.ListIndex = 0 Else cboBase_Click
    End If
  End If
  Calculate
End Sub

Private Sub chkAcid_Click()
  EstimateSOL
End Sub

Private Sub cmdClose_Click()
  Unload Me
End Sub

Private Sub Form_Activate()
  HelpAnchor = "Estimate"
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
  Select Case KeyCode
    Case vbKeyF1:
      KeyCode = 0
      mnuHelp_Click
  End Select
End Sub

Private Sub Form_load()
Dim i As Long
Dim j As Long
Dim r As Long
Dim c As Long
Dim ubest As Long
Dim ubetf As Long
Dim wid As Long
Dim nrow As Long
Dim var As Variant
Dim pName As Variant
Dim sprI As fpSpread
Dim sprO As fpSpread

  Caption = "Estimates for " & frmCDBE.txtName & " (" & frmCDBE.txtCasid & ")"
  mnuHelp.Enabled = HelpAvailable
  EstimateLoaded = True
  est = Array("", "CLSOL", "CLHLC", "CLKOC", "CLKPERM", "ETF", "CLBFF")
  etf = Array("CLBFM", "CLBFI", "CLFMT", "CLFMK", "CLBVLV", "CLBVRV", _
                "CLBVOV", "CLBVFR", "CLBVCL", "CLBVAF", "CLBVAH", "CLBVAG")
  ubest = UBound(est)
  ubetf = UBound(est)
  
  LoadEstStruct "CLKOW", kowIndex
  LoadEstStruct "CLSOL", solIndex
  LoadEstStruct "CLKOC", kocIndex
  LoadEstStruct "CLVAP", vpIndex
  LoadEstStruct "CLWM", mwIndex
  LoadEstStruct "CLMP", mpIndex
  
  cboMethod.ListIndex = -1
  cboBase.ListIndex = -1
  
  Set sprI = frmCDBE.vaSpread1
  Set sprO = frmEstimate.vaSpread1
  
  For c = 0 To 4
    sprO.col = c
    Select Case c
      Case 0: wid = 9:  sprO.TypeHAlign = 0: sprO.CellType = 5:  sprO.SetText c, 0, "Name"
      Case 1: wid = 33: sprO.CellType = 5:  sprO.SetText c, 0, "Description"
      Case 2: wid = 12: sprO.CellType = 5:  sprO.SetText c, 0, "Units"
      Case 3: wid = 9:  sprO.CellType = 5:  sprO.SetText c, 0, "Value"
      Case 4: wid = 6:  sprO.CellType = 10: sprO.SetText c, 0, "Apply": sprO.TypeCheckCenter = True
    End Select
    sprO.ColWidth(c) = wid
  Next
  sprO.row = -1
  sprO.col = 0
  sprO.BackColor = lightYellow
  sprO.row = 0
  sprO.col = -1
  sprO.BackColor = lightYellow
  nrow = 0
  
  For i = 0 To ubest
    Select Case est(i)
      Case ""
        cboProp.AddItem ""
      Case "ETF"
        cboProp.AddItem "Environmental Transfer Factors"
        For j = 0 To ubetf
          For r = 0 To sprI.MaxRows
            sprI.GetText 0, r, var
            If var = etf(j) Then
              nrow = nrow + 1
              If j = 0 Then cboProp.itemdata(cboProp.NewIndex) = nrow
              For c = 0 To 3
                sprI.GetText c, r, var
                sprO.SetText c, nrow, var
              Next
              sprO.SetRowItemData nrow, r
              Exit For
            End If
          Next
        Next
      Case Else
        For r = 1 To sprI.MaxRows
          sprI.GetText 0, r, var
          If var = est(i) Then
              nrow = nrow + 1
              For c = 0 To 3
                sprI.GetText c, r, var
                sprO.SetText c, nrow, var
              Next
              sprO.SetRowItemData nrow, r
              sprO.GetText 1, nrow, var
              cboProp.AddItem var
              cboProp.itemdata(cboProp.NewIndex) = nrow
            Exit For
          End If
        Next
    End Select
  Next
  sprO.MaxRows = nrow
  sprO.MaxCols = 4
  
  For r = 1 To sprO.MaxRows
    sprO.row = r
    sprO.col = 3
    sprO.GetText 0, r, pName
    sprO.GetText 3, r, var
    If Len(var) > 0 Then
      If Not ValidInput(pName, var) Then sprO.BackColor = lightRed
    End If
  Next
  If cboProp.ListCount > 0 Then cboProp.ListIndex = 0 '
  
  HelpAnchor = "Estimate"
End Sub

Private Sub LoadEstStruct(Name As String, ix As Long)
Dim i As Long
Dim var As Variant
Dim frm As Form

  Set frm = frmCDBE
  For i = 1 To frm.vaSpread1.MaxRows
    frm.vaSpread1.SetRowItemData i, 0
    frm.vaSpread1.GetText 0, i, var
    If var = Name Then
      frm.vaSpread1.GetText 1, i, var
      Label1(ix).Caption = var & " (" & Right$(Name, Len(Name) - 2) & ")"
      Label1(ix).tag = i
      frm.vaSpread1.GetText 2, i, var
      Label2(ix) = var
      frm.vaSpread1.GetText 3, i, var
      Text1(ix) = var
      Text1(ix).tag = Name
      Exit For
    End If
  Next
  If Not ValidInput(Text1(ix).tag, Text1(ix)) Then
    Text1(ix).BackColor = lightRed
    Message "", ix
  End If
End Sub

Private Sub SetControls(Name As String)
Dim i As Long
Dim ub As Long

  cboBase.ListIndex = -1
  cboMethod.ListIndex = -1
  ub = UBound(est)
  For i = 1 To ub
    SetParam i - 1, False
  Next
  fraMethod.Enabled = False
  fraBase.Enabled = False
  chkAcid.Enabled = False
  Select Case Name
    Case "CLSOL": ' solubility
      fraMethod.Enabled = True
      cboMethod.Clear
      cboMethod.AddItem "GEMS"
      cboMethod.AddItem "Lyman"
      cboMethod.ListIndex = 0
    Case "CLHLC": ' henrys law constant
      SetParam solIndex, True
      SetParam vpIndex, True
      SetParam mwIndex, True
    Case "CLKOC": ' koc
      fraMethod.Enabled = True
      fraBase.Enabled = True
      cboMethod.Clear
      cboMethod.AddItem "GEMS"
      cboMethod.AddItem "Lyman"
      cboMethod.ListIndex = 0
    Case "CLKPERM": ' kp
      SetParam kowIndex, True
      SetParam mwIndex, True
    Case "ETF": ' env transfer factors
      SetParam kowIndex, True
    Case "CLBFF": ' BFF accum in fish
      fraMethod.Enabled = True
      cboMethod.Clear
      cboMethod.AddItem "Bintein"
      cboMethod.AddItem "Lyman"
      fraBase.Enabled = True
      cboBase.Clear
      cboBase.AddItem Label1(kowIndex)
      cboBase.AddItem Label1(solIndex)
      cboBase.AddItem Label1(kocIndex)
      cboMethod.ListIndex = 0
  End Select
End Sub

Private Sub SetParam(ix As Long, en As Boolean)
  Text1(ix).Enabled = en
  Label1(ix).Enabled = en
  Label2(ix).Enabled = en
  If en Then
    If ValidInput(Text1(ix).tag, Text1(ix)) Then
      Text1(ix).BackColor = lightGreen
    Else
      Text1(ix).BackColor = lightRed
    End If
  Else
    Text1(ix).BackColor = lightGray
  End If
End Sub

Private Function Log10MM(ByVal x As Double) As Double
  If x = 0 Then
    Log10MM = 0
  Else
    Log10MM = Log(x) / Log(10#)
  End If
End Function

Private Sub EstimateKP()
Dim mw As Double
Dim kp As Double
Dim kow As Double

  kow = Val(Text1(kowIndex).text)
  mw = Val(Text1(mwIndex).text)
  
'  If chkinorganic.value = 1 Then
  If False Then
    Text1(6) = "1.00E-03"
    Message "Default of 1.00E-03 for Inorganics.", -1
  ElseIf mw = 0 Then
    Message "Invalid value for molecular weight.", mwIndex
  ElseIf kow = 0 Then
    Message "Invalid value for Kow.", kowIndex
  Else
    kp = 10 ^ (-2.72 + (0.71 * Log10MM(kow)) - (0.0061 * mw))
    SetEstimate "CLKPERM", kp
  End If
End Sub

Private Sub EstimateSOL()
Dim mp As Double
Dim ws As Double
Dim kow As Double
Dim lkow As Double

  kow = Val(Text1(kowIndex))
  mp = Val(Text1(mpIndex))
  lkow = Log10MM(kow)
  If InStr(cboMethod, "GEMS") Then    'use S-1, S-2, or S-3
    If kow = 0 Then
      Message "Invalid value for Kow.", kowIndex
      Exit Sub
    ElseIf IsNumeric(Text1(mpIndex)) Then
      Message "Invalid value for melting point.", mpIndex
      Exit Sub
    ElseIf chkAcid.value = False Then    'non-acid
      If lkow >= 0.5 And lkow <= 8 Then   'S-1
        ws = 10 ^ ((-1.1123 * lkow) + 0.686 - (0.0099 * (mp - 25)))
      ElseIf lkow >= -0.5 And lkow <= 0.5 Then 'S-2
        ws = 10 ^ ((-1.034 * lkow) + 0.455 - (0.0099 * (mp - 25)))
      Else
        Message "Log Kow out of range.  Try using the Lyman method.", kowIndex
        Exit Sub
      End If
    Else               'acid, S-3
      ws = 10 ^ ((-0.65 * lkow) + 0.0279 - (0.0099 * (mp - 25)))
    End If
  Else                     'Lyman
    If kow = 0 Then
      Message "Invalid value for Kow.", kowIndex
      Exit Sub
    Else
      ws = 10 ^ ((-0.922 * lkow) + 4.184)
    End If
  End If
  SetEstimate "CLSOL", Format(ws, "0.00E+00")
End Sub

Private Sub EstimateETF()
Dim i As Long
Dim ub As Long
Dim kow As Double

  kow = Val(Text1(kowIndex))
  If kow = 0 Then
    Message "Invalid value for Kow.", kowIndex
  Else
    SetEstimate "CLBFM", 10 ^ (0.819 * Log10MM(kow) - 1.146)
    SetEstimate "CLBFI", 10 ^ (0.819 * Log10MM(kow) - 1.146)
    SetEstimate "CLFMT", 10 ^ (-7.6 + Log10MM(kow))
    SetEstimate "CLFMK", 10 ^ (-8.1 + Log10MM(kow))
    ub = UBound(etf)
    For i = 0 To ub
      If InStr(etf(i), "CLBV") Then
        SetEstimate etf(i), 10 ^ (1.588 - (0.578 * Log10MM(kow)))
      End If
    Next
  End If
End Sub

Private Sub EstimateBFF()
Dim ws As Double
Dim kow As Double
Dim koc As Double
Dim bcf As Double

  ws = Val(Text1(solIndex))
  koc = Val(Text1(kocIndex))
  kow = Val(Text1(kowIndex))
  If kow = 0 And ws = 0 And koc = 0 Then
    Message "Not enough information to calculate BCF.", -1
  Else
    If cboMethod.ListIndex = 0 Then
      bcf = 10 ^ (0.91 * Log10MM(kow) - 1.975 * Log10MM(0.00000068 * kow + 1) - 0.786)
    ElseIf cboBase.ListIndex = 0 Then
      bcf = 10 ^ (0.76 * Log10MM(kow) - 0.23)
    ElseIf cboBase.ListIndex = 1 Then
      bcf = 10 ^ (2.791 - 0.564 * Log10MM(ws))
    ElseIf cboBase.ListIndex = 2 Then
      bcf = 10 ^ (1.119 * Log10MM(koc) - 1.579)
    End If
    SetEstimate "CLBFF", bcf
  End If
End Sub

Private Sub EstimateKOC()
Dim ws As Double
Dim kow As Double
Dim koc As Double

  ws = Val(Text1(solIndex))
  kow = Val(Text1(kowIndex))
  
  If InStr(cboMethod, "gems") Then
    If InStr(cboBase, "pest") Then
      If ws = 0 Then
        Message "Invalid value for water solubility.", solIndex
      Else
        koc = 10 ^ (-0.55 * Log10MM(ws) + 3.64)
        SetEstimate "CLKOC", koc
      End If
    Else
      If kow = 0 Then
        Message "Invalid value for Kow.", kowIndex
      Else
        koc = 10 ^ (Log10MM(kow) - 0.21)
        SetEstimate "CLKOC", koc
      End If
    End If
  Else                        'lyman
    If kow = 0 Then
      Message "Invalid value for Kow.", kowIndex
    Else
      If InStr(cboBase, "gen") Then
        koc = 10 ^ (0.544 * Log10MM(kow) + 1.377)
      ElseIf InStr(cboBase, "aro") Then
        koc = 10 ^ (0.937 * Log10MM(kow) - 0.006)
      ElseIf InStr(cboBase, "s-tri") Then
        koc = 10 ^ (0.94 * Log10MM(kow) + 0.02)
      Else
        koc = 10 ^ (1.029 * Log10MM(kow) - 0.18)
      End If
      SetEstimate "CLKOC", koc
    End If
  End If
End Sub

Private Sub EstimateHLC()
Dim hlc As Double
Dim vap As Double
Dim mw As Double
Dim ws As Double

  vap = Val(Text1(vpIndex))
  mw = Val(Text1(mwIndex))
  ws = Val(Text1(solIndex))
  If vap = 0 Or mw = 0 Or ws = 0 Then
    Message "Not enough information to calculate Henry's Law Constant.", -1
  Else
    hlc = (vap * mw) / (ws * 760)
    SetEstimate "CLHLC", hlc
  End If
End Sub

Private Sub Message(msg As String, ix As Long)
  StatusBar1.Panels(2) = ""
  StatusBar1.Panels(1) = msg
  If ix >= 0 Then
    HelpAnchor = Text1(ix).tag
    SetRangeDisplay StatusBar1, Val(Val(frmCDBE.chkRad.value))
  End If
End Sub

Private Sub cboProp_Click()
  If cboProp.ListIndex < 0 Then Exit Sub
  
  Message "", -1
  vaSpread1.TopRow = cboProp.itemdata(cboProp.ListIndex)
  Select Case cboProp.ListIndex
    Case 0:           SetControls ""
    Case selSOL:      SetControls "CLSOL"
    Case selHLC:      SetControls "CLHLC"
    Case selKOC:      SetControls "CLKOC"
    Case selKP:       SetControls "CLKPERM"
    Case selETF:      SetControls "ETF"
    Case selBFF:      SetControls "CLBFF"
  End Select
  Calculate
  vaSpread1_Click 0, vaSpread1.TopRow
End Sub

Private Sub Form_Unload(Cancel As Integer)
  EstimateLoaded = False
End Sub

Private Sub mnuHelp_Click()
  frmCDBE.howto_Click
End Sub

Private Sub Text1_Change(Index As Integer)
  StatusBar1.Panels(1) = ""
  If Not ValidInput(Text1(Index).tag, Text1(Index)) Then
    Text1(Index).BackColor = lightRed
  Else
    Text1(Index).BackColor = lightGreen
  End If
  Calculate
End Sub

Private Sub SetEstimate(sName As Variant, result As Variant)
Dim i As Long
Dim var As Variant

  For i = 1 To vaSpread1.MaxRows
    vaSpread1.row = i
    vaSpread1.col = 3
    vaSpread1.GetText 0, i, var
    If var = sName Then
      vaSpread1.SetText 3, i, Format$(result, "0.00E+00")
      If ValidInput(sName, result) Then
        vaSpread1.BackColor = lightGreen
        vaSpread1.col = 4
        vaSpread1.value = 1
      Else
        vaSpread1.BackColor = lightRed
        vaSpread1.col = 4
        vaSpread1.value = 0
      End If
      Exit For
    End If
  Next
  StatusBar1.Panels(1) = ""
End Sub

Private Sub Calculate()
  Select Case cboProp.ListIndex
    Case selSOL:      EstimateSOL
    Case selHLC:      EstimateHLC
    Case selKOC:      EstimateKOC
    Case selKP:       EstimateKP
    Case selETF:      EstimateETF
    Case selBFF:      EstimateBFF
  End Select
End Sub

Private Sub Text1_GotFocus(Index As Integer)
  HelpAnchor = Text1(Index).tag
  SetRangeDisplay StatusBar1, Val(frmCDBE.chkRad.value)
End Sub

Private Sub vaSpread1_Click(ByVal col As Long, ByVal row As Long)
Dim var As Variant

  vaSpread1.GetText 0, row, var
  HelpAnchor = var
End Sub

Private Sub vaSpread1_LeaveCell(ByVal col As Long, ByVal row As Long, ByVal NewCol As Long, ByVal NewRow As Long, Cancel As Boolean)
Dim var As Variant

  If NewCol < 0 Or NewRow < 0 Then Exit Sub
  vaSpread1.GetText 0, NewRow, var
  HelpAnchor = var
  SetRangeDisplay StatusBar1, Val(frmCDBE.chkRad.value)
End Sub
