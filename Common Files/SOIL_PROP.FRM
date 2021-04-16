VERSION 5.00
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form frmSoilTri 
   Appearance      =   0  'Flat
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Soil Triangle"
   ClientHeight    =   5401
   ClientLeft      =   6644
   ClientTop       =   5280
   ClientWidth     =   9603
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   7.67
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H80000008&
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5401
   ScaleWidth      =   9603
   Tag             =   "soiltri"
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "&Cancel"
      Height          =   390
      Left            =   6270
      TabIndex        =   21
      Top             =   4485
      Width           =   1155
   End
   Begin VB.CommandButton cmdApply 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      Caption         =   "&Apply"
      Default         =   -1  'True
      Height          =   390
      Left            =   7800
      TabIndex        =   22
      Top             =   4485
      Width           =   1155
   End
   Begin Threed.SSFrame SSFrame1 
      Height          =   4290
      Index           =   0
      Left            =   5460
      TabIndex        =   0
      Top             =   0
      Width           =   4155
      _Version        =   65536
      _ExtentX        =   7329
      _ExtentY        =   7567
      _StockProps     =   14
      ForeColor       =   -2147483630
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   7.15
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ShadowStyle     =   1
      Begin VB.TextBox txt 
         BackColor       =   &H008080FF&
         Height          =   312
         Index           =   6
         Left            =   3096
         TabIndex        =   20
         Tag             =   "wpiron"
         Top             =   3828
         Width           =   600
      End
      Begin VB.ListBox lstSoils 
         Appearance      =   0  'Flat
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.85
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   1012
         ItemData        =   "SOIL_PROP.frx":0000
         Left            =   195
         List            =   "SOIL_PROP.frx":0028
         TabIndex        =   3
         Top             =   605
         Width           =   3750
      End
      Begin VB.TextBox txt 
         BackColor       =   &H008080FF&
         Height          =   312
         Index           =   5
         Left            =   3084
         TabIndex        =   17
         Tag             =   "wpiron"
         Top             =   3456
         Width           =   600
      End
      Begin VB.TextBox txt 
         BackColor       =   &H008080FF&
         Height          =   312
         Index           =   4
         Left            =   3084
         TabIndex        =   14
         Tag             =   "wpomc"
         Top             =   3096
         Width           =   600
      End
      Begin VB.TextBox txt 
         BackColor       =   &H008080FF&
         Height          =   312
         Index           =   3
         Left            =   3084
         TabIndex        =   11
         Tag             =   "wpclay"
         Top             =   2736
         Width           =   600
      End
      Begin VB.TextBox txt 
         BackColor       =   &H008080FF&
         Height          =   312
         Index           =   2
         Left            =   3090
         TabIndex        =   8
         Tag             =   "wpsilt"
         Top             =   2355
         Width           =   600
      End
      Begin VB.TextBox txt 
         BackColor       =   &H008080FF&
         Height          =   312
         Index           =   1
         Left            =   3084
         TabIndex        =   5
         Tag             =   "wpsand"
         Top             =   2010
         Width           =   600
      End
      Begin VB.Label lblParam 
         Caption         =   "% iron and aluminum - KD-IRON"
         Height          =   216
         Index           =   5
         Left            =   132
         TabIndex        =   16
         Top             =   3540
         Width           =   2808
      End
      Begin VB.Label lblParam 
         Caption         =   "% organic matter - KD-OMC"
         Height          =   216
         Index           =   4
         Left            =   120
         TabIndex        =   13
         Top             =   3168
         Width           =   2808
      End
      Begin VB.Label lblParam 
         Caption         =   "% clay - KD-CLAY"
         Height          =   216
         Index           =   3
         Left            =   156
         TabIndex        =   10
         Top             =   2784
         Width           =   2808
      End
      Begin VB.Label lblParam 
         Caption         =   "% silt - KD-SILT"
         Height          =   210
         Index           =   2
         Left            =   150
         TabIndex        =   7
         Top             =   2415
         Width           =   2805
      End
      Begin VB.Label lblParam 
         Caption         =   "% sand - KD-SAND"
         Height          =   210
         Index           =   1
         Left            =   150
         TabIndex        =   4
         Top             =   2070
         Width           =   2805
      End
      Begin VB.Label lblParam 
         Caption         =   "pH of soil - KD-PH"
         Height          =   216
         Index           =   0
         Left            =   144
         TabIndex        =   19
         Top             =   3900
         Width           =   2808
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   1
         Left            =   5760
         TabIndex        =   42
         Tag             =   "0"
         Top             =   960
         Width           =   1000
      End
      Begin VB.Label Label33 
         Caption         =   "%"
         Height          =   264
         Left            =   3756
         TabIndex        =   18
         Top             =   3480
         Width           =   192
      End
      Begin VB.Label Label32 
         Caption         =   "%"
         Height          =   270
         Left            =   3765
         TabIndex        =   15
         Top             =   3120
         Width           =   195
      End
      Begin VB.Label Label28 
         Caption         =   "%"
         Height          =   270
         Left            =   3750
         TabIndex        =   12
         Top             =   2745
         Width           =   195
      End
      Begin VB.Label Label27 
         Caption         =   "%"
         Height          =   270
         Left            =   3765
         TabIndex        =   9
         Top             =   2400
         Width           =   195
      End
      Begin VB.Label Label1 
         Caption         =   "%"
         Height          =   264
         Left            =   3756
         TabIndex        =   6
         Top             =   2040
         Width           =   192
      End
      Begin VB.Label Label29 
         Caption         =   "Texture                        %Sand    %Silt     %Clay"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   7.67
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   165
         Left            =   195
         TabIndex        =   2
         Top             =   405
         Width           =   3735
      End
      Begin VB.Label Label2 
         Caption         =   "Soil class - KD-CLASS"
         Height          =   252
         Left            =   168
         TabIndex        =   1
         Top             =   156
         Width           =   2088
      End
   End
   Begin Threed.SSPanel panRange 
      Height          =   300
      Left            =   45
      TabIndex        =   23
      Top             =   5070
      Width           =   9510
      _Version        =   65536
      _ExtentX        =   16775
      _ExtentY        =   529
      _StockProps     =   15
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   7.22
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      BevelOuter      =   1
      Alignment       =   1
   End
   Begin VB.Image Image1 
      Appearance      =   0  'Flat
      BorderStyle     =   1  'Fixed Single
      Height          =   5025
      Left            =   45
      Picture         =   "SOIL_PROP.frx":0160
      Stretch         =   -1  'True
      Top             =   0
      Width           =   5355
   End
   Begin VB.Label lblValue 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      ForeColor       =   &H80000008&
      Height          =   240
      Index           =   2
      Left            =   4350
      TabIndex        =   41
      Top             =   2310
      Visible         =   0   'False
      Width           =   615
   End
   Begin VB.Label lblValue 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      ForeColor       =   &H80000008&
      Height          =   240
      Index           =   1
      Left            =   2685
      TabIndex        =   40
      Top             =   2310
      Visible         =   0   'False
      Width           =   525
   End
   Begin VB.Label lblValue 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      ForeColor       =   &H80000008&
      Height          =   240
      Index           =   0
      Left            =   1215
      TabIndex        =   39
      Top             =   2310
      Visible         =   0   'False
      Width           =   555
   End
   Begin VB.Label lblUnit 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      ForeColor       =   &H80000008&
      Height          =   240
      Index           =   4
      Left            =   8535
      TabIndex        =   38
      Top             =   4470
      Width           =   645
   End
   Begin VB.Label lblUnit 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      ForeColor       =   &H80000008&
      Height          =   240
      Index           =   3
      Left            =   8535
      TabIndex        =   37
      Top             =   4080
      Width           =   645
   End
   Begin VB.Label lblUnit 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      ForeColor       =   &H80000008&
      Height          =   240
      Index           =   2
      Left            =   8535
      TabIndex        =   36
      Top             =   3735
      Width           =   645
   End
   Begin VB.Label lblUnit 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      ForeColor       =   &H80000008&
      Height          =   240
      Index           =   1
      Left            =   8535
      TabIndex        =   35
      Top             =   3375
      Width           =   645
   End
   Begin VB.Label lblUnit 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      ForeColor       =   &H80000008&
      Height          =   240
      Index           =   0
      Left            =   8535
      TabIndex        =   34
      Top             =   2970
      Width           =   645
   End
   Begin VB.Label lblValue 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      ForeColor       =   &H80000008&
      Height          =   240
      Index           =   7
      Left            =   7365
      TabIndex        =   33
      Top             =   4470
      Width           =   1080
   End
   Begin VB.Label lblValue 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      ForeColor       =   &H80000008&
      Height          =   240
      Index           =   6
      Left            =   7365
      TabIndex        =   32
      Top             =   4095
      Width           =   1080
   End
   Begin VB.Label lblValue 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      ForeColor       =   &H80000008&
      Height          =   240
      Index           =   5
      Left            =   7365
      TabIndex        =   31
      Top             =   3735
      Width           =   1080
   End
   Begin VB.Label lblValue 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      ForeColor       =   &H80000008&
      Height          =   240
      Index           =   4
      Left            =   7365
      TabIndex        =   30
      Top             =   3375
      Width           =   1080
   End
   Begin VB.Label lblValue 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      ForeColor       =   &H80000008&
      Height          =   240
      Index           =   3
      Left            =   7365
      TabIndex        =   29
      Top             =   2985
      Width           =   1080
   End
   Begin VB.Label lblParam 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      ForeColor       =   &H80000008&
      Height          =   240
      Index           =   7
      Left            =   5505
      TabIndex        =   28
      Top             =   4470
      Width           =   1755
   End
   Begin VB.Label lblParam 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      ForeColor       =   &H80000008&
      Height          =   240
      Index           =   6
      Left            =   5505
      TabIndex        =   27
      Top             =   4095
      Width           =   1755
   End
   Begin VB.Label lblParam 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      ForeColor       =   &H80000008&
      Height          =   240
      Index           =   16
      Left            =   5505
      TabIndex        =   26
      Top             =   3735
      Width           =   1755
   End
   Begin VB.Label lblParam 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      BackStyle       =   0  'Transparent
      ForeColor       =   &H80000008&
      Height          =   240
      Index           =   13
      Left            =   5505
      TabIndex        =   25
      Top             =   3375
      Width           =   1755
   End
   Begin VB.Label lblParam 
      Appearance      =   0  'Flat
      BackColor       =   &H000000FF&
      BorderStyle     =   1  'Fixed Single
      ForeColor       =   &H80000008&
      Height          =   240
      Index           =   12
      Left            =   7455
      TabIndex        =   24
      Top             =   1620
      Width           =   1755
   End
End
Attribute VB_Name = "frmSoilTri"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub cmdApply_Click()
Dim sum As Double, i As Integer

  For i = fSAND To fPH
    If er(i) Then Exit Sub
    If i >= fSAND And i <= fIRON Then
      sum = sum + Val(txt(i))
    End If
  Next
  If sum > (100 + 0.001) Or sum < (100 - 0.001) Then
    MsgBox "Sum of percentages must equal 100 (+/- " & 0.001 & ")"
    Exit Sub
  End If
  
  SetSoilParams lstSoils.ListIndex, txt(fSAND), txt(fSILT), txt(fCLAY), txt(fOMC), txt(fIRON), txt(fPH)
  'SetParamElem i, "KDCLASS", 0, 0, E_USERVAL, lstSoils.ListIndex
  'SetParamElem i, "KDSAND", 0, 0, E_USERVAL, txt(fSAND)
  'SetParamElem i, "KDSILT", 0, 0, E_USERVAL, txt(fSILT)
  'SetParamElem i, "KDCLAY", 0, 0, E_USERVAL, txt(fCLAY)
  'SetParamElem i, "KDOMC", 0, 0, E_USERVAL, txt(fOMC)
  'SetParamElem i, "KDIRON", 0, 0, E_USERVAL, txt(fIRON)
  'SetParamElem i, "KDPH", 0, 0, E_USERVAL, txt(fPH)
  Me.Hide
End Sub

Private Sub cmdCancel_Click()
  Me.Hide
  LoadSoilParams  ' restore
End Sub

Private Sub Form_load()
  MousePointer = vbHourglass
  CenterForm Me
  LoadSoilParams
  MousePointer = vbDefault
End Sub

Private Sub lblParam_Click(Index As Integer)
  If Index >= fSAND And Index <= fPH Then txt_GotFocus Index
End Sub

Private Sub lstSoils_Click()
Dim v
  v = vfProcessUserInput
  vfProcessUserInput = False
  Select Case lstSoils.ListIndex
  Case 0:    changetxt 92, 5, 3, 0.0066, 38, 1.64, 9#, 4.05
  Case 1:    changetxt 83, 11, 6, 0.0019, 43.7, 1.49, 12#, 4.38
  Case 2:    changetxt 65, 25, 10, 0.00072, 44.2, 1.48, 17.5, 4.9
  Case 3:    changetxt 42, 38, 20, 0.00037, 46.6, 1.42, 23.5, 5.39
  Case 4:    changetxt 20, 65, 15, 0.0002, 46.3, 1.42, 27.5, 5.3
  Case 5:    changetxt 7, 88, 5, 0.00013, 44.2, 1.48, 28#, 5.3
  Case 6:    changetxt 60, 14, 26, 0.00011, 39.8, 1.6, 24#, 7.12
  Case 7:    changetxt 32, 35, 33, 0.000062, 47.7, 1.39, 34#, 8.52
  Case 8:    changetxt 10, 57, 33, 0.000046, 49#, 1.35, 37.5, 7.75
  Case 9:    changetxt 52, 7, 41, 0.000034, 43#, 1.51, 32#, 10.4
  Case 10:   changetxt 7, 46, 47, 0.000026, 48.6, 1.36, 42#, 10.4
  Case 11:   changetxt 20, 20, 60, 0.000019, 47.5, 1.39, 40#, 11.4
  End Select
  vfProcessUserInput = v
End Sub
Private Sub changetxt(i As Integer, j As Integer, k As Integer, f1 As Double, f2 As Double, f3 As Double, f4 As Double, f5 As Double)
' If Not loadng Then
    txt(fSAND).Text = i
    txt(fSILT).Text = j
    txt(fCLAY).Text = k
    txt(fOMC).Text = 0
    txt(fIRON).Text = 0
'   If auto.Checked Then
'     set_unit unit(9), unit(9).Tag
'     set_unit unit(11), unit(11).Tag
'     txt(9).Text = f1 * 86400
'     txt(7).Text = f2
'     txt(11).Text = f3
'     txt(8).Text = f4
'     txt(16).Text = f5
'   End If
' End If
End Sub

Private Sub lstSoils_GotFocus()
  panRange = ""
End Sub

Private Sub txt_Change(Index As Integer)
Dim chk As Double
  On Error GoTo toolarge
  er Index
  chk = CDbl(txt(Index).Text)
  Exit Sub
toolarge:
  txt(Index).BackColor = &H8080FF
End Sub
Private Function er(Index As Integer) As Boolean
  Dim tval As Double
  
  er = False
    tval = Val(txt(Index).Text)
    Select Case Index
      Case fSAND, fSILT, fCLAY, fOMC, fIRON
        er = (tval < 0 Or tval > 100)
        If er And vfProcessUserInput Then
          MsgBox "Please enter value in the range 0 <= x <= 100"
        End If
      Case fPH
        er = (tval < 0.1 Or tval > 14)
        If er And vfProcessUserInput Then
          MsgBox "Please enter KD-PH in the range .1 <= x <= 14"
        End If
    End Select
  If er Then
    txt(Index).BackColor = ERR_RED
  Else
    txt(Index).BackColor = INP_GRN
  End If
End Function

Private Sub txt_GotFocus(Index As Integer)
Dim range As String
  Select Case Index
      Case fSAND, fSILT, fCLAY, fOMC, fIRON
        range = "0 <= x <= 100"
      Case fPH
        range = ".1 <= x <= 14"
  End Select
  panRange = Space(2) & "Range: " & range
End Sub
