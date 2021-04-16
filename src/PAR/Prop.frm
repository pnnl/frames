VERSION 5.00
Begin VB.Form frmProp 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Properties"
   ClientHeight    =   1755
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   5145
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1755
   ScaleWidth      =   5145
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.ComboBox cboDLine 
      Height          =   315
      Left            =   1245
      Style           =   2  'Dropdown List
      TabIndex        =   5
      Top             =   690
      Width           =   1440
   End
   Begin VB.ComboBox cboDColor 
      Height          =   315
      Left            =   1260
      Style           =   2  'Dropdown List
      TabIndex        =   4
      Tag             =   "polygon"
      Top             =   255
      Width           =   1440
   End
   Begin VB.ComboBox cboDFill 
      Height          =   315
      Left            =   1245
      Style           =   2  'Dropdown List
      TabIndex        =   3
      Top             =   1185
      Width           =   1440
   End
   Begin VB.PictureBox picDLayerColor 
      Height          =   315
      HelpContextID   =   301
      Left            =   2730
      MouseIcon       =   "Prop.frx":0000
      ScaleHeight     =   255
      ScaleWidth      =   285
      TabIndex        =   2
      Top             =   240
      WhatsThisHelpID =   301
      Width           =   345
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Cancel"
      Height          =   375
      Left            =   3840
      TabIndex        =   1
      Top             =   840
      Width           =   1095
   End
   Begin VB.CommandButton Command1 
      Caption         =   "OK"
      Default         =   -1  'True
      Height          =   375
      Left            =   3840
      TabIndex        =   0
      Top             =   240
      Width           =   1095
   End
   Begin VB.Label lblDLine 
      AutoSize        =   -1  'True
      Caption         =   "Line Style:"
      Height          =   195
      Left            =   240
      TabIndex        =   8
      Top             =   750
      Width           =   735
   End
   Begin VB.Label lblDColor 
      AutoSize        =   -1  'True
      Caption         =   "Color:"
      Height          =   195
      Left            =   255
      TabIndex        =   7
      Top             =   300
      Width           =   405
   End
   Begin VB.Label lblDFill 
      AutoSize        =   -1  'True
      Caption         =   "Fill Style:"
      Height          =   195
      Left            =   255
      TabIndex        =   6
      Top             =   1260
      Width           =   615
   End
End
Attribute VB_Name = "frmProp"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Compare Text
Option Explicit

Private desc As String

Private Sub cboDColor_Click()
  picDLayerColor.BackColor = QBColor(cboDColor.ItemData(cboDColor.ListIndex))
End Sub

Private Sub Command1_Click()
Dim recno As Long
  recno = frmMain.Dbcocx1.devrecno()
  Do
    If "E" = Trim(frmMain.Dbcocx1.devgetfield("SECTION")) And _
      Trim(SelectedEntityId) = Trim(frmMain.Dbcocx1.devgetfield("ID")) Then
      If cboDColor.visible Then
        frmMain.Dbcocx1.devsetfield "ECOLOR", CStr(cboDColor.ListIndex + 1)
      End If
      If cboDLine.visible Then
        frmMain.Dbcocx1.devsetfield "ELINE", CStr(cboDLine.ItemData(cboDLine.ListIndex))
      End If
      If cboDFill.visible Then
        frmMain.Dbcocx1.devsetfield "DESCR", Left(desc + Space(10), 10) & CStr(cboDFill.ItemData(cboDFill.ListIndex))
      End If
      frmMain.Dbcocx1.devskip 1
    Else
      Exit Do
    End If
  Loop
  frmMain.Dbcocx1.devgorecno recno
  frmMain.Dbcocx1.devgrclear 0
  frmMain.Dbcocx1.devgrdisplay
  Unload Me
End Sub

Private Sub Command2_Click()
  Unload Me
End Sub

Private Sub Form_Load()
Dim fld As String, lval As Long, i As Long
  frmConfig2.InitColors cboDColor, DPOLY
  frmConfig2.InitLineStyle cboDLine, DPOLY
  frmConfig2.InitFillStyle cboDFill, DPOLY
  
  desc = frmMain.Dbcocx1.devgetfield("DESCR")
  desc = Left(desc, InStr(desc, " ") - 1)
  Select Case desc
    Case TPOINT:
      cboDColor.Enabled = True
      lblDLine.visible = False: cboDLine.visible = False
      lblDFill.visible = False: cboDFill.visible = False
    Case TLINE:
      cboDColor.Enabled = True
      cboDLine.Enabled = True
      lblDFill.visible = False: cboDFill.visible = False
    Case TPOLYGON:
      cboDColor.Enabled = True
      cboDLine.Enabled = True
      cboDFill.Enabled = True
    Case "INSERT": ' polar and cartesian grid
      cboDColor.Enabled = True
      cboDLine.Enabled = True
      lblDFill.visible = False: cboDFill.visible = False
  End Select
  If cboDColor.Enabled Then
    fld = frmMain.Dbcocx1.devgetfield("ECOLOR"): lval = Val(fld)
    If lval >= 0 And lval < cboDColor.ListCount Then
      cboDColor.ListIndex = lval - 1
    Else
      'if lval = 256 then use the layer color
      cboDColor.ListIndex = 6 ' black
    End If
  End If
  If cboDLine.Enabled Then
    fld = frmMain.Dbcocx1.devgetfield("ELINE"): lval = Val(fld)
    cboDLine.ListIndex = 0
    For i = 0 To cboDLine.ListCount - 1
      If cboDLine.ItemData(i) = lval Then
        cboDLine.ListIndex = i
        Exit For
      End If
    Next i
    
  End If
  If cboDFill.Enabled Then
    fld = frmMain.Dbcocx1.devgetfield("DESCR"): lval = Val(Mid$(fld, InStr(fld, " ") + 1))
    cboDFill.ListIndex = 0
    For i = 0 To cboDFill.ListCount - 1
      If cboDFill.ItemData(i) = lval Then
        cboDFill.ListIndex = i
        Exit For
      End If
    Next i
  End If
End Sub

