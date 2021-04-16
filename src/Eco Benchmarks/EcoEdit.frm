VERSION 5.00
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "comctl32.ocx"
Object = "{48E59290-9880-11CF-9754-00AA00C00908}#1.0#0"; "msinet.ocx"
Object = "{B02F3647-766B-11CE-AF28-C3A2FBE76A13}#2.5#0"; "ss32x25.ocx"
Begin VB.Form EcoEdit 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Data Editor"
   ClientHeight    =   7608
   ClientLeft      =   4572
   ClientTop       =   3312
   ClientWidth     =   10584
   Icon            =   "EcoEdit.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   634
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   882
   StartUpPosition =   2  'CenterScreen
   Begin VB.Frame Frame8 
      Height          =   7455
      Left            =   120
      TabIndex        =   0
      Top             =   0
      Width           =   4815
      Begin VB.CheckBox Check2 
         Caption         =   "Full View"
         Height          =   255
         Left            =   120
         TabIndex        =   2
         Top             =   7080
         Width           =   1335
      End
      Begin ComctlLib.TreeView TreeView1 
         Height          =   6735
         Left            =   120
         TabIndex        =   1
         Top             =   240
         Width           =   4575
         _ExtentX        =   8065
         _ExtentY        =   11875
         _Version        =   327682
         HideSelection   =   0   'False
         Indentation     =   441
         LabelEdit       =   1
         LineStyle       =   1
         Style           =   7
         ImageList       =   "ImageList1"
         Appearance      =   1
      End
      Begin ComctlLib.ImageList ImageList1 
         Left            =   2160
         Top             =   3120
         _ExtentX        =   995
         _ExtentY        =   995
         BackColor       =   -2147483643
         ImageWidth      =   16
         ImageHeight     =   16
         MaskColor       =   12632256
         _Version        =   327682
         BeginProperty Images {0713E8C2-850A-101B-AFC0-4210102A8DA7} 
            NumListImages   =   5
            BeginProperty ListImage1 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
               Picture         =   "EcoEdit.frx":030A
               Key             =   "Data"
            EndProperty
            BeginProperty ListImage2 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
               Picture         =   "EcoEdit.frx":0624
               Key             =   "NoData"
            EndProperty
            BeginProperty ListImage3 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
               Picture         =   "EcoEdit.frx":093E
               Key             =   "Point1"
            EndProperty
            BeginProperty ListImage4 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
               Picture         =   "EcoEdit.frx":0C58
               Key             =   "Point2"
            EndProperty
            BeginProperty ListImage5 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
               Picture         =   "EcoEdit.frx":0F72
               Key             =   "Logo"
            EndProperty
         EndProperty
      End
   End
   Begin VB.Frame Frame7 
      BackColor       =   &H00C0706D&
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   7455
      Left            =   5040
      TabIndex        =   35
      Tag             =   "top"
      Top             =   0
      Width           =   5415
      Begin VB.Image Image1 
         Height          =   3570
         Left            =   120
         Picture         =   "EcoEdit.frx":128C
         Stretch         =   -1  'True
         Top             =   1440
         Width           =   5160
      End
   End
   Begin InetCtlsObjects.Inet Inet1 
      Left            =   0
      Top             =   0
      _ExtentX        =   995
      _ExtentY        =   995
      _Version        =   393216
      Protocol        =   0
      RemotePort      =   443
      URL             =   "https://"
   End
   Begin VB.Frame Frame1 
      Caption         =   "Data"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   7455
      Left            =   5040
      TabIndex        =   30
      Tag             =   "retrieve"
      Top             =   0
      Visible         =   0   'False
      Width           =   5415
      Begin VB.Timer Timer1 
         Enabled         =   0   'False
         Interval        =   500
         Left            =   3720
         Top             =   4080
      End
      Begin VB.CommandButton Command1 
         Caption         =   "Retreive Data"
         Height          =   495
         Left            =   240
         TabIndex        =   32
         Top             =   5760
         Width           =   4935
      End
      Begin ComctlLib.ProgressBar ProgressBar1 
         Height          =   375
         Left            =   240
         TabIndex        =   34
         Top             =   6840
         Visible         =   0   'False
         Width           =   4935
         _ExtentX        =   8700
         _ExtentY        =   656
         _Version        =   327682
         Appearance      =   1
         Max             =   150
      End
      Begin VB.Label Label8 
         BackColor       =   &H8000000B&
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Label8"
         Height          =   5055
         Left            =   240
         TabIndex        =   31
         Top             =   360
         Width           =   4935
         WordWrap        =   -1  'True
      End
      Begin VB.Label Label7 
         Caption         =   "Label7"
         Height          =   375
         Left            =   240
         TabIndex        =   33
         Top             =   6480
         Visible         =   0   'False
         Width           =   3735
      End
   End
   Begin VB.Frame Frame5 
      Caption         =   "Frame5"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   7455
      Left            =   5040
      TabIndex        =   19
      Tag             =   "view"
      Top             =   0
      Visible         =   0   'False
      Width           =   5415
      Begin VB.ComboBox Idx 
         Height          =   315
         Index           =   0
         Left            =   240
         Style           =   2  'Dropdown List
         TabIndex        =   21
         Tag             =   "view"
         Top             =   600
         Width           =   4900
      End
      Begin VB.ComboBox Idx 
         Height          =   315
         Index           =   1
         Left            =   240
         Style           =   2  'Dropdown List
         TabIndex        =   22
         Tag             =   "view"
         Top             =   1320
         Width           =   4900
      End
      Begin VB.ComboBox Idx 
         Height          =   315
         Index           =   2
         Left            =   240
         Style           =   2  'Dropdown List
         TabIndex        =   23
         Tag             =   "view"
         Top             =   2040
         Width           =   4900
      End
      Begin VB.ComboBox Idx 
         Height          =   315
         Index           =   3
         Left            =   240
         Style           =   2  'Dropdown List
         TabIndex        =   24
         Tag             =   "view"
         Top             =   2760
         Width           =   4900
      End
      Begin VB.ComboBox Idx 
         Height          =   315
         Index           =   4
         Left            =   240
         Style           =   2  'Dropdown List
         TabIndex        =   25
         Tag             =   "view"
         Top             =   3480
         Width           =   4900
      End
      Begin VB.ComboBox Idx 
         Height          =   315
         Index           =   5
         Left            =   240
         Style           =   2  'Dropdown List
         TabIndex        =   27
         Tag             =   "view"
         Top             =   4200
         Width           =   2145
      End
      Begin FPSpreadADO.fpSpread vaSpread1 
         Height          =   2595
         Left            =   240
         TabIndex        =   29
         Tag             =   "view"
         Top             =   4680
         Width           =   4905
         _Version        =   131077
         _ExtentX        =   8636
         _ExtentY        =   4043
         _StockProps     =   64
         ArrowsExitEditMode=   -1  'True
         AutoCalc        =   0   'False
         BackColorStyle  =   1
         EditEnterAction =   5
         EditModeReplace =   -1  'True
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         FormulaSync     =   0   'False
         MaxCols         =   1
         MaxRows         =   5000
         ScrollBarExtMode=   -1  'True
         ScrollBarShowMax=   0   'False
         SpreadDesigner  =   "EcoEdit.frx":1BF0
         VisibleCols     =   1
         VisibleRows     =   9
      End
      Begin VB.Label Label10 
         Caption         =   "Units"
         Height          =   240
         Left            =   2520
         TabIndex        =   26
         Top             =   3960
         Width           =   1095
      End
      Begin VB.Label Label9 
         BackColor       =   &H80000009&
         BorderStyle     =   1  'Fixed Single
         Height          =   290
         Left            =   2520
         TabIndex        =   28
         Tag             =   "view"
         Top             =   4200
         Width           =   2660
      End
      Begin VB.Label Label3 
         Caption         =   " "
         Height          =   240
         Index           =   1
         Left            =   120
         TabIndex        =   46
         Top             =   1080
         Width           =   4905
         WordWrap        =   -1  'True
      End
      Begin VB.Label Label3 
         Caption         =   " "
         Height          =   240
         Index           =   2
         Left            =   120
         TabIndex        =   45
         Top             =   1800
         Width           =   4905
         WordWrap        =   -1  'True
      End
      Begin VB.Label Label3 
         Caption         =   " "
         Height          =   240
         Index           =   3
         Left            =   120
         TabIndex        =   44
         Top             =   2520
         Width           =   4905
         WordWrap        =   -1  'True
      End
      Begin VB.Label Label3 
         Caption         =   " "
         Height          =   240
         Index           =   4
         Left            =   120
         TabIndex        =   43
         Top             =   3240
         Width           =   4905
         WordWrap        =   -1  'True
      End
      Begin VB.Label Label3 
         Caption         =   " "
         Height          =   240
         Index           =   5
         Left            =   120
         TabIndex        =   42
         Top             =   3960
         Width           =   2265
         WordWrap        =   -1  'True
      End
      Begin VB.Label Label3 
         Caption         =   "Label3(0)"
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   20
         Top             =   375
         Width           =   4905
      End
      Begin VB.Label Label4 
         BackColor       =   &H80000009&
         BorderStyle     =   1  'Fixed Single
         Height          =   290
         Index           =   0
         Left            =   240
         TabIndex        =   41
         Top             =   600
         Visible         =   0   'False
         Width           =   4910
      End
      Begin VB.Label Label4 
         BackColor       =   &H80000009&
         BorderStyle     =   1  'Fixed Single
         Height          =   290
         Index           =   1
         Left            =   240
         TabIndex        =   40
         Top             =   1320
         Visible         =   0   'False
         Width           =   4910
      End
      Begin VB.Label Label4 
         BackColor       =   &H80000009&
         BorderStyle     =   1  'Fixed Single
         Height          =   290
         Index           =   2
         Left            =   240
         TabIndex        =   39
         Top             =   2040
         Visible         =   0   'False
         Width           =   4910
      End
      Begin VB.Label Label4 
         BackColor       =   &H80000009&
         BorderStyle     =   1  'Fixed Single
         Height          =   290
         Index           =   3
         Left            =   240
         TabIndex        =   38
         Top             =   2760
         Visible         =   0   'False
         Width           =   4910
      End
      Begin VB.Label Label4 
         BackColor       =   &H80000009&
         BorderStyle     =   1  'Fixed Single
         Height          =   290
         Index           =   4
         Left            =   240
         TabIndex        =   37
         Top             =   3480
         Visible         =   0   'False
         Width           =   4910
      End
      Begin VB.Label Label4 
         BackColor       =   &H80000009&
         BorderStyle     =   1  'Fixed Single
         Height          =   290
         Index           =   5
         Left            =   240
         TabIndex        =   36
         Top             =   4200
         Visible         =   0   'False
         Width           =   2030
      End
   End
   Begin VB.Frame Frame4 
      Caption         =   "Organisms of Concern for this Assessment (Scientific Names)"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   7455
      Left            =   5040
      TabIndex        =   15
      Tag             =   "select"
      Top             =   0
      Visible         =   0   'False
      Width           =   5415
      Begin VB.CommandButton Command2 
         Caption         =   "Add &Organism"
         Height          =   495
         Left            =   3240
         TabIndex        =   17
         Tag             =   "add"
         Top             =   360
         Width           =   1935
      End
      Begin VB.ListBox OrgList 
         Height          =   5010
         Index           =   0
         Left            =   240
         Style           =   1  'Checkbox
         TabIndex        =   18
         Tag             =   "select"
         Top             =   1080
         Width           =   4935
      End
      Begin VB.CheckBox Check1 
         Caption         =   "Use Common Names"
         Height          =   255
         Index           =   2
         Left            =   240
         TabIndex        =   16
         Tag             =   "select"
         Top             =   360
         Width           =   2415
      End
      Begin VB.ListBox OrgList 
         Height          =   5010
         Index           =   1
         Left            =   240
         Style           =   1  'Checkbox
         TabIndex        =   51
         Top             =   1080
         Visible         =   0   'False
         Width           =   4935
      End
   End
   Begin VB.Frame Frame3 
      Caption         =   "Constituent Aliases"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   7455
      Left            =   5040
      TabIndex        =   3
      Tag             =   "chemical"
      Top             =   0
      Visible         =   0   'False
      Width           =   5415
      Begin VB.ListBox ChemSelect 
         Height          =   4848
         Index           =   0
         Left            =   240
         TabIndex        =   6
         Tag             =   "select"
         Top             =   1080
         Width           =   4935
      End
      Begin VB.ComboBox ChemAliasList 
         Enabled         =   0   'False
         Height          =   315
         Index           =   0
         Left            =   240
         Style           =   2  'Dropdown List
         TabIndex        =   8
         Tag             =   "chemical"
         Top             =   6600
         Width           =   4935
      End
      Begin VB.CheckBox Check1 
         Caption         =   "Use Constituent Names"
         Height          =   255
         Index           =   1
         Left            =   240
         TabIndex        =   4
         Tag             =   "chemical"
         Top             =   360
         Width           =   2655
      End
      Begin VB.ListBox ChemSelect 
         Height          =   3888
         Index           =   1
         Left            =   240
         TabIndex        =   48
         Top             =   1080
         Visible         =   0   'False
         Width           =   4935
      End
      Begin VB.ComboBox ChemAliasList 
         Enabled         =   0   'False
         Height          =   315
         Index           =   1
         Left            =   240
         Style           =   2  'Dropdown List
         TabIndex        =   47
         Top             =   6600
         Visible         =   0   'False
         Width           =   4935
      End
      Begin VB.Label Label1 
         Caption         =   "Selected Constituents of Concern for this Assessment"
         Height          =   255
         Left            =   240
         TabIndex        =   5
         Top             =   840
         Width           =   4935
      End
      Begin VB.Label Label2 
         Caption         =   "Aliased CAS ID for selected constituent"
         Height          =   255
         Left            =   240
         TabIndex        =   7
         Top             =   6360
         Width           =   4935
      End
   End
   Begin VB.Frame Frame6 
      Caption         =   "Mapping Organism Database Names"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   7455
      Left            =   5040
      TabIndex        =   9
      Tag             =   "organism"
      Top             =   0
      Visible         =   0   'False
      Width           =   5415
      Begin VB.CheckBox Check1 
         Caption         =   "Use Common Names"
         Height          =   255
         Index           =   3
         Left            =   240
         TabIndex        =   10
         Tag             =   "chemical"
         Top             =   360
         Width           =   2655
      End
      Begin VB.ListBox OrgSelect 
         Height          =   4848
         Index           =   0
         Left            =   240
         TabIndex        =   12
         Tag             =   "select"
         Top             =   1080
         Width           =   4935
      End
      Begin VB.ComboBox OrgAliasList 
         Enabled         =   0   'False
         Height          =   315
         Index           =   0
         Left            =   240
         Style           =   2  'Dropdown List
         TabIndex        =   14
         Tag             =   "chemical"
         Top             =   6600
         Width           =   5055
      End
      Begin VB.ListBox OrgSelect 
         Height          =   3888
         Index           =   1
         Left            =   240
         TabIndex        =   50
         Top             =   1080
         Visible         =   0   'False
         Width           =   4935
      End
      Begin VB.ComboBox OrgAliasList 
         Enabled         =   0   'False
         Height          =   315
         Index           =   1
         Left            =   240
         Style           =   2  'Dropdown List
         TabIndex        =   49
         Top             =   6600
         Visible         =   0   'False
         Width           =   5055
      End
      Begin VB.Label Label6 
         Caption         =   "Aliased scientific name for selected organism"
         Height          =   255
         Left            =   240
         TabIndex        =   13
         Top             =   6360
         Width           =   5055
      End
      Begin VB.Label Label5 
         Caption         =   "Selected Organisms of Concern for this Assessment"
         Height          =   255
         Left            =   240
         TabIndex        =   11
         Top             =   840
         Width           =   4935
      End
   End
   Begin VB.Menu File 
      Caption         =   "&File"
      Begin VB.Menu save 
         Caption         =   "&Save and Exit"
      End
      Begin VB.Menu leave 
         Caption         =   "E&xit"
      End
   End
   Begin VB.Menu help 
      Caption         =   "&Help"
      Begin VB.Menu howto 
         Caption         =   "How to ..."
      End
      Begin VB.Menu about 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "EcoEdit"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Const checked = 1
Const unchecked = 0

Dim AppPath As String
Dim dicPath As String
Dim notkeys As New Collection
Dim formload As Boolean
Dim iniPath As String
Dim UsrModName As String
Dim idxclick As Boolean
Dim listclick As Boolean
Dim comboclick As Boolean
Dim treeviewkey As String

Public Function setUrl(uName As String, pWord As String, uString As String) As String
Dim Index As Integer
Dim header As String
Dim url As String
  
  Index = InStr(1, uString, "//", vbTextCompare) + 1
  header = Mid(uString, 1, Index)
  url = Mid(uString, Index + 1, Len(uString))
  setUrl = header + uName + ":" + pWord + "@" + url
End Function

Public Function ReturnURL(stindex As Integer, tstring As String) As String
Dim i As Integer
Dim tempchar As String
Dim outstring As String

  outstring = ""
  For i = stindex To Len(tstring)
    tempchar = Mid(tstring, i, 1)
    If tempchar = " " Then
      Exit For
    Else
      outstring = outstring + tempchar
    End If
  Next
  ReturnURL = outstring
End Function

Public Function NumBoxPresent() As Boolean
Dim i As Integer
  
  NumBoxPresent = False
  For i = 0 To 5
    If Label4(i).Visible Then NumBoxPresent = True
  Next
End Function

Public Function NumCombosPresent() As Integer
Dim i As Integer
  
  NumCombosPresent = 0
  For i = 0 To 5
    If Idx(i).Visible Then NumCombosPresent = NumCombosPresent + 1
  Next
End Function

Public Function MakeTreeNodes() As Boolean
Dim i As Integer
Dim dummy As String
Dim nodx As Node
  
On Error GoTo TREEERROR
  MakeTreeNodes = False
  TreeView1.Nodes.Clear
  Set nodx = TreeView1.Nodes.Add(, , "R", "Data Editor", 5, 3)
  nodx.Expanded = True
  Set nodx = TreeView1.Nodes.Add("R", tvwChild, "chemaka", "Constituent Aliases", 1, 3)
  nodx.Expanded = True
  Set nodx = TreeView1.Nodes.Add("R", tvwChild, "lifesel", "Organism Selection", 1, 3)
  nodx.Expanded = True
  Set nodx = TreeView1.Nodes.Add("lifesel", tvwChild, "lifeaka", "Organism Aliases", 1, 3)
  nodx.Expanded = True
  dummy = "Viewing and Editing " + ModName + ""
  Set nodx = TreeView1.Nodes.Add("R", tvwChild, "data", dummy, 1, 3)
  nodx.Expanded = True
  For i = 1 To vars.count
    dummy = vars.item(i).name
    If (InStr(dummy, "Num") = 0) And (InStr(dummy, "~Table") = 0) And _
       (InStr(dummy, "LifeFormSci") = 0) And (InStr(dummy, "LifeformName") = 0) And _
       (InStr(dummy, "ChemID") = 0) And (InStr(dummy, "ChemName") = 0) Then
      If vars.item(i).values.count > 0 Then
        Set nodx = TreeView1.Nodes.Add("data", tvwChild, vars.item(i).name, vars.item(i).label, 1, 3)
      ElseIf CBool(Check2.value) Then
        Set nodx = TreeView1.Nodes.Add("data", tvwChild, vars.item(i).name, vars.item(i).label, 2, 3)
      End If
    End If
  Next
  MakeTreeNodes = True
TREEERROR:
End Function

Public Function DelTreeNodes() As Boolean
Dim i As Integer
Dim dummy As String
  
On Error Resume Next
  DelTreeNodes = False
  For i = 1 To vars.count
    dummy = vars.item(i).name
    If (InStr(dummy, "Num") = 0) And (InStr(dummy, "~Table") = 0) And _
       (InStr(dummy, "LifeFormSci") = 0) And (InStr(dummy, "LifeformName") = 0) And _
       (InStr(dummy, "ChemID") = 0) And (InStr(dummy, "ChemName") = 0) Then
      TreeView1.Nodes.Remove vars.item(i).name
    End If
  Next
  ClearVariables
  DelTreeNodes = True
End Function

Public Function GetKey(tempvar As Variable, count As Integer) As String
'uses idx combo boxes to retrieve key
Dim i As Integer
Dim j As Integer
Dim var As Variable
Dim keystring As String
Dim tempstring As String
Dim numflag As Boolean
  
  numflag = True
  tempstring = ""
  If InStr(tempvar.indices.item(tempvar.indices.count), "Num") = 0 Then numflag = False
  For i = 0 To 5
    If Idx(i).Visible Then
      tempstring = tempstring + CStr(Idx(i).ListIndex + 1) + ","
    Else
      If numflag Then Exit For
      tempstring = tempstring + "0,"
    End If
  Next
  count = 0
  If i < 6 Then
    Set var = vars.item(tempvar.indices.item(tempvar.indices.count))
    keystring = tempstring
    For j = i To 5
      keystring = keystring + "0,"
    Next
    On Error Resume Next
    count = Val(var.values.item(keystring))
  End If
  GetKey = tempstring
End Function

Public Function WriteGID(filename As String) As Boolean
Dim i As Integer
Dim j As Integer
Dim vcount As Long
Dim fileout As csv
  
  vcount = 0
  WriteGID = False
  If open_csv(fileout, filename, 1) Then
    WriteGIDRec fileout, "numChems", 0, 0, 0, 0, 0, 0, 0, "", "", CStr(ChemSelect(0).ListCount)
    For i = 1 To ChemSelect(0).ListCount
      WriteGIDRec fileout, "ChemID", i, 0, 0, 0, 0, 0, 0, "", "", ChemSelect(0).List(i - 1)
      WriteGIDRec fileout, "ChemName", i, 0, 0, 0, 0, 0, 0, "", "", ChemSelect(1).List(i - 1)
      WriteGIDRec fileout, "akaChemId", i, 0, 0, 0, 0, 0, 0, "", "", ChemAliasList(0).List(ChemSelect(0).ItemData(i - 1))
      WriteGIDRec fileout, "akaChemName", i, 0, 0, 0, 0, 0, 0, "", "", ChemAliasList(1).List(ChemSelect(0).ItemData(i - 1))
    Next
    WriteGIDRec fileout, "numLifeForm", 0, 0, 0, 0, 0, 0, 0, "", "", CStr(OrgSelect(0).ListCount)
    For i = 1 To OrgSelect(0).ListCount
      WriteGIDRec fileout, "LifeFormSci", i, 0, 0, 0, 0, 0, 0, "", "", OrgSelect(0).List(i - 1)
      WriteGIDRec fileout, "LifeFormName", i, 0, 0, 0, 0, 0, 0, "", "", OrgSelect(1).List(i - 1)
      WriteGIDRec fileout, "akaLifeFormSci", i, 0, 0, 0, 0, 0, 0, "", "", OrgAliasList(0).List(OrgSelect(0).ItemData(i - 1))
      WriteGIDRec fileout, "akaLifeFormName", i, 0, 0, 0, 0, 0, 0, "", "", OrgAliasList(1).List(OrgSelect(0).ItemData(i - 1))
    Next
    For i = 1 To vars.count
      If (vars(i).name <> "NumChems" And vars(i).name <> "NumLifeForm" And _
          vars(i).name <> "ChemId" And vars(i).name <> "ChemName" And _
          vars(i).name <> "akaChemId" And vars(i).name <> "akaChemName" And _
          vars(i).name <> "LifeFormSci" And vars(i).name <> "LifeformName" And _
          vars(i).name <> "akaLifeFormSci" And vars(i).name <> "akaLifeFormName") Then

        For j = 1 To vars(i).values.count
          put_val fileout, vars(i).name
          fileout.putbuff = fileout.putbuff + "," + vars(i).keys.item(j) + ""
          If vars(i).usrUnit.count > 0 Then
            put_val fileout, vars(i).usrUnit.item(j)
          Else
            put_val fileout, ""
          End If
          If vars(i).defUnit.count > 0 Then
            put_val fileout, vars(i).defUnit.item(j)
          Else
            put_val fileout, ""
          End If
          put_val fileout, vars(i).values.item(j)
          put_line fileout
          vcount = vcount + 1
        Next
      End If
    Next
    close_csv fileout
    If vcount > 0 Then WriteGID = True
  End If
End Function

Public Function CheckChemAliases(chemstring As String) As Boolean
'returns true if all chemicals are aliased
'returns false if any chemical is not aliased
'and copies gives the first occurence into chemstring
Dim i As Integer
  
  chemstring = ""
  CheckChemAliases = True
  For i = 1 To ChemSelect(0).ListCount
    If ChemSelect(0).ItemData(i - 1) < 0 Then
      chemstring = ChemSelect(0).List(i - 1)
      CheckChemAliases = False
      Exit Function
    End If
  Next
End Function

Public Function CheckLifeAliases(lifestring As String) As Boolean
'returns true if all organisms are aliased
'returns false if any organism is not aliased
'and copies gives the first occurence into lifestring
Dim i As Integer
  
  lifestring = ""
  CheckLifeAliases = True
  For i = 1 To OrgSelect(0).ListCount
    If OrgSelect(0).ItemData(i - 1) < 0 Or OrgSelect(0).ItemData(i - 1) >= OrgAliasList(0).ListCount Then
      lifestring = OrgSelect(0).List(i - 1)
      CheckLifeAliases = False
      Exit Function
    End If
  Next
End Function

Public Function AddKeyVariableValue(name As String, value As String) As Integer
Dim valIndex As Integer
Dim numKeyName As String
Dim numKey As String
Dim varkey As String
Dim var As Variable
Dim num As Variable
  
  Set var = vars.item(name)
  valIndex = SearchCollection(var.values, value)
  If valIndex = -1 Then
    varkey = CStr(var.values.count + 1) + ",0,0,0,0,0,"
    var.values.Add value, varkey
    var.usrUnit.Add var.unit, varkey
    var.defUnit.Add var.unit, varkey
    var.keys.Add varkey, varkey
    var.filled = True
    AddKeyVariableValue = var.values.count
    numKeyName = SearchforNumKey(var.indices)
    If Not numKeyName = "" Then
      Set num = vars.item(numKeyName)
      numKey = "0,0,0,0,0,0,"
      If num.values.count = 0 Then
        num.values.Add var.values.count, numKey
        num.usrUnit.Add var.unit, numKey
        num.defUnit.Add var.unit, numKey
        num.keys.Add numKey, numKey
      ElseIf num.values.count = 1 Then
        num.values.Remove 1
        num.values.Add var.values.count, numKey
      End If
      num.filled = True
    End If
  Else
    AddKeyVariableValue = valIndex
  End If
End Function

Public Function AddKeyZeros(key As String) As String
Dim i As Integer
Dim cnt As Integer
Dim tchar As String
Dim tstring As String
  
  cnt = 0
  For i = 1 To Len(key)
    tchar = Mid(key, i, 1)
    If tchar = "," Then cnt = cnt + 1
  Next
  tstring = key
  For i = cnt To 5
    tstring = tstring + "0,"
  Next
  AddKeyZeros = tstring
End Function

Public Function AddVariableValue(var As Variable, value As String, varkey As String) As Integer
Dim i As Integer
Dim Index As Integer
Dim key As String

  key = AddKeyZeros(varkey)
  var.values.Add value, key
  var.usrUnit.Add var.unit, key
  var.defUnit.Add var.unit, key
  var.keys.Add key, key
  var.filled = True
  AddVariableValue = var.values.count
End Function

Public Sub SetRecords(chemindex As Integer, lifeindex As Integer, var As Variable, temptoken As Token)
  Dim i As Integer
  Dim j As Integer
  Dim k As Integer
  Dim recCount As Integer
  
  Dim key As String
  Dim keyIndex As Integer
  Dim keyCount As Integer
  Dim KeyName As New Collection
  Dim keyNameIdx(6) As Integer
  Dim numKeyName As String
  Dim numKey As String
  Dim num As Variable
  
  Dim tempRec As New Collection
  Dim count As Integer
  
  'a little upfront work to speed update
  recCount = Val(temptoken.GetNextToken)
  numKeyName = SearchforNumKey(var.indices)
  If Not numKeyName = "" Then Set num = vars.item(numKeyName)
  Set KeyName = temptoken.GetLabels
  
  For j = 1 To var.indices.count
    For k = 1 To KeyName.count
      If var.indices(j) = KeyName(k) Then
        keyNameIdx(k) = j
        Exit For
      End If
    Next
  Next
  
  For i = 1 To recCount
    Set tempRec = temptoken.GetNextRecSet
  
    'add indexing key values to their respective variables
    'and get indexing key indices for values added
    'keyindex is used to construct variable key
    key = CStr(chemindex) + "," + CStr(lifeindex) + ","
    For j = 1 To KeyName.count
      If keyNameIdx(j) <> 0 Then
        keyIndex = AddKeyVariableValue(KeyName(j), tempRec(j))
        key = key + CStr(keyIndex) + ","
      End If
    Next
    
    'update num variable
    If Not numKeyName = "" Then
      numKey = AddKeyZeros(key)
      If SearchForKey(num.values, numKey) = True Then
        count = Val(num.values.item(numKey)) + 1
        num.values.Remove numKey
        num.keys.Remove numKey
      Else
        count = 1
      End If
      num.values.Add count, numKey
      num.keys.Add numKey, numKey
      key = key + CStr(count) + ","
    End If
    
    'add values for variable and attribute variables
    For j = 1 To KeyName.count
      If keyNameIdx(j) = 0 Then keyIndex = AddVariableValue(vars(KeyName(j)), tempRec(j), key)
    Next
  Next
End Sub

Public Sub GenerateCertificateError(filepath As String)
Dim urls() As String
Dim urlcount As Integer
Dim tstring As String
Dim httpindex As Integer
Dim errorstring As String
Dim i As Integer
Dim fnum As Long
  
  On Error Resume Next
  'Grab URL from DES file and display for the user
  fnum = FreeFile
  Open filepath For Input As #fnum
  If Err.Number <> 0 Then
    PutError "Unable to open des file:" + filepath
    Exit Sub
  End If
  urlcount = 0
  Do While Not EOF(fnum)
    Line Input #fnum, tstring
    httpindex = InStr(1, tstring, "http", vbTextCompare)
    If httpindex > 0 Then
      urlcount = urlcount + 1
      ReDim Preserve urls(urlcount) As String
      urls(urlcount - 1) = ReturnURL(httpindex, tstring)
    End If
  Loop
  Close #fnum
  
  errorstring = "Authentication Certificate is missing on client machine." + Chr(13) + Chr(10)
  errorstring = errorstring + Chr(13) + Chr(10) + Chr(13) + Chr(10)
  errorstring = errorstring + "More information may be found at one of the following url addresses." + Chr(13) + Chr(10)
  If urlcount < 2 Then
    errorstring = errorstring + "No additional url addresses are available"
  Else
    For i = 1 To urlcount - 1
      If InStr(1, errorstring, urls(i), vbTextCompare) = 0 Then
        errorstring = errorstring + urls(i) + Chr(13) + Chr(10)
      End If
    Next
  End If
  PutError errorstring
  
End Sub
Public Function GetQueryUrl(url As String, parm As String, Optional key1, Optional key2, Optional val1, Optional val2) As String
Dim query As String
Dim count As String
  
  count = "2"
  If IsMissing(key2) Then count = "1"
  If IsMissing(key1) Then count = "0"
  If count = "0" Then
    query = url + "&parameter=" + "'" + parm + "'" + "&keycount=" + count
  ElseIf count = "1" Then
    query = url + "&parameter=" + "'" + parm + "'" + "&keycount=" + count + "&key1=" + key1 + "&value1=" + val1
  Else
    query = url + "&parameter=" + "'" + parm + "'" + "&keycount=" + count + "&key1=" + key1 + "&key2=" + key2 + "&value1=" + val1 + "&value2=" + val2
  End If
  GetQueryUrl = Replace(query, " ", "+")
End Function

Private Sub GetVar(var As Variable)
Dim i As Integer
Dim j As Integer
Dim url As String
Dim urlQuery As String
Dim result As String
Dim temptoken As New Token
  
  If (var.filled) Then Exit Sub
  url = parms(0) + parms(1) + "&" + parms(2) + "&" + parms(3)
  If var.indices.item(1) = "ChemID" Then
    For i = 1 To ChemSelect(0).ListCount
      AddKeyVariableValue "ChemId", ChemSelect(0).List(i - 1)
      AddKeyVariableValue "ChemName", ChemSelect(1).List(i - 1)
      AddKeyVariableValue "akaChemId", ChemAliasList(0).List(ChemSelect(0).ItemData(i - 1))
      AddKeyVariableValue "akaChemName", ChemAliasList(1).List(ChemSelect(0).ItemData(i - 1))
      For j = 1 To OrgSelect(0).ListCount
        If i = 1 Then
          AddKeyVariableValue "LifeformSci", OrgSelect(0).List(j - 1)
          AddKeyVariableValue "LifeformName", OrgSelect(1).List(j - 1)
          AddKeyVariableValue "akaLifeformSci", OrgAliasList(0).List(OrgSelect(0).ItemData(j - 1))
          AddKeyVariableValue "akaLifeformName", OrgAliasList(1).List(OrgSelect(0).ItemData(j - 1))
        End If
'        check = CheckVarName(var.name)
        urlQuery = GetQueryUrl(url, var.name, "ChemID", "LifeFormSci", ChemAliasList(0).List(ChemSelect(0).ItemData(i - 1)), OrgAliasList(0).List(OrgSelect(0).ItemData(j - 1)))
        temptoken.SetString CheckURL(urlQuery)
        SetRecords i, j, var, temptoken
      Next
    Next
  End If
  For i = 1 To var.siblings.count
    vars.item(var.siblings.item(i)).filled = True
  Next
  var.filled = True
End Sub

Private Sub Inet1_StateChanged(ByVal State As Integer)
Dim msgstring As String
  
  Select Case State
'    Case icNone
'    Case icResolvingHost
'    Case icHostResolved
'    Case icConnecting
'    Case icConnected
'    Case icRequesting
'    Case icRequestSent
'    Case icReceivingResponse
'    Case icResponseReceived
'    Case icDisconnecting
'    Case icDisconnected
'    Case icResponseCompleted
    Case icError
        msgstring = Inet1.ResponseInfo
        If msgstring = "Client authorization not setup" Then
          GenerateCertificateError (Mid(AppPath, 1, 2) + DesName)
        Else
          PutError Inet1.ResponseInfo
        End If
        EndModule
  End Select
End Sub
  
Function CheckURL(url As String) As String
Dim result As String
Dim fnum As Long
  
  CheckURL = ""
  'check usrName only once
  If UsrName = "" Then
    Login.Show vbModal, Me
    If Not LoginSucceeded Then Exit Function
  End If

  Inet1.Cancel
  Inet1.url = url
  Inet1.UserName = UsrName
  Inet1.Password = pssword
  result = Inet1.OpenURL
  Inet1.AccessType = 0
  
  #If QFILE Then
    fnum = FreeFile
    Open RunName + ".qry" For Append As #fnum
    Print #fnum, url
    Print #fnum, result
    Close #fnum
  #End If
  
  If InStr(1, result, "<!DOCTYPE HTML PUBLIC") <> 0 Then
    MsgBox "Unable to determine error for URL." & vbCrLf & "Check login and password!"
  Else
    CheckURL = result
  End If
End Function

Public Function LoadOrganismData(url As String, FUNCTION_TYPE As String, DATA_DIC As String, DATA_DB As String)
'LoadOrganismData
'load data into List and Combo arrays
' LifeFormSci --> into OrgList(0) OrgAliasList(0)
' LifeFormNames --> into OrgList(1) and OrgAliasList(1)
'-Parameters-----------------------
'URL            'URL to DET extraction asp
'FUNCTION_TYPE  'recognized function name for extraction asp
'DATA_DIC       'data ditionary name
'Data_DB        'database/dictionary mapping to use
Dim i As Integer
Dim IdIdx As Integer
Dim NameIdx As Integer
Dim count As Integer
Dim reccoll As New Collection
Dim tempstring As String
Dim temptoken As New Token
  
  'get data from DET
  tempstring = CheckURL(url + FUNCTION_TYPE + "&" + DATA_DIC + "&" + DATA_DB + "&parameter='LifeFormSci'&keycount=0")
  temptoken.SetString tempstring
  count = Val(temptoken.GetNextToken())
  If count = 0 Then
    PutError "Unable to Retrieve Organism Data"
    EndModule
  End If
    
  Set reccoll = temptoken.GetLabels()
  For i = 1 To reccoll.count
    Select Case reccoll.item(i)
      Case "LifeFormSci":    IdIdx = i
      Case "LifeFormName":   NameIdx = i
    End Select
  Next
  
  OrgList(0).Clear
  OrgList(1).Clear
  OrgAliasList(0).Clear
  OrgAliasList(1).Clear
  For i = 1 To count
    Set reccoll = temptoken.GetNextRecSet()
    OrgList(0).AddItem reccoll.item(IdIdx)
    OrgList(1).AddItem reccoll.item(NameIdx)
    OrgAliasList(0).AddItem reccoll.item(IdIdx)
    OrgAliasList(1).AddItem reccoll.item(NameIdx)
  Next
End Function

Public Sub LoadChemicalData(url As String, FUNCTION_TYPE As String, DATA_DIC As String, DATA_DB As String)
'-LoadChemicalData-----------------
'load data into Combo array
' ChemIDs --> into ChemAliasList(0)
' ChemNames --> into ChemAliasList(1)
'-Parameters-----------------------
'URL            'URL to DET extraction asp
'FUNCTION_TYPE  'recognized function name for extraction asp
'DATA_DIC       'data ditionary name
'Data_DB        'database/dictionary mapping to use
Dim i As Integer
Dim IdIdx As Integer
Dim NameIdx As Integer
Dim count As Integer
Dim reccoll As New Collection
Dim tempstring As String
Dim temptoken As New Token
  
  'get data from DET
  tempstring = CheckURL(url + FUNCTION_TYPE + "&" + DATA_DIC + "&" + DATA_DB + "&parameter='ChemID'&keycount=0")
  temptoken.SetString tempstring
  count = Val(temptoken.GetNextToken())
  If count = 0 Then
    PutError "Unable to Retrieve Constituent Data"
    EndModule
  End If
  
  Set reccoll = temptoken.GetLabels()
  For i = 1 To reccoll.count
    Select Case reccoll.item(i)
      Case "ChemID":     IdIdx = i
      Case "ChemName":   NameIdx = i
    End Select
  Next
  
  ChemAliasList(0).Clear
  ChemAliasList(1).Clear
  For i = 1 To count
    Set reccoll = temptoken.GetNextRecSet()
    ChemAliasList(0).AddItem reccoll.item(IdIdx)
    ChemAliasList(1).AddItem reccoll.item(NameIdx)
  Next
End Sub

Private Function UnLoadValues(name As String, outfile As csv, box As ComboBox) As Boolean
'load a collection of values into the GLOBAL 'Vars' collection given a name and a file
Dim i As Integer
  
On Error GoTo WRITEERROR
  UnLoadValues = False
  put_val outfile, name
  put_val outfile, box.ListCount
  put_line outfile
  For i = 1 To box.ListCount
    put_val outfile, box.List(i - 1)
    put_line outfile
  Next
  UnLoadValues = True
WRITEERROR:
End Function

Private Function WritePersistentData(filename As String) As Boolean
'Write the persistent data for organisms and consituents
'Persist only those ids/names acually in the data source i.e. *aliaslist
Dim fileout As csv
  
  WritePersistentData = False
  If open_csv(fileout, filename, F_WRITE, False) Then
    If UnLoadValues("OrganismID", fileout, OrgAliasList(0)) Then
      If UnLoadValues("OrganismName", fileout, OrgAliasList(1)) Then
        If UnLoadValues("ChemID", fileout, ChemAliasList(0)) Then
          If UnLoadValues("ChemName", fileout, ChemAliasList(1)) Then WritePersistentData = True
        End If
      End If
    End If
    close_csv fileout
  End If
End Function

Private Function LoadValues(name As String, infile As csv, box As ComboBox) As Boolean
'load a collection of values into the GLOBAL 'Vars' collection given a name and a file
Dim i As Integer
Dim varname As String
Dim count As Long
  
On Error GoTo READERROR
  LoadValues = False
  varname = get_val(infile)
  count = Val(get_val(infile))
  get_line infile
  If count = 0 Or name <> varname Then Exit Function
  For i = 1 To count
    box.AddItem get_val(infile)
    get_line infile
  Next
  LoadValues = True
READERROR:
End Function

Private Function ReadPersistentData(filename As String) As Boolean
'Read the persistent data for organisms and consituents
Dim i As Integer
Dim infile As csv
  
  ReadPersistentData = False
  If open_csv(infile, filename, F_READ, False) Then
    If LoadValues("OrganismID", infile, OrgAliasList(0)) Then
      For i = 0 To OrgAliasList(0).ListCount - 1
        OrgList(0).AddItem OrgAliasList(0).List(i)
      Next
      If LoadValues("OrganismName", infile, OrgAliasList(1)) Then
        For i = 0 To OrgAliasList(1).ListCount - 1
          OrgList(1).AddItem OrgAliasList(1).List(i)
        Next
        If LoadValues("ChemID", infile, ChemAliasList(0)) Then
          If LoadValues("ChemName", infile, ChemAliasList(1)) Then ReadPersistentData = True
        End If
      End If
    End If
    close_csv infile
  End If
End Function

Private Function ParseCommandline() As Boolean
Dim i As Integer
Dim gotlogin As Integer
Dim values As Variant
  
  called = False
  ParseCommandline = False
  For i = 0 To 8
    parms(i) = ""
  Next
  
  UsrName = ""
  pssword = ""
  gotlogin = 0
  values = Split(Trim(Replace(Command(), "  ", " ")))
 
  If UBound(values) < 7 Or UBound(values) > 10 Then
    MsgBox "Invalid arguments passed to module" & Chr(10) & Command & Chr(10) & "Contact PNNL", 16, "Usage error!"
    End
  End If
  
  If UBound(values) = 9 Then
    UsrName = values(0)
    pssword = values(1)
    gotlogin = 2
  End If
  
  dicPath = AppPath & values(gotlogin + 1)
  parms(0) = values(gotlogin)                'URL to DET extraction asp
  parms(1) = "?step=extract"                 'recognized function name for extraction asp
  parms(2) = "DD=" & values(gotlogin + 1)    'data ditionary name
  parms(3) = "DB=" & values(gotlogin + 2)    'database/dictionary mapping to use
  parms(4) = values(gotlogin + 3)            'GID file name no extension
  parms(5) = values(gotlogin + 4)            'output file name no extension
  parms(6) = values(gotlogin + 5)            'site index
  parms(7) = values(gotlogin + 6)            'module prefix index
  parms(8) = values(gotlogin + 7)            'module id
  
  FUIName = parms(4) & ".gid"
  RunName = parms(5)
  siteIdx = Val(parms(6))
  modIdx = Val(parms(7))
  ModName = parms(8)
  
  If siteIdx + modIdx < 2 Then
    MsgBox "Invalid arguments passed to module" & Chr(10) & Command & Chr(10) & "Contact PNNL", 16, "Usage error!"
    End
  End If
  If Not open_csv(errfile, RunName & ".ERR", 1) Then
    MsgBox "Unable to create file " & RunName & ".ERR" & Chr(10) & "Check directory permissions", 16, "File IO error!"
    End
  End If
  AnError = False
  Set frm = Me
  Set frmRef = Nothing
  SetHelpFile AppPath + "EBF DCE.html"
End Function

Private Sub InitStuff()
Dim i As Integer
Dim j As Integer
Dim k As Integer
Dim id As String
Dim aka As String
Dim name As String
Dim key As String
Dim msg As String
Dim vid As Variable
Dim vname As Variable
Dim valias As Variable

  Label2.Caption = "Aliased CAS ID for selected constituent"
  Label8.Caption = "All data associated with mapped Constituent(s) and Organism(s) combinations are retrieved." + Chr(13) + Chr(10) + Chr(13) + Chr(10)
  Label8.Caption = Label8.Caption + "Data are retrieved for viewing or editing only." + Chr(13) + Chr(10) + Chr(13) + Chr(10)
  Label8.Caption = Label8.Caption + "Reduction of the data set actually occurs in the models that are "
  Label8.Caption = Label8.Caption + "connected to the databases and not at this time"
  
  'load dictionary data
  If Not ReadDictionary(dicPath) Then
    PutError "Unable to open dictionary file: " + dicPath
    EndModule
  End If
  
  'load aliasing data
  If Not ReadPersistentData(iniPath) Then
    LoadChemicalData parms(0), parms(1), parms(2), parms(3)
    LoadOrganismData parms(0), parms(1), parms(2), parms(3)
    WritePersistentData iniPath
  End If
  
  'load chemical data
  If Not ReadGidChemicals(FUIName, ChemSelect(0), ChemSelect(1)) Then
    PutError "Unable to read constituent list from GID file: " + FUIName
    EndModule
  End If
  
  'load user interface data
  If Not ReadGID(FUIName, ModName) Then
    PutError "Unable to read user interface values from GID file: " + FUIName
    EndModule
  End If
  
  On Error GoTo InitError
  'connect chemical alias data to screen
  Set valias = vars.item("akaChemId")
  For i = 1 To ChemSelect(0).ListCount
    key = CStr(i) & ",0,0,0,0,0,"
    If SearchForKey(valias.values, key) Then
      ChemSelect(0).ItemData(i - 1) = SearchComboBox(valias.values.item(key), ChemAliasList(0))
    Else
      ChemSelect(0).ItemData(i - 1) = SearchNoDashBox(ChemSelect(0).List(i - 1), ChemAliasList(0))
    End If
    ChemSelect(1).ItemData(i - 1) = ChemSelect(0).ItemData(i - 1)
  Next
  
  'connect organism data to screen
  msg = ""
  Set vid = vars.item("LifeFormSci")
  Set vname = vars.item("LifeFormName")
  Set valias = vars.item("akaLifeFormSci")
  For i = 1 To vid.values.count
    key = CStr(i) & ",0,0,0,0,0,"
    id = vid.values.item(key)
    name = vname.values.item(key)
    j = SearchListBox(id, OrgList(0))
    If j = -1 Then
      'add the user specified item
      OrgList(0).AddItem id
      OrgList(1).AddItem name
      j = OrgList(0).NewIndex
    End If
    listclick = True
    OrgList(0).Selected(j) = True
    OrgList(1).Selected(j) = True
    listclick = False
    aka = valias.values.item(key)
    k = SearchComboBox(aka, OrgAliasList(0))
    If k = -1 Then
      msg = msg & vbCrLf & name & " alais " & aka
    End If
    OrgSelect(0).AddItem id
    OrgSelect(0).ItemData(OrgSelect(0).NewIndex) = k
    OrgSelect(1).AddItem name
    OrgSelect(1).ItemData(OrgSelect(1).NewIndex) = k
  Next
  If msg <> "" Then MsgBox "Alias not found:" & msg & vbCrLf & "Must select new alias"
  MakeTreeNodes
  Unload Loading
  formload = False
  Set TreeView1.SelectedItem = TreeView1.Nodes.item(1)
  EcoEdit.Show
  Exit Sub
  
InitError:
  MsgBox "Failed to initialize!"
  EndModule
End Sub

Private Sub Timer1_Timer()
  Timer1.Enabled = False
  Select Case Timer1.Tag
  Case "load1"
    InitStuff
    Timer1.Tag = ""
  Case Else
    If ProgressBar1.value = 150 Then
      ProgressBar1.value = 0
    Else
      ProgressBar1.value = ProgressBar1.value + 10
    End If
    Timer1.Enabled = True
  End Select
End Sub

Private Sub Form_load()
  On Error Resume Next
  
'  AppPath = "c:\frames\"
  AppPath = App.Path + "\"
  
  formload = True
  ParseCommandline
  UsrModName = ReadModLabel(FUIName, siteIdx, modIdx, ModName)
  iniPath = Replace(DesName, "des", "ini")
  If Dir(RunName + ".qry") <> "" Then Kill RunName + ".qry"
  Timer1.Tag = "load1"
  Timer1.Interval = 500
  Timer1.Enabled = True
End Sub

Private Sub Form_Unload(Cancel As Integer)
Dim answer As Long
  Inet1.Protocol = icUnknown
  If Not called Then
    answer = MsgBox("Do you want to save changes?", 51, App.Title)
    If answer = 6 Then save_Click
    If answer = 7 Then EndModule
    If answer = 2 Then Cancel = 1
  End If
End Sub

Private Sub save_Click()
Dim gidcheck As Boolean
Dim chemstring As String
Dim lifestring As String
Dim errormsg As String
  
  If Not CheckChemAliases(chemstring) Then
    errormsg = "Selected Constituent " + chemstring + " is not aliased." + Chr(13) + Chr(10)
    errormsg = errormsg + "All selected constituents must be aliased before saving!" + Chr(13) + Chr(10)
  End If
  
  If OrgList(0).SelCount = 0 Then
    errormsg = "An organism(s) must be selected before saving!" + Chr(13) + Chr(10)
  End If
  
  If Not CheckLifeAliases(lifestring) Then
    errormsg = "Selected Organism " + lifestring + " is not aliased." + Chr(13) + Chr(10)
    errormsg = errormsg + "All selected organisms must be aliased before saving!" + Chr(13) + Chr(10)
  End If
    
  If errormsg <> "" Then
    MsgBox errormsg, vbExclamation + vbOKOnly, "Save Error"
    Exit Sub
  End If
  
  If Not WriteGID(RunName + ".gid") Then
    MsgBox "New values need to be retrieved!" & vbCrLf & "Please click 'Viewing and Editing'," & vbCrLf & "then click 'Retrieve Data'", vbExclamation + vbOKOnly, "Save Error"
    Exit Sub
  End If
  
  gidcheck = WritePersistentData(iniPath)
  EndModule
End Sub

Private Sub leave_Click()
  Form_Unload 0
End Sub

Private Sub howto_Click()
  GetHelp
End Sub

Private Sub about_Click()
  frmAbout.picIcon = frmAbout.ImageList1.ListImages(1).Picture
  frmAbout.Show 1, Me
End Sub

Private Sub Command1_Click()
Dim i As Integer
Dim gidcheck As Boolean
Dim tempvar As Variable
Dim var As Variable
Dim showmsg As Boolean

  showmsg = True
  Timer1.Enabled = True
  Label7.Visible = True
  ProgressBar1.Visible = True
  
  ClearVariables
  For i = 1 To vars.count
    Set var = vars.item(i)
    Label7.Caption = "Attemping to retrieve " + var.name + " data"
    If (InStr(var.name, "num") = 0) And (InStr(var.name, "aka") = 0) And _
    (Not var.name = "LifeFormSci") And (Not var.name = "LifeFormName") And _
    (Not var.name = "ChemID") And (Not var.name = "ChemName") And _
    (InStr(var.name, "~Table") = 0) And (var.indices.count > 1) Then 'notkey
      GetVar var
      If var.values.count > 0 Then
        showmsg = False
      End If
    End If
  Next
  MakeTreeNodes
  
  If showmsg Then MsgBox "No data available for selections." & vbCrLf & "Try aliasing chemicals or organisms.", vbCritical, "Warning"
  Label7.Visible = False
  Timer1.Enabled = False
  ProgressBar1.Visible = False
End Sub

Private Sub Command2_Click()
  AddKey.SetAddKeyLabels "New Organism Key", "Scientific Name", "Common Name"
  AddKey.Show 1, EcoEdit
  If AddKeySucceeded Then
    OrgList(0).AddItem KeyId
    OrgList(1).AddItem KeyName
    OrgList(0).Selected(OrgList(0).NewIndex) = True
    WritePersistentData iniPath
  End If
End Sub

Private Sub Check1_Click(Index As Integer)
  Select Case Index
    Case 1
      If Check1(Index).value = checked Then
        ChemSelect(0).Visible = False
        ChemSelect(1).Visible = True
        ChemAliasList(0).Visible = False
        ChemAliasList(1).Visible = True
        Label1.Caption = "Constituent Common Names"
        Label2.Caption = "Aliased common name for selected constituent"
      Else
        ChemSelect(0).Visible = True
        ChemSelect(1).Visible = False
        ChemAliasList(0).Visible = True
        ChemAliasList(1).Visible = False
        Label1.Caption = "Constituent CAS Id"
        Label2.Caption = "Aliased CAS Id for selected constituent"
      End If
    Case 2
      If Check1(Index).value = checked Then
        OrgList(0).Visible = False
        OrgList(1).Visible = True
        Frame4.Caption = "Organisms of Concern for this Assessment (Common Names)"
      Else
        OrgList(0).Visible = True
        OrgList(1).Visible = False
        Frame4.Caption = "Organisms of Concern for this Assessment (Scientific Names)"
      End If
    Case 3
      If Check1(Index).value = checked Then
        OrgSelect(0).Visible = False
        OrgSelect(1).Visible = True
        OrgAliasList(0).Visible = False
        OrgAliasList(1).Visible = True
        Label5.Caption = "Organism Common Names"
        Label6.Caption = "Aliased common name for selected organism"
      Else
        OrgSelect(0).Visible = True
        OrgSelect(1).Visible = False
        OrgAliasList(0).Visible = True
        OrgAliasList(1).Visible = False
        Label5.Caption = "Organism Scientific Names"
        Label6.Caption = "Aliased scientific name for selected organism"
      End If
  End Select
End Sub

Private Sub Check2_Click()
  MakeTreeNodes
  TreeView1.SelectedItem = TreeView1.Nodes("data")
  TreeView1_Click
End Sub

Private Sub ChemSelect_Click(Index As Integer)
  If listclick Then Exit Sub
  listclick = True
  If ChemSelect(Index).ListIndex > -1 Then
    If Index = 0 Then
      ChemSelect(1).ListIndex = ChemSelect(Index).ListIndex
    Else
      ChemSelect(0).ListIndex = ChemSelect(Index).ListIndex
    End If
    comboclick = True
    ChemAliasList(0).ListIndex = ChemSelect(Index).ItemData(ChemSelect(Index).ListIndex)
    ChemAliasList(1).ListIndex = ChemSelect(Index).ItemData(ChemSelect(Index).ListIndex)
    comboclick = False
    ChemAliasList(0).Enabled = True
    ChemAliasList(1).Enabled = True
  End If
  listclick = False
End Sub

Private Sub OrgSelect_Click(Index As Integer)
  If listclick Then Exit Sub
  listclick = True
  If OrgSelect(Index).ListIndex > -1 Then
    If Index = 0 Then
      OrgSelect(1).ListIndex = OrgSelect(Index).ListIndex
    Else
      OrgSelect(0).ListIndex = OrgSelect(Index).ListIndex
    End If
    comboclick = True
    If OrgSelect(Index).ItemData(OrgSelect(Index).ListIndex) > OrgAliasList(0).ListCount - 1 Then
      OrgAliasList(0).ListIndex = -1
      OrgAliasList(1).ListIndex = -1
    Else
      OrgAliasList(0).ListIndex = OrgSelect(Index).ItemData(OrgSelect(Index).ListIndex)
      OrgAliasList(1).ListIndex = OrgSelect(Index).ItemData(OrgSelect(Index).ListIndex)
    End If
    comboclick = False
    OrgAliasList(0).Enabled = True
    OrgAliasList(1).Enabled = True
  End If
  listclick = False
End Sub

Private Sub ChemAliasList_Click(Index As Integer)
  If listclick And comboclick Then Exit Sub
  If comboclick Then Exit Sub
  comboclick = True
  If ChemSelect(Index).SelCount = 0 Then Exit Sub
  If Index = 0 Then
    ChemAliasList(1).ListIndex = ChemAliasList(Index).ListIndex
  Else
    ChemAliasList(0).ListIndex = ChemAliasList(Index).ListIndex
  End If
  ChemSelect(0).ItemData(ChemSelect(Index).ListIndex) = ChemAliasList(Index).ListIndex
  ChemSelect(1).ItemData(ChemSelect(Index).ListIndex) = ChemAliasList(Index).ListIndex
  comboclick = False
  DelTreeNodes
End Sub

Private Sub OrgAliasList_Click(Index As Integer)
  If listclick And comboclick Then Exit Sub
  If comboclick Then Exit Sub
  comboclick = True
  If OrgSelect(Index).SelCount = 0 Then Exit Sub
  If Index = 0 Then
    OrgAliasList(1).ListIndex = OrgAliasList(Index).ListIndex
  Else
    OrgAliasList(0).ListIndex = OrgAliasList(Index).ListIndex
  End If
  OrgSelect(0).ItemData(OrgSelect(Index).ListIndex) = OrgAliasList(Index).ListIndex
  OrgSelect(1).ItemData(OrgSelect(Index).ListIndex) = OrgAliasList(Index).ListIndex
  comboclick = False
  DelTreeNodes
End Sub

Private Sub OrgList_Click(Index As Integer)
Dim i As Integer
Dim j As Integer
Dim Idx As Integer
Dim deltree As Boolean

  If listclick Then Exit Sub
  deltree = False
  listclick = True
  
  'sync lists
  Idx = 0
  If Index = 0 Then Idx = 1
  For i = 1 To OrgList(Index).ListCount
    If OrgList(Idx).Selected(i - 1) <> OrgList(Index).Selected(i - 1) Then deltree = True
    OrgList(Idx).Selected(i - 1) = OrgList(Index).Selected(i - 1)
  Next
  
  'Check if there are any selections
  If OrgList(Index).SelCount = 0 Then
    OrgSelect(0).Clear
    OrgSelect(1).Clear
  Else
    'Update OrgSelect with new OrgList selections
    OrgSelect(0).Clear
    For i = 1 To OrgList(0).ListCount
      If OrgList(0).Selected(i - 1) Then
        OrgSelect(0).AddItem OrgList(0).List(i - 1)
        OrgSelect(0).ItemData(OrgSelect(0).NewIndex) = i - 1
      End If
    Next
    'Retain user aliases
    For i = 1 To OrgSelect(0).ListCount
      For j = 1 To OrgSelect(1).ListCount
        If OrgSelect(0).List(i - 1) = OrgSelect(1).List(j - 1) Then
          OrgSelect(0).ItemData(i - 1) = OrgSelect(1).ItemData(j - 1)
          Exit For
        End If
      Next
    Next
    'Sync OrgSelect names (Index  = 1) with OrgSelect ids (Index = 0)
    OrgSelect(1).Clear
    For i = 1 To OrgList(1).ListCount
      If OrgList(0).Selected(i - 1) Then
        OrgSelect(1).AddItem OrgList(1).List(i - 1)
        OrgSelect(1).ItemData(OrgSelect(1).NewIndex) = OrgSelect(0).ItemData(OrgSelect(1).NewIndex)
      End If
    Next
  End If
  listclick = False
  If deltree Then DelTreeNodes
End Sub

Private Sub Idx_Click(Index As Integer)
Dim boxcount As Integer
Dim numindex As Integer
Dim j As Integer
Dim i As Integer
Dim align As Integer
Dim keystring As String
Dim tempstring As String
Dim var As Variable

  If idxclick Then Exit Sub
  For i = 0 To 5
    Label4(i).Caption = 0
  Next
  vaSpread1.Row = -1
  vaSpread1.col = -1
  vaSpread1.Action = 12
  vaSpread1.MaxRows = 1
  On Error Resume Next
  Set var = vars.item(TreeView1.SelectedItem.key)
  If Err.Number <> 0 Then Exit Sub
  If var.datatype = "STRING" Then
    align = 0
  Else
    align = 1
  End If
  keystring = GetKey(var, numindex)
  If numindex = 0 Then
    boxcount = var.indices.count
    For i = boxcount To 6
      keystring = keystring + "0,"
    Next
    vaSpread1.Row = 1
    vaSpread1.col = 1
    vaSpread1.MaxRows = 1
    vaSpread1.TypeHAlign = align
    vaSpread1.Text = var.values.item(keystring)
    Label9.Caption = var.unit
  Else
    boxcount = var.indices.count + 1
    vaSpread1.MaxRows = numindex
    For j = 1 To numindex
      tempstring = keystring + CStr(j) + ","
      For i = boxcount To 6
        tempstring = tempstring + "0,"
      Next
      vaSpread1.Row = j
      vaSpread1.col = 1
      vaSpread1.TypeHAlign = align
      vaSpread1.Text = var.values.item(tempstring)
      Label9.Caption = var.unit
    Next
    Label4(boxcount - 2).Caption = numindex
  End If
  If Label9.Caption = "" Then Label9.Caption = "N/A"
  vaSpread1.Visible = CBool(numindex)
End Sub

Private Sub TreeView1_Click()
Dim i As Integer
Dim j As Integer
Dim k As Integer
Dim treeKey As String
Dim tempstring As String
Dim keyCount As Integer
Dim KeyName As String
Dim keyValue As String
Dim key As Variable
Dim var As Variable

  If formload Then Exit Sub
  
  treeKey = TreeView1.SelectedItem.key
  Frame1.Visible = False
  Frame3.Visible = False
  Frame4.Visible = False
  Frame5.Visible = False
  Frame6.Visible = False
  Frame7.Visible = False
  Select Case treeKey
    Case "R"
      Frame7.Visible = True
      HelpAnchor = Frame7.Tag
    Case "chemaka"
      ChemSelect(0).ListIndex = -1
      ChemSelect(1).ListIndex = -1
      ChemAliasList(0).Enabled = False
      ChemAliasList(0).ListIndex = -1
      ChemAliasList(1).Enabled = False
      ChemAliasList(1).ListIndex = -1
      Frame3.Visible = True
      HelpAnchor = Frame3.Tag
      tempstring = "Mapping " + UsrModName + " Database Names"
      Frame3.Caption = tempstring
    Case "lifesel"
      Frame4.Visible = True
      HelpAnchor = Frame4.Tag
    Case "lifeaka"
      OrgSelect(0).ListIndex = -1
      OrgSelect(1).ListIndex = -1
      OrgAliasList(0).Enabled = False
      OrgAliasList(0).ListIndex = -1
      OrgAliasList(1).Enabled = False
      OrgAliasList(1).ListIndex = -1
      Frame6.Visible = True
      HelpAnchor = Frame6.Tag
    Case "data"
      Label7.Visible = False
      Command1.Enabled = True
      If CheckChemAliases(tempstring) = False Then
        Label7.Caption = "Selected constituent " + tempstring + " is not aliased!" + Chr(13) + Chr(10)
        Label7.Visible = True
        Command1.Enabled = False
      End If
      If OrgList(0).SelCount = 0 Then
        Label7.Caption = "No Organisms Selected!"
        Label7.Visible = True
        Command1.Enabled = False
      End If
      If CheckLifeAliases(tempstring) = False Then
        Label7.Caption = "Selected organism " + tempstring + " is not aliased!" + Chr(13) + Chr(10)
        Label7.Visible = True
        Command1.Enabled = False
      End If
      Frame1.Visible = True
      HelpAnchor = Frame1.Tag
    Case Else 'variable
      'clear display
      For i = 0 To 5
        Idx(i).Clear
        Idx(i).Visible = False
        Label3(i).Visible = False
        Label4(i).Visible = False
      Next
      vaSpread1.col = -1
      vaSpread1.Row = -1
      vaSpread1.Action = 12
      
      'get variable and fill index boxes
      Set var = vars.item(treeKey)
      Frame5.Caption = var.label
      For i = 0 To var.indices.count - 1
        KeyName = var.indices(i + 1)
        Set key = vars(KeyName)
        Label3(i).Caption = key.label
        Label3(i).Visible = True
        If InStr(KeyName, "Num") = 0 Then
          Idx(i).Visible = True
          Label4(i).Visible = False
          For k = 1 To key.values.count
            If KeyName = "ChemID" Then
              keyValue = vars.item("ChemName").values.item(k) + " (" + key.values(k) + ")"
            ElseIf KeyName = "LifeFormSci" Then
              keyValue = vars.item("LifeformName").values.item(k) + " (" + key.values(k) + ")"
            Else
              keyValue = key.values(k)
            End If
            Idx(i).AddItem keyValue
          Next
        Else 'a num[*] variable key (self indexed)
          Idx(i).Visible = False
          Label4(i).Visible = True
          Label4(i).Caption = ""
        End If
        idxclick = True
        If Idx(i).ListCount > 0 Then Idx(i).ListIndex = 0
        idxclick = False
      Next
      Frame5.Visible = True
      HelpAnchor = Frame5.Tag
      Idx_Click (1)
  End Select
End Sub

Private Sub Command2_GotFocus()
  HelpAnchor = Command2.Tag
End Sub

Private Sub Check1_GotFocus(Index As Integer)
  HelpAnchor = Check1(Index).Tag
End Sub

Private Sub ChemSelect_GotFocus(Index As Integer)
  HelpAnchor = ChemSelect(Index).Tag
End Sub

Private Sub OrgList_GotFocus(Index As Integer)
  HelpAnchor = OrgList(Index).Tag
End Sub

Private Sub Idx_GotFocus(Index As Integer)
  HelpAnchor = Idx(Index).Tag
End Sub

Private Sub vaSpread1_GotFocus()
  treeviewkey = TreeView1.SelectedItem.key
  HelpAnchor = vaSpread1.Tag
End Sub

Private Sub TreeView1_KeyUp(KeyCode As Integer, Shift As Integer)
  If formload Then Exit Sub
  TreeView1_Click
End Sub

Private Sub vaSpread1_KeyUp(KeyCode As Integer, Shift As Integer)
Dim i As Integer
Dim indx As Integer
Dim count As Integer
Dim key As String
Dim combokey As String
Dim var As New Variable
  
  Set var = vars.item(treeviewkey)
  combokey = ""
  For i = 0 To 5
    If Idx(i).Visible Then
      combokey = combokey + CStr(Idx(i).ListIndex + 1) + ","
    Else
      If Label4(i).Visible Then
        count = Val(Label4(i).Caption)
      Else
        count = 1
      End If
      Exit For
    End If
  Next
  For i = 1 To count
    If NumBoxPresent() Then
      key = combokey + CStr(i) + ","
    Else
      key = combokey
    End If
    key = AddKeyZeros(key)
    vaSpread1.col = 1
    vaSpread1.Row = i
    
    'grab indx in collection
    indx = SearchCollection(var.keys, key)
    'remove the old value
    var.values.Remove key
    'add the new value
    If indx > var.keys.count Then
      'place value at the end of the collection
      var.values.Add vaSpread1.Text, key
    Else
      'place value at the same indx
      var.values.Add vaSpread1.Text, key, indx
    End If
  Next
End Sub

Private Sub Form_KeyPress(KeyAscii As Integer)
  If formload Then Exit Sub
  If KeyAscii = 13 Then KeyAscii = 0
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
  If formload Then Exit Sub
  Select Case KeyCode
  Case vbKeyF1:
    KeyCode = 0
    GetHelp
  Case vbKeyUp:
    KeyCode = 0
    SendKeys "+{TAB}"
  Case vbKeyDown:
    KeyCode = 0
    SendKeys "{TAB}"
  Case vbKeyReturn:
    KeyCode = 0
    SendKeys "{TAB}"
  End Select
End Sub
