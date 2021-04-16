VERSION 5.00
Object = "{3B7C8863-D78F-101B-B9B5-04021C009402}#1.2#0"; "RICHTX32.OCX"
Object = "{831FDD16-0C5C-11D2-A9FC-0000F8754DA1}#2.0#0"; "MSCOMCTL.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form frmView 
   Caption         =   "Viewing file "
   ClientHeight    =   7200
   ClientLeft      =   1140
   ClientTop       =   1515
   ClientWidth     =   9600
   Icon            =   "frmView.frx":0000
   LinkTopic       =   "Form1"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   7200
   ScaleWidth      =   9600
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton Command2 
      Caption         =   "Print"
      Height          =   375
      Left            =   6720
      TabIndex        =   6
      Top             =   240
      Width           =   1215
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Done"
      Height          =   375
      Left            =   8160
      TabIndex        =   5
      Top             =   240
      Width           =   1215
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   6120
      Top             =   240
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin MSComctlLib.TabStrip TabStrip1 
      Height          =   375
      Left            =   0
      TabIndex        =   3
      Top             =   720
      Width           =   9615
      _ExtentX        =   16960
      _ExtentY        =   661
      _Version        =   393216
      BeginProperty Tabs {1EFB6598-857C-11D1-B16A-00C0F0283628} 
         NumTabs         =   2
         BeginProperty Tab1 {1EFB659A-857C-11D1-B16A-00C0F0283628} 
            Caption         =   "File Contents"
            ImageVarType    =   2
         EndProperty
         BeginProperty Tab2 {1EFB659A-857C-11D1-B16A-00C0F0283628} 
            Caption         =   "Module Description"
            ImageVarType    =   2
         EndProperty
      EndProperty
   End
   Begin VB.ComboBox Combo1 
      Height          =   315
      Left            =   720
      Style           =   2  'Dropdown List
      TabIndex        =   1
      Top             =   240
      Width           =   5655
   End
   Begin RichTextLib.RichTextBox RichTextBox1 
      Height          =   6015
      Left            =   0
      TabIndex        =   0
      Top             =   1080
      Width           =   9255
      _ExtentX        =   16325
      _ExtentY        =   10610
      _Version        =   393217
      ReadOnly        =   -1  'True
      ScrollBars      =   3
      TextRTF         =   $"frmView.frx":030A
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Terminal"
         Size            =   9
         Charset         =   255
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
   Begin RichTextLib.RichTextBox RichTextBox2 
      Height          =   6015
      Left            =   120
      TabIndex        =   4
      Top             =   1200
      Width           =   9255
      _ExtentX        =   16325
      _ExtentY        =   10610
      _Version        =   393217
      ReadOnly        =   -1  'True
      ScrollBars      =   3
      TextRTF         =   $"frmView.frx":0389
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Terminal"
         Size            =   9
         Charset         =   255
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
   Begin VB.Label Label1 
      Caption         =   "File:"
      Height          =   255
      Left            =   120
      TabIndex        =   2
      Top             =   240
      Width           =   495
   End
End
Attribute VB_Name = "frmView"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Private rtbSizeX As Long
Private rtbSizeY As Long


Public vfui As FuiCls

Private Sub Command1_Click()
  Unload Reference
  Unload Me
End Sub

Private Sub Form_load()
  Dim fin As String, fout As String, Line As String, Module As String
  Dim Lines As Long, i As Long, done As Boolean
  Dim rc As Integer
  Dim parser As csv
  Dim dic As New Collection
  Dim thistype As Variant
  Dim vmod As ModCls
  
  Dim source As String
  Dim sourcelabel As String
  Dim stype As String
  Dim sourcedes As String

  StartModule Me, "Text File View/Print", 6
  
  RichTextBox1.Move 100, TabStrip1.Top + TabStrip1.Height, _
                    Me.ScaleWidth - 100, Me.ScaleHeight - (430 + TabStrip1.Height)
  TabStrip1.Left = RichTextBox1.Left
  TabStrip1.Width = RichTextBox1.Width
  rtbSizeX = Me.Width - RichTextBox1.Width
  rtbSizeY = Me.Height - RichTextBox1.Height
  RichTextBox2.Move RichTextBox1.Left, RichTextBox1.Top, _
                    RichTextBox1.Width, RichTextBox1.Height
                    
  
  Set vfui = New FuiCls
  vfui.siteIdx = siteIdx
  vfui.Init Left(FUIName, InStr(FUIName, ".gid") - 1), RunName, siteIdx, modIdx - 1, ModName, argv(0)
  vfui.GetSource source, sourcelabel, stype, sourcedes
  Set vmod = vfui.GetModule(source, vfui)
  
  If "" <> Dir(FUIName) Then Combo1.AddItem FUIName & " (raw)"
  If vfui.argv0 <> "gid" Then
    If "" <> Dir(FUIName) Then Combo1.AddItem FUIName
  End If
  
  RefMode = 0
  SetRefFile ReplaceExt(FUIName, "ref")
  Load Reference
  
  parser.fnum = 1
  parser.getbuff = stype
  parser.separator = ","
  parser.leng = Len(stype)
  parser.pos = 0
  On Error Resume Next
  Do
    thistype = get_val(parser)
    If thistype <> "" Then dic.Add thistype, thistype
  Loop Until (thistype = "")
  
  FUIName = Left(FUIName, InStr(FUIName, ".gid") - 1)
  For Each thistype In dic
    If "" <> Dir(FUIName + "." + thistype) Then Combo1.AddItem FUIName + "." + thistype
  Next
  
  FUIName = FUIName + ".gid"
  vfui.DisplayModuleDescription source, RichTextBox2, vfui
  Combo1.ListIndex = Combo1.NewIndex
  
  If argc > 6 And IsNumeric(argv(argc - 6)) Then
    If 1 = Val(argv(argc - 6)) Then
      Command2_Click
      End
    End If
  End If

End Sub


Private Sub Form_Resize()
  If Me.ScaleHeight > rtbSizeY Then RichTextBox1.Height = Me.ScaleHeight - rtbSizeY
  If Me.ScaleWidth > rtbSizeX Then RichTextBox1.Width = Me.ScaleWidth - rtbSizeX
  
  TabStrip1.Width = RichTextBox1.Width
  RichTextBox2.Move RichTextBox1.Left, RichTextBox1.Top, _
                    RichTextBox1.Width, RichTextBox1.Height
End Sub

Private Sub Command2_Click()

  Dim flags As Long
  On Error GoTo mnuPrint_Exit
  Dim cdl As CommonDialog
  
' CommonDialog1.ShowPrinter fails to return when view is called from the FUI
  
' CommonDialog1.CancelError = True
' CommonDialog1.flags = cdlPDReturnDC + cdlPDNoPageNums
' If RichTextBox1.SelLength = 0 Then
'   CommonDialog1.flags = CommonDialog1.flags + cdlPDAllPages
' Else
'   CommonDialog1.flags = CommonDialog1.flags + cdlPDSelection
' End If
' CommonDialog1.ShowPrinter

' If RichTextBox1.SelLength = 0 Then
    flags = PD_NOPAGENUMS + PD_NOSELECTION '  PD_ALLPAGES
' Else
'   flags = PD_SELECTION
' End If
  If ShowPrinter(Me, flags) Then
    If flags And PD_PRINTTOFILE Then
     Set cdl = CommonDialog1
     On Error Resume Next
     If cdl.InitDir = "" Then cdl.InitDir = vfui.FUIName
     cdl.DialogTitle = "Print To File"
     If cdl.fileName = "" Then cdl.fileName = "*.rtf"
     cdl.Filter = "Rich Text Files (*.rtf)|*.rtf ' |Text Files (*.txt)|*.txt"
     cdl.FilterIndex = 0
     cdl.DefaultExt = "rtf"
     cdl.flags = cdlOFNOverwritePrompt Or cdlOFNHideReadOnly
     cdl.flags = cdl.flags Or cdlOFNExtensionDifferent
     cdl.CancelError = True
     On Error Resume Next
     cdl.ShowOpen
     If Not Err = cdlCancel Then
        RichTextBox1.SaveFile cdl.fileName, 0  ' & ".rtf", 0
     End If
    Else
      If TabStrip1.Tabs(1).Selected Then PrintRTF RichTextBox1, 1440, 1440, 1440, 1440
      If TabStrip1.Tabs(2).Selected Then PrintRTF RichTextBox2, 1440, 1440, 1440, 1440
    End If
  End If
mnuPrint_Exit:
  Exit Sub
End Sub

Private Sub Form_Unload(Cancel As Integer)
  EndModule
End Sub

Private Sub TabStrip1_Click()
  RichTextBox1.Visible = TabStrip1.Tabs(1).Selected
  RichTextBox2.Visible = Not RichTextBox1.Visible
End Sub
Private Sub Combo1_Click()
Dim fin As String
  fin = Combo1.list(Combo1.ListIndex)
  frmView.Caption = "Viewing File (" + fin + ") Section (" + vfui.ModName + ")"
' DisplayFileContents SiteIdx, fin, RichTextBox1, modname
'  vfui.argv0 = argv(0)
  vfui.raw = False
  vfui.argv0 = Right(fin, 3)
  If vfui.argv0 = "aw)" Then
    vfui.argv0 = "gid"
    vfui.raw = True
  End If
  vfui.DisplayFileContents RichTextBox1, vfui, True
End Sub
