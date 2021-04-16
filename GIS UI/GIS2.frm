VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "COMCTL32.OCX"
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "THREED32.OCX"
Object = "{C0A90FC0-6A87-11D2-98B7-0000E8CCF7AF}#1.0#0"; "Dbcgeo.ocx"
Object = "{EB962840-1204-11D0-98B4-0000E8CCF7AF}#1.0#0"; "Dbcocx32.ocx"
Begin VB.Form frmMain 
   Caption         =   "FRAMES LDRD GIS"
   ClientHeight    =   11490
   ClientLeft      =   60
   ClientTop       =   630
   ClientWidth     =   12780
   Icon            =   "GIS2.frx":0000
   LinkTopic       =   "Form1"
   MinButton       =   0   'False
   MouseIcon       =   "GIS2.frx":0442
   ScaleHeight     =   11490
   ScaleWidth      =   12780
   StartUpPosition =   2  'CenterScreen
   Begin ComctlLib.Toolbar Toolbar2 
      Height          =   390
      Left            =   2940
      TabIndex        =   7
      Top             =   120
      Width           =   3120
      _ExtentX        =   5503
      _ExtentY        =   688
      ButtonWidth     =   635
      ButtonHeight    =   582
      ImageList       =   "ImageList1"
      _Version        =   327682
      BeginProperty Buttons {0713E452-850A-101B-AFC0-4210102A8DA7} 
         NumButtons      =   11
         BeginProperty Button1 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Object.Visible         =   0   'False
            Key             =   "regen"
            Object.ToolTipText     =   "Redraw"
            Object.Tag             =   ""
         EndProperty
         BeginProperty Button2 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Object.Tag             =   ""
            Style           =   3
            MixedState      =   -1  'True
         EndProperty
         BeginProperty Button3 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   "zoomin"
            Object.ToolTipText     =   "Zoom In"
            Object.Tag             =   ""
            ImageKey        =   "zoomin"
         EndProperty
         BeginProperty Button4 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   "zoomout"
            Object.ToolTipText     =   "Zoom Out"
            Object.Tag             =   ""
            ImageKey        =   "zoomout"
         EndProperty
         BeginProperty Button5 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   "zoomarea"
            Object.ToolTipText     =   "Zoom Area"
            Object.Tag             =   ""
            ImageKey        =   "zoomarea"
         EndProperty
         BeginProperty Button6 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   "zoomprev"
            Object.ToolTipText     =   "Zoom Previous"
            Object.Tag             =   ""
            ImageKey        =   "zoomprev"
         EndProperty
         BeginProperty Button7 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   "zoomall"
            Object.ToolTipText     =   "Zoom All"
            Object.Tag             =   ""
            ImageKey        =   "zoomall"
         EndProperty
         BeginProperty Button8 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Object.Tag             =   ""
            Style           =   3
            MixedState      =   -1  'True
         EndProperty
         BeginProperty Button9 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   "zoomreset"
            Object.ToolTipText     =   "Zoom Reset"
            Object.Tag             =   ""
            ImageKey        =   "zoomreset"
         EndProperty
         BeginProperty Button10 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Object.Tag             =   ""
            Style           =   3
            MixedState      =   -1  'True
         EndProperty
         BeginProperty Button11 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   "overview"
            Object.ToolTipText     =   "Overview"
            Object.Tag             =   ""
            ImageKey        =   "overview"
         EndProperty
      EndProperty
   End
   Begin ComctlLib.Toolbar Toolbar1 
      Height          =   390
      Left            =   6300
      TabIndex        =   6
      Top             =   135
      Width           =   2625
      _ExtentX        =   4630
      _ExtentY        =   688
      ButtonWidth     =   635
      ButtonHeight    =   582
      ImageList       =   "ImageList1"
      _Version        =   327682
      BeginProperty Buttons {0713E452-850A-101B-AFC0-4210102A8DA7} 
         NumButtons      =   8
         BeginProperty Button1 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   "Point"
            Object.ToolTipText     =   "Insert Point"
            Object.Tag             =   ""
            ImageKey        =   "point"
         EndProperty
         BeginProperty Button2 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   "Line"
            Object.ToolTipText     =   "Insert Line"
            Object.Tag             =   ""
            ImageKey        =   "polyline"
         EndProperty
         BeginProperty Button3 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   "Polygon"
            Object.ToolTipText     =   "Insert Polygon"
            Object.Tag             =   ""
            ImageKey        =   "polygon"
         EndProperty
         BeginProperty Button4 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Enabled         =   0   'False
            Object.Visible         =   0   'False
            Key             =   "Circle"
            Object.ToolTipText     =   "Insert Circle"
            Object.Tag             =   ""
            ImageKey        =   "circle"
         EndProperty
         BeginProperty Button5 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   "Rectangle"
            Object.ToolTipText     =   "Insert Rectangle"
            Object.Tag             =   ""
            ImageKey        =   "rectangle"
         EndProperty
         BeginProperty Button6 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   "Polar"
            Object.ToolTipText     =   "Insert Polar Grid"
            Object.Tag             =   ""
            ImageKey        =   "polar"
         EndProperty
         BeginProperty Button7 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Key             =   "Cartesian"
            Object.ToolTipText     =   "Insert Cartesian Grid"
            Object.Tag             =   ""
            ImageKey        =   "cartesian"
         EndProperty
         BeginProperty Button8 {0713F354-850A-101B-AFC0-4210102A8DA7} 
            Object.Visible         =   0   'False
            Key             =   "PointGrid"
            Object.ToolTipText     =   "Insert Point Grid"
            Object.Tag             =   ""
            ImageKey        =   "pointgrid"
         EndProperty
      EndProperty
   End
   Begin VB.ComboBox cboLayer 
      Height          =   315
      Left            =   10920
      Style           =   2  'Dropdown List
      TabIndex        =   8
      Top             =   195
      Visible         =   0   'False
      Width           =   1560
   End
   Begin ComctlLib.StatusBar StatusBar1 
      Align           =   2  'Align Bottom
      Height          =   375
      Left            =   0
      TabIndex        =   4
      Top             =   11115
      Width           =   12780
      _ExtentX        =   22543
      _ExtentY        =   661
      SimpleText      =   ""
      _Version        =   327682
      BeginProperty Panels {0713E89E-850A-101B-AFC0-4210102A8DA7} 
         NumPanels       =   5
         BeginProperty Panel1 {0713E89F-850A-101B-AFC0-4210102A8DA7} 
            Alignment       =   1
            AutoSize        =   1
            Object.Width           =   16880
            Object.Tag             =   ""
         EndProperty
         BeginProperty Panel2 {0713E89F-850A-101B-AFC0-4210102A8DA7} 
            Object.Tag             =   ""
         EndProperty
         BeginProperty Panel3 {0713E89F-850A-101B-AFC0-4210102A8DA7} 
            Object.Tag             =   ""
         EndProperty
         BeginProperty Panel4 {0713E89F-850A-101B-AFC0-4210102A8DA7} 
            AutoSize        =   2
            Object.Visible         =   0   'False
            Object.Width           =   1270
            MinWidth        =   1270
            Object.Tag             =   ""
         EndProperty
         BeginProperty Panel5 {0713E89F-850A-101B-AFC0-4210102A8DA7} 
            AutoSize        =   2
            Object.Visible         =   0   'False
            Object.Width           =   1270
            MinWidth        =   1270
            Object.Tag             =   ""
         EndProperty
      EndProperty
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
   Begin VB.ListBox List1 
      Height          =   255
      Left            =   2220
      TabIndex        =   3
      Top             =   4545
      Visible         =   0   'False
      Width           =   1215
   End
   Begin Threed.SSPanel SSPanel3 
      Height          =   10590
      Left            =   2520
      TabIndex        =   0
      Top             =   570
      Width           =   135
      _Version        =   65536
      _ExtentX        =   238
      _ExtentY        =   18680
      _StockProps     =   15
      BackColor       =   13160660
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
   End
   Begin DBCGEOLib.DbcGeo DbcGeo1 
      Left            =   6240
      Top             =   3360
      _Version        =   65536
      _ExtentX        =   4048
      _ExtentY        =   2990
      _StockProps     =   0
   End
   Begin MSComDlg.CommonDialog cmndlg 
      Left            =   2415
      Top             =   3585
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin ComctlLib.TreeView TreeView1 
      Height          =   10500
      Left            =   15
      TabIndex        =   1
      Top             =   555
      Width           =   2535
      _ExtentX        =   4471
      _ExtentY        =   18521
      _Version        =   327682
      LabelEdit       =   1
      Style           =   5
      ImageList       =   "ImageList1"
      Appearance      =   1
   End
   Begin DBCOCXLib.Dbcocx Dbcocx1 
      Height          =   10485
      Left            =   2775
      TabIndex        =   2
      Top             =   570
      Width           =   9975
      _Version        =   65958
      _ExtentX        =   17595
      _ExtentY        =   18494
      _StockProps     =   0
      ButtonFlag      =   0
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      Caption         =   "Module Input Requirements"
      Height          =   315
      Left            =   0
      TabIndex        =   5
      Top             =   150
      Width           =   2535
   End
   Begin ComctlLib.ImageList ImageList1 
      Left            =   2520
      Top             =   240
      _ExtentX        =   1005
      _ExtentY        =   1005
      BackColor       =   -2147483643
      ImageWidth      =   16
      ImageHeight     =   16
      MaskColor       =   12632256
      _Version        =   327682
      BeginProperty Images {0713E8C2-850A-101B-AFC0-4210102A8DA7} 
         NumListImages   =   20
         BeginProperty ListImage1 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "GIS2.frx":0594
            Key             =   "closed"
         EndProperty
         BeginProperty ListImage2 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "GIS2.frx":08AE
            Key             =   "open"
         EndProperty
         BeginProperty ListImage3 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "GIS2.frx":0BC8
            Key             =   "red"
         EndProperty
         BeginProperty ListImage4 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "GIS2.frx":0F1A
            Key             =   "green"
         EndProperty
         BeginProperty ListImage5 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "GIS2.frx":126C
            Key             =   "circle"
         EndProperty
         BeginProperty ListImage6 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "GIS2.frx":15BE
            Key             =   "polygon"
         EndProperty
         BeginProperty ListImage7 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "GIS2.frx":1910
            Key             =   "polyline"
         EndProperty
         BeginProperty ListImage8 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "GIS2.frx":1C62
            Key             =   "rectangle"
         EndProperty
         BeginProperty ListImage9 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "GIS2.frx":1FB4
            Key             =   "zoomin"
         EndProperty
         BeginProperty ListImage10 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "GIS2.frx":26C6
            Key             =   "point"
         EndProperty
         BeginProperty ListImage11 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "GIS2.frx":2A18
            Key             =   "zoomout"
         EndProperty
         BeginProperty ListImage12 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "GIS2.frx":312A
            Key             =   "zoomarea"
         EndProperty
         BeginProperty ListImage13 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "GIS2.frx":383C
            Key             =   "zoomprev"
         EndProperty
         BeginProperty ListImage14 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "GIS2.frx":3F4E
            Key             =   "zoomall"
         EndProperty
         BeginProperty ListImage15 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "GIS2.frx":4660
            Key             =   "zoomreset"
         EndProperty
         BeginProperty ListImage16 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "GIS2.frx":4D72
            Key             =   "zoomreset2"
         EndProperty
         BeginProperty ListImage17 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "GIS2.frx":5484
            Key             =   "overview"
         EndProperty
         BeginProperty ListImage18 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "GIS2.frx":59E6
            Key             =   "cartesian"
         EndProperty
         BeginProperty ListImage19 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "GIS2.frx":5D38
            Key             =   "pointgrid"
         EndProperty
         BeginProperty ListImage20 {0713E8C3-850A-101B-AFC0-4210102A8DA7} 
            Picture         =   "GIS2.frx":608A
            Key             =   "polar"
         EndProperty
      EndProperty
   End
   Begin VB.Menu mnuFile 
      Caption         =   "&File"
      Begin VB.Menu mnuExitSave 
         Caption         =   "&Save and Exit"
      End
      Begin VB.Menu mnuExit 
         Caption         =   "E&xit"
      End
   End
   Begin VB.Menu mnuCoverages 
      Caption         =   "Coverages..."
   End
   Begin VB.Menu mnuZoom 
      Caption         =   "Zoom"
      Begin VB.Menu mnuZoomIn 
         Caption         =   "Zoom In"
      End
      Begin VB.Menu mnuZoomOut 
         Caption         =   "Zoom Out"
      End
      Begin VB.Menu mnuZoomArea 
         Caption         =   "Zoom Area"
      End
      Begin VB.Menu mnuZoomPrev 
         Caption         =   "Zoom Previous"
      End
      Begin VB.Menu mnuZoomReset 
         Caption         =   "Zoom Reset"
      End
      Begin VB.Menu mnuZoomSep1 
         Caption         =   "-"
      End
      Begin VB.Menu mnuZoomAll 
         Caption         =   "Zoom All"
      End
      Begin VB.Menu mnuZoomSep2 
         Caption         =   "-"
      End
      Begin VB.Menu mnuZoomOverview 
         Caption         =   "Show OverView"
      End
   End
   Begin VB.Menu mnuInsert 
      Caption         =   "Insert"
      Begin VB.Menu mnuInsPoint 
         Caption         =   "Point"
      End
      Begin VB.Menu mnuInsLine 
         Caption         =   "Line"
      End
      Begin VB.Menu mnuInsPolygon 
         Caption         =   "Polygon"
      End
      Begin VB.Menu mnuInsCircle 
         Caption         =   "Circle"
         Visible         =   0   'False
      End
      Begin VB.Menu mnuInsRectangle 
         Caption         =   "Rectangle"
      End
      Begin VB.Menu mnuInsPGrid 
         Caption         =   "Polar Grid"
      End
      Begin VB.Menu mnuInsCGrid 
         Caption         =   "Cartesian Grid"
      End
      Begin VB.Menu mnuInsPtGrid 
         Caption         =   "Point Grid"
         Enabled         =   0   'False
         Visible         =   0   'False
      End
   End
   Begin VB.Menu mnuEdit 
      Caption         =   "Edit"
      Visible         =   0   'False
      Begin VB.Menu mnuEditPick 
         Caption         =   "Pick an entity"
      End
   End
   Begin VB.Menu mnuPolygon 
      Caption         =   "Polygon"
      Visible         =   0   'False
      Begin VB.Menu mnuPolyInsert 
         Caption         =   "Insert"
         Begin VB.Menu mnuPolyInsEnd 
            Caption         =   "End"
         End
         Begin VB.Menu mnuPolyInsUndoAll 
            Caption         =   "Undo All"
         End
         Begin VB.Menu mnuPolyInsUndoLast 
            Caption         =   "Undo Last"
         End
         Begin VB.Menu mnuPolyInsCont 
            Caption         =   "Continue"
         End
         Begin VB.Menu mnuPolyInsIsle 
            Caption         =   "New Island"
         End
         Begin VB.Menu mnuPolyInsHole 
            Caption         =   "New Hole"
         End
      End
      Begin VB.Menu mnuPolyEdit 
         Caption         =   "Edit"
         Visible         =   0   'False
         Begin VB.Menu mnuPolyEditEnd 
            Caption         =   "End"
         End
         Begin VB.Menu mnuPolyEditUndo 
            Caption         =   "Undo All"
         End
         Begin VB.Menu mnuPolyEditMove 
            Caption         =   "Move All"
         End
         Begin VB.Menu mnuPolyEditMoveVrtx 
            Caption         =   "Move a Vertex"
         End
         Begin VB.Menu mnuPolyEditAddVrtx 
            Caption         =   "Add a Vertex"
         End
         Begin VB.Menu mnuPolyEditDelVrtx 
            Caption         =   "Delete a Vertex"
         End
      End
   End
   Begin VB.Menu mnuEntity 
      Caption         =   "Entity"
      Visible         =   0   'False
      Begin VB.Menu mnuEntityData 
         Caption         =   "Data"
      End
      Begin VB.Menu mnuEntityMove 
         Caption         =   "Move"
      End
      Begin VB.Menu mnuEntityDel 
         Caption         =   "Delete"
      End
      Begin VB.Menu mnuEntityProperties 
         Caption         =   "Properties"
      End
      Begin VB.Menu mnuEntityCancel 
         Caption         =   "Cancel"
      End
   End
   Begin VB.Menu mnuDB 
      Caption         =   "GraphicDB..."
      Enabled         =   0   'False
   End
   Begin VB.Menu mnuTreeview 
      Caption         =   "Treeview"
      Visible         =   0   'False
      Begin VB.Menu mnuTvwDeselect 
         Caption         =   "Deselect"
      End
      Begin VB.Menu mnuTvwView 
         Caption         =   "View Data"
      End
      Begin VB.Menu mnuTvwCancel 
         Caption         =   "Cancel"
      End
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Compare Text
Option Explicit

Public XCentr As Double, YCentr As Double
Public ViewX0 As Double
Public ViewY0 As Double
Public ViewX1 As Double
Public ViewY1 As Double
Public ViewZf As Double
Public ViewRf As Double
Public ReDisplay As Boolean ' set by frmConfig

Public LastSelectedEntityId As String
Private LastHighLightedId As String
Public DontHighLight As Boolean
Private DispMap As Boolean
Private DispPhoto As Boolean
Private DispDwg As Boolean

Private DbcSizeX As Long
Private DbcSizeY As Long
Private TvwSizeX As Long
Private TvwSizeY As Long

Private moving As Boolean

Public unloading As Boolean





Private Sub cboLayer_Click()
  myLayer = layers(cboLayer.ItemData(cboLayer.ListIndex))
  Dbcocx1.devgrcmdlayerset myLayer.Name
  Dbcocx1.devgrcmdecolorset myLayer.lcolor
  Dbcocx1.devgrcmdelineset myLayer.linetype
End Sub


Private Sub cboObjType_Click()
  If Not enableState Then Exit Sub
  
End Sub


Private Sub Dbcocx1_GrClick(ByVal Button As Integer, ByVal x As Double, ByVal y As Double)
  Dim OkToDelete As Boolean
  Dim FirstFind As Long
  Dim objtype As String
  Dim l As Long
  
  DisplayStatus ""
  
  If 0 = UBound(layers) Then
    mnuCoverages_Click
    Exit Sub
  End If
  
  RefreshGrDisplay
  
  If Devgrseek(x, y) Then
    FirstFind = Dbcocx1.devrecno()
    Do
        If Left(Dbcocx1.devgetfield("DESCR"), 7) <> TPOLYGON Then Exit Do
        Dbcocx1.Devgrseek x, y, Dbcocx1.devrecno()
        If Dbcocx1.devfound() = 0 Then
            Dbcocx1.devgorecno FirstFind
            Exit Do
        End If
    Loop
    
    FirstFind = Dbcocx1.devrecno()
    SelectedEntityId = Dbcocx1.devgetid()
    
    SelectDbgr SelectedLayer, True

    objtype = Dbcocx1.devgetfield("DESCR")
    objtype = Left(objtype, InStr(objtype, " ") - 1)
    
    Dbcocx1.devgrdisplayid SelectedEntityId, 1
    If OpMode = CLIENT Then
      If Button = 2 Then 'options
        frmMain.PopupMenu frmMain.mnuEntity
      Else
        KeyPadSel = KPS_NONE ' KPS_DATA
      End If
      Select Case KeyPadSel
          Case KPS_DELETE
              Dbcocx1.devgrcmddelete "E", SelectedEntityId
              Dbcocx1.devpack
              Dbcocx1.devgrclear 0
              Dbcocx1.devgrdisplay
              
          Case KPS_PROP
            Dbcocx1.devseek "E" & SelectedEntityId
            frmProp.Show 1
            Dbcocx1.devgrdisplayid "", 256 ' redisplay all stored changes
          Case KPS_DATA
              Dbcocx1.devseek "E" & SelectedEntityId
              If 0 = Dbcocx1.devfound() Then
'               Debug.Print "not found E" & SelectedEntityId
              Else
              frmData.Show 1
              End If
              Dbcocx1.devgrdisplayid SelectedEntityId, 1
          Case KPS_MOVE
            Select Case objtype
              Case TPOLYGON:
                MovePolygon frmMain.Dbcocx1
              Case TLINE:
              Case TPOINT:
            End Select
              Dbcocx1.devgrclear 0
              Dbcocx1.devgrdisplay
          Case Else
      End Select
    Else
      If GisObj.ValidObject(Dbcocx1.devgetfield("DESCR"), SelectedEntityId) Then
      Else
        Beep
      End If
    End If
  Else
    If Button = 2 Then frmMain.PopupMenu frmMain.mnuZoom
  End If
End Sub

Private Sub Dbcocx1_GrMove(ByVal x As Double, ByVal y As Double)
    StatusBar1.Panels(2).text = "X:" & Format(x, "0.00")
    StatusBar1.Panels(3).text = "Y:" & Format(y, "0.00")
End Sub

Private Sub Dbcocx1_ViewChange()
  ViewChange
End Sub

Private Sub Form_load()
  FRAMES_INI = App.Path + "\\FramesUI.ini"
  If unloading Then Exit Sub
  
  If FAILURE = GisConnect(Command) Then
    End
  End If
  
  If Not SelectGraphicDatabase(RunName, "dbf") Then Exit Sub

  If "" = dir(FUIName & ".dbf") Then
    CreateGraphicDatabase FUIName, "dbf"
  End If
  If "" <> dir$(RunName & ".dbf") Then Kill RunName & ".dbf"
  If "" <> dir$(RunName & ".ndx") Then Kill RunName & ".ndx"
  FileCopy FUIName & ".dbf", RunName & ".dbf"
  FileCopy FUIName & ".ndx", RunName & ".ndx"

  SSPanel3.BevelOuter = 0
  TreeView1.Width = SSPanel3.Left
  Dbcocx1.Left = (TreeView1.Width + SSPanel3.Width) + 2 * Screen.TwipsPerPixelX
  Dbcocx1.Width = frmMain.ScaleWidth - (TreeView1.Width + SSPanel3.Width)
  
  OpMode = CLIENT
  
  FormLoad
End Sub


Public Sub DisplayBackground()
Dim id As Integer, l As Integer
Dim x As Double
Dim y As Double
Dim cal As Boolean
Dim calibrated As Boolean

 MousePointer = vbHourglass
 
 If 0 < UBound(layers) Then
   StatusBar1.Panels(1).text = "Please wait while maps are being drawn..."
 End If
 
 Dbcocx1.devdisplay "" ' clear displayed raster images
 Dbcocx1.devgrclear 2 ' clears all vector drawings (background and database)
 
  For l = 1 To UBound(layers)
    If layers(l).filename <> "" And layers(l).visible Then
      calibrated = True
      Dbcocx1.devdrawdwg layers(l).filename, layers(l).lcolor, ""
    End If
  Next l
  If Not calibrated Then
'    Dbcocx1.devdisplayxyext 1000, 1000, 1 ' raster background
'    Dbcocx1.devcalibrate 0, 999, 0, 0, 999, 0, 1000, 1000
  End If

 If calibrated Then
  SelectDbgr 0, True
  Dbcocx1.devgrdisplay
  'ZoomReset
  'ZoomPrevious
  If XCentr <> 0 Then
  Dbcocx1.devzoomw ViewX0, ViewY0, ViewX1, ViewY1
End If
 End If
 
 StatusBar1.Panels(1).text = ""
 MousePointer = vbDefault
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
  MousePointer = vbDefault
End Sub

Private Sub Form_Resize()
  If unloading Then Exit Sub
  Dbcocx1.Left = (TreeView1.Width + SSPanel3.Width) + 2 * Screen.TwipsPerPixelX
  Dbcocx1.Width = frmMain.ScaleWidth - (TreeView1.Width + SSPanel3.Width + 2 * Screen.TwipsPerPixelX)
  
'   If Me.Width > DbcSizeX Then Dbcocx1.Width = Me.Width - DbcSizeX
    If Me.Height > DbcSizeY Then Dbcocx1.Height = Me.Height - DbcSizeY
'   zoomOn XCentr, YCentr
    
  TreeView1.Width = SSPanel3.Left
'   If Me.Width > TvwSizeX Then TreeView1.Width = Me.Width - TvwSizeX
  If Me.Height > TvwSizeY Then TreeView1.Height = Me.Height - TvwSizeY
  SSPanel3.Height = TreeView1.Height
    
End Sub

Private Sub mnuCoverages_Click()
  DisplayStatus ""
  ConfigureCoverages
End Sub


Private Sub mnuDB_Click()
   GRDBVIEW.Show 1
End Sub

Private Sub mnuEditPick_Click()
    DisableUI True
    Dim px As Double, py As Double
    
    GetPoint "Pick an entity (right button to abort):", px, py
    If Dbcocx1.devlastkey() = 13 Then
        Dbcocx1.Devgrseek px, py, 0
        If Dbcocx1.devfound() <> 0 Then
            ModifyPoly Dbcocx1, 1, DbcGeo1
        End If
    End If
    DisableUI False
End Sub

Private Sub mnuEntity_Click()
' mnuEntityData.Enabled = (0 = SelectedLayer) ' always true
  mnuEntityDel.Enabled = (0 = SelectedLayer) And Not EntityInUse
  mnuEntityMove.Enabled = False ' temporarily (0 = SelectedLayer)
End Sub

Private Sub mnuEntityCancel_Click()
  KeyPadSel = KPS_NONE
End Sub
Private Sub mnuEntityProperties_Click()
  KeyPadSel = KPS_PROP
End Sub

Private Sub mnuEntityData_Click()
  KeyPadSel = KPS_DATA
End Sub

Private Sub mnuEntityDel_Click()
  KeyPadSel = KPS_DELETE
End Sub

Private Sub mnuEntityMove_Click()
  KeyPadSel = KPS_MOVE
End Sub

Private Sub mnuExit_Click()
  Dbcocx1.devcloseall
  If "" <> dir$(RunName & ".dbf") Then Kill RunName & ".dbf"
  If "" <> dir$(RunName & ".ndx") Then Kill RunName & ".ndx"
  GisEndModule
End Sub


Private Sub mnuExitSave_Click()
Dim res

  SelectDbgr 0, True
  If Not UpdateTreeviewStatus() Then
    res = MsgBox("Module input is incomplete. Continue with save?", vbYesNo, "Warning")
    If res = vbYes Then
      put_val errfile, "Module input is incomplete."
      put_line errfile
      AnError = True
    Else
      Exit Sub
    End If
  End If
  SaveGid
  Dbcocx1.devcloseall
  SaveDatabase ' copies RunName.dbf/.ndx to FUIName.dbf/.ndx
  GisEndModule
End Sub

Private Sub mnuFile_Click()
  DisplayStatus ""
  mnuExitSave.Enabled = enableState
  mnuExit.Enabled = enableState
End Sub

Private Sub mnuInsCGrid_Click()
Dim id As String
  DisableUI True
  SelectDbgr 0, True
  LayerSet Dbcocx1, TUSER
  id = Trim(InsertGridCartesian(Dbcocx1))
  DisableUI False
End Sub

Private Sub mnuInsCircle_Click()
Dim id As String
  DisableUI True
  SelectDbgr 0, True
  LayerSet Dbcocx1, TUSER
  id = Trim(InsertCircle(Dbcocx1))
  DisableUI False
End Sub

Private Sub mnuInsert_Click()
  DisplayStatus ""
  myLayer = layers(0) ' layers(cboLayer.ItemData(cboLayer.ListIndex))
' mnuInsPoint.enabled = (myLayer.objtype = TPOINT)
' mnuInsLine.enabled = (myLayer.objtype = TLINE)
' mnuInsPolygon.enabled = (myLayer.objtype = TPOLYGON)
' mnuInsCircle.enabled = mnuInsPolygon.enabled
' mnuInsRectangle.enabled = mnuInsPolygon.enabled
' mnuInsPGrid.enabled = mnuInsPoint.enabled
' mnuInsCGrid.enabled = mnuInsPoint.enabled
End Sub

Private Sub mnuInsLine_Click()
Dim id As String
  SelectDbgr 0, True
  LayerSet Dbcocx1, TUSER
  id = Trim(InsertLine(Dbcocx1))
End Sub

Private Sub mnuInsPGrid_Click()
Dim id As String
  DisableUI True
  SelectDbgr 0, True
  LayerSet Dbcocx1, TUSER
  id = Trim(InsertGridPolar(Dbcocx1))
  DisableUI False
End Sub

Private Sub mnuInsPoint_Click()
Dim id As String
  DisableUI True
  SelectDbgr 0, True
  LayerSet Dbcocx1, TUSER
  id = Trim(InsertPoint(Dbcocx1))
  DisableUI False
End Sub

Private Sub mnuInsPolygon_Click()
Dim id As String
  
  DisableUI True
  SelectDbgr 0, True
  LayerSet Dbcocx1, TUSER
  ' here you can set the ecolor and eline too
  ' InsertPoly(Dbcocx1 As Dbcocx, PolyType As Integer, WorkArea As Long,
  '             Optional DbcGeo1 As DbcGeo = Nothing, Optional TempArea As Long = 0,
  '              Optional ID As String = "", Optional style As Long = 6, Optional BitMask As String = "")
  InsertPoly Dbcocx1, POLYGON, 1, DbcGeo1, GetRandomWorkArea(), id, defaults(DPOLY).fill
  DisableUI False
End Sub

Private Sub mnuInsPtGrid_Click()
Dim id As String, ptid As String
Dim recno As Long, l As Long, found As Boolean, eid As Long
Dim lname As String

  DisableUI True
  SelectDbgr 0, True
  LayerSet Dbcocx1, TUSER
  id = Trim(InsertPointGrid(Dbcocx1, POLYGON, 1, DbcGeo1, GetRandomWorkArea(), "", defaults(DPOLY).fill))
  LocatePointGridPoints Dbcocx1, id, True
  DisableUI False
End Sub

Private Sub mnuInsRectangle_Click()
Dim id As String
  DisableUI True
  SelectDbgr 0, True
  LayerSet Dbcocx1, TUSER
  id = Trim(InsertRectangle(Dbcocx1))
  DisableUI False
End Sub

Private Sub mnuPolyEditAddVrtx_Click()
  PolyEdit.KeyPadSel = KPS_ADDVRTX
End Sub

Private Sub mnuPolyEditDelVrtx_Click()
  PolyEdit.KeyPadSel = KPS_DELVRTX
End Sub

Private Sub mnuPolyEditEnd_Click()
  PolyEdit.KeyPadSel = KPS_END
End Sub

Private Sub mnuPolyEditMove_Click()
  PolyEdit.KeyPadSel = KPS_MOVEALL
End Sub

Private Sub mnuPolyEditMoveVrtx_Click()
  PolyEdit.KeyPadSel = KPS_MOVEVRTX
End Sub

Private Sub mnuPolyEditUndo_Click()
  PolyEdit.KeyPadSel = KPS_UNDOALL
End Sub

Private Sub mnuPolyInsCont_Click()
  PolyEdit.KeyPadSel = KPS_CONTINUE
End Sub

Private Sub mnuPolyInsEnd_Click()
  PolyEdit.KeyPadSel = KPS_END
End Sub

Private Sub mnuPolyInsHole_Click()
  PolyEdit.KeyPadSel = KPS_HOLE
End Sub

Private Sub mnuPolyInsIsle_Click()
  PolyEdit.KeyPadSel = KPS_ISLE
End Sub

Private Sub mnuPolyInsUndoAll_Click()
  PolyEdit.KeyPadSel = KPS_UNDOALL
End Sub

Private Sub mnuPolyInsUndoLast_Click()
  PolyEdit.KeyPadSel = KPS_UNDOLAST
End Sub


Private Sub LoadGid()
Dim fle As parmfile, temp As parmrec, i As Long, j As Long, modix As Long, m As Long
Dim parm As parmrec

  
  If open_parm(fle, FUIName & ".GID", 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pName
          Case "csm"
            For i = 1 To temp.idx1
              If 1 = read_parmrec(fle, temp) Then
                If temp.idx1 = SiteIdx Then
                  Select Case temp.pName
                    Case "modid":
                      If temp.pval = ModName Then modix = temp.idx2
                    Case "modsinkid"
                      If temp.idx2 = modix Then
                        AddModule temp.pval
'                       cboModId.AddItem Temp.pval
                      End If
                  End Select
                End If
              End If
            Next
            Exit Do
          Case Else
            For m = 1 To temp.idx1
              get_line fle.file
            Next
        End Select
      End If
    Loop
    close_parm fle
  End If
  If open_parm(fle, FUIName & ".GID", 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pName
          Case "csm"
            For i = 1 To temp.idx1
              If 1 = read_parmrec(fle, temp) Then
                If temp.idx1 = SiteIdx Then
                  Select Case temp.pName
                    Case "modid":
                      modix = 0
                      For j = 1 To UBound(module)
                        If temp.pval = module(j).id Then
                          modix = temp.idx2
                          Do Until temp.idx2 <> modix
                            read_parmrec fle, temp
                            If modix = temp.idx2 And temp.pName = "ModLabel" Then
                              module(j).Name = temp.pval
                            End If
                            If modix = temp.idx2 And temp.pName = "ModDesPath" Then
                              module(j).Load temp.pval
                              Exit For
                            End If
                          Loop
                          Exit For
                        End If
                      Next j
                  End Select
                End If
              End If
            Next
            Exit Do
          Case Else
            For m = 1 To temp.idx1
              get_line fle.file
            Next
        End Select
      End If
    Loop
    close_parm fle
  End If
  
  ReDim gidmod(0)
  ReDim gidobj(0)
  
  If open_parm(fle, FUIName & ".GID", 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pName
          Case ModName
            For i = 1 To temp.idx1
              If 1 = read_parmrec(fle, temp) Then
                Select Case temp.pName
                  Case "XCenter": XCentr = Val(temp.pval)
                  Case "YCenter": YCentr = Val(temp.pval)
                  Case "ViewX0": ViewX0 = Val(temp.pval)
                  Case "ViewY0": ViewY0 = Val(temp.pval)
                  Case "ViewX1": ViewX1 = Val(temp.pval)
                  Case "ViewY1": ViewY1 = Val(temp.pval)
                  
                  Case "NumLayer":
                    ReDim Preserve layers(Val(temp.pval))
'                 Case "LayerName":
'                   layers(temp.idx1).name = temp.pval
                  Case "LayerColor":
                    layers(temp.idx1).color = Val(temp.pval)
                    layers(temp.idx1).lcolor = Val(temp.pval)
                  Case "LayerLinetype":
                    layers(temp.idx1).linetype = Val(temp.pval)
                  Case "LayerFilename":
                    layers(temp.idx1).filename = temp.pval
                  Case "LayerVisible":
                    layers(temp.idx1).visible = CBool(temp.pval)
                  Case "LayerPickable":
                    layers(temp.idx1).pickable = CBool(temp.pval)
                  Case "NumDefaults":
                    ReDim Preserve defaults(Val(temp.pval) - 1)
                  Case "DefType":
                    defaults(temp.idx1 - 1).objtype = temp.pval
                  Case "DefColor":
                    defaults(temp.idx1 - 1).color = Val(temp.pval)
                  Case "DefLine":
                    defaults(temp.idx1 - 1).linetype = Val(temp.pval)
                  Case "DefFill":
                    defaults(temp.idx1 - 1).fill = Val(temp.pval)
                  Case "DefX":
                    defaults(temp.idx1 - 1).x = Val(temp.pval)
                  Case "DefY":
                    defaults(temp.idx1 - 1).y = Val(temp.pval)
                    
                  Case "NumModules"
                    ReDim gidmod(Val(temp.pval))
                  Case "ModuleId"
                    gidmod(temp.idx1).id = temp.pval
                  Case "NumModGeoObj"
                    gidmod(temp.idx1).numobj = Val(temp.pval)
                  Case "ModGeoObjIndex"
                    gidmod(temp.idx1).objindex(temp.idx2) = Val(temp.pval)
                  Case "NumGeoObjects":
                    ReDim gidobj(Val(temp.pval))
                  Case "GeoObjId":
                    gidobj(temp.idx1).id = temp.pval
                  Case "GeoObjLabel":
                    gidobj(temp.idx1).label = temp.pval
                  Case "GeoObjKey":
                    gidobj(temp.idx1).key = temp.pval
                  Case "GeoObjType"
                    gidobj(temp.idx1).type = temp.pval
                  Case "GeoObjIndex":
                    gidobj(temp.idx1).Index = Val(temp.pval)
                End Select
              End If
            Next
            Exit Do
          Case Else
            For m = 1 To temp.idx1
              get_line fle.file
            Next
        End Select
      End If
    Loop
    close_parm fle
  End If
  
  
  If 0 < UBound(gidmod) Then
    If 0 < UBound(gidobj) Then
      For i = 1 To UBound(module)
        module(i).LoadGid i
      Next i
    End If
  End If
  
  ReDim gidmod(0)
  ReDim gidobj(0)

End Sub


Sub ZoomOn(ByVal x As Double, ByVal y As Double, Optional zoom100 As Boolean = True)
    Dim DeltaX As Double, DeltaY As Double
    Dim x1 As Double, y1 As Double, x2 As Double, y2 As Double, zf As Double, rf As Double
    If zoom100 Then Dbcocx1.devzoom 100
    Dbcocx1.devgetview x1, y1, x2, y2, zf, rf
    DeltaX = x2 - x1
    DeltaY = y2 - y1
    x1 = x - (DeltaX / 2#)
    y1 = y - (DeltaY / 2#)
    x2 = x + (DeltaX / 2#)
    y2 = y + (DeltaY / 2#)
    Dbcocx1.devzoomw x1, y1, x2, y2
End Sub

Private Sub mnuTreeview_Click()
  KeyPadSel = KPS_NONE
End Sub

Private Sub mnuTvwCancel_Click()
  KeyPadSel = KPS_NONE
End Sub

Private Sub mnuTvwDeselect_Click()
  KeyPadSel = KPS_DELETE
End Sub

Private Sub mnuTvwView_Click()
  KeyPadSel = KPS_DATA
End Sub

Private Sub mnuZoom_Click()
  DisplayStatus ""
End Sub

Private Sub mnuZoomAll_Click()
  ZoomAll
End Sub

Private Sub mnuZoomArea_Click()
  ZoomArea
End Sub

Private Sub mnuZoomIn_Click()
  ZoomIn
End Sub

Private Sub mnuZoomOut_Click()
  ZoomOut
End Sub


Private Sub mnuZoomOverview_Click()
  ZoomOverview
End Sub

Private Sub mnuZoomPrev_Click()
  ZoomPrevious
End Sub

Private Sub mnuZoomReset_Click()
  ZoomReset
End Sub

Public Sub OpenGrDatabase()
Dim i As Long
Dim action As String

On Error GoTo OpenGrDatabase_error

  Dbcocx1.devsetautoregen 1 ' autoregen = true
  Dbcocx1.devsetaperture 6 ' for making easy to catch a block with the mouse
  Dbcocx1.devsetgrseekall 1 ' search also the polygon lines, not only the insertion point

  Dbcocx1.devdisplayxyext 1000, 1000, 1 ' default raster background
  Dbcocx1.devcalibrate 0, 999, 0, 0, 999, 0, 1000, 1000

  mnuDB.Enabled = True
    
' GetDatabaseLayers Dbcocx1, 0, cboLayer, layers()

' cboLayer.Clear
' If 0 < UBound(layers) Then
    DisplayBackground
'   For i = 1 To UBound(layers)
'     If layers(i).visible And Not layers(i).bkgd Then
'       cboLayer.AddItem layers(i).name
'       cboLayer.ItemData(cboLayer.NewIndex) = i
'     End If
'   Next i
'   If 0 < cboLayer.ListCount Then cboLayer.ListIndex = 0
' End If
' Dbcocx1.devgrdisplay
    
  mnuInsert.Enabled = VisibleDataLayers()
  mnuZoom.Enabled = mnuInsert.Enabled
  Exit Sub
  
OpenGrDatabase_error:
  MsgBox Error & " action:" & action

End Sub
Public Function SelectGraphicDatabase(dbname As String, ext As String) As Boolean
  SelectGraphicDatabase = False
  
  DBFileTitle = SplitPath(dbname, SP_TITLE) & ".dbf"
  If (".gid") = Right(DBFileTitle, 4) Then
    DBFileTitle = Left(DBFileTitle, Len(DBFileTitle) - 4) & ".dbf"
  End If
  
  DBTableName = dbname ' & ".dbf"
  DBFileDir = SplitPath(dbname, SP_DIR)
  
  If ("." & ext) = Right(DBTableName, 4) Then
    DBTableName = Left(DBTableName, Len(DBTableName) - 4)
  End If
  
  SelectGraphicDatabase = True
End Function



Private Sub CreateGraphicDatabase(dbname As String, ext As String)
Dim id As String
    
  Dbcocx1.devsetdbdrv "dbcdbf32.dll", "", 0
  Dbcocx1.devgrclear 0
  Dbcocx1.devgrcreateext dbname, 25
  
  Dbcocx1.devgrcmdblock "FancyPoint"
  id = Dbcocx1.devgetid()
  Dbcocx1.devgrcmdline id, 0, 0, 3, 3
  Dbcocx1.devgrcmdline id, 0, 0, 3, -3
  Dbcocx1.devgrcmdline id, 0, 0, -3, -3
  Dbcocx1.devgrcmdline id, 0, 0, -3, 3
  Dbcocx1.devgrcmdcircle id, 0, 0, 3
  Dbcocx1.devgrcmdendblock
  
  Dbcocx1.devgrcmdlayerdef TUSER, 7, 1
' dbcocx1.devgrcmdlayerdef TPICK, 7, 1
  
  Dbcocx1.devcloseall
End Sub


Public Function GetData(id As String, var, v1 As Double, Optional v2 As Double = 0#, Optional v3 As Double = 0#) As Integer
  GetData = FAILURE
  SelectDbgr 0, True
  Select Case var
    Case GET_AREA
      Dbcocx1.devseek "E" & id
      If 0 <> Dbcocx1.devfound() Then
        v1 = Val(Dbcocx1.devgetfield("VY2"))
        GetData = SUCCESS
      End If
    Case GET_LENGTH
      Dbcocx1.devseek "E" & id
      If 0 <> Dbcocx1.devfound() Then
        v1 = Val(Dbcocx1.devgetfield("VY2"))
        GetData = SUCCESS
      End If
    Case GET_THICK
      Dbcocx1.devseek "V" & id
      If 0 <> Dbcocx1.devfound() Then
        v1 = Val(Dbcocx1.devgetfield("VY2"))
        GetData = SUCCESS
      End If
    Case GET_CENTROID
      Dbcocx1.devseek "V" & id
      If 0 <> Dbcocx1.devfound() Then
        v1 = Val(Dbcocx1.devgetfield("VX1"))
        v2 = Val(Dbcocx1.devgetfield("VY1"))
        v3 = Val(Dbcocx1.devgetfield("VX2"))
        GetData = SUCCESS
      End If
  End Select
' CloseDbgr
End Function
Public Sub FormLoad()
Dim i As Long
  Dim hMenu As Long
  Dim menuItemCount As Long

  On Error GoTo form_load_error
  
' Obtain the handle to the form's system menu
  hMenu = GetSystemMenu(Me.hwnd, 0)
  If hMenu Then
   'Obtain the number of items in the menu
    menuItemCount = GetMenuItemCount(hMenu)
  
   'Remove the system menu Close menu item.
   'The menu item is 0-based, so the last
   'item on the menu is menuItemCount - 1
    Call RemoveMenu(hMenu, menuItemCount - 1, MF_REMOVE Or MF_BYPOSITION)
 
   'Remove the system menu separator line
    Call RemoveMenu(hMenu, menuItemCount - 2, MF_REMOVE Or MF_BYPOSITION)
  
   'Force a redraw of the menu. This
   'refreshes the titlebar, dimming the X
    Call DrawMenuBar(Me.hwnd)
  End If

  
  If OpMode = SERVER Then
'   SSFrame1.visible = True
'   Text1 = GisObj.msgText
  Else
'   SSFrame1.visible = False
  End If
  DbcSizeX = Me.Width - Dbcocx1.Width
  DbcSizeY = Me.Height - Dbcocx1.Height
  TvwSizeX = Me.Width - TreeView1.Width
  TvwSizeY = Me.Height - TreeView1.Height
  
' SetDbControl Dbcocx1
  Dbcocx1.devsetautoregen 1 ' autoregen = true
  Dbcocx1.devsetaperture 6 ' for making easy to catch a block with the mouse
' Dbcocx1.devdisplayxyext 1000, 1000, 1 ' raster background
' Dbcocx1.devcalibrate 0, 999, 0, 0, 999, 0, 1000, 1000

  TreeView1.ImageList = ImageList1

  ReDim layers(0)
  ReDim module(0)
  
  frmConfig2.InitDefaults ' ReDim defaults(0)
  
  load_convert
  LoadGid
  OpenGrDatabase
  
  For i = 1 To UBound(module)
    module(i).UpdateTreeviewStatus
  Next i
  
  DisplayBackground
  
  DisableUI False
  
  SelectDbgr 0, True
  
  DoEvents  ' do i need a pause??????
  
  Exit Sub
form_load_error:
  MsgBox Error
  mnuExit_Click
  
End Sub

Private Sub SSPanel3_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
  If Button = 1 Then
    MousePointer = vbCustom ' vbSizeWE
  End If
End Sub

Private Sub SSPanel3_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
 Dim zz As POINTAPI 'Declare variable
  frmMain.MousePointer = vbCustom
  If Button = 1 And Not moving Then
    GetCursorPos zz
    moving = True
    SSPanel3.Move ((zz.x * Screen.TwipsPerPixelX) - frmMain.Left)
    moving = False
  End If
End Sub

Private Sub SSPanel3_MouseUp(Button As Integer, Shift As Integer, x As Single, y As Single)
  frmMain.MousePointer = vbDefault
  TreeView1.Width = SSPanel3.Left
  Dbcocx1.Left = (TreeView1.Width + SSPanel3.Width) + 2 * Screen.TwipsPerPixelX
  Dbcocx1.Width = frmMain.ScaleWidth - (TreeView1.Width + SSPanel3.Width + 2 * Screen.TwipsPerPixelX)
  
End Sub


Private Sub ConfigureCoverages()
Dim l As Long

  If ConfigureWhat = CONF_DATALAYERS Then Exit Sub

  ConfigureWhat = CONF_DATALAYERS
  
  frmConfig2.Show 1
  
  If ReDisplay Then DisplayBackground
  
  SelectDbgr 0, True
  If 0 < UBound(layers) Then
    cboLayer.Clear
    For l = 1 To UBound(layers)
      If Not layers(l).bkgd And layers(l).visible Then
        cboLayer.AddItem layers(l).Name & "  (" & layers(l).objtype & ")"
        cboLayer.ItemData(cboLayer.NewIndex) = l
      End If
    Next l
    If cboLayer.ListCount > 0 Then cboLayer.ListIndex = 0
    Dbcocx1.devgrdisplay
  End If
  
  mnuInsert.Enabled = VisibleDataLayers()
  mnuZoom.Enabled = mnuInsert.Enabled

  ConfigureWhat = -1
End Sub

Private Sub Toolbar1_ButtonClick(ByVal Button As ComctlLib.Button)
  If Not enableState Then Exit Sub
  DisplayStatus ""
  If 0 = UBound(layers) Then
    mnuCoverages_Click
    If 0 = UBound(layers) Then Exit Sub
  End If
  Select Case Button.key
    Case TPOINT:
      mnuInsPoint_Click
    Case TLINE:
      mnuInsLine_Click
    Case TPOLYGON:
      mnuInsPolygon_Click
    Case TCIRCLE:
      mnuInsCircle_Click
    Case TRECTANGLE:
      mnuInsRectangle_Click
    Case TPGRID:
      mnuInsPGrid_Click
    Case TCGRID:
      mnuInsCGrid_Click
    Case TPTGRID:
      mnuInsPtGrid_Click
  End Select
End Sub

Private Sub Toolbar2_ButtonClick(ByVal Button As ComctlLib.Button)
  If Not enableState Then Exit Sub
  DisplayStatus ""
  If 0 = UBound(layers) Then
    mnuCoverages_Click
    If 0 = UBound(layers) Then Exit Sub
  End If
  Select Case Button.key
    Case "zoomin":
      ZoomIn
'     Dbcocx1.devzoom 1
    Case "zoomout":
      ZoomOut
'     Dbcocx1.devzoom -1
    Case "zoomarea":
      ZoomArea
'     Dbcocx1.devzoom 2
    Case "zoomprev":
      ZoomPrevious
'     Dbcocx1.devzoom -2
    Case "zoomall":
      ZoomAll
'     Dbcocx1.devzoom 0
    Case "zoomreset":
      ZoomReset
'     ZoomOn frmMain.XCentr, frmMain.YCentr
    Case "overview":
      ZoomOverview
'     Dbcocx1.devoverview
  End Select
End Sub

Private Sub TreeView1_DblClick()
Dim nod As Node, i As Long
  If IgnoreEvents Then Exit Sub
  Set nod = TreeView1.SelectedItem
  If nod.Parent Is Nothing Then Exit Sub
  
  If 0 = UBound(layers) Then
    mnuCoverages_Click
    If 0 = UBound(layers) Then Exit Sub
  End If
  For i = 1 To UBound(module)
    If module(i).id = nod.Parent.key Or module(i).id = Left(nod.Parent.key, Len(module(i).id)) Then
'     module(i).GetData nod.key, layers(cboLayer.ItemData(cboLayer.ListIndex)).objtype
      module(i).GetData nod.key, ""
      module(i).UpdateTreeviewStatus
    End If
  Next i
End Sub

Private Sub TreeView1_MouseDown(Button As Integer, Shift As Integer, x As Single, y As Single)
Dim nod As Node, i As Long
  If IgnoreEvents Then Exit Sub
  Set nod = TreeView1.SelectedItem
  If nod.Children > 0 Then Exit Sub
  If Button = vbRightButton Then
    For i = 1 To UBound(module)
      If module(i).id = Left(nod.Parent.key, Len(module(i).id)) Then
        module(i).DisplayTreeviewMenu nod.key
        module(i).UpdateTreeviewStatus
        Exit For
      End If
    Next i
  End If
End Sub

Private Sub TreeView1_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
  frmMain.MousePointer = vbDefault
End Sub

Private Sub TreeView1_NodeClick(ByVal Node As ComctlLib.Node)
Dim i As Long
  If IgnoreEvents Then Exit Sub
  If Node.Parent Is Nothing Then
    RefreshGrDisplay
  Else
    If 0 = UBound(layers) Then
      mnuCoverages_Click
      If 0 = UBound(layers) Then Exit Sub
    End If
    For i = 1 To UBound(module)
      If module(i).id = Node.Parent.key Or module(i).id = Left(Node.Parent.key, Len(module(i).id)) Then
        module(i).DisplayData (Node.key)
        Exit For
      End If
    Next i
  End If
End Sub

Private Function UpdateTreeviewStatus() As Boolean
Dim i As Long, complete As Boolean
  complete = True
  For i = 1 To UBound(module)
    complete = complete And module(i).UpdateTreeviewStatus()
  Next i
  UpdateTreeviewStatus = complete
End Function

Public Function DevgrseekV2(x As Double, y As Double, otype As String) As Boolean
Dim l As Long, found As Boolean, qry As New QryClass, recno As Long
  
  MousePointer = vbHourglass
  SelectDbgr 0, True
  Dbcocx1.Devgrseek x, y, 0
  If 0 <> Dbcocx1.devfound() Then
    SelectedLayer = 0
    found = True
  End If
  If Not found Then
    For l = 1 To UBound(layers)
      If layers(l).visible And layers(l).pickable Then
        SelectDbgr l, True
        recno = 0
        Dbcocx1.Devgrseek x, y, recno
        Do While 0 <> Dbcocx1.devfound()
          If qry.SeekTest(Dbcocx1, otype) Then
            SelectedLayer = l
            found = True
            Exit Do
          End If
          recno = Dbcocx1.devrecno()
          Dbcocx1.Devgrseek x, y, recno
        Loop
      End If
      If found Then Exit For
    Next l
  End If
  DevgrseekV2 = found
  MousePointer = vbDefault

End Function

Public Function Devgrseek(x As Double, y As Double) As Boolean
Dim l As Long, found As Boolean
  MousePointer = vbHourglass
  SelectDbgr 0, True
  Dbcocx1.Devgrseek x, y, 0
  If 0 <> Dbcocx1.devfound() Then
    SelectedLayer = 0
    found = True
  End If
  If Not found Then
    For l = 1 To UBound(layers)
      If layers(l).visible And layers(l).pickable Then
        SelectDbgr l, True
        Dbcocx1.Devgrseek x, y, 0
        If 0 <> Dbcocx1.devfound() Then
          SelectedLayer = l
          found = True
          Exit For
        End If
      End If
    Next l
  End If
  Devgrseek = found
  MousePointer = vbDefault
End Function

Public Sub ZoomIn()
  Dbcocx1.devzoom 1
End Sub

Public Sub ZoomOut()
  Dbcocx1.devzoom -1
End Sub

Public Sub ZoomArea()
  Dbcocx1.devzoom 2
End Sub

Public Sub ZoomPrevious()
  Dbcocx1.devzoom -2
End Sub

Public Sub ZoomReset()
  ZoomOn XCentr, YCentr
End Sub

Public Sub ZoomAll()
  Dbcocx1.devzoom 0
End Sub

Public Sub ZoomOverview()
  Dbcocx1.devoverview
End Sub

Public Sub ShowState()
  StatusBar1.Panels(4).text = "ES: " & enableState
  StatusBar1.Panels(5).text = "IE: " & IgnoreEvents
End Sub

Public Sub ViewChange()
   Dbcocx1.devgetview ViewX0, ViewY0, ViewX1, ViewY1, ViewZf, ViewRf
   XCentr = ViewX0 + 0.5 * (ViewX1 - ViewX0)
   YCentr = ViewY0 + 0.5 * (ViewY1 - ViewY0)
End Sub
