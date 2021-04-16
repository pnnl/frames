VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{3B7C8863-D78F-101B-B9B5-04021C009402}#1.2#0"; "RICHTX32.OCX"
Begin VB.Form frmMain 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Import GID to Frames V2"
   ClientHeight    =   3810
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   8775
   Icon            =   "frmMain.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   3810
   ScaleWidth      =   8775
   StartUpPosition =   2  'CenterScreen
   Begin VB.ListBox List1 
      Height          =   255
      Left            =   2415
      TabIndex        =   4
      Top             =   75
      Visible         =   0   'False
      Width           =   4590
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Export Summary"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   720
      Index           =   2
      Left            =   7080
      TabIndex        =   3
      Top             =   2250
      Width           =   1320
   End
   Begin RichTextLib.RichTextBox RichTextBox1 
      Height          =   2745
      Left            =   255
      TabIndex        =   2
      Top             =   645
      Width           =   6630
      _ExtentX        =   11695
      _ExtentY        =   4842
      _Version        =   393217
      Enabled         =   -1  'True
      ReadOnly        =   -1  'True
      ScrollBars      =   2
      TextRTF         =   $"frmMain.frx":030A
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Create SIM"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   510
      Index           =   1
      Left            =   7095
      TabIndex        =   1
      Top             =   1605
      Width           =   1320
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   1560
      Top             =   30
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Open GID"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   510
      Index           =   0
      Left            =   7080
      TabIndex        =   0
      Top             =   960
      Width           =   1320
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      AutoSize        =   -1  'True
      Caption         =   "Summary"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.75
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   240
      Left            =   300
      TabIndex        =   5
      Top             =   345
      Width           =   6600
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Compare Text
Option Explicit
Public hLibModule As Long
Public fSuppress As Boolean


Sub ErrorCheck()
' Look for registered des file
' for each variable in GID
'   lookup variables in UI dictionary
'   compare dimensions
'   compare type
'   compare unit
'
'   display all errors to text box
'
  Dim i As Long, j As Long
  Dim modct As Long
  Dim rc As Integer
  Dim pfile As parmfile
  Dim scsv As Variant
  Dim tStr As String
  Dim found As Boolean
  Dim obj As Variant
  Dim dic As String
  Dim vdim As Long
  Dim nwrn As Long
  Dim nvar As Long
  
  List1.Clear
  
  If NumSites = 0 Then Exit Sub
  If Sites(0).NumGlyphs = 0 Then Exit Sub
  Dim modid As String
  
  If DisplayError(ModuleList(PID, "|", tStr), "ModuleList", True) Then Exit Sub
  scsv = Split(tStr, "|")
  Set colModules = New Collection
  For i = 0 To UBound(scsv)
    colModules.Add CStr(scsv(i))
  Next i
  ' ""
  For i = 0 To Sites(0).NumGlyphs - 1
    curGlyph = Sites(0).Glyphs(i)
    With Sites(0).Glyphs(i)
      AddText ""
      AddText "Name: " & .Name
      AddText "Label: " & .label
      AddText "Model 1.X: " & .model

       model = .model
    End With
    found = False
    
    If model <> "" Then
      For Each obj In colModules
        If CStr(obj) = model Then
          found = True
          Exit For
        End If
      Next obj
      If Not found Then
        frmModel.Show vbModal
      Else
        
      End If
      
      If model <> "" Then
        AddText "Model 2.0: " & model
        Sites(0).Glyphs(i).model = model
        If Not DisplayError(GetModString(PID, model, "Dictionary", "", SetIdx(), dic), "GetModString", False) Then
            AddText "Dictionary: " & dic
            rc = LoadGIDData(pfile, cdl.filename, Sites(0).Glyphs(i).Name)
            
            ' get unique list of variables
            If UBound(parmlist) > 0 Then
              ReduceVariables
            End If
            AddText "Warnings:"
            nwrn = 0
            nvar = 0
            For j = 1 To UBound(colVars)
              nvar = nvar + 1
              If Not DisplayError(GetVarDimension(PID, dic, colVars(j).pname, vdim), "GetVarDimension", False) Then
                If vdim < colVars(j).dim And "con" = Left(curGlyph.Name, 3) Then
                   DisplayError GetVarDimension(PID, dic, "DK" & colVars(j).pname, vdim), "GetVarDimension", False
                End If
                If vdim < colVars(j).dim Then
                  nwrn = nwrn + 1
                  AddText vbTab & colVars(j).pname & " dimension: GID=" & colVars(j).dim & " Dictionary=" & vdim
                End If
                If Not DisplayError(GetVarType(PID, dic, colVars(j).pname, tStr), "GetVarType", False) Then
                  ' nwrn = nwrn + 1
                Else
                End If
              Else
                nwrn = nwrn + 1
                AddText vbTab & colVars(j).pname & " - Variable Not Found in Dictionary: "
              End If
            Next j
            If nwrn = 0 Then AddText vbTab & "No warnings"
            If nvar = 0 Then AddText vbTab & "No variables"
        Else
          AddText "Dictionary: UNDEFINED"
        End If
      Else
        AddText "Model 2.0: NOT FOUND"
      End If
    End If
    
  Next

End Sub

Private Sub Command1_Click(Index As Integer)
  Dim fName As String
  Select Case Index
    Case 0:
      rtb.Text = ""
      If OpenSites(LCase(GetFileName(GID_OPEN)), False) Then
        AddText "Summary of Import GID"
        AddText ""
        AddText "File: " & cdl.filename
        ErrorCheck
      Else
      End If
    Case 1:
      If Not GetSimulationFilename(False, SimPath, SimSet) Then Exit Sub
      If DisplayError(NewSim(PID, SimPath, SimSet), "_NewSim", True) = PASS Then
        fName = Dir$(SimPath & "\" & SimSet & ".*", vbNormal)   ' Retrieve the first entry.
        If fName <> BLANK And fName <> "." And fName <> ".." Then
          Kill SimPath & "\" & SimSet & ".*"
        End If
        AddModuleIcons
        DisplayError SaveSim(PID), "_SaveSim", True
        DisplayError CloseSim(PID), "_CloseSIM", True
      End If
    Case 2:
      Dim flags As Long
      flags = PD_NOPAGENUMS + PD_NOSELECTION
      If ShowPrinter(Me, flags) Then
      
        If flags And PD_PRINTTOFILE Then
         On Error Resume Next
         'If CommonDialog1.InitDir = "" Then CommonDialog1.InitDir = SplitPath(AppPath, SP_DIR)
         CommonDialog1.DialogTitle = "Print To File"
         If CommonDialog1.filename = "" Then CommonDialog1.filename = "*.rtf"
         CommonDialog1.Filter = "Rich Text Files (*.rtf)|*.rtf ' |Text Files (*.txt)|*.txt"
         CommonDialog1.FilterIndex = 0
         CommonDialog1.DefaultExt = "rtf"
         CommonDialog1.flags = cdlOFNOverwritePrompt Or cdlOFNHideReadOnly
         CommonDialog1.flags = CommonDialog1.flags Or cdlOFNExtensionDifferent Or cdlOFNNoChangeDir
         CommonDialog1.CancelError = True
         On Error Resume Next
         CommonDialog1.ShowOpen
         If Not Err = cdlCancel Then
            RichTextBox1.SaveFile CommonDialog1.filename, 0  ' & ".rtf", 0
         End If
        Else
          PrintRTF RichTextBox1, 1440, 1440, 1440, 1440
        End If
      End If
'     ShellAssocExec x.txt
  End Select
End Sub

Private Sub Form_load()
  Dim SiteIndex As Integer
  Dim errfile As csv
  Dim RunName As String
  Dim FUIName As String
  Dim strFileName As String
  Dim lngCount As Long
  Dim lStatus As Long
  Dim stPath As String

  FRAMES_INI = App.path + "\\FramesUI.ini"
  
  fSuppress = False
  GetCommandLine
  If argc > 1 Then
    MsgBox "Too many arguments.  A simulation must not be open."
    End
  End If
  AppPath = argv(1)
  strFileName = String(255, 0)
  lngCount = GetModuleFileName(App.hInstance, strFileName, 255)
  strFileName = Left(strFileName, lngCount)
  
  If UCase(Right(strFileName, 7)) = "VB6.EXE" Then
      strFileName = AppPath
      strFileName = Replace(strFileName, Chr$(34), "") ' remove quotes
      ChDir strFileName
      hLibModule = LoadLibrary(strFileName & "\systemio.dll")
'''      PID = OpenINI(AppPath)
'''  Else
''''      PID = OpenIO(argv(argc - 2), argv(argc - 1), argv(argc))
'''      PID = OpenINI(AppPath)
  End If
  PID = OpenINI(AppPath)
  If PID <= 0 Then
      MsgBox "Invalid PID: " & PID & " " & AppPath
      End
  End If
  
  Set cdl = CommonDialog1
  Set lst = List1
  Set rtb = RichTextBox1
End Sub

Private Sub Form_Unload(Cancel As Integer)
    On Error Resume Next
   
    Close
    If 0 <> hLibModule Then
      CloseINI PID, 0
      FreeLibrary hLibModule
    Else
      CloseIO PID, 0
    End If

End Sub

Private Sub AddModuleIcons()
  Dim i As Long
  Dim j As Long
  Dim id As String
  Dim posX As Long, posY As Long
  Dim ix As tindex
  Dim icon As IconCls
  Set ModIcon = New Collection
  Dim group As String
  ActiveScope = 0
      
  For i = 0 To Sites(0).NumGlyphs - 1
    FindDropPosition posX, posY
    
    posX = (i Mod 5) * ICON_XY
    posY = i * ICON_XY
    ix.i1 = 0
    
    With Sites(0).Glyphs(i)
      If 0 = InStr(.Name, "sen") Then
        If InStr(.Name, "con") Then
          ActiveScope = 0
          group = "Database"
        Else
          ActiveScope = 1
          group = "Model"
        End If
        If DisplayError(AddIcon(PID, ActiveScope, "FRAMES", group, "", "", id), "_AddIcon", True) = PASS Then
          DisplayError VarLookUp(PID, SimSet, "ModId", id, ix), "_VarLookUp"
          WriteString1 PID, SimSet, "ModLabel", BLANK, ix.i1, .label
          WriteInt1 PID, SimSet, "ModPosX", BLANK, ix.i1, CLng(.sx * 0.8) 'CLng(posX)
          WriteInt1 PID, SimSet, "ModPosY", BLANK, ix.i1, CLng(.sy * 0.8) ' CLng(posY)
          If (.model <> "") Then
            WriteString1 PID, SimSet, "ModName", BLANK, ix.i1, .model
          End If
          .SIMid = id
           Set icon = New IconCls
           icon.posX = posX
           icon.posY = posY
          ' icon.scope = ActiveScope
           ModIcon.Add icon
        End If
      End If
    End With
  Next i

  For i = 0 To Sites(0).NumGlyphs - 1
    If (0 = InStr(Sites(0).Glyphs(i).Name, "sen")) Then
      For j = 0 To Sites(0).NumGlyphs - 1
        If Sites(0).Connect(i, j) And Sites(0).SrcSnk(i, j) = OSNK Then
          AddLink PID, Sites(0).Glyphs(i).SIMid, Sites(0).Glyphs(j).SIMid
        End If
      Next j
    End If
  Next i
End Sub
Public Function GetSimulationFilename(mustexist As Boolean, fpath As String, fName As String) As Boolean
  Dim Ok As Boolean
  
  GetSimulationFilename = False
  
  cdl.DialogTitle = "Open"
  cdl.Filter = "Simulation File (*.sim)|*.sim"
  cdl.FilterIndex = 1
  cdl.DefaultExt = "sim"
  cdl.CancelError = True
  
  On Error Resume Next
  Do While Not Ok
    cdl.flags = cdlOFNHideReadOnly Or cdlOFNExtensionDifferent
    If mustexist Then cdl.flags = cdl.flags Or cdlOFNFileMustExist
    cdl.filename = "*.sim"
    cdl.ShowOpen
    If Err <> 0 Then Exit Function
    If (cdl.flags And cdlOFNExtensionDifferent) Then
      MsgBox "Specified file name (" & cdl.FileTitle & ") is invalid" & Chr(10) & "-- the file extension must be SIM.", vbExclamation
    Else
      Ok = True
    End If
  Loop
  fName = cdl.FileTitle
  fpath = Left(cdl.filename, InStr(cdl.filename, fName) - 2)
  If "sim" <> Right(fName, 3) Then fName = fName & ".sim"
  GetSimulationFilename = True
End Function

Sub FindDropPosition(ByRef sx As Long, ByRef sy As Long)
Dim maxy As Single, used As Boolean
Dim xCt As Long, yCt As Long, mcls As IconCls
Dim x1 As Long, y1 As Long
Dim x As Long, y As Long, x2 As Long, y2 As Long

'  xCt = picSite.Width / (ICON_XY * Screen.TwipsPerPixelX)
'  yCt = picSite.Height / (ICON_XY * Screen.TwipsPerPixelY)

  xCt = 50000 / (ICON_XY * Screen.TwipsPerPixelX)
  yCt = 50000 / (ICON_XY * Screen.TwipsPerPixelY)
  
  For x = 1 To xCt
    x1 = (x - 1) * ICON_XY:    x2 = x * ICON_XY
    For y = 1 To yCt
      used = False
      y1 = (y - 1) * ICON_XY:      y2 = y * ICON_XY
      For Each mcls In ModIcon
        If mcls.scope = ActiveScope Then
          used = ((mcls.posX >= x1 And mcls.posX <= x2) And (mcls.posY >= y1 And mcls.posY <= y2))
          If used Then Exit For
        End If
      Next mcls
      If Not used Then Exit For
    Next y
    If Not used Then Exit For
  Next x
  sx = x1 + ICON_XY ' 150 ' 5
  sy = y1 + ICON_XY ' 125 ' 5
End Sub

