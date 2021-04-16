VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Object = "{B02F3647-766B-11CE-AF28-C3A2FBE76A13}#2.5#0"; "ss32x25.ocx"
Begin VB.Form SCFImport 
   Caption         =   "WES SCF Spreadsheet Import"
   ClientHeight    =   5088
   ClientLeft      =   132
   ClientTop       =   708
   ClientWidth     =   7296
   ControlBox      =   0   'False
   Icon            =   "SCFImport.frx":0000
   LinkTopic       =   "SCFImport"
   ScaleHeight     =   5088
   ScaleWidth      =   7296
   StartUpPosition =   3  'Windows Default
   Begin VB.ComboBox cboDtype 
      Height          =   288
      ItemData        =   "SCFImport.frx":0442
      Left            =   2160
      List            =   "SCFImport.frx":045B
      Style           =   2  'Dropdown List
      TabIndex        =   8
      Top             =   1560
      Width           =   4920
   End
   Begin VB.ComboBox cboDataset 
      Height          =   288
      Left            =   2148
      Style           =   2  'Dropdown List
      TabIndex        =   6
      Top             =   1068
      Width           =   4920
   End
   Begin VB.TextBox Text1 
      Height          =   315
      Left            =   2148
      Locked          =   -1  'True
      TabIndex        =   5
      Text            =   "Text1"
      Top             =   60
      Width           =   3615
   End
   Begin FPSpreadADO.fpSpread vaSpread1 
      Height          =   2880
      Left            =   120
      TabIndex        =   4
      Top             =   2040
      Visible         =   0   'False
      Width           =   7020
      _Version        =   131077
      _ExtentX        =   12382
      _ExtentY        =   5080
      _StockProps     =   64
      BackColorStyle  =   1
      ColHeaderDisplay=   0
      DisplayRowHeaders=   0   'False
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      GridSolid       =   0   'False
      ScrollBars      =   2
      SelectBlockOptions=   0
      ShadowColor     =   12632064
      SpreadDesigner  =   "SCFImport.frx":04D5
   End
   Begin VB.ComboBox Combo1 
      Height          =   288
      Left            =   2148
      Style           =   2  'Dropdown List
      TabIndex        =   1
      Top             =   576
      Width           =   4932
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   5880
      Top             =   480
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Browse..."
      Height          =   375
      Left            =   5916
      TabIndex        =   0
      Top             =   48
      Width           =   1095
   End
   Begin VB.Label Label4 
      Alignment       =   1  'Right Justify
      AutoSize        =   -1  'True
      Caption         =   "Use"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   192
      Left            =   1632
      TabIndex        =   9
      Top             =   1572
      Width           =   348
   End
   Begin VB.Label Label3 
      Alignment       =   1  'Right Justify
      AutoSize        =   -1  'True
      Caption         =   "Site Location:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   192
      Left            =   828
      TabIndex        =   7
      Top             =   1080
      Width           =   1140
   End
   Begin VB.Label Label2 
      Alignment       =   1  'Right Justify
      AutoSize        =   -1  'True
      Caption         =   "Site (worksheet):"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   192
      Left            =   636
      TabIndex        =   3
      Top             =   600
      Width           =   1392
   End
   Begin VB.Label Label1 
      Alignment       =   1  'Right Justify
      AutoSize        =   -1  'True
      Caption         =   "Spreadsheet Filename:"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   192
      Left            =   120
      TabIndex        =   2
      Top             =   120
      Width           =   1944
   End
   Begin VB.Menu mnuFile 
      Caption         =   "&File"
      Begin VB.Menu mnuSave 
         Caption         =   "&Save and Exit"
      End
      Begin VB.Menu mnuExit 
         Caption         =   "E&xit"
      End
   End
   Begin VB.Menu hlp 
      Caption         =   "&Help"
      Begin VB.Menu howto 
         Caption         =   "How to..."
      End
      Begin VB.Menu about 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "SCFImport"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim wbo As Object
Dim fileOK As Boolean
Dim loadng As Boolean
Dim mode As Long
Dim sitename As String
Dim temp As parmrec

Private Type cont_type
  sel As Boolean
  name As String
  cas As String
  start As Date
End Type

Private Type ds_type
  name As String
  ncon As Integer
  cont() As cont_type
End Type

Private Type gid_type
  fname As String
  wsname As String
  dtype As Long
  numds As Long
  ds() As ds_type
  dataset As Long
  consumer As String
End Type

Private Type fui_type
  ncon As Long
  cont() As cont_type
End Type

Dim ds() As ds_type
Dim gidData As gid_type
Dim fuiData As fui_type
Dim des As csv                 'des file declaration

Private Sub GetArguments()
  Dim args As String
  Dim pos As Long
    
  argc = 0
  args = Trim$(Command$)
  If Len(args) > 0 Then
    Do
      pos = InStr(args, " ")
      argc = argc + 1
      If pos > 0 Then
        ReDim Preserve argv(argc) As String
        argv(argc - 1) = Trim$(Left$(args, pos))
        args = Trim$(Right$(args, Len(args) - pos))
      Else
        ReDim Preserve argv(argc)
        argv(argc - 1) = Trim$(args)
      End If
    Loop Until pos = 0
  End If
End Sub

Private Sub about_Click()
  frmAbout.picIcon = frmAbout.ImageList1.ListImages(1).Picture
  frmAbout.Show 1, SCFImport
End Sub

Private Sub cboDataset_Click()
  SpreadsheetLoad cboDataset.ListIndex + 1
End Sub

Private Sub cboDtype_Change()
  gidData.dtype = cboDtype.ListIndex
End Sub

Private Sub combo1_click()
  Dim nds As Long
  If Combo1.ListIndex < 0 Then Exit Sub
  cboDataset.Clear
  vaSpread1.Visible = False
  DatasetLoad Combo1.List(Combo1.ListIndex)
  For nds = 1 To UBound(ds)
    cboDataset.AddItem ds(nds).name
  Next nds
  If (cboDataset.ListCount > 0) Then
    cboDataset.ListIndex = gidData.dataset - 1
    cboDtype.ListIndex = gidData.dtype
  Else
    cboDtype.ListIndex = -1
  End If
  mnuSave.Enabled = cboDataset.ListCount > 0
End Sub

Private Sub Combo2_Click()
'just cause
End Sub

Private Sub Command1_Click()
 Dim wb As Integer
 Dim ws As Integer
    
    ' Set CancelError is True
    CommonDialog1.CancelError = True
    On Error GoTo errhandler
    
    CommonDialog1.filename = Text1
    ' Set flags
    CommonDialog1.Flags = cdlOFNHideReadOnly
    ' Set filters
    CommonDialog1.Filter = "Excel Files (*.xls)|*.xls"
    ' Specify default filter
    CommonDialog1.FilterIndex = 2
    ' Display the Open dialog box
    CommonDialog1.ShowOpen
    ' Display name of selected file

    Text1 = CommonDialog1.filename
    Combo1.Clear
    cboDataset.Clear
    
    If Not wbo Is Nothing Then
      On Error Resume Next
      Set wbo = GetObject(, "excel.application")
      If Err <> 0 Then
        On Error GoTo errhandler
        Set wbo = CreateObject("excel.application")
      End If
    Else
      wbo.ActiveWorkbook.Close
    End If
    
    wbo.workbooks.Open filename:=CommonDialog1.filename
    With wbo.ActiveWorkbook
      For ws = 1 To .Worksheets.Count
        If QuickTest(ws) Then
          Combo1.AddItem .Worksheets(ws).name
        End If
      Next ws
    End With
    
    If Combo1.ListIndex = 0 Then
      combo1_click
    Else
      Combo1.ListIndex = 0
    End If
      

    Exit Sub
    
errhandler:
  MsgBox Err.Description
'    User pressed the Cancel button
'    MsgBox Error
End Sub


Private Sub Form_load()
Dim cnt As Long
  Dim i As Integer, wb As Integer, ws As Integer
  
  loadng = True
  
  Left = 0.5 * (Screen.Width - Width)
  Top = 0.5 * (Screen.Height - Height)
  fileOK = False
  
  StartModule SCFImport, "SCF Soil Spreadsheet Import", 6
  mode = Val(argv(0))
  SetHelpFile App.Path + "\SCFImport.html"
  loadprm
  
  On Error Resume Next
  Set wbo = GetObject(, "excel.application")
  If Err <> 0 Then
    On Error GoTo errhandler
    Set wbo = CreateObject("excel.application")
  End If
  
  ' confirm file path
  If 0 < Len(gidData.fname) Then
    fileOK = (0 < Len(Dir(gidData.fname)))
  End If

  Select Case mode
    Case 0: ' input
      If fileOK Then
        i = -1
        Text1 = gidData.fname
        Combo1.Clear
        wbo.workbooks.Open filename:=gidData.fname
        With wbo.ActiveWorkbook
          For ws = 1 To .Worksheets.Count
            If QuickTest(ws) Then
              Combo1.AddItem .Worksheets(ws).name
              If Combo1.List(Combo1.NewIndex) = gidData.wsname Then i = ws
            End If
          Next ws
        End With
        Combo1.ListIndex = i - 1
        If cboDataset.ListCount > 0 Then
          cboDataset.ListIndex = gidData.dataset - 1
        End If
        
      Else
        Text1 = ""
        Combo1.Clear
      End If
    Case 1: ' output
      If Not fileOK Then
        PutError "File not found: " & gidData.fname
      Else
        Make_SCF
      End If
      EndModule
  End Select
  loadng = False
  Refresh
  Exit Sub
errhandler:
  End
End Sub

Private Sub Form_Unload(Cancel As Integer)
 On Error Resume Next
 wbo.ActiveWorkbook.Close
 Set wbo = Nothing
 Close
End Sub

Function xlRange(col1, row1) As String
Dim s As String, n1 As Integer, n2 As Integer
Dim alpha As String
  alpha = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
  n1 = col1 Mod 26
  n2 = Int(col1 / 26)
  If n2 > 0 Then s = Mid$(alpha, n2, 1)
  s = s & Mid$(alpha, n1, 1)
  s = s & row1
  xlRange = s
End Function

Private Function Make_SCF()
  Dim i As Long, j As Long, k As Long
  Dim l As Long, m As Long, n As Long
  Dim connum As Long
  Dim file As csv
  Dim f_numcon As Integer
  Dim date1 As Date, date2 As Date
  Dim nr As Integer, nc As Integer
  Dim sel As Boolean
  Dim numds As Long, nd As Long
  Dim str As String
  Dim cname As String, casid As String, nprog As Integer, nyear As Integer
  Dim np As Integer
  
  On Error GoTo errhandler
  
  wbo.workbooks.Open filename:=gidData.fname
    
  If open_csv(file, RunName & ".SCF", 1) Then
    nr = PutHeader(file)
    'sets the index to the soil concentration dataset
    i = 4
    
  With wbo.ActiveWorkbook.Worksheets(gidData.wsname)
    nr = nr + 3
    put_val file, 1
    put_line file
    put_val file, gidData.consumer
    put_val file, "Soil"
    numds = .range(xlRange(2, nr))
    nr = nr + 3
    For nd = 1 To numds
      f_numcon = .range(xlRange(8, nr))
    
      ' we only get to do one dataset
      If nd = gidData.dataset Then
        ' compare cas for valid constituents
        For i = 1 To gidData.ds(nd).ncon
          gidData.ds(nd).cont(i).sel = False
          For j = 1 To fuiData.ncon
            If fuiData.cont(j).cas = gidData.ds(nd).cont(i).cas Then
              gidData.ds(nd).cont(i).sel = True
              Exit For
            End If
          Next j
        Next i
      
        nc = 0
        For i = 1 To gidData.ds(nd).ncon
          If gidData.ds(nd).cont(i).sel Then nc = nc + 1
        Next i
      
  ''    put_val file, "Source" ' .Range(xlRange(1, nr))
        For j = 2 To 7
          put_val file, .range(xlRange(j, nr))
        Next
        
        put_val file, nc ' f_numcon
        put_line file
      
        nr = nr + 2
        
        For j = 1 To f_numcon
          cname = .range(xlRange(1, nr))
          casid = .range(xlRange(2, nr))
          nprog = .range(xlRange(3, nr))
          nyear = .range(xlRange(4, nr))
          
          sel = gidData.ds(nd).cont(j).sel
          
          If sel Then
            put_val file, cname  ' constituent name
            put_sval file, casid ' casid
            put_val file, "yr"
            put_val file, .range(xlRange(7, nr))
            put_val file, nyear
            put_val file, nprog
            put_line file
          End If
          
          For k = 1 To nyear
            nr = nr + 1
            date2 = .range(xlRange(6, nr))
            If sel Then
              put_val file, Format(DateDiff("d", 0, date2) / 365.25, "Standard")
              put_val file, CStr(Val(.range(xlRange(7 + gidData.dtype, nr))))
              put_line file
            End If
          Next k
          
          For np = 1 To nprog
            nr = nr + 1
            
            ' NOTE: this has to be same as parent
            nyear = .range(xlRange(4, nr))
            
            If sel Then
              put_val file, .range(xlRange(1, nr))
              put_val file, .range(xlRange(2, nr))
              put_val file, "yr"
              put_val file, .range(xlRange(7, nr))
              put_val file, nyear
              put_val file, cname  ' constituent name
              put_sval file, casid ' casid
              put_line file
            End If
            
            For k = 1 To nyear
              nr = nr + 1
              date2 = .range(xlRange(6, nr))
              If sel Then
                put_val file, Format(DateDiff("d", 0, date2) / 365.25, "Standard")
                put_val file, CStr(Val(.range(xlRange(7 + gidData.dtype, nr))))
                put_line file
              End If
            Next k
            
          Next np
          nr = nr + 1
        Next j
        nr = nr + 2
      Else
        nr = nr + 2
        For j = 1 To f_numcon
          nprog = .range(xlRange(3, nr))
          nyear = .range(xlRange(4, nr))
          nr = nr + nyear
          For k = 1 To nprog
            nr = nr + 1
            nyear = .range(xlRange(4, nr))
            nr = nr + nyear
          Next k
          nr = nr + 1
        Next j
        nr = nr + 2
      End If

    Next nd
    
    End With
    
    close_csv file
    
  Else
    PutError "Unable to create soil concentration file" & RunName & ".SCF"
  End If
  
  wbo.ActiveWorkbook.Close
  Exit Function
errhandler:
  If Err.Number > 0 Then
    PutError "There is a problem reading spreadsheet and/or writing SCF."
    PutError Err.Description
  Else
    AnError = False
  End If
End Function
Private Function PutHeader(file As csv) As Integer
  Dim nr As Integer, i As Integer
  
  With wbo.ActiveWorkbook.Worksheets(gidData.wsname)
    nr = .range(xlRange(1, 1))
  
    put_val file, nr
    put_line file
    
    For i = 1 To nr
      put_val file, .range(xlRange(1, i + 1))
      put_line file
    Next i
  End With
  PutHeader = nr
End Function

Private Sub howto_Click()
  GetHelp
End Sub

Private Sub mnuExit_Click()
 EndModule
End Sub

Private Sub mnuFile_Click()
  mnuSave.Enabled = cboDataset.ListCount > 0
End Sub

Private Sub mnuSave_Click()
  Dim i As Long, j As Long, k As Long
  Dim l As Long, m As Long, n As Long
  Dim y As Long
  Dim Ok(4) As Boolean
  Dim fname As String
  Dim parm As parmrec
  Dim fle As parmfile
  Dim numds As Long, ncon As Long, nr As Long
  Dim var As Variant
  
  fname = RunName & ".GID"
  If open_parm(fle, fname, 1) Then
    set_parm parm, "Filename", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Text1
    write_parmrec fle, parm
    set_parm parm, "Worksheet", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", Combo1.List(Combo1.ListIndex)
    write_parmrec fle, parm
    
    set_parm parm, "Consumer", 0, 0, 0, 0, 0, 0, 0, "", "", "All"
    write_parmrec fle, parm
    set_parm parm, "Dataset", 0, 0, 0, 0, 0, 0, 0, "", "", CStr(cboDataset.ListIndex + 1)
    write_parmrec fle, parm
    
    set_parm parm, "Dtype", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(cboDtype.ListIndex)
    write_parmrec fle, parm
    
    nr = 0
    numds = UBound(ds)
    set_parm parm, "NumDS", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(numds)
    write_parmrec fle, parm
    
    For i = 1 To numds
      set_parm parm, "Location", 0, 0, 0, 0, 0, 0, 0, "", "", cboDataset.ListIndex + 1
      nr = nr + 1
      ncon = UBound(ds(i).cont)
      set_parm parm, "NumCon", i, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(ncon)
      write_parmrec fle, parm
      
      For j = 1 To ncon
        nr = nr + 1
        set_parm parm, "CasId", i, j, 0, 0, 0, 0, 0, "N/A", "N/A", ds(i).cont(j).cas
        write_parmrec fle, parm
        set_parm parm, "StartDate", i, j, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(ds(i).cont(j).start)
        write_parmrec fle, parm
      Next j
    Next i
    close_parm fle
  Else
    PutError "Unable to create transaction file" & RunName & ".GID"
  End If
  EndModule

End Sub
Private Sub loadprm()
  Dim i As Long, j As Long, k As Long, l As Long, m As Long
  Dim fle As parmfile
  Dim sval As Boolean
  Dim baddata As Boolean
  
  'f_numcon = 0
  ReDim gidData.ds(0)
  ReDim gidData.ds(0).cont(0)
  baddata = False
  
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pname
          Case "fui"
            For i = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  Select Case temp.pname
                    Case "fscname"
                      If temp.idx3 = 0 Then
                        If temp.idx2 > fuiData.ncon Then
                          fuiData.ncon = temp.idx2
                          ReDim Preserve fuiData.cont(fuiData.ncon)
                        End If
                        fuiData.cont(temp.idx2).name = temp.pval
                      End If
                    Case "fscasid"
                      If temp.idx3 = 0 Then
                        If temp.idx2 > fuiData.ncon Then
                          fuiData.ncon = temp.idx2
                          ReDim Preserve fuiData.cont(fuiData.ncon)
                        End If
                        fuiData.cont(temp.idx2).cas = temp.pval
                      End If
                   End Select
                End If
              End If
            Next
          Case ModName
            gidData.dataset = 1       ' default for backward compatibility
            gidData.consumer = "All"  ' default for backward compatibility
            For i = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If Not baddata Then
                  Select Case temp.pname
                    Case "dtype":
                       gidData.dtype = Val(temp.pval)
                       cboDtype.ListIndex = gidData.dtype
                    Case "Filename":
                       gidData.fname = temp.pval
  '                    text1.Caption = temp.pval
                       fileOK = True
                    Case "Worksheet":
                       gidData.wsname = temp.pval
  '                    Combo1.AddItem temp.pval
  '                    MaskEdBox1 = temp.pval
                    Case "Consumer":
                      gidData.consumer = temp.pval
                    Case "Dataset":
                      gidData.dataset = Val(temp.pval)
                    Case "NumLoc", "NumDS":
                      ReDim Preserve gidData.ds(temp.pval)
                      gidData.numds = temp.pval
                    Case "NumCon":
                      If temp.idx1 > UBound(gidData.ds()) Then
                        ReDim Preserve gidData.ds(temp.idx1)
                      End If
                      ReDim Preserve gidData.ds(temp.idx1).cont(temp.pval)
                      gidData.ds(temp.idx1).ncon = temp.pval
                    Case "CasId":
                      If temp.idx1 > UBound(gidData.ds()) Then
                        ReDim Preserve gidData.ds(temp.idx1)
                      End If
                      If temp.idx2 > UBound(gidData.ds(temp.idx1).cont()) Then
                        ReDim Preserve gidData.ds(temp.idx1).cont(temp.idx2)
                      End If
                      gidData.ds(temp.idx1).cont(temp.idx2).cas = temp.pval
                    Case "StartDate":
                      If temp.idx1 > UBound(gidData.ds()) Then
                        ReDim Preserve gidData.ds(temp.idx1)
                      End If
                      If temp.idx2 > UBound(gidData.ds(temp.idx1).cont()) Then
                        ReDim Preserve gidData.ds(temp.idx1).cont(temp.idx2)
                      End If
                      gidData.ds(temp.idx1).cont(temp.idx2).start = CVar(temp.pval)
                    Case Else
                      baddata = True
                  End Select
                End If
              End If
            Next i
          Case Else:
            For m = 1 To temp.idx1
              get_line fle.file
            Next m
        End Select
      End If
    Loop
    close_parm fle
  Else
    PutError "Can't find or open file " & FUIName
    EndModule
  End If
  If baddata Then
    gidData.numds = 0
    ReDim gidData.ds(0)
    ReDim gidData.ds(0).cont(0)
  End If
End Sub
Sub PutError(myError As String)
  put_val errfile, myError
  put_line errfile
  AnError = True
End Sub

Private Function QuickTest(ws As Integer)
  Dim nr As Long
  Dim nds As Integer
  Dim str As String
  Dim ncon As Integer
  Dim numds As Integer
  
  
  On Error GoTo ErrorHandler
  
  QuickTest = False
  With wbo.ActiveWorkbook.Worksheets(ws)
    nr = .range(xlRange(1, 1))
    nr = nr + 3
    numds = .range(xlRange(2, nr))
    ReDim ds(numds) As ds_type
    nr = nr + 3
    For nds = 1 To numds
      ds(nds).name = .range(xlRange(1, nr))
      ncon = .range(xlRange(8, nr))
      ds(nds).ncon = ncon
      ReDim ds(nds).cont(ncon)
      
      nr = nr + 1
      str = .range(xlRange(1, nr))
      If "Constituent" = Trim(str) Then
        QuickTest = True
        Exit Function
      End If
    Next nds
  End With
ErrorHandler:
End Function

Private Sub SpreadsheetLoad(nds As Integer)
  Dim j As Long
  Dim nd As Long
  Dim nc As Long
  Dim nr As Long
  Dim sel As Boolean
  Dim cwid As Long, wid As Long, ht As Long, swid As Single
  
  vaSpread1.Row = -1
  vaSpread1.col = -1
  vaSpread1.Action = 3 ' clear
  
  vaSpread1.ReDraw = False
  vaSpread1.MaxRows = 100
  vaSpread1.MaxCols = 3 ' 4
  vaSpread1.Row = -1
  vaSpread1.col = -1
  vaSpread1.ForeColor = vbBlack
  vaSpread1.CellType = 5 ' static text
  vaSpread1.col = 3
  vaSpread1.CellType = 0 ' date
  vaSpread1.TypeDateCentury = True
  vaSpread1.TypeDateMin = "01011900"

  vaSpread1.col = 1
  vaSpread1.AllowCellOverflow = True
  vaSpread1.ColWidth(2) = 12
  vaSpread1.ColWidth(3) = 12
  vaSpread1.ColWidthToTwips 12, cwid
  cwid = cwid * 2
  vaSpread1.GetClientArea wid, ht
  vaSpread1.TwipsToColWidth wid - cwid, swid
  vaSpread1.ColWidth(1) = swid
 
  vaSpread1.RowHeight(0) = 1.5 * vaSpread1.RowHeight(1)
  
  vaSpread1.SetText 1, 0, "Constituent"
  vaSpread1.SetText 2, 0, "CAS Id"
  vaSpread1.SetText 3, 0, "Start Date"
    
  nr = 0
  For j = 1 To ds(nds).ncon
    sel = fuiCasid(ds(nds).cont(j).cas)
    nr = nr + 1
    vaSpread1.Row = nr
    vaSpread1.SetText 1, nr, ds(nds).cont(j).name
    vaSpread1.SetText 2, nr, ds(nds).cont(j).cas
    vaSpread1.col = 2
    If Not sel Then vaSpread1.ForeColor = vbRed Else vaSpread1.ForeColor = vbBlack
    vaSpread1.col = 3
    vaSpread1.Value = Format(ds(nds).cont(j).start, "mmddyyyy")
    vaSpread1.CellType = 5 ' static text
    If Not sel Then vaSpread1.BackColor = vbWhite Else vaSpread1.BackColor = vbGreen
  Next j
  vaSpread1.MaxRows = nr
  vaSpread1.ReDraw = True
  vaSpread1.Visible = True

End Sub

Private Function DatasetLoad(ws As String) As Boolean
  Dim i As Long, j As Long, k As Long
  Dim l As Long, m As Long, n As Long, nc As Integer
  Dim connum As Long
  Dim file As csv
  Dim numds As Integer, f_numcon As Integer
  Dim date1 As Date, date2 As Date
  Dim nds As Long
  Dim ncon As Integer, nprog As Long, nyear As Long
  Dim nr As Long, np As Long
  Dim sel As Boolean
  Dim str As String, chemtype As Boolean
  
  On Error GoTo errhandler
  
  cboDataset.Clear
  
  With wbo.ActiveWorkbook.Worksheets(ws)
    nr = .range(xlRange(1, 1))
    nr = nr + 3
    numds = .range(xlRange(2, nr))
    
    ReDim ds(numds) As ds_type
    
    nr = nr + 3
    For nds = 1 To numds
      ds(nds).name = .range(xlRange(1, nr))
      
      ncon = .range(xlRange(8, nr))
      ds(nds).ncon = ncon
      ReDim ds(nds).cont(ncon)
      
      nr = nr + 1
      str = .range(xlRange(1, nr))
      If "Constituent" = Trim(str) Then
        chemtype = True
      ElseIf "Property" = Left(Trim(str), 8) Then
        chemtype = False
      Else
      ' Raise the exception
        Err.Raise vbObjectError + 513, "SCFImport.PreTest()", _
          "Worksheet does not have expected layout per template."
      End If
    
      nr = nr + 1
    
      For nc = 1 To ncon
        ds(nds).cont(nc).name = .range(xlRange(1, nr))
        ds(nds).cont(nc).cas = .range(xlRange(2, nr))
        nprog = IIf(chemtype, .range(xlRange(3, nr)), 0)
        nyear = IIf(chemtype, .range(xlRange(4, nr)), .range(xlRange(3, nr)))
        
        ds(nds).cont(nc).start = .range(xlRange(IIf(chemtype, 6, 5), nr + 1))
        nr = nr + IIf(chemtype, nyear, nyear + 1)
        
        For np = 1 To nprog
          nr = nr + 1
          nyear = .range(xlRange(4, nr))
          nr = nr + nyear
        Next np
        nr = nr + 1
      Next nc
      nr = nr + 2
    Next nds
  End With
  
  
  If gidData.fname = Text1 And gidData.wsname = Combo1.List(Combo1.ListIndex) Then
    ' the data in the gid file applies to this worksheet
    ' apply user changes (if any) to start dates
    For nds = 1 To numds
      For nc = 1 To ncon
        If gidData.numds > 0 Then
          If gidData.ds(nds).ncon > 0 Then
            On Error Resume Next
            ds(nds).cont(nc).start = gidData.ds(nds).cont(nc).start
          End If
        End If
      Next nc
    Next nds
  End If
  
errhandler:
  If Err.Number <> 0 Then
    MsgBox Error, vbOKOnly, "Worksheet does not match template."
    DatasetLoad = False
    ClearSpread
  End If
End Function

Private Sub Text1_Change()
  If loadng Then Exit Sub
  Combo1.Clear
  ClearSpread
End Sub

Private Function fuiCasid(cas As String) As Boolean
  Dim i As Integer
  For i = 1 To fuiData.ncon
    If fuiData.cont(i).cas = cas Then
      fuiCasid = True
      Exit Function
    End If
  Next
  fuiCasid = False
End Function

Private Sub ClearSpread()
  vaSpread1.Row = -1
  vaSpread1.col = -1
  vaSpread1.Action = 12 ' clear text
End Sub

Private Sub vaSpread1_Change(ByVal col As Long, ByVal Row As Long)
  Dim var As Variant
  vaSpread1.GetText col, Row, var
  If col = 2 Then
    ds(cboDataset.ListIndex + 1).cont(Row).cas = var
  Else
    ds(cboDataset.ListIndex + 1).cont(Row).start = var
  End If
  
End Sub
