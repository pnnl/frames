VERSION 5.00
Object = "{F856EC8B-F03C-4515-BDC6-64CBD617566A}#7.0#0"; "FPSPR70.ocx"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{3B7C8863-D78F-101B-B9B5-04021C009402}#1.2#0"; "RICHTX32.OCX"
Begin VB.Form frmPrint 
   Caption         =   "Print"
   ClientHeight    =   7032
   ClientLeft      =   60
   ClientTop       =   348
   ClientWidth     =   8052
   LinkTopic       =   "Form1"
   ScaleHeight     =   7032
   ScaleWidth      =   8052
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton Command1 
      Caption         =   "Close"
      Height          =   375
      Index           =   2
      Left            =   6720
      TabIndex        =   1
      Top             =   1200
      Width           =   1095
   End
   Begin RichTextLib.RichTextBox RichTextBox1 
      Height          =   1095
      Left            =   240
      TabIndex        =   3
      Top             =   1560
      Visible         =   0   'False
      Width           =   5655
      _ExtentX        =   9970
      _ExtentY        =   1926
      _Version        =   393217
      Enabled         =   -1  'True
      ScrollBars      =   2
      TextRTF         =   $"Print.frx":0000
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   6120
      Top             =   240
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Print"
      Height          =   375
      Index           =   1
      Left            =   6720
      TabIndex        =   0
      Top             =   720
      Width           =   1095
   End
   Begin FPSpreadADO.fpSpread vaSpread1 
      Height          =   1200
      Left            =   240
      TabIndex        =   2
      Top             =   240
      Width           =   5685
      _Version        =   458752
      _ExtentX        =   10028
      _ExtentY        =   2117
      _StockProps     =   64
      BackColorStyle  =   1
      ColHeaderDisplay=   0
      DisplayRowHeaders=   0   'False
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      GrayAreaBackColor=   16777215
      GridSolid       =   0   'False
      ProcessTab      =   -1  'True
      RowHeaderDisplay=   0
      ScrollBarExtMode=   -1  'True
      SelectBlockOptions=   0
      SpreadDesigner  =   "Print.frx":0082
      VisibleRows     =   15
   End
   Begin RichTextLib.RichTextBox RichTextBox2 
      Height          =   1095
      Left            =   240
      TabIndex        =   4
      Top             =   2880
      Visible         =   0   'False
      Width           =   5655
      _ExtentX        =   9970
      _ExtentY        =   1926
      _Version        =   393217
      Enabled         =   -1  'True
      ScrollBars      =   2
      TextRTF         =   $"Print.frx":02F1
   End
End
Attribute VB_Name = "frmPrint"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim offx As Long
Dim offy As Long

Dim oldsite As Integer
Dim IgnoreEvents As Boolean
Dim vfui As FuiCls
Dim vHeight As Long

Private Sub Command1_Click(Index As Integer)
Dim flags As Long
Dim cdl As CommonDialog
  
 If Index = 1 Then
   flags = PD_NOPAGENUMS + PD_NOSELECTION
   If ShowPrinter(Me, flags) Then
     PrintPreview
     If flags And PD_PRINTTOFILE Then
      Set cdl = CommonDialog1
      On Error Resume Next
      If cdl.InitDir = "" Then cdl.InitDir = SplitPath(vfui.FUIName, SP_DIR)
      cdl.DialogTitle = "Print To File"
      If cdl.FileName = "" Then cdl.FileName = "*.rtf"
      cdl.Filter = "Rich Text Files (*.rtf)|*.rtf ' |Text Files (*.txt)|*.txt"
      cdl.FilterIndex = 0
      cdl.DefaultExt = "rtf"
      cdl.flags = cdlOFNOverwritePrompt Or cdlOFNHideReadOnly
      cdl.flags = cdl.flags Or cdlOFNExtensionDifferent Or cdlOFNNoChangeDir
      cdl.CancelError = True
      On Error Resume Next
      cdl.ShowOpen
      If Not Err = cdlCancel Then
         PrintPreview
         RichTextBox1.SaveFile cdl.FileName, 0  ' & ".rtf", 0
      End If
     Else
       PrintPreview True
     End If
   End If
   Exit Sub
 End If
 
 If Index = 2 Then
   Unload Me
   Exit Sub
 End If
End Sub

Private Sub Form_load()
Dim i As Long, j As Long, row As Long
Dim dx As Long, dy As Long
Dim rid As Long, id As Long

  
  oldsite = currentSite
  RefMode = 0
  SetRefFile LongGidTitle & DOT_REF
  Load Reference
  
  offx = 15 * Screen.TwipsPerPixelX
  offy = 15 * Screen.TwipsPerPixelY
  
  vaSpread1.Left = offx
  vaSpread1.Top = offy
  vaSpread1.MaxCols = 5
  For i = 0 To NumSites - 1
    row = row + 2 + Sites(i).NumGlyphs
  Next i
  vaSpread1.MaxRows = row
  vaSpread1.AutoSize = True
  vaSpread1.ShadowColor = wbcolor
  vaSpread1.RowHeight(0) = 2 * vaSpread1.RowHeight(1)
  vaSpread1.SetText 2, 0, "All"
  vaSpread1.SetText 3, 0, "Module" & vbCrLf & "Description"
  vaSpread1.SetText 4, 0, "Input" & vbCrLf & "Data"
  vaSpread1.SetText 5, 0, "Input" & vbCrLf & "Reference"
  vaSpread1.ColWidth(1) = 25
  vaSpread1.ColWidth(3) = 11
  row = 0
  For i = 0 To NumSites - 1
    IgnoreEvents = True
    row = row + 1
    rid = row: id = 100 * (i + 1)
    
    vaSpread1.SetRowItemData row, id
    vaSpread1.SetText 1, row, Sites(i).Name
    vaSpread1.col = 2: vaSpread1.row = row
    vaSpread1.CellType = 10: vaSpread1.TypeCheckCenter = True

    row = row + 1
    vaSpread1.SetRowItemData row, id
    vaSpread1.SetText 1, row, Space(5) & "Layout Picture"
    vaSpread1.row = row: vaSpread1.col = 2
    vaSpread1.CellType = 10: vaSpread1.TypeCheckCenter = True
    
    For j = 0 To Sites(i).NumGlyphs - 1
      With Sites(i).Glyphs(j)
        row = row + 1
        vaSpread1.SetRowItemData row, id + j + 1
        vaSpread1.SetText 1, row, Space(5) & .label & " (" & .Name & ")"
        vaSpread1.row = row
        If Sites(i).Glyphs(j).state >= MODULE_OK Then
          vaSpread1.col = 2: vaSpread1.CellType = 10: vaSpread1.TypeCheckCenter = True
          vaSpread1.col = 3: vaSpread1.CellType = 10: vaSpread1.TypeCheckCenter = True
          If Sites(i).Glyphs(j).state >= INPUT_OK Then
            vaSpread1.col = 4: vaSpread1.CellType = 10: vaSpread1.TypeCheckCenter = True
'            If Group(Sites(i).Glyphs(j).GrpIdx).Type <> DB Then
            If InStr(1, Module(Sites(i).Glyphs(j).modIdx).UIBat, "contsel.exe") = 0 Then
              vaSpread1.col = 5: vaSpread1.CellType = 10: vaSpread1.TypeCheckCenter = True
            End If
          End If
        End If
      End With
    Next j
    IgnoreEvents = False
    vaSpread1.row = rid: vaSpread1.col = 2: vaSpread1.value = True
  Next i
' vaSpread1.ColWidth(1) = vaSpread1.MaxTextColWidth(1) + 10
' vaSpread1.ColWidth(3) = Len("Description")
  
  'static moves
  Command1(1).Left = vaSpread1.Left + vaSpread1.Width + offx
  Command1(2).Left = vaSpread1.Left + vaSpread1.Width + offx
  RichTextBox1.Left = vaSpread1.Left
  RichTextBox1.Top = offy + vaSpread1.Top + vaSpread1.Height

  Me.Width = Command1(1).Left + Command1(1).Width - (Me.Width - Me.ScaleWidth) + (2 * offx)
  Me.Width = Command1(1).Left + Command1(1).Width + (1 * offx)
  
End Sub

Private Sub Form_Resize()
  If IgnoreEvents Then Exit Sub
  IgnoreEvents = True
  RichTextBox1.Visible = False
  If Me.ScaleWidth - 2 * RichTextBox1.Left > 100 Then RichTextBox1.Width = Me.ScaleWidth - 2 * RichTextBox1.Left
  If Me.Height - (RichTextBox1.Top + 3 * offy) > 100 Then RichTextBox1.Height = Me.Height - (RichTextBox1.Top + 3 * offy)
  RichTextBox1.Visible = True
  IgnoreEvents = False
End Sub

Private Sub Form_Unload(Cancel As Integer)
  RichTextBox1.Text = ""
  RichTextBox2.Text = ""
  Set vfui = Nothing
  Unload Reference
  'must refresh orginal site drawing, may have drawn on it for printing
  frmFUI.mnuSites_Click oldsite
End Sub

Private Sub vaSpread1_ButtonClicked(ByVal col As Long, ByVal row As Long, ByVal ButtonDown As Integer)
Dim SiteIndex As Long, GlyphIndex As Long
Dim r As Long, c As Long, ix As Long, txt As Variant, rid As Long
Dim r2 As Long, same As Boolean

  Dim value
  If IgnoreEvents Then Exit Sub
  
  rid = vaSpread1.GetRowItemData(row)
  SiteIndex = rid / 100
  GlyphIndex = rid - (SiteIndex * 100)
  
  vaSpread1.row = row
  vaSpread1.col = col
  
  value = vaSpread1.value
    Select Case col
      Case 2:
        If GlyphIndex = 0 Then
          vaSpread1.GetText 1, row, txt
          If 0 = InStr(txt, "Picture") Then
            IgnoreEvents = True
            vaSpread1.row = row + 1: vaSpread1.col = 2
            If vaSpread1.CellType = 10 Then vaSpread1.value = value
            For r = row + 2 To vaSpread1.DataRowCnt
              rid = vaSpread1.GetRowItemData(r): ix = rid / 100
              If SiteIndex = ix And 0 < rid - (ix * 100) Then
                vaSpread1.row = r
                For c = 2 To 5
                  vaSpread1.col = c
                  If vaSpread1.CellType = 10 Then vaSpread1.value = value
                Next c
              Else
                Exit For
              End If
            Next r
          End If
        Else
          IgnoreEvents = True
          vaSpread1.col = 3: If vaSpread1.CellType = 10 Then vaSpread1.value = value
          vaSpread1.col = 4: If vaSpread1.CellType = 10 Then vaSpread1.value = value
          vaSpread1.col = 5: If vaSpread1.CellType = 10 Then vaSpread1.value = value
          ResetAllSite SiteIndex
        End If
        IgnoreEvents = False
      Case 3:
        IgnoreEvents = True
        If value Then
          vaSpread1.col = 4
          If vaSpread1.CellType = 10 Then
            If vaSpread1.value Then
              vaSpread1.col = 5
              If vaSpread1.CellType = 10 Then
                If vaSpread1.value Then
                  vaSpread1.col = 2: vaSpread1.value = True
                Else
                  vaSpread1.col = 2: vaSpread1.value = False
                End If
              Else
                vaSpread1.col = 2: vaSpread1.value = True
              End If
            Else
              vaSpread1.col = 2: vaSpread1.value = False
            End If
          Else
            vaSpread1.col = 2: vaSpread1.value = True
          End If
        Else
          vaSpread1.col = 2: vaSpread1.value = False
        End If
        ResetAllSite SiteIndex
        IgnoreEvents = False
      Case 4:
        IgnoreEvents = True
        If value Then
          vaSpread1.col = 5
          If vaSpread1.CellType = 10 Then vaSpread1.value = True
          vaSpread1.col = 3
          If vaSpread1.value Then
            vaSpread1.col = 2: vaSpread1.value = True
          Else
            vaSpread1.col = 2: vaSpread1.value = False
          End If
        Else
          vaSpread1.col = 5
          If vaSpread1.CellType = 10 Then vaSpread1.value = False
          vaSpread1.col = 2: vaSpread1.value = False
        End If
        ResetAllSite SiteIndex
        IgnoreEvents = False
      Case 5:
        IgnoreEvents = True
        If value Then
          vaSpread1.col = 4
          vaSpread1.value = True
          vaSpread1.col = 3
          If vaSpread1.value Then
            vaSpread1.col = 2: vaSpread1.value = True
          Else
            vaSpread1.col = 2: vaSpread1.value = False
          End If
        Else
          vaSpread1.col = 2: vaSpread1.value = False
        End If
        ResetAllSite SiteIndex
        IgnoreEvents = False
    End Select
    PrintPreview
End Sub

Private Sub ResetAllSite(SiteIndex As Long)
Dim r As Long
Dim r2 As Long
Dim allsel As Boolean
Dim rid As Long
Dim ix As Long
  
  allsel = True
  vaSpread1.col = 2
  For r = 1 To vaSpread1.DataRowCnt
    rid = vaSpread1.GetRowItemData(r): ix = rid / 100
    If ix = SiteIndex Then
      For r2 = r + 1 To vaSpread1.DataRowCnt
        rid = vaSpread1.GetRowItemData(r2): ix = rid / 100
        If ix = SiteIndex Then
          vaSpread1.row = r2
          If vaSpread1.CellType = 10 Then
            allsel = allsel And (vaSpread1.value)
          End If
          If Not allsel Then Exit For
        Else
          Exit For
        End If
      Next r2
      vaSpread1.row = r
      IgnoreEvents = True
      If vaSpread1.CellType = 10 Then vaSpread1.value = allsel
      IgnoreEvents = False
      Exit For
    End If
  Next r
End Sub

Public Sub PrintPreview(Optional prt As Boolean = False)
Dim r As Long
Dim row As Long
Dim rid As Long
Dim siteIdx As Long
Dim modIdx As Long
Dim ix As Long
Dim txt As Variant

    If Not prt Then RichTextBox1.Text = ""
    RichTextBox2.Text = ""
  
    Set vfui = New FuiCls
    vfui.FUIName = LongGidTitle
    vfui.RunName = TmpTitle
    For row = 1 To vaSpread1.DataRowCnt
      vaSpread1.row = row
      vaSpread1.GetText 1, row, txt
      If 0 < InStr(txt, "Picture") Then
        rid = vaSpread1.GetRowItemData(row)
        siteIdx = rid / 100
        vfui.siteIdx = siteIdx
        vaSpread1.col = 2
        frmFUI.mnuSites_Click siteIdx - 1
        If vaSpread1.CellType = 10 And vaSpread1.value Then
          Clipboard.Clear
          Clipboard.SetData pic.Image
          If prt Then
            SendMessage RichTextBox2.hWnd, WM_PASTE, 0, 0
'            RichTextBox2.Visible = True: RichTextBox2.ZOrder 0
            PrintRTF RichTextBox2, 1440, 1440, 1440, 1440
          Else
            SendMessage RichTextBox1.hWnd, WM_PASTE, 0, 0
          End If
        End If
        For r = row + 1 To vaSpread1.DataRowCnt
          rid = vaSpread1.GetRowItemData(r)
          ix = rid / 100
          If siteIdx = ix And 0 < rid - (ix * 100) Then
            modIdx = rid - (siteIdx * 100)
            vfui.modIdx = modIdx - 1
            vfui.ModName = Sites(siteIdx - 1).Glyphs(modIdx - 1).Name
            vfui.argv0 = "gid"
            vaSpread1.row = r
            vaSpread1.col = 3
            If vaSpread1.CellType = 10 Then
              If vaSpread1.value Then
                vfui.DisplayModuleDescription vfui.ModName, RichTextBox2, vfui
                If Not prt Then
                  RichTextBox1.SelRTF = RichTextBox2.TextRTF
                Else
'                 RichTextBox2.Visible = True: RichTextBox2.ZOrder 0
                  PrintRTF RichTextBox2, 1440, 1440, 1440, 1440
                End If
              End If
            End If
          Else
            Exit For
          End If
        Next r
        For r = row + 1 To vaSpread1.DataRowCnt
          rid = vaSpread1.GetRowItemData(r)
          ix = rid / 100
          If siteIdx = ix And 0 < rid - (ix * 100) Then
            modIdx = rid - (siteIdx * 100)
            vfui.modIdx = modIdx - 1
            vfui.ModName = Sites(siteIdx - 1).Glyphs(modIdx - 1).Name
            vfui.argv0 = "gid"
            vaSpread1.row = r
            vaSpread1.col = 4
            If vaSpread1.CellType = 10 Then
              If vaSpread1.value Then
                If Group(Sites(siteIdx - 1).Glyphs(modIdx - 1).GrpIdx).Type = DB Then
                  vfui.DisplayFileContents RichTextBox2, vfui, False
                Else
                  vaSpread1.col = 5
                  vfui.DisplayFileContents RichTextBox2, vfui, vaSpread1.value
                End If
                If Not prt Then
                  RichTextBox1.SelRTF = RichTextBox2.TextRTF
                Else
'                 RichTextBox2.Visible = True: RichTextBox2.ZOrder 0
                  PrintRTF RichTextBox2, 1440, 1440, 1440, 1440
                End If
              End If
            End If
          Else
            Exit For
          End If
          row = row + 1
        Next r
      End If
    Next row
End Sub
