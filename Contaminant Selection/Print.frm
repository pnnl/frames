VERSION 5.00
Begin VB.Form frmPrint 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Print Consituent Properties"
   ClientHeight    =   6012
   ClientLeft      =   48
   ClientTop       =   336
   ClientWidth     =   7056
   ClipControls    =   0   'False
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   7.8
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form6"
   LockControls    =   -1  'True
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   6012
   ScaleWidth      =   7056
   StartUpPosition =   1  'CenterOwner
   Begin VB.Frame Frame2 
      Caption         =   "Select Constituents to Print"
      ClipControls    =   0   'False
      ForeColor       =   &H00FF0000&
      Height          =   5160
      Left            =   135
      TabIndex        =   3
      Top             =   195
      Width           =   6765
      Begin VB.ComboBox cboPrtList 
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   9.6
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   336
         ItemData        =   "Print.frx":0000
         Left            =   1590
         List            =   "Print.frx":000A
         Style           =   2  'Dropdown List
         TabIndex        =   4
         Top             =   315
         Width           =   4980
      End
      Begin VB.ListBox List1 
         Height          =   3696
         Left            =   210
         MultiSelect     =   2  'Extended
         TabIndex        =   5
         Top             =   840
         Width           =   6345
      End
      Begin VB.CheckBox chkRef 
         Caption         =   "Include cited references"
         Height          =   240
         Left            =   510
         TabIndex        =   7
         Top             =   4800
         Width           =   2580
      End
      Begin VB.Label Label4 
         AutoSize        =   -1  'True
         Caption         =   "Select From:"
         Height          =   195
         Left            =   420
         TabIndex        =   6
         Top             =   390
         Width           =   885
      End
   End
   Begin VB.CommandButton Command1 
      Caption         =   "&Close"
      Height          =   360
      Left            =   5430
      TabIndex        =   9
      Top             =   5505
      Width           =   1335
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "&Print"
      Enabled         =   0   'False
      Height          =   360
      Left            =   3930
      TabIndex        =   8
      Top             =   5505
      Width           =   1335
   End
   Begin VB.Frame Frame1 
      Caption         =   "Default Printer"
      ClipControls    =   0   'False
      ForeColor       =   &H00FF0000&
      Height          =   975
      Left            =   120
      TabIndex        =   0
      Top             =   240
      Visible         =   0   'False
      Width           =   6765
      Begin VB.CommandButton cmdPropert 
         Caption         =   "Select..."
         Height          =   360
         Left            =   5160
         TabIndex        =   2
         Top             =   360
         Width           =   1260
      End
      Begin VB.Label lblPrinter 
         AutoSize        =   -1  'True
         Caption         =   "Name:"
         Height          =   195
         Left            =   375
         TabIndex        =   1
         Top             =   443
         Width           =   465
      End
   End
End
Attribute VB_Name = "frmPrint"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Private Sub cboPrtList_Click()
Dim i As Long
  
  List1.Clear
  frmCDBE.cboView.ListIndex = cboPrtList.ListIndex
  For i = 0 To frmCDBE.lstCon.ListCount - 1
    FindSynonyms frmCDBE.lstCon.itemdata(i), "", ""
    List1.AddItem tblChems!Name
    List1.itemdata(List1.NewIndex) = tblChems!rowid
  Next
  List1.ListIndex = -1
End Sub

Private Sub Command1_Click()
  Unload Me
End Sub

Private Sub Form_Activate()
  frmCDBE.SaveAnchor HelpAnchor
  HelpAnchor = "Print"
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
  Select Case KeyCode
    Case vbKeyF1:
      KeyCode = 0
      frmCDBE.howto_Click ' GetHelp
  End Select
End Sub

Private Sub Form_load()
  cboPrtList.list(0) = frmCDBE.cboView.list(0)
  cboPrtList.list(1) = frmCDBE.cboView.list(1)
  cboPrtList.ListIndex = frmCDBE.cboView.ListIndex
  If cboPrtList.ListIndex < 0 Then
    cboPrtList.ListIndex = 0
    If List1.ListCount = 0 Then
      cboPrtList.ListIndex = -1
    End If
  End If
  If Not ReferenceLoaded Then
    frmReference.Show
    frmReference.Hide
  End If
End Sub

Private Sub cmdOK_Click()
Dim i As Long
Dim j As Long
Dim row As Long
Dim rowid As Long
Dim buf As String
Dim hdr As String
Dim ftr As String
Dim qry As String
Dim task As String
Dim pName As String
Dim pcasid As String
Dim font1 As String
Dim font2 As String
Dim var As Variant
Dim spr As fpSpread
Dim nodx As Node

On Error GoTo Error_Handler

  ' Create a font as Times, size 10, no bold, italics,
  ' underline, no strikethrough and save it as font #2
  font1 = "/fn""Arial""/fz""10""/fb1/fi0/fu1/fk0/fs1"
  font1 = "/fn""Arial""/fz""10""/fb0/fi0/fu0/fk0/fs1"
  ' Create a font as Times, size 20, no bold, italics, no
  ' underline, strikethrough and save it as font #2
  font2 = "/fn""Times""/fz""20""/fb0/fi1/fu0/fk1/fs2"
  font2 = "/fn""Times""/fz""20""/fb0/fi1/fu0/fk0/fs2"
  ' Recall font configurations and set the header and footer text
  buf = font1 & font2 & "/f1This is font #1/n/f2This is font #2"
  hdr = font1 & font2 & "/n/n/f2This is a header /n/n/n"
  ftr = font1 & font2 & "/n/n/f1This is a footer /nPage /p"

  frmCDBE.cboView.ListIndex = cboPrtList.ListIndex

  font1 = "/fn""Arial""/fz""12""/fb0/fi0/fu0/fk0/fs1"
  font2 = "/fn""Times""/fz""20""/fb0/fi0/fu0/fk0/fs2"

  For i = 0 To List1.ListCount - 1
    Set spr = frmCDBE.vaSpread1
    If List1.Selected(i) Then
      frmCDBE.lstCon.ListIndex = i

      spr.PrintMarginLeft = 1440 ' 1 inch
      spr.PrintMarginRight = 720 ' 1/2 inch

      hdr = font1 & font2 & "/n/n/f1CAS Id: " & frmCDBE.txtCasid & "/rPage /p/n"
      hdr = hdr & "Name(s): " & frmCDBE.txtName
      If Len(frmCDBE.txtSyns) > 0 Then
        buf = frmCDBE.txtSyns
        Do While Len(buf) > 0
          j = InStr(buf, vbCrLf)
          If j = 0 Then
            hdr = hdr & "," & buf
            buf = ""
          Else
            hdr = hdr & ", " & Left$(buf, j - 1)
            buf = Mid$(buf, j + 2)
          End If
        Loop
      End If
      hdr = hdr & "/n/nImpact: " & frmCDBE.chkRad & "/n"
      hdr = hdr & "Classification: " & frmCDBE.Combo0 & "/n"
      If frmCDBE.tvwDKview(DK_FLAT).Nodes.count > 0 Then
        hdr = hdr & "/nProgeny: "
        j = 1
        For Each nodx In frmCDBE.tvwDKview(DK_FLAT).Nodes
          frmCDBE.SplitNodeTag nodx.tag, rowid, pcasid, pName
          If pcasid <> frmCDBE.txtCasid Then
              If (0 = j Mod 4) Then hdr = hdr & "/n     " Else hdr = hdr & ", "
              hdr = hdr & nodx.text
              j = j + 1
          End If
        Next
      End If
      hdr = hdr & "/n/n"

      ftr = font1 & font2 & "/n/n/f1" & DBName & "/r "
      ftr = ftr & Format(Date, "mm/dd/yyyy") & " " & Format(Time, "hh.mmam/pm") & "/n"

      spr.PrintFooter = ftr
      spr.PrintHeader = hdr
      spr.row = 0
      spr.Row2 = spr.MaxRows
      spr.col = 0
      spr.Col2 = 4
      ' Set the cell range to be printed
      spr.PrintType = 1 ' SS_PRINT_CELL_RANGE
      spr.PrintColor = False
      spr.PrintUseDataMax = False
      spr.Sheet = 1
      spr.PrintSheet 0

      If chkRef.value = 1 Then
        qry = ""
        For row = 1 To spr.MaxRows
          spr.GetText 4, row, var
          If IsNumeric(var) Then
            If 0 < Val(var) Then
              If Len(qry) > 0 Then qry = qry & ","
              qry = qry & var
            End If
          End If
        Next

        If Len(qry) > 0 Then
          qry = "SELECT * FROM ref WHERE refnum IN (" & qry & ")"

          Set tblRef = DB.OpenRecordset(qry, dbOpenDynaset)
          frmReference.LoadReference
          Set spr = frmReference.vaSpread1

          spr.PrintMarginLeft = 1440 ' 1 inch
          spr.PrintMarginRight = 720 ' 1/2 inch

          hdr = font1 & font2 & "/n/n/f1CAS Id: " & frmCDBE.txtCasid & "/rPage /p/n"
          hdr = hdr & "Name(s): " & frmCDBE.txtName
          hdr = hdr & "/n/nCited References: /n/n"

          ftr = font1 & font2 & "/n/n/f1" & DBName & "/r "
          ftr = ftr & Format(Date, "mm/dd/yyyy") & " " & Format(Time, "hh.mmam/pm") & "/n"

          spr.PrintFooter = ftr
          spr.PrintHeader = hdr
          spr.row = 0
          spr.Row2 = spr.MaxRows
          spr.col = 0
          spr.Col2 = 4
          ' Set the cell range to be printed
          spr.PrintType = 1 ' SS_PRINT_CELL_RANGE
          spr.PrintColor = False
          spr.PrintUseDataMax = False
          spr.Sheet = 1
          spr.PrintSheet 0
        End If
      End If
    End If
  Next

  ' restore reference

  ' The next statement was added, seemingly unnecessarily, because it changed the behavior
  ' of the first call to tblRef in Reference.LoadReference for the following condition:
  '   In standalone mode, when File|Save is followed by File|Print Constituent Properties an error
  '   was thrown (object invalid or no longer set) for no discernible reason.
  task = "Set tblRef = Nothing"
  Set tblRef = Nothing

  ' In the situation described above, this tblRef object is OK before this call to OpenRecordset
  ' but invalid after the call.  Can only speculate that something is getting corrupted,
  ' perhaps by spreadsheet operations in this sub.
  ' Setting the recordset to nothing first before OpenRecordset resolved the problem.
  task = "Set tblRef = DB.OpenRecordset(""ref"", dbOpenDynaset)"
  Set tblRef = DB.OpenRecordset("ref", dbOpenDynaset)

  task = "frmReference.LoadReference"
  frmReference.LoadReference
      
Error_Handler:
  If Err.Number <> 0 Then
    MsgBox task & vbCrLf & Error, vbOKOnly, "cmkOK_Click"
  End If
End Sub

Private Sub List1_Click()
  cmdOK.Enabled = List1.SelCount > 0
End Sub
