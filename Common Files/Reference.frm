VERSION 5.00
Begin VB.Form Reference 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "References"
   ClientHeight    =   5895
   ClientLeft      =   1635
   ClientTop       =   5595
   ClientWidth     =   8400
   ControlBox      =   0   'False
   Icon            =   "Reference.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   393
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   560
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton Clear 
      Caption         =   "Clear Unused &References"
      Height          =   492
      Left            =   3720
      TabIndex        =   8
      Top             =   5280
      Visible         =   0   'False
      Width           =   1455
   End
   Begin VB.CommandButton Command1 
      Caption         =   "&Print"
      Height          =   492
      Left            =   2160
      TabIndex        =   5
      Top             =   5280
      Visible         =   0   'False
      Width           =   1455
   End
   Begin VB.Frame Frame1 
      Height          =   5172
      Left            =   120
      TabIndex        =   0
      Top             =   0
      Width           =   8172
      Begin VB.CommandButton NextRec 
         Caption         =   ">>"
         Height          =   252
         Left            =   1200
         TabIndex        =   12
         Top             =   4680
         Width           =   495
      End
      Begin VB.CommandButton PrevRec 
         Caption         =   "<<"
         Height          =   252
         Left            =   120
         TabIndex        =   11
         Top             =   4680
         Width           =   495
      End
      Begin VB.CommandButton FindNext 
         Caption         =   "&Search"
         Height          =   372
         Left            =   1800
         TabIndex        =   10
         Top             =   4680
         Width           =   1215
      End
      Begin VB.TextBox Match 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   372
         Left            =   3000
         TabIndex        =   9
         Top             =   4680
         Width           =   5052
      End
      Begin VB.TextBox Text2 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   3192
         Left            =   120
         MaxLength       =   256
         MultiLine       =   -1  'True
         TabIndex        =   4
         Top             =   1320
         Width           =   7935
      End
      Begin VB.TextBox Text1 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   312
         Left            =   120
         MaxLength       =   100
         TabIndex        =   2
         Top             =   540
         Width           =   7935
      End
      Begin VB.Label Label2 
         Alignment       =   2  'Center
         BorderStyle     =   1  'Fixed Single
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   9.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   252
         Left            =   600
         TabIndex        =   13
         Top             =   4680
         Width           =   612
      End
      Begin VB.Label Label3 
         AutoSize        =   -1  'True
         Caption         =   "Long description"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   192
         Left            =   120
         TabIndex        =   3
         Top             =   1020
         Width           =   1392
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Short description"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   192
         Left            =   120
         TabIndex        =   1
         Top             =   240
         Width           =   1416
      End
   End
   Begin VB.CommandButton Cancel 
      Caption         =   "&Cancel"
      Height          =   492
      Left            =   6840
      TabIndex        =   7
      Top             =   5280
      Width           =   1455
   End
   Begin VB.CommandButton Ok 
      Caption         =   "&Ok"
      Height          =   492
      Left            =   5280
      TabIndex        =   6
      Top             =   5280
      Width           =   1455
   End
End
Attribute VB_Name = "Reference"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim GREFCOUNTS() As Long

' This form module uses five global variables that the
' user must define in a BAS file.  Those variables are:

'Global RefFileName As String     'the name of the reference file
'Global RefAvailable As Boolean   'does the reference file exist
'Global RefMode As long        'if 0 then select a reference else add a reference
'Global RefIdx As long         'the current selected reference
'Global RefItem As long        'the current selected item index

' I would have put them here but for some reason
' you can't have global data on a form.

Private Type refrec
  idx As String
  abrv As String
  ref As String
End Type

Private Type RefFile
  file As csv
  find_pattern As String
  rec As refrec
  numrec As Long
  recpos() As Long
  recidx() As Long
  recnum As Long
End Type

Dim textchange As Boolean
Private rfile As RefFile

Public Function add_ref(abrv As String, ref As String, Optional showmsg As Boolean = False) As Long
' This function reads a reference file sequentially looking for a Match
' in the "abrv" and "ref" fields of a reference record.
' When a Match is found the refidx is returned.
' If no Match is found it is added and the new refidx is returnd

  RefIdx = 0
  On Error GoTo ffopenerror
  reset_csv rfile.file
  get_line rfile.file
  get_line rfile.file
  While Not EOCF(rfile.file)
    read_refrec
    If rfile.rec.abrv = abrv And rfile.rec.ref = ref Then
      RefIdx = CInt(rfile.rec.idx)
      add_ref = RefIdx
      If showmsg Then MsgBox "Reference already exists as reference " & CStr(add_ref) & ".", vbExclamation
      Exit Function
    End If
    RefIdx = CInt(rfile.rec.idx)
  Wend
  
  RefIdx = RefIdx + 1
  add_ref = RefIdx
  rfile.numrec = rfile.numrec + 1
  ReDim Preserve rfile.recpos(rfile.numrec) As Long
  ReDim Preserve rfile.recidx(rfile.numrec) As Long
  rfile.recpos(rfile.numrec) = Seek(rfile.file.fnum)
  rfile.recidx(rfile.numrec) = RefIdx
  rfile.rec.idx = CStr(RefIdx)
  rfile.rec.abrv = Trim(abrv)
  rfile.rec.ref = Trim(ref)
  write_refrec
  Exit Function

ffopenerror:
  MsgBox Error() & " add_ref"
  Exit Function
End Function

Private Function rfind_frst() As Boolean
' This function reads a reference file sequentially looking for "Match.Text"
' in the "abrv" field of a reference record.
' When a Match is found true is returned and the values stored in "Ref".
' If no Match is found a mesaage is displayed and the function returns false
' If an error occurs the function returns false.

  rfind_frst = False
  On Error GoTo ffopenerror
  rfile.find_pattern = Trim(Match.Text)
  If rfile.find_pattern = "" Then
    rfile.recnum = 0
    rfind_frst = True
    Exit Function
  End If
  reset_csv rfile.file
  get_line rfile.file
  get_line rfile.file
  rfile.recnum = 1
  Do
    read_refrec
    If InStr(rfile.rec.abrv, rfile.find_pattern) > 0 Then
      rfind_frst = True
      Exit Function
    End If
    If EOCF(rfile.file) Then Exit Do
    rfile.recnum = rfile.recnum + 1
  Loop
  rfile.recnum = 0
  Exit Function

ffopenerror:
  MsgBox Error() & " find_frst"
  Exit Function
End Function

Private Function rfind_next() As Boolean
' This function reads a reference file from the current position looking for the next
' occurrance of "find_pattern" in the "abrv" field of a reference record.
' When a Match is found true is returned and the values stored in "Ref".
' If no Match is found a message is displayed and the function returns false.
' If an error occurs the function returns false.
  
  rfind_next = False
  On Error GoTo fnopenerror
  If rfile.find_pattern = "" Then
    rfile.recnum = 0
    rfind_next = True
    Exit Function
  End If
  Do
    read_refrec
    rfile.recnum = rfile.recnum + 1
    If InStr(rfile.rec.abrv, rfile.find_pattern) > 0 Then
      rfind_next = True
      Exit Function
    End If
    If EOCF(rfile.file) Then Exit Do
  Loop
  rfile.recnum = 0
  Exit Function

fnopenerror:
  MsgBox Error() & " find_next"
  Exit Function
End Function

Private Function open_ref(fmode As Long) As Boolean
  open_ref = False
  rfile.find_pattern = ""
  If open_csv(rfile.file, RefFileName, fmode) Then
    open_ref = True
    rfile.numrec = 0
    Do
      ReDim Preserve rfile.recpos(rfile.numrec) As Long
      ReDim Preserve rfile.recidx(rfile.numrec) As Long
      rfile.recpos(rfile.numrec) = Seek(rfile.file.fnum)
      get_line rfile.file
      rfile.rec.idx = get_val(rfile.file)
      rfile.recidx(rfile.numrec) = Val(rfile.rec.idx)
      If rfile.rec.idx <> "" Then rfile.numrec = rfile.numrec + 1
      If EOCF(rfile.file) Then Exit Do
    Loop
  End If
End Function

Public Sub new_ref(Optional showmsg As Boolean = False)
  Dim fle As csv
  Dim msg As String
  Dim Title As String
  
  msg = "No reference file found!" & Chr(10) & "Creating new file."
  Title = "Reference Error"
  If showmsg Then MsgBox msg, vbInformation, Title
  If open_csv(fle, RefFileName, 1) Then
    put_val fle, "Reference"
    put_val fle, "Short Description"
    put_val fle, "Long Description"
    put_line fle
    put_val fle, "0"
    put_val fle, "No Value"
    put_val fle, "No Value"
    put_line fle
    close_csv fle
    RefAvailable = True
  Else
    msg = "Unable to create file!" & Chr(10) & "Check permissions."
    MsgBox msg, vbInformation, Title
    RefIdx = -1
    Unload Reference
  End If
End Sub

Private Sub read_refrec()
  rfile.rec.idx = get_val(rfile.file)
  rfile.rec.abrv = get_val(rfile.file)
  rfile.rec.ref = get_val(rfile.file)
  get_line rfile.file
End Sub

Private Sub write_refrec()
  put_val rfile.file, rfile.rec.idx, ""
  put_val rfile.file, rfile.rec.abrv, ""
  put_val rfile.file, rfile.rec.ref, ""
  put_line rfile.file
End Sub

Private Sub get_refrec()
  rfile.rec.idx = Str(RefIdx)
  rfile.rec.abrv = Trim(Text1.Text)
  rfile.rec.ref = Trim(Text2.Text)
End Sub

Private Sub put_refrec()
  Seek rfile.file.fnum, rfile.recpos(rfile.recnum)
  get_line rfile.file
  read_refrec
  Label2.Caption = rfile.rec.idx
  RefIdx = rfile.rec.idx
  Text1.Text = rfile.rec.abrv
  Text2.Text = rfile.rec.ref
End Sub
    
Function get_recnum(rIdx As Long)
  Dim i As Long
  
  get_recnum = 0
  For i = 1 To rfile.numrec
    If rIdx = rfile.recidx(i) Then
      get_recnum = i
      Exit For
    End If
  Next
End Function

Sub get_ref(recnum As Long, abbr As String, ref As String)
  If recnum >= rfile.numrec Then
    abbr = "Invalid Reference"
    ref = "Invalid Description"
  Else
    Seek rfile.file.fnum, rfile.recpos(recnum)
    get_line rfile.file
    read_refrec
    abbr = rfile.rec.abrv
    ref = rfile.rec.ref
  End If
End Sub

#If ui Then
Public Sub DisplayReferences(richtxt As RichTextBox)
Dim i As Long
Dim refabbr As String
Dim refdes As String

  richtxt.SelRTF = "==========================================================" & vbCrLf
  richtxt.SelRTF = "  References for " & RefFileName & vbCrLf
  richtxt.SelRTF = "==========================================================" & vbCrLf
  
  For i = 0 To rfile.numrec - 1
    get_ref i, refabbr, refdes
    richtxt.SelRTF = vbCrLf & "Reference ID #" & CStr(i) & vbCrLf & _
                      "Reference: " & refabbr & vbCrLf & _
                      "Reference Description: " & refdes & vbCrLf
  Next
End Sub
#End If

Private Sub Form_load()
  If Not RefAvailable Then
    new_ref
    RefMode = 1
  End If
  If RefMode = 0 Then
    open_ref 2
    rfile.recnum = 0
    If RefIdx > 0 Then rfile.recnum = get_recnum(RefIdx)
    put_refrec
    Text1.Locked = True
    Text2.Locked = True
#If ui = 1 Then
    Text1.Locked = False
    Text2.Locked = False
    Command1.Visible = True
    Clear.Visible = True
#End If
  Else
    open_ref 3
    Label1.Caption = "Short description for reference"
    Text1.Text = ""
    Text2.Text = ""
    PrevRec.Visible = False
    NextRec.Visible = False
    Match.Visible = False
    Label2.Visible = False
    FindNext.Visible = False
  End If
End Sub

Private Sub Form_Unload(Cancel As Integer)
  close_csv rfile.file
End Sub

Private Sub Clear_Click()
  #If ui Then
    Dim fcheck As Boolean
    Dim resp As String
    resp = MsgBox("This will delete all unused references from the reference file. Do you wish to continue?", vbYesNo + vbCritical, "Clear References")
    If (resp = vbYes) Then
      fcheck = GetGIDRefCounts(GidTitle + ".gid")
      If fcheck = True Then
          close_csv rfile.file
          fcheck = CleanReferenceFile(GidTitle + ".ref")
          Form_load
      End If
    End If
  #End If
End Sub
  
Sub SaveEdit()
#If ui Then
  If textchange Then
    textchange = False
    close_csv rfile.file
    UpdateReferenceFile (GidTitle + ".ref")
    Form_load
  End If
#End If
End Sub

Private Sub Ok_Click()
  If RefMode = 0 Then
    RefIdx = rfile.recidx(rfile.recnum)
    SaveEdit
  Else
    RefIdx = add_ref(Text1.Text, Text2.Text, True)
  End If
  Unload Reference
End Sub

Private Sub Cancel_Click()
  RefIdx = -1
  Unload Reference
End Sub

Private Sub PrevRec_Click()
  SaveEdit
  If rfile.recnum > 0 Then
    rfile.recnum = rfile.recnum - 1
  Else
    rfile.recnum = rfile.numrec - 1
  End If
  put_refrec
End Sub

Private Sub NextRec_Click()
  SaveEdit
  If rfile.recnum < rfile.numrec - 1 Then
    rfile.recnum = rfile.recnum + 1
  Else
    rfile.recnum = 0
  End If
  put_refrec
End Sub

Private Sub FindNext_Click()
Dim last As Long
  SaveEdit
  last = rfile.recnum
  If rfind_next Then
    put_refrec
    Match.BackColor = &HC0FFC0
  Else
    rfile.recnum = last
    Match.BackColor = &H8080FF
  End If
End Sub

Private Sub Match_Change()
Dim last As Long
  SaveEdit
  last = rfile.recnum
  If rfind_frst Then
    put_refrec
    Match.BackColor = &HC0FFC0
  Else
    rfile.recnum = last
    Match.BackColor = &H8080FF
  End If
End Sub

Private Sub Text1_KeyPress(KeyAscii As Integer)
  If KeyAscii = vbKeyReturn Then KeyAscii = 0
  textchange = True
End Sub

Private Sub Text2_KeyPress(KeyAscii As Integer)
  If KeyAscii = vbKeyReturn Then KeyAscii = 0
  textchange = True
End Sub

Private Sub Command1_Click()
#If ui Then
  PrintReferences
#End If
End Sub

Public Function GetGIDRefCounts(fpath As String) As Boolean
    Dim tfile As parmfile
    Dim trec As parmrec
    Dim fcheck As Boolean
    Dim pcheck As Long
    On Error GoTo EH
    ReDim GREFCOUNTS(1) As Long
    fcheck = open_parm(tfile, fpath, F_READ)
    If fcheck = False Then
        MsgBox "Unable to open file:" & Chr(10) & fpath, vbCritical + vbOKOnly, "Clear References"
        GetGIDRefCounts = False
        Exit Function
    End If
    pcheck = read_parmrec(tfile, trec)
    If pcheck = 0 Then
        MsgBox "Unable to read record from file:" & Chr(10) & fpath, vbOKOnly + vbCritical, "Clear References"
        GetGIDRefCounts = False
        Exit Function
    End If
    While EOCF(tfile.file) = False
        If pcheck = 0 Then
            MsgBox "Unable to read record from file:" & Chr(10) & fpath, vbOKOnly + vbCritical, "Clear References"
            GetGIDRefCounts = False
            Exit Function
        End If
        If trec.ref > 0 Then
            If trec.ref > UBound(GREFCOUNTS) Then
                ReDim Preserve GREFCOUNTS(trec.ref) As Long
            End If
            GREFCOUNTS(trec.ref) = GREFCOUNTS(trec.ref) + 1
        End If
        pcheck = read_parmrec(tfile, trec)
        If pcheck = 0 Then
            MsgBox "Unable to read record from file:" & Chr(10) & fpath, vbOKOnly + vbCritical, "Clear References"
            GetGIDRefCounts = False
            Exit Function
        End If
    Wend
    close_parm tfile
    GetGIDRefCounts = True
    Exit Function
EH:
    MsgBox Err.Description, vbOKOnly + vbCritical, "Clear References"
    close_parm tfile
    GetGIDRefCounts = False
End Function

Public Function CleanReferenceFile(fpath As String) As Boolean
    Dim fnum As Long
    Dim tarray
    Dim txtline As String
    Dim tindex As Long
    Dim tcoll As New Collection
    Dim i As Long
    
    On Error GoTo EH
    fnum = FreeFile()
    Open fpath For Input As #fnum
    Line Input #fnum, txtline
    tcoll.Add txtline
    Line Input #fnum, txtline
    tcoll.Add txtline
    Do
        Line Input #fnum, txtline
        tarray = Split(txtline, ",")
        tindex = Val(tarray(0))
        If GREFCOUNTS(tindex) > 0 Then
            tcoll.Add txtline
        End If
    Loop While EOF(fnum) = False
    Close #fnum
    fnum = FreeFile()
    Open fpath For Output As #fnum
    For i = 1 To tcoll.Count
        txtline = tcoll.item(i)
        Print #fnum, txtline
    Next i
    Close #fnum
    CleanReferenceFile = True
    MsgBox "Unused references successfully removed.", vbOKOnly, "Clear References"
    Exit Function
EH:
    MsgBox Err.Description, vbOKOnly + vbCritical, "Clear References"
    Close #fnum
    CleanReferenceFile = False
End Function

Public Function UpdateReferenceFile(fpath As String) As Boolean
Dim fnum As Long
Dim tarray
Dim txtline As String
Dim tcoll As New Collection
Dim i As Long
    
    On Error GoTo EH
    fnum = FreeFile()
    Open fpath For Input As #fnum
    Line Input #fnum, txtline
    tcoll.Add txtline
    Do
      Line Input #fnum, txtline
      tarray = Split(txtline, ",")
      If Val(tarray(0)) = Val(Label2.Caption) Then
        txtline = Label2.Caption & ",""" & Trim(Text1.Text) & """,""" & Trim(Text2.Text) & """"
        tcoll.Add txtline
      Else
        tcoll.Add txtline
      End If
    Loop While EOF(fnum) = False
    Close #fnum
    fnum = FreeFile()
    Open fpath For Output As #fnum
    For i = 1 To tcoll.Count
        txtline = tcoll.item(i)
        Print #fnum, txtline
    Next i
    Close #fnum
    UpdateReferenceFile = True
    Exit Function
EH:
    MsgBox Err.Description, vbOKOnly + vbCritical, "Update References"
    Close #fnum
    UpdateReferenceFile = False
End Function



