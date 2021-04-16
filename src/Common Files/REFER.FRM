VERSION 5.00
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form refer 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "References"
   ClientHeight    =   4368
   ClientLeft      =   2676
   ClientTop       =   1884
   ClientWidth     =   9168
   ControlBox      =   0   'False
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   4368
   ScaleWidth      =   9168
   Begin Threed.SSFrame frm 
      Height          =   4092
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   8892
      _Version        =   65536
      _ExtentX        =   15690
      _ExtentY        =   7223
      _StockProps     =   14
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   7.8
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ShadowStyle     =   1
      Begin VB.TextBox Text1 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   375
         Left            =   480
         MaxLength       =   100
         TabIndex        =   1
         Top             =   1080
         Width           =   6615
      End
      Begin VB.TextBox Text2 
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   1935
         Left            =   480
         MaxLength       =   256
         MultiLine       =   -1  'True
         TabIndex        =   2
         Top             =   1800
         Width           =   6615
      End
      Begin Threed.SSCommand SSCommand4 
         Height          =   375
         Left            =   8160
         TabIndex        =   6
         Top             =   2520
         Width           =   495
         _Version        =   65536
         _ExtentX        =   873
         _ExtentY        =   661
         _StockProps     =   78
         Caption         =   ">>"
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Font3D          =   2
      End
      Begin Threed.SSCommand SSCommand3 
         Height          =   375
         Left            =   7560
         TabIndex        =   5
         Top             =   2520
         Width           =   495
         _Version        =   65536
         _ExtentX        =   873
         _ExtentY        =   661
         _StockProps     =   78
         Caption         =   "<<"
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Font3D          =   2
      End
      Begin Threed.SSCommand SSCommand1 
         Height          =   375
         Left            =   7560
         TabIndex        =   3
         Top             =   1080
         Width           =   1095
         _Version        =   65536
         _ExtentX        =   1931
         _ExtentY        =   661
         _StockProps     =   78
         Caption         =   "&Ok"
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Font3D          =   2
      End
      Begin Threed.SSCommand SSCommand2 
         Height          =   375
         Left            =   7560
         TabIndex        =   4
         Top             =   1800
         Width           =   1095
         _Version        =   65536
         _ExtentX        =   1931
         _ExtentY        =   661
         _StockProps     =   78
         Caption         =   "&Cancel"
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Font3D          =   2
      End
      Begin VB.Label Label1 
         Caption         =   "Reference number"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   240
         TabIndex        =   9
         Top             =   360
         Width           =   6855
      End
      Begin VB.Label Label2 
         Caption         =   "Short description"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   240
         TabIndex        =   8
         Top             =   840
         Width           =   2295
      End
      Begin VB.Label Label3 
         Caption         =   "Long description"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   7.8
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   240
         TabIndex        =   7
         Top             =   1560
         Width           =   3855
      End
   End
End
Attribute VB_Name = "refer"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Type refrec
  idx As String
  abrv As String
  ref As String
End Type

Private Type reffile
  file As csv
Rem   fnum As Integer
  find_pattern As String
End Type

Dim filepos() As Long
Dim refnums() As Integer
Dim poslen As Integer
Dim recnum As Integer
Dim tpos As Long
Dim temp As refrec
Dim rfile As reffile
  
Private Sub close_ref(rfile As reffile)
  close_csv rfile.file
  rfile.find_pattern = ""
End Sub

Private Function rfind_frst(rfile As reffile, ref As refrec, match As String) As Integer
' This function reads a parameter file sequentially looking for "idx"
' in the "idx" field of a parameter record.  When a match is found a 1
' is returned and the values stored in "ref".  If no .ref file a found mesaage is
' displayed and the function returns a 0.  If the pname is not found the function returns a 0

  Dim temp As refrec
  On Error GoTo ffopenerror
  rfile.find_pattern = match
  reset_csv rfile.file
  Do Until EOCF(rfile.file)
  If read_refrec(rfile, temp) Then
    If StrComp(Trim(temp.idx), Trim(rfile.find_pattern), 1) = 0 Then
    rfind_frst = 1
    ref = temp
    Exit Function
    End If
  End If
  Loop
  rfind_frst = 0
  Exit Function

ffopenerror:
  MsgBox Error() & " find_frst"
  rfind_frst = 0
  Exit Function
End Function

Private Function rfind_next(rfile As reffile, ref As refrec) As Integer
' This function reads a reference file sequentially looking for the next
' occurrance of "idx" in the "idx" field of a parameter record.  When a match
' is found a 1' is returned and the values stored in "ref".  If no .ref file a message is
' displayed and the function returns a 0.  If the pname is not found the function returns a 0
  
  Dim temp As refrec
  On Error GoTo fnopenerror
  Do Until EOCF(rfile.file)
  If read_refrec(rfile, temp) Then
    If StrComp(Trim(temp.idx), Trim(rfile.find_pattern), 1) = 0 Then
    rfind_next = 1
    ref = temp
    Exit Function
    End If
  End If
  Loop
  rfind_next = 0
  Exit Function

fnopenerror:
  MsgBox Error() & " find_next"
  rfind_next = 0
  Exit Function
End Function

Private Function open_ref(rfile As reffile, fname As String, fmode As Long) As Boolean
  If open_csv(rfile.file, fname, fmode) Then
  ReDim Preserve filepos(1) As Long
  ReDim Preserve refnums(1) As Integer
  tpos = Seek(rfile.file.fnum)
  poslen = 1
  filepos(0) = 1
  refnums(0) = -1
  rfile.find_pattern = ""
  open_ref = True
  Else
  open_ref = False
  End If
End Function

Private Function read_refrec(rfile As reffile, temp As refrec) As Integer
  temp.idx = get_val(rfile.file)
  temp.abrv = get_val(rfile.file)
  temp.ref = get_val(rfile.file)
  If temp.idx <> "" Then
  If filepos(poslen - 1) < tpos Then
    filepos(poslen) = tpos
    refnums(poslen - 1) = Val(temp.idx)
    poslen = poslen + 1
    ReDim Preserve filepos(poslen) As Long
    ReDim Preserve refnums(poslen) As Integer
  End If
  read_refrec = 1
  Else
  read_refrec = 0
  End If
  get_line rfile.file
  tpos = Seek(rfile.file.fnum)
End Function

Private Sub set_ref(temp As refrec, f1 As String, f2 As String, f3 As String)
  temp.idx = f1
  temp.abrv = f2
  temp.ref = f3
End Sub

Private Sub write_refrec(rfile As reffile, temp As refrec)
  put_val rfile.file, temp.idx, ""
  put_val rfile.file, temp.abrv, ""
  put_val rfile.file, temp.ref, ""
  put_line rfile.file
End Sub

Private Sub Form_Load()
  Dim i As Integer
  Dim j As Integer
  Dim tpos As Long
  Dim msg As String
  Dim style As Integer
  Dim title As String
  Dim response As Integer
  
  RefName = App.Path & thismod & ".REF"
  If refmode = 0 Then
  If open_ref(rfile, RefName, 2) Then
    read_refrec rfile, temp
    read_refrec rfile, temp
    Label1.Caption = "Reference number " & temp.idx
    Text1.Text = temp.abrv
    Text2.Text = temp.ref
    recnum = 1
  Else
    msg = "No message file found!" & Chr(10) & "Creating new file."   ' Define message.
    style = vbInformation
    title = "Reference Error"   ' Define title.
    MsgBox msg, style, title
    If open_ref(rfile, RefName, 1) Then
    set_ref temp, "REFNUM", "SNAME", "BNAME"
    write_refrec rfile, temp
    set_ref temp, "0", "No Value", "No Value"
    write_refrec rfile, temp
    close_ref rfile
    refmode = 1
    GoTo mode1
    Else
    msg = "Unable to create file!" & Chr(10) & "Check permissions."
    MsgBox msg, style, title
    Unload refer
    End If
  End If
  Else
mode1:
  If open_ref(rfile, RefName, 3) Then
    If LOF(rfile.file.fnum) < 10 Then
    set_ref temp, "REFNUM", "SNAME", "BNAME"
    write_refrec rfile, temp
    set_ref temp, "0", "No Value", "No Value"
    write_refrec rfile, temp
    Label1.Caption = "Reference number 1"
    recnum = 2
    currentref = 1
    Else
    Do Until EOCF(rfile.file)
      read_refrec rfile, temp
    Loop
    For i = 1 To poslen
      For j = 1 To poslen
      If refnums(j) = i Then Exit For
      Next
      If j >= poslen Then
      Label1.Caption = "Reference number " & Str(i)
      currentref = Val(i)
      Exit For
      End If
    Next
    If i >= poslen Then
      Label1.Caption = "Reference number " & Str(i + 1)
      currentref = Val(i + 1)
    End If
    End If
    Text1.Text = ""
    Text2.Text = ""
    SSCommand3.Visible = False
    SSCommand4.Visible = False
  End If
  End If
   Left = (Screen.Width - Width) / 2
   Top = (Screen.Height - Height) / 2
End Sub

Private Sub SSCommand1_Click()
  If refmode = 0 Then
  currentref = refnums(recnum)
  Else
  set_ref temp, Trim$(Str$(currentref)), Text1.Text, Text2.Text
  write_refrec rfile, temp
  End If
  close_ref rfile
  Unload refer
End Sub

Private Sub SSCommand2_Click()
  currentref = -1
  close_ref rfile
  Unload refer
End Sub

Private Sub SSCommand3_Click()
  Dim i As Integer
  If recnum > 1 Then
  recnum = recnum - 1
  Seek rfile.file.fnum, filepos(recnum)
  get_line rfile.file
  read_refrec rfile, temp
  Label1.Caption = "Reference number " & temp.idx
  Text1.Text = temp.abrv
  Text2.Text = temp.ref
  End If
End Sub

Private Sub SSCommand4_Click()
  Dim i As Integer
  If Not EOCF(rfile.file) Then
  recnum = recnum + 1
  If recnum <= poslen - 1 Then
    Seek rfile.file.fnum, filepos(recnum)
    get_line rfile.file
  End If
  read_refrec rfile, temp
  Label1.Caption = "Reference number " & temp.idx
  Text1.Text = temp.abrv
  Text2.Text = temp.ref
  End If
End Sub

Private Sub Text1_KeyPress(KeyAscii As Integer)
  If KeyAscii = vbKeyReturn Then
  KeyAscii = 0
  End If
End Sub

Private Sub Text2_KeyPress(KeyAscii As Integer)
  If KeyAscii = vbKeyReturn Then
  KeyAscii = 0
  End If
End Sub

