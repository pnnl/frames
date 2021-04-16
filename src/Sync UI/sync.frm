VERSION 5.00
Begin VB.Form Sync1 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Temporal Operator"
   ClientHeight    =   5844
   ClientLeft      =   120
   ClientTop       =   804
   ClientWidth     =   5436
   Icon            =   "sync.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   5844
   ScaleWidth      =   5436
   StartUpPosition =   3  'Windows Default
   Begin VB.TextBox Text1 
      Alignment       =   1  'Right Justify
      Height          =   288
      Index           =   4
      Left            =   3000
      TabIndex        =   14
      Tag             =   "year"
      Top             =   120
      Width           =   1100
   End
   Begin VB.ComboBox Combo1 
      Height          =   288
      Index           =   3
      Left            =   4080
      Style           =   2  'Dropdown List
      TabIndex        =   13
      Tag             =   "percent"
      Top             =   1200
      Width           =   1212
   End
   Begin VB.TextBox Text1 
      Alignment       =   1  'Right Justify
      Height          =   288
      Index           =   3
      Left            =   3000
      TabIndex        =   11
      Tag             =   "error"
      Top             =   1200
      Width           =   1100
   End
   Begin VB.ComboBox Combo2 
      Height          =   288
      Left            =   3000
      Style           =   2  'Dropdown List
      TabIndex        =   10
      Tag             =   "yr"
      Top             =   2040
      Width           =   2292
   End
   Begin VB.TextBox Text1 
      Alignment       =   1  'Right Justify
      Height          =   288
      Index           =   2
      Left            =   3000
      TabIndex        =   7
      Tag             =   "size"
      Top             =   840
      Width           =   1100
   End
   Begin VB.ComboBox Combo1 
      Height          =   288
      Index           =   1
      Left            =   4080
      Style           =   2  'Dropdown List
      TabIndex        =   3
      Tag             =   "yr"
      Top             =   480
      Width           =   1212
   End
   Begin VB.TextBox Text1 
      Alignment       =   1  'Right Justify
      Height          =   288
      Index           =   1
      Left            =   3000
      TabIndex        =   2
      Tag             =   "interval"
      Top             =   480
      Width           =   1100
   End
   Begin VB.ComboBox Combo1 
      Height          =   288
      Index           =   0
      Left            =   4080
      Style           =   2  'Dropdown List
      TabIndex        =   1
      Tag             =   "yr"
      Top             =   2400
      Width           =   1212
   End
   Begin VB.TextBox Text1 
      Alignment       =   1  'Right Justify
      Height          =   288
      Index           =   0
      Left            =   3000
      TabIndex        =   0
      Tag             =   "starttime"
      Top             =   2400
      Width           =   1100
   End
   Begin VB.Label Label1 
      BackColor       =   &H80000005&
      Caption         =   "Start year"
      Height          =   252
      Index           =   5
      Left            =   240
      TabIndex        =   15
      Top             =   120
      Width           =   2412
   End
   Begin VB.Line Line1 
      X1              =   240
      X2              =   5280
      Y1              =   1680
      Y2              =   1680
   End
   Begin VB.Label Label1 
      BackColor       =   &H80000005&
      Caption         =   "Incoming Module ID"
      Height          =   252
      Index           =   4
      Left            =   240
      TabIndex        =   12
      Top             =   2040
      Width           =   2412
   End
   Begin VB.Label Label1 
      BackColor       =   &H80000005&
      Caption         =   "Acceptable Error"
      Height          =   252
      Index           =   3
      Left            =   240
      TabIndex        =   9
      Top             =   1200
      Width           =   2412
   End
   Begin VB.Label Label1 
      BackColor       =   &H80000005&
      Caption         =   "Number of Intervals"
      Height          =   252
      Index           =   2
      Left            =   240
      TabIndex        =   8
      Top             =   840
      Width           =   2412
   End
   Begin VB.Label Label2 
      Caption         =   $"sync.frx":0442
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.6
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   2772
      Left            =   240
      TabIndex        =   6
      Top             =   2880
      Width           =   5052
   End
   Begin VB.Label Label1 
      BackColor       =   &H80000005&
      Caption         =   "Interval size"
      Height          =   252
      Index           =   1
      Left            =   240
      TabIndex        =   5
      Top             =   480
      Width           =   2412
   End
   Begin VB.Label Label1 
      BackColor       =   &H80000005&
      Caption         =   "Start time offset"
      Height          =   252
      Index           =   0
      Left            =   480
      TabIndex        =   4
      Top             =   2400
      Width           =   2172
   End
   Begin VB.Menu file 
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
      Begin VB.Menu about 
         Caption         =   "&About"
      End
   End
End
Attribute VB_Name = "Sync1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim oldMod As String
Dim temp As parmrec
Dim loadng As Boolean

Private Sub SizeLocal()
  If temp.idx3 > numsrc Then
    numsrc = temp.idx3
    ReDim Preserve src(numsrc) As srcMod
  End If
End Sub

Private Sub fillet(Idx As Long)
  On Error Resume Next

  If temp.cunit <> "N/A" Then set_unit Combo1(Idx), temp.uunit
  Text1(Idx) = convert(temp.cunit, temp.uunit, val(temp.pval))
End Sub

Private Sub modfillet(Idx As Long)
  If temp.idx1 > m_numsrc Then
    m_numsrc = temp.idx1
    ReDim Preserve m_src(m_numsrc) As srcParm
  End If
  m_src(temp.idx1).offsetVal = convert(temp.cunit, temp.uunit, CDbl(temp.pval))
  m_src(temp.idx1).offsetUnt = temp.uunit
End Sub

Private Sub loadprm()
Dim i As Long
Dim j As Long
Dim m As Long
Dim mx As Long
Dim fle As parmfile

  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pname
          Case modName
            Loading.Gauge1.max = val(temp.idx1)
            Loading.Gauge1.Value = 0
            For i = 1 To temp.idx1
              Loading.update
              If read_parmrec(fle, temp) Then
                Select Case temp.pname
                  Case "name"
                    If temp.idx1 > m_numsrc Then
                      m_numsrc = temp.idx1
                      ReDim Preserve m_src(m_numsrc) As srcParm
                    End If
                    m_src(temp.idx1).name = temp.pval
                    m_src(temp.idx1).Idx = 0
                  Case "starttime":            modfillet 0
                  Case "interval":             fillet 1
                  Case "size":                 fillet 2
                  Case "year":                 fillet 4
                  Case "error":                fillet 3
                End Select
              End If
            Next
          Case "csm"
            mx = -1
            For i = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                'assumes modname will always occur before sink variables
                If temp.pname = "modid" And temp.pval = modName Then
                  mx = temp.idx2
                End If
                If mx > -1 And temp.idx2 = mx And temp.idx1 = siteIdx Then
                  Select Case temp.pname
                    Case "moddespath"
                      DesName = temp.pval
                    Case "modsrcid"
                      SizeLocal
                      src(temp.idx3).name = temp.pval
                    Case "modsrclabel"
                      SizeLocal
                      src(temp.idx3).lbl = temp.pval
                    Case "modsrctype"
                      SizeLocal
                      src(temp.idx3).type = temp.pval
                    Case "modsrcqual"
                      SizeLocal
                      src(temp.idx3).qual = temp.pval
                  End Select
                End If
              End If
            Next
          Case Else
            For i = 1 To temp.idx1
              get_line fle.file
            Next
        End Select
      End If
    Loop
    close_parm fle
  
    For i = 1 To numsrc
      For j = 1 To m_numsrc
        If m_src(j).name = src(i).name And _
          (InStr(src(i).type, "wcf") Mod 4 = 1 Or _
           InStr(src(i).type, "scf") Mod 4 = 1) Then
          m_src(j).Idx = i
          m_src(j).lbl = src(i).lbl
          Exit For
        End If
      Next
      If j > m_numsrc And _
          (InStr(src(i).type, "wcf") Mod 4 = 1 Or _
           InStr(src(i).type, "scf") Mod 4 = 1) Then
        m_numsrc = m_numsrc + 1
        ReDim Preserve m_src(m_numsrc) As srcParm
        m_src(m_numsrc).Idx = i
        m_src(m_numsrc).name = src(i).name
        m_src(m_numsrc).lbl = src(i).lbl
        m_src(m_numsrc).offsetUnt = "day"
      End If
    Next
    
    For i = 1 To m_numsrc
      If m_src(i).Idx > 0 Then Combo2.AddItem m_src(i).lbl + " (" + m_src(i).name + ")"
    Next
    If m_numsrc > 0 Then Combo2.ListIndex = 0
  
  
  Else
    PutError "Can't find or open file " & FUIName
    EndModule
  End If
End Sub

Private Sub about_Click()
  frmAbout.Show 1, Me
End Sub

Private Sub Combo2_Click()
  modchange Combo2.Text, oldMod
  oldMod = Combo2.Text
End Sub

Private Sub modchange(tx As String, oldtx As String)
  Dim i As Long
  
  For i = 1 To m_numsrc
    If m_src(i).lbl + " (" + m_src(i).name + ")" = oldtx Then
      m_src(i).offsetVal = Text1(0).Text
      m_src(i).offsetUnt = Combo1(0).Text
      Exit For
    End If
  Next
  For i = 1 To m_numsrc
    If m_src(i).lbl + " (" + m_src(i).name + ")" = tx Then
      set_unit Combo1(0), m_src(i).offsetUnt
      Text1(0).Text = m_src(i).offsetVal
      Exit For
    End If
  Next
End Sub

Private Sub Form_load()
Dim i As Integer
  
  StartModule Sync1, App.Title, 5
  Loading.Show
  loadng = True
'set conversion comboboxes
  For i = 0 To 3
    If i = 2 Then i = 3
    get_conversion_items Combo1(i).Tag, Combo1(i)
  Next
  set_unit Combo1(0), "day"
  set_unit Combo1(1), "day"
  loadprm
  loadng = False
  Unload Loading
End Sub

Private Sub Form_Unload(Cancel As Integer)
  Dim answer As Long
  If Not called Then
    answer = MsgBox("Do you want to save changes?", 51, App.Title)
    If answer = 6 Then save_Click
    If answer = 7 Then
      EndModule
    End If
    If answer = 2 Then Cancel = 1
  End If
End Sub

Private Sub leave_Click()
  Form_Unload 0
End Sub

Private Sub save_Click()
  Dim i As Long
  Dim cnt As Long
  Dim fname As String
  Dim parm As parmrec
  Dim fle As parmfile

  fname = RunName & ".GID"
  If open_parm(fle, fname, 1) Then
    
    cnt = 0
    For i = 1 To m_numsrc
      If m_src(i).Idx > 0 Then
        Combo2.ListIndex = cnt
        cnt = cnt + 1
        set_parm parm, "name", cnt, 0, 0, 0, 0, 0, 0, "N/A", "N/A", m_src(i).name
        write_parmrec fle, parm
        set_parm parm, Text1(0).Tag, cnt, 0, 0, 0, 0, 0, 0, m_src(i).offsetUnt, Combo1(0).Tag, convert(m_src(i).offsetUnt, Combo1(0).Tag, val(m_src(i).offsetVal))
        write_parmrec fle, parm
      End If
    Next
    
    For i = 1 To 4
      If i = 2 Or i = 4 Then
        set_parm parm, Text1(i).Tag, 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(CInt(Text1(i)))
        write_parmrec fle, parm
      Else
        set_parm parm, Text1(i).Tag, 0, 0, 0, 0, 0, 0, 0, Combo1(i).Text, Combo1(i).Tag, convert(Combo1(i).Text, Combo1(i).Tag, val(Text1(i)))
        write_parmrec fle, parm
      End If
    Next
    close_parm fle
  Else
    PutError "Unable to create transaction file" & RunName & ".GID"
  End If
  EndModule
End Sub
