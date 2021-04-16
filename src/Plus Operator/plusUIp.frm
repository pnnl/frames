VERSION 5.00
Begin VB.Form PlusUIp 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Plus-SG Operator"
   ClientHeight    =   1530
   ClientLeft      =   150
   ClientTop       =   840
   ClientWidth     =   5400
   Icon            =   "plusUIp.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1530
   ScaleWidth      =   5400
   StartUpPosition =   3  'Windows Default
   Begin VB.TextBox Text1 
      Alignment       =   1  'Right Justify
      Height          =   288
      Left            =   3000
      TabIndex        =   4
      Tag             =   "flux"
      Top             =   840
      Width           =   1100
   End
   Begin VB.ComboBox Combo2 
      Height          =   315
      Left            =   3000
      Style           =   2  'Dropdown List
      TabIndex        =   1
      Tag             =   "yr"
      Top             =   120
      Width           =   2292
   End
   Begin VB.ComboBox Combo1 
      Height          =   315
      Left            =   4080
      Style           =   2  'Dropdown List
      TabIndex        =   0
      Tag             =   "fraction"
      Top             =   840
      Width           =   1212
   End
   Begin VB.Label Label1 
      BackColor       =   &H80000005&
      Caption         =   "Groundwater flow to surface water"
      Height          =   255
      Index           =   1
      Left            =   120
      TabIndex        =   3
      Top             =   840
      Width           =   2535
   End
   Begin VB.Label Label1 
      BackColor       =   &H80000005&
      Caption         =   "Incoming Module ID"
      Height          =   255
      Index           =   4
      Left            =   120
      TabIndex        =   2
      Top             =   120
      Width           =   2535
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
Attribute VB_Name = "PlusUIp"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim rate As Boolean
Dim oldMod As String
Dim temp As parmrec
Dim loadng As Boolean

Private Sub SizeLocal()
  If temp.idx3 > numsrc Then
    numsrc = temp.idx3
    ReDim Preserve src(numsrc) As srcMod
  End If
End Sub

Private Sub modfillet(Idx As Long)
  If temp.idx1 > m_numsrc Then
    m_numsrc = temp.idx1
    ReDim Preserve m_src(m_numsrc) As srcParm
  End If
  m_src(temp.idx1).fluxVal = convert(temp.cunit, temp.uunit, CDbl(temp.pval))
  m_src(temp.idx1).fluxUnt = temp.uunit
End Sub

Private Sub loadprm()
Dim i As Long
Dim j As Long
Dim m As Long
Dim mx As Long
Dim fle As parmfile

  rate = True
  Combo1.Tag = "m^3/yr"
  If argc > 5 Then
    If argv(argc - 6) = "/fraction" Then
      rate = False
      Combo1.Tag = "fraction"
    End If
  End If
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pName
          Case modName
            Loading.Gauge1.max = val(temp.idx1)
            Loading.Gauge1.Value = 0
            For i = 1 To temp.idx1
              Loading.update
              If read_parmrec(fle, temp) Then
                Select Case temp.pName
                  Case "name"
                    If temp.idx1 > m_numsrc Then
                      m_numsrc = temp.idx1
                      ReDim Preserve m_src(m_numsrc) As srcParm
                    End If
                    m_src(temp.idx1).name = temp.pval
                    m_src(temp.idx1).Idx = 0
                  Case "flux", "percentflux":
                    If temp.cunit = Combo1.Tag Then
                      modfillet 0
                    End If
                End Select
              End If
            Next
          Case "csm"
            mx = -1
            For i = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                'assumes modname will always occur before sink variables
                If temp.pName = "modid" And temp.pval = modName Then
                  mx = temp.idx2
                End If
                If mx > -1 And temp.idx2 = mx And temp.idx1 = siteIdx Then
                  Select Case temp.pName
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
          (InStr(src(i).type, "wff") Mod 4 = 1) And (InStr(src(i).qual, "aquifer")) Then
          m_src(j).Idx = i
          m_src(j).lbl = src(i).lbl
          Exit For
        End If
      Next
      If j > m_numsrc And _
          (InStr(src(i).type, "wff") Mod 4 = 1) And (InStr(src(i).qual, "aquifer")) Then
        m_numsrc = m_numsrc + 1
        ReDim Preserve m_src(m_numsrc) As srcParm
        m_src(m_numsrc).Idx = i
        m_src(m_numsrc).name = src(i).name
        m_src(m_numsrc).lbl = src(i).lbl
        m_src(m_numsrc).fluxUnt = Combo1.Tag
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
      m_src(i).fluxVal = Text1.Text
      m_src(i).fluxUnt = Combo1.Text
      Exit For
    End If
  Next
  For i = 1 To m_numsrc
    If m_src(i).lbl + " (" + m_src(i).name + ")" = tx Then
      set_unit Combo1, m_src(i).fluxUnt
      Text1.Text = m_src(i).fluxVal
      Exit For
    End If
  Next
End Sub

Private Sub Form_load()
Dim i As Integer
  
  StartModule PlusUIp, App.Title, 4
  Loading.Show
  loadng = True
'set conversion comboboxes

  get_conversion_items Combo1.Tag, Combo1

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
  Dim tmp As Double
  Dim fname As String
  Dim parm As parmrec
  Dim fle As parmfile
  
  'Added to save if module happens to be the preselected module
  For i = 1 To m_numsrc
    If m_src(i).lbl + " (" + m_src(i).name + ")" = Combo2.Text Then
      set_unit Combo1, m_src(i).fluxUnt
      m_src(i).fluxVal = Text1.Text
      Exit For
    End If
  Next
  
  'Error check
  If rate Then
    For i = 1 To m_numsrc
      If CDbl(val(m_src(i).fluxVal)) <= 0 Then
        MsgBox "Error! " & m_src(i).name & " Flux Constant must be greater than 0", vbExclamation, "Error"
        Exit Sub
      End If
    Next
  Else
    For i = 1 To m_numsrc
      tmp = CDbl(convert(m_src(i).fluxUnt, Combo1.Tag, val(m_src(i).fluxVal)))
      If tmp <= 0 Or tmp > 1 Then
        MsgBox "Error! " & m_src(i).name & " flux constant must be greater than 0 and less or equal to 1", vbExclamation, "Error"
        Exit Sub
      End If
    Next
  End If

  fname = RunName & ".GID"
  If open_parm(fle, fname, 1) Then
    
    cnt = 0
    For i = 1 To m_numsrc
      If m_src(i).Idx > 0 Then
        Combo2.ListIndex = cnt
        cnt = cnt + 1
        set_parm parm, "name", cnt, 0, 0, 0, 0, 0, 0, "N/A", "N/A", m_src(i).name
        write_parmrec fle, parm
        set_parm parm, Text1.Tag, cnt, 0, 0, 0, 0, 0, 0, m_src(i).fluxUnt, Combo1.Tag, convert(m_src(i).fluxUnt, Combo1.Tag, val(m_src(i).fluxVal))
        write_parmrec fle, parm
      End If
    Next
    set_parm parm, "fluxCount", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(cnt)
    write_parmrec fle, parm
    close_parm fle
  Else
    PutError "Unable to create transaction file" & RunName & ".GID"
  End If
  EndModule
End Sub

