VERSION 5.00
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Begin VB.Form Dose 
   BorderStyle     =   1  'Fixed Single
   ClientHeight    =   3864
   ClientLeft      =   2316
   ClientTop       =   1836
   ClientWidth     =   7680
   Icon            =   "Dose.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   NegotiateMenus  =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   322
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   640
   StartUpPosition =   2  'CenterScreen
   Begin VB.TextBox mes 
      Alignment       =   2  'Center
      BackColor       =   &H00C0FFFF&
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   9.6
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   360
      Left            =   0
      Locked          =   -1  'True
      TabIndex        =   29
      TabStop         =   0   'False
      Top             =   3480
      Width           =   7692
   End
   Begin Threed.SSFrame frm 
      Height          =   3375
      Index           =   0
      Left            =   120
      TabIndex        =   0
      Top             =   0
      Width           =   7455
      _Version        =   65536
      _ExtentX        =   13150
      _ExtentY        =   5953
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
      Font3D          =   2
      ShadowStyle     =   1
      Begin VB.ComboBox inhale 
         BackColor       =   &H00FFFFFF&
         Height          =   315
         ItemData        =   "Dose.frx":030A
         Left            =   4080
         List            =   "Dose.frx":0314
         Style           =   2  'Dropdown List
         TabIndex        =   28
         Tag             =   "inhale"
         Top             =   2865
         Width           =   1955
      End
      Begin VB.ComboBox unit 
         BackColor       =   &H00FFFFFF&
         Height          =   315
         Index           =   5
         Left            =   5040
         Style           =   2  'Dropdown List
         TabIndex        =   25
         Tag             =   "yr"
         Top             =   2505
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         BackColor       =   &H00FFFFFF&
         Height          =   288
         Index           =   4
         Left            =   5040
         Style           =   2  'Dropdown List
         TabIndex        =   21
         Tag             =   "yr"
         Top             =   2160
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         BackColor       =   &H00FFFFFF&
         Height          =   288
         Index           =   3
         Left            =   5040
         Style           =   2  'Dropdown List
         TabIndex        =   17
         Tag             =   "L/day"
         Top             =   1800
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         BackColor       =   &H00FFFFFF&
         Height          =   288
         Index           =   2
         Left            =   5040
         Style           =   2  'Dropdown List
         TabIndex        =   13
         Tag             =   "L/day"
         Top             =   1440
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         BackColor       =   &H00FFFFFF&
         Height          =   288
         Index           =   0
         Left            =   5040
         Style           =   2  'Dropdown List
         TabIndex        =   3
         Tag             =   "kg"
         Top             =   360
         Width           =   1000
      End
      Begin VB.ComboBox unit 
         BackColor       =   &H00FFFFFF&
         Height          =   288
         Index           =   1
         Left            =   5040
         Style           =   2  'Dropdown List
         TabIndex        =   7
         Tag             =   "yr"
         Top             =   720
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00C0FFC0&
         Height          =   312
         Index           =   5
         Left            =   4080
         TabIndex        =   24
         Tag             =   "tage2"
         Text            =   "70"
         Top             =   2520
         Width           =   1000
      End
      Begin VB.Timer Timer1 
         Interval        =   100
         Left            =   6960
         Top             =   2400
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00C0FFC0&
         Height          =   312
         Index           =   4
         Left            =   4068
         TabIndex        =   20
         Tag             =   "tage1"
         Text            =   "0"
         Top             =   2160
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00C0FFC0&
         Height          =   312
         Index           =   3
         Left            =   4080
         TabIndex        =   16
         Tag             =   "udwsw"
         Text            =   "2.0"
         Top             =   1800
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00C0FFC0&
         Height          =   312
         Index           =   2
         Left            =   4068
         TabIndex        =   12
         Tag             =   "udwgw"
         Text            =   "2.0"
         Top             =   1440
         Width           =   1000
      End
      Begin VB.ComboBox kexpth 
         BackColor       =   &H00FFFFFF&
         Height          =   315
         ItemData        =   "Dose.frx":0339
         Left            =   4080
         List            =   "Dose.frx":0343
         Style           =   2  'Dropdown List
         TabIndex        =   10
         Tag             =   "kexpth"
         Top             =   1080
         Width           =   1955
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00C0FFC0&
         Height          =   312
         Index           =   0
         Left            =   4080
         TabIndex        =   2
         Tag             =   "BODYWT"
         Text            =   "70"
         Top             =   360
         Width           =   1000
      End
      Begin VB.TextBox txt 
         Alignment       =   1  'Right Justify
         BackColor       =   &H008080FF&
         Height          =   312
         Index           =   1
         Left            =   4080
         TabIndex        =   6
         Tag             =   "EXPDUR"
         Top             =   720
         Width           =   1000
      End
      Begin VB.Label Label2 
         Caption         =   "Method for inhalation impact analysis -- HE-INHAL"
         Height          =   255
         Left            =   120
         TabIndex        =   27
         Tag             =   "HEINHAL"
         Top             =   2880
         Width           =   3795
      End
      Begin VB.Label lbl 
         Caption         =   "Age upper bound for individual -- IC-TAGE2"
         Height          =   252
         Index           =   5
         Left            =   120
         TabIndex        =   23
         Tag             =   "ICTAGE2"
         Top             =   2520
         Width           =   3804
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   5
         Left            =   6120
         TabIndex        =   26
         Tag             =   "0"
         Top             =   2556
         Width           =   996
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   4
         Left            =   6120
         TabIndex        =   22
         Tag             =   "0"
         Top             =   2196
         Width           =   996
      End
      Begin VB.Label lbl 
         Caption         =   "Age lower bound for individual -- IC-TAGE1"
         Height          =   252
         Index           =   4
         Left            =   120
         TabIndex        =   19
         Tag             =   "ICTAGE1"
         Top             =   2160
         Width           =   3804
      End
      Begin VB.Label lbl 
         Caption         =   "Surface water ingestion rate -- IW-UDWSW"
         Height          =   252
         Index           =   3
         Left            =   120
         TabIndex        =   15
         Tag             =   "IWUDWSW"
         Top             =   1800
         Width           =   3804
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   3
         Left            =   6120
         TabIndex        =   18
         Tag             =   "0"
         Top             =   1836
         Width           =   996
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   2
         Left            =   6120
         TabIndex        =   14
         Tag             =   "0"
         Top             =   1476
         Width           =   996
      End
      Begin VB.Label lbl 
         Caption         =   "Ground water ingestion rate -- IG-UDWGW"
         Height          =   252
         Index           =   2
         Left            =   120
         TabIndex        =   11
         Tag             =   "IGUDWGW"
         Top             =   1440
         Width           =   3804
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   0
         Left            =   6120
         TabIndex        =   4
         Tag             =   "0"
         Top             =   396
         Width           =   996
      End
      Begin VB.Label lbl 
         Caption         =   "Body weight of individual -- IC-BODYWT"
         Height          =   252
         Index           =   0
         Left            =   120
         TabIndex        =   1
         Tag             =   "ICBODYWT"
         Top             =   360
         Width           =   3800
      End
      Begin VB.Label ref 
         Caption         =   "Ref: 0"
         Height          =   252
         Index           =   1
         Left            =   6120
         TabIndex        =   8
         Tag             =   "0"
         Top             =   756
         Width           =   996
      End
      Begin VB.Label lbl 
         Caption         =   "Exposure duration -- IC-EXPDUR"
         Height          =   252
         Index           =   1
         Left            =   120
         TabIndex        =   5
         Tag             =   "ICEXPDUR"
         Top             =   720
         Width           =   3800
      End
      Begin VB.Label Label1 
         Caption         =   "Water dermal absorbtion model -- IC-DERM"
         Height          =   252
         Left            =   120
         TabIndex        =   9
         Tag             =   "ICDERM"
         Top             =   1080
         Width           =   3800
      End
   End
   Begin VB.Menu file 
      Caption         =   "&File"
      WindowList      =   -1  'True
      Begin VB.Menu setdef 
         Caption         =   "Reset &Defaults"
      End
      Begin VB.Menu save 
         Caption         =   "&Save and Exit"
      End
      Begin VB.Menu leave 
         Caption         =   "E&xit"
      End
   End
   Begin VB.Menu noact 
      Caption         =   "&Reference"
      Begin VB.Menu addref 
         Caption         =   "&Add"
      End
      Begin VB.Menu selref 
         Caption         =   "Se&lect"
      End
   End
   Begin VB.Menu custom 
      Caption         =   "&Customize"
   End
   Begin VB.Menu NumFormat 
      Caption         =   "For&mat"
      Visible         =   0   'False
      Begin VB.Menu NumGeneral 
         Caption         =   "General"
      End
      Begin VB.Menu NumStandard 
         Caption         =   "Standard"
      End
      Begin VB.Menu NumFixed 
         Caption         =   "Fixed"
      End
      Begin VB.Menu NumScientific 
         Caption         =   "Scientific"
      End
      Begin VB.Menu NumUser 
         Caption         =   "User Defined"
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
Attribute VB_Name = "Dose"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text
Dim temp As parmrec
Dim exp_cnt As Long
Dim srcname() As String
Dim src_cnt As Long
Dim exp() As exprec

Private Sub about_Click()
  frmAbout.Show 1
End Sub

Private Sub Form_Unload(Cancel As Integer)
  Dim answer As Long
  If Not called Then
    answer = MsgBox("Do you want to save changes?", 51, App.Title)
    If answer = 6 Then save_Click
    If answer = 7 Then EndModule
    If answer = 2 Then Cancel = 1
  End If
End Sub

Private Sub howto_Click()
  GetHelp
End Sub

Private Sub leave_Click()
  Form_Unload 0
End Sub

'Subs of the Format menu
Private Sub NumFixed_Click()
    CVTFormat = "Fixed"
    FormatChecked (CVTFormat)
     'tell user how number will appear
     MsgBox "Displays at least one digit to the left and two " + vbCrLf + _
            "digits to the right of the decimal separator.", vbOKOnly, "Fixed Number Format Selected"
End Sub
Private Sub NumGeneral_Click()
    CVTFormat = "General Number"
    FormatChecked (CVTFormat)
    'tell user how number will appear
    MsgBox "Displays number with no thousand separator.", vbOKOnly, "General Number Format Selected"
End Sub
Private Sub NumScientific_Click()
    CVTFormat = "Scientific"
    FormatChecked (CVTFormat)
    'tell user how number will appear
    MsgBox "Uses standard scientific notation", vbOKOnly, "Scientific Number Format Selected"
End Sub
Private Sub NumStandard_Click()
    CVTFormat = "Standard"
    FormatChecked (CVTFormat)
    'tell user how number will appear
    MsgBox "Displays number with thousand separator, at least " + vbCrLf + _
           "one digit to the left and two digits to the " + vbCrLf + _
           "right of the decimal separator.", vbOKOnly, "Standard Number Format Selected"
End Sub
Private Sub NumUser_Click()
Dim UserStrg As String
Dim PreviousStrg As String
Dim OkayStr As Boolean
                
    UserStrg = InputBox("Enter your format in one of these styles:  " + vbCrLf + vbCrLf + _
        vbTab + "  ##0.0##  OR  0.0##E+00" + _
        vbCrLf + vbCrLf + "The '#' character may be used as many times as " + vbCrLf + _
        "desired (where indicated), including zero times.  " + vbCrLf + vbCrLf + _
        "Current format is " + CVTFormat, "User Defined Number Format")

    If UserStrg = "" Then Exit Sub
    UserStrg = UCase(UserStrg)
    OkayStr = CheckUserDefinedFormat(UserStrg)       'basic check of characters
    If OkayStr Then
        CVTFormat = UserStrg
        FormatChecked (CVTFormat)
    End If
End Sub
Private Sub FormatChecked(FormatStr As String)
    NumFixed.Checked = False
    NumGeneral.Checked = False
    NumStandard.Checked = False
    NumScientific.Checked = False
    NumUser.Checked = False
    Select Case FormatStr
        Case "Standard"
            NumStandard.Checked = True
        Case "Fixed"
            NumFixed.Checked = True
        Case "General Number"
            NumGeneral.Checked = True
        Case "Scientific"
            NumScientific.Checked = True
        Case Else
            NumUser.Checked = True
        End Select
End Sub
'end format menu

Private Sub selref_Click()
  RefMode = 0
  GetRef ref(RefItem)
End Sub

Private Sub addref_Click()
  RefMode = 1
  GetRef ref(RefItem)
End Sub

Private Sub custom_Click()
  Pathway.Show 1
End Sub

Private Sub fillet(idx As Long)
  ref(idx).Tag = temp.ref
  ref(idx).Caption = "Ref:" & Str(temp.ref)
  On Error Resume Next
  If temp.cunit <> "N/A" Then set_unit unit(idx), temp.uunit
  txt(idx).Text = convert(temp.cunit, temp.uunit, Val(temp.pval))
End Sub

Sub addsrc()
  If temp.idx3 > src_cnt Then
    src_cnt = temp.idx3
    ReDim Preserve srcname(src_cnt) As String
  End If
End Sub

Sub addexp()
  If temp.idx2 > exp_cnt Then
    exp_cnt = temp.idx2
    ReDim Preserve exp(exp_cnt) As exprec
  End If
End Sub

Private Sub loaddef()
  Dim i As Long
  Dim j As Long
  Dim m As Long
  Dim numref As Long
  Dim abrv As String
  Dim refer As String
  Dim pre As String
  Dim name As String
  Dim fle As csv
  
  On Error GoTo ErrorHandler
  
  If open_csv(fle, App.Path & "\meprcp.rf_", F_READ) Then
    'load references inot reference file
    RefMode = 1
    Reference.Hide
    numref = Val(get_val(fle))
    get_line fle
    ReDim refs(numref) As Long
    For i = 1 To numref
      j = Val(get_val(fle))
      If j > UBound(refs) Then ReDim refs(j) As Long
      abrv = get_val(fle)
      refer = get_val(fle)
      refs(j) = Reference.add_ref(abrv, refer)
      get_line fle
    Next
    Unload Reference
    
    numref = Val(get_val(fle))
    get_line fle
    For i = 1 To numref
      temp.pname = get_val(fle)
      temp.ref = refs(Val(get_val(fle)))
      temp.pval = get_val(fle)
      temp.cunit = get_val(fle)
      get_line fle
      
      m = -1
      pre = Left(temp.pname, 2)
      name = Right(temp.pname, Len(temp.pname) - 3)
      Select Case name
        Case "bodywt":    m = 0
        Case "expdur":    m = 1
        Case "udwgw":     m = 2
        Case "udwsw":     m = 3
        Case "tage1":     m = 4
        Case "tage2":     m = 5
      End Select
      If m > -1 Then
        ref(m).Tag = temp.ref
        ref(m).Caption = "Ref:" & Str(temp.ref)
        txt(m).Text = temp.pval
        If temp.cunit <> "" And temp.cunit <> "fraction" Then set_unit unit(m), temp.cunit
      End If
    Next
    close_csv fle
  Else
    MsgBox "Can't find or open file default reference file", vbExclamation
  End If
  
ErrorHandler:
  If Err.Number <> 0 Then
    MsgBox Err.Description, vbOKOnly, "loadref"
  End If
End Sub

Private Sub loadprm()
  Dim i As Long
  Dim j As Long
  Dim k As Long
  Dim m As Long
  Dim fle As parmfile
  Dim sval As Boolean
   
  If open_parm(fle, FUIName, 2) Then
    Do Until EOCF(fle.file)
      If read_parmrec(fle, temp) Then
        Select Case temp.pname
          Case "fui"
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                If temp.idx1 = siteIdx Then
                  Select Case temp.pname
                    Case "rcpdespath"
                      DesName = temp.pval
                  End Select
                End If
              End If
            Next
          Case modName
            Loading.Gauge1.Max = Val(temp.idx1)
            Loading.Gauge1.Value = 0
            For m = 1 To temp.idx1
              If read_parmrec(fle, temp) Then
                Loading.update
                Select Case temp.pname
                  Case "CVTFormat":    CVTFormat = "General Number" 'CVTFormat = temp.pval
                  Case "bodywt":    fillet 0
                  Case "expdur":    fillet 1
                  Case "udwgw":     fillet 2
                  Case "udwsw":     fillet 3
                  Case "tage1":     fillet 4
                  Case "tage2":     fillet 5
                  Case "kexpth":    kexpth.ListIndex = Val(temp.pval) - 1
                  Case "inhale":    inhale.ListIndex = Val(temp.pval)
                End Select
              End If
            Next
          Case Else
            For m = 1 To temp.idx1
              get_line fle.file
            Next
        End Select
      End If
    Loop
    close_parm fle
    SetFormat Dose
  Else
    PutError "Can't find or open file " & FUIName
    EndModule
  End If
End Sub

Public Sub Enable_Boxes(Index As Long, kpath As Long)
  lbl(Index).Enabled = kpath
  txt(Index).Enabled = kpath
  unit(Index).Enabled = kpath
  ref(Index).Enabled = kpath
End Sub

Private Sub Form_load()
Dim i As Long
Dim fle As parmfile
  
  StartModule Dose, App.Title, 5
  SetHelpFile App.Path + "\rcp.htm"
  SetRefFile ReplaceExt(FUIName, "ref")
  'set conversion comboboxes
  For i = 0 To 5
    get_conversion_items unit(i).Tag, unit(i)
  Next
  er 4
  UsedCustomized = False
  inhale.ListIndex = 0
  kexpth.ListIndex = 0
  Loading.Show
  If open_parm(fle, FUIName, 2) Then
    If FindSection(fle, modName) = 0 Then
      loaddef
    End If
    close_parm fle
  End If
  loadprm
  Unload Loading
  Refresh
End Sub

Private Sub save_Click()
  Dim i As Long
  Dim fname As String
  Dim parm As parmrec
  Dim tmp As parmfile
  Dim fle As parmfile
  Dim UserFormat As String
  UserFormat = CVTFormat
   
  
  fname = RunName & ".GID"
  If open_parm(fle, fname, 1) Then
    set_parm parm, "CVTFormat", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CVTFormat
    write_parmrec fle, parm
    CVTFormat = "General Number"
  
    InitConstants
    For i = 1 To 25
      set_parm parm, "Pathways", i, 0, 0, 0, 0, 0, 0, "N/A", "N/A", paths(i)
      write_parmrec fle, parm
    Next
    For i = 1 To 2
      set_parm parm, "AquFood", i, 0, 0, 0, 0, 0, 0, "N/A", "N/A", af(i)
      write_parmrec fle, parm
    Next
    For i = 1 To 4
      set_parm parm, "TerFood", i, 0, 0, 0, 0, 0, 0, "N/A", "N/A", tf(i)
      write_parmrec fle, parm
    Next
    For i = 1 To 4
      set_parm parm, "Activity", i, 0, 0, 0, 0, 0, 0, "N/A", "N/A", act(i)
      write_parmrec fle, parm
    Next
    
    set_parm parm, "KEXPTH", 2, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(kexpth.ItemData(kexpth.ListIndex))
    write_parmrec fle, parm
    set_parm parm, "KEXPTH", 11, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(kexpth.ItemData(kexpth.ListIndex))
    write_parmrec fle, parm
    set_parm parm, "INHALE", 0, 0, 0, 0, 0, 0, 0, "N/A", "N/A", CStr(inhale.ItemData(inhale.ListIndex))
    write_parmrec fle, parm
        
    For i = 0 To 5
      If i = 2 Then i = 4
      If er(i) Then PutError "Parameter " & lbl(i).Tag & " is invalid"
      set_parm parm, UCase(txt(i).Tag), 0, 0, 0, 0, 0, 0, ref(i).Tag, unit(i).Text, unit(i).Tag, convert(unit(i).Text, unit(i).Tag, Val(txt(i)))
      write_parmrec fle, parm
    Next
  
  'ground water settings
      If er(2) Then PutError "Parameter " & lbl(2).Tag & " is invalid"
      set_parm parm, UCase(txt(2).Tag), 0, 0, 0, 0, 0, 0, ref(2).Tag, unit(2).Text, unit(2).Tag, convert(unit(2).Text, unit(2).Tag, Val(txt(2)))
      write_parmrec fle, parm
  
  'surface water settings
      If er(3) Then PutError "Parameter " & lbl(3).Tag & " is invalid"
      set_parm parm, UCase(txt(3).Tag), 0, 0, 0, 0, 0, 0, ref(3).Tag, unit(3).Text, unit(3).Tag, convert(unit(3).Text, unit(3).Tag, Val(txt(3)))
      write_parmrec fle, parm
  
    If Not (UsedCustomized) Then
      Pathway.Form_load
      Pathway.save_Click
    End If
    fname = RunName & ".~rp"
    If open_parm(tmp, fname, 2) Then
      read_parmrec tmp, parm
      Do Until EOCF(tmp.file)
        If read_parmrec(tmp, parm) Then
          write_parmrec fle, parm
        End If
      Loop
      close_parm tmp
      Kill fname
    End If
    close_parm fle
    CVTFormat = UserFormat
  Else
    PutError "Unable to create transaction file" & RunName & ".GID"
  End If
  EndModule
End Sub
'
'Private Sub meta_Click()
'
'  If opendes(App.Path + "\hazrcp.des") Then
'
'    put_val des, "mf"
'    put_val des, "version 2.0 beta"
'    put_line des
'    put_val des, "Receptor Intake"
'    put_val des, "Mepas Chronic Intake Mocule"
'    put_val des, "rcp4.exe"
'    put_val des, "hazrcp.bat"
'    put_line des
'    des.putbuff = """MEPAS 4.0 Chronic Intake Module" + Chr(13) + Chr(10) + Chr(13) + Chr(10) + _
'         "The MEPAS chronic intake module evaluates the" + Chr(13) + Chr(10) + _
'         "intake or exposure of an individual from consumption" + Chr(13) + Chr(10) + _
'         "or contact with contaminated media, or exposure to" + Chr(13) + Chr(10) + _
'         "radiation in contaminated media.  Standard EPA" + Chr(13) + Chr(10) + _
'         "methods are used to evaluate the average daily" + Chr(13) + Chr(10) + _
'         "intake rate of chemical pollutants for each exposure" + Chr(13) + Chr(10) + _
'         "pathway, based on user defined consumption/contact" + Chr(13) + Chr(10)
'
'    des.putbuff = des.putbuff + _
'         "rates and body weight. The module includes" + Chr(13) + Chr(10) + _
'         "consideration of domestic water use, farm product" + Chr(13) + Chr(10) + _
'         "consumption, aquatic food consumption, surface water" + Chr(13) + Chr(10) + _
'         "recreational activities, soil contact exposure, and" + Chr(13) + Chr(10) + _
'         "air exposures.  Both chemical and radioactive" + Chr(13) + Chr(10) + _
'         "pollutants may be evaluated.  EPA models are used" + Chr(13) + Chr(10) + _
'         "to evaluate dermal contact with soil and water." + Chr(13) + Chr(10) + _
'         "The module evaluates intakes for one age group per" + Chr(13) + Chr(10)
'    des.putbuff = des.putbuff + _
'         "receptor definition and for all input exposure routes" + Chr(13) + Chr(10) + _
'         "defined in the EPF file and recognized by MEPAS." + Chr(13) + Chr(10) + Chr(13) + Chr(10)
'
'    des.putbuff = des.putbuff + _
'         "Limitations:  None known at this time." + Chr(13) + Chr(10) + Chr(13) + Chr(10)
'
'    des.putbuff = des.putbuff + _
'         "Reference:" + Chr(13) + Chr(10) + _
'         "    Strenge, D.L., and P.J. Chamberlain.  1995." + Chr(13) + Chr(10) + _
'         "    Multimedia Environmental Pollutant Assessment System (MEPAS):" + Chr(13) + Chr(10) + _
'         "    Exposure Pathway and Human Health Impact Assessment Models." + Chr(13) + Chr(10) + _
'         "    PNL-10523.  Pacific Northwest Laboratory, Richland, WA." + Chr(13) + Chr(10) + Chr(13) + Chr(10)
'
'    des.putbuff = des.putbuff + _
'         "Point of Contact:" + Chr(13) + Chr(10) + _
'         "    Pacific Northwest National Laboratory" + Chr(13) + Chr(10) + _
'         "    Mitch Pelton" + Chr(13) + Chr(10) + _
'         "    P.O. Box 999 MS K6-80" + Chr(13) + Chr(10) + _
'         "    Richland WA 99352" + Chr(13) + Chr(10) + _
'         "    EMail mitch.pelton@pnl.gov"""
'    put_line des
'
'    put_val des, 2
'    put_line des
'
'    putfile EPF, 1
'    putfile RIF, 2
'
'' count the number of entries below and in the pathway module and place that number here
'    put_val des, 44
'    put_line des
'    putmeta "KEXPTH", "NOT STOCHASTIC", "N/A", "", 1, 0, 1
'    putlabel "Pathway #", "Index1"
'
'    putmeta UCase(txt(0).Tag), "CONTINUOUS", unit(0).Tag, lbl(0).Caption, 0, 1, 100
'    putmeta UCase(txt(1).Tag), "CONTINUOUS", unit(1).Tag, lbl(1).Caption, 0, 1, 100
'
''Ingestion rates
'    putmeta UCase(txt(3).Tag), "CONTINUOUS", unit(3).Tag, lbl(3).Caption, 0, 0, 10
'    putmeta UCase(txt(10).Tag), "CONTINUOUS", unit(10).Tag, lbl(10).Caption, 0, 0, 10
'    putmeta UCase(txt(24).Tag), "CONTINUOUS", unit(24).Tag, lbl(24).Caption, 0, 0, 10
'    putmeta UCase(txt(30).Tag), "CONTINUOUS", unit(24).Tag, lbl(30).Caption, 0, 0, 10
'
''Frequency
'    putmeta UCase(txt(31).Tag), "CONTINUOUS", "N/A", "Frequecy of occurance", 1, 0, 1
'    putlabel "Pathway #", "Index1"
'
'    Pathway.save_metadata
'    closedes
'  Else
'    MsgBox "Unable to create description file"
'  End If
'
'End Sub

Private Sub setdef_Click()
  loaddef
End Sub

Private Sub Timer1_Timer()
  mes = Mid(mes, 2) & Mid(mes, 1, 1)
End Sub

Private Sub unit_Click(Index As Integer)
  er CLng(Index)
End Sub

Private Function er(Index As Long, Optional enable As Boolean = True) As Boolean
  Dim tval As Double
  Dim t1 As String
  Dim t2 As String
  Dim m As String
  
  m = ""
  er = False
  tval = Val(txt(Index).Text)
  If txt(Index).Text = "" Then er = True
  Select Case Index
    Case 5
      t1 = convert(unit(4).Text, unit(Index).Text, Val(txt(4).Text))
      t2 = convert(unit(Index).Tag, unit(Index).Text, 100)
      m = "Value must be greater than TAGE1, " + t1 + ", and less than " + t2 + " " + unit(Index).Text
      If (tval <= t1 Or tval >= Val(t2)) Then er = True
    Case 4
      t1 = convert(unit(5).Text, unit(Index).Text, Val(txt(5).Text))
      m = "Value must be greater than or equal to 0 and less than TAGE2, " + t1 + " " + unit(Index).Text
      If (tval < 0 Or tval >= Val(t1)) Then er = True
    Case 2, 3
      t1 = convert(unit(Index).Tag, unit(Index).Text, 10)
      m = "Value must be between 0 and " + t1 + " " + unit(Index).Text
      If (tval <= 0 Or tval > Val(t1)) Then er = True
    Case 0, 1
      t1 = convert(unit(Index).Tag, unit(Index).Text, 1)
      t2 = convert(unit(Index).Tag, unit(Index).Text, 100)
      m = "Value must be between " + t1 + " and " + t2 + " " + unit(Index).Text
      If (tval < Val(t1) Or tval > Val(t2)) Then er = True
  End Select
  mes = Space(140 - Len(m)) & m
  If enable Then
    If er Then
      txt(Index).BackColor = &H8080FF
    Else
      txt(Index).BackColor = &HC0FFC0
    End If
  Else
    txt(Index).BackColor = &HE0E0E0
  End If
End Function

Private Sub txt_Change(Index As Integer)
Dim chk As Double
On Error GoTo toolarge
  If Index = 4 Then er 5
  If Index = 5 Then er 4
  er CLng(Index)
  chk = CDbl(txt(Index).Text)
  Exit Sub
toolarge:
  txt(Index).BackColor = &H8080FF
End Sub

Private Sub txt_GotFocus(Index As Integer)
  er CLng(Index)
  RefItem = Index
  noact.Enabled = True
  HelpAnchor = lbl(Index).Tag
End Sub

Private Sub unit_GotFocus(Index As Integer)
  er CLng(Index)
  RefItem = Index
  noact.Enabled = True
  HelpAnchor = lbl(Index).Tag
End Sub

Private Sub inhale_GotFocus()
Dim m As String
  m = "Choose the method to be used to determine inhalation health impacts"
  mes = Space(140 - Len(m)) & m
  noact.Enabled = False
  HelpAnchor = Label2.Tag
End Sub

Private Sub kexpth_GotFocus()
Dim m As String
  m = "Choose dermal absorbtion model"
  mes = Space(140 - Len(m)) & m
  noact.Enabled = False
  HelpAnchor = Label1.Tag
End Sub

Private Sub form_KeyPress(KeyAscii As Integer)
  If KeyAscii = 13 Then KeyAscii = 0
End Sub

Private Sub form_KeyDown(KeyCode As Integer, Shift As Integer)
  Select Case KeyCode
  Case vbKeyF1:
    KeyCode = 0
    GetHelp
  Case vbKeyUp:
    KeyCode = 0
    SendKeys "+{TAB}"
  Case vbKeyDown:
    KeyCode = 0
    SendKeys "{TAB}"
  Case vbKeyReturn:
    KeyCode = 0
    SendKeys "{TAB}"
  End Select
End Sub
