VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form frmProcHrly 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Input for Hourly Processing Code"
   ClientHeight    =   5760
   ClientLeft      =   2250
   ClientTop       =   885
   ClientWidth     =   7680
   Icon            =   "ProcHrly.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   384
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   512
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton cmdExit 
      Caption         =   "Exit"
      Height          =   372
      Left            =   6144
      TabIndex        =   31
      Top             =   5280
      Width           =   1452
   End
   Begin VB.CommandButton cmdProcMet 
      Caption         =   "Process Data"
      Height          =   372
      Left            =   4608
      TabIndex        =   30
      Top             =   5280
      Width           =   1428
   End
   Begin VB.Frame Frame1 
      Height          =   5196
      Left            =   96
      TabIndex        =   32
      Top             =   0
      Width           =   7476
      Begin VB.CommandButton Command1 
         Caption         =   "Browse"
         Height          =   300
         Index           =   2
         Left            =   6528
         TabIndex        =   11
         Top             =   1632
         Width           =   780
      End
      Begin VB.CheckBox chkTD3240 
         Caption         =   "Use TD3240 Data"
         Height          =   192
         Left            =   192
         TabIndex        =   12
         Top             =   2016
         Width           =   2652
      End
      Begin VB.Frame frmTD3240 
         BorderStyle     =   0  'None
         Height          =   636
         Left            =   96
         TabIndex        =   33
         Top             =   1920
         Width           =   7308
         Begin VB.OptionButton optTD3240 
            Caption         =   "Variable"
            Height          =   192
            Index           =   1
            Left            =   5376
            TabIndex        =   15
            Top             =   96
            Width           =   900
         End
         Begin VB.OptionButton optTD3240 
            Caption         =   "Fixed"
            Height          =   192
            Index           =   0
            Left            =   4416
            TabIndex        =   14
            Top             =   96
            Value           =   -1  'True
            Width           =   804
         End
         Begin VB.CommandButton Command1 
            Caption         =   "Browse"
            Height          =   300
            Index           =   3
            Left            =   6432
            TabIndex        =   17
            Top             =   288
            Width           =   780
         End
         Begin VB.Label Label4 
            Caption         =   "Format"
            Height          =   204
            Left            =   3648
            TabIndex        =   13
            Top             =   96
            Width           =   624
         End
         Begin VB.Label lblTD3240File 
            BackColor       =   &H80000005&
            BorderStyle     =   1  'Fixed Single
            Caption         =   "Label14"
            Height          =   288
            Left            =   480
            TabIndex        =   16
            Top             =   288
            Width           =   5820
         End
      End
      Begin VB.CommandButton Command1 
         Caption         =   "Browse"
         Height          =   300
         Index           =   1
         Left            =   6528
         TabIndex        =   8
         Top             =   1056
         Width           =   780
      End
      Begin VB.CommandButton Command1 
         Caption         =   "Browse"
         Height          =   300
         Index           =   0
         Left            =   6528
         TabIndex        =   2
         Top             =   480
         Width           =   780
      End
      Begin VB.TextBox txtz0Meas 
         Height          =   288
         Left            =   5280
         TabIndex        =   27
         Top             =   4320
         Width           =   1000
      End
      Begin VB.TextBox txtz0App 
         Height          =   288
         Left            =   5280
         TabIndex        =   29
         Top             =   4704
         Width           =   1000
      End
      Begin VB.OptionButton optType 
         Caption         =   "CD144"
         Height          =   195
         Index           =   0
         Left            =   4416
         TabIndex        =   5
         Top             =   864
         Value           =   -1  'True
         Width           =   900
      End
      Begin VB.OptionButton optType 
         Caption         =   "SAMSON"
         Height          =   195
         Index           =   1
         Left            =   5376
         TabIndex        =   6
         Top             =   864
         Width           =   1000
      End
      Begin VB.TextBox txtWndHgt 
         Height          =   288
         Left            =   5280
         TabIndex        =   25
         Top             =   3936
         Width           =   1000
      End
      Begin VB.TextBox txtLon 
         Height          =   288
         Left            =   5280
         TabIndex        =   23
         Top             =   3552
         Width           =   1000
      End
      Begin VB.ComboBox cboHrsGMT 
         Height          =   288
         Left            =   5280
         TabIndex        =   19
         Text            =   "Combo1"
         Top             =   2784
         Width           =   2004
      End
      Begin VB.TextBox txtLat 
         Height          =   288
         Left            =   5280
         TabIndex        =   21
         Top             =   3168
         Width           =   1000
      End
      Begin VB.Label lblInpFile 
         BackColor       =   &H80000005&
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Label2"
         Height          =   288
         Left            =   576
         TabIndex        =   1
         Top             =   480
         Width           =   5820
      End
      Begin VB.Label lblInpMetFile 
         BackColor       =   &H80000005&
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Label4"
         Height          =   288
         Left            =   576
         TabIndex        =   7
         Top             =   1056
         Width           =   5820
      End
      Begin VB.Label lblOutMetFile 
         BackColor       =   &H80000005&
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Label6"
         Height          =   288
         Left            =   576
         TabIndex        =   10
         Top             =   1632
         Width           =   5820
      End
      Begin VB.Label Label2 
         Caption         =   "Format"
         Height          =   204
         Left            =   3648
         TabIndex        =   4
         Top             =   864
         Width           =   624
      End
      Begin VB.Label Label9 
         Caption         =   "Surface roughness for application site (m)"
         Height          =   252
         Left            =   192
         TabIndex        =   28
         Top             =   4704
         Width           =   5004
      End
      Begin VB.Label Label6 
         Caption         =   "Surface roughness for measurment site (m)"
         Height          =   252
         Left            =   192
         TabIndex        =   26
         Top             =   4320
         Width           =   5004
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Path for file to store intermediate parameters entered from this page"
         Height          =   195
         Left            =   195
         TabIndex        =   0
         Top             =   285
         Width           =   4710
      End
      Begin VB.Label Label3 
         AutoSize        =   -1  'True
         Caption         =   "Path of Input Hourly Meteorological Data"
         Height          =   288
         Left            =   192
         TabIndex        =   3
         Top             =   864
         Width           =   2904
      End
      Begin VB.Label Label5 
         Caption         =   "Path of Output Meteorological Data"
         Height          =   252
         Left            =   192
         TabIndex        =   9
         Top             =   1440
         Width           =   2904
      End
      Begin VB.Label Label11 
         Caption         =   "Anemometer Height (m)"
         Height          =   252
         Left            =   192
         TabIndex        =   24
         Top             =   3936
         Width           =   5004
      End
      Begin VB.Label Label10 
         Caption         =   "Longitude (deg)  (+ West of Greenwich)"
         Height          =   252
         Left            =   192
         TabIndex        =   22
         Top             =   3552
         Width           =   5004
      End
      Begin VB.Label Label8 
         Caption         =   "Hours from Greenwich Mean Time  (+ West of Greenwich)"
         Height          =   300
         Left            =   192
         TabIndex        =   18
         Top             =   2784
         Width           =   5004
      End
      Begin VB.Label Label7 
         Caption         =   "Latitude (deg)  (+ North of Equator)"
         Height          =   252
         Left            =   192
         TabIndex        =   20
         Top             =   3168
         Width           =   5004
      End
   End
   Begin MSComDlg.CommonDialog dlgOpenFile 
      Left            =   0
      Top             =   5376
      _ExtentX        =   688
      _ExtentY        =   688
      _Version        =   393216
   End
End
Attribute VB_Name = "frmProcHrly"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Sub ChkParm(inpfile As String)
    Dim nn As Long, inpline As String
    Dim errtext As String
    Dim comcol As Long
    
    On Local Error GoTo ErrorParm
    
    nn = FreeFile
    
    Open inpfile For Input As nn
        Line Input #nn, inpline
        lblOutMetFile.Caption = inpline
        Line Input #nn, inpline
        lblInpMetFile.Caption = inpline
        Line Input #nn, inpline
        If inpline = "CD144" Then
            optType(0).Value = True
        Else
            optType(1).Value = True
        End If
        
        Line Input #nn, inpline
        comcol = InStr(inpline, ",")
        txtLat.Text = Mid(inpline, 1, comcol - 1)
        txtLon.Text = Mid(inpline, comcol + 1)
        Line Input #nn, inpline
        cboHrsGMT.Text = inpline
        Line Input #nn, inpline
        txtz0Meas.Text = inpline
        Line Input #nn, inpline
        txtz0App.Text = inpline
        Line Input #nn, inpline
        txtWndHgt.Text = inpline
        Line Input #nn, inpline
        
        If inpline = "F" Then
            chkTD3240.Value = 0
            frmTD3240.Visible = False
            Command1(3).Enabled = False
        Else
            chkTD3240.Value = 1
            frmTD3240.Visible = True
            Command1(3).Enabled = True
            Line Input #nn, inpline
            lblTD3240File.Caption = inpline
            Line Input #nn, inpline
            If inpline = "F" Then
                optTD3240(0).Value = True
            Else
                optTD3240(1).Value = True
            End If
        End If
    
    Close nn
    
    Exit Sub

ErrorParm:
    errtext = "Error occurred reading parameter file." + Chr(13) + Chr(10)
    errtext = errtext + "Error Number - " + Str(Err)
    errtext = errtext + Chr(13) + Chr(10)
    errtext = errtext + Error
    MsgBox errtext
End Sub

Sub ChkVals(errtext As String)

    errtext = ""
    
    If lblInpFile.Caption = "" Then
        errtext = errtext + "No file name given for Model Parameter file"
        errtext = errtext + Chr(13) + Chr(10)
    End If
    
    If lblInpMetFile.Caption = "" Then
        errtext = errtext + "No filename given for Input Meteorological Data file."
        errtext = errtext + Chr$(13) + Chr(10)
    ElseIf Dir(lblInpMetFile.Caption) = "" Then
        errtext = errtext + "Invalid file name for Input Meteorological data file - file does not exist."
        errtext = errtext + Chr(13) + Chr(10)
    End If
    
    If lblOutMetFile.Caption = "" Then
        errtext = errtext + "No file name given for Output Meteorological data file."
        errtext = errtext + Chr(13) + Chr(10)
    End If
    
    If Abs(Val(txtLat.Text)) > 90 Then
        errtext = errtext + "Invalid latitude - absolute value greater than 90 degrees."
        errtext = errtext + Chr(13) + Chr(10)
    End If
    
    If txtLat.Text = "" Then
        errtext = errtext + "Need to input a latitude."
        errtext = errtext + Chr(13) + Chr(10)
    End If
    
    If txtLon.Text = "" Then
        errtext = errtext + "Need to input a longitude."
        errtext = errtext + Chr(13) + Chr(10)
    End If
    
    If Abs(Val(txtLon.Text)) > 180 Then
        errtext = errtext + "Invalid longitude - absolute value greater than 180 degrees"
        errtext = errtext + Chr(13) + Chr(10)
    End If
    
    If cboHrsGMT.Text = "" Then
        errtext = errtext + "Need to input the hours from Greenwich Mean Time."
        errtext = errtext + Chr(13) + Chr(10)
    End If
    
    If Abs(Val(cboHrsGMT.Text)) > 12 Then
        errtext = errtext + "Invalid hours from Greenwich Mean Time - absoulte value greater than 12."
        errtext = errtext + Chr(13) + Chr(10)
    End If
    
    If Val(txtz0Meas.Text) <= 0 Then
        errtext = errtext + "Invalid Surface Roughness for Measurement Site - must be greater than zero."
        errtext = errtext + Chr(13) + Chr(10)
    End If
    
    If Val(txtz0Meas.Text) <= 0 Then
        errtext = errtext + "Invalid Surface Roughness for Application Site - must be greater than zero."
        errtext = errtext + Chr(13) + Chr(10)
    End If
    
    If Val(txtWndHgt.Text) <= 0 Then
        errtext = errtext + "Anemometer Height must be greater than zero."
        errtext = errtext + Chr(13) + Chr(10)
    End If
    
    If chkTD3240.Value = 1 Then
        If lblTD3240File.Caption = "" Then
            errtext = errtext + "No file name given for TD3240 file."
            errtext = errtext + Chr(13) + Chr(10)
        ElseIf Dir(lblTD3240File.Caption) = "" Then
            errtext = errtext + "Invalid file name given for TD3240 file - does not exist."
            errtext = errtext + Chr(13) + Chr(10)
        End If
    End If
End Sub

Sub GetValsFromForm()
    Dim fname As String, fnum As Long
    Dim outstr As String

    fname = lblInpFile.Caption
    fnum = FreeFile
    
    Open fname For Output As fnum
    
    Print #fnum, lblOutMetFile.Caption
    Print #fnum, lblInpMetFile.Caption
    If optType(0).Value = True Then
        Print #fnum, "CD144"
    Else
        Print #fnum, "SAMSON"
    End If
    outstr = LTrim(RTrim(Val(txtLat.Text))) + "," + LTrim(RTrim(Val(txtLon.Text)))
    Print #fnum, outstr
    outstr = LTrim(RTrim(Val(cboHrsGMT.Text)))
    Print #fnum, outstr
    outstr = LTrim(RTrim(Val(txtz0Meas.Text)))
    Print #fnum, outstr
    outstr = LTrim(RTrim(Val(txtz0App.Text)))
    Print #fnum, outstr
    outstr = LTrim(RTrim(Val(txtWndHgt.Text)))
    Print #fnum, outstr
    If chkTD3240.Value = 0 Then
        Print #fnum, "F"
    Else
        Print #fnum, "T"
        Print #fnum, lblTD3240File.Caption
        If optTD3240(0).Value = True Then
            Print #fnum, "F"
        Else
            Print #fnum, "V"
        End If
    End If
    
    Close fnum
End Sub

Private Sub chkTD3240_Click()
    If chkTD3240.Value = 0 Then
        frmTD3240.Visible = False
    Else
        frmTD3240.Visible = True
    End If
End Sub

Private Sub cmdExit_Click()
    Dim msgtext As String
    Dim response As Long
    
    msgtext = "Do you wish to Exit the Program?"
    response = MsgBox(msgtext, vbYesNo, "Exit Program?")
    If response = vbYes Then
        End
    Else
        Exit Sub
    End If
End Sub

Private Sub cmdProcMet_Click()
    Dim errtext As String, response As Long
    Dim runcode As String, sh As Long
    Dim de As Long
    Dim errfile As String

    ChkVals errtext
    
    If errtext <> "" Then
        response = MsgBox(errtext, vbExclamation, "ERROR!")
        Exit Sub
    End If
    
    GetValsFromForm
'    runcode = "Command.COM /C " + App.Path + "\runcode.bat " + App.Path + "\hrlyproc " + lblInpFile.Caption
    runcode = App.Path + "\hrlyproc " + lblInpFile.Caption
    
    'sh = Shell(runcode, 1)
    'If sh <= 0 Then
    '    errtext = "Unable to shell " + runcode
    '    response = MsgBox(errtext, vbExclamation, "ERROR!")
    '    Exit Sub
    'End If
   '
   ' While GetModuleUsage(sh) > 0
   '     de = DoEvents()
   ' Wend
    
    ExecCmd runcode
    
    errfile = App.Path + "\code.err"
    If Dir(errfile) <> "" Then
        WRITEERR errfile
    End If
 
End Sub

Private Sub Form_Load()
    
    lblInpFile.Caption = App.Path + "\HRLYPROC.INP"
    lblOutMetFile.Caption = ""
    lblInpMetFile.Caption = ""
    lblTD3240File.Caption = ""
    
    txtLat.Text = ""
    txtLon.Text = ""
    txtWndHgt.Text = ""
    cboHrsGMT.Text = ""
    cboHrsGMT.AddItem "8 !Pacific Time Zone"
    cboHrsGMT.AddItem "7 !Mountain Time Zone"
    cboHrsGMT.AddItem "6 !Central Time Zone"
    cboHrsGMT.AddItem "5 !Eastern Time Zone"
    txtz0Meas.Text = ""
    txtz0App.Text = ""
    
    
    chkTD3240.Value = 0
    frmTD3240.Visible = False
    
    optType(0).Value = True
    optTD3240(0).Value = 1
    
    If Dir(lblInpFile.Caption) <> "" Then
        ChkParm lblInpFile
    End If
        
End Sub

Private Sub Command1_Click(Index As Integer)
    Dim Ftitle As String, fname As String, filt As String
    Dim msgtext As String, response As Long
    Dim tmpfile As String
    Dim dum As String, dirpath As String, Ext As String
    

    On Local Error GoTo OpenError
    
    Select Case (Index)
        
        Case (0)
            Ftitle = "Change Model Parameter File"
            fname = lblInpFile.Caption
            filt = "All Files (*.*)|*.*|Input Files (*.INP)|*.INP"
        Case (1)
            Ftitle = "Change Hourly Met Data (Input)"
            fname = lblInpMetFile.Caption
            filt = "All Files (*.*)|*.*|Met Files (*.MET)|*.MET"
        Case (2)
            Ftitle = "Change Output Hourly Met File"
            fname = lblOutMetFile.Caption
            filt = "All Files (*.*)|*.*|Met Files (*.MET)|*.MET"
        Case (3)
            Ftitle = "Change TD3240 File"
            fname = lblTD3240File.Caption
            filt = "All Files (*.*)|*.*"
        Case Else
            msgtext = "Error - undefined Change Index of " + Index
            MsgBox msgtext
            Exit Sub
    End Select
    
    dlgOpenFile.DialogTitle = Ftitle
    dlgOpenFile.FileName = fname
    If fname <> "" Then
        If Dir(fname) <> "" Then
            GetNamePathExt fname, dum, dirpath, Ext
        Else
            dirpath = App.Path
        End If
    Else
        dirpath = App.Path
    End If
    dlgOpenFile.InitDir = dirpath
    dlgOpenFile.Filter = filt
    dlgOpenFile.CancelError = True
    dlgOpenFile.Action = 1
    
    tmpfile = dlgOpenFile.FileName
    
    Select Case (Index)
    
        Case (0)
            If Dir(tmpfile) <> "" Then
                msgtext = "The information in this file may be overwritten."
                msgtext = msgtext + Chr(13) + Chr(10)
                msgtext = msgtext + "Do you wish to change the name of the file?"
                response = MsgBox(msgtext, vbYesNo, "Change File?")
                If response = vbYes Then
                    Command1_Click Index
                    Exit Sub
                Else
                    msgtext = "Do you want to use the information in this file?"
                    response = MsgBox(msgtext, vbYesNo, "Use Information?")
                    If response = vbYes Then
                        ChkParm tmpfile
                       
                    End If
                End If
            End If
            
            lblInpFile.Caption = tmpfile
                
        Case (1)
            If Dir(tmpfile) = "" Then
                msgtext = "This file does not exist - must select an existing file"
                response = MsgBox(msgtext, vbExclamation, "ERROR!")
                Exit Sub
            End If
               
            lblInpMetFile.Caption = tmpfile
            
        Case (2)
            If Dir(tmpfile) <> "" Then
                msgtext = "The information in this file will be overwritten." + Chr(13) + Chr(10)
                msgtext = msgtext + "Do you wish to change the name of the file?"
                response = MsgBox(msgtext, vbYesNo, "Change File?")
                If response = vbYes Then
                    Command1_Click Index
                    Exit Sub
                End If
            End If
               
            lblOutMetFile.Caption = tmpfile
            
        Case (3)
                
            If Dir(tmpfile) = "" Then
                msgtext = "This file does not exist - must select an existing file"
                response = MsgBox(msgtext, vbExclamation, "ERROR!")
                Exit Sub
            End If
                
            lblTD3240File.Caption = tmpfile
                    
    End Select
        
    Exit Sub

OpenError:
    If Err = cdlCancel Then Exit Sub
    msgtext = "Error Changing File" + Chr(13) + Chr(10)
    msgtext = msgtext + "Error Number " + Str(Err)
    msgtext = msgtext + Chr(13) + Chr(10)
    msgtext = msgtext + Error
    response = MsgBox(msgtext, vbExclamation, "Error!")
    
End Sub

