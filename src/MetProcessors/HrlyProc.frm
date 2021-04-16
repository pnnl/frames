VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form frmHrlyProc 
   Caption         =   "Input for Hourly Processing Code"
   ClientHeight    =   6210
   ClientLeft      =   2250
   ClientTop       =   1125
   ClientWidth     =   8520
   LinkTopic       =   "Form1"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   6210
   ScaleWidth      =   8520
   Begin VB.CommandButton cmdExit 
      Caption         =   "Exit"
      Height          =   372
      Left            =   5040
      TabIndex        =   26
      Top             =   5640
      Width           =   2892
   End
   Begin VB.CommandButton cmdProcMet 
      Caption         =   "Process Meteorological Data"
      Height          =   372
      Left            =   720
      TabIndex        =   25
      Top             =   5640
      Width           =   2772
   End
   Begin VB.Frame frmTD3240 
      Height          =   1452
      Left            =   120
      TabIndex        =   19
      Top             =   4080
      Width           =   8052
      Begin VB.Frame Frame3 
         Caption         =   "Format of File"
         Height          =   492
         Left            =   1920
         TabIndex        =   22
         Top             =   840
         Width           =   4092
         Begin VB.OptionButton optTD3240 
            Caption         =   "Variable"
            Height          =   192
            Index           =   1
            Left            =   2520
            TabIndex        =   24
            Top             =   240
            Width           =   972
         End
         Begin VB.OptionButton optTD3240 
            Caption         =   "Fixed"
            Height          =   192
            Index           =   0
            Left            =   240
            TabIndex        =   23
            Top             =   240
            Width           =   1332
         End
      End
      Begin VB.Label lblTD3240File 
         AutoSize        =   -1  'True
         Caption         =   "Label14"
         Height          =   192
         Left            =   240
         TabIndex        =   21
         Top             =   480
         Width           =   576
      End
      Begin VB.Label Label13 
         AutoSize        =   -1  'True
         Caption         =   "Path and Name of TD3240 Data: "
         Height          =   192
         Left            =   240
         TabIndex        =   20
         Top             =   240
         Width           =   2340
      End
   End
   Begin VB.CheckBox chkTD3240 
      Caption         =   "Use TD3240 Data"
      Height          =   192
      Left            =   120
      TabIndex        =   18
      Top             =   3840
      Width           =   2652
   End
   Begin VB.Frame Frame1 
      Caption         =   "Station Information"
      Height          =   1935
      Left            =   120
      TabIndex        =   6
      Top             =   1680
      Width           =   8055
      Begin VB.Frame Frame2 
         Height          =   375
         Left            =   3000
         TabIndex        =   29
         Top             =   120
         Width           =   4815
         Begin VB.OptionButton optType 
            Caption         =   "SAMSON"
            Height          =   195
            Index           =   1
            Left            =   2760
            TabIndex        =   31
            Top             =   120
            Width           =   1215
         End
         Begin VB.OptionButton optType 
            Caption         =   "CD144"
            Height          =   195
            Index           =   0
            Left            =   360
            TabIndex        =   30
            Top             =   120
            Width           =   855
         End
      End
      Begin VB.TextBox txtz0App 
         Height          =   288
         Left            =   6840
         TabIndex        =   17
         Text            =   "Text5"
         Top             =   1560
         Width           =   852
      End
      Begin VB.TextBox txtWndHgt 
         Height          =   288
         Left            =   6840
         TabIndex        =   16
         Text            =   "Text4"
         Top             =   960
         Width           =   852
      End
      Begin VB.TextBox txtLon 
         Height          =   288
         Left            =   6840
         TabIndex        =   14
         Text            =   "Text3"
         Top             =   600
         Width           =   852
      End
      Begin VB.ComboBox cboHrsGMT 
         Height          =   315
         Left            =   3000
         TabIndex        =   12
         Text            =   "Combo1"
         Top             =   960
         Width           =   1812
      End
      Begin VB.TextBox txtz0Meas 
         Height          =   288
         Left            =   3000
         TabIndex        =   10
         Text            =   "Text2"
         Top             =   1560
         Width           =   852
      End
      Begin VB.TextBox txtLat 
         Height          =   288
         Left            =   3000
         TabIndex        =   8
         Top             =   600
         Width           =   732
      End
      Begin VB.Label Label6 
         Caption         =   "Type of Data File"
         Height          =   255
         Left            =   240
         TabIndex        =   32
         Top             =   240
         Width           =   2415
      End
      Begin VB.Label Label4 
         AutoSize        =   -1  'True
         Caption         =   "Application Site "
         Height          =   195
         Left            =   6720
         TabIndex        =   28
         Top             =   1320
         Width           =   1155
      End
      Begin VB.Label Label2 
         AutoSize        =   -1  'True
         Caption         =   "Measurement Site "
         Height          =   195
         Left            =   2760
         TabIndex        =   27
         Top             =   1320
         Width           =   1335
      End
      Begin VB.Label Label11 
         Caption         =   "Anemometer Height (m)"
         Height          =   255
         Left            =   4920
         TabIndex        =   15
         Top             =   960
         Width           =   1815
      End
      Begin VB.Label Label10 
         Caption         =   "Longitude (deg)  (+ West of Greenwich)"
         Height          =   255
         Left            =   3840
         TabIndex        =   13
         Top             =   600
         Width           =   2895
      End
      Begin VB.Label Label9 
         Caption         =   "Surface Roughness (m)"
         Height          =   255
         Left            =   240
         TabIndex        =   11
         Top             =   1560
         Width           =   2415
      End
      Begin VB.Label Label8 
         Caption         =   "Hours from Greenwich Mean Time  (+ West of Greenwich)"
         Height          =   495
         Left            =   240
         TabIndex        =   9
         Top             =   960
         Width           =   2535
      End
      Begin VB.Label Label7 
         Caption         =   "Latitude (deg)  (+ North of Equator)"
         Height          =   255
         Left            =   240
         TabIndex        =   7
         Top             =   600
         Width           =   2535
      End
   End
   Begin MSComDlg.CommonDialog dlgOpenFile 
      Left            =   6720
      Top             =   480
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Label lblOutMetFile 
      AutoSize        =   -1  'True
      Caption         =   "Label6"
      Height          =   192
      Left            =   120
      TabIndex        =   5
      Top             =   1200
      Width           =   492
   End
   Begin VB.Label Label5 
      Caption         =   "Path and Name of Output Meteorological Data:"
      Height          =   252
      Left            =   120
      TabIndex        =   4
      Top             =   960
      Width           =   4332
   End
   Begin VB.Label lblInpMetFile 
      AutoSize        =   -1  'True
      Caption         =   "Label4"
      Height          =   192
      Left            =   120
      TabIndex        =   3
      Top             =   720
      Width           =   492
   End
   Begin VB.Label Label3 
      AutoSize        =   -1  'True
      Caption         =   "Path and Name of Input Hourly Meteorological Data (CD144 or SAMSON Format Only!):  "
      Height          =   195
      Left            =   120
      TabIndex        =   2
      Top             =   480
      Width           =   6270
   End
   Begin VB.Label lblInpFile 
      AutoSize        =   -1  'True
      Caption         =   "Label2"
      Height          =   192
      Left            =   120
      TabIndex        =   1
      Top             =   240
      Width           =   492
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Path and Name of File where Model Parameters are stored:  "
      Height          =   192
      Left            =   120
      TabIndex        =   0
      Top             =   0
      Width           =   4296
   End
   Begin VB.Menu mnuFile 
      Caption         =   "&File"
      Begin VB.Menu mnuFiChange 
         Caption         =   "Change &Parameter File"
         Index           =   0
      End
      Begin VB.Menu mnuFiChange 
         Caption         =   "Change &Input  Met Data File"
         Index           =   1
      End
      Begin VB.Menu mnuFiChange 
         Caption         =   "Change &Output Met Data File"
         Index           =   2
      End
      Begin VB.Menu mnuFiChange 
         Caption         =   "Change &TD3240 File"
         Index           =   3
      End
   End
End
Attribute VB_Name = "frmHrlyProc"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit


Sub ChkParm(inpfile As String)
    Dim nn As Integer, inpline As String
    Dim errtext As String
    Dim comcol As Integer
    
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
            mnuFiChange(3).Enabled = False
        Else
            chkTD3240.Value = 1
            frmTD3240.Visible = True
            mnuFiChange(3).Enabled = True
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
    Dim fname As String, fnum As Integer
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
        mnuFiChange(3).Enabled = False
    Else
        frmTD3240.Visible = True
        mnuFiChange(3).Enabled = True
    End If
    
End Sub

Private Sub cmdExit_Click()
    Dim msgtext As String
    Dim response As Integer
    
    msgtext = "Do you wish to Exit the Program?"
    response = MsgBox(msgtext, vbYesNo, "Exit Program?")
    If response = vbYes Then
        End
    Else
        Exit Sub
    End If
    
End Sub

Private Sub cmdProcMet_Click()
    Dim errtext As String, response As Integer
    Dim runcode As String, sh As Integer
    Dim de As Integer
    Dim errfile As String

    Call ChkVals(errtext)
    
    If errtext <> "" Then
        response = MsgBox(errtext, vbExclamation, "ERROR!")
        Exit Sub
    End If
    
    Call GetValsFromForm
    runcode = "Command.COM /C " + App.Path + "\runcode.bat " + App.Path + "\hrlyproc " + lblInpFile.Caption
    
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
    
    Call ExecCmd(runcode)
    
    errfile = App.Path + "\code.err"
    If Dir(errfile) <> "" Then
        Call WRITEERR(errfile)
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
        Call ChkParm(lblInpFile)
    End If
        
End Sub

Private Sub mnuFiChange_Click(Index As Integer)
    Dim Ftitle As String, fname As String, filt As String
    Dim msgtext As String, response As Integer
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
            Call GetNamePathExt(fname, dum, dirpath, Ext)
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
                    Call mnuFiChange_Click(Index)
                    Exit Sub
                Else
                    msgtext = "Do you want to use the information in this file?"
                    response = MsgBox(msgtext, vbYesNo, "Use Information?")
                    If response = vbYes Then
                        Call ChkParm(tmpfile)
                       
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
                    Call mnuFiChange_Click(Index)
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


