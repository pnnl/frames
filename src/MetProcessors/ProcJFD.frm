VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Object = "{B02F3647-766B-11CE-AF28-C3A2FBE76A13}#2.5#0"; "SS32X25.OCX"
Begin VB.Form frmProcJFD 
   Caption         =   "Input for Processing Joint Frequency Distribution"
   ClientHeight    =   6105
   ClientLeft      =   120
   ClientTop       =   630
   ClientWidth     =   8940
   LinkTopic       =   "Form1"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   6105
   ScaleWidth      =   8940
   Begin VB.CommandButton cmdExit 
      Caption         =   "Exit"
      Height          =   492
      Left            =   5160
      TabIndex        =   26
      Top             =   5520
      Width           =   2892
   End
   Begin VB.CommandButton cmdProcJFD 
      Caption         =   "Process Joint Frequency Distribution"
      Height          =   492
      Left            =   720
      TabIndex        =   25
      Top             =   5520
      Width           =   2892
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   3852
      Left            =   120
      TabIndex        =   6
      Top             =   1560
      Width           =   8652
      _ExtentX        =   15266
      _ExtentY        =   6800
      _Version        =   393216
      Tab             =   1
      TabHeight       =   423
      TabCaption(0)   =   "Station Information"
      Tab(0).ControlEnabled=   0   'False
      Tab(0).Control(0)=   "Label7"
      Tab(0).Control(1)=   "Label8"
      Tab(0).Control(2)=   "Label9"
      Tab(0).Control(3)=   "Label10"
      Tab(0).Control(4)=   "Label14"
      Tab(0).Control(5)=   "Label15"
      Tab(0).Control(6)=   "txtLat"
      Tab(0).Control(7)=   "txtWndHgt"
      Tab(0).Control(8)=   "txtz0Meas"
      Tab(0).Control(9)=   "txtz0App"
      Tab(0).Control(10)=   "txtHrsJFD"
      Tab(0).ControlCount=   11
      TabCaption(1)   =   "Joint Frequency Info"
      Tab(1).ControlEnabled=   -1  'True
      Tab(1).Control(0)=   "Label11"
      Tab(1).Control(0).Enabled=   0   'False
      Tab(1).Control(1)=   "Label12"
      Tab(1).Control(1).Enabled=   0   'False
      Tab(1).Control(2)=   "Label13"
      Tab(1).Control(2).Enabled=   0   'False
      Tab(1).Control(3)=   "Label2"
      Tab(1).Control(3).Enabled=   0   'False
      Tab(1).Control(4)=   "spdSpdMid"
      Tab(1).Control(4).Enabled=   0   'False
      Tab(1).Control(5)=   "spdMonTemp"
      Tab(1).Control(5).Enabled=   0   'False
      Tab(1).Control(6)=   "txtNumStab"
      Tab(1).Control(6).Enabled=   0   'False
      Tab(1).Control(7)=   "Frame1"
      Tab(1).Control(7).Enabled=   0   'False
      Tab(1).ControlCount=   8
      TabCaption(2)   =   "Precipitation"
      Tab(2).ControlEnabled=   0   'False
      Tab(2).Control(0)=   "frmPrecp"
      Tab(2).Control(1)=   "chkPrecp"
      Tab(2).ControlCount=   2
      Begin VB.Frame frmPrecp 
         Height          =   2532
         Left            =   -74880
         TabIndex        =   29
         Top             =   1080
         Width           =   8412
         Begin VB.ComboBox cboPrecp 
            Height          =   288
            Left            =   2640
            Style           =   2  'Dropdown List
            TabIndex        =   31
            Top             =   240
            Width           =   5412
         End
         Begin FPSpread.vaSpread spdPrecp 
            Height          =   1332
            Left            =   840
            TabIndex        =   30
            Top             =   840
            Width           =   6852
            _Version        =   131077
            _ExtentX        =   12086
            _ExtentY        =   2350
            _StockProps     =   64
            BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
               Name            =   "MS Sans Serif"
               Size            =   8.25
               Charset         =   0
               Weight          =   700
               Underline       =   0   'False
               Italic          =   0   'False
               Strikethrough   =   0   'False
            EndProperty
            MaxCols         =   5
            MaxRows         =   4
            SpreadDesigner  =   "PROCJFD.frx":0000
            VisibleCols     =   5
            VisibleRows     =   4
         End
         Begin VB.Label lblPrecp 
            Caption         =   "Choose a representative City"
            Height          =   252
            Left            =   120
            TabIndex        =   32
            Top             =   240
            Width           =   2532
         End
      End
      Begin VB.CheckBox chkPrecp 
         Caption         =   "Include Precipitation"
         Height          =   192
         Left            =   -74760
         TabIndex        =   24
         Top             =   720
         Width           =   2892
      End
      Begin VB.Frame Frame1 
         Height          =   492
         Left            =   6120
         TabIndex        =   19
         Top             =   840
         Width           =   2172
         Begin VB.OptionButton optNumDir 
            Caption         =   "36"
            Height          =   192
            Index           =   1
            Left            =   1200
            TabIndex        =   21
            Top             =   240
            Width           =   852
         End
         Begin VB.OptionButton optNumDir 
            Caption         =   "16"
            Height          =   192
            Index           =   0
            Left            =   120
            TabIndex        =   20
            Top             =   240
            Width           =   852
         End
      End
      Begin VB.TextBox txtNumStab 
         Height          =   288
         Left            =   7440
         TabIndex        =   17
         Text            =   "Text6"
         Top             =   480
         Width           =   852
      End
      Begin VB.TextBox txtHrsJFD 
         Height          =   288
         Left            =   -67560
         TabIndex        =   15
         Text            =   "Text5"
         Top             =   2640
         Width           =   852
      End
      Begin VB.TextBox txtz0App 
         Height          =   288
         Left            =   -67560
         TabIndex        =   13
         Text            =   "Text4"
         Top             =   2040
         Width           =   852
      End
      Begin VB.TextBox txtz0Meas 
         Height          =   288
         Left            =   -69480
         TabIndex        =   12
         Text            =   "Text3"
         Top             =   2040
         Width           =   852
      End
      Begin VB.TextBox txtWndHgt 
         Height          =   288
         Left            =   -67560
         TabIndex        =   10
         Text            =   "Text2"
         Top             =   1200
         Width           =   852
      End
      Begin VB.TextBox txtLat 
         Height          =   288
         Left            =   -67560
         TabIndex        =   8
         Text            =   "Text1"
         Top             =   600
         Width           =   852
      End
      Begin FPSpread.vaSpread spdMonTemp 
         Height          =   972
         Left            =   240
         TabIndex        =   34
         Top             =   2760
         Width           =   8172
         _Version        =   131077
         _ExtentX        =   14415
         _ExtentY        =   1715
         _StockProps     =   64
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         MaxCols         =   12
         MaxRows         =   2
         SpreadDesigner  =   "PROCJFD.frx":0197
         VisibleCols     =   12
         VisibleRows     =   2
      End
      Begin FPSpread.vaSpread spdSpdMid 
         Height          =   732
         Left            =   240
         TabIndex        =   22
         Top             =   1680
         Width           =   8052
         _Version        =   131077
         _ExtentX        =   14203
         _ExtentY        =   1291
         _StockProps     =   64
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         MaxCols         =   10
         MaxRows         =   1
         SpreadDesigner  =   "PROCJFD.frx":0329
         VisibleCols     =   10
         VisibleRows     =   1
      End
      Begin VB.Label Label2 
         AutoSize        =   -1  'True
         Caption         =   "Temperature (deg F)"
         Height          =   192
         Left            =   240
         TabIndex        =   33
         Top             =   2520
         Width           =   1488
      End
      Begin VB.Label Label15 
         AutoSize        =   -1  'True
         Caption         =   "Application Site"
         Height          =   192
         Left            =   -67680
         TabIndex        =   28
         Top             =   1800
         Width           =   1116
      End
      Begin VB.Label Label14 
         AutoSize        =   -1  'True
         Caption         =   "Measurement Site"
         Height          =   192
         Left            =   -69720
         TabIndex        =   27
         Top             =   1800
         Width           =   1296
      End
      Begin VB.Label Label13 
         AutoSize        =   -1  'True
         Caption         =   "Wind Speed Mid Points (m/s):"
         Height          =   192
         Left            =   240
         TabIndex        =   23
         Top             =   1440
         Width           =   2136
      End
      Begin VB.Label Label12 
         Caption         =   "Number of Wind Direction Sectors "
         Height          =   252
         Left            =   240
         TabIndex        =   18
         Top             =   840
         Width           =   3132
      End
      Begin VB.Label Label11 
         Caption         =   "Number of Stability Classes (1-7)"
         Height          =   252
         Left            =   240
         TabIndex        =   16
         Top             =   480
         Width           =   2652
      End
      Begin VB.Label Label10 
         Caption         =   "Number of Hours in JFD"
         Height          =   252
         Left            =   -74760
         TabIndex        =   14
         Top             =   2640
         Width           =   2412
      End
      Begin VB.Label Label9 
         Caption         =   "Surface Roughness (m)"
         Height          =   252
         Left            =   -74760
         TabIndex        =   11
         Top             =   2040
         Width           =   2052
      End
      Begin VB.Label Label8 
         Caption         =   "Anemometer Height (m)"
         Height          =   252
         Left            =   -74760
         TabIndex        =   9
         Top             =   1200
         Width           =   3612
      End
      Begin VB.Label Label7 
         Caption         =   "Latitude (deg) (+ North of Equator)"
         Height          =   252
         Left            =   -74760
         TabIndex        =   7
         Top             =   600
         Width           =   3372
      End
   End
   Begin MSComDlg.CommonDialog dlgOpenFile 
      Left            =   7320
      Top             =   1080
      _ExtentX        =   688
      _ExtentY        =   688
      _Version        =   393216
   End
   Begin VB.Label lblOutMetFile 
      AutoSize        =   -1  'True
      Caption         =   "Label6"
      Height          =   192
      Left            =   120
      TabIndex        =   5
      Top             =   1320
      Width           =   492
   End
   Begin VB.Label Label5 
      AutoSize        =   -1  'True
      Caption         =   "Path and Name of Output Meteorological Data File:  "
      Height          =   192
      Left            =   120
      TabIndex        =   4
      Top             =   1080
      Width           =   3684
   End
   Begin VB.Label lblJFDFile 
      AutoSize        =   -1  'True
      Caption         =   "Label4"
      Height          =   192
      Left            =   120
      TabIndex        =   3
      Top             =   840
      Width           =   492
   End
   Begin VB.Label lblJFDLabel 
      AutoSize        =   -1  'True
      Caption         =   "Path and Name of Joint Frequency Distribution File: "
      Height          =   192
      Left            =   120
      TabIndex        =   2
      Top             =   600
      Width           =   3660
   End
   Begin VB.Label lblInpFile 
      AutoSize        =   -1  'True
      Caption         =   "Label2"
      Height          =   192
      Left            =   120
      TabIndex        =   1
      Top             =   360
      Width           =   492
   End
   Begin VB.Label Label1 
      AutoSize        =   -1  'True
      Caption         =   "Path and Name of file where model parameter are saved:  "
      Height          =   192
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   4140
   End
   Begin VB.Menu mnuFile 
      Caption         =   "&File"
      Begin VB.Menu mnuFiChange 
         Caption         =   "Change &Parameter File"
         Index           =   0
      End
      Begin VB.Menu mnuFiChange 
         Caption         =   "Change &Joint Frequency Distribution File"
         Index           =   1
      End
      Begin VB.Menu mnuFiChange 
         Caption         =   "Change Output Meteorological Data File"
         Index           =   2
      End
   End
End
Attribute VB_Name = "frmProcJFD"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Dim gprecp() As Single
Dim gJFDType As Integer
Sub ChkParm(infile As String)
    Dim nn As Integer, inpline As String
    Dim num As Integer, lenval As Integer, tmpval() As Single
    Dim i As Integer, j As Integer
    Dim comcol As Integer, numspd As Integer
    Dim precp(4) As String
    
    precp(1) = "Light Rain"
    precp(2) = "Mod/Heavy Rain"
    precp(3) = "Light Snow"
    precp(4) = "Mod/Heavy Snow"
    
    nn = FreeFile
    
    Open infile For Input As nn
        Line Input #nn, inpline
        lblOutMetFile.Caption = inpline
        Line Input #nn, inpline
        lblJFDFile.Caption = inpline
        Line Input #nn, inpline
        gJFDType = Val(inpline)
        Line Input #nn, inpline
        If gJFDType = 0 Then
            lblJFDLabel.Caption = "Path and Name of Joint Frequency Distribution File: (EPA's STAR Format)"
        Else
            lblJFDLabel.Caption = "Path and Name of Joint Frequency Distribution File: (GENII Format)"
        End If
        txtLat.Text = inpline
        Line Input #nn, inpline
        txtWndHgt.Text = inpline
        Line Input #nn, inpline
        txtz0Meas.Text = inpline
        Line Input #nn, inpline
        txtz0App.Text = inpline
        Line Input #nn, inpline
        txtNumStab.Text = inpline
        Line Input #nn, inpline
        If Val(inpline) = 16 Then
            optNumDir(0).Value = True
        ElseIf Val(inpline) = 36 Then
            optNumDir(1).Value = True
        Else
            MsgBox "Invalid Number of Direction = " + Str(Val(inpline)) + "- Using Default Value."
            optNumDir(0).Value = True
        End If
        
        Line Input #nn, inpline
        numspd = Val(inpline)
        
        Line Input #nn, inpline
        lenval = Len(inpline)
        
        num = 0
        Do Until lenval <= 0
            comcol = InStr(inpline, ",")
            If comcol <> 0 Then
                num = num + 1
                ReDim Preserve tmpval(num) As Single
                tmpval(num) = Val(Mid(inpline, 1, comcol - 1))
                If comcol < lenval Then
                    inpline = Mid(inpline, comcol + 1)
                Else
                    inpline = ""
                End If
            Else
                num = num + 1
                ReDim Preserve tmpval(num) As Single
                tmpval(num) = Val(inpline)
                inpline = ""
            End If
            lenval = Len(inpline)
        Loop
        
        spdSpdMid.Row = 1
        For i = 1 To num
            spdSpdMid.Col = i
            spdSpdMid.Text = tmpval(num)
        Next i
        
        Line Input #nn, inpline
        
        lenval = Len(inpline)
        num = 0
        ReDim tmpval(num) As Single
        Do Until lenval <= 0
            comcol = InStr(inpline, ",")
            If comcol > 0 Then
                num = num + 1
                ReDim Preserve tmpval(num) As Single
                tmpval(num) = Val(Mid(inpline, 1, comcol - 1))
                If comcol < lenval Then
                    inpline = Mid(inpline, comcol + 1)
                Else
                    inpline = ""
                End If
            Else
                num = num + 1
                ReDim Preserve tmpval(num) As Single
                tmpval(num) = Val(inpline)
                inpline = ""
            End If
            
            lenval = Len(inpline)
        Loop
        
        If num <> 12 Then
            MsgBox "Number of Maximum Temperatures = " + Str(num) + " is invalid." + Chr(13) + Chr(10) + "Values not written to Form."
        Else
            spdMonTemp.Row = 1
            For i = 1 To num
                spdMonTemp.Col = i
                spdMonTemp.Text = tmpval(i)
            Next i
        End If
        
        Line Input #nn, inpline
        
        lenval = Len(inpline)
        num = 0
        ReDim tmpval(num) As Single
        
        Do Until lenval <= 0
            comcol = InStr(inpline, ",")
            If comcol > 0 Then
                num = num + 1
                ReDim Preserve tmpval(num) As Single
                tmpval(num) = Val(Mid(inpline, 1, comcol - 1))
                If comcol < lenval Then
                    inpline = Mid(inpline, comcol + 1)
                Else
                    inpline = ""
                End If
            Else
                num = num + 1
                ReDim Preserve tmpval(num) As Single
                tmpval(num) = Val(inpline)
                inpline = ""
            End If
            lenval = Len(inpline)
        Loop
        
        If num <> 12 Then
            MsgBox "Number of Minimum Temperature = " + Str(num) + " is invalid." + Chr(13) + Chr(10) + "No values written to form."
        Else
            spdMonTemp.Row = 2
            For i = 1 To num
                spdMonTemp.Col = i
                spdMonTemp.Text = tmpval(i)
            Next i
        End If
        
        Line Input #nn, inpline
        If inpline = "F" Then
            chkPrecp.Value = 0
            frmPrecp.Visible = False
        Else
            chkPrecp.Value = 1
            frmPrecp.Visible = True
            For i = 1 To 4
                Line Input #nn, inpline
                lenval = Len(inpline)
                num = 0
                ReDim tmpval(num) As Single
                Do Until lenval <= 0
                    comcol = InStr(inpline, ",")
                    If comcol > 0 Then
                        num = num + 1
                        ReDim Preserve tmpval(num) As Single
                        tmpval(num) = Val(Mid(inpline, 1, comcol - 1))
                        If comcol < lenval Then
                            inpline = Mid(inpline, comcol + 1)
                        Else
                            inpline = ""
                        End If
                    Else
                        num = num + 1
                        ReDim Preserve tmpval(num) As Single
                        tmpval(num) = Val(inpline)
                        inpline = ""
                    End If
                    lenval = Len(inpline)
                Loop
                
                If num <> 5 Then
                    MsgBox "Invalid number (= " + Str(num) + ") of Probabilities for " + precp(i) + Chr(13) + Chr(10) + "No values written to form."
                Else
                    spdPrecp.Row = i
                    For j = 1 To num
                        spdPrecp.Col = j
                        spdPrecp.Text = tmpval(j)
                    Next j
                End If
            Next i
        End If 'Whether Precp or not
        Line Input #nn, inpline
        txtHrsJFD.Text = inpline
        
    Close nn
    
End Sub

Sub ChkVals(errtext As String)
    Dim i As Integer, j As Integer
    Dim mon(12) As String, precp(4) As String
    Dim stab(5) As String
    Dim noval As Boolean
    
    mon(1) = "JAN"
    mon(2) = "FEB"
    mon(3) = "MAR"
    mon(4) = "APR"
    mon(5) = "MAY"
    mon(6) = "JUN"
    mon(7) = "JUL"
    mon(8) = "AUG"
    mon(9) = "SEP"
    mon(10) = "OCT"
    mon(11) = "NOV"
    mon(12) = "DEC"
    
    precp(1) = "Light Rain"
    precp(2) = "Mod/Heavy Rain"
    precp(3) = "Light Snow"
    precp(4) = "Mod/Heavy Snow"
    
    stab(1) = "Stab A-B"
    stab(2) = "Stab C"
    stab(3) = "Stab D"
    stab(4) = "Stab E"
    stab(5) = "Stab F-G"
    
    errtext = ""
    
    If lblInpFile.Caption = "" Then
        errtext = errtext + "No name given for parameter file."
        errtext = errtext + Chr(13) + Chr(10)
    End If
    
    If lblJFDFile.Caption = "" Then
        errtext = errtext + "No name given for JFD file."
        errtext = errtext + Chr(13) + Chr(10)
    ElseIf Dir(lblJFDFile.Caption) = "" Then
        errtext = errtext + "Invalid name given for JFD file - doesn't exist."
        errtext = errtext + Chr(13) + Chr(10)
    End If
    
    If lblOutMetFile.Caption = "" Then
        errtext = errtext + "No name given for Output meteorological data file."
        errtext = errtext + Chr(13) + Chr(10)
    End If
    
    If Abs(Val(txtLat.Text)) > 90 Then
        errtext = errtext + "Invalid latitude - absolute value must be 90 deg or less."
        errtext = errtext + Chr(13) + Chr(10)
    End If
    
    If Val(txtWndHgt.Text) <= 0 Then
        errtext = errtext + "Invalid Anemometer Height - must be greater than zero."
        errtext = errtext + Chr(13) + Chr(10)
    End If

    If Val(txtz0Meas.Text) <= 0 Then
        errtext = errtext + "Invalid surface roughness at measurement site - must be greater than zero."
        errtext = errtext + Chr(13) + Chr(10)
    End If
    
    If Val(txtz0App.Text) <= 0 Then
        errtext = errtext + "Invalid surface roughness at application site - must be greater than zero."
        errtext = errtext + Chr(13) + Chr(10)
    End If
    
    If Val(txtHrsJFD.Text) <= 0 Then
        errtext = errtext + "Invalid number of hours in JFD - must be greater than zero."
        errtext = errtext + Chr(13) + Chr(10)
    End If
    
    If Val(txtNumStab.Text) < 1 Or Val(txtNumStab.Text) > 7 Then
        errtext = errtext + "Invalid number of stability classes - must be between 1 and 7."
        errtext = errtext + Chr(13) + Chr(10)
    End If
    
    spdSpdMid.Row = 1
    noval = True
    For i = 1 To 8
        spdSpdMid.Col = i
        If spdSpdMid.Text <> "" Then
            noval = False
            If Val(spdSpdMid.Text) <= 0 Or Val(spdSpdMid.Text) > 99.9 Then
                errtext = errtext + "Invalid midpoint speed for speed class " + Str(i) + " - must be between 0 and 99.9 m/s."
                errtext = errtext + Chr(13) + Chr(10)
            End If
        End If
    Next i
    
    If noval Then
        errtext = errtext + "Need to input speed midpoints."
        errtext = errtext + Chr(13) + Chr(10)
    End If
        
    spdMonTemp.Row = 1
    For i = 1 To 12
        spdMonTemp.Col = i
        If spdMonTemp.Text = "" Then
            errtext = errtext + "No Maximum Temperature given for " + mon(i)
            errtext = errtext + Chr(13) + Chr(10)
        End If
    Next i
    
    spdMonTemp.Row = 2
    For i = 1 To 12
        spdMonTemp.Col = i
        If spdMonTemp.Text = "" Then
            errtext = errtext + "No Minimum Temperature given for " + mon(i)
            errtext = errtext + Chr(13) + Chr(10)
        End If
    Next i
    
    If chkPrecp.Value = 1 Then
        For i = 1 To 4
            spdPrecp.Row = i
            For j = 1 To 5
                spdPrecp.Col = j
                If Val(spdPrecp.Text) < 0 Or Val(spdPrecp.Text) > 100 Then
                    errtext = errtext + "Invalid probability for " + precp(i) + "and" + stab(j)
                    errtext = errtext + Chr(13) + Chr(10)
                End If
            Next j
        Next i
    End If
    
End Sub


Sub GetGENIIVals(infile As String)
    Dim i As Integer
    Dim start As Integer, leng As Integer
    Dim inline As String, fnum As Integer
    Dim numspd As Integer

    fnum = FreeFile
    
    Open infile For Input As fnum
    
        'Skip first two lines (header)
        For i = 1 To 2
            Line Input #fnum, inline
        Next i
        
        Line Input #fnum, inline
        start = 1
        leng = 5
        numspd = Val(Mid(inline, start, leng))
        start = start + leng
        txtNumStab = Str(Val(Mid(inline, start, leng)))
        For i = 1 To 2
            start = start + leng
        Next i
        start = start + leng
        leng = 10
        txtWndHgt = Str(Val(Mid(inline, start, leng)))
        
        Line Input #fnum, inline
        
        spdSpdMid.Row = 1
        start = 1
        leng = 7
        For i = 1 To numspd
            spdSpdMid.Col = i
            spdSpdMid.Text = Val(Mid(inline, start, leng))
            start = start + leng
        Next i
               
    Close fnum
    

End Sub

Sub GetValsFromForm()
    Dim fname As String, fnum As Integer
    Dim i As Integer, j As Integer
    Dim outstr As String, numspd As Integer

    fname = lblInpFile.Caption
    fnum = FreeFile
    
    Open fname For Output As fnum
    
        Print #fnum, lblOutMetFile.Caption
        Print #fnum, lblJFDFile.Caption
        Print #fnum, LTrim(RTrim(gJFDType))
        Print #fnum, LTrim(RTrim(Val(txtLat.Text)))
        Print #fnum, LTrim(RTrim(Val(txtWndHgt.Text)))
        Print #fnum, LTrim(RTrim(Val(txtz0Meas.Text)))
        Print #fnum, LTrim(RTrim(Val(txtz0App.Text)))
        Print #fnum, LTrim(RTrim(Val(txtNumStab.Text)))
        If optNumDir(1).Value = True Then
            Print #fnum, "36"
        Else
            Print #fnum, "16"
        End If
        outstr = ""
        numspd = 0
        spdSpdMid.Row = 1
        For i = 1 To 8
            spdSpdMid.Col = i
            If spdSpdMid.Text <> "" Then
                numspd = numspd + 1
                outstr = outstr + LTrim(RTrim(Val(spdSpdMid.Text))) + ","
            End If
        Next i
        Print #fnum, LTrim(RTrim(numspd))
        Print #fnum, outstr
        For j = 1 To 2
            outstr = ""
            spdMonTemp.Row = j
            For i = 1 To 12
                spdMonTemp.Col = i
                outstr = outstr + LTrim(RTrim(Val(spdMonTemp.Text))) + ","
            Next i
            Print #fnum, outstr
        Next j
       
        If chkPrecp.Value = 0 Then
            Print #fnum, "F"
        Else
            Print #fnum, "T"
            For j = 1 To 4
                spdPrecp.Row = j
                outstr = ""
                For i = 1 To 5
                    spdPrecp.Col = i
                    outstr = outstr + LTrim(RTrim(Val(spdPrecp.Text))) + ","
                Next i
                Print #fnum, outstr
            Next j
        End If
        Print #fnum, LTrim(RTrim(Val(txtHrsJFD.Text)))
    
    Close fnum
    

End Sub


Sub LoadPrecp()
    Dim fname As String, fnum As Integer
    Dim i As Integer, j As Integer, k As Integer
    Dim errflg As Boolean, comcol As Integer
    Dim staname As String, stastate As String
    Dim numsta As Integer, lenval As Integer
    Dim inpline As String, prptype As String
    

    fname = App.Path + "\JFDPRECP.CSV"
    If Dir(fname) = "" Then
        lblPrecp.Caption = "Unable to find JFDPRECP.CSV file."
        cboPrecp.Visible = False
        Exit Sub
    Else
        lblPrecp.Visible = True
        cboPrecp.Visible = True
    End If
    fnum = FreeFile
    
    Open fname For Input As fnum
        Line Input #fnum, inpline
        numsta = Val(inpline)
        ReDim gprecp(4, 5, numsta)
       
        For i = 1 To numsta
            For j = 1 To 4
                Line Input #fnum, inpline
                lenval = Len(inpline)
                
                For k = 1 To 8
                    errflg = False
                    comcol = InStr(inpline, ",")
                    Select Case (k)
                        Case (1)
                            If comcol <= 0 Then
                                errflg = True
                            Else
                                staname = Mid(inpline, 1, comcol - 1)
                                If comcol < lenval Then
                                    inpline = Mid(inpline, comcol + 1)
                                Else
                                    errflg = True
                                End If
                                    
                            End If
                        Case (2)
                            If comcol <= 0 Then
                                errflg = True
                            Else
                                stastate = Mid(inpline, 1, comcol - 1)
                                If comcol < lenval Then
                                    inpline = Mid(inpline, comcol + 1)
                                Else
                                    errflg = True
                                End If
                            End If
                        Case (3)
                           If comcol <= 0 Then
                                errflg = True
                            Else
                                prptype = Mid(inpline, 1, comcol - 1)
                                If comcol < lenval Then
                                    inpline = Mid(inpline, comcol + 1)
                                Else
                                    errflg = True
                                End If
                            End If
                        Case 4 To 7
                            If comcol <= 0 Then
                                errflg = True
                            Else
                                gprecp(j, k - 3, i) = Val(Mid(inpline, 1, comcol - 1))
                                If comcol < lenval Then
                                    inpline = Mid(inpline, comcol + 1)
                                Else
                                    errflg = False
                                End If
                            End If
                        Case 8
                            gprecp(j, 4, i) = Val(inpline)
                    End Select
                    
                    If errflg = True Then
                        lblPrecp.Caption = "Error in JFDPRECP.CSV file."
                        cboPrecp.Visible = False
                        Close fnum
                        Exit Sub
                    End If
                    
                Next k
                If j = 1 Then
                    cboPrecp.AddItem staname + "," + stastate
                End If
                 
            Next j
        Next i
                                          
    
    
    Close fnum
    

End Sub

Private Sub cboPrecp_Click()
    Dim i As Integer, j As Integer, indx As Integer
    
    indx = cboPrecp.ListIndex
    For i = 1 To 4
        spdPrecp.Row = i
        For j = 1 To 5
            spdPrecp.Col = j
            spdPrecp.Text = gprecp(i, j, indx + 1)
        Next j
    Next i
    
End Sub


Private Sub chkprecp_Click()
    If chkPrecp.Value = 0 Then
        frmPrecp.Visible = False
    Else
        frmPrecp.Visible = True
    End If
    
End Sub


Private Sub cmdExit_Click()
    Dim msgtext As String, response As Integer
    
    msgtext = "Do you wish to Exit the Program?"
    response = MsgBox(msgtext, vbYesNo, "Exit Program?")
    If response = vbYes Then
        End
    Else
        Exit Sub
    End If
    
End Sub

Private Sub cmdProcJFD_Click()
    Dim errtext As String, response As Integer
    Dim runcode As String, sh As Integer, de As Integer
    
    Call ChkVals(errtext)
    If errtext <> "" Then
        response = MsgBox(errtext, vbExclamation, "ERROR!")
        Exit Sub
    End If
    
    Call GetValsFromForm
    
    runcode = "Command.COM /C " + App.Path + "\runcode.bat " + App.Path + "\jjfdproc " + lblInpFile.Caption
    
    'sh = Shell(runcode, 1)
    'If sh <= 0 Then
    '    errtext = "Unable to shell " + runcode
    '    response = MsgBox(errtext, vbExclamation, "ERROR!")
    '    Exit Sub
    'End If
    
    'While GetModuleUsage(sh) > 0
    '    de = DoEvents()
    'Wend
    
    Call ExecCmd(runcode)

End Sub


Private Sub Form_Load()
    Dim i As Integer
    Dim mon(12) As String, stab(5) As String, precp(4) As String
    
    mon(1) = "JAN"
    mon(2) = "FEB"
    mon(3) = "MAR"
    mon(4) = "APR"
    mon(5) = "MAY"
    mon(6) = "JUN"
    mon(7) = "JUL"
    mon(8) = "AUG"
    mon(9) = "SEP"
    mon(10) = "OCT"
    mon(11) = "NOV"
    mon(12) = "DEC"
    
    stab(1) = "Stab A-B"
    stab(2) = "Stab C"
    stab(3) = "Stab D"
    stab(4) = "Stab E"
    stab(5) = "Stab F-G"
    
    precp(1) = "Light Rain"
    precp(2) = "Mod/Heavy Rain"
    precp(3) = "Light Snow"
    precp(4) = "Mod/Heavy Snow"
    
    lblInpFile.Caption = App.Path + "\JFDPROC.INP"
    lblOutMetFile.Caption = ""
    lblJFDFile.Caption = ""
    
    txtLat.Text = ""
    txtWndHgt.Text = ""
    txtz0Meas.Text = ""
    txtz0App.Text = ""
    txtHrsJFD.Text = 8760
    
    txtNumStab = ""
    optNumDir(0).Value = True
    
    For i = 1 To 10
        spdSpdMid.Row = 0
        spdSpdMid.Col = i
        spdSpdMid.Text = "Class " + LTrim(RTrim(i))
        spdSpdMid.Row = 1
        spdSpdMid.Text = ""
    Next i
    
    spdMonTemp.Col = 0
    spdMonTemp.Row = 1
    spdMonTemp.Text = "Maximum"
    spdMonTemp.Row = 2
    spdMonTemp.Text = "Minimum"
    
    For i = 1 To 12
        spdMonTemp.Row = 0
        spdMonTemp.Col = i
        spdMonTemp.Text = mon(i)
        spdMonTemp.Row = 1
        spdMonTemp.Text = ""
        spdMonTemp.Row = 2
        spdMonTemp.Text = ""
    Next i
    
    chkPrecp.Value = 0
    frmPrecp.Visible = False
        
    Call LoadPrecp
    
    spdPrecp.Row = 0
    For i = 1 To 5
        spdPrecp.Col = i
        spdPrecp.Text = stab(i)
    Next i
    
    spdPrecp.Col = 0
    For i = 1 To 4
        spdPrecp.Row = i
        spdPrecp.Text = precp(i)
    Next i
    
    If Dir(lblInpFile.Caption) <> "" Then
        Call ChkParm(lblInpFile.Caption)
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
            Ftitle = "Change Joint Frequency File"
            fname = lblJFDFile.Caption
            filt = "All Files (*.*)|*.*|JFD Files (*.JFD)|*.JFD"
        Case (2)
            Ftitle = "Change Output Hourly Met File"
            fname = lblOutMetFile.Caption
            filt = "All Files (*.*)|*.*|Met Files (*.MET)|*.MET"
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
                    'Exit Sub
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
               
            msgtext = "Program only supports two formats - EPA's STAR and GENII." + Chr(13) + Chr(10)
            msgtext = msgtext + "Select Yes if file in EPA's STAR; Select NO if file in GENII format; Otherwise, select Cancel"
            response = MsgBox(msgtext, vbYesNoCancel, "Format")
            If response = vbYes Then
                gJFDType = 0
                lblJFDLabel.Caption = "Path and Name of Joint Frequency Distribution File: (EPA's STAR Format)"
            ElseIf response = vbNo Then
                gJFDType = 1
                lblJFDLabel.Caption = "Path and Name of Joint Frequency Distribution File: (GENII Format)"
                Call GetGENIIVals(tmpfile)
            Else
                Call mnuFiChange_Click(Index)
                Exit Sub
            End If
            
            lblJFDFile.Caption = tmpfile
            
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


