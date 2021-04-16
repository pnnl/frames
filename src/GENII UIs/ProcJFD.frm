VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Object = "{BDC217C8-ED16-11CD-956C-0000C04E4C0A}#1.1#0"; "TABCTL32.OCX"
Object = "{B02F3647-766B-11CE-AF28-C3A2FBE76A13}#2.5#0"; "Ss32x25.ocx"
Begin VB.Form frmProcJFD 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Input for Processing Joint Frequency Distribution"
   ClientHeight    =   5760
   ClientLeft      =   105
   ClientTop       =   390
   ClientWidth     =   7680
   Icon            =   "ProcJFD.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   384
   ScaleMode       =   3  'Pixel
   ScaleWidth      =   512
   StartUpPosition =   2  'CenterScreen
   Begin VB.Frame Frame3 
      Height          =   2028
      Left            =   96
      TabIndex        =   17
      Top             =   0
      Width           =   7500
      Begin VB.CommandButton Command1 
         Caption         =   "Browse"
         Height          =   300
         Index           =   2
         Left            =   6528
         TabIndex        =   26
         Top             =   1572
         Width           =   780
      End
      Begin VB.CommandButton Command1 
         Caption         =   "Browse"
         Height          =   300
         Index           =   1
         Left            =   6528
         TabIndex        =   25
         Top             =   996
         Width           =   780
      End
      Begin VB.CommandButton Command1 
         Caption         =   "Browse"
         Height          =   300
         Index           =   0
         Left            =   6528
         TabIndex        =   24
         Top             =   432
         Width           =   780
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         Caption         =   "Path to store intermediate parameters entered on this page"
         Height          =   195
         Left            =   195
         TabIndex        =   23
         Top             =   195
         Width           =   4125
      End
      Begin VB.Label lblInpFile 
         BackColor       =   &H80000005&
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Label2"
         Height          =   288
         Left            =   480
         TabIndex        =   22
         Top             =   432
         Width           =   5904
      End
      Begin VB.Label lblJFDLabel 
         AutoSize        =   -1  'True
         Caption         =   "Path of Joint Frequency Distribution file"
         Height          =   192
         Left            =   192
         TabIndex        =   21
         Top             =   768
         Width           =   2736
      End
      Begin VB.Label lblJFDFile 
         BackColor       =   &H80000005&
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Label4"
         Height          =   288
         Left            =   480
         TabIndex        =   20
         Top             =   1008
         Width           =   5904
      End
      Begin VB.Label Label5 
         AutoSize        =   -1  'True
         Caption         =   "Path of Output Meteorological Data file"
         Height          =   192
         Left            =   192
         TabIndex        =   19
         Top             =   1344
         Width           =   2724
      End
      Begin VB.Label lblOutMetFile 
         BackColor       =   &H80000005&
         BorderStyle     =   1  'Fixed Single
         Caption         =   "Label6"
         Height          =   288
         Left            =   480
         TabIndex        =   18
         Top             =   1584
         Width           =   5904
      End
   End
   Begin VB.CommandButton cmdExit 
      Caption         =   "Exit"
      Height          =   396
      Left            =   6432
      TabIndex        =   9
      Top             =   5280
      Width           =   1164
   End
   Begin VB.CommandButton cmdProcJFD 
      Caption         =   "Process Joint Frequency Distribution"
      Height          =   396
      Left            =   3360
      TabIndex        =   8
      Top             =   5280
      Width           =   2892
   End
   Begin TabDlg.SSTab SSTab1 
      Height          =   2988
      Left            =   96
      TabIndex        =   0
      Top             =   2208
      Width           =   7500
      _ExtentX        =   13229
      _ExtentY        =   5265
      _Version        =   393216
      Style           =   1
      Tab             =   2
      TabHeight       =   423
      TabCaption(0)   =   "Station Information"
      TabPicture(0)   =   "ProcJFD.frx":0442
      Tab(0).ControlEnabled=   0   'False
      Tab(0).Control(0)=   "Frame2"
      Tab(0).ControlCount=   1
      TabCaption(1)   =   "Joint Frequency Info"
      TabPicture(1)   =   "ProcJFD.frx":045E
      Tab(1).ControlEnabled=   0   'False
      Tab(1).Control(0)=   "Label11"
      Tab(1).Control(1)=   "Label13"
      Tab(1).Control(2)=   "Label2"
      Tab(1).Control(3)=   "spdSpdMid"
      Tab(1).Control(4)=   "spdMonTemp"
      Tab(1).Control(5)=   "Frame1"
      Tab(1).Control(6)=   "txtNumStab"
      Tab(1).ControlCount=   7
      TabCaption(2)   =   "Precipitation"
      TabPicture(2)   =   "ProcJFD.frx":047A
      Tab(2).ControlEnabled=   -1  'True
      Tab(2).Control(0)=   "Frame4"
      Tab(2).Control(0).Enabled=   0   'False
      Tab(2).Control(1)=   "frmPrecp"
      Tab(2).Control(1).Enabled=   0   'False
      Tab(2).Control(2)=   "chkPrecp"
      Tab(2).Control(2).Enabled=   0   'False
      Tab(2).ControlCount=   3
      Begin VB.ComboBox txtNumStab 
         Height          =   288
         ItemData        =   "ProcJFD.frx":0496
         Left            =   -72504
         List            =   "ProcJFD.frx":04AF
         Style           =   2  'Dropdown List
         TabIndex        =   38
         Top             =   480
         Width           =   780
      End
      Begin VB.CheckBox chkPrecp 
         Caption         =   "Include Precipitation"
         Height          =   192
         Left            =   480
         TabIndex        =   7
         Top             =   576
         Width           =   2028
      End
      Begin VB.Frame Frame2 
         Height          =   2508
         Left            =   -74808
         TabIndex        =   16
         Top             =   288
         Width           =   7116
         Begin VB.TextBox txtLat 
            Height          =   288
            Left            =   5100
            TabIndex        =   31
            Text            =   "Text1"
            Top             =   288
            Width           =   1000
         End
         Begin VB.TextBox txtWndHgt 
            Height          =   288
            Left            =   5100
            TabIndex        =   30
            Text            =   "Text2"
            Top             =   672
            Width           =   1000
         End
         Begin VB.TextBox txtz0Meas 
            Height          =   288
            Left            =   5100
            TabIndex        =   29
            Text            =   "Text3"
            Top             =   1056
            Width           =   1000
         End
         Begin VB.TextBox txtz0App 
            Height          =   288
            Left            =   5100
            TabIndex        =   28
            Text            =   "Text4"
            Top             =   1440
            Width           =   1000
         End
         Begin VB.TextBox txtHrsJFD 
            Height          =   288
            Left            =   5100
            TabIndex        =   27
            Text            =   "Text5"
            Top             =   1824
            Width           =   1000
         End
         Begin VB.Label Label3 
            Caption         =   "Surface roughness application site (m)"
            Height          =   252
            Left            =   288
            TabIndex        =   36
            Top             =   1536
            Width           =   3996
         End
         Begin VB.Label Label7 
            Caption         =   "Latitude (deg) (+ North of Equator)"
            Height          =   252
            Left            =   288
            TabIndex        =   35
            Top             =   384
            Width           =   3996
         End
         Begin VB.Label Label8 
            Caption         =   "Anemometer Height (m)"
            Height          =   252
            Left            =   288
            TabIndex        =   34
            Top             =   768
            Width           =   3996
         End
         Begin VB.Label Label9 
            Caption         =   "Surface roughness measurement site (m)"
            Height          =   252
            Left            =   288
            TabIndex        =   33
            Top             =   1152
            Width           =   3996
         End
         Begin VB.Label Label10 
            Caption         =   "Number of Hours in JFD"
            Height          =   252
            Left            =   288
            TabIndex        =   32
            Top             =   1920
            Width           =   3996
         End
      End
      Begin VB.Frame frmPrecp 
         Height          =   2508
         Left            =   192
         TabIndex        =   10
         Top             =   288
         Width           =   7116
         Begin VB.ComboBox cboPrecp 
            Height          =   288
            Left            =   2880
            Style           =   2  'Dropdown List
            TabIndex        =   12
            Top             =   672
            Width           =   2340
         End
         Begin FPSpread.vaSpread spdPrecp 
            Height          =   1152
            Left            =   288
            TabIndex        =   11
            ToolTipText     =   "Enter the percent of the time it is raining when the conditions are in this category"
            Top             =   1152
            Width           =   6360
            _Version        =   131077
            _ExtentX        =   11245
            _ExtentY        =   2143
            _StockProps     =   64
            AutoSize        =   -1  'True
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
            ScrollBarExtMode=   -1  'True
            SpreadDesigner  =   "ProcJFD.frx":04C8
            VisibleCols     =   5
            VisibleRows     =   4
         End
         Begin VB.Label lblPrecp 
            Caption         =   "Choose a representative City"
            Height          =   252
            Left            =   288
            TabIndex        =   13
            Top             =   672
            Width           =   2532
         End
      End
      Begin VB.Frame Frame1 
         Caption         =   "Number of Wind Direction Sectors"
         Height          =   588
         Left            =   -70776
         TabIndex        =   2
         Top             =   384
         Width           =   3036
         Begin VB.OptionButton optNumDir 
            Caption         =   "36"
            Height          =   192
            Index           =   1
            Left            =   1536
            TabIndex        =   4
            Top             =   288
            Width           =   852
         End
         Begin VB.OptionButton optNumDir 
            Caption         =   "16"
            Height          =   192
            Index           =   0
            Left            =   288
            TabIndex        =   3
            Top             =   288
            Width           =   852
         End
      End
      Begin FPSpread.vaSpread spdMonTemp 
         Height          =   1380
         Left            =   -70776
         TabIndex        =   15
         Top             =   1344
         Width           =   2592
         _Version        =   131077
         _ExtentX        =   4763
         _ExtentY        =   2566
         _StockProps     =   64
         AutoSize        =   -1  'True
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         MaxCols         =   2
         MaxRows         =   12
         ScrollBarExtMode=   -1  'True
         ScrollBars      =   2
         ScrollBarShowMax=   0   'False
         SpreadDesigner  =   "ProcJFD.frx":0631
         VisibleCols     =   2
         VisibleRows     =   5
      End
      Begin FPSpread.vaSpread spdSpdMid 
         Height          =   1380
         Left            =   -74712
         TabIndex        =   5
         Top             =   1344
         Width           =   2604
         _Version        =   131077
         _ExtentX        =   4789
         _ExtentY        =   2566
         _StockProps     =   64
         AutoSize        =   -1  'True
         BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         MaxCols         =   1
         MaxRows         =   10
         ScrollBarExtMode=   -1  'True
         ScrollBars      =   2
         ScrollBarShowMax=   0   'False
         SpreadDesigner  =   "ProcJFD.frx":0788
         VisibleCols     =   1
         VisibleRows     =   5
      End
      Begin VB.Frame Frame4 
         Height          =   2508
         Left            =   192
         TabIndex        =   37
         Top             =   288
         Width           =   7116
      End
      Begin VB.Label Label2 
         AutoSize        =   -1  'True
         Caption         =   "Temperature (deg F)"
         Height          =   192
         Left            =   -70776
         TabIndex        =   14
         Top             =   1152
         Width           =   1488
      End
      Begin VB.Label Label13 
         AutoSize        =   -1  'True
         Caption         =   "Wind Speed Mid Points (m/s):"
         Height          =   192
         Left            =   -74712
         TabIndex        =   6
         Top             =   1152
         Width           =   2136
      End
      Begin VB.Label Label11 
         Caption         =   "Number of Stability Classes"
         Height          =   252
         Left            =   -74712
         TabIndex        =   1
         Top             =   480
         Width           =   2172
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
Attribute VB_Name = "frmProcJFD"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Dim gprecp() As Double
Dim gJFDType As Long
Sub ChkParm(infile As String)
    Dim nn As Long, inpline As String
    Dim num As Long, lenval As Long, tmpval() As Double
    Dim i As Long, j As Long
    Dim comcol As Long, numspd As Long
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
                ReDim Preserve tmpval(num) As Double
                tmpval(num) = Val(Mid(inpline, 1, comcol - 1))
                If comcol < lenval Then
                    inpline = Mid(inpline, comcol + 1)
                Else
                    inpline = ""
                End If
            Else
                num = num + 1
                ReDim Preserve tmpval(num) As Double
                tmpval(num) = Val(inpline)
                inpline = ""
            End If
            lenval = Len(inpline)
        Loop
        
        spdSpdMid.Col = 1
        For i = 1 To num
            spdSpdMid.Row = i
            spdSpdMid.Text = tmpval(i)
        Next i
        
        Line Input #nn, inpline
        
        lenval = Len(inpline)
        num = 0
        ReDim tmpval(num) As Double
        Do Until lenval <= 0
            comcol = InStr(inpline, ",")
            If comcol > 0 Then
                num = num + 1
                ReDim Preserve tmpval(num) As Double
                tmpval(num) = Val(Mid(inpline, 1, comcol - 1))
                If comcol < lenval Then
                    inpline = Mid(inpline, comcol + 1)
                Else
                    inpline = ""
                End If
            Else
                num = num + 1
                ReDim Preserve tmpval(num) As Double
                tmpval(num) = Val(inpline)
                inpline = ""
            End If
            
            lenval = Len(inpline)
        Loop
        
        If num <> 12 Then
            MsgBox "Number of Maximum Temperatures = " + Str(num) + " is invalid." + Chr(13) + Chr(10) + "Values not written to Form."
        Else
            spdMonTemp.Col = 1
            For i = 1 To num
                spdMonTemp.Row = i
                spdMonTemp.Text = tmpval(i)
            Next i
        End If
        
        Line Input #nn, inpline
        
        lenval = Len(inpline)
        num = 0
        ReDim tmpval(num) As Double
        
        Do Until lenval <= 0
            comcol = InStr(inpline, ",")
            If comcol > 0 Then
                num = num + 1
                ReDim Preserve tmpval(num) As Double
                tmpval(num) = Val(Mid(inpline, 1, comcol - 1))
                If comcol < lenval Then
                    inpline = Mid(inpline, comcol + 1)
                Else
                    inpline = ""
                End If
            Else
                num = num + 1
                ReDim Preserve tmpval(num) As Double
                tmpval(num) = Val(inpline)
                inpline = ""
            End If
            lenval = Len(inpline)
        Loop
        
        If num <> 12 Then
            MsgBox "Number of Minimum Temperature = " + Str(num) + " is invalid." + Chr(13) + Chr(10) + "No values written to form."
        Else
            spdMonTemp.Col = 2
            For i = 1 To num
                spdMonTemp.Row = i
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
                ReDim tmpval(num) As Double
                Do Until lenval <= 0
                    comcol = InStr(inpline, ",")
                    If comcol > 0 Then
                        num = num + 1
                        ReDim Preserve tmpval(num) As Double
                        tmpval(num) = Val(Mid(inpline, 1, comcol - 1))
                        If comcol < lenval Then
                            inpline = Mid(inpline, comcol + 1)
                        Else
                            inpline = ""
                        End If
                    Else
                        num = num + 1
                        ReDim Preserve tmpval(num) As Double
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
    Dim i As Long, j As Long
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
    
    spdSpdMid.Col = 1
    noval = True
    For i = 1 To 8
        spdSpdMid.Row = i
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
        
    spdMonTemp.Col = 1
    For i = 1 To 12
        spdMonTemp.Row = i
        If spdMonTemp.Text = "" Then
            errtext = errtext + "No Maximum Temperature given for " + mon(i)
            errtext = errtext + Chr(13) + Chr(10)
        End If
    Next i
    
    spdMonTemp.Col = 2
    For i = 1 To 12
        spdMonTemp.Row = i
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
    Dim i As Long
    Dim start As Long, leng As Long
    Dim inline As String, fnum As Long
    Dim numspd As Long

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
        txtNumStab = Val(Mid(inline, start, leng))
'        txtNumStab = Str(Val(Mid(inline, start, leng)))
        For i = 1 To 2
            start = start + leng
        Next i
        start = start + leng
        leng = 10
        txtWndHgt = Str(Val(Mid(inline, start, leng)))
        
        Line Input #fnum, inline
        
        spdSpdMid.Col = 1
        start = 1
        leng = 7
        For i = 1 To numspd
            spdSpdMid.Row = i
            spdSpdMid.Text = Val(Mid(inline, start, leng))
            start = start + leng
        Next i
               
    Close fnum
    

End Sub

Sub GetValsFromForm()
    Dim fname As String, fnum As Long
    Dim i As Long, j As Long
    Dim outstr As String, numspd As Long

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
        spdSpdMid.Col = 1
        For i = 1 To 8
            spdSpdMid.Row = i
            If spdSpdMid.Text <> "" Then
                numspd = numspd + 1
                outstr = outstr + LTrim(RTrim(Val(spdSpdMid.Text))) + ","
            End If
        Next i
        Print #fnum, LTrim(RTrim(numspd))
        Print #fnum, outstr
        For j = 1 To 2
            outstr = ""
            spdMonTemp.Col = j
            For i = 1 To 12
                spdMonTemp.Row = i
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
    Dim fname As String, fnum As Long
    Dim i As Long, j As Long, k As Long
    Dim errflg As Boolean, comcol As Long
    Dim staname As String, stastate As String
    Dim numsta As Long, lenval As Long
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
                            gprecp(j, 5, i) = Val(inpline)
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
    Dim i As Long, j As Long, indx As Long
    
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
    Dim msgtext As String, response As Long
    
    msgtext = "Do you wish to Exit the Program?"
    response = MsgBox(msgtext, vbYesNo, "Exit Program?")
    If response = vbYes Then
        End
    Else
        Exit Sub
    End If
End Sub

Private Sub cmdProcJFD_Click()
    Dim errtext As String, response As Long
    Dim runcode As String, sh As Long, de As Long
    
    ChkVals errtext
    If errtext <> "" Then
        response = MsgBox(errtext, vbExclamation, "ERROR!")
        Exit Sub
    End If
    
    GetValsFromForm
    
'    runcode = "Command.COM /C " + App.Path + "\runcode.bat " + App.Path + "\jjfdproc " + lblInpFile.Caption
    runcode = App.Path + "\jjfdproc " + lblInpFile.Caption
    
    'sh = Shell(runcode, 1)
    'If sh <= 0 Then
    '    errtext = "Unable to shell " + runcode
    '    response = MsgBox(errtext, vbExclamation, "ERROR!")
    '    Exit Sub
    'End If
    
    'While GetModuleUsage(sh) > 0
    '    de = DoEvents()
    'Wend
    
    ExecCmd runcode

End Sub

Private Sub Form_Load()
    Dim i As Long
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
    
    'txtNumStab = ""
    optNumDir(0).Value = True
    
    For i = 1 To 10
        spdSpdMid.Col = 0
        spdSpdMid.Row = i
        spdSpdMid.Text = "Wind Speed " + LTrim(RTrim(i))
        spdSpdMid.Col = 1
        spdSpdMid.Text = ""
    Next i
    
    spdMonTemp.Row = 0
    spdMonTemp.Col = 1
    spdMonTemp.Text = "Maximum"
    spdMonTemp.Col = 2
    spdMonTemp.Text = "Minimum"
    
    For i = 1 To 12
        spdMonTemp.Col = 0
        spdMonTemp.Row = i
        spdMonTemp.Text = mon(i)
        spdMonTemp.Col = 1
        spdMonTemp.Text = ""
        spdMonTemp.Col = 2
        spdMonTemp.Text = ""
    Next i
    
    chkPrecp.Value = 0
    frmPrecp.Visible = False
        
    LoadPrecp
    
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
        ChkParm lblInpFile.Caption
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
                    'Exit Sub
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
               
            msgtext = "Program only supports two formats - EPA's STAR and GENII." + Chr(13) + Chr(10)
            msgtext = msgtext + "Select Yes if file in EPA's STAR; Select NO if file in GENII format; Otherwise, select Cancel"
            response = MsgBox(msgtext, vbYesNoCancel, "Format")
            If response = vbYes Then
                gJFDType = 0
                lblJFDLabel.Caption = "Path and Name of Joint Frequency Distribution File: (EPA's STAR Format)"
            ElseIf response = vbNo Then
                gJFDType = 1
                lblJFDLabel.Caption = "Path and Name of Joint Frequency Distribution File: (GENII Format)"
                GetGENIIVals tmpfile
            Else
                Command1_Click Index
                Exit Sub
            End If
            
            lblJFDFile.Caption = tmpfile
            
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

