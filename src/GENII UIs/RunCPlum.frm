VERSION 5.00
Begin VB.Form frmRunCPlume 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Form1"
   ClientHeight    =   540
   ClientLeft      =   900
   ClientTop       =   1224
   ClientWidth     =   5784
   Icon            =   "RunCPlum.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   540
   ScaleWidth      =   5784
   StartUpPosition =   2  'CenterScreen
   Visible         =   0   'False
   Begin VB.Label lblProcess 
      Caption         =   "Processing Data - Please Wait"
      Height          =   252
      Left            =   600
      TabIndex        =   0
      Top             =   120
      Width           =   4812
   End
End
Attribute VB_Name = "frmRunCPlume"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Private Sub Form_Load()
    Dim loadng As Boolean
    Dim etext As String
    Dim runfile As String, runcode As String
    Dim sh As Long
    Dim de As Long
    
    GetArguments
    
    If argc >= 5 Then
        Loading.Gauge1.Max = 1500
        Loading.Show
        loadng = True
        FUIName = LTrim(RTrim(argv(0))) & ".GID"
        RunName = LTrim(RTrim(argv(1)))
        siteIdx = Val(argv(2))
        modIdx = Val(argv(3))
        modName = argv(4)
        LstData.RSFile = LTrim(RTrim(RunName)) + ".RS"
        LstData.OutFile = LTrim(RTrim(RunName)) + ".ATO"
        LstData.NucDataFile = LTrim(RTrim(RunName)) + ".NUC"
               
        If open_csv(errfile, LTrim(RTrim(RunName)) & ".ERR", 1) Then
            put_val errfile, "Error report for Atmospheric Model"
            put_line errfile
        Else
            MsgBox "Unable to create file " & RunName & ".ERR" & Chr(10) & "Check directory permission"
            End
        End If
                
        GetGIDStuff
        GetAFFStuff "chronic"
        LSTWrite
        RSWrite
        ChainsWrite
        
        Unload Loading
        loadng = False
    
        'Run Code
        runfile = App.Path + "\chplume.bat "
        runcode = App.Path + "\chronsrc.exe "
        If Dir(runfile) = "" Then
            etext = "Unable to find chplume.bat - reinstall codes"
            put_val errfile, etext
            put_line errfile
            MsgBox etext
            close_csv errfile
            End
        End If
        
        If Dir(runcode) = "" Then
            etext = "Unabled to find chronsrc.exe - reinstall codes"
            put_val errfile, etext
            put_line errfile
            MsgBox etext
            close_csv errfile
            End
        End If
        
        runfile = runfile + runcode + LTrim(RTrim(RunName)) + ".LST " + modName + " " + LTrim(RTrim(RunName)) + ".ERP " + " " + RunName
        ExecCmd runfile
    Else
        MsgBox "Not enough arguements passed " & Chr(10) & "Contact PNNL"
        put_val errfile, "No enough arguements passed " + Chr(10) + "Contact PNNL"
        put_line errfile
        close_csv errfile
        End
    End If
    
    If Dir(LTrim(RTrim(RunName)) + ".ERP") <> "" Then
        put_val errfile, "Error running Chronsrc.exe"
        put_line errfile
        close_csv errfile
        Kill LTrim(RTrim(RunName)) + ".ERP"
    Else
        close_csv errfile
        Kill RunName + ".ERR"
    End If
    
    End
End Sub
