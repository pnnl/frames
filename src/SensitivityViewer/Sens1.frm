VERSION 5.00
Object = "{0BA686C6-F7D3-101A-993E-0000C0EF6F5E}#1.0#0"; "threed32.ocx"
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "COMCTL32.OCX"
Begin VB.Form frmView 
   Caption         =   "MEPAS Sensitivity/Uncertainty Viewer using (R)"
   ClientHeight    =   750
   ClientLeft      =   5100
   ClientTop       =   4950
   ClientWidth     =   4605
   ControlBox      =   0   'False
   Icon            =   "Sens1.frx":0000
   LinkTopic       =   "Form1"
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   750
   ScaleWidth      =   4605
   Begin VB.Timer Timer1 
      Interval        =   300
      Left            =   1
      Top             =   1
   End
   Begin Threed.SSFrame SSFrame2 
      Height          =   660
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   4500
      _Version        =   65536
      _ExtentX        =   7937
      _ExtentY        =   1164
      _StockProps     =   14
      Caption         =   "Status"
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Begin ComctlLib.ProgressBar ProgressBar1 
         Height          =   255
         Left            =   120
         TabIndex        =   15
         Top             =   240
         Width           =   4215
         _ExtentX        =   7435
         _ExtentY        =   450
         _Version        =   327682
         Appearance      =   1
      End
      Begin VB.Label lblMemStat 
         Height          =   252
         Index           =   0
         Left            =   2760
         TabIndex        =   14
         Top             =   1320
         Width           =   1452
      End
      Begin VB.Label Label1 
         Alignment       =   1  'Right Justify
         Caption         =   "Percent of memory in use:"
         Height          =   252
         Index           =   0
         Left            =   360
         TabIndex        =   13
         Top             =   1320
         Width           =   2292
      End
      Begin VB.Label Label1 
         Alignment       =   1  'Right Justify
         Caption         =   "Bytes of physical memory:"
         Height          =   252
         Index           =   1
         Left            =   360
         TabIndex        =   12
         Top             =   1680
         Width           =   2292
      End
      Begin VB.Label Label1 
         Alignment       =   1  'Right Justify
         Caption         =   "Free physical memory:"
         Height          =   252
         Index           =   2
         Left            =   360
         TabIndex        =   11
         Top             =   2040
         Width           =   2292
      End
      Begin VB.Label Label1 
         Alignment       =   1  'Right Justify
         Caption         =   "Paging file (bytes):"
         Height          =   252
         Index           =   3
         Left            =   360
         TabIndex        =   10
         Top             =   2400
         Width           =   2292
      End
      Begin VB.Label Label1 
         Alignment       =   1  'Right Justify
         Caption         =   "Free paging file (bytes):"
         Height          =   252
         Index           =   4
         Left            =   360
         TabIndex        =   9
         Top             =   2760
         Width           =   2292
      End
      Begin VB.Label Label1 
         Alignment       =   1  'Right Justify
         Caption         =   "User bytes of address space:"
         Height          =   252
         Index           =   5
         Left            =   360
         TabIndex        =   8
         Top             =   3120
         Width           =   2292
      End
      Begin VB.Label Label1 
         Alignment       =   1  'Right Justify
         Caption         =   "Free user bytes:"
         Height          =   252
         Index           =   6
         Left            =   360
         TabIndex        =   7
         Top             =   3480
         Width           =   2292
      End
      Begin VB.Label lblMemStat 
         Height          =   252
         Index           =   1
         Left            =   2760
         TabIndex        =   6
         Top             =   1680
         Width           =   1452
      End
      Begin VB.Label lblMemStat 
         Height          =   252
         Index           =   2
         Left            =   2760
         TabIndex        =   5
         Top             =   2040
         Width           =   1452
      End
      Begin VB.Label lblMemStat 
         Height          =   252
         Index           =   3
         Left            =   2760
         TabIndex        =   4
         Top             =   2400
         Width           =   1452
      End
      Begin VB.Label lblMemStat 
         Height          =   252
         Index           =   4
         Left            =   2760
         TabIndex        =   3
         Top             =   2760
         Width           =   1452
      End
      Begin VB.Label lblMemStat 
         Height          =   252
         Index           =   5
         Left            =   2760
         TabIndex        =   2
         Top             =   3120
         Width           =   1452
      End
      Begin VB.Label lblMemStat 
         Height          =   252
         Index           =   6
         Left            =   2760
         TabIndex        =   1
         Top             =   3480
         Width           =   1452
      End
   End
End
Attribute VB_Name = "frmView"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Dim VarInNames As New Collection
Dim ShortInNames As New Collection
Dim InNames As New Collection
Dim VarOutNames As New Collection
Dim ShortOutNames As New Collection
Dim OutNames As New Collection
Dim AllShortOutNames As New Collection
Dim OutOpt As New Collection

Private Sub Form_Load()
  Dim pfile As parmfile
  
  GetArguments
  Call StartModule(frmView, "MEPAS Sensitivity/Uncertainty Viewer", argc)
End Sub

Private Sub writeCSV()
  Dim suf As csv
  Dim Module As String
  Dim temp As String
  Dim first As String
  Dim count As Integer
  Dim read As Boolean
  Dim i As Integer
  Dim j As Integer
  Dim k As Integer
  Dim ins As Integer
  Dim outs As Integer
  Dim chPos As Long
  Dim temp2 As String
  If (open_csv(suf, argv(1) & ".suf", F_READ)) Then
    Open argv(2) & ".csv" For Output As 65
    Module = get_val(suf)
    count = Val(get_val(suf))
    Call get_line(suf)
    read = False
    While (Not read And Not EOCF(suf))
      If (Not (Module = argv(5))) Then
        For i = 1 To count
          Call get_line(suf)
        Next i
      Else
        read = True
        count = Val(get_val(suf))
        For i = 1 To count
          Call get_line(suf)
          temp = get_val(suf)
        Next i
        Call get_line(suf)
        ins = Val(get_val(suf))
        outs = Val(get_val(suf))
        Call get_line(suf)
        For i = 1 To ins
          temp = get_val(suf)
          temp = Replace(temp, ".", "_")
          temp = Replace(temp, " ", "_")
          temp = Replace(temp, "~", "tilde_") 'R operator
          temp = Replace(temp, "-", "dash_")
          temp = Replace(temp, "+", "plus_")
          temp = Replace(temp, "*", "asterisk_")
          temp = Replace(temp, "/", "slash_")
          'numbers in front can cause problems
          chPos = InStr(temp, "0")
          If chPos = 1 Then temp = Replace(temp, "0", "one_")
          chPos = InStr(temp, "1")
          If chPos = 1 Then temp = Replace(temp, "1", "one_")
          chPos = InStr(temp, "2")
          If chPos = 1 Then temp = Replace(temp, "2", "two_")
          chPos = InStr(temp, "3")
          If chPos = 1 Then temp = Replace(temp, "3", "three_")
          chPos = InStr(temp, "4")
          If chPos = 1 Then temp = Replace(temp, "4", "four_")
          chPos = InStr(temp, "5")
          If chPos = 1 Then temp = Replace(temp, "5", "five_")
          chPos = InStr(temp, "6")
          If chPos = 1 Then temp = Replace(temp, "6", "six_")
          chPos = InStr(temp, "7")
          If chPos = 1 Then temp = Replace(temp, "7", "seven_")
          chPos = InStr(temp, "8")
          If chPos = 1 Then temp = Replace(temp, "8", "eight_")
          chPos = InStr(temp, "9")
          If chPos = 1 Then temp = Replace(temp, "9", "nine_")
          ShortInNames.Add (temp)
          VarInNames.Add ("In" & i)
          temp = get_val(suf)
          InNames.Add (temp)
          Call get_line(suf)
          If (i > 1) Then Print #65, ",";
          Print #65, ShortInNames(i);
        Next i
        j = 1
        For i = 1 To outs
          temp = get_val(suf)
          temp = Replace(temp, ".", "_")
          temp = Replace(temp, " ", "_")
          temp = Replace(temp, "~", "tilde_") 'R operator
          temp = Replace(temp, "-", "dash_")
          temp = Replace(temp, "+", "plus_")
          temp = Replace(temp, "*", "asterisk_")
          temp = Replace(temp, "/", "slash_")
          'numbers in front can cause problems
          chPos = InStr(temp, "0")
          If chPos = 1 Then temp = Replace(temp, "0", "one_")
          chPos = InStr(temp, "1")
          If chPos = 1 Then temp = Replace(temp, "1", "one_")
          chPos = InStr(temp, "2")
          If chPos = 1 Then temp = Replace(temp, "2", "two_")
          chPos = InStr(temp, "3")
          If chPos = 1 Then temp = Replace(temp, "3", "three_")
          chPos = InStr(temp, "4")
          If chPos = 1 Then temp = Replace(temp, "4", "four_")
          chPos = InStr(temp, "5")
          If chPos = 1 Then temp = Replace(temp, "5", "five_")
          chPos = InStr(temp, "6")
          If chPos = 1 Then temp = Replace(temp, "6", "six_")
          chPos = InStr(temp, "7")
          If chPos = 1 Then temp = Replace(temp, "7", "seven_")
          chPos = InStr(temp, "8")
          If chPos = 1 Then temp = Replace(temp, "8", "eight_")
          chPos = InStr(temp, "9")
          If chPos = 1 Then temp = Replace(temp, "9", "nine_")
          ShortOutNames.Add (temp)
          OutNames.Add (get_val(suf))
          OutOpt.Add (get_val(suf))
          temp2 = get_val(suf) ' Units
          For k = 1 To Val(OutOpt(i))
            temp2 = get_val(suf)
            VarOutNames.Add ("Out" & j)
            AllShortOutNames.Add (temp & "_" & temp2)
            Print #65, ",", AllShortOutNames(j);
            j = j + 1
          Next k
          Call get_line(suf)
        Next i
        Print #65,
        count = Val(get_val(suf))
        Call get_line(suf)
        Call get_line(suf) ' Ignore table headings
        For i = 1 To count
          ProgressBar1.Value = (25 * i) / count
          Call get_val(suf)
            For j = 1 To ins
              If (j > 1) Then Print #65, ",";
              Print #65, Val(get_val(suf));
            Next j
            For j = 1 To outs
              For k = 1 To Val(OutOpt(j))
                Print #65, ",", Val(get_val(suf));
              Next k
            Next j
          Call get_line(suf)
          Print #65,
        Next i
        Close 65
      End If
    Wend
  End If
End Sub

Private Sub PartialR()
  Dim i As Integer
  Dim j As Integer
  Dim ovar As String
  Dim ivar As String
  Dim first As Boolean
      Print #65, "fname<-""" & argv(2) & ".R.txt"""
      Print #65, "pngname<-""" & argv(2) & ".Scatter.png"""
      Print #65, "png(pngname,width=1280,height=960)"
      Print #65, "plot.window(xlim=c(1,800),ylim=c(1,600))"
'      Print #65, "plot(da,xlim=c(0,1023),ylim=c(0,767))"
      Print #65, "plot(da)"
      Print #65, "dev.off()"
      ProgressBar1.Value = 50
      Print #65, "capture.output(noquote(""Partial R results""),file=fname,append=FALSE)"
      For i = 1 To AllShortOutNames.count
        ovar = AllShortOutNames(i)
        Print #65, "Nda<-class(""data.frame"")"
        Print #65, "Nda$" & ovar & "<-da$" & ovar & "/sd(da$" & ovar & ")"
        Print #65, "eq<-" & ovar & " ~ ";
        first = True
        For j = 1 To ShortInNames.count
          If (Not first) Then
            Print #65, " + ";
          End If
          first = False
          ivar = ShortInNames(j)
          Print #65, ivar;
        Next j
        Print #65,
        Print #65, "gfit<-lm(formula = eq, data = da)"
        Print #65, "capture.output(eq,file=fname,append=TRUE)"
        Print #65, "capture.output(summary(gfit),file=fname,append=TRUE)"
        For j = 1 To ShortInNames.count
          ivar = ShortInNames(j)
          Print #65, "Nda$" & ivar & "<-da$" & ivar & "/sd(da$" & ivar & ")"
        Next j
        Print #65, "capture.output(noquote(""Contributions to " & ovar & """),file=fname,append=TRUE)"
        Print #65, "Ngfit<-lm(formula = eq, data = Nda)"
        For j = 1 To ShortInNames.count
          ivar = ShortInNames(j)
          Print #65, "capture.output(Ngfit$coef[""" & ivar & """]*cor(da$""" & ovar & """,da$""" & ivar & """),file=fname,append=TRUE)"
        Next j
      Next i
      ProgressBar1.Value = 75
End Sub

Private Sub CDF()
   Dim onRow As Integer
   Dim onCol As Integer
   Dim i As Integer
   Dim ovar As String
   Dim ivar As String
   ProgressBar1.Value = 50
   Print #65, "pngname<-""" & argv(2) & ".CDF.png"""
   Print #65, "png(pngname,width=1280,height=960)"
   onRow = Int(Exp(Log(AllShortOutNames.count + ShortInNames.count) * 0.5))
   onCol = Int((AllShortOutNames.count + ShortInNames.count) / onRow)
   Print #65, "nf<-layout(matrix(c(1:" & (onRow * onCol) & "),ncol=" & onRow & "))"
   Print #65, "nf"
   Print #65, "layout.show(nf)"
   For i = 1 To AllShortOutNames.count
     ovar = AllShortOutNames(i)
     Print #65, "plot(ecdf(da$" & ovar & "),do.points=FALSE,verticals=TRUE,main=""" & ovar & """)"
     Print #65, "abline(v=0.95)"
   Next i
   For i = 1 To ShortInNames.count
     ivar = ShortInNames(i)
     Print #65, "plot(ecdf(da$" & ivar & "),do.points=FALSE,verticals=TRUE,main=""" & ivar & """)"
     Print #65, "abline(v=0.95)"
   Next i
   Print #65, "dev.off()"
   ProgressBar1.Value = 75
End Sub

Private Sub Histogram()
   Dim onRow As Integer
   Dim onCol As Integer
   Dim i As Integer
   Dim ovar As String
   Dim ivar As String
   ProgressBar1.Value = 50
   Print #65, "pngname<-""" & argv(2) & ".Dist.png"""
   Print #65, "png(pngname,width=1280,height=960)"
   onRow = Int(Exp(Log(AllShortOutNames.count + ShortInNames.count) * 0.5))
   onCol = Int((AllShortOutNames.count + ShortInNames.count) / onRow)
   Print #65, "nf<-layout(matrix(c(1:" & (onRow * onCol) & "),ncol=" & onRow & "))"
   Print #65, "nf"
   Print #65, "layout.show(nf)"
   For i = 1 To AllShortOutNames.count
     ovar = AllShortOutNames(i)
     Print #65, "hist(da$" & ovar & ",main=""" & ovar & """)"
     Print #65, "abline(v=0.95)"
   Next i
   For i = 1 To ShortInNames.count
     ivar = ShortInNames(i)
     Print #65, "hist(da$" & ivar & ",main=""" & ivar & """)"
     Print #65, "abline(v=0.95)"
   Next i
   Print #65, "dev.off()"
   ProgressBar1.Value = 75
End Sub

Private Sub Timer1_Timer()
  Dim temp As String
  Dim onRow As Integer
  Dim i As Integer
  Timer1.Enabled = False
  writeCSV
  argv(2) = Replace(argv(2), "\", "\\")
'Write the R Script that will generate the summary and the script
    Open argv(2) & ".R" For Output As 65
    Print #65, "da <-read.csv(""" & argv(2) & ".csv"",strip.white=TRUE)"
    onRow = 3
    If (argv(0) = "/PartialR") Then
      PartialR
    ElseIf (argv(0) = "/CDF") Then
      CDF
    ElseIf (argv(0) = "/Histogram") Then
      Histogram
    End If
    ProgressBar1.Value = 100
    Close 65
  End
End Sub
