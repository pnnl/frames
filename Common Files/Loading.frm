VERSION 5.00
Object = "{6B7E6392-850A-101B-AFC0-4210102A8DA7}#1.3#0"; "COMCTL32.OCX"
Begin VB.Form Loading 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "Please Wait..."
   ClientHeight    =   945
   ClientLeft      =   5010
   ClientTop       =   4185
   ClientWidth     =   3855
   ControlBox      =   0   'False
   Icon            =   "Loading.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   MousePointer    =   11  'Hourglass
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   945
   ScaleWidth      =   3855
   ShowInTaskbar   =   0   'False
   StartUpPosition =   2  'CenterScreen
   Begin ComctlLib.ProgressBar Gauge1 
      Height          =   375
      Left            =   120
      TabIndex        =   1
      Top             =   480
      Width           =   3615
      _ExtentX        =   6376
      _ExtentY        =   661
      _Version        =   327682
      Appearance      =   1
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   240
      TabIndex        =   0
      Top             =   120
      Width           =   2655
   End
End
Attribute VB_Name = "Loading"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Sub update()
  Gauge1.Value = Gauge1.Value + 1
  Label1 = "Loading ... " + Format(Gauge1.Value / Gauge1.Max * 100, "###") + "%"
  Refresh
End Sub
