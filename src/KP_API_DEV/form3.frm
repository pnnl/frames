VERSION 5.00
Begin VB.Form Form3 
   Caption         =   "PDA Simulator - Logged in as 'dorow'"
   ClientHeight    =   5505
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   4350
   LinkTopic       =   "Form3"
   ScaleHeight     =   5505
   ScaleWidth      =   4350
   StartUpPosition =   3  'Windows Default
   Begin VB.PictureBox picButton 
      Height          =   735
      Left            =   120
      ScaleHeight     =   675
      ScaleWidth      =   4035
      TabIndex        =   54
      Top             =   4560
      Width           =   4095
      Begin VB.CommandButton Command5 
         Caption         =   "Close"
         Height          =   375
         Left            =   2880
         TabIndex        =   56
         Top             =   120
         Width           =   975
      End
      Begin VB.CommandButton Command4 
         Caption         =   "Send"
         Height          =   375
         Left            =   120
         TabIndex        =   55
         Top             =   120
         Width           =   975
      End
   End
   Begin VB.PictureBox picMain 
      Height          =   4335
      Left            =   120
      ScaleHeight     =   4275
      ScaleWidth      =   4035
      TabIndex        =   0
      Top             =   120
      Width           =   4095
      Begin VB.PictureBox picDamage 
         BorderStyle     =   0  'None
         Height          =   2415
         Left            =   -120
         ScaleHeight     =   2415
         ScaleWidth      =   4215
         TabIndex        =   26
         Top             =   1800
         Width           =   4215
         Begin VB.TextBox Text14 
            Height          =   285
            Left            =   1080
            TabIndex        =   53
            Top             =   600
            Width           =   3015
         End
         Begin VB.TextBox Text13 
            Height          =   285
            Left            =   1080
            TabIndex        =   52
            Top             =   960
            Width           =   3015
         End
         Begin VB.TextBox Text12 
            Height          =   285
            Left            =   1080
            TabIndex        =   51
            Text            =   "0.00"
            Top             =   1680
            Width           =   1455
         End
         Begin VB.ComboBox Combo3 
            Height          =   315
            Left            =   1080
            TabIndex        =   31
            Top             =   120
            Width           =   3015
         End
         Begin VB.TextBox Text7 
            Height          =   285
            Left            =   1080
            TabIndex        =   30
            Text            =   "0.00"
            Top             =   1320
            Width           =   1455
         End
         Begin VB.TextBox Text6 
            Height          =   285
            Left            =   1080
            TabIndex        =   29
            Text            =   "0.00"
            Top             =   2040
            Width           =   975
         End
         Begin VB.TextBox Text4 
            Height          =   285
            Left            =   2520
            TabIndex        =   28
            Text            =   "0.00"
            Top             =   2040
            Width           =   1095
         End
         Begin VB.CommandButton Command2 
            Caption         =   "Map"
            Height          =   255
            Left            =   240
            TabIndex        =   27
            Top             =   2040
            Width           =   495
         End
         Begin VB.Label Label18 
            Caption         =   "Subtype"
            Height          =   255
            Left            =   240
            TabIndex        =   38
            Top             =   120
            Width           =   1095
         End
         Begin VB.Label Label13 
            Caption         =   "Dam Amt"
            Height          =   255
            Left            =   240
            TabIndex        =   37
            Top             =   1320
            Width           =   1215
         End
         Begin VB.Label Label12 
            Caption         =   "Ins Amt"
            Height          =   255
            Left            =   240
            TabIndex        =   36
            Top             =   1680
            Width           =   1215
         End
         Begin VB.Label Label11 
            Caption         =   "County"
            Height          =   255
            Left            =   240
            TabIndex        =   35
            Top             =   960
            Width           =   1215
         End
         Begin VB.Label Label10 
            Caption         =   "Applicant"
            Height          =   255
            Left            =   240
            TabIndex        =   34
            Top             =   600
            Width           =   1215
         End
         Begin VB.Label Label9 
            Caption         =   "Lat"
            Height          =   255
            Left            =   800
            TabIndex        =   33
            Top             =   2040
            Width           =   255
         End
         Begin VB.Label Label8 
            Caption         =   "Lon"
            Height          =   255
            Left            =   2160
            TabIndex        =   32
            Top             =   2040
            Width           =   375
         End
      End
      Begin VB.PictureBox picUser 
         BorderStyle     =   0  'None
         Height          =   2415
         Left            =   5280
         ScaleHeight     =   2415
         ScaleWidth      =   4215
         TabIndex        =   39
         Top             =   1920
         Width           =   4215
         Begin VB.TextBox Text10 
            Height          =   285
            Left            =   1080
            TabIndex        =   48
            Text            =   "0.00"
            Top             =   2040
            Width           =   975
         End
         Begin VB.TextBox Text9 
            Height          =   285
            Left            =   2520
            TabIndex        =   47
            Text            =   "0.00"
            Top             =   2040
            Width           =   1095
         End
         Begin VB.CommandButton Command3 
            Caption         =   "Map"
            Height          =   255
            Left            =   240
            TabIndex        =   46
            Top             =   2040
            Width           =   495
         End
         Begin VB.ComboBox Combo4 
            Height          =   315
            Left            =   1080
            TabIndex        =   44
            Top             =   120
            Width           =   3015
         End
         Begin VB.CheckBox Check5 
            Height          =   255
            Left            =   1080
            TabIndex        =   43
            Top             =   960
            Value           =   1  'Checked
            Width           =   1455
         End
         Begin VB.TextBox Text8 
            Height          =   285
            Left            =   1080
            TabIndex        =   42
            Text            =   "0.00"
            Top             =   600
            Width           =   1455
         End
         Begin VB.Label Label23 
            Caption         =   "Lat"
            Height          =   255
            Left            =   795
            TabIndex        =   50
            Top             =   2040
            Width           =   255
         End
         Begin VB.Label Label22 
            Caption         =   "Lon"
            Height          =   255
            Left            =   2160
            TabIndex        =   49
            Top             =   2040
            Width           =   375
         End
         Begin VB.Label Label21 
            Caption         =   "Subtype"
            Height          =   255
            Left            =   240
            TabIndex        =   45
            Top             =   120
            Width           =   1095
         End
         Begin VB.Label Label20 
            Caption         =   "Respond"
            Height          =   255
            Left            =   240
            TabIndex        =   41
            Top             =   960
            Width           =   1215
         End
         Begin VB.Label Label19 
            Caption         =   "Destination"
            Height          =   255
            Left            =   240
            TabIndex        =   40
            Top             =   600
            Width           =   1215
         End
      End
      Begin VB.PictureBox picTrafficWeather 
         BorderStyle     =   0  'None
         Height          =   2415
         Left            =   5160
         ScaleHeight     =   2415
         ScaleWidth      =   4215
         TabIndex        =   8
         Top             =   0
         Width           =   4215
         Begin VB.CommandButton Command1 
            Caption         =   "Map"
            Height          =   255
            Left            =   240
            TabIndex        =   25
            Top             =   2040
            Width           =   495
         End
         Begin VB.TextBox Text3 
            Height          =   285
            Left            =   2520
            TabIndex        =   22
            Text            =   "0.00"
            Top             =   2040
            Width           =   1095
         End
         Begin VB.TextBox Text1 
            Height          =   285
            Left            =   1080
            TabIndex        =   21
            Text            =   "0.00"
            Top             =   2040
            Width           =   975
         End
         Begin VB.CheckBox Check3 
            Height          =   255
            Left            =   1080
            TabIndex        =   20
            Top             =   600
            Value           =   1  'Checked
            Width           =   1455
         End
         Begin VB.CheckBox Check4 
            Height          =   255
            Left            =   1080
            TabIndex        =   19
            Top             =   1680
            Value           =   1  'Checked
            Width           =   1455
         End
         Begin VB.TextBox Text11 
            Height          =   285
            Left            =   1080
            TabIndex        =   18
            Text            =   "0.00"
            Top             =   1320
            Width           =   1455
         End
         Begin VB.OptionButton Option6 
            Caption         =   "Low"
            Height          =   255
            Index           =   2
            Left            =   3000
            TabIndex        =   17
            Top             =   960
            Width           =   735
         End
         Begin VB.OptionButton Option6 
            Caption         =   "Medium"
            Height          =   255
            Index           =   1
            Left            =   1920
            TabIndex        =   16
            Top             =   960
            Width           =   855
         End
         Begin VB.OptionButton Option6 
            Caption         =   "High"
            Height          =   255
            Index           =   0
            Left            =   1080
            TabIndex        =   15
            Top             =   960
            Value           =   -1  'True
            Width           =   735
         End
         Begin VB.ComboBox Combo2 
            Height          =   315
            Left            =   1080
            TabIndex        =   10
            Top             =   120
            Width           =   3015
         End
         Begin VB.Label Label7 
            Caption         =   "Lon"
            Height          =   255
            Left            =   2160
            TabIndex        =   24
            Top             =   2040
            Width           =   375
         End
         Begin VB.Label Label6 
            Caption         =   "Lat"
            Height          =   255
            Left            =   800
            TabIndex        =   23
            Top             =   2040
            Width           =   255
         End
         Begin VB.Label Label14 
            Caption         =   "Emergency"
            Height          =   255
            Left            =   240
            TabIndex        =   14
            Top             =   600
            Width           =   1215
         End
         Begin VB.Label Label15 
            Caption         =   "Priority"
            Height          =   255
            Left            =   240
            TabIndex        =   13
            Top             =   960
            Width           =   1215
         End
         Begin VB.Label Label16 
            Caption         =   "Respond"
            Height          =   255
            Left            =   240
            TabIndex        =   12
            Top             =   1680
            Width           =   1215
         End
         Begin VB.Label Label17 
            Caption         =   "Destination"
            Height          =   255
            Left            =   240
            TabIndex        =   11
            Top             =   1320
            Width           =   1215
         End
         Begin VB.Label Label4 
            Caption         =   "Subtype"
            Height          =   255
            Left            =   240
            TabIndex        =   9
            Top             =   120
            Width           =   1095
         End
      End
      Begin VB.ComboBox Combo1 
         Height          =   315
         Left            =   960
         Style           =   2  'Dropdown List
         TabIndex        =   5
         Top             =   1440
         Width           =   3015
      End
      Begin VB.TextBox Text5 
         Height          =   615
         Left            =   960
         TabIndex        =   4
         Text            =   "Test Message Text"
         Top             =   720
         Width           =   3015
      End
      Begin VB.TextBox Text2 
         Height          =   285
         Left            =   960
         TabIndex        =   1
         Text            =   "Test Message 1"
         Top             =   360
         Width           =   3015
      End
      Begin VB.Label Label3 
         Caption         =   "Type"
         Height          =   255
         Left            =   120
         TabIndex        =   7
         Top             =   1440
         Width           =   495
      End
      Begin VB.Label Label1 
         Caption         =   "EarthAlert Message"
         Height          =   255
         Left            =   1200
         TabIndex        =   6
         Top             =   0
         Width           =   1455
      End
      Begin VB.Label Label5 
         Caption         =   "Text"
         Height          =   255
         Left            =   120
         TabIndex        =   3
         Top             =   720
         Width           =   615
      End
      Begin VB.Label Label2 
         Caption         =   "Name"
         Height          =   255
         Left            =   120
         TabIndex        =   2
         Top             =   360
         Width           =   615
      End
   End
End
Attribute VB_Name = "Form3"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private Sub Combo1_Click()
    If Combo1.Text = "Damage Assessment" Then
        picDamage.Left = -120
        picDamage.Top = 1800
        picDamage.Visible = True
        picTrafficWeather.Visible = False
        picUser.Visible = False
    ElseIf Combo1.Text = "Traffic Conditions" Or Combo1.Text = "Weather Condition" Then
        picTrafficWeather.Left = -120
        picTrafficWeather.Top = 1800
        picTrafficWeather.Visible = True
        picDamage.Visible = False
        picUser.Visible = False
        
        If Combo1.Text = "Traffic Conditions" Then
            Combo2.AddItem "Accidents"
            Combo2.AddItem "Adverse Road Conditions"
            Combo2.AddItem "Road Closure"
            Combo2.AddItem "RoadSide Assistance"
            Combo2.Text = "Accidents"
        Else
            Combo2.AddItem "Flood Warning"
            Combo2.AddItem "Flood Watch"
            Combo2.AddItem "Icy Conditions"
            Combo2.AddItem "Rain Storm"
            Combo2.AddItem "ThunderStorm Warning"
            Combo2.AddItem "ThunderStorm Watch"
            Combo2.AddItem "Tornado Warning"
            Combo2.AddItem "Tornado Watch"
            Combo2.AddItem "Snow Storm"
            Combo2.Text = "Flood Warning"
        End If
    Else
        picUser.Left = -120
        picUser.Top = 1800
        picUser.Visible = True
        picTrafficWeather.Visible = False
        picDamage.Visible = False
    End If
End Sub

Private Sub Form_Load()

    Combo1.AddItem "Damage Assessment"
    Combo1.AddItem "Traffic Conditions"
    Combo1.AddItem "User Defined Condition"
    Combo1.AddItem "Weather Condition"
    
    'Damage Assessment
    Combo3.AddItem "A - Debris Removal"
    Combo3.AddItem "B - Emergency Protective Measures"
    Combo3.AddItem "C - Roads & Bridges"
    Combo3.AddItem "D - Water Control Facilities"
    Combo3.AddItem "E - Public Buildings & Equipment"
    Combo3.AddItem "F - Public Facilities"
    Combo3.AddItem "G - Parks, Recreation, etc"
    Combo3.Text = "A - Debris Removal"
    
    Combo4.Text = ""
    Combo4.Enabled = False
    
    Combo1.Text = "User Defined Condition"
    
End Sub
