VERSION 5.00
Begin VB.Form Form2 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Form2"
   ClientHeight    =   7005
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   8655
   LinkTopic       =   "Form2"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   7005
   ScaleWidth      =   8655
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton cmdPaR 
      Caption         =   "Insert PAR"
      Height          =   375
      Left            =   2880
      TabIndex        =   117
      Top             =   6480
      Width           =   1095
   End
   Begin VB.CommandButton cmdTA 
      Caption         =   "Insert TA"
      Height          =   375
      Left            =   6000
      TabIndex        =   116
      Top             =   6480
      Width           =   1095
   End
   Begin VB.CommandButton Command3 
      Caption         =   "Delete Point"
      Height          =   375
      Left            =   4320
      TabIndex        =   115
      Top             =   6480
      Width           =   1455
   End
   Begin VB.Frame fraGenClass 
      Caption         =   "Generic Class"
      Height          =   3015
      Left            =   120
      TabIndex        =   94
      Top             =   2880
      Width           =   8535
      Begin VB.Frame fraGeneric 
         Caption         =   "Generic Attributes"
         Height          =   3015
         Left            =   2400
         TabIndex        =   99
         Top             =   0
         Width           =   6135
         Begin VB.TextBox txtAttrName5 
            Height          =   375
            Left            =   1920
            TabIndex        =   109
            Top             =   2160
            Width           =   1335
         End
         Begin VB.TextBox txtAttrValue5 
            Height          =   375
            Left            =   3840
            TabIndex        =   108
            Top             =   2160
            Width           =   1335
         End
         Begin VB.TextBox txtAttrName4 
            Height          =   375
            Left            =   1920
            TabIndex        =   107
            Top             =   1680
            Width           =   1335
         End
         Begin VB.TextBox txtAttrValue4 
            Height          =   375
            Left            =   3840
            TabIndex        =   106
            Top             =   1680
            Width           =   1335
         End
         Begin VB.TextBox txtAttrValue3 
            Height          =   375
            Left            =   3840
            TabIndex        =   105
            Top             =   1200
            Width           =   1335
         End
         Begin VB.TextBox txtAttrName3 
            Height          =   375
            Left            =   1920
            TabIndex        =   104
            Top             =   1200
            Width           =   1335
         End
         Begin VB.TextBox txtAttrName2 
            Height          =   375
            Left            =   1920
            TabIndex        =   103
            Top             =   720
            Width           =   1335
         End
         Begin VB.TextBox txtAttrValue2 
            Height          =   375
            Left            =   3840
            TabIndex        =   102
            Top             =   720
            Width           =   1335
         End
         Begin VB.TextBox txtAttrValue1 
            Height          =   375
            Left            =   3840
            TabIndex        =   101
            Top             =   240
            Width           =   1335
         End
         Begin VB.TextBox txtAttrName1 
            Height          =   375
            Left            =   1920
            TabIndex        =   100
            Top             =   240
            Width           =   1335
         End
         Begin VB.Label Label31 
            Caption         =   "Attr Name 4"
            Height          =   255
            Left            =   120
            TabIndex        =   114
            Top             =   1680
            Width           =   1575
         End
         Begin VB.Label Label30 
            Caption         =   "Attr Name 2"
            Height          =   255
            Left            =   120
            TabIndex        =   113
            Top             =   720
            Width           =   1575
         End
         Begin VB.Label Label29 
            Caption         =   "Attr Name 1"
            Height          =   255
            Left            =   120
            TabIndex        =   112
            Top             =   360
            Width           =   1575
         End
         Begin VB.Label Label28 
            Caption         =   "Attr Name 3"
            Height          =   375
            Left            =   120
            TabIndex        =   111
            Top             =   1200
            Width           =   1215
         End
         Begin VB.Label Label27 
            Caption         =   "Attr Name 5"
            Height          =   255
            Left            =   120
            TabIndex        =   110
            Top             =   2280
            Width           =   1575
         End
      End
      Begin VB.TextBox txtClassname 
         Height          =   285
         Left            =   960
         TabIndex        =   96
         Top             =   480
         Width           =   1095
      End
      Begin VB.TextBox txtSubclassname 
         Height          =   285
         Left            =   960
         TabIndex        =   95
         Top             =   1200
         Width           =   1095
      End
      Begin VB.Label lblClass 
         Caption         =   "Class Name"
         Height          =   375
         Left            =   120
         TabIndex        =   98
         Top             =   360
         Width           =   735
      End
      Begin VB.Label Label26 
         Caption         =   "Subclass Name"
         Height          =   735
         Left            =   120
         TabIndex        =   97
         Top             =   1080
         Width           =   735
      End
   End
   Begin VB.Frame fraGenerators 
      Caption         =   "Gen"
      Height          =   3495
      Left            =   2520
      TabIndex        =   77
      Top             =   3000
      Width           =   6135
      Begin VB.OptionButton OptionGen 
         Caption         =   "A - Alert"
         Height          =   255
         Index           =   7
         Left            =   240
         TabIndex        =   93
         Top             =   240
         Width           =   2895
      End
      Begin VB.TextBox Text17 
         Height          =   285
         Left            =   1440
         TabIndex        =   87
         Text            =   "10000"
         Top             =   3240
         Width           =   1455
      End
      Begin VB.TextBox Text13 
         Height          =   285
         Left            =   1440
         TabIndex        =   86
         Text            =   "10000"
         Top             =   2880
         Width           =   1455
      End
      Begin VB.TextBox Text14 
         Height          =   285
         Left            =   1440
         TabIndex        =   85
         Text            =   "5000"
         Top             =   2520
         Width           =   1455
      End
      Begin VB.TextBox Text15 
         Height          =   285
         Left            =   1440
         TabIndex        =   84
         Text            =   "1000"
         Top             =   2160
         Width           =   4455
      End
      Begin VB.TextBox Text16 
         Height          =   285
         Left            =   1440
         TabIndex        =   83
         Text            =   "John and Jane Doe"
         Top             =   1800
         Width           =   4455
      End
      Begin VB.OptionButton OptionGen 
         Caption         =   "F - Warning"
         Height          =   255
         Index           =   8
         Left            =   3360
         TabIndex        =   82
         Top             =   480
         Width           =   2175
      End
      Begin VB.OptionButton OptionGen 
         Caption         =   "E - Offline"
         Height          =   255
         Index           =   9
         Left            =   3360
         TabIndex        =   81
         Top             =   240
         Width           =   2655
      End
      Begin VB.OptionButton OptionGen 
         Caption         =   "D - OK"
         Height          =   255
         Index           =   10
         Left            =   240
         TabIndex        =   80
         Top             =   960
         Width           =   2175
      End
      Begin VB.OptionButton OptionGen 
         Caption         =   "C - No Data"
         Height          =   255
         Index           =   11
         Left            =   240
         TabIndex        =   79
         Top             =   720
         Width           =   2175
      End
      Begin VB.OptionButton OptionGen 
         Caption         =   "B - Generator"
         Height          =   255
         Index           =   12
         Left            =   240
         TabIndex        =   78
         Top             =   480
         Width           =   2895
      End
      Begin VB.Label Label25 
         Caption         =   "Reference"
         Height          =   255
         Left            =   240
         TabIndex        =   92
         Top             =   3240
         Width           =   1215
      End
      Begin VB.Line Line7 
         BorderColor     =   &H00FFFFFF&
         X1              =   0
         X2              =   6120
         Y1              =   1545
         Y2              =   1545
      End
      Begin VB.Line Line8 
         BorderColor     =   &H00808080&
         X1              =   0
         X2              =   6120
         Y1              =   1560
         Y2              =   1560
      End
      Begin VB.Label Label21 
         Caption         =   "ID"
         Height          =   255
         Index           =   0
         Left            =   240
         TabIndex        =   91
         Top             =   2880
         Width           =   1215
      End
      Begin VB.Label Label22 
         Caption         =   "Reserve_Margin"
         Height          =   255
         Left            =   240
         TabIndex        =   90
         Top             =   2520
         Width           =   1215
      End
      Begin VB.Label Label23 
         Caption         =   "Power_Output#"
         Height          =   255
         Left            =   240
         TabIndex        =   89
         Top             =   2160
         Width           =   1215
      End
      Begin VB.Label Label24 
         Caption         =   "Status"
         Height          =   255
         Left            =   240
         TabIndex        =   88
         Top             =   1800
         Width           =   1215
      End
   End
   Begin VB.TextBox Text5 
      Height          =   855
      Left            =   1680
      TabIndex        =   20
      Text            =   "Test Message Text"
      Top             =   960
      Width           =   6735
   End
   Begin VB.Frame fraTrafficCondition 
      Caption         =   "Traffic Condition Subtype"
      Height          =   3255
      Left            =   2520
      TabIndex        =   15
      Top             =   2880
      Width           =   6135
      Begin VB.Frame fraWeatherCondition 
         Caption         =   "Weather Condition Subtype"
         Height          =   3255
         Left            =   0
         TabIndex        =   56
         Top             =   0
         Width           =   6135
         Begin VB.OptionButton Option4 
            Caption         =   "Flood Warning"
            Height          =   255
            Index           =   0
            Left            =   240
            TabIndex        =   72
            Top             =   240
            Width           =   2895
         End
         Begin VB.OptionButton Option4 
            Caption         =   "Flood Watch"
            Height          =   255
            Index           =   1
            Left            =   240
            TabIndex        =   71
            Top             =   480
            Width           =   2895
         End
         Begin VB.OptionButton Option4 
            Caption         =   "Icy Conditions"
            Height          =   255
            Index           =   2
            Left            =   240
            TabIndex        =   70
            Top             =   720
            Width           =   2895
         End
         Begin VB.OptionButton Option4 
            Caption         =   "Rain Storm"
            Height          =   255
            Index           =   3
            Left            =   240
            TabIndex        =   69
            Top             =   960
            Width           =   2895
         End
         Begin VB.OptionButton Option4 
            Caption         =   "Thunderstorm Warning"
            Height          =   255
            Index           =   4
            Left            =   240
            TabIndex        =   68
            Top             =   1200
            Width           =   2895
         End
         Begin VB.OptionButton Option4 
            Caption         =   "Thunderstorm Watch"
            Height          =   255
            Index           =   5
            Left            =   3360
            TabIndex        =   67
            Top             =   240
            Width           =   2055
         End
         Begin VB.OptionButton Option4 
            Caption         =   "Tornado Warning"
            Height          =   255
            Index           =   6
            Left            =   3360
            TabIndex        =   66
            Top             =   480
            Width           =   1815
         End
         Begin VB.OptionButton Option4 
            Caption         =   "Tornado Watch"
            Height          =   255
            Index           =   7
            Left            =   3360
            TabIndex        =   65
            Top             =   720
            Width           =   2175
         End
         Begin VB.OptionButton Option4 
            Caption         =   "Snow Storm"
            Height          =   255
            Index           =   8
            Left            =   3360
            TabIndex        =   64
            Top             =   960
            Width           =   2295
         End
         Begin VB.CheckBox Check3 
            Height          =   255
            Left            =   1800
            TabIndex        =   63
            Top             =   1800
            Value           =   1  'Checked
            Width           =   1455
         End
         Begin VB.CheckBox Check4 
            Height          =   255
            Left            =   1800
            TabIndex        =   62
            Top             =   2880
            Value           =   1  'Checked
            Width           =   1455
         End
         Begin VB.TextBox Text11 
            Height          =   285
            Left            =   1800
            TabIndex        =   61
            Text            =   "0.00"
            Top             =   2520
            Width           =   1455
         End
         Begin VB.Frame Frame3 
            BorderStyle     =   0  'None
            Height          =   495
            Left            =   1800
            TabIndex        =   57
            Top             =   2040
            Width           =   2655
            Begin VB.OptionButton Option6 
               Caption         =   "Low"
               Height          =   255
               Index           =   2
               Left            =   1920
               TabIndex        =   60
               Top             =   120
               Width           =   735
            End
            Begin VB.OptionButton Option6 
               Caption         =   "Medium"
               Height          =   255
               Index           =   1
               Left            =   840
               TabIndex        =   59
               Top             =   120
               Width           =   855
            End
            Begin VB.OptionButton Option6 
               Caption         =   "High"
               Height          =   255
               Index           =   0
               Left            =   0
               TabIndex        =   58
               Top             =   120
               Value           =   -1  'True
               Width           =   735
            End
         End
         Begin VB.Line Line5 
            BorderColor     =   &H00808080&
            X1              =   0
            X2              =   6120
            Y1              =   1575
            Y2              =   1575
         End
         Begin VB.Line Line6 
            BorderColor     =   &H00FFFFFF&
            X1              =   0
            X2              =   6120
            Y1              =   1560
            Y2              =   1560
         End
         Begin VB.Label Label14 
            Caption         =   "Emergency"
            Height          =   255
            Left            =   240
            TabIndex        =   76
            Top             =   1800
            Width           =   1215
         End
         Begin VB.Label Label15 
            Caption         =   "Priority"
            Height          =   255
            Left            =   240
            TabIndex        =   75
            Top             =   2160
            Width           =   1215
         End
         Begin VB.Label Label16 
            Caption         =   "Respond"
            Height          =   255
            Left            =   240
            TabIndex        =   74
            Top             =   2880
            Width           =   1215
         End
         Begin VB.Label Label17 
            Caption         =   "Destination"
            Height          =   255
            Left            =   240
            TabIndex        =   73
            Top             =   2520
            Width           =   1215
         End
      End
      Begin VB.Frame Frame2 
         BorderStyle     =   0  'None
         Height          =   495
         Left            =   1680
         TabIndex        =   35
         Top             =   2040
         Width           =   3015
         Begin VB.OptionButton Option5 
            Caption         =   "High"
            Height          =   255
            Index           =   0
            Left            =   120
            TabIndex        =   38
            Top             =   120
            Value           =   -1  'True
            Width           =   735
         End
         Begin VB.OptionButton Option5 
            Caption         =   "Medium"
            Height          =   255
            Index           =   1
            Left            =   960
            TabIndex        =   37
            Top             =   120
            Width           =   855
         End
         Begin VB.OptionButton Option5 
            Caption         =   "Low"
            Height          =   255
            Index           =   2
            Left            =   2040
            TabIndex        =   36
            Top             =   120
            Width           =   735
         End
      End
      Begin VB.TextBox Text10 
         Height          =   285
         Left            =   1800
         TabIndex        =   29
         Top             =   2520
         Width           =   1455
      End
      Begin VB.CheckBox Check2 
         Height          =   255
         Left            =   1800
         TabIndex        =   26
         Top             =   2880
         Value           =   1  'Checked
         Width           =   1455
      End
      Begin VB.CheckBox Check1 
         Height          =   255
         Left            =   1800
         TabIndex        =   23
         Top             =   1800
         Value           =   1  'Checked
         Width           =   1455
      End
      Begin VB.OptionButton Option3 
         Caption         =   "Raodside Assistance"
         Height          =   255
         Index           =   3
         Left            =   3360
         TabIndex        =   19
         Top             =   480
         Width           =   2655
      End
      Begin VB.OptionButton Option3 
         Caption         =   "Road Closure"
         Height          =   255
         Index           =   2
         Left            =   3360
         TabIndex        =   18
         Top             =   240
         Width           =   1815
      End
      Begin VB.OptionButton Option3 
         Caption         =   "Adverse Road Conditions"
         Height          =   255
         Index           =   1
         Left            =   240
         TabIndex        =   17
         Top             =   480
         Width           =   2895
      End
      Begin VB.OptionButton Option3 
         Caption         =   "Accidents"
         Height          =   255
         Index           =   0
         Left            =   240
         TabIndex        =   16
         Top             =   240
         Width           =   1815
      End
      Begin VB.Label Label13 
         Caption         =   "Destination"
         Height          =   255
         Left            =   240
         TabIndex        =   28
         Top             =   2520
         Width           =   1215
      End
      Begin VB.Label Label12 
         Caption         =   "Respond"
         Height          =   255
         Left            =   240
         TabIndex        =   27
         Top             =   2880
         Width           =   1215
      End
      Begin VB.Label Label7 
         Caption         =   "Priority"
         Height          =   255
         Left            =   240
         TabIndex        =   25
         Top             =   2160
         Width           =   1215
      End
      Begin VB.Label Label6 
         Caption         =   "Emergency"
         Height          =   255
         Left            =   240
         TabIndex        =   24
         Top             =   1800
         Width           =   1215
      End
      Begin VB.Line Line4 
         BorderColor     =   &H00FFFFFF&
         X1              =   0
         X2              =   6120
         Y1              =   1560
         Y2              =   1560
      End
      Begin VB.Line Line3 
         BorderColor     =   &H00808080&
         X1              =   0
         X2              =   6120
         Y1              =   1575
         Y2              =   1575
      End
   End
   Begin VB.TextBox Text2 
      Height          =   285
      Left            =   1680
      TabIndex        =   9
      Text            =   "New"
      Top             =   600
      Width           =   1695
   End
   Begin VB.TextBox Text4 
      Height          =   285
      Left            =   1680
      TabIndex        =   6
      Text            =   "-117.2534"
      Top             =   2280
      Width           =   1695
   End
   Begin VB.TextBox Text3 
      Height          =   285
      Left            =   1680
      TabIndex        =   5
      Text            =   "48.9814"
      Top             =   1920
      Width           =   1695
   End
   Begin VB.TextBox Text1 
      Height          =   285
      Left            =   1680
      TabIndex        =   3
      Top             =   240
      Width           =   1695
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Send Conditional Message"
      Height          =   375
      Left            =   240
      TabIndex        =   1
      Top             =   6480
      Width           =   2175
   End
   Begin VB.CommandButton Command1 
      Caption         =   "&Close"
      Height          =   375
      Left            =   7200
      TabIndex        =   0
      Top             =   6480
      Width           =   1335
   End
   Begin VB.Frame fraUserDefinedConds 
      Caption         =   "User Defined Condition Destination"
      Height          =   3255
      Left            =   2400
      TabIndex        =   22
      Top             =   2880
      Width           =   6135
      Begin VB.TextBox Text12 
         Height          =   285
         Left            =   2040
         TabIndex        =   32
         Top             =   840
         Width           =   1455
      End
      Begin VB.CheckBox Check5 
         Height          =   255
         Left            =   2040
         TabIndex        =   30
         Top             =   480
         Value           =   1  'Checked
         Width           =   1455
      End
      Begin VB.Label Label20 
         Caption         =   "Destination Type"
         Height          =   255
         Left            =   480
         TabIndex        =   34
         Top             =   1200
         Width           =   1215
      End
      Begin VB.Label Label19 
         Caption         =   "Destination"
         Height          =   255
         Left            =   480
         TabIndex        =   33
         Top             =   840
         Width           =   1215
      End
      Begin VB.Label Label18 
         Caption         =   "Respond"
         Height          =   255
         Left            =   480
         TabIndex        =   31
         Top             =   480
         Width           =   1215
      End
   End
   Begin VB.Frame Frame1 
      Caption         =   "Type"
      Height          =   3735
      Left            =   120
      TabIndex        =   2
      Top             =   2640
      Width           =   8415
      Begin VB.Frame fraDamageAssessment 
         Caption         =   "Damage Assessment Subtype"
         Height          =   2415
         Left            =   2400
         TabIndex        =   40
         Top             =   120
         Width           =   6135
         Begin VB.TextBox Text9 
            Height          =   285
            Left            =   1440
            TabIndex        =   51
            Text            =   "10000"
            Top             =   2880
            Width           =   1455
         End
         Begin VB.TextBox Text8 
            Height          =   285
            Left            =   1440
            TabIndex        =   50
            Text            =   "5000"
            Top             =   2520
            Width           =   1455
         End
         Begin VB.TextBox Text7 
            Height          =   285
            Left            =   1440
            TabIndex        =   49
            Text            =   "Benton"
            Top             =   2160
            Width           =   4455
         End
         Begin VB.TextBox Text6 
            Height          =   285
            Left            =   1440
            TabIndex        =   48
            Text            =   "John and Jane Doe"
            Top             =   1800
            Width           =   4455
         End
         Begin VB.OptionButton Option2 
            Caption         =   "G - Parks, Recreation, etc"
            Height          =   255
            Index           =   6
            Left            =   3360
            TabIndex        =   47
            Top             =   720
            Width           =   2175
         End
         Begin VB.OptionButton Option2 
            Caption         =   "F - Public Facilities"
            Height          =   255
            Index           =   5
            Left            =   3360
            TabIndex        =   46
            Top             =   480
            Width           =   2175
         End
         Begin VB.OptionButton Option2 
            Caption         =   "E - Public Buildings && Equipment"
            Height          =   255
            Index           =   4
            Left            =   3360
            TabIndex        =   45
            Top             =   240
            Width           =   2655
         End
         Begin VB.OptionButton Option2 
            Caption         =   "D - Water Control Facilities"
            Height          =   255
            Index           =   3
            Left            =   240
            TabIndex        =   44
            Top             =   960
            Width           =   2175
         End
         Begin VB.OptionButton Option2 
            Caption         =   "C - Roads && Bridges"
            Height          =   255
            Index           =   2
            Left            =   240
            TabIndex        =   43
            Top             =   720
            Width           =   2175
         End
         Begin VB.OptionButton Option2 
            Caption         =   "B - Emergency Protiective Measures"
            Height          =   255
            Index           =   1
            Left            =   240
            TabIndex        =   42
            Top             =   480
            Width           =   2895
         End
         Begin VB.OptionButton Option2 
            Caption         =   "A - Debris Removal"
            Height          =   255
            Index           =   0
            Left            =   240
            TabIndex        =   41
            Top             =   240
            Value           =   -1  'True
            Width           =   2175
         End
         Begin VB.Line Line2 
            BorderColor     =   &H00FFFFFF&
            X1              =   0
            X2              =   6120
            Y1              =   1545
            Y2              =   1545
         End
         Begin VB.Line Line1 
            BorderColor     =   &H00808080&
            X1              =   0
            X2              =   6120
            Y1              =   1560
            Y2              =   1560
         End
         Begin VB.Label Label11 
            Caption         =   "Insurance Amt"
            Height          =   255
            Left            =   240
            TabIndex        =   55
            Top             =   2880
            Width           =   1215
         End
         Begin VB.Label Label10 
            Caption         =   "Damage Amt"
            Height          =   255
            Left            =   240
            TabIndex        =   54
            Top             =   2520
            Width           =   1215
         End
         Begin VB.Label Label9 
            Caption         =   "County"
            Height          =   255
            Left            =   240
            TabIndex        =   53
            Top             =   2160
            Width           =   1215
         End
         Begin VB.Label Label8 
            Caption         =   "Applicant"
            Height          =   255
            Left            =   240
            TabIndex        =   52
            Top             =   1800
            Width           =   1215
         End
      End
      Begin VB.OptionButton Option1 
         Caption         =   "Generator"
         Height          =   255
         Index           =   4
         Left            =   120
         TabIndex        =   39
         Top             =   1320
         Width           =   2055
      End
      Begin VB.OptionButton Option1 
         Caption         =   "Weather Condition"
         Height          =   255
         Index           =   3
         Left            =   120
         TabIndex        =   14
         Top             =   1080
         Width           =   2055
      End
      Begin VB.OptionButton Option1 
         Caption         =   "User Defined"
         Height          =   255
         Index           =   2
         Left            =   120
         TabIndex        =   13
         Top             =   840
         Width           =   2055
      End
      Begin VB.OptionButton Option1 
         Caption         =   "Traffic Condition"
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   12
         Top             =   600
         Width           =   2055
      End
      Begin VB.OptionButton Option1 
         Caption         =   "Damage Assessment"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   11
         Top             =   360
         Value           =   -1  'True
         Width           =   2055
      End
   End
   Begin VB.Label Label5 
      Caption         =   "Message Text"
      Height          =   255
      Left            =   120
      TabIndex        =   21
      Top             =   960
      Width           =   1215
   End
   Begin VB.Label Label2 
      Caption         =   "Message Name"
      Height          =   255
      Left            =   120
      TabIndex        =   10
      Top             =   600
      Width           =   1215
   End
   Begin VB.Label Label4 
      Caption         =   "GPS Longitude:"
      Height          =   255
      Left            =   120
      TabIndex        =   8
      Top             =   2280
      Width           =   1455
   End
   Begin VB.Label Label3 
      Caption         =   "GPS Latitude"
      Height          =   255
      Left            =   120
      TabIndex        =   7
      Top             =   1920
      Width           =   1455
   End
   Begin VB.Label Label1 
      Caption         =   "User Name:"
      Height          =   255
      Left            =   120
      TabIndex        =   4
      Top             =   240
      Width           =   855
   End
End
Attribute VB_Name = "Form2"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Const CLASS_DAMAGE_ASSESSMENT                   As String = "Damage_Assessment"
Private Const SUBCLASS_DEBRIS_REMOVAL                   As String = "A - Debris Removal"
Private Const SUBCLASS_EMERGENCY_PROTECTIVE_MEASURES    As String = "B - Emergency Protective Measures"
Private Const SUBCLASS_ROADS_BRIDGES                    As String = "C - Roads & Bridges"
Private Const SUBCLASS_WATER_CONTROL_FACILITIES         As String = "D - Water Control Facilities"
Private Const SUBCLASS_PUBLIC_BUILDINGS_EQUIPMENT       As String = "E - Public Buildings & Equipment"
Private Const SUBCLASS_PUBLIC_FACILITIES                As String = "F - Public Facilities"
Private Const SUBCLASS_PARKS_RECREATION                 As String = "G - Parks, Recreation, etc"

Private Const CLASS_TRAFFIC_CONDITION                   As String = "Traffic_Conditions"
Private Const SUBCLASS_ACCIDENTS                        As String = "Accidents"
Private Const SUBCLASS_ADVERSE_ROAD_CONDITIONS          As String = "Adverse Road Conditions"
Private Const SUBCLASS_ROAD_CLOSURE                     As String = "Road Closure"
Private Const SUBCLASS_ROADSIDE_ASSISTANCE              As String = "RoadSide Assistance"

Private Const CLASS_USER_DEFINED                        As String = "User_Defined_Cond_Msgs"
Private Const SUBCLASS_USER_DEFINED                     As String = "User_Defined_Cond_Msgs"

Private Const CLASS_WEATHER_CONDTION                    As String = "Weather_Conditions"
Private Const SUBCLASS_FLOOD_WARNING                    As String = "Flood Warning"
Private Const SUBCLASS_FLOOD_WATCH                      As String = "Flood Watch"
Private Const SUBCLASS_ICY_CONDITIONS                   As String = "Icy Conditions"
Private Const SUBCLASS_RAIN_STORM                       As String = "Rain Storm"
Private Const SUBCLASS_THUNDERSTORM_WARNING             As String = "ThunderStorm Warning"
Private Const SUBCLASS_THUNDERSTORM_WATCH               As String = "ThunderStorm Watch"
Private Const SUBCLASS_TORNADO_WARNING                  As String = "Tornado Warning"
Private Const SUBCLASS_TORNADO_WATCH                    As String = "Tornado Watch"
Private Const SUBCLASS_SNOW_STORM                       As String = "Snow Storm"

Private Const CLASS_GENERATOR                           As String = "Generator"
Private Const SUBCLASS_ALERT                            As String = "Alert"
Private Const SUBCLASS_GENERATOR                        As String = "Generator"
Private Const SUBCLASS_NO_DATA                          As String = "No Data"
Private Const SUBCLASS_OK                               As String = "OK"
Private Const SUBCLASS_OFFLINE                          As String = "OFFLINE"
Private Const SUBCLASS_WARNING                          As String = "WARNING"



Private mstrClass                                       As String
Private mstrSubclass                                    As String


Private Sub cmdPAR_Click()
Form4.Show
Form4.Command1.Caption = "PAR Name"
Form4.Command2.Caption = "PAR or PAD"
Form4.Command3.Visible = True
Form4.Text3.Visible = True
Form4.Command4.Visible = True
Form4.Text4.Visible = True
Form4.Command5.Visible = True
Form4.Text5.Visible = True
Form4.Command6.Visible = True
Form4.Text6.Visible = True
Form4.Command7.Visible = True
Form4.Text7.Visible = True
Form4.cmdTA.Visible = False


End Sub

Private Sub cmdTA_Click()

Form4.Show
Form4.Command1.Caption = "Threat Area Name"
Form4.Command2.Caption = "Polygon Data"
Form4.Text2.Text = "Polygon?1?5?0304250.96,5083747.95?0311362.67,5081166.30?0309657.81,5076197.84?0303763.86,5076051.71?0304250.96,5083747.95"
Form4.Text3.Visible = False
Form4.Command3.Visible = False
Form4.Text4.Visible = False
Form4.Command4.Visible = False
Form4.Text5.Visible = False

Form4.Command5.Visible = True 'close button
Form4.Text6.Visible = False
Form4.Command6.Visible = False
Form4.Text7.Visible = False
Form4.Command7.Visible = False
Form4.Text8.Visible = False
Form4.Command8.Visible = False
Form4.Text9.Visible = False
Form4.Command9.Visible = False
Form4.Command10.Visible = False
Form4.cmdPAR.Visible = False



End Sub

Private Sub Command1_Click()

    Unload Me
    
End Sub

Private Sub Command2_Click()

    Dim objEA           As New KP_API.clsEarthAlert
    Dim success         As Boolean
    Dim strUsername     As String
    Dim strMsgTitle     As String
    Dim strMsgText      As String
    Dim strLat          As String
    Dim strLon          As String
    Dim strAttr1        As String
    Dim strAttr2        As String
    Dim strAttr3        As String
    Dim strAttr4        As String
    Dim strAttr5        As String
    
    strAttr1 = ""
    strAttr2 = ""
    strAttr3 = ""
    strAttr4 = ""
    strAttr5 = ""
    
    
    
    
    
    
    
'-----------------------------------------------------------------------
'   Set up parameters that are passed for all message types
'-----------------------------------------------------------------------
    strUsername = Text1.Text
    strMsgTitle = Text2.Text
    strMsgText = Text5.Text
    strLat = Text3.Text
    strLon = Text4.Text
    
'-----------------------------------------------------------------------
'   Set up parameters based on the type of message to be sent.
'-----------------------------------------------------------------------
    If mstrClass = CLASS_DAMAGE_ASSESSMENT Then
        strAttr1 = Text6.Text
        strAttr2 = Text7.Text
        strAttr3 = Text8.Text
        strAttr4 = Text9.Text
    ElseIf mstrClass = CLASS_TRAFFIC_CONDITION Then
        If Check1.Value Then
            strAttr1 = "TRUE"
        Else
            strAttr1 = "FALSE"
        End If
        
        If Option5(0).Value Then
            strAttr2 = "1"
        ElseIf Option5(1).Value Then
            strAttr2 = "5"
        ElseIf Option5(2).Value Then
            strAttr2 = "5"
        End If
        
        strAttr3 = Text10.Text
        
        If Check2.Value Then
            strAttr4 = "TRUE"
        Else
            strAttr4 = "FALSE"
        End If
    ElseIf mstrClass = CLASS_USER_DEFINED Then
        If Check5.Value Then
            strAttr3 = "TRUE"
        Else
            strAttr3 = "FALSE"
        End If
        strAttr4 = Text12.Text
        
    ElseIf mstrClass = CLASS_WEATHER_CONDTION Then
        If Check3.Value Then
            strAttr1 = "TRUE"
        Else
            strAttr1 = "FALSE"
        End If
        
        If Option6(0).Value Then
            strAttr2 = "1"
        ElseIf Option6(1).Value Then
            strAttr2 = "5"
        ElseIf Option6(2).Value Then
            strAttr2 = "5"
        End If
        
        strAttr3 = Text11.Text
        
        If Check4.Value Then
            strAttr4 = "TRUE"
        Else
            strAttr4 = "FALSE"
        End If
    ElseIf mstrClass = CLASS_GENERATOR Then
        strAttr1 = Text16.Text
        strAttr2 = Text15.Text
        strAttr3 = Text14.Text
        strAttr4 = Text13.Text
        strAttr5 = Text12.Text
        
    
    End If
    
    If txtClassname.Text <> vbNullString Then
        'This is specific to PowerGrid project
        mstrClass = txtClassname.Text
        mstrSubclass = txtSubclassname.Text
        strAttr1 = txtAttrValue1.Text
        strAttr2 = txtAttrValue2.Text
        strAttr3 = txtAttrValue3.Text
        strAttr4 = txtAttrValue4.Text
        strAttr5 = txtAttrValue5.Text
        
        success = objEA.SetConditionalPointData(strUsername, strMsgTitle, mstrClass, mstrSubclass, strMsgText, _
                                           txtAttrName1.Text, strAttr1, txtAttrName2.Text, strAttr2, txtAttrName3.Text, strAttr3, _
                                           txtAttrName4.Text, strAttr4, txtAttrName5.Text, strAttr5, _
                                           CDbl(strLat), CDbl(strLon))
    Else
        ' This is specific to Earthalert project
        
         success = objEA.SetConditionalData(strUsername, strMsgTitle, mstrClass, mstrSubclass, strMsgText, _
                                           strAttr1, strAttr2, strAttr3, strAttr4, strAttr5, CDbl(strLat), CDbl(strLon))
    
    End If
    
    
   
   
    
    If success Then
        MsgBox "User Defined Point Successfully Added!!!", vbOKOnly
    Else
        MsgBox "There was an error setting the User Defined Point.", vbExclamation
    End If

End Sub

Private Sub Command3_Click()
Dim objEA As New KP_API.clsEarthAlert
    Dim success As Integer
    Dim strMsgTitle     As String
    
    success = objEA.DeletePoint(Text2.Text, txtClassname.Text)
        
    
    If success = 0 Then
        MsgBox "User Defined point successfully delete!!", vbOKOnly
    Else
        MsgBox "There was an error setting the User Defined Point.", vbExclamation
    End If
    
End Sub

Private Sub Command4_Click()

End Sub

Private Sub Form_Load()

        Option1(0).Value = True
        Option2(0).Value = True
        Option3(0).Value = True
        Option4(0).Value = True
        Option5(0).Value = True
        Option6(0).Value = True
        fraGenClass.Visible = True
        
        fraDamageAssessment.Visible = False
        fraTrafficCondition.Visible = False
        fraUserDefinedConds.Visible = False
        fraWeatherCondition.Visible = False
        fraGenerators.Visible = False
        fraGeneric.Visible = True
       ' mstrClass = CLASS_DAMAGE_ASSESSMENT
      '  mstrSubclass = SUBCLASS_DEBRIS_REMOVAL
        
End Sub

Private Sub Option1_Click(Index As Integer)

    If Index = 0 Then
        'Damage Assessment
        fraDamageAssessment.Visible = True
        fraTrafficCondition.Visible = False
        fraUserDefinedConds.Visible = False
        fraWeatherCondition.Visible = False
        fraGenerators.Visible = False
        mstrClass = CLASS_DAMAGE_ASSESSMENT
        mstrSubclass = SUBCLASS_DEBRIS_REMOVAL
        Option2(0).Value = True
    ElseIf Index = 1 Then
        'Traffic Condition
        fraDamageAssessment.Visible = False
        fraTrafficCondition.Visible = True
        fraUserDefinedConds.Visible = False
        fraWeatherCondition.Visible = False
        fraGenerators.Visible = False
        mstrClass = CLASS_TRAFFIC_CONDITION
        mstrSubclass = SUBCLASS_ACCIDENTS
        Option3(0).Value = True
    ElseIf Index = 2 Then
        'User Defined
        fraDamageAssessment.Visible = False
        fraTrafficCondition.Visible = False
        fraUserDefinedConds.Visible = True
        fraGenerators.Visible = False
        fraWeatherCondition.Visible = False
        mstrClass = CLASS_USER_DEFINED
        mstrSubclass = SUBCLASS_USER_DEFINED
        
    ElseIf Index = 3 Then
        'Weather Condition
        fraDamageAssessment.Visible = False
        fraTrafficCondition.Visible = False
        fraUserDefinedConds.Visible = False
        fraGenerators.Visible = False
        fraWeatherCondition.Visible = True
        mstrClass = CLASS_WEATHER_CONDTION
        mstrSubclass = SUBCLASS_FLOOD_WARNING
        Option4(0).Value = True
    ElseIf Index = 4 Then
        'Generators for Power Grid
        fraDamageAssessment.Visible = False
        fraTrafficCondition.Visible = False
        fraUserDefinedConds.Visible = False
        fraWeatherCondition.Visible = False
        fraGenerators.Visible = True
        
        mstrClass = CLASS_GENERATOR
        mstrSubclass = SUBCLASS_GENERATOR
        OptionGen(12).Value = True
    End If

End Sub

Private Sub Option2_Click(Index As Integer)

    If Index = 0 Then
        mstrSubclass = SUBCLASS_DEBRIS_REMOVAL
    ElseIf Index = 1 Then
        mstrSubclass = SUBCLASS_EMERGENCY_PROTECTIVE_MEASURES
    ElseIf Index = 2 Then
        mstrSubclass = SUBCLASS_ROADS_BRIDGES
    ElseIf Index = 3 Then
        mstrSubclass = SUBCLASS_WATER_CONTROL_FACILITIES
    ElseIf Index = 4 Then
        mstrSubclass = SUBCLASS_PUBLIC_BUILDINGS_EQUIPMENT
    ElseIf Index = 5 Then
        mstrSubclass = SUBCLASS_PUBLIC_FACILITIES
    ElseIf Index = 6 Then
        mstrSubclass = SUBCLASS_PARKS_RECREATION
    End If

End Sub

Private Sub Option3_Click(Index As Integer)

    If Index = 0 Then
        mstrSubclass = SUBCLASS_ACCIDENTS
    ElseIf Index = 1 Then
        mstrSubclass = SUBCLASS_ADVERSE_ROAD_CONDITIONS
    ElseIf Index = 2 Then
        mstrSubclass = SUBCLASS_ROAD_CLOSURE
    ElseIf Index = 3 Then
        mstrSubclass = SUBCLASS_ROADSIDE_ASSISTANCE
    End If

End Sub

Private Sub Option4_Click(Index As Integer)

    If Index = 0 Then
        mstrSubclass = SUBCLASS_FLOOD_WARNING
    ElseIf Index = 1 Then
        mstrSubclass = SUBCLASS_FLOOD_WATCH
    ElseIf Index = 2 Then
        mstrSubclass = SUBCLASS_ICY_CONDITIONS
    ElseIf Index = 3 Then
        mstrSubclass = SUBCLASS_RAIN_STORM
    ElseIf Index = 4 Then
        mstrSubclass = SUBCLASS_THUNDERSTORM_WARNING
    ElseIf Index = 5 Then
        mstrSubclass = SUBCLASS_THUNDERSTORM_WATCH
    ElseIf Index = 6 Then
        mstrSubclass = SUBCLASS_TORNADO_WARNING
    ElseIf Index = 7 Then
        mstrSubclass = SUBCLASS_TORNADO_WATCH
    ElseIf Index = 8 Then
        mstrSubclass = SUBCLASS_SNOW_STORM
    End If
    
End Sub

Private Sub OptionGen_Click(Index As Integer)
    If Index = 7 Then
        mstrSubclass = SUBCLASS_ALERT
    ElseIf Index = 12 Then
        mstrSubclass = SUBCLASS_GENERATOR
    ElseIf Index = 11 Then
        mstrSubclass = SUBCLASS_NO_DATA
    ElseIf Index = 10 Then
        mstrSubclass = SUBCLASS_OK
    ElseIf Index = 9 Then
        mstrSubclass = SUBCLASS_OFFLINE
    ElseIf Index = 8 Then
        mstrSubclass = SUBCLASS_WARNING
    End If
End Sub

