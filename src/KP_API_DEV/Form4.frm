VERSION 5.00
Begin VB.Form Form4 
   Caption         =   "Form4"
   ClientHeight    =   4545
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   7995
   LinkTopic       =   "Form4"
   ScaleHeight     =   4545
   ScaleWidth      =   7995
   StartUpPosition =   3  'Windows Default
   Begin VB.TextBox Text9 
      Height          =   375
      Left            =   6360
      TabIndex        =   20
      Text            =   "zone"
      Top             =   2040
      Width           =   1095
   End
   Begin VB.CommandButton Command10 
      Caption         =   "zone"
      Height          =   375
      Left            =   4560
      TabIndex        =   19
      Top             =   2040
      Width           =   1335
   End
   Begin VB.TextBox Text8 
      Height          =   375
      Left            =   6360
      TabIndex        =   18
      Top             =   1440
      Width           =   1095
   End
   Begin VB.CommandButton Command9 
      Caption         =   "Zone Name"
      Height          =   375
      Left            =   4560
      TabIndex        =   17
      Top             =   1440
      Width           =   1335
   End
   Begin VB.TextBox Text7 
      Height          =   375
      Left            =   3360
      TabIndex        =   16
      Top             =   3600
      Width           =   615
   End
   Begin VB.CommandButton Command8 
      Caption         =   "Time4"
      Height          =   375
      Left            =   120
      TabIndex        =   15
      Top             =   3600
      Width           =   2175
   End
   Begin VB.TextBox Text6 
      Height          =   375
      Left            =   3360
      TabIndex        =   14
      Top             =   3120
      Width           =   615
   End
   Begin VB.CommandButton Command7 
      Caption         =   "Time3"
      Height          =   375
      Left            =   120
      TabIndex        =   13
      Top             =   3120
      Width           =   2175
   End
   Begin VB.TextBox Text5 
      Height          =   375
      Left            =   3360
      TabIndex        =   12
      Top             =   2760
      Width           =   615
   End
   Begin VB.CommandButton Command6 
      Caption         =   "Time2"
      Height          =   375
      Left            =   120
      TabIndex        =   11
      Top             =   2640
      Width           =   2175
   End
   Begin VB.CommandButton Command5 
      Caption         =   "Close"
      Height          =   375
      Left            =   5880
      TabIndex        =   10
      Top             =   4080
      Width           =   1815
   End
   Begin VB.CommandButton Command4 
      Caption         =   "Time1"
      Height          =   375
      Left            =   120
      TabIndex        =   9
      Top             =   2160
      Width           =   2175
   End
   Begin VB.CommandButton Command3 
      Caption         =   "Recom Index"
      Height          =   495
      Left            =   120
      TabIndex        =   8
      Top             =   1440
      Width           =   2175
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Command2"
      Height          =   375
      Left            =   120
      TabIndex        =   7
      Top             =   840
      Width           =   2175
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Command1"
      Height          =   375
      Left            =   120
      TabIndex        =   6
      Top             =   240
      Width           =   2175
   End
   Begin VB.CommandButton cmdTA 
      Caption         =   "Insert TA"
      Height          =   375
      Left            =   1560
      TabIndex        =   5
      Top             =   4080
      Width           =   1455
   End
   Begin VB.CommandButton cmdPAR 
      Caption         =   "Insert Par"
      Height          =   375
      Left            =   3720
      TabIndex        =   4
      Top             =   4080
      Width           =   1815
   End
   Begin VB.TextBox Text4 
      Height          =   375
      Left            =   3360
      TabIndex        =   3
      Top             =   2160
      Width           =   615
   End
   Begin VB.TextBox Text3 
      Height          =   375
      Left            =   3360
      TabIndex        =   2
      Top             =   1440
      Width           =   615
   End
   Begin VB.TextBox Text2 
      Height          =   375
      Left            =   3360
      TabIndex        =   1
      Top             =   840
      Width           =   2535
   End
   Begin VB.TextBox Text1 
      Height          =   375
      Left            =   3360
      TabIndex        =   0
      Top             =   240
      Width           =   2535
   End
End
Attribute VB_Name = "Form4"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Public mlObjectID As Long

Private Sub cmdPAR_Click()
Dim objEA As New KP_API.clsEarthAlert
 Dim intSuccess As Integer
 'Dim lObjectID As Long
 Dim stParName As String
 Dim stParType As String
 
 
 stParName = Text1.Text
 stParType = Text2.Text
 
 
 
 mlObjectID = objEA.SavePAR(stParName, stParType, "www.arm.gov", False, True)
 If mlObjectID > 0 Then
 
intSuccess = objEA.SavePADetail(mlObjectID, Text8.Text, Text9.Text, Text3.Text, Text4.Text, Text5.Text, Text6.Text, Text7.Text)
    'intSuccess = objEA.SavePADetail(lObjectID, "R", "zone", 5, 120, 120, 120, 120)
    
    If intSuccess Then
        intSuccess = objEA.SavePADetail(mlObjectID, "Raytheon Demil Site", "point", "6", "70", "70", "70", "120")
        If intSuccess Then
            MsgBox "PAR Successfully Added!!!", vbOKOnly
        Else
            MsgBox "Could not add times to points in PA unit table", vbOKOnly
        End If
        
    Else
        MsgBox "Could not add times to zones in PA unit table", vbOKOnly
        
    End If
    
    
    
 Else
    MsgBox "PAR Not a success!!!", vbOKOnly
End If
End Sub


Private Sub cmdTA_Click()
 Dim objEA As New KP_API.clsEarthAlert
 Dim intSuccess As Integer
 Dim stTAcat As String
 Dim stName As String
 Dim stType As String
 Dim IsolRad As Long
 Dim dDist As Double
 Dim ldir As Long
 Dim langle As Long
 Dim dlon As Double
 Dim dlat As Double
 Dim stLoctype As String
 Dim stData As String
 
 stTAcat = "userdefined"
 stType = "polygon" '"wedge" '
 stName = Text1.Text '"Test Chitra"
 IsolRad = 0
dDist = 0 ' 498.95
 ldir = 0
 langle = 0
 dlon = 0 ' 45.838301
 dlat = 0  ' -119.47491
 stLoctype = "" '"point" 'object, point
'stData = "Polygon?1?5?0304250.96,5083747.95?0311362.67,5081166.30?0309657.81,5076197.84?0303763.86,5076051.71?0304250.96,5083747.95"
stData = Text2.Text

 intSuccess = objEA.SaveTA(stTAcat, stType, stName, stData, 0, "no effects", langle, dDist, ldir, dlat, dlon, "", 0)
 If intSuccess Then
    MsgBox "TA Successfully Added!!!", vbOKOnly
 Else
    MsgBox "TA Not a success!!!", vbOKOnly
End If

    
End Sub


Private Sub Command11_Click()

End Sub

Private Sub cmdZoneTime_Click()
Dim objEA As New KP_API.clsEarthAlert
Dim intSuccess As Integer



If mlObjectID < 1 Then
     mlObjectID = objEA.SavePAR(Text1.Text, Text2.Text, "www.arm.gov", False, True)
End If

intSuccess = objEA.SavePADetail(mlObjectID, Text8.Text, Text9.Text, Text3.Text, Text4.Text, Text5.Text, Text6.Text, Text7.Text)
If intSuccess Then
            MsgBox "PAR Successfully Added!!!", vbOKOnly
        Else
            MsgBox "Could not add times to points in PA unit table", vbOKOnly
        End If
End Sub

Private Sub Command5_Click()
Unload Me

End Sub


