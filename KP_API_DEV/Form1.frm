VERSION 5.00
Begin VB.Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   5865
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   11040
   LinkTopic       =   "Form1"
   ScaleHeight     =   5865
   ScaleWidth      =   11040
   StartUpPosition =   3  'Windows Default
   Begin VB.CommandButton Command15 
      Caption         =   "Command15"
      Height          =   375
      Left            =   8760
      TabIndex        =   43
      Top             =   1200
      Width           =   855
   End
   Begin VB.CheckBox Check1 
      Caption         =   "Static"
      Height          =   375
      Left            =   4680
      TabIndex        =   42
      Top             =   1320
      Width           =   1095
   End
   Begin VB.CommandButton Command14 
      Caption         =   "Set Conditional Data"
      Height          =   735
      Left            =   4920
      TabIndex        =   41
      Top             =   240
      Width           =   4575
   End
   Begin VB.Frame Frame2 
      Height          =   1095
      Left            =   4800
      TabIndex        =   33
      Top             =   2640
      Width           =   6135
      Begin VB.TextBox Text14 
         Alignment       =   1  'Right Justify
         Height          =   285
         Left            =   2400
         TabIndex        =   40
         Text            =   "10000109"
         Top             =   720
         Width           =   975
      End
      Begin VB.TextBox Text13 
         Alignment       =   1  'Right Justify
         Height          =   285
         Left            =   2400
         TabIndex        =   39
         Text            =   "10000108"
         Top             =   480
         Width           =   975
      End
      Begin VB.TextBox Text7 
         Alignment       =   1  'Right Justify
         Height          =   285
         Left            =   2400
         TabIndex        =   38
         Text            =   "10000106"
         Top             =   240
         Width           =   975
      End
      Begin VB.OptionButton Option2 
         Caption         =   "Department"
         Height          =   255
         Index           =   1
         Left            =   3480
         TabIndex        =   37
         Top             =   480
         Width           =   1335
      End
      Begin VB.OptionButton Option2 
         Caption         =   "Person"
         Height          =   255
         Index           =   2
         Left            =   3480
         TabIndex        =   36
         Top             =   720
         Width           =   1335
      End
      Begin VB.OptionButton Option2 
         Caption         =   "Agency"
         Height          =   255
         Index           =   0
         Left            =   3480
         TabIndex        =   35
         Top             =   240
         Value           =   -1  'True
         Width           =   1335
      End
      Begin VB.CommandButton Command11 
         Caption         =   "Get Contacts Notif"
         Height          =   375
         Left            =   120
         TabIndex        =   34
         Top             =   240
         Width           =   2175
      End
   End
   Begin VB.Frame Frame1 
      Height          =   1335
      Left            =   4800
      TabIndex        =   23
      Top             =   4320
      Width           =   6135
      Begin VB.TextBox Text12 
         Alignment       =   1  'Right Justify
         Height          =   285
         Left            =   2400
         TabIndex        =   32
         Text            =   "10000103"
         Top             =   960
         Width           =   975
      End
      Begin VB.TextBox Text11 
         Alignment       =   1  'Right Justify
         Height          =   285
         Left            =   2400
         TabIndex        =   31
         Text            =   "10000101"
         Top             =   720
         Width           =   975
      End
      Begin VB.TextBox Text10 
         Alignment       =   1  'Right Justify
         Height          =   285
         Left            =   2400
         TabIndex        =   30
         Text            =   "10000102"
         Top             =   480
         Width           =   975
      End
      Begin VB.OptionButton Option1 
         Caption         =   "Weather_Conditions"
         Height          =   255
         Index           =   3
         Left            =   3480
         TabIndex        =   29
         Top             =   960
         Width           =   2295
      End
      Begin VB.OptionButton Option1 
         Caption         =   "User_Defined_Cond_Msg"
         Height          =   255
         Index           =   2
         Left            =   3480
         TabIndex        =   28
         Top             =   720
         Width           =   2295
      End
      Begin VB.OptionButton Option1 
         Caption         =   "Traffic_Conditions"
         Height          =   255
         Index           =   1
         Left            =   3480
         TabIndex        =   27
         Top             =   480
         Width           =   2295
      End
      Begin VB.TextBox Text9 
         Alignment       =   1  'Right Justify
         Height          =   285
         Left            =   2400
         TabIndex        =   26
         Text            =   "10000104"
         Top             =   240
         Width           =   975
      End
      Begin VB.OptionButton Option1 
         Caption         =   "Damage_Assessment"
         Height          =   255
         Index           =   0
         Left            =   3480
         TabIndex        =   25
         Top             =   240
         Value           =   -1  'True
         Width           =   2295
      End
      Begin VB.CommandButton Command13 
         Caption         =   "Get Conditional Data Notif"
         Height          =   375
         Left            =   120
         TabIndex        =   24
         Top             =   240
         Width           =   2175
      End
   End
   Begin VB.TextBox Text8 
      Alignment       =   1  'Right Justify
      Height          =   285
      Left            =   7200
      TabIndex        =   21
      Text            =   "10000040"
      Top             =   3840
      Width           =   975
   End
   Begin VB.TextBox Text6 
      Alignment       =   1  'Right Justify
      Height          =   285
      Left            =   7200
      TabIndex        =   20
      Text            =   "10000002"
      Top             =   1800
      Width           =   975
   End
   Begin VB.TextBox Text5 
      Alignment       =   1  'Right Justify
      Height          =   285
      Left            =   7200
      TabIndex        =   19
      Text            =   "10000024"
      Top             =   2280
      Width           =   975
   End
   Begin VB.CommandButton Command12 
      Caption         =   "Get User Data Notif"
      Height          =   375
      Left            =   4920
      TabIndex        =   18
      Top             =   3840
      Width           =   2175
   End
   Begin VB.CommandButton Command10 
      Caption         =   "Get Raw Weather Notif"
      Height          =   375
      Left            =   4920
      TabIndex        =   17
      Top             =   2280
      Width           =   2175
   End
   Begin VB.CommandButton Command9 
      Caption         =   "Get Facilities Notif"
      Height          =   375
      Left            =   4920
      TabIndex        =   16
      Top             =   1800
      Width           =   2175
   End
   Begin VB.CommandButton Command8 
      Caption         =   "Get Conditional Data"
      Height          =   375
      Left            =   2520
      TabIndex        =   15
      Top             =   4560
      Width           =   2175
   End
   Begin VB.TextBox Text1 
      Height          =   285
      Left            =   2880
      TabIndex        =   10
      Top             =   240
      Width           =   1695
   End
   Begin VB.TextBox Text2 
      Height          =   285
      Left            =   2880
      TabIndex        =   9
      Top             =   600
      Width           =   1695
   End
   Begin VB.TextBox Text3 
      Height          =   285
      Left            =   2880
      TabIndex        =   8
      Text            =   "39.3943"
      Top             =   960
      Width           =   1695
   End
   Begin VB.TextBox Text4 
      Height          =   285
      Left            =   2880
      TabIndex        =   7
      Text            =   "-76.2534"
      Top             =   1320
      Width           =   1695
   End
   Begin VB.CommandButton Command7 
      Caption         =   "Get User Data"
      Height          =   375
      Left            =   2520
      TabIndex        =   6
      Top             =   3840
      Width           =   2175
   End
   Begin VB.CommandButton Command6 
      Caption         =   "Get Contacts"
      Height          =   375
      Left            =   2520
      TabIndex        =   5
      Top             =   2880
      Width           =   2175
   End
   Begin VB.CommandButton Command5 
      Caption         =   "Get Raw Weather"
      Height          =   375
      Left            =   2520
      TabIndex        =   4
      Top             =   2280
      Width           =   2175
   End
   Begin VB.CommandButton Command4 
      Caption         =   "Get Facilities"
      Height          =   375
      Left            =   2520
      TabIndex        =   3
      Top             =   1800
      Width           =   2175
   End
   Begin VB.CommandButton Command3 
      Caption         =   "Logout PDA User"
      Height          =   375
      Left            =   120
      TabIndex        =   2
      Top             =   2760
      Width           =   2175
   End
   Begin VB.CommandButton Command2 
      Caption         =   "Update PDA User"
      Height          =   375
      Left            =   120
      TabIndex        =   1
      Top             =   2280
      Width           =   2175
   End
   Begin VB.CommandButton Command1 
      Caption         =   "Login PDA User"
      Height          =   375
      Left            =   120
      TabIndex        =   0
      Top             =   1800
      Width           =   2175
   End
   Begin VB.Label Label5 
      Caption         =   "Notification ID"
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
      Left            =   7080
      TabIndex        =   22
      Top             =   1440
      Width           =   1335
   End
   Begin VB.Label Label1 
      Caption         =   "EMA Username:"
      Height          =   255
      Left            =   1320
      TabIndex        =   14
      Top             =   240
      Width           =   1455
   End
   Begin VB.Label Label2 
      Caption         =   "EMA Password:"
      Height          =   255
      Left            =   1320
      TabIndex        =   13
      Top             =   600
      Width           =   1455
   End
   Begin VB.Label Label3 
      Caption         =   "GPS Latitude"
      Height          =   255
      Left            =   1320
      TabIndex        =   12
      Top             =   960
      Width           =   1455
   End
   Begin VB.Label Label4 
      Caption         =   "GPS Longitude:"
      Height          =   255
      Left            =   1320
      TabIndex        =   11
      Top             =   1320
      Width           =   1455
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub Command1_Click()

    Dim objEA As New KP_API.clsEarthAlert
    Dim success As Integer
    
    success = objEA.PDA_Login(Text1, Text2, CDbl(Text3), CDbl(Text4), True)
    If success = 0 Then
        MsgBox "User was logged in sucessfully", vbOKOnly
    Else
        MsgBox "User was not logged in successfully", vbExclamation
    End If
    
End Sub

Private Sub Command10_Click()

    Dim objEA As New KP_API.clsEarthAlert
    Dim rsTemp As ADODB.Recordset
    Dim success As Integer
    Dim lngReccount As Integer
    Dim lngID As Long
    
    If Len(Text5.Text) Then
        lngID = CDbl(Text5.Text)
    Else
        Text5.Text = "10000024" 'default test value
        lngID = CDbl(Text5.Text)
    End If
    
    Set rsTemp = New ADODB.Recordset
    
    success = objEA.GetMETDataNotif(Text1, lngID, rsTemp)
    If success = 0 Then
        ' get the record count from the recordset returned
        lngReccount = rsTemp.RecordCount
        MsgBox lngReccount & " Raw Weather records were returned", vbOKOnly
        rsTemp.Close
        Set rsTemp = Nothing
    Else
        MsgBox "There was an error returning the raw weather records", vbExclamation
    End If

End Sub

Private Sub Command11_Click()

    Dim objEA As New KP_API.clsEarthAlert
    Dim rsTemp As ADODB.Recordset
    Dim success As Integer
    Dim lngReccount As Integer
    Dim lngID As Long
    
    If Option2(0).Value Then
        If Len(Text7.Text) Then
            lngID = CDbl(Text7.Text)
        Else
            Text7.Text = "10000106" 'default test value
            lngID = CDbl(Text7.Text)
        End If
    ElseIf Option2(1).Value Then
        If Len(Text13.Text) Then
            lngID = CDbl(Text13.Text)
        Else
            Text13.Text = "10000106" 'default test value
            lngID = CDbl(Text13.Text)
        End If
    ElseIf Option2(2).Value Then
        If Len(Text14.Text) Then
            lngID = CDbl(Text14.Text)
        Else
            Text14.Text = "10000109" 'default test value
            lngID = CDbl(Text14.Text)
        End If
    End If
    
    
    
    
    
    
    
    
    Set rsTemp = New ADODB.Recordset
    
    success = objEA.GetContactDataNotif(Text1, lngID, rsTemp)
    If success = 0 Then
        ' get the record count from the recordset returned
        lngReccount = rsTemp.RecordCount
        MsgBox lngReccount & " Contact records were returned", vbOKOnly
        rsTemp.Close
        Set rsTemp = Nothing
    Else
        MsgBox "There was an error returning the contact records", vbExclamation
    End If
End Sub

Private Sub Command12_Click()

    Dim objEA As New KP_API.clsEarthAlert
    Dim rsTemp As ADODB.Recordset
    Dim success As Integer
    Dim lngReccount As Integer
    Dim lngID As Long
    
    If Len(Text8.Text) Then
        lngID = CDbl(Text8.Text)
    Else
        Text5.Text = "10000040" 'default test value
        lngID = CDbl(Text8.Text)
    End If
    
    Set rsTemp = New ADODB.Recordset
    
    success = objEA.GetUserDataNotif(Text1, lngID, rsTemp)
    If success = 0 Then
        ' get the record count from the recordset returned
        lngReccount = rsTemp.RecordCount
        MsgBox lngReccount & " User records were returned", vbOKOnly
        rsTemp.Close
        Set rsTemp = Nothing
    Else
        MsgBox "There was an error returning the User records", vbExclamation
    End If
End Sub

Private Sub Command13_Click()

    Dim objEA As New KP_API.clsEarthAlert
    Dim rsTemp As ADODB.Recordset
    Dim success As Integer
    Dim lngReccount As Integer
    Dim lngID As Long
    
    If Option1(0).Value Then
        If Len(Text9.Text) Then
            lngID = CDbl(Text9.Text)
        Else
            Text9.Text = "10000104"
            lngID = CDbl(Text9.Text)
        End If
    ElseIf Option1(1).Value Then
        If Len(Text10.Text) Then
            lngID = CDbl(Text10.Text)
        Else
            Text10.Text = "10000102"
            lngID = CDbl(Text10.Text)
        End If
    ElseIf Option1(2).Value Then
        If Len(Text11.Text) Then
            lngID = CDbl(Text11.Text)
        Else
            Text11.Text = "10000101"
        End If
    Else
        If Len(Text12.Text) Then
            lngID = CDbl(Text12.Text)
        Else
            Text12.Text = "10000103"
        End If
    End If
    
    Set rsTemp = New ADODB.Recordset
    
    success = objEA.GetConditionalDataNotif(Text1, lngID, rsTemp)
    If success = 0 Then
        ' get the record count from the recordset returned
        lngReccount = rsTemp.RecordCount
        MsgBox lngReccount & " User records were returned", vbOKOnly
        rsTemp.Close
        Set rsTemp = Nothing
    Else
        MsgBox "There was an error returning the User records", vbExclamation
    End If
End Sub

Private Sub Command14_Click()

    Form2.Show
    
End Sub

Private Sub Command15_Click()
    Form3.Show
End Sub

Private Sub Command2_Click()

    Dim objEA As New KP_API.clsEarthAlert
    Dim success As Integer
    
    success = objEA.PDA_Update(Text1, CDbl(Text3), CDbl(Text4))
    If success = 0 Then
        MsgBox "User was updated successfully", vbOKOnly
    Else
        MsgBox "User was not updated successfully", vbExclamation
    End If
    
End Sub

Private Sub Command3_Click()
    
    Dim objEA As New KP_API.clsEarthAlert
    Dim success As Integer
    
    success = objEA.PDA_Logout(Text1)
    If success = 0 Then
        MsgBox "User was logged out successfully", vbOKCancel
    Else
        MsgBox "User was not logged out successfully", vbExclamation
    End If
    
End Sub

Private Sub Command4_Click()

    Dim objEA As New KP_API.clsEarthAlert
    Dim rsTemp As ADODB.Recordset
    Dim success As Integer
    Dim lngReccount As Integer
    
    Set rsTemp = New ADODB.Recordset
    
    success = objEA.GetFacilityData(Text1, rsTemp)
    If success = 0 Then
        ' get the record count from the recordset returned
        lngReccount = rsTemp.RecordCount
        MsgBox lngReccount & " facility records were returned for user dorow", vbOKOnly
        rsTemp.Close
        Set rsTemp = Nothing
    Else
        MsgBox "There was an error returning the facility records for user dorow", vbExclamation
    End If

End Sub

Private Sub Command5_Click()

    Dim objEA As New KP_API.clsEarthAlert
    Dim rsTemp As ADODB.Recordset
    Dim success As Integer
    Dim lngReccount As Integer
    
    Set rsTemp = New ADODB.Recordset
    
    success = objEA.GetMETData(Text1, rsTemp)
    If success = 0 Then
        ' get the record count from the recordset returned
        lngReccount = rsTemp.RecordCount
        MsgBox lngReccount & " Raw Weather records were returned", vbOKOnly
        rsTemp.Close
        Set rsTemp = Nothing
    Else
        MsgBox "There was an error returning the raw weather records", vbExclamation
    End If
End Sub

Private Sub Command6_Click()

    Dim objEA As New KP_API.clsEarthAlert
    Dim rsTemp As ADODB.Recordset
    Dim success As Integer
    Dim lngReccount As Integer
    
    Set rsTemp = New ADODB.Recordset
    
    success = objEA.GetContactData(Text1, rsTemp)
    If success = 0 Then
        ' get the record count from the recordset returned
        lngReccount = rsTemp.RecordCount
        MsgBox lngReccount & " Contact records were returned", vbOKOnly
        rsTemp.Close
        Set rsTemp = Nothing
    Else
        MsgBox "There was an error returning the contact records", vbExclamation
    End If
End Sub

Private Sub Command7_Click()

    Dim objEA As New KP_API.clsEarthAlert
    Dim rsTemp As ADODB.Recordset
    Dim success As Integer
    Dim lngReccount As Integer
    
    Set rsTemp = New ADODB.Recordset
    
    success = objEA.GetUserData(Text1, rsTemp)
    If success = 0 Then
        ' get the record count from the recordset returned
        lngReccount = rsTemp.RecordCount
        MsgBox lngReccount & " User records were returned", vbOKOnly
        rsTemp.Close
        Set rsTemp = Nothing
    Else
        MsgBox "There was an error returning the User records", vbExclamation
    End If
End Sub

Private Sub Command8_Click()

    Dim objEA As New KP_API.clsEarthAlert
    Dim rsTemp As ADODB.Recordset
    Dim success As Integer
    Dim lngReccount As Integer
    
    Set rsTemp = New ADODB.Recordset
    
    success = objEA.GetConditionalData(Text1, rsTemp)
    If success = 0 Then
        ' get the record count from the recordset returned
        lngReccount = rsTemp.RecordCount
        MsgBox lngReccount & " User records were returned", vbOKOnly
        rsTemp.Close
        Set rsTemp = Nothing
    Else
        MsgBox "There was an error returning the User records", vbExclamation
    End If
End Sub

Private Sub Command9_Click()

    Dim objEA As New KP_API.clsEarthAlert
    Dim rsTemp As ADODB.Recordset
    Dim success As Integer
    Dim lngReccount As Integer
    Dim lngID As Long
    
    If Len(Text6.Text) Then
        lngID = CDbl(Text6.Text)
    Else
        Text5.Text = "10000002" 'default test value
        lngID = CDbl(Text6.Text)
    End If
    
    Set rsTemp = New ADODB.Recordset
    
    success = objEA.GetFacilityDataNotif(Text1, lngID, rsTemp)
    If success = 0 Then
        ' get the record count from the recordset returned
        lngReccount = rsTemp.RecordCount
        MsgBox lngReccount & " facility records were returned for user dorow", vbOKOnly
        rsTemp.Close
        Set rsTemp = Nothing
    Else
        MsgBox "There was an error returning the facility records for user dorow", vbExclamation
    End If

End Sub

