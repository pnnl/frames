VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form Messenger 
   Caption         =   "Message"
   ClientHeight    =   5805
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   6375
   LinkTopic       =   "Form1"
   ScaleHeight     =   5805
   ScaleWidth      =   6375
   StartUpPosition =   3  'Windows Default
   Begin VB.Frame Frame1 
      Caption         =   "Messenger"
      Height          =   5055
      Left            =   360
      TabIndex        =   0
      Top             =   120
      Width           =   5775
      Begin VB.CommandButton cmdGetURLPAR 
         Height          =   330
         Left            =   4080
         Picture         =   "Messenger.frx":0000
         Style           =   1  'Graphical
         TabIndex        =   18
         ToolTipText     =   "Select a document"
         Top             =   3600
         Width           =   435
      End
      Begin VB.TextBox txtfilenamePAR 
         Height          =   285
         Left            =   2400
         TabIndex        =   17
         Top             =   3600
         Width           =   1575
      End
      Begin VB.TextBox txtFilenameTA 
         Height          =   285
         Left            =   2400
         TabIndex        =   15
         Top             =   2400
         Width           =   1575
      End
      Begin VB.CommandButton cmdGetURLTA 
         Height          =   330
         Left            =   4080
         Picture         =   "Messenger.frx":014A
         Style           =   1  'Graphical
         TabIndex        =   14
         ToolTipText     =   "Select a document"
         Top             =   2400
         Width           =   435
      End
      Begin VB.CommandButton cmdPAR 
         Caption         =   "Insert PAR"
         Height          =   375
         Left            =   2400
         TabIndex        =   13
         Top             =   3960
         Width           =   1095
      End
      Begin VB.CommandButton cmdTA 
         Caption         =   "Insert TA"
         Height          =   375
         Left            =   2400
         TabIndex        =   12
         Top             =   2760
         Width           =   1095
      End
      Begin VB.TextBox txtFileName 
         Height          =   285
         Left            =   1800
         TabIndex        =   10
         Top             =   240
         Width           =   2535
      End
      Begin VB.CommandButton cmdGetURL 
         Height          =   330
         Left            =   4440
         Picture         =   "Messenger.frx":0294
         Style           =   1  'Graphical
         TabIndex        =   9
         ToolTipText     =   "Select a document"
         Top             =   240
         Width           =   435
      End
      Begin VB.CommandButton Command8 
         Caption         =   "Delete Status 4"
         Enabled         =   0   'False
         Height          =   375
         Left            =   480
         TabIndex        =   8
         Top             =   4200
         Width           =   1215
      End
      Begin VB.CommandButton Command7 
         Caption         =   "Add Status 4"
         Enabled         =   0   'False
         Height          =   375
         Left            =   480
         TabIndex        =   7
         Top             =   3720
         Width           =   1215
      End
      Begin VB.CommandButton Command6 
         Caption         =   "Delete Status 3"
         Enabled         =   0   'False
         Height          =   375
         Left            =   480
         TabIndex        =   6
         Top             =   3240
         Width           =   1215
      End
      Begin VB.CommandButton Command5 
         Caption         =   "Add Status3"
         Enabled         =   0   'False
         Height          =   375
         Left            =   480
         TabIndex        =   5
         Top             =   2760
         Width           =   1215
      End
      Begin VB.CommandButton Command4 
         Caption         =   "Delete Status 2"
         Enabled         =   0   'False
         Height          =   375
         Left            =   480
         TabIndex        =   4
         Top             =   2280
         Width           =   1215
      End
      Begin VB.CommandButton Command3 
         Caption         =   "Add Status 2"
         Enabled         =   0   'False
         Height          =   375
         Left            =   480
         TabIndex        =   3
         Top             =   1800
         Width           =   1215
      End
      Begin VB.CommandButton Command2 
         Caption         =   "Delete Status 1"
         Enabled         =   0   'False
         Height          =   375
         Left            =   480
         TabIndex        =   2
         Top             =   1320
         Width           =   1215
      End
      Begin VB.CommandButton Command1 
         Caption         =   "Add Status 1"
         Enabled         =   0   'False
         Height          =   375
         Left            =   480
         TabIndex        =   1
         Top             =   840
         Width           =   1215
      End
      Begin MSComDlg.CommonDialog dlgFileName 
         Left            =   1800
         Top             =   4560
         _ExtentX        =   847
         _ExtentY        =   847
         _Version        =   393216
         CancelError     =   -1  'True
      End
      Begin VB.Label Label2 
         Caption         =   "Config for PAR"
         Height          =   255
         Left            =   2400
         TabIndex        =   19
         Top             =   3240
         Width           =   1935
      End
      Begin VB.Label Label1 
         Caption         =   "Config for TA"
         Height          =   255
         Left            =   2400
         TabIndex        =   16
         Top             =   2040
         Width           =   1935
      End
      Begin VB.Label lblconfig 
         Caption         =   "Configuration File:"
         Height          =   255
         Left            =   240
         TabIndex        =   11
         Top             =   240
         Width           =   1575
      End
   End
End
Attribute VB_Name = "Messenger"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Public gstConfigFilePath As String
Public gstConfigFileNmae As String
Public gstFirstFile As String
Public gstSecondFile As String
Public gstThirdFile As String
Public gstFourthFile As String
Public gstFifthFile As String
Public gstSixthFile As String
Public gstSeventhFile As String
Public gstEightthFile As String
Public gstTAFile As String
Public gstPARFile As String


Public Function ReadConfigFile(stfilename As String, stfilepath As String)
Dim objFSO As New FileSystemObject
Dim infile As File
Dim inStream As TextStream
Dim strTemp As String
Dim GetEXEPath As String

    GetEXEPath = stfilename
    
    Set infile = objFSO.GetFile(GetEXEPath)
    ' set up the file to be read
    Set inStream = infile.OpenAsTextStream(ForReading, TristateFalse)
    ' read in the global constants from the ini file
    gstFirstFile = stfilepath & inStream.ReadLine
    If gstFirstFile <> vbNullString Then
        Command1.Enabled = True
    End If
    
    gstSecondFile = stfilepath & inStream.ReadLine
    If gstSecondFile <> vbNullString Then
        Command2.Enabled = True
    End If
    gstThirdFile = stfilepath & inStream.ReadLine
    If gstThirdFile <> vbNullString Then
        Command3.Enabled = True
    End If
     gstFourthFile = stfilepath & inStream.ReadLine
    
    If gstFourthFile <> vbNullString Then
        Command4.Enabled = True
    End If
    
    
   
    gstFifthFile = stfilepath & inStream.ReadLine
      If gstFifthFile <> vbNullString Then
        Command5.Enabled = True
    End If
    gstSixthFile = stfilepath & inStream.ReadLine
      If gstSixthFile <> vbNullString Then
        Command6.Enabled = True
    End If
        
    gstSeventhFile = stfilepath & inStream.ReadLine
      If gstSeventhFile <> vbNullString Then
        Command7.Enabled = True
    End If
    gstEightthFile = stfilepath & inStream.ReadLine
      If gstEightthFile <> vbNullString Then
        Command8.Enabled = True
    End If
    
    
    ' close the file
    inStream.Close
    Set inStream = Nothing
    Set infile = Nothing
    Set objFSO = Nothing
    

    
End Function

Public Function ReadConfigTAFile(stfilename As String)
Dim objFSO As New FileSystemObject
Dim infile As File
Dim inStream As TextStream
Dim strTemp As String
Dim GetEXEPath As String

    GetEXEPath = stfilename
    
    Set infile = objFSO.GetFile(GetEXEPath)
    ' set up the file to be read
    Set inStream = infile.OpenAsTextStream(ForReading, TristateFalse)
    ' read in the global constants from the ini file
    gstTAFile = inStream.ReadLine
    If gstTAFile <> vbNullString Then
        cmdTA.Enabled = True
    End If
    
   
    
    
    
    ' close the file
    inStream.Close
    Set inStream = Nothing
    Set infile = Nothing
    Set objFSO = Nothing
    

    
End Function
Public Function ReadConfigPARFile(stfilename As String)
Dim objFSO As New FileSystemObject
Dim infile As File
Dim inStream As TextStream
Dim strTemp As String
Dim GetEXEPath As String

    GetEXEPath = stfilename
    
    Set infile = objFSO.GetFile(GetEXEPath)
    ' set up the file to be read
    Set inStream = infile.OpenAsTextStream(ForReading, TristateFalse)
    ' read in the global constants from the ini file
    gstPARFile = inStream.ReadLine
    If gstPARFile <> vbNullString Then
        cmdPAR.Enabled = True
    End If
    
   
    
    
    
    ' close the file
    inStream.Close
    Set inStream = Nothing
    Set infile = Nothing
    Set objFSO = Nothing
    

    
End Function


Public Function AddPoint(filename As String)
'This reads a text file based on the config file info and calls the dll
'and adds all the records in the txt comma delimited file.

Dim objFSO As New FileSystemObject
Dim inStream As TextStream
Dim strTemp As String
Dim GetEXEPath As String
Dim infile As File
Dim strRow As String
Dim strField As String
Dim intPos As Integer
Dim stPoint As String
Dim s As String, sItems() As String
Dim dlat As Double, dLon As Double
Dim stUserName As String
Dim stClassName As String
Dim stSubclassName As String
Dim stNotes As String, stAttrName1 As String, stAttrValue1 As String
Dim stAttrName2 As String, stAttrValue2 As String, stAttrName3 As String, stAttrValue3 As String
Dim stAttrName4 As String, stAttrValue4 As String, stAttrName5 As String, stAttrValue5 As String
Dim objEA As New KP_API.clsEarthAlert
Dim success As Boolean

On Error GoTo errHandler



    
's$ = "dog,cat,pig,boy,girl,man,woman"
'sItems() = Split(s$, ",")
'Print sItems(0)
    


    

    

    Set infile = objFSO.GetFile(filename)
    
    ' set up the file to be read
    Set inStream = infile.OpenAsTextStream(ForReading, TristateFalse)
    ' read in the global constants from the ini file
    
    Open filename For Input As #1
    Line Input #1, strRow 'Ignore the comment line
    Do Until EOF(1)
        
        Line Input #1, strRow
        sItems() = Split(strRow, ",")
        stUserName = sItems(0)
        stPoint = sItems(1)
        stClassName = sItems(2)
        stSubclassName = sItems(3)
        stNotes = sItems(4)
        stAttrName1 = sItems(5)
        stAttrValue1 = sItems(6)
        stAttrName2 = sItems(7)
        stAttrValue2 = sItems(8)
        stAttrName3 = sItems(9)
        stAttrValue3 = sItems(10)
        stAttrName4 = sItems(11)
        stAttrValue4 = sItems(12)
        stAttrName5 = sItems(13)
        stAttrValue5 = sItems(14)
        dlat = sItems(15)
        dLon = sItems(16)
        success = objEA.SetConditionalPointData(stUserName, stPoint, stClassName, stSubclassName, stNotes, _
                                           stAttrName1, stAttrValue1, stAttrName2, stAttrValue2, stAttrName3, stAttrValue3, _
                                           stAttrName4, stAttrValue4, stAttrName5, stAttrValue5, _
                                           CDbl(dlat), CDbl(dLon))
    Loop
    
    Close
        
    If success Then
        MsgBox "Added points successfully", vbOKOnly
    Else
        MsgBox "Could not add points successfully", vbOKOnly
        
    End If
    
    
    ' close the file
    inStream.Close
    Set inStream = Nothing
    Set infile = Nothing
    Set objFSO = Nothing
    
    Exit Function

errHandler:
    MsgBox "Error reading the file"
    Close
    Exit Function
End Function

Public Function AddTA(filename As String)
'This reads a text file based on the config file info and calls the dll
'and adds all the records in the txt comma delimited file.

Dim objFSO As New FileSystemObject
Dim inStream As TextStream
Dim strTemp As String
Dim GetEXEPath As String
Dim infile As File
Dim strRow As String
Dim strField As String
Dim intPos As Integer
Dim stPoint As String
Dim s As String, sItems() As String
Dim dlat As Double, dLon As Double
Dim stTACat As String, stType As String, stName As String
Dim stData As String, stImpactlevel As String, stLocType As String
Dim lModelCaseid As Long, lAngle As Long, lIsolRadius As Long, lDir As Long
Dim dDist As Double
Dim objEA As New KP_API.clsEarthAlert
Dim success As Boolean

On Error GoTo errHandler
    

    Set infile = objFSO.GetFile(filename)
    
    ' set up the file to be read
    Set inStream = infile.OpenAsTextStream(ForReading, TristateFalse)
    ' read in the global constants from the ini file
    
    Open filename For Input As #1
    Line Input #1, strRow 'Ignore the comment line
          
        Line Input #1, strRow
        sItems() = Split(strRow, "|")
        stTACat = sItems(0)
        stType = sItems(1)
        stName = sItems(2)
        stData = sItems(3)
        lModelCaseid = sItems(4)
        stImpactlevel = sItems(5)
        lAngle = sItems(6)
        dDist = sItems(7)
        lDir = sItems(8)
        dlat = sItems(9)
        dLon = sItems(10)
        stLocType = sItems(11)
        lIsolRadius = sItems(12)
       
        success = objEA.SaveTA(stTACat, stType, stName, stData, lModelCaseid, stImpactlevel, lAngle, dDist, lDir, dlat, dLon, stLocType, lIsolRadius)

   
    
    Close
        
    
    
    ' close the file
    inStream.Close
    Set inStream = Nothing
    Set infile = Nothing
    Set objFSO = Nothing
    MsgBox "Added a Threat Area Successfully", vbOKOnly
    
    
    Exit Function

errHandler:
    MsgBox "Error reading the file"
    Close
    Exit Function
End Function


Public Function AddPAR(filename As String)
'This reads a text file based on the config file info and calls the dll
'and adds all the records in the txt comma delimited file.

Dim objFSO As New FileSystemObject
Dim inStream As TextStream
Dim strTemp As String
Dim GetEXEPath As String
Dim infile As File
Dim strRow As String
Dim strField As String
Dim intPos As Integer
Dim stPoint As String
Dim s As String, sItems() As String
Dim objEA As New KP_API.clsEarthAlert
Dim success As Boolean
Dim intSuccess As Integer
Dim lObjectID As Long
Dim stPARName As String, stPAtype As String, stNotesName As String
Dim stModel As Boolean, stshared As Boolean
Dim stZonename As String, stZoneType As String
Dim intRecom As Integer, intTime1 As Integer, intTime2 As Integer, intTime3 As Integer, intTime4 As Integer
Dim stURL As String


On Error GoTo errHandler
    Set infile = objFSO.GetFile(filename)
    
    ' set up the file to be read
    Set inStream = infile.OpenAsTextStream(ForReading, TristateFalse)
    ' read in the global constants from the ini file
    
    Open filename For Input As #1
    Line Input #1, strRow 'Ignore the comment line
    Line Input #1, strRow
        sItems() = Split(strRow, ",")
        stPARName = sItems(0)
        stPAtype = sItems(1)
        stNotesName = sItems(2)
        stModel = sItems(3)
        stshared = sItems(4)
      ' mlObjectID = objEA.SavePAR(stParName, stParType, "www.arm.gov", False, True)
       lObjectID = objEA.SavePAR(stPARName, stPAtype, stNotesName, stModel, stshared)
       If lObjectID > 0 Then
         
            Line Input #1, strRow ' ignore comment
            Do Until EOF(1)
            Line Input #1, strRow
             sItems() = Split(strRow, ",")
            'intSuccess = objEA.SavePADetail(lObjectID, "R", "zone", 5, 120, 120, 120, 120)
            stZonename = sItems(0)
            stZoneType = sItems(1)
            intRecom = sItems(2)
            intTime1 = sItems(3)
            intTime2 = sItems(4)
            intTime3 = sItems(5)
            intTime4 = sItems(6)
            stURL = sItems(7)
            
            intSuccess = objEA.SavePADetail(lObjectID, stZonename, stZoneType, intRecom, intTime1, intTime2, intTime3, intTime4, stURL)
           

        Loop
        If intSuccess Then
                 MsgBox "PAR/PAD Successfully Added!!!", vbOKOnly
        Else
                MsgBox "Could not add times to points in PA unit table", vbOKOnly
        End If
        Else
               MsgBox "PAR/PAD Not a success!!!", vbOKOnly
        End If
        
    Close
        
    
    
    ' close the file
    inStream.Close
    Set inStream = Nothing
    Set infile = Nothing
    Set objFSO = Nothing
    
    Exit Function

errHandler:
    MsgBox "Error reading the file"
    Close
    Exit Function
End Function



Public Function DeletePoint(filename As String)
'This reads a text file based on the config file info and calls the dll
'and deletes all the records in the txt comma delimited file.

Dim objFSO As New FileSystemObject
Dim inStream As TextStream
Dim strTemp As String
Dim GetEXEPath As String
Dim infile As File
Dim strRow As String
Dim strField As String
Dim intPos As Integer
Dim stPoint As String
Dim s As String, sItems() As String
Dim dlat, dLon As Double
Dim stClassName As String
Dim objEA As New KP_API.clsEarthAlert
Dim success As Boolean

On Error GoTo errHandler

    Set infile = objFSO.GetFile(filename)
    
    ' set up the file to be read
    Set inStream = infile.OpenAsTextStream(ForReading, TristateFalse)
    ' read in the global constants from the ini file
    
    Open filename For Input As #1
    Line Input #1, strRow 'Ignore the comment line
    Do Until EOF(1)
        
        Line Input #1, strRow
        sItems() = Split(strRow, ",")
        
        stPoint = sItems(0)
        stClassName = sItems(1)
        
         success = objEA.DeletePoint(stPoint, stClassName)
        
    Loop
    
    Close
        
    If success Then
        MsgBox "Deleted point successfully", vbOKOnly
    Else
        MsgBox "Could not delete points", vbOKOnly
    End If
    
    
    ' close the file
    inStream.Close
    Set inStream = Nothing
    Set infile = Nothing
    Set objFSO = Nothing
    
    Exit Function

errHandler:
    MsgBox "Error reading the file"
    Close
    Exit Function
End Function




Private Sub cmdGetURL_Click()
 On Error Resume Next

    Call GetURL_CommonDialog(dlgFileName, txtFileName)
    Call ReadConfigFile(txtFileName.Text, txtFileName.Tag)
End Sub

'**-----------------------------------------------------------------**
'** Display common dialog per selection of a Web Address URL.       **
'**-----------------------------------------------------------------**
'
Public Sub GetURL_CommonDialog(dlgPtr As Control, _
                               urlPtr As TextBox)
    Dim stfilename  As String

    On Error Resume Next

    With dlgPtr
        .CancelError = True
        .Flags = cdlOFNHideReadOnly
        .ShowOpen
        If Err.Number = 0 Then
'''            stfilename = Trim$(.filename)
'''            If InStr(1, stfilename, " ") Then
'''                stfilename = """" & stfilename & """"
'''            End If
'''            urlPtr.Text = stfilename
            urlPtr.Text = .filename
            urlPtr.Tag = Left(.filename, InStr(.filename, .FileTitle) - 1)       ' filename without path
        End If
    End With

    Err.Clear
End Sub



Private Sub cmdGetURLPAR_Click()
On Error Resume Next

    Call GetURL_CommonDialog(dlgFileName, txtfilenamePAR)
    ReadConfigPARFile (txtfilenamePAR.Text)
End Sub

Private Sub cmdGetURLTA_Click()
On Error Resume Next

    Call GetURL_CommonDialog(dlgFileName, txtFilenameTA)
    ReadConfigTAFile (txtFilenameTA.Text)
End Sub

Private Sub cmdPAR_Click()
On Error Resume Next
    
If gstPARFile <> vbNullString Then
    AddPAR (gstPARFile)
End If
End Sub

Private Sub cmdTA_Click()
On Error Resume Next
    
If gstTAFile <> vbNullString Then
    AddTA (gstTAFile)
End If

End Sub

Private Sub Command1_Click()
On Error Resume Next
    
    If gstFirstFile <> vbNullString Then
    
        AddPoint (gstFirstFile)
    End If
    

   
End Sub


Private Sub Command2_Click()
If gstSecondFile <> vbNullString Then
    DeletePoint (gstSecondFile)
End If

End Sub


Private Sub Command3_Click()
On Error Resume Next
If gstThirdFile <> vbNullString Then
    
    AddPoint (gstThirdFile)
End If


End Sub


Private Sub Command4_Click()
On Error Resume Next
    
If gstFourthFile <> vbNullString Then
    DeletePoint (gstFourthFile)
End If

End Sub


Private Sub Command5_Click()
On Error Resume Next
    
If gstFifthFile <> vbNullString Then
    AddPoint (gstFifthFile)
End If


End Sub


Private Sub Command6_Click()
On Error Resume Next
    
If gstSixthFile <> vbNullString Then
    DeletePoint (gstSixthFile)
End If

End Sub


Private Sub Command7_Click()
On Error Resume Next
    
If gstSeventhFile <> vbNullString Then
    AddPoint (gstSeventhFile)
End If

End Sub

Private Sub Command8_Click()
On Error Resume Next
    
If gstEightthFile <> vbNullString Then
    DeletePoint (gstEightthFile)
End If

End Sub

