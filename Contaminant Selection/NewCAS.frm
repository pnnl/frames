VERSION 5.00
Begin VB.Form frmNewCAS 
   BorderStyle     =   3  'Fixed Dialog
   Caption         =   "New Constituent Identification"
   ClientHeight    =   2340
   ClientLeft      =   48
   ClientTop       =   432
   ClientWidth     =   8256
   ClipControls    =   0   'False
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   7.8
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2340
   ScaleWidth      =   8256
   StartUpPosition =   2  'CenterScreen
   Begin VB.CheckBox chkRadionuclide 
      Caption         =   "Radionuclide"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9.6
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   4080
      TabIndex        =   7
      TabStop         =   0   'False
      Top             =   480
      Width           =   1485
   End
   Begin VB.CommandButton cmdOK 
      Caption         =   "OK"
      Default         =   -1  'True
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9.6
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   350
      Left            =   5520
      TabIndex        =   2
      Top             =   1425
      Width           =   1120
   End
   Begin VB.CommandButton cmdCancel 
      Cancel          =   -1  'True
      Caption         =   "Cancel"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9.6
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   350
      Left            =   6840
      TabIndex        =   3
      Top             =   1425
      Width           =   1120
   End
   Begin VB.TextBox txtCasid 
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9.6
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   360
      Left            =   1785
      TabIndex        =   0
      Top             =   390
      Width           =   1830
   End
   Begin VB.TextBox txtName 
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9.6
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   360
      Left            =   1785
      TabIndex        =   1
      Top             =   870
      Width           =   6165
   End
   Begin VB.Label lblNote 
      Alignment       =   2  'Center
      Caption         =   "Note:  Copy is for properties only. The decay chain is not copied."
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9.6
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H000000C0&
      Height          =   276
      Left            =   0
      TabIndex        =   6
      Top             =   2016
      Width           =   8292
   End
   Begin VB.Label Label3 
      AutoSize        =   -1  'True
      Caption         =   "CAS ID"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9.6
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   240
      Left            =   300
      TabIndex        =   5
      Top             =   465
      Width           =   645
   End
   Begin VB.Label Label2 
      AutoSize        =   -1  'True
      Caption         =   "Common Name"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   9.6
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   240
      Left            =   300
      TabIndex        =   4
      Top             =   900
      Width           =   1410
   End
End
Attribute VB_Name = "frmNewCAS"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

Private Sub cmdCancel_Click()
  Unload Me
End Sub

Private Sub CopyRecord(rs As Variant, fromcas As String, tocas As String, cName As String)
Dim i As Long
Dim qry As String
Dim strfld As String
Dim strval As String
Dim var As Variant
  
  rs.Seek "=", fromcas
  If Not rs.NoMatch Then
    strfld = ""
    strval = ""
    For i = 0 To rs.Fields.count - 1
      var = rs.Fields(i).value
      If Not (IsEmpty(var) Or IsNull(var)) Then
        strfld = strfld & IIf(i = 0, "", ",") & rs.Fields(i).Name
        strval = strval & IIf(i = 0, "", ",")
        If rs.Fields(i).Name = CASID Then
          strval = strval & "'" & tocas & "'"
        ElseIf rs.Fields(i).Name = "Name" Then
          strval = strval & "'" & cName & "'"
        Else
          strval = strval & IIf(rs.Fields(i).Type = dbText, "'", "") & rs.Fields(i).value & IIf(rs.Fields(i).Type = dbText, "'", "")
        End If
      End If
    Next
    qry = "INSERT INTO " & rs.Name & " (" & strfld & ") VALUES (" & strval & ")"

On Error Resume Next
    DB.Execute qry
    If Err.Number <> 0 Then
      Debug.Print "ERROR: " & Error
    End If
  End If
End Sub

Private Sub cmdOK_Click()
Dim idCHEM As Long
Dim idKTYPE As Long
Dim idRTYPE As Long
Dim idPTYPE As Long
Dim idETYPE As Long
Dim var As Variant
  
  If Len(txtCasid) = 0 Or Len(txtName) = 0 Then Exit Sub

  tblChems.Index = "Primary"
  tblChems.Seek "=", txtCasid
  If Not tblChems.NoMatch Then
    MsgBox "Duplicate CAS Id", vbExclamation
    Exit Sub
  End If
  
'  tblChems.Index = "Name"
'  tblChems.Seek "=", txtName
'  If Not tblChems.NoMatch Then
'    MsgBox "Duplicate Name", vbExclamation
'    Exit Sub
'  End If
  
  If Me.tag = "copy" Then
    FindSynonyms frmCDBE.lstCon.itemdata(frmCDBE.lstCon.ListIndex), "", ""
    idKTYPE = tblChems!CLKTYPE
    idCHEM = tblChems!CLCHEM
    idPTYPE = tblChems!CLPTYPE
    idETYPE = tblChems!CLETYPE
    idRTYPE = tblChems!CLRTYPE
  Else
    idKTYPE = IIf(chkRadionuclide.value = 1, 1, 0)
    idPTYPE = 0          'undefined
    idCHEM = 0           'undefined
    If idKTYPE = 1 Then  'radionuclide
      idRTYPE = 14       'radionuclide
      idETYPE = 1        'radionclide exposure
    Else                 'chemical
      idRTYPE = 0        'undefined
      idETYPE = 5        'default exposure type
    End If
  End If
  
  tblChems.AddNew
  tblChems!CASID = txtCasid
  tblChems!Name = txtName
  tblChems!CLKTYPE = idKTYPE
  tblChems!CLCHEM = idCHEM
  tblChems!CLPTYPE = idPTYPE
  tblChems!CLETYPE = idETYPE
  tblChems!CLRTYPE = idRTYPE
  tblChems.update
  
  If Me.tag = "copy" Then
    For Each var In coldata
      CopyRecord var, frmCDBE.txtCasid.text, txtCasid.text, txtName.text
    Next
  End If
  
  tblChems.Index = "Primary"
  tblChems.Seek "=", txtCasid.text
  
  frmCDBE.lstAllName.AddItem txtName.text
  frmCDBE.lstAllName.itemdata(frmCDBE.lstAllName.NewIndex) = tblChems!rowid
  frmCDBE.lstAllCasid.AddItem txtCasid.text
  frmCDBE.lstAllCasid.itemdata(frmCDBE.lstAllCasid.NewIndex) = tblChems!rowid
  
  If frmCDBE.chkShowCasid Then
    frmCDBE.lstCon.AddItem txtCasid.text
  Else
    frmCDBE.lstCon.AddItem txtName.text
  End If
  frmCDBE.lstCon.itemdata(frmCDBE.lstCon.NewIndex) = tblChems!rowid
  frmCDBE.lstCon.ListIndex = frmCDBE.lstCon.NewIndex
  
  Unload Me
End Sub

Private Sub Form_Activate()
  If Me.tag = "new" Then
    Me.Caption = "New Constituent"
    lblNote.Visible = False
  End If
  If Me.tag = "copy" Then
    FindSynonyms frmCDBE.lstCon.itemdata(frmCDBE.lstCon.ListIndex), "", ""
    Me.Caption = "Copy Constituent " & tblChems!Name & " (" & tblChems!CASID & ")"
    lblNote.Visible = True
  End If
End Sub
