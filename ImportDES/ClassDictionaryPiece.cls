VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ClassDicDeclaration"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

'Dictionary file variable declaration
Public Name As String
Public Description As String
Public DataType As String
Public Scalar As Long
Public Measure As String
Public Unit As String
Public Minimum As Double
Public Maximum As Double
Public Stochastic As Long
Public Preposition As String

Public PrimaryKey As Long
Public Indices As Collection 'String

Private Sub Class_Initialize()
  Name = BLANK
  Description = BLANK
  DataType = dtSTRING
  PrimaryKey = FalseFlag
  Scalar = TrueFlag
  Minimum = 0
  Maximum = 0
  Measure = BLANK
  Unit = BLANK
  Stochastic = FalseFlag
  Preposition = BLANK
  Set Indices = New Collection
End Sub

Private Sub Class_Terminate()
  Set Indices = Nothing
End Sub

