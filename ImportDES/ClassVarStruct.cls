VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ClassDesDeclaration"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Option Compare Text

'Descripion file variable declaration
Public name As String
Public description As String
Public DataType As String
Public Scalar As Long
Public Unit As String
Public Minimum As Double
Public Maximum As Double
Public Stochastic As String

Public VarMinS As String
Public VarMaxS As String
Public VarNq   As Long
Public VarQue  As Collection 'ClassDesQue
Public indices As Collection 'string

Private Sub Class_Initialize()
    name = BLANK
    description = BLANK
    DataType = dtSTRING
    Stochastic = BLANK
    Scalar = TrueFlag
    Unit = BLANK
    Minimum = 0
    Maximum = 0
    VarNq = 0
    VarMinS = "0"
    VarMaxS = "0"
    Set VarQue = New Collection
    Set indices = New Collection
End Sub

Private Sub Class_Terminate()
    Set VarQue = Nothing
    Set indices = Nothing
End Sub
