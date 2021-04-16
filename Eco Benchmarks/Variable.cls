VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Variable"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Public name As String
Public label As String
Public min As String
Public max As String
Public datatype As String
Public unit As String
Public table As Boolean
Public filled As Boolean
Public keys As New Collection
Public indices As New Collection
Public values As New Collection
Public usrUnit As New Collection
Public defUnit As New Collection
Public siblings As New Collection
Public references As New Collection

Public Sub ClearCollections()
Dim i As Integer
    
  filled = False
  For i = values.count To 1 Step -1
    values.Remove (i)
  Next
  For i = references.count To 1 Step -1
    references.Remove (i)
  Next
  For i = usrUnit.count To 1 Step -1
    usrUnit.Remove (i)
  Next
  For i = defUnit.count To 1 Step -1
    defUnit.Remove (i)
  Next
  For i = keys.count To 1 Step -1
    keys.Remove (i)
  Next
End Sub
