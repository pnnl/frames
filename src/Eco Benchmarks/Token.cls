VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "Token"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Private mvartokstring As String
Private mvartokindex As Integer
Public Function GetLabels() As Collection
    Dim recstring As String
    Dim tempstring As String
    Dim tempchar As String
    Dim Index As Integer
    Dim tempcollection As New Collection
    Index = 1
    recstring = GetNextToken
    tempstring = ""
    While Index < Len(recstring) + 1
        tempchar = Mid(recstring, Index, 1)
        While (Not tempchar = ",") And (Index < Len(recstring) + 1)
            Index = Index + 1
            tempstring = tempstring + tempchar
            tempchar = Mid(recstring, Index, 1)
        Wend
        Index = Index + 1
        tempcollection.Add tempstring
        tempstring = ""
    Wend
    Set GetLabels = tempcollection
End Function
Public Function GetNextRecSet() As Collection
    Dim recstring As String
    Dim tempstring As String
    Dim tempchar As String
    Dim Index As Integer
    Dim tempcollection As New Collection
    
    Index = 2
    recstring = GetNextToken
    tempstring = ""
    tempchar = Mid(recstring, Index, 1)
    While Index < Len(recstring) + 1
        tempchar = Mid(recstring, Index, 1)
        While Not tempchar = Chr(34)
            Index = Index + 1
            tempstring = tempstring + tempchar
            tempchar = Mid(recstring, Index, 1)
        Wend
        Index = Index + 3
        tempcollection.Add tempstring
        tempstring = ""
    Wend
    Set GetNextRecSet = tempcollection
End Function
Public Function GetNextToken() As String
    Dim quoted As Boolean
    Dim tempchar As String
    quoted = False
    
    Do
        tempchar = Mid(tokstring, tokindex, 1)
        If tempchar = Chr(34) Then quoted = Not quoted
        If (tempchar = Chr(13) And quoted) Or tempchar <> Chr(13) Then
          If tempchar = Chr(13) Or tempchar = Chr(10) Then
            tempchar = " "
          End If
          GetNextToken = GetNextToken + tempchar
        End If
        tokindex = tokindex + 1
    Loop Until (tempchar = Chr(13) And Not quoted) Or (tokindex >= Len(tokstring))
    If tokindex < Len(tokstring) - 1 Then tokindex = tokindex + 1
End Function
Public Sub ResetToken()
    tokindex = 1
End Sub
Public Sub SetString(inputstring As String)
    tokstring = inputstring
    tokindex = 1
End Sub
Public Property Let tokindex(ByVal vData As Integer)
    mvartokindex = vData
End Property
Public Property Get tokindex() As Integer
    tokindex = mvartokindex
End Property
Public Property Let tokstring(ByVal vData As String)
    mvartokstring = vData
End Property
Public Property Get tokstring() As String
    tokstring = mvartokstring
End Property

