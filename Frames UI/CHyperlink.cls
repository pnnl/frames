VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "CHyperlink"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit

'API used to open the hyperlink
Private Declare Function ShellExecute _
    Lib "shell32.dll" Alias "ShellExecuteA" ( _
    ByVal hWnd As Long, _
    ByVal lpOperation As String, _
    ByVal lpFile As String, _
    ByVal lpParameters As String, _
    ByVal lpDirectory As String, _
    ByVal nShowCmd As Long) As Long

'local variable(s) to hold property value(s)
Private mstrLinkText As String 'local copy
Private mstrLinkURL As String 'local copy

Public Property Let LinkURL(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.LinkURL = 5
  mstrLinkURL = vData
End Property

Public Property Get LinkURL() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.LinkURL
  LinkURL = mstrLinkURL
End Property

Public Property Let LinkText(ByVal vData As String)
'used when assigning a value to the property, on the left side of an assignment.
'Syntax: X.LinkText = 5
  mstrLinkText = vData
End Property

Public Property Get LinkText() As String
'used when retrieving value of a property, on the right side of an assignment.
'Syntax: Debug.Print X.LinkText
  LinkText = mstrLinkText
End Property

Public Sub OpenLink()
  ShellExecute 0&, "Open", mstrLinkURL, "", "", vbNormalFocus
End Sub

