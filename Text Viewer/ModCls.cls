VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ModCls"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Option Compare Text

Private mvarName As String
Private mvarsiteIdx As Long
Private mvarmodIdx As Long
Private mvarId As String
Private mvarLabel As String
Private mvarDesPath As String

Private Property Let DesPath(ByVal vData As String)
  mvarDesPath = vData
End Property

Public Property Get DesPath() As String
  DesPath = mvarDesPath
End Property

Public Property Let label(ByVal vData As String)
  mvarLabel = vData
End Property

Public Property Get label() As String
  label = mvarLabel
End Property

Public Property Let Prefix(ByVal vData As String)
  mvarId = vData
End Property

Public Property Get Prefix() As String
  Prefix = mvarId
End Property

Public Property Let Key(vData As String)
  mvarId = vData
End Property

Public Property Get Key() As String
  Key = mvarId
End Property

Public Property Let modIdx(ByVal vData As Long)
  mvarmodIdx = vData
End Property

Public Property Get modIdx() As Long
  modIdx = mvarmodIdx
End Property

Public Property Let siteIdx(ByVal vData As Long)
  mvarsiteIdx = vData
End Property

Public Property Get siteIdx() As Long
  siteIdx = mvarsiteIdx
End Property

Public Property Let mname(ByVal vData As String)
  mvarName = vData
End Property

Public Property Get mname() As String
  mname = mvarName
End Property

Public Function DisplayFileContents(ByVal fin As String, ByVal fout As String, _
                                    rtb As RichTextBox, vfui As FuiCls, prtref As Boolean) As Boolean
Dim vgid As GidCls, vars As Collection, found As Boolean, vdes As DesCls
  Set vgid = vfui.GetData(Prefix)
' vgid.LoadGIDData siteIdx, modname, fin
  Set vdes = vfui.GetDes(DesPath)
  Set vars = vfui.GetDesVariables(DesPath)
  vgid.FormatGIDData fout, vars, vfui, prtref

'     sdes.LoadGID SiteIndex, source, fin
'     done = sdes.FormatGID(source, fin, fout)
  DisplayFileContents = True
End Function

Public Sub DisplayModuleDescription(rtb As RichTextBox, vfui As FuiCls)
Dim vdes As DesCls
Dim found As Boolean

  Set vdes = vfui.GetDes(DesPath)
  vdes.DisplayModuleDescription rtb, vfui
End Sub

Public Sub Init(vfui As FuiCls)
Dim sid As String
Dim slabel As String
Dim stype As String
Dim sdes As String

  vfui.GetSource sid, slabel, stype, sdes
  Prefix = sid
  label = slabel
  DesPath = sdes
End Sub
