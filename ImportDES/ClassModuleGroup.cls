VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "ClassModGroup"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit
Option Compare Text
'Basic module information from description file

Public ModuleType    As String          ' ex: Database, Model, Viewer, System, any others are considered to be model.
Public ModuleName    As String          ' ex: Constituent, Aquifer, Sensitivty/Uncertainty, etc
Public ModulePrefix  As String          ' ex: con, aqu, sen, etc
Public ModuleIcoPath As String
Public ModuleSel     As Boolean

Private Sub Class_Initialize()
  ModuleType = BLANK
  ModuleName = BLANK
  ModulePrefix = BLANK
  ModuleIcoPath = BLANK
  ModuleSel = False
End Sub
