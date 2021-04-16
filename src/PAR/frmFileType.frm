VERSION 5.00
Begin VB.Form Form1 
   Caption         =   "Form1"
   ClientHeight    =   1656
   ClientLeft      =   48
   ClientTop       =   432
   ClientWidth     =   7728
   LinkTopic       =   "Form1"
   ScaleHeight     =   1656
   ScaleWidth      =   7728
   StartUpPosition =   3  'Windows Default
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Private Sub Form_Load()
  Dim lngCount As Long
  Dim strFileName As String

  StartModule 3
  
  strFileName = String(255, 0)
  lngCount = GetModuleFileName(App.hInstance, strFileName, 255)
  strFileName = Left(strFileName, lngCount)
  
  If UCase(Right(strFileName, 7)) = "VB6.EXE" Then
      strFileName = "C:\Program files\Framesv2"
      strFileName = Replace(strFileName, Chr$(34), "") ' remove quotes
      ChDir strFileName
      hLibModule = LoadLibrary(strFileName & "\systemio.dll")
  End If
  
  PID = ModuleDevOpen(SimPath, SimName, modid)
  If PID <= 0 Then
    MsgBox "Invalid PID: " & PID & " " & SimPath
    End
  End If
    
End Sub

Private Sub Form_Unload(cancel As Integer)
    On Error Resume Next
   
    Close
    If 0 <> hLibModule Then
      ModuleDevClose PID, 0
      FreeLibrary hLibModule
    Else
      ModuleDevClose PID, 0
    End If

End Sub

