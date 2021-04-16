Attribute VB_Name = "SOIL_PROP"
Option Explicit
Option Compare Text

' for frmSoilTri form
Global vfProcessUserInput As Boolean

Global Const ERR_RED = &H8080FF
Global Const INP_GRN = &HC0FFC0

Sub CenterForm(frm As Form)
  frm.Left = 0.5 * (Screen.Width - frm.Width)
  frm.Top = 0.5 * (Screen.Height - frm.Height)
End Sub

Function EstimateKd() As Boolean
  
  LoadSoilParams
  frmSoilTri.Show 1
  
End Function

