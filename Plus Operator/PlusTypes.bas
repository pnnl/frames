Attribute VB_Name = "PlusTypes"

Type srcMod
  lbl As String
  name As String
  type As String
  qual As String
End Type

Type srcParm
  lbl As String
  name As String
  Idx As Long
  fluxVal As String
  fluxUnt As String
End Type

Global numsrc As Long
Global src() As srcMod
Global m_numsrc As Long
Global m_src() As srcParm

