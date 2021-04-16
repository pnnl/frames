Attribute VB_Name = "SyncTypes"

Type srcMod
  lbl As String
  name As String
  type As String
  qual As String
End Type

Type srcParm
  lbl As String
  name As String
  idx As Long
  offsetVal As String
  offsetUnt As String
End Type

Global numsrc As Long
Global src() As srcMod
Global m_numsrc As Long
Global m_src() As srcParm

