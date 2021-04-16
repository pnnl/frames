VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "kp_utility"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
' Event declarations
Public Event CustomEvent(ByVal EventCode As Long, ByVal Arg1 As Variant, ByVal Arg2 As Variant, ByVal Arg3 As Variant, ByVal Arg4 As Variant, ByVal Arg5 As Variant, ByRef ReturnValue As Variant)



' Raise a custom event, with optional data arguments and return value.  Useful for extending the functionality of this control.
Public Function RaiseCustomEvent(ByVal EventCode As Long, Optional ByVal Arg1 As Variant, Optional ByVal Arg2 As Variant, Optional ByVal Arg3 As Variant, Optional ByVal Arg4 As Variant, Optional ByVal Arg5 As Variant) As Variant
    Dim vResult As Variant

    ' Raise a custom event
    RaiseEvent CustomEvent(EventCode, Arg1, Arg2, Arg3, Arg4, Arg5, vResult)

    ' Return the result
    If IsObject(vResult) Then
        Set RaiseCustomEvent = vResult
    Else
        RaiseCustomEvent = vResult
    End If
End Function


' Reserved function for testing and undocumented functions
Public Function ZF01(ByVal FuncCode As Long, Optional ByVal Arg1 As Variant, Optional ByVal Arg2 As Variant, Optional ByVal Arg3 As Variant, Optional ByVal Arg4 As Variant, Optional ByVal Arg5 As Variant) As Variant
Attribute ZF01.VB_MemberFlags = "40"
    Dim vResult As Variant

    ' Process the function request
    Select Case FuncCode
        Case 0:
            '=================================================================
            '   Dummy function
            '=================================================================
            vResult = 0

        Case Else:
    End Select

    ' Return the result
    ZF01 = vResult
End Function
