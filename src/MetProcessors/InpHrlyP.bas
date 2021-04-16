Attribute VB_Name = "Module1"
Option Explicit

Declare Function GetModuleUsage% Lib "Kernel" (ByVal hModule%)


Sub GetNamePathExt(inpfile As String, Name As String, Path As String, Ext As String)
    'Determine the file name, path, and extension
    
    Dim teststring As String, namestring As String
    Dim lastpos As Integer, fextpos As Integer, lextpos As Integer
    Dim fnamepos As Integer, lnamepos As Integer

    If inpfile = "" Then
        Name = ""
        Path = ""
        Ext = ""
    Else
        'Get Extension
        teststring = LTrim$(RTrim$(inpfile))
        lastpos = Len(teststring)
        lextpos = 0
        fextpos = InStr(teststring, ".")
        Do While fextpos <> 0
            lextpos = fextpos
            fextpos = InStr(lextpos + 1, teststring, ".")
        Loop
        'MsgBox "Test String = " + teststring + ", LastPos = " + Str(lastpos)
        'MsgBox "lExtPos = " + Str(lextpos)
        
        If lextpos <= 0 Then
            Ext = ""
            namestring = teststring
        Else
            namestring = Mid(teststring, 1, (lextpos - 1))
            Ext = Mid(teststring, lextpos + 1)
        End If
        
        'MsgBox "Namestring = " + namestring
        'MsgBox "Ext = " + Ext
        'Get Name
        lastpos = Len(namestring)
        lnamepos = 0
        fnamepos = InStr(namestring, "\")
        Do While fnamepos <> 0
            lnamepos = fnamepos
            fnamepos = InStr(lnamepos + 1, namestring, "\")
        Loop
        
        If lnamepos <= 0 Then
            fnamepos = InStr(namestring, ":")
            If fnamepos <= 0 Then
                Name = namestring
                Path = 0
            Else
                Path = Mid(namestring, 1, (fnamepos))
                Name = Mid(namestring, fnamepos + 1)
            End If
        Else
            Path = Mid(namestring, 1, (lnamepos))
            Name = Mid(namestring, lnamepos + 1)
        End If
    End If
    'MsgBox "Path = " + Path + ", Name = " + Name + ", Ext = " + Ext
End Sub
Sub WRITEERR(infile As String)
    'Write Error Message from Processing Program
    
    Dim l As String, ln As String
    Dim nn As Integer

    nn = FreeFile
    Open infile For Input As nn
        Do While Not EOF(nn)
            Line Input #nn, ln
            l = l + ln + Chr$(13) + Chr$(10)
        Loop

    Close nn
            
    Call StatusTitle("")
    Call ShowStatus(l)
End Sub
Sub ShowStatus(s As String)
    'Write string to Status Form
        
    Dim de As Integer
    'shows text in the status window
    On Local Error Resume Next
    If StatusForm.Visible = 0 Then
        StatusForm.Show
    End If
    If StatusForm.WindowState = 1 Then
        StatusForm.WindowState = 0
    End If
    'MsgBox "s=" + s$
    StatusForm.StatusText.Text = ""
    StatusForm.StatusText.Text = s$
    StatusForm.Refresh
    'StatusForm.StatusText.SelStart = Len(s$)
    StatusForm.SetFocus
    de = DoEvents()
End Sub

Sub StatusTitle(t As String)
    'Write Title for Status Form
    
    If t = "" Then
        StatusForm.Caption = "ERROR Message"
    Else
        StatusForm.Caption = "ERROR Message " + t$
    End If
End Sub

