VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "CHyperlinks"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Ext_KEY = "SavedWithClassBuilder6" ,"Yes"
Attribute VB_Ext_KEY = "Collection" ,"CHyperlink"
Attribute VB_Ext_KEY = "Member0" ,"CHyperlink"
Attribute VB_Ext_KEY = "Top_Level" ,"Yes"
Option Explicit
Option Compare Text

'This const value is set to the maximum length allowed of
'one line in the RTF box.
'No checks are performed though so the code
'in the RichTextBox_Click event might fail if the line of
'Text is longer then this value!!!!
Private Const MAXLINELENGTH = 512

'reference a RTF box control
Public WithEvents RichTextBox As RichTextBox
Attribute RichTextBox.VB_VarHelpID = -1
'local variable to that know how to launch itself
Private thlnLink As CHyperlink
'local variable to hold collection
Private mCol As Collection

Public Function Add(LinkText As String, LinkURL As String, pos As Long) As CHyperlink
Dim iLoop As Integer
Dim lngOldColor As Long
Dim blnUnderline As Boolean
Dim hlnLink As CHyperlink
Dim objNewMember As CHyperlink

  'Loop through the LinkText and change
  'all spaces into chr(160).
  'Chr(160) looks like a space but it will
  'not wrap. ALWAYS keep the hyperlink Text on
  'one line in the RTF box.
  For iLoop = 1 To Len(LinkText)
    If Mid$(LinkText, iLoop, 1) = " " Then
      Mid$(LinkText, iLoop) = Chr$(160)
    End If
  Next
  
  'create a new object
  Set objNewMember = New CHyperlink
  'set the properties passed into the method
  objNewMember.LinkText = LinkText
  objNewMember.LinkURL = LinkURL
  mCol.Add objNewMember
  'Change the selected Text off the RichTextBox
  With RichTextBox
    'Save the selected colour and underline state
    lngOldColor = .SelColor
    blnUnderline = .SelUnderline
    'Set the string colour and underline state
    .SelStart = InStr(pos, .Text, LinkText) - 1
    .SelLength = Len(LinkText)
    .SelColor = vbBlue
    .SelUnderline = True
    'Restore the colour and underline state
    .SelLength = 0
    .SelColor = lngOldColor
    .SelUnderline = blnUnderline
  End With
  'return the object created
  Set Add = objNewMember
  Set objNewMember = Nothing
  End Function
  
  Public Property Get item(vntIndexKey As Variant) As CHyperlink
  'used when referencing an element in the collection
  'vntIndexKey contains either the Index or Key to the collection,
  'this is why it is declared as a Variant
  'Syntax: Set foo = x.Item(xyz) or Set foo = x.Item(5)
  Set item = mCol(vntIndexKey)
End Property

Public Property Get Count() As Long
'used when retrieving the number of elements in the
'collection. Syntax: Debug.Print x.Count
  Count = mCol.Count
End Property

Public Sub Clear()
  Set mCol = New Collection
End Sub

Public Sub Remove(vntIndexKey As Variant)
'used when removing an element from the collection
'vntIndexKey contains either the Index or Key, which is why
'it is declared as a Variant
'Syntax: x.Remove(xyz)
  mCol.Remove vntIndexKey
End Sub

Public Property Get NewEnum() As IUnknown
'this property allows you to enumerate
'this collection with the For...Each syntax
  Set NewEnum = mCol.[_NewEnum]
End Property

Private Sub Class_Initialize()
'creates the collection when this class is created
  Set mCol = New Collection
End Sub

Private Sub Class_Terminate()
'destroys collection when this class is terminated
  Set mCol = Nothing
End Sub

Private Sub RichTextBox_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
Dim strline As String
Dim lngLine As Long
Dim lngLineIndex As Long
Dim lngSelStart As Long
Dim lngHWnd As Long
Dim barrLine(MAXLINELENGTH - 1) As Byte
Dim hlnLink As CHyperlink
Dim i As Integer
Dim pt As Point
Dim pos As Long
  
  Set thlnLink = Nothing
  pt.x = CLng(x / Screen.TwipsPerPixelX)
  pt.y = CLng(y / Screen.TwipsPerPixelY)
  lngHWnd = RichTextBox.hWnd
  'get the character index
  lngSelStart = SendMessage(lngHWnd, EM_CHARFROMPOS, 0&, pt)
  'Get the line number from the caret position
  lngLine = SendMessage(lngHWnd, EM_LINEFROMCHAR, lngSelStart, 0&)
  'get the poition of the first character on that line
  lngLineIndex = SendMessage(lngHWnd, EM_LINEINDEX, lngLine, 0&)
  'save the MAXLINELENGTH in the first word of
  'the byte array
  barrLine(0) = MAXLINELENGTH And 255
  barrLine(1) = MAXLINELENGTH \ 256
  'get the line and save it in the byte array
  SendMessage lngHWnd, EM_GETLINE, lngLine, barrLine(0)
  strline = barrLine
  'calculate the caret position on the "active" line
  i = lngSelStart - lngLineIndex
  'get the word
  Do While i > 0
   If (barrLine(i) = 32 Or barrLine(i) = 8 Or barrLine(i) = 10 Or barrLine(i) = 13) Then
     i = i + 1
     Exit Do
   End If
   i = i - 1
  Loop
  
  strline = ""
  For i = i To MAXLINELENGTH - 1
    If barrLine(i) = 0 Or barrLine(i) = 32 Or barrLine(i) = 8 Or barrLine(i) = 10 Or barrLine(i) = 13 Then Exit For
    strline = strline & Chr$(barrLine(i))
  Next
  
  'Loop through the collection and search for a hyperlink
  For Each hlnLink In mCol
    pos = InStr(hlnLink.LinkText, strline)
    ' have to adjust for line wrap otherwise just a string compare would have worked
    If (pos = 1 And strline <> "") Or (pos > 0 And pos = Len(hlnLink.LinkText) - Len(strline) + 1) Then
      Set thlnLink = hlnLink
      Exit For
    End If
  Next
  If thlnLink Is Nothing Then
    RichTextBox.MousePointer = rtfDefault
  Else
    RichTextBox.MousePointer = rtfArrow
  End If
End Sub

Private Sub RichTextBox_Click()
  If Not thlnLink Is Nothing Then thlnLink.OpenLink
End Sub
