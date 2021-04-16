Attribute VB_Name = "rtfPrint"
 Option Explicit

   Private Type Rect
      Left As Long
      Top As Long
      Right As Long
      Bottom As Long
   End Type

   Private Type CharRange
     cpMin As Long     ' First character of range (0 for start of doc)
     cpMax As Long     ' Last character of range (-1 for end of doc)
   End Type

   Private Type FormatRange
     hdc As Long       ' Actual DC to draw on
     hdcTarget As Long ' Target DC for determining Text formatting
     rc As Rect        ' Region of the DC to draw to (in twips)
     rcPage As Rect    ' Region of the entire DC (page size) (in twips)
     chrg As CharRange ' Range of Text to draw (see above declaration)
   End Type

   Public Const WM_PASTE As Long = &H302
   Private Const WM_USER As Long = &H400
   Private Const EM_FORMATRANGE As Long = WM_USER + 57
   Private Const EM_SETTARGETDEVICE As Long = WM_USER + 72
   Private Const PHYSICALOFFSETX As Long = 112
   Private Const PHYSICALOFFSETY As Long = 113
   
   Private Declare Function GetDeviceCaps Lib "gdi32" ( _
      ByVal hdc As Long, ByVal nIndex As Long) As Long
   Private Declare Function CreateDC Lib "gdi32" Alias "CreateDCA" _
      (ByVal lpDriverName As String, ByVal lpDeviceName As String, _
      ByVal lpOutput As Long, ByVal lpInitData As Long) As Long
    
   ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
   '
   ' WYSIWYG_RTF - Sets an RTF control to display itself the same as it
   '               would print on the default printer
   '
   ' RTF - A RichTextBox control to set for WYSIWYG display.
   '
   ' LeftMarginWidth - Width of desired left margin in twips
   '
   ' RightMarginWidth - Width of desired right margin in twips
   '
   ' Returns - The length of a line on the printer in twips
   '''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
   Public Function WYSIWYG_RTF(RTF As RichTextBox, LeftMarginWidth As Long, _
      RightMarginWidth As Long) As Long
      Dim LeftOffset As Long, LeftMargin As Long, RightMargin As Long
      Dim LineWidth As Long
      Dim PrinterhDC As Long
      Dim r As Long

      ' Start a print job to initialize printer object
      Printer.Print Space(1)
      Printer.ScaleMode = vbTwips

      ' Get the offset to the printable area on the page in twips
      LeftOffset = Printer.ScaleX(GetDeviceCaps(Printer.hdc, _
         PHYSICALOFFSETX), vbPixels, vbTwips)

      ' Calculate the Left, and Right margins
      LeftMargin = LeftMarginWidth - LeftOffset
      RightMargin = (Printer.Width - RightMarginWidth) - LeftOffset

      ' Calculate the line width
      LineWidth = RightMargin - LeftMargin

      ' Create an hDC on the Printer pointed to by the Printer object
      ' This DC needs to remain for the RTF to keep up the WYSIWYG display
      PrinterhDC = CreateDC(Printer.DriverName, Printer.DeviceName, 0, 0)

      ' Tell the RTF to base it's display off of the printer
      '    at the desired line width
      r = SendMessage(RTF.hWnd, EM_SETTARGETDEVICE, PrinterhDC, _
         ByVal LineWidth)

      ' Abort the temporary print job used to get printer info
      Printer.KillDoc

      WYSIWYG_RTF = LineWidth
   End Function

   ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
   '
   ' PrintRTF - Prints the contents of a RichTextBox control using the
   '            provided margins
   '
   ' RTF - A RichTextBox control to print
   '
   ' LeftMarginWidth - Width of desired left margin in twips
   '
   ' TopMarginHeight - Height of desired top margin in twips
   '
   ' RightMarginWidth - Width of desired right margin in twips
   '
   ' BottomMarginHeight - Height of desired bottom margin in twips
   '
   ' Notes - If you are also using WYSIWYG_RTF() on the provided RTF
   '         parameter you should specify the same LeftMarginWidth and
   '         RightMarginWidth that you used to call WYSIWYG_RTF()
   ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''
   Public Sub PrintRTF(RTF As RichTextBox, LeftMarginWidth As Long, _
      TopMarginHeight, RightMarginWidth, BottomMarginHeight)
      Dim LeftOffset As Long, TopOffset As Long
      Dim LeftMargin As Long, TopMargin As Long
      Dim RightMargin As Long, BottomMargin As Long
      Dim fr As FormatRange
      Dim rcDrawTo As Rect
      Dim rcPage As Rect
      Dim TextLength As Long
      Dim NextCharPosition As Long
      Dim r As Long, cpMin As Long, cpMax As Long
      
      Dim RTFtmp As RichTextBox
      

      ' Start a print job to get a valid Printer.hDC
      Printer.Print Space(1)
      Printer.ScaleMode = vbTwips

      ' Get the offsett to the printable area on the page in twips
      LeftOffset = Printer.ScaleX(GetDeviceCaps(Printer.hdc, _
         PHYSICALOFFSETX), vbPixels, vbTwips)
      TopOffset = Printer.ScaleY(GetDeviceCaps(Printer.hdc, _
         PHYSICALOFFSETY), vbPixels, vbTwips)

      ' Calculate the Left, Top, Right, and Bottom margins
      LeftMargin = LeftMarginWidth - LeftOffset
      TopMargin = TopMarginHeight - TopOffset
      RightMargin = (Printer.Width - RightMarginWidth) - LeftOffset
      BottomMargin = (Printer.Height - BottomMarginHeight) - TopOffset

      ' Set printable area rect
      rcPage.Left = 0
      rcPage.Top = 0
      rcPage.Right = Printer.ScaleWidth
      rcPage.Bottom = Printer.ScaleHeight

      ' Set rect in which to print (relative to printable area)
      rcDrawTo.Left = LeftMargin
      rcDrawTo.Top = TopMargin
      rcDrawTo.Right = RightMargin
      rcDrawTo.Bottom = BottomMargin

      ' Set up the print instructions
      fr.hdc = Printer.hdc   ' Use the same DC for measuring and rendering
      fr.hdcTarget = Printer.hdc  ' Point at printer hDC
      fr.rc = rcDrawTo            ' Indicate the area on page to draw to
      fr.rcPage = rcPage          ' Indicate entire size of page
      fr.chrg.cpMin = 0           ' Indicate start of Text through
      fr.chrg.cpMax = -1          ' end of the Text

      ' Get length of Text in RTF
      cpMin = 0
      cpMax = -1
      TextLength = Len(RTF.Text)
      If RTF.SelLength > 0 Then
        cpMin = RTF.SelStart
        cpMax = RTF.SelStart + RTF.SelLength
        TextLength = cpMax ' fr.chrg.cpMax
      End If
      
      ' Loop printing each page until done
      Do
         ' Print the page by sending EM_FORMATRANGE message
         fr.chrg.cpMin = cpMin
         fr.chrg.cpMax = cpMax ' TextLength ' cpMax
         fr.hdc = Printer.hdc
         fr.hdcTarget = Printer.hdc
         NextCharPosition = SendMessage(RTF.hWnd, EM_FORMATRANGE, True, fr)
         If NextCharPosition >= TextLength Then Exit Do  'If done then exit
         cpMin = NextCharPosition
         
'        MsgBox "new page", vbOK
         Printer.NewPage                  ' Move on to next page
         
'        MsgBox "print space", vbOK
         Printer.Print Space(1) ' Re-initialize hDC
         fr.hdc = Printer.hdc
         fr.hdcTarget = Printer.hdc
      Loop

      ' Commit the print job
'     MsgBox "enddoc", vbOK
      Printer.EndDoc

'     MsgBox "send message close", vbOK
      ' Allow the RTF to free up memory
      r = SendMessage(RTF.hWnd, EM_FORMATRANGE, False, ByVal CLng(0))
   End Sub

