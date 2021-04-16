Attribute VB_Name = "Interface"
Option Explicit
Option Compare Text

' FontValues
Public FontName As String
Public FontSize As Double
Public FontBold As Boolean
Public FontItalic As Boolean
Public LogoFile As String

' Window bars values
Public rbar As Long                'vert right edge x
Public lbar As Long                'vert left edge x
Public tbar(1) As Long             'horz top edge y     0=left 1=right
Public bbar(1) As Long             'horz bottom edge y  0=left 1=right
Public barwidth As Long            'border width
Public hbar As Long                'horz bar selection

Public dx As Single                'picture.ScaleWidth / 1000#   (0< glyph.sx <1000)
Public dy As Single                'picture.ScaleHeight / 1000#  (0< glyph.sy <1000)

' Color values
Public dbcolor As Long
Public dfcolor As Long
Public dvis As Long
Public mbcolor As Long
Public mfcolor As Long
Public mvis As Long
Public sbcolor As Long
Public sfcolor As Long
Public svis As Long
Public wbcolor As Long
Public wfcolor As Long
Public wvis As Long

Public frm As Form
Public logo As Image
Public tree As TreeView
Public pic As PictureBox
Public icon As PictureBox
Public images As ImageList
Public status As StatusBar
Public rtb As RichTextBox
Public rtb2 As RichTextBox

Sub GetFont()
Dim tmp As String
  
  cdl.FontName = "MS Sans Serif"
  cdl.FontSize = 8.25
  cdl.FontBold = True
  cdl.FontItalic = False
  
  If ReadIniString("Font", "Name", BLANK, tmp) Then cdl.FontName = tmp
  If ReadIniString("Font", "Size", BLANK, tmp) Then cdl.FontSize = Val(tmp)
  If ReadIniString("Font", "Bold", BLANK, tmp) Then cdl.FontBold = CBool(tmp)
  If ReadIniString("Font", "Italic", BLANK, tmp) Then cdl.FontItalic = CBool(tmp)
End Sub

Sub SetFont()
  tree.Font.Name = cdl.FontName
  tree.Font.Size = cdl.FontSize
  tree.Font.Bold = cdl.FontBold
  tree.Font.Italic = cdl.FontItalic
  pic.Font.Name = cdl.FontName
  pic.Font.Size = cdl.FontSize
  pic.Font.Bold = cdl.FontBold
  pic.Font.Italic = cdl.FontItalic
  status.Font.Name = cdl.FontName
  status.Font.Size = cdl.FontSize
  status.Font.Bold = cdl.FontBold
  status.Font.Italic = cdl.FontItalic
  WriteIniString "Font", "Name", cdl.FontName
  WriteIniString "Font", "Size", cdl.FontSize
  WriteIniString "Font", "Bold", cdl.FontBold
  WriteIniString "Font", "Italic", cdl.FontItalic
End Sub

Sub GetColors()
  ReadIniLong "Colors", "DataBack", vbCyan, dbcolor
  ReadIniLong "Colors", "DataFore", vbCyan, dfcolor
  ReadIniLong "Colors", "DataVis", 1, dvis
  ReadIniLong "Colors", "ModelBack", vbWhite, mbcolor
  ReadIniLong "Colors", "ModelFore", vbWhite, mfcolor
  ReadIniLong "Colors", "ModelVis", 1, mvis
  ReadIniLong "Colors", "SysBack", vbRed, sbcolor
  ReadIniLong "Colors", "SysFore", vbRed, sfcolor
  ReadIniLong "Colors", "SysVis", 1, svis
  ReadIniLong "Colors", "WorkBack", vbBlack, wbcolor
  ReadIniLong "Colors", "WorkFore", vbYellow, wfcolor
  ReadIniLong "Colors", "WorkVis", 1, wvis
End Sub

Sub SetColors()
  WriteIniLong "Colors", "DataBack", dbcolor
  WriteIniLong "Colors", "DataFore", dfcolor
  WriteIniLong "Colors", "DataVis", dvis
  WriteIniLong "Colors", "ModelBack", mbcolor
  WriteIniLong "Colors", "ModelFore", mfcolor
  WriteIniLong "Colors", "ModelVis", mvis
  WriteIniLong "Colors", "SysBack", sbcolor
  WriteIniLong "Colors", "SysFore", sfcolor
  WriteIniLong "Colors", "SysVis", svis
  WriteIniLong "Colors", "WorkBack", wbcolor
  WriteIniLong "Colors", "WorkFore", wfcolor
  WriteIniLong "Colors", "WorkVis", wvis
End Sub

Sub GetBarPos()
Dim tmp As Long

  ReadIniLong "Bar Pos", "barwidth", 50, barwidth
  ReadIniLong "Bar Pos", "LBar", 3000, lbar
  ReadIniLong "Bar Pos", "RBar", 3050, rbar
  ReadIniLong "Window Pos", "Height", Screen.Height * 0.8, tmp
  ReadIniLong "Bar Pos", "LTBar", tmp * 0.2, tbar(0)
  ReadIniLong "Bar Pos", "LBBar", tmp * 0.2 + barwidth, bbar(0)
  ReadIniLong "Bar Pos", "RTBar", tmp * 0.8, tbar(1)
  ReadIniLong "Bar Pos", "RBBar", tmp * 0.8 + barwidth, bbar(1)
End Sub

Sub SetBarPos()
  WriteIniLong "Bar Pos", "barwidth", barwidth
  WriteIniLong "Bar Pos", "LBar", lbar
  WriteIniLong "Bar Pos", "RBar", rbar
  WriteIniLong "Bar Pos", "LTBar", tbar(0)
  WriteIniLong "Bar Pos", "LBBar", bbar(0)
  WriteIniLong "Bar Pos", "RTBar", tbar(1)
  WriteIniLong "Bar Pos", "RBBar", bbar(1)
End Sub

Sub GetWinPos()
Dim tmp As Long
  
  ReadIniLong "Window Pos", "X", Screen.Width * 0.1, tmp
  frm.Left = tmp
  ReadIniLong "Window Pos", "Y", Screen.Height * 0.1, tmp
  frm.Top = tmp
  ReadIniLong "Window Pos", "Width", Screen.Width * 0.8, tmp
  frm.Width = tmp
  ReadIniLong "Window Pos", "Height", Screen.Height * 0.8, tmp
  frm.Height = tmp
End Sub

Sub SetWinPos()
  frm.WindowState = vbNormal
  If frm.Top < 0 Then frm.Top = 0
  If frm.Left < 0 Then frm.Left = 0
  If frm.Width > Screen.Width Then
   frm.Width = Screen.Width
   frm.Left = 0
  End If
  If frm.Height > Screen.Height Then
    frm.Height = Screen.Height
    frm.Top = 0
  End If
  
  WriteIniLong "Window Pos", "X", frm.Left
  WriteIniLong "Window Pos", "Y", frm.Top
  WriteIniLong "Window Pos", "Width", frm.Width
  WriteIniLong "Window Pos", "Height", frm.Height
End Sub

Sub GetOptions(afrm As Form)
Dim i As Long
Dim tmp As String
  
  Set frm = afrm
  Set cdl = frm.cdlAll
  Set pic = frm.picSite
  Set icon = frm.picIcon
  Set tree = frm.TreeView1
  Set logo = frm.Image1
  Set images = frm.ImageList1
  Set status = frm.StatusBar1
  Set rtb = frm.RichTextBox1
  Set rtb2 = frm.RichTextBox2
  
  If ReadIniString("Options", "ShowId", True, tmp) Then
    frm.mnuShowId.Checked = CBool(tmp)
  Else
    frm.mnuShowId.Checked = True
  End If
  If ReadIniString("Options", "UseModIcon", False, tmp) Then
    frm.mnuModIcon.Checked = CBool(tmp)
  Else
    frm.mnuModIcon.Checked = False
  End If
  If ReadIniString("Options", "ShowIcon", True, tmp) Then
    frm.mnuShowIcon.Checked = CBool(tmp)
  Else
    frm.mnuShowIcon.Checked = True
  End If
  If frm.mnuShowIcon.Checked Then
    tree.Style = 5
  Else
    tree.Style = 4
  End If
  
  If ReadIniString("Options", "ShowMessage", False, tmp) Then
    frm.mnuShowMsg.Checked = CBool(tmp)
  Else
    frm.mnuShowMsg.Checked = False
  End If
  
  If ReadIniString("Options", "ShowLogo", False, tmp) Then
    frm.mnuShowLogo.Checked = CBool(tmp)
  Else
    frm.mnuShowLogo.Checked = False
  End If
  If ReadIniString("Options", "LogoFile", BLANK, tmp) Then
    LogoFile = tmp
  Else
    LogoFile = ""
  End If
  
  Err.Clear
  On Error Resume Next
  Set logo.Picture = LoadPicture(LogoFile)
  If Err.Number > 0 Then
    frm.mnuShowLogo.Checked = False
    MsgBox "Error loading logo image: " & LogoFile
    LogoFile = BLANK
    Err.Clear
  End If
  
  GetFont
  SetFont
  GetColors
  GetBarPos
  GetWinPos
  
  For i = 0 To MAX_RECENT
    If ReadIniString("Recent File List", "File" & i + 1, BLANK, tmp) And tmp <> BLANK Then

' dir errors when drive is not present (removeable drives)
On Error GoTo DriveNotFound
      If Dir(tmp) <> BLANK Then
        frm.mnuFileN(i).Caption = LCase(tmp)
        frm.mnuFileN(i).Visible = True
        frm.mnuSep3.Visible = True
      End If

DriveNotFound:
    End If
  Next
End Sub

Sub SetOptions()
Dim i As Long
  
  WriteIniString "App Path", FUI, AppPath
  WriteIniString "Options", "ShowId", frm.mnuShowId.Checked
  WriteIniString "Options", "UseModIcon", frm.mnuModIcon.Checked
  WriteIniString "Options", "ShowIcon", frm.mnuShowIcon.Checked
  WriteIniString "Options", "ShowLogo", frm.mnuShowLogo.Checked
  WriteIniString "Options", "ShowMessage", frm.mnuShowMsg.Checked
  WriteIniString "Options", "LogoFile", LogoFile
  
  SetFont
  SetColors
  SetBarPos
  SetWinPos
  
  For i = 0 To MAX_RECENT
    WriteIniString "Recent File List", "File" & i + 1, LCase(frm.mnuFileN(i).Caption)
  Next
End Sub

Sub UpdateFileMenu(FileName As String)
Dim i As Long
Dim j As Long
Dim found As Boolean

  For i = 0 To 3
    If frm.mnuFileN(i).Caption = FileName Then
      found = True
      Exit For
    End If
  Next
  
  If Not found Then i = 3
  For j = i To 1 Step -1
    frm.mnuFileN(j).Caption = frm.mnuFileN(j - 1).Caption
    If frm.mnuFileN(j).Caption <> BLANK Then
      frm.mnuFileN(j).Visible = True
    Else
      frm.mnuFileN(j).Visible = False
    End If
  Next
  frm.mnuFileN(0).Caption = FileName
  frm.mnuFileN(0).Visible = True
  frm.mnuSep3.Visible = True
End Sub

Public Sub UpdateSiteMenu(Index As Long)
Dim i As Long
  
  For i = 0 To MAX_SITES - 1
    frm.mnuSites(i).Visible = NumSites > i
    frm.mnuSites(i).Checked = False
    If NumSites > i Then frm.mnuSites(i).Caption = Sites(i).Name
  Next i
  frm.mnuSites(Index).Checked = True
  frm.mnuDelSite.Enabled = NumSites > 1
End Sub

Function Closest(siteIdx As Long, x As Single, y As Single) As Long
'this finds an icon with a snap distance of the icon width from the xy given
Dim i As Long
Dim d1 As Double
Dim d2 As Double
Dim tx As Double
Dim ty As Double
Dim ofy As Double
Dim ofx As Double
  
  'icon x,y in upper left corner of stop light
  ofx = 1000# / pic.ScaleWidth * images.ImageWidth * 0.8 * Screen.TwipsPerPixelX
  ofy = 1000# / pic.ScaleHeight * images.ImageHeight * 0.5 * Screen.TwipsPerPixelY
  
  Closest = 0
  d1 = 10000000000#
  For i = 0 To Sites(siteIdx).NumGlyphs - 1
    tx = x - (Sites(siteIdx).Glyphs(i).sx + ofx)
    ty = y - (Sites(siteIdx).Glyphs(i).sy + ofy)
    d2 = tx * tx + ty * ty
    If d1 > d2 Then
      Closest = i
      d1 = d2
    End If
  Next
  If d1 > (ofy * ofy) Then Closest = -1
End Function

Sub FindDropPosition(sx As Single, sy As Single)
Dim i As Long
Dim x As Long, x1 As Long, x2 As Long
Dim y As Long, y1 As Long, y2 As Long
Dim xCt As Long, yCt As Long
Dim used As Boolean

  xCt = 1 + (pic.ScaleWidth / (images.ImageWidth * Screen.TwipsPerPixelX))
  yCt = 1 + (pic.ScaleHeight / (images.ImageHeight * Screen.TwipsPerPixelY))
  For x = 1 To xCt
    x1 = (x - 1) * images.ImageWidth
    x2 = (x * images.ImageWidth)
    For y = 1 To yCt
      used = False
      y1 = ((y - 1) * images.ImageHeight)
      y2 = (y * images.ImageHeight)
      For i = 0 To Sites(currentSite).NumGlyphs - 1
        used = ((Sites(currentSite).Glyphs(i).sx >= x1 And Sites(currentSite).Glyphs(i).sx <= x2) And _
                (Sites(currentSite).Glyphs(i).sy >= y1 And Sites(currentSite).Glyphs(i).sy <= y2))
        If used Then Exit For
      Next
      If Not used Then Exit For
    Next
    If Not used Then Exit For
  Next x
  sx = x1 + 5
  sy = y1 + 5
End Sub

Sub Arrow_Draw(p As Object, x1 As Single, y1 As Single, x2 As Single, y2 As Single, fColor As Long, bColor As Long)
Dim x(6) As Double
Dim y(6) As Double
Dim sc As Double
Dim mx As Single
Dim my As Single
Dim rad As Double
Dim j As Long
  
  sc = 125
  mx = (x2 + x1) * 0.5
  my = (y2 + y1) * 0.5
  x(0) = x2 - x1
  y(0) = y2 - y1
  x(1) = 0
  y(1) = 1
  If x(0) < 0 Then y(1) = -1
  
'  rad = (X(0) * X(1) + Y(0) * Y(1)) / Sqr(X(0) * X(0) + Y(0) * Y(0)) * Sqr(X(1) * X(1) + Y(1) * Y(1))
  If y(0) = 0 And x(0) = 0 Then
    rad = 0
  Else
    rad = (y(0) * y(1)) / Sqr(x(0) * x(0) + y(0) * y(0))
  End If
  'Arccos(X) = Atn(-X / Sqr(-X * X + 1)) + 2 * Atn(1)
  rad = Atn(-rad / Sqr(-rad * rad + 1)) + 2 * 0.7854
  If x(0) < 0 Then rad = rad + 3.14159
  
  x(0) = 0#:      y(0) = 0.8
  x(1) = 0.45:    y(1) = -0.5
  x(2) = -0.45:   y(2) = -0.5
  x(3) = 0#:      y(3) = 0.1        'fill point
  
  p.CurrentX = sc * (x(2) * Cos(rad) + y(2) * Sin(rad)) + mx
  p.CurrentY = sc * (x(2) * -Sin(rad) + y(2) * Cos(rad)) + my
  For j = 0 To 2
    x(5) = sc * (x(j) * Cos(rad) + y(j) * Sin(rad)) + mx
    y(5) = sc * (x(j) * -Sin(rad) + y(j) * Cos(rad)) + my
    p.Line -(x(5), y(5)), fColor
  Next
  
'  X(5) = (sc * (X(3) * Cos(rad) + Y(3) * Sin(rad)) + mx) / Screen.TwipsPerPixelX
'  Y(5) = (sc * (X(3) * -Sin(rad) + Y(3) * Cos(rad)) + my) / Screen.TwipsPerPixelY
  x(5) = (sc * (y(3) * Sin(rad)) + mx) / Screen.TwipsPerPixelX
  y(5) = (sc * (y(3) * Cos(rad)) + my) / Screen.TwipsPerPixelY
  p.FillColor = bColor
  FloodFill p.hdc, x(5), y(5), fColor
End Sub

Sub Hermite_Draw(p As Object, p1x As Single, p1y As Single, _
                 p4x As Single, p4y As Single, r1x As Single, r1y As Single, _
                 r4x As Single, r4y As Single, fColor As Long, bColor As Long)
  Dim numstep As Long, cnt As Long
  Dim dx As Single, dy As Single
  Dim ox As Single, oy As Single
  Dim nx As Single, ny As Single
  Dim t As Single, dt As Single
  Dim t2 As Single, t3 As Single
  Dim cx(3) As Single, cy(3) As Single
  
  dx = Abs(p1x - p4x)
  dy = Abs(p1y - p4y)
  ox = p1x
  oy = p1y
  cx(0) = 2 * p1x - 2 * p4x + r1x + r4x
  cx(1) = -3 * p1x + 3 * p4x - 2 * r1x - r4x
  cx(2) = r1x
  cx(3) = p1x
  cy(0) = 2 * p1y - 2 * p4y + r1y + r4y
  cy(1) = -3 * p1y + 3 * p4y - 2 * r1y - r4y
  cy(2) = r1y
  cy(3) = p1y
  dt = 300# / (dx + dy)
  cnt = 0
  numstep = (1# / dt + 1) / 2#
  If numstep = 0 Then numstep = 1
  For t = 0 To 1# Step dt
    t2 = t * t
    t3 = t2 * t
    nx = cx(0) * t3 + cx(1) * t2 + cx(2) * t + cx(3)
    ny = cy(0) * t3 + cy(1) * t2 + cy(2) * t + cy(3)
'    If (Abs(t - 0.5) < dt / 2) Then
    If (cnt Mod numstep = 0) Then
      Arrow_Draw p, ox, oy, nx, ny, fColor, bColor
    End If
    p.Line (ox, oy)-(nx, ny), fColor
    ox = nx
    oy = ny
    cnt = cnt + 1
  Next t
End Sub

Sub DrawSite(s As Site, p As Object, printout As Boolean)
Dim i As Long, j As Long
Dim x As Single, y As Single
Dim Ex As Single, Ey As Single
Dim rad As Single
Dim gLabel As String
Dim fColor As Long
Dim bColor As Long
Dim vis As Long
Dim ox As Single
Dim oy As Single
Dim ox2 As Single
Dim oy2 As Single
Dim ox3 As Single
Dim oy3 As Single
  
  status.Panels(1) = "File: " & LongGidTitle & DOT_GID
  status.Panels(2) = "Site: " & s.Name
  gLabel = s.Glyphs(currentGlyph).label
  If frm.mnuShowId.Checked Then gLabel = gLabel & " (" & s.Glyphs(currentGlyph).Name & ")"
  status.Panels(3) = "Icon: " & gLabel

  If printout Then
    p.Orientation = 2
    p.ScaleHeight = pic.Height
    p.ScaleWidth = pic.Width
    dx = p.ScaleWidth / 1000#
    dy = p.ScaleHeight / 1000#
  Else
    'Set scaling
    dx = p.ScaleWidth / 1000#
    dy = p.ScaleHeight / 1000#
    If LongGidDir <> BLANK Then
      p.BackColor = wbcolor
      p.ForeColor = wfcolor
      p.cls
      p.TabStop = True
    Else
      p.BackColor = vbApplicationWorkspace
      p.ForeColor = vbApplicationWorkspace
      p.cls
      p.TabStop = False
      Exit Sub
    End If
  End If
 
  'Get size of icon to get icon center
  'for line start and end points
  ox = images.ImageWidth * Screen.TwipsPerPixelX
  oy = images.ImageHeight * Screen.TwipsPerPixelY
  ox2 = ox * 0.5
  oy2 = oy * 0.5
  'calculate stop light icon size
  ox3 = ox * 0.75
  oy3 = oy * 0.75
  ' Draw all the lines
  For i = 0 To s.NumGlyphs - 1
    For j = 0 To s.NumGlyphs - 1
      If s.connect(i, j) = OSNK Then
        If Group(s.Glyphs(j).GrpIdx).Type = SYS Then
          fColor = sfcolor
          bColor = sbcolor
          vis = svis
        ElseIf Group(s.Glyphs(i).GrpIdx).Type = DB Then
          fColor = dfcolor
          bColor = dbcolor
          vis = dvis
        Else
          fColor = mfcolor
          bColor = mbcolor
          vis = mvis
        End If
        x = dx * s.Glyphs(i).sx + ox
        y = dy * s.Glyphs(i).sy + oy2
        Ex = dx * s.Glyphs(j).sx + ox
        Ey = dy * s.Glyphs(j).sy + oy2
        rad = Abs(x - Ex) + Abs(y - Ey)
        If vis = 1 Then Hermite_Draw p, x, y, Ex, Ey, rad, 0, rad, 0, fColor, bColor
      End If
    Next
  Next
  
  rad = ox2 * 0.3
  For i = 0 To s.NumGlyphs - 1
    On Error Resume Next
    If frm.mnuModIcon.Checked Then
      icon.Picture = images.ListImages(Module(s.Glyphs(i).modIdx).FileName).Picture
    Else
      icon.Picture = images.ListImages(Group(s.Glyphs(i).GrpIdx).Prefix).Picture
    End If
    If Err.Number > 0 Then
      Err.Clear
      icon.Picture = images.ListImages("unknown").Picture ' worst case
    End If
    On Error GoTo 0
    
    'draw Icon
    x = dx * s.Glyphs(i).sx
    y = dy * s.Glyphs(i).sy
 
'draws a bar instead of a stop light
' Dim cl, bc, fc
' Select Case s.Glyphs(i).state
' Case -1: cl = vbBlack
' Case 0:  cl = vbRed
' Case 1:  cl = vbYellow
' Case 2:  cl = vbGreen
' End Select
'
' fc = p.ForeColor
' p.ForeColor = cl
' p.Line (x + (ox2 / 2), y)-(x + ox2, y + oy), cl, BF
' p.ForeColor = fc
    
    p.FillStyle = 0
    If Not printout And currentGlyph = i Then
      'draw inverted icon image
      vis = BitBlt(p.hdc, (x + ox2) / Screen.TwipsPerPixelX, y / Screen.TwipsPerPixelX, 32, 32, icon.hdc, 0, 0, vbNotSrcCopy)
      If vis = 0 Then p.PaintPicture icon, x + ox2, y, , oy
    Else
      'draw regular icon image
      p.PaintPicture icon, x + ox2, y, , oy
    End If
    
    'draw stop light
    icon.Picture = images.ListImages(s.Glyphs(i).state + 5).Picture
    p.PaintPicture icon, x, y, ox3, oy3
    
    'draw label
    If wvis = 1 Then
      gLabel = s.Glyphs(i).label
      If frm.mnuShowId.Checked Then gLabel = gLabel & " (" & s.Glyphs(i).Name & ")"
      p.ForeColor = wfcolor
      p.CurrentX = x + ox2
      p.CurrentY = y + oy
      p.Print gLabel
    End If
  Next
  If printout Then p.EndDoc
End Sub

Sub PrintReferences()
Dim flags As Long

  On Error Resume Next
  flags = PD_NOPAGENUMS + PD_NOSELECTION
  If ShowPrinter(frm, flags) Then
    rtb2.Text = ""
    rtb2.SelStart = 0
    Reference.DisplayReferences rtb2
    If flags And PD_PRINTTOFILE Then
      cdl.DialogTitle = "Print To File"
      cdl.FileName = "*.rtf"
      cdl.Filter = "Rich Text Files (*.rtf)|*.rtf ' |Text Files (*.txt)|*.txt"
      cdl.FilterIndex = 0
      cdl.DefaultExt = "rtf"
      cdl.flags = cdlOFNOverwritePrompt Or cdlOFNHideReadOnly Or cdlOFNExtensionDifferent Or cdlOFNNoChangeDir
      cdl.CancelError = True
      cdl.ShowOpen
      If Err.Number <> cdlCancel Then
        rtb2.SaveFile cdl.FileName, 0
      End If
    Else
      PrintRTF rtb2, 1440, 1440, 1440, 1440
    End If
  End If
End Sub

