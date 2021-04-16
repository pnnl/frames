Attribute VB_Name = "AutoIt3API"
Option Explicit
' API Name: AutoIt3 API
'-------------------------------------------------------------------------
' Documentation for: Init
' Resets AutoIt to defaults (window delays, key delays, etc.). This function is called automatically when the DLL is loaded.
DECLARE Sub Init LIB "AutoItX3.dll" Alias "__Init@0" (  ) 

'-------------------------------------------------------------------------
' Documentation for: BlockInput
' Will prevent any user input via the mouse or keyboard. This function will have different effects depending on the operating system used:. Operating System "BlockInput" Results <br />Windows 95 No effect. <br />Windows 98 User input is blocked but AutoIt is also unable to simulate input. <br />Windows NT 4 (Without ServicePack 6) No effect. <br />Windows NT 4 (With ServicePack 6) User input is blocked and AutoIt can simulate input. <br />Windows 2000 User input is blocked and AutoIt can simulate input.
DECLARE Sub BlockInput LIB "AutoItX3.dll" Alias "__BlockInput@4" (  ByVal nToggle as long ) 

'-------------------------------------------------------------------------
' Documentation for: CDTray
' Controls CD tray of a local system.
DECLARE Sub CDTray LIB "AutoItX3.dll" Alias "__CDTray@8" (  ByVal szDrive as string,  ByVal szAction as string ) 

'-------------------------------------------------------------------------
' Documentation for: ClipGet
' Allows you to get text from the clipboard. A maximum of 4096 characters of text can be received; ensure that the text buffer you pass is at least this large.
DECLARE Sub DLLClipGet LIB "AutoItX3.dll" Alias "__ClipGet@8" (  ByVal szClip as string,  ByVal nBufSize as long ) 

'-------------------------------------------------------------------------
' Documentation for: ClipPut
' Allows you to get text from the clipboard. A maximum of 4096 characters of text can be received; ensure that the text buffer you pass is at least this large.
DECLARE Sub ClipPut LIB "AutoItX3.dll" Alias "__ClipPut@4" (  ByVal szClip as string ) 

'-------------------------------------------------------------------------
' Documentation for: ControlClick
' Sends a mouse click command to a given control.
DECLARE Function ControlClick LIB "AutoItX3.dll" Alias "__ControlClick@20" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szControl as string,  ByVal szButton as string,  ByVal nNumClicks as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ControlCommand
' Sends a command to a control.
DECLARE Sub DLLControlCommand LIB "AutoItX3.dll" Alias "__ControlCommand@28" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szControl as string,  ByVal szCommand as string,  ByVal szExtra as string,  ByVal szResult as string,  ByVal nBufSize as long ) 

'-------------------------------------------------------------------------
' Documentation for: ControlListView
' Sends a command to a ListView32 control.
DECLARE Sub DLLControlListView LIB "AutoItX3.dll" Alias "__ControlListView@32" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szControl as string,  ByVal szCommand as string,  ByVal szExtra1 as string,  ByVal szExtra2 as string,  ByVal szResult as string,  ByVal nBufSize as long ) 

'-------------------------------------------------------------------------
' Documentation for: ControlDisable
' Disables or "grays-out" a control.
DECLARE Function ControlDisable LIB "AutoItX3.dll" Alias "__ControlDisable@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szControl as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ControlEnable
' 
DECLARE Function ControlEnable LIB "AutoItX3.dll" Alias "__ControlEnable@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szControl as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ControlFocus
' Sets input focus to a given control on a window.
DECLARE Function ControlFocus LIB "AutoItX3.dll" Alias "__ControlFocus@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szControl as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ControlGetFocus
' Returns the ControlRef# of the control that has keyboard focus within a specified window.
DECLARE Sub DLLControlGetFocus LIB "AutoItX3.dll" Alias "__ControlGetFocus@16" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szControlWithFocus as string,  ByVal nBufSize as long ) 

'-------------------------------------------------------------------------
' Documentation for: ControlGetHandle
' Retrieves the internal handle of a control.
DECLARE Sub DLLControlGetHandle LIB "AutoItX3.dll" Alias "__ControlGetHandle@20" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szControl as string,  ByVal szRetText as string,  ByVal nBufSize as long ) 

'-------------------------------------------------------------------------
' Documentation for: ControlGetPosX
' Returns the X position of a window.
DECLARE Function ControlGetPosX LIB "AutoItX3.dll" Alias "__ControlGetPosX@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szControl as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ControlGetPosY
' Returns the Y position of a window.
DECLARE Function ControlGetPosY LIB "AutoItX3.dll" Alias "__ControlGetPosY@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szControl as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ControlGetPosHeight
' Returns the height of a window.
DECLARE Function ControlGetPosHeight LIB "AutoItX3.dll" Alias "__ControlGetPosHeight@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szControl as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ControlGetPosWidth
' Returns the width of a window.
DECLARE Function ControlGetPosWidth LIB "AutoItX3.dll" Alias "__ControlGetPosWidth@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szControl as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ControlGetText
' Retrieves text from a control.
DECLARE Sub DLLControlGetText LIB "AutoItX3.dll" Alias "__ControlGetText@20" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szControl as string,  ByVal szControlText as string,  ByVal nBufSize as long ) 

'-------------------------------------------------------------------------
' Documentation for: ControlHide
' Hides a control
DECLARE Function ControlHide LIB "AutoItX3.dll" Alias "__ControlHide@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szControl as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ControlMove
' Moves a control within a window.
DECLARE Function ControlMove LIB "AutoItX3.dll" Alias "__ControlMove@28" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szControl as string,  ByVal nX as long,  ByVal nY as long,  ByVal nWidth as long,  ByVal nHeight as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ControlSend
' Sends a string of characters to a control.
DECLARE Function ControlSend LIB "AutoItX3.dll" Alias "__ControlSend@20" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szControl as string,  ByVal szSendText as string,  ByVal nMode as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ControlSetText
' Sets text of a control.
DECLARE Function ControlSetText LIB "AutoItX3.dll" Alias "__ControlSetText@16" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szControl as string,  ByVal szControlText as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ControlShow
' Shows a control that was hidden.
DECLARE Function ControlShow LIB "AutoItX3.dll" Alias "__ControlShow@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szControl as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DriveMapAdd
' Maps a network drive.
DECLARE Sub DLLDriveMapAdd LIB "AutoItX3.dll" Alias "__DriveMapAdd@28" (  ByVal szDevice as string,  ByVal szShare as string,  ByVal nFlags as long,  ByVal szUser as string,  ByVal szPwd as string,  ByVal szResult as string,  ByVal nBufSize as long ) 

'-------------------------------------------------------------------------
' Documentation for: DriveMapDel
' Disconnects a network drive.
DECLARE Function DriveMapDel LIB "AutoItX3.dll" Alias "__DriveMapDel@4" (  ByVal szDevice as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DriveMapGet
' Retreives the details of a mapped drive.
DECLARE Sub DLLDriveMapGet LIB "AutoItX3.dll" Alias "__DriveMapGet@12" (  ByVal szDevice as string,  ByVal szMapping as string,  ByVal nBufSize as long ) 

'-------------------------------------------------------------------------
' Documentation for: IniDelete
' Deletes a value from a standard format .ini file.
DECLARE Function IniDelete LIB "AutoItX3.dll" Alias "__IniDelete@12" (  ByVal szFilename as string,  ByVal szSection as string,  ByVal szKey as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IniRead
' Reads a value from a standard format .ini file.
DECLARE Sub DLLIniRead LIB "AutoItX3.dll" Alias "__IniRead@24" (  ByVal szFileName as string,  ByVal szSection as string,  ByVal szKey as string,  ByVal szDefault as string,  ByVal szValue as string,  ByVal nBufSize as long ) 

'-------------------------------------------------------------------------
' Documentation for: IniWrite
' Writes a value to a standard format .ini file.
DECLARE Function IniWrite LIB "AutoItX3.dll" Alias "__IniWrite@16" (  ByVal szFileName as string,  ByVal szSection as string,  ByVal szKey as string,  ByVal szValue as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IsAdmin
' Checks if the current user has administrator privileges.
DECLARE Function IsAdmin LIB "AutoItX3.dll" Alias "__IsAdmin@0" (  )  as integer 

'-------------------------------------------------------------------------
' Documentation for: MouseClick
' Perform a mouse click operation.
DECLARE Function MouseClick LIB "AutoItX3.dll" Alias "__MouseClick@20" (  ByVal szButton as string,  ByVal nX as long,  ByVal nY as long,  ByVal nClicks as long,  ByVal nSpeed as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: MouseClickDrag
' Perform a mouse click and drag operation.
DECLARE Function MouseClickDrag LIB "AutoItX3.dll" Alias "__MouseClickDrag@24" (  ByVal szButton as string,  ByVal nX1 as long,  ByVal nY1 as long,  ByVal nX2 as long,  ByVal nY2 as long,  ByVal nSpeed as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: MouseDown
' Perform a mouse down event at the current mouse position.
DECLARE Sub MouseDown LIB "AutoItX3.dll" Alias "__MouseDown@4" (  ByVal szButton as string ) 

'-------------------------------------------------------------------------
' Documentation for: MouseGetCursor
' Returns a cursor ID Number: 0 = UNKNOWN (this includes pointing and grabbing hand icons) 1 = APPSTARTING 2 = ARROW 3 = CROSS 4 = HELP 5 = IBEAM 6 = ICON 7 = NO 8 = SIZE 9 = SIZEALL 10 = SIZENESW 11 = SIZENS 12 = SIZENWSE 13 = SIZEWE 14 = UPARROW 15 = WAIT
DECLARE Function MouseGetCursor LIB "AutoItX3.dll" Alias "__MouseGetCursor@0" (  )  as integer 

'-------------------------------------------------------------------------
' Documentation for: MouseGetPosX
' Gets the X coordinate of the mouse pointer. The X and Y co-ordinates are relative to the currently active window. Run the full version of AutoIt in reveal mode to determine the required co-ordinates of a window.
DECLARE Function MouseGetPosX LIB "AutoItX3.dll" Alias "__MouseGetPosX@0" (  )  as integer 

'-------------------------------------------------------------------------
' Documentation for: MouseGetPosY
' Gets the Y coordinate of the mouse pointer. The X and Y co-ordinates are relative to the currently active window. Run the full version of AutoIt in reveal mode to determine the required co-ordinates of a window.
DECLARE Function MouseGetPosY LIB "AutoItX3.dll" Alias "__MouseGetPosY@0" (  )  as integer 

'-------------------------------------------------------------------------
' Documentation for: MouseMove
' Moves the mouse pointer to the specified coordinates. The X and Y co-ordinates are relative to the currently active window. Run the full version of AutoIt in reveal mode to determine the required co-ordinates of a window.
DECLARE Function MouseMove LIB "AutoItX3.dll" Alias "__MouseMove@12" (  ByVal nX as long,  ByVal nY as long,  ByVal nSpeed as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: MouseUp
' Perform a mouse up event at the current mouse position.
DECLARE Sub MouseUp LIB "AutoItX3.dll" Alias "__MouseUp@4" (  ByVal szButton as string ) 

'-------------------------------------------------------------------------
' Documentation for: MouseWheel
' Moves the mouse wheel up or down. NT/2000/XP ONLY.
DECLARE Sub MouseWheel LIB "AutoItX3.dll" Alias "__MouseWheel@8" (  ByVal szDirection as string,  ByVal nClicks as long ) 

'-------------------------------------------------------------------------
' Documentation for: Opt
' This option will set Function options in AutoIT.
DECLARE Function Opt LIB "AutoItX3.dll" Alias "__Opt@8" (  ByVal szOption as string,  ByVal nValue as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: PixelChecksum
' Generates a checksum for a region of pixels.
DECLARE Function PixelChecksum LIB "AutoItX3.dll" Alias "__PixelChecksum@20" (  ByVal nLeft as long,  ByVal nTop as long,  ByVal nRight as long,  ByVal nBottom as long,  ByVal nStep as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: PixelGetColor
' Returns a pixel color according to x,y pixel coordinates.
DECLARE Function PixelGetColor LIB "AutoItX3.dll" Alias "__PixelGetColor@8" (  ByVal nX as long,  ByVal nY as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: PixelSearch
' Searches a rectangle of pixels for the pixel color provided.
DECLARE Sub DLLPixelSearch LIB "AutoItX3.dll" Alias "__PixelSearch@36" (  ByVal nLeft as long,  ByVal nTop as long,  ByVal nRight as long,  ByVal nBottom as long,  ByVal nCol as long,  ByVal nVar as long,  ByVal nStep as long,  ByVal szResult as string,  ByVal nBufSize as long ) 

'-------------------------------------------------------------------------
' Documentation for: ProcessClose
' Terminates a named process.
DECLARE Function ProcessClose LIB "AutoItX3.dll" Alias "__ProcessClose@4" (  ByVal szProcess as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ProcessExists
' Checks to see if a specified process exists.
DECLARE Function ProcessExists LIB "AutoItX3.dll" Alias "__ProcessExists@4" (  ByVal szProcess as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ProcessSetPriority
' Changes the priority of a process
DECLARE Function ProcessSetPriority LIB "AutoItX3.dll" Alias "__ProcessSetPriority@8" (  ByVal szProcess as string,  ByVal nPriority as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ProcessWait
' Pauses script execution until a given process exists.
DECLARE Function ProcessWait LIB "AutoItX3.dll" Alias "__ProcessWait@8" (  ByVal szProcess as string,  ByVal nTimeout as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ProcessWaitClose
' Pauses script execution until a given process does not exist.
DECLARE Function ProcessWaitClose LIB "AutoItX3.dll" Alias "__ProcessWaitClose@8" (  ByVal szProcess as string,  ByVal nTimeout as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: RegDeleteKey
' Deletes a key or value from the registry.
DECLARE Function RegDeleteKey LIB "AutoItX3.dll" Alias "__RegDeleteKey@4" (  ByVal szKeyName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: RegDeleteVal
' Deletes a key or value from the registry.
DECLARE Function RegDeleteVal LIB "AutoItX3.dll" Alias "__RegDeleteVal@8" (  ByVal szKeyName as string,  ByVal szValuename as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: RegEnumKey
' Reads the name of a subkey according to it's instance.
DECLARE Sub DLLRegEnumKey LIB "AutoItX3.dll" Alias "__RegEnumKey@16" (  ByVal szKeyName as string,  ByVal nInstance as long,  ByVal szResult as string,  ByVal nBufSize as long ) 

'-------------------------------------------------------------------------
' Documentation for: RegEnumVal
' Reads the name of a value according to it's instance.
DECLARE Sub DLLRegEnumVal LIB "AutoItX3.dll" Alias "__RegEnumVal@16" (  ByVal szKeyName as string,  ByVal nInstance as long,  ByVal szResult as string,  ByVal nBufSize as long ) 

'-------------------------------------------------------------------------
' Documentation for: RegRead
' Reads a value from the registry.
DECLARE Sub DLLRegRead LIB "AutoItX3.dll" Alias "__RegRead@16" (  ByVal szKeyName as string,  ByVal szValuename as string,  ByVal szRetText as string,  ByVal nBufSize as long ) 

'-------------------------------------------------------------------------
' Documentation for: RegWrite
' Creates a key or value in the registry.
DECLARE Function RegWrite LIB "AutoItX3.dll" Alias "__RegWrite@16" (  ByVal szKeyName as string,  ByVal szValueName as string,  ByVal szType as string,  ByVal szValue as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: Run
' Runs an external program.
DECLARE Function Run LIB "AutoItX3.dll" Alias "__Run@12" (  ByVal szRun as string,  ByVal szDir as string,  ByVal nShowFlags as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: RunAsSet
' Initialise a set of user credentials to use during Run and RunWait operations. 2000/XP or later ONLY.
DECLARE Function RunAsSet LIB "AutoItX3.dll" Alias "__RunAsSet@16" (  ByVal szUser as string,  ByVal szDomain as string,  ByVal szPassword as string,  ByVal nOptions as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: RunWait
' Runs an external program and pauses script execution until the program finishes.
DECLARE Function RunWait LIB "AutoItX3.dll" Alias "__RunWait@12" (  ByVal szRun as string,  ByVal szDir as string,  ByVal nShowFlags as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: Send
' Set an error message. The Error subroutine will add the string value passed by the calling program to the Error File and then terminate the calling program. The "Send" command syntax is similar to that of ScriptIt and the Visual Basic "SendKeys" command. Characters are sent as written with the exception of the following characters: '!' This tells AutoIt to send an ALT keystroke, therefore Send "This is text!a" would send the keys "This is text" and then press "ALT+a". N.B. Some programs are very choosy about capital letters and ALT keys, i.e. "!A" is different to "!a". The first says ALT+SHIFT+A, the second is ALT+a. If in doubt, use lowercase! '+' This tells AutoIt to send a SHIFT keystroke, therefore Send "Hell+o" would send the text "HellO". Send "!+a" would send "ALT+SHIFT+a". '^' This tells AutoIt to send a CONTROL keystroke, therefore Send "^!a" would send "CTRL+ALT+a". N.B. Some programs are very choosy about capital letters and CTRL keys, i.e. "^A" is different to "^a". The first says CTRL+SHIFT+A, the second is CTRL+a. If in doubt, use lowercase! '#' The hash is used as a key delimiter to make a line easier to read. i.e. Send "H#e#l#l#o" is the same as Send "Hello". Certain special keys can be sent and should be enclosed in braces:
DECLARE Sub Send LIB "AutoItX3.dll" Alias "__Send@8" (  ByVal szSentText as string,  ByVal nMode as long ) 

'-------------------------------------------------------------------------
' Documentation for: Shutdown
' This function can perform various types of shutdown on all Windows operating systems. The value of nFlag can be a combination of the flags below: Function <br />Log off the current user 0 <br />Shutdown the workstation 1 <br />Reboot the workstation 2 <br />Force closing of applications (may lose unsaved work) 4 <br />Shutdown and power off (if supported) 8 i.e. To shutdown and reboot, the value of nFlag would be 3.
DECLARE Function Shutdown LIB "AutoItX3.dll" Alias "__Shutdown@4" (  ByVal nFlag as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: Sleep
' This function simply pauses for an amount of time.
DECLARE Sub Sleep LIB "AutoItX3.dll" Alias "__Sleep@4" (  ByVal nMilliseconds as long ) 

'-------------------------------------------------------------------------
' Documentation for: StatusbarGetText
' Retrieves the text from a standard status bar control.
DECLARE Sub DLLStatusbarGetText LIB "AutoItX3.dll" Alias "__StatusbarGetText@20" (  ByVal szTitle as string,  ByVal szText as string,  ByVal nPart as long,  ByVal szStatusText as string,  ByVal nBufSize as long ) 

'-------------------------------------------------------------------------
' Documentation for: ToolTip
' Creates a tooltip anywhere on the screen.
DECLARE Sub ToolTip LIB "AutoItX3.dll" Alias "__ToolTip@12" (  ByVal szTip as string,  ByVal nX as long,  ByVal nY as long ) 

'-------------------------------------------------------------------------
' Documentation for: WinActivate
' This function will activate a specified window.
DECLARE Sub WinActivate LIB "AutoItX3.dll" Alias "__WinActivate@8" (  ByVal szTitle as string,  ByVal szText as string ) 

'-------------------------------------------------------------------------
' Documentation for: WinActive
' Checks if a given window is currently active. Returns 1 if the given window was active, otherwise it returns 0.
DECLARE Function WinActive LIB "AutoItX3.dll" Alias "__WinActive@8" (  ByVal szTitle as string,  ByVal szText as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinClose
' This function will close a specified window
DECLARE Function WinClose LIB "AutoItX3.dll" Alias "__WinClose@8" (  ByVal szTitle as string,  ByVal szText as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinExists
' Checks if a given window current exists (in any state). Returns 1 if the given window exists (regardless of its state), otherwise it returns 0.
DECLARE Function WinExists LIB "AutoItX3.dll" Alias "__WinExists@8" (  ByVal szTitle as string,  ByVal szText as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinGetCaretPosX
' Returns a 2-element array containing the following information: array[0] = X coordinate
DECLARE Function WinGetCaretPosX LIB "AutoItX3.dll" Alias "__WinGetCaretPosX@0" (  )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinGetCaretPosY
' Returns a 2-element array containing the following information: array[1] = Y coordinate
DECLARE Function WinGetCaretPosY LIB "AutoItX3.dll" Alias "__WinGetCaretPosY@0" (  )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinGetClassList
' Retrieves the classes from a window.
DECLARE Sub DLLWinGetClassList LIB "AutoItX3.dll" Alias "__WinGetClassList@16" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szRetText as string,  ByVal nBufSize as long ) 

'-------------------------------------------------------------------------
' Documentation for: WinGetClientSizeHeight
' Retrieves the size of a given window's client area.
DECLARE Function WinGetClientSizeHeight LIB "AutoItX3.dll" Alias "__WinGetClientSizeHeight@8" (  ByVal szTitle as string,  ByVal szText as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinGetClientSizeWidth
' Retrieves the size of a given window's client area.
DECLARE Function WinGetClientSizeWidth LIB "AutoItX3.dll" Alias "__WinGetClientSizeWidth@8" (  ByVal szTitle as string,  ByVal szText as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinGetHandle
' A string pointer to receive the window title.
DECLARE Sub DLLWinGetHandle LIB "AutoItX3.dll" Alias "__WinGetHandle@16" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szRetText as string,  ByVal nBufSize as long ) 

'-------------------------------------------------------------------------
' Documentation for: WinGetPosX
' Retrieves the position and size of a given window.
DECLARE Function WinGetPosX LIB "AutoItX3.dll" Alias "__WinGetPosX@8" (  ByVal szTitle as string,  ByVal szText as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinGetPosY
' Retrieves the position and size of a given window.
DECLARE Function WinGetPosY LIB "AutoItX3.dll" Alias "__WinGetPosY@8" (  ByVal szTitle as string,  ByVal szText as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinGetPosHeight
' Retrieves the height of a given window.
DECLARE Function WinGetPosHeight LIB "AutoItX3.dll" Alias "__WinGetPosHeight@8" (  ByVal szTitle as string,  ByVal szText as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinGetPosWidth
' Retrieves the width of a given window.
DECLARE Function WinGetPosWidth LIB "AutoItX3.dll" Alias "__WinGetPosWidth@8" (  ByVal szTitle as string,  ByVal szText as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinGetProcess
' Retrieves the Process ID (PID) associated with a window
DECLARE Sub DLLWinGetProcess LIB "AutoItX3.dll" Alias "__WinGetProcess@16" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szRetText as string,  ByVal nBufSize as long ) 

'-------------------------------------------------------------------------
' Documentation for: WinGetState
' Returns a value indicating the state of the window. Multiple values are added together so use BitAND() to examine the part you are interested in: 1 = Window exists 2 = Window is visible 4 = Windows is enabled 8 = Window is active 16 = Window is minimized 32 = Windows is maximized
DECLARE Function WinGetState LIB "AutoItX3.dll" Alias "__WinGetState@8" (  ByVal szTitle as string,  ByVal szText as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinGetText
' Retrieves the text from a window.
DECLARE Sub DLLWinGetText LIB "AutoItX3.dll" Alias "__WinGetText@16" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szRetText as string,  ByVal nBufSize as long ) 

'-------------------------------------------------------------------------
' Documentation for: WinGetTitle
' Retrieves the title from a window.
DECLARE Sub DLLWinGetTitle LIB "AutoItX3.dll" Alias "__WinGetTitle@16" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szRetText as string,  ByVal nBufSize as long ) 

'-------------------------------------------------------------------------
' Documentation for: WinKill
' This function will forceably close a specified window.
DECLARE Function WinKill LIB "AutoItX3.dll" Alias "__WinKill@8" (  ByVal szTitle as string,  ByVal szText as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinMenuSelectItem
' Invokes a menu item of a window.
DECLARE Function WinMenuSelectItem LIB "AutoItX3.dll" Alias "__WinMenuSelectItem@40" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szItem1 as string,  ByVal szItem2 as string,  ByVal szItem3 as string,  ByVal szItem4 as string,  ByVal szItem5 as string,  ByVal szItem6 as string,  ByVal szItem7 as string,  ByVal szItem8 as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinMinimizeAll
' This function will minimize all windows.
DECLARE Sub WinMinimizeAll LIB "AutoItX3.dll" Alias "__WinMinimizeAll@0" (  ) 

'-------------------------------------------------------------------------
' Documentation for: WinMinimizeAllUndo
' This function will undo a previous WinMinimizeAll call.
DECLARE Sub WinMinimizeAllUndo LIB "AutoItX3.dll" Alias "__WinMinimizeAllUndo@0" (  ) 

'-------------------------------------------------------------------------
' Documentation for: WinMove
' Use this function to move/resize a specified window.
DECLARE Function WinMove LIB "AutoItX3.dll" Alias "__WinMove@24" (  ByVal szTitle as string,  ByVal szText as string,  ByVal nX as long,  ByVal nY as long,  ByVal nWidth as long,  ByVal nHeight as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinSetOnTop
' Change a window's "Always On Top" attribute.
DECLARE Function WinSetOnTop LIB "AutoItX3.dll" Alias "__WinSetOnTop@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal nFlag as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinSetState
' Shows, hides, minimizes, maximizes, or restores a window.
DECLARE Function WinSetState LIB "AutoItX3.dll" Alias "__WinSetState@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal nFlag as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinSetTitle
' Changes the title of a specified window.
DECLARE Function WinSetTitle LIB "AutoItX3.dll" Alias "__WinSetTitle@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szNewTitle as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinSetTrans
' Sets the transparency of a window. (Windows 2000/XP or later)
DECLARE Function WinSetTrans LIB "AutoItX3.dll" Alias "__WinSetTrans@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal nTrans as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinWait
' This function will pause until the specified window exists (or the function times out).
DECLARE Function WinWait LIB "AutoItX3.dll" Alias "__WinWait@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal nTimeout as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinWaitActive
' This function will pause until the specified window is active (or the function times out).
DECLARE Function WinWaitActive LIB "AutoItX3.dll" Alias "__WinWaitActive@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal nTimeout as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinWaitClose
' This function will pause until the specified window doesn't exist (or the function times out).
DECLARE Function WinWaitClose LIB "AutoItX3.dll" Alias "__WinWaitClose@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal nTimeout as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinWaitNotActive
' This function will pause until the specified window is not active (or the function times out).
DECLARE Function WinWaitNotActive LIB "AutoItX3.dll" Alias "__WinWaitNotActive@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal nTimeout as long )  as integer 

Sub ClipGet ( szClip as string  ,  ByVal nBufSize as long ) 
      Dim retStr1 as String * MAXFIELD
      call DLLClipGet(retstr1, nBufSize)
      szClip=StripTerminator(retStr1)
End  Sub 

Sub ControlCommand (  ByVal szTitle as string  ,  ByVal szText as string  ,  ByVal szControl as string  ,  ByVal szCommand as string  ,  ByVal szExtra as string  , szResult as string  ,  ByVal nBufSize as long ) 
      Dim retStr6 as String * MAXFIELD
      call DLLControlCommand(szTitle, szText, szControl, szCommand, szExtra, retstr6, nBufSize)
      szResult=StripTerminator(retStr6)
End  Sub 

Sub ControlListView (  ByVal szTitle as string  ,  ByVal szText as string  ,  ByVal szControl as string  ,  ByVal szCommand as string  ,  ByVal szExtra1 as string  ,  ByVal szExtra2 as string  , szResult as string  ,  ByVal nBufSize as long  ) 
      Dim retStr7 as String * MAXFIELD
      call DLLControlListView(szTitle, szText, szControl, szCommand, szExtra1, szExtra2, retstr7, nBufSize)
      szResult=StripTerminator(retStr7)
End  Sub 

Sub ControlGetFocus (  ByVal szTitle as string  ,  ByVal szText as string  , szControlWithFocus as string  ,  ByVal nBufSize as long ) 
      Dim retStr3 as String * MAXFIELD
      call DLLControlGetFocus(szTitle, szText, retstr3, nBufSize)
      szControlWithFocus=StripTerminator(retStr3)
End  Sub 

Sub ControlGetHandle (  ByVal szTitle as string  ,  ByVal szText as string  ,  ByVal szControl as string  , szRetText as string  ,  ByVal nBufSize as long ) 
      Dim retStr4 as String * MAXFIELD
      call DLLControlGetHandle(szTitle, szText, szControl, retstr4, nBufSize)
      szRetText=StripTerminator(retStr4)
End  Sub 

Sub ControlGetText (  ByVal szTitle as string  ,  ByVal szText as string  ,  ByVal szControl as string  , szControlText as string  ,  ByVal nBufSize as long ) 
      Dim retStr4 as String * MAXFIELD
      call DLLControlGetText(szTitle, szText, szControl, retstr4, nBufSize)
      szControlText=StripTerminator(retStr4)
End  Sub 

Sub DriveMapAdd (  ByVal szDevice as string  ,  ByVal szShare as string  ,  ByVal nFlags as long,  ByVal szUser as string  ,  ByVal szPwd as string  , szResult as string  ,  ByVal nBufSize as long ) 
      Dim retStr6 as String * MAXFIELD
      call DLLDriveMapAdd(szDevice, szShare, nFlags, szUser, szPwd, retstr6, nBufSize)
      szResult=StripTerminator(retStr6)
End  Sub 

Sub DriveMapGet (  ByVal szDevice as string  , szMapping as string  ,  ByVal nBufSize as long ) 
      Dim retStr2 as String * MAXFIELD
      call DLLDriveMapGet(szDevice, retstr2, nBufSize)
      szMapping=StripTerminator(retStr2)
End  Sub 

Sub IniRead (  ByVal szFileName as string  ,  ByVal szSection as string  ,  ByVal szKey as string  ,  ByVal szDefault as string  , szValue as string  ,  ByVal nBufSize as long ) 
      Dim retStr5 as String * MAXFIELD
      call DLLIniRead(szFileName, szSection, szKey, szDefault, retstr5, nBufSize)
      szValue=StripTerminator(retStr5)
End  Sub 

Sub PixelSearch (  ByVal nLeft as long,  ByVal nTop as long,  ByVal nRight as long,  ByVal nBottom as long,  ByVal nCol as long,  ByVal nVar as long,  ByVal nStep as long, szResult as string  ,  ByVal nBufSize as long ) 
      Dim retStr8 as String * MAXFIELD
      call DLLPixelSearch(nLeft, nTop, nRight, nBottom, nCol, nVar, nStep, retstr8, nBufSize)
      szResult=StripTerminator(retStr8)
End  Sub 

Sub RegEnumKey (  ByVal szKeyName as string  ,  ByVal nInstance as long, szResult as string  ,  ByVal nBufSize as long ) 
      Dim retStr3 as String * MAXFIELD
      call DLLRegEnumKey(szKeyName, nInstance, retstr3, nBufSize)
      szResult=StripTerminator(retStr3)
End  Sub 

Sub RegEnumVal (  ByVal szKeyName as string  ,  ByVal nInstance as long, szResult as string  ,  ByVal nBufSize as long ) 
      Dim retStr3 as String * MAXFIELD
      call DLLRegEnumVal(szKeyName, nInstance, retstr3, nBufSize)
      szResult=StripTerminator(retStr3)
End  Sub 

Sub RegRead (  ByVal szKeyName as string  ,  ByVal szValuename as string  , szRetText as string  ,  ByVal nBufSize as long ) 
      Dim retStr3 as String * MAXFIELD
      call DLLRegRead(szKeyName, szValuename, retstr3, nBufSize)
      szRetText=StripTerminator(retStr3)
End  Sub 

Sub StatusbarGetText (  ByVal szTitle as string  ,  ByVal szText as string  ,  ByVal nPart as long, szStatusText as string  ,  ByVal nBufSize as long ) 
      Dim retStr4 as String * MAXFIELD
      call DLLStatusbarGetText(szTitle, szText, nPart, retstr4, nBufSize)
      szStatusText=StripTerminator(retStr4)
End  Sub 

Sub WinGetClassList (  ByVal szTitle as string  ,  ByVal szText as string  , szRetText as string  ,  ByVal nBufSize as long ) 
      Dim retStr3 as String * MAXFIELD
      call DLLWinGetClassList(szTitle, szText, retstr3, nBufSize)
      szRetText=StripTerminator(retStr3)
End  Sub 

Sub WinGetHandle (  ByVal szTitle as string  ,  ByVal szText as string  , szRetText as string  ,  ByVal nBufSize as long ) 
      Dim retStr3 as String * MAXFIELD
      call DLLWinGetHandle(szTitle, szText, retstr3, nBufSize)
      szRetText=StripTerminator(retStr3)
End  Sub 

Sub WinGetProcess (  ByVal szTitle as string  ,  ByVal szText as string  , szRetText as string  ,  ByVal nBufSize as long ) 
      Dim retStr3 as String * MAXFIELD
      call DLLWinGetProcess(szTitle, szText, retstr3, nBufSize)
      szRetText=StripTerminator(retStr3)
End  Sub 

Sub WinGetText (  ByVal szTitle as string  ,  ByVal szText as string  , szRetText as string  ,  ByVal nBufSize as long ) 
      Dim retStr3 as String * MAXFIELD
      call DLLWinGetText(szTitle, szText, retstr3, nBufSize)
      szRetText=StripTerminator(retStr3)
End  Sub 

Sub WinGetTitle (  ByVal szTitle as string  ,  ByVal szText as string  , szRetText as string  ,  ByVal nBufSize as long ) 
      Dim retStr3 as String * MAXFIELD
      call DLLWinGetTitle(szTitle, szText, retstr3, nBufSize)
      szRetText=StripTerminator(retStr3)
End  Sub 
