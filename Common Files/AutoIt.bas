Attribute VB_Name = "AutoItAPI"
Option Explicit
' API Name: AutoIt API
'-------------------------------------------------------------------------
' Documentation for: Close
' Closes down AutoIt. This function is called automatically when the DLL is unloaded.
DECLARE Sub Close LIB "AutoItDLL.dll" Alias "__Close@0" (  ) 

'-------------------------------------------------------------------------
' Documentation for: Init
' Resets AutoIt to defaults (window delays, key delays, etc.). This function is called automatically when the DLL is loaded.
DECLARE Sub Init LIB "AutoItDLL.dll" Alias "__Init@0" (  ) 

'-------------------------------------------------------------------------
' Documentation for: Shutdown
' This function can perform various types of shutdown on all Windows operating systems. The value of nFlag can be a combination of the flags below: Function <br />Log off the current user 0 <br />Shutdown the workstation 1 <br />Reboot the workstation 2 <br />Force closing of applications (may lose unsaved work) 4 <br />Shutdown and power off (if supported) 8 i.e. To shutdown and reboot, the value of nFlag would be 3.
DECLARE Sub Shutdown LIB "AutoItDLL.dll" Alias "__Shutdown@4" (  ByVal nFlag as long ) 

'-------------------------------------------------------------------------
' Documentation for: Sleep
' This function simply pauses for an amount of time.
DECLARE Sub Sleep LIB "AutoItDLL.dll" Alias "__Sleep@4" (  ByVal nMilliseconds as long ) 

'-------------------------------------------------------------------------
' Documentation for: BlockInput
' Will prevent any user input via the mouse or keyboard. This function will have different effects depending on the operating system used:. Operating System "BlockInput" Results <br />Windows 95 No effect. <br />Windows 98 User input is blocked but AutoIt is also unable to simulate input. <br />Windows NT 4 (Without ServicePack 6) No effect. <br />Windows NT 4 (With ServicePack 6) User input is blocked and AutoIt can simulate input. <br />Windows 2000 User input is blocked and AutoIt can simulate input.
DECLARE Sub BlockInput LIB "AutoItDLL.dll" Alias "__BlockInput@4" (  ByVal nToggle as long ) 

'-------------------------------------------------------------------------
' Documentation for: LeftClick
' Simulates a mouse left-click at a given coordinate. The X and Y co-ordinates are relative to the currently active window. Run the full version of AutoIt in reveal mode to determine the required co-ordinates of a window. To perform a double-click, simply run the command twice :)
DECLARE Sub LeftClick LIB "AutoItDLL.dll" Alias "__LeftClick@8" (  ByVal nX as long,  ByVal nY as long ) 

'-------------------------------------------------------------------------
' Documentation for: LeftClickDrag
' Simulates a mouse left-click-drag operation. The X and Y co-ordinates are relative to the currently active window. Run the full version of AutoIt in reveal mode to determine the required co-ordinates of a window.
DECLARE Sub LeftClickDrag LIB "AutoItDLL.dll" Alias "__LeftClickDrag@16" (  ByVal nX1 as long,  ByVal nY1 as long,  ByVal nX2 as long,  ByVal nY2 as long ) 

'-------------------------------------------------------------------------
' Documentation for: MouseMove
' Moves the mouse pointer to the specified coordinates. The X and Y co-ordinates are relative to the currently active window. Run the full version of AutoIt in reveal mode to determine the required co-ordinates of a window.
DECLARE Sub MouseMove LIB "AutoItDLL.dll" Alias "__MouseMove@8" (  ByVal nX as long,  ByVal nY as long ) 

'-------------------------------------------------------------------------
' Documentation for: MouseGetPosX
' Gets the X coordinate of the mouse pointer. The X and Y co-ordinates are relative to the currently active window. Run the full version of AutoIt in reveal mode to determine the required co-ordinates of a window.
DECLARE Function MouseGetPosX LIB "AutoItDLL.dll" Alias "__MouseGetPosX@0" (  )  as integer 

'-------------------------------------------------------------------------
' Documentation for: MouseGetPosY
' Gets the Y coordinate of the mouse pointer. The X and Y co-ordinates are relative to the currently active window. Run the full version of AutoIt in reveal mode to determine the required co-ordinates of a window.
DECLARE Function MouseGetPosY LIB "AutoItDLL.dll" Alias "__MouseGetPosY@0" (  )  as integer 

'-------------------------------------------------------------------------
' Documentation for: RightClick
' Simulates a mouse right-click at a given coordinate. The X and Y co-ordinates are relative to the currently active window. Run the full version of AutoIt in reveal mode to determine the required co-ordinates of a window. To perform a double-click, simply run the command twice :)
DECLARE Sub RightClick LIB "AutoItDLL.dll" Alias "__RightClick@8" (  ByVal nX as long,  ByVal nY as long ) 

'-------------------------------------------------------------------------
' Documentation for: RightClickDrag
' Simulates a mouse right-click-drag operation. The X and Y co-ordinates are relative to the currently active window. Run the full version of AutoIt in reveal mode to determine the required co-ordinates of a window.
DECLARE Sub RightClickDrag LIB "AutoItDLL.dll" Alias "__RightClickDrag@16" (  ByVal nX1 as long,  ByVal nY1 as long,  ByVal nX2 as long,  ByVal nY2 as long ) 

'-------------------------------------------------------------------------
' Documentation for: Send
' Set an error message. The Error subroutine will add the string value passed by the calling program to the Error File and then terminate the calling program. The "Send" command syntax is similar to that of ScriptIt and the Visual Basic "SendKeys" command. Characters are sent as written with the exception of the following characters: !' This tells AutoIt to send an ALT keystroke, therefore Send "This is text!a" would send the keys "This is text" and then press "ALT+a". N.B. Some programs are very choosy about capital letters and ALT keys, i.e. "!A" is different to "!a". The first says ALT+SHIFT+A, the second is ALT+a. If in doubt, use lowercase! +' This tells AutoIt to send a SHIFT keystroke, therefore Send "Hell+o" would send the text "HellO". Send "!+a" would send "ALT+SHIFT+a". ^' This tells AutoIt to send a CONTROL keystroke, therefore Send "^!a" would send "CTRL+ALT+a". N.B. Some programs are very choosy about capital letters and CTRL keys, i.e. "^A" is different to "^a". The first says CTRL+SHIFT+A, the second is CTRL+a. If in doubt, use lowercase! #' The hash is used as a key delimiter to make a line easier to read. i.e. Send "H#e#l#l#o" is the same as Send "Hello". Certain special keys can be sent and should be enclosed in braces:
DECLARE Sub Send LIB "AutoItDLL.dll" Alias "__Send@4" (  ByVal szLine as string ) 

'-------------------------------------------------------------------------
' Documentation for: SetCapslockState
' This command will correctly set the state of the CAPSLOCK key to either on or off.
DECLARE Sub SetCapslockState LIB "AutoItDLL.dll" Alias "__SetCapslockState@4" (  ByVal nToggle as long ) 

'-------------------------------------------------------------------------
' Documentation for: SetKeyDelay
' This functions will alter the amount of time that AutoIt pauses between each simulated keypress.
DECLARE Sub SetKeyDelay LIB "AutoItDLL.dll" Alias "__SetKeyDelay@4" (  ByVal nDelay as long ) 

'-------------------------------------------------------------------------
' Documentation for: SetStoreCapslockMode
' By default, at the start of a "Send" command AutoIt will store the state of the CAPSLOCK key; at the end of the "Send" command this status will be restored. Use this command to turn off this behaviour.
DECLARE Sub SetStoreCapslockMode LIB "AutoItDLL.dll" Alias "__SetStoreCapslockMode@4" (  ByVal nToggle as long ) 

'-------------------------------------------------------------------------
' Documentation for: DetectHiddenText
' Some programs use hidden text on windows this can cause problems when trying to script them. This command allows you to tell AutoIt whether or not to detect this hidden text. When you use the "/reveal" mode of the full AutoIt product, you will see all text, including hidden text.
DECLARE Sub DetectHiddenText LIB "AutoItDLL.dll" Alias "__DetectHiddenText@4" (  ByVal nToggle as long ) 

'-------------------------------------------------------------------------
' Documentation for: IfWinActive
' Checks if a given window is currently active. Returns 1 if the given window was active, otherwise it returns 0.
DECLARE Function IfWinActive LIB "AutoItDLL.dll" Alias "__IfWinActive@8" (  ByVal szTitle as string,  ByVal szText as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IfWinExist
' Checks if a given window current exists (in any state). Returns 1 if the given window exists (regardless of its state), otherwise it returns 0.
DECLARE Function IfWinExist LIB "AutoItDLL.dll" Alias "__IfWinExist@8" (  ByVal szTitle as string,  ByVal szText as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SetTitleMatchMode
' This function will alter the way that AutoIt matches window titles in functions such as WinWait, IfWinActive, etc. <p>mode 1: In the szTitle string you specify the start of a window title to match. i.e. for the notepad.exe window (Untitled - Notepad), valid matches would be: Untitled", "Untitled -", "Unt" and "Untitled - Notepad".<p> <p>mode 2: In the szTitle string you can specify ANY substring of the window title you want to match. Again for the notepad.exe window valid matches would be: Untitled", "Untitled - Notepad", "Notepad", "No".<p>
DECLARE Sub SetTitleMatchMode LIB "AutoItDLL.dll" Alias "__SetTitleMatchMode@4" (  ByVal nMode as long ) 

'-------------------------------------------------------------------------
' Documentation for: SetWinDelay
' This functions will alter the amount of time that AutoIt pauses after performing a WinWait-type function.
DECLARE Sub SetWinDelay LIB "AutoItDLL.dll" Alias "__SetWinDelay@4" (  ByVal nDelay as long ) 

'-------------------------------------------------------------------------
' Documentation for: WinActivate
' This function will activate a specified window.
DECLARE Sub WinActivate LIB "AutoItDLL.dll" Alias "__WinActivate@8" (  ByVal szTitle as string,  ByVal szText as string ) 

'-------------------------------------------------------------------------
' Documentation for: WinClose
' This function will close a specified window
DECLARE Sub WinClose LIB "AutoItDLL.dll" Alias "__WinClose@8" (  ByVal szTitle as string,  ByVal szText as string ) 

'-------------------------------------------------------------------------
' Documentation for: WinGetActiveTitle
' A string pointer to receive the window title.
DECLARE Sub DLLWinGetActiveTitle LIB "AutoItDLL.dll" Alias "__WinGetActiveTitle@4" (  ByVal szTitle as string ) 

'-------------------------------------------------------------------------
' Documentation for: WinHide
' This function will hide a specified window.
DECLARE Sub WinHide LIB "AutoItDLL.dll" Alias "__WinHide@8" (  ByVal szTitle as string,  ByVal szText as string ) 

'-------------------------------------------------------------------------
' Documentation for: WinKill
' This function will forceably close a specified window.
DECLARE Sub WinKill LIB "AutoItDLL.dll" Alias "__WinKill@8" (  ByVal szTitle as string,  ByVal szText as string ) 

'-------------------------------------------------------------------------
' Documentation for: WinMaximize
' This function will maximize a specified window.
DECLARE Sub WinMaximize LIB "AutoItDLL.dll" Alias "__WinMaximize@8" (  ByVal szTitle as string,  ByVal szText as string ) 

'-------------------------------------------------------------------------
' Documentation for: WinMinimize
' This function will minimize a specified window.
DECLARE Sub WinMinimize LIB "AutoItDLL.dll" Alias "__WinMinimize@8" (  ByVal szTitle as string,  ByVal szText as string ) 

'-------------------------------------------------------------------------
' Documentation for: WinMinimizeAll
' This function will minimize all windows.
DECLARE Sub WinMinimizeAll LIB "AutoItDLL.dll" Alias "__WinMinimizeAll@0" (  ) 

'-------------------------------------------------------------------------
' Documentation for: WinMinimizeAllUndo
' This function will undo a previous WinMinimizeAll call.
DECLARE Sub WinMinimizeAllUndo LIB "AutoItDLL.dll" Alias "__WinMinimizeAllUndo@0" (  ) 

'-------------------------------------------------------------------------
' Documentation for: WinMove
' Use this function to move/resize a specified window.
DECLARE Sub WinMove LIB "AutoItDLL.dll" Alias "__WinMove@24" (  ByVal szTitle as string,  ByVal szText as string,  ByVal nX as long,  ByVal nY as long,  ByVal nWidth as long,  ByVal nHeight as long ) 

'-------------------------------------------------------------------------
' Documentation for: WinRestore
' This function will restore a window from a minimized state.
DECLARE Sub WinRestore LIB "AutoItDLL.dll" Alias "__WinRestore@8" (  ByVal szTitle as string,  ByVal szText as string ) 

'-------------------------------------------------------------------------
' Documentation for: WinSetTitle
' Changes the title of a specified window.
DECLARE Sub WinSetTitle LIB "AutoItDLL.dll" Alias "__WinSetTitle@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal szNewTitle as string ) 

'-------------------------------------------------------------------------
' Documentation for: WinShow
' This function will show a specified window previously hidden.
DECLARE Sub WinShow LIB "AutoItDLL.dll" Alias "__WinShow@8" (  ByVal szTitle as string,  ByVal szText as string ) 

'-------------------------------------------------------------------------
' Documentation for: WinWait
' This function will pause until the specified window exists (or the function times out).
DECLARE Function WinWait LIB "AutoItDLL.dll" Alias "__WinWait@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal nTimeout as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinWaitActive
' This function will pause until the specified window is active (or the function times out).
DECLARE Function WinWaitActive LIB "AutoItDLL.dll" Alias "__WinWaitActive@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal nTimeout as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinWaitNotActive
' This function will pause until the specified window is not active (or the function times out).
DECLARE Function WinWaitNotActive LIB "AutoItDLL.dll" Alias "__WinWaitNotActive@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal nTimeout as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: WinWaitClose
' This function will pause until the specified window doesn't exist (or the function times out).
DECLARE Function WinWaitClose LIB "AutoItDLL.dll" Alias "__WinWaitClose@12" (  ByVal szTitle as string,  ByVal szText as string,  ByVal nTimeout as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: IniDelete
' Deletes a specified value from a standard .INI file. <p>An INI file is taken to be a file of the following format: <pre> [SectionName] Value=Result </pre> The full path of the INI file must be given, however, it is acceptable to use something similar to ".\myfile.ini" to indicate a file in the current working directory. </p> See also IniRead, IniWrite
DECLARE Sub IniDelete LIB "AutoItDLL.dll" Alias "__IniDelete@12" (  ByVal szFile as string,  ByVal szSection as string,  ByVal szValue as string ) 

'-------------------------------------------------------------------------
' Documentation for: IniRead
' Reads a specified value from a standard .INI file.
DECLARE Sub DLLIniRead LIB "AutoItDLL.dll" Alias "__IniRead@16" (  ByVal szFile as string,  ByVal szSection as string,  ByVal szValue as string,  ByVal szResult as string ) 

'-------------------------------------------------------------------------
' Documentation for: IniWrite
' Write a specified value to a standard .INI file. <p>An INI file is taken to be a file of the following format: <pre> [SectionName] Value=Result </pre> The full path of the INI file must be given, however, it is acceptable to use something similar to ".\myfile.ini" to indicate a file in the current working directory. </p>
DECLARE Sub IniWrite LIB "AutoItDLL.dll" Alias "__IniWrite@16" (  ByVal szFile as string,  ByVal szSection as string,  ByVal szValue as string,  ByVal szResult as string ) 

'-------------------------------------------------------------------------
' Documentation for: ClipGet
' Allows you to get text from the clipboard. A maximum of 4096 characters of text can be received; ensure that the text buffer you pass is at least this large.
DECLARE Sub DLLClipGet LIB "AutoItDLL.dll" Alias "__ClipGet@4" (  ByVal szText as string ) 

'-------------------------------------------------------------------------
' Documentation for: ClipPut
' Allows you to get text from the clipboard. A maximum of 4096 characters of text can be received; ensure that the text buffer you pass is at least this large.
DECLARE Sub ClipPut LIB "AutoItDLL.dll" Alias "__ClipPut@4" (  ByVal szText as string ) 

Sub WinGetActiveTitle ( szTitle as string   ) 
      Dim retStr1 as String * MAXFIELD
      call DLLWinGetActiveTitle(retstr1)
      szTitle=StripTerminator(retStr1)
End  Sub 

Sub IniRead (  ByVal szFile as string  ,  ByVal szSection as string  ,  ByVal szValue as string  , szResult as string   ) 
      Dim retStr4 as String * MAXFIELD
      call DLLIniRead(szFile, szSection, szValue, retstr4)
      szResult=StripTerminator(retStr4)
End  Sub 

Sub ClipGet ( szText as string   ) 
      Dim retStr1 as String * MAXFIELD
      call DLLClipGet(retstr1)
      szText=StripTerminator(retStr1)
End  Sub 
