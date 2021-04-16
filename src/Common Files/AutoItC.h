#ifndef __AUTOIT_H
#define __AUTOIT_H
#include <windows.h>

///////////////////////////////////////////////////////////////////////////////
//
// AutoIt.h
// (c)1999-2002 Jonathan Bennett
//
// Web  : http://www.hiddensoft.com/AutoIt/
// Email: support@hiddensoft.com
//        jbennett@hidden.demon.co.uk
//
///////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////
// Useful defines
///////////////////////////////////////////////////////////////////////////////

#define AUTOIT_LINESIZE	16384	// Max size of a line of text
								// If you will receive any data from AutoItDLL
								// e.g. WinGetActiveTitle, ensure that you have
								// a buffer of this size for the result


#ifdef __cplusplus
		#define AUTOIT_API extern "C"
#else
		#define AUTOIT_API
#endif


///////////////////////////////////////////////////////////////////////////////
// Exported functions
///////////////////////////////////////////////////////////////////////////////

AUTOIT_API void WINAPI AUTOIT_BlockInput(int nToggle);

AUTOIT_API void WINAPI AUTOIT_ClipGet(char *szText);
AUTOIT_API void WINAPI AUTOIT_ClipPut(char *szText);
AUTOIT_API void WINAPI AUTOIT_Close(void);


AUTOIT_API void WINAPI AUTOIT_DetectHiddenText(int nToggle);

AUTOIT_API int  WINAPI AUTOIT_IfWinActive(char *szTitle, char *szText);
AUTOIT_API int  WINAPI AUTOIT_IfWinExist(char *szTitle, char *szText);
AUTOIT_API void WINAPI AUTOIT_Init(void);
AUTOIT_API void WINAPI AUTOIT_IniDelete(char *szFile, char *szSection, char *szValue);
AUTOIT_API void WINAPI AUTOIT_IniRead(char *szFile, char *szSection, char *szValue, char *szResult);
AUTOIT_API void WINAPI AUTOIT_IniWrite(char *szFile, char *szSection, char *szValue, char *szResult);

AUTOIT_API void WINAPI AUTOIT_LeftClick(int nX, int nY);
AUTOIT_API void WINAPI AUTOIT_LeftClickDrag(int nX1, int nY1, int nX2, int nY2);

AUTOIT_API void WINAPI AUTOIT_MouseMove(int nX, int nY);
AUTOIT_API int  WINAPI AUTOIT_MouseGetPosX();
AUTOIT_API int  WINAPI AUTOIT_MouseGetPosY();

AUTOIT_API void WINAPI AUTOIT_RightClick(int nX, int nY);
AUTOIT_API void WINAPI AUTOIT_RightClickDrag(int nX1, int nY1, int nX2, int nY2);

AUTOIT_API void WINAPI AUTOIT_Send(char *szLine);
AUTOIT_API void WINAPI AUTOIT_SetCapslockState(int nToggle);
AUTOIT_API void WINAPI AUTOIT_SetKeyDelay(int nDelay);
AUTOIT_API void WINAPI AUTOIT_SetStoreCapslockMode(int nToggle);
AUTOIT_API void WINAPI AUTOIT_SetTitleMatchMode(int nMode);
AUTOIT_API void WINAPI AUTOIT_SetWinDelay(int nDelay);
AUTOIT_API void WINAPI AUTOIT_Shutdown(int nFlag);
AUTOIT_API void WINAPI AUTOIT_Sleep(int nMilli);

AUTOIT_API int  WINAPI AUTOIT_WinWait(char *szTitle, char *szText, int nTimeout);
AUTOIT_API int  WINAPI AUTOIT_WinWaitActive(char *szTitle, char *szText, int nTimeout);
AUTOIT_API int  WINAPI AUTOIT_WinWaitNotActive(char *szTitle, char *szText, int nTimeout);
AUTOIT_API int  WINAPI AUTOIT_WinWaitClose(char *szTitle, char *szText, int nTimeout);
AUTOIT_API void WINAPI AUTOIT_WinHide(char *szTitle, char *szText);
AUTOIT_API void WINAPI AUTOIT_WinRestore(char *szTitle, char *szText);
AUTOIT_API void WINAPI AUTOIT_WinMinimize(char *szTitle, char *szText);
AUTOIT_API void WINAPI AUTOIT_WinMaximize(char *szTitle, char *szText);
AUTOIT_API void WINAPI AUTOIT_WinMinimizeAll();
AUTOIT_API void WINAPI AUTOIT_WinActivate(char *szTitle, char *szText);
AUTOIT_API void WINAPI AUTOIT_WinClose(char *szTitle, char *szText);
AUTOIT_API void WINAPI AUTOIT_WinMove(char *szTitle, char *szText, int nX,
					int nY, int nWidth, int nHeight);
AUTOIT_API void WINAPI AUTOIT_WinSetTitle(char *szTitle, char *szText, char *szNewTitle);
AUTOIT_API void WINAPI AUTOIT_WinGetActiveTitle(char *szTitle);
AUTOIT_API void WINAPI AUTOIT_WinShow(char *szTitle, char *szText);
AUTOIT_API void WINAPI AUTOIT_WinKill(char *szTitle, char *szText);
AUTOIT_API void WINAPI AUTOIT_WinMinimizeAllUndo();


///////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////

#endif
