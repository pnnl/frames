Attribute VB_Name = "FNOTIF1"
'ToDo: Enable enhanced code in FNotifyMsg_To_MSGDATA after v1.5.2 is distributed.
'ToDo: Modify the old-style functions to call the new functions.
'ToDo: Move some constants to K_NOTIF.BAS?


'*** Warning:  If you change this file, contact LaMar Stoops (375-2843).
'              Otherwise, your changes may be destroyed by the next update.
'
'== ##BEGIN_FILE_HEADER## ====================================================
'$Header: q:/kp_api/rcs/fnotif1.bas 1.1 2005/05/27 16:31:12Z d3k077 Exp d3k077 $
'
'$RCSfile: fnotif1.bas $
'
'Abstract:   FEMIS Notification Service functions and declarations
'
'Subsystem:  Application Manager
'Module:     Notification Service
'Unit:
'
'== ##END_FILE_HEADER## ======================================================

'== ##BEGIN_FILE_NOTES## =====================================================
'
'== ##END_FILE_NOTES## =======================================================

'== ##BEGIN_FILE_HISTORY## ===================================================
'
'  date    by     description
'--------  ---  ----------------------------------------
'07/21/94  LRS  File created.
'01/22/99  LRS  Added NsGetLinkServerInfo() function.
'01/25/99  LRS  Added functions to register and remove interests for all hazards.
'10/05/99  LRS  Added stFolderID to the NS_DATACHG_PARAMS and NS_ACTION_PARAMS structures.
'10/18/99  LRS  Fixed the missing AuxData3-5 arguments in NSPostActionMsg and NSPostDataChangeMsg functions (SEPR 27630).
'10/18/99  LRS  Modified the NSPostActionMsg and NSPostDataChangeMsg functions to set FolderID info.
'10/18/99  LRS  Modified the NSPostActionMsg and NSPostDataChangeMsg functions to support an optional FolderID argument.
'12/07/99  LRS  Added the AnX001, AnX002, and AnX003 functions.
'03/20/00  LRS  Added support for data acknowledgement strings in the NSPostXxxMsg functions.
'11/15/00  SAS  Strip the CR out of the Acknowledgement strings.
'12/08/00  LRS  Modified NsPostActionMsgDD and NsPostDataChangeMsgDD to use unambiguous date formats (SEPR 30260).
'12/27/00  LRS  Modified NsGetMsgDDParams so correct data data is used for XmitInitDate(n).
'12/27/00  LRS  Added new functions to set and copy message info.
'07/26/01  LRS  Added the NS_EF_MY_PROCESS_ONLY constant.
'08/08/01  LRS  Removed history for 1994-1998 to save space.
'08/09/01  LRS  Added a declaration for the AnZF01 function.
'01/06/02  LRS  Added the iNSRegisterInterestAllExHz and iNSRemoveInterestAllExHz functions.
'05/20/02  LRS  Added functions to support FNotifyMsg message objects.
'05/29/02  LRS  Removed the iNSGetEventBoardData, iNSGetEventData, iNSGetQLinkEvent, iFEVTDATA_To_NSEVENT, iNSEVENT_To_FEVTDATA, iVBToByteStr, LPSZ_To_VBStr functions.
'05/29/02  LRS  Removed the stVBToCStr, vNSQueryEventBoardTime, vTimeSMToVB, ulNSOpenQLinkX functions.
'05/29/02  LRS  Removed the Ns_FEVTDATA_to_MSGDATA, Ns_MSGDATA_to_FEVTDATA, Ns_CTimeToVBTime, NsSetFevtDDParams functions.
'06/24/02  LRS  Changed all message boxes to use ifxMsgBox (SEPR 33303).
'07/18/02  LRS  Added a local ifxMsgBox function to eliminate "Undefined function" errors.
'== ##END_FILE_HISTORY## =====================================================

Option Explicit

'....RCS file identifier
'Const FNOTIF1_BAS_RCS_ID = "$Id: fnotif1.bas 1.1 2005/05/27 16:31:12Z d3k077 Exp d3k077 $"

'....Structure definitions
Type NS_DD_PARAMS                       '** data-driven notification parameters **
    stTableName(3)      As String       'table names
    stRowID(3)          As String       'row IDs
    vXmitInitDate(3)    As Date         'timestamps
    stAction(3)         As String       'actions
    stListener          As String       'database listener name
    lDDFlags            As Long         'data-driven flags
    stEOCCode           As String       'EOC code
    stDDHandler         As String       'server DD handler key
End Type

Type NS_DATACHG_PARAMS                  '** DataChange notification message data **
    stNotifName     As String           'notification name
    stChangeType    As String           'change type
    stDatasetName   As String           'name of dataset
    vChangeDate     As Date             'date-time of change
    stItemID1       As String           'item ID 1
    stItemID2       As String           'item ID 2
    stAuxData(5)    As String           'auxiliary data
    tDDParams       As NS_DD_PARAMS     'data-driven parameters
    stSiteCode      As String           'site code
    stEOCCode       As String           'EOC code
    stHazardID      As String           'hazard ID
    stExerID        As String           'exercise ID
    stFolderID      As String           'folder ID
End Type

Type NS_ACTION_PARAMS                  '** Action notification message data **
    stActionName    As String           'action name
    stActionType    As String           'action type
    stActionStatus  As String           'action status
    vActionDate     As Date             'action date-time
    stItemID1       As String           'item ID 1
    stItemID2       As String           'item ID 2
    stAuxData(5)    As String           'auxiliary data
    tDDParams       As NS_DD_PARAMS     'data-driven parameters
    stSiteCode      As String           'site code
    stEOCCode       As String           'EOC code
    stHazardID      As String           'hazard ID
    stExerID        As String           'exercise ID
    stFolderID      As String           'folder ID
End Type

Type NS_MSG_DATA                        '** generic notification message data **
    ulMsgID         As Long             'message ID
    vMsgTime        As Date             'message timestamp
    ulMsgFlags      As Long             'message flags
    stMsgClass      As String           'message class
    stMsgName       As String           'message name
    stMsgDest       As String           'message destination
    stExerID        As String           'message exercise ID
    stAuxProcessIdent As String         'auxilliary process identifier
    ulAuxProcessFlags As Long           'auxilliary process flags
    stMsgParm(50)   As String           'message parameter strings
End Type

Type NSEVENT                            '** notification message data (old format) **
    ulSrcID    As Long                  'source ID
    vEvtTime   As Date                  'message timestamp
    ulRecID    As Long                  'record ID
    ulMsgFlags As Long                  'message flags
    stEvtName  As String                'message name
    stExerID   As String                'exercise ID
    stParm1    As String                'parameter 1
    stParm2    As String                'parameter 2
    stParm3    As String                'parameter 3
End Type

' Notification DLL message data structures
Type FEVTDATA2                          '** new DLL notification message structure **
    ' message header
    usRecType       As Integer          'record type code
    usRecLen        As Integer          'record length
    usRecFmt        As Integer          'record format code
    usRecFlags      As Integer          'record flags
    ulSrcID         As Long             'source ID
    ulClientID      As Long             'client ID
    ulHostID        As Long             'host ID
    ulMsgTime       As Long             'message timestamp (sec)
    usMsgMsec       As Integer          'message timestamp (fraction)
    usRsvd0         As Integer          'reserved
    ulRsvd1         As Long             'reserved
    ulRsvd2         As Long             'reserved
    ulRecID         As Long             'message ID
    ' message content
    ulMsgFlags      As Long             'message flags
    stMsgName      As Long             'message name
    pszExerID       As Long             'message exercise ID
    pszAuxProcessIdent As Long          'auxilliary process identifier
    ulAuxProcessFlags As Long           'auxilliary process flags
    iMsgParmArrLen  As Long             'message parameter strings
    pszMsgParm(50)  As Long             'message parameter strings
    ' internal data
    ulXtra1         As Long             'reserved
    ulXtra2         As Long             'reserved
    tMemPool(2000)  As Byte             'memory pool area
    tSafety(32)     As Byte             'extra length for safety
End Type

Type FEVTDATA                           '** old DLL notification message structure **
    usRecType       As Integer          'record type code
    usRecLen        As Integer          'record length
    usRecFmt        As Integer          'record format code
    usRecFlags      As Integer          'record flags
    ulSrcID         As Long             'source ID
    ulClientID      As Long             'client ID
    ulHostID        As Long             'host ID
    ulEvtTime       As Long             'message time (sec)
    usEvtMsec       As Integer          'message time (fraction)
    usRsvd0         As Integer          'reserved
    ulRsvd1         As Long             'reserved
    ulRsvd2         As Long             'reserved
    ulRecID         As Long             'record ID
    ulMsgFlags      As Long             'message flags
    szEvtName(31)   As Byte             'message name
    szExerID(11)    As Byte             'exercise ID
    szParm1(15)     As Byte             'parameter 1
    szParm2(15)     As Byte             'parameter 2
    szParm3(15)     As Byte             'parameter 3
    ulXtra1         As Long             'reserved
    ulXtra2         As Long             'reserved
End Type

'....FEVTDATA format codes
Global Const FEVTDATA_FMT_1 = &H100             'format 1 code
Global Const FEVTDATA_FMT_2 = &H200             'format 2 code
Global Const FEVTDATA_FMT_STD = FEVTDATA_FMT_1  'standard format code


'....Global Constants
' notification message type codes
Global Const NS_MT_EVENTMSG = 5

' message attribute flags
Global Const NS_EF_NORMAL           As Long = &H0
Global Const NS_EF_LOCAL_ONLY       As Long = &H1
Global Const NS_EF_MY_PROCESS_ONLY  As Long = &H10
Global Const NS_EF_SVR_NOPOST       As Long = &H100000
Global Const NS_EF_TO_AUX_PROCESS   As Long = &H1000000
Global Const NS_EF_GLOBAL           As Long = &H10000000
Global Const NS_EF_DEFAULT          As Long = NS_EF_NORMAL

' link control flags
Global Const NS_LF_NORMAL               As Long = &H0
Global Const NS_LF_LOOPBACK_OK          As Long = &H2
Global Const NS_LF_LATEST_INSTANCE_ONLY As Long = &H1000
Global Const NS_LF_DEFAULT              As Long = NS_LF_NORMAL

' message queue flags
Global Const NS_QF_DEFAULT  As Long = &H0

'....Global data
Global gstNotifWinMsgName As String
Global giNotifWinMsgNo As Integer               'window message number

'....Notification DLL function declarations
' Message functions
Declare Function AnPostEvent Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2) As Long
Declare Function AnPostEventLF Lib "FNOTIF32.DLL" (ByVal ulLinkID As Long, ByVal ulMsgFlags As Long, pEvtData As FEVTDATA2) As Long
Declare Function AnPostEvent3P Lib "FNOTIF32.DLL" (ByVal ulLinkID As Long, ByVal ulMsgFlags As Long, ByVal stMsgName As String, ByVal pszExerID As String, ByVal pszParm1 As String, ByVal pszParm2 As String, ByVal pszParm3 As String) As Long
Declare Function AnPostEventNV Lib "FNOTIF32.DLL" (ByVal ulLinkID As Long, ByVal ulMsgFlags As Long, ByVal stMsgName As String, ByVal pszExerID As String, ByVal lParmCt As Long, ByVal pszParmArrBase As String) As Long
Declare Function AnQueryEventBuffer Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2, ByVal ulRecID As Long) As Long
Declare Function AnQueryEventBoard Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2, ByVal stMsgName As String, ByVal pszExerID As String) As Long
Declare Function AnQueryEventBoardTimeSec Lib "FNOTIF32.DLL" (ByVal stMsgName As String, ByVal pszExerID As String) As Long
Declare Function AnIsQLinkEvent Lib "FNOTIF32.DLL" (ByVal ulLinkID As Long, ByVal ulRecID As Long) As Long
Declare Function AnGetQLinkEvent Lib "FNOTIF32.DLL" (ByVal ulLinkID As Long, pEvtData As FEVTDATA, ByVal ulRecID As Long) As Long
Declare Function AnGetQLinkEvent2 Lib "FNOTIF32.DLL" Alias "AnGetQLinkEvent" (ByVal ulLinkID As Long, pEvtData As FEVTDATA2, ByVal ulRecID As Long) As Long
Declare Function AnReleaseEvent Lib "FNOTIF32.DLL" (ByVal ulRecID As Long) As Long

' Link management
Declare Function AnOpenLinkWM Lib "FNOTIF32.DLL" (ByVal ulLinkFlags As Long, ByVal hWinHdl As Long, ByVal uWinMsg As Long) As Long
Declare Function AnOpenLinkWMQ Lib "FNOTIF32.DLL" (ByVal ulLinkFlags As Long, ByVal hWinHdl As Long, ByVal uWinMsg As Long) As Long
Declare Function AnOpenLinkWMQF Lib "FNOTIF32.DLL" (ByVal ulLinkFlags As Long, ByVal hWinHdl As Long, ByVal uWinMsg As Long, ByVal ulQueueLimit As Long, ByVal ulQueueFlags As Long) As Long
Declare Function AnCloseLink Lib "FNOTIF32.DLL" (ByVal ulLinkID As Long) As Long

Declare Function AnOpenServerLink Lib "FNOTIF32.DLL" (ByVal stServerHost As String, ByVal usServerPort As Long, ByVal ulLinkFlags As Long) As Long
Declare Function AnCloseServerLink Lib "FNOTIF32.DLL" (ByVal stServerHost As String, ByVal usServerPort As Long) As Long
Declare Function AnGetLinkServerInfo Lib "FNOTIF32.DLL" (ByVal ulLinkID As Long, plServerID As Long, ByVal pszServerHost As String, plServerPort As Long) As Long

' Message interests
Declare Function AnRegisterInterest Lib "FNOTIF32.DLL" (ByVal ulLinkID As Long, ByVal stMsgName As String, ByVal pszExerID As String) As Long
Declare Function AnRemoveInterest Lib "FNOTIF32.DLL" (ByVal ulLinkID As Long, ByVal stMsgName As String, ByVal pszExerID As String) As Long
Declare Function AnVerifyInterest Lib "FNOTIF32.DLL" (ByVal ulLinkID As Long, ByVal stMsgName As String, ByVal pszExerID As String) As Long

' Message Queue functions
Declare Function AnFlushEventQueues Lib "FNOTIF32.DLL" () As Long

' Support functions
Declare Function AnClientInit Lib "FNOTIF32.DLL" () As Long
Declare Function AnClientCleanup Lib "FNOTIF32.DLL" () As Long

' FEVTDATA functions
Declare Function AnInitFEVTDATA Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA, ByVal iFmtCode As Long) As Long
Declare Function AnInitFEVTDATA2 Lib "FNOTIF32.DLL" Alias "AnInitFEVTDATA" (pEvtData As FEVTDATA2, ByVal iFmtCode As Long) As Long

Declare Function AnFEVTDATA_MsgName Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2) As Long
Declare Function AnFEVTDATA_MsgDest Lib "FNOTIF32.DLL" Alias "AnFEVTDATA_ExerID" (pEvtData As FEVTDATA2) As Long
Declare Function AnFEVTDATA_ExerID Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2) As Long
Declare Function AnFEVTDATA_MsgParm Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2, ByVal iIndex As Long) As Long
Declare Function AnFEVTDATA_MsgParmArrLen Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2) As Long
Declare Function AnFEVTDATA_AuxProcessIdent Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2) As Long

Declare Function AnFEVTDATA_SetMsgName Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2, ByVal pszStr As String) As Long
Declare Function AnFEVTDATA_SetMsgDest Lib "FNOTIF32.DLL" Alias "AnFEVTDATA_SetExerID" (pEvtData As FEVTDATA2, ByVal pszStr As String) As Long
'Declare Function AnFEVTDATA_SetExerID Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2, ByVal pszStr As String) As Long
Declare Function AnFEVTDATA_SetMsgParm Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2, ByVal iIndex As Long, ByVal pszStr As String) As Long
Declare Function AnFEVTDATA_SetAuxProcessIdent Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2, ByVal pszStr As String) As Long

' Status functions
'Declare Function AnEvtBrdTotRecCt Lib "FNOTIF32.DLL" () As Long
'Declare Function AnEvtBrdCurRecCt Lib "FNOTIF32.DLL" () As Long
'Declare Function AnEvtBrdPeakRecCt Lib "FNOTIF32.DLL" () As Long
'Declare Function AnEvtBufTotRecCt Lib "FNOTIF32.DLL" () As Long
'Declare Function AnEvtBufCurRecCt Lib "FNOTIF32.DLL" () As Long
'Declare Function AnEvtBufPeakRecCt Lib "FNOTIF32.DLL" () As Long
'Declare Function AnIntListTotRecCt Lib "FNOTIF32.DLL" () As Long
'Declare Function AnIntListCurRecCt Lib "FNOTIF32.DLL" () As Long
'Declare Function AnIntListPeakRecCt Lib "FNOTIF32.DLL" () As Long
'Declare Function AnLinkTblTotRecCt Lib "FNOTIF32.DLL" () As Long
'Declare Function AnLinkTblCurRecCt Lib "FNOTIF32.DLL" () As Long
'Declare Function AnLinkTblPeakRecCt Lib "FNOTIF32.DLL" () As Long
'Declare Function AnEvtTagTblTotRecCt Lib "FNOTIF32.DLL" () As Long
'Declare Function AnEvtTagTblCurRecCt Lib "FNOTIF32.DLL" () As Long
'Declare Function AnEvtTagTblPeakRecCt Lib "FNOTIF32.DLL" () As Long

' DLL info functions
Declare Function AnDLLRefCt Lib "FNOTIF32.DLL" () As Long
Declare Function AnGetDLLTitle Lib "FNOTIF32.DLL" (ByVal pszDestBuf As String, ByVal iMaxLen As Long) As Long
Declare Function AnGetDLLVersion Lib "FNOTIF32.DLL" (ByVal pszDestBuf As String, ByVal iMaxLen As Long) As Long
Declare Function AnGetDLLBuildInfo Lib "FNOTIF32.DLL" (ByVal pszDestBuf As String, ByVal iMaxLen As Long) As Long

' Utility functions
Declare Function AnSecMsecToCTimeStr Lib "FNOTIF32.DLL" (ByVal ulSec As Long, ByVal usMsec As Long, ByVal pszDestBuf As String) As Long
Declare Function AnStrNCpyZ Lib "FNOTIF32.DLL" (ByVal pszDest As String, ByVal pszSrc As Any, ByVal iMaxLen As Integer) As Long

' Reserved functions
Declare Function AnX001 Lib "FNOTIF32.DLL" (ByVal L1 As Long, ByVal L2 As Long, ByVal L3 As Long, ByVal L4 As Long, ByVal S1 As String, ByVal S2 As String) As Long
Declare Function AnX002 Lib "FNOTIF32.DLL" (ByVal L1 As Long, ByVal L2 As Long, ByVal L3 As Long, ByVal L4 As Long, ByVal S1 As String, ByVal S2 As String) As String
Declare Function AnX003 Lib "FNOTIF32.DLL" (ByVal L1 As Long, ByVal L2 As Long, ByVal L3 As Long, ByVal L4 As Long, ByVal v1 As Any, ByVal V1Len As Long, ByVal v2 As Any, ByVal V2Len As Long) As Long
Declare Function AnZF01 Lib "FNOTIF32.DLL" (ByVal FCode As Long, ByRef Arg1 As Variant, ByRef Arg2 As Variant, ByRef Arg3 As Variant, ByRef Arg4 As Variant, ByRef Arg5 As Variant, ByRef Arg6 As Variant) As Variant

' Message destination constants
'(these MUST be synchronized with NsGenFEMISMsgDest() !!!)
Global Const NS_FEMIS_MSGDEST_PATTERN = "/femis/<hazard_name>/<site_name>/Exer.<exercise_num>"
Global Const NS_MSGDEST_ROOT = "/femis"
Global Const NS_MSGDEST_EXER_PREFIX = "/Exer."

'....Data-Driven notification constants
'Message class names
Global Const NS_MSG_CLASS_ACTION = "Action"
Global Const NS_MSG_CLASS_DATA_CHANGE = "DataChange"

'DD handler
Global Const NS_DDHANDLER_ACTION = "Action"
Global Const NS_DDHANDLER_DATA_CHANGE = "DataChange"

'DD flags
Global Const NS_DD_USE_DD = &H2             'USE DD flag
Global Const NS_DD_SENT = &H8               'DATA SENT flag
Global Const NS_DD_ARRIVED = &H10           'DATA ARRIVED flag
Global Const NS_DD_NOT_ARRIVED = &H20       'DATA TIMEOUT flag
Global Const NS_DD_DAI_JOURNAL = &H40       'DAI JOURNAL flag

'DD data format
Global Const NS_DD_XmitInitDate_FMT = "DD-MMM-YYYY HH:MM:SS"

' Generic message parameter indexes
Global Const NS_MsgClass_IDX = 0

' Action message parameter indexes
Global Const NS_AC_ActionType_IDX = 20
Global Const NS_AC_ActionStatus_IDX = 21
Global Const NS_AC_ActionDate_IDX = 22
Global Const NS_AC_ItemID1_IDX = 23
Global Const NS_AC_ItemID2_IDX = 24
Global Const NS_AC_AuxData1_IDX = 25
Global Const NS_AC_AuxData2_IDX = 26
Global Const NS_AC_AuxData3_IDX = 27
Global Const NS_AC_AuxData4_IDX = 28
Global Const NS_AC_AuxData5_IDX = 29
Global Const NS_AC_ExerID_IDX = 30
Global Const NS_AC_SiteCode_IDX = 31
Global Const NS_AC_EOCCode_IDX = 32
Global Const NS_AC_HazardID_IDX = 33
Global Const NS_AC_FolderID_IDX = 34
Global Const NS_AC_AckFlag_IDX = 35
Global Const NS_AC_AckString_IDX = 36

' DataChange message parameter indexes
Global Const NS_DC_ChangeType_IDX = 20
Global Const NS_DC_DatasetName_IDX = 21
Global Const NS_DC_ChangeDate_IDX = 22
Global Const NS_DC_ItemID1_IDX = 23
Global Const NS_DC_ItemID2_IDX = 24
Global Const NS_DC_AuxData1_IDX = 25
Global Const NS_DC_AuxData2_IDX = 26
Global Const NS_DC_AuxData3_IDX = 27
Global Const NS_DC_AuxData4_IDX = 28
Global Const NS_DC_AuxData5_IDX = 29
Global Const NS_DC_ExerID_IDX = 30
Global Const NS_DC_SiteCode_IDX = 31
Global Const NS_DC_EOCCode_IDX = 32
Global Const NS_DC_HazardID_IDX = 33
Global Const NS_DC_FolderID_IDX = 34
Global Const NS_DC_AckFlag_IDX = 35
Global Const NS_DC_AckString_IDX = 36

' Data-Driven message parameter indexes
Global Const NS_DD_TblName1_IDX = 1
Global Const NS_DD_TblName2_IDX = 2
Global Const NS_DD_TblName3_IDX = 3
Global Const NS_DD_RowID1_IDX = 4
Global Const NS_DD_RowID2_IDX = 5
Global Const NS_DD_RowID3_IDX = 6
Global Const NS_DD_XmitInitDate1_IDX = 7
Global Const NS_DD_XmitInitDate2_IDX = 8
Global Const NS_DD_XmitInitDate3_IDX = 9
Global Const NS_DD_Action1_IDX = 10
Global Const NS_DD_Action2_IDX = 11
Global Const NS_DD_Action3_IDX = 12
Global Const NS_DD_EOCCode_IDX = 13
Global Const NS_DD_Listener_IDX = 14
Global Const NS_DD_DDFlags_IDX = 15
Global Const NS_DD_DDHandler_IDX = 16



Sub InitNotifGlobals()
    '-------------------------------------------------------------------------
    'Purpose: Initialize Notification Service globals
    '
    'Usage:   InitNotifGlobals()
    '
    'Inputs:  none
    '
    'Outputs: none
    '
    'Returns: none
    '
    'Description:
    '   This function initializes global Notification parameters, etc., and
    '   should be called before using any other Notification functions.
    '
    '   Note:  No harm will be done by calling this function multiple times
    '   during an application.
    '-------------------------------------------------------------------------

    Static fInitDone As Integer         'INIT DONE flag
    Dim stTemp As String                'string buffer
    Dim lTemp As Long
    Dim iRC As Integer                  'temp return code

    ' Exit if INIT already done
    If fInitDone Then
        Exit Sub
    End If

    ' Create notification window message number
    gstNotifWinMsgName = "FNotifWinMsg" 'window message name
    'giNotifWinMsgNo = RegisterWindowMessage(gstNotifWinMsgName)

    ' Get default notification info
    'stTemp = String$(256, 0)             'create buffer
    'iRC = GetPrivateProfileString("Notification", "DefaultNotifServerHost", "", stTemp, Len(stTemp) - 1, "fnotif1.ini")
    'stTemp = stAnsiStrToVB(stTemp)
    'gstDfltNotifServerHost = stTemp      'save name

    ' Initialize client
    iRC = iNSClientInit()

    ' Set INIT DONE flag
    fInitDone = 1
End Sub

Function iFEVTDATA_To_NSEVENT(FEvt As FEVTDATA, NSEvt As NSEVENT) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Convert a notification message data to VB format
    '
    'Usage:  <integer> = iFEVTDATA_To_NSEVENT (FEvt, NSEvt)
    '
    'Inputs:  FEVTDATA FEvt     - C message data structure
    '         NSEVENT  NSEvt    - VB message data structure
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '
    'Description:
    '   This function converts notification message data from C format to VB
    '   format.
    '
    'Warning:  This function is for internal use only and may change without
    ' notice.
    '-------------------------------------------------------------------------

    ' Copy message data to VB format
    With NSEvt
        .ulSrcID = FEvt.ulSrcID
        .ulMsgFlags = FEvt.ulMsgFlags
        .vEvtTime = Ns_CTimeToVBTime(FEvt.ulEvtTime)
        .ulRecID = FEvt.ulRecID
        .stEvtName = stAnsiStrToVB(FEvt.szEvtName)
        .stExerID = stAnsiStrToVB(FEvt.szExerID)
        .stParm1 = stAnsiStrToVB(FEvt.szParm1)
        .stParm2 = stAnsiStrToVB(FEvt.szParm2)
        .stParm3 = stAnsiStrToVB(FEvt.szParm3)
    End With
End Function

Function iNSClientCleanup() As Integer
    '-------------------------------------------------------------------------
    'Purpose: Do cleanup when client application closes
    '
    'Usage:   <integer> = iNSClientCleanup()
    '
    'Inputs:  none
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '
    'Description:
    '   This function performs necessary cleanup when a client application
    '   closes, including release of WinSock task info structures.
    '
    'Warning:  This function should be called only once, during application
    ' shutdown.  Unpredictable errors will occur if it is used at any other time.
    '-------------------------------------------------------------------------

    ' Do client cleanup
    iNSClientCleanup = AnClientCleanup()
End Function

Function iNSClientInit() As Integer
    '-------------------------------------------------------------------------
    'Purpose: Do initialization when client application opens
    '
    'Usage:   <integer> = iNSClientInit()
    '
    'Inputs:  none
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '
    'Description:
    '   This function performs necessary initialization when a client application
    '   opens, including allocation of WinSock task info structures.
    '
    'Warning:  This function should be called only once, during application
    ' startup.  Unpredictable errors will occur if it is used at any other time.
    '-------------------------------------------------------------------------

    ' Initialize client
    iNSClientInit = AnClientInit()
End Function

Function iNSCloseLink(ByVal ulLinkID As Long) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Close a notification client link
    '
    'Usage:   <integer> = iNSCloseLink (ulLinkID)
    '
    'Inputs:  long ulLinkID     - notification link ID
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '
    'Description:
    '   This function closes a notification client link and releases all
    '   resources associated with the link.
    '
    'Caution:  Each VB form that creates a notification link should call
    ' iNSCloseLink() in its shutdown process (Form_Unload event).  Otherwise,
    ' the notification link becomes an orphan and its resources are never
    ' released.
    '-------------------------------------------------------------------------

    ' Close link
    iNSCloseLink = AnCloseLink(ulLinkID)
End Function

Function iNSCloseServerLink(ByVal stServerHost As String, ByVal iServerPort As Integer) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Close link from local message manager to a notification server
    '
    'Usage:   <integer> = iNSCloseServerLink (stServerHost, iServerPort)
    '
    'Inputs:  string  stServerHost - server host name
    '         integer iServerPort  - server port
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '
    'Description:
    '   This function closes the link from your local message manager to a
    '   notification server and releases all resources associated with the
    '   link.
    '
    '   Since server links are automatically closed when the message manager
    '   is closed, this function should only be called when a server link
    '   is actually being discontinued.
    '
    'Warning:  This is a system-level function and should never be called
    ' by task-level code.
    '-------------------------------------------------------------------------

    ' Close link
    iNSCloseServerLink = AnCloseServerLink(stServerHost, iServerPort)
End Function

Function iNSGetQLinkEvent(ByVal ulLinkID As Long, ByVal ulRecID As Long, NSEvt As NSEVENT) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Get message data from a notification client's message queue
    '
    'Usage:   <integer> = iNSGetQLinkEvent (ulLinkID, ulRecID, NSEvt)
    '
    'Inputs:  long    ulLinkID  - link ID
    '         long    ulRecID   - message record ID
    '         NSEVENT NSEvt     - message data structure
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '
    'Description:
    '   This function retrieves message data for a notification message, based
    '   on link ID and a record ID.
    '
    '   If the message data cannot be retrieved, the function returns a negative
    '   error code.
    '-------------------------------------------------------------------------

    Dim FEvt As FEVTDATA
    Dim iRC As Integer

    ' Initialize data structure
    iRC = AnInitFEVTDATA(FEvt, FEVTDATA_FMT_STD)

    ' Get message data
    iRC = AnGetQLinkEvent(ulLinkID, FEvt, ulRecID)
    If iRC < 0 Then
        iNSGetQLinkEvent = -1       'status = FAILURE
        Exit Function               'abort operation
    Else
        iNSGetQLinkEvent = iRC      'save status code
    End If

    ' Copy message data to VB format
    iRC = iFEVTDATA_To_NSEVENT(FEvt, NSEvt)
End Function

Function iNSOpenServerLink(ByVal stServerHost As String, ByVal iServerPort As Integer, ByVal ulLinkFlags As Long) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Open link from local message manager to a notification server
    '
    'Usage:   <integer> = iNSOpenServerLink (stServerHost, iServerPort, ulLinkFlags)
    '
    'Inputs:  string  stServerHost  - server host name
    '         integer iServerPort   - server port
    '         long    ulLinkFlags   - link attribute flags  (0 = none)
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '
    'Description:
    '   This function opens a link from your local message manager to a
    '   notification server, based on the supplied host name and port.
    '
    '   Link attribute flags can be used to control how the link operates.
    '   However, no attribute flags are currently implemented, so you should
    '   use a value of zero (no flags).
    '
    '   Caution: This function is normally used only at startup and should not
    '   be called unless a different server is actually needed.
    '
    'Warning:  This is a system-level function and should never be called
    ' by task-level code.
    '-------------------------------------------------------------------------

    ' Set error trap
    On Error Resume Next    'needed if DLL function is not available

    ' Initialize globals, if necessary
    If giNotifWinMsgNo = 0 Then
        InitNotifGlobals
    End If

    ' Open link
    iNSOpenServerLink = AnOpenServerLink(stServerHost, iServerPort, ulLinkFlags)
End Function

Function iNSPostEvent3P(ByVal ulLinkID As Long, ByVal ulMsgFlags As Long, ByVal stMsgName As String, ByVal stMsgDest As String, ByVal pszParm1 As String, ByVal pszParm2 As String, ByVal pszParm3 As String) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Post a notification message with 3 parameters
    '
    'Usage:   <integer> = iNSPostEvent3P (ulLinkID, ulMsgFlags, stMsgName, stMsgDest, pszParm1, pszParm2, pszParm3)
    '
    'Inputs:  long   ulLinkID   - notification link ID  (0 = none)
    '         long   ulMsgFlags - message attribute flags (0 = none)
    '         string stMsgName  - message name
    '         string stMsgDest  - message destination or exercise number
    '         string pszParm1   - parameter 1 text
    '         string pszParm2   - parameter 2 text
    '         string pszParm3   - parameter 3 text
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '
    'Description:
    '   This function posts a notification message and is the primary method
    '   for generating notification messages.
    '
    '   Message attribute flags can be used to control how the message is
    '   processed.  The following flags are available:
    '
    '       NS_EF_DEFAULT       - default attributes
    '       NS_EF_NORMAL        - distributed to local clients and server
    '       NS_EF_LOCAL_ONLY    - distributed only to clients on your PC
    '       NS_EF_GLOBAL        - forwarded to ALL notification servers
    '
    '   These flags can be ORed together to produce any desired combination of
    '   attributes.
    '
    '   To prevent notification loopback, each message is associated with an
    '   message source ID (ulLinkID).  If the message source is unknown or not
    '   important, you can use ID = 0 to make anonymous message postings
    '
    '   Note:  Message names are limited to 30 characters; parameters are
    '   limited to 15 characters each.
    '-------------------------------------------------------------------------
    'Note: This function assumes that numeric MsgDest values are exercise numbers
    'and converts them to a fully qualified message destination string.
    '-------------------------------------------------------------------------

    Dim lExerNum As Long
    Dim iRC As Integer

    ' Prepare destination info
    If IsNumeric(stMsgDest) Then
        lExerNum = Val(stMsgDest)
        stMsgDest = NsGenFEMISMsgDest(lExerNum)
    End If

    ' Post message
    iRC = NsPostMsg3P(ulLinkID, ulMsgFlags, stMsgName, stMsgDest, pszParm1, pszParm2, pszParm3)

    ' Return result
    iNSPostEvent3P = iRC
End Function

Function iNSRegisterInterest(ByVal ulLinkID As Long, ByVal stMsgName As String, ByVal stMsgDest As String) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Register interest in a notification message
    '
    'Usage:   <integer> = iNSRegisterInterest (ulLinkID, stMsgName, stMsgDest)
    '
    'Inputs:  long   ulLinkID   - notification link ID
    '         string stMsgName  - message name
    '         string stMsgDest  - message destination or exercise number
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '
    'Description:
    '   This function registers a client's interest in a specific message and
    '   enables the client to be notified whenever that message occurs.  This
    '   interest remains in effect until the link is closed or
    '   iNSRemoveInterest() is called.
    '
    'Note:  Since this function registers interest in a specific message name,
    ' each unique message name must be registered separately.
    '-------------------------------------------------------------------------
    'Note: This function assumes that numeric MsgDest values are exercise numbers
    'and converts them to a fully qualified message destination string.
    '-------------------------------------------------------------------------

    Dim lExerNum As Long
    Dim iRC As Integer

    ' Prepare destination info
    If IsNumeric(stMsgDest) Then        'exercise number?
        lExerNum = Val(stMsgDest)
        stMsgDest = NsGenFEMISMsgDest(lExerNum)
    End If

    ' Register interest
    iRC = AnRegisterInterest(ulLinkID, stMsgName, stMsgDest)

    ' Return result
    iNSRegisterInterest = iRC
End Function

Function iNSRegisterInterestAllExHz(ByVal ulLinkID As Long, _
                                   ByVal stMsgName As String _
                                  ) As Integer

    '-------------------------------------------------------------------------
    'Purpose: Register interest in a notification message, for all exercises in all hazards
    '
    'Usage:   <integer> = iNSRegisterInterestAllExHz (ulLinkID, stMsgName, stMsgDest)
    '
    'Inputs:  long   ulLinkID   - notification link ID
    '         string stMsgName  - message name
    '         string stMsgDest  - message destination or exercise number
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '
    'Description:
    '   This function registers a client's interest in a specific message for
    '   all exercises and all hazards, enabling the client to be notified
    '   whenever that message occurs.  This interest remains in effect until
    '   the link is closed or iNSRemoveInterestAllExHz() is called.
    '
    'Note:  Since this function registers interest in a specific message name,
    'each unique message name must be registered separately.
    '-------------------------------------------------------------------------

    Dim stMsgDest       As String       'message destination specifier
    Dim lRC             As Long         'temp return code

    ' Register the interest for all exercises and hazards
    stMsgDest = "ALL"
    lRC = NxRegisterInterest(ulLinkID, stMsgName, stMsgDest)
    iNSRegisterInterestAllExHz = lRC
End Function

Function iNSReleaseEventRef(ByVal ulRecID As Long) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Release reference to an message, so it can be discarded
    '
    'Usage:   <integer> = iNSReleaseEventRef (ulRecID)
    '
    'Inputs:  long   ulRecID    - message record ID
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '
    'Description:
    '   This function releases message references and tells the Notification
    '   System that its data for an message can be discarded.
    '
    'Warning:  This function should only be used in a low-level notification
    ' message handler and will cause unpredictable errors if used elsewhere.
    '-------------------------------------------------------------------------
    'WARNING:  This function is obsolete but has been retained for compatibility
    '          purposes only.
    '-------------------------------------------------------------------------

    ' Do nothing
End Function

Function iNSRemoveInterest(ByVal ulLinkID As Long, ByVal stMsgName As String, ByVal stMsgDest As String) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Remove interest in a notification message
    '
    'Usage:   <integer> = iNSRemoveInterest (ulLinkID, stMsgName, stMsgDest)
    '
    'Inputs:  long   ulLinkID   - notification link ID
    '         string stMsgName  - message name
    '         string stMsgDest  - message destination or exercise number
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '
    'Description:
    '   This function removes a client's interest in a specific message and
    '   removes the client from the distribution list for that message.
    '
    '   Note:  All active interests are automatically removed during by the
    '   iNSCloseLink() function, so it is not necessary to remove interests
    '   before closing a notification link.
    '
    'Note:  Since this function removes interest in a specific message name,
    ' each unique message name must be removed separately.
    '-------------------------------------------------------------------------
    'Note: This function assumes that numeric MsgDest values are exercise numbers
    'and converts them to a fully qualified message destination string.
    '-------------------------------------------------------------------------

    Dim lExerNum As Long
    Dim iRC As Integer

    ' Prepare destination info
    If IsNumeric(stMsgDest) Then        'exercise number?
        lExerNum = Val(stMsgDest)
        stMsgDest = NsGenFEMISMsgDest(lExerNum)
    End If

    ' Remove interest
    iRC = AnRemoveInterest(ulLinkID, stMsgName, stMsgDest)

    ' Return result
    iNSRemoveInterest = iRC
End Function

Function iNSRemoveInterestAllExHz(ByVal ulLinkID As Long, _
                                   ByVal stMsgName As String _
                                  ) As Integer

    '-------------------------------------------------------------------------
    'Purpose: Remove interest in a notification message, for all exercises in all hazards
    '
    'Usage:   <integer> = iNSRemoveInterestAllExHz (ulLinkID, stMsgName, stMsgDest)
    '
    'Inputs:  long   ulLinkID   - notification link ID
    '         string stMsgName  - message name
    '         string stMsgDest  - message destination or exercise number
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '
    'Description:
    '   This function removes a client's interest in a specific message for
    '   all exercises and all hazards.
    '
    'Note:  Since this function removes interest in a specific message name,
    'each unique message name must be Removeed separately.
    '-------------------------------------------------------------------------

    Dim stMsgDest       As String       'message destination specifier
    Dim lRC             As Long         'temp return code

    ' Remove the interest for all exercises and hazards
    stMsgDest = "ALL"
    lRC = NxRemoveInterest(ulLinkID, stMsgName, stMsgDest)
    iNSRemoveInterestAllExHz = lRC
End Function

Function iNSVerifyInterest(ByVal ulLinkID As Long, ByVal stMsgName As String, ByVal stMsgDest As String) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Verify interest in a notification message
    '
    'Usage:   <integer> = iNSVerifyInterest (ulLinkID, stMsgName, stMsgDest)
    '
    'Inputs:  long   ulLinkID   - notification link ID
    '         string stMsgName  - message name
    '         string stMsgDest  - message destination or exercise number
    '
    'Outputs: none
    '
    'Returns: True or False
    '
    'Description:
    '   This function verifies a client's interest in a specific message.
    '
    'Note:  Since this function verifies interest in a specific message name,
    ' each unique message name must be verified separately.
    '-------------------------------------------------------------------------
    'Note: This function assumes that numeric MsgDest values are exercise numbers
    'and converts them to a fully qualified message destination string.
    '-------------------------------------------------------------------------

    Dim lExerNum As Long
    Dim iRC As Integer

    ' Prepare destination info
    If IsNumeric(stMsgDest) Then        'exercise number?
        lExerNum = Val(stMsgDest)
        stMsgDest = NsGenFEMISMsgDest(lExerNum)
    End If

    ' Verify interest
    iRC = AnVerifyInterest(ulLinkID, stMsgName, stMsgDest)

    ' Return result
    If iRC = 0 Then
        iNSVerifyInterest = False
    Else
        iNSVerifyInterest = True
    End If
End Function

Public Function Ns_CTimeToVBTime(ByVal ulCTime As Long) As Date
    '-------------------------------------------------------------------------
    'Purpose: Convert a C time value to a VB time value
    'Returns: VB time value
    '-------------------------------------------------------------------------

    Dim dVBTime As Date                 'VB time value
    Const SecsPerDay = 86400#           'seconds per day
    Const CTimeDayAdj = 25569#          'CTime day adjustment

    ' Convert C time value to VB time value
    dVBTime = CDate((ulCTime / SecsPerDay) + CTimeDayAdj)

    ' Return result
    Ns_CTimeToVBTime = dVBTime             'return VB time
End Function

Private Function Ns_FNotifyMsg_to_MSGDATA(oMsg As FNotifyMsg, tMsgData As NS_MSG_DATA) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Copy data from FNotifyMsg object to an NS_MSG_DATA structure
    '
    'Usage:   iSC = Ns_FNotifyMsg_to_MSGDATA(oMsg, tMsgData)
    '
    'Inputs:  oMsg      - FNotifyMsg message object
    '         tMsgData  - VB message data structure
    '         iSC       - status code
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '-------------------------------------------------------------------------
    'Note: This function assumes that the target structure is already initialized.
    '-------------------------------------------------------------------------

    Dim iParmArrLen As Integer          'parameter array length
    Dim i           As Integer          'loop index

    ' Setup
    On Error GoTo Error_Exit
    Ns_FNotifyMsg_to_MSGDATA = -2       'default status - FAILURE
    iParmArrLen = oMsg.MsgDataCount
    If iParmArrLen > UBound(tMsgData.stMsgParm) Then
        iParmArrLen = UBound(tMsgData.stMsgParm)
    End If

    ' Copy data from FNotifyMsg object to NS_MSG_DATA structure
    ' (For efficiency, use With keyword on FNotifyMsg, not tMsgData)
    With oMsg
        'message header
        tMsgData.ulMsgID = .MsgID
        tMsgData.vMsgTime = .MsgTime
        tMsgData.ulMsgFlags = .MsgFlags
        tMsgData.stMsgClass = .MsgData(NS_MsgClass_IDX)
        tMsgData.stMsgName = .MsgName
        tMsgData.stMsgDest = .MsgDest
        tMsgData.stAuxProcessIdent = .AuxProcessIdent
        tMsgData.ulAuxProcessFlags = .AuxProcessFlags

        'message parameters
        For i = 0 To iParmArrLen - 1
            tMsgData.stMsgParm(i) = .MsgData(i)
        Next

        ' Enable this code after v1.5.2 is distributed
        'If oMsg.GetMsgDataValues(tMsgData.stMsgParm(0), 0, iParmArrLen) = False Then
        '    Exit Function
        'End If

        'auxiliary info
        Select Case UCase(tMsgData.stMsgClass)
            Case UCase(NS_MSG_CLASS_ACTION)
                tMsgData.stExerID = .MsgData(NS_AC_ExerID_IDX)
            Case UCase(NS_MSG_CLASS_DATA_CHANGE)
                tMsgData.stExerID = .MsgData(NS_DC_ExerID_IDX)
            Case Else
                tMsgData.stExerID = ""
        End Select
    End With

    ' Return result
    Ns_FNotifyMsg_to_MSGDATA = 0        'status = SUCCESS
    Exit Function                       'all done

    ' Error exit
Error_Exit:
    ifxMsgBox "Error in Ns_FNotifyMsg_to_MSGDATA: " & Error$
    Ns_FNotifyMsg_to_MSGDATA = -2       'return FAILURE
End Function

Private Function Ns_MSGDATA_to_FNotifyMsg(tMsgData As NS_MSG_DATA, oMsg As FNotifyMsg) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Copy data from NS_MSG_DATA structure to FNotifyMsg object
    '
    'Usage:   iSC = Ns_MSGDATA_to_FNotifyMsg(tMsgData, oMsg)
    '
    'Inputs:  tMsgData  - VB message data structure
    '         oMsg      - FNotifyMsg message object
    '         iSC       - status code
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '-------------------------------------------------------------------------
    'Note: This function assumes that the target structure is already initialized.
    'Note: Derived fields (ExerciseID) are not copied.
    '-------------------------------------------------------------------------

    Dim iParmArrLen As Integer          'parameter array length
    Dim i           As Integer          'loop index

    ' Setup
    On Error GoTo Error_Exit
    Ns_MSGDATA_to_FNotifyMsg = -2       'default status = FAILURE
    iParmArrLen = oMsg.MsgDataCount
    If iParmArrLen > UBound(tMsgData.stMsgParm) Then
        iParmArrLen = UBound(tMsgData.stMsgParm)
    End If

    ' Copy data from NS_MSG_DATA structure to FNotifyMsg object
    ' (For efficiency, use With keyword on FNotifyMsg, not tMsgData)
    With oMsg
        'message header
        .MsgID = tMsgData.ulMsgID
        .MsgTime = tMsgData.vMsgTime
        .MsgFlags = tMsgData.ulMsgFlags
        .MsgName = tMsgData.stMsgName
        .MsgDest = tMsgData.stMsgDest
        .AuxProcessIdent = tMsgData.stAuxProcessIdent
        .AuxProcessFlags = tMsgData.ulAuxProcessFlags

        'message parameters
        For i = 0 To iParmArrLen - 1
            .MsgData(i) = tMsgData.stMsgParm(i)
        Next

        ' Enable this code after v1.5.2 is distributed
        'If oMsg.SetMsgDataValues(tMsgData.stMsgParm(0), 0, iParmArrLen) = False Then
        '    Exit Function
        'End If

    End With

    ' Return result
    Ns_MSGDATA_to_FNotifyMsg = 0          'status = SUCCESS
    Exit Function                       'all done

    ' Error exit
Error_Exit:
    ifxMsgBox "Error in Ns_MSGDATA_to_FNotifyMsg: " & Error$
    Ns_MSGDATA_to_FNotifyMsg = -2       'return FAILURE
End Function

Public Function NsCopyActionParams(tSrc As NS_ACTION_PARAMS, tDest As NS_ACTION_PARAMS) As Long
    '-------------------------------------------------------------------------
    'Purpose: Copy an NS_ACTION_PARAMS structure
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '-------------------------------------------------------------------------

    Dim i           As Integer          'loop index

    ' Copy the message
    With tDest
        .stActionName = tSrc.stActionName
        .stActionType = tSrc.stActionType
        .stActionStatus = tSrc.stActionStatus
        .vActionDate = tSrc.vActionDate
        .stItemID1 = tSrc.stItemID1
        .stItemID2 = tSrc.stItemID2
        .tDDParams = tSrc.tDDParams
        .stSiteCode = tSrc.stSiteCode
        .stEOCCode = tSrc.stEOCCode
        .stHazardID = tSrc.stHazardID
        .stExerID = tSrc.stExerID
        .stFolderID = tSrc.stFolderID
        For i = 0 To UBound(.stAuxData)
            .stAuxData(i) = tSrc.stAuxData(i)
        Next
    End With

    ' Return a status code
    NsCopyActionParams = 0              'status = SUCCESS
End Function

Public Function NsCopyNS_MSG_DATA(tSrc As NS_MSG_DATA, tDest As NS_MSG_DATA) As Long
    '-------------------------------------------------------------------------
    'Purpose: Copy an NS_MSG_DATA structure
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '-------------------------------------------------------------------------

    Dim i           As Integer          'loop index

    ' Copy the message
    With tDest
        .ulMsgID = tSrc.ulMsgID
        .vMsgTime = tSrc.vMsgTime
        .ulMsgFlags = tSrc.ulMsgFlags
        .stMsgClass = tSrc.stMsgClass
        .stMsgName = tSrc.stMsgName
        .stMsgDest = tSrc.stMsgDest
        .stExerID = tSrc.stExerID
        .stAuxProcessIdent = tSrc.stAuxProcessIdent
        .ulAuxProcessFlags = tSrc.ulAuxProcessFlags
        For i = 0 To 49
            .stMsgParm(i) = tSrc.stMsgParm(i)
        Next
    End With

    ' Return a status code
    NsCopyNS_MSG_DATA = 0               'status = SUCCESS
End Function

Public Function NsGenFEMISMsgDest(ByVal lExerNum As Long) As String
    '-------------------------------------------------------------------------
    'Purpose: Generate a FEMIS message destination string
    'Returns: FEMIS message destination string
    '-------------------------------------------------------------------------

    Static stMsgDest    As String       'message destination string
    Static stOldHazName As String       'saved hazard name
    Static stOldSiteCode As String      'saved site code
    Static stOldEOCCode As String       'saved EOC code
    Static lOldExerNum  As Long         'saved exercise number

    ' Use the saved message destination string, if valid
    If stMsgDest <> "" _
        And stOldHazName = gstND_HazName _
        And stOldSiteCode = gstUCurSite _
        And stOldEOCCode = gstUCurEOCCode _
        And lOldExerNum = lExerNum _
        Then
            ' Return saved destination string
            NsGenFEMISMsgDest = stMsgDest
            Exit Function
    End If

    ' Generate a new message destination string
    stMsgDest = NsGenFEMISMsgDestEx(gstND_HazName, gstUCurSite, gstUCurEOCCode, lExerNum)

    ' Save component info
    stOldHazName = gstND_HazName
    stOldSiteCode = gstUCurSite
    stOldEOCCode = gstUCurEOCCode
    lOldExerNum = lExerNum

    ' Return result
    NsGenFEMISMsgDest = stMsgDest
End Function

Public Function NsGenFEMISMsgDestEx(ByVal stHazardName As String, _
                                    ByVal stSiteCode As String, _
                                    ByVal stEOCCode As String, _
                                    ByVal lExerNum As Long) As String

    '-------------------------------------------------------------------------
    'Purpose: Generate a FEMIS message destination string, based on explicit values
    'Returns: FEMIS message destination string
    '-------------------------------------------------------------------------

    Static stMsgDest    As String       'message destination string
    Static stOldHazName As String       'saved hazard name
    Static stOldSiteCode As String      'saved site code
    Static stOldEOCCode As String       'saved EOC code
    Static lOldExerNum  As Long         'saved exercise number
    Dim i               As Integer      'loop index

    ' Use the saved message destination string, if valid
    If stMsgDest <> "" _
        And stOldHazName = stHazardName _
        And stOldSiteCode = stSiteCode _
        And stOldEOCCode = stEOCCode _
        And lOldExerNum = lExerNum _
        Then
            ' Return saved destination string
            NsGenFEMISMsgDestEx = stMsgDest
            Exit Function
    End If

    ' Generate a new message destination string
    stMsgDest = "/femis/" & stHazardName & "/" & stSiteCode & "/Exer." & lExerNum

    ' Save component info
    stOldHazName = stHazardName
    stOldSiteCode = stSiteCode
    stOldEOCCode = stEOCCode
    lOldExerNum = lExerNum

    ' Return result
    NsGenFEMISMsgDestEx = stMsgDest
End Function

Public Function NsGetActionMsg(ByVal ulLinkID As Long, ByVal ulMsgID As Long, tMsgData As NS_ACTION_PARAMS) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Get an Action message from client's message queue
    '
    'Usage:   iSC = NsGetActionMsg(ulLinkID, ulMsgID, tMsgData)
    '
    'Inputs:  ulLinkID  - client's link ID
    '         ulMsgID   - message record ID
    '         tMsgData  - message data structure
    '         iSC       - status code
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '-------------------------------------------------------------------------
    '
    'Description:
    '   This function retrieves an Action message from a notification client's
    '   message queue, based on a link ID and a record ID.
    '
    '   If the message cannot be retrieved, the function returns a negative
    '   status code.
    '-------------------------------------------------------------------------
    'Note: This function assumes that the target structure is already initialized.
    '-------------------------------------------------------------------------

    Dim tGenMsg As NS_MSG_DATA          'generic message data structure

    ' Get generic message data
    If NsGetLinkMsg(ulLinkID, ulMsgID, tGenMsg) < 0 Then
        NsGetActionMsg = -1             'status = FAILURE
        Exit Function                   'exit now
    End If

    ' Extract Action info from generic message
    NsGetMsgActionInfo tGenMsg, tMsgData
    NsGetMsgDDParams tGenMsg, tMsgData.tDDParams

    ' Return result
    NsGetActionMsg = 0                  'status = SUCCESS
End Function

Public Function NsGetDataChangeMsg(ByVal ulLinkID As Long, ByVal ulMsgID As Long, tMsgData As NS_DATACHG_PARAMS) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Get a DataChange message from client's message queue
    '
    'Usage:   iSC = NsGetDataChangeMsg(ulLinkID, ulMsgID, tMsgData)
    '
    'Inputs:  ulLinkID  - client's link ID
    '         ulMsgID   - message record ID
    '         tMsgData  - message data structure
    '         iSC       - status code
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '-------------------------------------------------------------------------
    '
    'Description:
    '   This function retrieves a DataChange message from a notification client's
    '   message queue, based on a link ID and a record ID.
    '
    '   If the message cannot be retrieved, the function returns a negative
    '   status code.
    '-------------------------------------------------------------------------
    'Note: This function assumes that the target structure is already initialized.
    '-------------------------------------------------------------------------

    Dim tGenMsg As NS_MSG_DATA          'generic message data structure

    ' Get generic message data
    If NsGetLinkMsg(ulLinkID, ulMsgID, tGenMsg) < 0 Then
        NsGetDataChangeMsg = -1             'status = FAILURE
        Exit Function                   'exit now
    End If

    ' Extract DataChange info from generic message
    NsGetMsgDataChangeInfo tGenMsg, tMsgData
    NsGetMsgDDParams tGenMsg, tMsgData.tDDParams

    ' Return result
    NsGetDataChangeMsg = 0              'status = SUCCESS
End Function

Public Function NsGetLinkMsg(ByVal ulLinkID As Long, ByVal ulMsgID As Long, tMsgData As NS_MSG_DATA) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Get a generic message from client's message queue
    '
    'Usage:   iSC = NsGetLinkMsg(ulLinkID, ulMsgID, tMsgData)
    '
    'Inputs:  ulLinkID  - client's link ID
    '         ulMsgID   - message record ID
    '         tMsgData  - message data structure
    '         iSC       - status code
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '-------------------------------------------------------------------------
    '
    'Description:
    '   This function retrieves a generic message from a notification client's
    '   message queue, based on a link ID and a record ID.
    '
    '   If the message cannot be retrieved, the function returns a negative
    '   status code.
    '-------------------------------------------------------------------------
    'Note: This function assumes that the target structure is already initialized.
    '-------------------------------------------------------------------------
    '05/29/02  LRS  Modified to use FNotifyMsg object.

    Dim oMsg    As FNotifyMsg           'notification message object

    ' Setup
    On Error GoTo Error_Exit            'set error trap
    NsGetLinkMsg = -2                   'default status = FAILURE

    ' Create message object
    Set oMsg = New FNotifyMsg
    If oMsg Is Nothing Then
        Exit Function
    End If

    ' Get message from client's message queue
    If oMsg.GetClientMsg(ulLinkID, ulMsgID) = False Then
        Exit Function
    End If

    ' Convert message to VB format
    If Ns_FNotifyMsg_to_MSGDATA(oMsg, tMsgData) < 0 Then
        Exit Function
    End If

    ' Normal exit
    NsGetLinkMsg = 0                    'status = SUCCESS
    Exit Function                       'all done

    ' Error exit
Error_Exit:
    ifxMsgBox "NsGetLinkMsg: Error " & Err & ":  " & Error$
    NsGetLinkMsg = -2                   'return FAILURE
End Function

Public Function NsGetLinkServerInfo(ByVal ulLinkID As Long, ulServerID As Long, stServerHost As String, lServerPort As Long) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Get information about a link's notification server
    '
    'Usage:   <integer> = NsGetLinkServerInfo(ulLinkID, ulServerID, stServerHost, iServerPort)
    '
    'Inputs:  long    ulLinkID  - link ID
    '
    'Outputs: long    ulServerID    - server ID
    '         string  stServerHost  - server host name
    '         integer iServerPort   - server port
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '
    'Description:
    '   This function returns information about a link's notification server,
    '   based on the supplied link ID.
    '
    '-------------------------------------------------------------------------

    Dim stServerName    As String       'server name buffer
    Dim stTemp          As String       'string buffer
    Dim iSLen           As Integer      'string length
    Dim istatus         As Integer      'status code

    ' Set error trap
    On Error Resume Next    'needed if DLL function is not available

    ' Prepare server name buffer
    stTemp = String$(256, 32)             'create buffer

    ' Get information about the link's notification server
    If AnGetLinkServerInfo(ulLinkID, ulServerID, stTemp, lServerPort) < 0 Then
        NsGetLinkServerInfo = -2        'status = FAILURE
        Exit Function
    End If

    ' Get server name
    iSLen = InStr(stTemp, Chr$(0))
    If iSLen > 0 Then
        stServerHost = Left$(stTemp, iSLen - 1)
    Else
        stServerHost = ""
    End If

    ' Return a status code
    NsGetLinkServerInfo = 0             'status = SUCCESS
End Function

Public Sub NsGetMsgActionInfo(tMsgData As NS_MSG_DATA, tActionData As NS_ACTION_PARAMS)
    '-------------------------------------------------------------------------
    'Purpose: Extract Action info from a generic notification message.
    '
    'Usage:   iSC = NsGetMsgActionInfo (tMsgData, tActionData)
    '
    'Arguments:
    '   tMsgData    - message data structure
    '   tActionData - Action data structure
    '   iSC         - status code
    '
    'Outputs: none
    '
    'Returns: nothing
    '-------------------------------------------------------------------------
    'Note: This function assumes that the target structure is already initialized.
    '-------------------------------------------------------------------------

    ' Extract Action info from a generic notification message.
    With tActionData
        .stActionName = tMsgData.stMsgName
        .stActionType = tMsgData.stMsgParm(NS_AC_ActionType_IDX)
        .stActionStatus = tMsgData.stMsgParm(NS_AC_ActionStatus_IDX)
        If IsDate(tMsgData.stMsgParm(NS_AC_ActionDate_IDX)) Then _
            .vActionDate = CDate(tMsgData.stMsgParm(NS_AC_ActionDate_IDX))
        .stItemID1 = tMsgData.stMsgParm(NS_AC_ItemID1_IDX)
        .stItemID2 = tMsgData.stMsgParm(NS_AC_ItemID2_IDX)
        .stAuxData(0) = tMsgData.stMsgParm(NS_AC_AuxData1_IDX)
        .stAuxData(1) = tMsgData.stMsgParm(NS_AC_AuxData2_IDX)
        .stAuxData(2) = tMsgData.stMsgParm(NS_AC_AuxData3_IDX)
        .stAuxData(3) = tMsgData.stMsgParm(NS_AC_AuxData4_IDX)
        .stAuxData(4) = tMsgData.stMsgParm(NS_AC_AuxData5_IDX)

        .stSiteCode = tMsgData.stMsgParm(NS_AC_SiteCode_IDX)
        .stEOCCode = tMsgData.stMsgParm(NS_AC_EOCCode_IDX)
        .stHazardID = tMsgData.stMsgParm(NS_AC_HazardID_IDX)
        .stExerID = tMsgData.stMsgParm(NS_AC_ExerID_IDX)
        .stFolderID = tMsgData.stMsgParm(NS_AC_FolderID_IDX)
    End With

    ' Extract Data-Driven parameter info
    NsGetMsgDDParams tMsgData, tActionData.tDDParams

End Sub

Public Function NsGetMsgClass(tMsgData As NS_MSG_DATA) As String
    '-------------------------------------------------------------------------
    'Purpose: Get the message class value from a generic notification message.
    '
    'Usage:   stMsgClass = NsGetMsgClass (tMsgData)
    '
    'Arguments:
    '   tMsgData    - message data structure
    '   stMsgClass  - message class value
    '
    'Outputs: none
    '
    'Returns: nothing
    '-------------------------------------------------------------------------
    'Note: This function assumes that the target structure is already initialized.
    '-------------------------------------------------------------------------

    Dim stMsgClass As String            'message class string

    ' Get the message class value
    stMsgClass = tMsgData.stMsgParm(NS_MsgClass_IDX)

    ' Return result
    NsGetMsgClass = stMsgClass          'return message class
End Function

Public Sub NsGetMsgDataChangeInfo(tMsgData As NS_MSG_DATA, tChangeData As NS_DATACHG_PARAMS)
    '-------------------------------------------------------------------------
    'Purpose: Extract DataChange info from a generic notification message.
    '
    'Usage:   iSC = NsGetMsgDataChangeInfo (tMsgData, tChangeData)
    '
    'Arguments:
    '   tMsgData    - message data structure
    '   tChangeData - DataChange data structure
    '   iSC         - status code
    '
    'Outputs: none
    '
    'Returns: nothing
    '-------------------------------------------------------------------------
    'Note: This function assumes that the target structure is already initialized.
    '-------------------------------------------------------------------------

    ' Setup
    On Error Resume Next                'ignore errors

    ' Extract DataChange info from a generic notification message.
    With tChangeData
        .stNotifName = tMsgData.stMsgName
        .stChangeType = tMsgData.stMsgParm(NS_DC_ChangeType_IDX)
        .stDatasetName = tMsgData.stMsgParm(NS_DC_DatasetName_IDX)
        If IsDate(tMsgData.stMsgParm(NS_DC_ChangeDate_IDX)) Then _
            .vChangeDate = CDate(tMsgData.stMsgParm(NS_DC_ChangeDate_IDX))
        .stItemID1 = tMsgData.stMsgParm(NS_DC_ItemID1_IDX)
        .stItemID2 = tMsgData.stMsgParm(NS_DC_ItemID2_IDX)
        .stAuxData(0) = tMsgData.stMsgParm(NS_DC_AuxData1_IDX)
        .stAuxData(1) = tMsgData.stMsgParm(NS_DC_AuxData2_IDX)
        .stAuxData(2) = tMsgData.stMsgParm(NS_DC_AuxData3_IDX)
        .stAuxData(3) = tMsgData.stMsgParm(NS_DC_AuxData4_IDX)
        .stAuxData(4) = tMsgData.stMsgParm(NS_DC_AuxData5_IDX)

        .stSiteCode = tMsgData.stMsgParm(NS_DC_SiteCode_IDX)
        .stEOCCode = tMsgData.stMsgParm(NS_DC_EOCCode_IDX)
        .stHazardID = tMsgData.stMsgParm(NS_DC_HazardID_IDX)
        .stExerID = tMsgData.stMsgParm(NS_DC_ExerID_IDX)
        .stFolderID = tMsgData.stMsgParm(NS_DC_FolderID_IDX)
    End With

    ' Extract Data-Driven parameter info
    NsGetMsgDDParams tMsgData, tChangeData.tDDParams

End Sub

Public Sub NsGetMsgDDParams(tMsgData As NS_MSG_DATA, tDDParams As NS_DD_PARAMS)
    '-------------------------------------------------------------------------
    'Purpose: Extract DataDriven parameter info from a generic notification message.
    '
    'Usage:   iSC = NsGetMsgDDParams (tMsgData, tDDParams)
    '
    'Arguments:
    '   tMsgData    - message data structure
    '   tDDParams   - data-driven processing parameters
    '   iSC         - status code
    '
    'Outputs: none
    '
    'Returns: nothing
    '-------------------------------------------------------------------------
    'Note: This function assumes that the target structure is already initialized.
    '-------------------------------------------------------------------------

    Dim vXmitInitDate(3) As Date        'record timestamps

    ' Prepare timestamp values
    If IsDate(tMsgData.stMsgParm(NS_DD_XmitInitDate1_IDX)) Then _
        vXmitInitDate(0) = CDate(tMsgData.stMsgParm(NS_DD_XmitInitDate1_IDX))
    If IsDate(tMsgData.stMsgParm(NS_DD_XmitInitDate2_IDX)) Then _
        vXmitInitDate(1) = CDate(tMsgData.stMsgParm(NS_DD_XmitInitDate2_IDX))
    If IsDate(tMsgData.stMsgParm(NS_DD_XmitInitDate3_IDX)) Then _
        vXmitInitDate(2) = CDate(tMsgData.stMsgParm(NS_DD_XmitInitDate3_IDX))

    ' Extract DataDriven parameter info from a generic notification message.
    tDDParams.stTableName(0) = tMsgData.stMsgParm(NS_DD_TblName1_IDX)
    tDDParams.stTableName(1) = tMsgData.stMsgParm(NS_DD_TblName2_IDX)
    tDDParams.stTableName(2) = tMsgData.stMsgParm(NS_DD_TblName3_IDX)
    tDDParams.stRowID(0) = tMsgData.stMsgParm(NS_DD_RowID1_IDX)
    tDDParams.stRowID(1) = tMsgData.stMsgParm(NS_DD_RowID2_IDX)
    tDDParams.stRowID(2) = tMsgData.stMsgParm(NS_DD_RowID3_IDX)
    tDDParams.vXmitInitDate(0) = vXmitInitDate(0)
    tDDParams.vXmitInitDate(1) = vXmitInitDate(1)
    tDDParams.vXmitInitDate(2) = vXmitInitDate(2)
    tDDParams.stAction(0) = tMsgData.stMsgParm(NS_DD_Action1_IDX)
    tDDParams.stAction(1) = tMsgData.stMsgParm(NS_DD_Action2_IDX)
    tDDParams.stAction(2) = tMsgData.stMsgParm(NS_DD_Action3_IDX)
    tDDParams.stListener = tMsgData.stMsgParm(NS_DD_Listener_IDX)
    tDDParams.lDDFlags = Val(tMsgData.stMsgParm(NS_DD_DDFlags_IDX))
    tDDParams.stEOCCode = tMsgData.stMsgParm(NS_DD_EOCCode_IDX)
    tDDParams.stDDHandler = tMsgData.stMsgParm(NS_DD_DDHandler_IDX)

    ' Return result
    'NsGetMsgDDParams = 0                'status = SUCCESS
End Sub

Public Function NsIsLinkMsgID(ByVal ulLinkID As Long, ByVal ulMsgID As Long) As Boolean
    '-------------------------------------------------------------------------
    'Purpose: Test whether a message queue contains the specified message ID
    '
    'Usage:   fFound = NsIsLinkMsgID (ulLinkID, ulMsgID, tMsgData)
    '
    'Inputs:  ulLinkID  - client's link ID
    '         ulMsgID   - message record ID
    '         fFound    - result code
    '
    'Outputs: none
    '
    'Returns: TRUE or FALSE
    '-------------------------------------------------------------------------

    Dim iRC As Integer                  'temp return code

    ' Test whether message queue contains the specified message ID
    iRC = AnIsQLinkEvent(ulLinkID, ulMsgID)

    ' Return result
    If iRC = 0 Then
        NsIsLinkMsgID = False           'ID not valid
    Else
        NsIsLinkMsgID = True            'ID is valid
    End If
End Function

Public Function NsPostActionMsg(ByVal ulLinkID As Long, _
                                ByVal ulMsgFlags As Long, _
                                ByVal stActionName As String, _
                                ByVal stActionType As String, _
                                ByVal stActionStatus As String, _
                                ByVal vActionDate As Date, _
                                Optional vItemID1, _
                                Optional vItemID2, _
                                Optional vAuxData1, _
                                Optional vAuxData2, _
                                Optional vAuxData3, _
                                Optional vAuxData4, _
                                Optional vAuxData5, _
                                Optional vFolderID, _
                                Optional vAckString _
                                ) As Integer

    '-------------------------------------------------------------------------
    'Purpose: Post an Action notification message
    '
    'Usage:   iSC = NsPostActionMsg (ulLinkID, ulMsgFlags, stActionName, stActionType, stActionStatus, vActionDate, vItemID1, vItemID2, vAuxData1, vAuxData2)
    '
    'Arguments:
    '   ulLinkID        - notification link ID  (0 = Anonymous)
    '   ulMsgFlags      - message attribute flags
    '   stActionName    - action name
    '   stActionType    - action type
    '   stActionStatus  - action status
    '   vActionDate     - action date-time
    '   vItemID1        - item ID 1  (optional)
    '   vItemID2        - item ID 2  (optional)
    '   vAuxData1       - auxiliary data 1  (optional)
    '   vAuxData2       - auxiliary data 2  (optional)
    '   vAuxData3       - auxiliary data 3  (optional)
    '   vAuxData4       - auxiliary data 4  (optional)
    '   vAuxData5       - auxiliary data 5  (optional)
    '   vFolderID       - folder ID (optional)
    '   vAckString      - data acknowledgement string (optional)
    '   iSC             - status code
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '-------------------------------------------------------------------------
    '
    'Description:
    '   This function posts an Action notification message using the supplied
    '   parameters and the current exercise number.
    '
    '   If the message cannot be posted, the function returns a negative
    '   status code.
    '
    '   Message attribute flags can be used to control how the message is
    '   processed.  The following flags are available:
    '
    '       NS_EF_DEFAULT       - default attributes
    '       NS_EF_NORMAL        - distributed to local clients and server
    '       NS_EF_LOCAL_ONLY    - distributed only to clients on your PC
    '       NS_EF_GLOBAL        - forwarded to ALL notification servers
    '
    '   These flags can be ORed together to produce any desired combination
    '   of attributes.
    '
    '   To prevent notification loopback, each message is associated with an
    '   message source ID (ulLinkID).  If the message source is unknown or
    '   not important, you can use ID = 0 to make anonymous message postings.
    '
    '-------------------------------------------------------------------------
    'Note: This function automatically adds a NS_DD_ARRIVED flag to the message.
    '-------------------------------------------------------------------------

    Dim tDDParams   As NS_DD_PARAMS     'data-driven parameters
    Dim iRC         As Integer          'temp return code

    ' Setup
    tDDParams.stDDHandler = "(none)"    'set no-DD flag
    tDDParams.lDDFlags = NS_DD_ARRIVED Or tDDParams.lDDFlags  'set ARRIVED flag

    ' Post Action message with dummy DD parameters
    iRC = NsPostActionMsgDD(ulLinkID, ulMsgFlags, tDDParams, stActionName, stActionType, stActionStatus, vActionDate, vItemID1, vItemID2, vAuxData1, vAuxData2, vAuxData3, vAuxData4, vAuxData5, vFolderID)

    ' Return result
    NsPostActionMsg = iRC               'return status code
End Function

Public Function NsPostActionMsgDD(ByVal ulLinkID As Long, _
                                  ByVal ulMsgFlags As Long, _
                                  tDDParams As NS_DD_PARAMS, _
                                  ByVal stActionName As String, _
                                  ByVal stActionType As String, _
                                  ByVal stActionStatus As String, _
                                  ByVal vActionDate As Date, _
                                  Optional vItemID1, _
                                  Optional vItemID2, _
                                  Optional vAuxData1, _
                                  Optional vAuxData2, _
                                  Optional vAuxData3, _
                                  Optional vAuxData4, _
                                  Optional vAuxData5, _
                                  Optional vFolderID, _
                                  Optional vAckString _
                                  ) As Integer

    '-------------------------------------------------------------------------
    'Purpose: Post an Action notification message with Data-Driven parameters
    '
    'Usage:   iSC = NsPostActionMsgDD (ulLinkID, ulMsgFlags, tDDParams, stActionName, stActionType, stActionStatus, vActionDate, vItemID1, vItemID2, vAuxData1, vAuxData2)
    '
    'Arguments:
    '   ulLinkID        - notification link ID  (0 = Anonymous)
    '   ulMsgFlags      - message attribute flags
    '   tDDParams       - data-driven processing parameters
    '   stActionName    - action name
    '   stActionType    - action type
    '   stActionStatus  - action status
    '   vActionDate     - action date-time
    '   vItemID1        - item ID 1  (optional)
    '   vItemID2        - item ID 2  (optional)
    '   vAuxData1       - auxiliary data 1  (optional)
    '   vAuxData2       - auxiliary data 2  (optional)
    '   vAuxData3       - auxiliary data 3  (optional)
    '   vAuxData4       - auxiliary data 4  (optional)
    '   vAuxData5       - auxiliary data 5  (optional)
    '   vFolderID       - folder ID (optional)
    '   vAckString      - data acknowledgement string (optional)
    '   iSC             - status code
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '-------------------------------------------------------------------------
    '
    'Description:
    '   This function posts an Action notification message using the supplied
    '   parameters and the current exercise number.
    '
    '   If the message cannot be posted, the function returns a negative
    '   status code.
    '
    '   Message attribute flags can be used to control how the message is
    '   processed.  The following flags are available:
    '
    '       NS_EF_DEFAULT       - default attributes
    '       NS_EF_NORMAL        - distributed to local clients and server
    '       NS_EF_LOCAL_ONLY    - distributed only to clients on your PC
    '       NS_EF_GLOBAL        - forwarded to ALL notification servers
    '
    '   These flags can be ORed together to produce any desired combination
    '   of attributes.
    '
    '   To prevent notification loopback, each message is associated with an
    '   message source ID (ulLinkID).  If the message source is unknown or
    '   not important, you can use ID = 0 to make anonymous message postings.
    '
    '-------------------------------------------------------------------------
    '05/29/02  LRS  Modified to use FNotifyMsg object.

    Dim oMsg            As FNotifyMsg   'notification message object
    Dim stMsgDest       As String       'message destination
    Dim stActionDate    As String       'action timestamp
    Dim stItemID1       As String       'item ID 1
    Dim stItemID2       As String       'item ID 2
    Dim stAuxData1      As String       'auxiliary data 1
    Dim stAuxData2      As String       'auxiliary data 2
    Dim stAuxData3      As String       'auxiliary data 3
    Dim stAuxData4      As String       'auxiliary data 4
    Dim stAuxData5      As String       'auxiliary data 5
    Dim lExerNum        As Long         'exercise number
    Dim lFolderID       As Long         'folder ID
    Dim stAckString     As String       'data acknowlegment string
    Dim fAckString      As Boolean      'HAVE ACK STRING flag
    'Dim iRC             As Integer      'temp return code

    ' Setup
    On Error GoTo Error_Exit
    NsPostActionMsgDD = -2             'default status = FAILURE

    ' Create message object
    Set oMsg = New FNotifyMsg
    If oMsg Is Nothing Then
        Exit Function
    End If

    ' Fill optional arguments
    If Not IsMissing(vItemID1) Then stItemID1 = vItemID1
    If Not IsMissing(vItemID2) Then stItemID2 = vItemID2
    If Not IsMissing(vAuxData1) Then stAuxData1 = vAuxData1
    If Not IsMissing(vAuxData2) Then stAuxData2 = vAuxData2
    If Not IsMissing(vAuxData3) Then stAuxData3 = vAuxData3
    If Not IsMissing(vAuxData4) Then stAuxData4 = vAuxData4
    If Not IsMissing(vAuxData5) Then stAuxData5 = vAuxData5
    If IsNumeric(vFolderID) Then
        lFolderID = vFolderID
    Else
        lFolderID = gtFolder.lFolderID
    End If
    If IsMissing(vAckString) Then
        fAckString = False
        stAckString = ""
    Else
        fAckString = True
        stAckString = vAckString & ""
    End If

    ' Prepare message data
    lExerNum = gsEID
    stMsgDest = NsGenFEMISMsgDest(lExerNum)
    If vActionDate <> 0 Then stActionDate = Format$(vActionDate, "DD-MMM-YYYY HH:MM:SS")

    ' Set message parameter values
    With oMsg
        .MsgName = stActionName
        .MsgDest = stMsgDest
        .MsgData(NS_MsgClass_IDX) = NS_MSG_CLASS_ACTION

        .MsgData(NS_AC_ActionType_IDX) = stActionType
        .MsgData(NS_AC_ActionStatus_IDX) = stActionStatus
        .MsgData(NS_AC_ActionDate_IDX) = stActionDate
        .MsgData(NS_AC_ItemID1_IDX) = stItemID1
        .MsgData(NS_AC_ItemID2_IDX) = stItemID2
        .MsgData(NS_AC_AuxData1_IDX) = stAuxData1
        .MsgData(NS_AC_AuxData2_IDX) = stAuxData2
        .MsgData(NS_AC_AuxData3_IDX) = stAuxData3
        .MsgData(NS_AC_AuxData4_IDX) = stAuxData4
        .MsgData(NS_AC_AuxData5_IDX) = stAuxData5

        .MsgData(NS_AC_SiteCode_IDX) = gstUCurSite
        .MsgData(NS_AC_EOCCode_IDX) = gstUCurEOCCode
        .MsgData(NS_AC_HazardID_IDX) = glCurrHazID
        .MsgData(NS_AC_ExerID_IDX) = lExerNum
        .MsgData(NS_AC_FolderID_IDX) = lFolderID

        If fAckString = False Then
            .MsgData(NS_AC_AckFlag_IDX) = "N"
        Else
            .MsgData(NS_AC_AckFlag_IDX) = "Y"
            .MsgData(NS_AC_AckString_IDX) = stAckString
        End If
    End With

    ' Set automatic data-driven parameter values
    If tDDParams.stDDHandler <> "(none)" Then
        tDDParams.stDDHandler = NS_DDHANDLER_ACTION
        ulMsgFlags = ulMsgFlags Or NS_EF_TO_AUX_PROCESS
    End If

    ' Add data-driven parameters to the message
    If NsSetFMsgDDParams(oMsg, tDDParams) < 0 Then
        Exit Function
    End If

    ' Post the message
    If oMsg.PostMsg(ulLinkID, ulMsgFlags) = False Then
        Exit Function
    End If

    ' Normal exit
    NsPostActionMsgDD = 0               'status = SUCCESS
    Exit Function                       'all done

    ' Error exit
Error_Exit:
    ifxMsgBox "NsPostActionMsgDD: Error " & Err & ":  " & Error$
    NsPostActionMsgDD = -2             'status = FAILURE
End Function

Public Function NsPostDataChangeMsg(ByVal ulLinkID As Long, _
                                    ByVal ulMsgFlags As Long, _
                                    ByVal stNotifName As String, _
                                    ByVal stChangeType As String, _
                                    ByVal stDatasetName As String, _
                                    ByVal vChangeDate As Date, _
                                    Optional vItemID1, _
                                    Optional vItemID2, _
                                    Optional vAuxData1, _
                                    Optional vAuxData2, _
                                    Optional vAuxData3, _
                                    Optional vAuxData4, _
                                    Optional vAuxData5, _
                                    Optional vFolderID, _
                                    Optional vAckString _
                                    ) As Integer

    '-------------------------------------------------------------------------
    'Purpose: Post a DataChange notification message
    '
    'Usage:   iSC = NsPostDataChangeMsg (ulLinkID, ulMsgFlags, stNotifName, stChangeType, stDatasetName, vChangeDate, vItemID1, vItemID2, vAuxData1, vAuxData2)
    '
    'Arguments:
    '   ulLinkID        - notification link ID  (0 = Anonymous)
    '   ulMsgFlags      - message attribute flags
    '   stNotifName     - notification name
    '   stChangeType    - change type
    '   stDatasetName   - dataset name
    '   vChangeDate     - change date-time
    '   vItemID1        - item ID 1  (optional)
    '   vItemID2        - item ID 2  (optional)
    '   vAuxData1       - auxiliary data 1  (optional)
    '   vAuxData2       - auxiliary data 2  (optional)
    '   vAuxData3       - auxiliary data 3  (optional)
    '   vAuxData4       - auxiliary data 4  (optional)
    '   vAuxData5       - auxiliary data 5  (optional)
    '   vFolderID       - folder ID (optional)
    '   vAckString      - data acknowledgement string (optional)
    '   iSC             - status code
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '-------------------------------------------------------------------------
    '
    'Description:
    '   This function posts a DataChange notification message using the
    '   supplied parameters and the current exercise number.
    '
    '   If the message cannot be posted, the function returns a negative
    '   status code.
    '
    '   Message attribute flags can be used to control how the message is
    '   processed.  The following flags are available:
    '
    '       NS_EF_DEFAULT       - default attributes
    '       NS_EF_NORMAL        - distributed to local clients and server
    '       NS_EF_LOCAL_ONLY    - distributed only to clients on your PC
    '       NS_EF_GLOBAL        - forwarded to ALL notification servers
    '
    '   These flags can be ORed together to produce any desired combination
    '   of attributes.
    '
    '   To prevent notification loopback, each message is associated with an
    '   message source ID (ulLinkID).  If the message source is unknown or
    '   not important, you can use ID = 0 to make anonymous message postings.
    '-------------------------------------------------------------------------
    'Note: This function automatically adds a NS_DD_ARRIVED flag to the message.
    '-------------------------------------------------------------------------

    Dim tDDParams   As NS_DD_PARAMS     'data-driven parameters
    Dim iRC         As Integer          'temp return code

    ' Setup
    tDDParams.stDDHandler = "(none)"    'set no-DD flag
    tDDParams.lDDFlags = NS_DD_ARRIVED Or tDDParams.lDDFlags  'set ARRIVED flag

    ' Post DataChange message with dummy DD parameters
    iRC = NsPostDataChangeMsgDD(ulLinkID, ulMsgFlags, tDDParams, stNotifName, stChangeType, stDatasetName, vChangeDate, vItemID1, vItemID2, vAuxData1, vAuxData2, vAuxData3, vAuxData4, vAuxData5, vFolderID)

    ' Return result
    NsPostDataChangeMsg = iRC           'return status code
End Function

Public Function NsPostDataChangeMsgDD(ByVal ulLinkID As Long, _
                                      ByVal ulMsgFlags As Long, _
                                      tDDParams As NS_DD_PARAMS, _
                                      ByVal stNotifName As String, _
                                      ByVal stChangeType As String, _
                                      ByVal stDatasetName As String, _
                                      ByVal vChangeDate As Date, _
                                      Optional vItemID1, _
                                      Optional vItemID2, _
                                      Optional vAuxData1, _
                                      Optional vAuxData2, _
                                      Optional vAuxData3, _
                                      Optional vAuxData4, _
                                      Optional vAuxData5, _
                                      Optional vFolderID, _
                                      Optional vAckString _
                                      ) As Integer

    '-------------------------------------------------------------------------
    'Purpose: Post a DataChange notification message with Data-Driven parameters
    '
    'Usage:   iSC = NsPostDataChangeMsgDD (ulLinkID, ulMsgFlags, tDDParams, stNotifName, stChangeType, stDatasetName, vChangeDate, vItemID1, vItemID2, vAuxData1, vAuxData2)
    '
    'Arguments:
    '   ulLinkID        - notification link ID  (0 = Anonymous)
    '   ulMsgFlags      - message attribute flags
    '   tDDParams       - data-driven processing parameters
    '   stNotifName     - notification name
    '   stChangeType    - change type
    '   stDatasetName   - dataset name
    '   vChangeDate     - change date-time
    '   vItemID1        - item ID 1  (optional)
    '   vItemID2        - item ID 2  (optional)
    '   vAuxData1       - auxiliary data 1  (optional)
    '   vAuxData2       - auxiliary data 2  (optional)
    '   vAuxData3       - auxiliary data 3  (optional)
    '   vAuxData4       - auxiliary data 4  (optional)
    '   vAuxData5       - auxiliary data 5  (optional)
    '   vFolderID       - folder ID (optional)
    '   vAckString      - data acknowledgement string (optional)
    '   iSC             - status code
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '-------------------------------------------------------------------------
    '
    'Description:
    '   This function posts a DataChange notification message using the
    '   supplied parameters and the current exercise number.
    '
    '   If the message cannot be posted, the function returns a negative
    '   status code.
    '
    '   Message attribute flags can be used to control how the message is
    '   processed.  The following flags are available:
    '
    '       NS_EF_DEFAULT       - default attributes
    '       NS_EF_NORMAL        - distributed to local clients and server
    '       NS_EF_LOCAL_ONLY    - distributed only to clients on your PC
    '       NS_EF_GLOBAL        - forwarded to ALL notification servers
    '
    '   These flags can be ORed together to produce any desired combination
    '   of attributes.
    '
    '   To prevent notification loopback, each message is associated with an
    '   message source ID (ulLinkID).  If the message source is unknown or
    '   not important, you can use ID = 0 to make anonymous message postings.
    '-------------------------------------------------------------------------
    '05/29/02  LRS  Modified to use FNotifyMsg object.

    Dim oMsg            As FNotifyMsg   'notification message object
    Dim stMsgDest       As String       'message destination
    Dim stChangeDate    As String       'change timestamp
    Dim stItemID1       As String       'item ID 1
    Dim stItemID2       As String       'item ID 2
    Dim stAuxData1      As String       'auxiliary data 1
    Dim stAuxData2      As String       'auxiliary data 2
    Dim stAuxData3      As String       'auxiliary data 3
    Dim stAuxData4      As String       'auxiliary data 4
    Dim stAuxData5      As String       'auxiliary data 5
    Dim lExerNum        As Long         'exercise number
    Dim lFolderID       As Long         'folder ID
    Dim stAckString     As String       'data acknowlegment string
    Dim fAckString      As Boolean      'HAVE ACK STRING flag
    Dim iRC             As Integer      'temp return code

    ' Setup
    On Error GoTo Error_Exit
    NsPostDataChangeMsgDD = -2         'default status = FAILURE

    ' Create message object
    Set oMsg = New FNotifyMsg
    If oMsg Is Nothing Then
        Exit Function
    End If

    ' Fill optional arguments
    If Not IsMissing(vItemID1) Then stItemID1 = vItemID1
    If Not IsMissing(vItemID2) Then stItemID2 = vItemID2
    If Not IsMissing(vAuxData1) Then stAuxData1 = vAuxData1
    If Not IsMissing(vAuxData2) Then stAuxData2 = vAuxData2
    If Not IsMissing(vAuxData3) Then stAuxData3 = vAuxData3
    If Not IsMissing(vAuxData4) Then stAuxData4 = vAuxData4
    If Not IsMissing(vAuxData5) Then stAuxData5 = vAuxData5
    If IsNumeric(vFolderID) Then
        lFolderID = vFolderID
    Else
        lFolderID = gtFolder.lFolderID
    End If
    If IsMissing(vAckString) Then
        fAckString = False
        stAckString = ""
    Else
        fAckString = True
        stAckString = vAckString & ""
    End If

    ' Prepare message data
    lExerNum = gsEID
    stMsgDest = NsGenFEMISMsgDest(lExerNum)
    If vChangeDate <> 0 Then stChangeDate = Format$(vChangeDate, "DD-MMM-YYYY HH:MM:SS")

    ' Set message parameter values
    With oMsg
        .MsgName = stNotifName
        .MsgDest = stMsgDest
        .MsgData(NS_MsgClass_IDX) = NS_MSG_CLASS_DATA_CHANGE

        .MsgData(NS_DC_ChangeType_IDX) = stChangeType
        .MsgData(NS_DC_DatasetName_IDX) = stDatasetName
        .MsgData(NS_DC_ChangeDate_IDX) = stChangeDate
        .MsgData(NS_DC_ItemID1_IDX) = stItemID1
        .MsgData(NS_DC_ItemID2_IDX) = stItemID2
        .MsgData(NS_DC_AuxData1_IDX) = stAuxData1
        .MsgData(NS_DC_AuxData2_IDX) = stAuxData2
        .MsgData(NS_DC_AuxData3_IDX) = stAuxData3
        .MsgData(NS_DC_AuxData4_IDX) = stAuxData4
        .MsgData(NS_DC_AuxData5_IDX) = stAuxData5

        .MsgData(NS_DC_SiteCode_IDX) = gstUCurSite
        .MsgData(NS_DC_EOCCode_IDX) = gstUCurEOCCode
        .MsgData(NS_DC_HazardID_IDX) = glCurrHazID
        .MsgData(NS_DC_ExerID_IDX) = lExerNum
        .MsgData(NS_DC_FolderID_IDX) = lFolderID

        If fAckString = False Then
            .MsgData(NS_DC_AckFlag_IDX) = "N"
        Else
            .MsgData(NS_DC_AckFlag_IDX) = "Y"
            stAckString = Replace(Replace(Replace(stAckString, vbCrLf, " "), "'", ""), "\", "")
            .MsgData(NS_DC_AckString_IDX) = stAckString
        End If

        ' Set automatic data-driven parameter values
        If tDDParams.stDDHandler <> "(none)" Then
            tDDParams.stDDHandler = NS_DDHANDLER_ACTION
            ulMsgFlags = ulMsgFlags Or NS_EF_TO_AUX_PROCESS
        End If
    End With

    ' Add data-driven parameters to the message
    If NsSetFMsgDDParams(oMsg, tDDParams) < 0 Then
        Exit Function
    End If

    ' Post the message
    If oMsg.PostMsg(ulLinkID, ulMsgFlags) = False Then
        Exit Function
    End If

    ' Normal exit
    NsPostDataChangeMsgDD = 0          'status = SUCCESS
    Exit Function                       'all done

    ' Error exit
Error_Exit:
    ifxMsgBox "NsPostDataChangeMsgDD: Error " & Err & ":  " & Error$
    NsPostDataChangeMsgDD = -2         'return FAILURE
End Function

Public Function NsPostMsg(ByVal ulLinkID As Long, ByVal ulMsgFlags As Long, tMsgData As NS_MSG_DATA) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Post a generic notification message
    '
    'Usage:   iSC = NsPostMsg(ulLinkID, ulMsgFlags, tMsgData)
    '
    'Arguments:
    '   ulLinkID    - notification link ID  (0 = Anonymous)
    '   ulMsgFlags  - message attribute flags
    '   tMsgData    - message data structure
    '   iSC         - status code
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '-------------------------------------------------------------------------
    '
    'Description:
    '   This function posts a notification message from data in a generic
    '   message structure.
    '
    '   If the message cannot be posted, the function returns a negative
    '   status code.
    '
    '   Message attribute flags can be used to control how the message is
    '   processed.  The following flags are available:
    '
    '       NS_EF_DEFAULT       - default attributes
    '       NS_EF_NORMAL        - distributed to local clients and server
    '       NS_EF_LOCAL_ONLY    - distributed only to clients on your PC
    '       NS_EF_GLOBAL        - forwarded to ALL notification servers
    '
    '   These flags can be ORed together to produce any desired combination
    '   of attributes.
    '
    '   To prevent notification loopback, each message is associated with an
    '   message source ID (ulLinkID).  If the message source is unknown or
    '   not important, you can use ID = 0 to make anonymous message postings.
    '-------------------------------------------------------------------------
    '05/29/02  LRS  Modified to use FNotifyMsg object.

    Dim oMsg    As FNotifyMsg           'message object

    ' Setup
    On Error GoTo Error_Exit
    NsPostMsg = -2                     'default status = FAILURE

    ' Create a message object
    Set oMsg = New FNotifyMsg
    If oMsg Is Nothing Then
        Exit Function
    End If

    ' Copy data to the message object
    If Ns_MSGDATA_to_FNotifyMsg(tMsgData, oMsg) = False Then
        Exit Function
    End If

    ' Post the message
    If oMsg.PostMsg(ulLinkID, ulMsgFlags) = False Then
        Exit Function
    End If

    ' Return result
    NsPostMsg = 0                      'status = SUCCESS
    Exit Function

    ' Error exit
Error_Exit:
    ifxMsgBox "NsPostMsg: Error " & Err & ":  " & Error$, vbExclamation, "Error"
    NsPostMsg = -1                    'return FAILURE
End Function

Public Function NsPostMsg3P(ByVal ulLinkID As Long, ByVal ulMsgFlags As Long, ByVal stMsgName As String, ByVal stMsgDest As String, ByVal pszParm1 As String, ByVal pszParm2 As String, ByVal pszParm3 As String) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Post a notification message with 3 parameters
    '
    'Usage:   iSC = NsPostMsg3P (ulLinkID, ulMsgFlags, stMsgName, stMsgDest, pszParm1, pszParm2, pszParm3)
    '
    'Inputs:
    '   ulLinkID    - notification link ID  (0 = Anonymous)
    '   ulMsgFlags  - message attribute flags
    '   stMsgName   - message name
    '   stMsgDest   - message destination
    '   pszParm1    - parameter 1 text
    '   pszParm2    - parameter 2 text
    '   pszParm3    - parameter 3 text
    '   iSC         - status code
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '-------------------------------------------------------------------------
    '
    'Description:
    '   This function posts a notification message with three parameter values.
    '
    '   Message attribute flags can be used to control how the message is
    '   processed.  The following flags are available:
    '
    '       NS_EF_DEFAULT       - default attributes
    '       NS_EF_NORMAL        - distributed to local clients and server
    '       NS_EF_LOCAL_ONLY    - distributed only to clients on your PC
    '       NS_EF_GLOBAL        - forwarded to ALL notification servers
    '
    '   These flags can be ORed together to produce any desired combination of
    '   attributes.
    '
    '   To prevent notification loopback, each message is associated with an
    '   message source ID (ulLinkID).  If the message source is unknown or not
    '   important, you can use ID = 0 to make anonymous message postings.
    '-------------------------------------------------------------------------
    '05/29/02  LRS  Modified to use FNotifyMsg object.

    Dim oMsg    As FNotifyMsg           'message object

    ' Setup
    On Error GoTo Error_Exit
    NsPostMsg3P = -2                    'default status = FAILURE

    ' Create a message object
    Set oMsg = New FNotifyMsg
    If oMsg Is Nothing Then
        Exit Function
    End If

    ' Set message parameter values
    With oMsg
        .MsgName = stMsgName
        .MsgDest = stMsgDest
        .MsgData(0) = pszParm1
        .MsgData(1) = pszParm2
        .MsgData(2) = pszParm3
    End With

    ' Post the message
    If oMsg.PostMsg(ulLinkID, ulMsgFlags) = False Then
        Exit Function
    End If

    ' Normal exit
    NsPostMsg3P = 0                     'status = SUCCESS
    Exit Function

    ' Error exit
Error_Exit:
    ifxMsgBox "NsPostMsg3P: Error " & Err & ":  " & Error$
    NsPostMsg3P = -2                    'status = FAILURE
End Function

Function NxRegisterInterest(ByVal ulLinkID As Long, ByVal stMsgName As String, ByVal stMsgDest As String) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Register interest in a notification message
    '
    'Usage:   <integer> = NxRegisterInterest (ulLinkID, stMsgName, stMsgDest)
    '
    'Inputs:  long   ulLinkID   - notification link ID
    '         string stMsgName  - message name
    '         string stMsgDest  - message destination
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '
    'Description:
    '   This function registers a client's interest in a specific message and
    '   enables the client to be notified whenever that message occurs.  This
    '   interest remains in effect until the link is closed or
    '   iNSRemoveInterest() is called.
    '
    'Note:  Since this function registers interest in a specific message name,
    ' each unique message name must be registered separately.
    '-------------------------------------------------------------------------

    Dim iRC As Integer

    ' Register interest
    iRC = AnRegisterInterest(ulLinkID, stMsgName, stMsgDest)

    ' Return result
    NxRegisterInterest = iRC
End Function

Function NxRemoveInterest(ByVal ulLinkID As Long, ByVal stMsgName As String, ByVal stMsgDest As String) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Remove interest in a notification message
    '
    'Usage:   <integer> = NxRemoveInterest (ulLinkID, stMsgName, stMsgDest)
    '
    'Inputs:  long   ulLinkID   - notification link ID
    '         string stMsgName  - message name
    '         string stMsgDest  - message destination
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '
    'Description:
    '   This function removes a client's interest in a specific message and
    '   removes the client from the distribution list for that message.
    '
    '   Note:  All active interests are automatically removed during by the
    '   iNSCloseLink() function, so it is not necessary to remove interests
    '   before closing a notification link.
    '
    'Note:  Since this function removes interest in a specific message name,
    ' each unique message name must be removed separately.
    '-------------------------------------------------------------------------

    Dim iRC As Integer

    ' Remove interest
    iRC = AnRemoveInterest(ulLinkID, stMsgName, stMsgDest)

    ' Return result
    NxRemoveInterest = iRC
End Function

Public Sub NsSetDDParams(tDDParams As NS_DD_PARAMS, _
                         ByVal stTableName As String, _
                         ByVal stRowID As String, _
                         ByVal vXmitInitDate As Date, _
                         ByVal stAction As String, _
                         ByVal stListener As String, _
                         ByVal lDDFlags As Long)

    '-------------------------------------------------------------------------
    'Purpose: Populate a Data-Driven Notification parameter structure
    'Returns: nothing
    '-------------------------------------------------------------------------
    'Arguments:
    '   tDDParams           - parameter structure
    '   stTableName         - primary table name
    '   stRowID             - primary row ID
    '   vXmitInitDate       - primary timestamp
    '   stAction            - primary action
    '   stListener          - database listener name
    '   lDDFlags            - data-driven notification flags
    '-------------------------------------------------------------------------
    'Note: This function assumes that the target structure is already initialized.
    '-------------------------------------------------------------------------

    Const iIndex = 0            'array index for primary table

    ' Populate parameter structure
    NsSetDDParamsT1 tDDParams, stTableName, stRowID, vXmitInitDate, stAction
    tDDParams.stListener = stListener                   'database listener name
    tDDParams.lDDFlags = lDDFlags                       'data-driven flags
    tDDParams.stEOCCode = gstUCurEOCCode                'EOC code
End Sub

Public Sub NsSetDDParamsT1(tDDParams As NS_DD_PARAMS, _
                           ByVal stTableName As String, _
                           ByVal stRowID As String, _
                           ByVal vXmitInitDate As Date, _
                           ByVal stAction As String)

    '-------------------------------------------------------------------------
    'Purpose: Set Data-Driven Notification parameters for primary table
    'Returns: nothing
    '-------------------------------------------------------------------------
    'Arguments:
    '   tDDParams           - parameter structure
    '   stTableName         - table name
    '   stRowID             - row ID
    '   vXmitInitDate       - timestamp
    '   stAction            - action
    '-------------------------------------------------------------------------
    'Note: This function assumes that the target structure is already initialized.
    '-------------------------------------------------------------------------

    Const iIndex = 0            'array index for this table

    ' Set Data-Driven Notification parameters for table
    NsSetDDParamsTN tDDParams, iIndex, stTableName, stRowID, vXmitInitDate, stAction
End Sub

Public Sub NsSetDDParamsT2(tDDParams As NS_DD_PARAMS, _
                           ByVal stTableName As String, _
                           ByVal stRowID As String, _
                           ByVal vXmitInitDate As Date, _
                           ByVal stAction As String)

    '-------------------------------------------------------------------------
    'Purpose: Set Data-Driven Notification parameters for second table
    'Returns: nothing
    '-------------------------------------------------------------------------
    'Arguments:
    '   tDDParams           - parameter structure
    '   stTableName         - table name
    '   stRowID             - row ID
    '   vXmitInitDate       - timestamp
    '   stAction            - action
    '-------------------------------------------------------------------------
    'Note: This function assumes that the target structure is already initialized.
    '-------------------------------------------------------------------------

    Const iIndex = 1            'array index for this table

    ' Set Data-Driven Notification parameters for table
    NsSetDDParamsTN tDDParams, iIndex, stTableName, stRowID, vXmitInitDate, stAction
End Sub

Public Sub NsSetDDParamsT3(tDDParams As NS_DD_PARAMS, _
                           ByVal stTableName As String, _
                           ByVal stRowID As String, _
                           ByVal vXmitInitDate As Date, _
                           ByVal stAction As String)

    '-------------------------------------------------------------------------
    'Purpose: Set Data-Driven Notification parameters for third table
    'Returns: nothing
    '-------------------------------------------------------------------------
    'Arguments:
    '   tDDParams           - parameter structure
    '   stTableName         - table name
    '   stRowID             - row ID
    '   vXmitInitDate       - timestamp
    '   stAction            - action
    '-------------------------------------------------------------------------
    'Note: This function assumes that the target structure is already initialized.
    '-------------------------------------------------------------------------

    Const iIndex = 2            'array index for this table

    ' Set Data-Driven Notification parameters for table
    NsSetDDParamsTN tDDParams, iIndex, stTableName, stRowID, vXmitInitDate, stAction
End Sub

Public Sub NsSetDDParamsTN(tDDParams As NS_DD_PARAMS, _
                           ByVal iIndex As Integer, _
                           ByVal stTableName As String, _
                           ByVal stRowID As String, _
                           ByVal vXmitInitDate As Date, _
                           ByVal stAction As String)

    '-------------------------------------------------------------------------
    'Purpose: Set Data-Driven Notification parameters for a table, by index
    'Returns: nothing
    '-------------------------------------------------------------------------
    'Arguments:
    '   tDDParams           - parameter structure
    '   iIndex              - DD table array index (0-based)
    '   stTableName         - table name
    '   stRowID             - row ID
    '   vXmitInitDate       - timestamp
    '   stAction            - action
    '-------------------------------------------------------------------------
    'Note: This function assumes that the target structure is already initialized.
    '-------------------------------------------------------------------------

    ' Check arguments
    If iIndex < 0 Or iIndex > 2 Then Exit Sub           'exit if not valid

    ' Set Data-Driven Notification parameters
    tDDParams.stTableName(iIndex) = stTableName         'table name
    tDDParams.stRowID(iIndex) = stRowID                 'row ID
    tDDParams.vXmitInitDate(iIndex) = vXmitInitDate     'timestamp
    tDDParams.stAction(iIndex) = stAction               'action
End Sub

Public Function NsSetFMsgDDParams(oMsg As FNotifyMsg, tDDParams As NS_DD_PARAMS) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Set the Data-Driven parameters in an FNotifyMsg object
    '
    'Usage:   iSC = NsSetFMsgDDParams (oMsg, tDDParams)
    '
    'Arguments:
    '   oMsg        - message object
    '   tDDParams   - data-driven processing parameters
    '   iSC         - status code
    '
    'Outputs: none
    '
    'Returns: SUCCESS (0), status code (+), or error code (-)
    '-------------------------------------------------------------------------
    'Note: This function assumes that the target structure is already initialized.
    '-------------------------------------------------------------------------
    '05/29/02  LRS  Modified to use FNotifyMsg object.

    Dim stXmitInitDate(3) As String     'timestamp strings

    ' Prepare values
    If tDDParams.vXmitInitDate(0) <> 0 Then _
        stXmitInitDate(0) = Format$(tDDParams.vXmitInitDate(0), NS_DD_XmitInitDate_FMT)
    If tDDParams.vXmitInitDate(1) <> 0 Then _
        stXmitInitDate(1) = Format$(tDDParams.vXmitInitDate(1), NS_DD_XmitInitDate_FMT)
    If tDDParams.vXmitInitDate(2) <> 0 Then _
        stXmitInitDate(2) = Format$(tDDParams.vXmitInitDate(2), NS_DD_XmitInitDate_FMT)

    ' Set message parameter values
    With oMsg
        .MsgData(NS_DD_TblName1_IDX) = tDDParams.stTableName(0)
        .MsgData(NS_DD_TblName2_IDX) = tDDParams.stTableName(1)
        .MsgData(NS_DD_TblName3_IDX) = tDDParams.stTableName(2)
        .MsgData(NS_DD_RowID1_IDX) = tDDParams.stRowID(0)
        .MsgData(NS_DD_RowID2_IDX) = tDDParams.stRowID(1)
        .MsgData(NS_DD_RowID3_IDX) = tDDParams.stRowID(2)
        .MsgData(NS_DD_XmitInitDate1_IDX) = stXmitInitDate(0)
        .MsgData(NS_DD_XmitInitDate2_IDX) = stXmitInitDate(1)
        .MsgData(NS_DD_XmitInitDate3_IDX) = stXmitInitDate(2)
        .MsgData(NS_DD_Action1_IDX) = tDDParams.stAction(0)
        .MsgData(NS_DD_Action2_IDX) = tDDParams.stAction(1)
        .MsgData(NS_DD_Action3_IDX) = tDDParams.stAction(2)
        .MsgData(NS_DD_Listener_IDX) = tDDParams.stListener
        .MsgData(NS_DD_DDFlags_IDX) = tDDParams.lDDFlags
        .MsgData(NS_DD_EOCCode_IDX) = tDDParams.stEOCCode
        .MsgData(NS_DD_DDHandler_IDX) = tDDParams.stDDHandler

        ' Set AuxProcess info
        .AuxProcessIdent = tDDParams.stDDHandler
    End With

    ' Return result
    NsSetFMsgDDParams = 0               'status = SUCCESS
End Function

Public Sub NsSetMsgActionInfo(tMsgData As NS_MSG_DATA, tActionData As NS_ACTION_PARAMS)
    '-------------------------------------------------------------------------
    'Purpose: Set the Action info in a generic notification message.
    '
    'Usage:   iSC = NsSetMsgActionInfo (tMsgData, tActionData)
    '
    'Arguments:
    '   tMsgData    - message data structure
    '   tActionData - Action data structure
    '   iSC         - status code
    '
    'Outputs: none
    '
    'Returns: nothing
    '-------------------------------------------------------------------------
    'Note: This function assumes that the target structure is already initialized.
    '-------------------------------------------------------------------------

    ' Set the Action info in the generic notification message.
    With tActionData
        tMsgData.stMsgName = .stActionName
        tMsgData.stMsgParm(NS_AC_ActionType_IDX) = .stActionType
        tMsgData.stMsgParm(NS_AC_ActionStatus_IDX) = .stActionStatus
        If .vActionDate <> 0 Then
            tMsgData.stMsgParm(NS_AC_ActionDate_IDX) = Format$(.vActionDate, "DD-MMM-YYYY HH:MM:SS")
        Else
            tMsgData.stMsgParm(NS_AC_ActionDate_IDX) = ""
        End If
        tMsgData.stMsgParm(NS_AC_ItemID1_IDX) = .stItemID1
        tMsgData.stMsgParm(NS_AC_ItemID2_IDX) = .stItemID2
        tMsgData.stMsgParm(NS_AC_AuxData1_IDX) = .stAuxData(0)
        tMsgData.stMsgParm(NS_AC_AuxData2_IDX) = .stAuxData(1)
        tMsgData.stMsgParm(NS_AC_AuxData3_IDX) = .stAuxData(2)
        tMsgData.stMsgParm(NS_AC_AuxData4_IDX) = .stAuxData(3)
        tMsgData.stMsgParm(NS_AC_AuxData5_IDX) = .stAuxData(4)

        tMsgData.stMsgParm(NS_AC_SiteCode_IDX) = .stSiteCode
        tMsgData.stMsgParm(NS_AC_EOCCode_IDX) = .stEOCCode
        tMsgData.stMsgParm(NS_AC_HazardID_IDX) = .stHazardID
        tMsgData.stMsgParm(NS_AC_ExerID_IDX) = .stExerID
        tMsgData.stMsgParm(NS_AC_FolderID_IDX) = .stFolderID
    End With

    ' Set the Data-Driven parameter info
    NsSetMsgDDParams tMsgData, tActionData.tDDParams
End Sub

Public Sub NsSetMsgDDParams(tMsgData As NS_MSG_DATA, tDDParams As NS_DD_PARAMS)
    '-------------------------------------------------------------------------
    'Purpose: Set the DataDriven parameter info in a generic notification message.
    '
    'Usage:   iSC = NsSetMsgDDParams (tMsgData, tDDParams)
    '
    'Arguments:
    '   tMsgData    - message data structure
    '   tDDParams   - data-driven processing parameters
    '   iSC         - status code
    '
    'Outputs: none
    '
    'Returns: nothing
    '-------------------------------------------------------------------------
    'Note: This function assumes that the target structure is already initialized.
    '-------------------------------------------------------------------------

    Dim stXmitInitDate(3) As String        'record timestamps

    ' Prepare timestamp strings
    If tDDParams.vXmitInitDate(0) <> 0 Then stXmitInitDate(0) = Format$(tDDParams.vXmitInitDate(0), "DD-MMM-YYYY HH:MM:SS")
    If tDDParams.vXmitInitDate(1) <> 0 Then stXmitInitDate(1) = Format$(tDDParams.vXmitInitDate(0), "DD-MMM-YYYY HH:MM:SS")
    If tDDParams.vXmitInitDate(2) <> 0 Then stXmitInitDate(2) = Format$(tDDParams.vXmitInitDate(0), "DD-MMM-YYYY HH:MM:SS")

    ' Set the DataDriven parameter info in the generic notification message.
    With tMsgData
        .stMsgParm(NS_DD_TblName1_IDX) = tDDParams.stTableName(0)
        .stMsgParm(NS_DD_TblName2_IDX) = tDDParams.stTableName(1)
        .stMsgParm(NS_DD_TblName3_IDX) = tDDParams.stTableName(2)
        .stMsgParm(NS_DD_RowID1_IDX) = tDDParams.stRowID(0)
        .stMsgParm(NS_DD_RowID2_IDX) = tDDParams.stRowID(1)
        .stMsgParm(NS_DD_RowID3_IDX) = tDDParams.stRowID(2)
        .stMsgParm(NS_DD_XmitInitDate1_IDX) = stXmitInitDate(0)
        .stMsgParm(NS_DD_XmitInitDate2_IDX) = stXmitInitDate(1)
        .stMsgParm(NS_DD_XmitInitDate3_IDX) = stXmitInitDate(2)
        .stMsgParm(NS_DD_Action1_IDX) = tDDParams.stAction(0)
        .stMsgParm(NS_DD_Action2_IDX) = tDDParams.stAction(1)
        .stMsgParm(NS_DD_Action3_IDX) = tDDParams.stAction(2)
        .stMsgParm(NS_DD_Listener_IDX) = tDDParams.stListener
        .stMsgParm(NS_DD_DDFlags_IDX) = tDDParams.lDDFlags
        .stMsgParm(NS_DD_EOCCode_IDX) = tDDParams.stEOCCode
        .stMsgParm(NS_DD_DDHandler_IDX) = tDDParams.stDDHandler
    End With
End Sub

Function NxVerifyInterest(ByVal ulLinkID As Long, ByVal stMsgName As String, ByVal stMsgDest As String) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Verify interest in a notification message
    '
    'Usage:   <integer> = NxVerifyInterest (ulLinkID, stMsgName, stMsgDest)
    '
    'Inputs:  long   ulLinkID   - notification link ID
    '         string stMsgName  - message name
    '         string stMsgDest  - message destination
    '
    'Outputs: none
    '
    'Returns: True or False
    '
    'Description:
    '   This function verifies a client's interest in a specific message.
    '
    'Note:  Since this function verifies interest in a specific message name,
    ' each unique message name must be verified separately.
    '-------------------------------------------------------------------------

    Dim iRC As Integer

    ' Verify interest
    iRC = AnVerifyInterest(ulLinkID, stMsgName, stMsgDest)

    ' Return result
    If iRC = 0 Then
        NxVerifyInterest = False
    Else
        NxVerifyInterest = True
    End If
End Function

Function stAnsiStrToVB(ByVal pszAnsiStr As String) As String
    '-------------------------------------------------------------------------
    'Purpose: Convert a null-terminated ANSI string to a standard VB string
    '
    'Usage:   <string> = stAnsiStrToVB (pszAnsiStr)
    '
    'Inputs:  pszAnsiStr - null-terminated ANSI string or byte array
    '
    'Returns: VB string, without a null terminator
    '-------------------------------------------------------------------------
    '
    'Description:
    '   This function converts a null-terminated ANSI string or byte array to
    '   standard Visual Basic string.  This is useful for converting strings
    '   used in C or DLL functions.
    '
    '-------------------------------------------------------------------------
    'Note:  This function does not work with Unicode strings, since they use
    '   two bytes per character and may contain embedded zeros.
    '-------------------------------------------------------------------------
    'Warning: DO NOT remove ByVal qualifiers from the argument list!!  They are
    '   needed for automatic type conversions.
    '-------------------------------------------------------------------------
    'Warning: This function is for internal use only and may change without
    '   notice.
    '-------------------------------------------------------------------------

    Dim stStrBuf As String          'local buffer
    Dim iSLen As Integer            'source string length
    Dim i As Integer                'loop index
    Dim c As Integer                'character code

    ' Setup
    iSLen = LenB(pszAnsiStr)            'source string length, in bytes

    ' Convert string
    For i = 1 To iSLen
        c = AscB(MidB(pszAnsiStr, i, 1))  'get source character code
        If c = 0 Then                   'null terminator?
            Exit For                    'end of conversion
        End If
        stStrBuf = stStrBuf & Chr$(c)   'add character to result
    Next

    ' Return result
    stAnsiStrToVB = stStrBuf
End Function

Function stByteStrToVB(pszByteStr() As Byte) As String
    '-------------------------------------------------------------------------
    'Purpose: Convert null-terminated byte string to standard VB string
    '
    'Usage:   <string> = stByteStrToVB (pszByteStr)
    '
    'Inputs:  pszByteStr - null-terminated byte string
    '
    'Returns: Normal VB string, without null terminator
    '-------------------------------------------------------------------------
    '
    'Description:
    '   This function converts a null-terminated byte string to standard Visual
    '   Basic string.  This is useful for converting strings used in C or DLL
    '   functions.
    '
    '-------------------------------------------------------------------------
    'Warning: DO NOT remove ByVal qualifiers from the argument list!!  They are
    '   needed for automatic type conversions.
    '-------------------------------------------------------------------------
    'Warning: This function is for internal use only and may change without
    '   notice.
    '-------------------------------------------------------------------------

    Dim stStrBuf As String          'local buffer
    Dim iSLen As Integer            'source string length
    Dim i As Integer                'loop index
    Dim c As Integer                'character code

    ' Setup
    iSLen = 20000                       'use a high limit to avoid truncation

    ' Convert string
    For i = 1 To iSLen
        c = pszByteStr(i - 1)           'get source character code
        If c = 0 Then                   'null terminator?
            Exit For                    'end of conversion
        End If
        stStrBuf = stStrBuf & Chr$(c)   'add character to result
    Next

    ' Return result
    stByteStrToVB = stStrBuf
End Function

Function ulNSOpenQLink(ByVal ulLinkFlags As Long, frm As Form) As Long
    '-------------------------------------------------------------------------
    'Purpose: Open a queued notification client link
    '
    'Usage:   <long> = ulNSOpenQLink (ulLinkFlags, frm)
    '
    'Inputs:  long  ulLinkFlags - link attribute flags  (0 = none)
    '         form  frm         - client form
    '
    'Outputs: none
    '
    'Returns: link ID (non-zero) or FAILURE (zero)
    '-------------------------------------------------------------------------
    '
    'Description:
    '   This function opens a queued notification client link and returns a unique
    '   link ID (0 = failure).  This link ID is used in all future operations
    '   for that link.
    '
    '   Link attribute flags can be used to control how the link operates.
    '   The following flags are available:
    '
    '       NS_LF_DEFAULT       - default attributes
    '       NS_LF_NORMAL        - no loopback
    '       NS_LF_LOOPBACK_OK   - allows link to notify itself
    '
    '   These flags can be ORed together to produce any desired combination of
    '   attributes.
    '
    'Caution:  Each VB form that creates a notification link should call
    ' iNSCloseLink() in its shutdown process (Form_Unload event).  Otherwise,
    ' the notification link becomes an orphan and its resources are never
    ' released.
    '-------------------------------------------------------------------------

    Dim ulLinkID As Long

    ' Set error trap
    On Error Resume Next    'needed if frm object variable is not set

    ' Initialize globals, if necessary
    If giNotifWinMsgNo = 0 Then
        InitNotifGlobals
    End If

    ' Add default flags
    ulLinkFlags = ulLinkFlags Or NS_LF_LATEST_INSTANCE_ONLY

    ' Open notification link
    ulLinkID = AnOpenLinkWMQ(ulLinkFlags, frm.hwnd, giNotifWinMsgNo)

    ' Return result
    ulNSOpenQLink = ulLinkID
End Function

'=============================================================================
'Purpose: Display a VB message box and input a response from the user
'Returns: Response value
'=============================================================================
' This function adds additional message flags to make sure the message is not
' hidden behind other windows.
'=============================================================================
'
Private Function ifxMsgBox(ByVal stPrompt As String, _
                           Optional ByVal iButtons As Long = 0, _
                           Optional ByVal stTitle As String = vbNullString _
                          ) As Integer

    Dim stT As String

    On Error Resume Next

    stT = stTitle
    If (stT = vbNullString) Then stT = App.Title

    '**See if there is already a modal window in the current app.  If so, then ... do what????
    '****** MAY NEED CODE HERE TO CHECK IF THERE IS ALREADY A MODAL WINDOW IN THE CURRENT APP ********
    '
    '  AppActivate App.Title  '***Commented out on 01/03/01 RAB***
    ifxMsgBox = MsgBox(stPrompt, iButtons Or vbSystemModal, stT)  '***Added vbSystemModal (RE: LR Stoops) 01/03/01 RAB***
    '  ifxMsgBox = MsgBox(stPrompt, iButtons + vbSystemModal + VbMsgBoxSetForeground, stT)  ' ????
    Exit Function

End Function


