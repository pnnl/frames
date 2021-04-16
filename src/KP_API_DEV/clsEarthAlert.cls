VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "clsEarthAlert"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
Option Explicit

' Author:   Kevin Dorow
' Created:  2/14/2001
' Purpose:  This DLL will allow the Earth Alert RIMS server
'           to be able to retrieve and store information to
'           the EMAdvantage Oracle database.  It will also
'           facilitate communication between EMAdavantage
'           users both PC and PDA based via the EMAdvantage
'           P/S notification system.

' declare a reference to the femis decryption function
' Modified  9/25/04
'           The DLL has been modified to visualized the Power Grid
'           project.  This now adds a point to any user defined class
'           All points are shared and are assumed to be unique.
'           This dll will be called from Matlab using actxserver calls.
            
Private Declare Function FxstDecrypt Lib "FEMIS32.DLL" _
                        Alias "_FxScrnPrntS0@12" (ByVal pszStr$, ByVal pszKey$, _
                                                  ByVal pszDest$) As Integer

Private Const M_lBUFFER_LEN     As Long = 255           '** Buffer length for API strings
'....RCS file identifier
Private Const FNOTIF1_BAS_RCS_ID = "$Id: clsearthalert.cls 1.4 2005/07/12 16:17:02Z d3k077 Exp $"

' these are constants that should be extracted from an INI file
'Private Const gstUCurEOCCode = "SBCC"
'Private Const gstUCurEOCName = "SBCCOM"
'Private Const gsEId = 7
'Private Const mlFolderID = 10000664
'Private Const gstMessageDest = "/femis/CSEPP/SBCC/Exer.7"
'Private Const gstUListener = "fip"
'Private Const gstNotifServerName = "spar"
'Private Const gstNotifServerPort = 9021
'Made these public so we can access it from ThreatAreaFunction.bas

'Private mlfolderID As Long
Private gstMessageDest As String
Private gstUListener As String
Private gstNotifServerName As String
Private gstNotifServerPort As Integer
'Private gstUCurDataset As String
Private gstUListenerNum As String
Private gstIniFileDest As String

'-----------------------------------------------------------
' Define the different navigator indexes and IDs
'-----------------------------------------------------------
Private Const NAV_MIN_INDEX         As Integer = 1
Private Const NAV_MAX_INDEX         As Integer = 9

Private Const NAV_ACTIVITY_INDEX    As Integer = 1 'i.e. workplan
Private Const NAV_MODEL_INDEX       As Integer = 2 'i.e. D2PC
Private Const NAV_THREAT_INDEX      As Integer = 3
Private Const NAV_RISK_INDEX        As Integer = 4
Private Const NAV_COMMUNITY_INDEX   As Integer = 5
Private Const NAV_PAD_INDEX         As Integer = 6
Private Const NAV_PAR_INDEX         As Integer = 7
Private Const NAV_MITIGATION_INDEX  As Integer = 8  'i.e. evac
Private Const NAV_PLAN_SEL_INDEX    As Integer = 9  'button to select plan

Private mlNavIDs(NAV_MIN_INDEX, NAV_MAX_INDEX)



Private Const HKEY_LOCAL_MACHINE     As Long = &H80000002

                                                        

''....Structure definitions
'Private Type NS_DD_PARAMS                       '** data-driven notification parameters **
'    stTableName(3)      As String       'table names
'    stRowID(3)          As String       'row IDs
'    vXmitInitDate(3)    As Date         'timestamps
'    stAction(3)         As String       'actions
'    stListener          As String       'database listener name
'    lDDFlags            As Long         'data-driven flags
'    stEOCCode           As String       'EOC code
'    stDDHandler         As String       'server DD handler key
'End Type

Private Type NS_DATACHG_PARAMS                  '** DataChange notification message data **
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

Private Type NS_ACTION_PARAMS                  '** Action notification message data **
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

Private Type NS_MSG_DATA                        '** generic notification message data **
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

Private Type NSEVENT                            '** notification message data (old format) **
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
Private Type FEVTDATA2                          '** new DLL notification message structure **
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

Private Type FEVTDATA                           '** old DLL notification message structure **
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
Private Const FEVTDATA_FMT_1 = &H100             'format 1 code
Private Const FEVTDATA_FMT_2 = &H200             'format 2 code
Private Const FEVTDATA_FMT_STD = FEVTDATA_FMT_1  'standard format code


'....Private Constants
' notification message type codes
Private Const NS_MT_EVENTMSG = 5

' message attribute flags
Private Const NS_EF_NORMAL = &H0
Private Const NS_EF_LOCAL_ONLY = &H1
Private Const NS_EF_SVR_NOPOST = &H100000
Private Const NS_EF_TO_AUX_PROCESS = &H1000000
Private Const NS_EF_GLOBAL = &H10000000
Private Const NS_EF_DEFAULT = NS_EF_NORMAL

' link control flags
Private Const NS_LF_NORMAL = &H0
Private Const NS_LF_LOOPBACK_OK = &H2
Private Const NS_LF_LATEST_INSTANCE_ONLY = &H1000
Private Const NS_LF_DEFAULT = NS_LF_NORMAL

' message queue flags
Private Const NS_QF_DEFAULT = &H0

'....Private data
Private gstNotifWinMsgName As String
Private giNotifWinMsgNo As Integer               'window message number

'....Notification DLL function declarations
' Message functions
Private Declare Function AnPostEvent Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2) As Long
Private Declare Function AnPostEventLF Lib "FNOTIF32.DLL" (ByVal ulLinkID As Long, ByVal ulMsgFlags As Long, pEvtData As FEVTDATA2) As Long
Private Declare Function AnPostEvent3P Lib "FNOTIF32.DLL" (ByVal ulLinkID As Long, ByVal ulMsgFlags As Long, ByVal stMsgName As String, ByVal pszExerID As String, ByVal pszParm1 As String, ByVal pszParm2 As String, ByVal pszParm3 As String) As Long
Private Declare Function AnPostEventNV Lib "FNOTIF32.DLL" (ByVal ulLinkID As Long, ByVal ulMsgFlags As Long, ByVal stMsgName As String, ByVal pszExerID As String, ByVal lParmCt As Long, ByVal pszParmArrBase As String) As Long
Private Declare Function AnQueryEventBuffer Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA, ByVal ulRecID As Long) As Long
Private Declare Function AnQueryEventBoard Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA, ByVal stMsgName As String, ByVal pszExerID As String) As Long
Private Declare Function AnQueryEventBoardTimeSec Lib "FNOTIF32.DLL" (ByVal stMsgName As String, ByVal pszExerID As String) As Long
Private Declare Function AnIsQLinkEvent Lib "FNOTIF32.DLL" (ByVal ulLinkID As Long, ByVal ulRecID As Long) As Long
Private Declare Function AnGetQLinkEvent Lib "FNOTIF32.DLL" (ByVal ulLinkID As Long, pEvtData As FEVTDATA, ByVal ulRecID As Long) As Long
Private Declare Function AnGetQLinkEvent2 Lib "FNOTIF32.DLL" Alias "AnGetQLinkEvent" (ByVal ulLinkID As Long, pEvtData As FEVTDATA2, ByVal ulRecID As Long) As Long
Private Declare Function AnReleaseEvent Lib "FNOTIF32.DLL" (ByVal ulRecID As Long) As Long

' Link management
Private Declare Function AnOpenLinkWM Lib "FNOTIF32.DLL" (ByVal ulLinkFlags As Long, ByVal hWinHdl As Long, ByVal uWinMsg As Long) As Long
Private Declare Function AnOpenLinkWMQ Lib "FNOTIF32.DLL" (ByVal ulLinkFlags As Long, ByVal hWinHdl As Long, ByVal uWinMsg As Long) As Long
Private Declare Function AnOpenLinkWMQF Lib "FNOTIF32.DLL" (ByVal ulLinkFlags As Long, ByVal hWinHdl As Long, ByVal uWinMsg As Long, ByVal ulQueueLimit As Long, ByVal ulQueueFlags As Long) As Long
Private Declare Function AnCloseLink Lib "FNOTIF32.DLL" (ByVal ulLinkID As Long) As Long

Private Declare Function AnOpenServerLink Lib "FNOTIF32.DLL" (ByVal stServerHost As String, ByVal usServerPort As Long, ByVal ulLinkFlags As Long) As Long
Private Declare Function AnCloseServerLink Lib "FNOTIF32.DLL" (ByVal stServerHost As String, ByVal usServerPort As Long) As Long
Private Declare Function AnGetLinkServerInfo Lib "FNOTIF32.DLL" (ByVal ulLinkID As Long, plServerID As Long, ByVal pszServerHost As String, plServerPort As Long) As Long

' Message interests
Private Declare Function AnRegisterInterest Lib "FNOTIF32.DLL" (ByVal ulLinkID As Long, ByVal stMsgName As String, ByVal pszExerID As String) As Long
Private Declare Function AnRemoveInterest Lib "FNOTIF32.DLL" (ByVal ulLinkID As Long, ByVal stMsgName As String, ByVal pszExerID As String) As Long
Private Declare Function AnVerifyInterest Lib "FNOTIF32.DLL" (ByVal ulLinkID As Long, ByVal stMsgName As String, ByVal pszExerID As String) As Long

' Message Queue functions
Private Declare Function AnFlushEventQueues Lib "FNOTIF32.DLL" () As Long

' Support functions
Private Declare Function AnClientInit Lib "FNOTIF32.DLL" () As Long
Private Declare Function AnClientCleanup Lib "FNOTIF32.DLL" () As Long

' FEVTDATA functions
Private Declare Function AnInitFEVTDATA Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA, ByVal iFmtCode As Long) As Long
Private Declare Function AnInitFEVTDATA2 Lib "FNOTIF32.DLL" Alias "AnInitFEVTDATA" (pEvtData As FEVTDATA2, ByVal iFmtCode As Long) As Long

Private Declare Function AnFEVTDATA_MsgName Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2) As Long
Private Declare Function AnFEVTDATA_MsgDest Lib "FNOTIF32.DLL" Alias "AnFEVTDATA_ExerID" (pEvtData As FEVTDATA2) As Long
Private Declare Function AnFEVTDATA_ExerID Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2) As Long
Private Declare Function AnFEVTDATA_MsgParm Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2, ByVal iIndex As Long) As Long
Private Declare Function AnFEVTDATA_MsgParmArrLen Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2) As Long
Private Declare Function AnFEVTDATA_AuxProcessIdent Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2) As Long

Private Declare Function AnFEVTDATA_SetMsgName Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2, ByVal pszStr As String) As Long
Private Declare Function AnFEVTDATA_SetMsgDest Lib "FNOTIF32.DLL" Alias "AnFEVTDATA_SetExerID" (pEvtData As FEVTDATA2, ByVal pszStr As String) As Long
'Private Declare Function AnFEVTDATA_SetExerID Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2, ByVal pszStr As String) As Long
Private Declare Function AnFEVTDATA_SetMsgParm Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2, ByVal iIndex As Long, ByVal pszStr As String) As Long
Private Declare Function AnFEVTDATA_SetAuxProcessIdent Lib "FNOTIF32.DLL" (pEvtData As FEVTDATA2, ByVal pszStr As String) As Long

' Status functions
'Private Declare Function AnEvtBrdTotRecCt Lib "FNOTIF32.DLL" () As Long
'Private Declare Function AnEvtBrdCurRecCt Lib "FNOTIF32.DLL" () As Long
'Private Declare Function AnEvtBrdPeakRecCt Lib "FNOTIF32.DLL" () As Long
'Private Declare Function AnEvtBufTotRecCt Lib "FNOTIF32.DLL" () As Long
'Private Declare Function AnEvtBufCurRecCt Lib "FNOTIF32.DLL" () As Long
'Private Declare Function AnEvtBufPeakRecCt Lib "FNOTIF32.DLL" () As Long
'Private Declare Function AnIntListTotRecCt Lib "FNOTIF32.DLL" () As Long
'Private Declare Function AnIntListCurRecCt Lib "FNOTIF32.DLL" () As Long
'Private Declare Function AnIntListPeakRecCt Lib "FNOTIF32.DLL" () As Long
'Private Declare Function AnLinkTblTotRecCt Lib "FNOTIF32.DLL" () As Long
'Private Declare Function AnLinkTblCurRecCt Lib "FNOTIF32.DLL" () As Long
'Private Declare Function AnLinkTblPeakRecCt Lib "FNOTIF32.DLL" () As Long
'Private Declare Function AnEvtTagTblTotRecCt Lib "FNOTIF32.DLL" () As Long
'Private Declare Function AnEvtTagTblCurRecCt Lib "FNOTIF32.DLL" () As Long
'Private Declare Function AnEvtTagTblPeakRecCt Lib "FNOTIF32.DLL" () As Long

' DLL info functions
Private Declare Function AnDLLRefCt Lib "FNOTIF32.DLL" () As Long
Private Declare Function AnGetDLLTitle Lib "FNOTIF32.DLL" (ByVal pszDestBuf As String, ByVal iMaxLen As Long) As Long
Private Declare Function AnGetDLLVersion Lib "FNOTIF32.DLL" (ByVal pszDestBuf As String, ByVal iMaxLen As Long) As Long
Private Declare Function AnGetDLLBuildInfo Lib "FNOTIF32.DLL" (ByVal pszDestBuf As String, ByVal iMaxLen As Long) As Long

' Utility functions
Private Declare Function AnSecMsecToCTimeStr Lib "FNOTIF32.DLL" (ByVal ulSec As Long, ByVal usMsec As Long, ByVal pszDestBuf As String) As Long
Private Declare Function AnStrNCpyZ Lib "FNOTIF32.DLL" (ByVal pszDest As String, ByVal pszSrc As Any, ByVal iMaxLen As Integer) As Long

' Reserved functions
Private Declare Function AnX001 Lib "FNOTIF32.DLL" (ByVal L1 As Long, ByVal L2 As Long, ByVal L3 As Long, ByVal L4 As Long, ByVal S1 As String, ByVal S2 As String) As Long
Private Declare Function AnX002 Lib "FNOTIF32.DLL" (ByVal L1 As Long, ByVal L2 As Long, ByVal L3 As Long, ByVal L4 As Long, ByVal S1 As String, ByVal S2 As String) As String
Private Declare Function AnX003 Lib "FNOTIF32.DLL" (ByVal L1 As Long, ByVal L2 As Long, ByVal L3 As Long, ByVal L4 As Long, ByVal v1 As Any, ByVal V1Len As Long, ByVal v2 As Any, ByVal V2Len As Long) As Long
Private Declare Function RegCloseKey Lib "advapi32.dll" (ByVal hKey&) As Long
Private Declare Function RegQueryValueLong Lib "advapi32.dll" Alias "RegQueryValueExA" (ByVal hKey&, ByVal lpValueName$, _
                                                  ByVal lpReserved&, lpType&, lpData&, _
                                                        lpcbData&) As Long 'lpdata as any

Private Declare Function RegOpenKey Lib "advapi32.dll" Alias "RegOpenKeyA" (ByVal hKey&, ByVal lpSubKey$, phkResult&) As Long
                        
Private Const ERROR_SUCCESS          As Long = 0&
Private Const REG_SZ                 As Long = 1
Private Const REG_DWORD              As Long = 4
                        
' Message destination constants
'(these MUST be synchronized with NsGenFEMISMsgDest() !!!)
Private Const NS_FEMIS_MSGDEST_PATTERN = "/femis/<hazard_name>/<site_name>/Exer.<exercise_num>"
Private Const NS_MSGDEST_ROOT = "/femis"
Private Const NS_MSGDEST_EXER_PREFIX = "/Exer."

'....Data-Driven notification constants
'Message class names
Private Const NS_MSG_CLASS_ACTION = "Action"
Private Const NS_MSG_CLASS_DATA_CHANGE = "DataChange"

'DD handler
Private Const NS_DDHANDLER_ACTION = "Action"
Private Const NS_DDHANDLER_DATA_CHANGE = "DataChange"

'DD flags
Private Const NS_DD_USE_DD = &H2             'USE DD flag
Private Const NS_DD_SENT = &H8               'DATA SENT flag
Private Const NS_DD_ARRIVED = &H10           'DATA ARRIVED flag
Private Const NS_DD_NOT_ARRIVED = &H20       'DATA TIMEOUT flag
Private Const NS_DD_DAI_JOURNAL = &H40       'DAI JOURNAL flag

'DD data format
Private Const NS_DD_XmitInitDate_FMT = "DD-MMM-YYYY HH:MM:SS"

' Generic message parameter indexes
Private Const NS_MsgClass_IDX = 0

' Action message parameter indexes
Private Const NS_AC_ActionType_IDX = 20
Private Const NS_AC_ActionStatus_IDX = 21
Private Const NS_AC_ActionDate_IDX = 22
Private Const NS_AC_ItemID1_IDX = 23
Private Const NS_AC_ItemID2_IDX = 24
Private Const NS_AC_AuxData1_IDX = 25
Private Const NS_AC_AuxData2_IDX = 26
Private Const NS_AC_AuxData3_IDX = 27
Private Const NS_AC_AuxData4_IDX = 28
Private Const NS_AC_AuxData5_IDX = 29
Private Const NS_AC_ExerID_IDX = 30
Private Const NS_AC_SiteCode_IDX = 31
Private Const NS_AC_EOCCode_IDX = 32
Private Const NS_AC_HazardID_IDX = 33
Private Const NS_AC_FolderID_IDX = 34
Private Const NS_AC_AckFlag_IDX = 35
Private Const NS_AC_AckString_IDX = 36

' DataChange message parameter indexes
Private Const NS_DC_ChangeType_IDX = 20
Private Const NS_DC_DatasetName_IDX = 21
Private Const NS_DC_ChangeDate_IDX = 22
Private Const NS_DC_ItemID1_IDX = 23
Private Const NS_DC_ItemID2_IDX = 24
Private Const NS_DC_AuxData1_IDX = 25
Private Const NS_DC_AuxData2_IDX = 26
Private Const NS_DC_AuxData3_IDX = 27
Private Const NS_DC_AuxData4_IDX = 28
Private Const NS_DC_AuxData5_IDX = 29
Private Const NS_DC_ExerID_IDX = 30
Private Const NS_DC_SiteCode_IDX = 31
Private Const NS_DC_EOCCode_IDX = 32
Private Const NS_DC_HazardID_IDX = 33
Private Const NS_DC_FolderID_IDX = 34
Private Const NS_DC_AckFlag_IDX = 35
Private Const NS_DC_AckString_IDX = 36

' Data-Driven message parameter indexes
Private Const NS_DD_TblName1_IDX = 1
Private Const NS_DD_TblName2_IDX = 2
Private Const NS_DD_TblName3_IDX = 3
Private Const NS_DD_RowID1_IDX = 4
Private Const NS_DD_RowID2_IDX = 5
Private Const NS_DD_RowID3_IDX = 6
Private Const NS_DD_XmitInitDate1_IDX = 7
Private Const NS_DD_XmitInitDate2_IDX = 8
Private Const NS_DD_XmitInitDate3_IDX = 9
Private Const NS_DD_Action1_IDX = 10
Private Const NS_DD_Action2_IDX = 11
Private Const NS_DD_Action3_IDX = 12
Private Const NS_DD_EOCCode_IDX = 13
Private Const NS_DD_Listener_IDX = 14
Private Const NS_DD_DDFlags_IDX = 15
Private Const NS_DD_DDHandler_IDX = 16

Private Const stU_NOTIF_EVENT            As String = "CSEPPEvent"
Private Const stU_NOTIF_EVENT2           As String = "CSEPPEvent2"
Private Const stU_NOTIF_EVENT3           As String = "CSEPPEvent3"
Private Const stU_NOTIF_EVENT4           As String = "CSEPPEvent4"
Private Const stU_NOTIF_EVENT5           As String = "CSEPPEvent5"
Private Const stU_NOTIF_EVENT6           As String = "CSEPPEvent6"
Private Const stU_NOTIF_EVENTCLASS       As String = "CSEPPEventClass"
Private Const stU_NOTIF_PLANPROF         As String = "UPlanProfile"    '!Dataset specific
Private Const stU_NOTIF_COMMUNITY        As String = "UCommunity"
Private Const stU_NOTIF_WORKPLAN         As String = "UWorkplan"
Private Const stU_NOTIF_FACILITY         As String = "UFacility"
Private Const stU_NOTIF_FACILITY_DEL     As String = "UFacilityDel"
Private Const stU_NOTIF_DEPARTMENT       As String = "UDept"
Private Const stU_NOTIF_DEPARTMENT_DEL   As String = "UDeptDel"
Private Const stU_NOTIF_RESTYPE          As String = "UResType"
Private Const stU_NOTIF_PERSON           As String = "UPerson"
Private Const stU_NOTIF_PERSON_DEL       As String = "UPersonDel"
Private Const stU_NOTIF_AGENCY           As String = "UAgency"
Private Const stU_NOTIF_AGENCY_DEL       As String = "UAgencyDel"
Private Const stU_NOTIF_MOU              As String = "UMou"
Private Const stU_NOTIF_LOCALID          As String = "ULocalID"
Private Const stU_NOTIF_MATRIX           As String = "UMatrix"           '**gsEId always=0
Private Const stU_NOTIF_NEWONPOSTDATA    As String = "UNewOnPostData"
Private Const stU_NOTIF_PAR              As String = "UPAR"              '!Dataset specific
Private Const stU_NOTIF_PAD              As String = "UPAD"              '!Dataset specific
Private Const stU_NOTIF_PALT             As String = "UPALT"
Private Const stU_NOTIF_THREAT           As String = "UThreat"
Private Const stU_NOTIF_RISKAREA         As String = "URiskArea"
Private Const stU_NOTIF_BROKELOCK        As String = "UBrokeLock"
Private Const stU_NOTIF_DELEXER          As String = "UDelExer"
Private Const stU_NT_DELDS               As String = "UDelDataset"
Private Const stU_NOTIF_TEXT             As String = "GENTEXT"
Private Const stU_NOTIF_RES_ASS          As String = "UResAss"
Private Const stU_NOTIF_RES_ASS_DEL      As String = "UResAssDel"

'**Notification Subjects...
Private Const stU_WPD2       As String = "WP_to_D2"      '!Notification interest for Work Plan/D2
Private Const stU_D2WP       As String = "D2_to_WP"
Private Const stU_CAID2      As String = "CAI_to_D2"     '!Notification interest for CAI/D2
Private Const stU_D2CAI      As String = "D2_to_CAI"
Private Const stU_CAI_MSB    As String = "CAI_to_MSB"    '!Notification interest for model status board
Private Const stU_MSB_CAI    As String = "MSB_to_CAI"
Private Const stU_RUND2      As String = "RunD2"

'**Notification stuff from Workplan to D2 and back.  (TRD & MJB)
'**1st Param=one of following, 2nd Param=D2CaseID
Private Const stU_WPD2_VIEW      As String = "View_D2"      'View only the passed caseID (disable sending back)
Private Const stU_WPD2_EDIT      As String = "Edit_D2"      'Edit the passed caseID
Private Const stU_D2WP_REPLY     As String = "Reply_WP"     'Got message from WP
Private Const stU_D2WP_UPDATE    As String = "Update_WP"    'Update workplan with CaseID
Private Const stU_D2WP_ERROR     As String = "Error"        'D2 had an error
'**CAI<->D2 notification messages
Private Const stU_CAID2_VIEW     As String = "View_D2"      'View only the passed caseID (disable sending back)
Private Const stU_CAID2_EDIT     As String = "Edit_D2"      'Edit the passed caseID
Private Const stU_D2CAI_REPLY    As String = "Reply_CAI"    'Got message from WP
Private Const stU_D2CAI_UPDATE   As String = "Update_CAI"   'Update workplan with CaseID
'**Model Status Board<->D2 notification messages
Private Const stU_MSB_CAI_REPLY  As String = "Reply_MSBCAI" 'Got message from model status board

'==========================================================
' Old notif stuff ends here (01/12/98)
' New notif stuff starts here (01/12/98)
'==========================================================

'==========================================================
' Below are constants used for data change notifications(01/12/98, TRD)
'==========================================================

'Possible NotifName values for data change notifications(01/12/98, TRD)
Private Const NOTIF_CHG_EVENT        As String = "Event Data Change"
Private Const NOTIF_CHG_EMERGENCY    As String = "Emergency Data Change"
Private Const NOTIF_CHG_WORK_PLAN    As String = "Work Plan Data Change"
Private Const NOTIF_CHG_WP_ACT_LIB   As String = "Work Plan Activity Data Change"    'Added 20000104 PDG
Private Const NOTIF_CHG_D2PC         As String = "D2PC Data Change"
Private Const NOTIF_CHG_THREAT       As String = "Threat Data Change"
Private Const NOTIF_CHG_RISK         As String = "Risk Data Change"
Private Const NOTIF_CHG_PAR          As String = "PAR Data Change"
Private Const NOTIF_CHG_PAD          As String = "PAD Data Change"
Private Const NOTIF_CHG_PLAN         As String = "Plan Data Change"
Private Const NOTIF_CHG_TASK         As String = "Task Data Change"
Private Const NOTIF_CHG_EVAC         As String = "Evac Data Change"
Private Const NOTIF_CHG_FOLDER       As String = "Folder Data Change"
Private Const NOTIF_CHG_COMMUNITY    As String = "Community Data Change"
Private Const NOTIF_CHG_FACILITY     As String = "Facility Data Change"
Private Const NOTIF_CHG_RES_DEF      As String = "Resource Def Data Change"
Private Const NOTIF_CHG_RES_ASS      As String = "Resource Assign Data Change"
Private Const NOTIF_CHG_AGENCY       As String = "Agency Data Change"
Private Const NOTIF_CHG_DEPT         As String = "Dept Data Change"
Private Const NOTIF_CHG_MOU          As String = "MOU Data Change"
Private Const NOTIF_CHG_PERSONNEL    As String = "Personnel Data Change"
Private Const NOTIF_CHG_TK_NOK       As String = "Next Of Kin Data Change"       'Added 19980310 PDG
Private Const NOTIF_CHG_TK_PERSON    As String = "Tracked Person Data Change"    'Added 19980310 PDG
Private Const NOTIF_CHG_IGLOO        As String = "Igloo Data Change"             'Added 19991001 PDG
Private Const NOTIF_CHG_KP           As String = "Known Point Data Change"       'Added 03-Mar-1998 RJC
Private Const NOTIF_CHG_KP_CLASS     As String = "KP Class Data Change"          'Added 19981104 TRD
Private Const NOTIF_CHG_KP_SUBCLASS  As String = "KP SubClass Data Change"       'Added 19981201 TRD
Private Const NOTIF_CHG_KPOLY        As String = "Known Polygon Data Change"     'Added 25-Jan-1999 RJC
Private Const NOTIF_CHG_MET          As String = "Met Data Change"
Private Const NOTIF_CHG_MET_TOWER    As String = "Met Tower Data Change"         'Added 22-Oct-99  RJC
Private Const NOTIF_CHG_MET_CLUSTER  As String = "Met Cluster Data Change"       'Added 22_Oct-99  RJC
Private Const NOTIF_CHG_CAI          As String = "CAI Data Change"
Private Const NOTIF_CHG_CASUALTY     As String = "Casualty Data Change"
Private Const NOTIF_CHG_EVACUEE      As String = "Evacuee Data Change"
Private Const NOTIF_CHG_SHELTER      As String = "Shelter Data Change"
Private Const NOTIF_CHG_SD_DATA      As String = "Status Board Data Change"
Private Const NOTIF_CHG_SD_DESIGN    As String = "Status Board Design Change"
Private Const NOTIF_CHG_EV_LOG       As String = "Event Log Data Change"
Private Const NOTIF_CHG_JOURNAL      As String = "Journal Data Change"
Private Const NOTIF_CHG_SH_EV_LOG    As String = "Shared Event Log Data Change"
Private Const NOTIF_CHG_SH_REPORT    As String = "Shared Report Data Change"
Private Const NOTIF_CHG_SH_JOURNAL   As String = "Shared Journal Data Change"
Private Const NOTIF_CHG_LOCK         As String = "Lock Data Change"
Private Const NOTIF_CHG_EXER_SETUP   As String = "Exercise Setup Change"
Private Const NOTIF_CHG_HAZ_DEF      As String = "Hazard Definition Data Change" 'Added 20000105 for SAS per TRD request
Private Const NOTIF_CHG_FFL_LOCAL    As String = "Free Form Log Data Change"         'Updated 20000120 LKA
Private Const NOTIF_CHG_FFL_SHARED   As String = "Shared Free Form Log Data Change"  'Added 20000120 LKA
Private Const NOTIF_CHG_DAI          As String = "DAI Data Change"                   'Added 03/17/2000, TRD.
Private Const NOTIF_CHG_RAD          As String = "RAD Data Change"                   'Added 05/26/2000, PDG.

Private Const NOTIF_CHG_MAP_FAC      As String = "Facility Map Change"
Private Const NOTIF_CHG_MAP_D2PC     As String = "D2PC Map Change"
Private Const NOTIF_CHG_MAP_THREAT   As String = "Threat Map Change"
Private Const NOTIF_CHG_MAP_RISK     As String = "Risk Map Change"
Private Const NOTIF_CHG_MAP_PAR      As String = "PAR Map Change"
Private Const NOTIF_CHG_MAP_PAD      As String = "PAD Map Change"
Private Const NOTIF_CHG_MAP_MET      As String = "MET Map Change"
Private Const NOTIF_CHG_MAP_EVAC     As String = "Evac Map Change"
Private Const NOTIF_CHG_MAP_KP       As String = "Known Point Map Change"
Private Const NOTIF_CHG_MAP_KP_CLASS As String = "KP Class Map Change"
Private Const NOTIF_CHG_MAP_KP_SUBCLASS As String = "KP SubClass Map Change"
Private Const NOTIF_CHG_MAP_KPOLY    As String = "Known Polygon Map Change"
Private Const NOTIF_CHG_MAP_IGLOO    As String = "Igloo Map Change"              'Added 20000119 PDG

Private Const NOTIF_CHG_REPL_FAIL    As String = "Replication Failure"


'Possible ChangeType values for data change notifications(01/12/98, TRD)
Private Const NOTIF_CHG_TYPE_ADD                 As String = "Add"
Private Const NOTIF_CHG_TYPE_DEL                 As String = "Delete"
Private Const NOTIF_CHG_TYPE_MOD                 As String = "Modify"
Private Const NOTIF_CHG_TYPE_CLOSE               As String = "Close"
Private Const NOTIF_CHG_TYPE_SELECT              As String = "Select"
Private Const NOTIF_CHG_TYPE_SEND_AS_CURRENT     As String = "Send current"
Private Const NOTIF_CHG_TYPE_SEND_AS_NONCURRENT  As String = "Send non current"
Private Const NOTIF_CHG_TYPE_DECLARE             As String = "Declare"
Private Const NOTIF_CHG_TYPE_END                 As String = "End"
Private Const NOTIF_CHG_TYPE_END_ALL             As String = "End all"
Private Const NOTIF_CHG_TYPE_END_MANY            As String = "End many"
Private Const NOTIF_CHG_TYPE_BROKE_LOCK          As String = "Broke lock"


'==========================================================
' Below are constants used for action (interprocess communication) notifications(01/12/98, TRD)
'==========================================================

'Possible ActionName values for ACTION notifications(01/12/98, TRD)
Private Const NOTIF_ACTION_D2PC_NAV  As String = "D2PC Action Nav"
Private Const NOTIF_ACTION_D2PC_CAI  As String = "D2PC Action CAI"
Private Const NOTIF_ACTION_D2PC_WP   As String = "D2PC Action WP"
Private Const NOTIF_ACTION_WORK_PLAN As String = "Work Plan Action"
Private Const NOTIF_ACTION_EV_ANIM   As String = "Event Anim Action"
Private Const NOTIF_ACTION_CAI       As String = "CAI Action"

'Possible ActionTypes values for ACTION notifications(01/12/98, TRD)
Private Const NOTIF_ACTION_TYPE_CREATE           As String = "Create"
Private Const NOTIF_ACTION_TYPE_OPEN_EDIT        As String = "Open Edit" 'Open item in edit mode
Private Const NOTIF_ACTION_TYPE_OPEN_VIEW        As String = "Open View" 'Open item in view mode
Private Const NOTIF_ACTION_TYPE_OPEN_OTHER_EOC   As String = "Open other EOC" 'Open item from other EOC
Private Const NOTIF_ACTION_TYPE_RETURN           As String = "Return" 'This is after the user clicks a button to return to FEMIS.  (i.e. clicking the "Update Work Plan" button within D2PC interface)
Private Const NOTIF_ACTION_TYPE_REPLY            As String = "Reply" 'This is the intitial reply saying, "yes we got the notification".  (i.e. D2PC saying "Yes it got the message from workplan to open a case")
                
'Possible ActionStatus values for ACTION notifications(01/12/98, TRD)
Private Const NOTIF_ACTION_STATUS_WAITING    As String = "Waiting"
Private Const NOTIF_ACTION_STATUS_START      As String = "Start"
Private Const NOTIF_ACTION_STATUS_STARTED    As String = "Started"
Private Const NOTIF_ACTION_STATUS_DONE       As String = "Done"
Private Const NOTIF_ACTION_STATUS_FAILED     As String = "Failed"
Private Const NOTIF_ACTION_STATUS_TIME_OUT   As String = "Timed Out"
            
'==========================================================
' Below are constants used for data driven notification (01/12/98, TRD)
'==========================================================
' Constants used for stAction field of data driven notification(01/12/98, TRD)
Private Const NOTIF_DD_ADD   As String = "Add"
Private Const NOTIF_DD_MOD   As String = "Modify"
Private Const NOTIF_DD_DEL   As String = "Delete"

' Constants used for lDAIFlag and lDDFlag field of data driven notification(01/12/98, TRD)
Private Const NOTIF_DD_YES   As Integer = 1
Private Const NOTIF_DD_NO    As Integer = 0

' Constants used for ulAuxProcessFlags field of data driven notification(01/12/98, TRD)
Private Const NOTIF_DD_SENT          As Integer = 8
Private Const NOTIF_DD_ARRIVED       As Integer = 16
Private Const NOTIF_DD_NOT_ARRIVED   As Integer = 32

'Constants for Conditional Data - DO NOT REMOVE (3/16/01, MAF)
Private Const COND_TRAFFIC                      As String = "Traffic_Conditions"
Private Const COND_USER_DEFINED                 As String = "User_Defined_Cond_Msgs"
Private Const COND_WEATHER                      As String = "Weather_Conditions"
Private Const COND_DAMAGE_ASSESSMENT            As String = "Damage_Assessment"

Private Const OBJ_ATTR_START                    As Integer = 1
Private Const OBJ_ATTR_MAX                      As Integer = 5

Private Const MESSAGE_CLASS                     As String = "Message Class"
Private Const MESSAGE_VALUE                     As String = "Value"
Private Const MESSAGE_NAME                      As String = "Name"
Private Const MESSAGE_NOTES                     As String = "Notes"
Private Const MESSAGE_DATATYPE                  As String = "Data Type"
Private Const MESSAGE_CLASS_DEFINED_SIZE        As Long = 32
Private Const MESSAGE_VALUE_DEFINED_SIZE        As Long = 255
Private Const MESSAGE_NAME_DEFINED_SIZE         As Long = 16
Private Const MESSAGE_NOTES_DEFINED_SIZE        As Long = 2000
Private Const MESSAGE_DATATYPE_DEFINED_SIZE     As Long = 32
'Threat Area

Private mfIsShared          As Boolean
Private mstThreatAreaName   As String
Private mstThreatAreaType   As String   'TA_WEDGE_ENGLISH, TA_CYLINDER_ENGLISH, TA_CIRCLE_ENGLISH, TA_POLYGON_ENGLISH
Private mstThreatAreaCategory As String 'modeldefined, userdefined or template.
Private mstImpactType       As String   'local copy
Private mstDescShort        As String
Private mstDescLong         As String

Private mlAngle             As Long     'local copy
Private mdDistance          As Double   'local copy
Private mlDirection         As Long     '
Private mlIsolationRadius   As Long     'local copy
'Public mlSourceGeoObjectID As Long     'used in the TA.bas file
Private mstPolygonData      As String
'----------------------------------------------------
' made some generic variables to store lat or dist,
' long or dir based on location type.
'----------------------------------------------------
'Threat Area

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
Public Function SaveTA(ByVal stTAcategory As String, _
                    ByVal stTAType As String, _
                    ByVal stTAName As String, _
                    Optional ByVal stPolygonData As String = "", _
                    Optional ByVal lModelCaseID As Long = 0, _
                    Optional ByVal stImpactLevel As String = "", _
                    Optional ByVal lTAAngle As Long = 0, _
                    Optional ByVal dTADist As Double = 0, _
                    Optional ByVal lTADir As Long = 0, _
                    Optional ByVal dLat As Double = 0, _
                    Optional ByVal dLon As Double = 0, _
                    Optional ByVal stLocType As String = "", _
                    Optional ByVal lIsolRad As Long) As Boolean
Dim lCaseID As Long
Dim intErr As Integer
'Dim gdbFemis As New ADODB.Connection
Dim rsList As New ADODB.Recordset
Dim strSql As String
Dim sql As String
Dim lthreatId As Long


On Error GoTo InsertNewThreat
SaveTA = False

'Set gdbFemis = New ADODB.Connection
'gdbFemis.Open DB_Connect



strSql = "SELECT THREAT_ID.NEXTVAL FROM DUAL"
rsList.Open strSql, gdbFemis, adOpenStatic, adLockReadOnly

    If (rsList.EOF) Then
            rsList.Close
    
    Else
    'Tim: This was having some problems with indices.
    'mlNavIDs (NAV_THREAT_INDEX)
            lthreatId = CLng(rsList("NEXTVAL"))
            rsList.Close
    End If
    
    
mstThreatAreaType = stTAType
mstThreatAreaName = stTAName '& lthreatId ' mlNavIDs(NAV_THREAT_INDEX)


If Not ValidateEntries(mstThreatAreaName, dTADist, lIsolRad, lTAAngle, lTADir, 0, dLat, dLon, stTAType) Then
    Exit Function
End If


If mstThreatAreaType = TA_POLYGON_ENGLISH Then
    If stPolygonData <> vbNullString Then
    
        If Not LocationParameters(TA_POLYGON_ENGLISH, stPolygonData) Then
            SaveTA = False
            Exit Function
        End If
        
    Else
        Exit Function
    End If

End If


'------------------------------------------------------------------------
' Now insert the THREAT_area record
'------------------------------------------------------------------------
  sql = "INSERT INTO THREAT_AREA "
  sql = sql & " (threat_id, threat_date, xmit_init_date, "
  sql = sql & " threat_cat_type, threat_area_type, threat_area_name,"
  sql = sql & " model_case_id,"
  sql = sql & " ta_isol_rad,"
  sql = sql & " ta_distance, ta_wind_direction, ta_angle, "
  sql = sql & " impact_level, geo_object_id, "
  sql = sql & " geo_lon_or_dist, geo_lat_or_dir, geo_loc_type, "
  sql = sql & " exercise_num, eoc_name, hazard_id, folder_id"
  sql = sql & ") VALUES (" & lthreatId
  sql = sql & ", " & stTO_DATE((vZuluNow))
  sql = sql & ", " & IIf(mfIsShared, stTO_DATE((vZuluNow)), "NULL")
  sql = sql & ", '" & (stTAcategory)
  sql = sql & "', '" & (stTAType)
  sql = sql & "', '" & (mstThreatAreaName)
  sql = sql & "', " & lModelCaseID
  sql = sql & ", " & lIsolRad
  sql = sql & "," & dTADist
  sql = sql & "," & lTADir
  sql = sql & "," & lTAAngle
  sql = sql & ",'" & (mstImpactType)
  sql = sql & "'," & mlSourceGeoObjectID
  sql = sql & "," & dLat
  sql = sql & "," & dLon
  sql = sql & ",'" & (stLocType)
  sql = sql & "', " & gsEID & ",'" & (gstUCurEOCName) & "'," & glCurrHazID & "," & glFolderID & ")"
  gdbFemis.Execute sql
  
  If Not UpdateHCItemInMemory(HC_TYPE_TA, lthreatId) Then Exit Function
  If Not MakeOperational() Then Exit Function
  

SaveTA = True
Exit Function

InsertNewThreat:
  SaveTA = False
  
  Exit Function
End Function
Public Function LoadZones() As Boolean

    
    Dim stQ             As String
    Dim colList As New Collection
    Dim iIndex As Integer
    Dim intErr As Integer
    'Dim gdbFemis As New ADODB.Connection
    Dim rsList As New ADODB.Recordset
   

    On Error GoTo LoadfromDberr

    LoadZones = False
   
        
        'gdbFemis.Open DB_Connect
        'The polygonal layer id is hardcoded here - need to look into it.
        
        stQ = "Select * from eoc_zone where  polygonal_layer_id=10000000  and eoc_name= '" & gstUCurEOCName & "'"
        
        
        
        stQ = "Select * from PA_UNIT where exercise_num=" & gsEID
        rsList.Open stQ, gdbFemis, adOpenStatic, adLockReadOnly
        
        iIndex = -1
        Do While Not (rsList.EOF)
            iIndex = iIndex + 1

                colList.Add (CStr(rsList.Fields("ZONE_NAME")))
                
                'stListItem = stListItem & CStr(rsTemp.Fields("PA_UNIT_ID"))
                              
            rsList.MoveNext


        Loop
    LoadZones = True
    
    Exit Function
LoadfromDberr:
    Exit Function
    

End Function


Public Function SavePAR(ByVal stPAName As String, _
                        ByVal stPAType As String, _
                        ByVal stImpactLevel As String, _
                        ByVal fModelBased As Boolean, _
                        ByVal fShared As Boolean) As Long

    
    Dim stQ             As String
    Dim colList As New Collection
    Dim iIndex As Integer
    Dim intErr As Integer
    'Dim gdbFemis As New ADODB.Connection
    Dim rsList As New ADODB.Recordset
    Dim strSql As String
    Dim lObjectID As Long
    Dim CommunityId As Long
    Dim PlanID As Long
    Dim stTimeNow As String
    
    

    

    On Error GoTo SaveDberr
    SavePAR = False
    stTimeNow = stTO_DATE(CStr(vZuluNow))
    CommunityId = 0
    PlanID = 0
         
        strSql = "SELECT PAD_ID.NEXTVAL FROM DUAL"
        rsList.Open strSql, gdbFemis, adOpenStatic, adLockReadOnly

        If (rsList.EOF) Then
            rsList.Close
        
        Else
            lObjectID = CLng(rsList("NEXTVAL"))
         rsList.Close
        End If

       
        
        stQ = "INSERT INTO PARPAD_HEADER (parpad_id, exercise_num, parpad_name, "
        stQ = stQ & "parpad_type, hazard_id, ss_id, plan_ref_id, model_based, "
        stQ = stQ & "hazard_case_id, impact_level, folder_id, xmit_init_date) "
        stQ = stQ & "VALUES (" & lObjectID & ", " & gsEID & ", '" & stPAName & "', '"
        stQ = stQ & (stPAType) & "', " & glCurrHazID & ", "
        stQ = stQ & NullForZero(CommunityId) & ", " & NullForZero(PlanID) & ", "
        stQ = stQ & "'" & IIf(fModelBased, "Y", "N") & "', "
        stQ = stQ & NullForZero(glHazardCaseID) & ", '" & (stImpactLevel) & "', " & glFolderID & ", "
        stQ = stQ & IIf(fShared, stTimeNow, "NULL") & ") "
    
        gdbFemis.Execute stQ
        If (stPAType = HC_TYPE_PAR) Then
                If Not UpdateHCItemInMemory(HC_TYPE_PAR, lObjectID) Then Exit Function
        Else
                If Not UpdateHCItemInMemory(HC_TYPE_PAD, lObjectID) Then Exit Function
        End If
        
        If Not MakeOperational() Then Exit Function
               
               
        SavePAR = lObjectID
        
    Exit Function
SaveDberr:
    SavePAR = False
    Exit Function
    

End Function

'**-----------------------------------------------------------------**
'** Return the word NULL if the provided numeric is <= zero.        **
'** Otherwise, return the String conversion of the numeric.         **
'**-----------------------------------------------------------------**
'
Public Function NullForZero(ByVal lInput As Long) As String
    
    NullForZero = IIf(lInput <= 0, "NULL", CStr(lInput))
    
End Function



Public Function DeletePoint(stPointName As String, stClassName As String) As Integer
Dim stSQL               As String
Dim objCon              As New ADODB.Connection
Dim intSuccess          As Integer
Dim lngLinkFlags        As Long
Dim lngULink            As Long
Dim dXmitDate1          As Date
Dim strRepID1           As String
Dim dXmitDate2          As Date
Dim strRepID2           As String
Dim nsDDParm            As NS_DD_PARAMS
Dim strFormTitle        As String
Dim rsTemp              As New ADODB.Recordset
Dim rsList              As New ADODB.Recordset
Dim stMsgType           As String
Dim lngGeoObject        As Long
Dim lngClassID          As Long
Dim lRecordID           As Long
Dim lngExercise         As Long
Dim stTimeNow           As String
Dim strSql              As String


    
On Error GoTo ErrorHandler
    DeletePoint = -1
    '----------------------------------------------------------------------------------
    ' open a connection to the Oracle Database
    '----------------------------------------------------------------------------------
    objCon.Open DB_Connect
    
    lngExercise = gsEID
    
    stMsgType = stClassName
    stTimeNow = stTO_DATE(CStr(vZuluNow))

'--------------------------------------------------------------------
'   Get the class
'--------------------------------------------------------------------
    strSql = "SELECT class_id " & _
             "  FROM OBJECT_CLASS " & _
             " WHERE class_name='" & stClassName & "' " & _
             "   AND exercise_num = " & lngExercise
    
    rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
    
    If rsList.EOF Then
        GoTo ErrorHandler
    Else
        lngClassID = rsList("class_id")
    End If
    rsList.Close
    strSql = "SELECT " & _
                "OBJECT_ID, " & _
                "GEO_OBJECT_ID " & _
             "FROM " & _
                "USER_DEFINED_OBJECT " & _
             "WHERE " & _
                "OBJECT_NAME = '" & stPointName & "' " & _
             "AND " & _
                "EXERCISE_NUM = " & CStr(lngExercise) & " " & _
             "AND " & _
                "EOC_NAME = '" & gstUCurEOCName & "' " & _
             "AND " & _
                "CLASS_ID = " & lngClassID & " "
               
    ' return the recordset to see if the message already exists
    rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
    ' see if any records were returned
    If Not rsTemp.EOF Then
        ' store the object and geo-object id
        lRecordID = rsTemp.Fields("OBJECT_ID").Value
        lngGeoObject = rsTemp.Fields("GEO_OBJECT_ID").Value
        ' close the recordset
        rsTemp.Close
    End If
'----------------------------------------------------------------------------
'  Prepare notification before deletion.
'----------------------------------------------------------------------------

    InitNotifGlobals
    giNotifWinMsgNo = 1
    lngLinkFlags = 0
    ' open a server link
    intSuccess = iNSOpenServerLink(gstNotifServerName, gstNotifServerPort, lngLinkFlags)
    
    lngULink = ulNSOpenQLinkX(0, 0, 0, 0)
    strSql = "SELECT " & _
                    "XMIT_INIT_DATE, " & _
                    "REPLICATION_ID " & _
                 "FROM " & _
                    "GEO_OBJECT " & _
                 "WHERE " & _
                    "GEO_OBJECT_ID = " & CStr(lngGeoObject)
        rsList.Open strSql, objCon
        ' store the REPLICATION_ID and XMIT_INIT_DATE in temporary storage
        dXmitDate1 = rsList.Fields("XMIT_INIT_DATE").Value
        strRepID1 = rsList.Fields("REPLICATION_ID").Value
        ' close the recordset
        rsList.Close
        
        
    ' extract the REPLICATION_ID and XMIT_INIT_DATE for USER_DEFINED_OBJECT Table
        strSql = "SELECT " & _
                    "XMIT_INIT_DATE, " & _
                    "REPLICATION_ID " & _
                 "FROM " & _
                    "USER_DEFINED_OBJECT " & _
                 "WHERE " & _
                    "OBJECT_ID = " & CStr(lRecordID)
        rsList.Open strSql, objCon
        ' store the REPLICATION_ID and XMIT_INIT_DATE in temporary storage
        If Not IsNull(rsList.Fields("XMIT_INIT_DATE").Value) Then
            dXmitDate2 = rsList.Fields("XMIT_INIT_DATE").Value
        Else
            dXmitDate2 = Empty
        End If
        strRepID2 = rsList.Fields("REPLICATION_ID").Value
        ' close the recordset
        rsList.Close
         'Send notifications after deleteing.
         
         'Cascade delete takes care of the deleting a point.
        
        stSQL = "DELETE FROM GEO_OBJECT WHERE "
        stSQL = stSQL & " GEO_OBJECT_ID=" & lngGeoObject
        stSQL = stSQL & " AND " & "exercise_num = " & lngExercise
        objCon.Execute stSQL
        
        
        NsSetDDParams nsDDParm, "USER_DEFINED_OBJECT", strRepID2, dXmitDate2, NOTIF_DD_DEL, _
                        gstUListenerNum, NOTIF_DD_SENT
        NsSetDDParamsT2 nsDDParm, "GEO_OBJECT", strRepID1, dXmitDate1, NOTIF_DD_DEL
        
        NsPostDataChangeMsgDD lngULink, NS_EF_GLOBAL, nsDDParm, NOTIF_CHG_KP, NOTIF_CHG_TYPE_DEL, _
                        gstUCurDataset, vZuluNow, CStr(lRecordID)
       
        NsPostDataChangeMsgDD lngULink, NS_EF_GLOBAL, nsDDParm, NOTIF_CHG_MAP_KP, NOTIF_CHG_TYPE_DEL, _
                    gstUCurDataset, vZuluNow, CStr(lRecordID), , stMsgType
        
        intSuccess = iNSCloseLink(lngULink)
        
         DeletePoint = 0
    Exit Function
         
ErrorHandler:
    DeletePoint = -1
End Function

Public Function PDA_Login(EMA_Username As String, EMA_Password As String, _
                          GPS_Latitude As Double, GPS_Longitude As Double, _
                          Static_Sub As Boolean) As Integer
' Author:   Kevin Dorow
' Created:  2/14/2001
' Purpose:  This function will login a PDA user into the
'           EMAdvantage database.
' Inputs:   EMA_Username (EMAdvantage Username)
'           EMA_Password (EMAdvantage Password)
'           GPS_Latitude (Current PDA Latitude Location)
'           GPS_Logitude (Current PDA Longitude Location)
'           Static_Sub (Flag to indicate whether to use static
'                       or location based subscription)
' Outputs:  The function will return an integer that will indicate the
'           success or failure of the login:
'               -1  indicates that the function failed (technical reason)
'               0   indicates that the login was successful
'               -99 indicates that the login was not successful (bad username/password)

Dim objCon As New ADODB.Connection
Dim rsUserInfo As New ADODB.Recordset
Dim rsTemp As New ADODB.Recordset
Dim strSql As String
Dim strPWD As String
Dim lngPersonRefNum As Long
Dim lngObjectClass As Long
Dim lngObjectSubClass As Long
Dim lngGISLayerID As Long
Dim lngGeoObjectID As Long
Dim lngObjectID As Long
Dim lngAttrID As Long
Dim blnInTrans As Boolean
Dim intSuccess As Integer
Dim lngLinkFlags As Long
Dim nsDDParm As NS_DD_PARAMS
Dim dXmitDate1 As Date
Dim strRepID1 As String
Dim dXmitDate2 As Date
Dim strRepID2 As String
Dim lngULink As Long
Dim dblLatitude As Double
Dim dblLongitude As Double

On Error GoTo Err_PDA_Login

    ' initialize blnInTrans
    blnInTrans = False
    
    ' open a connection to the Oracle Database
    objCon.Open DB_Connect
    
    ' first need to validate the username password--define a SQL string to
    '   retreive the user information from the database
    strSql = "SELECT * " & _
             "FROM   FEMIS_USER " & _
             "WHERE  LOWER(user_code) = '" & LCase$(EMA_Username) & "' " & _
             "AND   account_status   = 'enabled" & "'"

    ' retrieve the recordset
    rsUserInfo.Open strSql, objCon, adOpenStatic, adLockReadOnly

    ' check to see that the user account was found
    If (rsUserInfo.EOF) Then
        ' the user account was not found
        PDA_Login = -99
        Exit Function
    Else
        ' a user account was found--decrypt the password
        strPWD = rsUserInfo.Fields("user_pwd").Value
        strPWD = fnstDecryptUserPwd(strPWD)
        ' store the personal_ref_num
        lngPersonRefNum = rsUserInfo.Fields("PERSON_REF_NUM").Value
    End If

    rsUserInfo.Close
    Set rsUserInfo = Nothing

    ' validate the password
    If (LCase$(strPWD) <> LCase$(EMA_Password)) Then
        ' the passwords do not match
        PDA_Login = -99
        Exit Function
    End If
        
    ' start a transaction
    objCon.BeginTrans
    
    ' set blnInTrans to True
    blnInTrans = True
    
    ' the username / password has been validated--check to see if a valid Lat/Long
    '   were passed in
    If GPS_Latitude = -999.999 Then
        dblLatitude = 0#
        dblLongitude = 0#
    Else
        dblLatitude = GPS_Latitude
        dblLongitude = GPS_Longitude
    End If
    
    ' here we need to add a record to the PDA_User layer
    ' define a SQL string to retrieve the Object Class_ID for the PDA_User layer
    strSql = "SELECT " & _
                "OBJECT_CLASS.CLASS_ID, " & _
                "OBJECT_CLASS.GIS_LAYER_ID, " & _
                "OBJECT_SUBCLASS.SUBCLASS_ID " & _
             "FROM " & _
                "OBJECT_CLASS, " & _
                "OBJECT_SUBCLASS " & _
             "WHERE " & _
                "OBJECT_CLASS.CLASS_NAME = 'PDA_Users' " & _
            "AND OBJECT_CLASS.CLASS_ID = OBJECT_SUBCLASS.CLASS_ID " & _
             "AND OBJECT_CLASS.EXERCISE_NUM = " & CStr(gsEID)
    ' return the information in a recordset
    rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
    ' store the information from the recordset
    If Not rsTemp.EOF Then
        lngObjectClass = rsTemp.Fields("CLASS_ID").Value
        lngObjectSubClass = rsTemp.Fields("SUBCLASS_ID").Value
        lngGISLayerID = rsTemp.Fields("GIS_LAYER_ID").Value
    Else
        ' the PDA_Users layer is missing from the database--raise an error
        Error 1
    End If
    ' close the recordset
    rsTemp.Close
    
    ' we now have enough information to insert a record into the geo_object table
    ' first retrieve a new GEO_OBJECT_ID
    strSql = "SELECT " & _
                "GEO_OBJECT_ID.NEXTVAL " & _
             "FROM " & _
                "DUAL"
    ' return the new id and store it
    rsTemp.Open strSql, objCon
    If Not rsTemp.EOF Then
        lngGeoObjectID = rsTemp.Fields("NEXTVAL").Value
    Else
        ' the new id could not be retrieved-raise and error
        Error 1
    End If
    rsTemp.Close
    ' define a SQL statement to do the insert
    strSql = "INSERT INTO " & _
                "GEO_OBJECT (" & _
                    "EXERCISE_NUM, " & _
                    "GEO_OBJECT_NAME, " & _
                    "CLASS_ID, " & _
                    "SUBCLASS_ID, " & _
                    "GIS_LAYER_ID, " & _
                    "EOC_NAME, " & _
                    "FEMIS_POINT_X, " & _
                    "FEMIS_POINT_Y, " & _
                    "EXERCISE_FLAG, " & _
                    "XMIT_INIT_DATE, " & _
                    "GEO_OBJECT_ID) " & _
             "VALUES (" & _
                    CStr(gsEID) & ", " & _
                    "'" & EMA_Username & "', " & _
                    CStr(lngObjectClass) & ", " & _
                    CStr(lngObjectSubClass) & ", " & _
                    CStr(lngGISLayerID) & ", " & _
                    "'" & gstUCurEOCName & "', " & _
                    CStr(dblLongitude) & ", " & _
                    CStr(dblLatitude) & ", " & _
                    "'Y', " & _
                    stTO_DATE(CStr(Now)) & ", " & _
                    CStr(lngGeoObjectID) & ")"
    ' execute the insert
    objCon.Execute strSql
    
    ' next we need to insert a new record into the USER_DEFINED_OBJECT table
    ' first get a new object id
    strSql = "SELECT " & _
                "OBJECT_ID.NEXTVAL " & _
             "FROM " & _
                "DUAL"
    rsTemp.Open strSql, objCon
    ' store the new object ID
    lngObjectID = rsTemp.Fields("NEXTVAL").Value
    rsTemp.Close
    ' create an insert statement to load the record into the USER_DEFINED_OBJECT table
    strSql = "INSERT INTO " & _
                "USER_DEFINED_OBJECT (" & _
                    "EXERCISE_NUM, " & _
                    "OBJECT_NAME, " & _
                    "CLASS_ID, " & _
                    "SUBCLASS_ID, " & _
                    "GEO_OBJECT_ID, " & _
                    "EOC_NAME, " & _
                    "LAST_CHANGE_DATE, " & _
                    "LAST_CHANGE_PERSON, " & _
                    "OBJECT_ID) " & _
            "VALUES (" & _
                    CStr(gsEID) & ", " & _
                    "'" & EMA_Username & "', " & _
                    CStr(lngObjectClass) & ", " & _
                    CStr(lngObjectSubClass) & ", " & _
                    CStr(lngGeoObjectID) & ", " & _
                    "'" & gstUCurEOCName & "', " & _
                    stTO_DATE(CStr(Now)) & ", " & _
                    "'Earth Alert API', " & _
                    CStr(lngObjectID) & ")"
    ' execute the insert
    objCon.Execute strSql
    ' last, but not least, we need to insert a record into the OBJECT_ATTR table
    ' first we need to retreive the ATTR_ID's for the object class
    strSql = "SELECT " & _
                "ATTR_ID " & _
             "FROM " & _
                "OBJECT_ATTR_DEF " & _
             "WHERE " & _
                "CLASS_ID = " & lngObjectClass & " " & _
             "AND " & _
                "ATTR_NAME = 'Active' " & _
             "AND " & _
                "EXERCISE_NUM = " & CStr(gsEID)
    ' return the recordset
    rsTemp.Open strSql, objCon, adOpenDynamic, adLockBatchOptimistic
    ' store the ATTR_ID
    If Not rsTemp.EOF Then
        lngAttrID = rsTemp.Fields("ATTR_ID").Value
    Else
        Error 1
    End If
    ' close the recordset
    rsTemp.Close
    ' create an insert statement to update the OBJECT_ATTR table
    strSql = "INSERT INTO " & _
                "OBJECT_ATTR (" & _
                    "ATTR_ID, " & _
                    "OBJECT_ID, " & _
                    "EXERCISE_NUM, " & _
                    "EOC_NAME, " & _
                    "XMIT_INIT_DATE, " & _
                    "ATTR_VALUE) " & _
             "VALUES (" & _
                    CStr(lngAttrID) & ", " & _
                    CStr(lngObjectID) & ", " & _
                    CStr(gsEID) & ", " & _
                    "'" & gstUCurEOCName & "', " & _
                    stTO_DATE(CStr(Now)) & ", " & _
                    "'TRUE')"
    ' execute the SQL statement
    objCon.Execute strSql
    
    'Update EA_PDA_USER table
    strSql = "UPDATE " & _
                "EA_PDA_USER " & _
             "SET " & _
                "logged_in_sw = 'Y' "
    ' determine what the other fields need to be set to based on the information
    '   passed in
    ' check to see if the subscription is location based
    If Static_Sub = True Then
        strSql = strSql & ", location_based_sw = 'N' "
    Else
        strSql = strSql & ", location_based_sw = 'Y' "
    End If
    strSql = strSql & ", geo_object_id = " & lngGeoObjectID & " "
    ' check to see if valid GPS coordinates were passed in
    If GPS_Latitude <> -999.999 Then
        strSql = strSql & ", gps_active_sw = 'Y' "
    Else
        strSql = strSql & ", gps_active_sw = 'N' "
    End If
    strSql = strSql & ", pda_active_sw = 'Y' "
    strSql = strSql & "WHERE " & _
                "person_ref_num = '" & CStr(lngPersonRefNum) & "'"
    ' execute the update using the connection object
    objCon.Execute strSql
    
    objCon.CommitTrans
    
    ' set blnInTrans
    blnInTrans = False
    
    ' next we need to send a notification that the PDA_User layer has been updated
    ' first init notif information
    InitNotifGlobals
    giNotifWinMsgNo = 1
    lngLinkFlags = 0
    ' open a server link
    intSuccess = iNSOpenServerLink(gstNotifServerName, gstNotifServerPort, lngLinkFlags)
    
    lngULink = ulNSOpenQLinkX(0, 0, 0, 0)
    ' create a DB call to retrieve the REPLICATION_ID
    strSql = "SELECT " & _
                "XMIT_INIT_DATE, " & _
                "REPLICATION_ID " & _
             "FROM " & _
                "GEO_OBJECT " & _
             "WHERE " & _
                "GEO_OBJECT_ID = " & CStr(lngGeoObjectID)
    rsTemp.Open strSql, objCon
    ' store the REPLICATION_ID and XMIT_INIT_DATE in temporary storage
    dXmitDate1 = rsTemp.Fields("XMIT_INIT_DATE").Value
    strRepID1 = rsTemp.Fields("REPLICATION_ID").Value
    ' close the recordset
    rsTemp.Close
    If GPS_Latitude <> -999.999 Then
        ' set up the parameters to make a notification call for the map data change
        NsSetDDParams nsDDParm, "GEO_OBJECT", strRepID1, dXmitDate1, NOTIF_DD_ADD, gstUListener, _
                      NOTIF_DD_SENT
        ' post a notification about the map data change
        NsPostDataChangeMsgDD lngULink, NS_EF_GLOBAL, nsDDParm, NOTIF_CHG_MAP_KP, NOTIF_CHG_TYPE_ADD, _
                       gstUCurDataset, Now, CStr(lngObjectID), , "PDA_Users"
    End If
    ' extract the REPLICATION_ID and XMIT_INIT_DATE for USER_DEFINED_OBJECT Table
    strSql = "SELECT " & _
                "XMIT_INIT_DATE, " & _
                "REPLICATION_ID " & _
             "FROM " & _
                "USER_DEFINED_OBJECT " & _
             "WHERE " & _
                "OBJECT_ID = " & CStr(lngObjectID)
    rsTemp.Open strSql, objCon
    ' store the REPLICATION_ID and XMIT_INIT_DATE in temporary storage
    If Not IsNull(rsTemp.Fields("XMIT_INIT_DATE").Value) Then
        dXmitDate2 = rsTemp.Fields("XMIT_INIT_DATE").Value
    Else
        dXmitDate2 = Empty
    End If
    strRepID2 = rsTemp.Fields("REPLICATION_ID").Value
    ' close the recordset
    rsTemp.Close
    ' setup the parameters to make a notification call for the db data change
    NsSetDDParams nsDDParm, "USER_DEFINED_OBJECT", strRepID2, dXmitDate2, NOTIF_DD_ADD, _
                    gstUListener, NOTIF_DD_SENT
    ' set up the parameters for the secondary table "GEO_OBJECT"
    NsSetDDParamsT2 nsDDParm, "GEO_OBJECT", strRepID1, dXmitDate1, NOTIF_DD_ADD
    ' make the notification call
    NsPostDataChangeMsg lngULink, NS_EF_NORMAL, NOTIF_CHG_KP, NOTIF_CHG_TYPE_ADD, _
                    gstUCurDataset, Now, CStr(lngObjectID), "local", "PDA_Users"
    
    ' close the notification link to the server
    intSuccess = iNSCloseLink(lngULink)
    
    ' close and deallocate any objects that have been created
    objCon.Close
    Set rsTemp = Nothing
    Set rsUserInfo = Nothing
    Set objCon = Nothing
    
    ' indicate success
    PDA_Login = 0
        
Exit Function
Resume
Err_PDA_Login:
    ' an error occurred
    PDA_Login = -1
    ' check to see if we are in a transaction
    If blnInTrans Then
        'issue a rollback
        objCon.RollbackTrans
    End If
End Function

Public Function PDA_Update(EMA_Username As String, GPS_Latitude As Double, _
                           GPS_Longitude As Double) As Integer
' Author:   Kevin Dorow
' Created:  2/14/20001
' Purpose:  This function will update a logged in PDA user's location
'           and return the current logged in user list
' Inputs:   EMA_Username (EMAdvantage Username)
'           GPS_Latitude (Current PDA Latitude Location)
'           GPS_Logitude (Current PDA Longitude Location)
' Outputs:  The function will return an integer value that will indicate
'           success or failure of the function call:
'               -1  indicates that the function failed
'               0   indicates that the function was successful
'

Dim objCon As New ADODB.Connection
Dim rsTemp As New ADODB.Recordset
Dim strSql As String
Dim lngPersonRefNum As Long
Dim lngGeoObjectID As Long
Dim lngObjectID As Long
Dim blnInTrans As Boolean
Dim intSuccess As Integer
Dim lngLinkFlags As Long
Dim nsDDParm As NS_DD_PARAMS
Dim dXmitDate1 As Date
Dim strRepID1 As String
Dim dXmitDate2 As Date
Dim strRepID2 As String
Dim lngULink As Long
Dim dblLatitude As Double
Dim dblLongitude As Double

On Error GoTo PDA_Update_Err

    ' initialize local flags
    blnInTrans = False
    
    ' open a connection to the Oracle Database
    objCon.Open DB_Connect
    
    ' create a SQL string to retrieve the person_ref_num for this user
    strSql = "SELECT " & _
                "PERSON_REF_NUM " & _
             "FROM " & _
                "FEMIS_USER " & _
             "WHERE " & _
                "USER_CODE = '" & EMA_Username & "'"
    ' return the recordset
    rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
    If Not rsTemp.EOF Then
        ' store the person_ref_num
        lngPersonRefNum = rsTemp.Fields("PERSON_REF_NUM").Value
    Else
        ' raise an error
        Error 1
    End If
    ' close the recordset
    rsTemp.Close
    
    ' create a SQL string to determine if the user is logged in and
    '   has a geo_object_id associated to him / her
    strSql = "SELECT " & _
                "GEO_OBJECT_ID " & _
             "FROM " & _
                "EA_PDA_USER " & _
             "WHERE " & _
                "PERSON_REF_NUM = " & CStr(lngPersonRefNum) & " " & _
             "AND " & _
                "LOGGED_IN_SW = 'Y'"
    ' return the recordset
    rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
    ' check to see if any records were returned
    If rsTemp.EOF Then
        ' raise an error
        Error 1
    End If
    
    ' store the geo_object_id in local storage
    lngGeoObjectID = rsTemp.Fields("GEO_OBJECT_ID").Value
    ' close the recordset
    rsTemp.Close
    
    ' next we need to update the geo_object table
    ' first check to see if we were passed valid GIS coordinates
    If GPS_Latitude = -999.999 Then
        ' we were given bad GPS coordinates--set to 0,0
        dblLatitude = 0
        dblLongitude = 0
    Else
        dblLatitude = GPS_Latitude
        dblLongitude = GPS_Longitude
    End If
    
    ' build a SQL statement to extract the current GIS coordinates from
    '   the geo_object table
    strSql = "SELECT " & _
                "FEMIS_POINT_X, " & _
                "FEMIS_POINT_Y " & _
             "FROM " & _
                "GEO_OBJECT " & _
             "WHERE " & _
                "GEO_OBJECT_ID = " & CStr(lngGeoObjectID) & " " & _
             "AND " & _
                "EXERCISE_NUM = " & CStr(gsEID)
    ' return the recordset
    rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
    ' check to see if any records were returned
    If rsTemp.EOF Then
        ' raise an error
        Error 1
    End If
    ' see if the position of the pda user has changed
    If CDbl(rsTemp.Fields("FEMIS_POINT_X").Value) = dblLongitude And _
        CDbl(rsTemp.Fields("FEMIS_POINT_Y").Value) = dblLatitude Then
        ' here the point has not changed--nothing to do
        ' indicate the routine was successful and exit
        PDA_Update = 0
        Exit Function
    End If
    
    ' close the recordset
    rsTemp.Close
    
    ' start a transaction
    objCon.BeginTrans
    blnInTrans = True
    
    ' if we got to this point the location of the user has changed--update
    strSql = "UPDATE " & _
                "GEO_OBJECT " & _
             "SET " & _
                "FEMIS_POINT_X = " & CStr(dblLongitude) & ", " & _
                "FEMIS_POINT_Y = " & CStr(dblLatitude) & ", " & _
                "XMIT_INIT_DATE = " & stTO_DATE(CStr(Now)) & " " & _
             "WHERE " & _
                "GEO_OBJECT_ID = " & CStr(lngGeoObjectID) & " " & _
             "AND " & _
                "EXERCISE_NUM = " & CStr(gsEID)
    ' use the connection object to execute the update
    objCon.Execute strSql
    
    ' next we need to update the EA_PDA_USER table
    strSql = "UPDATE " & _
                "EA_PDA_USER " & _
             "SET "
    ' check to see if we were passed a valid GPS reading
    If GPS_Latitude <> -999.999 Then
        strSql = strSql & "GPS_ACTIVE_SW = 'Y' "
    Else
        strSql = strSql & "GPS_ACTIVE_SW = 'N' "
    End If
    ' add the WHERE clause
    strSql = strSql & "WHERE PERSON_REF_NUM = " & lngPersonRefNum & " " & _
                        "AND EXERCISE_NUM = " & CStr(gsEID)
    ' use the command object to execute the update
    objCon.Execute strSql
    
    ' commit the transactions
    objCon.CommitTrans
    blnInTrans = False
    
    ' before we send out notifications we need to extract the object id
    '   from the USER_DEFINED_OBJECT table
    strSql = "SELECT " & _
                "OBJECT_ID " & _
             "FROM " & _
                "USER_DEFINED_OBJECT " & _
             "WHERE " & _
                "GEO_OBJECT_ID = " & CStr(lngGeoObjectID) & " " & _
             "AND " & _
                "EXERCISE_NUM = " & CStr(gsEID)
    ' return the recordset
    rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
    ' make sure that a record was returned
    If rsTemp.EOF Then
        ' raise an error
        Error 1
    End If
    ' store the object id in local storage
    lngObjectID = rsTemp.Fields("OBJECT_ID").Value
    ' close the recordset
    rsTemp.Close
             
    ' next we need to send a notification that the PDA_User layer has been updated
    ' first init notif information
    InitNotifGlobals
    giNotifWinMsgNo = 1
    lngLinkFlags = 0
    ' open a server link
    intSuccess = iNSOpenServerLink(gstNotifServerName, gstNotifServerPort, lngLinkFlags)
    
    lngULink = ulNSOpenQLinkX(0, 0, 0, 0)
    ' create a DB call to retrieve the REPLICATION_ID
    strSql = "SELECT " & _
                "XMIT_INIT_DATE, " & _
                "REPLICATION_ID " & _
             "FROM " & _
                "GEO_OBJECT " & _
             "WHERE " & _
                "GEO_OBJECT_ID = " & CStr(lngGeoObjectID)
    rsTemp.Open strSql, objCon
    ' store the REPLICATION_ID and XMIT_INIT_DATE in temporary storage
    dXmitDate1 = rsTemp.Fields("XMIT_INIT_DATE").Value
    strRepID1 = rsTemp.Fields("REPLICATION_ID").Value
    ' close the recordset
    rsTemp.Close
    If GPS_Latitude <> -999.999 Then
        ' set up the parameters to make a notification call for the map data change
        NsSetDDParams nsDDParm, "GEO_OBJECT", strRepID1, dXmitDate1, NOTIF_DD_ADD, gstUListener, _
                      NOTIF_DD_SENT
        ' post a notification about the map data change
        NsPostDataChangeMsgDD lngULink, NS_EF_GLOBAL, nsDDParm, NOTIF_CHG_MAP_KP, NOTIF_CHG_TYPE_ADD, _
                       gstUCurDataset, Now, CStr(lngObjectID), , "PDA_Users"
    End If
    ' extract the REPLICATION_ID and XMIT_INIT_DATE for USER_DEFINED_OBJECT Table
    strSql = "SELECT " & _
                "XMIT_INIT_DATE, " & _
                "REPLICATION_ID " & _
             "FROM " & _
                "USER_DEFINED_OBJECT " & _
             "WHERE " & _
                "OBJECT_ID = " & CStr(lngObjectID)
    rsTemp.Open strSql, objCon
    ' store the REPLICATION_ID and XMIT_INIT_DATE in temporary storage
    If Not IsNull(rsTemp.Fields("XMIT_INIT_DATE").Value) Then
        dXmitDate2 = rsTemp.Fields("XMIT_INIT_DATE").Value
    Else
        dXmitDate2 = Empty
    End If
    strRepID2 = rsTemp.Fields("REPLICATION_ID").Value
    ' close the recordset
    rsTemp.Close
    ' setup the parameters to make a notification call for the db data change
    NsSetDDParams nsDDParm, "USER_DEFINED_OBJECT", strRepID2, dXmitDate2, NOTIF_DD_ADD, _
                    gstUListener, NOTIF_DD_SENT
    ' set up the parameters for the secondary table "GEO_OBJECT"
    NsSetDDParamsT2 nsDDParm, "GEO_OBJECT", strRepID1, dXmitDate1, NOTIF_DD_ADD
    ' make the notification call
    NsPostDataChangeMsg lngULink, NS_EF_NORMAL, NOTIF_CHG_KP, NOTIF_CHG_TYPE_ADD, _
                    gstUCurDataset, Now, CStr(lngObjectID), "local", "PDA_Users"
    
    ' close the notification link to the server
    intSuccess = iNSCloseLink(lngULink)
    
    ' close and deallocate any objects that have been created
    objCon.Close
    Set rsTemp = Nothing
    Set objCon = Nothing
    
    ' inidicate success
    PDA_Update = 0

Exit Function
Resume
PDA_Update_Err:
    ' an error occurred--indicate failure
    PDA_Update = -1
    ' check to see if we are in a transaction
    If blnInTrans Then
        'issue a rollback
        objCon.RollbackTrans
    End If
End Function

Public Function PDA_Logout(EMA_Username As String) As Integer
' Author:   Kevin Dorow
' Created:  2/14/2001
' Purpose:  This function will log out a PDA_User that is currently
'           logged in.
' Inputs:   EMA_Username (EMAdvantage Username)
' Outputs:  The function will return an integer value that will indicate
'           success or failure of the function call:
'               -1  indicates that the function failed
'               0   indicates that the function was successful

Dim objCon As New ADODB.Connection
Dim rsTemp As New ADODB.Recordset
Dim strSql As String
Dim lngGeoObjectID As Long
Dim lngObjectID As Long
Dim lngPersonRefNum As Long
Dim blnInTrans As Boolean
Dim intSuccess As Integer
Dim lngLinkFlags As Long
Dim nsDDParm As NS_DD_PARAMS
Dim dXmitDate1 As Date
Dim strRepID1 As String
Dim dXmitDate2 As Date
Dim strRepID2 As String
Dim lngULink As Long

On Error GoTo PDA_Logout_Err

    ' initialize local flags
    blnInTrans = False
    
    ' open a connection to the Oracle Database
    objCon.Open DB_Connect
    
    ' create a SQL string to retrieve the person_ref_num for this user
    strSql = "SELECT " & _
                "PERSON_REF_NUM " & _
             "FROM " & _
                "FEMIS_USER " & _
             "WHERE " & _
                "USER_CODE = '" & EMA_Username & "'"
    ' return the recordset
    rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
    If Not rsTemp.EOF Then
        ' store the person_ref_num
        lngPersonRefNum = rsTemp.Fields("PERSON_REF_NUM").Value
    Else
        ' raise an error
        Error 1
    End If
    ' close the recordset
    rsTemp.Close
    
    ' create a SQL string to determine if the user is logged in and
    '   has a geo_object_id associated to him / her
    strSql = "SELECT " & _
                "GEO_OBJECT_ID " & _
             "FROM " & _
                "EA_PDA_USER " & _
             "WHERE " & _
                "PERSON_REF_NUM = " & CStr(lngPersonRefNum) & " " & _
             "AND " & _
                "LOGGED_IN_SW = 'Y'"
    ' return the recordset
    rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
    ' check to see if any records were returned
    If rsTemp.EOF Then
        ' raise an error
        Error 1
    End If
    
    ' store the geo_object_id in local storage
    lngGeoObjectID = rsTemp.Fields("GEO_OBJECT_ID").Value
    ' close the recordset
    rsTemp.Close
    
    ' next we need to extract the object id for this object
    strSql = "SELECT " & _
                "OBJECT_ID " & _
             "FROM " & _
                "USER_DEFINED_OBJECT " & _
             "WHERE " & _
                "GEO_OBJECT_ID = " & CStr(lngGeoObjectID) & " " & _
             "AND " & _
                "EXERCISE_NUM = " & CStr(gsEID)
    ' return the recordset
    rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
    ' make sure that a record was returned
    If rsTemp.EOF Then
        ' raise an error
        Error 1
    End If
    ' store the object id in local storage
    lngObjectID = rsTemp.Fields("OBJECT_ID").Value
    ' close the recordset
    rsTemp.Close
    
    ' begin a transaction
    objCon.BeginTrans
    blnInTrans = True
    
    ' first we need to update the EA_PDA_USER table
    strSql = "UPDATE " & _
                "EA_PDA_USER " & _
             "SET " & _
                "GEO_OBJECT_ID = Null, " & _
                "LOGGED_IN_SW = 'N', " & _
                "PDA_ACTIVE_SW = 'N', " & _
                "GPS_ACTIVE_SW = 'N' " & _
             "WHERE " & _
                "PERSON_REF_NUM = " & CStr(lngPersonRefNum) & " " & _
             "AND " & _
                "EXERCISE_NUM = " & CStr(gsEID)
    ' use the connection object to perform the update
    objCon.Execute strSql
             
    ' now we can start cleaning out tables--first remove records from
    '   OBJECT_ATTR
    strSql = "DELETE " & _
             "FROM " & _
                "OBJECT_ATTR " & _
             "WHERE " & _
                "OBJECT_ID = " & CStr(lngObjectID) & " " & _
             "AND " & _
                "EXERCISE_NUM = " & CStr(gsEID)
    ' use the connection object to execute the delete
    objCon.Execute strSql
    ' next we can delete the records in USER_DEFINED_OBJECT
    strSql = "DELETE " & _
             "FROM " & _
                "USER_DEFINED_OBJECT " & _
             "WHERE " & _
                "OBJECT_ID = " & CStr(lngObjectID) & " " & _
             "AND " & _
                "EXERCISE_NUM = " & CStr(gsEID)
    ' use the connection object to execute the delete
    objCon.Execute strSql
    ' finally we can clean out the geo_object table records
    strSql = "DELETE " & _
             "FROM " & _
                "GEO_OBJECT " & _
             "WHERE " & _
                "GEO_OBJECT_ID = " & CStr(lngGeoObjectID) & " " & _
             "AND " & _
                "EXERCISE_NUM = " & CStr(gsEID)
    ' use the connection object to execute the delete
    objCon.Execute strSql
    
    objCon.CommitTrans
    blnInTrans = False
    
    ' next we need to send a notification that the PDA_User layer has been updated
    ' first init notif information
    InitNotifGlobals
    giNotifWinMsgNo = 1
    lngLinkFlags = 0
    ' open a server link
    intSuccess = iNSOpenServerLink(gstNotifServerName, gstNotifServerPort, lngLinkFlags)
    
    lngULink = ulNSOpenQLinkX(0, 0, 0, 0)
    ' store the REPLICATION_ID and XMIT_INIT_DATE in temporary storage
    dXmitDate1 = Empty
    strRepID1 = Empty
    ' set up the parameters to make a notification call for the map data change
    NsSetDDParams nsDDParm, "GEO_OBJECT", strRepID1, dXmitDate1, NOTIF_DD_ADD, gstUListener, _
                  NOTIF_DD_SENT
    ' post a notification about the map data change
    NsPostDataChangeMsgDD lngULink, NS_EF_GLOBAL, nsDDParm, NOTIF_CHG_MAP_KP, NOTIF_CHG_TYPE_ADD, _
                   gstUCurDataset, Now, CStr(lngObjectID), , "PDA_Users"
    ' set paramters for data change notification
    dXmitDate2 = Empty
    strRepID2 = Empty
    ' setup the parameters to make a notification call for the db data change
    NsSetDDParams nsDDParm, "USER_DEFINED_OBJECT", strRepID2, dXmitDate2, NOTIF_DD_ADD, _
                    gstUListener, NOTIF_DD_SENT
    ' set up the parameters for the secondary table "GEO_OBJECT"
    NsSetDDParamsT2 nsDDParm, "GEO_OBJECT", strRepID1, dXmitDate1, NOTIF_DD_ADD
    ' make the notification call
    NsPostDataChangeMsg lngULink, NS_EF_NORMAL, NOTIF_CHG_KP, NOTIF_CHG_TYPE_ADD, _
                    gstUCurDataset, Now, CStr(lngObjectID), "local", "PDA_Users"
    
    ' close the notification link to the server
    intSuccess = iNSCloseLink(lngULink)
    
    ' close and deallocate any objects that have been created
    objCon.Close
    Set rsTemp = Nothing
    Set objCon = Nothing
    
    ' indicate success
    PDA_Logout = 0

Exit Function
Resume
PDA_Logout_Err:
    ' an error occurred--indicate failure
    PDA_Logout = -1
    ' check to see if we are in a transaction
    If blnInTrans Then
        'issue a rollback
        objCon.RollbackTrans
    End If
End Function

Public Function GetMETDataNotif(EMA_Username As String, _
                                Notification_ID As Long, _
                                rsRawWeather As ADODB.Recordset) As Integer
' Author:   Kevin Dorow
' Created:  2/14/20001
' Purpose:  This function will retrieve a recordset containing
'           updated Raw Weather information based the EMA_Username and
'           the Notification_ID passed in.
' Inputs:   EMA_Username (EMAdvantage Username)
'           Notificaiton_ID (Established by EarthAlert P/S Notification filter)
' Outputs:  The function will return an integer value that will indicate
'           success or failure of the function call:
'               -1  indicates that the function failed
'               0   indicates that the function was successful
'
'           rsRawWeather is an output parameter that should be empty when passed
'           in and will contain a recordset containing the updated weather
'           inforamtion when the function completes

    Dim lngExerciseNum      As Long
    Dim lCount              As Long
    Dim strClusterNum       As String
    Dim strTowerName        As String
    Dim objCon              As New ADODB.Connection
    Dim rsTemp              As New ADODB.Recordset
    Dim strSql              As String
    Dim lngPersonRefNum     As Long
    
    On Error GoTo GetMETDataNotif_ERROR

    lngExerciseNum = 0
        
'--------------------------------------------------------------------
'   Assume the function return failure
'--------------------------------------------------------------------
    GetMETDataNotif = -1

'--------------------------------------------------------------------
'   Open a database connection
'--------------------------------------------------------------------
    objCon.Open DB_Connect
    
'--------------------------------------------------------------------------------
'   Get the person_ref_num based on the EMA_Username passed in
'--------------------------------------------------------------------------------
    strSql = "SELECT person_ref_num " & _
             "  FROM femis_user " & _
             " WHERE exercise_num = " & lngExerciseNum & " " & _
             "   AND user_code = '" & EMA_Username & "' "
    
    rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
    
    lngPersonRefNum = rsTemp("person_ref_num")
    
    rsTemp.Close
    
'--------------------------------------------------------------------------------
'   Get the facility id from the notification log table
'--------------------------------------------------------------------------------
    strSql = "SELECT parm23, parm24 " & _
             "  FROM ea_notification_log " & _
             " WHERE notification_id = " & Notification_ID
    
    rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly

    strTowerName = rsTemp("parm23")
    strClusterNum = rsTemp("parm24")
    
    rsTemp.Close
    
'--------------------------------------------------------------------------------
'   Get the data from the facility and geo object table for the facility
'--------------------------------------------------------------------------------
    strSql = "SELECT co.tower_name, co.wx_time_valid, co.exercise_num, co.cluster_num, co.wind_speed, " & _
             "       co.wind_dir, co.mixing_height, co.stability, co.temperature, co.pressure, co.humidity, " & _
             "       co.cloud_height, co.current_flag, co.cloud_cover, co.solar_rad, co.xmit_init_date, " & _
             "       co.wind_sigma , co.replication_id, co.rain_fall, geo.femis_point_x, geo.femis_point_y " & _
             "  FROM met_condition co, met_cluster cl, met_tower mt, geo_object geo " & _
             " WHERE co.tower_name = cl.tower_name " & _
             "   AND co.exercise_num = cl.exercise_num " & _
             "   AND co.cluster_num = cl.cluster_num " & _
             "   AND cl.tower_name = mt.tower_name " & _
             "   AND cl.exercise_num = mt.exercise_num " & _
             "   AND mt.exercise_num = geo.exercise_num " & _
             "   AND mt.geo_object_id = geo.geo_object_id " & _
             "   AND co.current_flag = 'Y' " & _
             "   AND co.exercise_num = " & lngExerciseNum & " " & _
             "   AND co.tower_name = '" & strTowerName & "' " & _
             "   AND cl.cluster_num = " & strClusterNum & " "

    rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
    
    For lCount = 0 To (rsTemp.Fields.Count - 1)
        If rsTemp.Fields(lCount).Type = adNumeric Then
            rsRawWeather.Fields.Append rsTemp.Fields(lCount).Name, adBigInt, rsTemp.Fields(lCount).DefinedSize, adFldUpdatable + adFldIsNullable
        Else
            rsRawWeather.Fields.Append rsTemp.Fields(lCount).Name, rsTemp.Fields(lCount).Type, rsTemp.Fields(lCount).DefinedSize, adFldUpdatable + adFldIsNullable
        End If
    Next lCount
        
    rsRawWeather.Open
    If Not (rsTemp.EOF) Then
        rsRawWeather.AddNew
        For lCount = 0 To (rsTemp.Fields.Count - 1)
            rsRawWeather.Fields(lCount).Value = rsTemp.Fields(lCount).Value
        Next lCount
        rsRawWeather.Update
    End If
    
    rsTemp.Close
    
'--------------------------------------------------------------------------------
'   If we get here, the function succedded.  This is the ONLY place that can occur.
'--------------------------------------------------------------------------------
    GetMETDataNotif = 0
    Exit Function

GetMETDataNotif_ERROR:
    'If we get here, we automatically return failure
End Function

Public Function GetMETData(EMA_Username As String, _
                           rsRawWeather As ADODB.Recordset) As Integer
' Author:   Kevin Dorow
' Created:  2/14/20001
' Purpose:  This function will retrieve a recordset containing
'           updated Raw Weather information based the EMA_Username.
' Inputs:   EMA_Username (EMAdvantage Username)
' Outputs:  The function will return an integer value that will indicate
'           success or failure of the function call:
'               -1  indicates that the function failed
'               0   indicates that the function was successful
'
'           rsRawWeather is an output parameter that should be empty when passed
'           in and will contain a recordset containing the updated weather
'           inforamtion when the function completes

    Dim objCon              As New ADODB.Connection
    Dim rsCtyGrp            As New ADODB.Recordset
    Dim rsFacID             As New ADODB.Recordset
    Dim rsTemp              As New ADODB.Recordset
    Dim varGroupID          As Variant
    Dim bDoIt               As Boolean
    Dim strSql              As String
    Dim strFacilityID       As String
    Dim strCountyID         As String
    Dim strGroupID          As String
    Dim strClusterID        As String
    Dim strTowerName        As String
    Dim lCount              As Long
    Dim lngExerciseNum      As Long
    Dim lngPersonRefNum     As Long
    Dim lGroupID            As Long
    
    On Error GoTo GetMETData_ERROR
    
    lngExerciseNum = 0
        
'--------------------------------------------------------------------
'   Assume the function return failure
'--------------------------------------------------------------------
    GetMETData = -1
    bDoIt = True

    ' open a connection to the Oracle Database
    objCon.Open DB_Connect
    
'--------------------------------------------------------------------------------
'   Get the person_ref_num based on the EMA_Username passed in
'--------------------------------------------------------------------------------
    strSql = "SELECT person_ref_num " & _
             "  FROM femis_user " & _
             " WHERE exercise_num = " & lngExerciseNum & " " & _
             "   AND user_code = '" & EMA_Username & "' "
    
    rsCtyGrp.Open strSql, objCon, adOpenStatic, adLockReadOnly
    
    lngPersonRefNum = rsCtyGrp("person_ref_num")
    
    rsCtyGrp.Close
    
'--------------------------------------------------------------------------------
'   Get the counties and groups that the person is a member of
'--------------------------------------------------------------------------------
    strSql = "SELECT county_id, group_id " & _
             "  FROM ea_person " & _
             " WHERE exercise_num = " & lngExerciseNum & " " & _
             "   AND person_ref_num = " & lngPersonRefNum & " "
    
    rsCtyGrp.Open strSql, objCon, adOpenStatic, adLockReadOnly
    Do While Not rsCtyGrp.EOF
    
        strCountyID = rsCtyGrp("county_id")
        varGroupID = rsCtyGrp("group_id")
        If IsNull(varGroupID) Then
            lGroupID = 0
        Else
            lGroupID = CLng(varGroupID)
        End If
        
        '--------------------------------------------------------------------------------
        '   Get the facility_id from ea_facility from the county and group
        '--------------------------------------------------------------------------------
        strSql = "SELECT cluster_id, tower_name " & _
                 "  FROM ea_tower_cluster " & _
                 " WHERE exercise_num = " & lngExerciseNum & " " & _
                 "   AND county_id = " & strCountyID & " "
                 
        If lGroupID <> 0 Then
            strSql = strSql & " AND group_id = " & lGroupID & " "
        Else
            strSql = strSql & " AND group_id is null"
        End If
        
        rsFacID.Open strSql, objCon, adOpenStatic, adLockReadOnly
        Do While Not rsFacID.EOF
            
            strClusterID = rsFacID("cluster_id")
            strTowerName = rsFacID("tower_name")
            
            '--------------------------------------------------------------------------------
            '   Get the facility data
            '--------------------------------------------------------------------------------
            strSql = "SELECT co.tower_name, co.wx_time_valid, co.exercise_num, co.cluster_num, co.wind_speed, " & _
                     "       co.wind_dir, co.mixing_height, co.stability, co.temperature, co.pressure, co.humidity, " & _
                     "       co.cloud_height, co.current_flag, co.cloud_cover, co.solar_rad, co.xmit_init_date, " & _
                     "       co.wind_sigma , co.replication_id, co.rain_fall, geo.femis_point_x, geo.femis_point_y " & _
                     "  FROM met_condition co, met_cluster cl, met_tower mt, geo_object geo " & _
                     " WHERE co.tower_name = cl.tower_name " & _
                     "   AND co.exercise_num = cl.exercise_num " & _
                     "   AND co.cluster_num = cl.cluster_num " & _
                     "   AND cl.tower_name = mt.tower_name " & _
                     "   AND cl.exercise_num = mt.exercise_num " & _
                     "   AND mt.exercise_num = geo.exercise_num " & _
                     "   AND mt.geo_object_id = geo.geo_object_id " & _
                     "   AND co.current_flag = 'Y' " & _
                     "   AND co.exercise_num = " & lngExerciseNum & " " & _
                     "   AND co.tower_name = '" & strTowerName & "' " & _
                     "   AND cl.cluster_id = " & strClusterID & " "
            
            rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
            
            If bDoIt Then
                bDoIt = False
                For lCount = 0 To (rsTemp.Fields.Count - 1)
                    If rsTemp.Fields(lCount).Type = adNumeric Then
                        rsRawWeather.Fields.Append rsTemp.Fields(lCount).Name, adBigInt, rsTemp.Fields(lCount).DefinedSize, adFldUpdatable + adFldIsNullable
                    Else
                        rsRawWeather.Fields.Append rsTemp.Fields(lCount).Name, rsTemp.Fields(lCount).Type, rsTemp.Fields(lCount).DefinedSize, adFldUpdatable + adFldIsNullable
                    End If
                Next lCount
                
                rsRawWeather.Open

            End If
            
            Do While Not rsTemp.EOF
                rsRawWeather.AddNew
                For lCount = 0 To (rsTemp.Fields.Count - 1)
                    rsRawWeather.Fields(lCount).Value = rsTemp.Fields(lCount).Value
                Next lCount
                rsRawWeather.Update
                
                rsTemp.MoveNext
            Loop
            rsTemp.Close
            rsFacID.MoveNext
        Loop
        rsFacID.Close
        rsCtyGrp.MoveNext
    Loop
    rsCtyGrp.Close
    
'--------------------------------------------------------------------------------
'   If we get here, the function successful.  This is the ONLY place that can occur.
'--------------------------------------------------------------------------------
    GetMETData = 0
    Exit Function

GetMETData_ERROR:
    'If we get here, we automatically return failure
End Function

Public Function GetFacilityDataNotif(EMA_Username As String, _
                                     Notification_ID As Long, _
                                     rsFacility As ADODB.Recordset) As Integer
' Author:   Kevin Dorow
' Created:  2/14/20001
' Purpose:  This function will retrieve a recordset containing
'           updated Facility information based the EMA_Username and
'           the Notification_ID passed in.
' Inputs:   EMA_Username (EMAdvantage Username)
'           Notificaiton_ID (Established by EarthAlert P/S Notification filter)
' Outputs:  The function will return an integer value that will indicate
'           success or failure of the function call:
'               -1  indicates that the function failed
'               0   indicates that the function was successful
'
'           rsFacility is an output parameter that should be empty when passed
'           in and will contain a recordset containing the updated Facility
'           inforamtion when the function completes

    
    Dim strFacilityID       As String
    Dim lngExerciseNum      As Long
    Dim lCount              As Long
    Dim objCon              As New ADODB.Connection
    Dim rsTemp              As New ADODB.Recordset
    Dim bDoIt               As Boolean
    Dim strSql              As String
    Dim lngPersonRefNum     As Long
    
    On Error GoTo GetFacilityDataNotif_ERROR
    
    lngExerciseNum = 0
        
'--------------------------------------------------------------------
'   Assume the function returns failure
'--------------------------------------------------------------------
    GetFacilityDataNotif = -1
    bDoIt = True

'--------------------------------------------------------------------
'   Open a database connection
'--------------------------------------------------------------------
    objCon.Open DB_Connect
    
'--------------------------------------------------------------------------------
'   Get the person_ref_num based on the EMA_Username passed in
'--------------------------------------------------------------------------------
    strSql = "SELECT person_ref_num " & _
             "  FROM femis_user " & _
             " WHERE exercise_num = " & lngExerciseNum & " " & _
             "   AND user_code = '" & EMA_Username & "' "
    
    rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
    
    lngPersonRefNum = rsTemp("person_ref_num")
    
    rsTemp.Close
    
'--------------------------------------------------------------------------------
'   Get the facility id from the notification log table
'--------------------------------------------------------------------------------
    strSql = "SELECT parm23 " & _
             "  FROM ea_notification_log " & _
             " WHERE notification_id = " & Notification_ID
    
    rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly

    strFacilityID = rsTemp("parm23")
    
    rsTemp.Close
    
    '--------------------------------------------------------------------------------
    '   Get the facility data
    '--------------------------------------------------------------------------------
    strSql = "SELECT distinct f.facility_id, f.exercise_num, f.facility_name, f.geo_object_id, f.facility_description, " & _
             "       f.facility_capacity, f.pressurized_flag, f.facility_address1, f.facility_city, f.facility_state_code, " & _
             "       f.facility_zip_code, f.facility_poc_agency, f.facility_poc_dept, f.facility_poc_position, " & _
             "       f.facility_poc_person_ref_num, f.facility_poc_phone, f.facility_shelter_cap_flag, f.facility_address2, " & _
             "       f.main_phone, f.fax_phone, f.cel_phone, f.beeper_phone, f.email_address, f.email_address2, f.web_address, " & _
             "       f.mou_id, f.shelter_activation_date, f.shelter_deactivation_date, f.shelter_description, f.shelter_capacity, " & _
             "       f.shelter_type, f.eoc_name, f.xmit_init_date, f.replication_id , geo.femis_point_x, geo.femis_point_y " & _
             "  FROM facility f, geo_object geo " & _
             " WHERE f.geo_object_id = geo.geo_object_id " & _
             "   AND f.exercise_num = " & lngExerciseNum & " " & _
             "   AND facility_id = " & strFacilityID & " "
                                 
    rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
            
    For lCount = 0 To (rsTemp.Fields.Count - 1)
        ' added code to prevent the truncation of the lat / long values 5/11/2001 KED
        If InStr(1, LCase(rsTemp.Fields(lCount).Name), "femis_point") > 0 Then
            rsFacility.Fields.Append rsTemp.Fields(lCount).Name, adDouble, rsTemp.Fields(lCount).DefinedSize, adFldUpdatable + adFldIsNullable
        ElseIf rsTemp.Fields(lCount).Type = adNumeric Then
            rsFacility.Fields.Append rsTemp.Fields(lCount).Name, adBigInt, rsTemp.Fields(lCount).DefinedSize, adFldUpdatable + adFldIsNullable
        Else
            rsFacility.Fields.Append rsTemp.Fields(lCount).Name, rsTemp.Fields(lCount).Type, rsTemp.Fields(lCount).DefinedSize, adFldUpdatable + adFldIsNullable
        End If
    Next lCount
                
    rsFacility.Open
    
    If Not (rsTemp.EOF) Then
        rsFacility.AddNew
        For lCount = 0 To (rsTemp.Fields.Count - 1)
            rsFacility.Fields(lCount).Value = rsTemp.Fields(lCount).Value
        Next lCount
        rsFacility.Update
    End If
                
    rsTemp.Close

'--------------------------------------------------------------------------------
'   If we get here, the function succedded.  This is the ONLY place that can occur.
'--------------------------------------------------------------------------------
    GetFacilityDataNotif = 0
    Exit Function

GetFacilityDataNotif_ERROR:
    'If we get here, we automatically return failure
End Function

Public Function GetFacilityData(EMA_Username As String, _
                                rsFacility As ADODB.Recordset) As Integer
' Author:   Kevin Dorow
' Created:  2/14/20001
' Purpose:  This function will retrieve a recordset containing
'           updated Facility information based the EMA_Username.
' Inputs:   EMA_Username (EMAdvantage Username)
' Outputs:  The function will return an integer value that will indicate
'           success or failure of the function call:
'               -1  indicates that the function failed
'               0   indicates that the function was successful
'
'           rsFacility is an output parameter that should be empty when passed
'           in and will contain a recordset containing the updated Facility
'           inforamtion when the function completes

    Dim objCon              As New ADODB.Connection
    Dim rsCtyGrp            As New ADODB.Recordset
    Dim rsFacID             As New ADODB.Recordset
    Dim rsTemp              As New ADODB.Recordset
    Dim varGroupID          As Variant
    Dim bDoIt               As Boolean
    Dim strSql              As String
    Dim strFacilityID       As String
    Dim strCountyID         As String
    Dim strGroupID          As String
    Dim lCount              As Long
    Dim lngExerciseNum      As Long
    Dim lngPersonRefNum     As Long
    Dim lGroupID            As Long
    
    On Error GoTo GetFacilityData_ERROR
    
    lngExerciseNum = 0
        
'--------------------------------------------------------------------
'   Assume the function returns failure
'--------------------------------------------------------------------
    GetFacilityData = -1
    bDoIt = True

'--------------------------------------------------------------------
'   Open a database connection
'--------------------------------------------------------------------
    objCon.Open DB_Connect
    
'--------------------------------------------------------------------------------
'   Get the person_ref_num based on the EMA_Username passed in
'--------------------------------------------------------------------------------
    strSql = "SELECT person_ref_num " & _
             "  FROM femis_user " & _
             " WHERE exercise_num = " & lngExerciseNum & " " & _
             "   AND user_code = '" & EMA_Username & "' "
    
    rsCtyGrp.Open strSql, objCon, adOpenStatic, adLockReadOnly
    
    lngPersonRefNum = rsCtyGrp("person_ref_num")
    
    rsCtyGrp.Close
    
'--------------------------------------------------------------------------------
'   Get the counties and groups that the person is a member of
'--------------------------------------------------------------------------------
    strSql = "SELECT county_id, group_id " & _
             "  FROM ea_person " & _
             " WHERE exercise_num = " & lngExerciseNum & " " & _
             "   AND person_ref_num = " & lngPersonRefNum & " "
    
    rsCtyGrp.Open strSql, objCon, adOpenStatic, adLockReadOnly
    Do While Not rsCtyGrp.EOF
    
        strCountyID = rsCtyGrp("county_id")
        varGroupID = rsCtyGrp("group_id")
        If IsNull(varGroupID) Then
            lGroupID = 0
        Else
            lGroupID = CLng(varGroupID)
        End If
        
        '--------------------------------------------------------------------------------
        '   Get the facility_id from ea_facility from the county and group
        '--------------------------------------------------------------------------------
        strSql = "SELECT facility_id " & _
                 "  FROM ea_facility " & _
                 " WHERE exercise_num = " & lngExerciseNum & " " & _
                 "   AND county_id = " & strCountyID & " "
                 
        If (lGroupID <> 0) Then
            strSql = strSql & " AND group_id = " & lGroupID & " "
        Else
            strSql = strSql & " AND group_id is null"
        End If
        
        'rsFacID.Close
        rsFacID.Open strSql, objCon, adOpenStatic, adLockReadOnly
        Do While Not rsFacID.EOF
            
            strFacilityID = rsFacID("facility_id")
            
        '--------------------------------------------------------------------------------
        '   Get the facility data
        '--------------------------------------------------------------------------------
            strSql = "SELECT distinct f.facility_id, f.exercise_num, f.facility_name, f.geo_object_id, f.facility_description, " & _
                     "       f.facility_capacity, f.pressurized_flag, f.facility_address1, f.facility_city, f.facility_state_code, " & _
                     "       f.facility_zip_code, f.facility_poc_agency, f.facility_poc_dept, f.facility_poc_position, " & _
                     "       f.facility_poc_person_ref_num, f.facility_poc_phone, f.facility_shelter_cap_flag, f.facility_address2, " & _
                     "       f.main_phone, f.fax_phone, f.cel_phone, f.beeper_phone, f.email_address, f.email_address2, f.web_address, " & _
                     "       f.mou_id, f.shelter_activation_date, f.shelter_deactivation_date, f.shelter_description, f.shelter_capacity, " & _
                     "       f.shelter_type, f.eoc_name, f.xmit_init_date, f.replication_id , geo.femis_point_x, geo.femis_point_y " & _
                     "  FROM facility f, geo_object geo " & _
                     " WHERE f.geo_object_id = geo.geo_object_id " & _
                     "   AND f.exercise_num = " & lngExerciseNum & " " & _
                     "   AND facility_id = " & strFacilityID & " "
                                 
            rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
            
            If bDoIt Then
                bDoIt = False
                For lCount = 0 To (rsTemp.Fields.Count - 1)
                    ' added code to prevent the truncation of the lat / long values 5/11/2001 KED
                    If InStr(1, LCase(rsTemp.Fields(lCount).Name), "femis_point") > 0 Then
                        rsFacility.Fields.Append rsTemp.Fields(lCount).Name, adDouble, rsTemp.Fields(lCount).DefinedSize, adFldUpdatable + adFldIsNullable
                    ElseIf rsTemp.Fields(lCount).Type = adNumeric Then
                        rsFacility.Fields.Append rsTemp.Fields(lCount).Name, adBigInt, rsTemp.Fields(lCount).DefinedSize, adFldUpdatable + adFldIsNullable
                    Else
                        rsFacility.Fields.Append rsTemp.Fields(lCount).Name, rsTemp.Fields(lCount).Type, rsTemp.Fields(lCount).DefinedSize, adFldUpdatable + adFldIsNullable
                    End If
                Next lCount
                
                rsFacility.Open
            End If
            
            Do While Not rsTemp.EOF
                rsFacility.AddNew
                For lCount = 0 To (rsTemp.Fields.Count - 1)
                    rsFacility.Fields(lCount).Value = rsTemp.Fields(lCount).Value
                Next lCount
                rsFacility.Update
                
                rsTemp.MoveNext
            Loop
            rsTemp.Close
            rsFacID.MoveNext
        Loop
        rsFacID.Close
        rsCtyGrp.MoveNext
    Loop
    rsCtyGrp.Close
    
'--------------------------------------------------------------------------------
'   If we get here, the function succedded.  This is the ONLY place that can occur.
'--------------------------------------------------------------------------------
    GetFacilityData = 0
    Exit Function

GetFacilityData_ERROR:
    'If we get here, we automatically return failure
End Function

Public Function GetContactDataNotif(EMA_Username As String, _
                                    Notification_ID As Long, _
                                    rsContact As ADODB.Recordset) As Integer
' Author:   Kevin Dorow
' Created:  2/14/20001
' Purpose:  This function will retrieve a recordset containing
'           updated Contact information based the EMA_Username and
'           the Notification_ID passed in.
' Inputs:   EMA_Username (EMAdvantage Username)
'           Notificaiton_ID (Established by EarthAlert P/S Notification filter)
' Outputs:  The function will return an integer value that will indicate
'           success or failure of the function call:
'               -1  indicates that the function failed
'               0   indicates that the function was successful
'
'           rsContact is an output parameter that should be empty when passed
'           in and will contain a recordset containing the updated Contact
'           inforamtion when the function completes

    Dim strID               As String
    Dim strMsgName          As String
    Dim stInternal          As String
    Dim lngExerciseNum      As Long
    Dim lCount              As Long
    Dim lngDeptCode         As Long
    Dim lngAgencyCode       As Long
    Dim objCon              As New ADODB.Connection
    Dim rsTemp              As New ADODB.Recordset
    Dim bDoIt               As Boolean
    Dim strSql              As String
    Dim lngPersonRefNum     As Long
    Dim lngColunmCount      As Long
    
    On Error GoTo GetContactDataNotif_ERROR
    
    lngExerciseNum = 0
        
'--------------------------------------------------------------------
'   Assume the function returns failure
'--------------------------------------------------------------------
    GetContactDataNotif = -1
    bDoIt = True

'--------------------------------------------------------------------
'   Open a database connection
'--------------------------------------------------------------------
    objCon.Open DB_Connect
    
'--------------------------------------------------------------------------------
'   Get the person_ref_num based on the EMA_Username passed in
'--------------------------------------------------------------------------------
    strSql = "SELECT person_ref_num " & _
             "  FROM femis_user " & _
             " WHERE exercise_num = " & lngExerciseNum & " " & _
             "   AND user_code = '" & EMA_Username & "' "
    
    rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
    
    lngPersonRefNum = rsTemp("person_ref_num")
    
    rsTemp.Close
    
'--------------------------------------------------------------------------------
'   Get the table name and id from the notification log table
'--------------------------------------------------------------------------------
    strSql = "SELECT msg_name, parm23 " & _
             "  FROM ea_notification_log " & _
             " WHERE notification_id = " & Notification_ID
    
    rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly

    strMsgName = rsTemp("msg_name")
    strID = rsTemp("parm23")
    
    rsTemp.Close
    
    
'--------------------------------------------------------------------
'   prepare the return recordset
'--------------------------------------------------------------------
    lngColunmCount = 24
    For lCount = 0 To lngColunmCount
        rsContact.Fields.Append "Field" & lCount, adVarChar, MESSAGE_VALUE_DEFINED_SIZE, adFldUpdatable + adFldIsNullable
    Next lCount
    
    rsContact.Open , , adOpenDynamic
    
'--------------------------------------------------------------------------------
'   Based on the table, get the appropriate record
'--------------------------------------------------------------------------------
    If strMsgName = "Agency Data Change" Then
    
        strSql = "SELECT agency_name, main_phone, email_address, cel_phone, fax_phone, beeper_phone " & _
                 "  FROM agency " & _
                 " WHERE agency_code = " & strID
                 
        rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
        
        If Not (rsTemp.EOF) Then
        
            rsContact.AddNew
            rsContact.Fields(0).Value = "AGENCY"
            rsContact.Fields(18).Value = strID
            rsContact.Fields(19).Value = rsTemp("agency_name")
            rsContact.Fields(20).Value = rsTemp("main_phone")
            rsContact.Fields(21).Value = rsTemp("email_address")
            rsContact.Fields(22).Value = rsTemp("cel_phone")
            rsContact.Fields(23).Value = rsTemp("fax_phone")
            rsContact.Fields(24).Value = rsTemp("beeper_phone")
            rsTemp.Close
        End If
    
    ElseIf strMsgName = "Dept Data Change" Then
    
        strSql = "SELECT dept_name, main_phone, email_address, cel_phone, fax_phone, beeper_phone " & _
                 "  FROM department " & _
                 " WHERE dept_code = " & strID
                
        rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
        
        If Not (rsTemp.EOF) Then
        
            rsContact.AddNew
            rsContact.Fields(0).Value = "DEPARTMENT"
            rsContact.Fields(11).Value = strID
            rsContact.Fields(12).Value = rsTemp("dept_name")
            rsContact.Fields(13).Value = rsTemp("main_phone")
            rsContact.Fields(13).Value = rsTemp("email_address")
            rsContact.Fields(15).Value = rsTemp("cel_phone")
            rsContact.Fields(16).Value = rsTemp("fax_phone")
            rsContact.Fields(17).Value = rsTemp("beeper_phone")
            rsTemp.Close
        End If
    
    ElseIf strMsgName = "Personnel Data Change" Then
    
        '--------------------------------------------------------------------
        '   Get the person information.
        '--------------------------------------------------------------------
        strSql = "SELECT fu.user_code, p.dept_code, p.agency_code, p.work_phone, p.person_email_address, p.cel_phone, p.person_fax_phone, p.person_beeper_phone " & _
                "  FROM person p, femis_user fu " & _
                " WHERE p.person_ref_num = fu.person_ref_num (+) " & _
                "   AND p.person_ref_num = " & strID & " " & _
                "   AND p.exercise_num = " & lngExerciseNum & " "
                
        rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
        
        If Not (rsTemp.EOF) Then
        
            rsContact.AddNew
            
            If Len(rsTemp("user_code")) Then
                stInternal = "Y"
            Else
                stInternal = "N"
            End If
            
            rsContact.Fields(0).Value = "PERSON"
            rsContact.Fields(1).Value = stInternal
            rsContact.Fields(2).Value = strID
            rsContact.Fields(3).Value = rsTemp("work_phone")
            rsContact.Fields(4).Value = rsTemp("person_email_address")
            rsContact.Fields(5).Value = rsTemp("cel_phone")
            rsContact.Fields(6).Value = rsTemp("person_fax_phone")
            rsContact.Fields(7).Value = rsTemp("person_beeper_phone")
            
            
            If Not (IsNull(rsTemp("dept_code"))) Then
                lngDeptCode = rsTemp("dept_code")
            Else
                lngDeptCode = 0
            End If
            
            If Not (IsNull(rsTemp("agency_code"))) Then
                lngAgencyCode = rsTemp("agency_code")
            Else
                lngAgencyCode = 0
            End If
            
            rsTemp.Close
            
            '--------------------------------------------------------------------
            '   Get the Department info
            '--------------------------------------------------------------------
            If lngDeptCode Then
            
                strSql = "SELECT dept_name, main_phone, email_address, cel_phone, fax_phone, beeper_phone " & _
                         "  FROM department " & _
                         " WHERE dept_code = " & lngDeptCode
                        
                rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
                
                rsContact.Fields(11).Value = lngDeptCode
                rsContact.Fields(12).Value = rsTemp("dept_name")
                rsContact.Fields(13).Value = rsTemp("main_phone")
                rsContact.Fields(14).Value = rsTemp("email_address")
                rsContact.Fields(15).Value = rsTemp("cel_phone")
                rsContact.Fields(16).Value = rsTemp("fax_phone")
                rsContact.Fields(17).Value = rsTemp("beeper_phone")
                rsTemp.Close
            
            End If
            
            '--------------------------------------------------------------------
            '   Get the Agency info
            '--------------------------------------------------------------------
            If lngAgencyCode Then
            
                strSql = "SELECT agency_name, main_phone, email_address, cel_phone, fax_phone, beeper_phone " & _
                         "  FROM agency " & _
                         " WHERE agency_code = " & lngAgencyCode
                        
                rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
                
                rsContact.Fields(18).Value = lngAgencyCode
                rsContact.Fields(19).Value = rsTemp("agency_name")
                rsContact.Fields(20).Value = rsTemp("main_phone")
                rsContact.Fields(21).Value = rsTemp("email_address")
                rsContact.Fields(22).Value = rsTemp("cel_phone")
                rsContact.Fields(23).Value = rsTemp("fax_phone")
                rsContact.Fields(24).Value = rsTemp("beeper_phone")
                rsTemp.Close
                        
            End If
        End If
                
    End If
    
'--------------------------------------------------------------------------------
'   If we get here, the function succedded.  This is the ONLY place that can occur.
'--------------------------------------------------------------------------------
    GetContactDataNotif = 0
    Exit Function

GetContactDataNotif_ERROR:
    'If we get here, we automatically return failure
End Function

Public Function GetContactData(EMA_Username As String, _
                               rsContact As ADODB.Recordset) As Integer
' Author:   Kevin Dorow
' Created:  2/14/20001
' Purpose:  This function will retrieve a recordset containing
'           updated Contact information based the EMA_Username.
' Inputs:   EMA_Username (EMAdvantage Username)
' Outputs:  The function will return an integer value that will indicate
'           success or failure of the function call:
'               -1  indicates that the function failed
'               0   indicates that the function was successful
'
'           rsContact is an output parameter that should be empty when passed
'           in and will contain a recordset containing the updated Contact
'           inforamtion when the function completes
    
    Dim strSql              As String
    Dim stTableName         As String
    Dim stTableCode         As String
    Dim stInternal          As String
    Dim lngPersonRefNum     As Long
    Dim lngColunmCount      As Long
    Dim lCount              As Long
    Dim lExerciseNum        As Long
    Dim lngDeptCode         As Long
    Dim lngAgencyCode       As Long
    Dim objCon              As New ADODB.Connection
    Dim rsList              As New ADODB.Recordset
    Dim rsCtyGrp            As New ADODB.Recordset
    Dim rsEAContact         As New ADODB.Recordset
    Dim rsPerson            As New ADODB.Recordset
    Dim rsData              As New ADODB.Recordset
    Dim strCountyID         As String
    Dim varGroupID          As Variant
    Dim lGroupID            As Long
    
    On Error GoTo GetContactData_ERROR
    
'--------------------------------------------------------------------
'   Assume the function return failure
'--------------------------------------------------------------------
    GetContactData = -1
    
    lExerciseNum = 0

'--------------------------------------------------------------------
'   Open a database connection
'--------------------------------------------------------------------
    objCon.Open DB_Connect
    
'--------------------------------------------------------------------
'   prepare the return recordset
'--------------------------------------------------------------------
    lngColunmCount = 24
    For lCount = 0 To lngColunmCount
        rsContact.Fields.Append "Field" & lCount, adVarChar, MESSAGE_VALUE_DEFINED_SIZE, adFldUpdatable + adFldIsNullable
    Next lCount
    
    rsContact.Open , , adOpenDynamic

'--------------------------------------------------------------------------------
'   Get the person_ref_num based on the EMA_Username passed in
'--------------------------------------------------------------------------------
    strSql = "SELECT person_ref_num " & _
             "  FROM femis_user " & _
             " WHERE exercise_num = " & lExerciseNum & " " & _
             "   AND user_code = '" & EMA_Username & "' "
    
    rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
    
    lngPersonRefNum = rsList("person_ref_num")
    
    rsList.Close
    
'--------------------------------------------------------------------------------
'   Get the counties and groups that the person is a member of
'--------------------------------------------------------------------------------
    strSql = "SELECT county_id, group_id " & _
             "  FROM ea_person " & _
             " WHERE exercise_num = " & lExerciseNum & " " & _
             "   AND person_ref_num = " & lngPersonRefNum & " "
    
    rsCtyGrp.Open strSql, objCon, adOpenStatic, adLockReadOnly
    Do While Not rsCtyGrp.EOF
    
        strCountyID = rsCtyGrp("county_id")
        varGroupID = rsCtyGrp("group_id")
        If IsNull(varGroupID) Then
            lGroupID = 0
        Else
            lGroupID = CLng(varGroupID)
        End If
        
        '--------------------------------------------------------------------------------
        '   Get the contact information
        '--------------------------------------------------------------------------------
        strSql = "SELECT person_ref_num, table_code, table_name, ea_user_sw " & _
                 "  FROM ea_contact " & _
                 " WHERE exercise_num = " & lExerciseNum & " " & _
                 "   AND county_id = " & strCountyID & " "
        
        If (lGroupID <> 0) Then
            strSql = strSql & " AND group_id = " & lGroupID & " "
        Else
            strSql = strSql & " AND group_id is null"
        End If
        
        rsEAContact.Open strSql, objCon, adOpenStatic, adLockReadOnly
        Do While Not rsEAContact.EOF
            stTableName = rsEAContact("table_name")
            stTableCode = rsEAContact("table_code")
            stInternal = rsEAContact("ea_user_sw")
            If IsNull(rsEAContact("person_ref_num")) Then
                lngPersonRefNum = 0
            Else
                lngPersonRefNum = rsEAContact("person_ref_num")
            End If
            
            rsContact.AddNew
            
            If lngPersonRefNum Then
            '--------------------------------------------------------------------
            '   Get the person information.
            '--------------------------------------------------------------------
                strSql = "SELECT p.dept_code, p.agency_code, p.work_phone, p.person_email_address, p.cel_phone, p.person_fax_phone, p.person_beeper_phone " & _
                        "  FROM person p " & _
                        " WHERE p.person_ref_num = " & lngPersonRefNum & " " & _
                        "   AND p.exercise_num = " & lExerciseNum & " "
                        
                rsPerson.Open strSql, objCon, adOpenStatic, adLockReadOnly
                
                rsContact.Fields(0).Value = "PERSON"
                rsContact.Fields(1).Value = stInternal
                rsContact.Fields(2).Value = lngPersonRefNum
                rsContact.Fields(3).Value = rsPerson("work_phone")
                rsContact.Fields(4).Value = rsPerson("person_email_address")
                rsContact.Fields(5).Value = rsPerson("cel_phone")
                rsContact.Fields(6).Value = rsPerson("person_fax_phone")
                rsContact.Fields(7).Value = rsPerson("person_beeper_phone")
                
                
                If Not (IsNull(rsPerson("dept_code"))) Then
                    lngDeptCode = rsPerson("dept_code")
                Else
                    lngDeptCode = 0
                End If
                
                If Not (IsNull(rsPerson("agency_code"))) Then
                    lngAgencyCode = rsPerson("agency_code")
                Else
                    lngAgencyCode = 0
                End If
                
                rsPerson.Close
                
                '--------------------------------------------------------------------
                '   If the contact is a position, get the position info
                '--------------------------------------------------------------------
                If stTableName = "POSITION" Then
                    
                    strSql = "SELECT position_phone, position_email_address " & _
                             "  FROM position " & _
                             " WHERE position_code = '" & stTableCode & "' "
                            
                    rsData.Open strSql, objCon, adOpenStatic, adLockReadOnly
                    
                    rsContact.Fields(8).Value = stTableCode
                    rsContact.Fields(9).Value = rsData("position_phone")
                    rsContact.Fields(10).Value = rsData("position_email_address")
                    rsData.Close
                    
                End If
                
                '--------------------------------------------------------------------
                '   Get the Department info
                '--------------------------------------------------------------------
                If lngDeptCode Then
                
                    strSql = "SELECT dept_name, main_phone, email_address, cel_phone, fax_phone, beeper_phone " & _
                             "  FROM department " & _
                             " WHERE dept_code = " & lngDeptCode
                            
                    rsData.Open strSql, objCon, adOpenStatic, adLockReadOnly
                    
                    rsContact.Fields(11).Value = lngDeptCode
                    rsContact.Fields(12).Value = rsData("dept_name")
                    rsContact.Fields(13).Value = rsData("main_phone")
                    rsContact.Fields(14).Value = rsData("email_address")
                    rsContact.Fields(15).Value = rsData("cel_phone")
                    rsContact.Fields(16).Value = rsData("fax_phone")
                    rsContact.Fields(17).Value = rsData("beeper_phone")
                    rsData.Close
                
                End If
                
                '--------------------------------------------------------------------
                '   Get the Agency info
                '--------------------------------------------------------------------
                If lngAgencyCode Then
                
                    strSql = "SELECT agency_name, main_phone, email_address, cel_phone, fax_phone, beeper_phone " & _
                             "  FROM agency " & _
                             " WHERE agency_code = " & lngAgencyCode
                            
                    rsData.Open strSql, objCon, adOpenStatic, adLockReadOnly
                    
                    rsContact.Fields(18).Value = lngAgencyCode
                    rsContact.Fields(19).Value = rsData("agency_name")
                    rsContact.Fields(20).Value = rsData("main_phone")
                    rsContact.Fields(21).Value = rsData("email_address")
                    rsContact.Fields(22).Value = rsData("cel_phone")
                    rsContact.Fields(23).Value = rsData("fax_phone")
                    rsContact.Fields(24).Value = rsData("beeper_phone")
                    rsData.Close
                            
                End If
                
            Else
                rsContact.Fields(0).Value = stTableName
                rsContact.Fields(1).Value = stInternal
            
                If stTableName = "POSITION" Then
                    '--------------------------------------------------------------------
                    '   Get position information.
                    '--------------------------------------------------------------------
                    strSql = "SELECT position_phone, position_email_address " & _
                             "  FROM position " & _
                             " WHERE position_code = '" & stTableCode & "' "
                            
                    rsData.Open strSql, objCon, adOpenStatic, adLockReadOnly
                    
                    rsContact.Fields(8).Value = stTableCode
                    rsContact.Fields(9).Value = rsData("position_phone")
                    rsContact.Fields(10).Value = rsData("position_email_address")
                    rsData.Close
                
                ElseIf stTableName = "DEPARTMENT" Then
                
                    strSql = "SELECT dept_name, main_phone, email_address, cel_phone, fax_phone, beeper_phone " & _
                             "  FROM department " & _
                             " WHERE dept_code = " & stTableCode
                            
                    rsData.Open strSql, objCon, adOpenStatic, adLockReadOnly
                    
                    rsContact.Fields(11).Value = lngDeptCode
                    rsContact.Fields(12).Value = rsData("dept_name")
                    rsContact.Fields(13).Value = rsData("main_phone")
                    rsContact.Fields(13).Value = rsData("email_address")
                    rsContact.Fields(15).Value = rsData("cel_phone")
                    rsContact.Fields(16).Value = rsData("fax_phone")
                    rsContact.Fields(17).Value = rsData("beeper_phone")
                    rsData.Close
                
                Else 'stTableName = "AGENCY"
                
                    strSql = "SELECT agency_name, main_phone, email_address, cel_phone, fax_phone, beeper_phone " & _
                             "  FROM agency " & _
                             " WHERE agency_code = " & stTableCode
                            
                    rsData.Open strSql, objCon, adOpenStatic, adLockReadOnly
                    
                    rsContact.Fields(18).Value = lngAgencyCode
                    rsContact.Fields(19).Value = rsData("agency_name")
                    rsContact.Fields(20).Value = rsData("main_phone")
                    rsContact.Fields(21).Value = rsData("email_address")
                    rsContact.Fields(22).Value = rsData("cel_phone")
                    rsContact.Fields(23).Value = rsData("fax_phone")
                    rsContact.Fields(24).Value = rsData("beeper_phone")
                    rsData.Close
                
                End If
            End If
            rsEAContact.MoveNext
        Loop
        
        rsEAContact.Close
        rsCtyGrp.MoveNext
    Loop
    
    rsCtyGrp.Close
    rsContact.Update

'--------------------------------------------------------------------
'   If we get here, the function successful.  This is the ONLY place that can occur.
'--------------------------------------------------------------------------------
    GetContactData = 0
    Exit Function

GetContactData_ERROR:
    'If we get here, we automatically return failure
End Function

Public Function GetUserDataNotif(EMA_Username As String, _
                                 Notification_ID As Long, _
                                 rsUser As ADODB.Recordset) As Integer
' Author:   Kevin Dorow
' Created:  2/14/20001
' Purpose:  This function will retrieve a recordset containing
'           updated User information based the EMA_Username and
'           the Notification_ID passed in.
' Inputs:   EMA_Username (EMAdvantage Username)
'           Notificaiton_ID (Established by EarthAlert P/S Notification filter)
' Outputs:  The function will return an integer value that will indicate
'           success or failure of the function call:
'               -1  indicates that the function failed
'               0   indicates that the function was successful
'
'           rsUser is an output parameter that should be empty when passed
'           in and will contain a recordset containing the updated User
'           inforamtion when the function completes

    Dim lngExerciseNum      As Long
    Dim lCount              As Long
    Dim strClusterNum       As String
    Dim strPersonRefNum     As String
    Dim objCon              As New ADODB.Connection
    Dim rsTemp              As New ADODB.Recordset
    Dim rsPerson            As New ADODB.Recordset
    Dim strSql              As String
    Dim lngPersonRefNum     As Long
    
    On Error GoTo GetUserDataNotif_ERROR
    
    lngExerciseNum = 0
        
'--------------------------------------------------------------------
'   Assume the function return failure
'--------------------------------------------------------------------
    GetUserDataNotif = -1

'--------------------------------------------------------------------
'   Open a database connection
'--------------------------------------------------------------------
    objCon.Open DB_Connect
    
'--------------------------------------------------------------------------------
'   Get the person_ref_num based on the EMA_Username passed in
'--------------------------------------------------------------------------------
    strSql = "SELECT person_ref_num " & _
             "  FROM femis_user " & _
             " WHERE exercise_num = " & lngExerciseNum & " " & _
             "   AND user_code = '" & EMA_Username & "' "
    
    rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
    
    lngPersonRefNum = rsTemp("person_ref_num")
    
    rsTemp.Close
    
'--------------------------------------------------------------------------------
'   Get the facility id from the notification log table
'--------------------------------------------------------------------------------
    strSql = "SELECT parm23 " & _
             "  FROM ea_notification_log " & _
             " WHERE notification_id = " & Notification_ID
    
    rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly

    strPersonRefNum = rsTemp("parm23")
    
    rsTemp.Close
    
    '--------------------------------------------------------------------------------
    '   Get the information on the person
    '--------------------------------------------------------------------------------
    strSql = "SELECT DISTINCT person_ref_num, exercise_num, name_last, name_first, name_second, home_phone, " & _
             "       work_phone, person_email_address, person_fax_phone, person_beeper_phone, person_work_hours, " & _
             "       person_status, person_comment, personal_id_num, person_nick_name, state_code, city_name, " & _
             "       street_address1, street_address2, zip_code, home_state_code, home_city_name, home_street_address1, " & _
             "       home_street_address2, home_zip_code, home_zone, work_zone, cel_phone, email_address2, " & _
             "       web_address, agency_code, dept_code, wk_position_id " & _
             "  FROM person " & _
             " WHERE exercise_num = 0 " & _
             "   AND person_ref_num = " & strPersonRefNum & " "
    
    rsPerson.Open strSql, objCon, adOpenStatic, adLockReadOnly
    
    '--------------------------------------------------------------------------------
    '   Setup the record set that will be passed back.  Only do this once.
    '--------------------------------------------------------------------------------
    For lCount = 0 To (rsPerson.Fields.Count - 1)
        If rsPerson.Fields(lCount).Type = adNumeric Then
            rsUser.Fields.Append rsPerson.Fields(lCount).Name, adBigInt, rsPerson.Fields(lCount).DefinedSize, adFldUpdatable + adFldIsNullable
        Else
            rsUser.Fields.Append rsPerson.Fields(lCount).Name, rsPerson.Fields(lCount).Type, rsPerson.Fields(lCount).DefinedSize, adFldUpdatable + adFldIsNullable
        End If
    Next lCount
    rsUser.Open
            
    rsUser.AddNew
    For lCount = 0 To (rsPerson.Fields.Count - 1)
        rsUser.Fields(lCount).Value = rsPerson.Fields(lCount).Value
    Next lCount
    rsUser.Update

    rsPerson.Close
    
'--------------------------------------------------------------------------------
'   If we get here, the function succedded.  This is the ONLY place that can occur.
'--------------------------------------------------------------------------------
    GetUserDataNotif = 0
    Exit Function

    
GetUserDataNotif_ERROR:
    'If we get here, we automatically return failure
End Function

Public Function GetUserData(EMA_Username As String, _
                            rsUser As ADODB.Recordset) As Integer
' Author:   Kevin Dorow
' Created:  2/14/20001
' Purpose:  This function will retrieve a recordset containing
'           updated User information based the EMA_Username.
' Inputs:   EMA_Username (EMAdvantage Username)
' Outputs:  The function will return an integer value that will indicate
'           success or failure of the function call:
'               -1  indicates that the function failed
'               0   indicates that the function was successful
'
'           rsUser is an output parameter that should be empty when passed
'           in and will contain a recordset containing the updated User
'           inforamtion when the function completes

    Dim objCon              As New ADODB.Connection
    Dim rsList              As New ADODB.Recordset
    Dim rsCtyGrp            As New ADODB.Recordset
    Dim rsPerson            As New ADODB.Recordset
    Dim lCount              As Long
    Dim lngPersonRefNum     As Long
    Dim lngExerciseNum      As Long
    Dim strSql              As String
    Dim bDoIt               As Boolean
    Dim varBookmark         As Variant
    Dim strCountyID         As String
    Dim varGroupID          As Variant
    Dim lGroupID            As Long
    Dim bExists             As Boolean
    Dim lCurrent            As Long
    
    On Error GoTo GetUserData_ERROR
    
    lngExerciseNum = 0
    bDoIt = True

'--------------------------------------------------------------------
'   Assume the function return failure
'--------------------------------------------------------------------
    GetUserData = -1

'--------------------------------------------------------------------
'   Open a database connection
'--------------------------------------------------------------------
    objCon.Open DB_Connect
    
'--------------------------------------------------------------------------------
'   Get the person_ref_num based on the EMA_Username passed in
'--------------------------------------------------------------------------------
    strSql = "SELECT person_ref_num " & _
             "  FROM femis_user " & _
             " WHERE exercise_num = " & lngExerciseNum & " " & _
             "   AND user_code = '" & EMA_Username & "' "
    
    rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
    
    lngPersonRefNum = rsList("person_ref_num")
    
    rsList.Close
    
'--------------------------------------------------------------------------------
'   Get the counties and groups that the person is a member of
'--------------------------------------------------------------------------------
    strSql = "SELECT county_id, group_id " & _
             "  FROM ea_person " & _
             " WHERE exercise_num = " & lngExerciseNum & " " & _
             "   AND person_ref_num = " & lngPersonRefNum & " "
    
    rsCtyGrp.Open strSql, objCon, adOpenStatic, adLockReadOnly
    Do While Not rsCtyGrp.EOF
    
        strCountyID = rsCtyGrp("county_id")
        varGroupID = rsCtyGrp("group_id")
        If IsNull(varGroupID) Then
            lGroupID = 0
        Else
            lGroupID = CLng(varGroupID)
        End If
        
        '--------------------------------------------------------------------------------
        '   Get the persons that are also members of the county and group pair.
        '--------------------------------------------------------------------------------
        strSql = "SELECT person_ref_num " & _
                 "  FROM ea_person " & _
                 " WHERE exercise_num = " & lngExerciseNum & " " & _
                 "   AND county_id = " & strCountyID & " " & _
                 "   AND person_ref_num <> " & lngPersonRefNum & " "
                 
        If lGroupID Then
            strSql = strSql & " and group_id = " & lGroupID & " "
        Else
            strSql = strSql & " and group_id is null "
        End If

        rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
        Do While Not rsList.EOF
        
            '--------------------------------------------------------------------------------
            '   Get the information on the person
            '--------------------------------------------------------------------------------
            strSql = "SELECT DISTINCT person_ref_num, exercise_num, name_last, name_first, name_second, home_phone, " & _
                     "       work_phone, person_email_address, person_fax_phone, person_beeper_phone, person_work_hours, " & _
                     "       person_status, person_comment, personal_id_num, person_nick_name, state_code, city_name, " & _
                     "       street_address1, street_address2, zip_code, home_state_code, home_city_name, home_street_address1, " & _
                     "       home_street_address2, home_zip_code, home_zone, work_zone, cel_phone, email_address2, " & _
                     "       web_address, agency_code, dept_code, wk_position_id " & _
                     "  FROM person " & _
                     " WHERE exercise_num = 0 " & _
                     "   AND person_ref_num = " & rsList("person_ref_num") & " "
            
            rsPerson.Open strSql, objCon, adOpenStatic, adLockReadOnly
            
            '--------------------------------------------------------------------------------
            '   Setup the record set that will be passed back.  Only do this once.
            '--------------------------------------------------------------------------------
            If bDoIt Then
                bDoIt = False
                
                For lCount = 0 To (rsPerson.Fields.Count - 1)
                    If rsPerson.Fields(lCount).Type = adNumeric Then
                        rsUser.Fields.Append rsPerson.Fields(lCount).Name, adBigInt, rsPerson.Fields(lCount).DefinedSize, adFldUpdatable + adFldIsNullable
                    Else
                        rsUser.Fields.Append rsPerson.Fields(lCount).Name, rsPerson.Fields(lCount).Type, rsPerson.Fields(lCount).DefinedSize, adFldUpdatable + adFldIsNullable
                    End If
                Next lCount
                
                rsUser.Open
            End If

            '--------------------------------------------------------------------
            '   See if the person ref num already exists.  If it does, don't add
            '   it again.
            '--------------------------------------------------------------------
            bExists = False
            If rsUser.RecordCount Then
                '--------------------------------------------------------------------
                '   Store the current location so we can get back.
                '--------------------------------------------------------------------
                varBookmark = rsUser.Bookmark
                
                '--------------------------------------------------------------------
                '   Move to the first record and check until we find a match or
                '   reach the end.
                '--------------------------------------------------------------------
                rsUser.MoveFirst
                Do While Not rsUser.EOF And Not (bExists)
                    lCurrent = rsUser.Fields(0).Value
                    If lCurrent = rsList("person_ref_num") Then
                        bExists = True
                        Exit Do
                    End If
                    rsUser.MoveNext
                Loop
                '--------------------------------------------------------------------
                '   Get back to where we started.
                '--------------------------------------------------------------------
                rsUser.Bookmark = varBookmark
            End If
            
            If Not (bExists) Then
                '--------------------------------------------------------------------------------
                '   Add the record to the recordset
                '--------------------------------------------------------------------------------
                rsUser.AddNew
                For lCount = 0 To (rsPerson.Fields.Count - 1)
                    rsUser.Fields(lCount).Value = rsPerson.Fields(lCount).Value
                Next lCount
                rsUser.Update

            End If
            
            rsPerson.Close
            rsList.MoveNext
        Loop
        rsList.Close
        rsCtyGrp.MoveNext
        
    Loop
    rsCtyGrp.Close
    
'--------------------------------------------------------------------
'   If we get here, the function successful.  This is the ONLY place that can occur.
'--------------------------------------------------------------------------------
    GetUserData = 0
    Exit Function

GetUserData_ERROR:
    'If we get here, we automatically return failure
End Function

Public Function GetConditionalDataNotif(EMA_Username As String, _
                                        Notification_ID As Long, _
                                        rsConditional As ADODB.Recordset) As Integer
' Author:   Kevin Dorow
' Created:  2/14/20001
' Purpose:  This function will retrieve a recordset containing
'           updated Conditional Data information based the EMA_Username and
'           the Notification_ID passed in.
' Inputs:   EMA_Username (EMAdvantage Username)
'           Notificaiton_ID (Established by EarthAlert P/S Notification filter)
' Outputs:  The function will return an integer value that will indicate
'           success or failure of the function call:
'               -1  indicates that the function failed
'               0   indicates that the function was successful
'
'           rsConditional is an output parameter that should be empty when passed
'           in and will contain a recordset containing the updated Conditional
'           inforamtion when the function completes

    Dim strSql                  As String
    Dim strID                   As String
    Dim strType                 As String
    Dim lngExerciseNum          As Long
    Dim lPersonRefNum           As Long
    Dim lCount                  As Long
    Dim lFieldCount             As Long
    Dim objCon                  As New ADODB.Connection
    Dim rsList                  As New ADODB.Recordset
    Dim bDoIt                   As Boolean
    Dim lngSubClassID           As Long
    Dim lngClassID              As Long
    Dim strSubClass             As String
    Dim rsTemp                  As New ADODB.Recordset
    
    On Error GoTo GetConditionalDataNotif_ERROR
    
    GetConditionalDataNotif = -1
    
    lngExerciseNum = 0
    lFieldCount = 20

    ' open a connection to the Oracle Database
    objCon.Open DB_Connect
    bDoIt = True

'--------------------------------------------------------------------
'   Assume the function return failure
'--------------------------------------------------------------------
    GetConditionalDataNotif = -1
    
'--------------------------------------------------------------------
'   Get the person_ref_num for the user.
'--------------------------------------------------------------------
    strSql = "SELECT person_ref_num " & _
             "  FROM femis_user " & _
             " WHERE exercise_num = " & lngExerciseNum & " " & _
             "   AND user_code = '" & EMA_Username & "' "
    
    rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
    
    lPersonRefNum = rsList("person_ref_num")
    
    rsList.Close
    
'--------------------------------------------------------------------
'   Get the record from the notification log table
'--------------------------------------------------------------------
    strSql = "SELECT msg_name, parm0, parm1, parm2, parm23, parm25 " & _
            "  FROM ea_notification_log " & _
            " WHERE notification_id = " & Notification_ID & " "
    
    rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly

    If rsList("msg_name") = "User_Defined_Cond_Msgs" Then
        strID = rsList("parm2")
        strType = "User_Defined_Cond_Msgs"
    Else
        If rsList("parm25") = "Damage_Assessment" Or rsList("parm25") = "Damage Assessment" Then
            strID = rsList("parm23")
            strType = "Damage_Assessment"
        ElseIf rsList("parm25") = "Traffic_Conditions" Or rsList("parm25") = "Traffic Conditions" Then
            strID = rsList("parm23")
            strType = "Traffic_Conditions"
        ElseIf rsList("parm25") = "Weather_Conditions" Or rsList("parm25") = "Weather Conditions" Then
            strID = rsList("parm23")
            strType = "Weather_Conditions"
        End If
    End If
    
    rsList.Close
    
    '--------------------------------------------------------------------------------
    '   Prepare the recordset for the data
    '--------------------------------------------------------------------------------
    For lCount = 0 To (rsList.Fields.Count - 1)
        If rsList.Fields(lCount).Type = adNumeric Then
            rsConditional.Fields.Append rsList.Fields(lCount).Name, adBigInt, rsList.Fields(lCount).DefinedSize, adFldUpdatable + adFldIsNullable
        Else
            rsConditional.Fields.Append rsList.Fields(lCount).Name, rsList.Fields(lCount).Type, rsList.Fields(lCount).DefinedSize, adFldUpdatable + adFldIsNullable
        End If
    Next lCount
    
    '--------------------------------------------------------------------
    '   Increase the size for a field to store the type of condition.
    '   This will tell if it's Traffic, User Defined, or Weather.
    '--------------------------------------------------------------------
    rsConditional.Fields.Append MESSAGE_CLASS, adVarChar, MESSAGE_CLASS_DEFINED_SIZE, adFldUpdatable + adFldIsNullable
    rsConditional.Fields.Append "Message Subclass", adVarChar, MESSAGE_CLASS_DEFINED_SIZE, adFldUpdatable + adFldIsNullable
    
    '--------------------------------------------------------------------
    '   Increase the size for each possible attribute.  There are 5
    '   possible and we store the value, name and type for each.
    '--------------------------------------------------------------------
    For lCount = OBJ_ATTR_START To OBJ_ATTR_MAX
            rsConditional.Fields.Append MESSAGE_VALUE & lCount, adVarChar, MESSAGE_VALUE_DEFINED_SIZE, adFldUpdatable + adFldIsNullable
            rsConditional.Fields.Append MESSAGE_NAME & lCount, adVarChar, MESSAGE_NAME_DEFINED_SIZE, adFldUpdatable + adFldIsNullable
            rsConditional.Fields.Append MESSAGE_DATATYPE & lCount, adVarChar, MESSAGE_DATATYPE_DEFINED_SIZE, adFldUpdatable + adFldIsNullable
    Next lCount
    
    rsConditional.Open
    
'--------------------------------------------------------------------------------
'   Get the record from user_defined_object table
'--------------------------------------------------------------------------------
    strSql = "SELECT udo.object_id, udo.object_name, udo.object_notes, geo.femis_point_x, geo.femis_point_y, udo.class_id, udo.subclass_id " & _
            "  FROM user_defined_object udo, geo_object geo " & _
            " WHERE udo.geo_object_id = geo.geo_object_id " & _
            "   AND udo.exercise_num = geo.exercise_num " & _
            "   AND udo.exercise_num = " & lngExerciseNum & " " & _
            "   AND udo.object_id = " & strID & " "

    rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
        
    If Not (rsList.EOF) Then
        rsConditional.AddNew

        For lCount = 0 To (rsList.Fields.Count - 3)
            rsConditional.Fields(lCount).Value = rsList.Fields(lCount).Value
        Next lCount
        
        ' store the subclass id
        lngSubClassID = rsList.Fields("subclass_id").Value
        lngClassID = rsList.Fields("class_id").Value
        rsList.Close
        
        rsConditional.Fields(lCount).Value = strType
        ' build a SQL statement to extract the subclass name
        strSql = "SELECT " & _
                    "SUBCLASS_NAME " & _
                 "FROM " & _
                    "OBJECT_SUBCLASS " & _
                 "WHERE " & _
                    "SUBCLASS_ID = " & CStr(lngSubClassID) & " " & _
                 "AND " & _
                    "CLASS_ID = " & lngClassID & " " & _
                 "AND " & _
                    "EXERCISE_NUM = " & lngExerciseNum
        ' return the recordset
        rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
        If Not rsTemp.EOF Then
            ' store the subclass name in the dynamic recordset
            rsConditional.Fields("Message Subclass").Value = rsTemp.Fields("SUBCLASS_NAME").Value
        Else
            rsConditional.Fields("Message Subclass").Value = ""
        End If
        ' close and deallocate the recordset object
        rsTemp.Close
        Set rsTemp = Nothing
        
            
        strSql = "SELECT distinct oa.attr_value, oad.attr_name, oad.attr_type, oa.attr_id " & _
             "  FROM object_attr oa, object_attr_def oad  " & _
             " WHERE oa.attr_id = oad.attr_id " & _
             "   AND oa.exercise_num = oad.exercise_num " & _
             "   AND oa.exercise_num = " & lngExerciseNum & " " & _
             "   AND oa.object_id = " & strID & " " & _
             "   ORDER BY oa.attr_id"
        
        rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
        
        lCount = lCount + 1
        
        Do While Not rsList.EOF
            
            '--------------------------------------------------------------------
            '   Load each value.
            '--------------------------------------------------------------------
            lCount = lCount + 1
            
            rsConditional.Fields(lCount).Value = rsList.Fields(0).Value
            
            lCount = lCount + 1
            rsConditional.Fields(lCount).Value = rsList.Fields(1).Value
            
            lCount = lCount + 1
            rsConditional.Fields(lCount).Value = rsList.Fields(2).Value
            
            rsList.MoveNext
        Loop
    End If
    rsList.Close

'--------------------------------------------------------------------------------
'   If we get here, the function successful.  This is the ONLY place that can occur.
'--------------------------------------------------------------------------------
    GetConditionalDataNotif = 0
    Exit Function
    
GetConditionalDataNotif_ERROR:
    'If we get here, we automatically return failure
End Function

Private Function IsWithinExtents(ByVal pCountyID As Long, _
                                 ByVal pGroupID As Long, _
                                 ByVal XVal As Double, _
                                 ByVal YVal As Double) As Boolean
'=======================================================================
' Returns true if the lat and lon passed in are within the extents of
' the county and group.
'=======================================================================
' 03/16/01, MAF. Creation.
'=======================================================================
    On Error GoTo IsWithinExtents_ERROR
    
    Dim strSql              As String
    Dim dblExtentMinX       As Double
    Dim dblExtentMinY       As Double
    Dim dblExtentMaxX       As Double
    Dim dblExtentMaxY       As Double
    Dim dblMinX             As Double
    Dim dblMinY             As Double
    Dim dblMaxX             As Double
    Dim dblMaxY             As Double
    Dim dblXVal             As Double
    Dim dblYVal             As Double
    Dim rsList              As New ADODB.Recordset
    Dim objCon              As New ADODB.Connection
    
    '----------------------------------------------------------------
    '   Assume false
    '----------------------------------------------------------------
    IsWithinExtents = False
    
    objCon.Open DB_Connect

    
    If (pGroupID <> 0) Then
    '----------------------------------------------------------------
    '   A group is selected, so check the passed point against the
    '   group extents.
    '----------------------------------------------------------------
        strSql = "SELECT extent_min_x, extent_min_y, extent_max_x, extent_max_y " & _
                 "  FROM ea_group" & _
                 " WHERE county_id = " & pCountyID & " " & _
                 "   AND group_id = " & pGroupID
        
        rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
        
    Else
    '----------------------------------------------------------------
    '   A group is NOT selected, so check the passed point against the
    '   count extents.
    '----------------------------------------------------------------
        strSql = "SELECT extent_min_x, extent_min_y, extent_max_x, extent_max_y " & _
                 "  FROM ea_county" & _
                 " WHERE county_id = " & pCountyID
        
        rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
        
    End If
    
    '----------------------------------------------------------------
    '   First, make sure there are not null values.  If there are,
    '   return false.
    '----------------------------------------------------------------
    If IsNull(rsList("extent_min_x")) Or IsNull(rsList("extent_min_y")) Or IsNull(rsList("extent_max_x")) Or IsNull(rsList("extent_max_y")) Then
        Exit Function
    End If
        
    '----------------------------------------------------------------
    '   Get the database values as absolute value so we don't have
    '   to deal with negative numbers.
    '----------------------------------------------------------------
    dblExtentMinX = Abs(CDbl(rsList("extent_min_x")))
    dblExtentMinY = Abs(CDbl(rsList("extent_min_y")))
    dblExtentMaxX = Abs(CDbl(rsList("extent_max_x")))
    dblExtentMaxY = Abs(CDbl(rsList("extent_max_y")))
    rsList.Close

    '----------------------------------------------------------------
    '   Same for the passed values.
    '----------------------------------------------------------------
    dblXVal = Abs(XVal)
    dblYVal = Abs(YVal)
    
    '----------------------------------------------------------------
    '   Now move the values into variables so we know that we really
    '   have the MinX in the right variable, MaxX in the right
    '   variable, etc.
    '----------------------------------------------------------------
    dblMinX = IIf(dblExtentMinX < dblExtentMaxX, dblExtentMinX, dblExtentMaxX)
    dblMinY = IIf(dblExtentMinY < dblExtentMaxY, dblExtentMinY, dblExtentMaxY)
    dblMaxX = IIf(dblExtentMinX < dblExtentMaxX, dblExtentMaxY, dblExtentMinX)
    dblMaxY = IIf(dblExtentMinY < dblExtentMaxY, dblExtentMaxX, dblExtentMinY)
    
    '----------------------------------------------------------------
    '   determine if the point is within the extents
    '----------------------------------------------------------------
    If (dblMinX <= dblXVal) And (dblXVal <= dblMaxX) And (dblMinY <= dblYVal) And (dblYVal <= dblMaxY) Then
        IsWithinExtents = True
        Exit Function
    End If
    
IsWithinExtents_ERROR:
    '   If we got here, the function returns false.
End Function

Public Function GetConditionalData(EMA_Username As String, _
                                   rsConditional As ADODB.Recordset) As Integer
' Author:   Kevin Dorow
' Created:  2/14/20001
' Purpose:  This function will retrieve a recordset containing
'           updated Conditional Data information based the EMA_Username.
' Inputs:   EMA_Username (EMAdvantage Username)
' Outputs:  The function will return an integer value that will indicate
'           success or failure of the function call:
'               -1  indicates that the function failed
'               0   indicates that the function was successful
'
'           rsConditional is an output parameter that should be empty when passed
'           in and will contain a recordset containing the updated Conditional
'           inforamtion when the function completes

    Dim stSQL               As String
    Dim stClassID           As String
    Dim stSubclassID        As String
    Dim varGroupID          As Variant
    Dim varBookmark         As Variant
    Dim lTrafficID          As Long
    Dim lUserCondID         As Long
    Dim lWeatherID          As Long
    Dim lDamageID           As Long
    Dim lPersonRefNum       As Long
    Dim lCount              As Long
    Dim lObjectID           As Long
    Dim lCountyID           As Long
    Dim lGroupID            As Long
    Dim lCurrent            As Long
    Dim lngExerciseNum      As Long
    Dim dblPointX           As Double
    Dim dblPointY           As Double
    Dim bDoIt               As Boolean
    Dim bExists             As Boolean
    Dim rsList              As New ADODB.Recordset
    Dim rsObj               As New ADODB.Recordset
    Dim rsAttr              As New ADODB.Recordset
    Dim objCon              As New ADODB.Connection
    Dim strSql              As String
    Dim stRecordType        As String
    Dim rsTemp              As New ADODB.Recordset
        
    On Error GoTo GetConditionalData_ERROR
    
    lngExerciseNum = 0

    ' open a connection to the Oracle Database
    objCon.Open DB_Connect
    bDoIt = True

        Debug.Print "End of run"
        Debug.Print ""
        Debug.Print ""
'--------------------------------------------------------------------
'   Assume the function return failure
'--------------------------------------------------------------------
    GetConditionalData = -1
    
'--------------------------------------------------------------------
'   Get the person_ref_num for the user.
'--------------------------------------------------------------------
    strSql = "SELECT person_ref_num " & _
             "  FROM femis_user " & _
             " WHERE exercise_num = " & lngExerciseNum & " " & _
             "   AND user_code = '" & EMA_Username & "' "
    
    rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
    
    lPersonRefNum = rsList("person_ref_num")
    
    rsList.Close
    
'--------------------------------------------------------------------
'   Get the class_id for Traffic Conditions
'--------------------------------------------------------------------
    strSql = "SELECT class_id " & _
             "  FROM object_class " & _
             " WHERE class_name = '" & COND_TRAFFIC & "' "
             
    rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
    
    lTrafficID = rsList("class_id")
    
    rsList.Close
    
'--------------------------------------------------------------------
'   Get the class_id from User Defined Conditions
'--------------------------------------------------------------------
    strSql = "SELECT class_id " & _
             "  FROM object_class " & _
             " WHERE class_name = '" & COND_USER_DEFINED & "' "

    rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
    
    lUserCondID = rsList("class_id")
    
    rsList.Close
    
'--------------------------------------------------------------------
'   Get the class_id for Weather Conditions
'--------------------------------------------------------------------
    strSql = "SELECT class_id " & _
             "  FROM object_class " & _
             " WHERE class_name = '" & COND_WEATHER & "' "
    
    rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
    
    lWeatherID = rsList("class_id")
    
    rsList.Close
    
'--------------------------------------------------------------------
'   Get the class_id for Damage Assessment
'--------------------------------------------------------------------
    strSql = "SELECT class_id " & _
             "  FROM object_class " & _
             " WHERE class_name = '" & COND_DAMAGE_ASSESSMENT & "' "
    
    rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
    
    lDamageID = rsList("class_id")
    
    rsList.Close
    
'--------------------------------------------------------------------
'   Get class and subclass set that the user is subscribed to.
'--------------------------------------------------------------------
    strSql = "SELECT c.class_id, c.subclass_id, c.county_id, c.group_id " & _
            "  FROM ea_person p, ea_condition c " & _
            " WHERE p.county_id = c.county_id " & _
            "   AND p.county_id = c.county_id " & _
            "   AND ((p.group_id = c.group_id) or ((p.group_id is null) and (c.group_id is null))) " & _
            "   AND c.exercise_num = " & lngExerciseNum & " " & _
            "   AND p.person_ref_num = " & lPersonRefNum & " "
            
    rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly

    Do While Not rsList.EOF
    
        stClassID = rsList("class_id")
        stSubclassID = rsList("subclass_id")
        lCountyID = rsList("county_id")
        varGroupID = rsList("group_id")
        If IsNull(varGroupID) Then
            lGroupID = 0
        Else
            lGroupID = CLng(varGroupID)
        End If
        
        Debug.Print "Checking Class:" & stClassID & "  Subclass:" & stSubclassID & "  Cty: " & lCountyID; "  Grp: " & lGroupID
        
        '--------------------------------------------------------------------
        '   Now get the object id and name based on the class and subclass
        '--------------------------------------------------------------------
        strSql = "SELECT udo.object_id, udo.object_name, udo.object_notes, geo.femis_point_x, geo.femis_point_y " & _
                "  FROM user_defined_object udo, geo_object geo " & _
                " WHERE udo.geo_object_id = geo.geo_object_id " & _
                "   AND udo.exercise_num = geo.exercise_num " & _
                "   AND udo.exercise_num = " & lngExerciseNum & " " & _
                "   AND udo.class_id = " & stClassID & " " & _
                "   AND udo.subclass_id = " & stSubclassID & " "

        rsObj.Open strSql, objCon, adOpenStatic, adLockReadOnly
        
        '--------------------------------------------------------------------
        '   Now make room in the record set that was passed in.  We only do
        '   this the first time through.
        '--------------------------------------------------------------------
        If bDoIt Then
            bDoIt = False
            '--------------------------------------------------------------------
            '   First make it large enough for the selected fields.
            '--------------------------------------------------------------------
            For lCount = 0 To (rsObj.Fields.Count - 1)
                If rsObj.Fields(lCount).Type = adNumeric Then
                    rsConditional.Fields.Append rsObj.Fields(lCount).Name, adBigInt, rsObj.Fields(lCount).DefinedSize, adFldUpdatable + adFldIsNullable
                Else
                    rsConditional.Fields.Append rsObj.Fields(lCount).Name, rsObj.Fields(lCount).Type, rsObj.Fields(lCount).DefinedSize, adFldUpdatable + adFldIsNullable
                End If
            Next lCount
            
            '--------------------------------------------------------------------
            '   Increase the size for a field to store the type of condition.
            '   This will tell if it's Traffic, User Defined, or Weather.
            '--------------------------------------------------------------------
            rsConditional.Fields.Append MESSAGE_CLASS, adVarChar, MESSAGE_CLASS_DEFINED_SIZE, adFldUpdatable + adFldIsNullable
            rsConditional.Fields.Append "Message Subclass", adVarChar, MESSAGE_CLASS_DEFINED_SIZE, adFldUpdatable + adFldIsNullable
            
            '--------------------------------------------------------------------
            '   Increase the size for each possible attribute.  There are 5
            '   possible and we store the value, name and type for each.
            '--------------------------------------------------------------------
            For lCount = OBJ_ATTR_START To OBJ_ATTR_MAX
                    rsConditional.Fields.Append MESSAGE_VALUE & lCount, adVarChar, MESSAGE_VALUE_DEFINED_SIZE, adFldUpdatable + adFldIsNullable
                    rsConditional.Fields.Append MESSAGE_NAME & lCount, adVarChar, MESSAGE_NAME_DEFINED_SIZE, adFldUpdatable + adFldIsNullable
                    rsConditional.Fields.Append MESSAGE_DATATYPE & lCount, adVarChar, MESSAGE_DATATYPE_DEFINED_SIZE, adFldUpdatable + adFldIsNullable
            Next lCount
            
            rsConditional.Open , , adOpenDynamic

        End If

        '--------------------------------------------------------------------
        '   Now we actually load the passed record set with data.
        '--------------------------------------------------------------------
        Do While Not rsObj.EOF
            Debug.Print "  Found Object :" & rsObj("object_name")
            
            lObjectID = rsObj("object_id")
                
            '--------------------------------------------------------------------
            '   See if the object ID already exists.  If it does, don't add
            '   it again.
            '--------------------------------------------------------------------
            bExists = False
            If rsConditional.RecordCount Then
                '--------------------------------------------------------------------
                '   Store the current location so we can get back.
                '--------------------------------------------------------------------
                varBookmark = rsConditional.Bookmark
                
                '--------------------------------------------------------------------
                '   Move to the first record and check until we find a match or
                '   reach the end.
                '--------------------------------------------------------------------
                rsConditional.MoveFirst
                Do While Not rsConditional.EOF And Not (bExists)
                    lCurrent = rsConditional.Fields(0).Value
                    If lCurrent = lObjectID Then
                        bExists = True
                        Exit Do
                    End If
                    rsConditional.MoveNext
                Loop
                '--------------------------------------------------------------------
                '   Get back to where we started.
                '--------------------------------------------------------------------
                rsConditional.Bookmark = varBookmark
            End If
            
            If Not (bExists) Then
                '--------------------------------------------------------------------
                '   Check to see if the location is within the extents of the county
                '   and group.
                '--------------------------------------------------------------------
                dblPointY = CDbl(rsObj("femis_point_y"))
                dblPointX = CDbl(rsObj("femis_point_x"))
                If IsWithinExtents(lCountyID, lGroupID, dblPointX, dblPointY) Then
                    Debug.Print "    Adding to RecordSet County: " & lCountyID & "   Group:" & lGroupID
                    '--------------------------------------------------------------------
                    '   Now we actually load the passed record set with data.
                    '--------------------------------------------------------------------
                    '   Add a new record and load in the object_id and object name
                    '--------------------------------------------------------------------
                    rsConditional.AddNew
                    For lCount = 0 To (rsObj.Fields.Count - 1)
                        rsConditional.Fields(lCount).Value = rsObj.Fields(lCount).Value
                    Next lCount
                                    
                    '--------------------------------------------------------------------
                    '   Set the record type
                    '--------------------------------------------------------------------
                    If stClassID = lTrafficID Then
                        stRecordType = COND_TRAFFIC
                    ElseIf stClassID = lUserCondID Then
                        stRecordType = COND_USER_DEFINED
                    ElseIf stClassID = lWeatherID Then
                        stRecordType = COND_WEATHER
                    ElseIf stClassID = lDamageID Then
                        stRecordType = COND_DAMAGE_ASSESSMENT
                    Else
                        GoTo GetConditionalData_ERROR
                    End If
                    rsConditional.Fields("Message Class").Value = stRecordType
                    ' now we need to extract the subclass type
                    strSql = "SELECT " & _
                                "SUBCLASS_NAME " & _
                             "FROM " & _
                                "OBJECT_SUBCLASS " & _
                             "WHERE " & _
                                "SUBCLASS_ID = " & stSubclassID & " " & _
                             "AND " & _
                                "CLASS_ID = " & stClassID & " " & _
                             "AND " & _
                                "EXERCISE_NUM = " & lngExerciseNum
                    ' return the recordset
                    rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
                    If Not rsTemp.EOF Then
                        ' store the subclass name in the dynamic recordset
                        rsConditional.Fields("Message Subclass").Value = rsTemp.Fields("SUBCLASS_NAME").Value
                    Else
                        rsConditional.Fields("Message Subclass").Value = ""
                    End If
                    ' close and deallocate the recordset object
                    rsTemp.Close
                    Set rsTemp = Nothing
                    
                    '--------------------------------------------------------------------
                    '   Now get the attribute information for the current object
                    '--------------------------------------------------------------------
                    strSql = "SELECT distinct oa.attr_value, oad.attr_name, oad.attr_type " & _
                             "  FROM object_attr oa, object_attr_def oad  " & _
                             " WHERE oa.attr_id = oad.attr_id " & _
                             "   AND oa.exercise_num = oad.exercise_num " & _
                             "   AND oa.exercise_num = " & lngExerciseNum & " " & _
                             "   AND oa.object_id = " & lObjectID & " "
                    
                    rsAttr.Open strSql, objCon, adOpenStatic, adLockReadOnly
                    
                    lCount = lCount + 1
                    
                    Do While Not rsAttr.EOF
                        
                        '--------------------------------------------------------------------
                        '   Load each value.
                        '--------------------------------------------------------------------
                        lCount = lCount + 1
                        rsConditional.Fields(lCount).Value = rsAttr.Fields(0).Value
                        
                        lCount = lCount + 1
                        rsConditional.Fields(lCount).Value = rsAttr.Fields(1).Value
                        
                        lCount = lCount + 1
                        rsConditional.Fields(lCount).Value = rsAttr.Fields(2).Value
                        
                        rsAttr.MoveNext
                    Loop
                    rsAttr.Close
                    
                End If
                
            End If
            rsObj.MoveNext
            
        Loop
        rsObj.Close
        rsList.MoveNext
        
    Loop
    rsList.Close
    ' check to see if there are any records to update
    If Not rsConditional.EOF Then
        rsConditional.Update
    End If
    
'--------------------------------------------------------------------------------
'   If we get here, the function successful.  This is the ONLY place that can occur.
'--------------------------------------------------------------------------------
    GetConditionalData = 0
    Exit Function

GetConditionalData_ERROR:
    'If we get here, we automatically return failure
End Function

Public Function SetConditionalData(EMA_Username As String, _
                                   MsgTitle As String, _
                                   ByVal MsgType As String, _
                                   MsgSubtype As String, _
                                   Optional MsgText As String = "", _
                                   Optional MsgAttr1 As String = "", _
                                   Optional MsgAttr2 As String = "", _
                                   Optional MsgAttr3 As String = "", _
                                   Optional MsgAttr4 As String = "", _
                                   Optional MsgAttr5 As String = "", _
                                   Optional GPS_Latitude As Double = 0, _
                                   Optional GPS_Longitude As Double = 0) As Integer

'-------------------------------------------------------------------------------------
'   Attributes should be passed per the following table.
'-------------------------------------------------------------------------------------
'Class      Damage_Assessment   Traffic_Conditions  User_Defined    Weather_Conditions
'-------------------------------------------------------------------------------------
'MsgAttr1   Applicant           Emergency                           Emergency
'MsgAttr2   County              Priority                            Priority
'MsgAttr3   Damage Amount       Destination                         Destination
'MsgAttr4   Insurance Amount    Respond             Respond         Respond
'MsgAttr5   Percent Complete
'-------------------------------------------------------------------------------------
' Outputs:  The function will return an integer value that will indicate
'           success or failure of the function call:
'               -1  indicates that the function failed
'               0   indicates that the function was successful

    Dim bSuccess     As Boolean
    Dim lngGeoObject As Long
    
    SetConditionalData = -1

'------------------------------------------------------------------------------------
'   If bad points are passed in, normalize them to 0.
'------------------------------------------------------------------------------------
    If GPS_Latitude = -999.999 Or GPS_Longitude = -999.999 Then
        GPS_Latitude = 0
        GPS_Longitude = 0
    End If

'------------------------------------------------------------------------------------
'   Add the geo object if there's a latitude and longitude
'------------------------------------------------------------------------------------
    'If GPS_Latitude And GPS_Longitude Then
        lngGeoObject = CreatePointObject(MsgTitle, MsgType, MsgSubtype, GPS_Longitude, GPS_Latitude)
    'End If
    
'------------------------------------------------------------------------------------
'   Add the user defined object and attributes
'------------------------------------------------------------------------------------
    bSuccess = AddItem(EMA_Username, lngGeoObject, MsgTitle, MsgType, MsgSubtype, MsgText, MsgAttr1, MsgAttr2, MsgAttr3, MsgAttr4, MsgAttr5, GPS_Latitude, GPS_Longitude)
    If bSuccess Then
        SetConditionalData = 0
    End If

End Function
Public Function SetConditionalPointData(EMA_Username As String, _
                                   MsgTitle As String, _
                                   ByVal MsgType As String, _
                                   MsgSubtype As String, _
                                   MsgText As String, _
                                   MsgName1 As String, _
                                    MsgAttr1 As String, _
                                    MsgName2 As String, _
                                    MsgAttr2 As String, _
                                    MsgName3 As String, _
                                    MsgAttr3 As String, _
                                    MsgName4 As String, _
                                    MsgAttr4 As String, _
                                    MsgName5 As String, _
                                    MsgAttr5 As String, _
                                    GPS_Latitude As Double, _
                                    GPS_Longitude As Double) As Boolean

'-------------------------------------------------------------------------------------
'   Made it generic enough to pass in the attribute name and values
'   Attributes should be passed per the following table.
'
' Outputs:  The function will return an integer value that will indicate
'           success or failure of the function call:
'               -1  indicates that the function failed
'               0   indicates that the function was successful

    Dim bSuccess     As Boolean
    Dim lngGeoObject As Long
    
    SetConditionalPointData = False

'With the addition of HCO, we are now setting the public variable user name

    gstUCurUser = EMA_Username
    
   
'------------------------------------------------------------------------------------
'   Add the geo object if there's a latitude and longitude
'------------------------------------------------------------------------------------
    'If GPS_Latitude And GPS_Longitude Then
        lngGeoObject = CreatePointObject(MsgTitle, MsgType, MsgSubtype, GPS_Longitude, GPS_Latitude)
    'End If
    
'------------------------------------------------------------------------------------
'   Add the user defined object and attributes
'------------------------------------------------------------------------------------
    bSuccess = AddItem(EMA_Username, lngGeoObject, MsgTitle, MsgType, MsgSubtype, MsgText, MsgAttr1, MsgAttr2, MsgAttr3, MsgAttr4, MsgAttr5, GPS_Latitude, GPS_Longitude, MsgName1, MsgName2, MsgName3, MsgName4, MsgName5)
    If bSuccess Then
        SetConditionalPointData = True
    End If

End Function


Private Function AddItem(EMA_Username As String, _
                         lngGeoObject As Long, _
                         MsgTitle As String, _
                         MsgType As String, _
                         MsgSubtype As String, _
                         MsgText As String, _
                         MsgAttr1 As String, _
                         MsgAttr2 As String, _
                         MsgAttr3 As String, _
                         MsgAttr4 As String, _
                         MsgAttr5 As String, _
                         GPS_Latitude As Double, _
                         GPS_Longitude As Double, _
                         Optional MsgName1 As String = "", _
                         Optional MsgName2 As String = "", _
                         Optional MsgName3 As String = "", _
                         Optional MsgName4 As String = "", _
                         Optional MsgName5 As String = "") As Boolean


Dim strSql As String
Dim rspoint As Recordset
Dim lRecordID As Long
Dim ssSource As Recordset
Dim vtimenow As Date
Dim stTimeNow As String
Dim vsPerson As Variant
Dim lsprRows As Long
Dim transmitdata As String
Dim lAttrId As Long
Dim stShared As String
Dim stNotify As String


    Dim lngExercise         As Long
    Dim lPersonRefNum       As Long
    Dim lngClassID          As Long
    Dim lngSubClassID       As Long
    Dim lngObjAttrID        As Long
    Dim objCon              As New ADODB.Connection
    Dim rsList              As New ADODB.Recordset
    Dim strPersonName       As String
    Dim intSuccess          As Integer
    Dim lngLinkFlags        As Long
    Dim lngULink            As Long
    Dim dXmitDate1          As Date
    Dim strRepID1           As String
    Dim dXmitDate2          As Date
    Dim strRepID2           As String
    Dim nsDDParm            As NS_DD_PARAMS
    Dim strFormTitle        As String
    Dim rsTemp              As New ADODB.Recordset
    
    On Error GoTo ErrorHandler

    AddItem = False
    '----------------------------------------------------------------------------------
    ' open a connection to the Oracle Database
    '----------------------------------------------------------------------------------
    objCon.Open DB_Connect
    
    lngExercise = gsEID
    
    '----------------------------------------------------------------------------------
    ' Get the sequence number for the user defined object
    '----------------------------------------------------------------------------------
    strSql = "SELECT OBJECT_ID.NEXTVAL FROM DUAL"
    rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
    If (rsList.EOF) Then
        rsList.Close
        GoTo ErrorHandler
    Else
        lRecordID = CLng(rsList("NEXTVAL"))
        rsList.Close
    End If
   
    stTimeNow = stTO_DATE(CStr(vZuluNow))
    
'--------------------------------------------------------------------
'   Get the person_ref_num for the user.
'   Not exercise dependent
'--------------------------------------------------------------------
    strSql = "SELECT person_ref_num " & _
             "  FROM femis_user " & _
             " WHERE user_code = '" & EMA_Username & "' "
    
    rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
    
    lPersonRefNum = rsList("person_ref_num")
    
    rsList.Close
    
'--------------------------------------------------------------------
'   Get the name of the user passed in.
'--------------------------------------------------------------------
    strSql = "SELECT name_last, name_first, name_second " & _
             "  FROM PERSON " & _
             " WHERE person_ref_num=" & lPersonRefNum & _
             "   AND exercise_num=" & lngExercise & " "
                                          
    rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
    
    If (rsList.EOF) Then
        vsPerson = vbNullString
    Else
        vsPerson = Trim$(Trim$(rsList("name_last") & vbNullString) & ", " & _
                                   Trim$(rsList("name_first") & vbNullString) & " " & _
                                   Trim$(rsList("name_second") & vbNullString))
    End If
    
    rsList.Close
    
'--------------------------------------------------------------------
'   Get the class
'--------------------------------------------------------------------
    strSql = "SELECT class_id " & _
             "  FROM OBJECT_CLASS " & _
             " WHERE class_name='" & MsgType & "' " & _
             "   AND exercise_num = " & lngExercise
    'strSql = "Select class_name from object_class where predefined_flag ='N' and external_flag='N'"
    rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
    
    If rsList.EOF Then
        GoTo ErrorHandler
    Else
        lngClassID = rsList("class_id")
    End If
    rsList.Close
        
'--------------------------------------------------------------------
'   Get the subclass
'--------------------------------------------------------------------
    strSql = "SELECT subclass_id " & _
             "  FROM OBJECT_SUBCLASS " & _
             " WHERE subclass_name='" & MsgSubtype & "' " & _
             " AND class_id = " & lngClassID & _
             "   AND exercise_num = " & lngExercise
          
    rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
    If (rsList.EOF) Then
        GoTo ErrorHandler
    Else
        lngSubClassID = rsList("subclass_id")
    End If
    rsList.Close
    
    transmitdata = False
    'stShared = " null " Changed this per Dave's suggestion for Power Grid Project to make all points shared
    stShared = stTimeNow
    
    ' start of added code 5/10/2001 KED
    ' adding check to see if an object with the same message name already exists
    strSql = "SELECT " & _
                "OBJECT_ID, " & _
                "GEO_OBJECT_ID " & _
             "FROM " & _
                "USER_DEFINED_OBJECT " & _
             "WHERE " & _
                "OBJECT_NAME = '" & MsgTitle & "' " & _
             "AND " & _
                "EXERCISE_NUM = " & CStr(lngExercise) & " " & _
             "AND " & _
                "EOC_NAME = '" & gstUCurEOCName & "' " & _
             "AND " & _
                "CLASS_ID = " & lngClassID & " "
                'Query based only on the class since we assume that the point name
                ' will be unique and they may want to update a point with a different subclass to
                ' flag for deletion.
                
                '  & _ "AND " & _
               ' "SUBCLASS_ID = " & lngSubClassID
    ' return the recordset to see if the message already exists
    rsTemp.Open strSql, objCon, adOpenStatic, adLockReadOnly
    ' see if any records were returned
    If Not rsTemp.EOF Then
        ' store the object and geo-object id
        lRecordID = rsTemp.Fields("OBJECT_ID").Value
        lngGeoObject = rsTemp.Fields("GEO_OBJECT_ID").Value
        ' close the recordset
        rsTemp.Close
        Set rsTemp = Nothing
        
        ' here the record already exists -- do an update
        strSql = "UPDATE " & _
                    "USER_DEFINED_OBJECT " & _
                 "SET " & _
                    "OBJECT_DESCRIPTION = '', " & _
                    "OBJECT_NOTES = '" & MsgText & "', " & _
                    "SUBCLASS_ID =" & CStr(lngSubClassID) & "," & _
                    "GEO_OBJECT_ID=" & CStr(lngGeoObject) & "," & _
                    "LAST_CHANGE_DATE = " & stTimeNow & ", " & _
                    "LAST_CHANGE_PERSON = '" & vsPerson & "' " & _
                 "WHERE " & _
                    "OBJECT_ID = " & CStr(lRecordID) & " " & _
                 "AND " & _
                    "EXERCISE_NUM = " & CStr(lngExercise)
        ' execute the update
        objCon.Execute strSql
        
        ' now delete all related records from object_attr
        strSql = "DELETE " & _
                 "FROM " & _
                    "OBJECT_ATTR " & _
                 "WHERE " & _
                    "OBJECT_ID = " & lRecordID & " " & _
                 "AND " & _
                    "EXERCISE_NUM = " & lngExercise
        ' execute the delete
        objCon.Execute strSql
        
        stNotify = "update"
           
        
        
        
        
    Else
        rsTemp.Close
        Set rsTemp = Nothing
        ' thes is a new message--do an insert
    
        '--------------------------------------------------------------------
        '   Insert the record
        '--------------------------------------------------------------------
         strSql = "INSERT INTO USER_DEFINED_OBJECT (OBJECT_ID, EXERCISE_NUM, OBJECT_NAME," & _
              " CLASS_ID, SUBCLASS_ID, GEO_OBJECT_ID, OBJECT_DESCRIPTION, EOC_NAME, POC, OBJECT_NOTES, " & _
              " LAST_CHANGE_DATE, LAST_CHANGE_PERSON, XMIT_INIT_DATE)" & _
              " VALUES (" & CStr(lRecordID) & ", " & CStr(lngExercise) & ",'" & MsgTitle & _
              "', " & lngClassID & "," & lngSubClassID & ", " & lngGeoObject & ", '', '" & gstUCurEOCName & "', " & _
              "'', '" & MsgText & "', " & stTimeNow & ", '" & vsPerson & "', " & stShared & ") "
   
        Debug.Print strSql
    
        objCon.Execute strSql
        
        stNotify = "add"
    
    End If
    
    If MsgType = COND_DAMAGE_ASSESSMENT Then
        
        strFormTitle = "Damage Assessment"
        
        '----------------------------------------------------------------------------
        '  Get the attr_id for Applicant
        '----------------------------------------------------------------------------
        strSql = "SELECT attr_id " & _
                 "  FROM object_attr_def " & _
                 " WHERE class_id = " & lngClassID & " " & _
                 "   AND attr_name = 'Applicant' "
                 
        rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
        If (rsList.EOF) Then
            GoTo ErrorHandler
        Else
            lngObjAttrID = CLng(rsList("attr_id"))
        End If
        rsList.Close
        
        strSql = "INSERT INTO object_attr (attr_id, object_id, exercise_num, eoc_name, xmit_init_date, attr_value) " & _
                 " VALUES ( " & lngObjAttrID & ", " & lRecordID & ", " & lngExercise & ", '" & gstUCurEOCName & "', " & stTimeNow & ", '" & MsgAttr1 & "') "
                 
        objCon.Execute strSql
        
        '----------------------------------------------------------------------------
        '  Get the attr_id for County
        '----------------------------------------------------------------------------
        strSql = "SELECT attr_id " & _
                 "  FROM object_attr_def " & _
                 " WHERE class_id = " & lngClassID & " " & _
                 "   AND attr_name = 'County' "
                 
        rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
        If (rsList.EOF) Then
            GoTo ErrorHandler
        Else
            lngObjAttrID = CLng(rsList("attr_id"))
        End If
        rsList.Close
        
        strSql = "INSERT INTO object_attr (attr_id, object_id, exercise_num, eoc_name, xmit_init_date, attr_value) " & _
                 " VALUES ( " & lngObjAttrID & ", " & lRecordID & ", " & lngExercise & ", '" & gstUCurEOCName & "', " & stTimeNow & ", '" & MsgAttr2 & "') "
                 
        objCon.Execute strSql
        
        '----------------------------------------------------------------------------
        '  Get the attr_id for Damage Amount
        '----------------------------------------------------------------------------
        strSql = "SELECT attr_id " & _
                 "  FROM object_attr_def " & _
                 " WHERE class_id = " & lngClassID & " " & _
                 "   AND attr_name = 'Damage_Amt' "
                 
        rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
        If (rsList.EOF) Then
            GoTo ErrorHandler
        Else
            lngObjAttrID = CLng(rsList("attr_id"))
        End If
        rsList.Close
        
        strSql = "INSERT INTO object_attr (attr_id, object_id, exercise_num, eoc_name, xmit_init_date, attr_value) " & _
                 " VALUES ( " & lngObjAttrID & ", " & lRecordID & ", " & lngExercise & ", '" & gstUCurEOCName & "', " & stTimeNow & ", '" & MsgAttr3 & "') "
                 
        objCon.Execute strSql
        
        '----------------------------------------------------------------------------
        '  Get the attr_id for Insurance Amt
        '----------------------------------------------------------------------------
        strSql = "SELECT attr_id " & _
                 "  FROM object_attr_def " & _
                 " WHERE class_id = " & lngClassID & " " & _
                 "   AND attr_name = 'Insurance_Amt' "
                 
        rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
        If (rsList.EOF) Then
            GoTo ErrorHandler
        Else
            lngObjAttrID = CLng(rsList("attr_id"))
        End If
        rsList.Close
        
        strSql = "INSERT INTO object_attr (attr_id, object_id, exercise_num, eoc_name, xmit_init_date, attr_value) " & _
                 " VALUES ( " & lngObjAttrID & ", " & lRecordID & ", " & lngExercise & ", '" & gstUCurEOCName & "', " & stTimeNow & ", '" & MsgAttr4 & "') "
                 
        objCon.Execute strSql
        
        '----------------------------------------------------------------------------
        '  Get the attr_id for Percent Complete
        '----------------------------------------------------------------------------
        strSql = "SELECT attr_id " & _
                 "  FROM object_attr_def " & _
                 " WHERE class_id = " & lngClassID & " " & _
                 "   AND attr_name = 'Percent_Complete' "
                 
        rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
        If (rsList.EOF) Then
            GoTo ErrorHandler
        Else
            lngObjAttrID = CLng(rsList("attr_id"))
        End If
        rsList.Close
        
        strSql = "INSERT INTO object_attr (attr_id, object_id, exercise_num, eoc_name, xmit_init_date, attr_value) " & _
                 " VALUES ( " & lngObjAttrID & ", " & lRecordID & ", " & lngExercise & ", '" & gstUCurEOCName & "', " & stTimeNow & ", '" & MsgAttr5 & "') "
                 
        objCon.Execute strSql
        
    ElseIf MsgType = COND_TRAFFIC Or MsgType = COND_WEATHER Then
    
        If MsgType = COND_TRAFFIC Then
            strFormTitle = "Traffic Conditions"
        Else
            strFormTitle = "Weather Conditions"
        End If
        
        '----------------------------------------------------------------------------
        '  Get the attr_id for Emergency
        '----------------------------------------------------------------------------
        strSql = "SELECT attr_id " & _
                 "  FROM object_attr_def " & _
                 " WHERE class_id = " & lngClassID & " " & _
                 "   AND attr_name = 'Emergency' "
                 
        rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
        If (rsList.EOF) Then
            GoTo ErrorHandler
        Else
            lngObjAttrID = CLng(rsList("attr_id"))
        End If
        rsList.Close
        
        strSql = "INSERT INTO object_attr (attr_id, object_id, exercise_num, eoc_name, xmit_init_date, attr_value) " & _
                 " VALUES ( " & lngObjAttrID & ", " & lRecordID & ", " & lngExercise & ", '" & gstUCurEOCName & "', " & stTimeNow & ", '" & MsgAttr1 & "') "
                 
        objCon.Execute strSql
        
        '----------------------------------------------------------------------------
        '  Get the attr_id for Priority
        '----------------------------------------------------------------------------
        strSql = "SELECT attr_id " & _
                 "  FROM object_attr_def " & _
                 " WHERE class_id = " & lngClassID & " " & _
                 "   AND attr_name = 'Priority' "
                 
        rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
        If (rsList.EOF) Then
            GoTo ErrorHandler
        Else
            lngObjAttrID = CLng(rsList("attr_id"))
        End If
        rsList.Close
        
        strSql = "INSERT INTO object_attr (attr_id, object_id, exercise_num, eoc_name, xmit_init_date, attr_value) " & _
                 " VALUES ( " & lngObjAttrID & ", " & lRecordID & ", " & lngExercise & ", '" & gstUCurEOCName & "', " & stTimeNow & ", '" & MsgAttr2 & "') "
                 
        objCon.Execute strSql
        
        '----------------------------------------------------------------------------
        '  Get the attr_id for Destination
        '----------------------------------------------------------------------------
        strSql = "SELECT attr_id " & _
                 "  FROM object_attr_def " & _
                 " WHERE class_id = " & lngClassID & " " & _
                 "   AND attr_name = 'Destination' "
                 
        rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
        If (rsList.EOF) Then
            GoTo ErrorHandler
        Else
            lngObjAttrID = CLng(rsList("attr_id"))
        End If
        rsList.Close
        
        strSql = "INSERT INTO object_attr (attr_id, object_id, exercise_num, eoc_name, xmit_init_date, attr_value) " & _
                 " VALUES ( " & lngObjAttrID & ", " & lRecordID & ", " & lngExercise & ", '" & gstUCurEOCName & "', " & stTimeNow & ", '" & MsgAttr3 & "') "
                 
        objCon.Execute strSql
        
        '----------------------------------------------------------------------------
        '  Get the attr_id for Respond
        '----------------------------------------------------------------------------
        strSql = "SELECT attr_id " & _
                 "  FROM object_attr_def " & _
                 " WHERE class_id = " & lngClassID & " " & _
                 "   AND attr_name = 'Respond' "
                 
        rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
        If (rsList.EOF) Then
            GoTo ErrorHandler
        Else
            lngObjAttrID = CLng(rsList("attr_id"))
        End If
        rsList.Close
        
        strSql = "INSERT INTO object_attr (attr_id, object_id, exercise_num, eoc_name, xmit_init_date, attr_value) " & _
                 " VALUES ( " & lngObjAttrID & ", " & lRecordID & ", " & lngExercise & ", '" & gstUCurEOCName & "', " & stTimeNow & ", '" & MsgAttr4 & "') "
                 
        objCon.Execute strSql
        
    ElseIf MsgType = COND_USER_DEFINED Then
    
            strFormTitle = "User Defined Conditions"

'        '----------------------------------------------------------------------------
'        '  Get the attr_id for Destination
'        '----------------------------------------------------------------------------
'        strSQL = "SELECT attr_id " & _
'                 "  FROM object_attr_def " & _
'                 " WHERE class_id = " & lngClassID & " " & _
'                 "   AND attr_name = 'Destination' "
'
'        rsList.Open strSQL, objCon, adOpenStatic, adLockReadOnly
'        If (rsList.EOF) Then
'            GoTo ErrorHandler
'        Else
'            lngObjAttrID = CLng(rsList("attr_id"))
'        End If
'        rsList.Close
'
'        strSQL = "INSERT INTO object_attr (attr_id, object_id, exercise_num, eoc_name, xmit_init_date, attr_value) " & _
'                 " VALUES ( " & lngObjAttrID & ", " & lRecordID & ", " & lngExercise & ", '" & gstUCurEOCName & "', " & stTimeNow & ", '" & MsgAttr3 & "') "
'
'        objCon.Execute strSQL
        
        '----------------------------------------------------------------------------
        '  Get the attr_id for Respond
        '----------------------------------------------------------------------------
        strSql = "SELECT attr_id " & _
                 "  FROM object_attr_def " & _
                 " WHERE class_id = " & lngClassID & " " & _
                 "   AND attr_name = 'Respond' "
                 
        rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
        If (rsList.EOF) Then
            GoTo ErrorHandler
        Else
            lngObjAttrID = CLng(rsList("attr_id"))
        End If
        rsList.Close
        
        strSql = "INSERT INTO object_attr (attr_id, object_id, exercise_num, eoc_name, xmit_init_date, attr_value) " & _
                 " VALUES ( " & lngObjAttrID & ", " & lRecordID & ", " & lngExercise & ", '" & gstUCurEOCName & "', " & stTimeNow & ", '" & MsgAttr4 & "') "
                 
        objCon.Execute strSql
        
    Else
        'This was added for the KP_API.dll to add a user defined point - CS
        
        'GoTo ErrorHandler
            strFormTitle = MsgType
'
        '----------------------------------------------------------------------------
        '  Get the attr_id for the generic classnames
        '----------------------------------------------------------------------------
       If MsgName1 <> vbNullString Then
       
        strSql = "SELECT attr_id " & _
                 "  FROM object_attr_def " & _
                 " WHERE class_id = " & lngClassID & " " & _
                 "   AND attr_name = '" & MsgName1 & "'"
                 
                 
        rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
          If (rsList.EOF) Then
               GoTo ErrorHandler
          Else
            lngObjAttrID = CLng(rsList("attr_id"))
          End If
        rsList.Close
        
        strSql = "INSERT INTO object_attr (attr_id, object_id, exercise_num, eoc_name, xmit_init_date, attr_value) " & _
                 " VALUES ( " & lngObjAttrID & ", " & lRecordID & ", " & lngExercise & ", '" & gstUCurEOCName & "', " & stTimeNow & ", '" & MsgAttr1 & "') "
                 
        objCon.Execute strSql
       End If
       If MsgName2 <> vbNullString Then
       
        strSql = "SELECT attr_id " & _
                 "  FROM object_attr_def " & _
                 " WHERE class_id = " & lngClassID & " " & _
                 "   AND attr_name = '" & MsgName2 & "'"
                 
                 
        rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
          If (rsList.EOF) Then
               GoTo ErrorHandler
          Else
            lngObjAttrID = CLng(rsList("attr_id"))
          End If
        rsList.Close
        
        strSql = "INSERT INTO object_attr (attr_id, object_id, exercise_num, eoc_name, xmit_init_date, attr_value) " & _
                 " VALUES ( " & lngObjAttrID & ", " & lRecordID & ", " & lngExercise & ", '" & gstUCurEOCName & "', " & stTimeNow & ", '" & MsgAttr2 & "') "
                 
        objCon.Execute strSql
       End If
       If MsgName3 <> vbNullString Then
       
        strSql = "SELECT attr_id " & _
                 "  FROM object_attr_def " & _
                 " WHERE class_id = " & lngClassID & " " & _
                 "   AND attr_name = '" & MsgName3 & "'"
                 
                 
        rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
          If (rsList.EOF) Then
               GoTo ErrorHandler
          Else
            lngObjAttrID = CLng(rsList("attr_id"))
          End If
        rsList.Close
        
        strSql = "INSERT INTO object_attr (attr_id, object_id, exercise_num, eoc_name, xmit_init_date, attr_value) " & _
                 " VALUES ( " & lngObjAttrID & ", " & lRecordID & ", " & lngExercise & ", '" & gstUCurEOCName & "', " & stTimeNow & ", '" & MsgAttr3 & "') "
                 
        objCon.Execute strSql
       End If
       If MsgName4 <> vbNullString Then
       
        strSql = "SELECT attr_id " & _
                 "  FROM object_attr_def " & _
                 " WHERE class_id = " & lngClassID & " " & _
                 "   AND attr_name = '" & MsgName4 & "'"
                 
                 
        rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
          If (rsList.EOF) Then
               GoTo ErrorHandler
          Else
            lngObjAttrID = CLng(rsList("attr_id"))
          End If
        rsList.Close
        
        strSql = "INSERT INTO object_attr (attr_id, object_id, exercise_num, eoc_name, xmit_init_date, attr_value) " & _
                 " VALUES ( " & lngObjAttrID & ", " & lRecordID & ", " & lngExercise & ", '" & gstUCurEOCName & "', " & stTimeNow & ", '" & MsgAttr4 & "') "
                 
        objCon.Execute strSql
       End If
       If MsgName5 <> vbNullString Then
       
        strSql = "SELECT attr_id " & _
                 "  FROM object_attr_def " & _
                 " WHERE class_id = " & lngClassID & " " & _
                 "   AND attr_name = '" & MsgName5 & "'"
                 
                 
        rsList.Open strSql, objCon, adOpenStatic, adLockReadOnly
          If (rsList.EOF) Then
               GoTo ErrorHandler
          Else
            lngObjAttrID = CLng(rsList("attr_id"))
          End If
        rsList.Close
        
        strSql = "INSERT INTO object_attr (attr_id, object_id, exercise_num, eoc_name, xmit_init_date, attr_value) " & _
                 " VALUES ( " & lngObjAttrID & ", " & lRecordID & ", " & lngExercise & ", '" & gstUCurEOCName & "', " & stTimeNow & ", '" & MsgAttr5 & "') "
                 
        objCon.Execute strSql
       End If
        
    End If
    
'----------------------------------------------------------------------------
'  Send notification that a User Defined Object has been created
'----------------------------------------------------------------------------

    InitNotifGlobals
    giNotifWinMsgNo = 1
    lngLinkFlags = 0
    ' open a server link
    intSuccess = iNSOpenServerLink(gstNotifServerName, gstNotifServerPort, lngLinkFlags)
    
    lngULink = ulNSOpenQLinkX(0, 0, 0, 0)
    strSql = "SELECT " & _
                    "XMIT_INIT_DATE, " & _
                    "REPLICATION_ID " & _
                 "FROM " & _
                    "GEO_OBJECT " & _
                 "WHERE " & _
                    "GEO_OBJECT_ID = " & CStr(lngGeoObject)
        rsList.Open strSql, objCon
        ' store the REPLICATION_ID and XMIT_INIT_DATE in temporary storage
        dXmitDate1 = rsList.Fields("XMIT_INIT_DATE").Value
        strRepID1 = rsList.Fields("REPLICATION_ID").Value
        ' close the recordset
        rsList.Close
        
        
    ' extract the REPLICATION_ID and XMIT_INIT_DATE for USER_DEFINED_OBJECT Table
        strSql = "SELECT " & _
                    "XMIT_INIT_DATE, " & _
                    "REPLICATION_ID " & _
                 "FROM " & _
                    "USER_DEFINED_OBJECT " & _
                 "WHERE " & _
                    "OBJECT_ID = " & CStr(lRecordID)
        rsList.Open strSql, objCon
        ' store the REPLICATION_ID and XMIT_INIT_DATE in temporary storage
        If Not IsNull(rsList.Fields("XMIT_INIT_DATE").Value) Then
            dXmitDate2 = rsList.Fields("XMIT_INIT_DATE").Value
        Else
            dXmitDate2 = Empty
        End If
        strRepID2 = rsList.Fields("REPLICATION_ID").Value
        ' close the recordset
        rsList.Close
   
        
    If stNotify = "update" Then
       'GeoObject Table
       NsSetDDParams nsDDParm, "GEO_OBJECT", strRepID1, dXmitDate1, NOTIF_DD_MOD, _
                gstUListener, NOTIF_DD_SENT
       'Map notification global and normal
       
       NsPostDataChangeMsgDD lngULink, NS_EF_GLOBAL, nsDDParm, NOTIF_CHG_MAP_KP, NOTIF_CHG_TYPE_MOD, _
                    gstUCurDataset, vZuluNow, CStr(lngGeoObject), , MsgType
       NsPostDataChangeMsg lngULink, NS_EF_NORMAL, NOTIF_CHG_MAP_KP, NOTIF_CHG_TYPE_MOD, _
                    gstUCurDataset, vZuluNow, CStr(lngGeoObject), , MsgType
                    
       'User Defined Object Table
       
       NsSetDDParams nsDDParm, "USER_DEFINED_OBJECT", strRepID2, dXmitDate2, NOTIF_DD_MOD, _
                    gstUListener, NOTIF_DD_SENT
       NsSetDDParamsT2 nsDDParm, "GEO_OBJECT", strRepID1, dXmitDate1, NOTIF_DD_MOD


       NsPostDataChangeMsgDD lngULink, NS_EF_GLOBAL, nsDDParm, NOTIF_CHG_KP, NOTIF_CHG_TYPE_MOD, _
                    gstUCurDataset, vZuluNow, CStr(lRecordID), "global", strFormTitle, "shared"
       NsPostDataChangeMsg lngULink, NS_EF_NORMAL, NOTIF_CHG_KP, NOTIF_CHG_TYPE_MOD, _
                    gstUCurDataset, vZuluNow, CStr(lRecordID), "local", strFormTitle, "shared"
    
    
    Else
        'Not specifying that the stNotify is add because we have other predefined classes for PDA.
        
        If GPS_Latitude <> -999.999 Then
            ' set up the parameters to make a notification call for the map data change
            NsSetDDParams nsDDParm, "GEO_OBJECT", strRepID1, dXmitDate1, NOTIF_DD_ADD, gstUListener, _
                          NOTIF_DD_SENT
            ' post a notification about the map data change
            NsPostDataChangeMsgDD lngULink, NS_EF_GLOBAL, nsDDParm, NOTIF_CHG_MAP_KP, NOTIF_CHG_TYPE_ADD, _
                           gstUCurDataset, vZuluNow, CStr(lngGeoObject), , MsgType
                           
            
                           
        End If
    
        
        ' setup the parameters to make a notification call for the db data change
        NsSetDDParams nsDDParm, "USER_DEFINED_OBJECT", strRepID2, dXmitDate2, NOTIF_DD_ADD, _
                        gstUListener, NOTIF_DD_SENT
        ' set up the parameters for the secondary table "GEO_OBJECT"
        NsSetDDParamsT2 nsDDParm, "GEO_OBJECT", strRepID1, dXmitDate1, NOTIF_DD_ADD
        ' make the notification call
        NsPostDataChangeMsgDD lngULink, NS_EF_GLOBAL, nsDDParm, NOTIF_CHG_KP, NOTIF_CHG_TYPE_ADD, _
                        gstUCurDataset, vZuluNow, CStr(lRecordID), "global", strFormTitle
        NsPostDataChangeMsg lngULink, NS_EF_NORMAL, NOTIF_CHG_KP, NOTIF_CHG_TYPE_ADD, _
                        gstUCurDataset, vZuluNow, CStr(lRecordID), "local", strFormTitle
    End If
        
        ' close the notification link to the server
        intSuccess = iNSCloseLink(lngULink)
   
    
'----------------------------------------------------------------------------
'  If we got here, the point was added and the notification was sent.
'----------------------------------------------------------------------------
    AddItem = True

    Exit Function
   
ErrorHandler:
    '
End Function

Function CreatePointObject(ByVal strName As String, _
                              ByVal strObjectType As String, _
                              ByVal strSubType As String, _
                              ByVal dx As Double, _
                              ByVal dy As Double) As Long

'   This is code copied and modified from gdaCreatePointObject

Dim intErr As Integer
Dim i As Integer
Dim lGeoObjectID As Long
Dim lClassID As Long
Dim lSubclassID As Long
Dim lLayerID As Long
Dim bFoundSubClass As Boolean
Dim sql As String
Dim strLayerDesc As String
Dim strLayerName As String
Dim snpNew As Recordset
Dim snpEOC As Recordset
Dim stUpdateType As String  ' Type of point object update ("add" or "mod")
Dim strThemeName As String   ' GIS theme name -- e.g. "facility", not "fa_<eoc_code>"

    Dim lngExercise         As Long
    Dim objCon              As New ADODB.Connection
    Dim rsList              As New ADODB.Recordset

    On Error GoTo gdaPointObjectError
    
    '----------------------------------------------------------------------------------
    ' open a connection to the Oracle Database
    '----------------------------------------------------------------------------------
    objCon.Open DB_Connect
    
    lngExercise = gsEID
    
    '----------------------------------------------------------------------------------
    ' assume function fails
    '----------------------------------------------------------------------------------
    lGeoObjectID = -1  ' Negative value indicates the routine was unsuccessful
    
    stUpdateType = vbNullString
    
    '----------------------------------------------------------------------------------
    ' Verify the User Defined Point Class
    '----------------------------------------------------------------------------------
    sql = "SELECT L.GIS_Layer_Name, L.GIS_Layer_ID, K.class_id, K.Class_Description, L.Generic_GIS_Layer_Code"
    sql = sql & " FROM OBJECT_CLASS K, GIS_LAYER L WHERE K.Class_Name='" & strObjectType & "' "
    sql = sql & " AND K.GIS_Layer_ID=L.GIS_Layer_ID"
    sql = sql & " AND K.Exercise_Num=" & lngExercise & " AND L.Exercise_Num=" & lngExercise
    
    rsList.Open sql, objCon, adOpenStatic, adLockReadOnly
    
    If rsList.EOF Then
        GoTo gdaPointObjectError
    Else
      'Found object type as a Class_Name in OBJECT_CLASS table
      strLayerDesc = vbNullString & rsList("Class_Description")
      strLayerName = vbNullString & rsList("GIS_Layer_Name")
      lLayerID = CLng(rsList("GIS_Layer_ID"))
      lClassID = CLng(rsList("class_id"))
      If (strLayerName = vbNullString) Then
        strLayerName = strObjectType
      End If
      strThemeName = strLayerName
      If (strLayerDesc = vbNullString) Then
        strLayerDesc = strObjectType
      End If
    End If
    rsList.Close


''***Get the subclass_id corresponding to this subclass name within this class_id
    If (strSubType = vbNullString) Then
        bFoundSubClass = False
    Else
        sql = "SELECT subclass_id " & _
              "  FROM OBJECT_SUBCLASS " & _
              " WHERE subclass_name='" & strSubType & "' " & _
              "   AND class_id=" & lClassID & " " & _
              "   AND exercise_num = " & lngExercise
              
        rsList.Open sql, objCon, adOpenStatic, adLockReadOnly
        If (rsList.EOF) Then
            bFoundSubClass = False
        Else
            bFoundSubClass = True
            lSubclassID = CLng(rsList("subclass_id"))
        End If
        rsList.Close
    End If

'' ***** ADDED 01/16/99 RAB / modified 12/10/99 RAB ******
'' See if this object already exists in GEO_OBJECT table
    sql = "SELECT geo_object_id FROM GEO_OBJECT WHERE geo_object_name='" & strName & "' "
    sql = sql & " AND class_id=" & lClassID & " AND exercise_num=" & gsEID
    
    rsList.Open sql, objCon, adOpenStatic, adLockReadOnly
    If rsList.EOF Then
       'This object does not exist in GEO_OBJECT (it's an ADD)
        stUpdateType = "add"
        rsList.Close
    
        sql = "SELECT GEO_OBJECT_ID.NEXTVAL FROM DUAL"
        
        rsList.Open sql, objCon, adOpenStatic, adLockReadOnly
        If (rsList.EOF) Then
            rsList.Close
            GoTo gdaPointObjectError
        Else
            lGeoObjectID = CLng(rsList("NEXTVAL"))
            rsList.Close
        End If
    '***Insert the information for the new point object into GEO_OBJECT table ***
        sql = "INSERT INTO GEO_OBJECT (geo_object_id, geo_object_name, eoc_name, gis_layer_id, class_id,"
        If (bFoundSubClass) Then
            sql = sql & "subclass_id,"
        End If
        sql = sql & "femis_point_x,femis_point_y,exercise_num,exercise_flag,xmit_init_date) VALUES (" & lGeoObjectID & ","
        sql = sql & " '" & Trim$(strName) & "', '" & gstUCurEOCName & "'," & lLayerID & "," & lClassID
        If (bFoundSubClass) Then
            sql = sql & "," & lSubclassID
        End If
        sql = sql & "," & dx & "," & dy & "," & gsEID & ",'Y'," & stTO_DATE(CStr(vZuluNow)) & ")"
        Debug.Print sql
        objCon.Execute sql
    Else
        'Object already exists in database, just update the subtype, latitude, and longitude in GEO_OBJECT table
        stUpdateType = "mod"
        lGeoObjectID = rsList("geo_object_id")
        rsList.Close
        sql = "UPDATE GEO_OBJECT SET femis_point_x=" & CStr(dx) & ",femis_point_y=" & CStr(dy)
        If (bFoundSubClass) Then
          sql = sql & ",subclass_id=" & lSubclassID
        End If
        sql = sql & " WHERE geo_object_id=" & lGeoObjectID & " AND Exercise_num = " & gsEID
        Debug.Print sql
        objCon.Execute sql
    
    End If
    
    sql = "UPDATE GIS_LAYER SET gis_layer_modification_date=" & stTO_DATE(CStr(vZuluNow)) & ",xmit_init_date=" _
        & stTO_DATE(CStr(vZuluNow)) & " WHERE gis_layer_name='" & strLayerName & "' AND Exercise_num = " & gsEID
    Debug.Print sql
    objCon.Execute sql
    
    DoEvents
    
    CreatePointObject = lGeoObjectID
    Exit Function

gdaPointObjectError:
    CreatePointObject = -1
    Exit Function

End Function

Private Function fnstDecryptUserPwd(stInput As String) As String
    
    Dim stOut   As String   '** Generated return string

    stOut = String$(M_lBUFFER_LEN, vbKeySpace)
    
    Call FxstDecrypt(stInput, "pad_id", stOut)
    
    stOut = Trim$(stOut)

    If (Len(stOut) > 1) Then
        stOut = Left$(stOut, Len(stOut) - 1)
    End If
    
    fnstDecryptUserPwd = stOut

End Function

' NOTIFICATION STUFF STARTS HERE

'ToDo: NsGetMsgDDParams is using wrong data for XmitInitDate(n)
'ToDo: Modify the old-style functions to call the new functions.
'ToDo: Move some constants to K_NOTIF.BAS?


'*** Warning:  If you change this file, contact LaMar Stoops (375-2843).
'              Otherwise, your changes may be destroyed by the next update.
'
'== ##BEGIN_FILE_HEADER## ====================================================
'$Header: q:/kp_api/rcs/clsearthalert.cls 1.4 2005/07/12 16:17:02Z d3k077 Exp $
'
'$RCSfile: clsearthalert.cls $
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
'03/07/97  iNSEVENT_To_FEVTDATA() has a bug that causes string parameters to
'          be truncated at one byte less than the data structure allows.
'          This bug MUST NOT be fixed because it will break other modules
'          that depend on this behavior.
'
'== ##END_FILE_NOTES## =======================================================

'== ##BEGIN_FILE_HISTORY## ===================================================
'
'  date    by     description
'--------  ---  ----------------------------------------
'07/21/94  LRS  File created.
'08/13/94  LRS  Increased size of szEvtName and szExerID fields in FEVTDATA.
'08/13/94  LRS  Added ulClientID field to FEVTDATA.
'08/15/94  LRS  Added ulHostID member to FEVTDATA structure.
'08/16/94  LRS  Split FEVTDATA member ulOflags into usOtype and usOflags.
'09/06/94  LRS  Added AnQueryEventBoardTimeSec() function.
'09/13/94  LRS  Added AnSecMsecToCTimeStr() function.
'09/15/94  LRS  Added AnOpenLinkWM() function.
'09/16/94  LRS  Added AnQueryEventBoardTimeVB() function.
'09/19/94  LRS  Added global variables and InitNotifGlobals() function.
'09/19/94  LRS  Added event data functions.
'09/20/94  LRS  Added global constant definitions.
'09/21/94  LRS  Added An_InitFEVTDATA() function.
'09/21/94  LRS  Added data structure conversion functions.
'09/21/94  LRS  Added ulNSOpenLink() and iNSCloseLink() functions.
'09/22/94  LRS  Changed NS_EF_GLOBAL value to match FNOTIF1.H.
'09/26/94  LRS  Added iNSVerifyInterest() function.
'09/26/94  LRS  Added iNSOpenServerLink() and iNSCloseServerLink() functions.
'09/28/94  LRS  Added iNSClientInit() and iNSClientCleanup() functions.
'09/30/94  LRS  Added vNSQueryEventBoardTime().
'10/03/94  LRS  Fixed bugs in vTimeSMToVB() function.
'10/11/94  LRS  Changed GetPrivateProfileString() argument type declarations.
'10/11/94  LRS  Disabled references to GetPrivateProfileString().
'10/19/94  LRS  Added An..RecCt() function declarations.
'11/02/94  LRS  Added NS_EF_NORMAL and NS_LF_NORMAL definitions.
'11/02/94  LRS  Added INIT DONE flag to InitNotifGlobals().
'12/12/94  LRS  Changed FEVTDATA member names to reflect usage.
'12/12/94  LRS  Added FEVTDATA format codes.
'01/05/95  LRS  Added iNSGetEventBoardData() function.
'01/06/95  LRS  Added check for bogus arguments in vTimeSMToVB().
'01/17/95  LRS  Moved test function declarations to FNOTIF1T.BAS .
'05/04/95  LRS  Added queued link functions (SEPR #18270).
'05/25/95  LRS  Modified iNSGetQLinkEvent(), iNSGetEventData(), and iNSGetEventBoardData() to trap data errors.
'09/21/95  LRS  Added support for link event queue parameters (SEPR 18741).
'09/21/95  LRS  Modified ulNSOpenQLink() to prevent errors with old DLL's.
'09/26/95  LRS  Added link control flag definitions.
'09/26/95  LRS  Modified ulNSOpenQLink() to add LATEST_INSTANCE_ONLY flag (SEPR 18741).
'03/07/96  LRS  Changed AnOpenServerLink() return type from Long to Integer.
'03/15/96  LRS  Disabled obsolete iNSReleaseEventRef().
'03/15/96  LRS  Removed obsolete ulNSOpenLink() function.
'08/09/96  LRS  Added initialization and error trapping in iNSOpenServerLink().
'02/12/97  LRS  Modified for compatibility with 32-bit DLL.
'03/07/97  LRS  Replaced stCStrToVB() with stAnsiStrToVB() for 32-bit compatibility.
'03/26/97  LRS  Added DLL info function declarations.
'03/26/97  LRS  Fixed bad argument types in 32-bit DLL function declarations.
'03/27/97  LRS  Fixed FEVTDATA string alignment problems.
'12/30/97  LRS  Added structure definitions for data-driven notification.
'01/19/98  LRS  Changed all FEVTDATA initialization calls to use AnInitFEVTDATA().
'01/20/98  LRS  Implemented the PostActionMsg and PostDataChangeMsg functions.
'01/21/98  LRS  Implemented the GetActionMsg and GetDataChangeMsg functions.
'01/23/98  LRS  Added NsGetMsgClass() function and reduced stAuxData array size in NS structures.
'01/23/98  LRS  Added functions to convert between VB time values and C time.
'01/23/98  LRS  Added flags for Data-Driven notification.
'01/23/98  LRS  Fixed hard-coded DDHandler names in message posting functions.
'01/26/98  LRS  Added code to handle empty dates in message processing functions.
'01/26/98  LRS  Added code to automatically set EOCCode and AuxProcessIdent.
'01/27/98  LRS  Modified the NsPostXXMsg functions to automatically set the ARRIVED flag.
'01/28/98  LRS  Fixed a potential number conversion error in NsGetMsgDDParams.
'01/29/98  LRS  Fixed TblName3 index error in NsSetFevtDDParams.
'01/29/98  LRS  Modified the NsGetXXMsgInfo functions to include DD params.
'01/29/98  LRS  Added NsIsLinkMsgID() function.
'01/29/98  LRS  Modified to NsPostMsg3P() to call AnPostEventLF().
'02/04/98  LRS  Modified the FEVTDATA2 data structure to fix compatibility problems.
'01/30/98  LRS  Fixed errors in DD flag handling on local messages.
'02/06/98  LRS  Modified iNSPostEvent3P() to call NsPostMsg3P().
'10/06/98  LRS  Added ByVal to many function arguments.
'10/22/98  LRS  Added 3 AuxData fields to the Action and DataChange message structures.
'10/22/98  LRS  Modified to use Message Destination instead of Exercise ID in message headers.
'11/20/98  LRS  Fixed NsGenFEMISMsgDest() to use site code instead of EOC code.
'11/20/98  LRS  Added ExerID field to DataChange and Action message structures.
'11/23/98  LRS  Added SiteCode, EOCCode, and HazardID fields to DataChange and Action message structures.
'11/24/98  LRS  Added NSRegisterInterest(), NSRemoveInterest(), and NSVerifyInterest() functions.
'11/24/98  LRS  Added AuxData3-5 arguments to the PostActionMsg and PostDataChangeMsg functions.
'12/16/98  LRS  Renamed the "NS..Interest" functions to "Nx.." to prevent confusion.
'01/22/99  LRS  Added NsGetLinkServerInfo() function.
'01/25/99  LRS  Added functions to register and remove interests for all hazards.
'10/05/99  LRS  Added stFolderID to the NS_DATACHG_PARAMS and NS_ACTION_PARAMS structures.
'10/18/99  LRS  Fixed the missing AuxData3-5 arguments in NSPostActionMsg and NSPostDataChangeMsg functions (SEPR 27630).
'10/18/99  LRS  Modified the NSPostActionMsg and NSPostDataChangeMsg functions to set FolderID info.
'10/18/99  LRS  Modified the NSPostActionMsg and NSPostDataChangeMsg functions to support an optional FolderID argument.
'12/07/99  LRS  Added the AnX001, AnX002, and AnX003 functions.
'03/20/00  LRS  Added support for data acknowledgement strings in the NSPostXxxMsg functions.
'== ##END_FILE_HISTORY## =====================================================





'Private Function iFEVTDATA_To_NSEVENT(FEvt As FEVTDATA, NSEvt As NSEVENT) As Integer
'    '-------------------------------------------------------------------------
'    'Purpose: Convert a notification message data to VB format
'    '
'    'Usage:  <integer> = iFEVTDATA_To_NSEVENT (FEvt, NSEvt)
'    '
'    'Inputs:  FEVTDATA FEvt     - C message data structure
'    '         NSEVENT  NSEvt    - VB message data structure
'    '
'    'Outputs: none
'    '
'    'Returns: SUCCESS (0), status code (+), or error code (-)
'    '
'    'Description:
'    '   This function converts notification message data from C format to VB
'    '   format.
'    '
'    'Warning:  This function is for internal use only and may change without
'    ' notice.
'    '-------------------------------------------------------------------------
'
'    ' Copy message data to VB format
'    With NSEvt
'        .ulSrcID = FEvt.ulSrcID
'        .ulMsgFlags = FEvt.ulMsgFlags
'        .vEvtTime = vTimeSMToVB(FEvt.ulEvtTime, FEvt.usEvtMsec)
'        .ulRecID = FEvt.ulRecID
'        .stEvtName = stAnsiStrToVB(FEvt.szEvtName)
'        .stExerID = stAnsiStrToVB(FEvt.szExerID)
'        .stParm1 = stAnsiStrToVB(FEvt.szParm1)
'        .stParm2 = stAnsiStrToVB(FEvt.szParm2)
'        .stParm3 = stAnsiStrToVB(FEvt.szParm3)
'    End With
'End Function
'
'Private Sub InitNotifGlobals()
'    '-------------------------------------------------------------------------
'    'Purpose: Initialize Notification Service globals
'    '
'    'Usage:   InitNotifGlobals()
'    '
'    'Inputs:  none
'    '
'    'Outputs: none
'    '
'    'Returns: none
'    '
'    'Description:
'    '   This function initializes global Notification parameters, etc., and
'    '   should be called before using any other Notification functions.
'    '
'    '   Note:  No harm will be done by calling this function multiple times
'    '   during an application.
'    '-------------------------------------------------------------------------
'
'    Static fInitDone As Integer         'INIT DONE flag
'    Dim stTemp As String                'string buffer
'    Dim lTemp As Long
'    Dim iRC As Integer                  'temp return code
'
'    ' Exit if INIT already done
'    If fInitDone Then
'        Exit Sub
'    End If
'
'    ' Create notification window message number
'    gstNotifWinMsgName = "FNotifWinMsg" 'window message name
'    'giNotifWinMsgNo = RegisterWindowMessage(gstNotifWinMsgName)
'
'    ' Get default notification info
'    'stTemp = String$(256, 0)             'create buffer
'    'iRC = GetPrivateProfileString("Notification", "DefaultNotifServerHost", "", stTemp, Len(stTemp) - 1, "fnotif1.ini")
'    'stTemp = stAnsiStrToVB(stTemp)
'    'gstDfltNotifServerHost = stTemp      'save name
'
'    ' Initialize client
'    iRC = iNSClientInit()
'
'    ' Set INIT DONE flag
'    fInitDone = 1
'End Sub
'
'Private Function iNSClientCleanup() As Integer
'    '-------------------------------------------------------------------------
'    'Purpose: Do cleanup when client application closes
'    '
'    'Usage:   <integer> = iNSClientCleanup()
'    '
'    'Inputs:  none
'    '
'    'Outputs: none
'    '
'    'Returns: SUCCESS (0), status code (+), or error code (-)
'    '
'    'Description:
'    '   This function performs necessary cleanup when a client application
'    '   closes, including release of WinSock task info structures.
'    '
'    'Warning:  This function should be called only once, during application
'    ' shutdown.  Unpredictable errors will occur if it is used at any other time.
'    '-------------------------------------------------------------------------
'
'    ' Do client cleanup
'    iNSClientCleanup = AnClientCleanup()
'End Function
'
'Private Function iNSClientInit() As Integer
'    '-------------------------------------------------------------------------
'    'Purpose: Do initialization when client application opens
'    '
'    'Usage:   <integer> = iNSClientInit()
'    '
'    'Inputs:  none
'    '
'    'Outputs: none
'    '
'    'Returns: SUCCESS (0), status code (+), or error code (-)
'    '
'    'Description:
'    '   This function performs necessary initialization when a client application
'    '   opens, including allocation of WinSock task info structures.
'    '
'    'Warning:  This function should be called only once, during application
'    ' startup.  Unpredictable errors will occur if it is used at any other time.
'    '-------------------------------------------------------------------------
'
'    ' Initialize client
'    iNSClientInit = AnClientInit()
'End Function
'
'Private Function iNSCloseLink(ByVal ulLinkID As Long) As Integer
'    '-------------------------------------------------------------------------
'    'Purpose: Close a notification client link
'    '
'    'Usage:   <integer> = iNSCloseLink (ulLinkID)
'    '
'    'Inputs:  long ulLinkID     - notification link ID
'    '
'    'Outputs: none
'    '
'    'Returns: SUCCESS (0), status code (+), or error code (-)
'    '
'    'Description:
'    '   This function closes a notification client link and releases all
'    '   resources associated with the link.
'    '
'    'Caution:  Each VB form that creates a notification link should call
'    ' iNSCloseLink() in its shutdown process (Form_Unload event).  Otherwise,
'    ' the notification link becomes an orphan and its resources are never
'    ' released.
'    '-------------------------------------------------------------------------
'
'    ' Close link
'    iNSCloseLink = AnCloseLink(ulLinkID)
'End Function
'
'Private Function iNSCloseServerLink(ByVal stServerHost As String, ByVal iServerPort As Integer) As Integer
'    '-------------------------------------------------------------------------
'    'Purpose: Close link from local message manager to a notification server
'    '
'    'Usage:   <integer> = iNSCloseServerLink (stServerHost, iServerPort)
'    '
'    'Inputs:  string  stServerHost - server host name
'    '         integer iServerPort  - server port
'    '
'    'Outputs: none
'    '
'    'Returns: SUCCESS (0), status code (+), or error code (-)
'    '
'    'Description:
'    '   This function closes the link from your local message manager to a
'    '   notification server and releases all resources associated with the
'    '   link.
'    '
'    '   Since server links are automatically closed when the message manager
'    '   is closed, this function should only be called when a server link
'    '   is actually being discontinued.
'    '
'    'Warning:  This is a system-level function and should never be called
'    ' by task-level code.
'    '-------------------------------------------------------------------------
'
'    ' Close link
'    iNSCloseServerLink = AnCloseServerLink(stServerHost, iServerPort)
'End Function
'
'Private Function iNSEVENT_To_FEVTDATA(NSEvt As NSEVENT, FEvt As FEVTDATA) As Integer
'    '-------------------------------------------------------------------------
'    'Purpose: Convert notification message data to C format
'    '
'    'Usage:   <integer> = iNSEVENT_To_FEVTDATA (NSEvt, FEvt)
'    '
'    'Inputs:  NSEVENT  NSEvt    - VB message data structure
'    '         FEVTDATA FEvt     - C message data structure
'    '
'    'Outputs: none
'    '
'    'Returns: SUCCESS (0), status code (+), or error code (-)
'    '
'    'Description:
'    '   This function converts notification message data from VB format to C
'    '   format.
'    '
'    '-------------------------------------------------------------------------
'    'CAUTION:  This function has a bug that causes string parameters to be
'    '   truncated at one byte less than the data structure allows.
'    '   This bug MUST NOT be fixed because it will break other modules that
'    '   depend on this behavior.
'    '-------------------------------------------------------------------------
'    'Warning:  This function is for internal use only and may change without
'    '   notice.
'    '-------------------------------------------------------------------------
'
'    Dim iEvtNameLen As Integer      'C structure string length
'    Dim iExerIDLen As Integer       'C structure string length
'    Dim iParmLen As Integer         'C structure string length
'    Dim iRC As Integer
'
'    ' Setup
'    #If Win32 Then
'        iEvtNameLen = UBound(FEvt.szEvtName)
'        iExerIDLen = UBound(FEvt.szExerID)
'        iParmLen = UBound(FEvt.szParm1)
'    #Else
'        iEvtNameLen = Len(FEvt.szEvtName)
'        iExerIDLen = Len(FEvt.szExerID)
'        iParmLen = Len(FEvt.szParm1)
'    #End If
'
'    ' Clear data structure
'    iRC = AnInitFEVTDATA(FEvt, FEVTDATA_FMT_STD)
'
'    ' Copy message data to VB format
'    FEvt.ulSrcID = NSEvt.ulSrcID
'    FEvt.ulMsgFlags = NSEvt.ulMsgFlags
'    'FEvt.ulEvtTime = 0
'    FEvt.usEvtMsec = 0
'    FEvt.ulRecID = NSEvt.ulRecID
'    #If Win32 Then
'        iRC = iVBToByteStr(NSEvt.stEvtName, FEvt.szEvtName, iEvtNameLen - 1)
'        iRC = iVBToByteStr(NSEvt.stExerID, FEvt.szExerID, iExerIDLen - 1)
'        iRC = iVBToByteStr(NSEvt.stParm1, FEvt.szParm1, iParmLen - 1)
'        iRC = iVBToByteStr(NSEvt.stParm2, FEvt.szParm2, iParmLen - 1)
'        iRC = iVBToByteStr(NSEvt.stParm3, FEvt.szParm3, iParmLen - 1)
'    #Else
'        FEvt.szEvtName = Left$(NSEvt.stEvtName, iEvtNameLen - 1) + Chr$(0)
'        FEvt.szExerID = Left$(NSEvt.stExerID, iExerIDLen - 1) + Chr$(0)
'        FEvt.szParm1 = Left$(NSEvt.stParm1, iParmLen - 1) + Chr$(0)
'        FEvt.szParm2 = Left$(NSEvt.stParm2, iParmLen - 1) + Chr$(0)
'        FEvt.szParm3 = Left$(NSEvt.stParm3, iParmLen - 1) + Chr$(0)
'    #End If
'
'    ' Truncate strings for compatibility with earlier versions
'
'End Function

'Private Function iNSGetEventBoardData(ByVal stMsgName As String, ByVal stMsgDest As String, NSEvt As NSEVENT) As Integer
'    '-------------------------------------------------------------------------
'    'Purpose: Get latest instance of a notification message from the message board.
'    '
'    'Usage:   <integer> = iNSGetEventBoardData (stMsgName, stMsgDest, NSEvt)
'    '
'    'Inputs:  string stMsgName  - message name
'    '         string stMsgDest  - message destination
'    '         NSEVENT NSEvt     - message data structure
'    '
'    'Outputs: none
'    '
'    'Returns: SUCCESS (0), status code (+), or error code (-)
'    '
'    'Description:
'    '   This function retrieves the latest instance of a notification message
'    '   from the message board, based on message name and exercise ID.
'    '
'    '   If the message data cannot be retrieved, the function returns a negative
'    '   error code.
'    '-------------------------------------------------------------------------
'
'    Dim FEvt As FEVTDATA
'    Dim iRC As Integer
'
'    ' Initialize data structure
'    iRC = AnInitFEVTDATA(FEvt, FEVTDATA_FMT_STD)
'
'    ' Get message data
'    iRC = AnQueryEventBoard(FEvt, stMsgName, stMsgDest)
'    If iRC < 0 Then
'        iNSGetEventBoardData = -1   'status = FAILURE
'        Exit Function               'abort operation
'    Else
'        iNSGetEventBoardData = iRC  'save status code
'    End If
'
'    ' Copy message data to VB format
'    iRC = iFEVTDATA_To_NSEVENT(FEvt, NSEvt)
'End Function
'
'Private Function iNSGetEventData(ByVal ulRecID As Long, NSEvt As NSEVENT) As Integer
'    '-------------------------------------------------------------------------
'    'Purpose: Get message data for a notification message
'    '
'    'Usage:   <integer> = iNSGetEventData (ulRecID, NSEvt)
'    '
'    'Inputs:  long    ulRecID   - message record ID
'    '         NSEVENT NSEvt     - message data structure
'    '
'    'Outputs: none
'    '
'    'Returns: SUCCESS (0), status code (+), or error code (-)
'    '
'    'Description:
'    '   This function retrieves message data for a notification message, based
'    '   on a record ID.
'    '
'    '   If the message data cannot be retrieved, the function returns a negative
'    '   error code.
'    '-------------------------------------------------------------------------
'
'    Dim FEvt As FEVTDATA
'    Dim iRC As Integer
'
'    ' Initialize data structure
'    iRC = AnInitFEVTDATA(FEvt, FEVTDATA_FMT_STD)
'
'    ' Get message data
'    iRC = AnQueryEventBuffer(FEvt, ulRecID)
'    If iRC < 0 Then
'        iNSGetEventData = -1        'status = FAILURE
'        Exit Function               'abort operation
'    Else
'        iNSGetEventData = iRC       'save status code
'    End If
'
'    ' Copy message data to VB format
'    iRC = iFEVTDATA_To_NSEVENT(FEvt, NSEvt)
'End Function
'
'Private Function iNSGetQLinkEvent(ByVal ulLinkID As Long, ByVal ulRecID As Long, NSEvt As NSEVENT) As Integer
'    '-------------------------------------------------------------------------
'    'Purpose: Get message data from a notification client's message queue
'    '
'    'Usage:   <integer> = iNSGetQLinkEvent (ulLinkID, ulRecID, NSEvt)
'    '
'    'Inputs:  long    ulLinkID  - link ID
'    '         long    ulRecID   - message record ID
'    '         NSEVENT NSEvt     - message data structure
'    '
'    'Outputs: none
'    '
'    'Returns: SUCCESS (0), status code (+), or error code (-)
'    '
'    'Description:
'    '   This function retrieves message data for a notification message, based
'    '   on link ID and a record ID.
'    '
'    '   If the message data cannot be retrieved, the function returns a negative
'    '   error code.
'    '-------------------------------------------------------------------------
'
'    Dim FEvt As FEVTDATA
'    Dim iRC As Integer
'
'    ' Initialize data structure
'    iRC = AnInitFEVTDATA(FEvt, FEVTDATA_FMT_STD)
'
'    ' Get message data
'    iRC = AnGetQLinkEvent(ulLinkID, FEvt, ulRecID)
'    If iRC < 0 Then
'        iNSGetQLinkEvent = -1       'status = FAILURE
'        Exit Function               'abort operation
'    Else
'        iNSGetQLinkEvent = iRC      'save status code
'    End If
'
'    ' Copy message data to VB format
'    iRC = iFEVTDATA_To_NSEVENT(FEvt, NSEvt)
'End Function
'
'Private Function iNSOpenServerLink(ByVal stServerHost As String, ByVal iServerPort As Integer, ByVal ulLinkFlags As Long) As Integer
'    '-------------------------------------------------------------------------
'    'Purpose: Open link from local message manager to a notification server
'    '
'    'Usage:   <integer> = iNSOpenServerLink (stServerHost, iServerPort, ulLinkFlags)
'    '
'    'Inputs:  string  stServerHost  - server host name
'    '         integer iServerPort   - server port
'    '         long    ulLinkFlags   - link attribute flags  (0 = none)
'    '
'    'Outputs: none
'    '
'    'Returns: SUCCESS (0), status code (+), or error code (-)
'    '
'    'Description:
'    '   This function opens a link from your local message manager to a
'    '   notification server, based on the supplied host name and port.
'    '
'    '   Link attribute flags can be used to control how the link operates.
'    '   However, no attribute flags are currently implemented, so you should
'    '   use a value of zero (no flags).
'    '
'    '   Caution: This function is normally used only at startup and should not
'    '   be called unless a different server is actually needed.
'    '
'    'Warning:  This is a system-level function and should never be called
'    ' by task-level code.
'    '-------------------------------------------------------------------------
'
'    ' Set error trap
'    On Error Resume Next    'needed if DLL function is not available
'
'    ' Initialize globals, if necessary
'    If giNotifWinMsgNo = 0 Then
'        InitNotifGlobals
'    End If
'
'    ' Open link
'    iNSOpenServerLink = AnOpenServerLink(stServerHost, iServerPort, ulLinkFlags)
'End Function
'
'
Private Function iVBToByteStr(ByVal stVBStr As String, pszByteStr() As Byte, ByVal iMaxLen As Integer) As Integer
    '-------------------------------------------------------------------------
    'Purpose: Convert standard VB string to null-terminated byte string
    '
    'Usage:   <string> = iVBToByteStr (stVBStr, pszByteStr, iMaxLen)
    '
    'Inputs:  stVBStr - standard VB string
    '         iMaxLen - output length limit, including null terminator
    '
    'Outputs: pszByteStr - null-terminated byte string
    '
    'Returns: Byte string length, excluding null terminator
    '
    'Description:
    '   This function converts a standard VB string to null-terminated byte
    '   string needed by C and DLL functions.  If the source string is longer
    '   than the target buffer, the output will be truncated to fit.
    '
    '-------------------------------------------------------------------------
    'Warning: DO NOT remove ByVal qualifiers from the argument list!!  They are
    '   needed for automatic type conversions.
    '-------------------------------------------------------------------------
    'Warning: This function is for internal use only and may change without
    '   notice.
    '-------------------------------------------------------------------------

    Dim iSLen As Integer            'source string length
    Dim i As Integer                'loop index
    Dim c As Integer                'character code

    ' Setup
    iSLen = Len(stVBStr)                'source string length
    If iSLen > iMaxLen - 1 Then         'longer than output buffer?
        iSLen = iMaxLen - 1             'limit length to prevent overflow
    End If

    ' Convert string
    For i = 1 To iSLen
        c = Asc(Mid(stVBStr, i, 1))     'get source character code
        If c = 0 Then                   'null terminator?
            Exit For                    'end of conversion
        ElseIf c > 255 Then             'larger than a byte?
            c = c And 255               'limit number range
        End If
        pszByteStr(i - 1) = c           'add character to result
    Next

    ' Add null terminator
    pszByteStr(iSLen) = 0

    ' Return result
    iVBToByteStr = iSLen
End Function

Private Function LPSZ_To_VBStr(ByVal lpsz As Long) As String
    '-------------------------------------------------------------------------
    'Purpose: Convert a C string pointer to a VB string
    'Returns: VB string without null-terminator
    '-------------------------------------------------------------------------

    Const iBufLen = 2000                'buffer length
    Dim stBuffer As String              'string buffer
    Dim iTLoc As Integer                'terminator location

    ' Setup
    stBuffer = String$(iBufLen + 1, "x")  'create buffer

    ' Get string value
    AnStrNCpyZ stBuffer, lpsz, iBufLen  'copy string to buffer

    ' Truncate string at the first null character
    iTLoc = InStr(stBuffer, Chr$(0))    'find terminator
    If iTLoc > 0 Then stBuffer = Left$(stBuffer, iTLoc - 1)

    ' Return result
    LPSZ_To_VBStr = stBuffer
End Function

Private Function Ns_CTimeToVBTime(ByVal ulCTime As Long) As Date
    '-------------------------------------------------------------------------
    'Purpose: Convert a C time value to a VB time value
    'Returns: VB time value
    '-------------------------------------------------------------------------

    Dim vVBTime As Date                 'VB time value
    Const SecsPerDay = 86400#           'seconds per day
    Const CTimeDayAdj = 25569#          'CTime day adjustment

    ' Convert C time value to VB time value
    vVBTime = CDate((ulCTime / SecsPerDay) + CTimeDayAdj)

    ' Return result
    Ns_CTimeToVBTime = vVBTime             'return VB time
End Function

'Private Function Ns_FEVTDATA_to_MSGDATA(tFevtData As FEVTDATA2, tMsgData As NS_MSG_DATA) As Integer
'    '-------------------------------------------------------------------------
'    'Purpose: Copy data from FEVTDATA structure to NS_MSG_DATA structure
'    '
'    'Usage:   iSC = Ns_FEVTDATA_to_MSGDATA(tFevtData, tMsgData)
'    '
'    'Inputs:  tFevtData - DLL message data structure
'    '         tMsgData  - VB message data structure
'    '         iSC       - status code
'    '
'    'Outputs: none
'    '
'    'Returns: SUCCESS (0), status code (+), or error code (-)
'    '-------------------------------------------------------------------------
'    'Note: This function assumes that the target structure is already initialized.
'    '-------------------------------------------------------------------------
'
'    Dim iParmArrLen As Integer          'parameter array length
'    Dim i As Integer                    'loop index
'
'    ' Setup
'    iParmArrLen = AnFEVTDATA_MsgParmArrLen(tFevtData)
'
'    ' Copy data from FEVTDATA structure to NS_MSG_DATA structure
'    With tMsgData
'        'header info
'        .ulMsgID = tFevtData.ulRecID
'        .vMsgTime = vTimeSMToVB(tFevtData.ulMsgTime, tFevtData.usMsgMsec)
'        .ulMsgFlags = tFevtData.ulMsgFlags
'        .stMsgClass = LPSZ_To_VBStr(AnFEVTDATA_MsgParm(tFevtData, NS_MsgClass_IDX))
'        .stMsgName = LPSZ_To_VBStr(AnFEVTDATA_MsgName(tFevtData))
'        .stMsgDest = LPSZ_To_VBStr(AnFEVTDATA_MsgDest(tFevtData))
'        .stAuxProcessIdent = LPSZ_To_VBStr(AnFEVTDATA_AuxProcessIdent(tFevtData))
'        .ulAuxProcessFlags = tFevtData.ulAuxProcessFlags
'
'        'message parameters
'        For i = 0 To iParmArrLen - 1
'            .stMsgParm(i) = LPSZ_To_VBStr(AnFEVTDATA_MsgParm(tFevtData, i))
'        Next
'
'        'auxiliary info
'        Select Case UCase(.stMsgClass)
'            Case UCase(NS_MSG_CLASS_ACTION)
'                .stExerID = LPSZ_To_VBStr(AnFEVTDATA_MsgParm(tFevtData, NS_AC_ExerID_IDX))
'            Case UCase(NS_MSG_CLASS_DATA_CHANGE)
'                .stExerID = LPSZ_To_VBStr(AnFEVTDATA_MsgParm(tFevtData, NS_DC_ExerID_IDX))
'            Case Else
'                .stExerID = ""
'        End Select
'    End With
'
'    ' Return result
'    Ns_FEVTDATA_to_MSGDATA = 0          'status = SUCCESS
'End Function
'
'Private Function Ns_MSGDATA_to_FEVTDATA(tMsgData As NS_MSG_DATA, tFevtData As FEVTDATA2) As Integer
'    '-------------------------------------------------------------------------
'    'Purpose: Copy data from NS_MSG_DATA structure to FEVTDATA structure
'    '
'    'Usage:   iSC = Ns_MSGDATA_to_FEVTDATA(tMsgData, tFevtData)
'    '
'    'Inputs:  tMsgData  - VB message data structure
'    '         tFevtData - DLL message data structure
'    '         iSC       - status code
'    '
'    'Outputs: none
'    '
'    'Returns: SUCCESS (0), status code (+), or error code (-)
'    '-------------------------------------------------------------------------
'    'Note: This function assumes that the target structure is already initialized.
'    'Note: Derived fields (ExerciseID) are not copied.
'    '-------------------------------------------------------------------------
'
'    Dim iParmArrLen As Integer          'parameter array length
'    Dim i As Integer                    'loop index
'
'    ' Setup
'    iParmArrLen = AnFEVTDATA_MsgParmArrLen(tFevtData)
'
'    ' Copy data from NS_MSG_DATA structure to FEVTDATA structure
'    With tFevtData
'        'header info
'        .ulRecID = tMsgData.ulMsgID
'        .ulMsgTime = 0
'        .usMsgMsec = 0
'        .ulMsgFlags = tMsgData.ulMsgFlags
'        AnFEVTDATA_SetMsgName tFevtData, tMsgData.stMsgName
'        AnFEVTDATA_SetMsgDest tFevtData, tMsgData.stMsgDest
'        AnFEVTDATA_SetAuxProcessIdent tFevtData, tMsgData.stAuxProcessIdent
'        .ulAuxProcessFlags = tMsgData.ulAuxProcessFlags
'
'        'message parameters
'        For i = 0 To iParmArrLen - 1
'            AnFEVTDATA_SetMsgParm tFevtData, i, tMsgData.stMsgParm(i)
'        Next
'    End With
'
'    ' Return result
'    Ns_MSGDATA_to_FEVTDATA = 0          'status = SUCCESS
'End Function
'
Private Function Ns_VBTimeToCTime(ByVal vVBTime As Date) As Long
    '-------------------------------------------------------------------------
    'Purpose: Convert a VB time value to a C time value
    'Returns: C time value
    '-------------------------------------------------------------------------

    Dim vCTime As Date                  'C time value (full)
    Dim ulCTime As Long                 'C time seconds
    Dim usCMsec As Integer              'C time fraction (msec)
    Const SecsPerDay = 86400#           'seconds per day
    Const CTimeDayAdj = 25569#          'CTime day adjustment

    'Purpose: Convert a VB time value to a C time value
    vCTime = CLng((CDbl(vVBTime) - CTimeDayAdj) * SecsPerDay)
    ulCTime = Fix(vCTime)               'C time seconds

    'Return result
    Ns_VBTimeToCTime = ulCTime          'return C time
End Function


'Private Sub NsGetMsgDDParams(tMsgData As NS_MSG_DATA, tDDParams As NS_DD_PARAMS)
'    '-------------------------------------------------------------------------
'    'Purpose: Extract DataDriven parameter info from a generic notification message.
'    '
'    'Usage:   iSC = NsSetMsgDDParams (tMsgData, tDDParams)
'    '
'    'Arguments:
'    '   tMsgData    - message data structure
'    '   tDDParams   - data-driven processing parameters
'    '   iSC         - status code
'    '
'    'Outputs: none
'    '
'    'Returns: nothing
'    '-------------------------------------------------------------------------
'    'Note: This function assumes that the target structure is already initialized.
'    '-------------------------------------------------------------------------
'
'    Dim vXmitInitDate(3) As Date        'record timestamps
'
'    ' Prepare timestamp values
'    If IsDate(tMsgData.stMsgParm(NS_DD_RowID1_IDX)) Then _
'        vXmitInitDate(0) = CDate(tMsgData.stMsgParm(NS_DD_RowID1_IDX))
'    If IsDate(tMsgData.stMsgParm(NS_DD_RowID2_IDX)) Then _
'        vXmitInitDate(1) = CDate(tMsgData.stMsgParm(NS_DD_RowID2_IDX))
'    If IsDate(tMsgData.stMsgParm(NS_DD_RowID3_IDX)) Then _
'        vXmitInitDate(2) = CDate(tMsgData.stMsgParm(NS_DD_RowID3_IDX))
'
'    ' Extract DataDriven parameter info from a generic notification message.
'    tDDParams.stTableName(0) = tMsgData.stMsgParm(NS_DD_TblName1_IDX)
'    tDDParams.stTableName(1) = tMsgData.stMsgParm(NS_DD_TblName2_IDX)
'    tDDParams.stTableName(2) = tMsgData.stMsgParm(NS_DD_TblName3_IDX)
'    tDDParams.stRowID(0) = tMsgData.stMsgParm(NS_DD_RowID1_IDX)
'    tDDParams.stRowID(1) = tMsgData.stMsgParm(NS_DD_RowID2_IDX)
'    tDDParams.stRowID(2) = tMsgData.stMsgParm(NS_DD_RowID3_IDX)
'    tDDParams.vXmitInitDate(0) = vXmitInitDate(0)
'    tDDParams.vXmitInitDate(1) = vXmitInitDate(1)
'    tDDParams.vXmitInitDate(2) = vXmitInitDate(2)
'    tDDParams.stAction(0) = tMsgData.stMsgParm(NS_DD_Action1_IDX)
'    tDDParams.stAction(1) = tMsgData.stMsgParm(NS_DD_Action2_IDX)
'    tDDParams.stAction(2) = tMsgData.stMsgParm(NS_DD_Action3_IDX)
'    tDDParams.stListener = tMsgData.stMsgParm(NS_DD_Listener_IDX)
'    tDDParams.lDDFlags = Val(tMsgData.stMsgParm(NS_DD_DDFlags_IDX))
'    tDDParams.stEOCCode = tMsgData.stMsgParm(NS_DD_EOCCode_IDX)
'    tDDParams.stDDHandler = tMsgData.stMsgParm(NS_DD_DDHandler_IDX)
'
'    ' Return result
'    'NsGetMsgDDParams = 0                'status = SUCCESS
'End Sub

'Private Function NsIsLinkMsgID(ByVal ulLinkID As Long, ByVal ulMsgID As Long) As Boolean
'    '-------------------------------------------------------------------------
'    'Purpose: Test whether a message queue contains the specified message ID
'    '
'    'Usage:   fFound = NsIsLinkMsgID (ulLinkID, ulMsgID, tMsgData)
'    '
'    'Inputs:  ulLinkID  - client's link ID
'    '         ulMsgID   - message record ID
'    '         fFound    - result code
'    '
'    'Outputs: none
'    '
'    'Returns: TRUE or FALSE
'    '-------------------------------------------------------------------------
'
'    Dim iRC As Integer                  'temp return code
'
'    ' Test whether message queue contains the specified message ID
'    iRC = AnIsQLinkEvent(ulLinkID, ulMsgID)
'
'    ' Return result
'    If iRC = 0 Then
'        NsIsLinkMsgID = False           'ID not valid
'    Else
'        NsIsLinkMsgID = True            'ID is valid
'    End If
'End Function


'Private Function NsPostDataChangeMsg(ByVal ulLinkID As Long, _
'                                    ByVal ulMsgFlags As Long, _
'                                    ByVal stNotifName As String, _
'                                    ByVal stChangeType As String, _
'                                    ByVal stDatasetName As String, _
'                                    ByVal vChangeDate As Date, _
'                                    Optional vItemID1, _
'                                    Optional vItemID2, _
'                                    Optional vAuxData1, _
'                                    Optional vAuxData2, _
'                                    Optional vAuxData3, _
'                                    Optional vAuxData4, _
'                                    Optional vAuxData5, _
'                                    Optional vFolderID, _
'                                    Optional vAckString _
'                                    ) As Integer
'
'    '-------------------------------------------------------------------------
'    'Purpose: Post a DataChange notification message
'    '
'    'Usage:   iSC = NsPostDataChangeMsg (ulLinkID, ulMsgFlags, stNotifName, stChangeType, stDatasetName, vChangeDate, vItemID1, vItemID2, vAuxData1, vAuxData2)
'    '
'    'Arguments:
'    '   ulLinkID        - notification link ID  (0 = Anonymous)
'    '   ulMsgFlags      - message attribute flags
'    '   stNotifName     - notification name
'    '   stChangeType    - change type
'    '   stDatasetName   - dataset name
'    '   vChangeDate     - change date-time
'    '   vItemID1        - item ID 1  (optional)
'    '   vItemID2        - item ID 2  (optional)
'    '   vAuxData1       - auxiliary data 1  (optional)
'    '   vAuxData2       - auxiliary data 2  (optional)
'    '   vAuxData3       - auxiliary data 3  (optional)
'    '   vAuxData4       - auxiliary data 4  (optional)
'    '   vAuxData5       - auxiliary data 5  (optional)
'    '   vFolderID       - folder ID (optional)
'    '   vAckString      - data acknowledgement string (optional)
'    '   iSC             - status code
'    '
'    'Outputs: none
'    '
'    'Returns: SUCCESS (0), status code (+), or error code (-)
'    '-------------------------------------------------------------------------
'    '
'    'Description:
'    '   This function posts a DataChange notification message using the
'    '   supplied parameters and the current exercise number.
'    '
'    '   If the message cannot be posted, the function returns a negative
'    '   status code.
'    '
'    '   Message attribute flags can be used to control how the message is
'    '   processed.  The following flags are available:
'    '
'    '       NS_EF_DEFAULT       - default attributes
'    '       NS_EF_NORMAL        - distributed to local clients and server
'    '       NS_EF_LOCAL_ONLY    - distributed only to clients on your PC
'    '       NS_EF_GLOBAL        - forwarded to ALL notification servers
'    '
'    '   These flags can be ORed together to produce any desired combination
'    '   of attributes.
'    '
'    '   To prevent notification loopback, each message is associated with an
'    '   message source ID (ulLinkID).  If the message source is unknown or
'    '   not important, you can use ID = 0 to make anonymous message postings.
'    '-------------------------------------------------------------------------
'    'Note: This function automatically adds a NS_DD_ARRIVED flag to the message.
'    '-------------------------------------------------------------------------
'
'    Dim tDDParams   As NS_DD_PARAMS     'data-driven parameters
'    Dim iRC         As Integer          'temp return code
'
'    ' Setup
'    tDDParams.stDDHandler = "(none)"    'set no-DD flag
'    tDDParams.lDDFlags = NS_DD_ARRIVED Or tDDParams.lDDFlags  'set ARRIVED flag
'
'    ' Post DataChange message with dummy DD parameters
'    iRC = NsPostDataChangeMsgDD(ulLinkID, ulMsgFlags, tDDParams, stNotifName, stChangeType, stDatasetName, vChangeDate, vItemID1, vItemID2, vAuxData1, vAuxData2, vAuxData3, vAuxData4, vAuxData5, vFolderID)
'
'    ' Return result
'    NsPostDataChangeMsg = iRC           'return status code
'End Function

'Private Function NsPostDataChangeMsgDD(ByVal ulLinkID As Long, _
'                                      ByVal ulMsgFlags As Long, _
'                                      tDDParams As NS_DD_PARAMS, _
'                                      ByVal stNotifName As String, _
'                                      ByVal stChangeType As String, _
'                                      ByVal stDatasetName As String, _
'                                      ByVal vChangeDate As Date, _
'                                      Optional vItemID1, _
'                                      Optional vItemID2, _
'                                      Optional vAuxData1, _
'                                      Optional vAuxData2, _
'                                      Optional vAuxData3, _
'                                      Optional vAuxData4, _
'                                      Optional vAuxData5, _
'                                      Optional vFolderID, _
'                                      Optional vAckString _
'                                      ) As Integer
'
'    '-------------------------------------------------------------------------
'    'Purpose: Post a DataChange notification message with Data-Driven parameters
'    '
'    'Usage:   iSC = NsPostDataChangeMsgDD (ulLinkID, ulMsgFlags, tDDParams, stNotifName, stChangeType, stDatasetName, vChangeDate, vItemID1, vItemID2, vAuxData1, vAuxData2)
'    '
'    'Arguments:
'    '   ulLinkID        - notification link ID  (0 = Anonymous)
'    '   ulMsgFlags      - message attribute flags
'    '   tDDParams       - data-driven processing parameters
'    '   stNotifName     - notification name
'    '   stChangeType    - change type
'    '   stDatasetName   - dataset name
'    '   vChangeDate     - change date-time
'    '   vItemID1        - item ID 1  (optional)
'    '   vItemID2        - item ID 2  (optional)
'    '   vAuxData1       - auxiliary data 1  (optional)
'    '   vAuxData2       - auxiliary data 2  (optional)
'    '   vAuxData3       - auxiliary data 3  (optional)
'    '   vAuxData4       - auxiliary data 4  (optional)
'    '   vAuxData5       - auxiliary data 5  (optional)
'    '   vFolderID       - folder ID (optional)
'    '   vAckString      - data acknowledgement string (optional)
'    '   iSC             - status code
'    '
'    'Outputs: none
'    '
'    'Returns: SUCCESS (0), status code (+), or error code (-)
'    '-------------------------------------------------------------------------
'    '
'    'Description:
'    '   This function posts a DataChange notification message using the
'    '   supplied parameters and the current exercise number.
'    '
'    '   If the message cannot be posted, the function returns a negative
'    '   status code.
'    '
'    '   Message attribute flags can be used to control how the message is
'    '   processed.  The following flags are available:
'    '
'    '       NS_EF_DEFAULT       - default attributes
'    '       NS_EF_NORMAL        - distributed to local clients and server
'    '       NS_EF_LOCAL_ONLY    - distributed only to clients on your PC
'    '       NS_EF_GLOBAL        - forwarded to ALL notification servers
'    '
'    '   These flags can be ORed together to produce any desired combination
'    '   of attributes.
'    '
'    '   To prevent notification loopback, each message is associated with an
'    '   message source ID (ulLinkID).  If the message source is unknown or
'    '   not important, you can use ID = 0 to make anonymous message postings.
'    '-------------------------------------------------------------------------
'
'
'    Dim oMsg            As FNotifyMsg   'notification message object
'    Dim stMsgDest       As String       'message destination
'    Dim stChangeDate    As String       'change timestamp
'    Dim stItemID1       As String       'item ID 1
'    Dim stItemID2       As String       'item ID 2
'    Dim stAuxData1      As String       'auxiliary data 1
'    Dim stAuxData2      As String       'auxiliary data 2
'    Dim stAuxData3      As String       'auxiliary data 3
'    Dim stAuxData4      As String       'auxiliary data 4
'    Dim stAuxData5      As String       'auxiliary data 5
'    Dim lExerNum        As Long         'exercise number
'    Dim lFolderID       As Long         'folder ID
'    Dim stAckString     As String       'data acknowlegment string
'    Dim fAckString      As Boolean      'HAVE ACK STRING flag
'    Dim iRC             As Integer      'temp return code
'
'    ' Setup
'    On Error GoTo Error_Exit
'    NsPostDataChangeMsgDD = -2         'default status = FAILURE
'
'    ' Create message object
'    Set oMsg = New FNotifyMsg
'    If oMsg Is Nothing Then
'        Exit Function
'    End If
'
'    ' Fill optional arguments
'    If Not IsMissing(vItemID1) Then stItemID1 = vItemID1
'    If Not IsMissing(vItemID2) Then stItemID2 = vItemID2
'    If Not IsMissing(vAuxData1) Then stAuxData1 = vAuxData1
'    If Not IsMissing(vAuxData2) Then stAuxData2 = vAuxData2
'    If Not IsMissing(vAuxData3) Then stAuxData3 = vAuxData3
'    If Not IsMissing(vAuxData4) Then stAuxData4 = vAuxData4
'    If Not IsMissing(vAuxData5) Then stAuxData5 = vAuxData5
'    If IsNumeric(vFolderID) Then
'        lFolderID = vFolderID
'    Else
'        lFolderID = gtFolder.lFolderID
'    End If
'    If IsMissing(vAckString) Then
'        fAckString = False
'        stAckString = ""
'    Else
'        fAckString = True
'        stAckString = vAckString & ""
'    End If
'
'    ' Prepare message data
'    lExerNum = gsEID
'    stMsgDest = NsGenFEMISMsgDest(lExerNum)
'    If vChangeDate <> 0 Then stChangeDate = Format$(vChangeDate, "DD-MMM-YYYY HH:MM:SS")
'
'    ' Set message parameter values
'    With oMsg
'        .MsgName = stNotifName
'        .MsgDest = stMsgDest
'        .MsgData(NS_MsgClass_IDX) = NS_MSG_CLASS_DATA_CHANGE
'
'        .MsgData(NS_DC_ChangeType_IDX) = stChangeType
'        .MsgData(NS_DC_DatasetName_IDX) = stDatasetName
'        .MsgData(NS_DC_ChangeDate_IDX) = stChangeDate
'        .MsgData(NS_DC_ItemID1_IDX) = stItemID1
'        .MsgData(NS_DC_ItemID2_IDX) = stItemID2
'        .MsgData(NS_DC_AuxData1_IDX) = stAuxData1
'        .MsgData(NS_DC_AuxData2_IDX) = stAuxData2
'        .MsgData(NS_DC_AuxData3_IDX) = stAuxData3
'        .MsgData(NS_DC_AuxData4_IDX) = stAuxData4
'        .MsgData(NS_DC_AuxData5_IDX) = stAuxData5
'
'        .MsgData(NS_DC_SiteCode_IDX) = gstUCurSite
'        .MsgData(NS_DC_EOCCode_IDX) = gstUCurEOCCode
'        .MsgData(NS_DC_HazardID_IDX) = glCurrHazID
'        .MsgData(NS_DC_ExerID_IDX) = lExerNum
'        .MsgData(NS_DC_FolderID_IDX) = lFolderID
'
'        If fAckString = False Then
'            .MsgData(NS_DC_AckFlag_IDX) = "N"
'        Else
'            .MsgData(NS_DC_AckFlag_IDX) = "Y"
'            stAckString = Replace(Replace(Replace(stAckString, vbCrLf, " "), "'", ""), "\", "")
'            .MsgData(NS_DC_AckString_IDX) = stAckString
'        End If
'
'        ' Set automatic data-driven parameter values
'        If tDDParams.stDDHandler <> "(none)" Then
'            tDDParams.stDDHandler = NS_DDHANDLER_ACTION
'            ulMsgFlags = ulMsgFlags Or NS_EF_TO_AUX_PROCESS
'        End If
'    End With
'
'    ' Add data-driven parameters to the message
'    If NsSetFMsgDDParams(oMsg, tDDParams) < 0 Then
'        Exit Function
'    End If
'
'    ' Post the message
'    If oMsg.PostMsg(ulLinkID, ulMsgFlags) = False Then
'        Exit Function
'    End If
'
'    ' Normal exit
'    NsPostDataChangeMsgDD = 0          'status = SUCCESS
'    Exit Function                       'all done
'
'    ' Error exit
'Error_Exit:
'    ifxMsgBox "NsPostDataChangeMsgDD: Error " & Err & ":  " & Error$
'    NsPostDataChangeMsgDD = -2         'return FAILURE
'End Function


'    Private Function NsPostMsg(ByVal ulLinkID As Long, ByVal ulMsgFlags As Long, tMsgData As NS_MSG_DATA) As Integer
'    '-------------------------------------------------------------------------
'    'Purpose: Post a generic notification message
'    '
'    'Usage:   iSC = NsPostMsg(ulLinkID, ulMsgFlags, tMsgData)
'    '
'    'Arguments:
'    '   ulLinkID    - notification link ID  (0 = Anonymous)
'    '   ulMsgFlags  - message attribute flags
'    '   tMsgData    - message data structure
'    '   iSC         - status code
'    '
'    'Outputs: none
'    '
'    'Returns: SUCCESS (0), status code (+), or error code (-)
'    '-------------------------------------------------------------------------
'    '
'    'Description:
'    '   This function posts a notification message from data in a generic
'    '   message structure.
'    '
'    '   If the message cannot be posted, the function returns a negative
'    '   status code.
'    '
'    '   Message attribute flags can be used to control how the message is
'    '   processed.  The following flags are available:
'    '
'    '       NS_EF_DEFAULT       - default attributes
'    '       NS_EF_NORMAL        - distributed to local clients and server
'    '       NS_EF_LOCAL_ONLY    - distributed only to clients on your PC
'    '       NS_EF_GLOBAL        - forwarded to ALL notification servers
'    '
'    '   These flags can be ORed together to produce any desired combination
'    '   of attributes.
'    '
'    '   To prevent notification loopback, each message is associated with an
'    '   message source ID (ulLinkID).  If the message source is unknown or
'    '   not important, you can use ID = 0 to make anonymous message postings.
'    '-------------------------------------------------------------------------
'
'    Dim tFevtData   As FEVTDATA2        'message data structure
'    Dim iRC         As Integer          'temp return code
'
'    ' Setup
'    On Error GoTo NsPostMsg_Err         'set error trap
'
'    ' Convert message to DLL format
'    Ns_MSGDATA_to_FEVTDATA tMsgData, tFevtData
'
'    ' Post message
'    iRC = AnPostEventLF(ulLinkID, ulMsgFlags, tFevtData)
'
'    ' Return result
'    NsPostMsg = iRC                     'status = return status code
'    Exit Function                       'all done
'
'    ' Error exit
'NsPostMsg_Err:
'    MsgBox "NsPostMsg: Error " & Err & ":  " & Error$(Err)
'    NsPostMsg = -1                    'return FAILURE
'    Exit Function                       'REQUIRED !!!
'End Function
'
'Private Function NsPostMsg3P(ByVal ulLinkID As Long, ByVal ulMsgFlags As Long, ByVal stMsgName As String, ByVal stMsgDest As String, ByVal pszParm1 As String, ByVal pszParm2 As String, ByVal pszParm3 As String) As Integer
'    '-------------------------------------------------------------------------
'    'Purpose: Post a notification message with 3 parameters
'    '
'    'Usage:   iSC = NsPostMsg3P (ulLinkID, ulMsgFlags, stMsgName, stMsgDest, pszParm1, pszParm2, pszParm3)
'    '
'    'Inputs:
'    '   ulLinkID    - notification link ID  (0 = Anonymous)
'    '   ulMsgFlags  - message attribute flags
'    '   stMsgName   - message name
'    '   stMsgDest   - message destination
'    '   pszParm1    - parameter 1 text
'    '   pszParm2    - parameter 2 text
'    '   pszParm3    - parameter 3 text
'    '   iSC         - status code
'    '
'    'Outputs: none
'    '
'    'Returns: SUCCESS (0), status code (+), or error code (-)
'    '-------------------------------------------------------------------------
'    '
'    'Description:
'    '   This function posts a notification message with three parameter values.
'    '
'    '   Message attribute flags can be used to control how the message is
'    '   processed.  The following flags are available:
'    '
'    '       NS_EF_DEFAULT       - default attributes
'    '       NS_EF_NORMAL        - distributed to local clients and server
'    '       NS_EF_LOCAL_ONLY    - distributed only to clients on your PC
'    '       NS_EF_GLOBAL        - forwarded to ALL notification servers
'    '
'    '   These flags can be ORed together to produce any desired combination of
'    '   attributes.
'    '
'    '   To prevent notification loopback, each message is associated with an
'    '   message source ID (ulLinkID).  If the message source is unknown or not
'    '   important, you can use ID = 0 to make anonymous message postings.
'    '-------------------------------------------------------------------------
'
'    Dim tFevtData   As FEVTDATA2        'message data structure
'    Dim iRC         As Integer          'temp return code
'
'    ' Setup
'    On Error GoTo NsPostMsg3P_Err       'set error trap
'
'    ' Initialize message data structure
'    iRC = AnInitFEVTDATA2(tFevtData, FEVTDATA_FMT_2)
'    If iRC < 0 Then                     'failure ?
'        NsPostMsg3P = iRC               'set return code
'        Exit Function                   'exit now
'    End If
'
'    ' Set message parameter values
'    AnFEVTDATA_SetMsgName tFevtData, stMsgName
'    AnFEVTDATA_SetMsgDest tFevtData, stMsgDest
'    AnFEVTDATA_SetMsgParm tFevtData, 0, pszParm1
'    AnFEVTDATA_SetMsgParm tFevtData, 1, pszParm2
'    AnFEVTDATA_SetMsgParm tFevtData, 2, pszParm3
'
'    ' Post message
'    iRC = AnPostEventLF(ulLinkID, ulMsgFlags, tFevtData)
'
'    ' Normal exit
'    NsPostMsg3P = iRC                   'return status code
'    Exit Function                       'all done
'
'    ' Error exit
'NsPostMsg3P_Err:
'    MsgBox "NsPostMsg3P: Error " & Err & ":  " & Error$(Err)
'    NsPostMsg3P = -1                    'return FAILURE
'    Exit Function                       'REQUIRED !!!
'End Function
'
'Private Function NxRegisterInterest(ByVal ulLinkID As Long, ByVal stMsgName As String, ByVal stMsgDest As String) As Integer
'    '-------------------------------------------------------------------------
'    'Purpose: Register interest in a notification message
'    '
'    'Usage:   <integer> = NxRegisterInterest (ulLinkID, stMsgName, stMsgDest)
'    '
'    'Inputs:  long   ulLinkID   - notification link ID
'    '         string stMsgName  - message name
'    '         string stMsgDest  - message destination
'    '
'    'Outputs: none
'    '
'    'Returns: SUCCESS (0), status code (+), or error code (-)
'    '
'    'Description:
'    '   This function registers a client's interest in a specific message and
'    '   enables the client to be notified whenever that message occurs.  This
'    '   interest remains in effect until the link is closed or
'    '   iNSRemoveInterest() is called.
'    '
'    'Note:  Since this function registers interest in a specific message name,
'    ' each unique message name must be registered separately.
'    '-------------------------------------------------------------------------
'
'    Dim iRC As Integer
'
'    ' Register interest
'    iRC = AnRegisterInterest(ulLinkID, stMsgName, stMsgDest)
'
'    ' Return result
'    NxRegisterInterest = iRC
'End Function
'
'Private Function NxRemoveInterest(ByVal ulLinkID As Long, ByVal stMsgName As String, ByVal stMsgDest As String) As Integer
'    '-------------------------------------------------------------------------
'    'Purpose: Remove interest in a notification message
'    '
'    'Usage:   <integer> = NxRemoveInterest (ulLinkID, stMsgName, stMsgDest)
'    '
'    'Inputs:  long   ulLinkID   - notification link ID
'    '         string stMsgName  - message name
'    '         string stMsgDest  - message destination
'    '
'    'Outputs: none
'    '
'    'Returns: SUCCESS (0), status code (+), or error code (-)
'    '
'    'Description:
'    '   This function removes a client's interest in a specific message and
'    '   removes the client from the distribution list for that message.
'    '
'    '   Note:  All active interests are automatically removed during by the
'    '   iNSCloseLink() function, so it is not necessary to remove interests
'    '   before closing a notification link.
'    '
'    'Note:  Since this function removes interest in a specific message name,
'    ' each unique message name must be removed separately.
'    '-------------------------------------------------------------------------
'
'    Dim iRC As Integer
'
'    ' Remove interest
'    iRC = AnRemoveInterest(ulLinkID, stMsgName, stMsgDest)
'
'    ' Return result
'    NxRemoveInterest = iRC
'End Function
'
'Private Sub NsSetDDParams(tDDParams As NS_DD_PARAMS, _
'                         ByVal stTableName As String, _
'                         ByVal stRowID As String, _
'                         ByVal vXmitInitDate As Date, _
'                         ByVal stAction As String, _
'                         ByVal stListener As String, _
'                         ByVal lDDFlags As Long)
'
'    '-------------------------------------------------------------------------
'    'Purpose: Populate a Data-Driven Notification parameter structure
'    'Returns: nothing
'    '-------------------------------------------------------------------------
'    'Arguments:
'    '   tDDParams           - parameter structure
'    '   stTableName         - primary table name
'    '   stRowID             - primary row ID
'    '   vXmitInitDate       - primary timestamp
'    '   stAction            - primary action
'    '   stListener          - database listener name
'    '   lDDFlags            - data-driven notification flags
'    '-------------------------------------------------------------------------
'    'Note: This function assumes that the target structure is already initialized.
'    '-------------------------------------------------------------------------
'
'    Const iIndex = 0            'array index for primary table
'
'    ' Populate parameter structure
'    NsSetDDParamsT1 tDDParams, stTableName, stRowID, vXmitInitDate, stAction
'    tDDParams.stListener = stListener                   'database listener name
'    tDDParams.lDDFlags = lDDFlags                       'data-driven flags
'    tDDParams.stEOCCode = gstUCurEOCCode                'EOC code
'End Sub
'
'Private Sub NsSetDDParamsT1(tDDParams As NS_DD_PARAMS, _
'                           ByVal stTableName As String, _
'                           ByVal stRowID As String, _
'                           ByVal vXmitInitDate As Date, _
'                           ByVal stAction As String)
'
'    '-------------------------------------------------------------------------
'    'Purpose: Set Data-Driven Notification parameters for primary table
'    'Returns: nothing
'    '-------------------------------------------------------------------------
'    'Arguments:
'    '   tDDParams           - parameter structure
'    '   stTableName         - table name
'    '   stRowID             - row ID
'    '   vXmitInitDate       - timestamp
'    '   stAction            - action
'    '-------------------------------------------------------------------------
'    'Note: This function assumes that the target structure is already initialized.
'    '-------------------------------------------------------------------------
'
'    Const iIndex = 0            'array index for this table
'
'    ' Set Data-Driven Notification parameters for table
'    NsSetDDParamsTN tDDParams, iIndex, stTableName, stRowID, vXmitInitDate, stAction
'End Sub
'
'Private Sub NsSetDDParamsT2(tDDParams As NS_DD_PARAMS, _
'                           ByVal stTableName As String, _
'                           ByVal stRowID As String, _
'                           ByVal vXmitInitDate As Date, _
'                           ByVal stAction As String)
'
'    '-------------------------------------------------------------------------
'    'Purpose: Set Data-Driven Notification parameters for second table
'    'Returns: nothing
'    '-------------------------------------------------------------------------
'    'Arguments:
'    '   tDDParams           - parameter structure
'    '   stTableName         - table name
'    '   stRowID             - row ID
'    '   vXmitInitDate       - timestamp
'    '   stAction            - action
'    '-------------------------------------------------------------------------
'    'Note: This function assumes that the target structure is already initialized.
'    '-------------------------------------------------------------------------
'
'    Const iIndex = 1            'array index for this table
'
'    ' Set Data-Driven Notification parameters for table
'    NsSetDDParamsTN tDDParams, iIndex, stTableName, stRowID, vXmitInitDate, stAction
'End Sub
'
'Private Sub NsSetDDParamsT3(tDDParams As NS_DD_PARAMS, _
'                           ByVal stTableName As String, _
'                           ByVal stRowID As String, _
'                           ByVal vXmitInitDate As Date, _
'                           ByVal stAction As String)
'
'    '-------------------------------------------------------------------------
'    'Purpose: Set Data-Driven Notification parameters for third table
'    'Returns: nothing
'    '-------------------------------------------------------------------------
'    'Arguments:
'    '   tDDParams           - parameter structure
'    '   stTableName         - table name
'    '   stRowID             - row ID
'    '   vXmitInitDate       - timestamp
'    '   stAction            - action
'    '-------------------------------------------------------------------------
'    'Note: This function assumes that the target structure is already initialized.
'    '-------------------------------------------------------------------------
'
'    Const iIndex = 2            'array index for this table
'
'    ' Set Data-Driven Notification parameters for table
'    NsSetDDParamsTN tDDParams, iIndex, stTableName, stRowID, vXmitInitDate, stAction
'End Sub
'
'Private Sub NsSetDDParamsTN(tDDParams As NS_DD_PARAMS, _
'                           ByVal iIndex As Integer, _
'                           ByVal stTableName As String, _
'                           ByVal stRowID As String, _
'                           ByVal vXmitInitDate As Date, _
'                           ByVal stAction As String)
'
'    '-------------------------------------------------------------------------
'    'Purpose: Set Data-Driven Notification parameters for a table, by index
'    'Returns: nothing
'    '-------------------------------------------------------------------------
'    'Arguments:
'    '   tDDParams           - parameter structure
'    '   iIndex              - DD table array index (0-based)
'    '   stTableName         - table name
'    '   stRowID             - row ID
'    '   vXmitInitDate       - timestamp
'    '   stAction            - action
'    '-------------------------------------------------------------------------
'    'Note: This function assumes that the target structure is already initialized.
'    '-------------------------------------------------------------------------
'
'    ' Check arguments
'    If iIndex < 0 Or iIndex > 2 Then Exit Sub           'exit if not valid
'
'    ' Set Data-Driven Notification parameters
'    tDDParams.stTableName(iIndex) = stTableName         'table name
'    tDDParams.stRowID(iIndex) = stRowID                 'row ID
'    tDDParams.vXmitInitDate(iIndex) = vXmitInitDate     'timestamp
'    tDDParams.stAction(iIndex) = stAction               'action
'End Sub
'
'Private Function NsSetFevtDDParams(tFevtData As FEVTDATA2, tDDParams As NS_DD_PARAMS) As Integer
'    '-------------------------------------------------------------------------
'    'Purpose: Set the Data-Driven parameters in an FEVTDATA structure
'    '
'    'Usage:   iSC = NsSetFevtDDParams (tFevtData, tDDParams)
'    '
'    'Arguments:
'    '   tFevtData    - message data structure
'    '   tDDParams   - data-driven processing parameters
'    '   iSC         - status code
'    '
'    'Outputs: none
'    '
'    'Returns: SUCCESS (0), status code (+), or error code (-)
'    '-------------------------------------------------------------------------
'    'Note: This function assumes that the target structure is already initialized.
'    '-------------------------------------------------------------------------
'
'    Dim stXmitInitDate(3) As String     'timestamp strings
'
'    ' Prepare values
'    If tDDParams.vXmitInitDate(0) <> 0 Then _
'        stXmitInitDate(0) = Format$(tDDParams.vXmitInitDate(0), NS_DD_XmitInitDate_FMT)
'    If tDDParams.vXmitInitDate(1) <> 0 Then _
'        stXmitInitDate(1) = Format$(tDDParams.vXmitInitDate(1), NS_DD_XmitInitDate_FMT)
'    If tDDParams.vXmitInitDate(2) <> 0 Then _
'        stXmitInitDate(2) = Format$(tDDParams.vXmitInitDate(2), NS_DD_XmitInitDate_FMT)
'
'    ' Set message parameter values
'    AnFEVTDATA_SetMsgParm tFevtData, NS_DD_TblName1_IDX, tDDParams.stTableName(0)
'    AnFEVTDATA_SetMsgParm tFevtData, NS_DD_TblName2_IDX, tDDParams.stTableName(1)
'    AnFEVTDATA_SetMsgParm tFevtData, NS_DD_TblName3_IDX, tDDParams.stTableName(2)
'    AnFEVTDATA_SetMsgParm tFevtData, NS_DD_RowID1_IDX, tDDParams.stRowID(0)
'    AnFEVTDATA_SetMsgParm tFevtData, NS_DD_RowID2_IDX, tDDParams.stRowID(1)
'    AnFEVTDATA_SetMsgParm tFevtData, NS_DD_RowID3_IDX, tDDParams.stRowID(2)
'    AnFEVTDATA_SetMsgParm tFevtData, NS_DD_XmitInitDate1_IDX, stXmitInitDate(0)
'    AnFEVTDATA_SetMsgParm tFevtData, NS_DD_XmitInitDate2_IDX, stXmitInitDate(1)
'    AnFEVTDATA_SetMsgParm tFevtData, NS_DD_XmitInitDate3_IDX, stXmitInitDate(2)
'    AnFEVTDATA_SetMsgParm tFevtData, NS_DD_Action1_IDX, tDDParams.stAction(0)
'    AnFEVTDATA_SetMsgParm tFevtData, NS_DD_Action2_IDX, tDDParams.stAction(1)
'    AnFEVTDATA_SetMsgParm tFevtData, NS_DD_Action3_IDX, tDDParams.stAction(2)
'    AnFEVTDATA_SetMsgParm tFevtData, NS_DD_Listener_IDX, tDDParams.stListener
'    AnFEVTDATA_SetMsgParm tFevtData, NS_DD_DDFlags_IDX, tDDParams.lDDFlags
'    AnFEVTDATA_SetMsgParm tFevtData, NS_DD_EOCCode_IDX, tDDParams.stEOCCode
'    AnFEVTDATA_SetMsgParm tFevtData, NS_DD_DDHandler_IDX, tDDParams.stDDHandler
'
'    ' Set AuxProcess info
'    AnFEVTDATA_SetAuxProcessIdent tFevtData, tDDParams.stDDHandler
'
'    ' Return result
'    NsSetFevtDDParams = 0               'status = SUCCESS
'End Function
'
'Private Function NxVerifyInterest(ByVal ulLinkID As Long, ByVal stMsgName As String, ByVal stMsgDest As String) As Integer
'    '-------------------------------------------------------------------------
'    'Purpose: Verify interest in a notification message
'    '
'    'Usage:   <integer> = NxVerifyInterest (ulLinkID, stMsgName, stMsgDest)
'    '
'    'Inputs:  long   ulLinkID   - notification link ID
'    '         string stMsgName  - message name
'    '         string stMsgDest  - message destination
'    '
'    'Outputs: none
'    '
'    'Returns: True or False
'    '
'    'Description:
'    '   This function verifies a client's interest in a specific message.
'    '
'    'Note:  Since this function verifies interest in a specific message name,
'    ' each unique message name must be verified separately.
'    '-------------------------------------------------------------------------
'
'    Dim iRC As Integer
'
'    ' Verify interest
'    iRC = AnVerifyInterest(ulLinkID, stMsgName, stMsgDest)
'
'    ' Return result
'    If iRC = 0 Then
'        NxVerifyInterest = False
'    Else
'        NxVerifyInterest = True
'    End If
'End Function
'
'Private Function stAnsiStrToVB(ByVal pszAnsiStr As String) As String
'    '-------------------------------------------------------------------------
'    'Purpose: Convert a null-terminated ANSI string to a standard VB string
'    '
'    'Usage:   <string> = stAnsiStrToVB (pszAnsiStr)
'    '
'    'Inputs:  pszAnsiStr - null-terminated ANSI string or byte array
'    '
'    'Returns: VB string, without a null terminator
'    '-------------------------------------------------------------------------
'    '
'    'Description:
'    '   This function converts a null-terminated ANSI string or byte array to
'    '   standard Visual Basic string.  This is useful for converting strings
'    '   used in C or DLL functions.
'    '
'    '-------------------------------------------------------------------------
'    'Note:  This function does not work with Unicode strings, since they use
'    '   two bytes per character and may contain embedded zeros.
'    '-------------------------------------------------------------------------
'    'Warning: DO NOT remove ByVal qualifiers from the argument list!!  They are
'    '   needed for automatic type conversions.
'    '-------------------------------------------------------------------------
'    'Warning: This function is for internal use only and may change without
'    '   notice.
'    '-------------------------------------------------------------------------
'
'    Dim stStrBuf As String          'local buffer
'    Dim iSLen As Integer            'source string length
'    Dim i As Integer                'loop index
'    Dim c As Integer                'character code
'
'    ' Setup
'    iSLen = LenB(pszAnsiStr)            'source string length, in bytes
'
'    ' Convert string
'    For i = 1 To iSLen
'        c = AscB(MidB(pszAnsiStr, i, 1))  'get source character code
'        If c = 0 Then                   'null terminator?
'            Exit For                    'end of conversion
'        End If
'        stStrBuf = stStrBuf & Chr$(c)   'add character to result
'    Next
'
'    ' Return result
'    stAnsiStrToVB = stStrBuf
'End Function
'
'Private Function stByteStrToVB(pszByteStr() As Byte) As String
'    '-------------------------------------------------------------------------
'    'Purpose: Convert null-terminated byte string to standard VB string
'    '
'    'Usage:   <string> = stByteStrToVB (pszByteStr)
'    '
'    'Inputs:  pszByteStr - null-terminated byte string
'    '
'    'Returns: Normal VB string, without null terminator
'    '-------------------------------------------------------------------------
'    '
'    'Description:
'    '   This function converts a null-terminated byte string to standard Visual
'    '   Basic string.  This is useful for converting strings used in C or DLL
'    '   functions.
'    '
'    '-------------------------------------------------------------------------
'    'Warning: DO NOT remove ByVal qualifiers from the argument list!!  They are
'    '   needed for automatic type conversions.
'    '-------------------------------------------------------------------------
'    'Warning: This function is for internal use only and may change without
'    '   notice.
'    '-------------------------------------------------------------------------
'
'    Dim stStrBuf As String          'local buffer
'    Dim iSLen As Integer            'source string length
'    Dim i As Integer                'loop index
'    Dim c As Integer                'character code
'
'    ' Setup
'    iSLen = 20000                       'use a high limit to avoid truncation
'
'    ' Convert string
'    For i = 1 To iSLen
'        c = pszByteStr(i - 1)           'get source character code
'        If c = 0 Then                   'null terminator?
'            Exit For                    'end of conversion
'        End If
'        stStrBuf = stStrBuf & Chr$(c)   'add character to result
'    Next
'
'    ' Return result
'    stByteStrToVB = stStrBuf
'End Function
'Private Function stVBToCStr(ByVal pszVBStr As String) As String
'    '-------------------------------------------------------------------------
'    'Purpose: Convert standard VB string to null-terminated VB string
'    '
'    'Usage:   <string> = stVBToCStr (pszVBStr)
'    '
'    'Inputs:  string pszCString - standard VB string
'    '
'    'Returns: VB string with null terminator
'    '-------------------------------------------------------------------------
'    '
'    'Description:
'    '   This function adds a null-terminator character to a VB string and is
'    '   useful for converting strings used in C functions.
'    '
'    '-------------------------------------------------------------------------
'    'Warning: DO NOT remove ByVal qualifiers from the argument list!!  They are
'    '   needed for automatic type conversions.
'    '-------------------------------------------------------------------------
'    'Warning: This function is for internal use only and may change without
'    '   notice.
'    '-------------------------------------------------------------------------
'
'    Dim stOutBuf As String
'    Dim iSLen As Integer
'
'    ' Add null terminator
'    stOutBuf = pszVBStr & Chr$(0)
'
'    ' Return result
'    stVBToCStr = stOutBuf
'End Function
'
'Private Function ulNSOpenQLink(ByVal ulLinkFlags As Long, frm As Form) As Long
'    '-------------------------------------------------------------------------
'    'Purpose: Open a queued notification client link
'    '
'    'Usage:   <long> = ulNSOpenQLink (ulLinkFlags, frm)
'    '
'    'Inputs:  long  ulLinkFlags - link attribute flags  (0 = none)
'    '         form  frm         - client form
'    '
'    'Outputs: none
'    '
'    'Returns: link ID (non-zero) or FAILURE (zero)
'    '-------------------------------------------------------------------------
'    '
'    'Description:
'    '   This function opens a queued notification client link and returns a unique
'    '   link ID (0 = failure).  This link ID is used in all future operations
'    '   for that link.
'    '
'    '   Link attribute flags can be used to control how the link operates.
'    '   The following flags are available:
'    '
'    '       NS_LF_DEFAULT       - default attributes
'    '       NS_LF_NORMAL        - no loopback
'    '       NS_LF_LOOPBACK_OK   - allows link to notify itself
'    '
'    '   These flags can be ORed together to produce any desired combination of
'    '   attributes.
'    '
'    'Caution:  Each VB form that creates a notification link should call
'    ' iNSCloseLink() in its shutdown process (Form_Unload event).  Otherwise,
'    ' the notification link becomes an orphan and its resources are never
'    ' released.
'    '-------------------------------------------------------------------------
'
'    Dim ulLinkID As Long
'
'    ' Set error trap
'    On Error Resume Next    'needed if frm object variable is not set
'
'    ' Initialize globals, if necessary
'    If giNotifWinMsgNo = 0 Then
'        InitNotifGlobals
'    End If
'
'    ' Add default flags
'    ulLinkFlags = ulLinkFlags Or NS_LF_LATEST_INSTANCE_ONLY
'
'    ' Open notification link
'    ulLinkID = AnOpenLinkWMQ(ulLinkFlags, frm.hwnd, giNotifWinMsgNo)
'
'    ' Return result
'    ulNSOpenQLink = ulLinkID
'End Function
'
Private Function ulNSOpenQLinkX(ByVal ulLinkFlags As Long, ByVal hwnd As Integer, ByVal ulQueueLimit As Long, ByVal ulQueueFlags As Long) As Long
    '-------------------------------------------------------------------------
    'Purpose: Open a queued notification client link
    '
    'Usage:   <long> = ulNSOpenQLink (ulLinkFlags, frm)
    '
    'Inputs:  long  ulLinkFlags  - link attribute flags  (0 = none)
    '         int   hWnd         - client window handle
    '         long  ulQueueLimit - queue size limit     (0 = none)
    '         long  ulQueueFlags - queue control flags  (0 = none)
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
    '   Queue control flags can be used to control how the queue operates.  The
    '   following flags are available:
    '
    '       NS_QF_DEFAULT               - default flags
    '       NS_QF_LATEST_INSTANCE_ONLY  - keep only latest instance of each message type
    '
    'Caution:  Each VB form that creates a notification link should call
    ' iNSCloseLink() in its shutdown process (Form_Unload event).  Otherwise,
    ' the notification link becomes an orphan and its resources are never
    ' released.
    '-------------------------------------------------------------------------

    Dim ulLinkID As Long

    ' Set error trap
    On Error Resume Next    'needed if DLL function is not available

    ' Initialize globals, if necessary
    If giNotifWinMsgNo = 0 Then
        InitNotifGlobals
    End If

    ' Open notification link
    ulLinkID = AnOpenLinkWMQF(ulLinkFlags, hwnd, giNotifWinMsgNo, ulQueueLimit, ulQueueFlags)

    ' Return result
    ulNSOpenQLinkX = ulLinkID
End Function

Private Function vNSQueryEventBoardTime(ByVal stMsgName As String, ByVal stMsgDest As String) As Variant
    '-------------------------------------------------------------------------
    'Purpose: Get message board timestamp in VB format
    '
    'Usage:   <variant> = vNSQueryEventBoardTime (stMsgName, stMsgDest)
    '
    'Inputs:  string stMsgName  - message name
    '         string stMsgDest  - message destination
    '
    'Outputs: none
    '
    'Returns: timestamp (VB date value)
    '-------------------------------------------------------------------------
    '
    'Description:
    '   This function gets an message board timestamp in VB format.
    '-------------------------------------------------------------------------

    Dim ulTimeSec As Long
    Dim vEvtTime As Variant

    ' Query message board
    ulTimeSec = AnQueryEventBoardTimeSec(stMsgName, stMsgDest)
    vEvtTime = vTimeSMToVB(ulTimeSec, 0)

    ' Return result
    vNSQueryEventBoardTime = vEvtTime
End Function

Private Function vTimeSMToVB(ByVal ulSec As Long, ByVal usMsec As Integer) As Variant
    '-------------------------------------------------------------------------
    'Purpose: Convert second and millisecond to VB date.
    '
    'Usage:   <variant> = vTimeSMToVB (ulSec, usMSec)
    '
    'Inputs:  long              - seconds
    '         integer  usMsec   - milliseconds
    '
    'Outputs: none
    '
    'Returns: VB date value
    '-------------------------------------------------------------------------
    '
    'Description:
    '   This function converts second and millisecond to a VB date, using
    '   Jan. 1, 1970 as the base point.
    '
    'Warning:  This function is for internal use only and may change without
    ' notice.
    '-------------------------------------------------------------------------

    Dim vTime As Variant
    Dim stCTime As String
    Dim lRC As Long

    ' Setup
    stCTime = String$(32, 0)

    ' Check arguments
    If ulSec = -1 Or ulSec = 0 Then
        vTimeSMToVB = 0     'no time
        Exit Function
    End If

    ' Get CTime string
    lRC = AnSecMsecToCTimeStr(ulSec, usMsec, stCTime)
    'stCTime = stAnsiStrToVB(stCTime)       'remove null terminator
    If stCTime <> "" Then
        stCTime = Mid$(stCTime, 5, 7) & Mid$(stCTime, 21, 4) & Mid$(stCTime, 11, 9)
        vTime = CVDate(stCTime)
    End If

    'test line
    'MsgBox "vTimeSMToVB: sec = " & ulSec & ", CTime = '" & stCTime & "', VB time = " & vTime

    ' Return result
    vTimeSMToVB = vTime
End Function


Private Sub Class_Initialize()

Dim objFSO As New FileSystemObject
Dim inFile As File
Dim inStream As TextStream
Dim strTemp As String
Dim GetEXEPath As String

    GetEXEPath = App.Path + "\KP_API.INI"
    
    'If database has changed then, we need to change the connection to the database in Db_connect.
    'It may be worthwhile to add this information in the ini file.

    ' open the ini file and store the global constants
   ' Set inFile = objFSO.GetFile("c:\WINNT\EA_API.INI")
    'Set the infile to be along with the executable to avoid the winNT/ Windows XP problem
    Set inFile = objFSO.GetFile(GetEXEPath)
    ' set up the file to be read
    Set inStream = inFile.OpenAsTextStream(ForReading, TristateFalse)
    ' read in the global constants from the ini file
    gstUCurEOCCode = inStream.ReadLine
    gstUCurEOCName = inStream.ReadLine
    gsEID = CInt(inStream.ReadLine)
    
    'glFolderID = CLng(inStream.ReadLine) 'this is being reset when we loadcurOperationID
    gstMessageDest = inStream.ReadLine
    gstUListener = inStream.ReadLine
    gstNotifServerName = inStream.ReadLine
    gstNotifServerPort = CInt(inStream.ReadLine)
    gstUCurDataset = inStream.ReadLine
    gstUListenerNum = inStream.ReadLine
    gstCurHazName = inStream.ReadLine
    gstSiteName = inStream.ReadLine 'MORE
    
       
    
    ' close the file
    inStream.Close
    Set inStream = Nothing
    Set inFile = Nothing
    Set objFSO = Nothing
    giMinsFromGMT = lReadRegistry(HKEY_LOCAL_MACHINE, "SYSTEM\CurrentControlSet\Control\TimeZoneInformation", "ActiveTimeBias")
    If OpenDB Then
    
        glCurrHazID = GetHazIDFromName(gstCurHazName, "G_DEF_HAZARD_CLASS", "HAZARD_ID", "HAZARD_NAME", gstUCurEOCName)
        If glCurrHazID < 1 Then
            MsgBox "The Current Hazard ID is not valid.  The current Hazard Name = " & gstCurHazName
        End If
        
            
        If Not LoadCurrOpIDs(gsEID, glCurrHazID, gstUCurEOCName, gstUCurEOCCode, gstSiteName) Then
            MsgBox "Exiting from LoadCurrOpIds Exer#" & gsEID & "HazID =" & glCurrHazID & "EOC Name= " & gstUCurEOCName & "EOCCODE= " & gstUCurEOCCode & "Site Name= " & gstSiteName

        End If
    Else
        'Failed to connect
        MsgBox "Unable to connect to the database Exer#" & gsEID & " HazID =" & glCurrHazID & " EOC Name= " & gstUCurEOCName & " EOCCODE= " & gstUCurEOCCode & " Site Name= " & gstSiteName


    End If
    

    
End Sub


Public Function SavePADetail(ByVal lParPadID As Long, _
                            ByVal stPAUnitName As String, _
                            ByVal stPAUnitType As String, _
                            ByVal intPaindex As Integer, _
                            ByVal lModelImpactTime As Long, _
                            ByVal lModelTipTime As Long, _
                            ByVal lModelTailtime As Long, _
                            ByVal lProtectiveActionTime As Long, _
                            ByVal stLink As String) As Boolean
                            

Dim stQ     As String
Dim rsList As New ADODB.Recordset
Dim IsShared As Long
Dim stTimeNow As String
Dim lPAUnitID As Long
Dim lGeoObjectID As Long
Dim stURLLink As String



 On Error GoTo SaveDberr
  SavePADetail = False
  'If BuildRecom Then
    'MsgBox "Yeahhhh!!!"
  'End If
  
  
  stTimeNow = stTO_DATE(CStr(vZuluNow))
  IsShared = True

'Read some file and get all the information about all the zonenames, and times from the file.

'Since the facility always take the same PA decision as the zone we need not write a seperate pa decision
    If stPAUnitType = "point" Then
            lGeoObjectID = GetIDFromName(stPAUnitName, "FACILITY", "GEO_OBJECT_ID", "FACILITY_NAME", gstUCurEOCName)
            lPAUnitID = GetIDFromName(CStr(lGeoObjectID), "PA_UNIT", "PA_UNIT_ID", "GEO_OBJECT_ID", gstUCurEOCName)
            
            
    ElseIf stPAUnitType = "zone" Then
            lPAUnitID = GetIDFromName(stPAUnitName, "PA_UNIT", "PA_UNIT_ID", "ZONE_NAME", gstUCurEOCName)

    End If
    
    If lPAUnitID > 0 Then
    
   
    stQ = "INSERT INTO PARPAD_DETAIL (pa_unit_id, parpad_id, exercise_num, "
            stQ = stQ & "prot_act_index, model_impact_time, model_tip_time, "
            stQ = stQ & "model_tail_time, palt_time, folder_id, xmit_init_date) "
            stQ = stQ & "VALUES (" & lPAUnitID & ", " & lParPadID & ", " & gsEID & ", "
            stQ = stQ & intPaindex & ", " & lModelImpactTime & ", " & lModelTipTime & ", "
            stQ = stQ & lModelTailtime & ", " & lProtectiveActionTime & ", " & glFolderID & ", "
            'If IsTypePRT Or IsTypePDT Or IsTypePDD Then
                'stQ = stQ & "NULL) "
            'Else
                stQ = stQ & IIf(IsShared, stTimeNow, "NULL") & ") "
            'End If
             gdbFemis.Execute (stQ)
             
        '--------------------------------------------------------------------------
        ' Now that we've inserted the record, try to update the URL_LINK field
        ' Note that this field is new, and may not exist on older databases.   That
        ' is the reason I'm changing to "on error resume next" to make this backward
        ' compatable.
        '--------------------------------------------------------------------------
        On Error Resume Next
        stURLLink = Mid$(stLink, 1, 255) 'Database field is 255
        stQ = "UPDATE PARPAD_DETAIL SET URL_LINK = " & Enquote(stURLLink) & _
              " WHERE pa_unit_id = " & lPAUnitID & _
              " AND parpad_id = " & lParPadID & _
              " AND exercise_num = " & gsEID & _
              " AND prot_act_index = " & intPaindex

         gdbFemis.Execute (stQ)
             
        SavePADetail = True
    End If
    
    
    Exit Function
SaveDberr:
    SavePADetail = False
    Exit Function

    
    
End Function



'**-----------------------------------------------------------------**
'** Return a Long as fetched from the Windows Registry.             **
'**                                                                 **
'** Date      Dev.  Revision Notes                                  **
'** --------  ----  ----------------------------------------------  **
'** 19980127  PDG   Code Standardization.                           **
'**-----------------------------------------------------------------**
'
Public Function lReadRegistry(ByVal hKeyRoot As Long, _
                              ByVal stKeyTree As String, _
                              ByVal stItem As String) As Long
                              
    On Error GoTo lReadRegistryErr
    
    Dim hKey        As Long     '** Registry Key address
    Dim lRetCode    As Long     '** API Return Code
    Dim lFetch      As Long     '** Fetched value
    Dim lType       As Long     '** Type of data fetch
    Dim lSize       As Long     '** Size of data fetch
    
    lReadRegistry = 0
    
    If (ERROR_SUCCESS = RegOpenKey(hKeyRoot, stKeyTree, hKey)) Then
        lFetch = 0
        lSize = 4
        lType = REG_DWORD
        
        lRetCode = RegQueryValueLong(hKey, stItem, 0, lType, lFetch, lSize)
        If (lRetCode = ERROR_SUCCESS) And (lType = REG_DWORD) Then
            lReadRegistry = lFetch
        End If
        
        Call RegCloseKey(hKey)
    End If
    Exit Function
    
lReadRegistryErr:
    Exit Function
    
End Function


