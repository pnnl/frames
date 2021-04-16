Attribute VB_Name = "KMN_NOTIFICATION"
Option Explicit
'   03/20/97, MJB.  Moved stU_NOTIF_TEXT here & made global...
'   01/12/98, TRD.  Added a bunch of constants for data driven notification and standards

Const RCSid = "$Id: k_notif.bas 1.1 2005/05/27 16:30:22Z d3k077 Exp d3k077 $"

'=========================================================
'   Notification stuff
'=========================================================
'**stU_NOTIF_EVENT  = ON, OFF, MOD for messages to user & new data here.
'**stU_NOTIF_EVENT2 = Modified, Ended, Declared for offpost logging.
'**stU_NOTIF_EVENT3 = All Over, All EMIS over.
'**stU_NOTIF_EVENT4 = Multiple events ended (just from DEI/EMIS for now).
'**stU_NOTIF_EVENT5 = Event date changed.
'**stU_NOTIF_EVENT6 = Set Event changed.

'==========================================================
' Old notif stuff starts here (01/12/98)
'==========================================================

Public Const stU_NOTIF_EVENT            As String = "CSEPPEvent"
Public Const stU_NOTIF_EVENT2           As String = "CSEPPEvent2"
Public Const stU_NOTIF_EVENT3           As String = "CSEPPEvent3"
Public Const stU_NOTIF_EVENT4           As String = "CSEPPEvent4"
Public Const stU_NOTIF_EVENT5           As String = "CSEPPEvent5"
Public Const stU_NOTIF_EVENT6           As String = "CSEPPEvent6"
Public Const stU_NOTIF_EVENTCLASS       As String = "CSEPPEventClass"
Public Const stU_NOTIF_PLANPROF         As String = "UPlanProfile"    '!Dataset specific
Public Const stU_NOTIF_COMMUNITY        As String = "UCommunity"
Public Const stU_NOTIF_WORKPLAN         As String = "UWorkplan"
Public Const stU_NOTIF_FACILITY         As String = "UFacility"
Public Const stU_NOTIF_FACILITY_DEL     As String = "UFacilityDel"
Public Const stU_NOTIF_DEPARTMENT       As String = "UDept"
Public Const stU_NOTIF_DEPARTMENT_DEL   As String = "UDeptDel"
Public Const stU_NOTIF_RESTYPE          As String = "UResType"
Public Const stU_NOTIF_PERSON           As String = "UPerson"
Public Const stU_NOTIF_PERSON_DEL       As String = "UPersonDel"
Public Const stU_NOTIF_AGENCY           As String = "UAgency"
Public Const stU_NOTIF_AGENCY_DEL       As String = "UAgencyDel"
Public Const stU_NOTIF_MOU              As String = "UMou"
Public Const stU_NOTIF_LOCALID          As String = "ULocalID"
Public Const stU_NOTIF_MATRIX           As String = "UMatrix"           '**gsEId always=0
Public Const stU_NOTIF_NEWONPOSTDATA    As String = "UNewOnPostData"
Public Const stU_NOTIF_PAR              As String = "UPAR"              '!Dataset specific
Public Const stU_NOTIF_PAD              As String = "UPAD"              '!Dataset specific
Public Const stU_NOTIF_PALT             As String = "UPALT"
Public Const stU_NOTIF_THREAT           As String = "UThreat"
Public Const stU_NOTIF_RISKAREA         As String = "URiskArea"
Public Const stU_NOTIF_BROKELOCK        As String = "UBrokeLock"
Public Const stU_NOTIF_DELEXER          As String = "UDelExer"
Public Const stU_NT_DELDS               As String = "UDelDataset"
Public Const stU_NOTIF_TEXT             As String = "GENTEXT"
Public Const stU_NOTIF_RES_ASS          As String = "UResAss"
Public Const stU_NOTIF_RES_ASS_DEL      As String = "UResAssDel"

'**Notification Subjects...
Public Const stU_WPD2       As String = "WP_to_D2"      '!Notification interest for Work Plan/D2
Public Const stU_D2WP       As String = "D2_to_WP"
Public Const stU_CAID2      As String = "CAI_to_D2"     '!Notification interest for CAI/D2
Public Const stU_D2CAI      As String = "D2_to_CAI"
Public Const stU_CAI_MSB    As String = "CAI_to_MSB"    '!Notification interest for model status board
Public Const stU_MSB_CAI    As String = "MSB_to_CAI"
Public Const stU_RUND2      As String = "RunD2"

'**Notification stuff from Workplan to D2 and back.  (TRD & MJB)
'**1st Param=one of following, 2nd Param=D2CaseID
Public Const stU_WPD2_VIEW      As String = "View_D2"      'View only the passed caseID (disable sending back)
Public Const stU_WPD2_EDIT      As String = "Edit_D2"      'Edit the passed caseID
Public Const stU_D2WP_REPLY     As String = "Reply_WP"     'Got message from WP
Public Const stU_D2WP_UPDATE    As String = "Update_WP"    'Update workplan with CaseID
Public Const stU_D2WP_ERROR     As String = "Error"        'D2 had an error
'**CAI<->D2 notification messages
Public Const stU_CAID2_VIEW     As String = "View_D2"      'View only the passed caseID (disable sending back)
Public Const stU_CAID2_EDIT     As String = "Edit_D2"      'Edit the passed caseID
Public Const stU_D2CAI_REPLY    As String = "Reply_CAI"    'Got message from WP
Public Const stU_D2CAI_UPDATE   As String = "Update_CAI"   'Update workplan with CaseID
'**Model Status Board<->D2 notification messages
Public Const stU_MSB_CAI_REPLY  As String = "Reply_MSBCAI" 'Got message from model status board

'==========================================================
' Old notif stuff ends here (01/12/98)
' New notif stuff starts here (01/12/98)
'==========================================================

'==========================================================
' Below are constants used for data change notifications(01/12/98, TRD)
'==========================================================

'Possible NotifName values for data change notifications(01/12/98, TRD)
Public Const NOTIF_CHG_EVENT        As String = "Event Data Change"
Public Const NOTIF_CHG_EMERGENCY    As String = "Emergency Data Change"
Public Const NOTIF_CHG_WORK_PLAN    As String = "Work Plan Data Change"
Public Const NOTIF_CHG_WP_ACT_LIB   As String = "Work Plan Activity Data Change"    'Added 20000104 PDG
Public Const NOTIF_CHG_D2PC         As String = "D2PC Data Change"
Public Const NOTIF_CHG_THREAT       As String = "Threat Data Change"
Public Const NOTIF_CHG_RISK         As String = "Risk Data Change"
Public Const NOTIF_CHG_PAR          As String = "PAR Data Change"
Public Const NOTIF_CHG_PAD          As String = "PAD Data Change"
Public Const NOTIF_CHG_PLAN         As String = "Plan Data Change"
Public Const NOTIF_CHG_TASK         As String = "Task Data Change"
Public Const NOTIF_CHG_EVAC         As String = "Evac Data Change"
Public Const NOTIF_CHG_FOLDER       As String = "Folder Data Change"
Public Const NOTIF_CHG_HCO_OBJECT   As String = "HCO Data Change"
Public Const NOTIF_CHG_NAV_OBJECT   As String = "Nav Object Data Change"
Public Const NOTIF_CHG_COMMUNITY    As String = "Community Data Change"
Public Const NOTIF_CHG_FACILITY     As String = "Facility Data Change"
Public Const NOTIF_CHG_RES_DEF      As String = "Resource Def Data Change"
Public Const NOTIF_CHG_RES_ASS      As String = "Resource Assign Data Change"
Public Const NOTIF_CHG_AGENCY       As String = "Agency Data Change"
Public Const NOTIF_CHG_DEPT         As String = "Dept Data Change"
Public Const NOTIF_CHG_MOU          As String = "MOU Data Change"
Public Const NOTIF_CHG_PERSONNEL    As String = "Personnel Data Change"
Public Const NOTIF_CHG_TK_NOK       As String = "Next Of Kin Data Change"       'Added 19980310 PDG
Public Const NOTIF_CHG_TK_PERSON    As String = "Tracked Person Data Change"    'Added 19980310 PDG
Public Const NOTIF_CHG_IGLOO        As String = "Igloo Data Change"             'Added 19991001 PDG
Public Const NOTIF_CHG_KP           As String = "Known Point Data Change"       'Added 03-Mar-1998 RJC
Public Const NOTIF_CHG_KP_CLASS     As String = "KP Class Data Change"          'Added 19981104 TRD
Public Const NOTIF_CHG_KP_SUBCLASS  As String = "KP SubClass Data Change"       'Added 19981201 TRD
Public Const NOTIF_CHG_KPOLY        As String = "Known Polygon Data Change"     'Added 25-Jan-1999 RJC
Public Const NOTIF_CHG_MET          As String = "Met Data Change"
Public Const NOTIF_CHG_MET_TOWER    As String = "Met Tower Data Change"         'Added 22-Oct-99  RJC
Public Const NOTIF_CHG_MET_CLUSTER  As String = "Met Cluster Data Change"       'Added 22_Oct-99  RJC
Public Const NOTIF_CHG_CAI          As String = "CAI Data Change"
Public Const NOTIF_CHG_CASUALTY     As String = "Casualty Data Change"
Public Const NOTIF_CHG_EVACUEE      As String = "Evacuee Data Change"
Public Const NOTIF_CHG_SHELTER      As String = "Shelter Data Change"
Public Const NOTIF_CHG_SD_DATA      As String = "Status Board Data Change"
Public Const NOTIF_CHG_SD_DESIGN    As String = "Status Board Design Change"
Public Const NOTIF_CHG_EV_LOG       As String = "Event Log Data Change"
Public Const NOTIF_CHG_JOURNAL      As String = "Journal Data Change"
Public Const NOTIF_CHG_SH_EV_LOG    As String = "Shared Event Log Data Change"
Public Const NOTIF_CHG_SH_REPORT    As String = "Shared Report Data Change"
Public Const NOTIF_CHG_SH_JOURNAL   As String = "Shared Journal Data Change"
Public Const NOTIF_CHG_LOCK         As String = "Lock Data Change"
Public Const NOTIF_CHG_EXER_SETUP   As String = "Exercise Setup Change"
Public Const NOTIF_CHG_HAZ_DEF      As String = "Hazard Definition Data Change" 'Added 20000105 for SAS per TRD request
Public Const NOTIF_CHG_FFL_LOCAL    As String = "Free Form Log Data Change"         'Updated 20000120 LKA
Public Const NOTIF_CHG_FFL_SHARED   As String = "Shared Free Form Log Data Change"  'Added 20000120 LKA
Public Const NOTIF_CHG_DAI          As String = "DAI Data Change"                   'Added 03/17/2000, TRD.
Public Const NOTIF_CHG_RAD          As String = "RAD Data Change"                   'Added 05/26/2000, PDG.
Public Const NOTIF_CHG_MODE         As String = "Mode Change"                       'Added 01/17/2001, SAS

Public Const NOTIF_CHG_MAP_FAC      As String = "Facility Map Change"
Public Const NOTIF_CHG_MAP_MET      As String = "MET Map Change"
Public Const NOTIF_CHG_MAP_KP       As String = "Known Point Map Change"
Public Const NOTIF_CHG_MAP_KP_CLASS As String = "KP Class Map Change"
Public Const NOTIF_CHG_MAP_KP_SUBCLASS As String = "KP SubClass Map Change"
Public Const NOTIF_CHG_MAP_KPOLY    As String = "Known Polygon Map Change"
Public Const NOTIF_CHG_MAP_IGLOO    As String = "Igloo Map Change"              'Added 20000119 PDG
'Public Const NOTIF_CHG_MAP_D2PC     As String = "D2PC Map Change"
Public Const NOTIF_CHG_MAP_THREAT   As String = "Threat Map Change" 'OBSOLETE--Don't use
'Public Const NOTIF_CHG_MAP_RISK     As String = "Risk Map Change"
'Public Const NOTIF_CHG_MAP_PAR      As String = "PAR Map Change"
'Public Const NOTIF_CHG_MAP_PAD      As String = "PAD Map Change"
'Public Const NOTIF_CHG_MAP_EVAC     As String = "Evac Map Change"

Public Const NOTIF_CHG_REPL_FAIL    As String = "Replication Failure"


'Possible ChangeType values for data change notifications(01/12/98, TRD)
Public Const NOTIF_CHG_TYPE_ADD                 As String = "Add"
Public Const NOTIF_CHG_TYPE_DEL                 As String = "Delete"
Public Const NOTIF_CHG_TYPE_MOD                 As String = "Modify"
Public Const NOTIF_CHG_TYPE_CLOSE               As String = "Close"
Public Const NOTIF_CHG_TYPE_SELECT              As String = "Select"
Public Const NOTIF_CHG_TYPE_SEND_AS_CURRENT     As String = "Send current"
Public Const NOTIF_CHG_TYPE_SEND_AS_NONCURRENT  As String = "Send non current"
Public Const NOTIF_CHG_TYPE_DECLARE             As String = "Declare"
Public Const NOTIF_CHG_TYPE_END                 As String = "End"
Public Const NOTIF_CHG_TYPE_END_ALL             As String = "End all"
Public Const NOTIF_CHG_TYPE_END_MANY            As String = "End many"
Public Const NOTIF_CHG_TYPE_BROKE_LOCK          As String = "Broke lock"


'==========================================================
' Below are constants used for action (interprocess communication) notifications(01/12/98, TRD)
'==========================================================

'Possible ActionName values for ACTION notifications(01/12/98, TRD)
Public Const NOTIF_ACTION_D2PC_NAV  As String = "D2PC Action Nav"
Public Const NOTIF_ACTION_D2PC_CAI  As String = "D2PC Action CAI"
Public Const NOTIF_ACTION_D2PC_WP   As String = "D2PC Action WP"
Public Const NOTIF_ACTION_WORK_PLAN As String = "Work Plan Action"
Public Const NOTIF_ACTION_EV_ANIM   As String = "Event Anim Action"
Public Const NOTIF_ACTION_CAI       As String = "CAI Action"

'Possible ActionTypes values for ACTION notifications(01/12/98, TRD)
Public Const NOTIF_ACTION_TYPE_CREATE           As String = "Create"
Public Const NOTIF_ACTION_TYPE_OPEN_EDIT        As String = "Open Edit" 'Open item in edit mode
Public Const NOTIF_ACTION_TYPE_OPEN_VIEW        As String = "Open View" 'Open item in view mode
Public Const NOTIF_ACTION_TYPE_OPEN_OTHER_EOC   As String = "Open other EOC" 'Open item from other EOC
Public Const NOTIF_ACTION_TYPE_RETURN           As String = "Return" 'This is after the user clicks a button to return to FEMIS.  (i.e. clicking the "Update Work Plan" button within D2PC interface)
Public Const NOTIF_ACTION_TYPE_REPLY            As String = "Reply" 'This is the intitial reply saying, "yes we got the notification".  (i.e. D2PC saying "Yes it got the message from workplan to open a case")
                
'Possible ActionStatus values for ACTION notifications(01/12/98, TRD)
Public Const NOTIF_ACTION_STATUS_WAITING    As String = "Waiting"
Public Const NOTIF_ACTION_STATUS_START      As String = "Start"
Public Const NOTIF_ACTION_STATUS_STARTED    As String = "Started"
Public Const NOTIF_ACTION_STATUS_DONE       As String = "Done"
Public Const NOTIF_ACTION_STATUS_FAILED     As String = "Failed"
Public Const NOTIF_ACTION_STATUS_TIME_OUT   As String = "Timed Out"
            
'==========================================================
' Below are constants used for data driven notification (01/12/98, TRD)
'==========================================================
' Constants used for stAction field of data driven notification(01/12/98, TRD)
Public Const NOTIF_DD_ADD   As String = "Add"
Public Const NOTIF_DD_MOD   As String = "Modify"
Public Const NOTIF_DD_DEL   As String = "Delete"

' Constants used for lDAIFlag and lDDFlag field of data driven notification(01/12/98, TRD)
Public Const NOTIF_DD_YES   As Integer = 1
Public Const NOTIF_DD_NO    As Integer = 0

' Constants used for ulAuxProcessFlags field of data driven notification(01/12/98, TRD)
Public Const NOTIF_DD_SENT          As Integer = 8
Public Const NOTIF_DD_ARRIVED       As Integer = 16
Public Const NOTIF_DD_NOT_ARRIVED   As Integer = 32

'**-----------------------------------------------------------------**
'** This routine takes a table name and where clause, and retrieves **
'** the XMIT_INIT_DATE and Oracle rowid to be used for data driven  **
'** notification (DDN).  This routine is merely meant as a shortcut **
'** to making the entire query yourself and extracting out the      **
'** parameters.  If you want to be sure your stWhere clause only    **
'** retrieved one record from stTableName, you should use routine   **
'** U_GetDDInfo instead.  Except for special purposes, you should   **
'** usually use U_GetDDInfo.                                        **
'**                                                                 **
'** <INPUT>                                                         **
'** -  stTableName  - This is the table name for which you are      **
'**                   trying to find rowid and xmit_init_date.      **
'** -  stWhere      - This is the SQL where clause required to      **
'**                   narrow down the query to one record in the    **
'**                   stTableName table.  This should include the   **
'**                   word "where".                                 **
'**                                                                 **
'**    Here's an example of the type of thing to be passed in:      **
'**                                                                 **
'**    "WHERE d2_case_id=" & glUCurD2 & " AND " & fnstE_SQLWhere()  **
'**                                                                 **
'** <OUTPUT>                                                        **
'** -  stRowId      - This is the Oracle rowid for the record in    **
'**                   table stTableName given the stWhere clause.   **
'** -  vXmitInitDate- This is the Transmit Date/Time for the record **
'**                   in table stTableName given the stWhere clause.**
'**                   This will be returned in the proper time zone **
'**                   and format for data driven notification --    **
'**                   namely F_DATETIME_FIXEDLEN.                   **
'**                                                                 **
'** <RETURN>                                                        **
'**    Zero for good status.                                        **
'**    Non-zero error code number for bad status.                   **
'**                                                                 **
'** Date      Dev.  Revision Notes                                  **
'** --------  ----  ----------------------------------------------  **
'** 19980128  JAA   Initial Dev. based on U_GetDDInfo.              **
'** 19980223  TRD   Pass in Table Name as Uppercase per the latest  **
'**                 requirements for the ORACLE ODBC 2.5 driver.    **
'** 20000106  TRD   Use new replication_id instead of rowid for     **
'**                 Oracle 8.                                        **
'**-----------------------------------------------------------------**
'
Public Function U_GetDDInfoMult(ByVal stTableName As String, _
                                ByVal stWhere As String, _
                                      stReplicationID As String, _
                                      vXmitInitDate As Date) As Long

    On Error GoTo U_GetDDInfoMult
    
    Dim snpRec As New ADODB.Recordset
    
    
    Dim stSQL   As String   '** SQL Query
    
    '**-------------------------------------**
    '** Set return code to assume success.  **
    '** Construct and submit the SQL Query. **
    '**-------------------------------------**
    U_GetDDInfoMult = 0
  
    stSQL = "SELECT replication_id, xmit_init_date " & _
            " FROM " & UCase$(stTableName) & " " & stWhere
    
    snpRec.Open stSQL, gdbFemis, adOpenStatic, adLockReadOnly
    
    If snpRec.EOF Then
        U_GetDDInfoMult = -1
    Else
        stReplicationID = snpRec("replication_id")
        vXmitInitDate = snpRec("xmit_init_date")
    End If
    Exit Function
    
U_GetDDInfoMult:
    U_GetDDInfoMult = Err.Number
    Exit Function
    
End Function

'**-----------------------------------------------------------------**
'** Post a Notification Event Message per the provided arguments.   **
'**-----------------------------------------------------------------**
'
Public Sub U_PostEvent(lFlags As Long, _
                       stType As String, _
                       stArg1 As String, _
                       stArg2 As String, _
                       stArg3 As String)
    
    Dim mulNLinkID  As Long     '** Notification Link ID

    '**-------------------------------------**
    '** Insure that Notification is Up.     **
    '** Set Link to anonymous (default).    **
    '** Post Message via per the arguments. **
    '**-------------------------------------**
    Call InitNotifGlobals
    
    mulNLinkID = 0
    
    Call iNSPostEvent3P(mulNLinkID, lFlags, stType, gsEID, stArg1, stArg2, stArg3)
    
End Sub

'**-----------------------------------------------------------------**
'** This routine takes a table name and where clause, and retrieves **
'** the XMIT_INIT_DATE and Oracle rowid to be used for data driven  **
'** notification (DDN).  This routine is merely meant as a shortcut **
'** to making the entire query yourself and extracting out the      **
'** parameters.  If your stWhere clause does not return EXACTLY ONE **
'** RECORD for the stTableName, this routine will return an error   **
'** value.  If you want to be sure your stWhere clause only         **
'** retrieved one record from stTableName, you should use routine   **
'** U_GetDDInfo instead.  Except for special purposes, you should   **
'** usually use U_GetDDInfo.                                        **
'**                                                                 **
'** <INPUT>                                                         **
'** -  stTableName  - This is the table name for which you are      **
'**                   trying to find replication_id and             **
'**                   xmit_init_date.                               **
'** -  stWhere      - This is the SQL where clause required to      **
'**                   narrow down the query to one record in the    **
'**                   stTableName table.  This should include the   **
'**                   word "where".                                 **
'**                                                                 **
'**    Here's an example of the type of thing to be passed in:      **
'**                                                                 **
'**    "WHERE d2_case_id=" & glUCurD2 & " AND " & fnstE_SQLWhere()  **
'**                                                                 **
'** <OUTPUT>                                                        **
'** -  stReplicationID- This is the Oracle replication_id for the   **
'**                   record in table stTableName given the stWhere **
'**                   clause.                                       **
'** -  vXmitInitDate- This is the Transmit Date/Time for the record **
'**                   in table stTableName given the stWhere clause.**
'**                   This will be returned in the proper time zone **
'**                   and format for data driven notification --    **
'**                   namely F_DATETIME_FIXEDLEN.                   **
'**                                                                 **
'** <RETURN>                                                        **
'**    Zero for good status.                                        **
'**    Non-zero error code number for bad status.                   **
'**                                                                 **
'** Date      Dev.  Revision Notes                                  **
'** --------  ----  ----------------------------------------------  **
'** 19980119  TRD   Initial Development.                            **
'** 19980223  TRD   Pass in Table Name as Uppercase per the latest  **
'**                 requirements for the ORACLE ODBC 2.5 driver.    **
'** 20000106  TRD   Use new replication_id instead of rowid for     **
'**                 Oracle 8.                                        **
'**-----------------------------------------------------------------**
'
Public Function U_GetDDInfo(ByVal stTableName As String, _
                            ByVal stWhere As String, _
                                  stReplicationID As String, _
                                  vXmitInitDate As Date) As Long

    On Error GoTo U_GetDDInfo
    
    Dim snpRec As New ADODB.Recordset
    
    
    Dim stSQL   As String   '** SQL Query
  
    '**-------------------------------------**
    '** Set return code to assume success.  **
    '** Construct and submit the SQL Query. **
    '**-------------------------------------**
    U_GetDDInfo = 0
  
    stSQL = "SELECT replication_id, xmit_init_date " & _
            " FROM " & UCase$(stTableName) & " " & stWhere
    
    snpRec.Open stSQL, gdbFemis, adOpenStatic, adLockReadOnly
    
    If (snpRec.EOF) Then
        U_GetDDInfo = -1
    Else
        stReplicationID = snpRec("replication_id")
        vXmitInitDate = snpRec("xmit_init_date")
        snpRec.MoveNext
        If Not snpRec.EOF Then
            U_GetDDInfo = -1
        End If
    End If
    Exit Function
    
U_GetDDInfo:
    U_GetDDInfo = Err.Number
    Exit Function
  
End Function

