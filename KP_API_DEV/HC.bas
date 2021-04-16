Attribute VB_Name = "HC"
Option Explicit

Public gdbFemis As ADODB.Connection


Public Const NO_VALUE As Integer = 0      'Number depicting that no ID is selected for something.

'-------------------------------------------------------
' Folder data type
'-------------------------------------------------------
Public Type FOLDER
  lFolderID             As Long      'Folder  ID
  lExerciseNum          As Long      'Exercise num for the folder
  lHazardID             As Long      'Hazard ID for the folder
  stHazardName          As String    'Hazard name for the folder
  lFolderNum            As Long      'Folder num that user sees
  stFolderName          As String    'Short folder name
  stFolderDesc          As String    'Long folder desc
  vCreationDate         As Variant   'Date folder was created  (needed?)
  vOpenDate             As Variant   'Date folder was opened
  vClosedDate           As Variant   'Date folder was closed
  fCurrentOps           As Boolean   'True if folder current op for given haz/exer/EOC
  fOpen                 As Boolean   'True if folder is open
  fDeletedLogical       As Boolean   'True if folder logically marked for deletion
  fDeletedPhysical      As Boolean   'True if folder PHYSICALly deleted from DB
  vDeletedLogicalDate   As Variant   'Date folder marked for deletion
  vDeletedPhysicalDate  As Variant   'Date folder physically deleted
  stEOCName             As String    'Name of EOC owning folder
  stExportFile          As String    'Name of file exported containing data
  vXmitInitDate         As Variant   'XMIT_INIT_DATE on folder record
End Type

Public gtFolder As FOLDER

'-------------------------------------------------------
' Setup module level variables for current hazard case, nav, and folder
'-------------------------------------------------------
Public glCurrHazID As Long
Public gstND_HazName As String
Public glHazardCaseID As Long
Public glModelID As Long
Public glModelCaseID As Long
Public glRevisionNum As Long

Public glFolderID As Long
Public glNavID As Long
Public glExerNum As Long
Public gsEID As Long
Public gstCurHazName As String
Public gstSiteName As String
Public gstUCurSite  As String
Public gstUCurEOCName As String
Public gstUCurEOCCode As String

Public gstMostRecentSenderID As String
Public gstUCurDataset As String              'TRD_CHANGE_THIS --> How is this set for DLL?
Public gstUCurUser As String                 'TRD_CHANGE_THIS --> How is this set for DLL?

Public Const USER_NAME As String = "Frames"  'Probably should make this dynamic after prototype

Public Const HC_TYPE_MODEL_INDEX As Integer = 21
Public Const HC_TYPE_TA_INDEX    As Integer = 22
Public Const HC_TYPE_RA_INDEX    As Integer = 23
Public Const HC_TYPE_PAR_INDEX   As Integer = 24
Public Const HC_TYPE_PAD_INDEX   As Integer = 25

Public Const HC_TYPE_MIN_INDEX = HC_TYPE_MODEL_INDEX
Public Const HC_TYPE_MAX_INDEX = HC_TYPE_PAD_INDEX

Public Const HC_TYPE_MODEL As String = "Model"
Public Const HC_TYPE_TA    As String = "Threat Area"
Public Const HC_TYPE_RA    As String = "Risk Area"
Public Const HC_TYPE_PAR   As String = "PAR"
Public Const HC_TYPE_PAD   As String = "PAD"


'----------------------------------------------------------
' Type definition to hold each sub-object.
' NOTE:  If this definition changes, you must update CopySubObjectInfo
'----------------------------------------------------------
Public Type HC_ITEM
   stType As String
   lID As Long
   vLastUpdatedAt As Variant
   vXmitInitDate As Variant
   stLastUpdatedBy As String
   stDescShort As String
   stDescLong As String
   
   fChangedSinceLoading As Boolean 'True if changed since loading
End Type

Public gtHC(HC_TYPE_MIN_INDEX To HC_TYPE_MAX_INDEX) As HC_ITEM

Public Const NOTIF_CHG_TYPE_SELECT              As String = "Select"


Public gstUPCName           As String

Public giMinsFromGMT  As Integer
Public giHrsFromGMT   As Integer
Public Const HKEY_LOCAL_MACHINE     As Long = &H80000002
Public Const ERROR_SUCCESS          As Long = 0&
Public Const REG_DWORD              As Long = 4

Public Declare Function RegOpenKey Lib "advapi32.dll" _
                        Alias "RegOpenKeyA" (ByVal hKey&, ByVal lpSubKey$, phkResult&) As Long
Public Declare Function RegQueryValueLong Lib "advapi32.dll" _
                        Alias "RegQueryValueExA" (ByVal hKey&, ByVal lpValueName$, _
                                                  ByVal lpReserved&, lpType&, lpData&, _
                                                        lpcbData&) As Long 'lpdata as any

Public Declare Function RegCloseKey Lib "advapi32.dll" (ByVal hKey&) As Long

Public Function SaveToDB_Nav(ByVal lNavID As Long, _
                             ByVal lExerNum As Long, _
                             ByVal lNewHazardCaseID As Long) As Boolean
'================================================================
' This routine writes the navigator pointer information to the
' database.   NOTE:   This assumes that we're updating an existing
' navigator record with the new hazard case ID.  If the lNavID does
' not already exist, then nothing will happen by calling this routine.
'
' <INPUT>:  lNavID - Navigator ID that we're updating.
'           lExerNum - Exercise number to update
'           lNewHazardCaseID - New hazard case ID to associate with given nav
'
' <OUTPUT>: None
' <Return>: True if OK, else false
'================================================================
' 03/23/01, TRD. Creation.
'================================================================
On Error GoTo SaveToDB_Nav_err
Dim stSQL As String


'----------------------------------------------------------------
' Looks like we made it OK.
'----------------------------------------------------------------
  SaveToDB_Nav = False
 

'----------------------------------------------------------------
' Update the hazard case ID associated with the navigator of interest.
'----------------------------------------------------------------
  stSQL = "UPDATE NAVIGATOR SET " & _
          " HAZARD_CASE_ID = " & lNewHazardCaseID & _
   " WHERE  NAV_ID = " & lNavID & _
      " AND EXERCISE_NUM = " & lExerNum
  gdbFemis.Execute (stSQL)

'----------------------------------------------------------------
' Looks like we made it OK.
'----------------------------------------------------------------
  SaveToDB_Nav = True
 
  Exit Function
SaveToDB_Nav_err:
' Call SetLastError("Unable to write Navigator to DB.", , "SaveToDB_Nav")
  Exit Function
End Function

Public Function MakeOperational() As Boolean
'========================================================
' This method will make the selected item operational
'========================================================
' 05/20/05, TRD. Creation;
'========================================================
On Error GoTo MakeOperational_err
Dim lNewHazardCaseID As Long

'--------------------------------------------------------
' Assume failure at first
'--------------------------------------------------------
  MakeOperational = False

'--------------------------------------------------------
' Save the hazard case to the DB
'--------------------------------------------------------
  lNewHazardCaseID = SaveToDB_HC()
  If lNewHazardCaseID <= 0 Then Exit Function
  glHazardCaseID = lNewHazardCaseID
    
'--------------------------------------------------------
' Now point the navigator at the new HC
'--------------------------------------------------------
  If Not SaveToDB_Nav(glNavID, glExerNum, lNewHazardCaseID) Then Exit Function

'--------------------------------------------------------
' Must be OK by now...
'--------------------------------------------------------
  MakeOperational = True

  Exit Function
MakeOperational_err:
  Exit Function
End Function

'Public Sub SetContext_Hardcoded()
''==========================================================
'' This sets the context of this object to a hardcoded context.
''
'' This routine is used for prototype testing, and should be
'' removed at the point this becomes a generalized thing.
''
'' At that point, SetContext should be called directly by the calling
'' program as part of setting up this object.
''==========================================================
'' 05/20/05, TRD. Creation;
''==========================================================
'Dim lExerNum As Long
'Dim lHazardID As Long
'Dim stEOCName As String
'Dim stSiteName As String
'Dim lFolderID As Long
'Dim stEOCCode As String
'
'  lExerNum = 0
'  lHazardID = 10000062
'  stEOCName = "UMCD"
'  stEOCName = "UMCD"
'  stSiteName = "MORE"
'  lFolderID = 10000126
'
'  Call SetContext(stEOCName, stEOCCode, lExerNum, lHazardID, lFolderID, stSiteName)
'
'End Sub
Private Function SaveToDB_HC() As Long
'=======================================================================
' This routine saves the HAZARD_CASE table stuff based on class vars
' <INPUT>:
' <Returns>:  New HAZARD_CASE_ID if successful.   If fails, returns -1.

'=======================================================================
' 08/14/01, TRD. Creation.
' 05/24/05, TRD. Yanked from FEMIS and simplified for DLL.
'=======================================================================
On Error GoTo SaveToDB_HC_err
Dim stSQL As String
Dim ii As Integer
Dim lNewHazardCaseID As Long
Const SHORT_DESC_LEN As Integer = 50
Const LONG_DESC_LEN  As Integer = 4000
Const MODEL_REV_NUM  As Integer = 1

'---------------------------------------------------------------------
' Assume failure at first
'---------------------------------------------------------------------
  SaveToDB_HC = -1
  
  lNewHazardCaseID = GetNextID("HAZARD_CASE_ID")
  
'-----------------------------------------------------------------------
' Now try to save.  Start with parent.
'-----------------------------------------------------------------------
  stSQL = "INSERT INTO HAZARD_CASE (HAZARD_CASE_ID, EXERCISE_NUM ," & _
          "HAZARD_ID, FOLDER_ID, SITE_NAME, EOC_NAME, " & _
          "MODEL_ID, MODEL_CASE_ID, REVISION_NUM, " & _
          "SENT_OFFPOST_DATE, SENT_OFFPOST_USER, XMIT_INIT_DATE" & _
          ") VALUES (" & _
          lNewHazardCaseID & "," & glExerNum & "," & glCurrHazID & "," & _
          glFolderID & "," & stEnquoteStr_SQL(gstSiteName) & "," & _
          stEnquoteStr_SQL(gstUCurEOCName) & "," & _
          glModelID & "," & _
          glModelCaseID & "," & _
          glRevisionNum & "," & _
          stTO_DATE(vZuluNow()) & "," & _
          stEnquoteStr_SQL(gstUCurUser) & "," & _
          stTO_DATE(vZuluNow()) & ")"
  gdbFemis.Execute (stSQL)
  
'--------------------------------------------------------------------
' Now that we have the parent records inserted or updated, do the same
' for the children
'--------------------------------------------------------------------
  For ii = HC_TYPE_MIN_INDEX To HC_TYPE_MAX_INDEX
        stSQL = "INSERT INTO HAZARD_CASE_ITEM  (HAZARD_CASE_ID, EXERCISE_NUM, " & _
                "HC_ITEM_TYPE, HC_ITEM_ID, HC_ITEM_DATE, HC_ITEM_USER , HC_ITEM_DESC_SHORT, " & _
                "HC_ITEM_DESC_LONG, XMIT_INIT_DATE) VALUES (" & _
                lNewHazardCaseID & "," & _
                glExerNum & "," & _
                stEnquoteStr_SQL(gtHC(ii).stType) & "," & _
                gtHC(ii).lID & "," & _
                stTO_DATE((gtHC(ii).vLastUpdatedAt)) & "," & _
                stEnquoteStr_SQL(gtHC(ii).stLastUpdatedBy) & "," & _
                stEnquoteStr_SQL(Mid$(gtHC(ii).stDescShort, 1, SHORT_DESC_LEN)) & "," & _
                stEnquoteStr_SQL(Mid$(gtHC(ii).stDescLong, 1, LONG_DESC_LEN)) & "," & _
                stTO_DATE(vZuluNow()) & ")"
     gdbFemis.Execute (stSQL)
  Next ii

'-----------------------------------------------------------------------
' Finally update the case management and state flags.   Per discussion
' with John Bower, I am only to update CM only under the following
' circumstances: 1) New case 2) No model associated, but there is a TA
'                3) Saving model as new rev or 4) Saving model as new case
'-----------------------------------------------------------------------
'  If (gtHC(HC_TYPE_MODEL_INDEX).objSubObject.ID = NO_VALUE And _
'      gtHC(HC_TYPE_TA_INDEX).lID <> NO_VALUE) Then
'      SaveToDB_HC = UpdateCaseManagement()
'  ElseIf mfIsNew And _
'     (gtHC(HC_TYPE_MODEL_INDEX).objSubObject.ID <> NO_VALUE Or _
'      gtHC(HC_TYPE_TA_INDEX).objSubObject.ID <> NO_VALUE) Then
'      SaveToDB_HC = UpdateCaseManagement()
'  Else
'      SaveToDB_HC = True
'  End If
  
  
'------------------------------------------------------------------
' Raise changed event up the chain
'------------------------------------------------------------------
  glHazardCaseID = lNewHazardCaseID
  For ii = HC_TYPE_MIN_INDEX To HC_TYPE_MAX_INDEX
     If gtHC(ii).fChangedSinceLoading Then
        SendNormalHCONotif ii, NOTIF_CHG_TYPE_SELECT
     End If
  Next ii
  
'------------------------------------------------------------------
' Saved OK by now.   Return new HAZARD_CASE_ID.
'------------------------------------------------------------------
  SaveToDB_HC = lNewHazardCaseID
  
    
  Exit Function
SaveToDB_HC_err:
' Call SetLastError("Unable to save hazard case.", , "SaveToDB_HC")
  Exit Function
End Function

Public Function SendNormalHCONotif(ByVal lSubObjectIndex As Long, _
                                   ByVal stChangeType As String, _
                          Optional ByVal stAuxData3 As String = vbNull, _
                          Optional ByVal fDoingAutoCalc As Boolean = False) As Boolean
'=============================================================
' This routine sends an HCO notification for the given HCO
' type.
'
' <INPUT>:
'    lSubObjectIndex - Index of type of HCO thing that requires
'                      notif.  Should be one of the following:
'                      HCO_TYPE_MODEL_INDEX
'                      HCO_TYPE_TA_INDEX
'                      HCO_TYPE_RA_INDEX
'                      HCO_TYPE_PAR_INDEX
'                      HCO_TYPE_PAD_INDEX
'    stChangeType - Change type of notification to send.  Choose
'                   from one of the following:
'                      NOTIF_CHG_TYPE_ADD
'                      NOTIF_CHG_TYPE_DEL
'                      NOTIF_CHG_TYPE_MOD
'                      NOTIF_CHG_TYPE_SELECT
'                      NOTIF_CHG_TYPE_SEND_AS_CURRENT
'                      NOTIF_CHG_TYPE_SEND_AS_NONCURRENT
'    stAuxNotif3   - Optional string to put on Aux 3 for notif message.
'
' <OUTPUT>: None
' <Returns>: True if OK, else false.
'=============================================================
' 04/26/01, TRD. Creation.
'=============================================================
On Error GoTo SendNormalHCONotif_err
Dim stNotifName As String

'-------------------------------------------------------------
' Assume failure at first
'-------------------------------------------------------------
  SendNormalHCONotif = False

'-------------------------------------------------------------
' Now send the notification.  Track a unique name for who sent this notif
'-------------------------------------------------------------
  gstMostRecentSenderID = gstUPCName & "_" & Now
  stNotifName = GetNavNotifNameByIndex(lSubObjectIndex)

'-------------------------------------------------------------
' NOTE:  If we're calling this while in the middle of calculating (mfCalculating)
' then we don't want to get into a loop of notifications, so pass the user doing
' the calculating
'-------------------------------------------------------------
  SendNormalHCONotif = (NsPostDataChangeMsg(0, _
                                            NS_EF_NORMAL, stNotifName, _
                                            stChangeType, gstUCurDataset, _
                                            vZuluNow(), _
                                            glHazardCaseID, CStr(lSubObjectIndex), _
                                            "Navigator", _
                                            gtHC(lSubObjectIndex).lID, _
                                            stAuxData3, _
                                            IIf(fDoingAutoCalc, gstUCurUser, vbNullString), _
                                            gstMostRecentSenderID) _
                                            >= 0)

  Exit Function
SendNormalHCONotif_err:
'  Call SetLastError("Unable to send HCO notification.", , "SendNormalHCONotif")
  Exit Function
End Function

Public Function GetNavNotifNameByIndex(ByVal iHCIndex As Integer) As String
'===================================================================
' This routine retrieves which notification name a specific
' navigator index should use.
'
' For inverse of this function see GetNavNotifIndexByName
'
' NOTE:  If it weren't for trying to be backward compatable with DEI,
' watchful eye, etc., we'd probably just use a new name for all HCO
' things and a new name for all Nav things, but there is not enough
' time in this cycle to make everything consistent with the new way
' of doing things.)
'
' <INPUT>: lObjIndex - Index of interest.   Please use one of
'                      the following for HCO stuff:
'                            HC_TYPE_MODEL_INDEX
'                            HC_TYPE_TA_INDEX
'                            HC_TYPE_RA_INDEX
'                            HC_TYPE_PAR_INDEX
'                            HC_TYPE_PAD_INDEX
' <Returns>: Notificaiton name string for the given index.  If
'            error or not found based on input, then returns
'            empty string.
'===================================================================
' 08/10/01, TRD. Creation.
'===================================================================
On Error GoTo GetNavNotifNameByIndex_err
Dim stName As String
Const BAD_VALUE As String = ""

'----------------------------------------------------------
' Assume failure at first
'----------------------------------------------------------
  GetNavNotifNameByIndex = BAD_VALUE
    
'----------------------------------------------------------
' Now do the case statement lookup
'----------------------------------------------------------
  stName = BAD_VALUE
  Select Case iHCIndex
     Case HC_TYPE_MODEL_INDEX:  stName = NOTIF_CHG_D2PC
     Case HC_TYPE_TA_INDEX:     stName = NOTIF_CHG_THREAT
     Case HC_TYPE_RA_INDEX:     stName = NOTIF_CHG_RISK
     Case HC_TYPE_PAR_INDEX:    stName = NOTIF_CHG_PAR
     Case HC_TYPE_PAD_INDEX:    stName = NOTIF_CHG_PAD
  End Select

'----------------------------------------------------------
' Finally, set the return value
'----------------------------------------------------------
  GetNavNotifNameByIndex = stName
  
  Exit Function
GetNavNotifNameByIndex_err:
'  Call UErr("Unable to get notification name.", "GetNavNotifNameByIndex")
  Exit Function
End Function


'**-----------------------------------------------------------------**
'** Return a SQL string for usage per Oracle's Date type.           **
'**                                                                 **
'** Date      Dev.  Revision Notes                                  **
'** --------  ----  ----------------------------------------------  **
'** 19940819  MJB   Initial Development.                            **
'** 19940909  WMC   Use Date/Time Publics and make certain that the **
'**                 Date/Time formats match.                        **
'** 19980127  PDG   Code Standardization.                           **
'** 20010530  LRS   Changed date-time arg from String to Variant    **
'**-----------------------------------------------------------------**
'
Public Function stTO_DATE(ByVal vdatetime As Variant) As String
    
    Dim stFmtDateTime   As String   '** Formatted Date/Time

    stFmtDateTime = Format$(vdatetime, "DD-MMM-YYYY H:MM:SS")
    
    stTO_DATE = "TO_DATE ('" & stFmtDateTime & "', '" & "DD-MON-YYYY HH24:MI:SS" & "')"
    
End Function


Public Function GetNextID(ByVal stField As String) As Long
'=================================================================
' This routine gets the "nextval" for the sequence number field name passed in
'=================================================================
' 05/28/05, TRD.  Stole from FEMIS and simplified.
'=================================================================
On Error GoTo lUGetNextIDErr
Dim snp As New ADODB.Recordset
Dim stSQL As String

  '-------------------------------------------------
  ' Fetch the Next ID from Oracle's DUAL table.
  '-------------------------------------------------
  stSQL = "SELECT " & stField & ".nextval FROM   DUAL"
  snp.Open stSQL, gdbFemis, adOpenStatic, adLockReadOnly

  If (snp.EOF) Then
      GetNextID = -1
      Exit Function
  Else
      GetNextID = snp("nextval")
  End If
  snp.Close
  
  Exit Function
lUGetNextIDErr:
    GetNextID = -1
'   Call UErr("Error getting the next ID value for " & stField & "!", "$RCSfile: hc.bas $GetNextID")
    Exit Function
End Function


Public Function SetContext(ByVal stEOCName As String, _
                           ByVal stEOCCode As String, _
                           ByVal lExerNum As Long, _
                           ByVal lHazardID As Long, _
                           ByVal lFolderID As Long, _
                           ByVal stSiteName As String) As Boolean
'================================================================
' This routine sets the context for this navigator.
' <INPUT>:  stEOCName - EOC name  for this object
'           lExerNum - exercise number for this object
'           lHazardID - Hazard ID for this object.  **NOT HAZARD CASE**
'           lFolderID - Folder ID  for this object
'           stSiteName- Site name  for this object
' <OUTPUT>:
' <Returns>: True if OK, else false.
'================================================================
' 03/22/01, TRD. Creation.
'================================================================
On Error GoTo SetContext_err

'----------------------------------------------------------------
' Assume failure at first
'----------------------------------------------------------------
  SetContext = False

'----------------------------------------------------------------
' Now start setting stuff in this object
'----------------------------------------------------------------
  gsEID = lExerNum
  glExerNum = lExerNum   ' Don't like this but have two variables used as same thing.  Set both.
  glCurrHazID = lHazardID
  gstUCurEOCName = stEOCName
  gstSiteName = stSiteName
  glFolderID = lFolderID
  gstUCurSite = stSiteName
  'gstUCurDataset = "Operations" 'Believe this is always Operatations.
  'this is set when reading the .ini file.
  gstUCurEOCCode = stEOCCode

  If gstUCurUser = vbNullString Then
    gstUCurUser = USER_NAME  'Just make something up for DLL notification
  End If
  

  
  
'----------------------------------------------------------------
' Looks like we made it
'----------------------------------------------------------------
  SetContext = True

  Exit Function
SetContext_err:
  Exit Function
End Function



Public Function GetCurrentOpFolder(ByVal lExerNum As Long, _
                                   ByVal lHazID As Long, _
                                   ByVal stEOCName As String) As Long
'============================================================================
' This routine returns the current operational folder ID for
' the given input parameters.  If you want a list for all EOCS, then
' call GetCurrentOpFolders instead.
'
' It should be noted that this routine gets the current ops folder ID for an EOC.
' This may or may not be your current folder ID.  If you want your current folder
' ID you should call GetCurrFolderID instead.
'
' <INPUT>:
'   lExerNum - Exercise num for folder
'   lHazID - Hazard ID for folder
'   stEOCName - EOC for folder.  Pass in gstUCurEOC if you want your current EOC
'
' <RETURN>: Folder ID of current operational folder for given exer/haz/EOC.
'           Negetive value returned if error.
'
' NOTE:  Might try to optimize this later once notification is in place
'        so that we only read this information once, then listen to notifications
'        to keep an array list up to date.   For now, however, I need something
'        that works so I'm going to read from the DB each time.
'============================================================================
' 09/22/99, TRD. Creation.
' 05/24/05, TRD. Updated / simplified for DLL purposes.
'============================================================================
On Error GoTo GetCurrentOpFolder_err
Const ERROR_NUM = -1
Dim tFolder As FOLDER
Dim stSQL As String
Dim stTable As String
Dim lFolderID As Long
Dim snp As New ADODB.Recordset


'---------------------------------------------------------------------------
' Assume failure
'---------------------------------------------------------------------------
  GetCurrentOpFolder = ERROR_NUM
  lFolderID = ERROR_NUM

'---------------------------------------------------------------------------
' OK, now let's do the query and see if there is a current operational folder
'---------------------------------------------------------------------------
  stTable = " FOLDER "  'Used to check if table remote named S_FOLDER
  stSQL = "SELECT FOLDER_ID FROM " & stTable & " WHERE EXERCISE_NUM=" & lExerNum & _
          " AND HAZARD_ID=" & lHazID & " AND EOC_NAME=" & stEnquoteStr_SQL(stEOCName) & _
          " AND CURRENT_OPS_FLAG='Y'"
  snp.Open stSQL, gdbFemis, adOpenStatic, adLockReadOnly

  If Not snp.EOF Then
     '--------------------------------------------------
     ' Found it.  Hurrah!
     '--------------------------------------------------
     lFolderID = fnlUNull2Long(snp("FOLDER_ID"))
     GetCurrentOpFolder = IIf(lFolderID > 0, lFolderID, ERROR_NUM)
  End If
  snp.Close

  
  Exit Function
GetCurrentOpFolder_err:
  Exit Function
End Function

'**-----------------------------------------------------------------**
'** Return 0 if the provided variant is NULL or is not numeric.     **
'** Otherwise, return the variant as a Long.                        **
'**                                                                 **
'** Date      Dev.  Revision Notes                                  **
'** --------  ----  ----------------------------------------------  **
'** 19980127  PDG   Code Standardization.                           **
'**-----------------------------------------------------------------**
'
Public Function fnlUNull2Long(ByVal vInput As Variant) As Long

    If (IsNull(vInput)) Or (Not IsNumeric(vInput)) Then
        fnlUNull2Long = 0
    Else
        fnlUNull2Long = vInput
    End If

End Function

'**-----------------------------------------------------------------**
'** Return a string encased in quotes according to SQL rules.       **
'** This function puts single quotes around a text string and       **
'** converts all embedded quote characters to SQL format.           **
'**                                                                 **
'** Parameters:         Description:                                **
'** I : vText           Text to be wrapped in quotes.               **
'**                                                                 **
'** Return:             The properly enquoted string for SQL.       **
'**                                                                 **
'** Date      Dev.  Revision Notes                                  **
'** --------  ----  ----------------------------------------------  **
'** 19941208  LRS   Initial Development.                            **
'** 19950511  LRS   Modified to use Variant argument.               **
'**                 Fixed per handling NULL values.                 **
'** 19950512  LRS   Added ByVal qualifier to protect arg source.    **
'** 19980224  PDG   Code Standardization.                           **
'**-----------------------------------------------------------------**
'
Public Function stEnquoteStr_SQL(ByVal vText As Variant) As String

    Dim stTemp  As String
    Dim stQStr  As String
    Dim iPos    As Integer

    '**-------------------------------------**
    '** Make a working copy of the string   **
    '** while attending to the NULL issue.  **
    '**-------------------------------------**
    stTemp = vText & vbNullString
    iPos = InStr(stTemp, "'")

    Do While iPos > 0
        stQStr = stQStr & Left$(stTemp, iPos - 1) & "''"
        stTemp = Mid$(stTemp, iPos + 1)
        iPos = InStr(stTemp, "'")
    Loop

    '**-------------------------------------**
    '** Tack on the remaining portion.      **
    '** Then, add the outer quotes.         **
    '**-------------------------------------**
    stQStr = stQStr & stTemp

    stEnquoteStr_SQL = "'" & stQStr & "'"

End Function



Public Function GetCurrNavID(ByVal lExerNum As Variant, _
                             ByVal lHazardID As Variant, _
                             ByVal lFolderID As Variant, _
                             ByVal stEOCName As Variant, _
                             ByVal stSiteName As Variant) As Long
'========================================================================
' This routine returns the navigator ID from the NAVIGATOR table
' for the given context.   See also GetCurrNavID and GetLastBroadcastNavID.
'
' <INPUT>: lExerNum   - Exercise number of interest.
'          lHazardID  - Hazard ID of interest.
'          lFolderID  - Folder ID of interest.
'          stEOCName  - EOC name of interest.
'          stSiteName - Site name of interest.
' <Returns>:  Most recently broadcast NAVIGATOR ID for the given parameters.
'             If none found or error, then NO_VALUE will be returned.
'========================================================================
' 07/27/01, TRD. Creation.
' 05/24/05, TRD. Updated / simplified for DLL purposes.
'========================================================================
On Error GoTo GetCurrNavID_err:
Dim snp As New ADODB.Recordset
Dim stSQL As String

'-------------------------------------------------------------------------
' Assume failure at first by initializing to nothing
'-------------------------------------------------------------------------
  GetCurrNavID = NO_VALUE

'-------------------------------------------------------------------------
' Now actually do the query
'-------------------------------------------------------------------------
  stSQL = "SELECT NAV_ID FROM NAVIGATOR WHERE " & _
          " EXERCISE_NUM = " & lExerNum & _
          " AND HAZARD_ID = " & lHazardID & _
          " AND EOC_NAME = " & Enquote(stEOCName) & _
          " AND SITE_NAME = " & Enquote(stSiteName) & _
          " AND FOLDER_ID = " & lFolderID
  snp.Open stSQL, gdbFemis, adOpenStatic, adLockReadOnly
  
  If Not snp.EOF Then
    GetCurrNavID = snp("NAV_ID")
  End If
  snp.Close
  
  Exit Function
GetCurrNavID_err:
  Exit Function
End Function


Public Function Enquote(ByVal stString As String) As String
'===========================================================
' This routine enquotes the string passed in so that
' it will work in SQL statements.
' <INPUT>: stString - String to enquote
' <Returns>: Enquoted string ready to be used in SQL
'===========================================================
' 04/25/01, TRD. Created because I can never remember the
'                annoyingly named function that does the real
'                work.
'===========================================================
On Error Resume Next

   Enquote = stEnquoteStr_SQL(stString)
   
End Function

Public Function GetCurrOpHazCaseID(ByVal lExerNum As Variant, _
                                   ByVal lHazID As Variant, _
                                   ByVal lFolderID As Variant, _
                                   ByVal stEOCName As Variant) As Long
'===============================================================
' This routine gets the current operational hazard case ID.
' NOTE:  This is DIFFERENT than the current hazard ID.
' <INPUT>:
'     lExerNum -  exercise num.
'     lHazID   -  hazard ID.
'     lFolderID-  folder ID.
'     stEOCName-  EOC name.
'
' <OUTPUT>: None
'
' <Returns>: Value of hazard case ID for given context.  If fails
'            for some reason, then it will return a negetive number.
'===============================================================
' 04/26/01, TRD. Creation.
' 05/24/05, TRD. Updated / simplified for DLL purposes.
'===============================================================
On Error GoTo GetCurrOpHazCaseID_err:
Const BAD_VALUE As Integer = -999
Dim stSQL As String
Dim snp As New ADODB.Recordset


'---------------------------------------------------------------
' Assume failure at first
'---------------------------------------------------------------
  GetCurrOpHazCaseID = BAD_VALUE

'---------------------------------------------------------------
' OK, now setup the SQL and find the current op hazard case ID
'---------------------------------------------------------------
  stSQL = "SELECT HAZARD_CASE_ID FROM NAVIGATOR WHERE " & _
          " EXERCISE_NUM = " & lExerNum & _
          " AND FOLDER_ID = " & lFolderID & _
          " AND HAZARD_ID = " & lHazID & _
          " AND EOC_NAME = " & Enquote(stEOCName)
  snp.Open stSQL, gdbFemis, adOpenStatic, adLockReadOnly

  If Not snp.EOF Then
    GetCurrOpHazCaseID = Val(snp("HAZARD_CASE_ID") & "")
    'glHazardCaseID = "HAZARD_CASE_ID"
    'glModelID = Val(snp("MODEL_ID"))
    'glModelCaseID = Val(snp("MODEL_CASE_ID"))
    'glRevisionNum = Val(snp("REVISION_NUM"))
  End If
  snp.Close

  Exit Function
GetCurrOpHazCaseID_err:
  Exit Function
End Function
Public Function LoadCurrOpIDs(ByVal lExerNum As Long, _
                              ByVal lHazID As Long, _
                              ByVal stEOCName As String, _
                              ByVal stEOCCode As String, _
                              ByVal stSiteName As String) As Boolean
'=============================================================
' This routine gets the current operational folder, navigator,
' and hazard case based on the context information passed in.
' <INPUT>:
'   lExerNum   - Exercise num of interest
'   lHazID     - Hazard ID of interest
'   stEOCName  - EOC name of interest.
'   stEOCCode  - EOC code of interest.
'   stSiteName - Site name of interest.
'
' <RETURN>: True if OK, else false.
'=============================================================
' 05/24/05, TRD. Creation.
'=============================================================
On Error GoTo LoadCurrOpIDs_err
Dim lFolderID As Long
Dim lNavID As Long
Dim lHazardCaseID As Long

'-------------------------------------------------------------
' Assume failure at first
'-------------------------------------------------------------
  LoadCurrOpIDs = False
  
'-------------------------------------------------------------
' Get folder ID of interest.   If get a negetive folder ID,
' then exit with failure
'-------------------------------------------------------------
  lFolderID = GetCurrentOpFolder(lExerNum, lHazID, stEOCName)
  If lFolderID < 1 Then Exit Function
  
'-------------------------------------------------------------
' Now get the navigator ID of interest.   If NO_VALUE returned
' then exit function with failure.
'-------------------------------------------------------------
  lNavID = GetCurrNavID(lExerNum, lHazID, lFolderID, stEOCName, stSiteName)
  If lNavID = NO_VALUE Then Exit Function

'-------------------------------------------------------------
' Now get the current operational hazard case ID.   If value
' returned is less than zero, then exit.
'-------------------------------------------------------------
  lHazardCaseID = GetCurrOpHazCaseID(lExerNum, lHazID, lFolderID, stEOCName)
  If lHazardCaseID <= 0 Then Exit Function
  gstND_HazName = GetHazardNameById(lHazID)
  
'-------------------------------------------------------------
' Load current hazard case item IDs, and exit with failure if failed
'-------------------------------------------------------------
  If Not LoadHCItemIDs(lHazardCaseID, lExerNum) Then Exit Function
  
'-------------------------------------------------------------
' Now set the module level values
'-------------------------------------------------------------
  glHazardCaseID = lHazardCaseID
  glFolderID = lFolderID
  gtFolder.lFolderID = glFolderID
  glNavID = lNavID
  
  
  If Not SetContext(stEOCName, stEOCCode, lExerNum, lHazID, glFolderID, stSiteName) Then Exit Function
  

'-------------------------------------------------------------
' Must be OK by now...
'-------------------------------------------------------------
  LoadCurrOpIDs = True
  
  Exit Function
LoadCurrOpIDs_err:
  Exit Function
End Function


Public Function GetHazardNameById(ByVal lHazID As Long) As String
'===========================================================
' This routine returns the hazard name for the lHazID passed
' in.  If the ID does not exist, then it will return an empty string.
'===========================================================
' 11/20/98, TRD. Creation;
'===========================================================
On Error GoTo GetHazardNameById_Error
Dim stSQL As String
Dim snp As New ADODB.Recordset
   
'--------------------------------------------------
' Let's assume failure
'--------------------------------------------------
   GetHazardNameById = vbNullString

'--------------------------------------------------
' Now let's find out for sure
'--------------------------------------------------
   stSQL = "SELECT HAZARD_NAME FROM S_G_DEF_HAZARD_CLASS WHERE " & _
           "HAZARD_ID = " & lHazID
   snp.Open stSQL, gdbFemis, adOpenStatic, adLockReadOnly
  
   If Not snp.EOF Then GetHazardNameById = fnstUNull2Str(snp("HAZARD_NAME"))
   snp.Close
  
   Exit Function
GetHazardNameById_Error:
   Exit Function
End Function

'**-----------------------------------------------------------------**
'** Return a string per the provided variant.  Nulls are converted  **
'** to the empty string.                                            **
'**                                                                 **
'** Date      Dev.  Revision Notes                                  **
'** --------  ----  ----------------------------------------------  **
'** 19980127  PDG   Code Standardization.                           **
'**-----------------------------------------------------------------**
'
Public Function fnstUNull2Str(ByVal vInput As Variant) As String

    fnstUNull2Str = vInput & vbNullString
    
End Function


Public Function LoadHCItemIDs(ByVal lHazardCaseID As Long, _
                              ByVal lExerNum As Long) As Boolean
'===============================================================
' This routine loads the individual IDs for a given hazard case/exer num.
' Reads into gtHC array.
'
' <INPUT>:  lHazardCaseID - Hazard Case ID of interest
'           lExerNum - Exercise number of interest
' <Returns>: True if read into memory OK.
'===============================================================
' 05/24/05, TRD. Creation.
'===============================================================
On Error GoTo LoadHCItemIDs_err
Dim iIndex As Integer
Dim stSQL As String
Dim snp As New ADODB.Recordset
Dim ii As Integer


'-----------------------------------------------------------------
' Assume failure at first
'-----------------------------------------------------------------
  LoadHCItemIDs = False

'-----------------------------------------------------------------
' Now setup the query and load into memory
'-----------------------------------------------------------------
  stSQL = "SELECT HC_ITEM_TYPE, HC_ITEM_ID, HC_ITEM_DATE, " & _
          "HC_ITEM_USER , HC_ITEM_DESC_SHORT, " & _
          "HC_ITEM_DESC_LONG, XMIT_INIT_DATE FROM HAZARD_CASE_ITEM " & _
          " WHERE  HAZARD_CASE_ID = " & lHazardCaseID & _
             " AND EXERCISE_NUM = " & lExerNum
  snp.Open stSQL, gdbFemis, adOpenStatic, adLockReadOnly
  
  If snp.EOF Then
     snp.Close
     Exit Function
  End If
  
  While Not snp.EOF
        
        '----------------------------------------------------------
        ' Make sure we're saving to the correct index
        '----------------------------------------------------------
        Select Case snp("HC_ITEM_TYPE")
           Case HC_TYPE_MODEL:    iIndex = HC_TYPE_MODEL_INDEX
           Case HC_TYPE_TA:       iIndex = HC_TYPE_TA_INDEX
           Case HC_TYPE_RA:       iIndex = HC_TYPE_RA_INDEX
           Case HC_TYPE_PAR:      iIndex = HC_TYPE_PAR_INDEX
           Case HC_TYPE_PAD:      iIndex = HC_TYPE_PAD_INDEX
        End Select
        
        
        '----------------------------------------------------------
        ' Now set the appropriate values
        '----------------------------------------------------------
        gtHC(iIndex).stType = snp("HC_ITEM_TYPE")
        gtHC(iIndex).lID = snp("HC_ITEM_ID")
        gtHC(iIndex).vLastUpdatedAt = snp("HC_ITEM_DATE") & ""
        gtHC(iIndex).vXmitInitDate = snp("XMIT_INIT_DATE") & ""
        gtHC(iIndex).stLastUpdatedBy = snp("HC_ITEM_USER") & ""
        gtHC(iIndex).stDescShort = snp("HC_ITEM_DESC_SHORT") & ""
        gtHC(iIndex).stDescLong = snp("HC_ITEM_DESC_LONG") & ""
        
        gtHC(iIndex).fChangedSinceLoading = False
        
        snp.MoveNext
  Wend

'-----------------------------------------------------------------
' Must be OK by now
'-----------------------------------------------------------------
  LoadHCItemIDs = True

  Exit Function
LoadHCItemIDs_err:

  Exit Function
End Function

Public Function UpdateHCItemInMemory(ByVal stType As String, _
                                     ByVal lID As Long, _
                            Optional ByVal vLastUpdatedBy As Variant, _
                            Optional ByVal vLastUpdatedAt As Variant, _
                            Optional ByVal vXmitInitDate As Variant, _
                            Optional ByVal vDescShort As Variant, _
                            Optional ByVal vDescLong As Variant) As Boolean
'======================================================================
' This routine allows you to update a hazard case item.   It only
' updates the pointers in memory.   It can be called multiple times if you
' need to update multiple items.   When you're ready to save the hazard
' case, you should call ???
'
' <INPUTS>:  stType         - Type of item being updated.   Chose from the
'                             following constants:
'                                  HC_TYPE_MODEL   (for model)
'                                  HC_TYPE_TA      (for threat area)
'                                  HC_TYPE_RA      (for risk area)
'                                  HC_TYPE_PAR     (for PAR)
'                                  HC_TYPE_PAD     (for PAD)
'             lID            - ID of the item being updated
'             vLastUpdatedBy - (Optional) Whoever is updating.
'             vLastUpdatedAt - (Optional) When updated
'             vXmitInitDate  - (Optional) Timestamp for replication
'             vDescShort     - (Optional) Short description for navigator
'             vDescLong      - (Optional) Long description
'======================================================================
' 05/28/05, TRD. Creation;
'======================================================================
On Error GoTo UpdateHCItemInMemory_err:
Dim iIndex As Integer
Const BAD_VALUE = -1

'----------------------------------------------------------------------
' Assume failure at first
'----------------------------------------------------------------------
  UpdateHCItemInMemory = False

'----------------------------------------------------------------------
' Check to make sure they gave us something good
'----------------------------------------------------------------------
  Select Case stType
     Case HC_TYPE_MODEL:    iIndex = HC_TYPE_MODEL_INDEX
     Case HC_TYPE_TA:       iIndex = HC_TYPE_TA_INDEX
     Case HC_TYPE_RA:       iIndex = HC_TYPE_RA_INDEX
     Case HC_TYPE_PAR:      iIndex = HC_TYPE_PAR_INDEX
     Case HC_TYPE_PAD:      iIndex = HC_TYPE_PAD_INDEX
     Case Else:             iIndex = BAD_VALUE
  End Select
  
  If iIndex = BAD_VALUE Then Exit Function ' with error status
  
'----------------------------------------------------------------------
' OK, they passed a good type, let's update stuff
'----------------------------------------------------------------------
  If gtHC(iIndex).lID = lID Then
    '------------------------------------------------------------------
    ' Already set to this ID so really no change
    '------------------------------------------------------------------
    UpdateHCItemInMemory = True
    Exit Function
  Else
    '------------------------------------------------------------------
    ' OK, really have a new ID so update it.
    '------------------------------------------------------------------
    gtHC(iIndex).fChangedSinceLoading = True
    gtHC(iIndex).lID = lID
    gtHC(iIndex).stType = stType
       
    gtHC(iIndex).vLastUpdatedAt = IIf(IsMissing(vLastUpdatedAt), vZuluNow, vLastUpdatedAt)
    gtHC(iIndex).vXmitInitDate = IIf(IsMissing(vXmitInitDate), vZuluNow, vXmitInitDate)
    gtHC(iIndex).stLastUpdatedBy = IIf(IsMissing(vLastUpdatedBy), gstUCurUser, vLastUpdatedBy)
    gtHC(iIndex).stDescShort = IIf(IsMissing(vDescShort), stType & " " & Now, vDescShort)
    gtHC(iIndex).stDescLong = IIf(IsMissing(vDescLong), stType & " " & Now, vDescLong)
  End If
  
 
'----------------------------------------------------------------------
' Must be OK by now
'----------------------------------------------------------------------
  UpdateHCItemInMemory = True
  
  Exit Function
UpdateHCItemInMemory_err:
  Exit Function
End Function
'**-----------------------------------------------------------------**
'** Change / Update the Caption on the Wait Bar.                    **
'**                                                                 **
'** Date      Dev.  Revision Notes                                  **
'** --------  ----  ----------------------------------------------  **
'** 1994____  MJB   Initial Development.                            **
'** 19980127  PDG   Code Standardization.                           **
'**-----------------------------------------------------------------**
'
Public Function vZuluNow() As Variant

    vZuluNow = vToZuluTime(Now)
    
End Function
'**-----------------------------------------------------------------**
'** Return the Zulu-Time per the provided Date/Time argument.       **
'**                                                                 **
'** Date      Dev.  Revision Notes                                  **
'** --------  ----  ----------------------------------------------  **
'** 1995____  MJB   Initial Development.                            **
'** 19951205  MJB   Handle Nulls.                                   **
'** 19951228  MJB   Pass back a Value if it's not a Date.           **
'** 19980127  PDG   Code Standardization.                           **
'**-----------------------------------------------------------------**
'
Public Function vToZuluTime(ByVal vdatetime As Variant) As Variant
    
    On Error Resume Next
    
    Dim vWork   As Variant  '** Working Date/Time value

    If (IsNull(vdatetime)) Then
        vToZuluTime = Null
        Exit Function
    End If
    
    If (Not IsDate(vdatetime)) Then
        vToZuluTime = vdatetime
        Exit Function
    End If
    
    vWork = DateAdd("h", giHrsFromGMT, vdatetime)
    vWork = DateAdd("n", giMinsFromGMT, vWork)

    vToZuluTime = vWork
    
End Function

'**-----------------------------------------------------------------**
'** Initialize lots of stuff for the User Interface.                **
'**                                                                 **
'** Date      Dev.  Revision Notes                                  **
'** --------  ----  ----------------------------------------------  **
'** 1995____  MJB   Initial Development.                            **
'** 19950829  MJB   Don't create the NFS GIS directories.           **
'** 19951103  MJB   Don't create NFS dirs if "Evac" not in Exename. **
'** 19951116  MJB   Added gstUCurOnpostCode.                        **
'** 19951212  MJB   Shell REC2INI to set the TimeZone info.         **
'** 19951228  MJB   Start doing the actual TimeZone stuff.          **
'** 19960130  MJB   Just in case, set the gstUModeName stuff.       **
'** 19960313  MJB   Don't need to create M:\user\EVAC.dir.          **
'** 19980225  PDG   Code Standardization.                           **
'** 19980303  PDG   Cache the Screen Twips per Pixel Ratios into    **
'**                 Public variables for application-wide use.      **
'** 19980403  TRD   Change the viewmark directory for Spyros        **
'** 19981120  TRD   Set multi-hazard stuff                          **
'** 19991025  PDG   Fetch Current User's Full Name into variable.   **
'** 19991230  SAS   Exclude multi-hazard stuff for FEMIS.           **
'** 20000405  PDG   Do NOT clear the Mode value (SEPR 28764).       **
'** 20000831  SAS   Remove reference to mail.                       **
'** 20010128  SAS   Don't tell user about M drive for viewmarks.    **
'**                 They were already told when before. One msg     **
'**                 instead of two.                                 **
'** 20010216  SAS   Set Troubleshooting guide constant.             **
'** 20010816  LRS   Modified to load hazard info for WatchfulEye.   **
'** 20010816  LRS   Modified to load the default hazard name and ID **
'**                 for Stand-alone WatchfulEye.                    **
'** 20011206  LRS   Disabled the code for creating the ViewMark     **
'**                 directory tree (SEPR 31011).                    **
'** 20011228  LRS   Disabled all references to the ViewMark         **
'**                 directory tree (SEPR 31847).                    **
'** 26 Jul 02 RJC   Removed references to MS Project Path           **
'**-----------------------------------------------------------------**
'
Public Sub U_InitStuff()

On Error GoTo UISubsInitStuffErr
'
'Static fOnce    As Boolean
'Dim fs                          'Will be used for a file system object
'Dim stDrive     As String
'
'Dim rsEOC       As Recordset
'
'Dim stTemp      As String   '** Temp directory / file variable
'Dim iPortNum    As Integer  '** Server Port Number
'
'Dim stTopDirNFS         As String
'
'    '**-------------------------------------------------**
'    '** Fall out if this routine's been called before.  **
'    '**-------------------------------------------------**
'    If fOnce Then
'        Exit Sub
'    End If
'
'    fOnce = True
'
'    '**-------------------------------------------------**
'    '** Cache the Screen Twips per Pixel Ratios.        **
'    '** These buggers are useful in Form_Resize code.   **
'    '**-------------------------------------------------**
'    gfTwipsX = Screen.TwipsPerPixelX
'    gfTwipsY = Screen.TwipsPerPixelY
'
'    '**-------------------------------------------------**
'    '** These are old globals for debug mode put in place for
'    '** MJB and JRW.   Would like to yank, but referenced in a
'    '** lot of code that I don't own.   Until it is gone, I'm
'    '** just setting to false.   TRD - 08/29/01
'    '**-------------------------------------------------**
'     gfUIDebug = False
'     fUDEBUG = False
'
'    '**-------------------------------------------------**
'    '** Get the NFS Path.                               **
'    '**-------------------------------------------------**
'    stTopDirNFS = fnstUReadINI(stUFEMIS_PC, "FemisUserTopDirNFS", vbNullString, stUFEMIS_INI)
'    gstUUserHomeDirNFS = Trim$(stTopDirNFS) & "\" & Trim$(gstUCurUser)
'
'    Call U_ReadUSRIni
'
'    '**-------------------------------------------------**
'    '** Fetch full name of current user per User ID.    **
'    '**-------------------------------------------------**
'    gstUCurUserName = fnstUGetPersonText(glUCurUserRefNum)
'
'    '**-------------------------------------------------**
'    '** Hardcode the initialization of some vars.       **
'    '** Setup the Date and Time Public variables.       **
'    '**-------------------------------------------------**
'    CRLF = Chr$(13)
'    fUDEBUG = False         '!No longer debug!
'    gfUAllPriv = False      'God mode.
'
'    Call SetDateTimeGlobals
'
'    '**-------------------------------------------------**
'    '** Get the User's Home directory from their INI.   **
'    '** If User lacks a Home Dir, then tell them that   **
'    '** life would be difficult without it, therefore,  **
'    '** the application will terminate.                 **
'    '**                                                 **
'    '** Note:  Perhaps this can be changed so that the  **
'    '**        application continues, but with fewer    **
'    '**        options available to the user.           **
'    '**-------------------------------------------------**
'    gstUHomeDir = fnstUReadINI(stUFEMIS_PC, "FemisTopDir", vbNullString, stUFEMIS_INI)
'
'    If (gstUHomeDir = vbNullString) Then
'        Call UFatalErr("No FemisTopDir entry in " & stUFEMIS_INI & "!" & vbCrLf & _
'                       "You may not have a " & stUFEMIS_INI & ".  FEMIS can not start.", _
'                       "$RCSfile: hc.bas $U_InitStuff")
'        End
'    End If
'
'    '**-------------------------------------------------**
'    '** Fetch the names of COTS EXEs from the INI file. **
'    '** Note that we can fetch other Apps in a future   **
'    '** release of the product per the commented lines. **
'    '**-------------------------------------------------**
'    gstUGISEXE = fnstUReadINI(stUFEMIS_COTS, "GisEXE", vbNullString, stUFEMIS_INI)
'
'    '**-------------------------------------------------**
'    '** Fetch the Socket / Network information.         **
'    '** Fetch the UNIX directories.                     **
'    '**-------------------------------------------------**
'    gstUPCName = fnstUReadINI(stUFEMIS_PC, "FemisThisPCName", vbNullString, stUFEMIS_INI)
'
'    If (gstUCurUser = vbNullString) Then
'        gstUCurUser = "UNKNOWN"
'        If (fUDEBUG) Then
'            ifxMsgBox "Username not set, contact JRW or MJB.", vbExclamation 'Should never do this, _
'                                                                           but for testing.
'        End If
'    End If
'
'    stTemp = fnstUReadINI(stUFEMIS_PC, "FemisUserTopDirUNIX", vbNullString, stUFEMIS_INI)
'
'    If (stTemp = vbNullString) Then
'        gstUUserHomeDirUnix = "/tmp/" & Trim$(gstUCurUser)
'    Else
'        gstUUserHomeDirUnix = Trim$(stTemp) & "/" & Trim$(gstUCurUser)
'    End If
'
'    '**-------------------------------------------------**
'    '** Fetch the UNIX EXE directory.                   **
'    '** Fetch the User's PC directory:  get the correct **
'    '** path or just make it up.                        **
'    '**-------------------------------------------------**
'    gstUHomeDirUnix = fnstUReadINI(stUFEMIS_PC, "FemisTopDirUNIX", vbNullString, stUFEMIS_INI)
'
'    stTemp = fnstUReadINI(stUFEMIS_PC, "FemisUserTopDirPC", vbNullString, stUFEMIS_INI)
'
'    If (stTemp = vbNullString) Then
'        stTemp = Environ$("temp")
'    End If
'    If (stTemp = vbNullString) Then
'        gstUUserHomeDir = "C:\TEMP\" & Trim$(gstUCurUser)
'    ElseIf (stTemp = "STANDARD") Then
'        gstUUserHomeDir = gstUHomeDir & "\" & Trim$(gstUCurUser)
'    Else
'        gstUUserHomeDir = Trim$(stTemp) & "\" & Trim$(gstUCurUser)
'    End If
'
'    '**-------------------------------------------------**
'    '** Init the "last" exercise num used.  Since none  **
'    '** used previously yet, set to bogus value.        **
'    '**-------------------------------------------------**
'    glLastExerNum = -1
'
'    '**-------------------------------------------------**
'    '** Point to the appropriate application Help File. **
'    '**-------------------------------------------------**
'    gstUHelpFile = gstUHomeDir & "\FEMIS.HLP"
'    gstUTroubleFile = gstUHomeDir & "\TSG.HLP"
'    App.HelpFile = gstUHelpFile
'
'    '**-------------------------------------------------**
'    '** Get EOC Code and other info about the Server.   **
'    '**-------------------------------------------------**
'    Set rsEOC = gdbFemis.OpenRecordset("SELECT *         " & _
'                                       "FROM   EOC       " & _
'                                       "WHERE  EOC_Name =" & stEnquoteStr_SQL(gstUCurEOC) & _
'                                       " AND   Site_Name=" & stEnquoteStr_SQL(gstUCurSite), _
'                                       dbOpenSnapshot, dbSQLPassThrough)
'    If (rsEOC.EOF) Then
'        Call UErr("Error finding data about this EOC!", "$RCSfile: hc.bas $U_InitStuff")
'        gfUOnpost = False
'    Else
'        gstUCurEOCCode = fnvUFromOra(rsEOC("eoc_code"))
'        gfUOnpost = (fnvUFromOra(rsEOC("eoc_type")) = "Onpost")
'        glUPortNumberUnix = CLng(fnstUNull20(rsEOC("eoc_unix_port")))
'        glUPortNumberNotif = CLng(fnstUNull20(rsEOC("eoc_notify_port")))
'        gstUSVRName = fnvUFromOra(rsEOC("eoc_server_name"))
'        gstUListener = fnvUFromOra(rsEOC("instance_name"))
'    End If
'    rsEOC.Close
'
'    '**-------------------------------------------------**
'    '** Get the Onpost Name.                            **
'    '**-------------------------------------------------**
'    Set rsEOC = gdbFemis.OpenRecordset("SELECT *   " & _
'                                       "FROM   EOC " & _
'                                       "WHERE  EOC_Type ='Onpost' " & _
'                                       " AND   Site_Name=" & stEnquoteStr_SQL(gstUCurSite), _
'                                       dbOpenSnapshot, dbSQLPassThrough)
'    If (rsEOC.EOF) Then
'        Call UErr("Error finding Onpost EOC!", "$RCSfile: hc.bas $U_InitStuff")
'    Else
'        gstUCurOnpost = fnvUFromOra(rsEOC("eoc_name"))
'        gstUCurOnpostCode = fnvUFromOra(rsEOC("eoc_code"))
'    End If
'    rsEOC.Close
'
'    gstUModeName(PLANNING) = "Planning"
'    gstUModeName(OPERATIONAL) = "Operations"
'    gstUModeName(PLANNING + 3) = "Exer. Plan"
'    gstUModeName(OPERATIONAL + 3) = "Exer. Oper"
'
'    '**-------------------------------------------------**
'    '** Do NOT clear the Mode value (SEPR 28764).       **
'    '**-------------------------------------------------**
'    'gsEId = 0
'
'    '**-------------------------------------------------**
'    '** Fetch the GIS-related settings for the User.    **
'    '**-------------------------------------------------**
'    gstUGISTopDirPC = fnstUReadINI(stUFEMIS_GIS, "GISTopDirPC", vbNullString, stUFEMIS_INI)
'    gstUGISTopDirNFS = fnstUReadINI(stUFEMIS_GIS, "GISTopDirNFS", vbNullString, stUFEMIS_INI)
'    gstUGISTopDirUNIX = fnstUReadINI(stUFEMIS_GIS, "GISTopDirUNIX", vbNullString, stUFEMIS_INI)
'    gstUGISViewScript = fnstUReadINI(stUFEMIS_GIS, "GISViewScript", vbNullString, stUFEMIS_INI)
'    gstUGISEditScript = fnstUReadINI(stUFEMIS_GIS, "GISEditScript", vbNullString, stUFEMIS_INI)
'    'stViewmarkDir = fnstUReadINI(stUFEMIS_GIS, "ViewmarkDir", vbNullString, stUFEMIS_INI)
'
'    '**-------------------------------------------------**
'    '** Ensure that the directory & subs exist.         **
'    '** Ignore errors if directories already present.   **
'    '**-------------------------------------------------**
'    On Error Resume Next
'
'    Call U_CreateTree(gstUUserHomeDir)
'
'    MkDir (gstUUserHomeDir)
'    MkDir (gstUUserHomeDir & "\D2")
'    MkDir (gstUUserHomeDir & "\ETC")
'    MkDir (gstUUserHomeDir & "\EVAC")
'
'    If (gstUGISTopDirPC <> vbNullString) Then
'        Call U_CreateTree(gstUGISTopDirPC)
'        '**TODO: make these for all eoc's?
'        MkDir (gstUGISTopDirPC & "\D2_" & gstUCurEOCCode)
'        MkDir (gstUGISTopDirPC & "\EV_" & gstUCurEOCCode)
'        MkDir (gstUGISTopDirPC & "\FA_" & gstUCurEOCCode)
'        MkDir (gstUGISTopDirPC & "\KP_" & gstUCurEOCCode)
'        MkDir (gstUGISTopDirPC & "\WA_" & gstUCurEOCCode)
'    End If
'
'    '**-------------------------------------------------**
'    '** Create the directories on the Network Drive.    **
'    '** For now, only create if the App is Evac...      **
'    '**-------------------------------------------------**
''    Call U_CreateTree(gstUUserHomeDirNFS)
''    MkDir (gstUUserHomeDirNFS)
''    stViewmarkDir = stTopDirNFS & "\GIS\VIEWMARK"
'
''    If (Dir(stViewmarkDir & "\") = "") Then
''        Call U_CreateTree(stViewmarkDir)
''    End If
'
'    '**-------------------------------------------------**
'    '** Fix SEPR 29288 -- Create the EOC Code directory **
'    '** as well as the Site Code directory for Viewmarks**
'    '**-------------------------------------------------**
'
'    '**-------------------------------------------------**
'    '** Below is commented out since we don't need the  **
'    '** Viewmark directories anymore.                   **
'    '**-------------------------------------------------**
'    'Set fs = CreateObject("Scripting.FileSystemObject")
'    'stDrive = fs.GetDriveName(gstUUserHomeDirNFS)
'    'If (fs.DriveExists(stDrive)) Then
'    '    stUserViewmarkDir = gstUUserHomeDirNFS & "\" & gstUCurEOCCode & "\GIS\VIEWMARK"
'    '    If (Dir(stUserViewmarkDir & "\") = "") Then
'    '        Call U_CreateTree(stUserViewmarkDir)
'    '    End If
'    '
'    '    stUserViewmarkDir = gstUUserHomeDirNFS & "\" & gstUCurSite & "\GIS\VIEWMARK"
'    '    If (Dir(stUserViewmarkDir & "\") = "") Then
'    '        Call U_CreateTree(stUserViewmarkDir)
'    '    End If
'    'End If
'
'    '**-------------------------------------------------**
'    '** Below is commented out since we don't need the  **
'    '** Evac directory anymore.                         **
'    '**-------------------------------------------------**
'    'If (InStr(UCase$(app.EXEName), "EVAC")) Then
'    '    'MkDir (gstUUserHomeDirNFS & "\D2")
'    '    'MkDir (gstUUserHomeDirNFS & "\ETC")
'    '    MkDir (gstUUserHomeDirNFS & "\EVAC")
'    'End If
'    'If (gstUGISTopDirNFS <> vbNullString) Then
'    '    U_CreateTree gstUGISTopDirNFS
'    '    MkDir (gstUGISTopDirNFS & "\FACMYEOC")
'    '    MkDir (gstUGISTopDirNFS & "\FACALL")
'    '    MkDir (gstUGISTopDirNFS & "\KPMYEOC")
'    '    MkDir (gstUGISTopDirNFS & "\KPALL")
'    '    MkDir (gstUGISTopDirNFS & "\D2_" & gstUCurEOCCode)
'    '    MkDir (gstUGISTopDirNFS & "\EV_" & gstUCurEOCCode)
'    'End If
'
'    '**-------------------------------------------------**
'    '** Set some default gen haz stuff.  Done here by   **
'    '** passing in some arbitrary bogus hazard_id.      **
'    '** Should reset later in code if needed/handled    **
'    '** for the specific executable.                    **
'    '** Exclude FEMIS it will be set during startup.    **
'    '**-------------------------------------------------**
'    #If xFEMIS Then
'    #ElseIf xSAWE Then
'        Dim stWEHazName     As String
'        stWEHazName = fnstUReadINI(stINI_EYE, "DefaultHaz", vbNullString, stUFEMISUSR_INI)
'        Call SetKeyGenHazVars(GetHazardIDByName(stWEHazName))
'    #Else
'        Call SetKeyGenHazVars(-999)
'    #End If
'    '**-------------------------------------------------**
'    '** Fill in all the polygonal layers which can be   **
'    '** used for risk, PAR, PAD, etc.                   **
'    '**-------------------------------------------------**
'    Call ReadPolyLayers
'
'    '**-------------------------------------------------**
'    '** Open the Notification Server Link per Port ID.  **
'    '**-------------------------------------------------**
'    iPortNum = CInt(glUPortNumberNotif)
'
'    Call iNSOpenServerLink(gstUSVRName, iPortNum, 0)
'
    '**-------------------------------------------------**
    '** Fetch TimeZone information from User Registry.  **
    '**-------------------------------------------------**
    giMinsFromGMT = lReadRegistry(HKEY_LOCAL_MACHINE, "SYSTEM\CurrentControlSet\Control\TimeZoneInformation", "ActiveTimeBias")
    giHrsFromGMT = 0

    Exit Sub
UISubsInitStuffErr:
    Exit Sub
End Sub

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

Public Function OpenDB() As Boolean
'=============================================================
' This routine opens a database connection for global use
'=============================================================
' 05/25/05, TRD. Creation.
'=============================================================
On Error GoTo OpenDB_err

'-------------------------------------------------------------
' Assume failure at first
'-------------------------------------------------------------
  OpenDB = False
  
'-------------------------------------------------------------
' Open the database
'-------------------------------------------------------------
  Set gdbFemis = New ADODB.Connection
  gdbFemis.Open DB_Connect
  
  
'-------------------------------------------------------------
' Must have opened OK
'-------------------------------------------------------------
  OpenDB = True
  
  Exit Function
OpenDB_err:
  Exit Function
End Function
Public Function DB_Connect() As String
' Author:   Kevin Dorow
' Created:  2/27/2001
' Purpose:  This function will return a connection string to the database

    'DB_Connect = "Provider=MSDASQL.1;Password=UMCD;Persist Security Info=True;User ID=UMCDA;Data Source=UMCD"
    DB_Connect = "Provider=MSDASQL.1;Password=" & gstUCurEOCCode & ";Persist Security Info=True;User ID=" & gstUCurEOCCode & "A;Data Source=" & gstUCurEOCCode

End Function

