Attribute VB_Name = "ThreatAreaFunc"

Option Explicit
'*****************************************************************
 '  CS  02/07/01    Creation of seperate bas file for threat area.
 '*****************************************************************
'----------------------------------------------------
' These constants are for the different types of threat areas.
' The strings should match those found in the GIS routine gdaMakeWedge
'----------------------------------------------------
' modified the TA constants to contain the Spanish translation--1/4/2001 KED
Public Const TA_WEDGE_ENGLISH As String = "wedge"
Public Const TA_CYLINDER_ENGLISH As String = "cylinder"
Public Const TA_CIRCLE_ENGLISH As String = "circle"
Public Const TA_WEDGE_TANGENT_ENGLISH As String = "wedgeTangent"
Public Const TA_POLYGON_ENGLISH     As String = "polygon"

Public Const TA_WEDGE_FOR_DISPLAY As String = "cuna"
Public Const TA_CYLINDER_FOR_DISPLAY As String = "cilindro"
Public Const TA_CIRCLE_FOR_DISPLAY As String = "circulo"
Public Const NO_NUMERIC_VALUE  As Integer = -999 ' Value used for numbers denoting no valid number (i.e. Number munitions)
'===============================================================
' GeoLocation.LocType constants (allowable types of GeoLocation object instances)
'===============================================================
Public Const GEO_TYPE_POINT        As String = "point"
Public Const GEO_TYPE_OFFSET       As String = "offset"
Public Const GEO_TYPE_OBJECT       As String = "object"
Public Const GEO_TYPE_EMPTY        As String = "empty"  ' Location is null / empty / not defined


'---------------------------------------------------------------------
' Below are public constants for the category of threat area
'---------------------------------------------------------------------

Public Const TEMPLATE          As String = "template"
Public Const USERDEFINED       As String = "userdefined"
Public Const MODELDEFINED     As String = "modeldefined"

Public Const KP_MODEL_SOURCE As String = "Model_Source"
'---------------------------------------------------------------------
' Below are public variables for the location of the spar model source
'---------------------------------------------------------------------
Public glSPAR_GeoObjectID As Long
Public gdSPAR_Lon  As Double
Public gdSPAR_Lat As Double
Public gstSPAR_SourceLocationAndWindDir As String

'---------------------------------------------------------------------
' Variable to store the location of the form.
'---------------------------------------------------------------------
Public gstThreatAreaRestoreForm     As String
Public mlSourceGeoObjectID          As Long
Public mstLocType                   As String
Public mstObjectName                As String
Public mdLonOrDist                  As Double
Public mdLatOrDir                  As Double


Public Const NO_VALUE As Long = 0

Private Const gstSQLDateTimeFormat = "DD-MON-YYYY HH24:MI:SS"
Private Const gstDateTimeFormat = "DD-MMM-YYYY H:MM:SS"


Public Function BuildRecom() As Boolean
Dim objFSO As New FileSystemObject
Dim GetEXEPath As String
Dim inFile As File
Dim inStream As TextStream
Dim stZoneName As String
Dim stcomment As String


On Error GoTo ErrHandler:

    BuildRecom = False

   GetEXEPath = App.Path + "\PAR.txt"
    
    Set inFile = objFSO.GetFile(GetEXEPath)
    ' set up the file to be read
    Set inStream = inFile.OpenAsTextStream(ForReading, TristateFalse)
    'stcomment = inStream.SkipLine
    
    stZoneName = inStream.ReadLine
    
    
    inStream.Close
    Set inStream = Nothing
    Set inFile = Nothing
    BuildRecom = True
    
ErrHandler:
    Exit Function
    
End Function

Public Function GenTAWedgeTypeSQLClause(ByVal stPrefix As String) As String
'==========================================================================
' This routine simply returns a string that can be used in an SQL query to
' select all the different types of APC wedges.
'
' <INPUT>:
'     stPrefix - If you have renamed a table for a join and need it
'                prepended to the threat_area_type portion of the string,
'                then pass it in here.  Be sure to pass in the "." as
'                part of your string.  For example, "A." would be for
'                a table renamed as "A".
'                NOTE:  IF YOU DON'T HAVE SOME SPECIAL JOIN HAPPENING, JUST
'                PASS AN EMPTY STRING.
'
'==========================================================================
' 12/01/00, TRD. Creation.
'==========================================================================
On Error Resume Next
    
  GenTAWedgeTypeSQLClause = " (" & stPrefix & "threat_area_type=" & stEnquoteStr_SQL(TA_WEDGE_ENGLISH) & " OR " & _
                            "  " & stPrefix & "threat_area_type=" & stEnquoteStr_SQL(TA_CIRCLE_ENGLISH) & " OR " & _
                            "  " & stPrefix & "threat_area_type=" & stEnquoteStr_SQL(TA_CYLINDER_ENGLISH) & " OR " & _
                            "  " & stPrefix & "threat_area_type=" & stEnquoteStr_SQL(TA_WEDGE_TANGENT_ENGLISH) & " OR " & _
                            "  " & stPrefix & "threat_area_type=" & stEnquoteStr_SQL(TA_POLYGON_ENGLISH) & " ) "


                
End Function

Public Function GetLayerID(ByVal lGeoObjectID As Long) As Long
'======================================================================
' This routine returns a GIS_LAYER_ID based on a GEO_OBJECT_ID.
' <INPUT>:
'     lGeoObjectID - GEO_OBJECT_ID to look up
' <RETURNS>:
'     GEO_LAYER_ID that corresponds to lGeoObjectID.  Zero returned if
'     none found.
'======================================================================
' 11/28/00, Chitra. Creation.
'======================================================================
Dim stSQL       As String
Dim gdbFemis    As New ADODB.Connection
Dim rsGisLayer  As Recordset
Dim lGISLayerID As Long
On Error GoTo GetLayerID_Error

'----------------------------------------------------------------------
' Assume failure at first
'----------------------------------------------------------------------
  GetLayerID = 0
    
'----------------------------------------------------------------------
' Now actually check the DB
'----------------------------------------------------------------------
  stSQL = " Select gis_layer_id from S_geo_object where geo_object_id=" & lGeoObjectID _
      & " And exercise_num=" & gsEID
  Set rsGisLayer = gdbFemis.CreateSnapshot(stSQL)
  If Not (rsGisLayer.EOF) Then
      GetLayerID = rsGisLayer("Gis_Layer_id")
  End If
  
  Exit Function
GetLayerID_Error:
  
  Exit Function
End Function

Public Function GetIDFromName(ByVal stName As String, _
                            ByVal stTable As String, _
                            ByVal stFieldID As String, _
                            ByVal stFieldName As String, _
                            ByVal stEOCName As String) As Long

'======================================================================
' This routine returns a ID based on the name in any table.
' <INPUT>:
'     stName - Name- could be class_name, gis_layer_name
'     stTable - Name of the table
'     stField - Name of the ID field - class_id, gis_layer_id
'     stEOC   - Name of the EOC.
' <RETURNS>:
'     ID that corresponds to the Name.  Zero returned if
'     none found.
'======================================================================
' 08/28/01, Chitra. Creation.
'======================================================================
Dim stSQL       As String
'Dim gdbFemis    As New ADODB.Connection
Dim rsGisLayer  As New ADODB.Recordset

Dim lGISLayerID As Long
On Error GoTo GetLayerID_Error

'----------------------------------------------------------------------
' Assume failure at first
'----------------------------------------------------------------------
  GetIDFromName = 0
   'gdbFemis.Open DB_Connect
  
 
'----------------------------------------------------------------------
' Now actually check the DB
'----------------------------------------------------------------------
  stSQL = " Select " & stFieldID & " from " & stTable & " where " & stFieldName & "='" & stName & _
            "' And EXERCISE_NUM=" & gsEID & " And EOC_NAME= '" & stEOCName & "'"
    rsGisLayer.Open stSQL, gdbFemis, adOpenStatic, adLockReadOnly
   
  If Not (rsGisLayer.EOF) Then
      GetIDFromName = rsGisLayer(stFieldID)
  Else
      GetIDFromName = 0
      rsGisLayer.Close
      
            
  End If
  
  Exit Function
GetLayerID_Error:
  Exit Function
  End Function

Public Function LocationParameters(ByVal stThreatAreaType As String, _
                                    ByVal stPolygonData As String) As Boolean
'--------------------------------------------------------------------------
'   07/25/01 CS Creation:  To update the location parameters
'                           based on the geo_loc_type
'                           "point" - will update only the lat and long
'                           "offset" - will update the dist and dir
'                           "object" - will update the geoobjectid only.
'   08/22/01               To write the polygon CLOB data to Geo_object
'   10/17/01                Calculate Offset lat and long in the object.
'   05/26/05               Will only update a polygon, point and object.
'                           will not update an offset since we are not connected
'                          to the gis.
'---------------------------------------------------------------------------
Dim stData          As String
Dim lGISLayerID     As Long
Dim lClassID        As Long
Dim dLat            As Double
Dim dLon            As Double
Dim stSQL           As String
Dim gdbFemis        As New ADODB.Connection
Dim rsTmp           As New ADODB.Recordset


On Error Resume Next

    'Assume Failure
    
    LocationParameters = False
    gdbFemis.Open DB_Connect

    stSQL = "SELECT GEO_OBJECT_ID.NEXTVAL FROM DUAL"
    Set rsTmp = gdbFemis.CreateSnapshot(stSQL)
    mlSourceGeoObjectID = CLng(rsTmp(0))
    rsTmp.Close

    
    If stThreatAreaType = TA_POLYGON_ENGLISH Then
         lGISLayerID = GetIDFromName("threat_" & gstUCurEOCCode, "GIS_LAYER", "GIS_LAYER_ID", "GIS_LAYER_NAME", gstUCurEOCName)
         
         If lGISLayerID > 0 Then
            lClassID = GetIDFromName("threat_a", "OBJECT_CLASS", "CLASS_ID", "CLASS_NAME", gstUCurEOCName)
                If lClassID > 0 Then
                    If Create_Geo_Object_From_Threat(lClassID, "TA polygon", lGISLayerID, 0, mlSourceGeoObjectID) Then
                        Call WritePolygonShapeToDB(mlSourceGeoObjectID, stPolygonData)
                        LocationParameters = True
                    End If
                End If
        End If
        
        
       
    Else
    
            
        mlSourceGeoObjectID = GetIDFromName(mstObjectName, "S_USER_DEFINED_OBJECT", "OBJECT_ID", "OBJECT_NAME", gstUCurEOCName)
       
        If mlSourceGeoObjectID > 0 Then
            
            'Just an object
              mstLocType = GEO_TYPE_OBJECT
              mdLonOrDist = 0
              mdLatOrDir = 0
              LocationParameters = True
        Else
            'Cannot do offset since we are not linked to the GIS
            '-----------------------------------
            ' point location
            '-----------------------------------
            mstLocType = GEO_TYPE_POINT
            mdLonOrDist = dLat
            mdLatOrDir = dLon
            LocationParameters = True
        End If
    
     
    End If
    

End Function


Public Function GetHazIDFromName(ByVal stName As String, _
                            ByVal stTable As String, _
                            ByVal stFieldID As String, _
                            ByVal stFieldName As String, _
                            ByVal stEOCName As String) As Long

'======================================================================
' This routine returns a ID based on the name in any table.
' <INPUT>:
'     stName - Name- could be class_name, gis_layer_name
'     stTable - Name of the table
'     stField - Name of the ID field - class_id, gis_layer_id
'     stEOC   - Name of the EOC.
' <RETURNS>:
'     ID that corresponds to the Name.  Zero returned if
'     none found.
'======================================================================
' 08/28/01, Chitra. Creation.
'           Modified without exercise num
'======================================================================
Dim stSQL       As String
Dim rsGisLayer  As New ADODB.Recordset

Dim lGISLayerID As Long
On Error GoTo GethazID_Error

'----------------------------------------------------------------------
' Assume failure at first
'----------------------------------------------------------------------
  GetHazIDFromName = 0

 
'----------------------------------------------------------------------
' Now actually check the DB
'----------------------------------------------------------------------
  stSQL = " Select " & stFieldID & " from " & stTable & " where " & stFieldName & "='" & stName & _
            "' And EOC_NAME= '" & stEOCName & "'"
    rsGisLayer.Open stSQL, gdbFemis, adOpenStatic, adLockReadOnly
   
  If Not (rsGisLayer.EOF) Then
      GetHazIDFromName = rsGisLayer(stFieldID)
  Else
      GetHazIDFromName = 0
      MsgBox "Unable to get ID from name. FieldName = " & stName & " EOC_Name= " & stEOCName
      rsGisLayer.Close
      
            
  End If
  
  Exit Function
GethazID_Error:
  Exit Function
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



Public Function WritePolygonShapeToDB(ByVal lGeoObjectID As Long, ByVal stData As String, _
                                        Optional ByVal lExNum As Long = -1) As Boolean
'Write a Polygon Shape definition string to the CLOB field of an existing record in GEO_OBJECT.
' (If creating a new polygon object, this function assumes the GEO_OBJECT record has already been created with a null CLOB field.)
'The existing GEO_OBJECT record is uniquely identified by the geo-object ID (lGeoObjectID) and the current exercise_num.
'If such a record exists, and the Object_Class is of type "polygon", the code attempts to update the GEO_OBJECT_SHAPE (CLOB) field
'    with the content of the string argument stData.  If the update is successful, the function returns True.
'If record doesn't exist or type is not polygon or update fails, the database is unmodified and the function returns False.
'Inputs:  lGeoObjectID:  Together with the current exercise_num, identifies a unique polygon object record in GEO_OBJECT.
'               stData:  The polygon shape string that is to be stored in the CLOB field of the GEO_OBJECT record
'Function value:  True ==> Success;
'                 False ==> An error occurred, or the record identified by lGeoObjectID was not found or is not a polygon object.
'08/01/01 RAB (initial version).
'10/05/01 RAB: Added an optional arg. lExNum (exercise number) for Polygon database migration (call from Put_Polygon).
'              Use the current exercise number (gsEId) if none is supplied.
'08/30/02 SAS: Change CursorType to adOpenKeyset instead of adOpenStatic to avoid ODBC errors.

Dim rsPoly As New ADODB.Recordset  'Use to update record in GEO_OBJECT table
'Dim rsGeo As Recordset
Dim sql As String
Dim lClassID As Long
Dim gdbFemis    As New ADODB.Connection

On Error GoTo WritePolyError
  
    WritePolygonShapeToDB = False
    
    If (lExNum < 0) Then lExNum = gsEID
    
    gdbFemis.Open DB_Connect
    

    sql = "SELECT * FROM GEO_OBJECT WHERE geo_object_id=" & CStr(lGeoObjectID) _
            & " AND (exercise_num=" & CStr(lExNum) & " or exercise_flag='N')"
    
    rsPoly.Open sql, gdbFemis, adOpenStatic, adLockOptimistic

    If (rsPoly.EOF) Then
         Exit Function
    Else
        lClassID = rsPoly("CLASS_ID")
        'If (LCase$(gstClassType(lClassID)) = "polygon") Then
            
            rsPoly("Geo_Object_Shape") = stData
            rsPoly.Update
            WritePolygonShapeToDB = True
        'Else
            
        'End If
    End If

    rsPoly.Close
    Set rsPoly = Nothing
    Exit Function

WritePolyError:
    Set rsPoly = Nothing
    Exit Function
    
End Function
Public Function ValidateEntries(stName As String, _
                                ByVal dDistMeters As Double, _
                                ByVal lIsolMeters As Long, _
                                ByVal lAngle As Long, _
                                ByVal lDirection As Long, _
                                ByVal lGeoObjectID As Long, _
                                ByVal dLat As Double, _
                                ByVal dLong As Double, _
                                ByVal stThreatShape As String) As Boolean
'********************************************************************
' Method: ValidateEntries
' Purpose:  To validate the entries when the object is loaded not
'           via the form. For example, the Model case may load the object
'           and then save the threat area object.
'  <INPUT>:
'        stName -        threat Area name (internally we do change the name if
'                       the length of the name is greater than 30 characters)
'        dDistMeters -  TA distance in Meters.
'        lIsolMeters -  TA isolation distance in Meters.
'        lAngle     -   TA angle only 3 digit numbers no decimals
'        lDirection -   TA direction with precision of 1
'        lGeoObjectID  _TA's Location of object - Geo Object ID.
'        dLat           TA's latitude
'        dLong          TA's longitude
'        stThreatShape  TA's wedge type - cylinder, circle, wedge or polygon.
'                       Right now the model case should give only a wedge type.
'   Optional bform      If true then this function is called from the form
'                       All setfocuses return to the form.
'   CS  20010125    Broke this out from cmdok
'   CS  20010129    No need for angle in Gen Hazard
'   CS  20010505    Made a copy to threat area object
'                   so values can be checked when D2 sets the
'                   parametric values.
'   CS  2001022     AddAutoCalMsg- invoke this method when called
'                   from AutoCalc and no message box indicating that it
'                   failed.
'   CS 20050526     Does not check for polygon validity.
'                   Need to pass in a lat long for an object.
'*********************************************************************


On Error GoTo ErrHandler

    ValidateEntries = False
    If ((stName = vbNullString)) Then
        Exit Function
        
    End If
    
    If (stThreatShape = vbNullString) Then
        Exit Function
    End If
    
    
    'check length of string
    
    If ((Len(stName) > 30)) Then '###value-->text### optabpc
        stName = Left(stName, 30)
    End If

  
    Select Case stThreatShape
        Case "wedge", "cylinder", "circle"
 
           
            If (lGeoObjectID = NO_VALUE) And (dLat = 0 And dLong = 0) Then
                    Exit Function
                
            End If
            
            If (dLat = 0 And dLong = 0) Then
               Exit Function
                
            End If
            
            
            
            If (((dDistMeters < 5) Or (dDistMeters > 99999))) Then
                Exit Function
            
            End If
            
    End Select
                  
                  
    Select Case stThreatShape
        Case "cylinder", "wedge"
            
            If (((lIsolMeters) < 0) Or ((lIsolMeters) > 99999)) Then
                Exit Function
            End If
           
            
            If ((lDirection < 0) Or (lDirection > 360)) Then   ': Exit Sub
                Exit Function
                
            End If
    End Select
    
    Select Case stThreatShape
        Case "wedge"
'            If (Not fnsUIsInt(lAngle)) Then
'                If bForm Then
'                    ifxMsgBox "You must enter a valid Wedge Angle between (5-360)."
'                    frmTemp.txtAngle.SetFocus
'                    Exit Function
'                Else
'                    AddAutoCalcMsg "The angle for Threat area is not between 5-360.", True, True
'                    Exit Function
'                End If
'
'            End If
            
            If ((lAngle < 5) Or (lAngle > 360)) Then
'                If bForm Then
'                    ifxMsgBox "You must enter a valid Wedge Angle between (5-360)."
'                    frmTemp.txtAngle.SetFocus
'                    Exit Function
'                Else
'                    AddAutoCalcMsg "The angle for Threat area is not between 5-360.", True, True
'                    Exit Function
'                End If
                Exit Function
            End If
            
            
    End Select
    
    Select Case stThreatShape
        Case "cylinder"
            If (((lIsolMeters) < 1) Or ((lIsolMeters) > 99999)) Then
'                If bForm Then
'                    ifxMsgBox "You must enter a valid Isolation Radius (1-99999 meters)."
'                    frmTemp.txtIsolRad.SetFocus
'                    Exit Function
'                Else
'                    AddAutoCalcMsg "The isolation radius for Threat area is not between 1-99999 meters.", True, True
'                    Exit Function
'                End If
               
            End If
    End Select
'
'    Select Case stThreatShape
'        'Assuming that the polygondata will be set outside of this routine
'
'        Case "polygon"
'            If lGeoObjectID <> NO_VALUE Then
'                If bForm Then
'                    If Me.PolygonData = vbNullString Then
'                        ifxMsgBox ("Please press the Select map button to draw a polygon.")
'                        Exit Function
'                    End If
'                Else
'                    AddAutoCalcMsg "There is no valid polygon for this Threat area.", True, True
'                End If
'
'            Else
'                If bForm Then
'                    ifxMsgBox ("No polygon shape was drawn. Please press the Select map button to draw a polygon.")
'                    Exit Function
'                Else
'                    AddAutoCalcMsg "There is no valid polygon for this Threat area.", True, True
'                End If
'
'            End If
'    End Select
    
    
    
    ValidateEntries = True
    Exit Function
    

    
ErrHandler:
    'Call SetLastError("Could not validate entries in the Threat Area Interface")
    Exit Function

End Function


Public Function Create_Geo_Object_From_Threat(ByVal lCLASS_ID As Long, _
                            ByVal stObjectName As String, _
                            ByVal lGISLayerID As Long, _
                            ByVal lSubclassID As Long, _
                            lGeoObjectID As Long) As Boolean

'==========================================================
'
'  INPUT:
'      Class ID
'      Object Name
'      SubclassID
'      GeoObjectID
'   OUTPUT:
'       User Defined ObjectID
'
'
'==========================================================
' 12/04/00, Chitra. Creation.
'==========================================================
'Dim rsObject    As Recordset
Dim stSQL         As String
Dim lObjectID     As Long
Dim snpZ        As New ADODB.Recordset
Dim gdbFemis    As New ADODB.Connection



On Error GoTo Create_Object_err

        
gdbFemis.Open DB_Connect

        
            stSQL = "SELECT * FROM GEO_OBJECT WHERE geo_object_id=" & CStr(lGeoObjectID) & " and (exercise_flag='N' OR exercise_num=" & CStr(gsEID) & ") "
            'Set snpZ = gdbFemis.OpenRecordset(stSQL)
            snpZ.Open stSQL, gdbFemis, adOpenStatic, adLockReadOnly
            If Not (snpZ.EOF) Then
            snpZ.Close
            stSQL = "SELECT GEO_OBJECT_ID.NEXTVAL FROM DUAL"
            snpZ.Open stSQL, gdbFemis, adOpenStatic, adLockReadOnly
            'Set snpZ = gdbfemis.CreateSnapshot(stSQL)
                If (snpZ.EOF) Then
                    snpZ.Close
                    GoTo Create_Object_err
    
                Else
                    lGeoObjectID = CLng(snpZ("NEXTVAL"))
                    snpZ.Close
                End If
            End If
          
        
        

            stSQL = "INSERT INTO GEO_OBJECT (GEO_OBJECT_ID, CLASS_ID, EXERCISE_NUM, " & _
                   " EOC_NAME, GEO_OBJECT_NAME,  GIS_LAYER_ID, EXERCISE_FLAG," & _
                   " XMIT_INIT_DATE)" & _
                   " VALUES (" & lGeoObjectID & ", " & lCLASS_ID & ", " & gsEID & ", '" & _
                   (gstUCurEOCName) & "', '" & ((stObjectName) & (lGeoObjectID)) & "', " & _
                   lGISLayerID & ", 'Y' ," & _
                   stTO_DATE((vZuluNow)) & ")"

            Debug.Print stSQL
            gdbFemis.Execute (stSQL)

            Create_Geo_Object_From_Threat = True

Exit Function
Create_Object_err:
Create_Geo_Object_From_Threat = False

Exit Function

End Function










Public Function IsValidWindDir(vWindDir As Variant) As Boolean
On Error Resume Next
  IsValidWindDir = False
  If IsNumeric(vWindDir) Then
    IsValidWindDir = (Val(vWindDir) >= 0) And (vWindDir <= 360)
  End If
End Function




