<%@ Language=VBScript %>
<!-- #include file = "MasterDB.asp" -->
<%	' load the request variables into local variables
	dbType = Request.Form("dbType")
	dbPath = Request.Form("dbPath")
	dbName = Request.Form("dbName")
	dbDesc = Request.Form("dbDesc")
	dbUName = Request.Form("dbUName")
	dbPassword = Request.Form("dbPassword")
	dbSource = Request.Form("dbSource")
	dbDictionary = Request.Form("dbDictionary")
	Set objMaster = Server.CreateObject("ADODB.Connection")
	Set rsConnect = Server.CreateObject("ADODB.Recordset")
	Set objTarget = Server.CreateObject("ADODB.Connection")
	Set rsSchema = Server.CreateObject("ADODB.Recordset")
	Set rsTemp = Server.CreateObject("ADODB.Recordset")

	' connect to the master database
    objMaster.Open MasterConnect
    ' create the connection string based on the data passed in
    Select Case CInt(dbType)
        Case 1
            ' this is a Microsoft Access database -- check to see
            '   if the username and password were included
            If dbUName <> "" Then
                strConnectString = "Provider=Microsoft.Jet.OLEDB.3.51;Persist Security Info=True;User ID=" & dbUName & ";Password=" & dbPassword & ";Data Source=" & dbPath
            Else
                strConnectString = "Provider=Microsoft.Jet.OLEDB.3.51;Persist Security Info=False;Data Source=" & dbPath
            End If
        Case 2
			' this is a Microsoft Access database -- check to see
            '   if the username and password were included
            If dbUName <> "" Then
                strConnectString = "Provider=Microsoft.Jet.OLEDB.4.0;Persist Security Info=True;User ID=" & dbUName & ";Password=" & dbPassword & ";Data Source=" & dbPath
            Else
                strConnectString = "Provider=Microsoft.Jet.OLEDB.4.0;Persist Security Info=False;Data Source=" & dbPath
            End If
        Case Else
            ' do nothing here for now--this will be to support new database
            '   types in the future
    End Select
    ' build a SQL string to insert the data passed in
    strSQL = "INSERT INTO " & _
                "tblDatabase(" & _
                    "DatabaseName, " & _
                    "Description, " & _
                    "Source, " & _
                    "ConnectString, " & _
                    "DictionaryID) " & _
             "VALUES (" & _
                    "'" & dbName & "', " & _
                    "'" & dbDesc & "', " & _
                    "'" & dbSource & "', " & _
                    "'" & strConnectString & "', " & _
						  dbDictionary & ")"
    ' execute the insert
    objMaster.Execute strSQL
    ' build a SQL statement to retrieve the new id for the record
    '   that was just inserted
    strSQL = "SELECT " & _
                "DatabaseID " & _
             "FROM " & _
                "tblDatabase " & _
             "WHERE " & _
                "DatabaseName = '" & dbName & "' " & _
             "AND " & _
                "ConnectString = '" & strConnectString & "'"
    ' get the recordset
    rsTemp.Open strSQL, objMaster, adOpenStatic, adLockReadOnly
    ' set the function equal to the id returned
    dbID = rsTemp.Fields("DatabaseID").Value
    ' close and deallocate the recordset and the connection
    rsTemp.Close

%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
</HEAD>
<body background="frames.jpg">
<%	' interrogate the target database and populate the
	'	info into the master database

	' build a query to extract the stored connection string
    strSQL = "SELECT " & _
                "ConnectString " & _
             "FROM " & _
                "tblDatabase " & _
             "WHERE " & _
                "DatabaseID = " & dbID
    ' return the recordset
    Set rsConnect.ActiveConnection = objMaster
    rsConnect.CursorLocation = adUseClient
    rsConnect.Open strSQL
    ' disconnect the recordset
    Set rsConnect.ActiveConnection = Nothing
    ' store the connection string
    strConnect = rsConnect.Fields("ConnectString").Value
    ' close the recordset
    rsConnect.Close


    On Error Resume Next
    ' open a connection to the target database using the connect string
    objTarget.Open strConnect
    If Err.number = 0 Then
	    ' use the ADO OpenSchema call to return the table and column names
	    Set rsSchema = objTarget.OpenSchema(adSchemaColumns)

	    ' initialize the current table
	    strCurTable = ""
	    ' loop through all the entries in the schema recordset--logging the tables
	    '   and columns
	    Do While Not rsSchema.EOF
	        ' *** this code is for MS-Access databases only ***
			' since this is MS-Access code only extended table / field names
			'	will be handled by adding square brackets around them--8/12/2002 KED
	        If Left(rsSchema.Fields("TABLE_NAME").Value, 4) <> "MSys" Then
	            ' check to see if the table name has changed
	            If strCurTable <> rsSchema.Fields("TABLE_NAME").Value Then
	                ' need to add the table to tblTables--build the insert statement
	                strSQL = "INSERT INTO " & _
	                            "tblTables(" & _
	                                "DatabaseID, " & _
	                                "TableName) " & _
	                         "VALUES (" & _
	                                dbID & ", " & _
	                                "'" & rsSchema.Fields("TABLE_NAME").Value & "')"
	                ' execute the insert
	                objMaster.Execute strSQL
	                ' build a SQL statement to return the new ID for the table
	                strSQL = "SELECT " & _
	                            "TableID " & _
	                         "FROM " & _
	                            "tblTables " & _
	                         "WHERE " & _
	                            "TableName = '" & rsSchema.Fields("TABLE_NAME").Value & "' " & _
	                         "AND " & _
	                            "DatabaseID = " & dbID
	                ' return the recordset
	                rsTemp.Open strSQL, objMaster,adOpenStatic,adLockReadOnly
	                ' store the table id
	                intTableID = rsTemp.Fields("TableID").Value
	                ' close the recordset
	                rsTemp.Close
	                ' set the strCurTable variable
	                strCurTable = rsSchema.Fields("TABLE_NAME").Value
	            End If
	            ' build a SQL statement to store the current field
	            strSQL = "INSERT INTO " & _
	                        "tblFields(" & _
	                            "TableID, " & _
	                            "DatabaseID, " & _
	                            "TableName, " & _
	                            "FieldName, " & _
	                            "FieldType) " & _
	                     "VALUES (" & _
	                            intTableID & ", " & _
	                            dbID & ", " & _
	                            "'" & rsSchema.Fields("TABLE_NAME").Value & "', " & _
	                            "'" & rsSchema.Fields("COLUMN_NAME").Value & "', " & _
	                            "'" & rsSchema.Fields("DATA_TYPE").Value & "')"
	            ' execute the insert
	            objMaster.Execute strSQL
	        End If
	        ' move the record pointer
	        rsSchema.MoveNext
	    Loop
	    ' close the schema recordset
	    rsSchema.Close
	    ' close the database connections
	    objMaster.Close
	    objTarget.Close
	    Set objMaster = Nothing
	    Set objTarget = Nothing
	    Set rsSchema = Nothing
	    Set rsTemp = Nothing%>
	<FORM name="GDBForm" method="POST" action="SelectParam.asp">
		<INPUT type="hidden" id=dbID name=dbID value=<%=dbID%>>
	</FORM>
	<SCRIPT Language=Javascript>
		// some javascript to automatically post the form
		document.GDBForm.submit();
	</SCRIPT>
<%	Else
		' here the database file could not be found
		' delete the record from tblDatabase
		strSQL = "DELETE FROM " & _
					"tblDatabase " & _
				 "WHERE " & _
					"DatabaseID = " & dbID
		objMaster.Execute strSQL
		' now post a message to the user%>
<H2 align="center">The database file was not found!</H2>
<UL>
	<LI>Click <A HREF="javascript:window.history.go(-1);" OnMouseOver="status = 'Go Back'; return true;" OnMouseOut="status = ''">here</A> to input the file information again.</LI>
</UL>
<%	End If%>
</BODY>
</HTML>
