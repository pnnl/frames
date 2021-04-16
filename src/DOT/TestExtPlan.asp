<%@ Language=VBScript %>
<!-- #include file = "MasterDB.asp" -->
<%
Sub ParseString(inString, inDelimiter, outArray)

    ' initialize the place holder
    intPlace = 1
    ' initialize the array counter
    intArrayCount = -1
    ' set up a loop to extract the elements and load them into an array
    Do While InStr(intPlace, inString, inDelimiter, vbTextCompare) <> 0
        ' add a space in the array for the new element
        intArrayCount = intArrayCount + 1
        ReDim Preserve outArray(intArrayCount)
        ' load the value into the new space
        outArray(intArrayCount) = Mid(inString, intPlace, _
                    InStr(intPlace, inString, inDelimiter, vbTextCompare) - intPlace)
        ' move the place holer
        intPlace = InStr(intPlace, inString, inDelimiter, vbTextCompare) + 1
    Loop
    ' load the last value into the array
    ' add a space in the array for the new element
    intArrayCount = intArrayCount + 1
    ReDim Preserve outArray(intArrayCount)
    ' load the value into the new space
    outArray(intArrayCount) = Mid(inString, intPlace)
End Sub


	'set up line feed identifier
	cr = vbCrLf

	dbdicid = Request("dbdicid")
	dbdicname = Request("dbdicname")%>

	<% 'create connection to the database
	Set objCon = Server.CreateObject("ADODB.Connection")
	objCon.Open MasterConnect
    Set rsParam = Server.CreateObject("ADODB.Recordset")
    'Response.Write "SELECT Count(*) from tblParamKeys where KeyName = '" & Request("ParameterName") & "'"
	strSQL = "SELECT " & _
				"Count(*) as PKcount " & _
			 "FROM " & _
				"tblParamKeys as PK, tblParameter as P " & _
			 "WHERE " & _
				"PK.KeyName = '" & Request("ParameterName") & "' " & _
			 "AND " & _
				"PK.ParameterID = P.ParameterID " & _
			 "AND " & _
				"P.DicID = " & dbdicid
	rsParam.Open strSQL,objCon
	If rsParam.Fields("PKcount").Value > 0 Or Request("unique") = "on" then
	   PKeyisParam = "distinct "
	else
	   PKeyisParam = ""
	end if

%>

<%		'process selected field information
	ListCount = 0
	If IsNumeric(Request("KCount")) Then
		KeyCount = Request("KCount")
	Else
		KeyCount = 999
	End If

'			strSQL = "Select "
'			strSQL = strSQL & PKeyisParam
'			strSQL = strSQL & "Count(*) as Count from "
'			'Response.Write strsql
'			strSQL = strSQL & tbNames
'			Response.Write fIDs
'			Response.Write fnames
'		end if
'	End If
	%>

	<INPUT type="hidden" name=KCount value=<%=KeyCount%>>


<%
	' determine if the user has entered the values for
	'	the key values
	TPCount = Request("TPCount")
	If (Request("testplan") <> "") Then
		' build the SQL statement with the key values
		strSQL = "SELECT " & PKeyisParam
        if TPCount = 1 then
           TPID = Left(Request("parameter"),Instr(1,Request("parameter"),".") - 1)
           TPName = Mid(Request("parameter"),Instr(1,Request("parameter"),".") + 1)
           fieldName = Request("fnames")
           strSQL = strSQL & " " & fieldname & " AS " & TPName
        else
           period1 = 1
           period2 = Instr(1,Request("parameter"),".")
           comma1 = instr(1,Request("parameter"),",")
           comma2 = instr(comma1 + 1,Request("parameter"),",")
          fieldstart = 1
           fieldstop = Instr(fieldstart,Request("fnames"),",")
		   for x=0 to TPCount -1
		       fieldName = Mid(Request("fnames"),fieldstart, fieldstop - fieldstart)
'		       Response.Write fieldname
		       TPID = Mid(Request("parameter"),period1,period2 - period1)
'		       Response.Write TPID
		       TPName = Mid(Request("parameter"),period2 + 1,comma1 - period2 -1)

'		       Response.Write TPName
		       if x = TPCount -1 then
		          strSQL = strSQL & " " & fieldname & " AS " & TPName
		       else
		          strSQL = strSQL & " " & fieldname & " AS " & TPName & ","
		       end if
		       fieldstart = fieldstop + 1
		       fieldstop = Instr(fieldstart + 1,Request("fnames"),",")
		       if fieldstop = 0 then
		          fieldstop = len(Request("fnames")) + 1
		       end if
		       period1 = comma1 +1
		       period2 = Instr(comma1,Request("parameter"),".")
	           if comma2 = 0 then
'		       Response.Write "here"
		          comma1 = len(Request("parameter")) + 1
		       else
		          comma1 = comma2
		       end if
		       comma2 = instr(comma1+1,Request("parameter"),",")
		   next
		end if
		strSQl = strSQL & " FROM "
		'strSQL = "SELECT " & _
		'         PKeyisParam &_
		'			Request("fnames") & " " & _
		'		 "FROM "
		'Response.Write strsql
		' add the table names
		strSQL = strSQL & Request("tbnames")
		'Response.Write strsql
		' check to see if the join clause is empty
		If Request("joinclause") <> "" Then
			' add the join clause to the WHERE condition
			strSQL = strSQL & " WHERE " & Request("joinclause")
		End If
		'If Request("joinclause") <> "" Then
		'	strSQL = strSQL & Request("joinclause")
		'Else
		'	strSQL = strSQL & Request("tbnames")
		'End If

		'create connection to the database
		Set objMaster = Server.CreateObject("ADODB.Connection")
		objMaster.Open MasterConnect
		fcount = Request("filtercount")
		If fcount > 0 Then
			' check to see if there was a join clause added
			If Request("joinclause") <> "" Then
				strSQL = strSQL & " AND "
			Else
				strSQL = strSQL & " WHERE "
			End If
			' set up a loop to process the filter(s)
			For i = 0 to (fcount - 1)
				' bust up the field id from the field name
				fieldName = Mid(Request("field" & i),Instr(1,Request("field" & i),",") + 1)
				fieldID = Left(Request("field" & i),Instr(1,Request("field" & i),",") - 1)
				' build a query to return the data type of field (to determine
				'	whether or not we need to put quotes around the value
				strGetType = "SELECT " & _
								"FieldType " & _
							 "FROM " & _
								"tblFields " & _
							 "WHERE " & _
								"FieldID = " & fieldID
				' define a recordset to get the data type with
				Set rsDataType = Server.CreateObject("ADODB.Recordset")
				' execute the query
				rsDataType.Open strGetType, objMaster
				' store the value in a local variable
				strDataType = rsDataType.Fields("FieldType").Value
				' close and deallocate the recordset
				rsDataType.Close
				Set rsDataType = Nothing
				' set up a case statement to determine whether or not quotes are needed
				Select Case strDataType
					Case "7","129","130","133","134","135"
						'these are either string or date--quotes are required
						' check to see if the operator has the quotes in it
						If instr(1,Request("operator" & i),"'") > 0 then
							strSQL = strSQL & fieldName & " " & Replace(Request("operator" & i),"v",Request("value" & i))
						Else
							strSQL = strSQL & fieldName & " " & Replace(Request("operator" & i),"v",chr(34) & Request("value" & i) & chr(34))
						End If
					Case Else
						' these types do not require quotes
						strSQL = strSQL & fieldName & " " & Replace(Request("operator" & i),"v",Request("value" & i))
				End Select
				' check the value of i
				If i < (fcount - 1) Then
					strSQL = strSQL & " AND "
				End If
			Next
		End If
		' check to see if any static filters were included
		If Request("static") <> "" Then
			' check to see if any key value filters were added
			If fcount > 0 Then
				' add the static filters to the where clause
				strSQL = strSQL & " AND " & Request("static")
			Else
				If Request("joinclause") <> "" then
					strSQL = strSQL & " AND " & Request("static")
				Else
					strSQL = strSQL & " WHERE " & Request("static")
				End If
			End If
		End If
		'Response.Write strsql
		' create a SQL statement to return the connection string to the target
		'	database
		strSQLTarget = "SELECT " & _
							"ConnectString " & _
					   "FROM " & _
							"tblDatabase " & _
					   "WHERE " & _
							"DatabaseID = " & Request("dbid")
		' create a recordset object to hold the results of the
		'	query
		Set rsSQLTarget = Server.CreateObject("ADODB.Recordset")
		rsSQLTarget.Open strSQLTarget,objMaster
		' store the connect string in a local variable
		strConnect = rsSQLTarget.Fields("ConnectString").Value
		' deallocate the recordset object
		rsSQLTarget.Close
		Set rsSQLTarget = Nothing
		objMaster.Close
		Set objMaster = Nothing
		' create a connection to the target database
		Set objTarget = Server.CreateObject("ADODB.Connection")
		objTarget.Open strConnect
		' create a recordset object to hold the results of the query
		Set rsExtPlan = Server.CreateObject("ADODB.Recordset")
		' set the properties of the recordset so that it can be
		'	disconnected from the source
		Set rsExtPlan.ActiveConnection = objTarget
		rsExtPlan.CursorLocation = adUseClient
		' execute the extraction plan and return the values
'		If IsNumeric(KeyCount) Then
'			if KeyCount = 0 then
'				strSQL = "Select "
'				strSQL = strSQL & PKeyisParam
'				strSQL = strSQL & "Count(*) as Count from "
'			strSQL = strSQL & Request("tbnames")
'			'strSQL = "Select Count(*) as Count from " & Request("tbnames")
'			end if
'		End If

		rsExtPlan.Open strSQL
		' disconnect the recordset and close the connection
		Set rsExtPlan.ActiveConnection = Nothing
		objTarget.Close
		Set objTarget = Nothing
	ElseIf Request("saveplan") <> "" Then
		strSQL = "SELECT "  & PKeyisParam
        if TPCount = 1 then
           TPID = Left(Request("parameter"),Instr(1,Request("parameter"),".") - 1)
           TPName = Mid(Request("parameter"),Instr(1,Request("parameter"),".") + 1)
           fieldName = Request("fnames")
           strSQL = strSQL & " " & fieldname & " AS " & TPName
        else
           period1 = 1
           period2 = Instr(1,Request("parameter"),".")
           comma1 = instr(1,Request("parameter"),",")
           comma2 = instr(comma1 + 1,Request("parameter"),",")
           fieldstart = 1
           fieldstop = Instr(fieldstart,Request("fnames"),",")
		   for x=0 to TPCount -1
		       fieldName = Mid(Request("fnames"),fieldstart, fieldstop - fieldstart)
'		       Response.Write fieldname
		       TPID = Mid(Request("parameter"),period1,period2 - period1)
'		       Response.Write TPID
		       TPName = Mid(Request("parameter"),period2 + 1,comma1 - period2 -1)

'		       Response.Write TPName
		       if x = TPCount -1 then
		          strSQL = strSQL & " " & fieldname & " AS " & TPName
		       else
		          strSQL = strSQL & " " & fieldname & " AS " & TPName & ","
		       end if
		       fieldstart = fieldstop + 1
		       fieldstop = Instr(fieldstart + 1,Request("fnames"),",")
		       if fieldstop = 0 then
		          fieldstop = len(Request("fnames")) + 1
		       end if
		       period1 = comma1 +1
		       period2 = Instr(comma1,Request("parameter"),".")
	           if comma2 = 0 then
'		       Response.Write "here"
		          comma1 = len(Request("parameter"))+1
		       else
		          comma1 = comma2
		       end if
		       comma2 = instr(comma1+1,Request("parameter"),",")
		   next
		end if
		strSQl = strSQL & " FROM "
		' the user wants to save the extraction plan
		' build the SQL statement without the key values
		'strSQL = "SELECT " & _
		'         PKeyisParam &_
		'			Request("fnames") & " " & _
		'		 "FROM "
		' add the table names
		strSQL = strSQL & Request("tbnames")
		' check to see if the join clause is empty
		If Request("joinclause") <> "" Then
			strSQL = strSQL & " WHERE " & Request("joinclause")
		End If
		' check to see if the join clause is empty
		'If Request("joinclause") <> "" Then
		'	strSQL = strSQL & Request("joinclause")
		'Else
		'	strSQL = strSQL & Request("tbnames")
		'End If
		fcount = Request("filtercount")
		If fcount > 0 Then
			' check to see if there was a join clause added
			If Request("joinclause") <> "" Then
				strSQL = strSQL & " AND "
			Else
				strSQL = strSQL & " WHERE "
			End If
			' set up a loop to process the filter(s)
			For i = 0 to (fcount - 1)
				' bust up the field id from the field name
				fieldName = Mid(Request("field" & i),Instr(1,Request("field" & i),",") + 1)
				operator = Replace(Request("operator" & i),"'","''")
				strSQL = strSQL & fieldName & " " & operator
				' check the value of i
				If i < (fcount - 1) Then
					strSQL = strSQL & " AND "
				End If
			Next
		End If
		' check to see if any static filters were included
		If Request("static") <> "" Then
			' check to see if any key value filters were added
			If fcount > 0 Then
				' add the static filters to the where clause
				strSQL = strSQL & " AND " & Replace(Request("static"),"'","''")
			Else
				If Request("joinclause") <> "" then
					strSQL = strSQL & " AND " & Replace(Request("static"),"'","''")
				Else
					strSQL = strSQL & " WHERE " & Replace(Request("static"),"'","''")
				End If
			End If
		End If

		'Response.Write strsql
  		Set objM = Server.CreateObject("ADODB.Connection")
    	objM.Open MasterConnect

	   Set rsEPIDs = Server.CreateObject("ADODB.Recordset")
	   strEPIDsql = "Select ExtPlanID from tblExtPlan where ParameterID in " & Request("parameteridlist")
	   rsEPIDs.Open strEPIDSQL,objCon
       DO While not rsEPIDs.EOF
       'Response.Write request("ExtPlanID")
		'?for each extplanid do the following
		'create connection to the database
'		Set objM = Server.CreateObject("ADODB.Connection")
'		objM.Open MasterConnect
		' check to see if there was an Existing Extraction Plan
		'If Request("ExtPlanID") <> "" Then
			' there was an existing extration plan--clean out the tables first
			' clear out any records in tblExtPlanFields
			strDelete = "DELETE FROM " & _
							"tblExtPlanFields " & _
						"WHERE " & _
							"ExtPlanID = " & rsEPIDs.fields("ExtPlanID").value
			' execute the delete
			objM.Execute strDelete
			' clear out any records in tblExtPlanJoin
			strDelete = "DELETE FROM " & _
							"tblExtPlanJoin " & _
						"WHERE " & _
							"ExtPlanID = " & rsEPIDs.fields("ExtPlanID").value
			' execute the delete
			objM.Execute strDelete
			' clear out any records in tblExtPlanUnits
			strDelete = "DELETE FROM " & _
							"tblExtPlanUnits " & _
						"WHERE " & _
							"ExtPlanID = " & rsEPIDs.Fields("ExtPlanID").Value
			' execute the delete
			objM.Execute strDelete
			' clear out any records in tblExtPlanTables
			strDelete = "DELETE FROM " & _
							"tblExtPlanTables " & _
						"WHERE " & _
							"ExtPlanID = " & rsEPIDs.fields("ExtPlanID").value
			' execute the delete
			objM.Execute strDelete
			' clear out any records in tblKeyFilter
			strDelete = "DELETE FROM " & _
							"tblKeyFilter " & _
						"WHERE " & _
							"ExtPlanID = " & rsEPIDs.fields("ExtPlanID").value
			' execute the delete
			objM.Execute strDelete
			' clear out any records in tblExtPlan
			strDelete = "DELETE FROM " & _
							"tblExtPlan " & _
						"WHERE " & _
							"ExtPlanID = " & rsEPIDs.fields("ExtPlanID").value
			' execute the delete
			objM.Execute strDelete
		'End If
		rsEPIDs.MoveNext
		Loop
		rsEPIDs.close
'		If IsNumeric(KeyCount) Then
'			if KeyCount = 0 then
'				strSQL = "Select "
'				strSQL = strSQL & PKeyisParam
'				strSQL = strSQL & "Count(*) as Count from "
'				strSQL = strSQL & Request("tbnames")
'			end if
'		End If
       defineacount = 1
	   Set rsParmIDs = Server.CreateObject("ADODB.Recordset")
	   strPIDsql = "Select ParameterID from tblParameter where ParameterID in " & Request("parameteridlist")
	   rsParmIDs.Open strPIDSQL,objCon
       DO While not rsParmIDs.EOF
		' build a SQL statement to insert a record into tblExtPlan
		strSQLInsert = "INSERT INTO " & _
							"tblExtPlan(" & _
								"ParameterID, " & _
								"ExtPlanSQL, " & _
								"ExtPlanCDate, " & _
								"DatabaseID) " & _
						"VALUES (" & _
								rsParmIDs.fields("ParameterID").value & ", " & _
								"'" & strSQL & "', " & _
								"'" & now() & "', " & _
								Request("dbid") & ")"
		' execute the insert
		objM.Execute strSQLInsert
		' build a SQL statement to extract the ID that was just created
		strSQLSelect =	"SELECT " & _
							"ExtPlanID " & _
						"FROM " & _
							"tblExtPlan " & _
						"WHERE " & _
							"DatabaseID = " & Request("dbid") & " " & _
						"AND " & _
							"ParameterID = " & rsParmIDs.fields("ParameterID").value
		' create a recordset object to hold the result
		Set rsExtPlanID = Server.CreateObject("ADODB.Recordset")
		rsExtPlanID.Open strSQLSelect, objCon
		' store the id
		ExtPlanID = rsExtPlanID.Fields("ExtPlanID").Value
		'Response.Write strsqlselect
		'esponse.Write ExtPlanID
		' close and deallocate the recordset
		rsExtPlanID.Close
		Set rsExtPlanID = Nothing
		if defineacount = 1 then
   		    ' parse the table id list
	    	strTemp = Request("tbids")
	    	'tbidArray = Array(strTemp)
		    Dim tbidArray()
		    ParseString strTemp,",",tbidArray
		end if
		' set up a loop to go through all the values in tbidArray
		For i = 0 to Ubound(tbidArray)
			' define a SQL statement for the insert into tblExtPlanTables
			strSQLInsert = "INSERT INTO " & _
								"tblExtPlanTables(" & _
									"ExtPlanID, " & _
									"TableID) " & _
							"VALUES (" & _
									ExtPlanID & ", " & _
									tbidArray(i) & ")"
			' execute the insert
			objM.Execute strSQLInsert
		Next
		' see if a join clause exists
		If Request("joinclause") <> "" Then
			' create an insert statement to save the join clause
			strSQLInsert = "INSERT INTO " & _
								"tblExtPlanJoin(" & _
									"ExtPlanID, " & _
									"JoinClause) " & _
						   "VALUES (" & _
									ExtPlanID & ", " & _
									"'" & Request("joinclause") & "')"
			'execute the insert
			objM.Execute strSQLInsert
		End If
		' see if a Units clause exists
		If Request("Units") <> "none" Then
			' create an insert statement to save the units clause
			strSQLInsert = "INSERT INTO " & _
								"tblExtPlanUnits(" & _
									"ExtPlanID, " & _
									"UnitsClause) " & _
						   "VALUES (" & _
									ExtPlanID & ", " & _
									"'" & Request("Units") & "')"
			' execute the insert
			objM.Execute strSQLInsert
		End If
		' set up a loop to load all the filter into tblKeyFilter
		fcount = Request("filtercount")
		If fcount > 0 Then
			For i = 0 to (fcount - 1)
				' bust up the field id from the field name
				fieldName = Mid(Request("field" & i),Instr(1,Request("field" & i),",") + 1)
				fieldID = Left(Request("field" & i),Instr(1,Request("field" & i),",") - 1)
				' make sure that any single quotes in operator are doubled up
				operatorTemp = Replace(Request("operator" & i),"'","''")
				' build the insert statement
				strSQLInsert = "INSERT INTO " & _
									"tblKeyFilter(" & _
										"ExtPlanID, " & _
										"ParamKeyID, " & _
										"FieldID, " & _
										"Operator) " & _
							   "VALUES (" & _
										ExtPlanID & ", " & _
										Request("KeyID" & i) & ", " & _
										fieldID & ", " & _
										"'" & operatorTemp & "')"
				' execute the insert
				objM.Execute strSQLInsert
			Next
		End If
		if defineacount = 1 then
		   ' parse the field id list
		   strTemp = Request("fids")
		   'fidArray = Array(strTemp)
		   Dim fidArray()
		   ParseString strTemp,",",fidArray
		end if
		' set up a loop to go through all the values in fidArray
		For i = 0 to Ubound(fidArray)
			' define a SQL statement for the insert into tblExtPlanTables
			strSQLInsert = "INSERT INTO " & _
								"tblExtPlanFields(" & _
									"ExtPlanID, " & _
									"FieldID) " & _
							"VALUES (" & _
									ExtPlanID & ", " & _
									fidArray(i) & ")"
			' execute the insert
			objM.Execute strSQLInsert
		Next
		' check to see if any static filters were included
		If Request("static") <> "" Then
			' build a SQL statement to insert the additional filters
			strSQLInsert = "INSERT INTO " & _
								"tblExtPlanStaticFilter(" & _
									"ExtPlanID, " & _
									"StaticFilter) " & _
						   "VALUES (" & _
									ExtPlanID & ", " & _
									"'" & Replace(Request("static"),"'","''") & "')"
			' execute the insert
			objM.Execute strSQLInsert
		End If
		rsParmIDs.MoveNext
		defineacount = defineacount + 1
		Loop'?end if for each parameter in table
		rsParmIDs.close
	Else

		'build the SQL statement without the key values
	  	strSQL = "SELECT " & PKeyisParam

        if TPCount = 1 then
           TPID = Left(Request("parameter"),Instr(1,Request("parameter"),".") - 1)
           TPName = Mid(Request("parameter"),Instr(1,Request("parameter"),".") + 1)
           fieldName = Request("fnames")
           strSQL = strSQL & " " & fieldname & " AS " & TPName
        else
           period1 = 1
           period2 = Instr(1,Request("parameter"),".")
           comma1 = instr(1,Request("parameter"),",")
           comma2 = instr(comma1 + 1,Request("parameter"),",")
           fieldstart = 1
           fieldstop = Instr(fieldstart,Request("fnames"),",")
		   for x=0 to TPCount -1
		       fieldName = Mid(Request("fnames"),fieldstart, fieldstop - fieldstart)
'		       Response.Write fieldname
		       TPID = Mid(Request("parameter"),period1,period2 - period1)
'		       Response.Write TPID
		       TPName = Mid(Request("parameter"),period2 + 1,comma1 - period2 -1)

'		       Response.Write TPName
		       if x = TPCount -1 then
		          strSQL = strSQL & " " & fieldname & " AS " & TPName
		       else
		          strSQL = strSQL & " " & fieldname & " AS " & TPName & ","
		       end if
		       fieldstart = fieldstop + 1
		       fieldstop = Instr(fieldstart + 1,Request("fnames"),",")
		       if fieldstop = 0 then
		          fieldstop = len(Request("fnames")) + 1
		       end if
		       period1 = comma1 +1
		       period2 = Instr(comma1,Request("parameter"),".")
	           if comma2 = 0 then
'		       Response.Write "here"
		          comma1 = len(Request("parameter")) + 1
		       else
		          comma1 = comma2
		       end if
		       comma2 = instr(comma1+1,Request("parameter"),",")
		   next
		end if
		strSQl = strSQL & " FROM "
		'  strSQL = "SELECT " & _
		'         PKeyisParam &_
		'			Request("fnames") & " " & _
		'		 "FROM "
		' add the table names
		strSQL = strSQL & Request("tbnames")
		' check to see if the join clause is empty
		If Request("joinclause") <> "" Then
			' add the join clause to the WHERE condition
			strSQL = strSQL & " WHERE " & Request("joinclause")
		End If

		' commented out 8/23/2000--KED
		'If Request("joinclause") <> "" Then
		'	strSQL = strSQL & Request("joinclause")
		'Else
		'	strSQL = strSQL & Request("tbnames")
		'End If

		fcount = Request("filtercount")
		If fcount > 0 Then
			' check to see if there was a join clause added
			If Request("joinclause") <> "" Then
				strSQL = strSQL & " AND "
			Else
				strSQL = strSQL & " WHERE "
			End If
			' set up a loop to process the filter(s)
			For i = 0 to (fcount - 1)
				' bust up the field id from the field name
				fieldName = Mid(Request("field" & i),Instr(1,Request("field" & i),",") + 1)
				strSQL = strSQL & fieldName & " " & Request("operator" & i)
				' check the value of i
				If i < (fcount - 1) Then
					strSQL = strSQL & " AND "
				End If
			Next
		End If
		' check to see if any static filters were included
		If Request("static") <> "" Then
			' check to see if any key value filters were added
			If fcount > 0 Then
				' add the static filters to the where clause
				strSQL = strSQL & " AND " & Request("static")
			Else
				If Request("joinclause") <> "" then
					strSQL = strSQL & " AND " & Request("static")
				Else
					strSQL = strSQL & " WHERE " & Request("static")
				End If
			End If
		End If

'?do above for each param in table param
'		If IsNumeric(KeyCount) Then
'			if KeyCount = 0 then
'				strSQL = "Select "
'				strSQL = strSQL & PKeyisParam
'				strSQL = strSQL & "Count(*) as Count from "
'				strSQL = strSQL & tbnames
'			end if
'		End If
	End If%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
<TITLE>Extraction Plan -- Test</TITLE>
<SCRIPT ID=clientEventHandlersJS LANGUAGE=javascript>
<!--

function cancel_onclick() {
	// redirect the window to the select parameter screen
	// window.location.href = "SelectParam.asp";
	document.DTForm.method = "POST";
	document.DTForm.action = "SelectParam.asp";
	document.DTForm.submit();
}

//-->
</SCRIPT>
</HEAD>
<body background="frames.jpg">
<H2>Extraction Plan -- Results</H2>
<%	If Request("saveplan") <> "" Then%>
	<H3>The save was successful</H3>
	<FORM name=DTForm>
	    <INPUT type="hidden" name="dbid" value="<%=Request("dbid")%>">
		<INPUT type="button" value="Return to Select Parameter Page" id=cancel name=cancel LANGUAGE=javascript onclick="return cancel_onclick()">
	</FORM>
<%		Response.End
	End If%>
<FORM name="DTForm" method="post" action="TestExtPlan.asp">
<INPUT type="hidden" name=KCount value=<%=KeyCount%>>
<INPUT type="hidden" name=TPCount value=<%=Request("TPCount")%>>
<INPUT type="hidden" name=static value="<%=Request("static")%>">
<INPUT type="hidden" name="dbid" value="<%=Request("dbid")%>">
<INPUT type="hidden" name="dbname" value="<%=Request("dbname")%>">
<INPUT type="hidden" name=dbdicid value=<%=dbdicid%>>
<INPUT type="hidden" name=dbdicname value=<%=dbname%>>
<INPUT type="hidden" name="fids" value="<%=Request("fids")%>">
<INPUT type="hidden" name="fnames" value="<%=Request("fnames")%>">
<INPUT type="hidden" name="tbids" value="<%=Request("tbids")%>">
<INPUT type="hidden" name="tbnames" value="<%=Request("tbnames")%>">
<INPUT type="hidden" name="joinclause" value="<%=Request("joinclause")%>">
<INPUT type="hidden" name="Parameter" value="<%=Request("Parameter")%>">
<INPUT type="hidden" name="ParameterName" value="<%=Request("ParameterName")%>">
<INPUT type="hidden" name="ParameterID" value="<%=Request("ParameterID")%>">
<INPUT type="hidden" name="ParameterIDList" value="<%=Request("ParameterIDList")%>">
<INPUT type="hidden" name="filtercount" value=<%=Request("filtercount")%>>
<INPUT type="hidden" name="Units" value=<%=Request("Units")%>>
<%	'set up a loop to list the filter stuff
	fcount = Request("filtercount")
	For i = 0 to (fcount - 1)%>
<INPUT type="hidden" name="keyid<%=i%>" value="<%=Request("keyid" & i)%>">
<INPUT type="hidden" name="field<%=i%>" value="<%=Request("field" & i)%>">
<INPUT type="hidden" name="operator<%=i%>" value="<%=Request("operator" & i)%>">
<%	Next%>
<INPUT type="hidden" name="ExtPlanID" value="<%=Request("ExtPlanID")%>">
The SQL string is: <%=strSQL%><BR><BR>
<INPUT type="hidden" name="unique" value="<%=Request("unique")%>">
<hr>
<%	If Request("filtercount") > 0 and Request("testplan") = "" Then%>
		<UL>
			<LI><%=Request("filtercount")%> key values are required to execute this extraction plan. Enter the values in the spaces below and hit the "Test Extraction Plan" button to continue.</LI>
		</UL>
		<TABLE border="1">
			<TR>
				<TH>Field Name</TH>
				<TH>Value</TH>
			<TR>
<%		fcount = Request("filtercount")
		For i = 0 to (fcount - 1)%>
			<TR>
				<TD><%=mid(Request("field" & i),instr(1,Request("field" & i),",") + 1)%></TD>
				<TD><INPUT type="text" id=value<%=i%> name=value<%=i%>><br></TD>
			</TR>
<%		Next%>
		</TABLE>
<INPUT type="submit" value="Test Extraction Plan" id=testplan name=testplan>
<%	Elseif Request("testplan") = "" Then %>
        <INPUT type="submit" value="Test Extraction Plan" id=testplan name=testplan>
<%	ElseIf Request("testplan") <> "" Then
		' first check to see if we have any conversions to worry about
		If Request("Units") <> "none" Then
			units = Request("Units")
			' there are conversions to be implemented--here we will create
			'	an array that contains (1) the fieldname of the field to be
			'	converted, (2) the conversion factor(s), and (3) the direction
			'	of the conversion (F means multiply, R means divide)
			Dim ConvertArray()
			arraycount = 0
			currentloc = 1

			Do While True
				' check to see if there is only one entry left in the list
				If (instr(currentloc,units,",")) = 0 Then
					' add a new element to the array
					arraycount = arraycount + 1
					ReDim Preserve ConvertArray(5, arraycount - 1)
					' there is no comma in the list so there is only one
					'	field to be converted
					tildaloc = Instr(currentloc,units,"~")
					periodloc = Instr(currentloc,units,".")
					' extract the fieldname
					ConvertArray(0, arraycount - 1) =  Mid(units,currentloc,tildaloc - currentloc)
					' extract the conversion id
					convertID = Mid(units,tildaloc + 1, periodloc - tildaloc - 1)
					' extract the direction of the conversion
					ConvertArray(5,arraycount - 1) = Mid(units, periodloc + 1)
					' next we need to generate an SQL statement to extract
					'	the conversion factors so that they can be loaded
					'	into the array
					strSQL = "SELECT " & _
								"OffsetFactor, " & _
								"LinearFactor, " & _
								"QuadraticFactor, " & _
								"CubicFactor " & _
							 "FROM " & _
								"tblConversion " & _
							 "WHERE " & _
								"ConvertID = " & convertID
					' create a recordset object to return the conversion factors
					Set rsFactors = Server.CreateObject("ADODB.Recordset")
					' return the recordset
					rsFactors.Open strSQL, objCon
					' load the factors into the array
					ConvertArray(1,arraycount - 1) = rsFactors.Fields("OffsetFactor").Value
					ConvertArray(2,arraycount - 1) = rsFactors.Fields("LinearFactor").Value
					ConvertArray(3,arraycount - 1) = rsFactors.Fields("QuadraticFactor").Value
					ConvertArray(4,arraycount - 1) = rsFactors.Fields("CubicFactor").Value
					' close the recordset
					rsFactors.Close
					Set rsFactors = Nothing
					' all the infomation is now stored in the array--exit the loop
					Exit Do
				Else
					' here we have more than one element left to process
					' add a new element to the array
					arraycount = arraycount + 1
					ReDim Preserve ConvertArray(5,arraycount - 1)
					' there is no comma in the list so there is only one
					'	field to be converted
					tildaloc = Instr(currentloc,units,"~")
					periodloc = Instr(currentloc,units,".")
					' extract the fieldname
					ConvertArray(0,arraycount - 1) =  Mid(units,currentloc,tildaloc - currentloc)
					' extract the conversion id
					convertID = Mid(units,tildaloc + 1, periodloc - tildaloc - 1)
					' extract the direction of the conversion
					ConvertArray(5,arraycount - 1) = Mid(units, periodloc + 1, Instr(currentloc,units,",") - periodloc - 1)
					' next we need to generate an SQL statement to extract
					'	the conversion factors so that they can be loaded
					'	into the array
					strSQL = "SELECT " & _
								"OffsetFactor, " & _
								"LinearFactor, " & _
								"QuadraticFactor, " & _
								"CubicFactor " & _
							 "FROM " & _
								"tblConversion " & _
							 "WHERE " & _
								"ConvertID = " & convertID
					' create a recordset object to return the conversion factors
					Set rsFactors = Server.CreateObject("ADODB.Recordset")
					' return the recordset
					rsFactors.Open strSQL, objCon
					' load the factors into the array
					ConvertArray(1,arraycount - 1) = rsFactors.Fields("OffsetFactor").Value
					ConvertArray(2,arraycount - 1) = rsFactors.Fields("LinearFactor").Value
					ConvertArray(3,arraycount - 1) = rsFactors.Fields("QuadraticFactor").Value
					ConvertArray(4,arraycount - 1) = rsFactors.Fields("CubicFactor").Value
					' close the recordset
					rsFactors.Close
					Set rsFactors = Nothing
					' now relocate currentloc for the next element
					currentloc = Instr(currentloc,units,",") + 1
				End If
			Loop
		Else
			' set the arraycount to 0
			arraycount = 0
		End If


		' first output the record count%>
		<P>Number of Records Returned: <%=rsExtPlan.RecordCount%></P>
<%		' next build a table to hold the recordset returned%>
		<B>TIP</B> : A * in the column header indicates that a unit conversion is taking place for the fields in that column<BR><BR>
		<TABLE BORDER=1>
			<TR>
<%				' set up a loop to list the field names
				For Each field In rsExtPlan.Fields
					' check to see if any conversions need to be performed
					If arraycount > 0 Then
						' check to see if the current field is one to be converted
						' initialize a boolean variable indicating whether the field name
						'	has been printed or not
						blnPrinted = false
						For i = 0 to arraycount -1
							If ConvertArray(0,i) = field.Name Then
								'here we need to indicate that this field will be converted%>
				<TH><%=field.Name%>*</TH>
<%								' indicate that the field was printed
								blnPrinted = True
								' now break out of the loop
								Exit For
							End If
						Next
						' check to see if the field name has been printed yet
						If blnPrinted = false Then
							' this is a non-converted field--print the field name%>
				<TH><%=field.Name%></TH>
<%						End If
					Else
						' here no fields will be converted--store as usual%>
				<TH><%=field.Name%></TH>
<%					End If
				Next%>
			</TR>
<%			' set up a loop to load the returned fields
			Do While not rsExtPlan.EOF%>
			<TR>
<%				For Each field In rsExtPlan.Fields
					' check to see if any conversions need to be performed
					If arraycount > 0 Then
						' check to see if the current field is one that needs to be
						'	converted
						blnPrinted = false
						For i = 0 to arraycount - 1
							If ConvertArray(0,i) = field.Name Then
								' this is a field we need to convert--perform the conversion
								' check to see if it is a forward or reverse conversion
								If ConvertArray(5,i) = "F" Then
									'it is a forward conversion
									varTemp = ConvertArray(1,i) + (ConvertArray(2,i) * field.Value) + _
												(ConvertArray(3,i) * (field.Value)^2) + _
												(ConvertArray(4,i) * (field.Value)^3)
								Else
									' it is a reverse conversion
									varTemp = field.Value / ConvertArray(2,i)
								End If
								' now we can print out the field%>
				<TD><%=varTemp%></TD>
<%								' indicate that the field was printed
								blnPrinted = true
								' break out of the for loop
								Exit For
							End If
						Next
						' check to see if the field has been printed
						If blnPrinted = false Then
							' print out the field with no conversion%>
				<TD><%=field.Value%></TD>
<%						End If
					Else
						' here no conversions need to be performed--print out the value%>
				<TD><%=field.Value%></TD>
<%					End If
				Next%>
			</TR>
<%				' move the recordpointer
				rsExtPlan.MoveNext
			Loop
			rsExtPlan.Close
			Set rsExtPlan = Nothing%>
		</TABLE>
<HR>
<P> Press the "Save Extraction Plan" button to save this plan in the database or press the "Cancel" button to
	return to the Select Parameter screen.</P>
<INPUT type="submit" value="Save Extraction Plan" id=saveplan name=saveplan>&nbsp;<INPUT type="button" value="       Cancel       " id=cancel name=cancel LANGUAGE=javascript onclick="return cancel_onclick()">
</FORM>
<%	End If%>
</FORM>
</BODY>
</HTML>
