<%@ Language=VBScript %>
<!-- #include file = "MasterDB.asp" -->
<%
	'set up line feed identifier
	cr = vbCrLf

	' set up local variables
	dbid = Request("dbid")
	dbName = Request("dbname")
	ExtPlanID = Request("ExtPlanID")
	dbdicid = Request("dbdicid")
	dbdicname = Request("dbdicname")
	ListCount = Request("listcount")
	parametername = Request("ParameterName")
	TPCount = Request("TPCount")

	'create connection to the database
	Set objCon = Server.CreateObject("ADODB.Connection")
	objCon.Open MasterConnect
	'set up line feed identifier

	' load up the parameter information that was selected on the previous page
    If Request("Parameter") = "" Then
		If TPCount = 1 Then
			parameter = Request("ParameterID") & "." & Request("ParameterName")
		else
			For i = 0 to Request("TPCount")-1
				if parameter = "" then
					parameter = Request("param" & i)
				else
					parameter = parameter & "," & Request("param" & i)
				end if
			next
		end if
	Else
		parameter = Request("Parameter")
	End If
%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
<TITLE>Extraction Plan -- Select Units of Database Field(s)</TITLE>
</HEAD>
<body background="frames.jpg">
<H2>Extraction Plan -- Select Units of Database Field(s)</H2>
Database ID Selected: <%=dbid%><BR>
Database Selected:	<%=dbName%><br>
Dictionary Selected: <%=dbdicname%><BR>
Parameter Selected: <%=Request("ParameterName")%><br>

<%  Set rsObject = Server.CreateObject("ADODB.Recordset")
    strSQL = "SELECT " & _
				"K.KeyName " & _
			 "FROM " & _
				"tblParamKeys as K, tblParameter as P " & _
			 "WHERE " & _
				"K.ParameterID = P.ParameterID " & _
			 "AND " & _
				"P.DicID = " & dbdicid & " " & _
			 "AND " & _
				"K.ParameterID = " & Request("ParameterID")
    'strSQL = "Select KeyName From tblParamKeys K Where K.ParameterID = " & Request("ParameterID")
	rsObject.Open strSQL,objCon

	reccount = 0
	strTemp = ""
	Do While not rsObject.EOF
		reccount = reccount + 1
		If reccount = 1 Then
			strTemp = Trim(rsObject.Fields("KeyName").Value)
		Else
			strTemp = strTemp & "," & rsObject.Fields("KeyName").Value
		End If
		rsObject.MoveNext
	Loop
%>
Parameter Keys: <%=strTemp%><br>
<%	'process selected field information
	fIDs = Request("fids")
	fNames = Request("fnames")%>
Table Source(s): <%=Request("tbnames")%><BR>
Field Source(s): <%=fNames%>
<UL>
	<LI>Select the units for the field(s)</LI>
</UL>
<% KCount = 0
		' determine if we need if the user has selected multiple tables
		If ListCount > 1 Then%>
		<FORM name=DTForm method="post" action="DefineJoin.asp">
<%		Else
'			If KeyTemp < 1 Then
'		<FORM name=DTForm method="post" action="TestExtPlan.asp">
'		<INPUT type="hidden" name="filtercount" value=0>
'			Else%>
		<FORM name=DTForm method="post" action="DefineFilter.asp">
<%		End If%>

<%	' we need to extract the fields and their corresponding parameters
	'	to determine which ones need the units selected

	' first lets parse out the field id and corresponding parameter id from
	'	the lists
	period1 = 1
    period2 = Instr(1,parameter,".")
    comma1 = instr(1,parameter,",")
    If comma1 = 0 Then
		comma1 = Len(parameter) + 1
	End If
    comma2 = instr(comma1 + 1,parameter,",")
    fieldstart = 1
    fieldstop = Instr(fieldstart,fnames,",")
    If fieldstop = 0 Then
		fieldstop = Len(fnames) + 1
	End If
	For x=0 To TPCount -1
	    ' extract the field name from the list
	    fieldName = Mid(fnames,fieldstart, fieldstop - fieldstart)
		' extract the id for the corresponding parameter
	    TPID = Mid(parameter,period1,period2 - period1)
		' extract the name for the corresponding parameter
	    TPName = Mid(parameter,period2 + 1,comma1 - period2 -1)
		' create a query to determine if the parameter has units
		strSQL = "SELECT " & _
					"ParameterUnits " & _
				 "FROM " & _
					"tblParameter " & _
				 "WHERE ParameterID = " & TPID
		' return the recordset
		Set rsUnits = Server.CreateObject("ADODB.Recordset")
		rsUnits.Open strSQL, objCon
		' store the units in a local variable
		unitsTemp = rsUnits.Fields("ParameterUnits").Value
		' close and deallocate the recordset
		rsUnits.Close
		Set rsUnits = Nothing
		' check to see if the current parameter has units
		If unitsTemp <> "" Then
			' next we need to create a SQL statement to retrieve all the available
			'	unit types (the ones we can convert from and to)
			strSQL = "SELECT DISTINCT " & _
						"ConvertID, " & _
						"FromUnits " & _
					 "FROM " & _
						"tblConversion " & _
					 "WHERE " & _
						"ToUnits = '" & unitsTemp & "'"
			' create a recordset object to return the list
			Set rsFromTo = Server.CreateObject("ADODB.Recordset")
			rsFromTo.Open strSQL, objCon
			' Next we need to check if there are any reverse conversions that were not included.
			'	In order to do this with out repetition, we can build a large "not in" list from
			'	the recordset we just opened
			' initialize strNotIn
			strNotIn = "("
			Do While not rsFromTo.EOF
				If strNotIn = "(" then
					strNotIn = strNotIn & "'" & rsFromTo.Fields("FromUnits") & "'"
				Else
					strNotIn = strNotIn & ",'" & rsFromTo.Fields("FromUnits") & "'"
				End If
				rsFromTo.MoveNext
			Loop
			' end the "not in" string with a parenthesis
			strNotIn = strNotIn & ")"
			' reset the recordpointer for rsFromTo
			If not rsFromTo.BOF Then
				rsFromTo.MoveFirst
			End If
			' now build the SQL statement to get the unique reverse conversions
			strSQL = "SELECT DISTINCT " & _
						"ConvertID, " & _
						"ToUnits " & _
					 "FROM " & _
						"tblConversion " & _
					 "WHERE " & _
						"FromUnits = '" & unitsTemp & "' " & _
					 "AND " & _
						"OffsetFactor = 0 "
			' here we need to check and see if strNotIn is empty
			If not strNotIn = "()" Then
				strSQL = strSQL  & "AND " & _
							"ToUnits Not In " & strNotIn
			End If
			' create a recordset object to retrieve the recordset
			Set rsToFrom = Server.CreateObject("ADODB.Recordset")
			' return the recordset
			rsToFrom.Open strSQL, objCon
			' now we have the fieldname and a list of the available units it could be in
			'	next we need to render this information to the user%>
			<BR><%=fieldName%>&nbsp;
			<SELECT name=units<%=x%>>
				<OPTION value="<%=TPName & "~"%>-1.F"><%=unitsTemp%></OPTION>
<%			' add the records in from rsFromTo
			Do While Not rsFromTo.EOF%>
				<OPTION value="<%=TPName & "~" & rsFromTo.Fields("ConvertID").Value & "." & "F"%>"><%=rsFromTo.Fields("FromUnits").Value%></OPTION>
<%				rsFromTo.MoveNext
			Loop
			' next add the records in from rsToFrom
			Do While Not rsToFrom.EOF%>
				<OPTION value="<%=TPName & "~" & rsToFrom.Fields("ConvertID").Value & "." & "R"%>"><%=rsToFrom.Fields("ToUnits").Value%></OPTION>
<%				rsToFrom.MoveNext
			Loop%>
			</SELECT>
<%			' close  and deallocate the recordsets
			rsToFrom.Close
			rsFromTo.Close
			Set rsToFrom = Nothing
			Set rsFromTo = Nothing
		End If

		' now move the pointers to the next field and paramter
		fieldstart = fieldstop + 1
		fieldstop = Instr(fieldstart + 1,fnames,",")
		If fieldstop = 0 Then
			fieldstop = Len(fnames) + 1
		End If
		period1 = comma1 +1
		period2 = Instr(comma1,parameter,".")
		If comma2 = 0 Then
			comma1 = Len(parameter) + 1
		Else
			comma1 = comma2
		End If
		comma2 = instr(comma1+1,parameter,",")
	Next%>

<INPUT type="hidden" name=dbid value=<%=dbid%>>
<INPUT type="hidden" name=dbname value="<%=dbname%>">
<INPUT type="hidden" name=dbdicid value=<%=dbdicid%>>
<INPUT type="hidden" name=dbdicname value="<%=dbdicname%>">
<INPUT type="hidden" name="tbids" value="<%=Request("tbids")%>">
<INPUT type="hidden" name="tbnames" value="<%=Request("tbnames")%>">
<INPUT type="hidden" name="fids" value="<%=fIDs%>">
<INPUT type="hidden" name="fnames" value="<%=fNames%>">
<INPUT type="hidden" name=ParameterIDlist value=<%=Request("ParameterIDlist")%>>
<INPUT type="hidden" name=ParameterID value=<%=Request("ParameterID")%>>
<INPUT type="hidden" name=ParameterName value=<%=Request("ParameterName")%>>
<INPUT type="hidden" name=ExtPlanID value=<%=Request("ExtPlanID")%>>
<INPUT type="hidden" name=TPCount value=<%=TPCount%>>
<INPUT type="hidden" name=KCount value=<%=KCount%>>
<INPUT type="hidden" name="fCount" value="<%=intCount - 1%>">
<INPUT type="hidden" name="Parameter" value="<%=parameter%>">
<INPUT type="hidden" name="Caller" value="SelectUnits.asp">
<P><INPUT type="submit" value="Continue" id=submit1 name=submit1></P>
</FORM>
</BODY>

<%	objCon.Close

	Set objCon = Nothing
	Set rsobject = Nothing
%>
</HTML>
