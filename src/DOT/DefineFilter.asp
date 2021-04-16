<%@ Language=VBScript %>
<!-- #include file = "MasterDB.asp" -->
<%
	'set up line feed identifier
	cr = vbCrLf

	' set up local variables
	dbid = Request("dbid")
	dbName = Request("dbName")
	dbdicid = Request("dbdicid")
	dbdicname = Request("dbdicname")
	KCount = Request("KCount")

	'create connection to the database
	Set objCon = Server.CreateObject("ADODB.Connection")
	objCon.Open MasterConnect


%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
<TITLE>Extraction Plan -- Filter Selection</TITLE>
</HEAD>
<body background="frames.jpg">
<H2>Extraction Plan -- Filter Selection</H2>
Database ID Selected: <%=dbid%><BR>
Database Selected:	<%=dbName%><br>
Dictionary Selected: <%=dbdicname%><BR>
Parameter Selected: <%=Request("ParameterName")%><br>
<%
    Set rsObject = Server.CreateObject("ADODB.Recordset")
    'strSQL = "Select Count(KeyName),KeyName,KeyID From tblParamKeys K Where K.ParameterID = " & Request("ParameterID") & "group by KeyName,KeyID"
    'Response.Write "SELECT tblParamKeys.KeyID, tblParamKeys.KeyName, tblParameter.ParameterDesc FROM tblParamKeys INNER JOIN tblParameter ON tblParamKeys.KeyName = tblParameter.ParameterName Where tblParamKeys.ParameterID = " & Request("ParameterID")

	strSQL = "SELECT " & _
				"tblParamKeys.KeyID, " & _
			    "tblParamKeys.KeyName, " & _
			    "tblParameter.ParameterDesc " & _
			  "FROM " & _
				"tblParamKeys " & _
			  "INNER JOIN " & _
				"tblParameter " & _
			  "ON " & _
				"tblParamKeys.KeyName = tblParameter.ParameterName " & _
			  "WHERE " & _
				"tblParamKeys.ParameterID = " & Request("ParameterID") & " " & _
			  "AND " & _
				"tblParameter.DicID = " & dbdicid
	rsObject.Open strSQL,objCon

	Set rsObjectP = Server.CreateObject("ADODB.Recordset")
    strSQL = "SELECT " & _
				"K.KeyName " & _
			 "FROM " & _
				"tblParamKeys K, tblParameter P  " & _
			 "WHERE " & _
				"K.ParameterID = " & Request("ParameterID") & " " & _
			 "AND " & _
				"P.ParameterID = K.ParameterID " & _
			 "AND " & _
				"P.DicID = " & dbdicid
	rsObjectP.Open strSQL,objCon
	reccount = 0
	strTemp = ""
	Do While not rsObjectP.EOF
		reccount = reccount + 1
		If reccount = 1 Then
			strTemp = Trim(rsObjectP.Fields("KeyName").Value)
		Else
			strTemp = strTemp & "," & rsObjectP.Fields("KeyName").Value
		End If
		rsObjectP.MoveNext
	Loop
	rsObjectP.Close
	Set rsObjectP = Nothing%>
Parameter Keys: <%=strTemp%><br>
<%   If Request("parameter") = "" Then
		if Request("TPCount") = 1 then
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

	 ' check to see if the units have been passed in
	If Request("Units") = "" Then
		If Request("Caller") = "SelectUnits.asp" Then
			For i = 0 to Request("TPCount")-1
				If Request("units" & i) <> "" Then
					' check to see if a conversion is needed
					tildaloc = Instr(1,Request("units" & i),"~")
					periodloc = Instr(1,Request("units" & i),".")
					varTemp = Mid(Request("units" & i),tildaloc + 1,periodloc - tildaloc - 1)
					' check to see if varTemp is not -1
					If varTemp <> "-1" Then
						if units = "" then
							units = Request("units" & i)
						else
							units = units & "," & Request("units" & i)
						end if
					End If
				End If
			next
			' check to see if any conversions were loaded into the string
			If units = "" Then
				units = "none"
			End If
		Else
			units = "none"
		End If
	Else
		units = Request("Units")
	End If
%>

<%	'process selected field information
	If request("fids") = "" Then
		NumFields = Request("fCount")
		For i = 0 to NumFields
			If Request("chk" & i) <> "" Then
				'create a comma delimited list of the table source ID's and names
				Set rsFields = Server.CreateObject("ADODB.Recordset")
				strSQL = "SELECT TableName,FieldName FROM tblFields WHERE FieldID = " & Request("chk" & i)
				rsFields.Open strSQL,objCon
				If fIDs = "" Then
					fIDs = Request("chk" & i)
					fNames = rsFields.Fields("TableName").Value & "." & rsFields.Fields("FieldName").Value
				Else
					fIDs = fIDs & "," & Request("chk" & i)
					fNames = fNames & ", " & rsFields.Fields("TableName").Value & "." & rsFields.Fields("FieldName").Value
				End If
				rsFields.Close
			End If
		Next
	Else
		fIDs = Request("fids")
		fNames = Request("fnames")
	End If
	%>
	Table Source(s): <%=Request("tbnames")%><BR>
	Field Source(s): <%=fNames%><BR>
<%	If Request("joinclause") <> "" then%>
	Join Clause:   <%=Request("joinclause")%><br>
<%	End If
	'create an array of tablename.fieldname to populate the drop down list boxes
	Set rsDropPop = Server.CreateObject("ADODB.Recordset")
	strSQL = "SELECT TableName, FieldName, FieldID FROM tblFields WHERE TableID in (" & Request("tbids") & ")"
	rsDropPop.Open strSQL,objCon
	intCount = -1
	Dim arrDropPop()
	Do while not rsDropPop.EOF
		intCount = intCount + 1
		ReDim Preserve arrDropPop(intCount)
		strTmpTableName = rsDropPop.Fields("TableName").Value
		If Instr(strTmpTableName," ") > 0 Then
			strTmpTableName = "[" + strTmpTableName + "]"
		End If
		strTmpFieldName = rsDropPop.Fields("FieldName").Value
		If Instr(strTmpFieldName," ") > 0 Then
			strTmpFieldName = "[" + strTmpFieldName + "]"
		End If
		arrDropPop(intCount) = strTmpTableName & "." & strTmpFieldName
		rsDropPop.MoveNext
	Loop%>

<%	' check to see if an there is an existing Extration Plan
	If Request("ExtPlanID") <> "" Then
		set rsObject2 = Server.CreateObject("ADODB.Recordset")
		strSQL = "Select ParamKeyID,FieldID,Operator From tblKeyFilter Where ExtPlanID = " & Request("ExtPlanID")
		rsObject2.Open strSQL,objCon
	End If%>
<UL>
  <LI>Build the filter criteria using the tools below:</LI>
</UL>
<FORM name=DTForm2 method="post" action="TestExtPlan.asp">
<INPUT type="hidden" name=dbname value="<%=dbName%>">
<INPUT type="hidden" name=dbid value=<%=dbid%>>
<INPUT type="hidden" name=dbdicid value=<%=dbdicid%>>
<INPUT type="hidden" name=dbdicname value=<%=dbname%>>
<INPUT type="hidden" name=KCount value=<%=KCount%>>
<INPUT type="hidden" name="TPCount" value=<%=Request("TPCount")%>>
<INPUT type="hidden" name="Parameter" value="<%=parameter%>">
<INPUT type="hidden" name=ParameterIDlist value=<%=Request("ParameterIDlist")%>>
<INPUT type="hidden" name="tbids" value="<%=Request("tbids")%>">
<INPUT type="hidden" name="tbnames" value="<%=Request("tbnames")%>">
<INPUT type="hidden" name="fids" value="<%=fIDs%>">
<INPUT type="hidden" name="fnames" value="<%=fNames%>">
<INPUT type="hidden" name="joinclause" value="<%=Request("joinclause")%>">
<INPUT type="hidden" name="filter" value="<%=tfilter%>">
<INPUT type="hidden" name="ParameterName" value="<%=Request("ParameterName")%>">
<INPUT type="hidden" name="ParameterID" value="<%=Request("ParameterID")%>">
<INPUT type="hidden" name="Units" value="<%=units%>">
Click here if you would like the results to be distinct: <INPUT TYPE="CHECKBOX" NAME="unique"><BR>
<%	' set up a loop to print out a filter definition
	'	section for each of the key values
	If Not(rsObject.EOF And rsObject.BOF) Then
		rsObject.MoveFirst
	End If
	count = 0
	Do While not rsObject.EOF
		' print out the key value%>
	<H3>Key value: <%=rsObject.Fields("KeyName").Value%> -- <%=rsObject.Fields("ParameterDesc").Value%></H3>
<TABLE WIDTH=75% BORDER=1 CELLSPACING=1 CELLPADDING=1>
	<INPUT type="hidden" name="keyid<%=count%>" value="<%=rsObject.Fields("KeyID").Value%>">
	<TR>
		<TD>Field:</TD>
		<TD>Operator:</TD>
	</TR>
	<TR>
		<TD>
			<SELECT name=field<%=count%>>
<%				' determine if there is an existing Extration Plan
				If Request("ExtPlanID") <> "" Then
					' an extraction plan existed--move the record pointer to
					'	the matching key
					rsObject2.MoveFirst
					blnFound = False
					Do While Not rsObject2.EOF And blnFound = False
						' check for a match
						If rsObject2.Fields("ParamKeyID").Value = rsObject.Fields("KeyID").Value Then
							' set blnFound
							blnFound = True
						Else
							' move to the next record
							rsObject2.MoveNext
						End If
					Loop
					' put in the blank...%>
					<OPTION></OPTION>
<%					' Now loop through the rest, looking for a match
					rsDropPop.MoveFirst
					For i = 0 to intCount
						' check to see if the curent field matches
						If CLng(rsDropPop.Fields("FieldID").Value) = CLng(rsObject2.Fields("FieldID").Value) Then%>
				<OPTION value="<%=rsDropPop.Fields("FieldID").Value & "," & arrDropPop(i)%>" selected><%=arrDropPop(i)%></OPTION>
<%						Else%>
				<OPTION value="<%=rsDropPop.Fields("FieldID").Value & "," & arrDropPop(i)%>"><%=arrDropPop(i)%></OPTION>
<%						End If
						rsDropPop.MoveNext
					Next%>
<%				Else%>
				<OPTION selected></OPTION>
				<%	rsDropPop.MoveFirst
					For i = 0 to intCount%>
				<OPTION value="<%=rsDropPop.Fields("FieldID").Value & "," & arrDropPop(i)%>"><%=arrDropPop(i)%></OPTION>
				<%		rsDropPop.MoveNext
					Next
				End If%>
			</SELECT>
		</TD>
		<TD>
			<SELECT name=operator<%=count%>>
<%				' determine if there is an existing extration plan
				If Request("ExtPlanID") <> "" Then
					' an extraction plan exists--make it match up%>
				<OPTION></OPTION>
<%					If Trim(rsObject2.Fields("Operator").Value) = "= v" Then%>
				<OPTION value="= v" selected>=</OPTION>
<%					Else%>
				<OPTION value="= v">=</OPTION>
<%					End If
					If Trim(rsObject2.Fields("Operator").Value) = "<> v" Then%>
				<OPTION value="<> v" selected><></OPTION>
<%					Else%>
				<OPTION value="<> v"><></OPTION>
<%					End If
					If Trim(rsObject2.Fields("Operator").Value) = "> v" Then%>
				<OPTION value="> v" selected>></OPTION>
<%					Else%>
				<OPTION value="> v">></OPTION>
<%					End If
					If Trim(rsObject2.Fields("Operator").Value) = "< v" Then%>
				<OPTION value="< v" selected><</OPTION>
<%					Else%>
				<OPTION value="< v"><</OPTION>
<%					End If
					If Trim(rsObject2.Fields("Operator").Value) = ">= v" Then%>
				<OPTION value=">= v" selected>>=</OPTION>
<%					Else%>
				<OPTION value=">= v">>=</OPTION>
<%					End If
					If Trim(rsObject2.Fields("Operator").Value) = "<= v" Then%>
				<OPTION value="<= v" selected><=</OPTION>
<%					Else%>
				<OPTION value="<= v"><=</OPTION>
<%					End If
					If Trim(rsObject2.Fields("Operator").Value) = "LIKE('v%')" Then%>
				<OPTION value="LIKE('v%')" selected>begins with</OPTION>
<%					Else%>
				<OPTION value="LIKE('v%')">begins with</OPTION>
<%					End If
					If Trim(rsObject2.Fields("Operator").Value) = "NOT LIKE('v%')" Then%>
				<OPTION value="NOT LIKE('v%')" selected>does not begin with</OPTION>
<%					Else%>
				<OPTION value="NOT LIKE('v%')">does not begin with</OPTION>
<%					End If
					If Trim(rsObject2.Fields("Operator").Value) = "LIKE('%v')" Then%>
				<OPTION value="LIKE('%v')" selected>ends with</OPTION>
<%					Else%>
				<OPTION value="LIKE('%v')">ends with</OPTION>
<%					End If
					If Trim(rsObject2.Fields("Operator").Value) = "NOT LIKE('%v')" Then%>
				<OPTION value="NOT LIKE('%v')" selected>does not end with</OPTION>
<%					Else%>
				<OPTION value="NOT LIKE('%v')">does not end with</OPTION>
<%					End If
					If Trim(rsObject2.Fields("Operator").Value) = "LIKE('%v%')" Then%>
				<OPTION value="LIKE('%v%')" selected>contains</OPTION>
<%					Else%>
				<OPTION value="LIKE('%v%')">contains</OPTION>
<%					End If
					If Trim(rsObject2.Fields("Operator").Value) = "NOT LIKE('%v%')" Then%>
				<OPTION value="NOT LIKE('%v%')" selected>does not contain</OPTION>
<%					Else%>
				<OPTION value="NOT LIKE('%v%')">does not contain</OPTION>
<%					End If
					If Trim(rsObject2.Fields("Operator").Value) = "LIKE('v')" Then%>
				<OPTION value="LIKE('v')" selected>like</OPTION>
<%					Else%>
				<OPTION value="LIKE('v')">like</OPTION>
<%					End If
					If Trim(rsObject2.Fields("Operator").Value) = "NOT LIKE('v')" Then%>
				<OPTION value="NOT LIKE('v')" selected>not like</OPTION>
<%					Else%>
				<OPTION value="NOT LIKE('v')">not like</OPTION>
<%					End If
					If Trim(rsObject2.Fields("Operator").Value) = "is Null" Then%>
				<OPTION selected>is Null</OPTION>
<%					Else%>
				<OPTION >is Null</OPTION>
<%					End If
					If Trim(rsObject2.Fields("Operator").Value) = "is Not Null" Then%>
				<OPTION selected>is Not Null</OPTION>
<%					Else%>
				<OPTION >is Not Null</OPTION>
<%					End If
				Else
					' no extraction plan exists%>
				<OPTION selected></OPTION>
				<OPTION value="= v">=</OPTION>
				<OPTION value="<> v"><></OPTION>
				<OPTION value="> v">></OPTION>
				<OPTION value="< v"><</OPTION>
				<OPTION value=">= v">>=</OPTION>
				<OPTION value="<= v"><=</OPTION>
				<OPTION value="LIKE('v%')">begins with</OPTION>
				<OPTION value="NOT LIKE('v%')">does not begin with</OPTION>
				<OPTION value="LIKE('%v')">ends with</OPTION>
				<OPTION value="NOT LIKE('%v')">does not end with</OPTION>
				<OPTION value="LIKE('%v%')">contains</OPTION>
				<OPTION value="NOT LIKE('%v%')">does not contain</OPTION>
				<OPTION value="LIKE('v')">like</OPTION>
				<OPTION value="NOT LIKE('v')">not like</OPTION>
				<OPTION>is Null</OPTION>
				<OPTION>is Not Null</OPTION>
<%				End If%>
			</SELECT>
		</TD>
</TABLE>
<%		' move the record pointer to the next record
		rsObject.MoveNext
		' increment count
		count = count + 1
	Loop
	rsDropPop.Close
	Set rsDropPop = Nothing%>

<%	' adding an additional text box for the user to enter any static filters
	' query the database to see if any additional filters exist already
	strStaticFilter = ""
	If Request("ExtPlanID") <> "" Then
		' build an SQL statement to extract the static filters
			strGetSF = "SELECT " & _
							"StaticFilter " & _
						 "FROM " & _
							"tblExtPlanStaticFilter " & _
						"WHERE " & _
							"ExtPlanID = " & Request("ExtPlanID")
			' define a recordset object to hold the resultset
			Set rsGetSF = Server.CreateObject("ADODB.Recordset")
			' execute the query
			rsGetSF.Open strGetSF,objCon
			' store the value in a local variable
			If not rsGetSF.EOF Then
				strStaticFilter = rsGetSF.Fields("StaticFilter").Value
			Else
				strStaticFilter = ""
			End If
			' close and deallocate the recordset
			rsGetSF.Close
			Set rsGetSF = Nothing
	End If%>
<HR><BR>
Add any additional static filters in the box below (TIP: Format is TABLE_NAME1.FIELD1 = [VALUE] AND TABLE_NAME2.FIELD2 <> 'TEXT-VALUE' OR TABLE_NAME3.FIELD3 IN (VALUE1,VALUE2)....):<BR><BR>
<TEXTAREA rows=3 cols=70 id=static name=static><%=strStaticFilter%></TEXTAREA><BR>
<INPUT type="hidden" name="filtercount" value="<%=count%>">
<INPUT type="hidden" name=ExtPlanID value=<%=Request("ExtPlanID")%>>
<HR>
<P><INPUT type="submit" value="Continue" id=submit2 name=submit2></P>
</FORM>
</BODY>
<%	rsObject.Close
	Set rsObject = Nothing
	objCon.Close
	Set rsFType = Nothing
	Set rsFields = Nothing
	Set objCon = Nothing%>
</HTML>
