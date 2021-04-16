<%@ Language=VBScript %>
<!-- #include file = "MasterDB.asp" -->
<%
	'set up line feed identifier
	cr = vbCrLf

	' set up local variables
	dbid = Request("dbid")
	dbName = Request("dbname")
	KCount = Request("KCount")
	dbdicid = Request("dbdicid")
	dbdicname = Request("dbdicname")
	TPCount = Request("TPCount")

	'create connection to the database
	Set objCon = Server.CreateObject("ADODB.Connection")
	objCon.Open MasterConnect

%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
<TITLE>Extraction Plan -- Join Selection</TITLE>
</HEAD>
<body background="frames.jpg">
<H2>Extraction Plan -- Join Selection</H2>
Database ID Selected: <%=dbid%><BR>
Database Selected:	<%=dbName%><br>
Dictionary Selected: <%=dbdicname%><BR>
Parameter Selected: <%=Request("ParameterName")%><br>
<%
    Set rsObject = Server.CreateObject("ADODB.Recordset")
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
	Loop%>
Parameter Keys: <%=strTemp%><br>
<%
     If Request("Parameter") = "" Then
		if TPCount = 1 then
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
<%		'process selected field information
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
Field Source(s): <%=fNames%>
<%	If Request("joinclause") <> "" then
		JClause = Request("joinclause")
	Else
		If Request("ExtPlanID") <> "" Then
			' set up a query to extract the predefined join (if there is one)
			strGetJoin = "SELECT " & _
							"JoinClause " & _
						 "FROM " & _
							"tblExtPlanJoin " & _
						"WHERE " & _
							"ExtPlanID = " & Request("ExtPlanID")
			' define a recordset object to hold the resultset
			Set rsGetJoin = Server.CreateObject("ADODB.Recordset")
			' execute the query
			rsGetJoin.Open strGetJoin,objCon
			' store the value in a local variable
			If not rsGetJoin.EOF Then
				strJoinClause = rsGetJoin.Fields("JoinClause").Value
			Else
				strJoinClause = ""
			End If
			' close and deallocate the recordset
			rsGetJoin.Close
			Set rsGetJoin = Nothing
		End If
	End If
	'add any additional join clauses that have been previously defined
	If Request("jleft") <> "" then
		'extract the table names
		LTable = mid(Request("jleft"),1,instr(1,Request("jleft"),".") - 1)
		RTable = mid(Request("jright"),1,instr(1,Request("jright"),".") - 1)
		If JClause <> "" Then
			'add the "AND"
			JClause = JClause & " AND "
		End If
		JClause = JClause & Request("jleft") & " " & Request("joperator") & " " & Request("jright") & vbCrLf
		'determine the joine type
		'Select case Request("jtype")
		'	Case 1
				'this is an inner join
				'JClause = JClause & LTable & " INNER JOIN " & RTable & " ON " & Request("jleft") & " " & Request("joperator") & " " & Request("jright") & vbCrLf
		'	Case 2
				'check the operator--it can only be =
		'		If Request("joperator") = "=" Then
					'this is an outer join
		'			JClause = JClause & LTable & " LEFT OUTER JOIN " & RTable & " ON " & Request("jleft") & " " & Request("joperator") & " " & Request("jright") & vbCrLf
		'		Else
		'			Response.Write("<SCRIPT LANGUAGE=javascript>" & vbCrLf)
		'			Response.Write("alert(""Only the = operator can be used with outer join types."");" & vbCrLf)
		'			Response.Write("</SCRIPT>" & vbCrLf)
		'		End If
		'	Case 3
				'check the operator--it can only be =
		'		If Request("joperator") = "=" Then
					'this is an outer join
		'			JClause = JClause & RTable & " LEFT OUTER JOIN " & LTable & " ON " & Request("jright") & " " & Request("joperator") & " " & Request("jleft") & vbCrLf
		'		Else
		'			Response.Write("<SCRIPT LANGUAGE=javascript>" & vbCrLf)
		'			Response.Write("alert(""Only the = operator can be used with outer join types."");" & vbCrLf)
		'			Response.Write("</SCRIPT>" & vbCrLf)
		'		End If
		'End Select
	End If
%>
<FORM name=DTForm1 method="post" action="DefineJoin.asp">
<INPUT type="hidden" name=dbname value="<%=dbName%>">
<INPUT type="hidden" name=dbid value=<%=dbid%>>
<INPUT type="hidden" name="tbids" value="<%=Request("tbids")%>">
<INPUT type="hidden" name="tbnames" value="<%=Request("tbnames")%>">
<INPUT type="hidden" name="fids" value="<%=fIDs%>">
<INPUT type="hidden" name="fnames" value="<%=fNames%>">
<INPUT type="hidden" name=dbdicid value=<%=dbdicid%>>
<INPUT type="hidden" name=dbdicname value=<%=dbname%>>
<INPUT type="hidden" name=dbdicname value=<%=dbname%>>
<INPUT type="hidden" name=Units value="<%=units%>">
<%	If strJoinClause <> "" Then%>
<INPUT type="hidden" name="joinclause" value="<%=strJoinClause%>">
<%	Else%>
<INPUT type="hidden" name="joinclause" value="<%=JClause%>">
<%	End If%>
<INPUT type="hidden" name=ParameterID value=<%=Request("ParameterID")%>>
<INPUT type="hidden" name=ParameterName value=<%=Request("ParameterName")%>>
<INPUT type="hidden" name=ExtPlanID value=<%=Request("ExtPlanID")%>>
<INPUT type="hidden" name="Parameter" value="<%=parameter%>">
<INPUT type="hidden" name=ParameterIDlist value=<%=Request("ParameterIDlist")%>>
<INPUT type="hidden" name=KCount value=<%=KCount%>>
<INPUT type="hidden" name=TPCount value=<%=Request("TPCount")%>>
<UL>
	<LI>
		<P>Build the table join criteria using the tools below:</P>
	</LI>
</UL>
<%
	'create an array of tablename.fieldname to populate the drop down list boxes
	Set rsDropPop = Server.CreateObject("ADODB.Recordset")
	strSQL = "SELECT TableName, FieldName FROM tblFields WHERE TableID in (" & Request("tbids") & ")"
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
	Loop
	rsDropPop.Close
%>
<TABLE WIDTH=75% BORDER=1 CELLSPACING=1 CELLPADDING=1>
	<TR>
		<TD>Left:</TD>
		<TD>Operator:</TD>
		<TD>Right:</TD>
	</TR>
	<TR>
		<TD>
			<SELECT name=jleft>
				<OPTION selected> </OPTION>
			<%	For i = 0 to intCount%>
					<OPTION><%=arrDropPop(i)%></OPTION>
			<%	Next%>
			</SELECT>
		</TD>
		<TD>
			<SELECT name=joperator>
			<OPTION>=</OPTION>
			<OPTION><></OPTION>
			<OPTION>></OPTION>
			<OPTION><</OPTION>
			<OPTION>>=</OPTION>
			<OPTION><=</OPTION>
			</SELECT>
		</TD>
		<TD>
			<SELECT name=jright>
				<OPTION selected> </OPTION>
			<%	For i = 0 to intCount%>
					<OPTION><%=arrDropPop(i)%></OPTION>
			<%	Next%>
			</SELECT>
		</TD>
	</TR>
</TABLE>
<%
'Join Includes:<br>
'<INPUT type="radio" name=jtype value=1 checked>1.  Only records from the two tables where the left field = the right field<br>
'<INPUT type="radio" name=jtype value=2>2.  All records from the left table and only records from the right table where the left field = the right field<br>
'<INPUT type="radio" name=jtype value=3>3.  All records from the right table and only records from the left table where the left field = the right field<br>
%>
<INPUT type="submit" value="Add Join" id=submit2 name=submit2>
</FORM>
<HR>
<FORM name=DTForm method="post" action="DefineFilter.asp">
<INPUT type="hidden" name=dbid value=<%=dbid%>>
<INPUT type="hidden" name=dbdicid value=<%=dbdicid%>>
<INPUT type="hidden" name=dbdicname value=<%=dbname%>>
<INPUT type="hidden" name="TPCount" value=<%=Request("TPCount")%>>
<INPUT type="hidden" name="Parameter" value="<%=parameter%>">
<INPUT type="hidden" name=ParameterIDlist value=<%=Request("ParameterIDlist")%>>
<INPUT type="hidden" name=KCount value=<%=KCount%>>
<INPUT type="hidden" name="tbids" value="<%=Request("tbids")%>">
<INPUT type="hidden" name="tbnames" value="<%=Request("tbnames")%>">
<INPUT type="hidden" name="fids" value="<%=fIDs%>">
<INPUT type="hidden" name="fnames" value="<%=fNames%>">
<INPUT type="hidden" name="fCount" value="<%=intCount - 1%>">
<INPUT type="hidden" name=dbname value="<%=dbName%>">
<INPUT type="hidden" name="Units" value="<%=units%>">
<%	' determine if there is a existing Extration Plan
	If Request("ExtPlanID") <> "" And JClause = "" Then
		' set up a query to extract the predefined join (if there is one)
		strGetJoin = "SELECT " & _
						"JoinClause " & _
					 "FROM " & _
						"tblExtPlanJoin " & _
					 "WHERE " & _
						"ExtPlanID = " & Request("ExtPlanID")
		' define a recordset object to hold the resultset
		Set rsGetJoin = Server.CreateObject("ADODB.Recordset")
		' execute the query
		rsGetJoin.Open strGetJoin,objCon
		' store the value in a local variable
		If not rsGetJoin.EOF Then
			strJoinClause = rsGetJoin.Fields("JoinClause").Value
		Else
			strJoinClause = ""
		End If
		' close and deallocate the recordset
		rsGetJoin.Close
		Set rsGetJoin = Nothing
	End If%>
<UL>
	<LI>
		<P>Defined joins in the transformation (TIP: Format is TABLE1_NAME.FIELD1 = TABLE2_NAME.FIELD2 AND TABLE1.FIELD3 = TABLE3.FIELD4....):</P>
	</LI>
</UL>
<%	If Request("ExtPlanID") <> "" And JClause = "" Then%>
<TEXTAREA rows=3 cols=70 name=joinclause><%Response.Write(strJoinClause)%></TEXTAREA>
<%	Else%>
<TEXTAREA rows=3 cols=70 name=joinclause><%Response.Write(JClause)%></TEXTAREA>
<%	End If%>
<INPUT type="hidden" name=ParameterID value=<%=Request("ParameterID")%>>
<INPUT type="hidden" name=ParameterName value=<%=Request("ParameterName")%>>
<INPUT type="hidden" name=ExtPlanID value=<%=Request("ExtPlanID")%>>
<P><INPUT type="submit" value="Continue" id=submit1 name=submit1></P>
</FORM>
</BODY>
<%
	objCon.Close
	Set rsDropPop = Nothing
	Set rsFields = Nothing
	Set objCon = Nothing
%>
</HTML>
