<%@ Language=VBScript %>
<!-- #include file = "MasterDB.asp" -->
<%
	'set up line feed identifier
	cr = vbCrLf

	dbid = Request("dbid")
	dbName = Request("dbname")
	dbdicid = Request("dbdicid")
	dbdicname = Request("dbdicname")

	'create connection to the database
	Set objCon = Server.CreateObject("ADODB.Connection")
	objCon.Open MasterConnect

	'get the object name for the object selected
	Set rsObject = Server.CreateObject("ADODB.Recordset")
%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
<TITLE>Extraction Plan -- Table Selection</TITLE>
</HEAD>
<body background="frames.jpg">
<H2>Extraction Plan -- Table Selection</H2>
<FORM name=DTForm method="post" action="SelectFields.asp">
	<%
	strSQL = "Select ParameterName From tblParameter P  Where ParameterID = " & Request("ExtPlanParam")
	rsObject.Open strSQL,objCon%>
Database ID Selected: <%=dbid%><BR>
Database Selected:	<%=dbName%><br>
Dictionary Selected: <%=dbdicname%><BR>
Parameter Selected: <%=rsObject.Fields("ParameterName").Value%><br>

	<INPUT type="hidden" name=ParameterID value=<%=Request("ExtPlanParam")%>>
	<INPUT type="hidden" name=ParameterName value=<%=rsObject.Fields("ParameterName").Value%>>

<%	rsObject.Close
    strSQL = "Select KeyName From tblParamKeys K Where K.ParameterID = " & Request("ExtPlanParam")
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
Parameter Keys: <%=strTemp%>
<br>

<%	rsObject.Close
    strSQL = "SELECT " & _
				"ExtPlanSQL, " & _
				"ExtPlanID " & _
			 "FROM " & _
				"tblExtPlan " & _
			 "WHERE " & _
				"ParameterID = " & Request("ExtPlanParam") & " " & _
			 "AND " & _
				"DatabaseID = " & dbid
	rsObject.Open strSQL,objCon%>

    <%	If rsObject.EOF then %>
    <P>There is no Existing Extraction Plan for this Parameter.<BR></P>
    <%	Else%>
    <P>Current Extraction Plan for this Parameter:<BR><%=rsObject.Fields("ExtPlanSQL").Value%><BR></P>
	<%		' load the extraction plan id into a local
			ExtPlanID = rsObject.Fields("ExtPlanID").Value
			set rsObject2 = Server.CreateObject("ADODB.Recordset")
			strSQL = "Select TableID From tblExtPlanTables Where ExtPlanID = " & ExtPlanID
			rsObject2.Open strSQL,objCon,adOpenStatic,adLockOptimistic
			Do While not rsObject2.EOF
				TableIDs = TableIDs & "," & rsObject2.Fields("TableID").Value
				'If tidID = "" Then
					'tIDs = rsObject2.Fields("TableID").Value
				'Else
					'tIDs = tIDs & "," & rsObject2.Fields("TableID").Value
				'End If
				rsObject2.MoveNext
			Loop
			' add the final delimiter
			TableIDs = TableIDs + ","
			rsObject2.Close
			Set rsObject2 = Nothing
		End If
		rsObject.Close

	set rsSTables = Server.CreateObject("ADODB.Recordset")
	strSQL = "Select TableID,TableName From tblTables Where DatabaseID = " & Request("dbid")
	rsSTables.Open strSQL,objCon%>
	<UL>
		<LI>Select the table(s) for the Extraction Plan</LI>
	</UL>

		<TABLE width=50% border=no>
<%		Count = 1

		Do While not rsSTables.EOF%>
			<TR>
<%			' check to see if there was an existing extraction plan
			If ExtPlanID <> "" Then
				' check to see if the current table is in the table list
				If Instr(1,TableIDs,"," & CStr(rsSTables.Fields("TableID").Value) & ",") > 0 Then
					' the table is in the list--check the box%>
				<TD><INPUT type="checkbox" name=chk<%=CStr(Count)%> value=<%=rsSTables.Fields("TableID").Value%> checked></TD>
<%				Else
					' the table is not in the list--not check in the box%>
				<TD><INPUT type="checkbox" name=chk<%=CStr(Count)%> value=<%=rsSTables.Fields("TableID").Value%>></TD>
<%				End If
			Else%>
				<TD><INPUT type="checkbox" name=chk<%=CStr(Count)%> value=<%=rsSTables.Fields("TableID").Value%>></TD>
<%			End If%>
				<TD><%=rsSTables.Fields("TableName").Value%></TD>
			</TR>
<%			Count = Count + 1
			rsSTables.MoveNext
		Loop
        %>
		</TABLE>


	<INPUT type="hidden" name=ExtPlanID value=<%=ExtPlanID%>>
	<INPUT type="hidden" name=count value=<%=(Count - 1)%>>
	<INPUT type="hidden" name=dbid value="<%=dbid%>">
	<INPUT type="hidden" name=dbname value="<%=dbName%>">
	<INPUT type="hidden" name="dbdicid" value="<%=dbdicid%>">
	<INPUT type="hidden" name="dbdicname" value="<%=dbdicname%>">
	<P><INPUT type="submit" value="Continue" id=submit1 name=submit1></P>
</FORM>
</BODY>
<%
	'rsObject.Close
	objCon.Close
	Set rsObject = nothing
	Set objCon = nothing
%>

</HTML>

