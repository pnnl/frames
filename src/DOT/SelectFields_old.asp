<%@ Language=VBScript %>
<!-- #include file = "MasterDB.asp" -->
<%
	'set up line feed identifier
	cr = vbCrLf
	
	dbid = Request("dbid")
	dbName = Request("dbname")
	KCount = Request("KCount")
	 
	'create connection to the database
	Set objCon = Server.CreateObject("ADODB.Connection")
	objCon.Open MasterConnect
%>

<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
<TITLE>Extraction Plan -- Field Selection</TITLE>
</HEAD>
<BODY bgcolor="#99ccff">
<H2>Extraction Plan -- Field Selection</H2>
Database Selected: <%=dbName%><br>
Parameter Selected: <%=Request("ParameterName")%><br>
<%	
    Set rsObject = Server.CreateObject("ADODB.Recordset")
    strSQL = "Select KeyName From tblParamKeys K Where K.ParameterID = " & Request("ParameterID")
	rsObject.Open strSQL,objCon%>

Parameter Keys:
<%	Do While not rsObject.EOF%>		
			<%=rsObject.Fields("KeyName").Value%><sp>
<%	
			rsObject.MoveNext
	Loop
%>
<br>
	<%
		NumTables = Request("count")
		ListCount = 0
		For i = 1 to NumTables
			If Request("chk" & i) <> "" Then
				ListCount = ListCount + 1
				'create a comma delimited list of the table source ID's and names
				Set rsTable = Server.CreateObject("ADODB.Recordset")
				strSQL = "SELECT TableName FROM tblTables WHERE TableID = " & Request("chk" & i)
				rsTable.Open strSQL,objCon
				If tIDs = "" Then
					tIDs = Request("chk" & i)
					tNames = rsTable.Fields("TableName").Value
				Else
					tIDs = tIDs & "," & Request("chk" & i)
					tNames = tNames & ", " & rsTable.Fields("TableName").Value
				End If
				rsTable.Close
			End If
		Next
	%>
	Table Source(s): <%=tNames%><br>
</P>
<%	If ListCount > 1 Then%>
		<FORM name=DTForm method="post" action="DefineJoin.asp">
<%	Else%>
		<FORM name=DTForm method="post" action="DefineFilter.asp">
<%	End If%>
<INPUT type="hidden" name=dbid value=<%=dbid%>>
<INPUT type="hidden" name=dbname value="<%=dbName%>">
<INPUT type="hidden" name=KCount value=<%=KCount%>>
<INPUT type="hidden" name="tbids" value="<%=tIDs%>">
<INPUT type="hidden" name="tbnames" value="<%=tNames%>">

<%	' check to see if there is an existing Extraction Plan
	If Request("ExtPlanID") <> "" Then
		Set rsObject2 = Server.CreateObject("ADODB.Recordset")
		strSQL = "Select FieldID From tblExtPlanFields Where ExtPlanID = " & Request("ExtPlanID")
		rsObject2.Open strSQL,objCon,adOpenStatic,adLockOptimistic
		Do While not rsObject2.EOF
			FieldIDs = FieldIDs & "," & rsObject2.Fields("FieldID").Value
			'If FieldIDs = "" Then
			'	FieldIDs = rsObject2.Fields("FieldID").Value
			'Else	
			'	FieldIDs = FieldIDs & "," & rsObject2.Fields("FieldID").Value
			'End If
			rsObject2.MoveNext
		Loop
		FieldIDs = FieldIDs + ","
		rsObject2.Close
		Set rsObject2 = Nothing
	End If%>
<UL>
	<LI>
		<P>Select the field(s) for the transformation from the list(s) below</P>
	</LI>
</UL>
<%	'set up a loop to list all the fields for the selected tables
	strTemp = tIDs
	intPlaceID = 1
	intPlaceName = 1
	blnContinue = true
	Set rsFields = Server.CreateObject("ADODB.Recordset")
	intCount = 0
	tCount = 0
	Do While blnContinue
		'extract table name from tNames and write to browser
		If Instr(intPlaceName,tNames,",") > 0 Then
			tmpName = Mid(tNames,intPlaceName,(Instr(intPlaceName,tNames,",") - intPlaceName))
		Else
			tmpName = Mid(tNames,intPlaceName)
		End If
		tCount = tCount + 1
		Response.Write("<H3>" & tmpName & "</H3>" & vbCrLf)
		'update intPlaceName
		intPlaceName = Instr(intPlaceName,tNames,",") + 2
		'extract the table id
		If (Instr(intPlaceID,strTemp,",")) > 0 Then
			intID = Mid(strTemp,intPlaceID,(Instr(intPlaceID,strTemp,",") - intPlaceID))
		Else
			intID = Mid(strTemp,intPlaceID)
		End if
		'update intPlaceID
		If Instr(intPlaceID,strTemp,",") <> 0 Then
			intPlaceID = Instr(intPlaceID,strTemp,",") + 1
		Else
			blnContinue = false
		End if
		'open a recordset on tblFields using the table id
		strSQL = "SELECT FieldID, FieldName FROM tblFields WHERE TableID = " & intID
		rsFields.Open strSQL,objCon
		Response.Write("<TABLE border=yes>" & vbCrLf)
		Response.Write("<TR>" & vbCrLf)
		'set up a loop to load all the fields into the table (4 across)
		tCount = 0
		Do While not rsFields.EOF
			If (tCount mod 4) = 0 Then
				'start a new row
				Response.Write("</TR>" & vbCrLf)
				Response.Write("<TR>" & vbCrLf)
			End If
			' check to see if there is an existing extraction plan
			If Request("ExtPlanID") <> "" Then
				' check to see if the current field is in the list
				If Instr(1,FieldIDs,"," & CStr(rsFields.Fields("FieldID").Value) & ",") > 0 Then
					' the field is in the list--check it%>
			<TD><INPUT type="checkbox" name=chk<%=CStr(intCount)%> value=<%=rsFields.Fields("FieldID").Value%> checked></TD>
<%				Else
					' the field is not in the list--no check%>
			<TD><INPUT type="checkbox" name=chk<%=CStr(intCount)%> value=<%=rsFields.Fields("FieldID").Value%>></TD>
<%				End If
			Else%>						
			<TD><INPUT type="checkbox" name=chk<%=CStr(intCount)%> value=<%=rsFields.Fields("FieldID").Value%>></TD>
<%			End If%>
			<TD><%=rsFields.Fields("FieldName").Value%></TD>
			<% 'increment the count
			intCount = intCount + 1
			tCount = tCount + 1
			'move to the next field in the recordset
			rsFields.MoveNext
		Loop
		rsFields.Close
		Response.Write("</TR>" & vbCrLf)
		Response.Write("</TABLE>" & vbCrLf)
	Loop
%>
<INPUT type="hidden" name=ParameterID value=<%=Request("ParameterID")%>>	
<INPUT type="hidden" name=ParameterName value=<%=Request("ParameterName")%>>	
<INPUT type="hidden" name=ExtPlanID value=<%=Request("ExtPlanID")%>>	
<INPUT type="hidden" name="fCount" value="<%=intCount - 1%>">
<P><INPUT type="submit" value="Submit" id=submit1 name=submit1></P>
</FORM>
</BODY>

<%	objCon.Close
	Set rsFields = nothing
	Set rsTable = nothing
	Set objCon = nothing 
%>
</HTML>