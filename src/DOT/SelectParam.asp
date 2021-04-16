<%@ Language=VBScript %>
<!-- #include file = "MasterDB.asp" -->
<%
	'set up line feed identifier
	cr = vbCrLf
	dbid = Request("dbid")
	 'Response.Write dbid

	'create connection to the database
	Set objCon = Server.CreateObject("ADODB.Connection")
	objCon.Open MasterConnect

	' check to see if we need to do a delete
	If Request("delparm") <> "" Then
		' load the parameter id into a local variable
		paramid = Request("ExtPlanParam")
		' build the SQL strings to get the ExtPlanID
		strSQL = "SELECT " & _
					"ExtPlanID " & _
				 "FROM " & _
					"tblExtPlan " & _
				 "WHERE " & _
					"ParameterID = " & paramid & " " & _
				 "AND " & _
					"DatabaseID = " & dbid
		' retrieve the id
		Set rsExtPlan = Server.CreateObject("ADODB.Recordset")
		rsExtPlan.Open strSQL,objCon,adOpenStatic,adLockReadOnly
		If Not rsExtPlan.EOF Then
			' store the ExtPlanID in a local variable
			strExtPlanID = rsExtPlan.Fields("ExtPlanID").Value
			rsExtPlan.Close
			Set rsExtPlan = Nothing
			' build the SQL statement to delete the records in tblKeyFilter
			strSQL = "DELETE " & _
					 "FROM " & _
						"tblKeyFilter " & _
					"WHERE " & _
						"ExtPlanID = " & strExtPlanID
			' execute the delete
			objCon.Execute strSQL
			' build the SQL statement to delete the records in tblExtPlanFields
			strSQL = "DELETE " & _
					 "FROM " & _
						"tblExtPlanFields " & _
					 "WHERE " & _
						"ExtPlanID = " & strExtPlanID
			' execute the delete
			objCon.Execute strSQL
			' build the SQL statement to delete the records in tblExtPlanStaticFilter
			strSQL = "DELETE " & _
					 "FROM " & _
						"tblExtPlanStaticFilter " & _
					 "WHERE " & _
						"ExtPlanID = " & strExtPlanID
			' execute the delete
			objCon.Execute strSQL
			' build the SQL statement to delete the records in tblExtPlanJoin
			strSQL = "DELETE " & _
					 "FROM " & _
						"tblExtPlanJoin " & _
					 "WHERE " & _
						"ExtPlanID = " & strExtPlanID
			' execute the delete
			objCon.Execute strSQL
			' build the SQL statement to delete the records in tblExtPlanTables
			strSQL = "DELETE " & _
					 "FROM " & _
						"tblExtPlanTables " & _
					 "WHERE " & _
						"ExtPlanID = " & strExtPlanID
			' execute the delete
			objCon.Execute strSQL
			' build the SQL statement to delete the records in tblExtPlan
			strSQL = "DELETE " & _
					 "FROM " & _
						"tblExtPlan " & _
					"WHERE " & _
						"ExtPlanID = " & strExtPlanID
			' execute the delete
			objCon.Execute strSQL
		Else
			rsExtPlan.Close
			Set rsExtPlan = Nothing
		End If
	End If

	' retrieve the database name and store it
	Set rsDBName = Server.CreateObject("ADODB.Recordset")
	' define a SQL statement to retrieve the name of the database
	'	and the dictionary id and name
	strSQL = "SELECT " & _
				"db.DatabaseName, " & _
				"db.DictionaryID, " & _
				"dic.DicName " & _
			 "FROM " & _
				"tblDatabase as db, tblDictionary as dic " & _
			 "WHERE " & _
				"dic.DicID = db.DictionaryID " & _
			 "AND " & _
				"DatabaseID = " & dbid
	' get the recordset
	rsDBName.Open strSQL,objCon
	' store the database name
	dbName = rsDBName.Fields("DatabaseName").Value
	' store the dictionary id
	dbDicID = rsDBName.Fields("DictionaryID").Value
	' store the dictionary name
	dbDicName = rsDBName.Fields("DicName").Value
	'close and deallocate the recordset
	rsDBName.close
	Set rsDBName = Nothing

	' retrieve the parameter names
	Set rsObject = Server.CreateObject("ADODB.Recordset")
	Set rsObject.ActiveConnection = objCon
	rsObject.CursorLocation = adUseClient
	strSQL = "SELECT DISTINCT " & _
				"P.ParameterID, " & _
				"ParameterName, " & _
				"ParameterDesc " & _
			 "FROM " & _
				"tblParameter P, tblParamKeys PK " & _
			 "WHERE " & _
				"P.parameterid = PK.parameterid " & _
			 "AND " & _
				"P.DicID = " & dbDicID & " " & _
			 "AND " & _
				"Visible = 'Y'"
	'rsObject.Open "Select distinct P.ParameterID, ParameterName,ParameterDesc from tblParameter P,tblParamKeys PK where P.parameterid =PK.parameterid",objCon
	' open the recordset
	rsObject.Open strSQL
	' disconnect the recordset
	Set rsObject.ActiveConnection = Nothing%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
<TITLE>Select Frames Parameter Used for Extraction Plan</TITLE>
<SCRIPT ID=clientEventHandlersJS LANGUAGE=javascript>
<!--

function continue_onclick() {
	// submit the form to SelectTables.asp
	document.DTForm.method = "POST";
	document.DTForm.action = "SelectTables.asp";
	document.DTForm.submit();
}

function delete_onclick() {
	// submit the form to itself (to delete the existing extract plan
	document.DTForm.delparm.value = "Yes";
	document.DTForm.method = "POST";
	document.DTForm.action = "SelectParam.asp";
	document.DTForm.submit();
}

//-->
</SCRIPT>
</HEAD>
<body background="frames.jpg">
<H2>Extraction Plan -- Parameter Selection</H2>
<FORM name=DTForm>
<P>Database ID Selected: <%=dbid%><BR>
   Database Selected: <%=dbName%><BR>
   Dictionary Selected: <%=dbDicName%></P>
<%	If Request("delparm")<>"" Then%>
	<H3>The delete of the existing extract plan was successful</H3>
<%	End If%>
<UL>
	<LI>Select Frames Parameter Used for Extraction Plan:
		<SELECT name="ExtPlanParam">
<%
		Do While not rsObject.EOF
%>
			<OPTION value=<%=rsObject.Fields("ParameterID").Value%>><%=rsObject.Fields("ParameterName").Value%> - <%=rsObject.Fields("ParameterDesc").Value%></OPTION>
<%
			rsObject.MoveNext
		Loop
%>
		</SELECT>
	</LI>
</UL>
<INPUT type="hidden" name="dbname" value="<%=dbname%>">
<INPUT type="hidden" name="dbid" value="<%=dbid%>">
<INPUT type="hidden" name="dbdicid" value="<%=dbdicid%>">
<INPUT type="hidden" name="dbdicname" value="<%=dbdicname%>">
<INPUT type="hidden" name="delparm">
<INPUT type="button" value="Continue" id="continue1" name="continue1" LANGUAGE=javascript onclick="return continue_onclick()">
<INPUT type="button" value="Delete existing plan" id="delete1" name="delete1" LANGUAGE=javascript onclick="return delete_onclick()">
</FORM>
</BODY>
<%
	rsObject.Close
	objCon.Close
	Set rsObject = nothing
	Set objCon = nothing
%>

</HTML>
