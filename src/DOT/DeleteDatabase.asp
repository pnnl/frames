<%@ Language=VBScript %>
<!-- #include file = "MasterDB.asp" -->
<%
	'set up line feed identifier
	cr = vbCrLf
	dbid = Request("dbID")

	'create connection to the database
	Set objCon = Server.CreateObject("ADODB.Connection")
	'objCon.Open "Provider=SQLOLEDB.1;User ID=sa;Initial Catalog=FramesStructures;Data Source=WD23657;Connect Timeout=240","sa",""
	objCon.Open MasterConnect

	'the user wants to perform the delete
	'first get all the ExtPlanID's from tblExtPlan
	strSQL = "SELECT " & _
				"ExtPlanID " & _
			 "FROM " & _
				"tblExtPlan " & _
			 "WHERE " & _
				"DatabaseID = " & dbid
	Set rsExtPlanID = Server.CreateObject("ADODB.Recordset")
	rsExtPlanID.Open strSQL,objCon
	' build the delimited list
	strExtPlanID = "("
	Do While Not rsExtPlanID.EOF
		If strExtPlanID = "(" Then
			strExtPlanID = strExtPlanID & rsExtPlanID.Fields("ExtPlanID").Value
		Else
			strExtPlanID = strExtPlanID & "," & rsExtPlanID.Fields("ExtPlanID").Value
		End If
		rsExtPlanID.MoveNext
	Loop
	' add the final parenthesis
	strExtPlanID = strExtPlanID & ")"
	' close the parameter recordset
	rsExtPlanID.Close
	Set rsExtPlanID = Nothing
	' check to see if any ParameterID's existed
	If strExtPlanID <> "()" Then
		' build the SQL statement to delete the records in tblKeyFilter
		strSQL = "DELETE " & _
				 "FROM " & _
					"tblKeyFilter " & _
				 "WHERE " & _
					"ExtPlanID IN " & strExtPlanID
		' execute the delete
		objCon.Execute strSQL
		' build the SQL statement to delete the records in tblExtPlanFields
		strSQL = "DELETE " & _
				 "FROM " & _
					"tblExtPlanFields " & _
				 "WHERE " & _
					"ExtPlanID IN " & strExtPlanID
		' execute the delete
		objCon.Execute strSQL
		' build the SQL statement to delete the records in tblExtPlanStaticFilter
		strSQL = "DELETE " & _
				 "FROM " & _
					"tblExtPlanStaticFilter " & _
				 "WHERE " & _
					"ExtPlanID IN " & strExtPlanID
		' execute the delete
		objCon.Execute strSQL
		' build the SQL statement to delete the records in tblExtPlanJoin
		strSQL = "DELETE " & _
				 "FROM " & _
					"tblExtPlanJoin " & _
				 "WHERE " & _
					"ExtPlanID IN " & strExtPlanID
		' execute the delete
		objCon.Execute strSQL
		' build the SQL statement to delete the records in tblExtPlanTables
		strSQL = "DELETE " & _
				 "FROM " & _
					"tblExtPlanTables " & _
				 "WHERE " & _
					"ExtPlanID IN " & strExtPlanID
		' execute the delete
		objCon.Execute strSQL
	End If
	' build the SQL statement to delete the records in tblExtPlan
	strSQL = "DELETE " & _
			 "FROM " & _
				"tblExtPlan " & _
			 "WHERE " & _
				"DatabaseID = " & dbid
	' execute the delete
	objCon.Execute strSQL
	' build the SQL statement to delete the records in tblFields
	strSQL = "DELETE " & _
			 "FROM " & _
				"tblFields " & _
			 "WHERE " & _
				"DatabaseID = " & dbid
	' execute the delete
	objCon.Execute strSQL
	' build the SQL statement to delete the records in tblTables
	strSQL = "DELETE " & _
			 "FROM " & _
				"tblTables " & _
			 "WHERE " & _
				"DatabaseID = " & dbid
	' execute the delete
	objCon.Execute strSQL
	' build the SQL statement to delete the records in tblDatabase
	strSQL = "DELETE " & _
			 "FROM " & _
				"tblDatabase " & _
			 "WHERE " & _
				"DatabaseID = " & dbid
	' execute the delete
	objCon.Execute strSQL
%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
<TITLE>Database Deleting Tool</TITLE>
</HEAD>
<body background="frames.jpg">
<H2>Deleting Deleting Tool</H2>
<H2>The delete was successful</H2>
<UL>
	<LI>Click <A HREF="welcome.htm">here</A> to return to the welcome page.</LI>
</UL>
</BODY>
<%
	objCon.Close
	Set objCon = nothing
%>
</HTML>