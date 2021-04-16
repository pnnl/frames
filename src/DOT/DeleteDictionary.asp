<%@ Language=VBScript %>
<!-- #include file = "MasterDB.asp" -->
<%
	'set up line feed identifier
	cr = vbCrLf

	'create connection to the database
	Set objCon = Server.CreateObject("ADODB.Connection")
	'objCon.Open "Provider=SQLOLEDB.1;User ID=sa;Initial Catalog=FramesStructures;Data Source=WD23657;Connect Timeout=240","sa",""
	objCon.Open MasterConnect

	If Request("submit") <> "Delete" Then
		'open a recordset on tblDatabase
		strSQL = "SELECT " & _
					"tblDictionary.DicID, " & _
					"tblDictionary.DicName, " & _
					"tblDictionary.Description " & _
				 "FROM " & _
					"tblDictionary " & _
				 "LEFT JOIN " & _
					"tblDatabase ON tblDictionary.DicID = tblDatabase.DictionaryID " & _
				 "WHERE " & _
					"tblDatabase.DictionaryID Is Null"
		Set rsObject = Server.CreateObject("ADODB.Recordset")
		rsObject.Open strSQL,objCon
	Else
		'the user wants to perform the delete
		'first get all the parameterID's from tblParameter
		strSQL = "SELECT " & _
					"ParameterID " & _
				 "FROM " & _
					"tblParameter " & _
				 "WHERE " & _
					"DicID = " & Request("dicid")
		Set rsParmID = Server.CreateObject("ADODB.Recordset")
		rsParmID.Open strSQL,objCon
		' build the delimited list
		strParmID = "("
		Do While Not rsParmID.EOF
			If strParmID = "(" Then
				strParmID = strParmID & rsParmID.Fields("ParameterID").Value
			Else
				strParmID = strParmID & "," & rsParmID.Fields("ParameterID").Value
			End If
			rsParmID.MoveNext
		Loop
		' add the final parenthesis
		strParmID = strParmID & ")"
		' close the parameter recordset
		rsParmID.Close
		Set rsParmID = Nothing
		' check to see if any ParameterID's existed
		If strParmID <> "()" Then
			' build the SQL statement to delete the records in tblParamKeys
			strSQL = "DELETE " & _
					 "FROM " & _
						"tblParamKeys " & _
					 "WHERE " & _
						"ParameterID IN " & strParmID
			' execute the delete
			objCon.Execute strSQL
		End If
		' build the SQL statement to delete the records in tblParameter
		strSQL = "DELETE " & _
				 "FROM " & _
					"tblParameter " & _
				 "WHERE " & _
					"DicID = " & Request("dicid")
		' execute the delete
		objCon.Execute strSQL
		' build the SQL statement to delete the records in tblDictionary
		strSQL = "DELETE " & _
				 "FROM " & _
					"tblDictionary " & _
				 "WHERE " & _
					"DicID = " & Request("dicid")
		' execute the delete
		objCon.Execute strSQL
	End If
%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
<TITLE>Dictionary Deleting Tool</TITLE>
<SCRIPT ID=clientEventHandlersJS LANGUAGE=javascript>
<!--

function cancel_onclick() {
	// redirect to the welcome page
	window.location.href = "welcome.htm";
}

//-->
</SCRIPT>
</HEAD>
<body background="frames.jpg">
<H2>Dictionary Deleting Tool</H2>
<%	If Request("submit") <> "Delete" Then%>
<FORM name=DTForm method="POST" action="DeleteDictionary.asp">
<%	If rsObject.EOF Then%>
<P>No dictionaries to delete<P>
<UL>
	<LI>Click <A HREF="welcome.htm">here</A> to return to the welcome page.</LI>
</UL>
<%	Else%>
<P>Select a dictionary to delete:</P>
	<SELECT id=dicid name=dicid>
<%	Do While Not rsObject.EOF%>
		<OPTION value="<%=rsObject.Fields("DicID").Value%>"><%=rsObject.Fields("DicName").Value%> -- <%=rsObject.Fields("Description").Value%></OPTION>
<%		rsObject.MoveNext
	Loop%>
	</SELECT>
<P>Choose an option below:</P>
<INPUT type="Submit" value="Delete" name="submit">&nbsp;&nbsp;<INPUT type="button" value="Cancel" name="cancel" LANGUAGE=javascript onclick="return cancel_onclick()">
</FORM>
<%	End If
		rsObject.Close
		Set rsObject = Nothing
	Else%>
<H2>The delete was successful</H2>
<UL>
	<LI>Click <A HREF="welcome.htm">here</A> to return to the welcome page.</LI>
</UL>
<%	End If%>
</BODY>
<%
	objCon.Close
	Set objCon = nothing
%>
</HTML>