<%@ Language=VBScript %>
<!-- #include file = "MasterDB.asp" -->
<%
	'set up line feed identifier
	cr = vbCrLf

	'create connection to the database
	Set objCon = Server.CreateObject("ADODB.Connection")
	'objCon.Open "Provider=SQLOLEDB.1;User ID=sa;Initial Catalog=FramesStructures;Data Source=WD23657;Connect Timeout=240","sa",""
	objCon.Open MasterConnect

	'open a recordset on tblDatabase
	Set rsObject = Server.CreateObject("ADODB.Recordset")
	Set rsObject.ActiveConnection = objCon
	rsObject.CursorLocation = adUseClient
	' define a SQL string to return the database names, ids, and dictionary names
	strSQL = "SELECT " & _
				"db.DatabaseName, " & _
				"db.DatabaseID, " & _
				"dic.DicName " & _
			 "FROM " & _
				"tblDatabase as db, tblDictionary as dic " & _
			 "WHERE " & _
				"dic.DicID = db.DictionaryID"
	'rsObject.Open "Select DatabaseID, DatabaseName from tblDatabase",objCon
	' open the recordset
	rsObject.Open strSQL
	' disconnect the recordset from the source
	Set rsObject.ActiveConnection = Nothing%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
<TITLE>Select Query Database</TITLE>
<SCRIPT ID=clientEventHandlersJS LANGUAGE=javascript>
<!--

function query_onclick() {
	// this function determines which page the form needs to be submitted to
	//	based on the value in source and then submits it
	if(document.DTForm.dbID.options[document.DTForm.dbID.selectedIndex].value == -1) {
		// redirect to the new data source page
		window.location.href = "NewDataSource.asp";
	} else {
		// submit the form
		document.DTForm.method = "POST";
		document.DTForm.action = "SelectParam.asp";
		document.DTForm.submit();
	}
}

function delete_onclick() {
	if(document.DTForm.dbID.options[document.DTForm.dbID.selectedIndex].value == -1) {
		// prompt the user that they have made an error
		window.alert("Error--you can't press the delete button if <new> is selected");
	} else {
		// submit the form to the delete page
		document.DTForm.method = "POST";
		document.DTForm.action = "DeleteDatabase.asp";
		document.DTForm.submit();
	}
}

//-->
</SCRIPT>
</HEAD>
<body background="frames.jpg">
<H2>Select Query Database</H2>
<FORM name=DTForm>
<UL>
	<LI>Select the source of the data to be queried / deleted or select &lt;new&gt; to create a new data source:&nbsp
		<SELECT name="dbID">
<%		Do While not rsObject.EOF%>
			<OPTION value=<%=rsObject.Fields("DatabaseID").Value%>><%=rsObject.Fields("DatabaseID").Value%> - <%=rsObject.Fields("DatabaseName").Value%> - <%=rsObject.Fields("DicName").Value%></OPTION>
<%			rsObject.MoveNext
		Loop%>
			<OPTION value=-1>&lt;new&gt;</OPTION>
		</SELECT>
	</LI>
</UL>
<INPUT type="button" value="Continue" name=query LANGUAGE=javascript onclick="return query_onclick()">
&nbsp;&nbsp;<INPUT type="button" value="Delete" id=delete name=delete LANGUAGE=javascript onclick="return delete_onclick()">
</FORM>
</BODY>
<%
	rsObject.Close
	objCon.Close
	Set rsObject = nothing
	Set objCon = nothing
%>

</HTML>
