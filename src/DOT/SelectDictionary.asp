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
	rsObject.Open "Select DicName, Description from tblDictionary",objCon
%>

<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
<TITLE>Dictionary Parsing Tool</TITLE>
<SCRIPT ID=clientEventHandlersJS LANGUAGE=javascript>
<!--

function delete_onclick() {
	// redirect the window to the DeleteDictionary.asp page
	window.location.href = "DeleteDictionary.asp";
}

//-->
</SCRIPT>
</HEAD>
<body background="frames.jpg">
<H2>Dictionary Parsing Tool</H2>
<FORM name=DTForm method="POST" action="NewDicSource.asp">
<P>The following dictionaries are currently available:</P>
<UL>
<%	Do While Not rsObject.EOF%>
	<LI><%=rsObject.Fields("DicName").Value%> -- <%=rsObject.Fields("Description").Value%></LI>
<%		rsObject.MoveNext
	Loop%>
</UL>
<P>Choose an option below:</P>
<INPUT type="Submit" value="Parse A New Dictionary" name="submit">&nbsp;&nbsp;<INPUT type="button" value="Delete a Dictionary" name=delete LANGUAGE=javascript onclick="return delete_onclick()">
</FORM>
</BODY>
<%
	rsObject.Close
	objCon.Close
	Set rsObject = nothing
	Set objCon = nothing
%>

</HTML>
