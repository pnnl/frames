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
	rsObject.Open "Select DicID, DicName from tblDictionary",objCon
%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
</HEAD>
<body background="frames.jpg">
<H2>New Query Database</H2>
<FORM name=NewDBForm method="post" action=GetDBInfo.asp>
<UL>
	<LI>Select the type of the new data source:<BR>
		<SELECT name="dbType">
			<OPTION value=1>Microsoft Access 97(.mdb file)</OPTION>
			<OPTION value=2>Microsoft Access 2000(.mdb file)</OPTION>
		</SELECT>
	</LI>
	<LI>Enter the full network path to the .mdb file (including the filename and extension):<BR>
		<INPUT type="text" id=dbPath name=dbPath>
	</LI>
	<LI>Enter a name for the database:<BR>
		<INPUT type="text" id=dbName name=dbName>
	</LI>
	<LI>Enter a short description for the database:<br>
		<TEXTAREA rows=2 cols=40 id=dbDesc name=dbDesc></TEXTAREA>
	</LI>
	<LI>Enter the name of the company providing the database:<br>
		<INPUT type="text" id=dbSource name=dbSource>
	</LI>
	<LI>Enter a username to access the database (if necessary):<BR>
		<INPUT type="text" id=dbUName name=dbUName>
	</LI>
	<LI>Enter a password to access the database (if necessary):<BR>
		<INPUT type="password" id=dbPassword name=dbPassword>
	</LI>
	<LI>Enter a FRAMES dictionary to associate the database with:<BR>
		<SELECT id=dbDictionary name=dbDictionary>
<%			' set up a while loop to load the options in
			Do While Not rsObject.EOF%>
			<OPTION value="<%=rsObject.Fields("DicID").Value%>"><%=rsObject.Fields("DicName").Value%></OPTION>
<%				rsObject.MoveNext
			Loop%>
		</SELECT>
	</LI>
</UL>
<INPUT type="submit" value="Continue" id=submit1 name=submit1>
</FORM>
</BODY>
</HTML>
