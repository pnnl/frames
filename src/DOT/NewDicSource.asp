<%@ Language=VBScript %>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
</HEAD>
<body background="frames.jpg">
<H2>New Dictionary Information</H2>
<FORM name=NewdicForm method="post" action=GetDicInfo.asp>
<UL>

	<LI>Enter the full network path to the dictionary file (including the filename and extension):<BR>
		<INPUT type="text" id=dicPath name=dicPath>
	</LI>
	<LI>Enter a name for the dictionary:<BR>
		<INPUT type="text" id=dicName name=dicName>
	</LI>
	<LI>Enter a short description for the dictionary:<br>
		<TEXTAREA rows=2 cols=40 id=dicDesc name=dicDesc></TEXTAREA>
	</LI>
	<LI>Enter the name of the company providing the dictionary:<br>
		<INPUT type="text" id=dicSource name=dicSource>
	</LI>
</UL>
<INPUT type="submit" value="Continue" id=submit1 name=submit1>
</FORM>
</BODY>
</HTML>
