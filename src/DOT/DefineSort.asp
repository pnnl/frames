<%@ Language=VBScript %>
<%	'set up line feed identifier
	cr = vbCrLf

	'store previously defined sorts in the sort variable
	If Request("sort") <> "" then
		sort = Request("sort")
	End If
	'if a new sort has been added, store it in sort
	If Request("sfield") <> "" Then
		'check to see sort is empty
		If sort = "" Then
			sort = Request("sfield") & " " & Request("stype")
		Else
			sort = sort & ", " & Request("sfield") & " " & Request("stype")
		End If
	End If
%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
<TITLE>Data Transformation Panel -- Sort Selection</TITLE>
</HEAD>
<body background="frames.jpg">
<H2>Data Transformation Panel -- Sort Selection</H2>

<P>	Data Source:   <%=Request("sname")%><BR>
	Data Target:   <%=Request("ttype")%><BR>
	Table Sources: <%=Request("tbnames")%><BR>
	Field Sources: <%=Request("fnames")%><BR>
<%If Request("joinclause") <> "" then%>
	Join Clause:   <%=Request("joinclause")%><BR>
<%End If%>
<%If Request("filter") <> "" then%>
	Filter(s):	   <%=Request("filter")%>
<%End If%>
</P>
<UL>
  <LI>Build the sort criteria using the tools below:</LI>
</UL>
<FORM name=DTForm1 method="post" action="TransformSort.asp">
<INPUT type="hidden" name="sname" value="<%=Request("sname")%>">
<INPUT type="hidden" name="sid" value="<%=Request("sid")%>">
<INPUT type="hidden" name="ttype" value="<%=Request("ttype")%>">
<INPUT type="hidden" name="tbids" value="<%=Request("tbids")%>">
<INPUT type="hidden" name="tbnames" value="<%=Request("tbnames")%>">
<INPUT type="hidden" name="fids" value="<%=Request("fids")%>">
<INPUT type="hidden" name="fnames" value="<%=Request("fnames")%>">
<INPUT type="hidden" name="joinclause" value="<%=Request("joinclause")%>">
<INPUT type="hidden" name="filter" value="<%=Request("filter")%>">
<INPUT type="hidden" name="sort" value="<%=sort%>">
<TABLE BORDER=1 CELLSPACING=1 CELLPADDING=1>
	<TR>
		<TD>Field:</TD>
		<TD>Sort Type</TD>
	</TR>
	<TR>
		<TD>
			<SELECT name=sfield>
				<OPTION selected></OPTION>
				<%
				'strip the field names from fNames
				fNames = Request("fnames")
				blnContinue = true
				intPlaceName = 1
				Do While blnContinue
					'extract field name from fNames
					If Instr(intPlaceName,fNames,",") > 0 Then
						tmpName = Mid(fNames,intPlaceName,(Instr(intPlaceName,fNames,",") - intPlaceName))
					Else
						tmpName = Mid(fNames,intPlaceName)
					End If
					Response.Write("<OPTION value=""" & tmpName & """>" & tmpName & "</OPTION>" & vbCrLf)
					'update intPlaceName
					If Instr(intPlaceName,fNames,",") <> 0 Then
						intPlaceName = Instr(intPlaceName,fNames,",") + 2
					Else
						blnContinue = false
					End If
				Loop
				%>
			</SELECT>
		</TD>
		<TD>
			<SELECT name=stype>
				<OPTION selected></OPTION>
				<OPTION value="">Ascending</OPTION>
				<OPTION value="DESC">Descending</OPTION>
			</SELECT>
		</TD>
</TABLE>
<INPUT type="submit" value="Add Sort" id=submit1 name=submit1>
</FORM>
<HR>
<UL>
	<LI>
		<P>Defined filters in the transformation:</P>
	</LI>
</UL>
<FORM name=DTForm2 method="post" action="TransformTarget.asp">
<INPUT type="hidden" name="sname" value="<%=Request("sname")%>">
<INPUT type="hidden" name="sid" value="<%=Request("sid")%>">
<INPUT type="hidden" name="ttype" value="<%=Request("ttype")%>">
<INPUT type="hidden" name="tbids" value="<%=Request("tbids")%>">
<INPUT type="hidden" name="tbnames" value="<%=Request("tbnames")%>">
<INPUT type="hidden" name="fids" value="<%=Request("fids")%>">
<INPUT type="hidden" name="fnames" value="<%=Request("fnames")%>">
<INPUT type="hidden" name="joinclause" value="<%=Request("joinclause")%>">
<INPUT type="hidden" name="filter" value="<%=Request("filter")%>">
<TEXTAREA rows=3 cols=70 name=sort readonly><%Response.Write(sort)%></TEXTAREA>
<P><INPUT type="submit" value="Continue" id=submit2 name=submit2></P>
</FORM>

</BODY>
</HTML>
