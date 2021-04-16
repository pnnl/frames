<%@ Language=VBScript %>
<!-- #include file = "MasterDB.asp" -->
<%
	'set up line feed identifier
	cr = vbCrLf

	' set up local variables
	dbid = Request("dbid")
	dbName = Request("dbname")
	KeyTemp = Request("KeyTemp")
	ExtPlanID = Request("ExtPlanID")
	dbdicid = Request("dbdicid")
	dbdicname = Request("dbdicname")
	ListCount = Request("listcount")

	'create connection to the database
	Set objCon = Server.CreateObject("ADODB.Connection")
	objCon.Open MasterConnect
	'set up line feed identifier

	parametername = Request("ParameterName")
%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft FrontPage 4.0">
<TITLE>Extraction Plan -- Match Parameter with Database field</TITLE>
</HEAD>
<body background="frames.jpg">
<H2>Extraction Plan -- Match Parameter with Database field</H2>
Database ID Selected: <%=dbid%><BR>
Database Selected:	<%=dbName%><br>
Dictionary Selected: <%=dbdicname%><BR>
Parameter Selected: <%=Request("ParameterName")%><br>

<%
   'rsObject.close
    Set rsObject = Server.CreateObject("ADODB.Recordset")
    strSQL = "SELECT " & _
				"K.KeyName " & _
			 "FROM " & _
				"tblParamKeys as K, tblParameter as P " & _
			 "WHERE " & _
				"K.ParameterID = P.ParameterID " & _
			 "AND " & _
				"P.DicID = " & dbdicid & " " & _
			 "AND " & _
				"K.ParameterID = " & Request("ParameterID")
    'strSQL = "Select KeyName From tblParamKeys K Where K.ParameterID = " & Request("ParameterID")
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
Parameter Keys: <%=strTemp%><br>
<%		'process selected field information
	If request("fids") = "" Then
		NumFields = Request("fCount")
		checkedcount = 0
		Dim fieldarray()
		For i = 0 to NumFields
			If Request("chk" & i) <> "" Then
				'create a comma delimited list of the table source ID's and names
				Set rsFields = Server.CreateObject("ADODB.Recordset")
				strSQL = "SELECT TableName,FieldName FROM tblFields WHERE FieldID = " & Request("chk" & i)
				rsFields.Open strSQL,objCon
                Redim Preserve fieldarray(checkedcount)
                If fIDs = "" Then
					fIDs = Request("chk" & i)
					strTmpTableName = rsFields.Fields("TableName").Value
					If Instr(strTmpTableName," ") > 0 Then
						strTmpTableName = "[" + strTmpTableName + "]"
					End If
					strTmpFieldName = rsFields.Fields("FieldName").Value
					If Instr(strTmpFieldName," ") > 0 Then
						strTmpFieldName = "[" + strTmpFieldName + "]"
					End If
					'fNames = rsFields.Fields("TableName").Value & "." & rsFields.Fields("FieldName").Value
					fNames = strTmpTableName & "." & strTmpFieldName
                    fieldarray(checkedcount) = strTmpTableName & "." & strTmpFieldName
				Else
					fIDs = fIDs & "," & Request("chk" & i)
					strTmpTableName = rsFields.Fields("TableName").Value
					If Instr(strTmpTableName," ") > 0 Then
						strTmpTableName = "[" + strTmpTableName + "]"
					End If
					strTmpFieldName = rsFields.Fields("FieldName").Value
					If Instr(strTmpFieldName," ") > 0 Then
						strTmpFieldName = "[" + strTmpFieldName + "]"
					End If
					fNames = fNames & ", " & strTmpTableName & "." & strTmpFieldName
                    fieldarray(checkedcount) = strTmpTableName & "." & strTmpFieldName
				End If
'				Response.Write fieldarray(checkedcount)
'                Response.Write checkedcount
				checkedcount = checkedcount + 1
				rsFields.Close
			End If
		Next
	Else
		fIDs = Request("fids")
		fNames = Request("fnames")
	End If
	%>
Table Source(s): <%=Request("tbnames")%><BR>
Field Source(s): <%=fNames%>
<UL>
	<LI>Match the fields with the correct parameter</LI>
</UL>
<%	KCount = 0
	' here we need to execute a query to determine if any of the
	'	parameters need have units
	' set up the where clause for the select statement
    paramstrsql = replace(ParameterName,",",chr(39) & "," & chr(39))
    paramstrsql = "(" & chr(39) & paramstrsql & chr(39) & ")"

    ' build the SQL statement to determine if any of the parameters need
    '	the units to be selected
    strSQL = "SELECT " & _
				"count(*) " & _
			 "FROM " & _
				"tblParameter " & _
			 "WHERE " & _
				"ParameterName in " & paramstrsql & " " & _
			 "AND " & _
				"DicID = " & dbdicid & " " & _
			 "AND " & _
				"ParameterUnits <> ''"
    ' return the count
    Set rsCount = Server.CreateObject("ADODB.Recordset")
    rsCount.Open strSQL, objCon
    UnitsCount = rsCount.Fields(0).Value
    ' close the recordset
    rsCount.Close
    Set rsCount = Nothing

	If UnitsCount > 0 Then%>
		<FORM name=DTForm method="post" action="SelectUnits.asp">
<%	Else
		If ListCount > 1 Then%>
		<FORM name=DTForm method="post" action="DefineJoin.asp">
<%		Else
'			If KeyTemp < 1 Then
'		<FORM name=DTForm method="post" action="TestExtPlan.asp">
'		<INPUT type="hidden" name="filtercount" value=0>
'			Else%>
		     <FORM name=DTForm method="post" action="DefineFilter.asp">
		     <% KCount = KeyTemp
			'End If
		End If
	End If

	'create an array of related Parameter Namestablename to populate the drop down list boxes
	Set rsDropPop = Server.CreateObject("ADODB.Recordset")
	strSQL = "SELECT " & _
				"ParameterID, " & _
				"ParameterName, " & _
				"ParameterDesc " & _
			 "FROM " & _
				"tblParameter " & _
			 "WHERE " & _
				"ParameterName in " & paramstrsql & " " & _
			 "AND " & _
				"DicID = " & dbdicid
	'strsql = "Select ParameterID, ParameterName,ParameterDesc from tblParameter where parametername in " & paramstrsql
	rsDropPop.Open strSQL,objCon
	parameteridlist =  "(" & Request("ParameterID")
	intCount = -1
	Dim arrDropPop()
	Do while not rsDropPop.EOF
		intCount = intCount + 1
		parameteridlist = parameteridlist & "," & rsDropPop.Fields("ParameterID").Value
		ReDim Preserve arrDropPop(intCount)
		arrDropPop(intCount) = rsDropPop.Fields("ParameterName").Value
		rsDropPop.MoveNext
	Loop
	parameteridlist = parameteridlist & ")"

	TPCount = intCount + 1
	' check to see that the user selected enough target fields
	If checkedcount <> TPCount Then
		' write an error message to the screen%>
		<H2 color=red>Error! The number of selected fields must be equal to the number of selected parameters</H2>
		<A HREF=javascript:window.history.back();>Go back</A>
<%		Response.End
	End If
	'changed checkedcount to TPcount (number of keys in table parameter vs number of fields checked
    count = 0
    parameter = ""

	For k = 0 to (TPCount -1) %>
    <BR>
	   <%=fieldarray(k)%>
	   <SELECT name=param<%=count%>>
<% 	   rsDropPop.MoveFirst
	   For i = 0 to intCount%>
		   <OPTION value="<%=rsDropPop.Fields("ParameterID").Value & "." & arrDropPop(i) %>"><%=arrDropPop(i)%></OPTION>
<%	     	rsDropPop.MoveNext
	   Next  %>
      </Select>
<%    ' increment count
      count = count + 1
    Next

%>
<INPUT type="hidden" name=dbid value=<%=dbid%>>
<INPUT type="hidden" name=dbname value="<%=dbname%>">
<INPUT type="hidden" name=dbdicid value=<%=dbdicid%>>
<INPUT type="hidden" name=dbdicname value="<%=dbdicname%>">
<INPUT type="hidden" name="tbids" value="<%=Request("tbids")%>">
<INPUT type="hidden" name="tbnames" value="<%=Request("tbnames")%>">
<INPUT type="hidden" name="fids" value="<%=fIDs%>">
<INPUT type="hidden" name="fnames" value="<%=fNames%>">
<INPUT type="hidden" name=ParameterIDlist value=<%=ParameterIDlist%>>
<INPUT type="hidden" name=ParameterID value=<%=Request("ParameterID")%>>
<INPUT type="hidden" name=ParameterName value=<%=Request("ParameterName")%>>
<INPUT type="hidden" name=ExtPlanID value=<%=Request("ExtPlanID")%>>
<INPUT type="hidden" name=TPCount value=<%=TPCount%>>
<INPUT type="hidden" name=KCount value=<%=KCount%>>
<INPUT type="hidden" name="fCount" value="<%=intCount - 1%>">
<INPUT type="hidden" name="listcount" value="<%=listcount%>">
<P><INPUT type="submit" value="Continue" id=submit1 name=submit1></P>
</FORM>
</BODY>

<%	rsDropPop.Close
	objCon.Close

	Set rsDropPop = Nothing
	Set rsFields = Nothing
	Set objCon = Nothing
	Set rsobject = Nothing
%>
</HTML>
