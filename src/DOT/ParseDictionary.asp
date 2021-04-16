<%@ Language=VBScript %>
<!-- #include file = "MasterDB.asp" -->
<%
Set objCon = Server.CreateObject("ADODB.Connection")
objCon.Open MasterConnect
Set objConString = Server.CreateObject("ADODB.Connection")
strCSQL = "select connectstring from tbldictionary where dicid = " & Request("dicid")
Set rsPath = Server.CreateObject("ADODB.Recordset")
rsPath.Open strCSQL,objCon
'objConString.Open strCSQL,objCon

set objFSO = CreateObject("Scripting.FileSystemObject")
'set objTxt = objFSO.OpenTextFile("\\WD23657\D$\INetPub\WWWRoot\FRAMES\Nila\edf2928.dic",ForReading,TristateFalse)
On Error Resume Next
set objTxt = objFSO.OpenTextFile(rsPath.Fields("connectstring").Value,ForReading,TristateFalse)
If Err.number = 0 Then
'set objFile = objFSO.GetFile("C:\My Documents\KevinDorow\edf95.dic")
'objTxt = objFile.OpenAsTextStream
oneline = objTxt.ReadLine
comma1 = instr(oneline,",")
linecount = mid(oneline,1,comma1 -1)
'read the second line
oneline = objTxt.ReadLine
DicID = Request("dicID")
Dim Pkeys()
for i = 3 to linecount
   ' clear local variables
   datatype = ""
   parammin = ""
   parammax = ""
   paramunits = ""
   'read a line
    oneline = objTxt.ReadLine
    comma1 = instr(oneline,",")
'    Response.Write comma1
    parametername = mid(oneline,1,comma1 -1)
    comma2 = instr(comma1 +1,oneline,",")
    if (mid(parametername,1,3) <> "aka") and (mid(parametername,1,3) <> "Num") then
'    Response.Write parametername
'       Response.write oneline
       keycount = mid(oneline,comma1 +1,comma2 - comma1 -1)
'       Response.Write keycount
       if mid(parametername,1,6) <> "~Table" then
		   ' move the comma positions to the next location on the line
		   comma1 = comma2
		   comma2 = instr(comma1 + 1, oneline,",")
		   ' extract the datatype for the parameter
		   datatype = mid(oneline,comma1 + 1, comma2 - comma1 - 1)
		   ' move the comma positions to the next location on the line
		   comma1 = comma2
		   comma2 = instr(comma1 + 1, oneline,",")
		   ' extract the min for the parameter
		   parammin = mid(oneline,comma1 + 1, comma2 - comma1 - 1)
		   ' move the comma positions to the next location on the line
		   comma1 = comma2
		   comma2 = instr(comma1 + 1, oneline,",")
		   ' extract the max for the parameter
		   parammax = mid(oneline,comma1 + 1, comma2 - comma1 - 1)
		   ' move the comma positions to the next location on the line
		   comma1 = comma2
		   comma2 = instr(comma1 + 1, oneline,",")
		   ' extract the units for the parameter
		   paramunits = mid(oneline,comma1 + 1, comma2 - comma1 - 1)
		   paramunits = replace(paramunits,"""","")

'          for x = 1 to 4
'              comma1 = comma2
'              comma2 = instr(comma1 + 1,oneline,",")
'              Response.Write comma1
'              Response.Write "here"
'              Response.Write comma2
'          next
          quotes1 = comma2 +1
'          Response.Write quotes1
		  ' added line to handle Scalar values
		  If keycount <> 0 Then
			quotes2 = instr(quotes1,oneline,",")- 2
		  Else
			quotes2 = Len(oneline) - 1
		  End If
'          Response.Write quotes2
          description = mid(oneline,quotes1+1,quotes2 - quotes1)
          comma1 = quotes2 + 2
'          response.write description
          lkeycount = 0
          for j = 0 to keycount-1
              if j  < keycount -1 then
                 comma2 = instr(comma1 + 1,oneline,",")
              else
                 comma2 = len(oneline)+1
              end if
              checkkey = mid(oneline,comma1 +1,comma2 - comma1-1)
              if (keycount = 1) or (mid(checkkey,1,3) <> "Num") then
                Redim preserve Pkeys(lkeycount)
                PKeys(lkeycount) = mid(oneline,comma1 +1,comma2 - comma1-1)
'                Response.Write pkeys(lkeycount)
                lkeycount = lkeycount +1
              end if
              comma1 = comma2
          next
       else
          tpkeycount = mid(oneline,comma1 +1,comma2-comma1 -1)
 '         Response.Write tpkeycount
 '         for m = 1 to tpkeycount
 '            comma2 = instr(comma2 +1,oneline,",")
 '         next
          'parametername = mid(oneline,comma1 +1,comma2 -comma1)
          'description = mid(oneline,comma1 +1,comma2 - comma1 -1)
          parametername = mid(oneline,comma2 +1,len(oneline)- comma2)
'          Response.Write parametername
          description = ""
          parmname = mid(parametername,1,instr(1,parametername,",")-1)
'         Response.Write parmname
          Set rskeys = Server.CreateObject("ADODB.Recordset")
'          Response.Write "select keyname from tblparamkeys pk,tblparameter p where pk.parameterid = p.parameterid and parametername =" & "'" & parmname & "'"
          strSQL = "select keyname from tblparamkeys pk,tblparameter p where pk.parameterid = p.parameterid and parametername =" & "'" & parmname & "'" & " and dicID = " & dicID
          rskeys.Open strSQL,objCon

          tmpcount = 0
          Do while not rskeys.EOF
             Redim preserve Pkeys(tmpcount)
             Pkeys(tmpcount) = rskeys.fields("keyname").value
             rskeys.movenext
'             Response.Write "PARMKEYS"
'             Response.Write pkeys(tmpcount)
             tmpcount = tmpcount +1
          loop
          lkeycount = tmpcount
       end if
'       insert into parameter
	  Set objMaster = Server.CreateObject("ADODB.Connection")
      objMaster.Open MasterConnect

       strSQL = "INSERT INTO " & _
                "tblParameter(" & _
                    "ParameterName, " & _
                    "ParameterDesc, " & _
                    "DicID, " & _
                    "ParameterUnits, " & _
                    "ParameterDimensions, " & _
                    "ParameterDataType, " & _
                    "ParameterMin, " & _
                    "ParameterMax) " & _
             "VALUES (" & _
                    "'" & parametername & "', " & _
                    "'" & description & "', " & _
                    DicID & ", " & _
                    "'" & paramunits & "', " & _
                    "'" & CStr(lkeycount) & "', " & _
                    "'" & datatype & "', " & _
                    "'" & parammin & "', " & _
                    "'" & parammax & "')"
       ' execute the insert
       objMaster.Execute strSQL

	   strSQLselect = 	"SELECT " & _
							"ParameterID " & _
						"FROM " & _
							"tblParameter " & _
						"WHERE " & _
						"DicID = " & DicID & " " & _
						"AND " & _
						"ParameterName = " & "'" & parametername & "'"
		' create a recordset object to hold the result
		Set rsParameterID = Server.CreateObject("ADODB.Recordset")
		rsParameterID.Open strSQLSelect, objCon
		' store the id
		ParameterID = rsParameterID.Fields("ParameterID").Value
		rsParameterID.close
       for k = 0 to lkeycount - 1
'           insert into paramkeys
        strSQL = "INSERT INTO " & _
                "tblParamKeys(" & _
                    "KeyName, " & _
                    "ParameterID) "& _
             "VALUES (" & _
                    "'" & PKeys(k) & "', " & _
                    parameterid & ")"
                           ' execute the insert
      objMaster.Execute strSQL
       next
       objMaster.close
    end if
next

	'set up a SQL string to extract all the table entries
	strSQL = "SELECT " & _
				"* " & _
			 "FROM " & _
				"tblParameter " & _
			 "WHERE " & _
				"Parametername LIKE('%,%') " & _
			 "AND " & _
				"DicID = " & DicID
	' create the recordset object
	Set rsDicTables = Server.CreateObject("ADODB.Recordset")
	Set rsDicTables.ActiveConnection = objCon
	rsDicTables.CursorLocation = adUseClient
	' return the recordset
	rsDicTables.Open strSQL
	' disconnect the recordset
	Set rsDicTables.ActiveConnection = Nothing
	' set up a loop to process each table
	Dim outArray()
	Do While Not rsDicTables.EOF
		' set up a loop to parse the comma list
		' initialize the place holder
		intPlace = 1
		' initialize the array counter
		intArrayCount = -1
		inString = rsDicTables.Fields("ParameterName").Value
		inDelimiter = ","
		strTableDesc = ""
		' set up a loop to extract the elements and load them into an array
		Do While InStr(intPlace, inString, inDelimiter, vbTextCompare) <> 0
			' add a space in the array for the new element
			intArrayCount = intArrayCount + 1
			ReDim Preserve outArray(intArrayCount)
			' load the value into the new space
			outArray(intArrayCount) = Mid(inString, intPlace, _
                    InStr(intPlace, inString, inDelimiter, vbTextCompare) - intPlace)
			' move the place holer
			intPlace = InStr(intPlace, inString, inDelimiter, vbTextCompare) + 1
		Loop
		' load the last value into the array
		' add a space in the array for the new element
		intArrayCount = intArrayCount + 1
		ReDim Preserve outArray(intArrayCount)
		' load the value into the new space
		outArray(intArrayCount) = Mid(inString, intPlace)
		' all the value have been loaded into outArray
		'	set up a loop to load the descriptions
		For i = 0 to intArrayCount
			strParamName = outArray(i)
			' build a SQL string to extract the description and ID
			strSQL = "SELECT " & _
						"ParameterID, " & _
						"ParameterDesc " & _
					 "FROM " & _
						"tblParameter " & _
					 "WHERE " & _
						"ParameterName = '" & strParamName & "' " & _
					 "AND " & _
						"DicID = " & DicID
			' create a recordset object to return the data into
			Set rsDicParam = Server.CreateObject("ADODB.Recordset")
			rsDicParam.Open strSQL,objCon
			' store the description and id
			strParamDesc = rsDicParam.Fields("ParameterDesc").Value
			intParamID = rsDicParam.Fields("ParameterID").Value
			' close the recordset
			rsDicParam.Close
			Set rsDicParam = Nothing
			' store the descriptions together
			If strTableDesc = "" Then
				strTableDesc = strParamDesc
			Else
				strTableDesc = strTableDesc & "," & strParamDesc
			End If
			' now make the row non-viewable
			strSQL = "UPDATE " & _
						"tblParameter " & _
					 "SET " & _
						"Visible = 'N' " & _
					 "WHERE " & _
						"ParameterID = " & intParamID
			'execute the SQL Statement
			objCon.Execute strSQL
		Next
		' update the table record description
		strSQL = "UPDATE " & _
					"tblParameter " & _
				 "SET " & _
					"ParameterDesc = '" & strTableDesc & "' " & _
				 "WHERE " & _
					"ParameterID = " & rsDicTables.Fields("ParameterID").Value
		' execute the update
		objCon.Execute strSQL
		' move to the next table record
		rsDicTables.MoveNext
	Loop
	rsDicTables.Close
	Set rsDictTables = Nothing

	'set up a SQL string to extract all the table entries
	strSQL = "SELECT " & _
				"* " & _
			 "FROM " & _
				"tblParameter " & _
			 "WHERE " & _
				"DicID = " & DicID
	' create the recordset object
	Set rsDicTables = Server.CreateObject("ADODB.Recordset")
	Set rsDicTables.ActiveConnection = objCon
	rsDicTables.CursorLocation = adUseClient
	' return the recordset
	rsDicTables.Open strSQL
	' disconnect the recordset
	Set rsDicTables.ActiveConnection = Nothing
%>

<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
</HEAD>
<body background="frames.jpg">
<H2 align="center">The dictionary was parsed successfully</H2>
<UL>
	<LI>Click <A HREF="welcome.htm">here</A> to return to the welcome page.</LI>
</UL>
The following parameters were loaded:<BR>
<UL>
<%	Do While Not rsDicTables.EOF%>
	<LI><%=rsDicTables.Fields("ParameterName").Value%> -- <%=rsDicTables.Fields("ParameterDesc").Value%></LI>
<%		rsDicTables.MoveNext
	Loop%>
</UL>
</BODY>
<%	' close the recordset and db connection
	rsDicTables.Close
	Set rsDicTables = Nothing
	rsPath.Close
	Set rsPath = Nothing
	objCon.Close
	Set objCon = Nothing%>
</HTML>
<%	Else
		' create a SQL string to delete the record in tblDictionary
		strSQL = "DELETE " & _
				 "FROM " & _
					"tblDictionary " & _
				 "WHERE " & _
					"DicID = " & Request("dicid")
		objCon.Execute strSQL

		rsPath.Close
		Set rsPath = Nothing
		objCon.Close
		Set objCon = Nothing%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
</HEAD>
<body background="frames.jpg">
<H2 align="center">The dictionary file was not found!</H2>
<UL>
	<LI>Click <A HREF="javascript:window.history.go(-2);" OnMouseOver="status = 'Go Back'; return true;" OnMouseOut="status = ''">here</A> to input the file information again.</LI>
</UL>
</BODY>
</HTML>
<%	End If%>
