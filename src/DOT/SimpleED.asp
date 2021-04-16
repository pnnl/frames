<%@ Language=VBScript %>
<!-- #include file = "MasterDB.asp" -->
<%	Sub ParseString(inString, inDelimiter, outArray)

    ' initialize the place holder
    intPlace = 1
    ' initialize the array counter
    intArrayCount = -1
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
End Sub	


        Method = Request.QueryString("Step")
        DataDic = Request.QueryString("DD")
        DatabaseID = Request.QueryString("DB")        
        Param = Request.Querystring("Parameter")
		NumKeys = Request.Querystring("KeyCount")
		Dim key()
		Dim kValue()
		'strTemp = Request.QueryString("Key" & i)
		'kValue = Array(strTemp)
		For i = 1 to NumKeys
				'create a comma delimited list of the keys and values of chosen parameter for which to find extraction plans
				If keystr = "" Then
					keystr = Request.QueryString("Key" & i)
					kValuestr = Request.QueryString("Value" & i) 
				Else
					keystr = keystr & "," & Request.Querystring("Key" & i)
					kValuestr = kValuestr & "," & Request.Querystring("Value" & i)
				End If
		Next
		ParseString keystr,",",key	
		ParseString kValuestr,",",kValue
				
	'set up line feed identifier
	cr = vbCrLf

	'create connection to the database
	Set objMaster = Server.CreateObject("ADODB.Connection")
	objMaster.Open MasterConnect

	' retrieve the corresponding ExtPlanIDs
	Set rsObject = Server.CreateObject("ADODB.Recordset")
	'Response.Write "select ExtPlanID,ExtPlanSQL from tblExtPlan EP, tblParameter P where DatabaseID = " & DatabaseID & " and EP.ParameterID = P.ParameterID and P.ParameterName = " & Param 
	strSQL =  "select ExtPlanID,ExtPlanSQL from tblExtPlan EP, tblParameter P where DatabaseID = " & DatabaseID & " and EP.ParameterID = P.ParameterID and P.ParameterName = " & Param 
	rsObject.Open strSQL,ObjMaster,adOpenStatic,adLockOptimistic
    If Not rsObject.EOF Then
		jcount = 0
		Set rstjoins = Server.CreateObject("ADODB.Recordset")
		strSQLf =  "select count(*)as jcount,JoinClause from tblExtPlanJoin where ExtPlanID = " & rsObject.Fields("ExtPlanID").Value & " Group by JoinClause"
		rstjoins.Open strSQLf,ObjMaster,adOpenStatic,adLockOptimistic
    
		DIM Joins()
		i =-1
		do while not rstjoins.EOF
		   i =i + 1
		   'Response.Write i
		   ReDim Preserve joins(i)
		   jcount = rstjoins.fields("JoinClause").value
		   rstjoins.movenext	
		Loop
		jcount = i
		rstjoins.Close
    
		' retrieve the corresponding filters to add to "where" clause
		Set rsfilters = Server.CreateObject("ADODB.Recordset")
		strSQLf =  "select FieldName,kf.FieldID,Operator from tblkeyfilter kf,tblfields f, tblExtPlanFields EPF where kf.FieldID = f.FieldID and EPF.FieldID = f.FieldID and kf.ExtPlanID = EPF.ExtPlanID and kf.ExtPlanID = " & rsObject.Fields("ExtPlanID").Value & " order by EPFieldID"
		rsfilters.Open strSQLf,ObjMaster,adOpenStatic,adLockOptimistic
		DIM FieldName ()
		DIM FieldID ()
		DIM Operator ()
    
		i= -1
		do while not rsfilters.EOF
		    i =i + 1
		    ReDim Preserve FieldName(i)
		    ReDim Preserve FieldID(i)
		    ReDim Preserve Operator(i)
		  FieldName(i) = rsfilters.Fields("FieldName").Value
		  FieldID(i) = rsfilters.Fields("FieldID").Value
		  Operator(i) = rsfilters.Fields("Operator").Value
		  rsfilters.movenext
		Loop
		fcount = i
		rsfilters.close
		
		' retrieve the static filter to add to "where" clause
		Set rsSfilter = Server.CreateObject("ADODB.Recordset")
		strSQLf =  "select StaticFilter from tblExtPlanStaticFilter where ExtPlanID = " & rsObject.Fields("ExtPlanID").Value 
		rsSfilter.Open strSQLf,ObjMaster,adOpenStatic,adLockOptimistic
		staticFilter = ""
        if not rsSFilter.EOF then
           staticfilter = rsSfilter.Fields("StaticFilter").Value
        end if
		rsSfilter.close
		
			'create connection to the database
			' create a SQL statement to return the connection string to the target
			'	database

			strSQLTarget = "SELECT " & _
								"ConnectString " & _
						   "FROM " & _
								"tblDatabase " & _
						   "WHERE " & _
								"DatabaseID = " & DatabaseID
								
			' create a recordset object to hold the results of the 
			'	query
			Set rsSQLTarget = Server.CreateObject("ADODB.Recordset")
			rsSQLTarget.Open strSQLTarget,objMaster
			' store the connect string in a local variable
			strConnect = rsSQLTarget.Fields("ConnectString").Value
			' deallocate the recordset object
			rsSQLTarget.Close
			Set rsSQLTarget = Nothing
			'objMaster.Close
			'Set objMaster = Nothing
			' create a connection to the target database
			Set objTarget = Server.CreateObject("ADODB.Connection")
			objTarget.Open strConnect
			' create a recordset object to hold the results of the query
			Set rsExtPlan = Server.CreateObject("ADODB.Recordset")
			' set the properties of the recordset so that it can be
			'	disconnected from the source
			'Response.Write rsExtPlan
			Set rsExtPlan.ActiveConnection = objTarget
			rsExtPlan.CursorLocation = adUseClient
		'end if

			'Response.write plancount
		    'if plancount > 0 then
		    'Parse ExtPlanSQL
			strSQL = rsObject.Fields("ExtPlanSQL").Value
			if jcount > 0 or NumKeys > 0 then
				strSQL = Left(strSQL,Instr(1,strSQL,"WHERE") + 5)
		    end if
			
				' set up a loop to process the filter(s)
				For i = 0 to jcount
				   strSQL = sqlSQL & join(i)
				   if i < jcount then
				       'Response.Write "here"
				       strSQL = strSQL & " And "
				   end if
			    next
				   
'				Dim reorderedkey(fcount)
'				let k= 0
'Response.Write strSQL 
'Response.Write fcount
				For i = 0 to fcount
				    ' "and" to where clause after join conditions if there is at least one
				    if jcount > 0 then
				       strSQL = strSQL & " And "
				    end if
				       
					' bust up the field id from the field name
					fName = FieldName(i)
					fID = FieldID(i)
					strParameterName = "SELECT ParameterName from tblKeyFilter KF,tblParameter P," & 
								 "WHERE KF.ParameterID = P.ParameterID" & _
								 "AND KF.FieldID = " & fID & _
								 "AND ExtPlanID = " & rsObject.Fields("ExtPlanID").Value 
					' define a recordset to get the data type with
					Set rsPName = Server.CreateObject("ADODB.Recordset")
					' execute the query
					rsPName.Open strParameterName, objMaster
					' store the value in a local variable
					PName = rsPName.Fields("ParameterName").Value
					' close and deallocate the recordset
					rsPName.Close
					Set rsPName = Nothing
													 									
					' build a query to return the data type of field (to determine
					'	whether or not we need to put quotes around the value
					strGetType = "SELECT " & _
									"FieldType " & _
								 "FROM " & _
									"tblFields " & _
								 "WHERE " & _
									"FieldID = " & fID
					' define a recordset to get the data type with
					Set rsDataType = Server.CreateObject("ADODB.Recordset")
					' execute the query
					rsDataType.Open strGetType, objMaster
					' store the value in a local variable
					strDataType = rsDataType.Fields("FieldType").Value
					' close and deallocate the recordset
					rsDataType.Close
					Set rsDataType = Nothing
					
					For j= 0 to NumKeys-1
					   if key(j) = PName then
		                  x=j
'		                  reorderedkey(k) = key(j)
'		                  k = k +1
		               end if
		            next      				
		            
					' set up a case statement to determine whether or not quotes are needed
					Select Case strDataType
						Case "7","129","130","133","134","135"
							'these are either string or date--quotes are required
							'kvalue(x) = char(34) & kvalue(x) & char(34)
						'Case Else
							' these types do not require quotes
							strSQL = strSQL & FName & Replace(Operator(i),"v",chr(34) & kvalue(x) & chr(34))
					End Select
				' check the value of i
					If i < NumKeys -1 Then
						strSQL = strSQL & " AND "
					End If
					
				Next
				if staticfilter <> "" then
				   if (fcount = 0) and (jcount = 0) then
				      strSQL = strSQL & staticfilter
		           else				    
	    	          strSQL = strSQL & " AND " & staticfilter
				   end if
				end if
				
'			Response.Write strSQL 
			objTarget.Execute strSQL
			rsExtPlan.Open strSQL 

			'<BR>Number of Records Returned:
			Response.Write(rsExtPlan.RecordCount & vbcrlf)
			' next build a table to hold the recordset returned
			' set up a loop to list the field names
		            i= 1
					For Each field In rsExtPlan.Fields
						if (i <> rsExtPlan.Fields.Count) then
							i = i+1
							Response.Write(field.Name & ",")
						else
							Response.Write(field.Name & vbcrlf)
						end if 
					Next

                'field name headings order as written to ExtPlanFields
                'should be same order as corresponding field in select clause
'                if NumKey = 0 then
'                   Response.write(Param & vbcrlf)
'                else   
'                i = 0
'                for i =0 to fcount
'                   if i <> fcount then
'                      Response.Write(FieldName(i)& ",")
'                      i = i+1
'                   else
'                      Response.Write(FieldName(i)& vbcrlf)
'                   end if
'                next
'                end if
                   
				' set up a loop to load the returned fields
				Do While not rsExtPlan.EOF
		          i= 1
					For Each field In rsExtPlan.Fields
						if (i <> rsExtPlan.Fields.Count) then
							i = i+1
				    		Response.Write(chr(34)& field.Value & chr(34) & ",")
						else
				    		Response.Write(chr(34) & field.Value & chr(34) & vbcrlf)
						end if 
		            Next
					' move the recordpointer
					rsExtPlan.MoveNext				
				Loop
				rsExtPlan.Close
				Set rsExtPlan = Nothing 
	Else
		Response.Write("0")
	End If
	rsObject.Close
	'objCon.Close
	Set rsObject = nothing
	'Set objCon = nothing%>