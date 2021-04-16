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
				    keyqstr = chr(39) & Request.QueryString("Key" & i) & chr(39)
					keystr = Request.QueryString("Key" & i)
					kValuestr = Request.QueryString("Value" & i) 
				Else
				    keyqstr = keyqstr & "," & chr(39) & Request.QueryString("Key" & i) & chr(39)
					keystr = keystr & "," & Request.Querystring("Key" & i)
					kValuestr = kValuestr & "," & Request.Querystring("Value" & i)
				End If
		Next
		if NumKeys > 0 then
		   ParseString keystr,",",key	
		   ParseString kValuestr,",",kValue
		end if		
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
		   Joins(i) = rstjoins.fields("JoinClause").value
		   rstjoins.movenext	
		Loop
		jcount = i
		rstjoins.Close
      
        Set rsparamkeys = Server.CreateObject("ADODB.Recordset")
	    strPKeys =  "select count(*) as PKeycount from tblkeyfilter where ExtPlanID = " & rsObject.Fields("ExtPlanID").Value 
	    rsparamkeys.Open strPKeys,ObjMaster,adOpenStatic,adLockOptimistic
		
		' extract the conversion units information for the selected extraction plan
		Set rsUnits = Server.CreateObject("ADODB.Recordset")
		' build the SQL statement to get the units conversion string
		strSQLu = "SELECT " & _
					"UnitsClause " & _
				  "FROM " & _
					"tblExtPlanUnits " & _
				  "WHERE " & _
					"ExtPlanID = " & rsObject.Fields("ExtPlanID").Value
		' return the recordset
		rsUnits.Open strSQLu, objMaster
		' check to see if any unit conversions were returned
		If Not rsUnits.EOF Then
			' store the units string in a local variable
			units = rsUnits.Fields("UnitsClause").Value
		Else
			' set the units string to "none"
			units = "none"
		End If
		' close and deallocate the recordset object
		rsUnits.Close
		Set rsUnits = Nothing

    	'Response.write "select FieldName,kf.FieldID,Operator from tblkeyfilter kf,tblfields f, tblExtPlanFields EPF where kf.FieldID = f.FieldID and EPF.FieldID = f.FieldID and kf.ExtPlanID = EPF.ExtPlanID and kf.ExtPlanID = " & rsObject.Fields("ExtPlanID").Value & " order by EPFieldID"

		' retrieve the corresponding filters to add to "where" clause
		Set rsfilters = Server.CreateObject("ADODB.Recordset")
		'response.write "select FieldName,kf.FieldID,Operator from tblkeyfilter kf,tblfields f where kf.FieldID = f.FieldID and kf.ExtPlanID = " & rsObject.Fields("ExtPlanID").Value 
		strSQLf =  "select TableName,FieldName,kf.FieldID,Operator from tblkeyfilter kf,tblfields f where kf.FieldID = f.FieldID and kf.ExtPlanID = " & rsObject.Fields("ExtPlanID").Value 
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
		  FieldName(i) = rsfilters.Fields("TableName").Value & "." & rsfilters.Fields("FieldName").Value 
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

			'Response.write jcount
		    'if plancount > 0 then
		    'Parse ExtPlanSQL
		    
		    strOSQL = rsObject.Fields("ExtPlanSQL").Value
			strSQL = rsObject.Fields("ExtPlanSQL").Value

			if (CINT(rsparamkeys.fields("PKeyCount").value) = CINT(NumKeys)) then
			   if jcount > 0 or NumKeys > 0 then
				  strSQL = Left(strSQL,Instr(1,strSQL,"WHERE") + 5)
		       end if
			else
'			response.write NumKeys
'			Response.write rsparamkeys.fields("PKeyCount").value
				strSQL = Left(strSQL,Instr(1,strSQL,"FROM") - 1)

				if (NumKeys) > 0 then
				   strKeyFind = "SELECT KeyName,TableName,FieldName from tblKeyFilter KF,tblParamKeys P,tblFields F " & _ 
								 " WHERE KF.ParamKeyID = P.KeyID" & _
			                     " AND F.FieldID = KF.FieldID" &_
			                     " AND ExtPlanID = " & rsObject.Fields("ExtPlanID").Value &_
								 " AND KeyName not in (" & keyqstr & ")"
			     else
			     	strKeyFind = "SELECT KeyName,TableName,FieldName from tblKeyFilter KF,tblParamKeys P,tblFields F " & _ 
								 " WHERE KF.ParamKeyID = P.KeyID" & _
			                     " AND F.FieldID = KF.FieldID" &_
			                     " AND ExtPlanID = " & rsObject.Fields("ExtPlanID").Value
			     end if
					' define a recordset to get the data type with
					Set rsKFind = Server.CreateObject("ADODB.Recordset")
			'		' execute the query
					rsKFind.Open strKeyFind, objMaster
			'   For each paramkey not passed in (in tblkeyfilter but DNE in key())
			   tmpcount = 0
			   do While not rsKFind.EOF 
                  tmpcount = tmpcount + 1
                  'if tmpcount = 1 then
                    strTmpTableName = rsKFind.Fields("TableName").Value
					If Instr(strTmpTableName," ") > 0 Then
						strTmpTableName = "[" + strTmpTableName + "]"
					End If
					strTmpFieldName = rskFind.Fields("FieldName").Value
					If Instr(strTmpFieldName," ") > 0 Then
						strTmpFieldName = "[" + strTmpFieldName + "]"
					End If
                     strSQL = strSQL & "," & strTmpTableName & "." & strTmpFieldName & " AS " & rsKFind.Fields("KeyName").Value    			        
                  'else
			      '   strSQL = strSQL & rsKFind.Fields("TableName").Value & "." & rsKFind.Fields("FieldName").Value & " AS " & rsKFind.Fields("KeyName").Value
                  'end if
                  rsKFind.movenext
			   Loop
			   'Response.Write tmpcount
			   rsKFind.close
			'   extract 'from' clause of extplansql 
			   intfSQL = Instr(1,strOSQL,"FROM")
			   intwSQL = Instr(1,strOSQL,"WHERE")
			   strSQL = strSQL & mid(strOSQL,intfSQL,intwSQL-intfSQL)
			   'if # of missing keys is < the total # of param keys for this param, add "where"
			   if (Cint(tmpcount) < CINT(rsparamkeys.fields("PKeyCount").value)) then
			      strSQL = strSQL & " WHERE "
			   end if
            end if
            rsParamKeys.close
'            Response.Write strSQL 
            
				' set up a loop to process the join conditions
				For i = 0 to jcount
				   If instr(strSQL,"Where") = 0 Then
						strSQL = strSQL & " WHERE "
				   End If
				   strSQL = strSQL & Joins(i)
				   if i < jcount then
				       'Response.Write "here"
				       strSQL = strSQL & " And "
				   end if
			    next
				   
'				Dim reorderedkey(fcount)
'				let k= 0
'Response.Write fcount
				addedfcount = 0
				
				' "and" to where clause after join conditions if there is at least one
				if jcount > -1 and CINT(NumKeys) > 0 then
				    strSQL = strSQL & " And "
				end if
				    
				For i = 0 to fcount

				       
					' bust up the field id from the field name
					fName = FieldName(i)
					fID = FieldID(i)
'Response.Write "SELECT KeyName from tblKeyFilter KF,tblParamKeys P" & _ 
'								 " WHERE KF.ParameterID = P.KeyID" & _
'								 " AND KF.FieldID = " & fID & _
'								 " AND ExtPlanID = " & rsObject.Fields("ExtPlanID").Value 
					strKeyName = "SELECT KeyName from tblKeyFilter KF,tblParamKeys P " & _ 
								 " WHERE KF.ParamKeyID = P.KeyID" & _
								 " AND KF.FieldID = " & fID & _
								 " AND ExtPlanID = " & rsObject.Fields("ExtPlanID").Value 
					' define a recordset to get the data type with
					Set rsKName = Server.CreateObject("ADODB.Recordset")
					' execute the query
					rsKName.Open strKeyName, objMaster
					' store the value in a local variable
					KName = rsKName.Fields("KeyName").Value
					KName = RTrim(KName)
					' close and deallocate the recordset
					rsKName.Close
					Set rsKName = Nothing
					'Response.Write KName								 									
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
							            
					x = -1
					For j= 0 to NumKeys-1
					   	  'Response.Write key(j)
					   	  'Response.Write KName
					   	  'Response.Write x
					   if key(j) = KName then
					   	  'Response.Write key(j)
					   	  'response.write j
		                  x=j
'		                  reorderedkey(k) = key(j)
'		                  k = k +1
		               end if
		            next      				
		            'Response.Write x
		            if (x > -1) then   
					' set up a case statement to determine whether or not quotes are needed
					    Select Case strDataType
						    Case "7","129","130","133","134","135"
								'these are either string or date--quotes are required
								'kvalue(x) = char(34) & kvalue(x) & char(34)
								'''''''''''''''''''''
								' commented out 4/19/2001 KED
								'strSQL = strSQL & FName & Replace(Operator(i),"v",chr(34) & kvalue(x) & chr(34))
								If instr(1,Operator(i),"'") > 0 then
									strSQL = strSQL & FName & " " & Replace(Operator(i),"v",kvalue(x))
								Else
									strSQL = strSQL & FName & " " & Replace(Operator(i),"v",chr(34) & kvalue(x) & chr(34))
								End If
							Case Else
								' these types do not require quotes
						   		'''''''''''''''''''''''''
						   		' commented out 4/19/2001 KED
						   		'strSQL = strSQL & FName & Replace(Operator(i),"v",kvalue(x))
						   		strSQL = strSQL & FName & " " & Replace(Operator(i),"v",kvalue(x))
					    End Select
					    addedfcount = addedfcount + 1
				        ' check the value of i
				        'Response.Write addedfcount
				        'Response.Write "here"
				        'Response.Write Numkeys
				        'Response.Write "heres"
				    	If addedfcount < CINT(NumKeys) Then
					    	strSQL = strSQL & " AND "
			    		End If
					end if
				Next
				
				if staticfilter <> "" then
				   if (fcount = 0) and (jcount = 0) then
				      strSQL = strSQL & staticfilter
		           else				    
	    	          strSQL = strSQL & " AND " & staticfilter
				   end if
				end if
				
			'Response.Write strSQL 
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
                   
				' check to see if we have any conversions to worry about
				If units <> "none" Then
					'units = Request("Units")
					' there are conversions to be implemented--here we will create
					'	an array that contains (1) the fieldname of the field to be
					'	converted, (2) the conversion factor(s), and (3) the direction
					'	of the conversion (F means multiply, R means divide)
					Dim ConvertArray()
					arraycount = 0
					currentloc = 1
					
					Do While True
						' check to see if there is only one entry left in the list
						If (instr(currentloc,units,",")) = 0 Then
							' add a new element to the array
							arraycount = arraycount + 1
							ReDim Preserve ConvertArray(5, arraycount - 1)
							' there is no comma in the list so there is only one
							'	field to be converted
							tildaloc = Instr(currentloc,units,"~")
							periodloc = Instr(currentloc,units,".")
							' extract the fieldname
							ConvertArray(0, arraycount - 1) =  Mid(units,currentloc,tildaloc - currentloc)
							' extract the conversion id
							convertID = Mid(units,tildaloc + 1, periodloc - tildaloc - 1)
							' extract the direction of the conversion
							ConvertArray(5,arraycount - 1) = Mid(units, periodloc + 1)
							' next we need to generate an SQL statement to extract
							'	the conversion factors so that they can be loaded 
							'	into the array
							strSQL = "SELECT " & _
										"OffsetFactor, " & _
										"LinearFactor, " & _
										"QuadraticFactor, " & _
										"CubicFactor " & _
									 "FROM " & _
										"tblConversion " & _
									 "WHERE " & _
										"ConvertID = " & convertID
							' create a recordset object to return the conversion factors
							Set rsFactors = Server.CreateObject("ADODB.Recordset")
							' return the recordset
							rsFactors.Open strSQL, objMaster
							' load the factors into the array
							ConvertArray(1,arraycount - 1) = rsFactors.Fields("OffsetFactor").Value
							ConvertArray(2,arraycount - 1) = rsFactors.Fields("LinearFactor").Value
							ConvertArray(3,arraycount - 1) = rsFactors.Fields("QuadraticFactor").Value
							ConvertArray(4,arraycount - 1) = rsFactors.Fields("CubicFactor").Value
							' close the recordset
							rsFactors.Close
							Set rsFactors = Nothing
							' all the infomation is now stored in the array--exit the loop
							Exit Do
						Else
							' here we have more than one element left to process
							' add a new element to the array
							arraycount = arraycount + 1
							ReDim Preserve ConvertArray(5,arraycount - 1)
							' there is no comma in the list so there is only one
							'	field to be converted
							tildaloc = Instr(currentloc,units,"~")
							periodloc = Instr(currentloc,units,".")
							' extract the fieldname
							ConvertArray(0,arraycount - 1) =  Mid(units,currentloc,tildaloc - currentloc)
							' extract the conversion id
							convertID = Mid(units,tildaloc + 1, periodloc - tildaloc - 1)
							' extract the direction of the conversion
							ConvertArray(5,arraycount - 1) = Mid(units, periodloc + 1, Instr(currentloc,units,",") - periodloc - 1)
							' next we need to generate an SQL statement to extract
							'	the conversion factors so that they can be loaded 
							'	into the array
							strSQL = "SELECT " & _
										"OffsetFactor, " & _
										"LinearFactor, " & _
										"QuadraticFactor, " & _
										"CubicFactor " & _
									 "FROM " & _
										"tblConversion " & _
									 "WHERE " & _
										"ConvertID = " & convertID
							' create a recordset object to return the conversion factors
							Set rsFactors = Server.CreateObject("ADODB.Recordset")
							' return the recordset
							rsFactors.Open strSQL, objMaster
							' load the factors into the array
							ConvertArray(1,arraycount - 1) = rsFactors.Fields("OffsetFactor").Value
							ConvertArray(2,arraycount - 1) = rsFactors.Fields("LinearFactor").Value
							ConvertArray(3,arraycount - 1) = rsFactors.Fields("QuadraticFactor").Value
							ConvertArray(4,arraycount - 1) = rsFactors.Fields("CubicFactor").Value
							' close the recordset
							rsFactors.Close
							Set rsFactors = Nothing
							' now relocate currentloc for the next element
							currentloc = Instr(currentloc,units,",") + 1
						End If
					Loop
				Else
					' set the arraycount to 0
					arraycount = 0
				End If

				' set up a loop to load the returned fields
				Do While not rsExtPlan.EOF
		          j = 1
					For Each field In rsExtPlan.Fields
						' check to see any unit conversion needs to take place
						If arraycount > 0 Then
							' check to see if the current field is one that needs to be 
							'	converted
							blnPrinted = false
							For i = 0 to arraycount - 1
								If ConvertArray(0,i) = field.Name Then
									' this is a field we need to convert--perform the conversion
									' check to see if it is a forward or reverse conversion
									If ConvertArray(5,i) = "F" Then
										'it is a forward conversion
										varTemp = ConvertArray(1,i) + (ConvertArray(2,i) * field.Value) + _
													(ConvertArray(3,i) * (field.Value)^2) + _
													(ConvertArray(4,i) * (field.Value)^3)
									Else
										' it is a reverse conversion
										varTemp = field.Value / ConvertArray(2,i)
									End If
									' varTemp now holds the value to be outputted--exit the loop
									blnPrinted = true
									Exit For
								End If
							Next
							' check to see if varTemp was loaded
							If blnPrinted = false Then
								' load varTemp with the unconverted valued
								varTemp = field.Value
							End If
						Else
							' here no conversions are required--load varTemp with the
							'	unconverted value
							varTemp = field.Value
						End If
						' now we are ready to output the value to the result page
						if (j <> rsExtPlan.Fields.Count) then
							j = j + 1
				    		Response.Write(chr(34)& varTemp & chr(34) & ",")
						else
				    		Response.Write(chr(34) & varTemp & chr(34) & vbcrlf)
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