'-----------------------------------------------------------------------------------------------
'Build Target record Set using ADO / ODBC. Build the target_rs disconnected record set 
'by calling OpenSchema adSchemaTables and adSchemaColumns.  This method has been "moved
'to the back burner" since there were several major datatype mapping anomalies
'that we judged too significant to ignore.  e.g. smalldatetime was reported as datetime.
'
'To make the result set compatible with the other "build_target_rs" queries a dynamic 
'record set is created and loaded.  This routine should/may still work for other database
'vendors with ODBC level 2 compliant drivers.
'
'Parameters:
'	target_rs		Output	The populated Target record Set
'	target_objCon			The database connection for the target
'	db_name					DB Name.  Required Query parameter.
'	db_owner				DB Owner.  Required Query parameter.
'	table_name				Table / View Name.  Optional Query parameter.
'	verbose					Verbose Output (for debugging)
'
' 6/13/00 JBurke: Original Version
'------------------------------------------------------------------------------------------------
if querydb != ""
target_rs = "tbltables"
target_objCon = "Provider=SQLOLEDB.1;User ID=sa;Initial Catalog=FramesStructures;Data Source=WD23657;Connect Timeout=240","sa",""
db_name = "querydb"
db_owner = "sa"
?Call build_target
<FORM name=DTForm method="post" action="BuildFramesStructure.asp">
<UL>
	<LI>Select the source of the data to be queried: 
		<SELECT name="source">
<%
		Do While not rsObject.EOF	
%>	
			<OPTION value=<%=rsObject.Fields("DatabaseID").Value%>><%=rsObject.Fields("DatabaseName").Value%></OPTION>
<%	
			rsObject.MoveNext
		Loop
%>
		</SELECT>
	</LI>
</UL>
<UL>	
	<LI>Enter a path to the query database: 
	<INPUT name="querydb">
	</LI>
</UL>
<INPUT type="submit" value="Submit" id=submit1 name=submit1>
</FORM>
</BODY>
<%
End If

Sub build_target_rs_ado (target_rs, target_objCon, db_name, db_owner, table_name, DBMSName, DBMSVersion, verbose)

	'target OpenSchema adSchemaTables and adSchemaColumns record sets
	dim target_schtab, target_schcol
	'control variables to enable/disable:
	'...wildcard testing on the table name, 
	'...building the columns record set for every row ,
	'...synchronizing table and column names (on the table name)
	dim match_wildcards, build_schemaColumns, synch_schemaColumns
	'regular expression variables
	dim reg, regSearch
	'set to true if the table should be included in the record set
	dim scanTable
	
    'if table_name contains % or is blank, then query tables to find matching enties
    if table_name = "" then
        ' Return all tables
        Set target_schtab = target_objCon.OpenSchema(adSchemaTables,Array(db_name, db_owner, Null))
        Set target_schcol = target_objCon.OpenSchema(adSchemaColumns,Array(db_name, db_owner, Null, Null))
        target_schtab.Sort = "TABLE_CATALOG ASC, TABLE_SCHEMA ASC, TABLE_NAME ASC"
        target_schcol.Sort = "TABLE_CATALOG ASC, TABLE_SCHEMA ASC, TABLE_NAME ASC, COLUMN_NAME ASC"
        match_wildcards = vbFalse
        build_schemaColumns = vbFalse
        synch_schemaColumns = vbTrue
    elseif inStr (table_name,"%") > 0 Then
        ' Return all tables, but build schema columns record sets only for applicable tables
        Set target_schtab = target_objCon.OpenSchema(adSchemaTables,Array(db_name, db_owner, Null))
        target_schtab.Sort = "TABLE_CATALOG ASC, TABLE_SCHEMA ASC, TABLE_NAME ASC"
        match_wildcards = vbTrue
        build_schemaColumns = vbTrue
        synch_schemaColumns = vbFalse
    else
        ' Return info on specified table only
        Set target_schtab = target_objCon.OpenSchema(adSchemaTables,Array(db_name, db_owner, table_name))
        Set target_schcol = target_objCon.OpenSchema(adSchemaColumns,Array(db_name, db_owner, table_name, Null))
        target_schtab.Sort = "TABLE_CATALOG ASC, TABLE_SCHEMA ASC, TABLE_NAME ASC"
        target_schcol.Sort = "TABLE_CATALOG ASC, TABLE_SCHEMA ASC, TABLE_NAME ASC, COLUMN_NAME ASC"
        match_wildcards = vbFalse
        build_schemaColumns = vbFalse
        synch_schemaColumns = vbFalse
    end if

    If verbose then
        showTable "OpenSchema adSchemaTables RecordSet", target_schtab
        if not build_schemaColumns Then
            showTable "OpenSchema adSchemaColumns RecordSet", target_schcol
        end if
    End If

    target_rs.CursorLocation = adUseClient
    target_rs.LockType = adLockBatchOptimistic

    target_rs.Fields.Append "TABLE_CATALOG",            adVarChar, 127, adFldUpdatable + adFldIsNullable
    target_rs.Fields.Append "TABLE_SCHEMA",             adVarChar, 127, adFldUpdatable + adFldIsNullable
    target_rs.Fields.Append "TABLE_NAME",               adVarChar, 127, adFldUpdatable
    target_rs.Fields.Append "TABLE_TYPE",               adVarChar, 15,  adFldUpdatable
    target_rs.Fields.Append "COLUMN_NAME",              adVarChar, 127, adFldUpdatable
    target_rs.Fields.Append "DATATYPE_TITLE",           adVarChar, 127, adFldUpdatable + adFldIsNullable
    target_rs.Fields.Append "NULLITY_TITLE",            adVarChar, 20,  adFldUpdatable + adFldIsNullable
    target_rs.Fields.Append "IS_NULLABLE",              adBoolean, ,    adFldUpdatable + adFldIsNullable
    target_rs.Fields.Append "DATA_TYPE",                adInteger, ,    adFldUpdatable + adFldIsNullable
    target_rs.Fields.Append "COLUMN_FLAGS",             adInteger, ,    adFldUpdatable + adFldIsNullable
    target_rs.Fields.Append "DATETIME_PRECISION",       adInteger, ,    adFldUpdatable + adFldIsNullable
    target_rs.Fields.Append "CHARACTER_MAXIMUM_LENGTH", adInteger, ,    adFldUpdatable + adFldIsNullable
    target_rs.Fields.Append "NUMERIC_PRECISION",        adInteger, ,    adFldUpdatable + adFldIsNullable
    target_rs.Fields.Append "NUMERIC_SCALE",            adInteger, ,    adFldUpdatable + adFldIsNullable

    ' open the new recordset structure
    target_rs.Open

    ' create a regular expression object
    Set reg = New RegExp
    reg.Global = vbTrue
    ' Prepare the search string: replacing % with .* (match any except linefeed 0 to many times) and _ with .{1} (match any except linefeed exactly 1 time)
    ' FIXME: Fieldnames that contain the Regular expression special characters are trouble (e.g. .)
    ' FIXME: The Special character issue is an issue for SQL Server7, ignore for now...
    regSearch = table_name
    reg.Pattern = "[\%]+"
    regSearch = reg.Replace(regSearch,".*")
    reg.Pattern = "\_"
    regSearch = reg.Replace(regSearch,".{1}")
    'response.write "Search Pattern: " & regSearch & "<br>"
    reg.Pattern = regSearch

    'response.write("Copying Rows")
    Do While Not target_schtab.EOF
        table_name = target_schtab.Fields("TABLE_NAME")

        'if wildcards used, then need to check if schema tables table name matches search table name (table_name)
        scanTable = not match_wildcards
        if reg.Test(table_name) and match_wildcards Then
            'response.write "Match on: " & table_name & "<br>"
            scanTable = vbTrue
        end if

        If scanTable Then
            if build_schemaColumns Then
                Set target_schcol = target_objCon.OpenSchema(adSchemaColumns,Array(db_name, db_owner, table_name, Null))
            end If

            Do While Not target_schcol.EOF
                'response.write target_schtab.Fields("TABLE_NAME") & "." & target_schcol.Fields("COLUMN_NAME") & "<br>"

                target_rs.AddNew
                target_rs.Fields("TABLE_CATALOG").Value = uCase(target_schtab.Fields("TABLE_CATALOG"))
                target_rs.Fields("TABLE_SCHEMA").Value = uCase(target_schtab.Fields("TABLE_SCHEMA"))
                target_rs.Fields("TABLE_NAME").Value = uCase(target_schtab.Fields("TABLE_NAME"))
                target_rs.Fields("TABLE_TYPE").Value = uCase(target_schtab.Fields("TABLE_TYPE"))

                target_rs.Fields("COLUMN_NAME").Value = uCase(target_schcol.Fields("COLUMN_NAME"))
                target_rs.Fields("IS_NULLABLE").Value = target_schcol.Fields("IS_NULLABLE")
                target_rs.Fields("DATA_TYPE").Value = target_schcol.Fields("DATA_TYPE")
                target_rs.Fields("COLUMN_FLAGS").Value = target_schcol.Fields("COLUMN_FLAGS")
                target_rs.Fields("DATETIME_PRECISION").Value = target_schcol.Fields("DATETIME_PRECISION")
                target_rs.Fields("CHARACTER_MAXIMUM_LENGTH").Value = target_schcol.Fields("CHARACTER_MAXIMUM_LENGTH")
                target_rs.Fields("NUMERIC_PRECISION").Value = target_schcol.Fields("NUMERIC_PRECISION")
                target_rs.Fields("NUMERIC_SCALE").Value = target_schcol.Fields("NUMERIC_SCALE")

                If target_schcol.Fields("IS_NULLABLE") Then
                    target_rs.Fields("NULLITY_TITLE").Value = "Null"
                Else
                    target_rs.Fields("NULLITY_TITLE").Value = "Not Null"
                End If
                target_rs.Fields("DATATYPE_TITLE").Value = get_ado_datatype_title(target_rs.Fields, DBMSName)

                'all fields have been loaded into the record--update
                target_rs.Update
                target_schcol.MoveNext

                if synch_schemaColumns and (Not target_schcol.EOF) Then
                    if target_schtab.Fields("TABLE_NAME") <> target_schcol.Fields("TABLE_NAME") Then
                         target_schtab.MoveNext
                    end if
                End If
            Loop
        End if

        if Not target_schtab.EOF Then
            target_schtab.MoveNext
        end if
    Loop

    If not (target_rs.EOF and target_rs.BOF) Then
        target_rs.MoveFirst
    End If

End Sub
 
'-----------------------------------------------------------------------------------------------
'Copy Column Definitions.  A handy routine for copying a record set definition.  The If statement
'for the unsignedInt fields was added because an ADO bug prevents these fields from being appended
'and used successfully (strange).  The ordinal_position test was added since Oracle actually duplicates
'the column in the OpenSchema definition (oracle odbc bug).  Column default was skipped because it was
'missing the required datatype size information (another oracle odbc bug).
'
'WARNING: Not used!!!!  I created the target_rs table definition statically.
'
'Parameters:
'	source_rs				The record set definition to copy
'	target_rs		Output	The Target record Set
'
' 6/13/00 JBurke: Original Version
'------------------------------------------------------------------------------------------------
Sub Copy_Column_Definitions (source_rs, target_rs)
    'NOT SURRENTLY USED !!!!!!!!!!!!!!!!!!!!!!!
    'Setup columns
    'response.write("Copy Column Definitions")
    For intloop = 0 To source_rs.Fields.Count - 1
        With source_rs.Fields(intloop)
            'response.write("N:"& .Name & " T:" & .Type & " DS:" & .DefinedSize & " A:" & .Attributes & "<br>")
            response.write("target_rs.Fields.Append "& .Name & "," & .Type & ", " & .DefinedSize & ", " & .Attributes & "<br>")
            'target_rs.Fields.Append .Name, .Type, .DefinedSize, adFldUpdatable + adFldIsNullable
            If .Type = adUnsignedInt Or .type = adUnsignedSmallInt Then
                target_rs.Fields.Append .Name, adInteger, , .Attributes
            ElseIf .Name = "COLUMN_DEFAULT" or .Name = "ORDINAL_POSITION" Then
                'With ORACLE Ordinal_position is defined twice and causes the .append to fail
                'With Oracle the COLUMN_DEFAULT is not given a size and causes the .append to fail
                'response.write("SKIPPED!!!! N:"& .Name & " T:" & .Type & " DS:" & .DefinedSize & " A:" & .Attributes & "<br>")
                'target_rs.Fields.Append .Name, .Type, 1, .Attributes
            Else
                target_rs.Fields.Append .Name, .Type, .DefinedSize, .Attributes
            End If
        End With
    Next

End Sub


'-----------------------------------------------------------------------------------------------
'Copy Rows.  A handy routine for copying the rows from one record set to another (assuming the
'two record sets have compatible structure).  The If statement for table_name and column_name
'was added for sorting.  The If for Column_Default and ordinal_position was added to skip
'these columns (they were trouble makers--see above).
'
'WARNING: Not used!!!!  I loaded the target_rs record set with a statically defined loop.
'
'Parameters:
'	source_rs				The record set definition to copy
'	target_rs		Output	The Target record Set
'
' 6/13/00 JBurke: Original Version
'------------------------------------------------------------------------------------------------
Sub Copy_Rows (source_rs, target_rs)
    'NOT SURRENTLY USED !!!!!!!!!!!!!!!!!!!!!!!
    'Copy rows
    'response.write("Copying Rows")
    Do While Not source_rs.EOF
        target_rs.AddNew
        ' set up a loop to load the columns into the programmatic recordset
        For intloop = 0 To source_rs.Fields.Count - 1
            With source_rs.Fields(intloop)
                If .Name = "TABLE_NAME" Or .Name = "COLUMN_NAME" Then
                    target_rs.Fields(.Name).Value = uCase(.Value)
                ElseIf not (.Name = "COLUMN_DEFAULT" or .Name = "ORDINAL_POSITION") Then
                    target_rs.Fields(.Name).Value = .Value
                End If
            End With
        Next
        'all fields have been loaded into the record--update
        target_rs.Update
        source_rs.MoveNext
    Loop
    If not (target_rs.EOF and target_rs.BOF) Then
        target_rs.MoveFirst
    End If

End Sub

%>