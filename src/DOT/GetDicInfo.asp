<%@ Language=VBScript %>
<!-- #include file = "MasterDB.asp" -->
<%	' load the request variables into local variables
	dicType = Request.Form("dicType")
	dicPath = Request.Form("dicPath")
	dicName = Request.Form("dicName")
	dicDesc = Request.Form("dicDesc")
	dicUName = Request.Form("dicUName")
	dicPassword = Request.Form("dicPassword")
	dicSource = Request.Form("dicSource")
	Set objMaster = Server.CreateObject("ADODB.Connection")
	Set rsConnect = Server.CreateObject("ADODB.Recordset")
	Set objTarget = Server.CreateObject("ADODB.Connection")
	Set rsSchema = Server.CreateObject("ADODB.Recordset")
	Set rsTemp = Server.CreateObject("ADODB.Recordset")

	' connect to the master database
    objMaster.Open MasterConnect
    ' build a SQL string to insert the data passed in
    strSQL = "INSERT INTO " & _
                "tblDictionary(" & _
                    "DicName, " & _
                    "Description, " & _
                    "Source, " & _
                    "ConnectString) " & _
             "VALUES (" & _
                    "'" & dicName & "', " & _
                    "'" & dicDesc & "', " & _
                    "'" & dicSource & "', " & _
                    "'" & dicPath & "')"
    ' execute the insert
    objMaster.Execute strSQL
      ' build a SQL statement to retrieve the new id for the record
    '   that was just inserted
    strSQL = "SELECT " & _
                "DicID " & _
             "FROM " & _
                "tblDictionary " & _
             "WHERE " & _
                "DicName = '" & dicName & "' " & _
             "AND " & _
                "ConnectString = '" & dicPath & "'"
    ' get the recordset
    rsTemp.Open strSQL, objMaster, adOpenStatic, adLockReadOnly
    ' set the function equal to the id returned
    dicID = rsTemp.Fields("DicID").Value
    ' close and deallocate the recordset and the connection
    rsTemp.Close%>
<HTML>
<HEAD>
<META NAME="GENERATOR" Content="Microsoft Visual Studio 6.0">
</HEAD>
<body background="frames.jpg">
<FORM name="GDBForm" method="POST" action="ParseDictionary.asp">
	<INPUT type="hidden" id=dicID name=dicID value=<%=dicID%>>
</FORM>
<SCRIPT Language=Javascript>
	// some javascript to automatically post the form
	document.GDBForm.submit();
</SCRIPT>
</BODY>
</HTML>
