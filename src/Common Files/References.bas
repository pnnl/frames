Attribute VB_Name = "ReferencesAPI"
Option Explicit
' API Name: FRAMES References API
'-------------------------------------------------------------------------
' Documentation for: SaveRefs
' Save the references permanently (to disk). Return is negative if error.
DECLARE Function SaveRefs LIB "systemio.dll" Alias "__SaveRefs@4" (  ByVal PID as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: SaveVarRefs
' Save the used references permanently (to disk). Return is negative if error.
DECLARE Function SaveVarRefs LIB "systemio.dll" Alias "__SaveVarRefs@4" (  ByVal PID as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: FindRef
' Returns the index of the reference with name _name. Search is linear, thus slow. Return is negative if failure.
DECLARE Function FindRef LIB "systemio.dll" Alias "__FindRef@12" (  ByVal PID as long,  ByVal _refName as string, pos as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: PutRef
' Puts a reference name/id and it's reference to position _pos. By put, i mean any values at _pos will be replaced. This is a useful function for renaming references or editing existing references.
DECLARE Function PutRef LIB "systemio.dll" Alias "__PutRef@16" (  ByVal PID as long,  ByVal _pos as long,  ByVal _refName as string,  ByVal _ref as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddRef
' Add a reference to the reference dataset.
DECLARE Function AddRef LIB "systemio.dll" Alias "__AddRef@12" (  ByVal PID as long,  ByVal _name as string,  ByVal _ref as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetRef
' Retreive a reference from the reference dataset, using the reference name/id as a search key.
DECLARE Function DLLGetRef LIB "systemio.dll" Alias "__GetRef@12" (  ByVal PID as long,  ByVal _refName as string,  ByVal _ref as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DelRef
' Deletes a reference from the reference dataste. WARNING this function doesn't track down all the variables/values that are associated with this references and remove them. You would have to go through manually and remove each references to this reference.
DECLARE Function DelRef LIB "systemio.dll" Alias "__DelRef@8" (  ByVal PID as long,  ByVal _name as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetRefCount
' Retrieves the number of references in the references dataset. Use this function with GetRefByIndex to enumerate through all the references.
DECLARE Function GetRefCount LIB "systemio.dll" Alias "__GetRefCount@8" (  ByVal PID as long, _count as Any )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetRefByIndex
' Retreive a reference from the reference dataset using an index.
DECLARE Function DLLGetRefByIndex LIB "systemio.dll" Alias "__GetRefByIndex@16" (  ByVal PID as long,  ByVal _index as long,  ByVal _refName as string,  ByVal _ref as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: AddVarRef
' Associate a reference with a variable or value using the indices _1 through _6. This function does no checking to verify that a value actually exists at the indices. The indices don't even have to point to the variable/value you want to associate with a reference, but it is easier to pretend that they do.
DECLARE Function AddVarRef LIB "systemio.dll" Alias "__AddVarRef@40" (  ByVal PID as long,  ByVal _dataset as string,  ByVal _variable as string,  ByVal _1 as long,  ByVal _2 as long,  ByVal _3 as long,  ByVal _4 as long,  ByVal _5 as long,  ByVal _6 as long,  ByVal _refName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: DelVarRef
' Dis-associate a reference with a variable or value using the indices _1 through _6. This function does no checking to verify that a value actually exists at the indices. The indices don't even have to point to the variable/value you want to associate with a reference, but it is easier to pretend that they do.
DECLARE Function DelVarRef LIB "systemio.dll" Alias "__DelVarRef@40" (  ByVal PID as long,  ByVal _dataset as string,  ByVal _variable as string,  ByVal _1 as long,  ByVal _2 as long,  ByVal _3 as long,  ByVal _4 as long,  ByVal _5 as long,  ByVal _6 as long,  ByVal _refName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetVarRefCount
' Returns the amount of references associated with the variable/value at _variable and indices _1 through _6. This function does no checking to verify that a value actually exists at the indices. The indices don't even have to point to the variable/value you want to associate with a reference, but it is easier to pretend that they do.
DECLARE Function GetVarRefCount LIB "systemio.dll" Alias "__GetVarRefCount@40" (  ByVal PID as long,  ByVal _dataset as string,  ByVal _variable as string,  ByVal _1 as long,  ByVal _2 as long,  ByVal _3 as long,  ByVal _4 as long,  ByVal _5 as long,  ByVal _6 as long, _refCount as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: GetVarRef
' Returns the reference at position _index in the references list associated with the variable/value at _variable and indices _1 through _6. Use GetRef with _refName to get the reference from the reference dataset. This function does no checking to verify that a value actually exists at the indices. The indices don't even have to point to the variable/value you want to associate with a reference, but it is easier to pretend that they do.
DECLARE Function DLLGetVarRef LIB "systemio.dll" Alias "__GetVarRef@44" (  ByVal PID as long,  ByVal _dataset as string,  ByVal _variable as string,  ByVal _1 as long,  ByVal _2 as long,  ByVal _3 as long,  ByVal _4 as long,  ByVal _5 as long,  ByVal _6 as long,  ByVal _refIndex as long,  ByVal _refName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ViewRefsDialog
' Display a modal box for managing all references via an interface. After closing the dialog, all changes are saved permanently. Return is negative if error.
DECLARE Function ViewRefsDialog LIB "systemio.dll" Alias "__ViewRefsDialog@4" (  ByVal PID as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: ViewRefDialog
' Display a modal box for managing references to a particular value. After closing the dialog, all changes are saved permanently. Return is negative if error.
DECLARE Function ViewRefDialog LIB "systemio.dll" Alias "__ViewRefDialog@36" (  ByVal PID as long,  ByVal _dataset as string,  ByVal _variable as string,  ByVal _1 as long,  ByVal _2 as long,  ByVal _3 as long,  ByVal _4 as long,  ByVal _5 as long,  ByVal _6 as long )  as integer 

Function GetRef (  ByVal PID as long,  ByVal _refName as string  , _ref as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      GetRef = DLLGetRef(PID, _refName, retstr3)
      _ref=StripTerminator(retStr3)
End  Function 

Function GetRefByIndex (  ByVal PID as long,  ByVal _index as long, _refName as string  , _ref as string   )  as integer 
      Dim retStr3 as String * MAXFIELD
      Dim retStr4 as String * MAXFIELD
      GetRefByIndex = DLLGetRefByIndex(PID, _index, retstr3, retstr4)
      _refName=StripTerminator(retStr3)
_ref=StripTerminator(retStr4)
End  Function 

Function GetVarRef (  ByVal PID as long,  ByVal _dataset as string  ,  ByVal _variable as string  ,  ByVal _1 as long,  ByVal _2 as long,  ByVal _3 as long,  ByVal _4 as long,  ByVal _5 as long,  ByVal _6 as long,  ByVal _refIndex as long, _refName as string   )  as integer 
      Dim retStr11 as String * MAXFIELD
      GetVarRef = DLLGetVarRef(PID, _dataset, _variable, _1, _2, _3, _4, _5, _6, _refIndex, retstr11)
      _refName=StripTerminator(retStr11)
End  Function 
