
#ifndef References_H
#define References_H
#ifndef BUILD_DLL
  #define FRAMES_API __declspec(dllimport) __stdcall
#else
  #define FRAMES_API __declspec(dllexport) __stdcall
#endif
// API Name: FRAMES References API
  #ifdef __cplusplus
    extern "C" {
  #else
    #define bool int
  #endif

   /*
   Reference functions.

   */
     
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SaveRefs
   Save the references permanently (to disk).
   Return is negative if error.
  
   */

       int FRAMES_API _SaveRefs (
       int PID);
       #ifndef BUILD_DLL
         #define SaveRefs _SaveRefs
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: SaveVarRefs
   Save the used references permanently (to disk).
   Return is negative if error.
  
   */

       int FRAMES_API _SaveVarRefs (
       int PID);
       #ifndef BUILD_DLL
         #define SaveVarRefs _SaveVarRefs
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: FindRef
   Returns the index of the reference with name _name.  
    Search is linear, thus slow.
    Return is negative if failure.
  
   */

       int FRAMES_API _FindRef (
       int PID, char *  _refName, int * pos);
       #ifndef BUILD_DLL
         #define FindRef _FindRef
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: PutRef
   Puts a reference name/id and it's reference to position
   _pos.  By put, i mean any values at _pos will be replaced.
   This is a useful function for renaming references or editing
   existing references.
  
   */

       int FRAMES_API _PutRef (
       int PID, int _pos, char *  _refName, char *  _ref);
       #ifndef BUILD_DLL
         #define PutRef _PutRef
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddRef
   Add a reference to the reference dataset.
  
   */

       int FRAMES_API _AddRef (
       int PID, char *  _name, char *  _ref);
       #ifndef BUILD_DLL
         #define AddRef _AddRef
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetRef
   Retreive a reference from the reference dataset, using the
    reference name/id as a search key.
  
   */

       int FRAMES_API _GetRef (
       int PID, char *  _refName, char *  _ref);
       #ifndef BUILD_DLL
         #define GetRef _GetRef
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DelRef
   Deletes a reference from the reference dataste.  WARNING this
   function doesn't track down all the variables/values that are
   associated with this references and remove them.  You would
   have to go through manually and remove each references to this
   reference.
  
   */

       int FRAMES_API _DelRef (
       int PID, char *  _name);
       #ifndef BUILD_DLL
         #define DelRef _DelRef
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetRefCount
   Retrieves the number of references in the references dataset.
   Use this function with GetRefByIndex to enumerate through
   all the references.
  
   */

       int FRAMES_API _GetRefCount (
       int PID, int * _count);
       #ifndef BUILD_DLL
         #define GetRefCount _GetRefCount
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetRefByIndex
   Retreive a reference from the reference dataset using an index.
  
   */

       int FRAMES_API _GetRefByIndex (
       int PID, int _index, char *  _refName, char *  _ref);
       #ifndef BUILD_DLL
         #define GetRefByIndex _GetRefByIndex
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: AddVarRef
  Associate a reference with a variable or value using the indices _1 through _6.
  This function does no checking to verify that a value actually exists at the
  indices.  The indices don't even have to point to the variable/value you want
  to associate with a reference, but it is easier to pretend that they do.
 
   */

       int FRAMES_API _AddVarRef (
       int PID, char *  _dataset, char *  _variable, int _1, int _2, int _3, int _4, int _5, int _6, char *  _refName);
       #ifndef BUILD_DLL
         #define AddVarRef _AddVarRef
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: DelVarRef
  Dis-associate a reference with a variable or value using the indices _1 through _6.
  This function does no checking to verify that a value actually exists at the
  indices.  The indices don't even have to point to the variable/value you want
  to associate with a reference, but it is easier to pretend that they do.
 
   */

       int FRAMES_API _DelVarRef (
       int PID, char *  _dataset, char *  _variable, int _1, int _2, int _3, int _4, int _5, int _6, char *  _refName);
       #ifndef BUILD_DLL
         #define DelVarRef _DelVarRef
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetVarRefCount
  Returns the amount of references associated with the variable/value at _variable
  and indices _1 through _6.
  This function does no checking to verify that a value actually exists at the
  indices.  The indices don't even have to point to the variable/value you want
  to associate with a reference, but it is easier to pretend that they do.
 
   */

       int FRAMES_API _GetVarRefCount (
       int PID, char *  _dataset, char *  _variable, int _1, int _2, int _3, int _4, int _5, int _6, int * _refCount);
       #ifndef BUILD_DLL
         #define GetVarRefCount _GetVarRefCount
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: GetVarRef
  Returns the reference at position _index in the references list associated
  with the variable/value at _variable and indices _1 through _6.  Use GetRef
  with _refName to get the reference from the reference dataset.
  This function does no checking to verify that a value actually exists at the
  indices.  The indices don't even have to point to the variable/value you want
  to associate with a reference, but it is easier to pretend that they do.
 
   */

       int FRAMES_API _GetVarRef (
       int PID, char *  _dataset, char *  _variable, int _1, int _2, int _3, int _4, int _5, int _6, int _refIndex, char *  _refName);
       #ifndef BUILD_DLL
         #define GetVarRef _GetVarRef
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ViewRefsDialog
   Display a modal box for managing all references via an interface.
   After closing the dialog, all changes are saved permanently.
   Return is negative if error.
  
   */

       int FRAMES_API _ViewRefsDialog (
       int PID);
       #ifndef BUILD_DLL
         #define ViewRefsDialog _ViewRefsDialog
       #endif

  
   /*-------------------------------------------------------------------------*/
   /* Documentation for: ViewRefDialog
   Display a modal box for managing references to a particular
   value.
   After closing the dialog, all changes are saved permanently.
   Return is negative if error.
  
   */

       int FRAMES_API _ViewRefDialog (
       int PID, char *  _dataset, char *  _variable, int _1, int _2, int _3, int _4, int _5, int _6);
       #ifndef BUILD_DLL
         #define ViewRefDialog _ViewRefDialog
       #endif

  
  #ifdef __cplusplus
  }
  #endif
#endif
