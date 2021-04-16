#ifndef FRAMES2API_F2Enumerate_H
#define FRAMES2API_F2Enumerate_H

#ifndef BUILD_DLL
  #define FRAMES2API __declspec(dllimport) __stdcall
#else
  #define FRAMES2API __declspec(dllexport) __stdcall
#endif

#ifdef __cplusplus
  extern "C" {
#else
  #define bool int
#endif

  // API Name: FRAMES Enumerations API
  /* 
       Date:       2001    Programmer: Bonnie Hoopes    Company:    Pacific 
       Northwest National Laboratories                Operated by Battelle 
       Memorial Institute                For Department of Energy            
               This collection of functions is intended for FRAMES 2.0 
       system developers. Most will not    do anything unless the caller has 
       a Process Id (PID) that was received by calling OpenINI.     This API 
       is intended to support an iterator pattern as a replacement for 
       deprecated "list"    API's from earlier FRAMES version.     The 
       Iterator Pattern provides a standard mechanism to access elements of 
       collections    sequentially without exposing its underlying 
       representation,  Using an Iterator pattern we    can provide a single 
       API so that stepping through a set of dictionaries can be done in    
       exactly the same way as stepping through the contents of a 
       simulation. In this design,    iterator is synonymous with 
       enumeration. The distinction here is that an enumeration is an    
       immutable snapshot of the specified elements at that time.     In 
       general the steps for acquiring an iterator, a collection and then 
       traversing that collection    are as follows: Note that iterator 
       handles can be reused but must be closed when no longer needed.       
        1) Get an iterator handle.       2) Connect the iterator with a 
       collection       3) Get handles or strings for the collection 
       elements       4) Release the iterator     For example:        	long 
       enumDics = EnumOpen(PID);                                  // get 
       iterator 	int dicCount = EnumDictionaries(enumDics)                   
           // get collection of dictionaries 	while (EnumHasNext(enumDics))  
                                        // iterate through dictionaries 	{ 	 
        long dicHandle = EnumGetNextHandle(enumDics);                 // get 
       element handle 	  long enumVars = EnumOpen(PID);                      
                 // get another iterator 	  int varCount = 
       EnumDictionaryVariables(enumVars, dichandle);  // get collection of 
       variables 	  while (EnumHasNext(enumVars)) {                          
            // iterate through variables 	    char *varName = 
       EnumGetNextString(enumVars);                // get element string 	   
        // Do something good 	  } 	  EnumClose(enumVars);                    
                             // release iterator 	} 	EnumClose(enumDics);    
                                               // release iterator
  */
  /*============================================================================
     Documentation for: F2EnumOpen
    ============================================================================
       Returns an iterator handle.
  */
  long  FRAMES2API _F2EnumOpen(int _PID);
    #ifndef BUILD_DLL
      #define F2EnumOpen _F2EnumOpen
    #endif

  /*============================================================================
     Documentation for: F2EnumCount
    ============================================================================
       Returns number of elements in collection.
  */
  int  FRAMES2API _F2EnumCount(long _enumHandle);
    #ifndef BUILD_DLL
      #define F2EnumCount _F2EnumCount
    #endif

  /*============================================================================
     Documentation for: F2EnumHasNext
    ============================================================================
       Returns 1 if iteration has more elements.
  */
  int  FRAMES2API _F2EnumHasNext(long _enumHandle);
    #ifndef BUILD_DLL
      #define F2EnumHasNext _F2EnumHasNext
    #endif

  /*============================================================================
     Documentation for: F2EnumGetNextHandle
    ============================================================================
       Returns the current element handle and then advances the iterator.
  */
  long  FRAMES2API _F2EnumGetNextHandle(long _enumHandle);
    #ifndef BUILD_DLL
      #define F2EnumGetNextHandle _F2EnumGetNextHandle
    #endif

  /*============================================================================
     Documentation for: F2EnumGetNextString
    ============================================================================
       Returns the current element string and then advances the iterator.
  */
  int  FRAMES2API _F2EnumGetNextString(long _enumHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2EnumGetNextString _F2EnumGetNextString
    #endif

  /*============================================================================
     Documentation for: F2EnumGetCurrentHandle
    ============================================================================
       Returns the handle element at current position in the iterator.
  */
  long  FRAMES2API _F2EnumGetCurrentHandle(long _enumHandle);
    #ifndef BUILD_DLL
      #define F2EnumGetCurrentHandle _F2EnumGetCurrentHandle
    #endif

  /*============================================================================
     Documentation for: F2EnumGetCurrentString
    ============================================================================
       Returns the string element at current position in the iterator.
  */
  int  FRAMES2API _F2EnumGetCurrentString(long _enumHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2EnumGetCurrentString _F2EnumGetCurrentString
    #endif

  /*============================================================================
     Documentation for: F2EnumMoveFirst
    ============================================================================
       Resets iterator to first element.
  */
  void  FRAMES2API _F2EnumMoveFirst(long _enumHandle);
    #ifndef BUILD_DLL
      #define F2EnumMoveFirst _F2EnumMoveFirst
    #endif

  /*============================================================================
     Documentation for: F2EnumGetHandleAtIndex
    ============================================================================
       Returns the handle element at iterator index.
  */
  long  FRAMES2API _F2EnumGetHandleAtIndex(long _enumHandle,int _indexIndex);
    #ifndef BUILD_DLL
      #define F2EnumGetHandleAtIndex _F2EnumGetHandleAtIndex
    #endif

  /*============================================================================
     Documentation for: F2EnumGetStringAtIndex
    ============================================================================
       Returns the string element at iterator index.
  */
  int  FRAMES2API _F2EnumGetStringAtIndex(long _enumHandle,int _indexIndex,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2EnumGetStringAtIndex _F2EnumGetStringAtIndex
    #endif

  /*============================================================================
     Documentation for: F2EnumFindString
    ============================================================================
       Returns the iterator index given the string.
  */
  int  FRAMES2API _F2EnumFindString(long _enumHandle,char* _keyName);
    #ifndef BUILD_DLL
      #define F2EnumFindString _F2EnumFindString
    #endif

  /*============================================================================
     Documentation for: F2EnumFindHandle
    ============================================================================
       Returns the iterator index given the handle.
  */
  int  FRAMES2API _F2EnumFindHandle(long _enumHandle,long _handleHandle);
    #ifndef BUILD_DLL
      #define F2EnumFindHandle _F2EnumFindHandle
    #endif

  /*============================================================================
     Documentation for: F2EnumGetHandle
    ============================================================================
       
  */
  long  FRAMES2API _F2EnumGetHandle(long _enumHandle,char* _keyName);
    #ifndef BUILD_DLL
      #define F2EnumGetHandle _F2EnumGetHandle
    #endif

  /*============================================================================
     Documentation for: F2EnumGetString
    ============================================================================
       
  */
  int  FRAMES2API _F2EnumGetString(long _enumHandle,long _handleHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define F2EnumGetString _F2EnumGetString
    #endif

  /*============================================================================
     Documentation for: F2EnumClose
    ============================================================================
       Frees the iterator handle.
  */
  void  FRAMES2API _F2EnumClose(long _enumHandle);
    #ifndef BUILD_DLL
      #define F2EnumClose _F2EnumClose
    #endif

  /*============================================================================
     Documentation for: F2EnumMeasures
    ============================================================================
       Get enumeration of all measures.
  */
  int  FRAMES2API _F2EnumMeasures(long _enumHandle);
    #ifndef BUILD_DLL
      #define F2EnumMeasures _F2EnumMeasures
    #endif

  /*============================================================================
     Documentation for: F2EnumUnitsOfMeasure
    ============================================================================
       Get enumeration of all units for a specified measure.
  */
  int  FRAMES2API _F2EnumUnitsOfMeasure(long _enumHandle,long _measureHandle);
    #ifndef BUILD_DLL
      #define F2EnumUnitsOfMeasure _F2EnumUnitsOfMeasure
    #endif

  /*============================================================================
     Documentation for: F2EnumDictionaries
    ============================================================================
       Get enumeration of all dictionaries.
  */
  int  FRAMES2API _F2EnumDictionaries(long _enumHandle);
    #ifndef BUILD_DLL
      #define F2EnumDictionaries _F2EnumDictionaries
    #endif

  /*============================================================================
     Documentation for: F2EnumDictionaryVariables
    ============================================================================
       Get enumeration of all variables for a specified dictionary.
  */
  int  FRAMES2API _F2EnumDictionaryVariables(long _enumHandle,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define F2EnumDictionaryVariables _F2EnumDictionaryVariables
    #endif

  /*============================================================================
     Documentation for: F2EnumVariableIndices
    ============================================================================
       Get enumeration of all indices for a specified variable.
  */
  int  FRAMES2API _F2EnumVariableIndices(long _enumHandle,long _dictionaryHandle,long _variableHandle);
    #ifndef BUILD_DLL
      #define F2EnumVariableIndices _F2EnumVariableIndices
    #endif

  /*============================================================================
     Documentation for: F2EnumModuleVariables
    ============================================================================
       Get enumeration of all variables for a specified module
  */
  int  FRAMES2API _F2EnumModuleVariables(long _enumHandle,long _moduleHandle);
    #ifndef BUILD_DLL
      #define F2EnumModuleVariables _F2EnumModuleVariables
    #endif

  /*============================================================================
     Documentation for: F2EnumModuleSchemes
    ============================================================================
       Get enumeration of all schemes for a specified module.
  */
  int  FRAMES2API _F2EnumModuleSchemes(long _enumHandle,long _moduleHandle);
    #ifndef BUILD_DLL
      #define F2EnumModuleSchemes _F2EnumModuleSchemes
    #endif

  /*============================================================================
     Documentation for: F2EnumSchemeInputDictionaries
    ============================================================================
       Get enumeration of all input dictionaries for a specified scheme.
  */
  int  FRAMES2API _F2EnumSchemeInputDictionaries(long _enumHandle,long _schemeHandle);
    #ifndef BUILD_DLL
      #define F2EnumSchemeInputDictionaries _F2EnumSchemeInputDictionaries
    #endif

  /*============================================================================
     Documentation for: F2EnumSchemeOutputDictionaries
    ============================================================================
       Get enumeration of all output dictionaries for a specified scheme.
  */
  int  FRAMES2API _F2EnumSchemeOutputDictionaries(long _enumHandle,long _schemeHandle);
    #ifndef BUILD_DLL
      #define F2EnumSchemeOutputDictionaries _F2EnumSchemeOutputDictionaries
    #endif

  /*============================================================================
     Documentation for: F2EnumDomains
    ============================================================================
       Get enumeration of all domains.
  */
  int  FRAMES2API _F2EnumDomains(long _enumHandle);
    #ifndef BUILD_DLL
      #define F2EnumDomains _F2EnumDomains
    #endif

  /*============================================================================
     Documentation for: F2EnumClasses
    ============================================================================
       Get enumeration of all classes for a specified domain.
  */
  int  FRAMES2API _F2EnumClasses(long _enumHandle,long _domainHandle);
    #ifndef BUILD_DLL
      #define F2EnumClasses _F2EnumClasses
    #endif

  /*============================================================================
     Documentation for: F2EnumGroups
    ============================================================================
       Get enumeration of all groups for a specified classes.
  */
  int  FRAMES2API _F2EnumGroups(long _enumHandle,long _domainHandle,long _classHandle);
    #ifndef BUILD_DLL
      #define F2EnumGroups _F2EnumGroups
    #endif

  /*============================================================================
     Documentation for: F2EnumSubGroups
    ============================================================================
       Get enumeration of all subgroups for a specified group.
  */
  int  FRAMES2API _F2EnumSubGroups(long _enumHandle,long _domainHandle,long _classHandle,long _groupHandle);
    #ifndef BUILD_DLL
      #define F2EnumSubGroups _F2EnumSubGroups
    #endif

  /*============================================================================
     Documentation for: EnumDomainDictionaries
    ============================================================================
       Get enumeration of all dictionaries for a specified domain.
  */
  int  FRAMES2API _EnumDomainDictionaries(long _enumHandle,long _domainHandle);
    #ifndef BUILD_DLL
      #define EnumDomainDictionaries _EnumDomainDictionaries
    #endif

  /*============================================================================
     Documentation for: EnumDomainDictionariesByPriv
    ============================================================================
       Get enumeration of all dictionaries for a specified domain.
  */
  int  FRAMES2API _EnumDomainDictionariesByPriv(long _enumHandle,long _domainHandle,int _privilege);
    #ifndef BUILD_DLL
      #define EnumDomainDictionariesByPriv _EnumDomainDictionariesByPriv
    #endif

  /*============================================================================
     Documentation for: EnumModules
    ============================================================================
       Get enumeration of all modules.
  */
  int  FRAMES2API _EnumModules(long _enumHandle);
    #ifndef BUILD_DLL
      #define EnumModules _EnumModules
    #endif

  /*============================================================================
     Documentation for: EnumClassModules
    ============================================================================
       Get enumeration of all class modules.
  */
  int  FRAMES2API _EnumClassModules(long _enumHandle,char* _className);
    #ifndef BUILD_DLL
      #define EnumClassModules _EnumClassModules
    #endif

  /*============================================================================
     Documentation for: F2EnumDomainModules
    ============================================================================
       Get enumeration of all modules for a specified domain and class.
  */
  int  FRAMES2API _F2EnumDomainModules(long _enumHandle,long _domainHandle,char* _className);
    #ifndef BUILD_DLL
      #define F2EnumDomainModules _F2EnumDomainModules
    #endif

  /*============================================================================
     Documentation for: F2EnumGroupModules
    ============================================================================
       Get enumeration of all modules for a specified domain, class and 
       group.
  */
  int  FRAMES2API _F2EnumGroupModules(long _enumHandle,long _domainHandle,long _classHandle,long _groupHandle);
    #ifndef BUILD_DLL
      #define F2EnumGroupModules _F2EnumGroupModules
    #endif

  /*============================================================================
     Documentation for: F2EnumSubGroupModules
    ============================================================================
       Get enumeration of all modules for a specified domain, class, group 
       and subgroup.
  */
  int  FRAMES2API _F2EnumSubGroupModules(long _enumHandle,long _domainHandle,long _classHandle,long _groupHandle,long _subgroupHandle);
    #ifndef BUILD_DLL
      #define F2EnumSubGroupModules _F2EnumSubGroupModules
    #endif

  /*============================================================================
     Documentation for: F2EnumDataSets
    ============================================================================
       Get enumeration of datasets, for the pid->simualtion if exists, else 
       environment.
  */
  int  FRAMES2API _F2EnumDataSets(long _enumHandle);
    #ifndef BUILD_DLL
      #define F2EnumDataSets _F2EnumDataSets
    #endif

  /*============================================================================
     Documentation for: F2EnumInputIcons
    ============================================================================
       Get enumeration of all input icons for the specified icon.
  */
  int  FRAMES2API _F2EnumInputIcons(long _enumHandle,long _toIconHandle);
    #ifndef BUILD_DLL
      #define F2EnumInputIcons _F2EnumInputIcons
    #endif

  /*============================================================================
     Documentation for: F2EnumOutputIcons
    ============================================================================
       Get enumeration of all output icons for the specified icon.
  */
  int  FRAMES2API _F2EnumOutputIcons(long _enumHandle,long _fromIconHandle);
    #ifndef BUILD_DLL
      #define F2EnumOutputIcons _F2EnumOutputIcons
    #endif

  /*============================================================================
     Documentation for: F2EnumInputIconDataSets
    ============================================================================
       Get enumeration of all datasets consumed from a input icon.
  */
  int  FRAMES2API _F2EnumInputIconDataSets(long _enumHandle,long _fromIconHandle,long _toIconHandle);
    #ifndef BUILD_DLL
      #define F2EnumInputIconDataSets _F2EnumInputIconDataSets
    #endif

  /*============================================================================
     Documentation for: F2EnumOutputIconDataSets
    ============================================================================
       Get enumeration of all datasets produced for a output icon.
  */
  int  FRAMES2API _F2EnumOutputIconDataSets(long _enumHandle,long _fromIconHandle,long _toIconHandle);
    #ifndef BUILD_DLL
      #define F2EnumOutputIconDataSets _F2EnumOutputIconDataSets
    #endif

  /*============================================================================
     Documentation for: F2EnumInputDataSets
    ============================================================================
       Get enumeration of all datasets consumed by an icon.
  */
  int  FRAMES2API _F2EnumInputDataSets(long _enumHandle,long _iconHandle);
    #ifndef BUILD_DLL
      #define F2EnumInputDataSets _F2EnumInputDataSets
    #endif

  /*============================================================================
     Documentation for: F2EnumOutputDataSets
    ============================================================================
       Get enumeration of all datasets produced by an icon.
  */
  int  FRAMES2API _F2EnumOutputDataSets(long _enumHandle,long _iconHandle);
    #ifndef BUILD_DLL
      #define F2EnumOutputDataSets _F2EnumOutputDataSets
    #endif

  /*============================================================================
     Documentation for: F2EnumInputDictionaryDataSets
    ============================================================================
       Get enumeration of datasets consumed by icon for specified dictionary.
  */
  int  FRAMES2API _F2EnumInputDictionaryDataSets(long _enumHandle,long _iconHandle,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define F2EnumInputDictionaryDataSets _F2EnumInputDictionaryDataSets
    #endif

  /*============================================================================
     Documentation for: F2EnumOutputDictionaryDataSets
    ============================================================================
       Get enumeration of datasets produced by output icon for specified 
       dictionary.
  */
  int  FRAMES2API _F2EnumOutputDictionaryDataSets(long _enumHandle,long _iconHandle,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define F2EnumOutputDictionaryDataSets _F2EnumOutputDictionaryDataSets
    #endif

  /*============================================================================
     Documentation for: F2EnumInputIconDictionaries
    ============================================================================
       Get enumeration of datasets consumed from input icon for specified 
       dictionary.
  */
  int  FRAMES2API _F2EnumInputIconDictionaries(long _enumHandle,long _fromIconHandle,long _toIconHandle);
    #ifndef BUILD_DLL
      #define F2EnumInputIconDictionaries _F2EnumInputIconDictionaries
    #endif

  /*============================================================================
     Documentation for: F2EnumOutputIconDictionaries
    ============================================================================
       Get enumeration of datasets produced for output icon for specified 
       dictionary.
  */
  int  FRAMES2API _F2EnumOutputIconDictionaries(long _enumHandle,long _fromIconHandle,long _toIconHandle);
    #ifndef BUILD_DLL
      #define F2EnumOutputIconDictionaries _F2EnumOutputIconDictionaries
    #endif

  /*============================================================================
     Documentation for: EnumSimulations
    ============================================================================
       Get enumeration of applicable viewers.
  */
  int  FRAMES2API _EnumSimulations(long _enumHandle);
    #ifndef BUILD_DLL
      #define EnumSimulations _EnumSimulations
    #endif

  /*============================================================================
     Documentation for: EnumIcons
    ============================================================================
       Get enumeration of applicable viewers.
  */
  int  FRAMES2API _EnumIcons(long _enumHandle);
    #ifndef BUILD_DLL
      #define EnumIcons _EnumIcons
    #endif

  /*============================================================================
     Documentation for: EnumProcessErrors
    ============================================================================
       Get enumeration of process error list.
  */
  int  FRAMES2API _EnumProcessErrors(long _enumHandle);
    #ifndef BUILD_DLL
      #define EnumProcessErrors _EnumProcessErrors
    #endif

  /*============================================================================
     Documentation for: EnumProcessWarnings
    ============================================================================
       Get enumeration of process warning list.
  */
  int  FRAMES2API _EnumProcessWarnings(long _enumHandle);
    #ifndef BUILD_DLL
      #define EnumProcessWarnings _EnumProcessWarnings
    #endif

  /*============================================================================
     Documentation for: EnumDomainNodeModules
    ============================================================================
       Get enumeration of modules for specified node.
  */
  int  FRAMES2API _EnumDomainNodeModules(long _enumHandle,long _nodeHandle);
    #ifndef BUILD_DLL
      #define EnumDomainNodeModules _EnumDomainNodeModules
    #endif

  /*============================================================================
     Documentation for: EnumFileList
    ============================================================================
       
  */
  int  FRAMES2API _EnumFileList(long _enumHandle,char* _delimiter,char* _list);
    #ifndef BUILD_DLL
      #define EnumFileList _EnumFileList
    #endif

  /*============================================================================
     Documentation for: EnumOpen
    ============================================================================
       Returns an iterator handle.
  */
  long  FRAMES2API _EnumOpen(int _PID);
    #ifndef BUILD_DLL
      #define EnumOpen _EnumOpen
    #endif

  /*============================================================================
     Documentation for: EnumCount
    ============================================================================
       Returns number of elements in collection.
  */
  int  FRAMES2API _EnumCount(long _enumHandle);
    #ifndef BUILD_DLL
      #define EnumCount _EnumCount
    #endif

  /*============================================================================
     Documentation for: EnumHasNext
    ============================================================================
       Returns 1 if iteration has more elements.
  */
  int  FRAMES2API _EnumHasNext(long _enumHandle);
    #ifndef BUILD_DLL
      #define EnumHasNext _EnumHasNext
    #endif

  /*============================================================================
     Documentation for: EnumGetNextHandle
    ============================================================================
       Returns the handle of the next element in the collection.     
       Advances the iterator.
  */
  long  FRAMES2API _EnumGetNextHandle(long _enumHandle);
    #ifndef BUILD_DLL
      #define EnumGetNextHandle _EnumGetNextHandle
    #endif

  /*============================================================================
     Documentation for: EnumGetNextString
    ============================================================================
       Returns the handle of the next element in the collection.     
       Advances the iterator.
  */
  int  FRAMES2API _EnumGetNextString(long _enumHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define EnumGetNextString _EnumGetNextString
    #endif

  /*============================================================================
     Documentation for: EnumGetCurrentHandle
    ============================================================================
       Returns handle of element at current iterator position.
  */
  long  FRAMES2API _EnumGetCurrentHandle(long _enumHandle);
    #ifndef BUILD_DLL
      #define EnumGetCurrentHandle _EnumGetCurrentHandle
    #endif

  /*============================================================================
     Documentation for: EnumGetCurrentString
    ============================================================================
       Returns handle of element at current iterator position.
  */
  int  FRAMES2API _EnumGetCurrentString(long _enumHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define EnumGetCurrentString _EnumGetCurrentString
    #endif

  /*============================================================================
     Documentation for: EnumMoveFirst
    ============================================================================
       Resets iterator to first element of collection.
  */
  void  FRAMES2API _EnumMoveFirst(long _enumHandle);
    #ifndef BUILD_DLL
      #define EnumMoveFirst _EnumMoveFirst
    #endif

  /*============================================================================
     Documentation for: EnumGetHandleAtIndex
    ============================================================================
       Returns handle of element at 'elementIndex'.
  */
  long  FRAMES2API _EnumGetHandleAtIndex(long _enumHandle,int _indexIndex);
    #ifndef BUILD_DLL
      #define EnumGetHandleAtIndex _EnumGetHandleAtIndex
    #endif

  /*============================================================================
     Documentation for: EnumGetStringAtIndex
    ============================================================================
       Returns handle of element at 'elementIndex'.
  */
  int  FRAMES2API _EnumGetStringAtIndex(long _enumHandle,int _indexIndex,char* _retvalue);
    #ifndef BUILD_DLL
      #define EnumGetStringAtIndex _EnumGetStringAtIndex
    #endif

  /*============================================================================
     Documentation for: EnumFindString
    ============================================================================
       
  */
  int  FRAMES2API _EnumFindString(long _enumHandle,char* _keyName);
    #ifndef BUILD_DLL
      #define EnumFindString _EnumFindString
    #endif

  /*============================================================================
     Documentation for: EnumFindHandle
    ============================================================================
       
  */
  int  FRAMES2API _EnumFindHandle(long _enumHandle,long _handleHandle);
    #ifndef BUILD_DLL
      #define EnumFindHandle _EnumFindHandle
    #endif

  /*============================================================================
     Documentation for: EnumGetHandle
    ============================================================================
       
  */
  long  FRAMES2API _EnumGetHandle(long _enumHandle,char* _keyName);
    #ifndef BUILD_DLL
      #define EnumGetHandle _EnumGetHandle
    #endif

  /*============================================================================
     Documentation for: EnumGetString
    ============================================================================
       
  */
  int  FRAMES2API _EnumGetString(long _enumHandle,long _handleHandle,char* _retvalue);
    #ifndef BUILD_DLL
      #define EnumGetString _EnumGetString
    #endif

  /*============================================================================
     Documentation for: EnumClose
    ============================================================================
       Frees the iterator handle.
  */
  void  FRAMES2API _EnumClose(long _enumHandle);
    #ifndef BUILD_DLL
      #define EnumClose _EnumClose
    #endif

  /*============================================================================
     Documentation for: EnumMeasures
    ============================================================================
       Get enumeration of measures.
  */
  int  FRAMES2API _EnumMeasures(long _enumHandle);
    #ifndef BUILD_DLL
      #define EnumMeasures _EnumMeasures
    #endif

  /*============================================================================
     Documentation for: EnumUnitsOfMeasure
    ============================================================================
       Get enumeration of units for specified measure..
  */
  int  FRAMES2API _EnumUnitsOfMeasure(long _enumHandle,long _measureHandle);
    #ifndef BUILD_DLL
      #define EnumUnitsOfMeasure _EnumUnitsOfMeasure
    #endif

  /*============================================================================
     Documentation for: EnumDictionaries
    ============================================================================
       Get enumeration of dictionaries.
  */
  int  FRAMES2API _EnumDictionaries(long _enumHandle);
    #ifndef BUILD_DLL
      #define EnumDictionaries _EnumDictionaries
    #endif

  /*============================================================================
     Documentation for: EnumDictionariesByPrivilege
    ============================================================================
       Get enumeration of dictionaries.
  */
  int  FRAMES2API _EnumDictionariesByPrivilege(long _enumHandle,int _privilege);
    #ifndef BUILD_DLL
      #define EnumDictionariesByPrivilege _EnumDictionariesByPrivilege
    #endif

  /*============================================================================
     Documentation for: EnumDictionaryVariables
    ============================================================================
       Get enumeration variables for specified dictionary.
  */
  int  FRAMES2API _EnumDictionaryVariables(long _enumHandle,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define EnumDictionaryVariables _EnumDictionaryVariables
    #endif

  /*============================================================================
     Documentation for: EnumVariableIndices
    ============================================================================
       Get enumeration variables for specified dictionary.
  */
  int  FRAMES2API _EnumVariableIndices(long _enumHandle,long _dictionaryHandle,long _variableHandle);
    #ifndef BUILD_DLL
      #define EnumVariableIndices _EnumVariableIndices
    #endif

  /*============================================================================
     Documentation for: EnumModuleVariables
    ============================================================================
       Get enumeration variables for specified module
  */
  int  FRAMES2API _EnumModuleVariables(long _enumHandle,long _moduleHandle);
    #ifndef BUILD_DLL
      #define EnumModuleVariables _EnumModuleVariables
    #endif

  /*============================================================================
     Documentation for: EnumModuleDictionaries
    ============================================================================
       Get enumeration of module ui dictionarioes.
  */
  int  FRAMES2API _EnumModuleDictionaries(long _enumHandle,long _moduleHandle);
    #ifndef BUILD_DLL
      #define EnumModuleDictionaries _EnumModuleDictionaries
    #endif

  /*============================================================================
     Documentation for: EnumModuleSchemes
    ============================================================================
       Get enumeration of schemes for specified module.
  */
  int  FRAMES2API _EnumModuleSchemes(long _enumHandle,long _moduleHandle);
    #ifndef BUILD_DLL
      #define EnumModuleSchemes _EnumModuleSchemes
    #endif

  /*============================================================================
     Documentation for: EnumSchemeInputDictionaries
    ============================================================================
       Get enumeration of input dictionaries for specified scheme.
  */
  int  FRAMES2API _EnumSchemeInputDictionaries(long _enumHandle,long _schemeHandle);
    #ifndef BUILD_DLL
      #define EnumSchemeInputDictionaries _EnumSchemeInputDictionaries
    #endif

  /*============================================================================
     Documentation for: EnumSchemeOutputDictionaries
    ============================================================================
       Get enumeration of output dictionaries for specified scheme.
  */
  int  FRAMES2API _EnumSchemeOutputDictionaries(long _enumHandle,long _schemeHandle);
    #ifndef BUILD_DLL
      #define EnumSchemeOutputDictionaries _EnumSchemeOutputDictionaries
    #endif

  /*============================================================================
     Documentation for: EnumDomains
    ============================================================================
       Get enumeration of domains.
  */
  int  FRAMES2API _EnumDomains(long _enumHandle);
    #ifndef BUILD_DLL
      #define EnumDomains _EnumDomains
    #endif

  /*============================================================================
     Documentation for: EnumClasses
    ============================================================================
       Get enumeration of classes for specified domain.
  */
  int  FRAMES2API _EnumClasses(long _enumHandle,long _domainHandle);
    #ifndef BUILD_DLL
      #define EnumClasses _EnumClasses
    #endif

  /*============================================================================
     Documentation for: EnumGroups
    ============================================================================
       Get enumeration of groups for specified classes.
  */
  int  FRAMES2API _EnumGroups(long _enumHandle,long _domainHandle,long _classHandle);
    #ifndef BUILD_DLL
      #define EnumGroups _EnumGroups
    #endif

  /*============================================================================
     Documentation for: EnumSubGroups
    ============================================================================
       Get enumeration of subgroups for specified group.
  */
  int  FRAMES2API _EnumSubGroups(long _enumHandle,long _domainHandle,long _classHandle,long _groupHandle);
    #ifndef BUILD_DLL
      #define EnumSubGroups _EnumSubGroups
    #endif

  /*============================================================================
     Documentation for: EnumDomainModules
    ============================================================================
       Get enumeration of modules for specified class. The class filter 
       (e.g. system, database, model, viewer). Pass empty class name to 
       retrieve all modules.
  */
  int  FRAMES2API _EnumDomainModules(long _enumHandle,long _domainHandle,char* _className);
    #ifndef BUILD_DLL
      #define EnumDomainModules _EnumDomainModules
    #endif

  /*============================================================================
     Documentation for: EnumGroupModules
    ============================================================================
       Get enumeration of modules for specified group.
  */
  int  FRAMES2API _EnumGroupModules(long _enumHandle,long _domainHandle,long _classHandle,long _groupHandle);
    #ifndef BUILD_DLL
      #define EnumGroupModules _EnumGroupModules
    #endif

  /*============================================================================
     Documentation for: EnumSubGroupModules
    ============================================================================
       Get enumeration of modules for specified subgroup.
  */
  int  FRAMES2API _EnumSubGroupModules(long _enumHandle,long _domainHandle,long _classHandle,long _groupHandle,long _subgroupHandle);
    #ifndef BUILD_DLL
      #define EnumSubGroupModules _EnumSubGroupModules
    #endif

  /*============================================================================
     Documentation for: EnumDataSets
    ============================================================================
       Get enumeration of datasets, for the pid->simualtion if exists, else 
       environment.
  */
  int  FRAMES2API _EnumDataSets(long _enumHandle);
    #ifndef BUILD_DLL
      #define EnumDataSets _EnumDataSets
    #endif

  /*============================================================================
     Documentation for: EnumInputIcons
    ============================================================================
       Get enumeration of input icons for specified icon.
  */
  int  FRAMES2API _EnumInputIcons(long _enumHandle,long _toIconHandle);
    #ifndef BUILD_DLL
      #define EnumInputIcons _EnumInputIcons
    #endif

  /*============================================================================
     Documentation for: EnumOutputIcons
    ============================================================================
       Get enumeration of output icons for specified icon.
  */
  int  FRAMES2API _EnumOutputIcons(long _enumHandle,long _fromIconHandle);
    #ifndef BUILD_DLL
      #define EnumOutputIcons _EnumOutputIcons
    #endif

  /*============================================================================
     Documentation for: EnumInputIconDataSets
    ============================================================================
       Get enumeration of datasets consumed from input icon.
  */
  int  FRAMES2API _EnumInputIconDataSets(long _enumHandle,long _fromIconHandle,long _toIconHandle);
    #ifndef BUILD_DLL
      #define EnumInputIconDataSets _EnumInputIconDataSets
    #endif

  /*============================================================================
     Documentation for: EnumOutputIconDataSets
    ============================================================================
       Get enumeration of datasets produced for output icon.
  */
  int  FRAMES2API _EnumOutputIconDataSets(long _enumHandle,long _fromIconHandle,long _toIconHandle);
    #ifndef BUILD_DLL
      #define EnumOutputIconDataSets _EnumOutputIconDataSets
    #endif

  /*============================================================================
     Documentation for: EnumInputDataSets
    ============================================================================
       Get enumeration of datasets consumed by an icon.
  */
  int  FRAMES2API _EnumInputDataSets(long _enumHandle,long _iconHandle);
    #ifndef BUILD_DLL
      #define EnumInputDataSets _EnumInputDataSets
    #endif

  /*============================================================================
     Documentation for: EnumOutputDataSets
    ============================================================================
       Get enumeration of datasets produced by an icon.
  */
  int  FRAMES2API _EnumOutputDataSets(long _enumHandle,long _iconHandle);
    #ifndef BUILD_DLL
      #define EnumOutputDataSets _EnumOutputDataSets
    #endif

  /*============================================================================
     Documentation for: EnumInputDictionaryDataSets
    ============================================================================
       Get enumeration of datasets consumed by icon for specified dictionary.
  */
  int  FRAMES2API _EnumInputDictionaryDataSets(long _enumHandle,long _iconHandle,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define EnumInputDictionaryDataSets _EnumInputDictionaryDataSets
    #endif

  /*============================================================================
     Documentation for: EnumOutputDictionaryDataSets
    ============================================================================
       Get enumeration of datasets produced by output icon for specified 
       dictionary.
  */
  int  FRAMES2API _EnumOutputDictionaryDataSets(long _enumHandle,long _iconHandle,long _dictionaryHandle);
    #ifndef BUILD_DLL
      #define EnumOutputDictionaryDataSets _EnumOutputDictionaryDataSets
    #endif

  /*============================================================================
     Documentation for: EnumInputIconDictionaries
    ============================================================================
       Get enumeration of datasets consumed from input icon for specified 
       dictionary.
  */
  int  FRAMES2API _EnumInputIconDictionaries(long _enumHandle,long _fromIconHandle,long _toIconHandle);
    #ifndef BUILD_DLL
      #define EnumInputIconDictionaries _EnumInputIconDictionaries
    #endif

  /*============================================================================
     Documentation for: EnumOutputIconDictionaries
    ============================================================================
       Get enumeration of datasets produced for output icon for specified 
       dictionary.
  */
  int  FRAMES2API _EnumOutputIconDictionaries(long _enumHandle,long _fromIconHandle,long _toIconHandle);
    #ifndef BUILD_DLL
      #define EnumOutputIconDictionaries _EnumOutputIconDictionaries
    #endif

  /*============================================================================
     Documentation for: EnumApplicableModules
    ============================================================================
       Get enumeration of applicable modules.
  */
  int  FRAMES2API _EnumApplicableModules(long _enumHandle,long _iconHandle,char* _delimiter,char* _domainName,char* _groupName,char* _subgroupName);
    #ifndef BUILD_DLL
      #define EnumApplicableModules _EnumApplicableModules
    #endif

  /*============================================================================
     Documentation for: EnumApplicableViewers
    ============================================================================
       Get enumeration of applicable viewers.
  */
  int  FRAMES2API _EnumApplicableViewers(long _enumHandle,long _iconHandle,char* _delimiter,char* _domainName,char* _groupName,char* _subgroupName);
    #ifndef BUILD_DLL
      #define EnumApplicableViewers _EnumApplicableViewers
    #endif

  /*============================================================================
     Documentation for: EnumIconOutputDataSets
    ============================================================================
       Enumerate all the datasets produced by the indicated icon.
  */
  int  FRAMES2API _EnumIconOutputDataSets(long _enumHandle,long _iconHandle);
    #ifndef BUILD_DLL
      #define EnumIconOutputDataSets _EnumIconOutputDataSets
    #endif

  /*============================================================================
     Documentation for: EnumIconUIDataSets
    ============================================================================
       Enumerate all the datasets produced by the indicated icon.
  */
  int  FRAMES2API _EnumIconUIDataSets(long _enumHandle,long _iconHandle);
    #ifndef BUILD_DLL
      #define EnumIconUIDataSets _EnumIconUIDataSets
    #endif

#ifdef __cplusplus
  }
#endif
#endif
