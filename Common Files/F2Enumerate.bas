Attribute VB_Name = "F2EnumerateAPI"
Option Explicit
' API Name: FRAMES Enumerations API
'-------------------------------------------------------------------------
' Documentation for: F2EnumOpen
' Returns an iterator handle.
DECLARE Function F2EnumOpen LIB "systemio.dll" Alias "__F2EnumOpen@4" (  ByVal PID as long )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2EnumCount
' Returns number of elements in collection.
DECLARE Function F2EnumCount LIB "systemio.dll" Alias "__F2EnumCount@4" (  ByVal enumHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumHasNext
' Returns 1 if iteration has more elements.
DECLARE Function F2EnumHasNext LIB "systemio.dll" Alias "__F2EnumHasNext@4" (  ByVal enumHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumGetNextHandle
' Returns the current element handle and then advances the iterator.
DECLARE Function F2EnumGetNextHandle LIB "systemio.dll" Alias "__F2EnumGetNextHandle@4" (  ByVal enumHandle as long )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2EnumGetNextString
' Returns the current element string and then advances the iterator.
DECLARE Function DLLF2EnumGetNextString LIB "systemio.dll" Alias "__F2EnumGetNextString@8" (  ByVal enumHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2EnumGetCurrentHandle
' Returns the handle element at current position in the iterator.
DECLARE Function F2EnumGetCurrentHandle LIB "systemio.dll" Alias "__F2EnumGetCurrentHandle@4" (  ByVal enumHandle as long )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2EnumGetCurrentString
' Returns the string element at current position in the iterator.
DECLARE Function DLLF2EnumGetCurrentString LIB "systemio.dll" Alias "__F2EnumGetCurrentString@8" (  ByVal enumHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2EnumMoveFirst
' Resets iterator to first element.
DECLARE Sub F2EnumMoveFirst LIB "systemio.dll" Alias "__F2EnumMoveFirst@4" (  ByVal enumHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2EnumGetHandleAtIndex
' Returns the handle element at iterator index.
DECLARE Function F2EnumGetHandleAtIndex LIB "systemio.dll" Alias "__F2EnumGetHandleAtIndex@8" (  ByVal enumHandle as long,  ByVal indexIndex as long )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2EnumGetStringAtIndex
' Returns the string element at iterator index.
DECLARE Function DLLF2EnumGetStringAtIndex LIB "systemio.dll" Alias "__F2EnumGetStringAtIndex@12" (  ByVal enumHandle as long,  ByVal indexIndex as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2EnumFindString
' Returns the iterator index given the string.
DECLARE Function F2EnumFindString LIB "systemio.dll" Alias "__F2EnumFindString@8" (  ByVal enumHandle as long,  ByVal keyName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumFindHandle
' Returns the iterator index given the handle.
DECLARE Function F2EnumFindHandle LIB "systemio.dll" Alias "__F2EnumFindHandle@8" (  ByVal enumHandle as long,  ByVal handleHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumGetHandle
' 
DECLARE Function F2EnumGetHandle LIB "systemio.dll" Alias "__F2EnumGetHandle@8" (  ByVal enumHandle as long,  ByVal keyName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2EnumGetString
' 
DECLARE Function DLLF2EnumGetString LIB "systemio.dll" Alias "__F2EnumGetString@12" (  ByVal enumHandle as long,  ByVal handleHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: F2EnumClose
' Frees the iterator handle.
DECLARE Sub F2EnumClose LIB "systemio.dll" Alias "__F2EnumClose@4" (  ByVal enumHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: F2EnumMeasures
' Get enumeration of all measures.
DECLARE Function F2EnumMeasures LIB "systemio.dll" Alias "__F2EnumMeasures@4" (  ByVal enumHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumUnitsOfMeasure
' Get enumeration of all units for a specified measure.
DECLARE Function F2EnumUnitsOfMeasure LIB "systemio.dll" Alias "__F2EnumUnitsOfMeasure@8" (  ByVal enumHandle as long,  ByVal measureHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumDictionaries
' Get enumeration of all dictionaries.
DECLARE Function F2EnumDictionaries LIB "systemio.dll" Alias "__F2EnumDictionaries@4" (  ByVal enumHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumDictionaryVariables
' Get enumeration of all variables for a specified dictionary.
DECLARE Function F2EnumDictionaryVariables LIB "systemio.dll" Alias "__F2EnumDictionaryVariables@8" (  ByVal enumHandle as long,  ByVal dictionaryHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumVariableIndices
' Get enumeration of all indices for a specified variable.
DECLARE Function F2EnumVariableIndices LIB "systemio.dll" Alias "__F2EnumVariableIndices@12" (  ByVal enumHandle as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumModuleVariables
' Get enumeration of all variables for a specified module
DECLARE Function F2EnumModuleVariables LIB "systemio.dll" Alias "__F2EnumModuleVariables@8" (  ByVal enumHandle as long,  ByVal moduleHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumModuleSchemes
' Get enumeration of all schemes for a specified module.
DECLARE Function F2EnumModuleSchemes LIB "systemio.dll" Alias "__F2EnumModuleSchemes@8" (  ByVal enumHandle as long,  ByVal moduleHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumSchemeInputDictionaries
' Get enumeration of all input dictionaries for a specified scheme.
DECLARE Function F2EnumSchemeInputDictionaries LIB "systemio.dll" Alias "__F2EnumSchemeInputDictionaries@8" (  ByVal enumHandle as long,  ByVal schemeHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumSchemeOutputDictionaries
' Get enumeration of all output dictionaries for a specified scheme.
DECLARE Function F2EnumSchemeOutputDictionaries LIB "systemio.dll" Alias "__F2EnumSchemeOutputDictionaries@8" (  ByVal enumHandle as long,  ByVal schemeHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumDomains
' Get enumeration of all domains.
DECLARE Function F2EnumDomains LIB "systemio.dll" Alias "__F2EnumDomains@4" (  ByVal enumHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumClasses
' Get enumeration of all classes for a specified domain.
DECLARE Function F2EnumClasses LIB "systemio.dll" Alias "__F2EnumClasses@8" (  ByVal enumHandle as long,  ByVal domainHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumGroups
' Get enumeration of all groups for a specified classes.
DECLARE Function F2EnumGroups LIB "systemio.dll" Alias "__F2EnumGroups@12" (  ByVal enumHandle as long,  ByVal domainHandle as long,  ByVal classHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumSubGroups
' Get enumeration of all subgroups for a specified group.
DECLARE Function F2EnumSubGroups LIB "systemio.dll" Alias "__F2EnumSubGroups@16" (  ByVal enumHandle as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumDomainDictionaries
' Get enumeration of all dictionaries for a specified domain.
DECLARE Function EnumDomainDictionaries LIB "systemio.dll" Alias "__EnumDomainDictionaries@8" (  ByVal enumHandle as long,  ByVal domainHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumDomainDictionariesByPriv
' Get enumeration of all dictionaries for a specified domain.
DECLARE Function EnumDomainDictionariesByPriv LIB "systemio.dll" Alias "__EnumDomainDictionariesByPriv@12" (  ByVal enumHandle as long,  ByVal domainHandle as long,  ByVal privilege as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumModules
' Get enumeration of all modules.
DECLARE Function EnumModules LIB "systemio.dll" Alias "__EnumModules@4" (  ByVal enumHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumClassModules
' Get enumeration of all class modules.
DECLARE Function EnumClassModules LIB "systemio.dll" Alias "__EnumClassModules@8" (  ByVal enumHandle as long,  ByVal className as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumDomainModules
' Get enumeration of all modules for a specified domain and class.
DECLARE Function F2EnumDomainModules LIB "systemio.dll" Alias "__F2EnumDomainModules@12" (  ByVal enumHandle as long,  ByVal domainHandle as long,  ByVal className as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumGroupModules
' Get enumeration of all modules for a specified domain, class and group.
DECLARE Function F2EnumGroupModules LIB "systemio.dll" Alias "__F2EnumGroupModules@16" (  ByVal enumHandle as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumSubGroupModules
' Get enumeration of all modules for a specified domain, class, group and subgroup.
DECLARE Function F2EnumSubGroupModules LIB "systemio.dll" Alias "__F2EnumSubGroupModules@20" (  ByVal enumHandle as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumDataSets
' Get enumeration of datasets, for the pid->simualtion if exists, else environment.
DECLARE Function F2EnumDataSets LIB "systemio.dll" Alias "__F2EnumDataSets@4" (  ByVal enumHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumInputIcons
' Get enumeration of all input icons for the specified icon.
DECLARE Function F2EnumInputIcons LIB "systemio.dll" Alias "__F2EnumInputIcons@8" (  ByVal enumHandle as long,  ByVal toIconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumOutputIcons
' Get enumeration of all output icons for the specified icon.
DECLARE Function F2EnumOutputIcons LIB "systemio.dll" Alias "__F2EnumOutputIcons@8" (  ByVal enumHandle as long,  ByVal fromIconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumInputIconDataSets
' Get enumeration of all datasets consumed from a input icon.
DECLARE Function F2EnumInputIconDataSets LIB "systemio.dll" Alias "__F2EnumInputIconDataSets@12" (  ByVal enumHandle as long,  ByVal fromIconHandle as long,  ByVal toIconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumOutputIconDataSets
' Get enumeration of all datasets produced for a output icon.
DECLARE Function F2EnumOutputIconDataSets LIB "systemio.dll" Alias "__F2EnumOutputIconDataSets@12" (  ByVal enumHandle as long,  ByVal fromIconHandle as long,  ByVal toIconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumInputDataSets
' Get enumeration of all datasets consumed by an icon.
DECLARE Function F2EnumInputDataSets LIB "systemio.dll" Alias "__F2EnumInputDataSets@8" (  ByVal enumHandle as long,  ByVal iconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumOutputDataSets
' Get enumeration of all datasets produced by an icon.
DECLARE Function F2EnumOutputDataSets LIB "systemio.dll" Alias "__F2EnumOutputDataSets@8" (  ByVal enumHandle as long,  ByVal iconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumInputDictionaryDataSets
' Get enumeration of datasets consumed by icon for specified dictionary.
DECLARE Function F2EnumInputDictionaryDataSets LIB "systemio.dll" Alias "__F2EnumInputDictionaryDataSets@12" (  ByVal enumHandle as long,  ByVal iconHandle as long,  ByVal dictionaryHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumOutputDictionaryDataSets
' Get enumeration of datasets produced by output icon for specified dictionary.
DECLARE Function F2EnumOutputDictionaryDataSets LIB "systemio.dll" Alias "__F2EnumOutputDictionaryDataSets@12" (  ByVal enumHandle as long,  ByVal iconHandle as long,  ByVal dictionaryHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumInputIconDictionaries
' Get enumeration of datasets consumed from input icon for specified dictionary.
DECLARE Function F2EnumInputIconDictionaries LIB "systemio.dll" Alias "__F2EnumInputIconDictionaries@12" (  ByVal enumHandle as long,  ByVal fromIconHandle as long,  ByVal toIconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: F2EnumOutputIconDictionaries
' Get enumeration of datasets produced for output icon for specified dictionary.
DECLARE Function F2EnumOutputIconDictionaries LIB "systemio.dll" Alias "__F2EnumOutputIconDictionaries@12" (  ByVal enumHandle as long,  ByVal fromIconHandle as long,  ByVal toIconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumSimulations
' Get enumeration of applicable viewers.
DECLARE Function EnumSimulations LIB "systemio.dll" Alias "__EnumSimulations@4" (  ByVal enumHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumIcons
' Get enumeration of applicable viewers.
DECLARE Function EnumIcons LIB "systemio.dll" Alias "__EnumIcons@4" (  ByVal enumHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumProcessErrors
' Get enumeration of process error list.
DECLARE Function EnumProcessErrors LIB "systemio.dll" Alias "__EnumProcessErrors@4" (  ByVal enumHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumProcessWarnings
' Get enumeration of process warning list.
DECLARE Function EnumProcessWarnings LIB "systemio.dll" Alias "__EnumProcessWarnings@4" (  ByVal enumHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumDomainNodeModules
' Get enumeration of modules for specified node.
DECLARE Function EnumDomainNodeModules LIB "systemio.dll" Alias "__EnumDomainNodeModules@8" (  ByVal enumHandle as long,  ByVal nodeHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumFileList
' 
DECLARE Function EnumFileList LIB "systemio.dll" Alias "__EnumFileList@12" (  ByVal enumHandle as long,  ByVal delimiter as string,  ByVal list as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumOpen
' Returns an iterator handle.
DECLARE Function EnumOpen LIB "systemio.dll" Alias "__EnumOpen@4" (  ByVal PID as long )  as long 

'-------------------------------------------------------------------------
' Documentation for: EnumCount
' Returns number of elements in collection.
DECLARE Function EnumCount LIB "systemio.dll" Alias "__EnumCount@4" (  ByVal enumHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumHasNext
' Returns 1 if iteration has more elements.
DECLARE Function EnumHasNext LIB "systemio.dll" Alias "__EnumHasNext@4" (  ByVal enumHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumGetNextHandle
' Returns the handle of the next element in the collection. Advances the iterator.
DECLARE Function EnumGetNextHandle LIB "systemio.dll" Alias "__EnumGetNextHandle@4" (  ByVal enumHandle as long )  as long 

'-------------------------------------------------------------------------
' Documentation for: EnumGetNextString
' Returns the handle of the next element in the collection. Advances the iterator.
DECLARE Function DLLEnumGetNextString LIB "systemio.dll" Alias "__EnumGetNextString@8" (  ByVal enumHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: EnumGetCurrentHandle
' Returns handle of element at current iterator position.
DECLARE Function EnumGetCurrentHandle LIB "systemio.dll" Alias "__EnumGetCurrentHandle@4" (  ByVal enumHandle as long )  as long 

'-------------------------------------------------------------------------
' Documentation for: EnumGetCurrentString
' Returns handle of element at current iterator position.
DECLARE Function DLLEnumGetCurrentString LIB "systemio.dll" Alias "__EnumGetCurrentString@8" (  ByVal enumHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: EnumMoveFirst
' Resets iterator to first element of collection.
DECLARE Sub EnumMoveFirst LIB "systemio.dll" Alias "__EnumMoveFirst@4" (  ByVal enumHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: EnumGetHandleAtIndex
' Returns handle of element at 'elementIndex'.
DECLARE Function EnumGetHandleAtIndex LIB "systemio.dll" Alias "__EnumGetHandleAtIndex@8" (  ByVal enumHandle as long,  ByVal indexIndex as long )  as long 

'-------------------------------------------------------------------------
' Documentation for: EnumGetStringAtIndex
' Returns handle of element at 'elementIndex'.
DECLARE Function DLLEnumGetStringAtIndex LIB "systemio.dll" Alias "__EnumGetStringAtIndex@12" (  ByVal enumHandle as long,  ByVal indexIndex as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: EnumFindString
' 
DECLARE Function EnumFindString LIB "systemio.dll" Alias "__EnumFindString@8" (  ByVal enumHandle as long,  ByVal keyName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumFindHandle
' 
DECLARE Function EnumFindHandle LIB "systemio.dll" Alias "__EnumFindHandle@8" (  ByVal enumHandle as long,  ByVal handleHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumGetHandle
' 
DECLARE Function EnumGetHandle LIB "systemio.dll" Alias "__EnumGetHandle@8" (  ByVal enumHandle as long,  ByVal keyName as string )  as long 

'-------------------------------------------------------------------------
' Documentation for: EnumGetString
' 
DECLARE Function DLLEnumGetString LIB "systemio.dll" Alias "__EnumGetString@12" (  ByVal enumHandle as long,  ByVal handleHandle as long, ByVal value as String )  as long 

'-------------------------------------------------------------------------
' Documentation for: EnumClose
' Frees the iterator handle.
DECLARE Sub EnumClose LIB "systemio.dll" Alias "__EnumClose@4" (  ByVal enumHandle as long ) 

'-------------------------------------------------------------------------
' Documentation for: EnumMeasures
' Get enumeration of measures.
DECLARE Function EnumMeasures LIB "systemio.dll" Alias "__EnumMeasures@4" (  ByVal enumHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumUnitsOfMeasure
' Get enumeration of units for specified measure..
DECLARE Function EnumUnitsOfMeasure LIB "systemio.dll" Alias "__EnumUnitsOfMeasure@8" (  ByVal enumHandle as long,  ByVal measureHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumDictionaries
' Get enumeration of dictionaries.
DECLARE Function EnumDictionaries LIB "systemio.dll" Alias "__EnumDictionaries@4" (  ByVal enumHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumDictionariesByPrivilege
' Get enumeration of dictionaries.
DECLARE Function EnumDictionariesByPrivilege LIB "systemio.dll" Alias "__EnumDictionariesByPrivilege@8" (  ByVal enumHandle as long,  ByVal privilege as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumDictionaryVariables
' Get enumeration variables for specified dictionary.
DECLARE Function EnumDictionaryVariables LIB "systemio.dll" Alias "__EnumDictionaryVariables@8" (  ByVal enumHandle as long,  ByVal dictionaryHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumVariableIndices
' Get enumeration variables for specified dictionary.
DECLARE Function EnumVariableIndices LIB "systemio.dll" Alias "__EnumVariableIndices@12" (  ByVal enumHandle as long,  ByVal dictionaryHandle as long,  ByVal variableHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumModuleVariables
' Get enumeration variables for specified module
DECLARE Function EnumModuleVariables LIB "systemio.dll" Alias "__EnumModuleVariables@8" (  ByVal enumHandle as long,  ByVal moduleHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumModuleDictionaries
' Get enumeration of module ui dictionarioes.
DECLARE Function EnumModuleDictionaries LIB "systemio.dll" Alias "__EnumModuleDictionaries@8" (  ByVal enumHandle as long,  ByVal moduleHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumModuleSchemes
' Get enumeration of schemes for specified module.
DECLARE Function EnumModuleSchemes LIB "systemio.dll" Alias "__EnumModuleSchemes@8" (  ByVal enumHandle as long,  ByVal moduleHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumSchemeInputDictionaries
' Get enumeration of input dictionaries for specified scheme.
DECLARE Function EnumSchemeInputDictionaries LIB "systemio.dll" Alias "__EnumSchemeInputDictionaries@8" (  ByVal enumHandle as long,  ByVal schemeHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumSchemeOutputDictionaries
' Get enumeration of output dictionaries for specified scheme.
DECLARE Function EnumSchemeOutputDictionaries LIB "systemio.dll" Alias "__EnumSchemeOutputDictionaries@8" (  ByVal enumHandle as long,  ByVal schemeHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumDomains
' Get enumeration of domains.
DECLARE Function EnumDomains LIB "systemio.dll" Alias "__EnumDomains@4" (  ByVal enumHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumClasses
' Get enumeration of classes for specified domain.
DECLARE Function EnumClasses LIB "systemio.dll" Alias "__EnumClasses@8" (  ByVal enumHandle as long,  ByVal domainHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumGroups
' Get enumeration of groups for specified classes.
DECLARE Function EnumGroups LIB "systemio.dll" Alias "__EnumGroups@12" (  ByVal enumHandle as long,  ByVal domainHandle as long,  ByVal classHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumSubGroups
' Get enumeration of subgroups for specified group.
DECLARE Function EnumSubGroups LIB "systemio.dll" Alias "__EnumSubGroups@16" (  ByVal enumHandle as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumDomainModules
' Get enumeration of modules for specified class. The class filter (e.g. system, database, model, viewer). Pass empty class name to retrieve all modules.
DECLARE Function EnumDomainModules LIB "systemio.dll" Alias "__EnumDomainModules@12" (  ByVal enumHandle as long,  ByVal domainHandle as long,  ByVal className as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumGroupModules
' Get enumeration of modules for specified group.
DECLARE Function EnumGroupModules LIB "systemio.dll" Alias "__EnumGroupModules@16" (  ByVal enumHandle as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumSubGroupModules
' Get enumeration of modules for specified subgroup.
DECLARE Function EnumSubGroupModules LIB "systemio.dll" Alias "__EnumSubGroupModules@20" (  ByVal enumHandle as long,  ByVal domainHandle as long,  ByVal classHandle as long,  ByVal groupHandle as long,  ByVal subgroupHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumDataSets
' Get enumeration of datasets, for the pid->simualtion if exists, else environment.
DECLARE Function EnumDataSets LIB "systemio.dll" Alias "__EnumDataSets@4" (  ByVal enumHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumInputIcons
' Get enumeration of input icons for specified icon.
DECLARE Function EnumInputIcons LIB "systemio.dll" Alias "__EnumInputIcons@8" (  ByVal enumHandle as long,  ByVal toIconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumOutputIcons
' Get enumeration of output icons for specified icon.
DECLARE Function EnumOutputIcons LIB "systemio.dll" Alias "__EnumOutputIcons@8" (  ByVal enumHandle as long,  ByVal fromIconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumInputIconDataSets
' Get enumeration of datasets consumed from input icon.
DECLARE Function EnumInputIconDataSets LIB "systemio.dll" Alias "__EnumInputIconDataSets@12" (  ByVal enumHandle as long,  ByVal fromIconHandle as long,  ByVal toIconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumOutputIconDataSets
' Get enumeration of datasets produced for output icon.
DECLARE Function EnumOutputIconDataSets LIB "systemio.dll" Alias "__EnumOutputIconDataSets@12" (  ByVal enumHandle as long,  ByVal fromIconHandle as long,  ByVal toIconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumInputDataSets
' Get enumeration of datasets consumed by an icon.
DECLARE Function EnumInputDataSets LIB "systemio.dll" Alias "__EnumInputDataSets@8" (  ByVal enumHandle as long,  ByVal iconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumOutputDataSets
' Get enumeration of datasets produced by an icon.
DECLARE Function EnumOutputDataSets LIB "systemio.dll" Alias "__EnumOutputDataSets@8" (  ByVal enumHandle as long,  ByVal iconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumInputDictionaryDataSets
' Get enumeration of datasets consumed by icon for specified dictionary.
DECLARE Function EnumInputDictionaryDataSets LIB "systemio.dll" Alias "__EnumInputDictionaryDataSets@12" (  ByVal enumHandle as long,  ByVal iconHandle as long,  ByVal dictionaryHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumOutputDictionaryDataSets
' Get enumeration of datasets produced by output icon for specified dictionary.
DECLARE Function EnumOutputDictionaryDataSets LIB "systemio.dll" Alias "__EnumOutputDictionaryDataSets@12" (  ByVal enumHandle as long,  ByVal iconHandle as long,  ByVal dictionaryHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumInputIconDictionaries
' Get enumeration of datasets consumed from input icon for specified dictionary.
DECLARE Function EnumInputIconDictionaries LIB "systemio.dll" Alias "__EnumInputIconDictionaries@12" (  ByVal enumHandle as long,  ByVal fromIconHandle as long,  ByVal toIconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumOutputIconDictionaries
' Get enumeration of datasets produced for output icon for specified dictionary.
DECLARE Function EnumOutputIconDictionaries LIB "systemio.dll" Alias "__EnumOutputIconDictionaries@12" (  ByVal enumHandle as long,  ByVal fromIconHandle as long,  ByVal toIconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumApplicableModules
' Get enumeration of applicable modules.
DECLARE Function EnumApplicableModules LIB "systemio.dll" Alias "__EnumApplicableModules@24" (  ByVal enumHandle as long,  ByVal iconHandle as long,  ByVal delimiter as string,  ByVal domainName as string,  ByVal groupName as string,  ByVal subgroupName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumApplicableViewers
' Get enumeration of applicable viewers.
DECLARE Function EnumApplicableViewers LIB "systemio.dll" Alias "__EnumApplicableViewers@24" (  ByVal enumHandle as long,  ByVal iconHandle as long,  ByVal delimiter as string,  ByVal domainName as string,  ByVal groupName as string,  ByVal subgroupName as string )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumIconOutputDataSets
' Enumerate all the datasets produced by the indicated icon.
DECLARE Function EnumIconOutputDataSets LIB "systemio.dll" Alias "__EnumIconOutputDataSets@8" (  ByVal enumHandle as long,  ByVal iconHandle as long )  as integer 

'-------------------------------------------------------------------------
' Documentation for: EnumIconUIDataSets
' Enumerate all the datasets produced by the indicated icon.
DECLARE Function EnumIconUIDataSets LIB "systemio.dll" Alias "__EnumIconUIDataSets@8" (  ByVal enumHandle as long,  ByVal iconHandle as long )  as integer 

Function F2EnumGetNextString (  ByVal enumHandle as long  )  as string 
      Dim retStr2 as String * MAXFIELD
      Dim value as long
      value = DLLF2EnumGetNextString(enumHandle, retStr2)
      F2EnumGetNextString=StripTerminator(retStr2)
End  Function 

Function F2EnumGetCurrentString (  ByVal enumHandle as long  )  as string 
      Dim retStr2 as String * MAXFIELD
      Dim value as long
      value = DLLF2EnumGetCurrentString(enumHandle, retStr2)
      F2EnumGetCurrentString=StripTerminator(retStr2)
End  Function 

Function F2EnumGetStringAtIndex (  ByVal enumHandle as long ,  ByVal indexIndex as long )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLF2EnumGetStringAtIndex(enumHandle, indexIndex, retStr3)
      F2EnumGetStringAtIndex=StripTerminator(retStr3)
End  Function 

Function F2EnumGetString (  ByVal enumHandle as long ,  ByVal handleHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLF2EnumGetString(enumHandle, handleHandle, retStr3)
      F2EnumGetString=StripTerminator(retStr3)
End  Function 

Function EnumGetNextString (  ByVal enumHandle as long  )  as string 
      Dim retStr2 as String * MAXFIELD
      Dim value as long
      value = DLLEnumGetNextString(enumHandle, retStr2)
      EnumGetNextString=StripTerminator(retStr2)
End  Function 

Function EnumGetCurrentString (  ByVal enumHandle as long  )  as string 
      Dim retStr2 as String * MAXFIELD
      Dim value as long
      value = DLLEnumGetCurrentString(enumHandle, retStr2)
      EnumGetCurrentString=StripTerminator(retStr2)
End  Function 

Function EnumGetStringAtIndex (  ByVal enumHandle as long ,  ByVal indexIndex as long )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLEnumGetStringAtIndex(enumHandle, indexIndex, retStr3)
      EnumGetStringAtIndex=StripTerminator(retStr3)
End  Function 

Function EnumGetString (  ByVal enumHandle as long ,  ByVal handleHandle as long  )  as string 
      Dim retStr3 as String * MAXFIELD
      Dim value as long
      value = DLLEnumGetString(enumHandle, handleHandle, retStr3)
      EnumGetString=StripTerminator(retStr3)
End  Function 
