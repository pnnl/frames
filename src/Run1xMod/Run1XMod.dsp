# Microsoft Developer Studio Project File - Name="Run1XMod" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=Run1XMod - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Run1XMod.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Run1XMod.mak" CFG="Run1XMod - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Run1XMod - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "Run1XMod - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""$/MEPAS", MICAAAAA"
# PROP Scc_LocalPath "."
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "Run1XMod - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir ""
# PROP Intermediate_Dir ""
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /Yu"stdafx.h" /FD /c
# ADD CPP /nologo /W3 /GX /O2 /I "..\Common Files" /I "..\Frames dll" /D "NDEBUG" /D "WIN32" /D "_CONSOLE" /D "_MBCS" /D "DIRECTACCESS" /FD /c
# SUBTRACT CPP /YX /Yc /Yu
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386

!ELSEIF  "$(CFG)" == "Run1XMod - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir ""
# PROP Intermediate_Dir ""
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /browser /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /Yu"stdafx.h" /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /I "..\Common Files" /I "..\Frames dll" /D "_DEBUG" /D "WIN32" /D "_CONSOLE" /D "_MBCS" /D "DIRECTACCESS" /FR /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no /debug /debugtype:both /machine:I386 /pdbtype:sept /verbose:lib
# SUBTRACT LINK32 /pdb:none

!ENDIF 

# Begin Target

# Name "Run1XMod - Win32 Release"
# Name "Run1XMod - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE="..\Frames DLL\affClass.cpp"
# End Source File
# Begin Source File

SOURCE=.\AFFv2.cpp
# End Source File
# Begin Source File

SOURCE="..\Frames DLL\atoClass.cpp"
# End Source File
# Begin Source File

SOURCE=.\ATOv2.cpp
# End Source File
# Begin Source File

SOURCE=.\CONv2.cpp
# End Source File
# Begin Source File

SOURCE="..\Common Files\csv.cpp"
# End Source File
# Begin Source File

SOURCE=.\EPFv2.cpp
# End Source File
# Begin Source File

SOURCE="..\Common Files\fcsv.cpp"
# End Source File
# Begin Source File

SOURCE="..\Common Files\gid.cpp"
# End Source File
# Begin Source File

SOURCE=.\glyph.cpp
# End Source File
# Begin Source File

SOURCE=.\HIFv2.cpp
# End Source File
# Begin Source File

SOURCE=.\OSv2.cpp
# End Source File
# Begin Source File

SOURCE=.\Param.cpp
# End Source File
# Begin Source File

SOURCE=.\pugixml.cpp
# End Source File
# Begin Source File

SOURCE=.\RIFv2.cpp
# End Source File
# Begin Source File

SOURCE="..\Common Files\robust.cpp"
# End Source File
# Begin Source File

SOURCE=.\Run1XMod.cpp
# End Source File
# Begin Source File

SOURCE="..\Frames DLL\scfClass.cpp"
# End Source File
# Begin Source File

SOURCE=.\SCFv2.cpp
# End Source File
# Begin Source File

SOURCE="..\Common Files\Series.cpp"
# End Source File
# Begin Source File

SOURCE="..\Common Files\SeriesV2.cpp"
# End Source File
# Begin Source File

SOURCE="..\Common Files\StringParser.cpp"
# End Source File
# Begin Source File

SOURCE="..\Common Files\Test.cpp"
# End Source File
# Begin Source File

SOURCE="..\Frames DLL\wcfClass.cpp"
# End Source File
# Begin Source File

SOURCE=.\WCFv2.cpp
# End Source File
# Begin Source File

SOURCE="..\Frames DLL\wffClass.cpp"
# End Source File
# Begin Source File

SOURCE=.\WFFv2.cpp
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# Begin Source File

SOURCE="..\Frames DLL\affClass.h"
# End Source File
# Begin Source File

SOURCE=.\AFFv2.h
# End Source File
# Begin Source File

SOURCE="..\Frames DLL\atoClass.h"
# End Source File
# Begin Source File

SOURCE=.\ATOv2.h
# End Source File
# Begin Source File

SOURCE=.\CONv2.h
# End Source File
# Begin Source File

SOURCE="..\Common Files\ConversionC.h"
# End Source File
# Begin Source File

SOURCE="..\Common Files\csv.h"
# End Source File
# Begin Source File

SOURCE="..\Common Files\DataSetC.h"
# End Source File
# Begin Source File

SOURCE=.\EPFv2.h
# End Source File
# Begin Source File

SOURCE="..\Common Files\ErrorC.h"
# End Source File
# Begin Source File

SOURCE="..\Common Files\F2DataSetC.h"
# End Source File
# Begin Source File

SOURCE="..\Common Files\F2EnumerateC.h"
# End Source File
# Begin Source File

SOURCE="..\Common Files\F2ErrorC.h"
# End Source File
# Begin Source File

SOURCE="..\Common Files\F2ModuleDevC.h"
# End Source File
# Begin Source File

SOURCE="..\Common Files\F2SystemDevC.h"
# End Source File
# Begin Source File

SOURCE="..\Common Files\fcsv.h"
# End Source File
# Begin Source File

SOURCE="..\Common Files\gid.h"
# End Source File
# Begin Source File

SOURCE=.\Glyph.h
# End Source File
# Begin Source File

SOURCE=.\HIFv2.h
# End Source File
# Begin Source File

SOURCE=.\OSv2.h
# End Source File
# Begin Source File

SOURCE=.\Param.h
# End Source File
# Begin Source File

SOURCE=.\RIFv2.h
# End Source File
# Begin Source File

SOURCE="..\Common Files\robust.h"
# End Source File
# Begin Source File

SOURCE="..\Frames DLL\scfClass.h"
# End Source File
# Begin Source File

SOURCE=.\SCFv2.h
# End Source File
# Begin Source File

SOURCE="..\Common Files\Series.h"
# End Source File
# Begin Source File

SOURCE="..\Common Files\SeriesV2.h"
# End Source File
# Begin Source File

SOURCE="..\Common Files\StringParser.h"
# End Source File
# Begin Source File

SOURCE="..\Common Files\Test.h"
# End Source File
# Begin Source File

SOURCE="..\Common Files\typedefs.h"
# End Source File
# Begin Source File

SOURCE="..\Frames DLL\wcfClass.h"
# End Source File
# Begin Source File

SOURCE=.\WCFv2.h
# End Source File
# Begin Source File

SOURCE="..\Frames DLL\wffClass.h"
# End Source File
# Begin Source File

SOURCE=.\WFFv2.h
# End Source File
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# Begin Source File

SOURCE=.\ReadMe.txt
# End Source File
# Begin Source File

SOURCE="..\Common Files\SystemIO.lib"
# End Source File
# End Target
# End Project
