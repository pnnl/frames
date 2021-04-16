# Microsoft Developer Studio Project File - Name="hourly" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=hourly - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "hourly.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "hourly.mak" CFG="hourly - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "hourly - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "hourly - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "hourly - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Target_Dir ""
# ADD BASE F90 /include:"Release/" /compile_only /nologo
# ADD F90 /include:"Release/" /compile_only /nologo
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "hourly - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Target_Dir ""
# ADD BASE F90 /include:"Debug/" /compile_only /nologo /debug:full /optimize:0
# ADD F90 /include:"Debug/" /compile_only /nologo /debug:full /optimize:0
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "hourly - Win32 Release"
# Name "hourly - Win32 Debug"
# Begin Source File

SOURCE=..\CCCODE.FOR
# End Source File
# Begin Source File

SOURCE=..\CD144.FOR
# End Source File
# Begin Source File

SOURCE=.\ceam.for
# End Source File
# Begin Source File

SOURCE=..\define.for
# End Source File
# Begin Source File

SOURCE=..\DELTT.FOR
# End Source File
# Begin Source File

SOURCE=..\FIL144.FOR
DEP_F90_FIL14=\
	"..\hrly.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\FILES.FOR
DEP_F90_FILES=\
	"..\hrly.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\FILMET.FOR
DEP_F90_FILME=\
	"..\hrly.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\FILSAM.FOR
DEP_F90_FILSA=\
	"..\hrly.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\FLOVEC.FOR
DEP_F90_FLOVE=\
	"..\hrly.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\getfld.for
# End Source File
# Begin Source File

SOURCE=..\HR0024.FOR
# End Source File
# Begin Source File

SOURCE=..\HRLYPROC.FOR
DEP_F90_HRLYP=\
	"..\hrly.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\INDECR.FOR
# End Source File
# Begin Source File

SOURCE=..\INVMOL.FOR
# End Source File
# Begin Source File

SOURCE=..\JULIAN.FOR
# End Source File
# Begin Source File

SOURCE=..\lwupr.for
# End Source File
# Begin Source File

SOURCE=..\MIX_HT.FOR
# End Source File
# Begin Source File

SOURCE=..\parser.for
# End Source File
# Begin Source File

SOURCE=..\PCODES.FOR
# End Source File
# Begin Source File

SOURCE=..\PRCPCODE.FOR
# End Source File
# Begin Source File

SOURCE=..\PREAD.FOR
# End Source File
# Begin Source File

SOURCE=..\PROFILE.FOR
# End Source File
# Begin Source File

SOURCE=..\READPARM.FOR
DEP_F90_READP=\
	"..\hrly.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\READPP.FOR
DEP_F90_READPP=\
	"..\hrly.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\READSFC.FOR
DEP_F90_READS=\
	"..\hrly.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\SAMSON.FOR
# End Source File
# Begin Source File

SOURCE=..\SOLALT.FOR
# End Source File
# Begin Source File

SOURCE=..\STABTURN.FOR
DEP_F90_STABT=\
	"..\hrly.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\stonum.for
# End Source File
# Begin Source File

SOURCE=..\SUNTIME.FOR
# End Source File
# Begin Source File

SOURCE=..\U01.FOR
# End Source File
# Begin Source File

SOURCE=..\UNCDP.FOR
# End Source File
# Begin Source File

SOURCE=..\UNITS.FOR
DEP_F90_UNITS=\
	"..\hrly.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\USTAR.FOR
# End Source File
# Begin Source File

SOURCE=..\VAR2FIX.FOR
# End Source File
# Begin Source File

SOURCE=..\VARBINT.FOR
DEP_F90_VARBI=\
	"..\hrly.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\WRITHRLY.FOR
DEP_F90_WRITH=\
	"..\hrly.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\WXMAP.FOR
# End Source File
# End Target
# End Project
