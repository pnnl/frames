# Microsoft Developer Studio Project File - Name="BiotView" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=BiotView - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "BiotView.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "BiotView.mak" CFG="BiotView - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "BiotView - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "BiotView - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "BiotView - Win32 Release"

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

!ELSEIF  "$(CFG)" == "BiotView - Win32 Debug"

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

# Name "BiotView - Win32 Release"
# Name "BiotView - Win32 Debug"
# Begin Source File

SOURCE=..\Atoread.for
DEP_F90_ATORE=\
	"..\NESHAPS.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Blkdat.for
DEP_F90_BLKDA=\
	"..\NESHAPS.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\GETINT.FOR
# End Source File
# Begin Source File

SOURCE=..\GETLOG.FOR
# End Source File
# Begin Source File

SOURCE=..\Getnam.for
DEP_F90_GETNA=\
	"..\NESHAPS.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\GETREAL.FOR
# End Source File
# Begin Source File

SOURCE=..\GETSET.FOR
# End Source File
# Begin Source File

SOURCE=..\GETSTR.FOR
# End Source File
# Begin Source File

SOURCE=..\infood.for
DEP_F90_INFOO=\
	"..\NESHAPS.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Inputs.for
DEP_F90_INPUT=\
	"..\DATABLKS.CMN"\
	"..\NESHAPS.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Main.for
DEP_F90_MAIN_=\
	"..\DATABLKS.CMN"\
	"..\DATARCP.CMN"\
	"..\NESHAPS.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\MOVESET.FOR
# End Source File
# Begin Source File

SOURCE=..\MOVETO.FOR
# End Source File
# Begin Source File

SOURCE=..\OUTAIR.FOR

!IF  "$(CFG)" == "BiotView - Win32 Release"

DEP_F90_OUTAI=\
	"..\DATABLKS.CMN"\
	"..\NESHAPS.CMN"\
	

!ELSEIF  "$(CFG)" == "BiotView - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\OUTEXP.FOR
DEP_F90_OUTEX=\
	"..\DATABLKS.CMN"\
	"..\DATARCP.CMN"\
	"..\NESHAPS.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\OUTSRC.FOR

!IF  "$(CFG)" == "BiotView - Win32 Release"

DEP_F90_OUTSR=\
	"..\DATABLKS.CMN"\
	"..\NESHAPS.CMN"\
	

!ELSEIF  "$(CFG)" == "BiotView - Win32 Debug"

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\Redcas.for
# End Source File
# Begin Source File

SOURCE=..\seqi.for
# End Source File
# End Target
# End Project
