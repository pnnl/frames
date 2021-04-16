# Microsoft Developer Studio Project File - Name="GENRCP" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=GENRCP - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "GENRCP.MAK".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "GENRCP.MAK" CFG="GENRCP - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "GENRCP - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "GENRCP - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "GENRCP - Win32 Release"

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

!ELSEIF  "$(CFG)" == "GENRCP - Win32 Debug"

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

# Name "GENRCP - Win32 Release"
# Name "GENRCP - Win32 Debug"
# Begin Source File

SOURCE=..\Blkdat.for
DEP_F90_BLKDA=\
	"..\DERMFAC.CMN"\
	"..\DEVICE.CMN"\
	"..\EXPNAM.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Blockd.for
DEP_F90_BLOCK=\
	"..\AIRPAR.CMN"\
	"..\ANMPAR.CMN"\
	"..\EXTPAR.CMN"\
	"..\FILES.CMN"\
	"..\FODPAR.CMN"\
	"..\LABELS.CMN"\
	"..\OPT.CMN"\
	"..\SOLPAR.CMN"\
	"..\SWPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\epfnum.for
DEP_F90_EPFNU=\
	"..\DEVICE.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Epfrad.for
DEP_F90_EPFRA=\
	"..\DEVICE.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\epfrcp.for
DEP_F90_EPFRC=\
	"..\DEVICE.CMN"\
	"..\OPT.CMN"\
	"..\RCPINFO.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\epfrif.for

!IF  "$(CFG)" == "GENRCP - Win32 Release"

DEP_F90_EPFRI=\
	"..\AGES.CMN"\
	"..\DEVICE.CMN"\
	"..\OPT.CMN"\
	"..\PARMTR.PAR"\
	"..\PATHFAC.CMN"\
	

!ELSEIF  "$(CFG)" == "GENRCP - Win32 Debug"

DEP_F90_EPFRI=\
	"..\AGES.CMN"\
	"..\DEVICE.CMN"\
	"..\OPT.CMN"\
	"..\PARMTR.PAR"\
	"..\PATHFAC.CMN"\
	"..\RADON.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\Exptest.for
DEP_F90_EXPTE=\
	"..\DEVICE.CMN"\
	"..\EXPNAM.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Getint.for
DEP_F90_GETIN=\
	"..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\Getlog.for
DEP_F90_GETLO=\
	"..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\Getnam.for
DEP_F90_GETNA=\
	"..\DEVICE.CMN"\
	"..\FNAMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Getreal.for
DEP_F90_GETRE=\
	"..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\Getset.for
DEP_F90_GETSE=\
	"..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\Getstr.for
DEP_F90_GETST=\
	"..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\intake.for

!IF  "$(CFG)" == "GENRCP - Win32 Release"

DEP_F90_INTAK=\
	"..\AFLAGS.CMN"\
	"..\AGES.CMN"\
	"..\ANMPAR.CMN"\
	"..\CONIN.CMN"\
	"..\DEVICE.CMN"\
	"..\FLUX.CMN"\
	"..\FNAMES.CMN"\
	"..\FODPAR.CMN"\
	"..\NUCNAM.CMN"\
	"..\OPT.CMN"\
	"..\PARMTR.PAR"\
	"..\RADATA.CMN"\
	"..\RCPINFO.CMN"\
	"..\SOLPAR.CMN"\
	"..\TIMES.CMN"\
	

!ELSEIF  "$(CFG)" == "GENRCP - Win32 Debug"

DEP_F90_INTAK=\
	"..\AFLAGS.CMN"\
	"..\AGES.CMN"\
	"..\ANMPAR.CMN"\
	"..\CONIN.CMN"\
	"..\DEVICE.CMN"\
	"..\FLUX.CMN"\
	"..\FNAMES.CMN"\
	"..\FODPAR.CMN"\
	"..\NUCNAM.CMN"\
	"..\OPT.CMN"\
	"..\PARMTR.PAR"\
	"..\RADATA.CMN"\
	"..\RADON.CMN"\
	"..\RCPINFO.CMN"\
	"..\SOLPAR.CMN"\
	"..\TIMES.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\Intaker.for

!IF  "$(CFG)" == "GENRCP - Win32 Release"

DEP_F90_INTAKE=\
	"..\AGES.CMN"\
	"..\ALLPAR.CMN"\
	"..\DEVICE.CMN"\
	"..\PATHFAC.CMN"\
	"..\RADATA.CMN"\
	

!ELSEIF  "$(CFG)" == "GENRCP - Win32 Debug"

DEP_F90_INTAKE=\
	"..\AGES.CMN"\
	"..\ALLPAR.CMN"\
	"..\DEVICE.CMN"\
	"..\PATHFAC.CMN"\
	"..\RADATA.CMN"\
	"..\RADON.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\Lenword.for
# End Source File
# Begin Source File

SOURCE=..\Markin.for
DEP_F90_MARKI=\
	"..\DEVICE.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Moveset.for
# End Source File
# Begin Source File

SOURCE=..\Moveto.for
DEP_F90_MOVET=\
	"..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\Pathset.for
DEP_F90_PATHS=\
	"..\AGES.CMN"\
	"..\ALLPAR.CMN"\
	"..\DERMFAC.CMN"\
	"..\DEVICE.CMN"\
	"..\OPT.CMN"\
	"..\PATHFAC.CMN"\
	"..\RADATA.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\Radon.cmn
# End Source File
# Begin Source File

SOURCE=..\radtest.for
DEP_F90_RADTE=\
	"..\DEVICE.CMN"\
	"..\RADATA.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Redcas.for
DEP_F90_REDCA=\
	"..\AGES.CMN"\
	"..\ALLPAR.CMN"\
	"..\DEVICE.CMN"\
	"..\OPT.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\rifdset.for
DEP_F90_RIFDS=\
	"..\AGES.CMN"\
	"..\DEVICE.CMN"\
	"..\OPT.CMN"\
	"..\RCPINFO.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\rifhead.for
DEP_F90_RIFHE=\
	"..\DEVICE.CMN"\
	"..\OPT.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Rifrad.for
DEP_F90_RIFRA=\
	"..\DEVICE.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\seqi.for
# End Source File
# Begin Source File

SOURCE=..\upperc.for
# End Source File
# End Target
# End Project
