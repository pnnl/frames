# Microsoft Developer Studio Project File - Name="genriv" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=genriv - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "genriv.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "genriv.mak" CFG="genriv - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "genriv - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "genriv - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "genriv - Win32 Release"

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

!ELSEIF  "$(CFG)" == "genriv - Win32 Debug"

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

# Name "genriv - Win32 Release"
# Name "genriv - Win32 Debug"
# Begin Source File

SOURCE=..\..\Avgcon.for
DEP_F90_AVGCO=\
	"..\..\DEVICE.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Blkdat.for
DEP_F90_BLKDA=\
	"..\..\DEVICE.CMN"\
	"..\..\OPT.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Chain.for
DEP_F90_CHAIN=\
	"..\..\DECAY.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Conout.for
DEP_F90_CONOU=\
	"..\..\DECAY.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\OPT.CMN"\
	"..\..\SWCON.CMN"\
	"..\..\SWINFO.CMN"\
	"..\..\SWPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Filerr.for
DEP_F90_FILER=\
	"..\..\DEVICE.CMN"\
	"..\..\FILES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Getdata.for
DEP_F90_GETDA=\
	"..\..\DEVICE.CMN"\
	"..\..\FNAMES.CMN"\
	"..\..\OPT.CMN"\
	"..\..\SWINFO.CMN"\
	"..\..\SWPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Getint.for
DEP_F90_GETIN=\
	"..\..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Getlog.for
DEP_F90_GETLO=\
	"..\..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Getnam.for
DEP_F90_GETNA=\
	"..\..\DEVICE.CMN"\
	"..\..\FNAMES.CMN"\
	"..\..\SWINFO.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Getreal.for
DEP_F90_GETRE=\
	"..\..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Getset.for
DEP_F90_GETSE=\
	"..\..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Getstr.for
DEP_F90_GETST=\
	"..\..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Headin.for
DEP_F90_HEADI=\
	"..\..\DEVICE.CMN"\
	"..\..\FNAMES.CMN"\
	"..\..\OPT.CMN"\
	"..\..\SWINFO.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Headout.for
DEP_F90_HEADO=\
	"..\..\DEVICE.CMN"\
	"..\..\FNAMES.CMN"\
	"..\..\OPT.CMN"\
	"..\..\SWINFO.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Lenword.for
# End Source File
# Begin Source File

SOURCE=..\..\Moveset.for
# End Source File
# Begin Source File

SOURCE=..\..\Moveto.for
DEP_F90_MOVET=\
	"..\..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Opnfil.for
DEP_F90_OPNFI=\
	"..\..\DEVICE.CMN"\
	"..\..\FILES.CMN"\
	"..\..\TITL.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\PARMTR.PAR
# End Source File
# Begin Source File

SOURCE=..\..\Recirc.for

!IF  "$(CFG)" == "genriv - Win32 Release"

!ELSEIF  "$(CFG)" == "genriv - Win32 Debug"

DEP_F90_RECIR=\
	"..\..\DECAY.CMN"\
	"..\..\SWCON.CMN"\
	"..\..\SWPAR.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\..\Rmdget.for
DEP_F90_RMDGE=\
	"..\..\DECAY.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\OPT.CMN"\
	"..\..\SWINFO.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Seq.for
# End Source File
# Begin Source File

SOURCE=..\..\seqi.for
# End Source File
# Begin Source File

SOURCE=..\..\Swacute.for
DEP_F90_SWACU=\
	"..\..\DECAY.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\OPT.CMN"\
	"..\..\SWCON.CMN"\
	"..\..\SWINFO.CMN"\
	"..\..\SWPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Swater.for
DEP_F90_SWATE=\
	"..\..\DEVICE.CMN"\
	"..\..\FNAMES.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\SWCON.CMN"\
	"..\..\SWINFO.CMN"\
	"..\..\SWPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Swchron.for
DEP_F90_SWCHR=\
	"..\..\DECAY.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\OPT.CMN"\
	"..\..\SWCON.CMN"\
	"..\..\SWINFO.CMN"\
	"..\..\SWPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Swconcal.for
DEP_F90_SWCON=\
	"..\..\DEVICE.CMN"\
	"..\..\SWCON.CMN"\
	"..\..\SWINFO.CMN"\
	"..\..\SWPAR.CMN"\
	
# End Source File
# End Target
# End Project
