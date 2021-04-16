# Microsoft Developer Studio Project File - Name="Biota" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=Biota - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Biota.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Biota.mak" CFG="Biota - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Biota - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "Biota - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "Biota - Win32 Release"

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

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

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

# Name "Biota - Win32 Release"
# Name "Biota - Win32 Debug"
# Begin Source File

SOURCE=.\AIRCAL.FOR

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_AIRCA=\
	".\AIRPAR.CMN"\
	".\ANMPAR.CMN"\
	".\AQUPAR.CMN"\
	".\CONC.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_AIRCA=\
	".\AIRPAR.CMN"\
	".\ANMPAR.CMN"\
	".\AQUPAR.CMN"\
	".\CONC.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\AIRSAVE.FOR
# End Source File
# Begin Source File

SOURCE=.\AIRSET.FOR
DEP_F90_AIRSE=\
	".\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=.\Anmcal.for

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_ANMCA=\
	".\ANMPAR.CMN"\
	".\CONC.CMN"\
	".\DEVICE.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_ANMCA=\
	".\ANMPAR.CMN"\
	".\CONC.CMN"\
	".\DEVICE.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\Aqucal.for

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_AQUCA=\
	".\AQUPAR.CMN"\
	".\CONC.CMN"\
	".\DEVICE.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\TIMES.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_AQUCA=\
	".\AQUPAR.CMN"\
	".\CONC.CMN"\
	".\DEVICE.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\TIMES.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\atodat.for

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_ATODA=\
	".\AIRINFO.CMN"\
	".\CONIN.CMN"\
	".\CURNAM.CMN"\
	".\DEVICE.CMN"\
	".\EXINFO.CMN"\
	".\EXPLOC.CMN"\
	".\FLUX.CMN"\
	".\FNAMES.CMN"\
	".\PARMTR.PAR"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_ATODA=\
	".\AIRINFO.CMN"\
	".\CONIN.CMN"\
	".\CURNAM.CMN"\
	".\DEVICE.CMN"\
	".\EXINFO.CMN"\
	".\EXPLOC.CMN"\
	".\FLUX.CMN"\
	".\FNAMES.CMN"\
	".\PARMTR.PAR"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\ATOIN.FOR

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_ATOIN=\
	".\DEVICE.CMN"\
	".\EXPLOC.CMN"\
	".\FLUX.CMN"\
	".\FNAMES.CMN"\
	".\PARMTR.PAR"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_ATOIN=\
	".\DEVICE.CMN"\
	".\EXPLOC.CMN"\
	".\FLUX.CMN"\
	".\FNAMES.CMN"\
	".\PARMTR.PAR"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\AVGCON.FOR
# End Source File
# Begin Source File

SOURCE=.\BIOTA.for

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_BIOTA=\
	".\AFLAGS.CMN"\
	".\AIRINFO.CMN"\
	".\ANMPAR.CMN"\
	".\CONIN.CMN"\
	".\DEVICE.CMN"\
	".\EXINFO.CMN"\
	".\FLUX.CMN"\
	".\FNAMES.CMN"\
	".\FODPAR.CMN"\
	".\NUCNAM.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	".\TIMES.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_BIOTA=\
	".\AFLAGS.CMN"\
	".\AIRINFO.CMN"\
	".\ANMPAR.CMN"\
	".\CONIN.CMN"\
	".\DEVICE.CMN"\
	".\EXINFO.CMN"\
	".\FLUX.CMN"\
	".\FNAMES.CMN"\
	".\FODPAR.CMN"\
	".\NUCNAM.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	".\TIMES.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\Blkdat.for

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_BLKDA=\
	".\DEVICE.CMN"\
	".\EXPNAM.CMN"\
	".\OPT.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_BLKDA=\
	".\DEVICE.CMN"\
	".\EXPNAM.CMN"\
	".\OPT.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\Blockd.for

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_BLOCK=\
	".\AIRPAR.CMN"\
	".\ANMPAR.CMN"\
	".\EXTPAR.CMN"\
	".\FILES.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\SOLPAR.CMN"\
	".\SWPAR.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_BLOCK=\
	".\AIRPAR.CMN"\
	".\ANMPAR.CMN"\
	".\EXTPAR.CMN"\
	".\FILES.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\SOLPAR.CMN"\
	".\SWPAR.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\CandhNEW.for

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_CANDH=\
	".\ANMPAR.CMN"\
	".\CONC.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_CANDH=\
	".\ANMPAR.CMN"\
	".\CONC.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\chain.for

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_CHAIN=\
	".\DECAY.CMN"\
	".\DEVICE.CMN"\
	".\PARMTR.PAR"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_CHAIN=\
	".\DECAY.CMN"\
	".\DEVICE.CMN"\
	".\PARMTR.PAR"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\Crpcal.for

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_CRPCA=\
	".\CONC.CMN"\
	".\DEVICE.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_CRPCA=\
	".\CONC.CMN"\
	".\DEVICE.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\DBreadC.for

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_DBREA=\
	".\ANMPAR.CMN"\
	".\AQUPAR.CMN"\
	".\BIOTDF.CMN"\
	".\DEVICE.CMN"\
	".\EXINFO.CMN"\
	".\FNAMES.CMN"\
	".\FODPAR.CMN"\
	".\LEACH.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\SOLPAR.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_DBREA=\
	".\ANMPAR.CMN"\
	".\AQUPAR.CMN"\
	".\BIOTDF.CMN"\
	".\DEVICE.CMN"\
	".\EXINFO.CMN"\
	".\FNAMES.CMN"\
	".\FODPAR.CMN"\
	".\LEACH.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\SOLPAR.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\DKHARV.FOR

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_DKHAR=\
	".\ANMPAR.CMN"\
	".\CONC.CMN"\
	".\DEVICE.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_DKHAR=\
	".\ANMPAR.CMN"\
	".\CONC.CMN"\
	".\DEVICE.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\EDRANM.FOR

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_EDRAN=\
	".\ANMPAR.CMN"\
	".\CONC.CMN"\
	".\DEVICE.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_EDRAN=\
	".\ANMPAR.CMN"\
	".\CONC.CMN"\
	".\DEVICE.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\EDRCRP.FOR

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_EDRCR=\
	".\CONC.CMN"\
	".\DEVICE.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_EDRCR=\
	".\CONC.CMN"\
	".\DEVICE.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\EDRRES.FOR

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_EDRRE=\
	".\CONC.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_EDRRE=\
	".\CONC.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\env.for

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_ENV_F=\
	".\AFLAGS.CMN"\
	".\CONC.CMN"\
	".\DECAY.CMN"\
	".\DEVICE.CMN"\
	".\NUCNAM.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	".\TIMES.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_ENV_F=\
	".\AFLAGS.CMN"\
	".\CONC.CMN"\
	".\DECAY.CMN"\
	".\DEVICE.CMN"\
	".\NUCNAM.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	".\TIMES.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\EPFDAT.FOR

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_EPFDA=\
	".\AIRINFO.CMN"\
	".\BIOTDF.CMN"\
	".\DEVICE.CMN"\
	".\EXINFO.CMN"\
	".\EXPALL.CMN"\
	".\EXPNAM.CMN"\
	".\FLUX.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\TIMES.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_EPFDA=\
	".\AIRINFO.CMN"\
	".\BIOTDF.CMN"\
	".\DEVICE.CMN"\
	".\EXINFO.CMN"\
	".\EXPALL.CMN"\
	".\EXPNAM.CMN"\
	".\FLUX.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\TIMES.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\Epfdset.for
DEP_F90_EPFDS=\
	".\DEVICE.CMN"\
	".\FLUX.CMN"\
	".\OPT.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\Epfgrid.for

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_EPFGR=\
	".\AIRINFO.CMN"\
	".\BIOTDF.CMN"\
	".\CONIN.CMN"\
	".\CURNAM.CMN"\
	".\DEVICE.CMN"\
	".\EXINFO.CMN"\
	".\EXPALL.CMN"\
	".\EXPNAM.CMN"\
	".\FLUX.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\TIMES.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_EPFGR=\
	".\AIRINFO.CMN"\
	".\BIOTDF.CMN"\
	".\CONIN.CMN"\
	".\CURNAM.CMN"\
	".\DEVICE.CMN"\
	".\EXINFO.CMN"\
	".\EXPALL.CMN"\
	".\EXPNAM.CMN"\
	".\FLUX.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\TIMES.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\Epfhead.for
DEP_F90_EPFHE=\
	".\DEVICE.CMN"\
	".\OPT.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\Exposr.for

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_EXPOS=\
	".\AFLAGS.CMN"\
	".\CONC.CMN"\
	".\DEVICE.CMN"\
	".\EXPALL.CMN"\
	".\FLUX.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\TIMES.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_EXPOS=\
	".\AFLAGS.CMN"\
	".\CONC.CMN"\
	".\DEVICE.CMN"\
	".\EXPALL.CMN"\
	".\FLUX.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\TIMES.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\EXTCAL.FOR

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_EXTCA=\
	".\AIRPAR.CMN"\
	".\CONC.CMN"\
	".\DEVICE.CMN"\
	".\EXTPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	".\SWPAR.CMN"\
	".\TIMES.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_EXTCA=\
	".\AIRPAR.CMN"\
	".\CONC.CMN"\
	".\DEVICE.CMN"\
	".\EXTPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	".\SWPAR.CMN"\
	".\TIMES.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\FILERR.FOR

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_FILER=\
	".\DEVICE.CMN"\
	".\FILES.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_FILER=\
	".\DEVICE.CMN"\
	".\FILES.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\GETINT.FOR
DEP_F90_GETIN=\
	".\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=.\Getleach.for

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_GETLE=\
	".\DEVICE.CMN"\
	".\EXINFO.CMN"\
	".\LEACH.CMN"\
	".\PARMTR.PAR"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_GETLE=\
	".\DEVICE.CMN"\
	".\EXINFO.CMN"\
	".\LEACH.CMN"\
	".\PARMTR.PAR"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\GETLOG.FOR
DEP_F90_GETLO=\
	".\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=.\Getnam.for

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_GETNA=\
	".\DEVICE.CMN"\
	".\EXINFO.CMN"\
	".\FNAMES.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_GETNA=\
	".\DEVICE.CMN"\
	".\EXINFO.CMN"\
	".\FNAMES.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\GETREAL.FOR
DEP_F90_GETRE=\
	".\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=.\GETSET.FOR
DEP_F90_GETSE=\
	".\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=.\GETSTR.FOR
DEP_F90_GETST=\
	".\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=.\GETXY.FOR
# End Source File
# Begin Source File

SOURCE=.\headin.for

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_HEADI=\
	".\DEVICE.CMN"\
	".\EXINFO.CMN"\
	".\FNAMES.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_HEADI=\
	".\DEVICE.CMN"\
	".\EXINFO.CMN"\
	".\FNAMES.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\INITNV.FOR

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_INITN=\
	".\ANMPAR.CMN"\
	".\AQUPAR.CMN"\
	".\CONC.CMN"\
	".\EXPALL.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	".\TIMES.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_INITN=\
	".\ANMPAR.CMN"\
	".\AQUPAR.CMN"\
	".\CONC.CMN"\
	".\EXPALL.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	".\TIMES.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\LENWORD.FOR
# End Source File
# Begin Source File

SOURCE=.\MARKIN.FOR
DEP_F90_MARKI=\
	".\DEVICE.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\moveset.for
# End Source File
# Begin Source File

SOURCE=.\moveto.for
DEP_F90_MOVET=\
	".\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=.\Opnfil.for

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_OPNFI=\
	".\DEVICE.CMN"\
	".\FILES.CMN"\
	".\TITL.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_OPNFI=\
	".\DEVICE.CMN"\
	".\FILES.CMN"\
	".\TITL.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\Redcas.for

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_REDCA=\
	".\AIRPAR.CMN"\
	".\ANMPAR.CMN"\
	".\AQUPAR.CMN"\
	".\BIOTDF.CMN"\
	".\EXTPAR.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\SOLPAR.CMN"\
	".\TIMES.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_REDCA=\
	".\AIRPAR.CMN"\
	".\ANMPAR.CMN"\
	".\AQUPAR.CMN"\
	".\BIOTDF.CMN"\
	".\EXTPAR.CMN"\
	".\FODPAR.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\SOLPAR.CMN"\
	".\TIMES.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\RMDGET.FOR

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_RMDGE=\
	".\AQUPAR.CMN"\
	".\DECAY.CMN"\
	".\DEVICE.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_RMDGE=\
	".\AQUPAR.CMN"\
	".\DECAY.CMN"\
	".\DEVICE.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\seqi.for
# End Source File
# Begin Source File

SOURCE=.\setair.for

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_SETAI=\
	".\AFLAGS.CMN"\
	".\CONC.CMN"\
	".\CONIN.CMN"\
	".\FLUX.CMN"\
	".\PARMTR.PAR"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_SETAI=\
	".\AFLAGS.CMN"\
	".\CONC.CMN"\
	".\CONIN.CMN"\
	".\FLUX.CMN"\
	".\PARMTR.PAR"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\SETANION.FOR

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_SETAN=\
	".\NUCNAM.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_SETAN=\
	".\NUCNAM.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\SETCON.FOR

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_SETCO=\
	".\CONC.CMN"\
	".\CONIN.CMN"\
	".\DEVICE.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_SETCO=\
	".\CONC.CMN"\
	".\CONIN.CMN"\
	".\DEVICE.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\SETELAW.FOR

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_SETEL=\
	".\NUCNAM.CMN"\
	".\PARMTR.PAR"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_SETEL=\
	".\NUCNAM.CMN"\
	".\PARMTR.PAR"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\TRNSPT.FOR

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_TRNSP=\
	".\CONC.CMN"\
	".\EXPALL.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	".\TIMES.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_TRNSP=\
	".\CONC.CMN"\
	".\EXPALL.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\SOLPAR.CMN"\
	".\TIMES.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\upperc.for
# End Source File
# Begin Source File

SOURCE=.\VINTERP.FOR

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_VINTE=\
	".\CONIN.CMN"\
	".\DEVICE.CMN"\
	".\PARMTR.PAR"\
	".\TIMES.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_VINTE=\
	".\CONIN.CMN"\
	".\DEVICE.CMN"\
	".\PARMTR.PAR"\
	".\TIMES.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\Wcfdat.for

!IF  "$(CFG)" == "Biota - Win32 Release"

DEP_F90_WCFDA=\
	".\CONIN.CMN"\
	".\DEVICE.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\TIMES.CMN"\
	

!ELSEIF  "$(CFG)" == "Biota - Win32 Debug"

DEP_F90_WCFDA=\
	".\CONIN.CMN"\
	".\DEVICE.CMN"\
	".\PARMTR.PAR"\
	".\RAD.CMN"\
	".\TIMES.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=.\WCFIN.FOR
DEP_F90_WCFIN=\
	".\DEVICE.CMN"\
	".\FNAMES.CMN"\
	".\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=.\xypoint.for
DEP_F90_XYPOI=\
	".\AIRINFO.CMN"\
	".\EXINFO.CMN"\
	
# End Source File
# End Target
# End Project
