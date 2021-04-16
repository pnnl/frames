# Microsoft Developer Studio Project File - Name="nearfield" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=nearfield - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "nearfield.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "nearfield.mak" CFG="nearfield - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "nearfield - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "nearfield - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "nearfield - Win32 Release"

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

!ELSEIF  "$(CFG)" == "nearfield - Win32 Debug"

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

# Name "nearfield - Win32 Release"
# Name "nearfield - Win32 Debug"
# Begin Source File

SOURCE=..\Airset.for
# End Source File
# Begin Source File

SOURCE=..\Anmcal.for
DEP_F90_ANMCA=\
	"..\AFPPAR.CMN"\
	"..\ANMPAR.CMN"\
	"..\CONC.CMN"\
	"..\FODPAR.CMN"\
	"..\OPT.CMN"\
	"..\RAD.CMN"\
	"..\SOLPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Biocal.for
DEP_F90_BIOCA=\
	"..\OPT.CMN"\
	"..\SOLPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Blkdat.for
DEP_F90_BLKDA=\
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

SOURCE=..\Candh.for
DEP_F90_CANDH=\
	"..\CONC.CMN"\
	"..\OPT.CMN"\
	"..\RAD.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\chain.for
DEP_F90_CHAIN=\
	"..\DECAY.CMN"\
	"..\DEVICE.CMN"\
	"..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\Crpcal.for
DEP_F90_CRPCA=\
	"..\AFPPAR.CMN"\
	"..\CONC.CMN"\
	"..\FODPAR.CMN"\
	"..\OPT.CMN"\
	"..\RAD.CMN"\
	"..\SOLPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\DBreadN.for
DEP_F90_DBREA=\
	"..\ANMPAR.CMN"\
	"..\AQUPAR.CMN"\
	"..\DEVICE.CMN"\
	"..\EXINFO.CMN"\
	"..\FNAMES.CMN"\
	"..\FODPAR.CMN"\
	"..\LEACH.CMN"\
	"..\OPT.CMN"\
	"..\PARMTR.PAR"\
	"..\SOLPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Dkharv.for
DEP_F90_DKHAR=\
	"..\ANMPAR.CMN"\
	"..\CONC.CMN"\
	"..\FODPAR.CMN"\
	"..\OPT.CMN"\
	"..\RAD.CMN"\
	"..\SOLPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Drkcal.for
DEP_F90_DRKCA=\
	"..\AQUPAR.CMN"\
	"..\CONC.CMN"\
	"..\OPT.CMN"\
	"..\RAD.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Edranm.for
DEP_F90_EDRAN=\
	"..\ANMPAR.CMN"\
	"..\CONC.CMN"\
	"..\DECAY.CMN"\
	"..\FODPAR.CMN"\
	"..\OPT.CMN"\
	"..\PARMTR.PAR"\
	"..\RAD.CMN"\
	"..\SOLPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Edrcrp.for
DEP_F90_EDRCR=\
	"..\AFPPAR.CMN"\
	"..\CONC.CMN"\
	"..\DECAY.CMN"\
	"..\FODPAR.CMN"\
	"..\OPT.CMN"\
	"..\PARMTR.PAR"\
	"..\RAD.CMN"\
	"..\SOLPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Edrnon.for
DEP_F90_EDRNO=\
	"..\ANMPAR.CMN"\
	"..\CONC.CMN"\
	"..\FODPAR.CMN"\
	"..\OPT.CMN"\
	"..\RAD.CMN"\
	"..\SOLPAR.CMN"\
	"..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Edrres.for
DEP_F90_EDRRE=\
	"..\AFPPAR.CMN"\
	"..\CONC.CMN"\
	"..\DECAY.CMN"\
	"..\FODPAR.CMN"\
	"..\OPT.CMN"\
	"..\PARMTR.PAR"\
	"..\RAD.CMN"\
	"..\SOLPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\env.for
DEP_F90_ENV_F=\
	"..\CONC.CMN"\
	"..\DECAY.CMN"\
	"..\OPT.CMN"\
	"..\RAD.CMN"\
	"..\SOLPAR.CMN"\
	"..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Envlib.for

!IF  "$(CFG)" == "nearfield - Win32 Release"

!ELSEIF  "$(CFG)" == "nearfield - Win32 Debug"

DEP_F90_ENVLI=\
	"..\AIRPAR.CMN"\
	"..\ANMPAR.CMN"\
	"..\AQUPAR.CMN"\
	"..\EXINFO.CMN"\
	"..\FODPAR.CMN"\
	"..\LEACH.CMN"\
	"..\OPT.CMN"\
	"..\RAD.CMN"\
	"..\SOLPAR.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\Epfdat.for
DEP_F90_EPFDA=\
	"..\AIRINFO.CMN"\
	"..\DEVICE.CMN"\
	"..\EXINFO.CMN"\
	"..\EXPALL.CMN"\
	"..\EXPNAM.CMN"\
	"..\FLUX.CMN"\
	"..\OPT.CMN"\
	"..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Epfdset.for
DEP_F90_EPFDS=\
	"..\DEVICE.CMN"\
	"..\FLUX.CMN"\
	"..\OPT.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Epfhead.for
DEP_F90_EPFHE=\
	"..\DEVICE.CMN"\
	"..\OPT.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Exposr.for
DEP_F90_EXPOS=\
	"..\CONC.CMN"\
	"..\EXPALL.CMN"\
	"..\OPT.CMN"\
	"..\RAD.CMN"\
	"..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Extcal.for
DEP_F90_EXTCA=\
	"..\CONC.CMN"\
	"..\DEVICE.CMN"\
	"..\EXTPAR.CMN"\
	"..\OPT.CMN"\
	"..\RAD.CMN"\
	"..\SOLPAR.CMN"\
	"..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Filerr.for
DEP_F90_FILER=\
	"..\DEVICE.CMN"\
	"..\FILES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Getint.for
DEP_F90_GETIN=\
	"..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\Getleach.for
DEP_F90_GETLE=\
	"..\DEVICE.CMN"\
	"..\EXINFO.CMN"\
	"..\LEACH.CMN"\
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
	"..\EXINFO.CMN"\
	"..\FNAMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Getreal.for
DEP_F90_GETRE=\
	"..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\Getrt.for

!IF  "$(CFG)" == "nearfield - Win32 Release"

!ELSEIF  "$(CFG)" == "nearfield - Win32 Debug"

DEP_F90_GETRT=\
	"..\DEVICE.CMN"\
	

!ENDIF 

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

SOURCE=..\Getxy.for
# End Source File
# Begin Source File

SOURCE=..\headin.for
DEP_F90_HEADI=\
	"..\DEVICE.CMN"\
	"..\EXINFO.CMN"\
	"..\FNAMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Inhcal.for
DEP_F90_INHCA=\
	"..\AFPPAR.CMN"\
	"..\CONC.CMN"\
	"..\EXTPAR.CMN"\
	"..\OPT.CMN"\
	"..\RAD.CMN"\
	"..\SOLPAR.CMN"\
	"..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Initnv.for
DEP_F90_INITN=\
	"..\ANMPAR.CMN"\
	"..\CONC.CMN"\
	"..\EXPALL.CMN"\
	"..\FODPAR.CMN"\
	"..\OPT.CMN"\
	"..\RAD.CMN"\
	"..\SOLPAR.CMN"\
	"..\TIMES.CMN"\
	
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

SOURCE=..\moveset.for
# End Source File
# Begin Source File

SOURCE=..\moveto.for
DEP_F90_MOVET=\
	"..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\near.for
DEP_F90_NEAR_=\
	"..\AFLAGS.CMN"\
	"..\ANMPAR.CMN"\
	"..\CONIN.CMN"\
	"..\DEVICE.CMN"\
	"..\EXINFO.CMN"\
	"..\FLUX.CMN"\
	"..\FNAMES.CMN"\
	"..\FODPAR.CMN"\
	"..\OPT.CMN"\
	"..\PARMTR.PAR"\
	"..\RAD.CMN"\
	"..\SOLPAR.CMN"\
	"..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Opnfil.for
DEP_F90_OPNFI=\
	"..\DEVICE.CMN"\
	"..\FILES.CMN"\
	"..\TITL.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Packag.for
DEP_F90_PACKA=\
	"..\CONC.CMN"\
	"..\DECAY.CMN"\
	"..\OPT.CMN"\
	"..\RAD.CMN"\
	"..\SOLPAR.CMN"\
	"..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Prior.for
DEP_F90_PRIOR=\
	"..\CONC.CMN"\
	"..\OPT.CMN"\
	"..\RAD.CMN"\
	"..\SOLPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Redcas.for
DEP_F90_REDCA=\
	"..\AIRPAR.CMN"\
	"..\ANMPAR.CMN"\
	"..\DEVICE.CMN"\
	"..\ENVPAR.CMN"\
	"..\EXTPAR.CMN"\
	"..\FODPAR.CMN"\
	"..\OPT.CMN"\
	"..\PARMTR.PAR"\
	"..\SOLPAR.CMN"\
	"..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Redist.for
DEP_F90_REDIS=\
	"..\CONC.CMN"\
	"..\OPT.CMN"\
	"..\RAD.CMN"\
	"..\SOLPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Rmdget.for
DEP_F90_RMDGE=\
	"..\AQUPAR.CMN"\
	"..\DECAY.CMN"\
	"..\DEVICE.CMN"\
	"..\OPT.CMN"\
	"..\RAD.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Scfdat.for
DEP_F90_SCFDA=\
	"..\CONIN.CMN"\
	"..\DEVICE.CMN"\
	"..\OPT.CMN"\
	"..\SOLPAR.CMN"\
	"..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Scfin.for
DEP_F90_SCFIN=\
	"..\CONIN.CMN"\
	"..\DEVICE.CMN"\
	"..\FNAMES.CMN"\
	"..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\Seq.for
# End Source File
# Begin Source File

SOURCE=..\seqi.for
# End Source File
# Begin Source File

SOURCE=..\setair.for

!IF  "$(CFG)" == "nearfield - Win32 Release"

!ELSEIF  "$(CFG)" == "nearfield - Win32 Debug"

DEP_F90_SETAI=\
	"..\AFLAGS.CMN"\
	"..\CONC.CMN"\
	"..\CONIN.CMN"\
	"..\FLUX.CMN"\
	

!ENDIF 

# End Source File
# Begin Source File

SOURCE=..\Setcon.for
DEP_F90_SETCO=\
	"..\CONC.CMN"\
	"..\CONIN.CMN"\
	"..\DEVICE.CMN"\
	"..\OPT.CMN"\
	"..\RAD.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Setelaw.for
DEP_F90_SETEL=\
	"..\RAD.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\Trnspt.for
DEP_F90_TRNSP=\
	"..\CONC.CMN"\
	"..\EXPALL.CMN"\
	"..\OPT.CMN"\
	"..\RAD.CMN"\
	"..\SOLPAR.CMN"\
	"..\SWPAR.CMN"\
	"..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\upperc.for
# End Source File
# End Target
# End Project
