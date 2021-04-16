# Microsoft Developer Studio Project File - Name="genexc" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=genexc - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "genexc.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "genexc.mak" CFG="genexc - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "genexc - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "genexc - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "genexc - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
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

!ELSEIF  "$(CFG)" == "genexc - Win32 Debug"

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

# Name "genexc - Win32 Release"
# Name "genexc - Win32 Debug"
# Begin Source File

SOURCE=..\..\Aircal.for
DEP_F90_AIRCA=\
	"..\..\AIRPAR.CMN"\
	"..\..\ANMPAR.CMN"\
	"..\..\AQUPAR.CMN"\
	"..\..\CONC.CMN"\
	"..\..\FODPAR.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\RAD.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Airsave.for
# End Source File
# Begin Source File

SOURCE=..\..\Airset.for
DEP_F90_AIRSE=\
	"..\..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Anmcal.for
DEP_F90_ANMCA=\
	"..\..\ANMPAR.CMN"\
	"..\..\CONC.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\FODPAR.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\RAD.CMN"\
	"..\..\SOLPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Aqucal.for
DEP_F90_AQUCA=\
	"..\..\AQUPAR.CMN"\
	"..\..\CONC.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\RAD.CMN"\
	"..\..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\atodat.for
DEP_F90_ATODA=\
	"..\..\AIRINFO.CMN"\
	"..\..\CONIN.CMN"\
	"..\..\CURNAM.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\EXINFO.CMN"\
	"..\..\EXPLOC.CMN"\
	"..\..\FLUX.CMN"\
	"..\..\FNAMES.CMN"\
	"..\..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Atoin.for
DEP_F90_ATOIN=\
	"..\..\DEVICE.CMN"\
	"..\..\EXPLOC.CMN"\
	"..\..\FLUX.CMN"\
	"..\..\FNAMES.CMN"\
	"..\..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Avgcon.for
# End Source File
# Begin Source File

SOURCE=..\..\Blkdat.for
DEP_F90_BLKDA=\
	"..\..\DEVICE.CMN"\
	"..\..\EXPNAM.CMN"\
	"..\..\OPT.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Blockd.for
DEP_F90_BLOCK=\
	"..\..\AIRPAR.CMN"\
	"..\..\ANMPAR.CMN"\
	"..\..\EXTPAR.CMN"\
	"..\..\FILES.CMN"\
	"..\..\FODPAR.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\SOLPAR.CMN"\
	"..\..\SWPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\CandhNEW.for
DEP_F90_CANDH=\
	"..\..\ANMPAR.CMN"\
	"..\..\CONC.CMN"\
	"..\..\FODPAR.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\RAD.CMN"\
	"..\..\SOLPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\chain.for
DEP_F90_CHAIN=\
	"..\..\DECAY.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Crpcal.for
DEP_F90_CRPCA=\
	"..\..\CONC.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\FODPAR.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\RAD.CMN"\
	"..\..\SOLPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\DBreadC.for
DEP_F90_DBREA=\
	"..\..\ANMPAR.CMN"\
	"..\..\AQUPAR.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\EXINFO.CMN"\
	"..\..\FNAMES.CMN"\
	"..\..\FODPAR.CMN"\
	"..\..\LEACH.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\SOLPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Dkharv.for
DEP_F90_DKHAR=\
	"..\..\ANMPAR.CMN"\
	"..\..\CONC.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\FODPAR.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\RAD.CMN"\
	"..\..\SOLPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Drkcal.for
DEP_F90_DRKCA=\
	"..\..\AQUPAR.CMN"\
	"..\..\CONC.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\RAD.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Edranm.for
DEP_F90_EDRAN=\
	"..\..\ANMPAR.CMN"\
	"..\..\CONC.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\FODPAR.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\RAD.CMN"\
	"..\..\SOLPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Edrcrp.for
DEP_F90_EDRCR=\
	"..\..\CONC.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\FODPAR.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\RAD.CMN"\
	"..\..\SOLPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Edrres.for
DEP_F90_EDRRE=\
	"..\..\CONC.CMN"\
	"..\..\FODPAR.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\RAD.CMN"\
	"..\..\SOLPAR.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\env.for
DEP_F90_ENV_F=\
	"..\..\AFLAGS.CMN"\
	"..\..\CONC.CMN"\
	"..\..\DECAY.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\NUCNAM.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\RAD.CMN"\
	"..\..\SOLPAR.CMN"\
	"..\..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Epfdat.for
DEP_F90_EPFDA=\
	"..\..\AIRINFO.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\EXINFO.CMN"\
	"..\..\EXPALL.CMN"\
	"..\..\EXPNAM.CMN"\
	"..\..\FLUX.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Epfdset.for
DEP_F90_EPFDS=\
	"..\..\DEVICE.CMN"\
	"..\..\FLUX.CMN"\
	"..\..\OPT.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Epfgrid.for
DEP_F90_EPFGR=\
	"..\..\AIRINFO.CMN"\
	"..\..\CONIN.CMN"\
	"..\..\CURNAM.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\EXINFO.CMN"\
	"..\..\EXPALL.CMN"\
	"..\..\EXPNAM.CMN"\
	"..\..\FLUX.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Epfhead.for
DEP_F90_EPFHE=\
	"..\..\DEVICE.CMN"\
	"..\..\OPT.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Expos.for
DEP_F90_EXPOS=\
	"..\..\AFLAGS.CMN"\
	"..\..\AIRINFO.CMN"\
	"..\..\ANMPAR.CMN"\
	"..\..\CONIN.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\EXINFO.CMN"\
	"..\..\FLUX.CMN"\
	"..\..\FNAMES.CMN"\
	"..\..\FODPAR.CMN"\
	"..\..\NUCNAM.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\RAD.CMN"\
	"..\..\SOLPAR.CMN"\
	"..\..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Exposr.for
DEP_F90_EXPOSR=\
	"..\..\AFLAGS.CMN"\
	"..\..\CONC.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\EXPALL.CMN"\
	"..\..\FLUX.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\RAD.CMN"\
	"..\..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Extcal.for
DEP_F90_EXTCA=\
	"..\..\AIRPAR.CMN"\
	"..\..\CONC.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\EXTPAR.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\RAD.CMN"\
	"..\..\SOLPAR.CMN"\
	"..\..\SWPAR.CMN"\
	"..\..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Filerr.for
DEP_F90_FILER=\
	"..\..\DEVICE.CMN"\
	"..\..\FILES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Getint.for
DEP_F90_GETIN=\
	"..\..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Getleach.for
DEP_F90_GETLE=\
	"..\..\DEVICE.CMN"\
	"..\..\EXINFO.CMN"\
	"..\..\LEACH.CMN"\
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
	"..\..\EXINFO.CMN"\
	"..\..\FNAMES.CMN"\
	
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

SOURCE=..\..\Getxy.for
# End Source File
# Begin Source File

SOURCE=..\..\headin.for
DEP_F90_HEADI=\
	"..\..\DEVICE.CMN"\
	"..\..\EXINFO.CMN"\
	"..\..\FNAMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Inhcal.for
DEP_F90_INHCA=\
	"..\..\CONC.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\EXTPAR.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\RAD.CMN"\
	"..\..\SOLPAR.CMN"\
	"..\..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Initnv.for
DEP_F90_INITN=\
	"..\..\ANMPAR.CMN"\
	"..\..\AQUPAR.CMN"\
	"..\..\CONC.CMN"\
	"..\..\EXPALL.CMN"\
	"..\..\FODPAR.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\RAD.CMN"\
	"..\..\SOLPAR.CMN"\
	"..\..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Lenword.for
# End Source File
# Begin Source File

SOURCE=..\..\Markin.for
DEP_F90_MARKI=\
	"..\..\DEVICE.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\moveset.for
# End Source File
# Begin Source File

SOURCE=..\..\moveto.for
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

SOURCE=..\..\Redcas.for
DEP_F90_REDCA=\
	"..\..\AIRPAR.CMN"\
	"..\..\ANMPAR.CMN"\
	"..\..\AQUPAR.CMN"\
	"..\..\EXTPAR.CMN"\
	"..\..\FODPAR.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\SOLPAR.CMN"\
	"..\..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Rmdget.for
DEP_F90_RMDGE=\
	"..\..\AQUPAR.CMN"\
	"..\..\DECAY.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\RAD.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\seqi.for
# End Source File
# Begin Source File

SOURCE=..\..\setair.for
DEP_F90_SETAI=\
	"..\..\AFLAGS.CMN"\
	"..\..\CONC.CMN"\
	"..\..\CONIN.CMN"\
	"..\..\FLUX.CMN"\
	"..\..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Setanion.for
DEP_F90_SETAN=\
	"..\..\NUCNAM.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\RAD.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Setcon.for
DEP_F90_SETCO=\
	"..\..\CONC.CMN"\
	"..\..\CONIN.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\RAD.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Setelaw.for
DEP_F90_SETEL=\
	"..\..\NUCNAM.CMN"\
	"..\..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Trnspt.for
DEP_F90_TRNSP=\
	"..\..\CONC.CMN"\
	"..\..\EXPALL.CMN"\
	"..\..\OPT.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\RAD.CMN"\
	"..\..\SOLPAR.CMN"\
	"..\..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\upperc.for
# End Source File
# Begin Source File

SOURCE=..\..\Vinterp.for
DEP_F90_VINTE=\
	"..\..\CONIN.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Wcfdat.for
DEP_F90_WCFDA=\
	"..\..\CONIN.CMN"\
	"..\..\DEVICE.CMN"\
	"..\..\PARMTR.PAR"\
	"..\..\RAD.CMN"\
	"..\..\TIMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=..\..\Wcfin.for
DEP_F90_WCFIN=\
	"..\..\DEVICE.CMN"\
	"..\..\FNAMES.CMN"\
	"..\..\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=..\..\xypoint.for
DEP_F90_XYPOI=\
	"..\..\AIRINFO.CMN"\
	"..\..\EXINFO.CMN"\
	
# End Source File
# End Target
# End Project
