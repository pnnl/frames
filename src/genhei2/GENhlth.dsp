# Microsoft Developer Studio Project File - Name="GENhlth" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=GENhlth - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "GENhlth.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "GENhlth.mak" CFG="GENhlth - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "GENhlth - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "GENhlth - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "GENhlth - Win32 Release"

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

!ELSEIF  "$(CFG)" == "GENhlth - Win32 Debug"

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

# Name "GENhlth - Win32 Release"
# Name "GENhlth - Win32 Debug"
# Begin Source File

SOURCE=.\Agealg.for
# End Source File
# Begin Source File

SOURCE=.\Blkdat.for
DEP_F90_BLKDA=\
	".\DERMFAC.CMN"\
	".\DEVICE.CMN"\
	".\EXPNAM.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\Blockd.for
DEP_F90_BLOCK=\
	".\ALLPAR.CMN"\
	".\LABELS.CMN"\
	".\OPT.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\DBread.for
DEP_F90_DBREA=\
	".\DEVICE.CMN"\
	".\DFACTR.CMN"\
	".\FNAMES.CMN"\
	".\PARMTR.PAR"\
	".\RCPINFO.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\dfset.for
DEP_F90_DFSET=\
	".\batch.cmn"\
	".\dcfpak.cmn"\
	".\FACTORS.CMN"\
	".\iolist.cmn"\
	".\pakparm.cmn"\
	
# End Source File
# Begin Source File

SOURCE=.\Doscof.for
DEP_F90_DOSCO=\
	".\batch.cmn"\
	".\dcfpak.cmn"\
	".\iolist.cmn"\
	".\pakparm.cmn"\
	
# End Source File
# Begin Source File

SOURCE=.\ebatman.for
DEP_F90_EBATM=\
	".\iolist.cmn"\
	".\pakparm.cmn"\
	
# End Source File
# Begin Source File

SOURCE=.\ebirch.for
DEP_F90_EBIRC=\
	".\dcfpak.cmn"\
	".\pakparm.cmn"\
	
# End Source File
# Begin Source File

SOURCE=.\eblock.for
DEP_F90_EBLOC=\
	".\dcfpak.cmn"\
	".\pakparm.cmn"\
	
# End Source File
# Begin Source File

SOURCE=.\echain.for
DEP_F90_ECHAI=\
	".\batch.cmn"\
	".\dcfpak.cmn"\
	".\iolist.cmn"\
	".\pakparm.cmn"\
	
# End Source File
# Begin Source File

SOURCE=.\echkab.for
# End Source File
# Begin Source File

SOURCE=.\echkmn.for
# End Source File
# Begin Source File

SOURCE=.\eclosm.for
DEP_F90_ECLOS=\
	".\iolist.cmn"\
	
# End Source File
# Begin Source File

SOURCE=.\efcheck.for
# End Source File
# Begin Source File

SOURCE=.\efexpf1.for
# End Source File
# Begin Source File

SOURCE=.\efexpfn.for
# End Source File
# Begin Source File

SOURCE=.\eflcase.for
# End Source File
# Begin Source File

SOURCE=.\efltrim.for
# End Source File
# Begin Source File

SOURCE=.\efrwrd.for
DEP_F90_EFRWR=\
	".\dcfpak.cmn"\
	".\iolist.cmn"\
	".\pakparm.cmn"\
	
# End Source File
# Begin Source File

SOURCE=.\eftimest.for
# End Source File
# Begin Source File

SOURCE=.\efucase.for
# End Source File
# Begin Source File

SOURCE=.\eibinry.for
DEP_F90_EIBIN=\
	".\iolist.cmn"\
	
# End Source File
# Begin Source File

SOURCE=.\eicutoff.for
# End Source File
# Begin Source File

SOURCE=.\enukok.for
# End Source File
# Begin Source File

SOURCE=.\eopenm.for
DEP_F90_EOPEN=\
	".\iolist.cmn"\
	
# End Source File
# Begin Source File

SOURCE=.\eorder.for
DEP_F90_EORDE=\
	".\dcfpak.cmn"\
	".\iolist.cmn"\
	".\pakparm.cmn"\
	
# End Source File
# Begin Source File

SOURCE=.\epath.for
DEP_F90_EPATH=\
	".\dcfpak.cmn"\
	".\pakparm.cmn"\
	
# End Source File
# Begin Source File

SOURCE=.\epfnum.for
DEP_F90_EPFNU=\
	".\DEVICE.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\Epfrad.for
DEP_F90_EPFRA=\
	".\DEVICE.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\epfrcp.for
DEP_F90_EPFRC=\
	".\DEVICE.CMN"\
	".\OPT.CMN"\
	".\RCPINFO.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\erecver.for
DEP_F90_ERECV=\
	".\dcfpak.cmn"\
	".\iolist.cmn"\
	".\pakparm.cmn"\
	
# End Source File
# Begin Source File

SOURCE=.\Exptest.for
DEP_F90_EXPTE=\
	".\DEVICE.CMN"\
	".\EXPNAM.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\ezero.for
DEP_F90_EZERO=\
	".\dcfpak.cmn"\
	".\pakparm.cmn"\
	
# End Source File
# Begin Source File

SOURCE=.\Filini.for
# End Source File
# Begin Source File

SOURCE=.\Flux.cmn
# End Source File
# Begin Source File

SOURCE=.\Getint.for
DEP_F90_GETIN=\
	".\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=.\Getlog.for
DEP_F90_GETLO=\
	".\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=.\Getnam.for
DEP_F90_GETNA=\
	".\DEVICE.CMN"\
	".\FNAMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\Getreal.for
DEP_F90_GETRE=\
	".\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=.\Getset.for
DEP_F90_GETSE=\
	".\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=.\Getstr.for
DEP_F90_GETST=\
	".\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=.\health1.for
DEP_F90_HEALT=\
	".\ALLPAR.CMN"\
	".\DEVICE.CMN"\
	".\FNAMES.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RADATA.CMN"\
	".\RCPINFO.CMN"\
	".\SETF.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\Hyphen.for
# End Source File
# Begin Source File

SOURCE=.\Lenword.for
# End Source File
# Begin Source File

SOURCE=.\Markin.for
DEP_F90_MARKI=\
	".\DEVICE.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\Moveset.for
# End Source File
# Begin Source File

SOURCE=.\Moveto.for
DEP_F90_MOVET=\
	".\PARMTR.PAR"\
	
# End Source File
# Begin Source File

SOURCE=.\new.for
DEP_F90_NEW_F=\
	".\ALLPAR.CMN"\
	".\dcfpak.cmn"\
	".\DEVICE.CMN"\
	".\FGR13D.CMN"\
	".\FNAMES.CMN"\
	".\OPT.CMN"\
	".\pakparm.cmn"\
	".\PARMTR.PAR"\
	".\RADATA.CMN"\
	".\RCPINFO.CMN"\
	".\SETF.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\old.for
DEP_F90_OLD_F=\
	".\ALLPAR.CMN"\
	".\DEVICE.CMN"\
	".\FNAMES.CMN"\
	".\OPT.CMN"\
	".\PARMTR.PAR"\
	".\RADATA.CMN"\
	".\RCPINFO.CMN"\
	".\SETF.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\Redcas.for
DEP_F90_REDCA=\
	".\ALLPAR.CMN"\
	".\FGR13D.CMN"\
	".\OPT.CMN"\
	".\RADATA.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\rifhead.for
DEP_F90_RIFHE=\
	".\DEVICE.CMN"\
	".\OPT.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\Riskcal2.for
DEP_F90_RISKC=\
	".\ALLPAR.CMN"\
	".\DEVICE.CMN"\
	".\EXPNAM.CMN"\
	".\pakparm.cmn"\
	".\SETF.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\Riskcalc.for
DEP_F90_RISKCA=\
	".\DERMFAC.CMN"\
	".\DEVICE.CMN"\
	".\EXPNAM.CMN"\
	".\SETF.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\seqi.for
# End Source File
# Begin Source File

SOURCE=.\Setfac.for
DEP_F90_SETFA=\
	".\ALLPAR.CMN"\
	".\DFACTR.CMN"\
	".\OPT.CMN"\
	".\SETF.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\Setfac2.for
DEP_F90_SETFAC=\
	".\ALLPAR.CMN"\
	".\FACTORS.CMN"\
	".\OPT.CMN"\
	".\SETF.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\upperc.for
# End Source File
# Begin Source File

SOURCE=.\Xref.for
# End Source File
# End Target
# End Project
