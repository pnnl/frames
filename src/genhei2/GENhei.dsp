# Microsoft Developer Studio Project File - Name="GENhei" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=GENhei - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "GENhei.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "GENhei.mak" CFG="GENhei - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "GENhei - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "GENhei - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "GENhei - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "GENhei__"
# PROP BASE Intermediate_Dir "GENhei__"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "GENhei__"
# PROP Intermediate_Dir "GENhei__"
# PROP Target_Dir ""
# ADD BASE F90 /include:"GENhei__/" /compile_only /nologo
# ADD F90 /include:"GENhei__/" /compile_only /nologo
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "GENhei - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "GENhei_0"
# PROP BASE Intermediate_Dir "GENhei_0"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "GENhei_0"
# PROP Intermediate_Dir "GENhei_0"
# PROP Target_Dir ""
# ADD BASE F90 /include:"GENhei_0/" /compile_only /nologo /debug:full /optimize:0
# ADD F90 /include:"GENhei_0/" /compile_only /nologo /debug:full /optimize:0
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

# Name "GENhei - Win32 Release"
# Name "GENhei - Win32 Debug"
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
	".\parmtr.par"\
	".\RCPINFO.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\dfset.for
DEP_F90_DFSET=\
	".\batch.cmn"\
	".\dcfpak.cmn"\
	".\FACTORS.CMN"\
	".\iolist.cmn"\
	".\pakparm.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\Doscof.for
DEP_F90_DOSCO=\
	".\batch.cmn"\
	".\dcfpak.cmn"\
	".\iolist.cmn"\
	".\pakparm.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\ebatman.for
DEP_F90_EBATM=\
	".\iolist.cmn"\
	".\pakparm.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\ebirch.for
DEP_F90_EBIRC=\
	".\dcfpak.cmn"\
	".\pakparm.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\eblock.for
DEP_F90_EBLOC=\
	".\dcfpak.cmn"\
	".\pakparm.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\echain.for
DEP_F90_ECHAI=\
	".\batch.cmn"\
	".\dcfpak.cmn"\
	".\iolist.cmn"\
	".\pakparm.CMN"\
	
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
	".\pakparm.CMN"\
	
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
	".\pakparm.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\epath.for
DEP_F90_EPATH=\
	".\dcfpak.cmn"\
	".\pakparm.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\epfnum.for
DEP_F90_EPFNU=\
	".\DEVICE.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\EPFRAD.FOR
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
	".\pakparm.CMN"\
	
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
	".\pakparm.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\FILINI.FOR
# End Source File
# Begin Source File

SOURCE=.\GETINT.FOR
DEP_F90_GETIN=\
	".\parmtr.par"\
	
# End Source File
# Begin Source File

SOURCE=.\GETLOG.FOR
DEP_F90_GETLO=\
	".\parmtr.par"\
	
# End Source File
# Begin Source File

SOURCE=.\Getnam.for
DEP_F90_GETNA=\
	".\DEVICE.CMN"\
	".\FNAMES.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\GETREAL.FOR
DEP_F90_GETRE=\
	".\parmtr.par"\
	
# End Source File
# Begin Source File

SOURCE=.\GETSET.FOR
DEP_F90_GETSE=\
	".\parmtr.par"\
	
# End Source File
# Begin Source File

SOURCE=.\GETSTR.FOR
DEP_F90_GETST=\
	".\parmtr.par"\
	
# End Source File
# Begin Source File

SOURCE=.\health1.for
DEP_F90_HEALT=\
	".\ALLPAR.CMN"\
	".\DEVICE.CMN"\
	".\FNAMES.CMN"\
	".\OPT.CMN"\
	".\parmtr.par"\
	".\RADATA.CMN"\
	".\RCPINFO.CMN"\
	".\SETF.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\HYPHEN.FOR
# End Source File
# Begin Source File

SOURCE=.\LENWORD.FOR
# End Source File
# Begin Source File

SOURCE=.\Markin.for
DEP_F90_MARKI=\
	".\DEVICE.CMN"\
	
# End Source File
# Begin Source File

SOURCE=.\MOVESET.FOR
# End Source File
# Begin Source File

SOURCE=.\MOVETO.FOR
DEP_F90_MOVET=\
	".\parmtr.par"\
	
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
	".\pakparm.CMN"\
	".\parmtr.par"\
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
	".\parmtr.par"\
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
	".\pakparm.CMN"\
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

SOURCE=.\XREF.FOR
# End Source File
# End Target
# End Project
