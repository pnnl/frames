# Microsoft Developer Studio Project File - Name="Percentile" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=Percentile - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Percentile.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Percentile.mak" CFG="Percentile - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Percentile - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "Percentile - Win32 Debug" (based on\
 "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""$/GENII/Plume/code/Percentile", KNXAAAAA"
# PROP Scc_LocalPath "."
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "Percentile - Win32 Release"

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

!ELSEIF  "$(CFG)" == "Percentile - Win32 Debug"

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

# Name "Percentile - Win32 Release"
# Name "Percentile - Win32 Debug"
# Begin Source File

SOURCE=..\AREACONC.FOR
DEP_F90_AREAC=\
	"..\conc_inp.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Areaint.for
DEP_F90_AREAI=\
	"..\conc_inp.inc"\
	"..\sigin.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Arsecint.for
DEP_F90_ARSEC=\
	"..\conc_inp.inc"\
	"..\sigin.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Arseconc.for
DEP_F90_ARSECO=\
	"..\conc_inp.inc"\
	"..\ptsrcin.inc"\
	"..\sigin.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\BOCCDIST.FOR
# End Source File
# Begin Source File

SOURCE=..\BOCCSIG.FOR
# End Source File
# Begin Source File

SOURCE=..\BUCDIST.FOR
# End Source File
# Begin Source File

SOURCE=..\BUCSIG.FOR
# End Source File
# Begin Source File

SOURCE=..\C_dkfact.for
DEP_F90_C_DKF=\
	"..\DECAY.INC"\
	"..\nuc_data.inc"\
	"..\parm.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Calcarea.for
DEP_F90_CALCA=\
	"..\conc_inp.inc"\
	"..\depos.inc"\
	"..\metdata.inc"\
	"..\parm.inc"\
	"..\ptsrcin.inc"\
	"..\sigin.inc"\
	"..\srcrec.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Calcdpl.for
DEP_F90_CALCD=\
	"..\depos.inc"\
	"..\parm.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\CALCRISE.FOR
DEP_F90_CALCR=\
	"..\iscwkin.inc"\
	"..\metdata.inc"\
	"..\parm.inc"\
	"..\srcrec.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Calcsig.for
DEP_F90_CALCS=\
	"..\sigin.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Chain.for
DEP_F90_CHAIN=\
	"..\DECAY.INC"\
	
# End Source File
# Begin Source File

SOURCE=..\checkmet.for
DEP_F90_CHECK=\
	"..\metinput.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\compstr.for
# End Source File
# Begin Source File

SOURCE=..\CUBICSLV.FOR
# End Source File
# Begin Source File

SOURCE=..\DDEPVEL.FOR
# End Source File
# Begin Source File

SOURCE=..\Depplum.for
# End Source File
# Begin Source File

SOURCE=..\Depprp.for
DEP_F90_DEPPR=\
	"..\depos.inc"\
	"..\metdata.inc"\
	"..\parm.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\DISTSIGZ.FOR
# End Source File
# Begin Source File

SOURCE=..\ENHCSIG.FOR
# End Source File
# Begin Source File

SOURCE=..\ERF1.FOR
# End Source File
# Begin Source File

SOURCE=..\FNPLMRS.FOR
# End Source File
# Begin Source File

SOURCE=..\Getfiles.for
DEP_F90_GETFI=\
	"..\files.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\INTSIMP.FOR
DEP_F90_INTSI=\
	"..\conc_inp.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\INVMOL.FOR
# End Source File
# Begin Source File

SOURCE=..\ISC2DIST.FOR
# End Source File
# Begin Source File

SOURCE=..\ISC2SIG.FOR
# End Source File
# Begin Source File

SOURCE=..\Iscwkflg.for
DEP_F90_ISCWK=\
	"..\iscwkin.inc"\
	"..\sigin.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\ISCWKSIG.FOR
# End Source File
# Begin Source File

SOURCE=..\MODSIGS.FOR
DEP_F90_MODSI=\
	"..\iscwkin.inc"\
	"..\parm.inc"\
	"..\sigin.inc"\
	"..\srcrec.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\new_rdnuclid.for
DEP_F90_NEW_R=\
	"..\depos.inc"\
	"..\nuc_data.inc"\
	"..\parm.inc"\
	"..\srcrec.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\newCalcconc.for
DEP_F90_NEWCA=\
	"..\depos.inc"\
	"..\nuc_data.inc"\
	"..\output.inc"\
	"..\parm.inc"\
	"..\sigin.inc"\
	"..\srcrec.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\newCalcpont.for
DEP_F90_NEWCAL=\
	"..\depos.inc"\
	"..\iscwkin.inc"\
	"..\metdata.inc"\
	"..\parm.inc"\
	"..\ptsrcin.inc"\
	"..\sigin.inc"\
	"..\srcrec.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\newClcshine.for
DEP_F90_NEWCL=\
	"..\nuc_data.inc"\
	"..\output.inc"\
	"..\parm.inc"\
	"..\pl_shine.inc"\
	"..\shine.inc"\
	"..\sigin.inc"\
	"..\srcrec.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Nrcdist.for
# End Source File
# Begin Source File

SOURCE=..\Nsig1.for
# End Source File
# Begin Source File

SOURCE=..\Nucdatin.for
DEP_F90_NUCDA=\
	"..\files.inc"\
	"..\nuc_data.inc"\
	"..\parm.inc"\
	"..\shine.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\OPMETFIL.for
# End Source File
# Begin Source File

SOURCE=..\Percentil.for
DEP_F90_PERCE=\
	"..\files.inc"\
	"..\metdata.inc"\
	"..\metinput.inc"\
	"..\parm.inc"\
	"..\srcrec.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\plumeshn1.for
DEP_F90_PLUME=\
	"..\nuc_data.inc"\
	"..\parm.inc"\
	"..\shine.inc"\
	"..\sigin.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\PlumeShn2.for
DEP_F90_PLUMES=\
	"..\nuc_data.inc"\
	"..\parm.inc"\
	"..\shine.inc"\
	"..\sigin.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\PLUMESIG.FOR
# End Source File
# Begin Source File

SOURCE=..\PLUMPerc.for
DEP_F90_PLUMP=\
	"..\files.inc"\
	"..\metdata.inc"\
	"..\nuc_data.inc"\
	"..\output.inc"\
	"..\parm.inc"\
	"..\srcrec.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Pntconc.for
DEP_F90_PNTCO=\
	"..\ptsrcin.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\PRE_AREA.FOR
DEP_F90_PRE_A=\
	"..\conc_inp.inc"\
	"..\metdata.inc"\
	"..\parm.inc"\
	"..\sigin.inc"\
	"..\srcrec.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Pre_plum.for
DEP_F90_PRE_P=\
	"..\iscwkin.inc"\
	"..\metdata.inc"\
	"..\parm.inc"\
	"..\sigin.inc"\
	"..\srcrec.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\PROFILE.FOR
# End Source File
# Begin Source File

SOURCE=..\Ptseconc.for
DEP_F90_PTSEC=\
	"..\ptsrcin.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Rdrsvara.for
DEP_F90_RDRSV=\
	"..\depos.inc"\
	"..\files.inc"\
	"..\metdata.inc"\
	"..\nuc_data.inc"\
	"..\parm.inc"\
	"..\shine.inc"\
	"..\srcrec.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\READMET.for
DEP_F90_READM=\
	"..\metinput.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\resetout.for
DEP_F90_RESET=\
	"..\output.inc"\
	"..\parm.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\ReSort.for
DEP_F90_RESOR=\
	"..\files.inc"\
	"..\nuc_data.inc"\
	"..\output.inc"\
	"..\parm.inc"\
	"..\srcrec.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\S1SORT2.for
# End Source File
# Begin Source File

SOURCE=..\S2SORT2.for
# End Source File
# Begin Source File

SOURCE=..\samedate.for
# End Source File
# Begin Source File

SOURCE=..\Sctorcac.for
DEP_F90_SCTOR=\
	"..\parm.inc"\
	"..\srcrec.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Setlvel.for
# End Source File
# Begin Source File

SOURCE=..\setplmet.for
DEP_F90_SETPL=\
	"..\metdata.inc"\
	"..\metinput.inc"\
	"..\output.inc"\
	"..\parm.inc"\
	"..\srcrec.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\ShnInterp.for
# End Source File
# Begin Source File

SOURCE=..\SSPLMRSE.FOR
# End Source File
# Begin Source File

SOURCE=..\TNPLMRSE.FOR
# End Source File
# Begin Source File

SOURCE=..\TRAPZD.FOR
DEP_F90_TRAPZ=\
	"..\conc_inp.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\TURBSIGV.FOR
# End Source File
# Begin Source File

SOURCE=..\TURBSIGW.FOR
# End Source File
# Begin Source File

SOURCE=..\U01.FOR
# End Source File
# Begin Source File

SOURCE=..\Ustar.for
# End Source File
# Begin Source File

SOURCE=..\Vertterm.for
# End Source File
# Begin Source File

SOURCE=..\WDEP_I2.FOR
# End Source File
# Begin Source File

SOURCE=..\WDEPPART.FOR
# End Source File
# Begin Source File

SOURCE=..\writhead.for
# End Source File
# End Target
# End Project
