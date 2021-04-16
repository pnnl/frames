# Microsoft Developer Studio Project File - Name="pufchron" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 5.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=pufchron - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "pufchron.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "pufchron.mak" CFG="pufchron - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "pufchron - Win32 Release" (based on\
 "Win32 (x86) Console Application")
!MESSAGE "pufchron - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "pufchron - Win32 Release"

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
CPP=cl.exe
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE F90 /include:"Release/" /compile_only /nologo /warn:nofileopt
# ADD F90 /include:"Release/" /compile_only /nologo
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib /nologo /subsystem:console /machine:I386

!ELSEIF  "$(CFG)" == "pufchron - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
CPP=cl.exe
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD BASE F90 /include:"Debug/" /compile_only /nologo /debug:full /check:bounds /warn:argument_checking /warn:nofileopt
# ADD F90 /include:"Debug/" /compile_only /nologo /debug:full /check:bounds /fpscomp:logicals /fpscomp:general /warn:argument_checking /fpscomp:filesfromcmd /fpscomp:ioformat /fpscomp:symbols /check:power
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

# Name "pufchron - Win32 Release"
# Name "pufchron - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=..\Boccdist.for
# End Source File
# Begin Source File

SOURCE=..\Boccsig.for
# End Source File
# Begin Source File

SOURCE=..\Bucdist.for
# End Source File
# Begin Source File

SOURCE=..\Bucsig.for
# End Source File
# Begin Source File

SOURCE=..\C_dkfact.for
DEP_F90_C_DKF=\
	"..\decay.inc"\
	"..\nuc_data.inc"\
	"..\parm.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Cg_dose.for
DEP_F90_CG_DO=\
	"..\depos.inc"\
	"..\matrix.inc"\
	"..\nuc_data.inc"\
	"..\parm.inc"\
	"..\puffs.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Chain.for
DEP_F90_CHAIN=\
	"..\decay.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\checkmet.for
DEP_F90_CHECK=\
	"..\metinput.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Clcpfsig.for
# End Source File
# Begin Source File

SOURCE=..\Clean2.for
DEP_F90_CLEAN=\
	"..\depos.inc"\
	"..\nuc_data.inc"\
	"..\parm.inc"\
	"..\puffs.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Combine.for
DEP_F90_COMBI=\
	"..\const.inc"\
	"..\depos.inc"\
	"..\nuc_data.inc"\
	"..\parm.inc"\
	"..\puffs.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\compstr.for
# End Source File
# Begin Source File

SOURCE=..\Corrustr.for
# End Source File
# Begin Source File

SOURCE=..\Cubicslv.for
# End Source File
# Begin Source File

SOURCE=..\Ddepvel.for
# End Source File
# Begin Source File

SOURCE=..\Difdep.for
DEP_F90_DIFDE=\
	"..\const.inc"\
	"..\depos.inc"\
	"..\difter.inc"\
	"..\met_data.inc"\
	"..\nuc_data.inc"\
	"..\parm.inc"\
	"..\pf_shine.inc"\
	"..\puffs.inc"\
	"..\shine.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Dk_puffs.for
DEP_F90_DK_PU=\
	"..\depos.inc"\
	"..\nuc_data.inc"\
	"..\parm.inc"\
	"..\puffs.inc"\
	"..\rel.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Domvarin.for
DEP_F90_DOMVA=\
	"..\const.inc"\
	"..\files.inc"\
	"..\met_data.inc"\
	"..\parm.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Dosvdist.for
DEP_F90_DOSVD=\
	"..\const.inc"\
	"..\depos.inc"\
	"..\difter.inc"\
	"..\met_data.inc"\
	"..\nuc_data.inc"\
	"..\parm.inc"\
	"..\pf_shine.inc"\
	"..\puffs.inc"\
	"..\shine.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Fnplmrs.for
# End Source File
# Begin Source File

SOURCE=..\Getfname.for
DEP_F90_GETFN=\
	"..\files.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Invmol.for
# End Source File
# Begin Source File

SOURCE=..\Isc2dist.for
# End Source File
# Begin Source File

SOURCE=..\Isc2sig.for
# End Source File
# Begin Source File

SOURCE=..\Nsig.for
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

SOURCE=..\Off_grid.for
# End Source File
# Begin Source File

SOURCE=..\Outp_int.for
DEP_F90_OUTP_=\
	"..\const.inc"\
	"..\matrix.inc"\
	"..\nuc_data.inc"\
	"..\parm.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Pre_puff.for
DEP_F90_PRE_P=\
	"..\const.inc"\
	"..\depos.inc"\
	"..\difter.inc"\
	"..\met_data.inc"\
	"..\parm.inc"\
	"..\puffs.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Prgstatt.for
DEP_F90_PRGST=\
	"..\files.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Profile.for
# End Source File
# Begin Source File

SOURCE=..\Pufchron.for
DEP_F90_PUFCH=\
	"..\const.inc"\
	"..\files.inc"\
	"..\metinput.inc"\
	"..\nuc_data.inc"\
	"..\parm.inc"\
	"..\puffs.inc"\
	"..\rel.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Puff_eff.for
DEP_F90_PUFF_=\
	"..\const.inc"\
	"..\depos.inc"\
	"..\difter.inc"\
	"..\matrix.inc"\
	"..\met_data.inc"\
	"..\parm.inc"\
	"..\puffs.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Puffcs.for
DEP_F90_PUFFC=\
	"..\const.inc"\
	"..\difter.inc"\
	"..\matrix.inc"\
	"..\parm.inc"\
	"..\pf_shine.inc"\
	"..\puffs.inc"\
	"..\shine.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Puffm.for
DEP_F90_PUFFM=\
	"..\const.inc"\
	"..\met_data.inc"\
	"..\parm.inc"\
	"..\puffs.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\puffout.for
DEP_F90_PUFFO=\
	"..\const.inc"\
	"..\files.inc"\
	"..\matrix.inc"\
	"..\nuc_data.inc"\
	"..\parm.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Puffr.for
DEP_F90_PUFFR=\
	"..\const.inc"\
	"..\depos.inc"\
	"..\met_data.inc"\
	"..\nuc_data.inc"\
	"..\parm.inc"\
	"..\puffs.inc"\
	"..\rel.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Puffsigz.for
# End Source File
# Begin Source File

SOURCE=..\Pufshine.for
DEP_F90_PUFSH=\
	"..\parm.inc"\
	"..\pf_shine.inc"\
	"..\shine.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Pufsigy.for
# End Source File
# Begin Source File

SOURCE=..\Qinterp.for
# End Source File
# Begin Source File

SOURCE=..\R2quad.for
DEP_F90_R2QUA=\
	"..\parm.inc"\
	"..\shine.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\rdnuclpf.for
DEP_F90_RDNUC=\
	"..\const.inc"\
	"..\depos.inc"\
	"..\nuc_data.inc"\
	"..\parm.inc"\
	"..\rel.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\rdrspuff.for
DEP_F90_RDRSP=\
	"..\const.inc"\
	"..\depos.inc"\
	"..\difter.inc"\
	"..\files.inc"\
	"..\parm.inc"\
	"..\puffs.inc"\
	"..\rel.inc"\
	"..\shine.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\READMET.for
DEP_F90_READM=\
	"..\metinput.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Setlvel.for
# End Source File
# Begin Source File

SOURCE=..\setpfmet.for
DEP_F90_SETPF=\
	"..\met_data.inc"\
	"..\metinput.inc"\
	"..\parm.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Shineint.for
# End Source File
# Begin Source File

SOURCE=..\Shineprp.for
DEP_F90_SHINE=\
	"..\parm.inc"\
	"..\shine.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Timestep.for
DEP_F90_TIMES=\
	"..\const.inc"\
	"..\difter.inc"\
	"..\met_data.inc"\
	"..\parm.inc"\
	"..\puffs.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Turbsigv.for
# End Source File
# Begin Source File

SOURCE=..\Turbsigw.for
# End Source File
# Begin Source File

SOURCE=..\Ustar.for
# End Source File
# Begin Source File

SOURCE=..\Verdist.for
DEP_F90_VERDI=\
	"..\const.inc"\
	"..\difter.inc"\
	"..\met_data.inc"\
	"..\parm.inc"\
	"..\puffs.inc"\
	
# End Source File
# Begin Source File

SOURCE=..\Wdep_i2.for
# End Source File
# Begin Source File

SOURCE=..\Wdeppart.for
# End Source File
# Begin Source File

SOURCE=..\writhead.for
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# End Target
# End Project
