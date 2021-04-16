# Microsoft Developer Studio Project File - Name="Radcon" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

CFG=Radcon - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "Radcon.mak".
!MESSAGE 
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "Radcon.mak" CFG="Radcon - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "Radcon - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "Radcon - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
RSC=rc.exe

!IF  "$(CFG)" == "Radcon - Win32 Release"

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
# ADD BASE F90 /compile_only /nologo /warn:nofileopt
# ADD F90 /compile_only /nologo /warn:nofileopt
# ADD BASE CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD CPP /nologo /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /c
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /machine:I386
# Begin Special Build Tool
SOURCE="$(InputPath)"
PostBuild_Cmds=copy Release\radcon.exe .
# End Special Build Tool

!ELSEIF  "$(CFG)" == "Radcon - Win32 Debug"

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
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /browser /check:bounds /compile_only /dbglibs /debug:full /fpp /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /YX /FD /GZ /c
# ADD CPP /nologo /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_CONSOLE" /D "_MBCS" /FR /YX /FD /GZ /c
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:console /incremental:no /debug /machine:I386 /pdbtype:sept

!ENDIF 

# Begin Target

# Name "Radcon - Win32 Release"
# Name "Radcon - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=.\AFLUX.FOR
# End Source File
# Begin Source File

SOURCE=.\AVG70Y.FOR
DEP_F90_AVG70=\
	".\CTIME.WTN"\
	".\INDEX3.WTN"\
	".\INTER.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\AWPL.FOR
# End Source File
# Begin Source File

SOURCE=.\BLANEY.FOR
# End Source File
# Begin Source File

SOURCE=.\BRUTE.FOR
# End Source File
# Begin Source File

SOURCE=.\CHECKS.FOR
DEP_F90_CHECK=\
	".\ERROR.WTN"\
	".\INDEX1.WTN"\
	".\INDEX2.WTN"\
	".\INDEX4.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\COMPAR.FOR
DEP_F90_COMPA=\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\CONC.FOR
DEP_F90_CONC_=\
	".\CONC1.WTN"\
	".\CTIME.WTN"\
	".\ERROR.WTN"\
	".\INDEX1.WTN"\
	".\INDEX2.WTN"\
	".\INDEX9.WTN"\
	".\isv.wtn"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\CONTAM.FOR
DEP_F90_CONTA=\
	".\CAP.WTN"\
	".\COM21.WTN"\
	".\CTIME.WTN"\
	".\DUMMY.WTN"\
	".\ERROR.WTN"\
	".\INDEX1.WTN"\
	".\INDEX2.WTN"\
	".\INDEX3.WTN"\
	".\INDEX4.WTN"\
	".\INDEX7.WTN"\
	".\INDEX9.WTN"\
	".\INIT.WTN"\
	".\INTER.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\CORPEN.FOR
# End Source File
# Begin Source File

SOURCE=.\COUPLE.FOR
DEP_F90_COUPL=\
	".\COM21.WTN"\
	".\CTIME.WTN"\
	".\INDEX1.WTN"\
	".\INDEX3.WTN"\
	".\INDEX5.WTN"\
	".\INDEX6.WTN"\
	".\INTER.WTN"\
	".\LIST.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\DECAY.FOR
DEP_F90_DECAY=\
	".\CAP.WTN"\
	".\ERROR.WTN"\
	".\INDEX3.WTN"\
	".\INDEX4.WTN"\
	".\INTER.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\DUMMYV.FOR
DEP_F90_DUMMY=\
	".\DUMMY.WTN"\
	".\INDEX1.WTN"\
	".\INDEX2.WTN"\
	".\INDEX3.WTN"\
	".\INDEX4.WTN"\
	".\INIT.WTN"\
	".\INPUT.WTN"\
	".\MAXIMUM.WTN"\
	".\MEDIA.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\EXPOSR.FOR
DEP_F90_EXPOS=\
	".\INDEX5.WTN"\
	".\INIT.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\FAOPET.FOR
DEP_F90_FAOPE=\
	".\INDEX3.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\FIND.FOR
DEP_F90_FIND_=\
	".\COM21.WTN"\
	".\CTIME.WTN"\
	".\DUMMY.WTN"\
	".\ERROR.WTN"\
	".\masss.wtn"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\FLXTABLE.FOR
DEP_F90_FLXTA=\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\FUNCTION.FOR
DEP_F90_FUNCT=\
	".\CTIME.WTN"\
	".\DUMMY.WTN"\
	".\ERROR.WTN"\
	".\INDEX1.WTN"\
	".\INDEX2.WTN"\
	".\INDEX3.WTN"\
	".\INDEX4.WTN"\
	".\INTER.WTN"\
	".\masss.wtn"\
	".\MAXIMUM.WTN"\
	".\MEDIA.WTN"\
	".\welldist.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\GATHER.FOR
DEP_F90_GATHE=\
	".\CTIME.WTN"\
	".\INDEX3.WTN"\
	".\INDEX4.WTN"\
	".\LIST.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\HOLDP.FOR
DEP_F90_HOLDP=\
	".\CTIME.WTN"\
	".\INDEX3.WTN"\
	".\INDEX4.WTN"\
	".\INTER.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\INITIN.FOR
DEP_F90_INITI=\
	".\CONC1.WTN"\
	".\CTIME.WTN"\
	".\ERROR.WTN"\
	".\EXTFLUX.WTN"\
	".\INDEX1.WTN"\
	".\INDEX2.WTN"\
	".\INDEX3.WTN"\
	".\INDEX4.WTN"\
	".\INDEX8.WTN"\
	".\INDEX9.WTN"\
	".\INIT.WTN"\
	".\INTER.WTN"\
	".\MAXIMUM.WTN"\
	".\MEDIA.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\INPUTM.FOR
DEP_F90_INPUT=\
	".\COM21.WTN"\
	".\CTIME.WTN"\
	".\DILUTION.WTN"\
	".\ERROR.WTN"\
	".\EXTFLUX.WTN"\
	".\INDEX1.WTN"\
	".\INDEX2.WTN"\
	".\INDEX3.WTN"\
	".\INDEX4.WTN"\
	".\INDEX6.WTN"\
	".\INDEX7.WTN"\
	".\INDEX8.WTN"\
	".\INDEX9.WTN"\
	".\INPUT.WTN"\
	".\INTER.WTN"\
	".\MAXIMUM.WTN"\
	".\MEDIA.WTN"\
	".\VLEA1.WTN"\
	".\welldist.WTN"\
	".\WETLAN.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\INTERP.FOR
DEP_F90_INTER=\
	".\CAP.WTN"\
	".\CTIME.WTN"\
	".\INDEX3.WTN"\
	".\INDEX4.WTN"\
	".\INDEX7.WTN"\
	".\INTER.WTN"\
	".\masss.wtn"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\KLIMIT1.FOR
DEP_F90_KLIMI=\
	".\COM21.WTN"\
	".\CTIME.WTN"\
	".\DUMMY.WTN"\
	".\ERROR.WTN"\
	".\INDEX4.WTN"\
	".\masss.wtn"\
	".\MAXIMUM.WTN"\
	".\MEDIA.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\KSIMPS.FOR
DEP_F90_KSIMP=\
	".\CTIME.WTN"\
	".\INDEX1.WTN"\
	".\INDEX4.WTN"\
	".\masss.wtn"\
	".\MAXIMUM.WTN"\
	".\MEDIA.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\L_INTERP.FOR
# End Source File
# Begin Source File

SOURCE=.\LEACH1.FOR
DEP_F90_LEACH=\
	".\EXTFLUX.WTN"\
	".\INDEX1.WTN"\
	".\INDEX3.WTN"\
	".\INDEX4.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\LEACHG.FOR
DEP_F90_LEACHG=\
	".\INDEX3.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\LEACHV.FOR
DEP_F90_LEACHV=\
	".\CONC1.WTN"\
	".\ERROR.WTN"\
	".\EXTFLUX.WTN"\
	".\INDEX1.WTN"\
	".\INDEX2.WTN"\
	".\INDEX3.WTN"\
	".\INIT.WTN"\
	".\INTER.WTN"\
	".\isv.wtn"\
	".\MAXIMUM.WTN"\
	".\tech.wtn"\
	".\VLEA1.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\LIMIT1.FOR
DEP_F90_LIMIT=\
	".\COM21.WTN"\
	".\CTIME.WTN"\
	".\DUMMY.WTN"\
	".\ERROR.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\LIMITS.FOR
DEP_F90_LIMITS=\
	".\COM21.WTN"\
	".\CTIME.WTN"\
	".\DUMMY.WTN"\
	".\ERROR.WTN"\
	".\INDEX1.WTN"\
	".\INDEX2.WTN"\
	".\INDEX3.WTN"\
	".\INDEX4.WTN"\
	".\MAXIMUM.WTN"\
	".\WETLAN.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\MAIN.FOR
# End Source File
# Begin Source File

SOURCE=.\MASS.FOR
DEP_F90_MASS_=\
	".\CAP.WTN"\
	".\INDEX3.WTN"\
	".\INTER.WTN"\
	".\masss.wtn"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\MASSBAL.FOR
DEP_F90_MASSB=\
	".\CTIME.WTN"\
	".\DUMMY.WTN"\
	".\INDEX1.WTN"\
	".\INDEX2.WTN"\
	".\INDEX3.WTN"\
	".\INDEX9.WTN"\
	".\INIT.WTN"\
	".\INTER.WTN"\
	".\masss.wtn"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\MAXTABLE.FOR
DEP_F90_MAXTA=\
	".\INDEX8.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\MEASUR.FOR
DEP_F90_MEASU=\
	".\COM21.WTN"\
	".\CTIME.WTN"\
	".\DUMMY.WTN"\
	".\ERROR.WTN"\
	".\INDEX1.WTN"\
	".\INDEX2.WTN"\
	".\INDEX3.WTN"\
	".\INDEX4.WTN"\
	".\INDEX6.WTN"\
	".\INDEX7.WTN"\
	".\INDEX9.WTN"\
	".\INIT.WTN"\
	".\INTER.WTN"\
	".\LIST.WTN"\
	".\MAXIMUM.WTN"\
	".\MEDIA.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\MERGE.FOR
DEP_F90_MERGE=\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\MESSAGE.FOR
# End Source File
# Begin Source File

SOURCE=.\MOUNDING.FOR
# End Source File
# Begin Source File

SOURCE=.\MULREC.FOR
DEP_F90_MULRE=\
	".\COM21.WTN"\
	".\CTIME.WTN"\
	".\DUMMY.WTN"\
	".\ERROR.WTN"\
	".\EXTFLUX.WTN"\
	".\INDEX1.WTN"\
	".\INDEX2.WTN"\
	".\INDEX3.WTN"\
	".\INDEX4.WTN"\
	".\INDEX5.WTN"\
	".\INDEX6.WTN"\
	".\INDEX8.WTN"\
	".\INDEX9.WTN"\
	".\INTER.WTN"\
	".\LIST.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\MYINTERP.FOR
DEP_F90_MYINT=\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\ORIGIN.FOR
DEP_F90_ORIGI=\
	".\COM21.WTN"\
	".\CTIME.WTN"\
	".\INDEX1.WTN"\
	".\INDEX3.WTN"\
	".\INDEX4.WTN"\
	".\INDEX8.WTN"\
	".\INDEX9.WTN"\
	".\INIT.WTN"\
	".\INTER.WTN"\
	".\masss.wtn"\
	".\MAXIMUM.WTN"\
	".\MEDIA.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\OUTPUT.FOR
DEP_F90_OUTPU=\
	".\ERROR.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\PENMAN.FOR
# End Source File
# Begin Source File

SOURCE=.\PFLUX.FOR
# End Source File
# Begin Source File

SOURCE=.\PLUME.FOR
# End Source File
# Begin Source File

SOURCE=.\PNTTIM.FOR
DEP_F90_PNTTI=\
	".\CONC1.WTN"\
	".\ERROR.WTN"\
	".\INDEX1.WTN"\
	".\INDEX2.WTN"\
	".\INDEX3.WTN"\
	".\INTER.WTN"\
	".\isv.wtn"\
	".\MAXIMUM.WTN"\
	".\tech.wtn"\
	
# End Source File
# Begin Source File

SOURCE=.\PRECIP.FOR
# End Source File
# Begin Source File

SOURCE=.\QTRAP.FOR
# End Source File
# Begin Source File

SOURCE=.\RADCOND.FOR
DEP_F90_RADCO=\
	".\CAP.WTN"\
	".\COM21.WTN"\
	".\CONC1.WTN"\
	".\CTIME.WTN"\
	".\DUMMY.WTN"\
	".\ERROR.WTN"\
	".\EXTFLUX.WTN"\
	".\INDEX1.WTN"\
	".\INDEX2.WTN"\
	".\INDEX3.WTN"\
	".\INDEX4.WTN"\
	".\INDEX5.WTN"\
	".\INDEX6.WTN"\
	".\INDEX7.WTN"\
	".\INDEX8.WTN"\
	".\INDEX9.WTN"\
	".\INIT.WTN"\
	".\INPUT.WTN"\
	".\INTER.WTN"\
	".\isv.wtn"\
	".\ITE10.WTN"\
	".\LIST.WTN"\
	".\masss.wtn"\
	".\MAXIMUM.WTN"\
	".\MEDIA.WTN"\
	".\tech.wtn"\
	".\VLEA1.WTN"\
	".\WETLAN.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\REFINE.FOR
# End Source File
# Begin Source File

SOURCE=.\RISKCONC.FOR
DEP_F90_RISKC=\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\RUNVOL.FOR
# End Source File
# Begin Source File

SOURCE=.\S_TERM.FOR
DEP_F90_S_TER=\
	".\CAP.WTN"\
	".\COM21.WTN"\
	".\EXTFLUX.WTN"\
	".\INDEX3.WTN"\
	".\INDEX9.WTN"\
	".\INIT.WTN"\
	".\INTER.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\SCENAR.FOR
DEP_F90_SCENA=\
	".\CAP.WTN"\
	".\INDEX3.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\SEDOVR.FOR
# End Source File
# Begin Source File

SOURCE=.\SOURCE.FOR
DEP_F90_SOURC=\
	".\CONC1.WTN"\
	".\CTIME.WTN"\
	".\ERROR.WTN"\
	".\INDEX1.WTN"\
	".\INDEX2.WTN"\
	".\INDEX3.WTN"\
	".\INTER.WTN"\
	".\MAXIMUM.WTN"\
	".\VLEA1.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\SSGW.FOR
DEP_F90_SSGW_=\
	".\CTIME.WTN"\
	".\DUMMY.WTN"\
	".\ERROR.WTN"\
	".\INDEX1.WTN"\
	".\INDEX2.WTN"\
	".\INDEX3.WTN"\
	".\INDEX4.WTN"\
	".\INDEX7.WTN"\
	".\INDEX8.WTN"\
	".\INIT.WTN"\
	".\INPUT.WTN"\
	".\INTER.WTN"\
	".\MAXIMUM.WTN"\
	".\MEDIA.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\SSWAP.FOR
DEP_F90_SSWAP=\
	".\ERROR.WTN"\
	".\INDEX1.WTN"\
	".\INDEX2.WTN"\
	".\INDEX3.WTN"\
	".\INDEX4.WTN"\
	".\INDEX6.WTN"\
	".\INDEX8.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\STORAG.FOR
# End Source File
# Begin Source File

SOURCE=.\STWL.FOR
DEP_F90_STWL_=\
	".\INDEX3.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\TABLE1.FOR
# End Source File
# Begin Source File

SOURCE=.\TRAPZD.FOR
# End Source File
# Begin Source File

SOURCE=.\VAPORP.FOR
# End Source File
# Begin Source File

SOURCE=.\WETLND.FOR
DEP_F90_WETLN=\
	".\ERROR.WTN"\
	".\EXTFLUX.WTN"\
	".\INDEX2.WTN"\
	".\INDEX3.WTN"\
	".\INIT.WTN"\
	".\MAXIMUM.WTN"\
	".\WETLAN.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\WLINIT.FOR
DEP_F90_WLINI=\
	".\INDEX3.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\WLIST.FOR
DEP_F90_WLIST=\
	".\COM21.WTN"\
	".\CTIME.WTN"\
	".\INDEX1.WTN"\
	".\INDEX3.WTN"\
	".\INDEX4.WTN"\
	".\LIST.WTN"\
	".\MAXIMUM.WTN"\
	".\MEDIA.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\WRITDATA.FOR
DEP_F90_WRITD=\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\WRITHEDR.FOR
# End Source File
# Begin Source File

SOURCE=.\WUNITS.FOR
DEP_F90_WUNIT=\
	".\CAP.WTN"\
	".\ERROR.WTN"\
	".\INDEX5.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\XLINE.FOR
# End Source File
# Begin Source File

SOURCE=.\XZPNT.FOR
# End Source File
# Begin Source File

SOURCE=.\YLINE.FOR
# End Source File
# Begin Source File

SOURCE=.\YMIXAREA.FOR
# End Source File
# Begin Source File

SOURCE=.\YMIXPNT.FOR
# End Source File
# Begin Source File

SOURCE=.\YZAVE.FOR
DEP_F90_YZAVE=\
	".\INDEX3.WTN"\
	".\INDEX4.WTN"\
	".\MAXIMUM.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\YZPNT.FOR
# End Source File
# Begin Source File

SOURCE=.\ZLINE.FOR
DEP_F90_ZLINE=\
	".\ERROR.WTN"\
	
# End Source File
# Begin Source File

SOURCE=.\ZMIXPNT.FOR
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
