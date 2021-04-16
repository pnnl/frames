REM Running MEPAS Air Module
REM  Usage: MepAir.bat FUIName RunName Site# Module# ModuleID
REM  where
REM    FUIName is the name of the scenario's PDCFs
REM    RunName is the run name for module's output PDCFs
REM    Site# is the index of the Site of this module
REM    Module# is the index for the instance of this module
REM    ModuleID is the FRAMES object id for this module

ECHO Running AirGen to Create Files > %2.err
AIRGEN.exe %1 %2 %3 %4 %5
IF ERRORLEVEL 1 GOTO BadGEN 
IF EXIST %2.err GOTO BadGEN

ECHO Running Rapscd:Transport Model > %2.err
RAPSCD.exe %5 >> %2.err
IF ERRORLEVEL 1 GOTO BadRAP
DEL %2.err

ECHO Run FrmConc:Calculate Concentration, etc. > %2.err
FRMCONC.exe %5 >> %2.err
IF ERRORLEVEL 1 GOTO BadCONC
COPY ~MEPAIR.ATO %2.ATO
DEL %2.err
GOTO DONE

:BadGEN
ECHO ERROR during execution of AIRGEN >> %2.err
GOTO DONE

:BadRAP
ECHO ERROR during execution of RAPSCD >> %2.err
GOTO DONE

:BadCONC
ECHO ERROR during execution of FRMCONC >> %2.err
GOTO DONE

:DONE
REM Delete files NOT normally saved
REM Remark out the next couple of lines to debug
DEL FACIL.ID
DEL ~MEPAIR.ato
DEL ~MEPAIR.als
DEL ~MEPAIR.met
DEL ~MEPAIR.top
DEL ~MEPAIR.sou
DEL ~MEPAIR.air
DEL ~MEPAIR.det
DEL ~MEPAIR.jfd
DEL ~MEPAIR.aff