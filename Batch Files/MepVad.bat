REM Running MEPAS Vadose Zone Module
REM  Usage: MepVad.bat FUIName RunName Site# Module# ModuleID
REM  where
REM    FUIName is the name of the scenario's PDCFs
REM    RunName is the run name for module's output PDCFs
REM    Site# is the index of the Site of this module
REM    Module# is the index for the instance of this module
REM    ModuleID is the FRAMES object id for this module

ECHO OFF
ECHO Error MEPAS Vadose Zone Module > %2.err
ECHO ON
PreVadose %1 %2 %3 %4 %5
IF ERRORLEVEL 1 GOTO ERROR
RADCON.exe
IF ERRORLEVEL 1 GOTO ERROR
IF EXIST MEPAS_VZ.err GOTO ERROR
GOTO OK
:ERROR
IF EXIST MEPAS_VZ.err COPY MEPAS_VZ.err %2.err
IF EXIST MEPAS_VZ.wls COPY MEPAS_VZ.wls %2.wls
IF EXIST MEPAS_VZ.wrn COPY MEPAS_VZ.wrn %2.wrn
IF EXIST MEPAS_VZ.win COPY MEPAS_VZ.win %2.win
DEL MEPAS_VZ.err
GOTO END
:OK
IF EXIST MEPAS_VZ.wff COPY MEPAS_VZ.wff %2.wff
IF EXIST MEPAS_VZ.wcf COPY MEPAS_VZ.wcf %2.wcf
IF EXIST MEPAS_VZ.wls COPY MEPAS_VZ.wls %2.wls
IF EXIST MEPAS_VZ.wrn COPY MEPAS_VZ.wrn %2.wrn
IF EXIST MEPAS_VZ.win COPY MEPAS_VZ.win %2.win
WrapSpec.exe out %1 %2 %3 %4 %5
DEL %2.err
:END
DEL FACIL.ID
DEL MEPAS_VZ.dbg
DEL MEPAS_VZ.wff
DEL MEPAS_VZ.wcf
DEL MEPAS_VZ.wls
DEL MEPAS_VZ.wrn
DEL MEPAS_VZ.pol
DEL MEPAS_VZ.win