REM Running MEPAS Surface Water Module
REM  Usage: MepRiv.bat FUIName RunName Site# Module# ModuleID
REM  where
REM    FUIName is the name of the scenario's PDCFs
REM    RunName is the run name for module's output PDCFs
REM    Site# is the index of the Site of this module
REM    Module# is the index for the instance of this module
REM    ModuleID is the FRAMES object id for this module

ECHO OFF
ECHO Error MEPAS Surface Water Module > %2.err
ECHO ON
PreRiver.exe %1 %2 %3 %4 %5
IF ERRORLEVEL 1 GOTO ERROR
RADCON.exe
IF ERRORLEVEL 1 GOTO ERROR
IF EXIST MEPAS_RV.err GOTO ERROR
GOTO OK
:ERROR
IF EXIST MEPAS_RV.err COPY MEPAS_RV.err %2.err
IF EXIST MEPAS_RV.wls COPY MEPAS_RV.wls %2.wls
IF EXIST MEPAS_RV.wrn COPY MEPAS_RV.wrn %2.wrn
IF EXIST MEPAS_RV.win COPY MEPAS_RV.win %2.win
DEL MEPAS_RV.err
GOTO END
:OK
IF EXIST MEPAS_RV.wff COPY MEPAS_RV.wff %2.wff
IF EXIST MEPAS_RV.wcf COPY MEPAS_RV.wcf %2.wcf
IF EXIST MEPAS_RV.wls COPY MEPAS_RV.wls %2.wls
IF EXIST MEPAS_RV.wrn COPY MEPAS_RV.wrn %2.wrn
IF EXIST MEPAS_RV.win COPY MEPAS_RV.win %2.win
WrapSpec.exe out %1 %2 %3 %4 %5
DEL %2.err
:END
DEL FACIL.ID
DEL MEPAS_RV.dbg
DEL MEPAS_RV.wff
DEL MEPAS_RV.wcf
DEL MEPAS_RV.wls
DEL MEPAS_RV.wrn
DEL MEPAS_RV.pol
DEL MEPAS_RV.win