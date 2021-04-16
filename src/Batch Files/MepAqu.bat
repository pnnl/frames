REM Running MEPAS Aquifer Module
REM  Usage: MepAqu.bat FUIName RunName Site# Module# ModuleID
REM  where
REM    FUIName is the name of the scenario's PDCFs
REM    RunName is the run name for module's output PDCFs
REM    Site# is the index of the Site of this module
REM    Module# is the index for the instance of this module
REM    ModuleID is the FRAMES object id for this module

ECHO OFF
ECHO Error MEPAS Aquifer Module > %2.err
ECHO ON
PreAquifer.exe %1 %2 %3 %4 %5
IF ERRORLEVEL 1 GOTO ERROR
RADCON.exe
IF ERRORLEVEL 1 GOTO ERROR
IF EXIST MEPAS_AQ.err GOTO ERROR
GOTO OK
:ERROR
IF EXIST MEPAS_AQ.err COPY MEPAS_AQ.err %2.err
IF EXIST MEPAS_AQ.wls COPY MEPAS_AQ.wls %2.wls
IF EXIST MEPAS_AQ.wrn COPY MEPAS_AQ.wrn %2.wrn
IF EXIST MEPAS_AQ.win COPY MEPAS_AQ.win %2.win
DEL MEPAS_AQ.err
GOTO END
:OK
IF EXIST MEPAS_AQ.wff COPY MEPAS_AQ.wff %2.wff
IF EXIST MEPAS_AQ.wcf COPY MEPAS_AQ.wcf %2.wcf
IF EXIST MEPAS_AQ.wls COPY MEPAS_AQ.wls %2.wls
IF EXIST MEPAS_AQ.wrn COPY MEPAS_AQ.wrn %2.wrn
IF EXIST MEPAS_AQ.win COPY MEPAS_AQ.win %2.win
WrapSpec.exe out %1 %2 %3 %4 %5
DEL %2.err
:END
DEL FACIL.ID
DEL MEPAS_AQ.dbg
DEL MEPAS_AQ.wff
DEL MEPAS_AQ.wcf
DEL MEPAS_AQ.wls
DEL MEPAS_AQ.wrn
DEL MEPAS_AQ.pol
DEL MEPAS_AQ.mbs
DEL MEPAS_AQ.win