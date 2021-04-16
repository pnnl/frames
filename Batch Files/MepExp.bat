REM Running MEPAS Exposure Pathways Module
REM  Usage: MepExp.bat FUIName RunName Site# Module# ModuleID
REM  where
REM    FUIName is the name of the scenario's PDCFs
REM    RunName is the run name for module's output PDCFs
REM    Site# is the index of the Site of this module
REM    Module# is the index for the instance of this module
REM    ModuleID is the FRAMES object id for this module

AHAZX.exe %1 %2 %3 %4 %5
IF EXIST %2.err GOTO ERROR
WrapSpec.exe out %1 %2 %3 %4 %5
:ERROR
REM The followings lines can be remmed out for debuging
COPY %2.ELS %2.msg
DEL %2.ELS
DEL %2.ATM
DEL %2.ETM
