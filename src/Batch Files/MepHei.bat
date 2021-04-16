REM Running MEPAS Health Impacts Module
REM  Usage: MepHei.bat FUIName RunName Site# Module# ModuleID
REM  where
REM    FUIName is the name of the scenario's PDCFs
REM    RunName is the run name for module's output PDCFs
REM    Site# is the index of the Site of this module
REM    Module# is the index for the instance of this module
REM    ModuleID is the FRAMES object id for this module

AHAZH %1 %2 %3 %4 %5
IF EXIST %2.err GOTO ERROR
:ERROR
REM The followings lines can be remmed out for debuging
COPY %2.HLS %2.msg
DEL %2.HLS
