REM Running MEPAS Secondary Source Term Module
REM  Usage: MepSSrc.bat FUIName RunName Site# Module# ModuleID
REM  where
REM    FUIName is the name of the scenario's PDCFs
REM    RunName is the run name for module's output PDCFs
REM    Site# is the index of the Site of this module
REM    Module# is the index for the instance of this module
REM    ModuleID is the FRAMES object id for this module

secsrc.exe %1 %2 %3 %4 %5
strm1.exe %1 %2 %3 %4 %5
IF EXIST %2.err GOTO END
IF NOT EXIST %2.aff GOTO END
DEL %2.err
affTrim %2.aff 0.01
IF EXIST %2.err GOTO END
WrapSpec.exe out %1 %2 %3 %4 %5
DEL %2.err
:END

