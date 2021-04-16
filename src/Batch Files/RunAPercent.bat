REM Running GENII Acute 95% Plume Module
REM  Usage: RunAPercent.bat FUIName RunName Site# Module# ModuleID
REM  where
REM    FUIName is the name of the scenario's PDCFs
REM    RunName is the run name for module's output PDCFs
REM    Site# is the index of the Site of this module
REM    Module# is the index for the instance of this module
REM    ModuleID is the FRAMES object id for this module

WrapSpec.exe in 1 %1 %2 %3 %4 %5
rem pause
RunAPercent.exe %1 %2 %3 %4 %5
rem pause
del %1.aff
copy %1xx.aff %1.aff
del %1xx.aff

IF NOT EXIST %2.err WrapSpec.exe out %1 %2 %3 %4 %5
