REM Running GENII Near Field Exposure Pathways Module
REM  Usage: Gen_NFd.bat FUIName RunName Site# Module# ModuleID
REM  where
REM    FUIName is the name of the scenario's PDCFs
REM    RunName is the run name for module's output PDCFs
REM    Site# is the index of the Site of this module
REM    Module# is the index for the instance of this module
REM    ModuleID is the FRAMES object id for this module

WrapSpec.exe in 1 %1 %2 %3 %4 %5

Nearfield.exe %1 %2 %3 %4 %5

del %1.scf
copy %1xx.scf %1.scf
del %1xx.scf

IF NOT EXIST %2.err WrapSpec.exe out %1 %2 %3 %4 %5
