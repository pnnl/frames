REM Running FRAMES GNUPlot Air Viewer
REM  Usage: MepVad.bat FUIName RunName Site# Module# ModuleID
REM  where
REM    FUIName is the name of the scenario's PDCFs
REM    RunName is the run name for module's output PDCFs
REM    Site# is the index of the Site of this module
REM    Module# is the index for the instance of this module
REM    ModuleID is the FRAMES object id for this module

start /wait ATOConvert.exe %1 %2 %3 %4 %5
start /wait wgnuplot.exe gnuplot.scp
DEL plot*.gif
DEL gnuplot.dat
DEL gnuplot.scp
DEL airconc.csv
DEL deposit.csv
