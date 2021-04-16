@echo off
rem
rem Usage
rem    areash.bat shellprog args
rem
rem where
rem
rem    shellprog is the program to shell
rem    args are the command line arguments for the shelled program
rem
rem
echo.
echo Running %1 %2 %3 %4 %5 %6 %7 %8 
echo.
START /w %1 %2 %3
echo.
echo Done with %1 %2 %3 %4 %5 %6 %7 %8 
echo.
if not exist %5.ato goto bad

:Good
   echo Successful completion.
   goto Done

:Bad
   echo *** ERROR ***
   echo ATO file not created > %4
   goto Done

:Done
rem pause
rem Remove all temp files
rem To debug - remark out the next four lines
del %5.rs
del %5.nuc
del %5.lst
del store.eq
