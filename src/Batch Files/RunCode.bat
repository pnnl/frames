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
rem    Delete error file

If exist code.err del code.err

rem     Create error file

ECHO   %1 > code.err

rem    Delete error file 

IF exist progstat.err del progstat.err

echo.
echo Run Program %1 %2 %3 %4 %5 %6 %7 %8 
echo.
%1 %2 %3 %4 %5 %6 %7 %8  
rem >> code.err


echo.
echo Done with %1 %2 %3 %4 %5 %6 %7 %8 
echo.

if ERRORLEVEL 1 goto Bad

:Good
   echo Successful completion.
   echo.
   If exist progstat.err type progstat.err >> code.err 
   if exist progstat.err Goto Bad
   del code.err
   goto Done

:Bad
   echo *** ERROR ***
   echo.

rem   wait.exe
   goto Done

:Done
rem pause
