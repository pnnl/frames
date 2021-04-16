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
 %1 %2 %3 %4 %5 %6 %7 %8
echo.
echo Done with %1 %2 %3 %4 %5 %6 %7 %8 
echo.
if ERRORLEVEL 1 goto Bad

:Good
   echo Successful completion.
   echo.
rem   wait.exe
rem echo EDPSSH.BAT > %1okay.$$$
rem  echo Done with %2 %3 %4 %5 %6 %7 %8 %9 >> %1okay.$$$
rem echo Good Run >> %1okay.$$$
   goto Done

:Bad
   echo *** ERROR ***
   echo.

rem   wait.exe
rem   echo EDPSSH.BAT > %1error.$$$
rem   echo Done with %2 %3 %4 %5 %6 %7 %8 %9 >> %1error.$$$
rem   echo *** ERROR *** >> %1error.$$$
   goto Done

:Done
pause
