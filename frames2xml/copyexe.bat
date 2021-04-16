REM check mode and copy respective exe

if NOT DEFINED FRAMESDIR SET FRAMESDIR=C:\program files\FramesV2
SET EXE=frames2xml.exe

if /I %12 == debug2   copy /Y "debug\%EXE%" "%FRAMESDIR%\%EXE%"
if /I %12 == release2 copy /Y "release\%EXE%" "%FRAMESDIR%\%EXE%"

pause
