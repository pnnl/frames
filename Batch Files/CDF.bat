sensvwr.exe /CDF %1 %2 %3 %4 %5
echo "Missing files " >> %2.err
del %2.CDF.png
del %1.CDF.png
if not exist C:\Progra~1\R\R-2.3.1\bin\Rterm.exe echo "R 2.3.1 not installed or Path in CDF.bat needs to be changed" >> %2.err
C:\Progra~1\R\R-2.3.1\bin\Rterm.exe --no-save < %2.R
copy %2.CDF.png %1.CDF.png
start %1.CDF.png
if exist %1.CDF.png del %2.err

