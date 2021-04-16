sensvwr.exe /PartialR %1 %2 %3 %4 %5
echo "Missing files" >> %2.err
del %1.Scatter.png
del %2.Scatter.png
del %1.R.txt
del %2.R.txt
if not exist C:\Progra~1\R\R-2.3.1\bin\Rterm.exe echo "R 2.3.1 not installed or Path in PartialR.bat needs to be changed" >> %2.err
C:\Progra~1\R\R-2.3.1\bin\Rterm.exe --no-save < %2.R
copy %2.scatter.png %1.scatter.png
copy %2.r.txt %1.r.txt
start %1.Scatter.png
start %1.R.txt
if exist %1.r.txt del %2.err
