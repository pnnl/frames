ECHO OFF
ECHO Error MEPAS Aquifer Module > %2.ERR
ECHO ON
COPY %1.wff MEPASBAK.WFF
PrePreAqu %1 %2 %3 %4 %5
IF EXIST %2.err GOTO ERROR
PreAquiferG %1 %2 %3 %4 %5
IF ERRORLEVEL 1 GOTO ERROR
RADCON.EXE
IF ERRORLEVEL 1 GOTO ERROR
IF EXIST MEPAS_AQ.ERR GOTO ERROR
GOTO OK
:ERROR
IF EXIST MEPAS_AQ.ERR COPY MEPAS_AQ.ERR %2.ERR
IF EXIST MEPAS_AQ.WLS COPY MEPAS_AQ.WLS %2.WLS
IF EXIST MEPAS_AQ.WRN COPY MEPAS_AQ.WRN %2.WRN
DEL MEPAS_AQ.ERR
GOTO END
:OK
IF EXIST MEPAS_AQ.WFF COPY MEPAS_AQ.WFF %2.WFF
IF EXIST MEPAS_AQ.WCF COPY MEPAS_AQ.WCF %2.WCF
IF EXIST MEPAS_AQ.WLS COPY MEPAS_AQ.WLS %2.WLS
IF EXIST MEPAS_AQ.WRN COPY MEPAS_AQ.WRN %2.WRN
DEL %2.ERR
:END
DEL %1.wff
COPY MEPASBAK.WFF %1.wff
DEL MEPASBAK.WFF
DEL FACIL.ID
DEL MEPAS_AQ.DBG
DEL MEPAS_AQ.WFF
DEL MEPAS_AQ.WCF
DEL MEPAS_AQ.WLS
DEL MEPAS_AQ.WRN
DEL MEPAS_AQ.POL
DEL MEPAS_AQ.MBS
DEL MEPAS_AQ.WIN