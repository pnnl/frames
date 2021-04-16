REM Running MEPAS Receptor Intakes Module
pause
PREHAZ %1 %2 %3 %4 %5
AHAZI %1 %2 %3 %4 %5
REM The followings lines can be remmed out for debuging
DEL FACIL.ID
DEL %2.CHM
DEL %2.RLS