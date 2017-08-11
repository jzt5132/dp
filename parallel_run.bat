timeout 4000
@echo off
set RRUN= C:\r\R-3.3.2\bin\x64\Rscript.exe
set RJOB= \\40278DFWYM13\Na\map.R


CALL %RRUN% %RJOB% 


timeout 4000
@echo off