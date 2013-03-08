@echo off
setlocal
set OEDIR=%cd%
call "%ProgramFiles(x86)%\Microsoft Visual Studio 9.0\VC\bin\vcvars32.bat"
rem SET FLEXDLL_RELOCATE=00401000
cd %OEDIR%
ocamleditor.exe
endlocal