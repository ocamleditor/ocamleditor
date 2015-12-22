@echo off
setlocal

set GTK=C:\OCPWin32\gtk\bin
set OCAML=C:\OCPWin32\bin
set GIT=C:\Program Files\Git\bin
set DOT=C:\Program Files (x86)\Graphviz2.36\bin
set WINDOWS=C:\WINDOWS\system32;C:\WINDOWS;C:\WINDOWS\System32\Wbem

set PATH=%WINDOWS%;%OCAML%;%GTK%;%DOT%;%GIT%

.\ocamleditor.exe

endlocal
