@echo off
cd ..
set GIT=D:\Git\bin
set DOT=C:\Program Files (x86)\Graphviz2.36\bin
set GTK=%cd%\gtk\bin

set PATH=%cd%\bin;%GTK%;%DOT%;%GIT%;C:\WINDOWS\system32;C:\WINDOWS;C:\WINDOWS\System32\Wbem

set OCAMLEDITOR_MINGW=1
cd bin
ocamleditor.exe
