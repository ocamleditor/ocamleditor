@echo off
setlocal
set OEDIR=%cd%

if exist "%ProgramFiles(x86)%\Microsoft Visual Studio 12.0\VC\bin\vcvars32.bat" (
  call "%ProgramFiles(x86)%\Microsoft Visual Studio 12.0\VC\bin\vcvars32.bat"
) ELSE (
  if exist "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\bin\vcvars32.bat" (
    call "%ProgramFiles(x86)%\Microsoft Visual Studio 11.0\VC\bin\vcvars32.bat"
  ) ELSE (
    if exist "%ProgramFiles(x86)%\Microsoft Visual Studio 10.0\VC\bin\vcvars32.bat" (
      call "%ProgramFiles(x86)%\Microsoft Visual Studio 10.0\VC\bin\vcvars32.bat"
    ) ELSE (
      if exist "%ProgramFiles(x86)%\Microsoft Visual Studio 9.0\VC\bin\vcvars32.bat" (
        call "%ProgramFiles(x86)%\Microsoft Visual Studio 9.0\VC\bin\vcvars32.bat"
      )
    )
  )
)

rem SET FLEXDLL_RELOCATE=00401000
CD "%OEDIR%"
ocamleditor.exe
endlocal

