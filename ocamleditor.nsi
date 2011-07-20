;  OCamlEditor
;  Copyright (C) 2010, 2011 Francesco Tovagliari
;
;  This file is part of OCamlEditor.
;
;  OCamlEditor is free software: you can redistribute it and/or modify
;  it under the terms of the GNU General Public License as published by
;  the Free Software Foundation, either version 3 of the License, or
;  (at your option) any later version.
;
;  OCamlEditor is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License
;  along with this program. If not, see <http://www.gnu.org/licenses/>.

Name "OCamlEditor"
OutFile "ocamleditor-1.6.2-setup.exe"
InstallDir $PROGRAMFILES\OCamlEditor

; Registry key to check for directory (so if you install again, it will 
; overwrite the old one automatically)
InstallDirRegKey HKLM "Software\OCamlEditor" "Install_Dir"

LicenseText "Please read the following License Agreement. You must accept the terms of this agreement before continuing with the installation."
LicenseData "COPYING"

; Request application privileges for Windows Vista
RequestExecutionLevel Admin

; Pages
;--------------------------------

Page license
Page components
Page directory
Page instfiles

UninstPage uninstConfirm
UninstPage instfiles

;--------------------------------

; The stuff to install
Section "OCamlEditor (required)"
  ; Set output path to the installation directory.
  SetOutPath $INSTDIR\bin
  File "src\ocamleditor.bat"
  File "src\ocamleditor.exe"
	File "src\oeproc\oeproc.exe"   
	File "src\oebuild\oebuild.exe"   
	File /NonFatal "src\oeproc\oeproc.opt.exe"   
	File /NonFatal "src\oebuild\oebuild.opt.exe"   
  SetOutPath $INSTDIR
  ; Put file there
  File "COPYING"
	File /r ".\pixmaps"
  ; Write the installation path into the registry
  WriteRegStr HKLM SOFTWARE\OCamlEditor "Install_Dir" "$INSTDIR"
  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\OCamlEditor" "DisplayName" "OCamlEditor"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\OCamlEditor" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\OCamlEditor" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\OCamlEditor" "NoRepair" 1
  WriteUninstaller "uninstall.exe"
  
SectionEnd

; Optional section (can be disabled by the user)
Section "Start Menu Shortcuts"
	SetShellVarContext All
	StrCpy $1 $DESKTOP
  CreateDirectory "$SMPROGRAMS\OCamlEditor"
  SetOutPath $INSTDIR\bin
  CreateShortCut "$SMPROGRAMS\OCamlEditor\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
  CreateShortCut "$SMPROGRAMS\OCamlEditor\OCamlEditor.lnk" "$INSTDIR\bin\ocamleditor.bat" "" "$INSTDIR\pixmaps\oe.ico" 0 SW_SHOWMINIMIZED
  CreateShortCut "$DESKTOP\OCamlEditor.lnk" "$INSTDIR\bin\ocamleditor.bat" "" "$INSTDIR\pixmaps\oe.ico" 0 SW_SHOWMINIMIZED 
  
SectionEnd

; Uninstaller
;--------------------------------

Section "Uninstall"
  ; Remove registry keys
  DeleteRegKey HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\OCamlEditor"
  DeleteRegKey HKLM SOFTWARE\OCamlEditor
  ; Remove shortcuts, if any
	SetShellVarContext All
	StrCpy $1 $DESKTOP
  Delete "$DESKTOP\OCamlEditor.lnk"
  Delete "$SMPROGRAMS\OCamlEditor\*.*"
  RMDir /R "$SMPROGRAMS\OCamlEditor"
  ; Remove files and uninstaller
  RMDir /r $INSTDIR
SectionEnd


