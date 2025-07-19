;  OCamlEditor
;  Copyright (C) 2010-2014 Francesco Tovagliari
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

;	!include "MUI2.nsh"
;!include FontRegAdv.nsh
;!include FontName.nsh

Name "OCamlEditor"
OutFile "OCamlEditor-1.18.0-ocaml414-Windows.exe"
InstallDir $PROGRAMFILES\OCamlEditor
InstallDirRegKey HKLM "Software\OCamlEditor" "Install_Dir"
#!define GTK_RUNTIME C:\devel\GTK2-Runtime
!define GTK_RUNTIME C:\devel\GTK-2.24.10-Runtime

;; Request application privileges for Windows Vista
RequestExecutionLevel Admin

;  !define MUI_HEADERIMAGE
;  !define MUI_HEADERIMAGE_BITMAP "nsis.bmp" ; optional
;  ;!define MUI_ABORTWARNING
;
;  !insertmacro MUI_PAGE_LICENSE "COPYING"
;  !insertmacro MUI_PAGE_COMPONENTS
;  !insertmacro MUI_PAGE_DIRECTORY
;  !insertmacro MUI_PAGE_INSTFILES
;  
;  !insertmacro MUI_UNPAGE_CONFIRM
;  !insertmacro MUI_UNPAGE_INSTFILES
;  !insertmacro MUI_LANGUAGE "English"

VIProductVersion "1.18.0-ocaml414.0"
VIAddVersionKey "ProductName" "OCamlEditor"
VIAddVersionKey "LegalCopyright" "Copyright � 2010-2012 Francesco Tovagliari"
VIAddVersionKey "FileDescription" "OCamlEditor Setup"
VIAddVersionKey "FileVersion" "1.18.0-ocaml414"
;Icon "share\ocamleditor\icons\ocamleditor.ico"

LicenseText "Please read the following License Agreement. You must accept the terms of this agreement before continuing with the installation."
LicenseData "COPYING"

; Pages
;--------------------------------

Page license
Page components
Page directory
Page instfiles
;
;UninstPage uninstConfirm
;UninstPage instfiles

;--------------------------------

Section "OCamlEditor (required)"
  SetOutPath $INSTDIR\bin
  File "src\ocamleditor.bat"
  !system 'if exist src\ocamleditor.opt.exe ren src\ocamleditor.exe ocamleditor.tmp'
  !system 'if exist src\ocamleditor.opt.exe ren src\ocamleditor.opt.exe ocamleditor.exe'
  File "src\ocamleditor.exe" 
  !system 'if exist src\ocamleditor.tmp ren src\ocamleditor.exe ocamleditor.opt.exe'
  !system 'if exist src\ocamleditor.tmp ren src\ocamleditor.tmp ocamleditor.exe'
  File "src\oebuild\oebuild.exe"   
  File /NonFatal "src\oebuild\oebuild.opt.exe"   
  File /NonFatal "src\ocamleditorw.exe"   

  SetOutPath $INSTDIR
  RMDir /r "$INSTDIR\pixmaps"; Remove old directory
  File /oname=README.txt "README"
  File /oname=NEWS.txt "NEWS"
  File /oname=COPIYNG.txt "COPYING"

  SetOutPath $INSTDIR\share\ocamleditor\icons
  File "src\ocamleditor.ico" 
  SetOutPath $INSTDIR\share\ocamleditor\icons
	File "icons\*.*"
  SetOutPath $INSTDIR\share\ocamleditor\plugins
	File /NonFatal "plugins\*.*"

  SetOutPath $INSTDIR\share
  ; Write the installation path into the registry
  WriteRegStr HKLM SOFTWARE\OCamlEditor "Install_Dir" "$INSTDIR"
  ; Write the uninstall keys for Windows
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\OCamlEditor" "DisplayIcon" '"$INSTDIR\bin\ocamleditor.exe"'
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\OCamlEditor" "DisplayName" "OCamlEditor"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\OCamlEditor" "DisplayVersion" "1.18.0-ocaml414"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\OCamlEditor" "UninstallString" '"$INSTDIR\uninstall.exe"'
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\OCamlEditor" "InstallLocation" '"$INSTDIR"'
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\OCamlEditor" "HelpLink" "http://ocamleditor.forge.ocamlcore.org"
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\OCamlEditor" "Readme" '"$INSTDIR\README.txt"'
  WriteRegStr HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\OCamlEditor" "Publisher" "Francesco Tovagliari"
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\OCamlEditor" "NoModify" 1
  WriteRegDWORD HKLM "Software\Microsoft\Windows\CurrentVersion\Uninstall\OCamlEditor" "NoRepair" 1
  WriteUninstaller "uninstall.exe"
  
SectionEnd

Section "GTK2 Runtime Environment"
  ; Set output path to the installation directory.
  SetOutPath $INSTDIR
  File /r "${GTK_RUNTIME}\bin"
  File /r "${GTK_RUNTIME}\etc"
  File /r "${GTK_RUNTIME}\lib"
  SetOutPath $INSTDIR\bin
  File /r "D:\curl-7.29.0\bin\*.dll"
  SetOutPath $INSTDIR\share\themes
  File /r "${GTK_RUNTIME}\share\themes\*"
SectionEnd

;Section "Fonts"
;  StrCpy $FONT_DIR $FONTS
;  !insertmacro InstallTTF "fonts\cmunss.ttf"
;  !insertmacro InstallTTF "fonts\cmunsx.ttf"
;  !insertmacro InstallTTF "fonts\cmunsi.ttf"
;  !insertmacro InstallTTF "fonts\cmunso.ttf"
;SectionEnd

Section "Start Menu Shortcuts"
	SetShellVarContext All
	StrCpy $1 $DESKTOP
  CreateDirectory "$SMPROGRAMS\OCamlEditor"
  SetOutPath $INSTDIR\bin
  CreateShortCut "$SMPROGRAMS\OCamlEditor\Uninstall.lnk" "$INSTDIR\uninstall.exe" "" "$INSTDIR\uninstall.exe" 0
  CreateShortCut "$SMPROGRAMS\OCamlEditor\OCamlEditor.lnk" "$INSTDIR\bin\ocamleditor.bat" "" "$INSTDIR\bin\ocamleditor.exe" 0 SW_SHOWMINIMIZED
  CreateShortCut "$SMPROGRAMS\OCamlEditor\OCamlEditor_x86.lnk" "$INSTDIR\bin\ocamleditor_x86.bat" "" "$INSTDIR\bin\ocamleditor.exe" 0 SW_SHOWMINIMIZED
  CreateShortCut "$DESKTOP\OCamlEditor.lnk" "$INSTDIR\bin\ocamleditor.bat" "" "$INSTDIR\bin\ocamleditor.exe" 0 SW_SHOWMINIMIZED 
  
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


