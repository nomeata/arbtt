[Setup]
; NOTE: The value of AppId uniquely identifies this application.
; Do not use the same AppId value in installers for other applications.
; (To generate a new GUID, click Tools | Generate GUID inside the IDE.)
AppId={{1DB6EA4F-D387-432D-A739-283E0E916AF6}
AppName=arbtt
;AppVerName=arbtt-0.4.4
#include "dist\setup-app-ver-name.iss"
AppPublisher=Joachim Breitner
AppPublisherURL=http://www.joachim-breitner.de/projects#arbtt
AppSupportURL=http://www.joachim-breitner.de/projects#arbtt
AppUpdatesURL=http://www.joachim-breitner.de/projects#arbtt
DefaultDirName={pf}\arbtt
DefaultGroupName=arbtt
AllowNoIcons=yes
OutputBaseFilename=arbtt-setup
Compression=lzma
SolidCompression=yes
; Is there a point in displaying the LICENSE file?
; LicenseFile=LICENSE
InfoBeforeFile=README.Win32
ChangesEnvironment=yes

[Tasks]
Name: modifypath; Description: "Add arbtt binaries to the system path"
Name: autorun; Description: "Start to capture data upon system start"
Name: runcapture; Description: "Start to capture data after the installation"

[Files]
Source: "dist\build\arbtt-dump\arbtt-dump.exe"; DestDir: "{app}\bin"
Source: "dist\build\arbtt-stats\arbtt-stats.exe"; DestDir: "{app}\bin"
Source: "dist\build\arbtt-capture\arbtt-capture.exe"; DestDir: "{app}\bin"
Source: "dist\build\arbtt-recover\arbtt-recover.exe"; DestDir: "{app}\bin"
Source: "C:\Programme\GnuWin32\bin\pcre3.dll"; DestDir: "{app}\bin"
Source: "categorize.cfg"; DestDir: "{userappdata}\arbtt"; Flags: onlyifdoesntexist uninsneveruninstall
Source: "doc\users_guide\*.*"; DestDir: "{app}\doc";

[Icons]
Name: "{group}\Edit categorize.cfg"; Filename: "wordpad.exe"; Parameters: """{userappdata}\arbtt\categorize.cfg"""; Flags: useapppaths
Name: "{group}\{cm:UninstallProgram,arbtt}"; Filename: "{uninstallexe}"
Name: "{group}\arbtt documentation"; Filename: "{app}\doc\index.html"
Name: "{commonstartup}\arbtt-capture"; Filename: "{app}\bin\arbtt-capture.exe"; Comment: "Collects data for computer useage statistics"; Tasks: autorun

[Run]
Filename: "{app}\bin\arbtt-capture.exe"; Description: "Start collecting usage data"; Flags: nowait; Tasks: runcapture

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"
Name: "german"; MessagesFile: "compiler:Languages\German.isl"

[Code]
function ModPathDir(): TArrayOfString;
	var
	Dir:	TArrayOfString;
	begin
	setArrayLength(Dir, 1)
	Dir[0] := ExpandConstant('{app}\bin');
	Result := Dir;
end;
#include "modpath.iss"

