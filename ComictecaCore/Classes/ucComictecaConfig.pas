unit ucComictecaConfig;

{< cComictecaConfig class unit.

  This file is part of Comicteca Core.

  Copyright (C) 2019 Chixpy

  This source is free software; you can redistribute it and/or modify it under
  the terms of the GNU General Public License as published by the Free
  Software Foundation; either version 3 of the License, or (at your option)
  any later version.

  This code is distributed in the hope that it will be useful, but WITHOUT ANY
  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
  FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
  details.

  A copy of the GNU General Public License is available on the World Wide Web
  at <http://www.gnu.org/copyleft/gpl.html>. You can also obtain it by writing
  to the Free Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
  MA 02111-1307, USA.
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles, FileUtil, LazFileUtils, LazUTF8,
  // CHX units
  uCHXStrUtils,
  // CHX abtracts
  uaCHXConfig;

const
  // [Tools]
  krsIniSecTools = 'Tools';
  krsIniKey7zCMExecutable = '7zCMExecutable';
  krsIniKey7zGExecutable = '7zGExecutable';

  // [Extensions]
  krsIniSecExtensions = 'Extensions';
  krsIniKeyCompressedExtensions = 'CompressedExtensions';

  // [Temp]
  krsIniSecTemp = 'Temp';
  krsIniKeyTempSubfolder = 'TempSubfolder';
  krsIniKeyTempFile = 'TempFile';

type

  { cComictecaConfig }
  cComictecaConfig = class(caCHXConfig)
  private
    FCompressedExtensions: TStringList;
    FTempFile: string;
    FTempSubfolder: string;
    Fz7CMExecutable: string;
    Fz7GExecutable: string;
    procedure SetTempFile(const AValue: string);
    procedure SetTempSubfolder(const AValue: string);
    procedure Setz7CMExecutable(const AValue: string);
    procedure Setz7GExecutable(const AValue: string);

  public
    procedure LoadFromIni(aIniFile: TMemIniFile); override;
    procedure ResetDefaultConfig; override;
    {< Sets config properties to default values. }

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveToIni(aIniFile: TMemIniFile); override;

  published
    // Tools
    property z7CMExecutable: string read Fz7CMExecutable
      write Setz7CMExecutable;
    {< 7z.exe path. }
    property z7GExecutable: string read Fz7GExecutable write Setz7GExecutable;
    {< 7zG.exe path. }

    // File extensions
    property CompressedExtensions: TStringList read FCompressedExtensions;
    {< List of compressed file extensions. }

    // Temp folder/file
    property TempSubfolder: string read FTempSubfolder write SetTempSubfolder;
    {< Subfolder for temporal files.

      It can be a full path folder, or if a relative path is stored
        a subfolder for system's temporal folder. }
    property TempFile: string read FTempFile write SetTempFile;
    {< Temp filename...

      TODO: Not used...}

  end;

implementation

{ cComictecaConfig }

procedure cComictecaConfig.Setz7CMExecutable(const AValue: string);
begin
  Fz7CMExecutable := SetAsFile(AValue);
end;

procedure cComictecaConfig.SetTempFile(const AValue: string);
begin
  FTempFile := SetAsFile(AValue);
end;

procedure cComictecaConfig.SetTempSubfolder(const AValue: string);
begin
  FTempSubFolder := SetAsFolder(AValue);
end;

procedure cComictecaConfig.Setz7GExecutable(const AValue: string);
begin
  Fz7GExecutable := SetAsFile(AValue);
end;

procedure cComictecaConfig.LoadFromIni(aIniFile: TMemIniFile);
begin
  // Tools
  z7CMExecutable := aIniFile.ReadString(krsIniSecTools,
    krsIniKey7zCMExecutable, z7CMExecutable);
  z7GExecutable := aIniFile.ReadString(krsIniSecTools,
    krsIniKey7zGExecutable, z7GExecutable);

  // File extensions
  CompressedExtensions.CommaText :=
    Trim(UTF8LowerCase(aIniFile.ReadString(krsIniSecExtensions,
    krsIniKeyCompressedExtensions, CompressedExtensions.CommaText)));

  // Temp
  TempSubfolder := aIniFile.ReadString(krsIniSecTemp,
    krsIniKeyTempSubfolder, TempSubfolder);
  TempFile := aIniFile.ReadString(krsIniSecTemp,
    krsIniKeyTempFile, TempFile);
end;

procedure cComictecaConfig.ResetDefaultConfig;
begin
  // Tools
  z7CMExecutable := 'Tools/7zip/7z.exe';
  z7GExecutable := 'Tools/7zip/7zG.exe';

  { CompressedExtensions.CommaText := w7zGetFileExts;

    Removed ext: 001,cab,chm,dll,iso,img,jar,msi,swf,ntfs,ppt,doc,,xls,xpi,vhd,
      deb,rpm,cpio,cramfs,dmg,fat,flv,mbr,nsis,sys,bpl,hfs,hxi,hxq,hxr,hxs,
      chi,chq,chw,hxw,msp,scap,squashfs,swm,wim,exe,lit,xar,xz,z,lzma,lzma86,
      r00,tar,taz,tbz,tbz2,tgz,tpz,txz,gz,gzip,lha,lzh,bz2,bzip2,
  }
  CompressedExtensions.CommaText := '7z,arj,rar,zip,cb7,cba,cbr,cbz';

  // Temp
  TempSubfolder := 'Comicteca/';
  TempFile := 'Comicteca.tmp';
end;

constructor cComictecaConfig.Create(aOwner: TComponent);
begin
  FCompressedExtensions := TStringList.Create;
  CompressedExtensions.Sorted := True;
  CompressedExtensions.CaseSensitive := False;

  inherited Create(aOwner);
end;

destructor cComictecaConfig.Destroy;
begin
  FreeAndNil(FCompressedExtensions);

  inherited Destroy;
end;

procedure cComictecaConfig.SaveToIni(aIniFile: TMemIniFile);
begin

end;

end.
