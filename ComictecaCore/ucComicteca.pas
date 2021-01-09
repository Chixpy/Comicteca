unit ucComicteca;

{< cComicteca class unit.

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
  Classes, SysUtils, FileUtil, LazFileUtils,
  // CHX units
  uCHXStrUtils, uCHX7zWrapper,
  // Comicteca Core classes
  ucComictecaConfig, ucComictecaVolume;

type

  { cComicteca }

  cComicteca = class(TComponent)
  private
    FBaseFolder: string;
    FConfig: cComictecaConfig;
    FTempFolder: string;
    procedure SetBaseFolder(const AValue: string);
    procedure SetTempFolder(const AValue: string);

  public

    procedure LoadConfig(const aFile: string);

    function LoadComicFile(const aFile: string): cComictecaVolume;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  published
    property BaseFolder: string read FBaseFolder write SetBaseFolder;
    {< Base folder for relative config paths. }

    property TempFolder: string read FTempFolder write SetTempFolder;

    property Config: cComictecaConfig read FConfig;
    {< Config component. }
  end;

implementation

{ cComicteca }

procedure cComicteca.SetBaseFolder(const AValue: string);
begin
  if FBaseFolder = AValue then
    Exit;
  FBaseFolder := AValue;
end;

procedure cComicteca.SetTempFolder(const AValue: string);
begin
  FTempFolder := SetAsFolder(AValue);
end;

procedure cComicteca.LoadConfig(const aFile: string);
begin
  Config.LoadFromFile(aFile); // if empty, then last config file.

  // Temp folder
  if FilenameIsAbsolute(Config.TempSubfolder) then
    TempFolder := Config.TempSubfolder
  else
    TempFolder := CreateAbsoluteSearchPath(Config.TempSubfolder, GetTempDir);
  ForceDirectories(TempFolder);

  // 7z executables
  if FilenameIsAbsolute(Config.z7CMExecutable) then
    w7zSetPathTo7zexe(Config.z7CMExecutable)
  else
    w7zSetPathTo7zexe(CreateAbsoluteSearchPath(Config.z7CMExecutable,
      GetCurrentDirUTF8));
  if FilenameIsAbsolute(Config.z7GExecutable) then
    w7zSetPathTo7zGexe(Config.z7GExecutable)
  else
    w7zSetPathTo7zGexe(CreateAbsoluteSearchPath(Config.z7GExecutable,
      GetCurrentDirUTF8));
end;

function cComicteca.LoadComicFile(const aFile: string): cComictecaVolume;
var
  IsFolder, NewDir: boolean;
  aFolder: string;
  CompError: integer;
begin

  Result := nil;

  IsFolder := DirectoryExistsUTF8(aFile);
  aFolder := aFile;
  NewDir := False;

  if not IsFolder then
  begin
    if not FileExistsUTF8(aFile) then
      Exit;
    // Decompress file
    aFolder := SetAsFolder(aFolder + ExtractFileNameOnly(aFile));

    NewDir := not DirectoryExistsUTF8(aFolder);
    if NewDir then
      ForceDirectoriesUTF8(aFolder);

    CompError := w7zExtractFile(aFile, AllFilesMask, aFolder, True, '');
  end;

  if CompError > 1 then
    Exit;

  // Open Folder
  Result := cComictecaVolume.Create(nil);

end;

constructor cComicteca.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  // Creating components
  FConfig := cComictecaConfig.Create(Self);
end;

destructor cComicteca.Destroy;
begin

  // Deleting temp folder
  // TODO: Crappy segurity check... :-(
  if (Length(TempFolder) > Length(Config.TempSubfolder) + 5) and
    DirectoryExistsUTF8(TempFolder) then
    DeleteDirectory(TempFolder, False);

  Config.SaveToFile('', True);

  FConfig.Free;
  inherited Destroy;
end;

initialization
  RegisterClass(cComicteca);

finalization
  UnRegisterClass(cComicteca);

end.
