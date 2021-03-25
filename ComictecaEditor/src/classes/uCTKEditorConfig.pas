unit uCTKEditorConfig;
{< uCTKEditorConfig class unit.

  This file is part of Comicteca Editor.

  Copyright (C) 2020 Chixpy

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
  Classes, SysUtils, IniFiles,
  // CHX units
  uCHXStrUtils,
  // CHX abstracts
  uaCHXConfig,
  // Comicteca Editor units
  uCTKEditorConst;

type

  { cCTKEditorConfig }

  cCTKEditorConfig = class(caCHXConfig)
  private
    FLastFolder: string;
    procedure SetLastFolder(AValue: string);

  public
    procedure LoadFromIni(aIniFile: TMemIniFile); override;
    procedure ResetDefaultConfig; override;
    procedure SaveToIni(aIniFile: TMemIniFile); override;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property LastFolder: string read FLastFolder write SetLastFolder;
  end;

implementation

{ cCTKEditorConfig }

procedure cCTKEditorConfig.SetLastFolder(AValue: string);
begin
  FLastFolder := SetAsFolder(AValue);
end;

procedure cCTKEditorConfig.LoadFromIni(aIniFile: TMemIniFile);
begin
  LastFolder := aIniFile.ReadString(kIniFilesKey, kIniLastFolderKey, '');
end;

procedure cCTKEditorConfig.ResetDefaultConfig;
begin
  LastFolder := '';
end;

procedure cCTKEditorConfig.SaveToIni(aIniFile: TMemIniFile);
begin
  aIniFile.WriteString(kIniFilesKey, kIniLastFolderKey, LastFolder);
end;

constructor cCTKEditorConfig.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor cCTKEditorConfig.Destroy;
begin
  inherited Destroy;
end;

end.

