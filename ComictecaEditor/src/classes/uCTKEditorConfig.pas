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
// CHX abstracts
  uaCHXConfig;

type

  { cCTKEditorConfig }

  cCTKEditorConfig = class(caCHXConfig)
  private

  public

    procedure LoadFromIni(aIniFile: TMemIniFile); override;
    procedure ResetDefaultConfig; override;
    procedure SaveToIni(aIniFile: TMemIniFile); override;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ cCTKEditorConfig }

procedure cCTKEditorConfig.LoadFromIni(aIniFile: TMemIniFile);
begin

end;

procedure cCTKEditorConfig.ResetDefaultConfig;
begin

end;

procedure cCTKEditorConfig.SaveToIni(aIniFile: TMemIniFile);
begin

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

