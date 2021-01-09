unit ucCTKGUIConfig;
{< cCTKGUIConfig class unit.

  This file is part of Comicteca GUI.

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
  Classes, SysUtils, IniFiles,
// CHX abstracts
  uaCHXConfig;

type

  { cCTKGUIConfig }

  cCTKGUIConfig = class(caCHXConfig)
  private

  public

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromIni(aIniFile: TMemIniFile); override;
    procedure ResetDefaultConfig; override;
    procedure SaveToIni(aIniFile: TMemIniFile); override;
  end;

implementation

{ cCTKGUIConfig }

constructor cCTKGUIConfig.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor cCTKGUIConfig.Destroy;
begin
  inherited Destroy;
end;

procedure cCTKGUIConfig.LoadFromIni(aIniFile: TMemIniFile);
begin

end;

procedure cCTKGUIConfig.ResetDefaultConfig;
begin

end;

procedure cCTKGUIConfig.SaveToIni(aIniFile: TMemIniFile);
begin

end;

end.

