unit uaComictecaFrame;

{< TfmCTKGUIMain frame unit.

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
  Classes, SysUtils, Laz2_DOM, laz2_XMLRead, Laz2_XMLWrite;

type

  { cComictecaVignette }

  { caComictecaFrame }

  caComictecaFrame = class(TComponent)
  private
    FRawText: TStringList;

  public

    procedure LoadFromXML(Parent: TDOMElement); virtual;
    procedure SaveToXML(aXMLDoc: TXMLDocument; Parent: TDOMElement); virtual;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published

  end;

implementation

{ caComictecaFrame }
procedure caComictecaFrame.LoadFromXML(Parent: TDOMElement);
begin

end;

procedure caComictecaFrame.SaveToXML(aXMLDoc: TXMLDocument;
  Parent: TDOMElement);
begin

end;

constructor caComictecaFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor caComictecaFrame.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(caComictecaFrame);

finalization
  UnRegisterClass(caComictecaFrame);
end.
