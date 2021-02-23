unit uaComictecaText;

{< caComictecaText abstact class unit.

  This file is part of Comicteca Core.

  Copyright (C) 2021 Chixpy

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
  Classes, SysUtils, Laz2_DOM, laz2_XMLRead, Laz2_XMLWrite,
  // CHX units
  uCHXRecordHelpers, uCHXStrUtils,
  // Comicteca Core units
  uCTKConst, uCTKCommon,
  // Comicteca Core class
  ucComictecaTextMap;

type

  { caComictecaText }

  caComictecaText = class(TComponent)
  private
    FContent: cComictecaTextMap;

  public
    Rect: TRect;

    property Content: cComictecaTextMap read FContent;

    procedure LoadFromXML(aXMLNode: TDOMElement); virtual;
    procedure SaveToXML(aXMLDoc: TXMLDocument; aXMLNode: TDOMElement); virtual;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{ caComictecaText }

procedure caComictecaText.LoadFromXML(aXMLNode: TDOMElement);
begin
  if not Assigned(aXMLNode) then
    Exit;

  Rect.FromString(aXMLNode[krsCTKXMLRect]);

  // Content
  Content.LoadFromXML(aXMLNode);
end;

procedure caComictecaText.SaveToXML(aXMLDoc: TXMLDocument;
  aXMLNode: TDOMElement);
begin
  if (not Assigned(aXMLNode)) or (not Assigned(aXMLDoc)) then
    Exit;

  aXMLNode[krsCTKXMLRect] := Rect.ToString;

  // Content
  Content.SaveToXML(aXMLDoc, aXMLNode);
end;

constructor caComictecaText.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FContent := cComictecaTextMap.Create(True);
end;

destructor caComictecaText.Destroy;
begin
  FContent.Free;

  inherited Destroy;
end;

initialization
  RegisterClass(caComictecaText);

finalization
  UnRegisterClass(caComictecaText);

end.

