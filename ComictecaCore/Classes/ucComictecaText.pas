unit ucComictecaText;

 {< cComictecaText class unit.

  Copyright (C) 2021 Chixpy

  This file is part of Comicteca Core.

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
  Classes, SysUtils,  Laz2_DOM, laz2_XMLRead, Laz2_XMLWrite,
  // Comiteca Core classes
  uaComictecaText, uaComictecaPage;

type

  { cComictecaText }

  cComictecaText = class(caComictecaText)
  private
    FPage: caComictecaPage;
    procedure SetPage(AValue: caComictecaPage);
  public
    property Page: caComictecaPage read FPage write SetPage;
    //< Page of the text, easy way. Not observer pattern (by now)

    procedure LoadFromXML(aXMLNode: TDOMElement); override;
    procedure SaveToXML(aXMLDoc: TXMLDocument; aXMLNode: TDOMElement);
      override;

    procedure CopyFrom(aCTKText: cComictecaText);

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCTKTextObjProc = procedure(aCTKText: cComictecaText) of object;

implementation


{ cComictecaText }

procedure cComictecaText.SetPage(AValue: caComictecaPage);
begin
  if FPage = AValue then Exit;
  FPage := AValue;
end;

procedure cComictecaText.LoadFromXML(aXMLNode: TDOMElement);
begin
  inherited LoadFromXML(aXMLNode);
end;

procedure cComictecaText.SaveToXML(aXMLDoc: TXMLDocument; aXMLNode: TDOMElement
  );
begin
  inherited SaveToXML(aXMLDoc, aXMLNode);
end;

procedure cComictecaText.CopyFrom(aCTKText: cComictecaText);
begin
  if not Assigned(aCTKText) then Exit;

  Self.Rect := aCTKText.Rect;
  Self.Page := aCTKText.Page;
end;

constructor cComictecaText.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor cComictecaText.Destroy;
begin
  inherited Destroy;
end;

end.
