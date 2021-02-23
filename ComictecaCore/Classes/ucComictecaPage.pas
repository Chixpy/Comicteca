unit ucComictecaPage;

{< cComictecaPage class unit.

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
  Classes, SysUtils, Laz2_DOM, laz2_XMLRead, Laz2_XMLWrite,
  // Comicteca Core abstracts
  uaComictecaPage,
  // Comicteca Core class
  ucComictecaTextList;

type

  { cComictecaPage class.

    Defines a full physical page (usually an image).

    Stores full comic page information. }

  cComictecaPage = class(caComictecaPage)
  private
    FTexts: cComictecaTextList;

  protected

  public
    property Texts: cComictecaTextList read FTexts;

    procedure LoadFromXML(aXMLNode: TDOMElement); override;
    procedure SaveToXML(aXMLDoc: TXMLDocument; aXMLNode: TDOMElement); override;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCTKPageObjProc = procedure(aCTKPage: cComictecaPage) of object;

implementation

{ cComictecaPage }

procedure cComictecaPage.LoadFromXML(aXMLNode: TDOMElement);
var
  i: Integer;
begin
  inherited LoadFromXML(aXMLNode);

  Texts.LoadFromXML(aXMLNode);

  // Assigning current page to texts.
  i := 0;
  while i < Texts.Count do
begin
  Texts[i].Page := Self;
  Inc(i);
end;
end;

procedure cComictecaPage.SaveToXML(aXMLDoc: TXMLDocument; aXMLNode: TDOMElement
  );
begin
  inherited SaveToXML(aXMLDoc, aXMLNode);

  Texts.SaveToXML(aXMLDoc, aXMLNode);
end;

constructor cComictecaPage.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FTexts := cComictecaTextList.Create(True);
end;

destructor cComictecaPage.Destroy;
begin
  FTexts.Free;

  inherited Destroy;
end;

initialization
  RegisterClass(cComictecaPage);

finalization
  UnRegisterClass(cComictecaPage);

end.
