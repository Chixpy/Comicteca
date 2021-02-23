unit ucComictecaPageList;

{< cComictecaPageList class unit.

  Copyright (C) 2020-2021 Chixpy

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
  Classes, SysUtils, fgl, Laz2_DOM, laz2_XMLRead, Laz2_XMLWrite,
  // Comiteca Core units
  uCTKConst,
  // Comiteca Core classes
  ucComictecaPage;

type

  cComictecaGenPageList = specialize TFPGObjectList<cComictecaPage>;

  { cComictecaPageList }

  cComictecaPageList = class(cComictecaGenPageList)
  public
  public
    function PageBySHA1(aSHA1: string): cComictecaPage;

    procedure LoadFromXML(Parent: TDOMElement); virtual;
    procedure SaveToXML(aXMLDoc: TXMLDocument; Parent: TDOMElement); virtual;
  end;

implementation

{ cComictecaPageList }

function cComictecaPageList.PageBySHA1(aSHA1: string): cComictecaPage;
var
  i: integer;
  aPage: cComictecaPage;
begin
  Result := nil;

  i := 0;
  while (i < Count) and (not assigned(Result)) do
  begin
    aPage := Items[i];
    if aPage.MatchSHA1(aSHA1) then
      Result := aPage;
    Inc(i);
  end;
end;

procedure cComictecaPageList.LoadFromXML(Parent: TDOMElement);
var
  PageNode: TDOMElement;
  PageList: TDOMNodeList;
  aPage: cComictecaPage;
  i: integer;
begin
  if not Assigned(Parent) then
    Exit;

  PageList := Parent.GetElementsByTagName(krsCTKXMLPage);

  i := 0;
  while i < PageList.Count do
  begin
    aPage := cComictecaPage.Create(nil);
    aPage.LoadFromXML(TDOMElement(PageList[i]));
    self.Add(aPage);
    Inc(i);
  end;
end;

procedure cComictecaPageList.SaveToXML(aXMLDoc: TXMLDocument;
  Parent: TDOMElement);
var
  i: integer;
  XMLPage: TDOMElement;
  aPage: cComictecaPage;
begin
  if (not Assigned(Parent)) or (not Assigned(aXMLDoc)) then
    Exit;

  i := 0;
  while i < Count do
  begin
    aPage := Items[i];

    XMLPage := aXMLDoc.CreateElement(krsCTKXMLPage);
    aPage.SaveToXML(aXMLDoc, XMLPage);
    Parent.AppendChild(XMLPage);
    Inc(i);
  end;
end;

//initialization
//  RegisterClass(cComictecaPageList);

//finalization
//  UnRegisterClass(cComictecaPageList);

end.
