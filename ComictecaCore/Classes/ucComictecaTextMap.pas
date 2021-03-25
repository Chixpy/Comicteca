unit ucComictecaTextMap;

{< cComictecaTextMap class unit.

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
  Classes, SysUtils, fgl, Laz2_DOM, laz2_XMLRead, Laz2_XMLWrite, LazUTF8,
  // Comicteca Core units
  uCTKConst;

type
  cgComictecaTextMap = specialize TFPGMapObject<string, TStringList>;

  { cComictecaTextMap }

  cComictecaTextMap = class(cgComictecaTextMap)
  protected

  public
    procedure LoadFromXML(Parent: TDOMElement);
    procedure SaveToXML(aXMLDoc: TXMLDocument; Parent: TDOMElement);

    procedure CTMSetStr(aKey: string; const aValue: string);
    function CTMGetStr(aKey: string): string;

    procedure CTMSetSL(aKey: string; aValue: TStrings);
    function CTMGetSL(aKey: string): TStringList;

    constructor Create(AFreeObjects: boolean);
  end;


implementation

{ cComictecaTextMap }

procedure cComictecaTextMap.LoadFromXML(Parent: TDOMElement);
var
  TextNode: TDOMElement;
  TextList: TDOMNodeEnumerator;
  aKey: string;
  aText: TStringList;
begin
  if not Assigned(Parent) then
    Exit;

  TextList := Parent.GetEnumerator;
  try
    while TextList.MoveNext do
    begin
      if TextList.Current is TDOMElement then
      begin
        TextNode := TDOMElement(TextList.Current);

        aKey := TextNode.TagName;

        if not TryGetData(aKey, aText) then
        begin
          aText := TStringList.Create;
          aText.SkipLastLineBreak := True;
          Self.Add(aKey, aText);
        end;

        aText.Text := TextNode.TextContent;
      end;
    end;
  finally
    TextList.Free;
  end;

  //// Special RAW case
  //if Self.IndexOf(krsCTKXMLRAW) < 0 then
  //begin
  //  aKey := krsCTKXMLRAW;
  //  aText := TStringList.Create;
  //  aText.SkipLastLineBreak := True;
  //  aText.Text := Parent.TextContent;
  //
  //  Self.Add(aKey, aText);
  //end;
end;

procedure cComictecaTextMap.SaveToXML(aXMLDoc: TXMLDocument;
  Parent: TDOMElement);
var
  i: integer;
  XMLText: TDOMElement;
  aText: string;
begin
  if (not Assigned(Parent)) or (not Assigned(aXMLDoc)) then
    Exit;

  i := 0;
  while i < Self.Count do
  begin
    Self.Data[i].SkipLastLineBreak := True;
    aText:=Self.Data[i].Text;

    if aText <> '' then
    begin
      XMLText := aXMLDoc.CreateElement(Self.Keys[i]);
      XMLText.TextContent := aText;
      Parent.AppendChild(XMLText);
    end;

    Inc(i);
  end;
end;

procedure cComictecaTextMap.CTMSetStr(aKey: string; const aValue: string);
var
  aText: TStringList;
begin
  if aKey = '' then
    aKey := krsCTKXMLRAW;

  if not TryGetData(aKey, aText) then
  begin
    aText := TStringList.Create;
    aText.SkipLastLineBreak := True;
    Self.Add(aKey, aText);
  end;

  aText.Text := aValue;
end;

function cComictecaTextMap.CTMGetStr(aKey: string): string;
var
  aText: TStringList;
begin
  Result := '';

  if aKey = '' then
    aKey := krsCTKXMLRAW;

  if TryGetData(aKey, aText) then
    Result := aText.Text;
end;

procedure cComictecaTextMap.CTMSetSL(aKey: string; aValue: TStrings);
var
  aText: TStringList;
begin
  if aKey = '' then
    aKey := krsCTKXMLRAW;

  if not TryGetData(aKey, aText) then
  begin
    aText := TStringList.Create;
    aText.SkipLastLineBreak := True;
    Self.Add(aKey, aText);
  end;

  aText.Assign(aValue);
end;

function cComictecaTextMap.CTMGetSL(aKey: string): TStringList;
var
  aText: TStringList;
begin
  Result := nil;

  if aKey = '' then
    aKey := krsCTKXMLRAW;

  if TryGetData(aKey, aText) then
    Result := aText;
end;

constructor cComictecaTextMap.Create(AFreeObjects: boolean);
begin
  inherited Create(AFreeObjects);

  OnKeyCompare := @AnsiCompareStr;
end;

end.
