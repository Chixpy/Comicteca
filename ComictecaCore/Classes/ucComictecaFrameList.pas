unit ucComictecaFrameList;

{< cComictecaFrameList class unit.

  Copyright (C) 2006-2019 Chixpy

  This file is part of Emuteca

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
  // Comicteca Core units
  uCTKConst,
  // Comicteca Core abstracts
  ucComictecaPageList, ucComictecaFrame;

type

  cComictecaGenFrameList = specialize TFPGObjectList<cComictecaFrame>;

  { cComictecaFrameList }

  cComictecaFrameList = class(cComictecaGenFrameList)
  private
    FPageList: cComictecaPageList;
    procedure SetPageList(AValue: cComictecaPageList);

  public
    property PageList: cComictecaPageList read FPageList write SetPageList;

    procedure LoadFromXML(Parent: TDOMElement); virtual;
    procedure SaveToXML(aXMLDoc: TXMLDocument; Parent: TDOMElement); virtual;
  end;

implementation

{ cComictecaFrameList }

procedure cComictecaFrameList.SetPageList(AValue: cComictecaPageList);
begin
  if FPageList = AValue then
    Exit;
  FPageList := AValue;
end;

procedure cComictecaFrameList.LoadFromXML(Parent: TDOMElement);
var
  FrameNode: TDOMElement;
  FrameList: TDOMNodeEnumerator;
  aFrame: cComictecaFrame;
  i: integer;
begin
  if not Assigned(Parent) then
    Exit;

  FrameList := Parent.GetEnumerator;
  try
    while FrameList.MoveNext do
    begin
      if FrameList.Current is TDOMElement then
      begin
        FrameNode := TDOMElement(FrameList.Current);

        if AnsiCompareText(FrameNode.TagName, krsCTKXMLFrame) = 0 then
        begin
          aFrame := cComictecaFrame.Create(nil);
          aFrame.LoadFromXML(FrameNode);

          // Assigning Page to Frame
          if Assigned(PageList) then
          begin
            i := 0;
            while (i < PageList.Count) and (not assigned(aFrame.Page)) do
            begin
              if PageList[i].MatchSHA1(aFrame.PageSHA1) then
                aFrame.Page := PageList[i];
              Inc(i);
            end;
          end;

          Self.Add(aFrame);
        end;
      end;
    end;
  finally
    FrameList.Free;
  end;
end;

procedure cComictecaFrameList.SaveToXML(aXMLDoc: TXMLDocument;
  Parent: TDOMElement);
var
  i: integer;
  XMLFrame: TDOMElement;
  aFrame: cComictecaFrame;
begin
  i := 0;
  while i < Count do
  begin
    aFrame := Items[i];

    XMLFrame := aXMLDoc.CreateElement(krsCTKXMLFrame);
    aFrame.SaveToXML(aXMLDoc, XMLFrame);
    Parent.AppendChild(XMLFrame);
    Inc(i);
  end;
end;

//initialization
//  RegisterClass(cComictecaFrameList);

//finalization
//  UnRegisterClass(cComictecaFrameList);
end.
