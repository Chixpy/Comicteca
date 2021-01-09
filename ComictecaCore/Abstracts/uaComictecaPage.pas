unit uaComictecaPage;

{< caComictecaPage abstact class unit.

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
  Classes, SysUtils, LazUTF8, Laz2_DOM, laz2_XMLRead, Laz2_XMLWrite,
  // CHX units
  uCHXStrUtils,
  // Comicteca Core units
  uCTKConst, uCTKCommon;

const
  kDefMultiplePages = 1;
  kDefPageContent = [CTKPTVignettes];


type


  { caComictecaPage abstract class.

    It defines a physical page.

    Stores basic comic page information that not depends from volume/issue.}

  caComictecaPage = class(TComponent)
  private

    FFileName: string;
    FMultiplePages: integer;
    FPageContent: tCTKPageContents;
    FSHA1: string;
    procedure SetFileName(AValue: string);
    procedure SetMultiplePages(const AValue: integer);
    procedure SetPageContent(AValue: tCTKPageContents);
    procedure SetSHA1(AValue: string);

  public

    procedure LoadFromXML(Node: TDOMElement); virtual;
    procedure SaveToXML(aXMLDoc: TXMLDocument; Node: TDOMElement); virtual;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published

    property SHA1: string read FSHA1 write SetSHA1;

    property FileName: string read FFileName write SetFileName;

    property MultiplePages: integer read FMultiplePages write SetMultiplePages;
    {< Number of pages in scanned image. }

    property PageContent: tCTKPageContents
      read FPageContent write SetPageContent;

  end;

implementation

{ caComictecaPage }

procedure caComictecaPage.SetMultiplePages(const AValue: integer);
begin
  if FMultiplePages = AValue then
    Exit;
  FMultiplePages := AValue;
end;

procedure caComictecaPage.SetFileName(AValue: string);
begin
  FFileName := SetAsFile(AValue);
end;

procedure caComictecaPage.SetPageContent(AValue: tCTKPageContents);
begin
  if FPageContent = AValue then
    Exit;
  FPageContent := AValue;
end;

procedure caComictecaPage.SetSHA1(AValue: string);
begin
  if FSHA1 = AValue then
    Exit;
  FSHA1 := AValue;
end;

procedure caComictecaPage.LoadFromXML(Node: TDOMElement);
begin
  if not Assigned(Node) then
    Exit;

  FileName := Node.TextContent;

  SHA1 := Node[krsCTKXMLSHA1Prop];

  MultiplePages := StrToIntDef(Node[krsCTKXMLMultipageProp], kDefMultiplePages);

  PageContent := Str2PageTypeKeySet(Node[krsCTKXMLContentProp]);
  if PageContent = [] then
    PageContent := kDefPageContent;
end;

procedure caComictecaPage.SaveToXML(aXMLDoc: TXMLDocument; Node: TDOMElement);
var
  aSL: TStringList;
  iProp: tCTKPageContent;
begin
  if not Assigned(Node) then
    Exit;

  if SHA1 = '' then
    Exit;

  Node.AppendChild(aXMLDoc.CreateTextNode(FileName));

  Node[krsCTKXMLSHA1Prop] := SHA1;

  // MultiplePages
  if MultiplePages <> kDefMultiplePages then
    Node[krsCTKXMLMultipageProp] := IntToStr(MultiplePages);

  // PageContent
  if PageContent <> kDefPageContent then
    Node[krsCTKXMLContentProp] := PageTypeKeySet2Str(PageContent);
end;

constructor caComictecaPage.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  // Default properties
  MultiplePages := kDefMultiplePages;
  PageContent := kDefPageContent;
end;

destructor caComictecaPage.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(caComictecaPage);

finalization
  UnRegisterClass(caComictecaPage);

end.
