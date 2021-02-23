unit uaComictecaPage;

{< caComictecaPage abstact class unit.

  This file is part of Comicteca Core.

  Copyright (C) 2019-2021 Chixpy

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
  kDefPageContent = [CTKFTVignette];


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

    function MatchSHA1(aSHA1: string): boolean;

    procedure LoadFromXML(aXMLNode: TDOMElement); virtual;
    procedure SaveToXML(aXMLDoc: TXMLDocument; aXMLNode: TDOMElement); virtual;

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

function caComictecaPage.MatchSHA1(aSHA1: string): boolean;
begin
  Result := UTF8CompareText(Self.SHA1, aSHA1) = 0;
end;

procedure caComictecaPage.LoadFromXML(aXMLNode: TDOMElement);
begin
  if not Assigned(aXMLNode) then
    Exit;

  FileName := aXMLNode[krsCTKXMLFileProp];
  // TODO: Delete fallback
  if FileName= '' then
    FileName := aXMLNode.TextContent;

  SHA1 := aXMLNode[krsCTKXMLSHA1Prop];

  MultiplePages := StrToIntDef(aXMLNode[krsCTKXMLMultipageProp], kDefMultiplePages);

  PageContent := Str2FrameTypeSet(aXMLNode[krsCTKXMLContentProp]);
  if PageContent = [] then
    PageContent := kDefPageContent;
end;

procedure caComictecaPage.SaveToXML(aXMLDoc: TXMLDocument; aXMLNode: TDOMElement);
var
  aSL: TStringList;
  iProp: tCTKFrameType;
begin
  if (not Assigned(aXMLNode)) or (not Assigned(aXMLDoc)) then
    Exit;

  if SHA1 = '' then
    Exit;

  aXMLNode[krsCTKXMLFileProp] := FileName;

  aXMLNode[krsCTKXMLSHA1Prop] := SHA1;

  // MultiplePages
  if MultiplePages <> kDefMultiplePages then
    aXMLNode[krsCTKXMLMultipageProp] := IntToStr(MultiplePages);

  // PageContent
  if PageContent <> kDefPageContent then
    aXMLNode[krsCTKXMLContentProp] := FrameTypeSet2Str(PageContent);
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
