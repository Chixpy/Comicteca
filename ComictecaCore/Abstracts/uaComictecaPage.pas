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
  uCHXStrUtils, uCHXRecordHelpers,
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
    FCallObservers: boolean;
    FCropGeometry: boolean;

    FFileName: string;
    FLinearGeometry: Boolean;
    FMultiplePages: integer;
    FPageContent: tCTKPageContents;
    FSHA1: string;
    procedure SetCallObservers(AValue: boolean);
    procedure SetCropGeometry(AValue: boolean);
    procedure SetFileName(AValue: string);
    procedure SetLinearGeometry(AValue: Boolean);
    procedure SetMultiplePages(const AValue: integer);
    procedure SetPageContent(AValue: tCTKPageContents);
    procedure SetSHA1(AValue: string);

  public
    GeomTL: TPoint;
    GeomTR: TPoint;
    GeomBL: TPoint;
    GeomBR: TPoint;

    property CallObservers: boolean read FCallObservers
      write SetCallObservers;
    {< If True (default), the object call its observed on any change.

      Change it to false at beginning of many changes will be done and want
        to update observers only one time at the end, when must be set to True
        again.
    }

    function HasGeometry: boolean;

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

    property CropGeometry: boolean read FCropGeometry write SetCropGeometry;
    property LinearGeometry: Boolean read FLinearGeometry write SetLinearGeometry;

  end;

implementation

{ caComictecaPage }

procedure caComictecaPage.SetMultiplePages(const AValue: integer);
begin
  if FMultiplePages = AValue then
    Exit;
  FMultiplePages := AValue;

  if CallObservers then
    FPONotifyObservers(Self, ooChange, nil);
end;

procedure caComictecaPage.SetFileName(AValue: string);
begin
  FFileName := SetAsFile(AValue);

  if CallObservers then
    FPONotifyObservers(Self, ooChange, nil);
end;

procedure caComictecaPage.SetLinearGeometry(AValue: Boolean);
begin
  if FLinearGeometry = AValue then Exit;
  FLinearGeometry := AValue;

  if CallObservers then
    FPONotifyObservers(Self, ooChange, nil);
end;

procedure caComictecaPage.SetCropGeometry(AValue: boolean);
begin
  if FCropGeometry = AValue then
    Exit;
  FCropGeometry := AValue;

  if CallObservers then
    FPONotifyObservers(Self, ooChange, nil);
end;

procedure caComictecaPage.SetCallObservers(AValue: boolean);
begin
  // if FCallObservers = AValue then Exit; <- Call if True anyway
  FCallObservers := AValue;

  if CallObservers then
    FPONotifyObservers(Self, ooChange, nil);
end;

procedure caComictecaPage.SetPageContent(AValue: tCTKPageContents);
begin
  if FPageContent = AValue then
    Exit;
  FPageContent := AValue;

  if CallObservers then
    FPONotifyObservers(Self, ooChange, nil);
end;

procedure caComictecaPage.SetSHA1(AValue: string);
begin
  if FSHA1 = AValue then
    Exit;
  FSHA1 := AValue;

  if CallObservers then
    FPONotifyObservers(Self, ooChange, nil);
end;

function caComictecaPage.HasGeometry: boolean;
begin
  Result := (GeomTL <> GeomTR) and (GeomTL <> GeomBL) and
    (GeomTL <> GeomBR) and (GeomTR <> GeomBL) and (GeomTR <> GeomBR) and
    (GeomBL <> GeomBR);
end;

function caComictecaPage.MatchSHA1(aSHA1: string): boolean;
begin
  Result := UTF8CompareText(Self.SHA1, aSHA1) = 0;
end;

procedure caComictecaPage.LoadFromXML(aXMLNode: TDOMElement);
var
  aSL: TStringList;
  aResult: boolean;
begin
  if not Assigned(aXMLNode) then
    Exit;

  CallObservers := False; // Don't notify Observer

  FileName := aXMLNode[krsCTKXMLFileProp];

  // TODO: Delete fallback
  if FileName = '' then
    FileName := aXMLNode.TextContent;

  SHA1 := aXMLNode[krsCTKXMLSHA1Prop];

  MultiplePages := StrToIntDef(aXMLNode[krsCTKXMLMultipageProp],
    kDefMultiplePages);

  PageContent := Str2FrameTypeSet(aXMLNode[krsCTKXMLContentProp]);
  if PageContent = [] then
    PageContent := kDefPageContent;

  // Perspective quadriteral
  aSL := TStringList.Create;
  try
    aSL.Delimiter := ';';
    aSL.QuoteChar := '''';
    aSL.DelimitedText := aXMLNode[krsCTKXMLGeometryProp];
    if aSL.Count >= 4 then
    begin
      aResult := True;
      aResult := aResult and GeomTL.FromString(aSL[0]);
      aResult := aResult and GeomTR.FromString(aSL[1]);
      aResult := aResult and GeomBR.FromString(aSL[2]);
      aResult := aResult and GeomBL.FromString(aSL[3]);

      if aSL.Count >= 5 then
        CropGeometry := StrToBool(aSL[4])
        else
          CropGeometry := False;

            if aSL.Count >= 6 then
        LinearGeometry := StrToBool(aSL[5])
        else
          LinearGeometry := False;
    end
    else
      aResult := False;

    if not aResult then
    begin
      GeomTL := TPoint.Zero;
      GeomTR := TPoint.Zero;
      GeomBR := TPoint.Zero;
      GeomBL := TPoint.Zero;
      CropGeometry := False;
      LinearGeometry := False;
    end;
  finally
    aSL.Free;
  end;

  CallObservers := True;
end;

procedure caComictecaPage.SaveToXML(aXMLDoc: TXMLDocument;
  aXMLNode: TDOMElement);
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

  // Perspective quadriteral
  if HasGeometry then
  begin
    aSL := TStringList.Create;
    aSL.Delimiter := ';';
    aSL.QuoteChar := '''';
    aSL.Add(GeomTL.ToString);
    aSL.Add(GeomTR.ToString);
    aSL.Add(GeomBR.ToString);
    aSL.Add(GeomBL.ToString);
    aSL.Add(BoolToStr(CropGeometry, True));
    aSL.Add(BoolToStr(LinearGeometry, True));
    aXMLNode[krsCTKXMLGeometryProp] := aSL.DelimitedText;
    aSL.Free;
  end;
end;

constructor caComictecaPage.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  // Default properties
  SHA1 := '';
  MultiplePages := kDefMultiplePages;
  PageContent := kDefPageContent;
  GeomTL := TPoint.Zero;
  GeomTR := TPoint.Zero;
  GeomBL := TPoint.Zero;
  GeomBR := TPoint.Zero;
  CropGeometry := False;
  LinearGeometry := False;
  CallObservers := True;
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
