unit uCTKCommon;

{< Comicteca Editor methods unit.

  This file is part of Comicteca Editor.

  Copyright (C) 2020-2021 Chixpy

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
  Classes, SysUtils, LazUTF8, StrUtils,
  // Comicteca Core
  uCTKConst;

function Str2FrameType(aString: string): tCTKFrameType;

function Str2FrameTypeSet(aString: string): tCTKPageContents;
function StrLst2FrameTypeSet(aStringList: TStringList): tCTKPageContents;

function FrameTypeSet2Str(aPageTypes: tCTKPageContents): string;

implementation

function Str2FrameType(aString: string): tCTKFrameType;
begin
  Result := CTKFTOther;

  if UTF8CompareText(aString, krsFTVignetteKey) = 0 then
    Result := CTKFTVignette
  else if UTF8CompareText(aString, krsFTSpineDustJacketKey) = 0 then
    Result := CTKFTSpineDustJacket
  else if UTF8CompareText(aString, krsFTFrontDustJacketKey) = 0 then
    Result := CTKFTFrontDustJacket
  else if UTF8CompareText(aString, krsFTSpineCoverKey) = 0 then
    Result := CTKFTSpineCover
  else if UTF8CompareText(aString, krsFTFrontCoverKey) = 0 then
    Result := CTKFTFrontCover
  else if UTF8CompareText(aString, krsFTEndPaperKey) = 0 then
    Result := CTKFTEndPaper
  else if UTF8CompareText(aString, krsFTFlyLeafKey) = 0 then
    Result := CTKFTFlyLeaf
  else if UTF8CompareText(aString, krsFTIndexKey) = 0 then
    Result := CTKFTFlyLeaf
  else if UTF8CompareText(aString, krsFTBastardTitleKey) = 0 then
    Result := CTKFTBastardTitle
  else if UTF8CompareText(aString, krsFTChapterTitleKey) = 0 then
    Result := CTKFTChapterTitle
  else if UTF8CompareText(aString, krsFTEditorialInfoKey) = 0 then
    Result := CTKFTEditorialInfo
  else if UTF8CompareText(aString, krsFTAuthorTextKey) = 0 then
    Result := CTKFTAuthorText
  else if UTF8CompareText(aString, krsFTAdsKey) = 0 then
    Result := CTKFTAds
  else if UTF8CompareText(aString, krsFTLicenseKey) = 0 then
    Result := CTKFTLicense
  else if UTF8CompareText(aString, krsFTBackCoverKey) = 0 then
    Result := CTKFTBackCover
  else if UTF8CompareText(aString, krsFTBackDustJacketKey) = 0 then
    Result := CTKFTBackDustJacket
  else if UTF8CompareText(aString, krsFTOtherKey) = 0 then
    Result := CTKFTOther;
end;

function Str2FrameTypeSet(aString: string): tCTKPageContents;
var
  aSL: TStringList;
begin
  Result := [];

  aSL := TStringList.Create;
  try
    aSL.CommaText := aString;
    Result := StrLst2FrameTypeSet(aSL);
  finally
    aSL.Free;
  end;
end;

function StrLst2FrameTypeSet(aStringList: TStringList): tCTKPageContents;
var
  i: integer;
begin
  Result := [];

  if not Assigned(aStringList) then
    Exit;

  i := 0;
  while i < aStringList.Count do
  begin
    if aStringList[i] <> '' then
      Include(Result, Str2FrameType(aStringList[i]));
    Inc(i);
  end;
end;

function FrameTypeSet2Str(aPageTypes: tCTKPageContents): string;
var
  aSL: TStringList;
  iProp: tCTKFrameType;
begin
  Result := '';

  try
    aSL := TStringList.Create;
    for  iProp := Low(tCTKFrameType) to High(tCTKFrameType) do
    begin
      if iProp in aPageTypes then
        aSL.Add(ComictecaFrameTypeKey[iProp]);
    end;

    Result := aSL.CommaText;

  finally
    FreeAndNil(aSL);
  end;
end;

end.
