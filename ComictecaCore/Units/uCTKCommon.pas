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

function Str2PageTypeKey(aString: string): tCTKPageContent;

function Str2PageTypeKeySet(aString: string): tCTKPageContents;
function StrLst2PageTypeKeySet(aStringList: TStringList): tCTKPageContents;

function PageTypeKeySet2Str(aPageTypes: tCTKPageContents): string;

implementation

function Str2PageTypeKey(aString: string): tCTKPageContent;
begin
  Result := CTKPTOther;

  if UTF8CompareText(aString, krsPCVignettesKey) = 0 then
    Result := CTKPTVignettes
  else if UTF8CompareText(aString, krsPCFrontCoverKey) = 0 then
    Result := CTKPTFrontCover
  else if UTF8CompareText(aString, krsPCBackCoverKey) = 0 then
    Result := CTKPTBackCover
  else if UTF8CompareText(aString, krsPCChapterTitleKey) = 0 then
    Result := CTKPTChapterTitle
  else if UTF8CompareText(aString, krsPCEditorialInfoKey) = 0 then
    Result := CTKPTEditorialInfo
  else if UTF8CompareText(aString, krsPCAutorTextKey) = 0 then
    Result := CTKPTAuthorText
  else if UTF8CompareText(aString, krsPCAdsKey) = 0 then
    Result := CTKPTAds
  else if UTF8CompareText(aString, krsPCOtherKey) = 0 then
    Result := CTKPTOther;

end;

function Str2PageTypeKeySet(aString: string): tCTKPageContents;
var
  aSL: TStringList;
begin
  Result := [];

  aSL := TStringList.Create;
  try
    aSL.CommaText := aString;
    Result := StrLst2PageTypeKeySet(aSL);
  finally
    aSL.Free;
  end;
end;

function StrLst2PageTypeKeySet(aStringList: TStringList): tCTKPageContents;
var
  i: integer;
begin
  Result := [];

  if not Assigned(aStringList) then Exit;

  i := 0;
  while i < aStringList.Count do
  begin
    if aStringList[i] <> '' then
      Include(Result, Str2PageTypeKey(aStringList[i]));
    Inc(i);
  end;
end;

function PageTypeKeySet2Str(aPageTypes: tCTKPageContents): string;
var
  aSL: TStringList;
  iProp: tCTKPageContent;
begin
  Result := '';

  try
    aSL := TStringList.Create;
    for  iProp := Low(tCTKPageContent) to High(tCTKPageContent) do
    begin
      if iProp in aPageTypes then
        aSL.Add(ComictecaPageContentKey[iProp]);
    end;

    Result := aSL.CommaText;

  finally
    FreeAndNil(aSL);
  end;
end;

end.
