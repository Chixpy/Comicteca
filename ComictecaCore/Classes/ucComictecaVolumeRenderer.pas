unit ucComictecaVolumeRenderer;

{< cComictecaVolumeRenderizer class unit.

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
  Classes, SysUtils, FileUtil, LazFileUtils,
  BGRABitmapTypes, BGRABitmap,
  // Comicteca classes
  ucComictecaVolume, ucComictecaPage;

type

  { cComictecaVolumeRenderer }

  cComictecaVolumeRenderer = class(TComponent)
  private
    FComic: cComictecaVolume;
    FFlipL2R: boolean;
    FIndexLastPage: integer;
    FLastPage: TBGRABitmap;
    procedure SetComic(AValue: cComictecaVolume);
    procedure SetFlipL2R(AValue: boolean);

  protected
    procedure ResetCache;

  public
    property LastPage: TBGRABitmap read FLastPage;
    property IndexLastPage: integer read FIndexLastPage;

    function RenderPage(aIndex: integer): TBGRABitmap;

    function RenderFrame(aIndex: integer): TBGRABitmap;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Comic: cComictecaVolume read FComic write SetComic;

    property FlipL2R: boolean read FFlipL2R write SetFlipL2R;
  end;

implementation

{ cComictecaVolumeRenderer }

procedure cComictecaVolumeRenderer.SetComic(AValue: cComictecaVolume);
begin
  if FComic = AValue then
    Exit;
  FComic := AValue;

  ResetCache;
end;

procedure cComictecaVolumeRenderer.SetFlipL2R(AValue: boolean);
begin
  if FFlipL2R = AValue then
    Exit;
  FFlipL2R := AValue;

  ResetCache;
end;

procedure cComictecaVolumeRenderer.ResetCache;
begin
  FreeAndNil(FLastPage);
  FIndexLastPage := -1;
end;

function cComictecaVolumeRenderer.RenderPage(aIndex: integer): TBGRABitmap;
var
  aFile: string;
  i: integer;
  aPage: cComictecaPage;
begin
  Result := nil;

  if (not assigned(Comic)) or (aIndex <= -1) or
    (aIndex >= Comic.Pages.Count) then
    Exit;

  if IndexLastPage = aIndex then
  begin
    Result := LastPage.Duplicate(True);
    Exit;
  end;

  FreeAndNil(FLastPage);
  aPage := Comic.Pages[aIndex];

  aFile := Comic.Folder + aPage.FileName;
  if FileExistsUTF8(aFile) then
  begin
    FLastPage := TBGRABitmap.Create;
    LastPage.LoadFromFile(aFile);

    if FlipL2R then
    begin
      i := 0;
      while i < aPage.Texts.Count do
      begin
        LastPage.HorizontalFlip(aPage.Texts[i].Rect);
        Inc(i);
      end;

      LastPage.HorizontalFlip;
    end;

    Result := LastPage.Duplicate(True);
    FIndexLastPage := aIndex;
  end
  else
    FIndexLastPage := -1;
end;

function cComictecaVolumeRenderer.RenderFrame(aIndex: integer): TBGRABitmap;
var
  PageIndex: integer;
  aPage: TBGRABitmap;
begin
  Result := nil;

  if (not assigned(Comic)) or (aIndex <= -1) or
    (aIndex >= Comic.Frames.Count) then
    Exit;

  PageIndex := Comic.Pages.IndexOf(cComictecaPage(Comic.Frames[aIndex].Page));

  if PageIndex = -1 then
    Exit;

  aPage := RenderPage(PageIndex);

  if not Comic.Frames[aIndex].Rect.IsEmpty then
  begin
    if FlipL2R then
      aPage.HorizontalFlip;
    BGRAReplace(aPage, aPage.GetPart(Comic.Frames[aIndex].Rect));
    if FlipL2R then
      aPage.HorizontalFlip;
  end;

  Result := aPage;
end;

constructor cComictecaVolumeRenderer.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FIndexLastPage := -1;
  FLastPage := nil;
end;

destructor cComictecaVolumeRenderer.Destroy;
begin
  FreeAndNil(FLastPage);

  inherited Destroy;
end;

end.
