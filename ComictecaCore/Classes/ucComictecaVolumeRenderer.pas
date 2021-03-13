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
  ucComictecaVolume, ucComictecaPage, ucComictecaFrame;

type

  { cComictecaVolumeRenderer }

  cComictecaVolumeRenderer = class(TComponent)
  private
    FComic: cComictecaVolume;
    FFixPerpective: boolean;
    FFlipL2R: boolean;
    FIndexLastPage: integer;
    FLastPage: TBGRABitmap;
    FShowFrameBorders: boolean;
    FShowPerspectiveQuad: boolean;
    FShowTextBorders: boolean;
    procedure SetComic(AValue: cComictecaVolume);
    procedure SetFixPerpective(AValue: boolean);
    procedure SetFlipL2R(AValue: boolean);
    procedure SetShowFrameBorders(AValue: boolean);
    procedure SetShowPerspectiveQuad(AValue: boolean);
    procedure SetShowTextBorders(AValue: boolean);

  protected
    procedure ResetCache;

  public
    property LastPage: TBGRABitmap read FLastPage;
    property IndexLastPage: integer read FIndexLastPage;

    function RenderPageByIdx(aIndex: integer): TBGRABitmap;
    function RenderFrameByIdx(aIndex: integer): TBGRABitmap;
    function RenderPage(aPage: cComictecaPage): TBGRABitmap;
    function RenderPageRect(aPage: cComictecaPage;
      const aRect: TRect): TBGRABitmap;
    function RenderFrame(aFrame: cComictecaFrame): TBGRABitmap;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Comic: cComictecaVolume read FComic write SetComic;

    property FlipL2R: boolean read FFlipL2R write SetFlipL2R;

    property FixPerpective: boolean read FFixPerpective write SetFixPerpective;

    property ShowPerspectiveQuad: boolean
      read FShowPerspectiveQuad write SetShowPerspectiveQuad;

    property ShowFrameBorders: boolean
      read FShowFrameBorders write SetShowFrameBorders;

    property ShowTextBorders: boolean read FShowTextBorders
      write SetShowTextBorders;
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

procedure cComictecaVolumeRenderer.SetFixPerpective(AValue: boolean);
begin
  if FFixPerpective = AValue then
    Exit;
  FFixPerpective := AValue;

  ResetCache;
end;

procedure cComictecaVolumeRenderer.SetFlipL2R(AValue: boolean);
begin
  if FFlipL2R = AValue then
    Exit;
  FFlipL2R := AValue;

  if Assigned(Comic) then
  begin
    if Comic.Right2Left then
      ResetCache;
  end;
end;

procedure cComictecaVolumeRenderer.SetShowFrameBorders(AValue: boolean);
begin
  if FShowFrameBorders = AValue then
    Exit;
  FShowFrameBorders := AValue;

  ResetCache;
end;

procedure cComictecaVolumeRenderer.SetShowPerspectiveQuad(AValue: boolean);
begin
  if FShowPerspectiveQuad = AValue then
    Exit;
  FShowPerspectiveQuad := AValue;

  ResetCache;
end;

procedure cComictecaVolumeRenderer.SetShowTextBorders(AValue: boolean);
begin
  if FShowTextBorders = AValue then
    Exit;
  FShowTextBorders := AValue;

  ResetCache;
end;

procedure cComictecaVolumeRenderer.ResetCache;
begin
  FreeAndNil(FLastPage);
  FIndexLastPage := -1;
end;

function cComictecaVolumeRenderer.RenderPageByIdx(aIndex: integer):
TBGRABitmap;
begin
  Result := nil;

  if (not assigned(Comic)) or (aIndex <= -1) or
    (aIndex >= Comic.Pages.Count) then
    Exit;

  Result := RenderPage(Comic.Pages[aIndex]);
end;

function cComictecaVolumeRenderer.RenderFrameByIdx(aIndex: integer):
TBGRABitmap;
begin
  Result := nil;

  if (not assigned(Comic)) or (aIndex <= -1) or
    (aIndex >= Comic.Frames.Count) then
    Exit;

  Result := RenderFrame(Comic.Frames[aIndex]);
end;

function cComictecaVolumeRenderer.RenderPage(
  aPage: cComictecaPage): TBGRABitmap;

  procedure DoFixPerspective(var aImage: TBGRABitmap; aPage: cComictecaPage);

    function GetLine(const a, b: TPoint): TLineDef;
    begin
      Result.origin := PointF(a);
      Result.dir := PointF(a - b);


      //if (a.X - b.X) = 0 then
      //  Result.dir := PointF(0, 1) // Vertical line
      //else
      //  Result.dir := PointF(1, (a.Y - b.Y) / (a.X - b.X));
    end;

  var // procedure DoFixPerspective(aImage: TBGRABitmap; aPage: cComictecaPage);
    TopLine, BottomLine, LeftLine, RightLine: TLineDef;
    cTL, cTR, cBR, cBL: TPointF;
    correct: TBGRABitmap;
  begin
    if not aPage.HasPerspective then
      Exit;

    correct := TBGRABitmap.Create(aImage.Width, aImage.Height,
      bgra(0, 0, 0));

    if not aPage.CropPerspective then
    begin
      // Lines
      TopLine := GetLine(aPage.PersTL, aPage.PersTR);
      TopLine.origin := PointF(aImage.Width shr 1, 0);

      RightLine := GetLine(aPage.PersTR, aPage.PersBR);
      RightLine.origin := PointF(aImage.Width, aImage.Height shr 1);

      BottomLine := GetLine(aPage.PersBR, aPage.PersBL);
      BottomLine.origin := PointF(aImage.Width shr 1, aImage.Height);

      LeftLine := GetLine(aPage.PersBL, aPage.PersTL);
      LeftLine.origin := PointF(0, aImage.Height shr 1);

      cTL := IntersectLine(TopLine, LeftLine);
      cTR := IntersectLine(TopLine, RightLine);
      cBR := IntersectLine(BottomLine, RightLine);
      cBL := IntersectLine(BottomLine, LeftLine);
    end
    else
    begin
      cTL := PointF(aPage.PersTL);
      cTR := PointF(aPage.PersTR);
      cBR := PointF(aPage.PersBR);
      cBL := PointF(aPage.PersBL);
    end;

    correct.FillQuadPerspectiveMappingAntialias(
      PointF(0, 0), PointF(aImage.Width, 0),
      PointF(aImage.Width, aImage.Height), PointF(0, aImage.Height),
      aImage,
      cTL, cTR, cBR, cBL);

    //correct.FillQuadLinearMappingAntialias(
    //PointF(0, 0), PointF(image.Width, 0),
    //PointF(image.Width, image.Height), PointF(0, image.Height),
    //  image,
    //  cTL, cTR, cBR, cBL);

    BGRAReplace(aImage, correct);
  end;

var
  PageIndex, i: integer;
  aFile: string;
begin
  Result := nil;

  if (not assigned(Comic)) or (not assigned(aPage)) then
    Exit;

  PageIndex := Comic.Pages.IndexOf(aPage);

  if PageIndex = -1 then
    Exit;

  if IndexLastPage = PageIndex then
  begin
    Result := LastPage.Duplicate(True);
    Exit;
  end;

  FreeAndNil(FLastPage);
  FIndexLastPage := -1;

  aFile := Comic.Folder + aPage.FileName;
  if FileExistsUTF8(aFile) then
  begin
    FLastPage := TBGRABitmap.Create;
    LastPage.LoadFromFile(aFile);

    if FixPerpective then
      DoFixPerspective(FLastPage, aPage);

    if ShowPerspectiveQuad and aPage.HasPerspective then
    begin
      LastPage.DrawPolygon([aPage.PersTL, aPage.PersTR,
        aPage.PersBR, aPage.PersBL], BGRA(0, 0, 255));
      LastPage.DrawLine(aPage.PersTL.X, aPage.PersTL.Y,
        aPage.PersBR.X, aPage.PersBR.Y, BGRA(0, 0, 255), True);
      LastPage.DrawLine(aPage.PersTR.X, aPage.PersTR.Y,
        aPage.PersBL.X, aPage.PersBL.Y, BGRA(0, 0, 255), True);
    end;

    if ShowFrameBorders then
    begin
      i := 0;
      while i < Comic.Frames.Count do
      begin
        if Comic.Frames[i].Page = aPage then
          LastPage.Rectangle(Comic.Frames[i].Rect, BGRA(0, 255, 0), dmSet);
        Inc(i);
      end;
    end;

    if ShowTextBorders then
    begin
      i := 0;
      while i < aPage.Texts.Count do
      begin
        LastPage.Rectangle(aPage.Texts[i].Rect, BGRA(255, 0, 255), dmSet);
        Inc(i);
      end;
    end;

    if Comic.Right2Left and FlipL2R then
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
    FIndexLastPage := PageIndex;
  end;
end;

function cComictecaVolumeRenderer.RenderPageRect(aPage: cComictecaPage;
  const aRect: TRect): TBGRABitmap;
var
  aPageImg: TBGRABitmap;
begin
  Result := nil;

  if (not assigned(Comic)) or (not assigned(aPage)) then
    Exit;

  aPageImg := RenderPage(aPage);

  if aPageImg = nil then
    Exit;

  if not aRect.IsEmpty then // if empty then full page.
  begin
    if Comic.Right2Left and FlipL2R then
      aPageImg.HorizontalFlip;
    BGRAReplace(aPageImg, aPageImg.GetPart(aRect));
    if Comic.Right2Left and FlipL2R then
      aPageImg.HorizontalFlip;
  end;

  Result := aPageImg;
end;

function cComictecaVolumeRenderer.RenderFrame(aFrame: cComictecaFrame):
TBGRABitmap;
begin
  Result := nil;

  if (not assigned(Comic)) or (not assigned(aFrame)) then
    Exit;

  Result := RenderPageRect(cComictecaPage(aFrame.Page), aFrame.Rect);
end;

constructor cComictecaVolumeRenderer.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FIndexLastPage := -1;
  FLastPage := nil;
  FFlipL2R := False;
  FFixPerpective := True;
  FShowFrameBorders := False;
  FShowTextBorders := False;
end;

destructor cComictecaVolumeRenderer.Destroy;
begin
  FreeAndNil(FLastPage);

  inherited Destroy;
end;

end.
