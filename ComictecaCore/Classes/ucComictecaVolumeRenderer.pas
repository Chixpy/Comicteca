unit ucComictecaVolumeRenderer;

{< cComictecaVolumeRenderizer class unit.

  This file is part of Comicteca Core.

  Copyright (C) 2023 Chixpy

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
  Classes, SysUtils, FileUtil, LazFileUtils, BGRABitmapTypes, BGRABitmap,
  // Comicteca Core units.
  uCTKConst,
  // Comicteca Core abstracts.
  uaComictecaShapedImage,
  // Comicteca Core classes.
  ucComictecaVolume, ucComictecaPage, ucComictecaFrame, ucComictecaText;

type

  { cComictecaVolumeRenderer }

  cComictecaVolumeRenderer = class(TComponent)
  private
    FComic: cComictecaVolume;
    FFixGeometry: boolean;
    FFlipL2R: boolean;
    FLastPageIndex: integer;
    FLastPage: TBGRABitmap;
    FDebugRender: boolean;
    FLastRawImage: TBGRABitmap;
    FLastRawImageFile: string;
    FShowFrameBorders: boolean;
    FShowGeometryQuad: boolean;
    FShowTextBorders: boolean;
    procedure SetComic(AValue: cComictecaVolume);
    procedure SetFixGeometry(AValue: boolean);
    procedure SetFlipL2R(AValue: boolean);
    procedure SetDebugRender(AValue: boolean);
    procedure SetShowFrameBorders(AValue: boolean);
    procedure SetShowGeometryQuad(AValue: boolean);
    procedure SetShowTextBorders(AValue: boolean);

  protected
    procedure IntRenderShape(aImg: TBGRABitmap; ImageShape: tCTKImageShape;
      X, Y: integer; DebugColor: TBGRAPixel);

  public
    FrameBorderColor: TBGRAPixel;
    FrameBorderFill: TBGRAPixel;
    TextBorderColor: TBGRAPixel;
    TextBorderFill: TBGRAPixel;
    GeomQuadColor: TBGRAPixel;

    property LastRawImage: TBGRABitmap read FLastRawImage;
    {< Cache of last loaded image file, without any processing. }
    property LastRawImageFile: string read FLastRawImageFile;
    {< Last file loaded. }
    property LastPage: TBGRABitmap read FLastPage;
    {< Cache of last showed page, processed or with debug. }
    property LastPageIndex: integer read FLastPageIndex;
    {< Index of last loaded page. }

    procedure ResetFileCache;
    procedure ResetPageCache;

    function RenderFile(aFile: string): TBGRABitmap;
    {< Renders a File, retrieving it from cache or storing it in cache.}

    function RenderPageByIdx(aIndex: integer): TBGRABitmap;
    function RenderFrameByIdx(aIndex: integer): TBGRABitmap;
    function RenderPage(aPage: cComictecaPage): TBGRABitmap;
    function RenderPageRect(aPage: cComictecaPage;
      const aRect: TRect): TBGRABitmap;
    function RenderFrame(aFrame: cComictecaFrame): TBGRABitmap;
    function RenderText(aText: cComictecaText): TBGRABitmap;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Comic: cComictecaVolume read FComic write SetComic;

    property FlipL2R: boolean read FFlipL2R write SetFlipL2R;

    property FixGeometry: boolean read FFixGeometry write SetFixGeometry;

    property ShowGeometryQuad: boolean
      read FShowGeometryQuad write SetShowGeometryQuad;

    property ShowFrameBorders: boolean
      read FShowFrameBorders write SetShowFrameBorders;

    property ShowTextBorders: boolean read FShowTextBorders
      write SetShowTextBorders;

    property DebugRender: boolean read FDebugRender write SetDebugRender;

  end;

implementation

{ cComictecaVolumeRenderer }

procedure cComictecaVolumeRenderer.SetComic(AValue: cComictecaVolume);
begin
  if FComic = AValue then
    Exit;
  FComic := AValue;

  ResetPageCache;
end;

procedure cComictecaVolumeRenderer.SetFixGeometry(AValue: boolean);
begin
  if FFixGeometry = AValue then
    Exit;
  FFixGeometry := AValue;

  ResetPageCache;
end;

procedure cComictecaVolumeRenderer.SetFlipL2R(AValue: boolean);
begin
  if FFlipL2R = AValue then
    Exit;
  FFlipL2R := AValue;

  if not Assigned(Comic) then
    Exit;

  if not Comic.Right2Left then
    Exit;


  if LastPageIndex < 0 then
    Exit;

  if Comic.Pages[LastPageIndex].NoFlip then
    Exit;

  ResetPageCache;
end;

procedure cComictecaVolumeRenderer.SetDebugRender(AValue: boolean);
begin
  if FDebugRender = AValue then
    Exit;
  FDebugRender := AValue;

  ResetPageCache;
end;

procedure cComictecaVolumeRenderer.SetShowFrameBorders(AValue: boolean);
begin
  if FShowFrameBorders = AValue then
    Exit;
  FShowFrameBorders := AValue;

  ResetPageCache;
end;

procedure cComictecaVolumeRenderer.SetShowGeometryQuad(AValue: boolean);
begin
  if FShowGeometryQuad = AValue then
    Exit;
  FShowGeometryQuad := AValue;

  ResetPageCache;
end;

procedure cComictecaVolumeRenderer.SetShowTextBorders(AValue: boolean);
begin
  if FShowTextBorders = AValue then
    Exit;
  FShowTextBorders := AValue;

  ResetPageCache;
end;

procedure cComictecaVolumeRenderer.IntRenderShape(aImg: TBGRABitmap;
  ImageShape: tCTKImageShape; X, Y: integer; DebugColor: TBGRAPixel);
var
  aMask: TBGRABitmap;
  aRect: TRect;
begin
  if not assigned(aImg) then
    Exit;

  if DebugRender then
  begin
    case ImageShape of
      CTKFSRndRect: aImg.RoundRect(0, 0, aImg.Width,
          aImg.Height, X, Y, DebugColor, dmDrawWithTransparency);
      CTKFSEllipse:
      begin
        aRect := Rect(0, 0, aImg.Width, aImg.Height);
        aImg.EllipseInRect(aRect, DebugColor, dmDrawWithTransparency);

        if X >= 0 then
          aRect.TopLeft.X := X
        else
          aRect.BottomRight.X := aRect.BottomRight.X + X;

        if Y >= 0 then
          aRect.TopLeft.Y := Y
        else
          aRect.BottomRight.Y := aRect.BottomRight.Y + Y;

        aImg.Rectangle(aRect, DebugColor, dmDrawWithTransparency);
      end;
      else
        ;
    end;
  end
  else
  begin
    // Render Frame Shape
    case ImageShape of
      CTKFSRndRect:
      begin
        aMask := TBGRABitmap.Create(aImg.Width, aImg.Height,
          BGRABlack);
        aMask.FillRoundRect(0, 0, aImg.Width, aImg.Height,
          X, Y, BGRAWhite);
        aImg.ApplyMask(aMask);
        aMask.Free;
      end;

      CTKFSEllipse:
      begin
        aMask := TBGRABitmap.Create(aImg.Width, aImg.Height,
          BGRABlack);
        aRect := Rect(0, 0, aImg.Width, aImg.Height);
        aMask.FillEllipseInRect(aRect, BGRAWhite);

        if X <> 0 then
        begin
          aRect := Rect(0, 0, aImg.Width, aImg.Height);

          if X > 0 then
            aRect.Right := X - 1
          else
            aRect.Left := aImg.Width + X;

          aMask.FillRect(aRect, BGRABlack);
        end;

        if Y <> 0 then
        begin
          aRect := Rect(0, 0, aImg.Width, aImg.Height);

          if Y > 0 then
            aRect.Top := Y
          else
            aRect.Bottom := aImg.Height + X;

          aMask.FillRect(aRect, BGRABlack);
        end;

        aImg.ApplyMask(aMask);
        aMask.Free;
      end;
      else // CTKFSRect
        ;
    end;
  end;
end;

procedure cComictecaVolumeRenderer.ResetFileCache;
begin
  // Page cache must reset too
  ResetPageCache;

  FreeAndNil(FLastRawImage);
  FLastRawImageFile := '';
end;

procedure cComictecaVolumeRenderer.ResetPageCache;
begin
  FreeAndNil(FLastPage);
  FLastPageIndex := -1;
end;

function cComictecaVolumeRenderer.RenderFile(aFile: string): TBGRABitmap;
begin
  Result := nil;

  if CompareFilenames(LastRawImageFile, aFile) = 0 then
  begin
    if Assigned(LastRawImage) then
      Result := LastRawImage.Duplicate(True);
    Exit;
  end;

  ResetFileCache;

  if not FileExistsUTF8(aFile) then
    Exit;

  FLastRawImage := TBGRABitmap.Create;
  LastRawImage.LoadFromFile(aFile);
  Result := LastRawImage.Duplicate(True);

  FLastRawImageFile := aFile;
end;

function cComictecaVolumeRenderer.RenderPageByIdx(aIndex: integer):
TBGRABitmap;
begin
  Result := nil;

  if (not assigned(Comic)) or (aIndex < -1) or
    (aIndex >= Comic.Pages.Count) then
    Exit;

  if aIndex = -1 then
  begin
    ResetPageCache;
    Exit;
  end;

  if LastPageIndex = aIndex then
  begin
    Result := LastPage.Duplicate(True);
    Exit;
  end;

  Result := RenderPage(Comic.Pages[aIndex]);
end;

function cComictecaVolumeRenderer.RenderFrameByIdx(aIndex:
  integer): TBGRABitmap;
begin
  Result := nil;

  if (not assigned(Comic)) or (aIndex <= -1) or
    (aIndex >= Comic.Frames.Count) then
    Exit;

  Result := RenderFrame(Comic.Frames[aIndex]);
end;

function cComictecaVolumeRenderer.RenderPage(
  aPage: cComictecaPage): TBGRABitmap;

  procedure DoFixGeometry(var aImage: TBGRABitmap; aPage: cComictecaPage);

    function GetLine(const a, b: TPoint): TLineDef;
    begin
      Result.origin := PointF(a);
      Result.dir := PointF(a - b);

      //if (a.X - b.X) = 0 then
      //  Result.dir := PointF(0, 1) // Vertical line
      //else
      //  Result.dir := PointF(1, (a.Y - b.Y) / (a.X - b.X));
    end;

  var // procedure DoFixGeometry(aImage: TBGRABitmap; aPage: cComictecaPage);
    TopLine, BottomLine, LeftLine, RightLine: TLineDef;
    cTL, cTR, cBR, cBL: TPointF;
    cWidth, cHeight: integer;
    correct: TBGRABitmap;
  begin
    if not aPage.HasGeometry then
      Exit;

    if not aPage.CropGeometry then
    begin
      // Lines
      TopLine := GetLine(aPage.GeomTL, aPage.GeomTR);
      TopLine.origin := PointF(aImage.Width shr 1, 0);

      RightLine := GetLine(aPage.GeomTR, aPage.GeomBR);
      RightLine.origin := PointF(aImage.Width, aImage.Height shr 1);

      BottomLine := GetLine(aPage.GeomBR, aPage.GeomBL);
      BottomLine.origin := PointF(aImage.Width shr 1, aImage.Height);

      LeftLine := GetLine(aPage.GeomBL, aPage.GeomTL);
      LeftLine.origin := PointF(0, aImage.Height shr 1);

      // Corners
      cTL := IntersectLine(TopLine, LeftLine);
      cTR := IntersectLine(TopLine, RightLine);
      cBR := IntersectLine(BottomLine, RightLine);
      cBL := IntersectLine(BottomLine, LeftLine);

      // Corrected Image size
      cWidth := aImage.Width;
      cHeight := aImage.Height;
    end
    else
    begin
      // Corners
      cTL := PointF(aPage.GeomTL);
      cTR := PointF(aPage.GeomTR);
      cBR := PointF(aPage.GeomBR);
      cBL := PointF(aPage.GeomBL);


      // Corrected Image size
      cWidth := Abs(Trunc((cTL.x - cTR.x) + (cBL.x - cBR.x))) shr 1;
      cHeight := Abs(Trunc((cTL.y - cBL.y) + (cTR.y - cBR.y))) shr 1;
    end;

    correct := TBGRABitmap.Create(cWidth, cHeight, bgra(0, 0, 0));

    if aPage.PerspGeometry then
      correct.FillQuadPerspectiveMappingAntialias(
        PointF(0, 0), PointF(cWidth, 0),
        PointF(cWidth, cHeight), PointF(0, cHeight),
        aImage, cTL, cTR, cBR, cBL)
    else
      correct.FillQuadLinearMappingAntialias(
        PointF(0, 0), PointF(cWidth, 0),
        PointF(cWidth, cHeight), PointF(0, cHeight),
        aImage, cTL, cTR, cBR, cBL);

    BGRAReplace(aImage, correct);
  end;

  procedure DrawBorders(aShape: tCTKImageShape; const aRect: TRect;
  const aPoint: TPoint; const BorderColor, BorderFill: TBGRAPixel);
  begin
    case aShape of
      CTKFSRndRect:
      begin
        LastPage.RoundRect(aRect.Left, aRect.Top, aRect.Right,
          aRect.Bottom, aPoint.X, aPoint.Y, BorderColor, BorderFill,
          dmLinearBlend);
      end;
      CTKFSEllipse:
      begin
        LastPage.EllipseInRect(aRect, BorderColor, BorderFill, dmLinearBlend);
      end;
      else // CTKFSRect
      begin
        LastPage.Rectangle(aRect, BorderColor, BorderFill, dmLinearBlend);
      end
    end;
  end;

var // cComictecaVolumeRenderer.RenderPage(aPage: cComictecaPage): TBGRABitmap;
  PageIndex, i: integer;
  aFrame: cComictecaFrame;
  aText: cComictecaText;
  TempImg: TBGRABitmap;
begin
  Result := nil;

  if (not assigned(Comic)) or (not assigned(aPage)) then
    Exit;

  // Search page in caché
  PageIndex := Comic.Pages.IndexOf(aPage);

  if PageIndex = -1 then
    Exit;

  if LastPageIndex = PageIndex then
  begin
    Result := LastPage.Duplicate(True);
    Exit;
  end;

  // Loading new page
  ResetPageCache;

  FLastPage := RenderFile(Comic.Folder + aPage.FileName);

  if not Assigned(LastPage) then
    Exit;

  // Fixing perspective
  if FixGeometry then
    DoFixGeometry(FLastPage, aPage);

  // Show perspective lines
  if ShowGeometryQuad and aPage.HasGeometry then
  begin
    LastPage.DrawPolygon([aPage.GeomTL, aPage.GeomTR,
      aPage.GeomBR, aPage.GeomBL], GeomQuadColor);
    // Diagonals
    LastPage.DrawLine(aPage.GeomTL.X, aPage.GeomTL.Y,
      aPage.GeomBR.X, aPage.GeomBR.Y, GeomQuadColor, True);
    LastPage.DrawLine(aPage.GeomTR.X, aPage.GeomTR.Y,
      aPage.GeomBL.X, aPage.GeomBL.Y, GeomQuadColor, True);
  end;

  // Draw frame borders
  if ShowFrameBorders then
  begin
    i := 0;
    while i < Comic.Frames.Count do
    begin
      aFrame := Comic.Frames[i];
      if aFrame.Page = aPage then
      begin
        DrawBorders(aFrame.ImgShape, aFrame.ImgRect, aFrame.ImgPoint,
          FrameBorderColor, FrameBorderFill);
      end;
      Inc(i);
    end;
  end;

  // Draw text borders
  if ShowTextBorders then
  begin
    i := 0;
    while i < aPage.Texts.Count do
    begin
      aText := aPage.Texts[i];
      DrawBorders(aText.ImgShape, aText.ImgRect, aText.ImgPoint,
        TextBorderColor, TextBorderFill);
      Inc(i);
    end;
  end;

  // Flip page and keep texts orientation
  if Comic.Right2Left and FlipL2R and (not aPage.NoFlip) then
  begin
    i := 0;
    while i < aPage.Texts.Count do
    begin
      aText := aPage.Texts[i];

      if not aText.ImgRect.IsEmpty then
      begin
      TempImg := LastPage.GetPart(aText.ImgRect);
      IntRenderShape(TempImg, aText.ImgShape, aText.ImgPoint.X,
        aText.ImgPoint.Y, BGRA(255, 0, 255));
      TempImg.HorizontalFlip;

      LastPage.PutImage(aText.ImgRect.Left, aText.ImgRect.Top, TempImg,
        dmDrawWithTransparency);

      TempImg.Free;
      end;
      Inc(i);
    end;

    LastPage.HorizontalFlip;
  end;

  // ¡Tachán!
  Result := LastPage.Duplicate(True);
  FLastPageIndex := PageIndex;
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

    // TODO: Replace HFlips with rect calculus?

    if Comic.Right2Left and FlipL2R and (not aPage.NoFlip) then
      aPageImg.HorizontalFlip;

    BGRAReplace(aPageImg, aPageImg.GetPart(aRect));

    if Comic.Right2Left and FlipL2R and (not aPage.NoFlip) then
      aPageImg.HorizontalFlip;
  end;

  Result := aPageImg;
end;

function cComictecaVolumeRenderer.RenderFrame(aFrame:
  cComictecaFrame): TBGRABitmap;
var
  aFrameImg: TBGRABitmap;
begin
  Result := nil;

  if (not assigned(Comic)) or (not assigned(aFrame)) then
    Exit;

  aFrameImg := RenderPageRect(cComictecaPage(aFrame.Page), aFrame.ImgRect);

  IntRenderShape(aFrameImg, aFrame.ImgShape, aFrame.ImgPoint.X,
    aFrame.ImgPoint.Y, FrameBorderColor);

  Result := aFrameImg;
end;

function cComictecaVolumeRenderer.RenderText(
  aText: cComictecaText): TBGRABitmap;
var
  aTextImg: TBGRABitmap;
begin
  Result := nil;

  if (not assigned(Comic)) or (not assigned(aText)) then
    Exit;

  aTextImg := RenderPageRect(cComictecaPage(aText.Page), aText.ImgRect);

  IntRenderShape(aTextImg, aText.ImgShape, aText.ImgPoint.X,
    aText.ImgPoint.Y, TextBorderColor);

  Result := aTextImg;
end;

constructor cComictecaVolumeRenderer.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FLastPageIndex := -1;
  FLastPage := nil;
  FLastRawImage := nil;
  FFlipL2R := False;
  FFixGeometry := True;
  FShowGeometryQuad := False;
  FShowFrameBorders := False;
  FShowTextBorders := False;
  FDebugRender := False;

  FrameBorderColor := BGRA(255, 0, 0);
  FrameBorderFill := BGRA(255, 0, 0, 128);
  TextBorderColor := BGRA(0, 0, 255);
  TextBorderFill := BGRA(0, 0, 255, 128);
  GeomQuadColor := BGRA(0, 128, 0);
end;

destructor cComictecaVolumeRenderer.Destroy;
begin
  FreeAndNil(FLastPage);
  FreeAndNil(FLastRawImage);

  inherited Destroy;
end;

end.
