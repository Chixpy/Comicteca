unit ufCTKEditorTextVisor;

{< TfmCTKEditorTextVisor frame unit.

  This file is part of Comicteca Core.

  Copyright (C) 2021-2024 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ColorBox, BGRABitmapTypes, BGRABitmap,
  // CHX frames
  ufCHXBGRAImgViewerEx,
  // Comicteca Core classes
  ucComictecaVolume, ucComictecaPage, ucComictecaText,
  ucComictecaVolumeRenderer,
  // Comicteca Editor abstract frames
  uafCTKEditorTextFrame;

type

  { TfmCTKEditorTextVisor }

  TfmCTKEditorTextVisor = class(TafmCTKEditorTextFrame)
    bOrigZoom : TButton;
    bZoomIn : TButton;
    bZoomOut : TButton;
    cbBackground : TColorBox;
    gbxBackground : TGroupBox;
    gbxZoom : TGroupBox;
    pImageVisor : TPanel;
    pTools : TPanel;
    tbxAutoZoom : TToggleBox;
    procedure bOrigZoomClick(Sender : TObject);
    procedure bZoomInClick(Sender : TObject);
    procedure bZoomOutClick(Sender : TObject);
    procedure cbBackgroundSelect(Sender : TObject);
    procedure tbxAutoZoomChange(Sender : TObject);

  private
    FfmVisor : TfmCHXBGRAImgViewerEx;
    FRenderer : cComictecaVolumeRenderer;
    FTextImage : TBGRABitmap;
    procedure SetTextImage(AValue : TBGRABitmap);

  protected
    property fmVisor : TfmCHXBGRAImgViewerEx read FfmVisor;
    property TextImage : TBGRABitmap read FTextImage write SetTextImage;

    property Renderer : cComictecaVolumeRenderer read FRenderer;

    procedure SetComic(AValue : cComictecaVolume); override;

    procedure DoLoadTextFrame; override;
    procedure DoClearTextFrame; override;

    procedure DoImgMouseDrag(Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; aRect : TRect);
  public
    procedure ShowPage(aPage : cComictecaPage);

    procedure LoadFrameData; override;
    procedure ClearFrameData; override;

    constructor Create(TheOwner : TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCTKEditorTextVisor }

procedure TfmCTKEditorTextVisor.bZoomInClick(Sender : TObject);
begin
  tbxAutoZoom.Checked := False;

  fmVisor.ZoomIn;
end;

procedure TfmCTKEditorTextVisor.bOrigZoomClick(Sender : TObject);
begin
  tbxAutoZoom.Checked := False;

  fmVisor.Zoom := 100;
end;

procedure TfmCTKEditorTextVisor.bZoomOutClick(Sender : TObject);
begin
  tbxAutoZoom.Checked := False;

  fmVisor.ZoomOut;
end;

procedure TfmCTKEditorTextVisor.cbBackgroundSelect(Sender : TObject);
begin
  fmVisor.sbxImage.Color := cbBackground.Selected;
end;

procedure TfmCTKEditorTextVisor.tbxAutoZoomChange(Sender : TObject);
begin
  fmVisor.AutoZoomOnLoad := tbxAutoZoom.Checked;

  if fmVisor.AutoZoomOnLoad then
    fmVisor.AutoZoom;
end;

procedure TfmCTKEditorTextVisor.SetTextImage(AValue : TBGRABitmap);
begin
  if FTextImage = AValue then
    Exit;
  FTextImage := AValue;
end;

procedure TfmCTKEditorTextVisor.SetComic(AValue : cComictecaVolume);
begin
  inherited SetComic(AValue);

  Renderer.Comic := AValue;
end;

procedure TfmCTKEditorTextVisor.ShowPage(aPage : cComictecaPage);
begin
  ClearFrameData;

  Enabled := Assigned(aPage) and Assigned(Comic);

  if not Enabled then
    Exit;

  Renderer.ShowTextBorders := True;
  FTextImage := Renderer.RenderPage(aPage);

  fmVisor.ActualImage := FTextImage;
end;

procedure TfmCTKEditorTextVisor.LoadFrameData;
begin
  ClearFrameData;

  inherited;

  Enabled := Assigned(CTKText) and Assigned(Comic);

  if not Enabled then
    Exit;

  Renderer.ShowTextBorders := CTKText.ImgRect.IsEmpty;
  Renderer.ResetPageCache;
  FTextImage := Renderer.RenderText(CTKText);

  fmVisor.ActualImage := FTextImage;
end;

procedure TfmCTKEditorTextVisor.ClearFrameData;
begin
  inherited;

  fmVisor.ActualImage := nil;
  FreeAndNil(FTextImage);
end;

procedure TfmCTKEditorTextVisor.DoLoadTextFrame;
begin
  LoadFrameData;
end;

procedure TfmCTKEditorTextVisor.DoClearTextFrame;
begin
  ClearFrameData;
end;

procedure TfmCTKEditorTextVisor.DoImgMouseDrag(Sender : TObject;
  Button : TMouseButton; Shift : TShiftState; aRect : TRect);
var
  CurrRect : TRect;
begin
  if not assigned(CTKText) then
    Exit;

  case Button of
    mbLeft : begin

      if CTKText.ImgRect.IsEmpty then
        CTKText.ImgRect := aRect
      else
      begin
        CurrRect := CTKText.ImgRect;
        CTKText.ImgRect := aRect;
        CTKText.ImgRect.Offset(CurrRect.TopLeft);
      end;

      // Changing Rect don't notify observers
      CTKText.FPONotifyObservers(CTKText, ooChange, nil);
    end;
    mbRight : ;
    mbMiddle : ;
    mbExtra1 : ;
    mbExtra2 : ;
    else
      ;
  end;
end;

constructor TfmCTKEditorTextVisor.Create(TheOwner : TComponent);

  procedure CreateFrames;
  begin
    FfmVisor := TfmCHXBGRAImgViewerEx.Create(pImageVisor);
    fmVisor.Align := alClient;
    fmVisor.sbxImage.Color := cbBackground.Selected;
    fmVisor.AutoZoomOnLoad := True;
    fmVisor.MouseActionMode := maiMouseSelectRect;
    fmVisor.OnImgMouseDrag := @DoImgMouseDrag;
    fmVisor.Parent := pImageVisor;
  end;

begin
  inherited Create(TheOwner);

  CreateFrames;

  FRenderer := cComictecaVolumeRenderer.Create(nil);
  Renderer.ShowFrameBorders := True;
  Renderer.ShowTextBorders := True;
  Renderer.ShowGeometryQuad := False;
  Renderer.DebugRender := True;
end;

destructor TfmCTKEditorTextVisor.Destroy;
begin
  FreeAndNil(FTextImage);
  FreeAndNil(FRenderer);

  inherited Destroy;
end;

end.
{
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
