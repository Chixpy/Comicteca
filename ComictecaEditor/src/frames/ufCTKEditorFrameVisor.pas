unit ufCTKEditorFrameVisor;

{< TfmCTKEditorFrameVisor frame unit.

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  BGRABitmapTypes, BGRABitmap,
  // CHX frames
  ufCHXBGRAImgViewerEx,
  // Comicteca Core classes
  ucComictecaVolume, ucComictecaFrame, ucComictecaVolumeRenderer,
  // Comicteca Editor abstract frames
  uafCTKEditorFrameFrame;

type

  { TfmCTKEditorFrameVisor }

  TfmCTKEditorFrameVisor = class(TafmCTKEditorFrameFrame)
    bOrigZoom: TButton;
    bZoomIn: TButton;
    bZoomOut: TButton;
    gbxZoom: TGroupBox;
    pImageVisor: TPanel;
    tbxAutoZoom: TToggleBox;
    procedure bOrigZoomClick(Sender: TObject);
    procedure bZoomInClick(Sender: TObject);
    procedure bZoomOutClick(Sender: TObject);
    procedure tbxAutoZoomChange(Sender: TObject);

  private
    FfmVisor: TfmCHXBGRAImgViewerEx;
    FFrameImage: TBGRABitmap;
    FRenderer: cComictecaVolumeRenderer;
    procedure SetFrameImage(AValue: TBGRABitmap);

  protected
    property fmVisor: TfmCHXBGRAImgViewerEx read FfmVisor;
    property FrameImage: TBGRABitmap read FFrameImage write SetFrameImage;

    property Renderer: cComictecaVolumeRenderer read FRenderer;

    procedure SetComic(AValue: cComictecaVolume); override;

    procedure DoLoadFrameData;
    procedure DoClearFrameData;

    procedure DoLoadFrameFrame; override;
    procedure DoClearFrameFrame; override;

    procedure DoImgMouseDrag(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; aRect: TRect);

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{$R *.lfm}

{ TfmCTKEditorFrameVisor }

procedure TfmCTKEditorFrameVisor.bZoomInClick(Sender: TObject);
begin
  tbxAutoZoom.Checked := False;

  fmVisor.ZoomIn;
end;

procedure TfmCTKEditorFrameVisor.bZoomOutClick(Sender: TObject);
begin
  tbxAutoZoom.Checked := False;

  fmVisor.ZoomOut;
end;

procedure TfmCTKEditorFrameVisor.tbxAutoZoomChange(Sender: TObject);
begin
  fmVisor.AutoZoomOnLoad := tbxAutoZoom.Checked;

  if fmVisor.AutoZoomOnLoad then
    fmVisor.AutoZoom;
end;

procedure TfmCTKEditorFrameVisor.bOrigZoomClick(Sender: TObject);
begin
  tbxAutoZoom.Checked := False;

  fmVisor.Zoom := 100;
end;

procedure TfmCTKEditorFrameVisor.SetFrameImage(AValue: TBGRABitmap);
begin
  if FFrameImage = AValue then
    Exit;
  FFrameImage := AValue;
end;

procedure TfmCTKEditorFrameVisor.SetComic(AValue: cComictecaVolume);
begin
  inherited SetComic(AValue);

  Renderer.Comic := AValue;
end;

procedure TfmCTKEditorFrameVisor.DoLoadFrameData;
begin
  ClearFrameData;

  Enabled := Assigned(CTKFrame) and Assigned(Comic);

  if not Enabled then
    Exit;

  Renderer.ShowFrameBorders := CTKFrame.FrameRect.IsEmpty;
  FFrameImage := Renderer.RenderFrame(CTKFrame);

  fmVisor.ActualImage := FrameImage;
end;

procedure TfmCTKEditorFrameVisor.DoClearFrameData;
begin
  fmVisor.ActualImage := nil;
  FreeAndNil(FFrameImage);
end;

procedure TfmCTKEditorFrameVisor.DoLoadFrameFrame;
begin
  DoLoadFrameData;
end;

procedure TfmCTKEditorFrameVisor.DoClearFrameFrame;
begin
  DoClearFrameData;
end;

procedure TfmCTKEditorFrameVisor.DoImgMouseDrag(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; aRect: TRect);
var
  CurrRect: TRect;
begin
  case Button of
    mbLeft:
    begin

      if CTKFrame.FrameRect.IsEmpty then
        CTKFrame.FrameRect := aRect
      else
      begin
        CurrRect := CTKFrame.FrameRect;
        CTKFrame.FrameRect := aRect;
        CTKFrame.FrameRect.Offset(CurrRect.TopLeft);
      end;

      // Changing Rect don't notify observers
      CTKFrame.FPONotifyObservers(CTKFrame, ooChange, nil);
    end;
    mbRight: ;
    mbMiddle: ;
    mbExtra1: ;
    mbExtra2: ;
  end;
end;

constructor TfmCTKEditorFrameVisor.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FfmVisor := TfmCHXBGRAImgViewerEx.Create(pImageVisor);
    fmVisor.Align := alClient;
    fmVisor.AutoZoomOnLoad := True;
    fmVisor.MouseActionMode := maiMouseSelectRect;
    fmVisor.OnImgMouseDrag := @DoImgMouseDrag;
    fmVisor.Parent := pImageVisor;
  end;

begin
  inherited Create(TheOwner);

  OnLoadFrameData := @DoLoadFrameData;
  OnClearFrameData := @DoClearFrameData;

  CreateFrames;

  FRenderer := cComictecaVolumeRenderer.Create(nil);
  Renderer.ShowFrameBorders := True;
  Renderer.ShowTextBorders := True;
  Renderer.ShowPerspectiveQuad := False;
  Renderer.DebugRender := True;
end;

destructor TfmCTKEditorFrameVisor.Destroy;
begin
  FreeAndNil(FFrameImage);
  FreeAndNil(FRenderer);

  inherited Destroy;
end;

end.
