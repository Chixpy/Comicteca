unit ufCTKEditorPageVisor;

{< TfmCTKEditorPageVisor frame unit.

  This file is part of Comicteca Editor.

  Copyright (C) 2020 Chixpy

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
  ucComictecaVolume, ucComictecaPage, ucComictecaVolumeRenderer,
  // Comicteca Editor abstract frames
  uafCTKEditorPageFrame;

type

  { TfmCTKEditorPageVisor }

  TfmCTKEditorPageVisor = class(TafmCTKEditorPageFrame)
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
    FImage: TBGRABitmap;
    FRenderer: cComictecaVolumeRenderer;
    procedure SetImage(AValue: TBGRABitmap);

  protected
    property fmVisor: TfmCHXBGRAImgViewerEx read FfmVisor;
    property Image: TBGRABitmap read FImage write SetImage;

    procedure SetComic(AValue: cComictecaVolume); override;

    procedure DoLoadFrameData;
    procedure DoClearFrameData;

  public
    property Renderer: cComictecaVolumeRenderer read FRenderer;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCTKEditorPageVisor }
procedure TfmCTKEditorPageVisor.bZoomInClick(Sender: TObject);
begin
  tbxAutoZoom.Checked := False;

  fmVisor.ZoomIn;
end;

procedure TfmCTKEditorPageVisor.bOrigZoomClick(Sender: TObject);
begin
  tbxAutoZoom.Checked := False;

  fmVisor.Zoom := 100;
end;

procedure TfmCTKEditorPageVisor.bZoomOutClick(Sender: TObject);
begin
  tbxAutoZoom.Checked := False;

  fmVisor.ZoomOut;
end;

procedure TfmCTKEditorPageVisor.tbxAutoZoomChange(Sender: TObject);
begin
   fmVisor.AutoZoomOnLoad := tbxAutoZoom.Checked;

   if fmVisor.AutoZoomOnLoad then fmVisor.AutoZoom;
end;

procedure TfmCTKEditorPageVisor.SetImage(AValue: TBGRABitmap);
begin
  if FImage = AValue then
    Exit;
  FImage := AValue;
end;

procedure TfmCTKEditorPageVisor.SetComic(AValue: cComictecaVolume);
begin
  inherited SetComic(AValue);

  Renderer.Comic := AValue;
end;

procedure TfmCTKEditorPageVisor.DoLoadFrameData;
begin
  ClearFrameData;

  Enabled := Assigned(Page) and Assigned(Comic);

  if not Enabled then
    Exit;

  FImage := Renderer.RenderPage(Page);

  fmVisor.ActualImage := Image;
end;

procedure TfmCTKEditorPageVisor.DoClearFrameData;
begin
  fmVisor.ActualImage := nil;
  FreeAndNil(FImage);
end;

constructor TfmCTKEditorPageVisor.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FfmVisor := TfmCHXBGRAImgViewerEx.Create(pImageVisor);
    fmVisor.Align := alClient;
    fmVisor.AutoZoomOnLoad := True;
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
  Renderer.ShowPerspectiveQuad := True;
end;

destructor TfmCTKEditorPageVisor.Destroy;
begin
  FreeAndNil(FImage);
  FreeAndNil(FRenderer);

  inherited Destroy;
end;

end.
