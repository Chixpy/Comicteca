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
    bUpdate: TButton;
    bZoomIn: TButton;
    bZoomOut: TButton;
    chkFixGeometry: TCheckBox;
    gbxZoom: TGroupBox;
    pButtons: TPanel;
    pImageVisor: TPanel;
    tbxAutoZoom: TToggleBox;
    procedure bOrigZoomClick(Sender: TObject);
    procedure bUpdateClick(Sender: TObject);
    procedure bZoomInClick(Sender: TObject);
    procedure bZoomOutClick(Sender: TObject);
    procedure chkFixGeometryChange(Sender: TObject);
    procedure tbxAutoZoomChange(Sender: TObject);

  private
    FfmVisor: TfmCHXBGRAImgViewerEx;
    FPageImage: TBGRABitmap;
    FRenderer: cComictecaVolumeRenderer;
    procedure SetPageImage(AValue: TBGRABitmap);

  protected
    property PageImage: TBGRABitmap read FPageImage write SetPageImage;

    procedure SetComic(AValue: cComictecaVolume); override;

    procedure DoLoadFrameData;
    procedure DoClearFrameData;

  public
    property Renderer: cComictecaVolumeRenderer read FRenderer;
    property fmVisor: TfmCHXBGRAImgViewerEx read FfmVisor;

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

procedure TfmCTKEditorPageVisor.bUpdateClick(Sender: TObject);
begin
  Renderer.ResetPageCache;

  LoadFrameData;
end;

procedure TfmCTKEditorPageVisor.bZoomOutClick(Sender: TObject);
begin
  tbxAutoZoom.Checked := False;

  fmVisor.ZoomOut;
end;

procedure TfmCTKEditorPageVisor.chkFixGeometryChange(Sender: TObject);
begin
  Renderer.FixGeometry := chkFixGeometry.Checked;
  Renderer.ShowGeometryQuad := not chkFixGeometry.Checked;

  LoadFrameData;
end;

procedure TfmCTKEditorPageVisor.tbxAutoZoomChange(Sender: TObject);
begin
   fmVisor.AutoZoomOnLoad := tbxAutoZoom.Checked;

   if fmVisor.AutoZoomOnLoad then fmVisor.AutoZoom;
end;

procedure TfmCTKEditorPageVisor.SetPageImage(AValue: TBGRABitmap);
begin
  if FPageImage = AValue then
    Exit;
  FPageImage := AValue;
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

  FPageImage := Renderer.RenderPage(Page);

  fmVisor.ActualImage := PageImage;
end;

procedure TfmCTKEditorPageVisor.DoClearFrameData;
begin
  fmVisor.ActualImage := nil;
  FreeAndNil(FPageImage);
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
  Renderer.ShowGeometryQuad := True;
  Renderer.FixGeometry := False;
  Renderer.DebugRender := True;
end;

destructor TfmCTKEditorPageVisor.Destroy;
begin
  FreeAndNil(FPageImage);
  FreeAndNil(FRenderer);

  inherited Destroy;
end;

end.
