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
  ufCHXBGRAImgViewer,
  // Comicteca Core classes
  ucComictecaPage,
  // Comicteca Editor abstract frames
  uafCTKEditorFrame;

type

  { TfmCTKEditorPageVisor }

  TfmCTKEditorPageVisor = class(TafmCTKEditorFrame)
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
    FfmVisor: TfmCHXBGRAImgViewer;
    FImage: TBGRABitmap;
    FPage: cComictecaPage;
    procedure SetImage(AValue: TBGRABitmap);
    procedure SetPage(AValue: cComictecaPage);

  protected

    property fmVisor: TfmCHXBGRAImgViewer read FfmVisor;
    property Image: TBGRABitmap read FImage write SetImage;

    procedure DoLoadFrameData;
    procedure DoClearFrameData;

  public

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  published

    property Page: cComictecaPage read FPage write SetPage;
  end;

implementation

{$R *.lfm}

{ TfmCTKEditorPageVisor }

procedure TfmCTKEditorPageVisor.SetPage(AValue: cComictecaPage);
begin
  if FPage = AValue then
    Exit;
  FPage := AValue;

  LoadFrameData;
end;

procedure TfmCTKEditorPageVisor.bZoomInClick(Sender: TObject);
begin
  fmVisor.ZoomIn;
end;

procedure TfmCTKEditorPageVisor.bOrigZoomClick(Sender: TObject);
begin
  fmVisor.Zoom := 100;
end;

procedure TfmCTKEditorPageVisor.bZoomOutClick(Sender: TObject);
begin
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

procedure TfmCTKEditorPageVisor.DoLoadFrameData;
begin
  ClearFrameData;

  FreeAndNil(FImage);

  Enabled := Assigned(Page) and Assigned(Comic);

  if not Enabled then
    Exit;

  if not FileExists(Comic.Folder + Page.FileName) then
    Exit;

  FImage := TBGRABitmap.Create;
  Image.LoadFromFile(Comic.Folder + Page.FileName);

  fmVisor.ActualImage := Image;
end;

procedure TfmCTKEditorPageVisor.DoClearFrameData;
begin
  fmVisor.ActualImage := nil;
end;

constructor TfmCTKEditorPageVisor.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FfmVisor := TfmCHXBGRAImgViewer.Create(pImageVisor);
    fmVisor.Align := alClient;
    fmVisor.AutoZoomOnLoad := True;
    fmVisor.Parent := pImageVisor;
  end;

begin
  inherited Create(TheOwner);

  OnLoadFrameData := @DoLoadFrameData;
  OnClearFrameData := @DoClearFrameData;

  CreateFrames;
end;

destructor TfmCTKEditorPageVisor.Destroy;
begin
  FreeAndNil(FImage);

  inherited Destroy;
end;

end.
