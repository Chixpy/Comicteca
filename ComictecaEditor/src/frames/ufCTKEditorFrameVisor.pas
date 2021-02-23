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
  ucComictecaFrame,
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
    FPageFile: string;
    FPageImage: TBGRABitmap;
    procedure SetFrameImage(AValue: TBGRABitmap);
    procedure SetPageFile(AValue: string);
    procedure SetPageImage(AValue: TBGRABitmap);

  protected
    property fmVisor: TfmCHXBGRAImgViewerEx read FfmVisor;

    property PageFile: string read FPageFile write SetPageFile;
    property PageImage: TBGRABitmap read FPageImage write SetPageImage;

    property FrameImage: TBGRABitmap read FFrameImage write SetFrameImage;

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

procedure TfmCTKEditorFrameVisor.SetPageImage(AValue: TBGRABitmap);
begin
  if FPageImage = AValue then
    Exit;
  FPageImage := AValue;
end;

procedure TfmCTKEditorFrameVisor.SetPageFile(AValue: string);
begin
  if FPageFile = AValue then
    Exit;
  FPageFile := AValue;

  FreeAndNil(FPageImage);

  if not FileExists(PageFile) then
    Exit;

  FPageImage := TBGRABitmap.Create;
  PageImage.LoadFromFile(PageFile);

  DoLoadFrameFrame;
end;

procedure TfmCTKEditorFrameVisor.SetFrameImage(AValue: TBGRABitmap);
begin
  if FFrameImage = AValue then
    Exit;
  FFrameImage := AValue;
end;

procedure TfmCTKEditorFrameVisor.DoLoadFrameData;
begin
  ClearFrameData;

  Enabled := Assigned(Frame) and Assigned(Comic);

  if not Enabled then
    Exit;

  PageFile := Comic.Folder + Frame.Page.FileName;
end;

procedure TfmCTKEditorFrameVisor.DoClearFrameData;
begin
  fmVisor.ActualImage := nil;
  FreeAndNil(FFrameImage);
end;

procedure TfmCTKEditorFrameVisor.DoLoadFrameFrame;
begin
  DoClearFrameFrame;

  Enabled := Assigned(Frame) and Assigned(Comic);

  if not Enabled then
    Exit;

  if not Assigned(Frame.Page) then
    Exit;

  PageFile := Comic.Folder + Frame.Page.FileName;

  if (not Assigned(PageImage)) then
    Exit;

  Frame.Rect.NormalizeRect;

  if Frame.Rect.IsEmpty then
  begin
    fmVisor.ActualImage := PageImage;
  end
  else
  begin
    FreeAndNil(FFrameImage);
    FFrameImage := PageImage.GetPart(Frame.Rect);
    fmVisor.ActualImage := FrameImage;
  end;
end;

procedure TfmCTKEditorFrameVisor.DoClearFrameFrame;
begin
  fmVisor.ActualImage := nil;
  FreeAndNil(FFrameImage);
end;

procedure TfmCTKEditorFrameVisor.DoImgMouseDrag(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; aRect: TRect);
var
  CurrRect: TRect;
begin
  case Button of
    mbLeft:
    begin

      if Frame.Rect.IsEmpty then
        Frame.Rect := aRect
      else
      begin
        CurrRect := Frame.Rect;
        Frame.Rect := aRect;
        Frame.Rect.Offset(CurrRect.TopLeft);
      end;

      // Changing Rect don't notify observers
      Frame.FPONotifyObservers(Frame, ooChange, nil);
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
end;

destructor TfmCTKEditorFrameVisor.Destroy;
begin
  FreeAndNil(FFrameImage);
  FreeAndNil(FPageImage);

  inherited Destroy;
end;

end.
