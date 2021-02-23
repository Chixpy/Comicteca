unit ufCTKEditorTextVisor;
{< TfmCTKEditorTextVisor frame unit.

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  BGRABitmapTypes, BGRABitmap,
  // CHX frames
  ufCHXBGRAImgViewerEx,
  // Comicteca Core classes
  ucComictecaText,
  // Comicteca Editor abstract frames
  uafCTKEditorTextFrame;

type

  { TfmCTKEditorTextVisor }

  TfmCTKEditorTextVisor = class(TafmCTKEditorTextFrame)
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
    FPageFile: string;
    FPageImage: TBGRABitmap;
    FTextImage: TBGRABitmap;
    procedure SetPageFile(AValue: string);
    procedure SetPageImage(AValue: TBGRABitmap);
    procedure SetTextImage(AValue: TBGRABitmap);

  protected
    property fmVisor: TfmCHXBGRAImgViewerEx read FfmVisor;

    property PageFile: string read FPageFile write SetPageFile;
    property PageImage: TBGRABitmap read FPageImage write SetPageImage;

    property TextImage: TBGRABitmap read FTextImage write SetTextImage;

    procedure DoLoadFrameData;
    procedure DoClearFrameData;

    procedure DoLoadTextFrame; override;
    procedure DoClearTextFrame; override;

    procedure DoImgMouseDrag(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; aRect: TRect);

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCTKEditorTextVisor }

procedure TfmCTKEditorTextVisor.bZoomInClick(Sender: TObject);
begin
  tbxAutoZoom.Checked := False;

  fmVisor.ZoomIn;
end;

procedure TfmCTKEditorTextVisor.bOrigZoomClick(Sender: TObject);
begin
  tbxAutoZoom.Checked := False;

  fmVisor.Zoom := 100;
end;

procedure TfmCTKEditorTextVisor.bZoomOutClick(Sender: TObject);
begin
  tbxAutoZoom.Checked := False;

  fmVisor.ZoomOut;
end;

procedure TfmCTKEditorTextVisor.tbxAutoZoomChange(Sender: TObject);
begin
  fmVisor.AutoZoomOnLoad := tbxAutoZoom.Checked;

  if fmVisor.AutoZoomOnLoad then
    fmVisor.AutoZoom;
end;

procedure TfmCTKEditorTextVisor.SetPageFile(AValue: string);
begin
  if FPageFile = AValue then
    Exit;
  FPageFile := AValue;

  FreeAndNil(FPageImage);

  if not FileExists(PageFile) then
    Exit;

  FPageImage := TBGRABitmap.Create;
  PageImage.LoadFromFile(PageFile);

  DoLoadTextFrame;
end;

procedure TfmCTKEditorTextVisor.SetPageImage(AValue: TBGRABitmap);
begin
  if FPageImage = AValue then
    Exit;
  FPageImage := AValue;
end;

procedure TfmCTKEditorTextVisor.SetTextImage(AValue: TBGRABitmap);
begin
  if FTextImage = AValue then
    Exit;
  FTextImage := AValue;
end;

procedure TfmCTKEditorTextVisor.DoLoadFrameData;
begin
  ClearFrameData;

  Enabled := Assigned(CTKText) and Assigned(Comic);

  if not Enabled then
    Exit;

  PageFile := Comic.Folder + CTKText.Page.FileName;
end;

procedure TfmCTKEditorTextVisor.DoClearFrameData;
begin
  fmVisor.ActualImage := nil;
  FreeAndNil(FTextImage);
end;

procedure TfmCTKEditorTextVisor.DoLoadTextFrame;
begin
  DoClearTextFrame;

  Enabled := Assigned(CTKText) and Assigned(Comic);

  if not Enabled then
    Exit;

  if not Assigned(CTKText.Page) then
    Exit;

  PageFile := Comic.Folder + CTKText.Page.FileName;

  if (not Assigned(PageImage)) then
    Exit;

  CTKText.Rect.NormalizeRect;

  if CTKText.Rect.IsEmpty then
  begin
    fmVisor.ActualImage := PageImage;
  end
  else
  begin
    FreeAndNil(FTextImage);
    FTextImage := PageImage.GetPart(CTKText.Rect);
    fmVisor.ActualImage := TextImage;
  end;
end;

procedure TfmCTKEditorTextVisor.DoClearTextFrame;
begin
  fmVisor.ActualImage := nil;
  FreeAndNil(FTextImage);
end;

procedure TfmCTKEditorTextVisor.DoImgMouseDrag(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; aRect: TRect);
var
  CurrRect: TRect;
begin
  case Button of
    mbLeft:
    begin

      if CTKText.Rect.IsEmpty then
        CTKText.Rect := aRect
      else
      begin
        CurrRect := CTKText.Rect;
        CTKText.Rect := aRect;
        CTKText.Rect.Offset(CurrRect.TopLeft);
      end;

      // Changing Rect don't notify observers
      CTKText.FPONotifyObservers(CTKText, ooChange, nil);
    end;
    mbRight: ;
    mbMiddle: ;
    mbExtra1: ;
    mbExtra2: ;
    else
      ;
  end;
end;

constructor TfmCTKEditorTextVisor.Create(TheOwner: TComponent);

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

destructor TfmCTKEditorTextVisor.Destroy;
begin
  FreeAndNil(FTextImage);
  FreeAndNil(FPageImage);

  inherited Destroy;
end;

end.
