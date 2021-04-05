unit ufCTKEditorMain;

{< TfmCTKEditorMain frame unit.

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls,
  // CHx units
  uCHXStrUtils,
  // Comicteca Core classes
  ucComictecaPage, ucComictecaFrame, ucComictecaText,
  // Comicteca Editor abstract frames
  uafCTKEditorFrame, uafCTKEditorPageFrame, uafCTKEditorFrameFrame,
  uafCTKEditorTextFrame,
  // Comicteca Editor frames
  ufCTKEditorImgList, ufCTKEditorFrameList, ufCTKEditorTextList,
  ufCTKEditorVolumeEdit, ufCTKEditorPageEdit, ufCTKEditorFrameEdit,
  ufCTKEditorTextEdit,
  ufCTKEditorPageVisor, ufCTKEditorFrameVisor, ufCTKEditorTextVisor;

const
  kDataFolder = 'Data';

type

  { TfmCTKEditorMain }

  TfmCTKEditorMain = class(TafmCTKEditorFrame)
    pCenter: TPanel;
    pLeft: TPanel;
    pRight: TPanel;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    tbcMainFrame: TTabControl;
    procedure tbcMainFrameChange(Sender: TObject);

  private
    FDataFolder: string;
    FfmCenter: TafmCTKEditorFrame;

    FfmLeft: TafmCTKEditorFrame;
    FfmRight: TafmCTKEditorFrame;
    procedure SetDataFolder(AValue: string);

  protected
    procedure DoLoadFrameData;

    procedure DoSelectPage(aCTKPage: cComictecaPage);
    procedure DoSelectFrame(aCTKFrame: cComictecaFrame);
    procedure DoSelectText(aCTKText: cComictecaText);

    procedure UpdateFrames;

  public
    property fmLeft: TafmCTKEditorFrame read FfmLeft;
    property fmCenter: TafmCTKEditorFrame read FfmCenter;
    property fmRight: TafmCTKEditorFrame read FfmRight;

    property DataFolder: string read FDataFolder write SetDataFolder;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCTKEditorMain }

procedure TfmCTKEditorMain.tbcMainFrameChange(Sender: TObject);
begin
  FreeAndNil(FfmLeft);
  FreeAndNil(FfmCenter);
  FreeAndNil(FfmRight);

  case tbcMainFrame.TabIndex of
    //0: ; Volume (it's below in else case)
    1: // Pages
    begin
      FfmLeft := TfmCTKEditorImgList.Create(pLeft);
      TfmCTKEditorImgList(fmLeft).OnPageSelect := @DoSelectPage;
      fmLeft.Align := alClient;
      fmLeft.Parent := pLeft;

      FfmCenter := TfmCTKEditorPageEdit.Create(pCenter);
      fmCenter.Align := alClient;
      fmCenter.Parent := pCenter;

      FfmRight := TfmCTKEditorPageVisor.Create(pRight);
      fmRight.Align := alClient;
      fmRight.Parent := pRight;

      TfmCTKEditorPageEdit(FfmCenter).Visor := TfmCTKEditorPageVisor(FfmRight).fmVisor;
    end;
    2: // Frames
    begin
      FfmLeft := TfmCTKEditorFrameList.Create(pLeft);
      TfmCTKEditorFrameList(fmLeft).OnFrameSelect := @DoSelectFrame;
      fmLeft.Align := alClient;
      fmLeft.Parent := pLeft;

      FfmCenter := TfmCTKEditorFrameEdit.Create(pCenter);
      fmCenter.Align := alClient;
      fmCenter.Parent := pCenter;

      FfmRight := TfmCTKEditorFrameVisor.Create(pRight);
      fmRight.Align := alClient;
      fmRight.Parent := pRight;
    end;
    3: // Texts
    begin
      FfmLeft := TfmCTKEditorTextList.Create(pLeft);
      TfmCTKEditorTextList(fmLeft).OnTextSelect := @DoSelectText;
      fmLeft.Align := alClient;
      fmLeft.Parent := pLeft;

      FfmCenter := TfmCTKEditorTextEdit.Create(pCenter);
      TfmCTKEditorTextEdit(fmCenter).DataFolder := kDataFolder;
      fmCenter.Align := alClient;
      fmCenter.Parent := pCenter;

      FfmRight := TfmCTKEditorTextVisor.Create(pRight);
      fmRight.Comic := Comic; // Fix to show first page
      fmRight.Align := alClient;
      fmRight.Parent := pRight;

      TfmCTKEditorTextList(fmLeft).OnPageSelect := @TfmCTKEditorTextVisor(fmRight).ShowPage;
    end;
    4: // Metadata
    begin

    end;
    else // Volume
    begin
      FfmLeft := TfmCTKEditorVolumeEdit.Create(pLeft);
      TfmCTKEditorVolumeEdit(fmLeft).DataFolder := kDataFolder;
      fmLeft.Align := alClient;
      fmLeft.Parent := pLeft;

      FfmCenter := TfmCTKEditorImgList.Create(pCenter);
      TfmCTKEditorImgList(fmCenter).OnPageSelect := @DoSelectPage;
      fmCenter.Align := alClient;
      fmCenter.Parent := pCenter;

      FfmRight := TfmCTKEditorPageVisor.Create(pRight);
      fmRight.Align := alClient;
      fmRight.Parent := pRight;
    end;
  end;

  UpdateFrames;
end;

procedure TfmCTKEditorMain.SetDataFolder(AValue: string);
begin
  FDataFolder := SetAsFolder(AValue);
end;

procedure TfmCTKEditorMain.DoLoadFrameData;
begin
  UpdateFrames;

  Enabled := Assigned(Comic);
end;

procedure TfmCTKEditorMain.DoSelectPage(aCTKPage: cComictecaPage);
begin
  if assigned(fmLeft) and (fmLeft is TafmCTKEditorPageFrame) then
    TafmCTKEditorPageFrame(fmLeft).Page := aCTKPage;

  if assigned(fmCenter) and (fmCenter is TafmCTKEditorPageFrame) then
    TafmCTKEditorPageFrame(fmCenter).Page := aCTKPage;

  if assigned(fmRight) and (fmRight is TafmCTKEditorPageFrame) then
    TafmCTKEditorPageFrame(fmRight).Page := aCTKPage;
end;

procedure TfmCTKEditorMain.DoSelectFrame(aCTKFrame: cComictecaFrame);
begin
  if assigned(fmLeft) and (fmLeft is TafmCTKEditorFrameFrame) then
    TafmCTKEditorFrameFrame(fmLeft).CTKFrame := aCTKFrame;

  if assigned(fmCenter) and (fmCenter is TafmCTKEditorFrameFrame) then
    TafmCTKEditorFrameFrame(fmCenter).CTKFrame := aCTKFrame;

  if assigned(fmRight) and (fmRight is TafmCTKEditorFrameFrame) then
    TafmCTKEditorFrameFrame(fmRight).CTKFrame := aCTKFrame;
end;

procedure TfmCTKEditorMain.DoSelectText(aCTKText: cComictecaText);
begin
  if assigned(fmLeft) and (fmLeft is TafmCTKEditorTextFrame) then
    TafmCTKEditorTextFrame(fmLeft).CTKText := aCTKText;

  if assigned(fmCenter) and (fmCenter is TafmCTKEditorTextFrame) then
    TafmCTKEditorTextFrame(fmCenter).CTKText := aCTKText;

  if assigned(fmRight) and (fmRight is TafmCTKEditorTextFrame) then
    TafmCTKEditorTextFrame(fmRight).CTKText := aCTKText;
end;

procedure TfmCTKEditorMain.UpdateFrames;
begin
  if assigned(fmLeft) then
    fmLeft.Comic := Comic;

  if assigned(fmCenter) then
    fmCenter.Comic := Comic;

  if assigned(fmRight) then
    fmRight.Comic := Comic;
end;

constructor TfmCTKEditorMain.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnLoadFrameData := @DoLoadFrameData;

  tbcMainFrameChange(tbcMainFrame);
end;

destructor TfmCTKEditorMain.Destroy;
begin
  inherited Destroy;
end;

end.
