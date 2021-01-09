unit ufCTKEditorMain;

{< TfmCTKEditorMain frame unit.

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls,
  // CHx units
  uCHXStrUtils,
  // Comicteca Core classes
  ucComictecaPage, ucComictecaFrame,
  // Comicteca Editor abstract frames
  uafCTKEditorFrame, uafCTKEditorPageFrame, uafCTKEditorFrameFrame,
  // Comicteca Editor frames
  ufCTKEditorVolumeEdit, ufCTKEditorImgList, ufCTKEditorPageEdit,
  ufCTKEditorPageVisor, ufCTKEditorFrameList;

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
    end;
    2: // Vignettes
    begin
      FfmLeft := TfmCTKEditorFrameList.Create(pLeft);
      fmLeft.Align := alClient;
      fmLeft.Parent := pLeft;
    end;
    3: // Texts
    begin
      FfmLeft := TfmCTKEditorFrameList.Create(pLeft);
      fmLeft.Align := alClient;
      fmLeft.Parent := pLeft;
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
    TafmCTKEditorPageFrame(fmRight).Page := aCTKPage
end;

procedure TfmCTKEditorMain.DoSelectFrame(aCTKFrame: cComictecaFrame);
begin
  if assigned(fmLeft) and (fmLeft is TafmCTKEditorFrameFrame) then
    TafmCTKEditorFrameFrame(fmLeft).Frame := aCTKFrame;

  if assigned(fmCenter) and (fmCenter is TafmCTKEditorFrameFrame) then
    TafmCTKEditorFrameFrame(fmCenter).Frame := aCTKFrame;

  if assigned(fmRight) and (fmRight is TafmCTKEditorFrameFrame) then
    TafmCTKEditorFrameFrame(fmRight).Frame := aCTKFrame;
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