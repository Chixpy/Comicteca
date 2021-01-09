unit ufCTKEditorImgList;

{< TfmCTKEditorImgList frame unit.

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, ExtCtrls, FileUtil, LazFileUtils,
  // CHX units
  uCHXStrUtils, uCHXFileUtils,
  // Comicteca Core classes;
  ucComictecaPage,
  // Comicteca Editor abstract frames
  uafCTKEditorFrame;

type

  { TfmCTKEditorImgList }

  TfmCTKEditorImgList = class(TafmCTKEditorFrame)
    BAddAllFiles: TButton;
    bMoveDown: TButton;
    bRemoveFile: TButton;
    bSubir: TButton;
    gbxImageFileList: TGroupBox;
    lvFileList: TListView;
    pImgListButtons: TPanel;
    procedure BAddAllFilesClick(Sender: TObject);
    procedure bMoveDownClick(Sender: TObject);
    procedure bRemoveFileClick(Sender: TObject);
    procedure bSubirClick(Sender: TObject);
    procedure lvFileListSelectItem(Sender: TObject; Item: TListItem;
      Selected: boolean);

  private
    FOnPageSelect: TCTKPageObjProc;
    procedure SetOnPageSelect(AValue: TCTKPageObjProc);

  protected

    procedure DoLoadFrameData;
    procedure DoClearFrameData;

  public

    property OnPageSelect: TCTKPageObjProc
      read FOnPageSelect write SetOnPageSelect;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCTKEditorImgList }

procedure TfmCTKEditorImgList.BAddAllFilesClick(Sender: TObject);
var
  aFileList: TStringList;
  aPage: cComictecaPage;
  i, j: integer;
begin
  if not assigned(Comic) then
    Exit;

  aFileList := TStringList.Create;

  // Loading all image files
  FindAllFiles(aFileList, Comic.Folder, GraphicFileMask(TGraphic), True);

  // Removing paths
  i := 0;
  while i < aFileList.Count do
  begin
    aFileList[i] := CreateRelativeSearchPath(SysPath(aFileList[i]),
      SysPath(Comic.Folder));
    Inc(i);
  end;

  // Removing duplicates
  j := 0;
  while (j < Comic.Pages.Count) do
  begin
    i := 0;
    while (i < aFileList.Count) do
    begin
      if CompareFilenames(Comic.Pages[j].FileName, aFileList[i]) = 0 then
      begin
        aFileList.Delete(i);
      end
      else
        Inc(i);
    end;
    Inc(j);
  end;

  // Adding new pages
  i := 0;
  while i < aFileList.Count do
  begin
    aPage := cComictecaPage.Create(nil);
    aPage.FileName := aFileList[i];

    // Searching SHA1
    if FileExistsUTF8(Comic.Folder + aPage.FileName) then
      aPage.SHA1 := SHA1FileStr(Comic.Folder + aPage.FileName);

    Comic.Pages.Add(aPage);
    Inc(i);
  end;

  aFileList.Free;

  LoadFrameData;
end;

procedure TfmCTKEditorImgList.bMoveDownClick(Sender: TObject);
begin
  if not assigned(Comic) then
    Exit;

  if not lvFileList.ItemIndex in [0..lvFileList.Items.Count - 1] then
    Exit;

  Comic.Pages.Exchange(lvFileList.ItemIndex, lvFileList.ItemIndex + 1);
  lvFileList.Items.Exchange(lvFileList.ItemIndex, lvFileList.ItemIndex + 1);
end;

procedure TfmCTKEditorImgList.bRemoveFileClick(Sender: TObject);
begin
  if not assigned(Comic) then
    Exit;

  if lvFileList.ItemIndex < 0 then
    Exit;

  Comic.Pages.Delete(lvFileList.ItemIndex);
  lvFileList.Items.Delete(lvFileList.ItemIndex);
end;

procedure TfmCTKEditorImgList.bSubirClick(Sender: TObject);
begin
    if not assigned(Comic) then
    Exit;

  if lvFileList.ItemIndex < 1 then
    Exit;

  Comic.Pages.Exchange(lvFileList.ItemIndex, lvFileList.ItemIndex - 1);
  lvFileList.Items.Exchange(lvFileList.ItemIndex, lvFileList.ItemIndex - 1);
end;

procedure TfmCTKEditorImgList.lvFileListSelectItem(Sender: TObject;
  Item: TListItem; Selected: boolean);
begin
  if not assigned(OnPageSelect) then
    Exit;
  if Selected then
    OnPageSelect(cComictecaPage(Item.Data))
  else
    OnPageSelect(nil);
end;

procedure TfmCTKEditorImgList.SetOnPageSelect(AValue: TCTKPageObjProc);
begin
  if FOnPageSelect = AValue then
    Exit;
  FOnPageSelect := AValue;
end;

procedure TfmCTKEditorImgList.DoLoadFrameData;
var
  i: integer;
begin
  ClearFrameData;

  Enabled := assigned(Comic);

  if not Enabled then
    Exit;

  i := 0;
  while (i < Comic.Pages.Count) do
  begin
    lvFileList.AddItem(Comic.Pages[i].FileName, Comic.Pages[i]);
    Inc(i);
  end;
end;

procedure TfmCTKEditorImgList.DoClearFrameData;
begin
  lvFileList.Clear;
end;

constructor TfmCTKEditorImgList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnLoadFrameData := @DoLoadFrameData;
  OnClearFrameData := @DoClearFrameData;
end;

destructor TfmCTKEditorImgList.Destroy;
begin
  inherited Destroy;
end;

end.
