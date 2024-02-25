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
    bAddAllFiles: TButton;
    bMoveDown: TButton;
    bRemoveFile: TButton;
    bSubir: TButton;
    bUpdateSHA1 : TButton;
    gbxImageFileList: TGroupBox;
    lbxImageList: TListBox;
    pImgListButtons: TPanel;
    procedure bAddAllFilesClick(Sender: TObject);
    procedure bMoveDownClick(Sender: TObject);
    procedure bRemoveFileClick(Sender: TObject);
    procedure bSubirClick(Sender: TObject);
    procedure bUpdateSHA1Click(Sender : TObject);
    procedure lbxImageListSelectionChange(Sender: TObject; User: boolean);

  private
    FOnPageSelect: TCTKPageObjProc;
    procedure SetOnPageSelect(AValue: TCTKPageObjProc);

  protected
    procedure LoadFrameData; override;
    procedure ClearFrameData; override;

  public
    property OnPageSelect: TCTKPageObjProc read FOnPageSelect write SetOnPageSelect;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCTKEditorImgList }

procedure TfmCTKEditorImgList.bAddAllFilesClick(Sender: TObject);
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

  // Adding only new pages
  i := 0;
  while i < aFileList.Count do
  begin
    aPage := cComictecaPage.Create(nil);
    aPage.FileName := aFileList[i];

    // Searching SHA1 of new pages
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

  if (lbxImageList.ItemIndex < 0) or
    (lbxImageList.ItemIndex > (lbxImageList.Items.Count - 2)) then
    Exit;

  Comic.Pages.Exchange(lbxImageList.ItemIndex, lbxImageList.ItemIndex + 1);
  lbxImageList.Items.Exchange(lbxImageList.ItemIndex, lbxImageList.ItemIndex + 1);

  lbxImageList.ItemIndex := lbxImageList.ItemIndex + 1;
end;

procedure TfmCTKEditorImgList.bRemoveFileClick(Sender: TObject);
var
  aPos: integer;
begin
  if not assigned(Comic) then
    Exit;

  if lbxImageList.ItemIndex < 0 then
    Exit;

  aPos := lbxImageList.ItemIndex;

  Comic.Pages.Delete(aPos);
  lbxImageList.Items.Delete(aPos);

  if lbxImageList.Count = 0 then
    OnPageSelect(nil)
  else
  begin
  if aPos >= lbxImageList.Count then
       lbxImageList.ItemIndex := lbxImageList.Count - 1
    else
      lbxImageList.ItemIndex := aPos;
  end;
end;

procedure TfmCTKEditorImgList.bSubirClick(Sender: TObject);
begin
    if not assigned(Comic) then
    Exit;

  if lbxImageList.ItemIndex < 1 then
    Exit;

  Comic.Pages.Exchange(lbxImageList.ItemIndex, lbxImageList.ItemIndex - 1);
  lbxImageList.Items.Exchange(lbxImageList.ItemIndex, lbxImageList.ItemIndex - 1);

  lbxImageList.ItemIndex := lbxImageList.ItemIndex - 1;
end;

procedure TfmCTKEditorImgList.bUpdateSHA1Click(Sender : TObject);
var
  aPage : cComictecaPage;
  i : LongInt;
begin
  // Updating SHA1 of all pages
  i := 0;
  while i < Comic.Pages.Count do
  begin
    aPage := Comic.Pages[i];

    if FileExistsUTF8(Comic.Folder + aPage.FileName) then
      aPage.SHA1 := SHA1FileStr(Comic.Folder + aPage.FileName);

    Inc(i);
  end;
end;

procedure TfmCTKEditorImgList.lbxImageListSelectionChange(Sender: TObject;
  User: boolean);
var
  aPage: cComictecaPage;
begin
  if lbxImageList.ItemIndex < 0 then
    aPage := nil
  else
    aPage := cComictecaPage(lbxImageList.Items.Objects[lbxImageList.ItemIndex]);

  if Assigned(OnPageSelect) then
    OnPageSelect(aPage);
end;

procedure TfmCTKEditorImgList.SetOnPageSelect(AValue: TCTKPageObjProc);
begin
  if FOnPageSelect = AValue then
    Exit;
  FOnPageSelect := AValue;
end;

procedure TfmCTKEditorImgList.LoadFrameData;
var
  i: integer;
begin
  ClearFrameData;

  inherited LoadFrameData;

  Enabled := assigned(Comic);

  if not Enabled then
    Exit;

  i := 0;
  while (i < Comic.Pages.Count) do
  begin
    lbxImageList.AddItem(Comic.Pages[i].FileName, Comic.Pages[i]);
    Inc(i);
  end;
end;

procedure TfmCTKEditorImgList.ClearFrameData;
begin
  inherited ClearFrameData;

  lbxImageList.Clear;
end;

constructor TfmCTKEditorImgList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmCTKEditorImgList.Destroy;
begin
  inherited Destroy;
end;

end.
