unit ufCTKEditorTextList;
{< TfmCTKEditorTextList frame unit.

  This file is part of Comicteca Core.

  Copyright (C) 2023 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  // CHX units
  uCHXRecords,
  // Comiteca Core classes
  ucComictecaPage, ucComictecaText,
  // Comicteca Editor abstract frames
  uafCTKEditorFrame;

type

  { TfmCTKEditorTextList }

  TfmCTKEditorTextList = class(TafmCTKEditorFrame)
    bAddText : TButton;
    bAddTextCopy : TButton;
    bMoveDown : TButton;
    bRemoveText : TButton;
    bMoveUp : TButton;
    cbxPage : TComboBox;
    gbxPage : TGroupBox;
    gbxTexts : TGroupBox;
    lbxTexts : TListBox;
    pFrameListButtons : TPanel;
    procedure bAddTextClick(Sender : TObject);
    procedure bAddTextCopyClick(Sender : TObject);
    procedure bMoveDownClick(Sender : TObject);
    procedure bRemoveTextClick(Sender : TObject);
    procedure bMoveUpClick(Sender : TObject);
    procedure cbxPageChange(Sender : TObject);
    procedure lbxTextsSelectionChange(Sender : TObject; User : boolean);

  private
    FCurrentPage : cComictecaPage;
    FOnPageSelect : TCTKPageObjProc;
    FOnTextSelect : TCTKTextObjProc;
    procedure SetCurrentPage(AValue : cComictecaPage);
    procedure SetOnPageSelect(AValue : TCTKPageObjProc);
    procedure SetOnTextSelect(AValue : TCTKTextObjProc);

  protected
    procedure LoadFrameData; override;
    procedure ClearFrameData; override;

    procedure LoadTextList;
  public
    property CurrentPage : cComictecaPage
      read FCurrentPage write SetCurrentPage;
    property OnTextSelect : TCTKTextObjProc
      read FOnTextSelect write SetOnTextSelect;
    property OnPageSelect : TCTKPageObjProc
      read FOnPageSelect write SetOnPageSelect;

    constructor Create(TheOwner : TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCTKEditorTextList }

procedure TfmCTKEditorTextList.bMoveDownClick(Sender : TObject);
begin
  if not assigned(CurrentPage) then
    Exit;

  if (lbxTexts.ItemIndex < 0) or (lbxTexts.ItemIndex >
    (lbxTexts.Items.Count - 2)) then
    Exit;

  CurrentPage.Texts.Exchange(lbxTexts.ItemIndex, lbxTexts.ItemIndex + 1);
  lbxTexts.Items.Exchange(lbxTexts.ItemIndex, lbxTexts.ItemIndex + 1);

  lbxTexts.ItemIndex := lbxTexts.ItemIndex + 1;
end;

procedure TfmCTKEditorTextList.bAddTextClick(Sender : TObject);
var
  aText : cComictecaText;
  aPos : integer;
begin
  if not assigned(CurrentPage) then
    Exit;

  aPos := lbxTexts.ItemIndex + 1;

  aText := cComictecaText.Create(nil);
  aText.Page := CurrentPage;


  if (aPos < 1) or (aPos >= lbxTexts.Count) then
  begin
    aPos := CurrentPage.Texts.Add(aText);
    lbxTexts.AddItem(IntToStr(aPos), aText);
  end
  else
  begin
    CurrentPage.Texts.Insert(aPos, aText);
    lbxTexts.Items.Insert(aPos, IntToStr(aPos));
    lbxTexts.Items.Objects[aPos] := aText;
  end;

  lbxTexts.ItemIndex := aPos;
end;

procedure TfmCTKEditorTextList.bAddTextCopyClick(Sender : TObject);
var
  aText : cComictecaText;
  aPos : integer;
begin
  if not assigned(CurrentPage) then
    Exit;

  if lbxTexts.ItemIndex < 0 then
  begin
    bAddText.Click;
    Exit;
  end;

  aText := cComictecaText.Create(nil);
  aText.CopyFrom(cComictecaText(lbxTexts.Items.Objects[lbxTexts.ItemIndex]));

  aPos := lbxTexts.ItemIndex + 1;

  if (aPos < 1) or (aPos >= lbxTexts.Count) then
  begin
    aPos := CurrentPage.Texts.Add(aText);
    lbxTexts.AddItem(aText.ImgRect.ToString(','), aText);
  end
  else
  begin
    CurrentPage.Texts.Insert(aPos, aText);
    lbxTexts.Items.Insert(aPos, aText.ImgRect.ToString(','));
    lbxTexts.Items.Objects[aPos] := aText;
  end;

  lbxTexts.ItemIndex := aPos;
end;

procedure TfmCTKEditorTextList.bRemoveTextClick(Sender : TObject);
var
  aPos : integer;
begin
  if not assigned(CurrentPage) then
    Exit;

  if lbxTexts.ItemIndex < 0 then
    Exit;

  aPos := lbxTexts.ItemIndex;

  CurrentPage.Texts.Delete(aPos);
  lbxTexts.Items.Delete(aPos);

  if lbxTexts.Count = 0 then
    OnTextSelect(nil)
  else
  begin
    if aPos >= lbxTexts.Count then
      lbxTexts.ItemIndex := lbxTexts.Count - 1
    else
      lbxTexts.ItemIndex := aPos;
  end;
end;

procedure TfmCTKEditorTextList.bMoveUpClick(Sender : TObject);
begin
  if not assigned(CurrentPage) then
    Exit;

  if lbxTexts.ItemIndex < 1 then
    Exit;

  CurrentPage.Texts.Exchange(lbxTexts.ItemIndex, lbxTexts.ItemIndex - 1);
  lbxTexts.Items.Exchange(lbxTexts.ItemIndex, lbxTexts.ItemIndex - 1);

  lbxTexts.ItemIndex := lbxTexts.ItemIndex - 1;
end;

procedure TfmCTKEditorTextList.cbxPageChange(Sender : TObject);
begin
  if Assigned(OnTextSelect) then
    OnTextSelect(nil);

  if (cbxPage.Items.Count <= 0) or (cbxPage.ItemIndex < 0) then
    CurrentPage := nil
  else
    CurrentPage := cComictecaPage(cbxPage.Items.Objects[cbxPage.ItemIndex]);

  if Assigned(OnPageSelect) then
    OnPageSelect(CurrentPage);
end;

procedure TfmCTKEditorTextList.lbxTextsSelectionChange(Sender : TObject;
  User : boolean);
var
  aText : cComictecaText;
begin
  if lbxTexts.ItemIndex < 0 then
    aText := nil
  else
    aText := cComictecaText(lbxTexts.Items.Objects[lbxTexts.ItemIndex]);

  if Assigned(OnTextSelect) then
    OnTextSelect(aText);
end;

procedure TfmCTKEditorTextList.SetOnTextSelect(AValue : TCTKTextObjProc);
begin
  if FOnTextSelect = AValue then
    Exit;
  FOnTextSelect := AValue;
end;

procedure TfmCTKEditorTextList.SetCurrentPage(AValue : cComictecaPage);
begin
  if FCurrentPage = AValue then
    Exit;
  FCurrentPage := AValue;

  LoadTextList;
end;

procedure TfmCTKEditorTextList.SetOnPageSelect(AValue : TCTKPageObjProc);
begin
  if FOnPageSelect = AValue then Exit;
  FOnPageSelect := AValue;
end;

procedure TfmCTKEditorTextList.LoadFrameData;
var
  i : integer;
begin
  inherited;

  Enabled := Assigned(Comic);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  i := 0;
  while (i < Comic.Pages.Count) do
  begin
    cbxPage.AddItem(Comic.Pages[i].FileName, Comic.Pages[i]);
    Inc(i);
  end;

  if cbxPage.Items.Count > 0 then
  begin
    cbxPage.ItemIndex := 0;
    cbxPageChange(cbxPage);
  end;
end;

procedure TfmCTKEditorTextList.ClearFrameData;
begin
  inherited;

  cbxPage.Clear;
  lbxTexts.Clear;
end;

procedure TfmCTKEditorTextList.LoadTextList;
var
  i : integer;
begin
  lbxTexts.Clear;

  gbxTexts.Enabled := Assigned(CurrentPage);

  if not gbxTexts.Enabled then
    Exit;

  i := 0;
  while (i < CurrentPage.Texts.Count) do
  begin
    if CurrentPage.Texts[i].ImgRect.IsEmpty then
      lbxTexts.AddItem(IntToStr(i), CurrentPage.Texts[i])
    else
      lbxTexts.AddItem(CurrentPage.Texts[i].ImgRect.ToString(','),
        CurrentPage.Texts[i]);

    Inc(i);
  end;
end;

constructor TfmCTKEditorTextList.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmCTKEditorTextList.Destroy;
begin
  inherited Destroy;
end;

end.
{
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
