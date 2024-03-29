unit ufCTKEditorTextEdit;

{< TfmCTKEditorTextEdit frame unit.

  This file is part of Comicteca Editor.

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ExtCtrls, LazFileUtils,
  // CHX units.
  uCHXStrUtils,
  // Comicteca Core units.
  uCTKConst, uCTKRstStr,
  // Comicteca Editor units.
  uCTKEditorConst,
    // Comicteca Core abstract clases
  uaComictecaShapedImage, uaComictecaText,
  // Comicteca Editor abstract frames.
  uafCTKEditorTextFrame;

type

  { TfmCTKEditorTextEdit }

  TfmCTKEditorTextEdit = class(TafmCTKEditorTextFrame)
    bResetFrame: TButton;
    cbxTextShape: TComboBox;
    cbxLangContent: TComboBox;
    eBottom: TSpinEdit;
    eLeft: TSpinEdit;
    eRight: TSpinEdit;
    eTop: TSpinEdit;
    eValX: TSpinEdit;
    eValY: TSpinEdit;
    gbxFrameEdit: TGroupBox;
    lBottom: TLabel;
    lContent: TLabel;
    lLeft: TLabel;
    lRight: TLabel;
    lTop: TLabel;
    lValX: TLabel;
    lValY: TLabel;
    mContent: TMemo;
    pContent: TPanel;
    pEmpty1: TPanel;
    pEmpty2: TPanel;
    pEmpty3: TPanel;
    pEmpty4: TPanel;
    pValX: TPanel;
    pValY: TPanel;
    procedure bResetFrameClick(Sender: TObject);
    procedure cbxLangContentEditingDone(Sender: TObject);
    procedure cbxTextShapeChange(Sender: TObject);
    procedure eBottomChange(Sender: TObject);
    procedure eLeftChange(Sender: TObject);
    procedure eRightChange(Sender: TObject);
    procedure eTopChange(Sender: TObject);
    procedure eValXChange(Sender: TObject);
    procedure eValYChange(Sender: TObject);
    procedure mContentEditingDone(Sender: TObject);

  private
    FDataFolder: string;
    procedure SetDataFolder(AValue: string);

  protected
    procedure DoLoadTextFrame; override;
    procedure DoClearTextFrame; override;

  public
    property DataFolder: string read FDataFolder write SetDataFolder;

    procedure LoadFrameData; override;
    procedure ClearFrameData; override;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCTKEditorTextEdit }

procedure TfmCTKEditorTextEdit.eTopChange(Sender: TObject);
begin
  if not Assigned(CTKText) then
    Exit;

  CTKText.ImgRect.Top := eTop.Value;
  CTKText.ImgRect.NormalizeRect;

  // Changing Rect don't notify observers
  CTKText.FPONotifyObservers(CTKText, ooChange, nil);
end;

procedure TfmCTKEditorTextEdit.eValXChange(Sender: TObject);
begin
  if not Assigned(CTKText) then
    Exit;

  CTKText.ImgPoint.X := eValX.Value;

  // Changing Point don't notify observers
  CTKText.FPONotifyObservers(CTKText, ooChange, nil);
end;

procedure TfmCTKEditorTextEdit.eValYChange(Sender: TObject);
begin
  if not Assigned(CTKText) then
    Exit;

  CTKText.ImgPoint.Y := eValY.Value;

  // Changing Point don't notify observers
  CTKText.FPONotifyObservers(CTKText, ooChange, nil);
end;

procedure TfmCTKEditorTextEdit.mContentEditingDone(Sender: TObject);
begin
  if not Assigned(CTKText) then
    Exit;

  CTKText.Content.CTMSetSL(cbxLangContent.Text, mContent.Lines);
end;

procedure TfmCTKEditorTextEdit.eRightChange(Sender: TObject);
begin
  if not Assigned(CTKText) then
    Exit;

  CTKText.ImgRect.Right := eRight.Value;
  CTKText.ImgRect.NormalizeRect;

  // Changing Rect don't notify observers
  CTKText.FPONotifyObservers(CTKText, ooChange, nil);
end;

procedure TfmCTKEditorTextEdit.eLeftChange(Sender: TObject);
begin
  if not Assigned(CTKText) then
    Exit;

  CTKText.ImgRect.Left := eLeft.Value;
  CTKText.ImgRect.NormalizeRect;

  // Changing Rect don't notify observers
  CTKText.FPONotifyObservers(CTKText, ooChange, nil);
end;

procedure TfmCTKEditorTextEdit.eBottomChange(Sender: TObject);
begin
  if not Assigned(CTKText) then
    Exit;

  CTKText.ImgRect.Bottom := eBottom.Value;
  CTKText.ImgRect.NormalizeRect;

  // Changing Rect don't notify observers
  CTKText.FPONotifyObservers(CTKText, ooChange, nil);
end;

procedure TfmCTKEditorTextEdit.bResetFrameClick(Sender: TObject);
begin
  if not Assigned(CTKText) then
    Exit;

  CTKText.ImgRect := Default(TRect);
  CTKText.ImgPoint := Default(TPoint);

  // Changing Rect don't notify observers
  CTKText.FPONotifyObservers(CTKText, ooChange, nil);
end;

procedure TfmCTKEditorTextEdit.cbxLangContentEditingDone(Sender: TObject);
begin
  if cbxLangContent.Items.IndexOf(cbxLangContent.Text) = -1 then
     cbxLangContent.Items.Add(cbxLangContent.Text);

  LoadFrameData;
end;

procedure TfmCTKEditorTextEdit.cbxTextShapeChange(Sender: TObject);
begin
    if not Assigned(CTKText) then
    Exit;

  CTKText.ImgShape := tCTKImageShape(cbxTextShape.ItemIndex);
end;

procedure TfmCTKEditorTextEdit.SetDataFolder(AValue: string);
begin
  FDataFolder := SetAsFolder(AValue);

  if not DirectoryExistsUTF8(DataFolder) then
    Exit;

  if FileExistsUTF8(DataFolder + kLanguageCBFile) then
    cbxLangContent.Items.LoadFromFile(DataFolder + kLanguageCBFile);
end;

procedure TfmCTKEditorTextEdit.DoLoadTextFrame;
begin
  LoadFrameData;
end;

procedure TfmCTKEditorTextEdit.DoClearTextFrame;
begin
  ClearFrameData;
end;

procedure TfmCTKEditorTextEdit.LoadFrameData;
var
  StrLst: TStringList;
begin
  inherited;

  Enabled := Assigned(CTKText) and assigned(Comic);

  if not Assigned(CTKText) then
  begin
    ClearFrameData;
    Exit;
  end;

  eLeft.Value := CTKText.ImgRect.Left;
  eTop.Value := CTKText.ImgRect.Top;
  eBottom.Value := CTKText.ImgRect.Bottom;
  eRight.Value := CTKText.ImgRect.Right;

  eValX.Value := CTKText.ImgPoint.X;
  eValY.Value := CTKText.ImgPoint.Y;

  cbxTextShape.ItemIndex := Ord(CTKText.ImgShape);

  // Nil can't be assigned to TStringList...
  StrLst := CTKText.Content.CTMGetSL(cbxLangContent.Text);
  if assigned(StrLst) then
    mContent.Lines.Assign(StrLst)
  else
    mContent.Clear;
end;

procedure TfmCTKEditorTextEdit.ClearFrameData;
begin
  inherited;

  eLeft.Value := 0;
  eTop.Value := 0;
  eBottom.Value := 0;
  eRight.Value := 0;

  eValX.Value := 0;
  eValY.Value := 0;

  cbxTextShape.ItemIndex := Ord(kCTKImgDefShape);

  mContent.Clear;
end;

constructor TfmCTKEditorTextEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  cbxTextShape.Items.AddStrings(ComictecaFrameShapeStr, True);
end;

destructor TfmCTKEditorTextEdit.Destroy;
begin
  if DirectoryExistsUTF8(DataFolder) then
  begin
    cbxLangContent.Items.SaveToFile(DataFolder + kLanguageCBFile);
  end;

  inherited Destroy;
end;

end.

