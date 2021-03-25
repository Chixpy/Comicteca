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
  uaComictecaText,
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

    procedure DoLoadFrameData;
    procedure DoClearFrameData;

  public
    property DataFolder: string read FDataFolder write SetDataFolder;

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

  CTKText.TextRect.Top := eTop.Value;
  CTKText.TextRect.NormalizeRect;

  // Changing Rect don't notify observers
  CTKText.FPONotifyObservers(CTKText, ooChange, nil);
end;

procedure TfmCTKEditorTextEdit.eValXChange(Sender: TObject);
begin
  if not Assigned(CTKText) then
    Exit;

  CTKText.TextPoint.X := eValX.Value;

  // Changing Point don't notify observers
  CTKText.FPONotifyObservers(CTKText, ooChange, nil);
end;

procedure TfmCTKEditorTextEdit.eValYChange(Sender: TObject);
begin
  if not Assigned(CTKText) then
    Exit;

  CTKText.TextPoint.Y := eValY.Value;

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

  CTKText.TextRect.Right := eRight.Value;
  CTKText.TextRect.NormalizeRect;

  // Changing Rect don't notify observers
  CTKText.FPONotifyObservers(CTKText, ooChange, nil);
end;

procedure TfmCTKEditorTextEdit.eLeftChange(Sender: TObject);
begin
  if not Assigned(CTKText) then
    Exit;

  CTKText.TextRect.Left := eLeft.Value;
  CTKText.TextRect.NormalizeRect;

  // Changing Rect don't notify observers
  CTKText.FPONotifyObservers(CTKText, ooChange, nil);
end;

procedure TfmCTKEditorTextEdit.eBottomChange(Sender: TObject);
begin
  if not Assigned(CTKText) then
    Exit;

  CTKText.TextRect.Bottom := eBottom.Value;
  CTKText.TextRect.NormalizeRect;

  // Changing Rect don't notify observers
  CTKText.FPONotifyObservers(CTKText, ooChange, nil);
end;

procedure TfmCTKEditorTextEdit.bResetFrameClick(Sender: TObject);
begin
  if not Assigned(CTKText) then
    Exit;

  CTKText.TextRect := Default(TRect);
  CTKText.TextPoint := Default(TPoint);

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

  CTKText.TextShape := tCTKFrameShape(cbxTextShape.ItemIndex);
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
  DoLoadFrameData;
end;

procedure TfmCTKEditorTextEdit.DoClearTextFrame;
begin
  DoClearFrameData;
end;

procedure TfmCTKEditorTextEdit.DoLoadFrameData;
var
  StrLst: TStringList;
begin
  Enabled := Assigned(CTKText) and assigned(Comic);

  if not Assigned(CTKText) then
  begin
    DoClearFrameData;
    Exit;
  end;

  eLeft.Value := CTKText.TextRect.Left;
  eTop.Value := CTKText.TextRect.Top;
  eBottom.Value := CTKText.TextRect.Bottom;
  eRight.Value := CTKText.TextRect.Right;

  eValX.Value := CTKText.TextPoint.X;
  eValY.Value := CTKText.TextPoint.Y;

  cbxTextShape.ItemIndex := Ord(CTKText.TextShape);

  // Nil can't be assigned to TStringList...
  StrLst := CTKText.Content.CTMGetSL(cbxLangContent.Text);
  if assigned(StrLst) then
    mContent.Lines.Assign(StrLst)
  else
    mContent.Clear;
end;

procedure TfmCTKEditorTextEdit.DoClearFrameData;
begin
  eLeft.Value := 0;
  eTop.Value := 0;
  eBottom.Value := 0;
  eRight.Value := 0;

  eValX.Value := 0;
  eValY.Value := 0;

  cbxTextShape.ItemIndex := Ord(kCTKTextDefShape);

  mContent.Clear;
end;

constructor TfmCTKEditorTextEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnLoadFrameData := @DoLoadFrameData;
  OnClearFrameData := @DoClearFrameData;

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

