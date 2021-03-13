unit ufCTKEditorFrameEdit;

{< TfmCTKEditorFrameEdit frame unit.

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ComCtrls, ExtCtrls, SpinEx,
  // Comicteca Core units
  uCTKConst, uCTKRstStr,
  // Comicteca Core abstract clases
  uaComictecaPage, uaComictecaFrame,
  // Comicteca Core abstract frames
  uafCTKEditorFrameFrame;

type

  { TfmCTKEditorFrameEdit }

  TfmCTKEditorFrameEdit = class(TafmCTKEditorFrameFrame)
    bResetFrame: TButton;
    cbxFramePage: TComboBox;
    cbxFrameType: TComboBox;
    chkEllipseFrame: TCheckBox;
    eLeft: TSpinEdit;
    eRight: TSpinEdit;
    eTop: TSpinEdit;
    eBottom: TSpinEdit;
    gbxFrameEdit: TGroupBox;
    lFrameType: TLabel;
    lPage: TLabel;
    lLeft: TLabel;
    lRight: TLabel;
    lTop: TLabel;
    lBottom: TLabel;
    pEmpty1: TPanel;
    pEmpty2: TPanel;
    pEmpty3: TPanel;
    pEmpty4: TPanel;
    procedure bResetFrameClick(Sender: TObject);
    procedure cbxFramePageChange(Sender: TObject);
    procedure cbxFrameTypeChange(Sender: TObject);
    procedure cbxFrameTypeEditingDone(Sender: TObject);
    procedure eBottomChange(Sender: TObject);
    procedure eLeftChange(Sender: TObject);
    procedure eRightChange(Sender: TObject);
    procedure eTopChange(Sender: TObject);
  private


  protected
    procedure DoLoadFrameFrame; override;
    procedure DoClearFrameFrame; override;

    procedure DoLoadFrameData;
    procedure DoClearFrameData;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  published

  end;

implementation

{$R *.lfm}

{ TfmCTKEditorFrameEdit }

procedure TfmCTKEditorFrameEdit.eTopChange(Sender: TObject);
begin
   if not Assigned(Frame) then
    Exit;

  Frame.Rect.Top := eTop.Value;
  Frame.Rect.NormalizeRect;

  // Changing Rect don't notify observers
  Frame.FPONotifyObservers(Frame, ooChange, nil);
end;

procedure TfmCTKEditorFrameEdit.cbxFrameTypeEditingDone(Sender: TObject);
begin
  if not Assigned(Frame) then
    Exit;

  Frame.FrameType := tCTKFrameType(cbxFrameType.ItemIndex);
end;

procedure TfmCTKEditorFrameEdit.eBottomChange(Sender: TObject);
begin
    if not Assigned(Frame) then
    Exit;

  Frame.Rect.Bottom := eBottom.Value;
  Frame.Rect.NormalizeRect;

  // Changing Rect don't notify observers
  Frame.FPONotifyObservers(Frame, ooChange, nil);
end;

procedure TfmCTKEditorFrameEdit.eLeftChange(Sender: TObject);
begin
  if not Assigned(Frame) then
    Exit;

  Frame.Rect.Left := eLeft.Value;
  Frame.Rect.NormalizeRect;

  // Changing Rect don't notify observers
  Frame.FPONotifyObservers(Frame, ooChange, nil);
end;

procedure TfmCTKEditorFrameEdit.eRightChange(Sender: TObject);
begin
    if not Assigned(Frame) then
    Exit;

  Frame.Rect.Right := eRight.Value;
  Frame.Rect.NormalizeRect;

  // Changing Rect don't notify observers
  Frame.FPONotifyObservers(Frame, ooChange, nil);
end;

procedure TfmCTKEditorFrameEdit.cbxFramePageChange(Sender: TObject);
begin
  if not Assigned(Frame) then
    Exit;

  Frame.Page := caComictecaPage(
    cbxFramePage.Items.Objects[cbxFramePage.ItemIndex]);
end;

procedure TfmCTKEditorFrameEdit.bResetFrameClick(Sender: TObject);
begin
  if not Assigned(Frame) then
    Exit;

  Frame.Rect := Default(TRect);

  // Changing Rect don't notify observers
  Frame.FPONotifyObservers(Frame, ooChange, nil);
end;

procedure TfmCTKEditorFrameEdit.cbxFrameTypeChange(Sender: TObject);
begin
  if not Assigned(Frame) then
    Exit;

  Frame.FrameType := tCTKFrameType(cbxFrameType.ItemIndex);
end;

procedure TfmCTKEditorFrameEdit.DoLoadFrameFrame;
var
  i: integer;
begin
  Enabled := Assigned(Frame) and assigned(Comic);

  if not Assigned(Frame) then
  begin
    DoClearFrameFrame;
    Exit;
  end;

  eLeft.Value := Frame.Rect.Left;
  eTop.Value := Frame.Rect.Top;
  eBottom.Value := Frame.Rect.Bottom;
  eRight.Value := Frame.Rect.Right;

  if not Assigned(Frame.Page) then
    cbxFramePageChange(cbxFramePage)
  else
  begin
    i := cbxFramePage.Items.IndexOfObject(Frame.Page);
    if i >= 0 then
      cbxFramePage.ItemIndex := i
    else
      cbxFramePageChange(cbxFramePage);
  end;

  cbxFrameType.ItemIndex := Ord(Frame.FrameType);
end;

procedure TfmCTKEditorFrameEdit.DoClearFrameFrame;
begin
  eLeft.Value := 0;
  eTop.Value := 0;
  eBottom.Value := 0;
  eRight.Value := 0;

  cbxFrameType.ItemIndex := Ord(CTKFTVignette);
end;

procedure TfmCTKEditorFrameEdit.DoLoadFrameData;
var
  i: integer;
begin
  // Called only when Comic is set
  Enabled := Assigned(Frame) and assigned(Comic);

  if (not assigned(Comic)) then
  begin
    ClearFrameData;
    Exit;
  end;

  cbxFramePage.Clear;
  i := 0;
  while i < Comic.Pages.Count do
  begin
    cbxFramePage.AddItem(Comic.Pages[i].FileName, Comic.Pages[i]);
    Inc(i);
  end;

  if cbxFramePage.Items.Count > 0 then
    cbxFramePage.ItemIndex := 0;
end;

procedure TfmCTKEditorFrameEdit.DoClearFrameData;
begin
  cbxFramePage.ItemIndex := -1;
  cbxFramePage.Clear;
end;

constructor TfmCTKEditorFrameEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnLoadFrameData := @DoLoadFrameData;
  OnClearFrameData := @DoClearFrameData;

  cbxFrameType.Items.AddStrings(ComictecaFrameTypeStr, True);
end;

destructor TfmCTKEditorFrameEdit.Destroy;
begin
  inherited Destroy;
end;

end.
