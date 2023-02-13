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
  uaComictecaShapedImage, uaComictecaPage, uaComictecaFrame,
  // Comicteca Core abstract frames
  uafCTKEditorFrameFrame;

type

  { TfmCTKEditorFrameEdit }

  TfmCTKEditorFrameEdit = class(TafmCTKEditorFrameFrame)
    bResetFrame: TButton;
    cbxFramePage: TComboBox;
    cbxFrameType: TComboBox;
    cbxFrameShape: TComboBox;
    eLeft: TSpinEdit;
    eRight: TSpinEdit;
    eTop: TSpinEdit;
    eBottom: TSpinEdit;
    eValX: TSpinEdit;
    eValY: TSpinEdit;
    gbxFrameEdit: TGroupBox;
    lFrameType: TLabel;
    lPage: TLabel;
    lLeft: TLabel;
    lRight: TLabel;
    lTop: TLabel;
    lBottom: TLabel;
    lValX: TLabel;
    lValY: TLabel;
    pEmpty1: TPanel;
    pEmpty2: TPanel;
    pEmpty3: TPanel;
    pEmpty4: TPanel;
    pValX: TPanel;
    pValY: TPanel;
    procedure bResetFrameClick(Sender: TObject);
    procedure cbxFramePageChange(Sender: TObject);
    procedure cbxFrameShapeChange(Sender: TObject);
    procedure cbxFrameTypeChange(Sender: TObject);
    procedure eBottomChange(Sender: TObject);
    procedure eLeftChange(Sender: TObject);
    procedure eRightChange(Sender: TObject);
    procedure eTopChange(Sender: TObject);
    procedure eValXChange(Sender: TObject);
    procedure eValYChange(Sender: TObject);
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
  if not Assigned(CTKFrame) then
    Exit;

  CTKFrame.ImgRect.Top := eTop.Value;
  CTKFrame.ImgRect.NormalizeRect;

  // Changing Rect don't notify observers
  CTKFrame.FPONotifyObservers(CTKFrame, ooChange, nil);
end;

procedure TfmCTKEditorFrameEdit.eValXChange(Sender: TObject);
begin
  if not Assigned(CTKFrame) then
    Exit;

  CTKFrame.ImgPoint.X := eValX.Value;

  // Changing Rect don't notify observers
  CTKFrame.FPONotifyObservers(CTKFrame, ooChange, nil);
end;

procedure TfmCTKEditorFrameEdit.eValYChange(Sender: TObject);
begin
    if not Assigned(CTKFrame) then
    Exit;

  CTKFrame.ImgPoint.Y := eValY.Value;

  // Changing Rect don't notify observers
  CTKFrame.FPONotifyObservers(CTKFrame, ooChange, nil);
end;

procedure TfmCTKEditorFrameEdit.eBottomChange(Sender: TObject);
begin
  if not Assigned(CTKFrame) then
    Exit;

  CTKFrame.ImgRect.Bottom := eBottom.Value;
  CTKFrame.ImgRect.NormalizeRect;

  // Changing Rect don't notify observers
  CTKFrame.FPONotifyObservers(CTKFrame, ooChange, nil);
end;

procedure TfmCTKEditorFrameEdit.eLeftChange(Sender: TObject);
begin
  if not Assigned(CTKFrame) then
    Exit;

  CTKFrame.ImgRect.Left := eLeft.Value;
  CTKFrame.ImgRect.NormalizeRect;

  // Changing Rect don't notify observers
  CTKFrame.FPONotifyObservers(CTKFrame, ooChange, nil);
end;

procedure TfmCTKEditorFrameEdit.eRightChange(Sender: TObject);
begin
  if not Assigned(CTKFrame) then
    Exit;

  CTKFrame.ImgRect.Right := eRight.Value;
  CTKFrame.ImgRect.NormalizeRect;

  // Changing Rect don't notify observers
  CTKFrame.FPONotifyObservers(CTKFrame, ooChange, nil);
end;

procedure TfmCTKEditorFrameEdit.cbxFramePageChange(Sender: TObject);
begin
  if not Assigned(CTKFrame) then
    Exit;

  CTKFrame.Page := caComictecaPage(
    cbxFramePage.Items.Objects[cbxFramePage.ItemIndex]);
end;

procedure TfmCTKEditorFrameEdit.cbxFrameShapeChange(Sender: TObject);
begin
  if not Assigned(CTKFrame) then
    Exit;

  CTKFrame.ImgShape := tCTKImageShape(cbxFrameShape.ItemIndex);
end;

procedure TfmCTKEditorFrameEdit.bResetFrameClick(Sender: TObject);
begin
  if not Assigned(CTKFrame) then
    Exit;

  CTKFrame.ImgRect := Default(TRect);
  CTKFrame.ImgPoint := Default(TPoint);

  // Changing Rect don't notify observers
  CTKFrame.FPONotifyObservers(CTKFrame, ooChange, nil);
end;

procedure TfmCTKEditorFrameEdit.cbxFrameTypeChange(Sender: TObject);
begin
  if not Assigned(CTKFrame) then
    Exit;

  CTKFrame.FrameType := tCTKFrameType(cbxFrameType.ItemIndex);
end;

procedure TfmCTKEditorFrameEdit.DoLoadFrameFrame;
var
  i: integer;
begin
  Enabled := Assigned(CTKFrame) and assigned(Comic);

  if not Assigned(CTKFrame) then
  begin
    DoClearFrameFrame;
    Exit;
  end;

  eLeft.Value := CTKFrame.ImgRect.Left;
  eTop.Value := CTKFrame.ImgRect.Top;
  eBottom.Value := CTKFrame.ImgRect.Bottom;
  eRight.Value := CTKFrame.ImgRect.Right;

  eValX.Value := CTKFrame.ImgPoint.X;
  eValY.Value := CTKFrame.ImgPoint.Y;

  if not Assigned(CTKFrame.Page) then
    cbxFramePageChange(cbxFramePage)
  else
  begin
    i := cbxFramePage.Items.IndexOfObject(CTKFrame.Page);
    if i >= 0 then
      cbxFramePage.ItemIndex := i
    else
      cbxFramePageChange(cbxFramePage);
  end;

  cbxFrameType.ItemIndex := Ord(CTKFrame.FrameType);
  cbxFrameShape.ItemIndex := Ord(CTKFrame.ImgShape);
end;

procedure TfmCTKEditorFrameEdit.DoClearFrameFrame;
begin
  eLeft.Value := 0;
  eTop.Value := 0;
  eBottom.Value := 0;
  eRight.Value := 0;

  eValX.Value := 0;
  eValY.Value := 0;

  cbxFrameType.ItemIndex := Ord(kCTKFrameDefType);
  cbxFrameShape.ItemIndex := Ord(kCTKImgDefShape);
end;

procedure TfmCTKEditorFrameEdit.DoLoadFrameData;
var
  i: integer;
begin
  // Called only when Comic is set
  Enabled := Assigned(CTKFrame) and assigned(Comic);

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
  cbxFrameShape.Items.AddStrings(ComictecaFrameShapeStr, True);
end;

destructor TfmCTKEditorFrameEdit.Destroy;
begin
  inherited Destroy;
end;

end.
