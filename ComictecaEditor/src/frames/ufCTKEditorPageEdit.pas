unit ufCTKEditorPageEdit;

{< TfmCTKEditorPageEdit frame unit.

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
  ExtCtrls,
  // CHX units
  uCHXRecordHelpers,
  // CHX frame units
  ufCHXBGRAImgViewerEx,
  // Comicteca Core units
  uCTKConst, uCTKRstStr,
  // Comicteca Core abstract classes
  uaComictecaPage,
  // Comicteca Core classes
  ucComictecaPage,
  // Comicteca Editor abstract frames
  uafCTKEditorPageFrame;

type

  { TfmCTKEditorPageEdit }

  TfmCTKEditorPageEdit = class(TafmCTKEditorPageFrame)
    bResetGeometry: TButton;
    bSetTL: TButton;
    bSetBL: TButton;
    bSetBR: TButton;
    bSetTR: TButton;
    cgrPageContent: TCheckGroup;
    chkCropToGeometry: TCheckBox;
    chkLinearGeometry: TCheckBox;
    eFilename: TEdit;
    eMultipage: TSpinEdit;
    eSHA1: TEdit;
    eTopLeftX: TSpinEdit;
    eTopRightX: TSpinEdit;
    eBottomLeftX: TSpinEdit;
    eBottomRightX: TSpinEdit;
    eTopLeftY: TSpinEdit;
    eTopRightY: TSpinEdit;
    eBottomLeftY: TSpinEdit;
    eBottomRightY: TSpinEdit;
    gbxFixGeometry: TGroupBox;
    gbxPage: TGroupBox;
    lCurrentPoint: TLabel;
    lFilename: TLabel;
    lMultipage: TLabel;
    lSHA1: TLabel;
    lTopLeft: TLabel;
    lTopRight: TLabel;
    lBottomLeft: TLabel;
    lBottomRight: TLabel;
    pGeomQuadPoints: TPanel;
    pValues: TPanel;
    procedure bResetGeometryClick(Sender: TObject);
    procedure bSetBLClick(Sender: TObject);
    procedure bSetBRClick(Sender: TObject);
    procedure bSetTLClick(Sender: TObject);
    procedure bSetTRClick(Sender: TObject);
    procedure cgrPageContentClick(Sender: TObject);
    procedure cgrPageContentItemClick(Sender: TObject; Index: integer);
    procedure chkCropToGeometryChange(Sender: TObject);
    procedure chkLinearGeometryChange(Sender: TObject);
    procedure eBottomLeftXChange(Sender: TObject);
    procedure eBottomLeftYChange(Sender: TObject);
    procedure eBottomRightXChange(Sender: TObject);
    procedure eBottomRightYChange(Sender: TObject);
    procedure eMultipageChange(Sender: TObject);
    procedure eTopLeftXChange(Sender: TObject);
    procedure eTopLeftYChange(Sender: TObject);
    procedure eTopRightXChange(Sender: TObject);
    procedure eTopRightYChange(Sender: TObject);

  private
    FVisor: TfmCHXBGRAImgViewerEx;
    procedure SetVisor(AValue: TfmCHXBGRAImgViewerEx);

  protected
    procedure DoLoadFrameData;
    procedure DoClearFrameData;

    procedure DoImgMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);

  public
    CurrentPoint: TPoint;

    property Visor: TfmCHXBGRAImgViewerEx read FVisor write SetVisor;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{$R *.lfm}

{ TfmCTKEditorPageEdit }

procedure TfmCTKEditorPageEdit.cgrPageContentClick(Sender: TObject);
var
  aProps: tCTKPageContents;
  i: tCTKFrameType;
begin
  if not assigned(Page) then
    Exit;

  aProps := [];
  for i := Low(tCTKPageContents) to High(tCTKPageContents) do
  begin
    if cgrPageContent.Checked[Ord(i)] then
      Include(aProps, i);
  end;

  Page.PageContent := aProps;
end;

procedure TfmCTKEditorPageEdit.bSetTLClick(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  eTopLeftX.Value := CurrentPoint.X;
  eTopLeftY.Value := CurrentPoint.Y;
  Page.GeomTL := CurrentPoint;
end;

procedure TfmCTKEditorPageEdit.bSetBLClick(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  eBottomLeftX.Value := CurrentPoint.X;
  eBottomLeftY.Value := CurrentPoint.Y;
  Page.GeomBL := CurrentPoint;
end;

procedure TfmCTKEditorPageEdit.bResetGeometryClick(Sender: TObject);
begin
  // SpinButtons
  eTopLeftX.Value := 0;
  eTopLeftY.Value := 0;
  eTopRightX.Value := 0;
  eTopRightY.Value := 0;
  eBottomLeftX.Value := 0;
  eBottomLeftY.Value := 0;
  eBottomRightX.Value := 0;
  eBottomRightY.Value := 0;

  // Page properties
  Page.GeomTL := TPoint.Zero;
  Page.GeomTR := TPoint.Zero;
  Page.GeomBL := TPoint.Zero;
  Page.GeomBR := TPoint.Zero;
end;

procedure TfmCTKEditorPageEdit.bSetBRClick(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  eBottomRightX.Value := CurrentPoint.X;
  eBottomRightY.Value := CurrentPoint.Y;
  Page.GeomBR := CurrentPoint;
end;

procedure TfmCTKEditorPageEdit.bSetTRClick(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  eTopRightX.Value := CurrentPoint.X;
  eTopRightY.Value := CurrentPoint.Y;
  Page.GeomTR := CurrentPoint;
end;

procedure TfmCTKEditorPageEdit.cgrPageContentItemClick(Sender: TObject;
  Index: integer);
var
  aProps: tCTKPageContents;
begin
  if not assigned(Page) then
    Exit;

  aProps := Page.PageContent;

  if cgrPageContent.Checked[Index] then
    Include(aProps, tCTKFrameType(Index))
  else
    Exclude(aProps, tCTKFrameType(Index));

  Page.PageContent := aProps;
end;

procedure TfmCTKEditorPageEdit.chkCropToGeometryChange(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  Page.CropGeometry := chkCropToGeometry.Checked;
end;

procedure TfmCTKEditorPageEdit.chkLinearGeometryChange(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  Page.LinearGeometry := chkLinearGeometry.Checked;
end;

procedure TfmCTKEditorPageEdit.eBottomLeftXChange(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  Page.GeomBL.X := eBottomLeftX.Value;
end;

procedure TfmCTKEditorPageEdit.eBottomLeftYChange(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  Page.GeomBL.Y := eBottomLeftY.Value;
end;

procedure TfmCTKEditorPageEdit.eBottomRightXChange(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  Page.GeomBR.X := eBottomRightX.Value;
end;

procedure TfmCTKEditorPageEdit.eBottomRightYChange(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  Page.GeomBR.Y := eBottomRightY.Value;
end;

procedure TfmCTKEditorPageEdit.eMultipageChange(Sender: TObject);
begin
  if not assigned(Page) then
    Exit;

  Page.MultiplePages := eMultipage.Value;
end;

procedure TfmCTKEditorPageEdit.eTopLeftXChange(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  Page.GeomTL.X := eTopLeftX.Value;
end;

procedure TfmCTKEditorPageEdit.eTopLeftYChange(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  Page.GeomTL.Y := eTopLeftY.Value;
end;

procedure TfmCTKEditorPageEdit.eTopRightXChange(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  Page.GeomTR.X := eTopRightX.Value;
end;

procedure TfmCTKEditorPageEdit.eTopRightYChange(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  Page.GeomTR.Y := eTopRightY.Value;
end;

procedure TfmCTKEditorPageEdit.SetVisor(AValue: TfmCHXBGRAImgViewerEx);
begin
  if FVisor = AValue then
    Exit;

  if Assigned(FVisor) then
  begin
    FVisor.MouseActionMode := maiNone;
    FVisor.OnImgMouseUp := nil;
  end;

  FVisor := AValue;

  if Assigned(FVisor) then
  begin
    FVisor.MouseActionMode := maiMouseClick;
    FVisor.OnImgMouseUp := @DoImgMouseUp;
  end;
end;

procedure TfmCTKEditorPageEdit.DoLoadFrameData;
var
  i: tCTKFrameType;
begin
  Enabled := assigned(Page) and assigned(Comic);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  eFilename.Text := Page.FileName;

  eSHA1.Text := Page.SHA1;

  if Page.MultiplePages < 1 then
    Page.MultiplePages := 1;

  eMultipage.Value := Page.MultiplePages;

  eTopLeftX.Value := Page.GeomTL.X;
  eTopLeftY.Value := Page.GeomTL.Y;
  eTopRightX.Value := Page.GeomTR.X;
  eTopRightY.Value := Page.GeomTR.Y;
  eBottomLeftX.Value := Page.GeomBL.X;
  eBottomLeftY.Value := Page.GeomBL.Y;
  eBottomRightX.Value := Page.GeomBR.X;
  eBottomRightY.Value := Page.GeomBR.Y;

  chkCropToGeometry.Checked := Page.CropGeometry;

  // Load property checkboxes
  for i := Low(tCTKPageContents) to High(tCTKPageContents) do
    cgrPageContent.Checked[Ord(i)] := i in Page.PageContent;
end;

procedure TfmCTKEditorPageEdit.DoClearFrameData;
var
  i: integer;
begin
  eFilename.Clear;

  eSHA1.Clear;

  eMultipage.Value := 1;

  eTopLeftX.Value := 0;
  eTopLeftY.Value := 0;
  eTopRightX.Value := 0;
  eTopRightY.Value := 0;
  eBottomLeftX.Value := 0;
  eBottomLeftY.Value := 0;
  eBottomRightX.Value := 0;
  eBottomRightY.Value := 0;

  chkCropToGeometry.Checked := False;

  i := 0;
  while i < cgrPageContent.Items.Count do
  begin
    cgrPageContent.Checked[i] := False;
    Inc(i);
  end;
end;

procedure TfmCTKEditorPageEdit.DoImgMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  CurrentPoint.X := X;
  CurrentPoint.Y := Y;

  lCurrentPoint.Caption := 'Point: ' + CurrentPoint.ToString(':');
end;

constructor TfmCTKEditorPageEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  cgrPageContent.Items.AddStrings(ComictecaFrameTypeStr);

  OnLoadFrameData := @DoLoadFrameData;
  OnClearFrameData := @DoClearFrameData;
end;

destructor TfmCTKEditorPageEdit.Destroy;
begin
  inherited Destroy;
end;

end.
