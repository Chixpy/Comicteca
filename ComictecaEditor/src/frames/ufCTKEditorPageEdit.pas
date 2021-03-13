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
    cgrPageContent: TCheckGroup;
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
    gbxFixPerspective: TGroupBox;
    gbxPage: TGroupBox;
    lFilename: TLabel;
    lMultipage: TLabel;
    lSHA1: TLabel;
    lTopLeft: TLabel;
    lTopRight: TLabel;
    lBottomLeft: TLabel;
    lBottomRight: TLabel;
    pValues: TPanel;
    procedure cgrPageContentClick(Sender: TObject);
    procedure cgrPageContentItemClick(Sender: TObject; Index: integer);
    procedure eBottomLeftXEditingDone(Sender: TObject);
    procedure eBottomLeftYEditingDone(Sender: TObject);
    procedure eBottomRightXEditingDone(Sender: TObject);
    procedure eBottomRightYEditingDone(Sender: TObject);
    procedure eMultipageChange(Sender: TObject);
    procedure eMultipageEditingDone(Sender: TObject);
    procedure eTopLeftXEditingDone(Sender: TObject);
    procedure eTopLeftYEditingDone(Sender: TObject);
    procedure eTopRightXEditingDone(Sender: TObject);
    procedure eTopRightYEditingDone(Sender: TObject);
  protected
    procedure DoLoadFrameData;
    procedure DoClearFrameData;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{$R *.lfm}

{ TfmCTKEditorPageEdit }

procedure TfmCTKEditorPageEdit.eMultipageEditingDone(Sender: TObject);
begin
  if not assigned(Page) then
    Exit;

  Page.MultiplePages := eMultipage.Value;
end;

procedure TfmCTKEditorPageEdit.eTopLeftXEditingDone(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  Page.PersTL.X := eTopLeftX.Value;

  // Changing Rect don't notify observers
  Page.FPONotifyObservers(Page, ooChange, nil);
end;

procedure TfmCTKEditorPageEdit.eTopLeftYEditingDone(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  Page.PersTL.Y := eTopLeftY.Value;

  // Changing Rect don't notify observers
  Page.FPONotifyObservers(Page, ooChange, nil);
end;

procedure TfmCTKEditorPageEdit.eTopRightXEditingDone(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  Page.PersTR.X := eTopRightX.Value;

  // Changing Rect don't notify observers
  Page.FPONotifyObservers(Page, ooChange, nil);
end;

procedure TfmCTKEditorPageEdit.eTopRightYEditingDone(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  Page.PersTR.Y := eTopRightY.Value;

  // Changing Rect don't notify observers
  Page.FPONotifyObservers(Page, ooChange, nil);
end;

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

procedure TfmCTKEditorPageEdit.eBottomLeftXEditingDone(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  Page.PersBL.X := eBottomLeftX.Value;

  // Changing Rect don't notify observers
  Page.FPONotifyObservers(Page, ooChange, nil);
end;

procedure TfmCTKEditorPageEdit.eBottomLeftYEditingDone(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  Page.PersBL.Y := eBottomLeftY.Value;

  // Changing Rect don't notify observers
  Page.FPONotifyObservers(Page, ooChange, nil);
end;

procedure TfmCTKEditorPageEdit.eBottomRightXEditingDone(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  Page.PersBR.X := eBottomRightX.Value;

  // Changing Rect don't notify observers
  Page.FPONotifyObservers(Page, ooChange, nil);
end;

procedure TfmCTKEditorPageEdit.eBottomRightYEditingDone(Sender: TObject);
begin
  if not Assigned(Page) then
    Exit;

  Page.PersBR.Y := eBottomRightY.Value;

  // Changing Rect don't notify observers
  Page.FPONotifyObservers(Page, ooChange, nil);
end;

procedure TfmCTKEditorPageEdit.eMultipageChange(Sender: TObject);
begin
  if not assigned(Page) then
    Exit;

  Page.MultiplePages := eMultipage.Value;
end;

procedure TfmCTKEditorPageEdit.DoLoadFrameData;
var
  i: tCTKFrameType;
begin
  ClearFrameData;

  Enabled := assigned(Page) and assigned(Comic);

  if not Enabled then
    Exit;

  eFilename.Text := Page.FileName;

  eSHA1.Text := Page.SHA1;

  if Page.MultiplePages < 1 then
    Page.MultiplePages := 1;

  eMultipage.Value := Page.MultiplePages;

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

  i := 0;
  while i < cgrPageContent.Items.Count do
  begin
    cgrPageContent.Checked[i] := False;
    Inc(i);
  end;
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
