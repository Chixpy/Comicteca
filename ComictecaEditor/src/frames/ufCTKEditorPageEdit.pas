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
    gbxPage: TGroupBox;
    lFilename: TLabel;
    lMultipage: TLabel;
    lSHA1: TLabel;
    pValues: TPanel;
    procedure cgrPageContentClick(Sender: TObject);
    procedure cgrPageContentItemClick(Sender: TObject; Index: integer);
    procedure eMultipageEditingDone(Sender: TObject);

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

procedure TfmCTKEditorPageEdit.cgrPageContentClick(Sender: TObject);
var
  aProps: tCTKPageContents;
  i: tCTKPageContent;
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
    Include(aProps, tCTKPageContent(Index))
  else
    Exclude(aProps, tCTKPageContent(Index));

  Page.PageContent := aProps;
end;

procedure TfmCTKEditorPageEdit.DoLoadFrameData;
var
  i: tCTKPageContent;
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
  i: Integer;
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

  cgrPageContent.Items.AddStrings(ComictecaPageContentStr);

  OnLoadFrameData := @DoLoadFrameData;
  OnClearFrameData := @DoClearFrameData;
end;

destructor TfmCTKEditorPageEdit.Destroy;
begin
  inherited Destroy;
end;

end.
