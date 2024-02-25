unit ufCTKEditorFrameList;
{< TfmCTKEditorFrameList frame unit.

  This file is part of Comicteca GUI.

  Copyright (C) 2023-2024 Chixpy
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls,
  // CHX units
  uCHXRecords,
  // Comicteca Core classes;
  ucComictecaFrame,
  // Comicteca Editor abstract frames
  uafCTKEditorFrame;

type

  { TfmCTKEditorFrameList }

  TfmCTKEditorFrameList = class(TafmCTKEditorFrame)
    bAddFrame : TButton;
    bMoveDown : TButton;
    bRemoveFrame : TButton;
    bSubir : TButton;
    gbxFrameList : TGroupBox;
    lbxFrameList : TListBox;
    pFrameListButtons : TPanel;
    procedure bAddFrameClick(Sender : TObject);
    procedure bMoveDownClick(Sender : TObject);
    procedure bRemoveFrameClick(Sender : TObject);
    procedure bSubirClick(Sender : TObject);
    procedure lbxFrameListSelectionChange(Sender : TObject; User : boolean);

  private
    FOnFrameSelect : TCTKFrameObjProc;
    procedure SetOnFrameSelect(AValue : TCTKFrameObjProc);

  protected
    procedure LoadFrameData; override;
    procedure ClearFrameData; override;
  public
    property OnFrameSelect : TCTKFrameObjProc
      read FOnFrameSelect write SetOnFrameSelect;

    constructor Create(TheOwner : TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCTKEditorFrameList }

procedure TfmCTKEditorFrameList.bSubirClick(Sender : TObject);
begin
  if not assigned(Comic) then
    Exit;

  if lbxFrameList.ItemIndex < 1 then
    Exit;

  Comic.Frames.Exchange(lbxFrameList.ItemIndex, lbxFrameList.ItemIndex - 1);
  lbxFrameList.Items.Exchange(lbxFrameList.ItemIndex,
    lbxFrameList.ItemIndex - 1);

  lbxFrameList.ItemIndex := lbxFrameList.ItemIndex - 1;
end;

procedure TfmCTKEditorFrameList.lbxFrameListSelectionChange(Sender : TObject;
  User : boolean);
var
  aFrame : cComictecaFrame;
begin
  if lbxFrameList.ItemIndex < 0 then
    aFrame := nil
  else
    aFrame := cComictecaFrame(
      lbxFrameList.Items.Objects[lbxFrameList.ItemIndex]);

  if Assigned(OnFrameSelect) then
    OnFrameSelect(aFrame);
end;

procedure TfmCTKEditorFrameList.SetOnFrameSelect(AValue : TCTKFrameObjProc);
begin
  if FOnFrameSelect = AValue then
    Exit;
  FOnFrameSelect := AValue;
end;

procedure TfmCTKEditorFrameList.LoadFrameData;
var
  i : integer;
  aCaption : string;
  aFrame : cComictecaFrame;
begin
  inherited;

  ClearFrameData;

  Enabled := Assigned(Comic);

  if not Enabled then
    Exit;

  i := 0;
  while (i < Comic.Frames.Count) do
  begin
    aFrame := Comic.Frames[i];
    if Assigned(aFrame) then
    begin
      if Assigned(aFrame.Page) then
        aCaption := aFrame.Page.FileName
      else
        aCaption := 'No page assigned';

      if not aFrame.ImgRect.IsEmpty then
        aCaption := aCaption + ' - ' + aFrame.ImgRect.ToString(',');

      lbxFrameList.AddItem(aCaption, aFrame);
    end;
    Inc(i);
  end;
end;

procedure TfmCTKEditorFrameList.ClearFrameData;
begin
  inherited;

  lbxFrameList.Clear;
end;

constructor TfmCTKEditorFrameList.Create(TheOwner : TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmCTKEditorFrameList.Destroy;
begin
  inherited Destroy;
end;

procedure TfmCTKEditorFrameList.bMoveDownClick(Sender : TObject);
begin
  if not assigned(Comic) then
    Exit;

  if (lbxFrameList.ItemIndex < 0) or (lbxFrameList.ItemIndex >
    (lbxFrameList.Items.Count - 2)) then
    Exit;

  Comic.Frames.Exchange(lbxFrameList.ItemIndex, lbxFrameList.ItemIndex + 1);
  lbxFrameList.Items.Exchange(lbxFrameList.ItemIndex,
    lbxFrameList.ItemIndex + 1);

  lbxFrameList.ItemIndex := lbxFrameList.ItemIndex + 1;
end;

procedure TfmCTKEditorFrameList.bAddFrameClick(Sender : TObject);
var
  aFrame : cComictecaFrame;
  aPos : integer;
begin
  if not assigned(Comic) then
    Exit;

  aPos := lbxFrameList.ItemIndex + 1;

  aFrame := cComictecaFrame.Create(nil);

  if (aPos < 1) or (aPos >= lbxFrameList.Count) then
  begin
    aPos := Comic.Frames.Add(aFrame);
    lbxFrameList.AddItem(IntToStr(aPos), aFrame);
  end
  else
  begin
    Comic.Frames.Insert(aPos, aFrame);
    lbxFrameList.Items.Insert(aPos, IntToStr(aPos));
    lbxFrameList.Items.Objects[aPos] := aFrame;
  end;

  lbxFrameList.ItemIndex := aPos;
end;

procedure TfmCTKEditorFrameList.bRemoveFrameClick(Sender : TObject);
var
  aPos : integer;
begin
  if not assigned(Comic) then
    Exit;

  if lbxFrameList.ItemIndex < 0 then
    Exit;

  aPos := lbxFrameList.ItemIndex;

  Comic.Frames.Delete(aPos);
  lbxFrameList.Items.Delete(aPos);

  if lbxFrameList.Count = 0 then
    OnFrameSelect(nil)
  else
  begin
    if aPos >= lbxFrameList.Count then
      lbxFrameList.ItemIndex := lbxFrameList.Count - 1
    else
      lbxFrameList.ItemIndex := aPos;
  end;
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
