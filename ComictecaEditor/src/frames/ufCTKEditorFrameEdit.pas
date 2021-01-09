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
  uaComictecaPage, uaComictecaFrame,
  uafCTKEditorFrame;

type

  { TfmCTKEditorFrameEdit }

  TfmCTKEditorFrameEdit = class(TafmCTKEditorFrame)
    cbxFramePage: TComboBox;
    eX: TSpinEdit;
    eWidth: TSpinEdit;
    eY: TSpinEdit;
    eHeight: TSpinEdit;
    gbxFrameEdit: TGroupBox;
    Label1: TLabel;
    lX: TLabel;
    lWidth: TLabel;
    lY: TLabel;
    lHeight: TLabel;

  private
    FComictecaFrame: caComictecaFrame;
    procedure SetComictecaFrame(AValue: caComictecaFrame);

  protected
    procedure DoLoadFrameData;
    procedure DoClearFrameData;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  published
    property ComictecaFrame: caComictecaFrame
      read FComictecaFrame write SetComictecaFrame;

  end;

implementation

{$R *.lfm}

{ TfmCTKEditorFrameEdit }

procedure TfmCTKEditorFrameEdit.SetComictecaFrame(AValue: caComictecaFrame);
begin
  if FComictecaFrame = AValue then
    Exit;
  FComictecaFrame := AValue;
end;

procedure TfmCTKEditorFrameEdit.DoLoadFrameData;
begin

end;

procedure TfmCTKEditorFrameEdit.DoClearFrameData;
begin

end;

constructor TfmCTKEditorFrameEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnLoadFrameData := @DoLoadFrameData;
  OnClearFrameData := @DoClearFrameData;
end;

destructor TfmCTKEditorFrameEdit.Destroy;
begin
  inherited Destroy;
end;

end.
