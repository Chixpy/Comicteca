unit ufCTKGUIFileEditor;

{< TfmCTKGUIFileEditor frame unit.

  This file is part of Comicteca GUI.

  Copyright (C) 2019 Chixpy

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, ActnList,
  // CHX frames
  ufCHXPropEditor,
  // Comicteca Core classe
  ucComictecaVolume;

type

  { TfmCTKGUIFileEditor }

  TfmCTKGUIFileEditor = class(TfmCHXPropEditor)
    cbxseries: TComboBox;
    eTitle: TEdit;
    lOrder: TLabel;
    lSeries: TLabel;
    lTitle: TLabel;

  private

  protected
    procedure DoClearFrameData;
    procedure DoLoadFrameData;
    procedure DoSaveFrameData;

  public

    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer);

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  published

  end;

implementation

{$R *.lfm}

{ TfmCTKGUIFileEditor }

procedure TfmCTKGUIFileEditor.DoClearFrameData;
begin
  cbxseries.Text := '';
  etitle.Clear;
end;

procedure TfmCTKGUIFileEditor.DoLoadFrameData;
begin
//   Enabled := assigned(ComictecaFile);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;
end;

procedure TfmCTKGUIFileEditor.DoSaveFrameData;
begin

end;

procedure TfmCTKGUIFileEditor.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin

end;

constructor TfmCTKGUIFileEditor.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmCTKGUIFileEditor.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(TfmCTKGUIFileEditor);

finalization
  UnRegisterClass(TfmCTKGUIFileEditor);
end.
