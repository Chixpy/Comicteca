unit uafCTKEditorFrameFrame;
{< TafmCTKEditorFrameFrame abstract frame unit.

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  // Comicteca Core clases
  ucComictecaFrame,
  // Comicteca Core abstract frames
  uafCTKEditorFrame;

type

  { TafmCTKEditorFrameFrame }

  TafmCTKEditorFrameFrame = class(TafmCTKEditorFrame, IFPObserver)
  private
    FFrame: cComictecaFrame;
    procedure SetFrame(AValue: cComictecaFrame);

  protected
    procedure DoLoadFrameFrame; virtual; abstract;
    procedure DoClearFrameFrame; virtual; abstract;

    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer); virtual;

  public
    property Frame: cComictecaFrame read FFrame write SetFrame;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TafmCTKEditorFrameFrame }

procedure TafmCTKEditorFrameFrame.SetFrame(AValue: cComictecaFrame);
begin
  if FFrame = AValue then
    Exit;

  if Assigned(FFrame) then
  begin
    FFrame.FPODetachObserver(Self);
  end;

  FFrame := AValue;

  if Assigned(FFrame) then
  begin
    FFrame.FPOAttachObserver(Self);
  end;

  DoLoadFrameFrame;
end;

procedure TafmCTKEditorFrameFrame.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if Frame = ASender then
  begin
    case Operation of
      ooChange: DoLoadFrameFrame;
      ooFree: Frame := nil;
      ooAddItem: ;
      ooDeleteItem: Frame := nil;
      ooCustom: ;
    end;
  end;
end;

constructor TafmCTKEditorFrameFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TafmCTKEditorFrameFrame.Destroy;
begin
  if Assigned(Frame) then
    Frame.FPODetachObserver(Self);

  inherited Destroy;
end;

end.
