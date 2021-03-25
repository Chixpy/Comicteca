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
    FCTKFrame: cComictecaFrame;
    procedure SetCTKFrame(AValue: cComictecaFrame);

  protected
    procedure DoLoadFrameFrame; virtual; abstract;
    procedure DoClearFrameFrame; virtual; abstract;

    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer); virtual;

  public
    property CTKFrame: cComictecaFrame read FCTKFrame write SetCTKFrame;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TafmCTKEditorFrameFrame }

procedure TafmCTKEditorFrameFrame.SetCTKFrame(AValue: cComictecaFrame);
begin
  if FCTKFrame = AValue then
    Exit;

  if Assigned(FCTKFrame) then
  begin
    FCTKFrame.FPODetachObserver(Self);
  end;

  FCTKFrame := AValue;

  if Assigned(FCTKFrame) then
  begin
    FCTKFrame.FPOAttachObserver(Self);
  end;

  DoLoadFrameFrame;
end;

procedure TafmCTKEditorFrameFrame.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if CTKFrame = ASender then
  begin
    case Operation of
      ooChange: DoLoadFrameFrame;
      ooFree: CTKFrame := nil;
      ooAddItem: ;
      ooDeleteItem: CTKFrame := nil;
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
  if Assigned(CTKFrame) then
    CTKFrame.FPODetachObserver(Self);

  inherited Destroy;
end;

end.
