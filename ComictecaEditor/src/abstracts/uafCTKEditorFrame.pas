unit uafCTKEditorFrame;
{< TafmCTKEditorFrame abstract frame unit.

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
  // CHX frames
  ufCHXFrame,
  // Comicteca classes
  ucComictecaVolume;

type

  { TafmCTKEditorFrame }

  TafmCTKEditorFrame = class(TfmCHXFrame)
  private
    FComic: cComictecaVolume;

  protected
    procedure SetComic(AValue: cComictecaVolume); virtual;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Comic: cComictecaVolume read FComic write SetComic;
  end;

implementation

{$R *.lfm}

{ TafmCTKEditorFrame }

procedure TafmCTKEditorFrame.SetComic(AValue: cComictecaVolume);
begin
  if FComic = AValue then Exit;
  FComic := AValue;

  LoadFrameData;
end;

constructor TafmCTKEditorFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TafmCTKEditorFrame.Destroy;
begin
  inherited Destroy;
end;

end.

