unit ucComictecaPage;

{< cComictecaPage class unit.

  This file is part of Comicteca Core.

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
  Classes, SysUtils,
  // Comicteca Core abstracts
  uaComictecaPage;

type

  { cComictecaPage class.

    Defines a full physical page (usually an image).

    Stores full comic page information. }

  cComictecaPage = class(caComictecaPage)
  private

  protected

  public

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCTKPageObjProc = procedure(aCTKPage: cComictecaPage) of object;

implementation

{ cComictecaPage }

constructor cComictecaPage.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor cComictecaPage.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(cComictecaPage);

finalization
  UnRegisterClass(cComictecaPage);

end.
