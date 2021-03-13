unit ucComictecaFrame;
{< TfmCTKGUIMain frame unit.

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
  Classes, SysUtils, Laz2_DOM, laz2_XMLRead, Laz2_XMLWrite,
    // Comicteca Core units
  uCTKConst, uCTKCommon,
  // Comicteca Core abstracts
  uaComictecaFrame, uaComictecaPage;


type

  { cComictecaFrame }

  cComictecaFrame = class(caComictecaFrame)
  private
    FPage: caComictecaPage;
    FPageSHA1: String;
    procedure SetPage(AValue: caComictecaPage);
    procedure SetPageSHA1(AValue: String);

  public
    property PageSHA1: String read FPageSHA1 write SetPageSHA1;

    procedure LoadFromXML(aXMLNode: TDOMElement); override;
    procedure SaveToXML(aXMLDoc: TXMLDocument; aXMLNode: TDOMElement); override;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;


  published
    property Page: caComictecaPage read FPage write SetPage;

  end;

  TCTKFrameObjProc = procedure(aCTKFrame: cComictecaFrame) of object;

implementation

{ cComictecaFrame }

procedure cComictecaFrame.SetPage(AValue: caComictecaPage);
begin
  if FPage = AValue then Exit;
  FPage := AValue;

  PageSHA1 := Page.SHA1;

  FPONotifyObservers(Self, ooChange, nil);
end;

procedure cComictecaFrame.SetPageSHA1(AValue: String);
begin
  if FPageSHA1 = AValue then Exit;
  FPageSHA1 := AValue;
end;

procedure cComictecaFrame.LoadFromXML(aXMLNode: TDOMElement);
begin
  inherited LoadFromXML(aXMLNode);

  PageSHA1 := aXMLNode[krsCTKXMLPage];
end;

procedure cComictecaFrame.SaveToXML(aXMLDoc: TXMLDocument; aXMLNode: TDOMElement
  );
begin
  if Assigned(Page) then
    aXMLNode[krsCTKXMLPage] := Page.SHA1;

  inherited SaveToXML(aXMLDoc, aXMLNode);
end;

constructor cComictecaFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
end;

destructor cComictecaFrame.Destroy;
begin
  inherited Destroy;
end;

end.

