unit ucComictecaFrame;
{< TfmCTKGUIMain frame unit.

  This file is part of Comicteca Core.

  Copyright (C) 2019-2024 Chixpy
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
    FPage : caComictecaPage;
    function GetPageSHA1 : string;
    procedure SetPage(AValue : caComictecaPage);

  protected
    FPageSHA1 : string;

  public
    property PageSHA1 : string read GetPageSHA1;

    procedure LoadFromXML(aXMLNode : TDOMElement); override;
    procedure SaveToXML(aXMLDoc : TXMLDocument; aXMLNode : TDOMElement);
      override;

    constructor Create(aOwner : TComponent); override;
    destructor Destroy; override;


  published
    property Page : caComictecaPage read FPage write SetPage;

  end;

  TCTKFrameObjProc = procedure(aCTKFrame : cComictecaFrame) of object;

implementation

{ cComictecaFrame }

procedure cComictecaFrame.SetPage(AValue : caComictecaPage);
begin
  if FPage = AValue then
    Exit;

  // if unassigned, at least keep last PageSHA1
  if (not Assigned(AValue)) and Assigned(Page) then
    FPageSHA1 := Page.SHA1;

  FPage := AValue;

  // Update PageSHA1
  if Assigned(Page) then
    FPageSHA1 := Page.SHA1;

  FPONotifyObservers(Self, ooChange, nil);
end;

function cComictecaFrame.GetPageSHA1 : string;
begin
  // Returns actual SHA1 or last SHA1
  if Assigned(Page) then
    Result := Page.SHA1
  else
    Result := FPageSHA1;
end;

procedure cComictecaFrame.LoadFromXML(aXMLNode : TDOMElement);
begin
  inherited LoadFromXML(aXMLNode);

  FPageSHA1 := aXMLNode[krsCTKXMLPage];
end;

procedure cComictecaFrame.SaveToXML(aXMLDoc : TXMLDocument;
  aXMLNode : TDOMElement);
begin
  aXMLNode[krsCTKXMLPage] := PageSHA1;

  inherited SaveToXML(aXMLDoc, aXMLNode);
end;

constructor cComictecaFrame.Create(aOwner : TComponent);
begin
  inherited Create(aOwner);
end;

destructor cComictecaFrame.Destroy;
begin
  inherited Destroy;
end;

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
end.
