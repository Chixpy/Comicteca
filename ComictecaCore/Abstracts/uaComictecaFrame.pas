unit uaComictecaFrame;

{< TfmCTKGUIMain frame unit.

  This file is part of Comicteca Core.

  Copyright (C) 2021 Chixpy

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
  // CHX units
  uCHXRecordHelpers,
  // Comicteca Core units
  uCTKConst, uCTKCommon,
  // Comicteca Core abstracts
  uaComictecaShapedImage;

const
  kCTKFrameDefType = CTKFTVignette;

type

  { caComictecaFrame }

  caComictecaFrame = class(caComictecaShapedImage)
  private
    FFrameType: tCTKFrameType;
    procedure SetFrameType(AValue: tCTKFrameType);

  public

    procedure LoadFromXML(aXMLNode: TDOMElement); override;
    procedure SaveToXML(aXMLDoc: TXMLDocument; aXMLNode: TDOMElement); override;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property FrameType: tCTKFrameType read FFrameType write SetFrameType;
    {< Content of the frame. }
  end;

implementation

procedure caComictecaFrame.SetFrameType(AValue: tCTKFrameType);
begin
  if FFrameType = AValue then
    Exit;
  FFrameType := AValue;

  if CallObservers then
    FPONotifyObservers(Self, ooChange, nil);
end;

procedure caComictecaFrame.LoadFromXML(aXMLNode: TDOMElement);
var
  aSrt: string;
begin
  if not Assigned(aXMLNode) then
    Exit;

  CallObservers := False; // Don't notify Observer

  ImgRect.FromString(aXMLNode[krsCTKXMLRectProp]);

  aSrt := aXMLNode[krsCTKXMLFrameType];
  if aSrt <> '' then
    FrameType := Str2FrameType(aSrt)
  else
    FrameType := kCTKFrameDefType;

  aSrt := aXMLNode[krsCTKXMLShapeProp];
  if aSrt <> '' then
    ImgShape := Str2FrameShape(aSrt)
  else
    ImgShape := kCTKImgDefShape;

  ImgPoint.FromString(aXMLNode[krsCTKXMLPointProp], ',');

  CallObservers := True;
end;

procedure caComictecaFrame.SaveToXML(aXMLDoc: TXMLDocument;
  aXMLNode: TDOMElement);
begin
  if (not Assigned(aXMLNode)) or (not Assigned(aXMLDoc)) then
    Exit;

  aXMLNode[krsCTKXMLRectProp] := ImgRect.ToString;

  if FrameType <> kCTKFrameDefType then
    aXMLNode[krsCTKXMLFrameType] := ComictecaFrameTypeKey[FrameType];

  if ImgShape <> kCTKImgDefShape then
    aXMLNode[krsCTKXMLShapeProp] := ComictecaFrameShapeKey[ImgShape];

  if not ImgPoint.IsZero then
    aXMLNode[krsCTKXMLPointProp] := ImgPoint.ToString(',');
end;

constructor caComictecaFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FrameType := kCTKFrameDefType;
end;

destructor caComictecaFrame.Destroy;
begin
  inherited Destroy;
end;

initialization
  RegisterClass(caComictecaFrame);

finalization
  UnRegisterClass(caComictecaFrame);
end.
