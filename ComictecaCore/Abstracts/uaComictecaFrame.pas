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
  uCTKConst, uCTKCommon;

const
  kCTKFrameDefType = CTKFTVignette;
  kCTKFrameDefShape = CTKFSRect;

type

  { caComictecaFrame }

  caComictecaFrame = class(TComponent)
  private
    FCallObservers: boolean;
    FFrameShape: tCTKFrameShape;
    FFrameType: tCTKFrameType;
    FNoFlip: Boolean;
    procedure SetCallObservers(AValue: boolean);
    procedure SetFrameShape(AValue: tCTKFrameShape);
    procedure SetFrameType(AValue: tCTKFrameType);
    procedure SetNoFlip(AValue: Boolean);

  public
    FrameRect: TRect;
    {< Rect of text.

      if Shape = CTKFSEllipse, external Rectangle where the ellipse is
        inscribed.
    }

    FramePoint: TPoint;
    {< Depends on Shape. Its values are relative to Frame.Rect

      * FrameShape = CTKFSRect -> Not used. (Maybe for: center of the text?)

      * FrameShape = CTKFSRndRect -> X and Y are the diameters of elliptical
        corners.

      * FrameShape = CTKFSEllipse -> Define a corner of a rectangle to cut the
        ellipse (for example, cut by vignette edges). Positive values are for
        left/top rectangle corner; negative for right/bottom corner. Examples:
        * 2, 5 -> Top Left corner.
        * -2, 5 -> Top Right corner.
        * 2, -5 -> Bottom Left corner.
        * -2, -5 -> Bottom Right corner.
    }

    property CallObservers: boolean read FCallObservers write SetCallObservers;
    {< If True (default), the object call its observed on any change.

      Change it to false at beginning of many changes will be done and want
        to update observers only one time at the end, when must be set to True
        again.
    }

    procedure LoadFromXML(aXMLNode: TDOMElement); virtual;
    procedure SaveToXML(aXMLDoc: TXMLDocument; aXMLNode: TDOMElement); virtual;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property FrameType: tCTKFrameType read FFrameType write SetFrameType;
    {< Content of the frame. }
    property FrameShape: tCTKFrameShape read FFrameShape write SetFrameShape;
    {< Shape of the frame. }

    property NoFlip: Boolean read FNoFlip write SetNoFlip;
    {< Don't flip frame (Right to left comics, manga). }

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

procedure caComictecaFrame.SetNoFlip(AValue: Boolean);
begin
  if FNoFlip = AValue then Exit;
  FNoFlip := AValue;

  if CallObservers then
    FPONotifyObservers(Self, ooChange, nil);
end;

procedure caComictecaFrame.SetCallObservers(AValue: boolean);
begin
  // if FCallObservers = AValue then Exit; <- Call if True anyway
  FCallObservers := AValue;

  if CallObservers then
    FPONotifyObservers(Self, ooChange, nil);
end;

procedure caComictecaFrame.SetFrameShape(AValue: tCTKFrameShape);
begin
  if FFrameShape = AValue then
    Exit;
  FFrameShape := AValue;

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

  FrameRect.FromString(aXMLNode[krsCTKXMLRectProp]);

  aSrt := aXMLNode[krsCTKXMLFrameType];
  if aSrt <> '' then
    FrameType := Str2FrameType(aSrt)
  else
    FrameType := kCTKFrameDefType;

  aSrt := aXMLNode[krsCTKXMLShapeProp];
  if aSrt <> '' then
    FrameShape := Str2FrameShape(aSrt)
  else
    FrameShape := kCTKFrameDefShape;

  FramePoint.FromString(aXMLNode[krsCTKXMLPointProp], ',');

  CallObservers := True;
end;

procedure caComictecaFrame.SaveToXML(aXMLDoc: TXMLDocument;
  aXMLNode: TDOMElement);
begin
  if (not Assigned(aXMLNode)) or (not Assigned(aXMLDoc)) then
    Exit;

  aXMLNode[krsCTKXMLRectProp] := FrameRect.ToString;

  if FrameType <> kCTKFrameDefType then
    aXMLNode[krsCTKXMLFrameType] := ComictecaFrameTypeKey[FrameType];

  if FrameShape <> kCTKFrameDefShape then
    aXMLNode[krsCTKXMLShapeProp] := ComictecaFrameShapeKey[FrameShape];

  if not FramePoint.IsZero then
    aXMLNode[krsCTKXMLPointProp] := FramePoint.ToString(',');
end;

constructor caComictecaFrame.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  CallObservers := False; // Don't notify Observers until created
  FrameRect := Default(TRect);
  FramePoint := Default(TPoint);
  FrameType := kCTKFrameDefType;
  FrameShape := kCTKFrameDefShape;
  CallObservers := True;
end;

destructor caComictecaFrame.Destroy;
begin
  FPONotifyObservers(Self, ooFree, nil);

  inherited Destroy;
end;

initialization
  RegisterClass(caComictecaFrame);

finalization
  UnRegisterClass(caComictecaFrame);
end.
