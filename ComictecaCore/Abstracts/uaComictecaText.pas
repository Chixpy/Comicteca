unit uaComictecaText;

{< caComictecaText abstact class unit.

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
  uCHXRecordHelpers, uCHXStrUtils,
  // Comicteca Core units
  uCTKConst, uCTKCommon,
  // Comicteca Core class
  ucComictecaTextMap;

const
  kCTKTextDefShape = CTKFSRect;

type

  { caComictecaText }

  caComictecaText = class(TComponent)
  private
    FContent: cComictecaTextMap;
    FCallObservers: boolean;
    FTextShape: tCTKFrameShape;
    procedure SetCallObservers(AValue: boolean);
    procedure SetTextShape(AValue: tCTKFrameShape);

  protected

  public
    TextRect: TRect;
    {< Rect of text.

      if Shape = CTKFSEllipse, external Rectangle where the ellipse is
        inscribed.
    }

    TextPoint: TPoint;
    {< Depends on Shape.

      * TextShape = CTKFSRect -> Not used.

      * TextShape = CTKFSRndRect -> X and Y are the diameters of elliptical
        corners.

      * TextShape = CTKFSEllipse -> Define a corner of a rectangle to cut the
        ellipse (for example, cut by vignette edges). Positive values are for
        left/top rectangle corner; negative for right/bottom corner.
    }

    property CallObservers: boolean read FCallObservers write SetCallObservers;
    {< If True (default), the object call its observed on any change.

      Change it to false at beginning of many changes will be done and want
        to update observers only one time at the end, when set to True again.
    }

    procedure LoadFromXML(aXMLNode: TDOMElement); virtual;
    procedure SaveToXML(aXMLDoc: TXMLDocument; aXMLNode: TDOMElement); virtual;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property TextShape: tCTKFrameShape read FTextShape write SetTextShape;

    property Content: cComictecaTextMap read FContent;
  end;

implementation

{ caComictecaText }

procedure caComictecaText.SetTextShape(AValue: tCTKFrameShape);
begin
  if FTextShape = AValue then
    Exit;
  FTextShape := AValue;

  if CallObservers then
    FPONotifyObservers(Self, ooChange, nil);
end;

procedure caComictecaText.SetCallObservers(AValue: boolean);
begin
  // if FCallObservers = AValue then Exit; <- Call if True anyway
  FCallObservers := AValue;

  if CallObservers then
    FPONotifyObservers(Self, ooChange, nil);
end;

procedure caComictecaText.LoadFromXML(aXMLNode: TDOMElement);
var
  aSrt: string;
begin
  if not Assigned(aXMLNode) then
    Exit;

  CallObservers := False; // Don't notify Observer

  TextRect.FromString(aXMLNode[krsCTKXMLRectProp]);

    aSrt := aXMLNode[krsCTKXMLShapeProp];
  if aSrt <> '' then
    TextShape := Str2FrameShape(aSrt)
  else
    TextShape := kCTKTextDefShape;

  TextPoint.FromString(aXMLNode[krsCTKXMLPointProp], ',');

  // Content
  Content.LoadFromXML(aXMLNode);

  CallObservers := True;
end;

procedure caComictecaText.SaveToXML(aXMLDoc: TXMLDocument;
  aXMLNode: TDOMElement);
begin
  if (not Assigned(aXMLNode)) or (not Assigned(aXMLDoc)) then
    Exit;

  aXMLNode[krsCTKXMLRectProp] := TextRect.ToString;

  if TextShape <> kCTKTextDefShape then
    aXMLNode[krsCTKXMLShapeProp] := ComictecaFrameShapeKey[TextShape];

  if not TextPoint.IsZero then
    aXMLNode[krsCTKXMLPointProp] := TextPoint.ToString(',');

  // Content
  Content.SaveToXML(aXMLDoc, aXMLNode);
end;

constructor caComictecaText.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  CallObservers := False; // Don't notify Observers until created
    TextRect := TRect.Empty;
  TextPoint := TPoint.Zero;
  FContent := cComictecaTextMap.Create(True);
  TextShape := kCTKTextDefShape;
  CallObservers := True;
end;

destructor caComictecaText.Destroy;
begin
  FPONotifyObservers(Self, ooFree, nil);

  FContent.Free;

  inherited Destroy;
end;

initialization
  RegisterClass(caComictecaText);

finalization
  UnRegisterClass(caComictecaText);

end.
