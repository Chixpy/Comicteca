unit uaComictecaShapedImage;
 
{< caComictecaText abstact class unit.

  This file is part of Comicteca Core.

  Copyright (C) 2023-2024 Chixpy
}
{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Laz2_DOM,
    // Comicteca Core units
  uCTKConst, uCTKCommon;

const
  kCTKImgDefShape = CTKFSRect;

type
  caComictecaShapedImage = class(TComponent)
    procedure SetCallObservers(AValue: boolean);
    procedure SetImgShape(AValue: tCTKImageShape);

   private
    FCallObservers: boolean;
    FImgShape: tCTKImageShape;

   public
    ImgRect: TRect;
    {< Rect of text.

      if Shape = CTKFSEllipse, external Rectangle where the ellipse is
        inscribed.
    }

    ImgPoint: TPoint;
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

    procedure LoadFromXML(aXMLNode: TDOMElement); virtual; abstract;
    procedure SaveToXML(aXMLDoc: TXMLDocument; aXMLNode: TDOMElement); virtual; abstract;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property ImgShape: tCTKImageShape read FImgShape write SetImgShape;

  end;

implementation

procedure caComictecaShapedImage.SetImgShape(AValue: tCTKImageShape);
begin
  if FImgShape = AValue then
    Exit;
  FImgShape := AValue;

  if CallObservers then
    FPONotifyObservers(Self, ooChange, nil);
end;

procedure caComictecaShapedImage.SetCallObservers(AValue: boolean);
begin
  // if FCallObservers = AValue then Exit; <- Call if True anyway
  FCallObservers := AValue;

  if CallObservers then
    FPONotifyObservers(Self, ooChange, nil);
end;

constructor caComictecaShapedImage.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  CallObservers := False; // Don't notify Observers until created
  ImgRect := TRect.Empty;
  ImgPoint := TPoint.Zero;
  ImgShape := kCTKImgDefShape;
  CallObservers := True;
end;

destructor caComictecaShapedImage.Destroy;
begin
  FPONotifyObservers(Self, ooFree, nil);

  inherited Destroy;
end;

initialization
  RegisterClass(caComictecaShapedImage);

finalization
  UnRegisterClass(caComictecaShapedImage);

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

