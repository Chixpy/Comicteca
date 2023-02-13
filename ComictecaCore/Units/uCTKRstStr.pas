unit uCTKRstStr;
{< Comicteca Core resource strings unit.

  This file is part of Comicteca Core.

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
  Classes, SysUtils,
    // Comicteca units
  uCTKConst;
resourcestring
  // Frame types.
  rsPCVignette = 'Vignette';
  rsPCSpineDustJacket = 'Spine Dust Jacket';
  rsPCFrontDustJacket = 'Front Dust Jacket';
  rsPCSpineCover = 'Spine (Cover)';
  rsPCFrontCover = 'Front Cover';
  rsPCEndPaper = 'End Paper';
  rsPCFlyLeaf = 'Fly Leaf';
  rsPCIndex = 'Index';
  rsPCBastardTitle = 'Bastard Title';
  rsPCChapterTitle = 'Chapter Title';
  rsPCEditorialInfo = 'Editorial Info';
  rsPCAuthorText = 'Author Text';
  rsPCAds = 'Advertising';
  rsPCLicense = 'License';
  rsPCBackCover = 'Back Cover';
  rsPCBackDustJacket  = 'Back Dust Jacket';
  rsPCOther = 'Other';

  // Frame shapes.
  rsFSRectKey = 'Rectangle';
  rsFSRndRectKey = 'Round Rect';
  rsFSEllipseKey = 'Ellipse';

const
  // Must be same order as ComictecaFrameTypeKey.
  ComictecaFrameTypeStr: array [tCTKFrameType] of string =
    (rsPCVignette, rsPCSpineDustJacket, rsPCFrontDustJacket,
    rsPCSpineCover, rsPCFrontCover, rsPCEndPaper,
    rsPCFlyLeaf, rsPCIndex, rsPCBastardTitle, rsPCChapterTitle,
    rsPCEditorialInfo, rsPCAuthorText, rsPCAds, rsPCLicense,
    rsPCBackCover, rsPCBackDustJacket, rsPCOther);

  // Must be same order as ComictecaFrameShapeKey.
    ComictecaFrameShapeStr: array [tCTKImageShape] of string =
    (rsFSRectKey, rsFSRndRectKey, rsFSEllipseKey);

implementation

end.

