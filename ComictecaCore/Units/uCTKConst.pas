unit uCTKConst;

{< Comicteca Core constants unit.

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
  Classes, SysUtils;

const
  krsFmtApplicationTitle = '%0:s %1:s';
  {< Aplication title used in forms, formated by
       uEmutecaCommon.krsFmtWindowCaption.

     @param(%0:s 'Comicteca Editor' (Application name)).
     @param(%1:s Version).
  }
  krsFmtWindowCaption = '%0:s: %1:s';
  //< Application title.
  krsLocaleFolder = 'locale/';
  //< Subfolder for localization files.

  // XML file, sections and keys.
  krsCTKXMLComicFile = 'CTKInfo.xml';
  krsCTKXMLComicRoot = 'CTKInfo';

  // Common XML tags
  krsCTKXMLRAW = 'RAW';

  // Common XML properties
  krsCTKXMLPointProp = 'Point';
  krsCTKXMLRectProp = 'Rect';
  krsCTKXMLOrderProp = 'Order';
  krsCTKXMLShapeProp = 'Shape';

  // Volume
  krsCTKXMLVolume = 'Volume';
  krsCTKXMLLanguage = 'Language';
  krsCTKXMLFanSuber = 'FanSuber';
  krsCTKXMLSerie = 'Serie';
  krsCTKXMLTitle = 'Title';
  krsCTKXMLEditor = 'Editor';
  krsCTKXMLPublisher = 'Publisher';
  krsCTKXMLR2L = 'Right2Left';
  krsCTKXMLSummary = 'Summary';

  // Pages
  krsCTKXMLPageList = 'PageList';
  krsCTKXMLPage = 'Page';

  krsCTKXMLFileProp = 'File';
  krsCTKXMLSHA1Prop = 'SHA1';
  krsCTKXMLMultipageProp = 'Multipage';
  krsCTKXMLContentProp = 'Content';
  krsCTKXMLGeometryProp = 'Geometry';
  krsCTKXMLNoFlip = 'NoFlip';

  // Frames
  krsCTKXMLFrameList = 'FrameList';
  krsCTKXMLFrame = 'Frame';

  krsCTKXMLFrameType = 'FrameType';


  // Frame Types
  krsFTVignetteKey = 'Vignette';
  //< Viñeta
  krsFTSpineDustJacketKey = 'SpineDustJacket';
  //< Lomo de la sobrecubierta
  krsFTFrontDustJacketKey = 'FrontDustJacket';
  //< Frontal de la sobrecubierta
  krsFTSpineCoverKey = 'SpineCover';
  //< Lomo de la cubierta
  krsFTFrontCoverKey = 'FrontCover';
  //< Frontal de la cubierta (Portada)
  krsFTEndPaperKey = 'EndPaper';
  //< Guarda (Páginas al inicio o final que unen la cubierta con el resto.)
  krsFTFlyLeafKey = 'FlyLeaf';
  //< Página de cortesía (en blanco)
  krsFTIndexKey = 'Index';
  krsFTBastardTitleKey = 'BastardTitle';
  //< Antetítulo (Págína con título, casi vacía)
  krsFTChapterTitleKey = 'ChapterTitle';
  krsFTEditorialInfoKey = 'EditorialInfo';
  krsFTAuthorTextKey = 'AuthorText';
  krsFTAdsKey = 'Advertising';
  krsFTLicenseKey = 'License';
  krsFTBackCoverKey = 'BackCover';
  //< Trasera de la cubierta (Contraportada)
  krsFTBackDustJacketKey = 'BackDustJacket';
  //< Trasera de la sobrecubierta
  krsFTOtherKey = 'Other';

  // Frame Shapes
  krsFSRectKey = 'Rect';
  krsFSRndRectKey = 'RndRect';
  krsFSEllipseKey = 'Ellipse';

  // Texts
  krsCTKXMLText = 'Text';

// Metadata

type
  tCTKFrameType = (CTKFTVignette, CTKFTSpineDustJacket, CTKFTFrontDustJacket,
    CTKFTSpineCover, CTKFTFrontCover, CTKFTEndPaper,
    CTKFTFlyLeaf, CTKFTIndex, CTKFTBastardTitle, CTKFTChapterTitle,
    CTKFTEditorialInfo, CTKFTAuthorText, CTKFTAds, CTKFTLicense,
    CTKFTBackCover, CTKFTBackDustJacket, CTKFTOther);
  tCTKPageContents = set of tCTKFrameType;

  tCTKImageShape = (CTKFSRect, CTKFSRndRect, CTKFSEllipse);

const
  ComictecaFrameTypeKey: array [tCTKFrameType] of string =
    (krsFTVignetteKey, krsFTSpineDustJacketKey, krsFTFrontDustJacketKey,
    krsFTSpineCoverKey, krsFTFrontCoverKey, krsFTEndPaperKey,
    krsFTFlyLeafKey, krsFTIndexKey, krsFTBastardTitleKey, krsFTChapterTitleKey,
    krsFTEditorialInfoKey, krsFTAuthorTextKey, krsFTAdsKey, krsFTLicenseKey,
    krsFTBackCoverKey, krsFTBackDustJacketKey, krsFTOtherKey);

  ComictecaFrameShapeKey: array [tCTKImageShape] of string =
    (krsFSRectKey, krsFSRndRectKey, krsFSEllipseKey);

implementation

end.
