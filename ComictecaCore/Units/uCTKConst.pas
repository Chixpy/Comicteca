unit uCTKConst;

{< Comicteca Editor constants unit.

  This file is part of Comicteca Editor.

  Copyright (C) 2020 Chixpy

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

  // Common
  krsCTKXMLRAW = 'RAW';
  krsCTKXMLOrderProp = 'Order';


  // Volume
  krsCTKXMLVolume = 'Volume';
  krsCTKXMLSerie = 'Serie';
  krsCTKXMLTitle = 'Title';
  krsCTKXMLEditor = 'Editor';
  krsCTKXMLPublisher = 'Publisher';
  krsCTKXMLLanguage = 'Language';
  krsCTKXMLR2L = 'Right2Left';
    krsCTKXMLSummary = 'Summary';

  // Pages
  krsCTKXMLPageList = 'PageList';
  krsCTKXMLPage = 'Page';

  krsCTKXMLSHA1Prop = 'SHA1';
  krsCTKXMLMultipageProp = 'Multipage';
  krsCTKXMLContentProp = 'Content';

  krsPCVignettesKey = 'Vignettes';
  krsPCFrontCoverKey = 'FrontCover';
  krsPCBackCoverKey = 'BackCover';
  krsPCChapterTitleKey = 'ChapterTitle';
  krsPCEditorialInfoKey = 'EditorialInfo';
  krsPCAutorTextKey = 'AuthorText';
  krsPCAdsKey = 'Advertising';
  krsPCOtherKey = 'Other';

  // Frames
  krsCTKXMLFrameList = 'FrameList';
  krsCTKXMLFrame = 'Frame';

  // Metadata

type
  tCTKPageContent = (CTKPTVignettes, CTKPTFrontCover, CTKPTBackCover,
    CTKPTChapterTitle, CTKPTEditorialInfo, CTKPTAuthorText, CTKPTAds, CTKPTOther);
  tCTKPageContents = set of tCTKPageContent;

const
  ComictecaPageContentKey: array [tCTKPageContent] of string =
    (krsPCVignettesKey, krsPCFrontCoverKey, krsPCBackCoverKey,
    krsPCChapterTitleKey, krsPCEditorialInfoKey, krsPCAutorTextKey,
    krsPCAdsKey, krsPCOtherKey);

implementation

end.
