unit uCTKRstStr;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
    // Comicteca units
  uCTKConst;
resourcestring
  rsPCVignettes = 'Vignettes';
  rsPCFrontCover = 'Front Cover';
  rsPCBackCover = 'Back Cover';
  rsPCChapterTitle = 'Chapter Title';
  rsPCEditorialInfo = 'Editorial Info';
  rsPCAuthorText = 'Author Text';
  rsPCAds = 'Advertising';
  rsPCOther = 'Other';

const
  ComictecaPageContentStr: array [tCTKPageContent] of string =
    (rsPCVignettes, rsPCFrontCover, rsPCBackCover,
    rsPCChapterTitle, rsPCEditorialInfo, rsPCAuthorText,
    rsPCAds, rsPCOther);

implementation

end.

