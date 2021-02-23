unit ucComictecaVolume;

{< cComictecaVolume class unit.

  This file is part of Comicteca Core.

  Copyright (C) 2019-2020 Chixpy

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
  Classes, SysUtils, LazUTF8, LazFileUtils, Laz2_DOM, laz2_XMLRead,
  Laz2_XMLWrite,
  // CHX units
  uCHXStrUtils, uCHXFileUtils,
  // Comicteca core units
  uCTKConst,
  // Comicteca Core abstracts
  uaComictecaVolume,
  // Comicteca Core classes
  ucComictecaPage, ucComictecaPageList, ucComictecaFrameList;

type

  { cComictecaVolume }

  cComictecaVolume = class(caComictecaVolume)
  private
    FPages: cComictecaPageList;
    FFrames: cComictecaFrameList;

  public
    procedure Clear; override;

    procedure LoadFromXML(aXMLDoc: TXMLDocument); override;
    procedure SaveToXML(aXMLDoc: TXMLDocument); override;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published

    property Pages: cComictecaPageList read FPages;
    property Frames: cComictecaFrameList read FFrames;

  end;

implementation

{ cComictecaVolume }

//procedure cComictecaVolume.LoadFromIni(aIni: TIniFile);
//var
//  aSL: TStringList;
//  i, Count: integer;
//  aPage: cComictecaPage;
//begin
//   if not Assigned(aIni) then Exit;

//  // Volume info
//  inherited LoadFromIni(aIni);

//  // Page List
//  aSL := TStringList.Create;

//  // WriteSectionRaw doesn't exists :-(
//  // aIni.ReadSectionRaw(krsIniSectionPages, aSL);

//  Count := aIni.ReadInteger(krsIniSectionPages, krsIniKeyNPages, 0);
//  i := 0;
//  while i < Count do
//  begin
//     aSL.Clear;

//     aSL.CommaText := aIni.ReadString(krsIniSectionPages, IntToStr(i), '');

//     if aSL.Count >= 1 then
//     begin
//       aPage := cComictecaPage.Create(nil);
//       aPage.FileName := UTF8Trim(aSL[0]);
//       if aSL.Count >= 2 then
//         aPage.SHA1 := UTF8Trim(aSL[1]);

//       // Searching SHA1 if decompressed, empty or wrong size
//       if (UTF8Length(UTF8Trim(aPage.SHA1)) <> 20) and
//         FileExistsUTF8(SetAsFolder(Folder) + aPage.FileName) then
//         aPage.SHA1 := SHA1FileStr(SetAsFolder(Folder) + aPage.FileName);

//       Pages.Add(aPage);

//       // Pages Info
//       aPage.LoadFromIni(aIni);
//     end;

//    Inc(i);
//  end;
//  aSL.Free;


//  // Vignettes

//  // Metadata

//end;

//procedure cComictecaVolume.SaveToIni(aIni: TIniFile);
//var
//  i: Integer;
//  aPage: cComictecaPage;
//begin
//   if not Assigned(aIni) then Exit;

//  // Volume info
//  inherited SaveToIni(aIni);

//  // Page List
//  // WriteSectionRaw doesn't exists :-(
//  // aIni.WriteSectionRaw(krsIniSectionPages, aSL);

//  aIni.WriteInteger(krsIniSectionPages, krsIniKeyNPages, Pages.Count);
//  i := 0;
//  while i < Pages.Count do
//  begin
//    aPage := Pages[i];
//           // Searching SHA1 if decompressed, empty or wrong size
//       if (UTF8Length(UTF8Trim(aPage.SHA1)) <> 20) and
//         FileExistsUTF8(SetAsFolder(Folder) + aPage.FileName) then
//         aPage.SHA1 := SHA1FileStr(SetAsFolder(Folder) + aPage.FileName);

//    aIni.WriteString(krsIniSectionPages, IntToStr(i),
//      '"' + aPage.FileName + '", ' + aPage.SHA1);

//    // Pages Info
//    aPage.SaveToIni(aIni);

//    Inc(i);
//  end;

//  // Vignettes

//  // Metadata
//end;

procedure cComictecaVolume.Clear;
begin
  inherited Clear;

  Frames.Clear;
  Pages.Clear;
end;

procedure cComictecaVolume.LoadFromXML(aXMLDoc: TXMLDocument);
var
  Root, XMLNode: TDOMElement;
  aPage: cComictecaPage;
  i: Integer;
begin
  if not Assigned(aXMLDoc) then
    Exit;

  inherited LoadFromXML(aXMLDoc);

  // Root
  Root := aXMLDoc.DocumentElement;
  if not assigned(Root) then
    Exit;

  // Page list
  XMLNode := TDOMElement(Root.FindNode(krsCTKXMLPageList));
  if Assigned(XMLNode) then
    Pages.LoadFromXML(XMLNode);

  // Searching SHA1 if empty or wrong size
  i := 0;
  while i < Pages.Count do
  begin
    aPage := Pages[i];

    if (UTF8Length(UTF8Trim(aPage.SHA1)) <> 20) and
      FileExistsUTF8(SetAsFolder(Folder) + aPage.FileName) then
      aPage.SHA1 := SHA1FileStr(SetAsFolder(Folder) + aPage.FileName);

    Inc(i);
  end;

  // Frame list
  XMLNode := TDOMElement(Root.FindNode(krsCTKXMLFrameList));
  if Assigned(XMLNode) then
    Frames.LoadFromXML(XMLNode);
end;

procedure cComictecaVolume.SaveToXML(aXMLDoc: TXMLDocument);
var
  Root, XMLNode: TDOMElement;
begin
  if not Assigned(aXMLDoc) then
    Exit;

  inherited SaveToXML(aXMLDoc);

  Root := aXMLDoc.DocumentElement;
  if not assigned(Root) then
    Exit;

  // Page list node
  XMLNode := aXMLDoc.CreateElement(krsCTKXMLPageList);
  Root.AppendChild(XMLNode);
  Pages.SaveToXML(aXMLDoc, XMLNode);


  // Frame list node
  XMLNode := aXMLDoc.CreateElement(krsCTKXMLFrameList);
  Root.AppendChild(XMLNode);
  Frames.SaveToXML(aXMLDoc, XMLNode);

end;

constructor cComictecaVolume.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  FPages := cComictecaPageList.Create(True);
  FFrames := cComictecaFrameList.Create(True);
  Frames.PageList := Pages;
end;

destructor cComictecaVolume.Destroy;
begin
  Frames.Free;
  Pages.Free;

  inherited Destroy;
end;

initialization
  RegisterClass(cComictecaVolume);

finalization
  UnRegisterClass(cComictecaVolume);

end.
