unit uaComictecaVolume;

{< caComictecaVolume abstact class unit.

  This file is part of Comiceca Core.

  Copyright (C) 2019-2021 Chixpy

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
  Classes, SysUtils, FileUtil, LazFileUtils, Laz2_DOM, laz2_XMLRead,
  Laz2_XMLWrite,
  // CHX units
  uCHXStrUtils, uCHX7zWrapper,
  // Comicteca Core units
  uCTKConst,
  // Comicteca Core class
  ucComictecaTextMap;

const
  kSerieOrder = -1;
  kTitleOrder = -1;

type

  { caComictecaVolume }

  caComictecaVolume = class(TComponent)
  private
    FArchive: string;
    FFanSuber: string;
    FLanguage: string;
    FPublisher: string;
    FSingleComic: boolean;
    FSummary: cComictecaTextMap;
    FTitle: cComictecaTextMap;
    FWriter: string;
    FFolder: string;
    FEditor: string;
    FRight2Left: boolean;
    FSerie: string;
    FSerieOrder: integer;
    FTitleOrder: integer;
    procedure SetArchive(AValue: string);
    procedure SetFanSuber(AValue: string);
    procedure SetLanguage(AValue: string);
    procedure SetPublisher(AValue: string);
    procedure SetSingleComic(AValue: boolean);
    procedure SetWriter(AValue: string);
    procedure SetFolder(AValue: string);
    procedure SetEditor(AValue: string);
    procedure SetRight2Left(AValue: boolean);
    procedure SetSerie(const AValue: string);
    procedure SetSerieOrder(AValue: integer);
    procedure SetTitleOrder(AValue: integer);

  public
    procedure Clear; virtual;

    procedure LoadFromArchive(aArchive: string);
    procedure LoadFromFolder(aFolder: string);
    procedure LoadFromFile(aFile: string);
    procedure LoadFromXML(aXMLDoc: TXMLDocument); virtual;

    procedure SaveToArchive;
    procedure SaveToFolder;
    procedure SaveToFile(aFile: string);
    procedure SaveToXML(aXMLDoc: TXMLDocument); virtual;

    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Archive: string read FArchive write SetArchive;
    {< Archive full path if loaded from a compressed archive. }
    property Folder: string read FFolder write SetFolder;
    {< Folder of comic.

      If loaded from compressed archive, its a temporal folder.

      But can be a simple folder not compressed. }

    // Volume properties
    property Language: string read FLanguage write SetLanguage;
    //< Language of the Volume
    property FanSuber: string read FFanSuber write SetFanSuber;
    {< If it's already fansubbed who did it. If it's original, empty.}

    property Serie: string read FSerie write SetSerie;

    property SerieOrder: integer read FSerieOrder write SetSerieOrder;

    property Title: cComictecaTextMap read FTitle;

    property TitleOrder: integer read FTitleOrder write SetTitleOrder;

    property Editor: string read FEditor write SetEditor;
    property Publisher: string read FPublisher write SetPublisher;

    property Right2Left: boolean read FRight2Left write SetRight2Left;
    {< Is it inverted? Usually original manga. }

    property Summary: cComictecaTextMap read FSummary;
  end;

implementation

{ caComictecaVolume }

procedure caComictecaVolume.SetSerie(const AValue: string);
begin
  if FSerie = AValue then
    Exit;
  FSerie := AValue;
end;

procedure caComictecaVolume.SetSerieOrder(AValue: integer);
begin
  if FSerieOrder = AValue then
    Exit;
  FSerieOrder := AValue;
end;

procedure caComictecaVolume.SetFolder(AValue: string);
begin
  FFolder := SetAsFolder(AValue);
end;

procedure caComictecaVolume.SetWriter(AValue: string);
begin
  if FWriter = AValue then
    Exit;
  FWriter := AValue;
end;

procedure caComictecaVolume.SetArchive(AValue: string);
begin
  if FArchive = AValue then
    Exit;
  FArchive := AValue;
end;

procedure caComictecaVolume.SetFanSuber(AValue: string);
begin
  if FFanSuber = AValue then
    Exit;
  FFanSuber := AValue;
end;

procedure caComictecaVolume.SetLanguage(AValue: string);
begin
  if FLanguage = AValue then
    Exit;
  FLanguage := AValue;
end;

procedure caComictecaVolume.SetPublisher(AValue: string);
begin
  if FPublisher = AValue then
    Exit;
  FPublisher := AValue;
end;

procedure caComictecaVolume.SetSingleComic(AValue: boolean);
begin
  if FSingleComic = AValue then
    Exit;
  FSingleComic := AValue;
end;

procedure caComictecaVolume.SetEditor(AValue: string);
begin
  if FEditor = AValue then
    Exit;
  FEditor := AValue;
end;

procedure caComictecaVolume.SetRight2Left(AValue: boolean);
begin
  if FRight2Left = AValue then
    Exit;
  FRight2Left := AValue;
end;

procedure caComictecaVolume.SetTitleOrder(AValue: integer);
begin
  if FTitleOrder = AValue then
    Exit;
  FTitleOrder := AValue;
end;

procedure caComictecaVolume.Clear;
begin
  // Archive is set and exists, Folder is a temp folder
  if (Archive <> '') and FileExistsUTF8(Archive) and
    DirectoryExistsUTF8(Folder) then
    DeleteDirectory(Folder, False);

  Archive := '';
  Folder := '';
  Language := '';
  FanSuber := '';
  Serie := '';
  SerieOrder := kSerieOrder;
  Title.Clear;
  TitleOrder := kTitleOrder;
  Editor := '';
  Publisher := '';
  Right2Left := False;
  Summary.Clear;
end;

procedure caComictecaVolume.LoadFromArchive(aArchive: string);
begin
  if not w7zPathsOK then
    Exit;

  if not FileExistsUTF8(aArchive) then
    Exit;

  if Folder = '' then
    Folder := SetAsFolder(GetTempDir(False)) + 'Comicteca';

  if w7zExtractFile(aArchive, '*', Folder, True, '') <> 0 then
  begin
    Folder := '';
    Exit;
  end;

  LoadFromFolder(Folder);
  // Comic is cleared on LoadFromFolder
  Archive := aArchive;
end;

procedure caComictecaVolume.LoadFromFolder(aFolder: string);
begin
  Self.Clear;
  Folder := aFolder;
  LoadFromFile(Folder + krsCTKXMLComicFile);
end;

procedure caComictecaVolume.LoadFromFile(aFile: string);
var
  aXMLFile: TXMLDocument;
begin
  if Folder = '' then
    Folder := ExtractFilePath(aFile);

  if not FileExistsUTF8(aFile) then
    Exit;

  // aXMLFile := TXMLDocument.Create; <- Autocreated with ReadXMLFile
  try
    ReadXMLFile(aXMLFile, aFile);
    LoadFromXML(aXMLFile);

  finally
    aXMLFile.Free;
  end;
end;

procedure caComictecaVolume.LoadFromXML(aXMLDoc: TXMLDocument);
var
  Root, Volume, Element: TDOMElement;
begin
  if not Assigned(aXMLDoc) then
    Exit;

  // Root
  Root := aXMLDoc.DocumentElement;
  if not assigned(Root) then
    Exit;

  // Volume
  Volume := TDOMElement(Root.FindNode(krsCTKXMLVolume));
  if not assigned(Volume) then
    Exit;

  // Serie
  Element := TDOMElement(Volume.FindNode(krsCTKXMLSerie));
  if assigned(Element) then
  begin
    Serie := Element.TextContent;
    SerieOrder := StrToIntDef(Element[krsCTKXMLOrderProp], -1);
  end;

  // Title
  Element := TDOMElement(Volume.FindNode(krsCTKXMLTitle));
  if assigned(Element) then
  begin
    Title.LoadFromXML(Element);
    TitleOrder := StrToIntDef(Element[krsCTKXMLOrderProp], -1);
  end;

  // Editor
  Element := TDOMElement(Volume.FindNode(krsCTKXMLEditor));
  if assigned(Element) then
    Editor := Element.TextContent;

  // Publisher
  Element := TDOMElement(Volume.FindNode(krsCTKXMLPublisher));
  if assigned(Element) then
    Publisher := Element.TextContent;

  // Language
  Element := TDOMElement(Volume.FindNode(krsCTKXMLLanguage));
  if assigned(Element) then
    Language := Element.TextContent;

  // FanSuber
  Element := TDOMElement(Volume.FindNode(krsCTKXMLFanSuber));
  if assigned(Element) then
    FanSuber := Element.TextContent;

  // Right 2 Left
  Element := TDOMElement(Volume.FindNode(krsCTKXMLR2L));
  Right2Left := assigned(Element);

  // Summary
  Element := TDOMElement(Volume.FindNode(krsCTKXMLSummary));
  if assigned(Element) then
    Summary.LoadFromXML(Element);
end;

procedure caComictecaVolume.SaveToArchive;
begin
  // Checking decompressed
  if Folder = '' then
    Exit;

  if not DirectoryExistsUTF8(Folder) then
    Exit;

  // Saving to Temp folder
  SaveToFolder;

  if not w7zPathsOK then
    Exit;

  Archive := SysPath(Archive);
  if FileExistsUTF8(Archive) then
    DeleteFileUTF8(Archive);
  if SupportedExtCT(Archive, 'zip,cbz') then
  begin
    w7zCompressFolder(Archive, Folder, False, True, 'zip');
  end
  else if SupportedExtCT(Archive, '7z,cb7') then
  begin
    w7zCompressFolder(Archive, Folder, False, True, '7z');
  end
  else
  begin
    Archive := ChangeFileExt(Archive, '.cbz');
    if FileExistsUTF8(Archive) then
      DeleteFileUTF8(Archive);
    w7zCompressFolder(Archive, Folder, False, True, 'zip');
  end;
end;

procedure caComictecaVolume.SaveToFolder;
begin
  if Folder = '' then
    Exit;

  if not DirectoryExistsUTF8(Folder) then
    Exit;

  SaveToFile(Folder + krsCTKXMLComicFile);
end;

procedure caComictecaVolume.SaveToFile(aFile: string);
var
  aXMLFile: TXMLDocument;
begin
  try
    aXMLFile := TXMLDocument.Create;
    SaveToXML(aXMLFile);
    WriteXMLFile(aXMLFile, aFile);

  finally
    aXMLFile.Free;
  end;
end;

procedure caComictecaVolume.SaveToXML(aXMLDoc: TXMLDocument);
var
  Root, Volume, Element: TDOMElement;
begin
  if not Assigned(aXMLDoc) then
    Exit;

  Root := aXMLDoc.CreateElement(krsCTKXMLComicRoot);
  aXMLDoc.Appendchild(Root);

  // Volume info node
  Volume := aXMLDoc.CreateElement(krsCTKXMLVolume);
  Root.AppendChild(Volume);

  // Language
  if Language <> '' then
  begin
    Element := aXMLDoc.CreateElement(krsCTKXMLLanguage);
    Volume.AppendChild(Element);
    Element.AppendChild(aXMLDoc.CreateTextNode(Language));
  end;

  // Fan subber
  if FanSuber <> '' then
  begin
    Element := aXMLDoc.CreateElement(krsCTKXMLFanSuber);
    Volume.AppendChild(Element);
    Element.AppendChild(aXMLDoc.CreateTextNode(FanSuber));
  end;

  // Serie
  if Serie <> '' then
  begin
    Element := aXMLDoc.CreateElement(krsCTKXMLSerie);
    Volume.AppendChild(Element);
    Element.AppendChild(aXMLDoc.CreateTextNode(Serie));
    // Serie Order
    if SerieOrder <> -1 then
      Element[krsCTKXMLOrderProp] := IntToStr(SerieOrder);
  end;

  // Title
  if Title.Count > 0 then
  begin
    Element := aXMLDoc.CreateElement(krsCTKXMLTitle);
    Volume.AppendChild(Element);
    Title.SaveToXML(aXMLDoc, Element);
    // Title Order
    if TitleOrder <> -1 then
      Element[krsCTKXMLOrderProp] := IntToStr(TitleOrder);
  end;

  // Editor
  if Editor <> '' then
  begin
    Element := aXMLDoc.CreateElement(krsCTKXMLEditor);
    Volume.AppendChild(Element);
    Element.AppendChild(aXMLDoc.CreateTextNode(Editor));
  end;

  // Publisher
  if Publisher <> '' then
  begin
    Element := aXMLDoc.CreateElement(krsCTKXMLPublisher);
    Volume.AppendChild(Element);
    Element.AppendChild(aXMLDoc.CreateTextNode(Publisher));
  end;

  // Right 2 Left
  if Right2Left then
  begin
    Element := aXMLDoc.CreateElement(krsCTKXMLR2L);
    Volume.AppendChild(Element);
  end;

  // Summary
  if Summary.Count > 0 then
  begin
    Element := aXMLDoc.CreateElement(krsCTKXMLSummary);
    Volume.AppendChild(Element);
    Summary.SaveToXML(aXMLDoc, Element);
  end;
end;

constructor caComictecaVolume.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);

  SerieOrder := kSerieOrder;
  TitleOrder := kTitleOrder;

  FTitle := cComictecaTextMap.Create(True);
  FSummary := cComictecaTextMap.Create(True);
end;

destructor caComictecaVolume.Destroy;
begin
  // Archive is set and exists, Folder is a temp folder
  if (Archive <> '') and FileExistsUTF8(Archive) and
    DirectoryExistsUTF8(Folder) then
    DeleteDirectory(Folder, False);

  FSummary.Free;
  FTitle.Free;

  inherited Destroy;
end;

initialization
  RegisterClass(caComictecaVolume);

finalization
  UnRegisterClass(caComictecaVolume);
end.
