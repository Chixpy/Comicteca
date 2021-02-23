unit ufrComictecaVisorMain;

{< TfComictecaVisorMain form unit.

  This file is part of Comicteca Visor.

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ActnList, StdActns,
  Menus, FileUtil, LazFileUtils, LCLType, lclintf, LCLTranslator,
  BGRABitmapTypes, BGRABitmap,
  // Misc units
  uVersionSupport,
  // CHX units
  uCHXStrUtils, uCHX7zWrapper,
  // CHX forms
  ufrCHXForm,
  // CHX frames
  ufCHXBGRAImgViewerEx,
  // Comicteca Core units
  uCTKConst,
  // Comicteca abstracts
  uaComictecaPage,
  // Comicteca Core classes
  ucComictecaVolume, ucComictecaPage, ucComictecaVolumeRenderer;

type

  TCTKMode = (CTKMPage, CTKMFrame);

  { TfComictecaVisorMain }

  TfComictecaVisorMain = class(TfrmCHXForm)
    actFirst: TAction;
    actFlipR2L: TAction;
    actLast: TAction;
    actNext: TAction;
    actPrevious: TAction;
    alCTKVisor: TActionList;
    actFileOpen: TFileOpen;
    actExit: TFileExit;
    mimmFlipR2L: TMenuItem;
    mimmOptions: TMenuItem;
    mimmLast: TMenuItem;
    mimmNext: TMenuItem;
    mimmPrevious: TMenuItem;
    mimmFirst: TMenuItem;
    mimmGo: TMenuItem;
    mimmFileOpen: TMenuItem;
    pmMainMenu: TPopupMenu;
    procedure actFileOpenAccept(Sender: TObject);
    procedure actFirstExecute(Sender: TObject);
    procedure actFlipR2LExecute(Sender: TObject);
    procedure actLastExecute(Sender: TObject);
    procedure actNextExecute(Sender: TObject);
    procedure actPreviousExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
  private
    FBaseFolder: string;
    FComic: cComictecaVolume;
    FCurrentImage: TBGRABitmap;
    FCurrentPos: integer;
    FImgViewer: TfmCHXBGRAImgViewerEx;
    FMode: TCTKMode;
    FRenderer: cComictecaVolumeRenderer;
    procedure SetBaseFolder(AValue: string);
    procedure SetComic(AValue: cComictecaVolume);
    procedure SetCurrentImage(AValue: TBGRABitmap);
    procedure SetCurrentPos(AValue: integer);
    procedure SetImgViewer(AValue: TfmCHXBGRAImgViewerEx);
    procedure SetMode(AValue: TCTKMode);

  protected
    procedure LoadCurrFrame;
    procedure LoadCurrPage;

    procedure ShowCurrentImage;

  public

  published
    property Comic: cComictecaVolume read FComic write SetComic;
    property Renderer: cComictecaVolumeRenderer read FRenderer;

    property BaseFolder: string read FBaseFolder write SetBaseFolder;

    property Mode: TCTKMode read FMode write SetMode;
    property CurrentPos: integer read FCurrentPos write SetCurrentPos;

    property CurrentImage: TBGRABitmap
      read FCurrentImage write SetCurrentImage;

    property ImgViewer: TfmCHXBGRAImgViewerEx
      read FImgViewer write SetImgViewer;

  end;

var
  fComictecaVisorMain: TfComictecaVisorMain;

implementation

{$R *.lfm}

{ TfComictecaVisorMain }

procedure TfComictecaVisorMain.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  FreeAndNil(FComic);
  FreeAndNil(FCurrentImage);
  FreeAndNil(FRenderer);

  CanClose := True;
end;

procedure TfComictecaVisorMain.actFileOpenAccept(Sender: TObject);
begin
  FreeAndNil(FComic);

  FComic := cComictecaVolume.Create(nil);
  Comic.LoadFromArchive(actFileOpen.Dialog.FileName);

  Renderer.Comic := Comic;

  CurrentPos := 0;
end;

procedure TfComictecaVisorMain.actFirstExecute(Sender: TObject);
begin
  CurrentPos := 0;
end;

procedure TfComictecaVisorMain.actFlipR2LExecute(Sender: TObject);
begin
  Renderer.FlipL2R := actFlipR2L.Checked;
end;

procedure TfComictecaVisorMain.actLastExecute(Sender: TObject);
begin
  if not Assigned(Comic) then
    Exit;

  case Mode of
    CTKMPage:
    begin
      CurrentPos := Comic.Pages.Count - 1;
    end
    else // CTKMFrame;
      CurrentPos := Comic.Frames.Count - 1;
  end;
end;

procedure TfComictecaVisorMain.actNextExecute(Sender: TObject);
begin
  CurrentPos := CurrentPos + 1;
end;

procedure TfComictecaVisorMain.actPreviousExecute(Sender: TObject);
begin
  CurrentPos := CurrentPos - 1;
end;

procedure TfComictecaVisorMain.FormCreate(Sender: TObject);
begin
  // Title of application, usually it's autodeleted in .lpr file...
  Application.Title := Format(krsFmtApplicationTitle,
    [Application.Title, GetFileVersion]);

  // Changing base folder to parents exe folder.
  BaseFolder := ExtractFileDir(ExcludeTrailingPathDelimiter(ProgramDirectory));
  ChDir(BaseFolder);

  // Loading translation
  if not DirectoryExistsUTF8(BaseFolder + krsLocaleFolder) then
    MkDir(BaseFolder + krsLocaleFolder);
  SetDefaultLang('', BaseFolder + krsLocaleFolder);

  // Standard format setting (for .ini and other conversions)
  // This overrides user local settings which can cause errors.
  StandardFormatSettings;

  // Windows Caption
  Caption := Format(krsFmtWindowCaption, [Application.Title, Caption]);

  //// Loading GUI config
  //FEditorConfig := cCTKEditorConfig.Create(self);
  //EditorConfig.DefaultFileName := SetAsAbsoluteFile(krsCTKEditorIni, BaseFolder);
  //EditorConfig.LoadFromFile('');

  // Creating frames
  FImgViewer := TfmCHXBGRAImgViewerEx.Create(Self);
  ImgViewer.AutoZoomOnLoad := True;
  ImgViewer.sbxImage.Color := clBlack;
  ImgViewer.Align := alClient;
  ImgViewer.Parent := Self;

  w7zSetPathTo7zexe('Tools\7zip\7z.exe');
  w7zSetPathTo7zGexe('Tools\7zip\7zG.exe');

  actFileOpen.Enabled := w7zPathsOK;
  if actFileOpen.Enabled then
    actFileOpen.Dialog.Filter :=
      'Comic Files|' + FileMaskFromCommaText(kw7zFileExts);

  FRenderer := cComictecaVolumeRenderer.Create(nil);

  Mode := CTKMFrame;
  CurrentPos := -1;

  Renderer.FlipL2R := actFlipR2L.Checked;

  // Parse command line

end;

procedure TfComictecaVisorMain.SetComic(AValue: cComictecaVolume);
begin
  if FComic = AValue then
    Exit;
  FComic := AValue;
end;

procedure TfComictecaVisorMain.SetCurrentImage(AValue: TBGRABitmap);
begin
  if FCurrentImage = AValue then
    Exit;
  FCurrentImage := AValue;
end;

procedure TfComictecaVisorMain.SetCurrentPos(AValue: integer);
begin
  if not assigned(Comic) then
    FCurrentPos := -1
  else
  begin
    FCurrentPos := AValue;

    if FCurrentPos < 0 then
      FCurrentPos := 0;

    case Mode of
      CTKMPage:
      begin
        if FCurrentPos >= Comic.Pages.Count then
          FCurrentPos := Comic.Pages.Count - 1;
      end;
      else // CTKMFrame
      begin
        if FCurrentPos >= Comic.Frames.Count then
          FCurrentPos := Comic.Frames.Count - 1;
      end;
    end;
  end;

  ShowCurrentImage;
end;

procedure TfComictecaVisorMain.SetBaseFolder(AValue: string);
begin
  if FBaseFolder = AValue then
    Exit;
  FBaseFolder := AValue;
end;

procedure TfComictecaVisorMain.SetImgViewer(AValue: TfmCHXBGRAImgViewerEx);
begin
  if FImgViewer = AValue then
    Exit;
  FImgViewer := AValue;
end;

procedure TfComictecaVisorMain.SetMode(AValue: TCTKMode);
begin
  if FMode = AValue then
    Exit;
  FMode := AValue;
end;

procedure TfComictecaVisorMain.LoadCurrFrame;
begin
  CurrentImage := Renderer.RenderFrame(CurrentPos);
end;

procedure TfComictecaVisorMain.LoadCurrPage;
begin
  CurrentImage := Renderer.RenderPage(CurrentPos);
end;

procedure TfComictecaVisorMain.ShowCurrentImage;
begin
  FreeAndNil(FCurrentImage);
  ImgViewer.ActualImage := nil;

  case Mode of
    CTKMPage:
    begin
      LoadCurrPage;
    end;
    else // CTKMFrame
    begin
      LoadCurrFrame;
    end;
  end;

  ImgViewer.ActualImage := CurrentImage;
end;

end.
