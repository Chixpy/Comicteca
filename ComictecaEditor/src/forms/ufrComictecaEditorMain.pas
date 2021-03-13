unit ufrComictecaEditorMain;

{< TfrmComictecaEditorMain form unit.

  This file is part of Comicteca Editor.

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, FileUtil,
  LazFileUtils, lclintf,
  LCLTranslator, ExtCtrls, StdCtrls, EditBtn, ComCtrls,
  // Miscelaneous unit
  uVersionSupport,
  // CHX units
  uCHXStrUtils, uCHX7zWrapper,
  // CHX forms
  ufrCHXForm,
  // Comicteca Core units
  uCTKConst,
  // Comicteca Core classes
  ucComictecaVolume,
  // Comicteca Editor units
  uCTKEditorConst,
  // Comicteca Editor classes
  uCTKEditorConfig,
  // Comicteca Editor frames
  ufCTKEditorMain;

type

  { TfrmComictecaEditorMain }

  TfrmComictecaEditorMain = class(TfrmCHXForm)
    bOpenArchiveFolder: TButton;
    bSaveArchive: TButton;
    bSaveFolder: TButton;
    eComicFolder: TDirectoryEdit;
    eCompressedFile: TFileNameEdit;
    pArchive: TPanel;
    pcComicFileType: TPageControl;
    pagFolder: TTabSheet;
    pagCompressedArchive: TTabSheet;
    pEditorFrame: TPanel;
    tOpenFolder: TButton;
    procedure bOpenArchiveFolderClick(Sender: TObject);
    procedure bSaveArchiveClick(Sender: TObject);
    procedure bSaveFolderClick(Sender: TObject);
    procedure eComicFolderAcceptDirectory(Sender: TObject; var Value: string);
    procedure eCompressedFileAcceptFileName(Sender: TObject; var Value: String);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure tOpenFolderClick(Sender: TObject);

  private
    FBaseFolder: string;
    FComic: cComictecaVolume;
    FEditorConfig: cCTKEditorConfig;
    FfmMain: TfmCTKEditorMain;
    procedure SetBaseFolder(AValue: string);
    procedure SetComic(AValue: cComictecaVolume);

  protected
    property fmMain: TfmCTKEditorMain read FfmMain;

  public
    property BaseFolder: string read FBaseFolder write SetBaseFolder;
    property EditorConfig: cCTKEditorConfig read FEditorConfig;

    property Comic: cComictecaVolume read FComic write SetComic;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  frmComictecaEditorMain: TfrmComictecaEditorMain;

implementation

{$R *.lfm}

{ TfrmComictecaEditorMain }

procedure TfrmComictecaEditorMain.FormCloseQuery(Sender: TObject;
  var CanClose: boolean);
begin
  CanClose := True;

  Comic.Free;

  FEditorConfig.Free;
end;

procedure TfrmComictecaEditorMain.eComicFolderAcceptDirectory(Sender: TObject;
  var Value: string);
begin
  if not DirectoryExistsUTF8(Value) then
  begin
    Value := '';
    Exit;
  end;

  FreeAndNil(FComic);

  fmMain.Comic := nil;

  FComic := cComictecaVolume.Create(nil);

  Comic.LoadFromFolder(Value);

  fmMain.Comic := Self.Comic;
end;

procedure TfrmComictecaEditorMain.eCompressedFileAcceptFileName(
  Sender: TObject; var Value: String);
begin
  if not FileExistsUTF8(Value) then
  begin
    Value := '';
    Exit;
  end;

  FreeAndNil(FComic);

  fmMain.Comic := nil;

  FComic := cComictecaVolume.Create(nil);

  Comic.LoadFromArchive(Value);

  if Comic.Folder = '' then
  begin
    eComicFolder.Text := '';
    FreeAndNil(FComic);
  end
  else
  begin
    eComicFolder.Text := SysPath(Comic.Folder);
  end;

  fmMain.Comic := Self.Comic;
end;

procedure TfrmComictecaEditorMain.bSaveFolderClick(Sender: TObject);
begin
  if not Assigned(Comic) then
    Exit;

  Comic.SaveToFolder;
end;

procedure TfrmComictecaEditorMain.bOpenArchiveFolderClick(Sender: TObject);
begin
  if not Assigned(Comic) then
    Exit;

  OpenDocument(Comic.Folder);
end;

procedure TfrmComictecaEditorMain.bSaveArchiveClick(Sender: TObject);
begin
  if not Assigned(Comic) then
    Exit;

  Comic.SaveToArchive;

  // if not a zip or 7z, file extension will be change to cbz
  eCompressedFile.Text := Comic.Archive;
end;

procedure TfrmComictecaEditorMain.FormCreate(Sender: TObject);

  procedure CreateFrames;
  begin
    FfmMain := TfmCTKEditorMain.Create(pEditorFrame);
    fmMain.Align := alClient;
    fmMain.Parent := pEditorFrame;
  end;

begin
  // Title of application, usually it's autodeleted in .lpr file...
  Application.Title := Format(krsFmtApplicationTitle,
    [Application.Title, GetFileVersion]);

  // Changing base folder to parents exe folder.
  BaseFolder := ExtractFileDir(ExcludeTrailingPathDelimiter(
    ProgramDirectory));
  ChDir(BaseFolder);

  // Loading translation
  if not DirectoryExistsUTF8(BaseFolder + krsLocaleFolder) then
    mkdir(BaseFolder + krsLocaleFolder);
  SetDefaultLang('', BaseFolder + krsLocaleFolder);

  // Standard format setting (for .ini and other conversions)
  // This overrides user local settings which can cause errors.
  StandardFormatSettings;

  // Windows Caption
  Caption := Format(krsFmtWindowCaption, [Application.Title, Caption]);

  // Loading GUI config
  FEditorConfig := cCTKEditorConfig.Create(self);
  EditorConfig.DefaultFileName :=
    SetAsAbsoluteFile(krsCTKEditorIni, BaseFolder);
  EditorConfig.LoadFromFile('');

  CreateFrames;

  w7zSetPathTo7zexe('Tools\7zip\7z.exe');
  w7zSetPathTo7zGexe('Tools\7zip\7zG.exe');

  pagCompressedArchive.Enabled := w7zPathsOK;

  if pagCompressedArchive.Enabled then
  begin
    pcComicFileType.ActivePage := pagCompressedArchive;
    eCompressedFile.Filter := 'Comic Files|' + FileMaskFromCommaText(kw7zFileExts);
  end;
end;

procedure TfrmComictecaEditorMain.tOpenFolderClick(Sender: TObject);
begin
  if not Assigned(Comic) then
    Exit;

  OpenDocument(Comic.Folder);
end;

procedure TfrmComictecaEditorMain.SetBaseFolder(AValue: string);
begin
  FBaseFolder := SetAsFolder(AValue);
end;

procedure TfrmComictecaEditorMain.SetComic(AValue: cComictecaVolume);
begin
  if FComic = AValue then
    Exit;
  FComic := AValue;
end;

constructor TfrmComictecaEditorMain.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfrmComictecaEditorMain.Destroy;
begin
  inherited Destroy;
end;

end.
