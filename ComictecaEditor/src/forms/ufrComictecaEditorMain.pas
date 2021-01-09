unit ufrComictecaEditorMain;

{< TfrmComictecaEditorMain form unit.

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, FileUtil,
  LazFileUtils,
  LCLTranslator, ExtCtrls, StdCtrls, EditBtn, ComCtrls,
  // Miscelaneous unit
  uVersionSupport,
  // CHX units
  uCHXStrUtils,
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
    bSaveArchive: TButton;
    eComicFolder: TDirectoryEdit;
    eCompressedFile: TFileNameEdit;
    pArchive: TPanel;
    pcComicFileType: TPageControl;
    pagFolder: TTabSheet;
    pagCompressedArchive: TTabSheet;
    pEditorFrame: TPanel;
    procedure bSaveArchiveClick(Sender: TObject);
    procedure eComicFolderAcceptDirectory(Sender: TObject; var Value: string);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);

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

  Comic.Free;

  FComic := cComictecaVolume.Create(nil);

  Comic.LoadFromFolder(Value);

  fmMain.Comic := Self.Comic;
end;

procedure TfrmComictecaEditorMain.bSaveArchiveClick(Sender: TObject);
begin
  if not Assigned(Comic) then
    Exit;

  Comic.SaveToFolder;
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

  FComic := nil;
end;

destructor TfrmComictecaEditorMain.Destroy;
begin
  inherited Destroy;

  Comic.Free;
end;

end.
