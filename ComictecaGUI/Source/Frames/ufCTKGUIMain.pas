unit ufCTKGUIMain;

{< TfmCTKGUIMain frame unit.

  This file is part of Comicteca GUI.

  Copyright (C) 2019 Chixpy

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  EditBtn, StdCtrls, ShellCtrls, LazFileUtils,
  // CHX frames
  ufCHXFrame,
  // Comicteca GUI frames
  ufCTKGUIFileEditor,
  // Comicteca Core classes
  ucComictecaVolume;

type

  { TfmCTKGUIMain }

  TfmCTKGUIMain = class(TfmCHXFrame)
    chkCompressedFile: TCheckBox;
    eComicFile: TFileNameEdit;
    eComicFolder: TDirectoryEdit;
    gbxFile: TGroupBox;
    gbxImageFiles: TGroupBox;
    lComicFolder: TLabel;
    lCompresedFile: TLabel;
    pcMain: TPageControl;
    pFileEditor: TPanel;
    pFilePreview: TPanel;
    pgComicFile: TTabSheet;
    sbxFileEditor: TScrollBox;
    slvImageFiles: TShellListView;
    Splitter1: TSplitter;
    pgComicPage: TTabSheet;
    Splitter2: TSplitter;
    procedure chkCompressedFileChange(Sender: TObject);
    procedure eComicFileAcceptFileName(Sender: TObject; var Value: String);
    procedure eComicFolderAcceptDirectory(Sender: TObject; var Value: String);

  private
    FCurrentFile: cComictecaVolume;
    FfmFileEditor: TfmCTKGUIFileEditor;
    procedure SetCurrentFile(const AValue: cComictecaVolume);

  protected
    property fmFileEditor: TfmCTKGUIFileEditor read FfmFileEditor;

  public

    property CurrentFile: cComictecaVolume read FCurrentFile write SetCurrentFile;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCTKGUIMain }

procedure TfmCTKGUIMain.eComicFileAcceptFileName(Sender: TObject;
  var Value: String);
begin
  FreeAndNil(FCurrentFile);

  ShowMessage('Extraer de fichero no implementado');
end;

procedure TfmCTKGUIMain.eComicFolderAcceptDirectory(Sender: TObject;
  var Value: String);
begin
  slvImageFiles.Clear;

  if not DirectoryExistsUTF8(Value) then Exit;

  slvImageFiles.Root := Value;
end;

procedure TfmCTKGUIMain.chkCompressedFileChange(Sender: TObject);
begin
  lCompresedFile.Enabled := chkCompressedFile.Checked;
  eComicFile.Enabled := chkCompressedFile.Checked;
end;


procedure TfmCTKGUIMain.SetCurrentFile(const AValue: cComictecaVolume);
begin
  if FCurrentFile = AValue then Exit;
  FCurrentFile := AValue;
end;

constructor TfmCTKGUIMain.Create(TheOwner: TComponent);

  procedure CreateFrames;
  begin
    FfmFileEditor := TfmCTKGUIFileEditor.Create(sbxFileEditor);
    fmFileEditor.Align := alClient;
    fmFileEditor.Parent := sbxFileEditor;
  end;

begin
  inherited Create(TheOwner);

  CreateFrames;

  Enabled := True;

  slvImageFiles.Mask := GraphicFileMask(TGraphic);
end;

destructor TfmCTKGUIMain.Destroy;
begin
  inherited Destroy;
end;

end.
