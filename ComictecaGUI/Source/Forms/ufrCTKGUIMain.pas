unit ufrCTKGUIMain;
{< TfrmCTKGUIMain form unit.

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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, FileUtil, LazFileUtils,
  LCLTranslator,
  // Misc units
  uVersionSupport,
  // CHX units
  uCHXStrUtils,
  // CHX forms
  ufrCHXForm,
  // Comiteca GUI units
  uCTKGUIConst,
  // Comiteca GUI classes
  ucCTKGUIConfig,
  // Comicteca frames
  ufCTKGUIMain;

type

  { TfrmCTKGUIMain }

  TfrmCTKGUIMain = class(TfrmCHXForm)
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);

  private
    FBaseFolder: string;
    FfmMain: TfmCTKGUIMain;
    FGUIConfig: cCTKGUIConfig;
    procedure SetBaseFolder(const AValue: string);
    procedure SetGUIConfig(const AValue: cCTKGUIConfig);

  protected
    property fmMain: TfmCTKGUIMain read FfmMain;

  public
    property BaseFolder: string read FBaseFolder write SetBaseFolder;
    property GUIConfig: cCTKGUIConfig read FGUIConfig write SetGUIConfig;

  end;

var
  frmCTKGUIMain: TfrmCTKGUIMain;

implementation

{$R *.lfm}

{ TfrmCTKGUIMain }

procedure TfrmCTKGUIMain.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := True;

  FGUIConfig.Free;
end;

procedure TfrmCTKGUIMain.FormCreate(Sender: TObject);

  procedure CreateFrames;
  begin
    FfmMain := TfmCTKGUIMain.Create(Self);
    fmMain.Align := alClient;
    fmMain.Parent := Self;
  end;

begin
    // Title of application, usually it's autodeleted in .lpr file...
  Application.Title := Format(krsFmtApplicationTitle,
    [Application.Title, GetFileVersion]);

  // Changing base folder to parents exe folder.
  BaseFolder := ExtractFileDir(ExcludeTrailingPathDelimiter(ProgramDirectory));
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
  FGUIConfig := cCTKGUIConfig.Create(self);
  GUIConfig.DefaultFileName := SetAsAbsoluteFile(krsGuiIni, BaseFolder);
  GUIConfig.LoadFromFile('');

  CreateFrames;
end;

procedure TfrmCTKGUIMain.SetBaseFolder(const AValue: string);
begin
  FBaseFolder := SetAsFolder(AValue);
end;

procedure TfrmCTKGUIMain.SetGUIConfig(const AValue: cCTKGUIConfig);
begin
  if FGUIConfig = AValue then Exit;
  FGUIConfig := AValue;
end;

end.

