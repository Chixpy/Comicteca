program ComictecaGUI;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ucComictecaPage, uaComictecaPage, uaComictecaSection,
  ucComictecaSection, uCHXConst, uCHXRscStr, uCHXImageUtils, uCHX7zWrapper,
  uCHXExecute, uCHXFileUtils, uaCHXStorable, ufrCTKGUIMain, uCTKGUIRscStr,
  uCTKGUIConst, ucCTKGUIConfig, uaComictecaVolume, ufCTKGUIMain,
  ufCTKGUIFileEditor, ucComicteca, ucComictecaVolume, ucComictecaVignette,
  ucComictecaConfig, ucComictecaVignetteList, ucComictecaPageList,
  ufCTKGUIPageEditor, ufCHXBGRAImgViewer;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmCTKGUIMain, frmCTKGUIMain);
  Application.Run;
end.

