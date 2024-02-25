program ComictecaGUI;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ucComictecaPage, uaComictecaPage, uaComictecaSection,
  ucComictecaSection, ufrCTKGUIMain, uCTKGUIRscStr, uCTKGUIConst,
  ucCTKGUIConfig, uaComictecaVolume, uaComictecaText, uaComictecaFrame,
  uaComictecaShapedImage, ufCTKGUIMain, ufCTKGUIFileEditor, ucComictecaVolume,
  ucComictecaVignette, ucComictecaConfig, ucComictecaPageList, uCHXStrUtils,
  uCHXConst, uCHXRscStr, uCHXImageUtils, uaCHXStorable, ufCTKGUIPageEditor;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmCTKGUIMain, frmCTKGUIMain);
  Application.Run;
end.

