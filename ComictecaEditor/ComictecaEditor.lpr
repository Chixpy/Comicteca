program ComictecaEditor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, lazcontrols, ufrComictecaEditorMain, uCTKEditorConfig,
  uafCTKEditorFrame, ufCTKEditorMain, ufCTKEditorImgList, ufCTKEditorPageEdit,
  ufCTKEditorVolumeEdit, ufCTKEditorPageVisor, uCTKRstStr, uCTKCommon,
  uaComictecaFrame, ufCTKEditorFrameEdit, ucComictecaTextMap,
  ufCTKEditorFrameList, ucComictecaFrame, uafCTKEditorPageFrame,
  uafCTKEditorFrameFrame, ufCTKEditorFrameVisor, uaComictecaText,
  ucComictecaTextList, ucComictecaText, uCHXFileUtils, uCHXImageUtils,
  uCHXConst, uCHXRscStr, uCHXRecordHelpers, uCHX7zWrapper, uCHXExecute,
  uaCHXStorable, ufCHXBGRAImgViewerEx, ufCHXBGRAImgViewer, ufCTKEditorTextList,
  uafCTKEditorTextFrame, ufCTKEditorTextEdit, ufCTKEditorTextVisor;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Title := 'Comicteca Editor';
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfrmComictecaEditorMain, frmComictecaEditorMain);
  Application.Run;
end.

