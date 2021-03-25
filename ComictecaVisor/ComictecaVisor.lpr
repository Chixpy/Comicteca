program ComictecaVisor;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, ufCHXBGRAImgViewer, ufCHXFrame, uCHXStrUtils, uCHXConst, uCHXRscStr,
  uCHX7zWrapper, uCHXExecute, uCHXFileUtils, ufrComictecaVisorMain
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled := True;
  Application.Initialize;
  Application.CreateForm(TfComictecaVisorMain, fComictecaVisorMain);
  Application.Run;
end.

