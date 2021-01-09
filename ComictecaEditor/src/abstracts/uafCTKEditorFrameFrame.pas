unit uafCTKEditorFrameFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  ucComictecaFrame,
  uafCTKEditorFrame;

type

  { TafmCTKEditorFrameFrame }

  TafmCTKEditorFrameFrame = class(TafmCTKEditorFrame)
  private
    FFrame: cComictecaFrame;
    procedure SetFrame(AValue: cComictecaFrame);

  public
    property Frame: cComictecaFrame read FFrame write SetFrame;


  end;

implementation

{$R *.lfm}

{ TafmCTKEditorFrameFrame }

procedure TafmCTKEditorFrameFrame.SetFrame(AValue: cComictecaFrame);
begin
  if FFrame = AValue then Exit;
  FFrame := AValue;

  LoadFrameData;
end;

end.

