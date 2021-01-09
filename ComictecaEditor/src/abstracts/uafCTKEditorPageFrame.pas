unit uafCTKEditorPageFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  uafCTKEditorFrame,
  ucComictecaPage;

type

  { TafmCTKEditorPageFrame }

  TafmCTKEditorPageFrame = class(TafmCTKEditorFrame)
  private
    FPage: cComictecaPage;
    procedure SetPage(AValue: cComictecaPage);

  public

  published
    property Page: cComictecaPage read FPage write SetPage;
  end;

implementation

{$R *.lfm}

{ TafmCTKEditorPageFrame }

procedure TafmCTKEditorPageFrame.SetPage(AValue: cComictecaPage);
begin
  if FPage = AValue then Exit;
  FPage := AValue;

  LoadFrameData;
end;

end.

