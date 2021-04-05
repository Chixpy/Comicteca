unit uafCTKEditorPageFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  // Comicteca Core classes
  ucComictecaPage,
  // Comicteca Editor abstract frames
  uafCTKEditorFrame;

type

  { TafmCTKEditorPageFrame }

  TafmCTKEditorPageFrame = class(TafmCTKEditorFrame, IFPObserver)
  private
    FPage: cComictecaPage;
    procedure SetPage(AValue: cComictecaPage);

  protected
    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer); virtual;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  published
    property Page: cComictecaPage read FPage write SetPage;

  end;

implementation

{$R *.lfm}

{ TafmCTKEditorPageFrame }

procedure TafmCTKEditorPageFrame.SetPage(AValue: cComictecaPage);
begin
  if FPage = AValue then
    Exit;

  if Assigned(FPage) then
  begin
    FPage.FPODetachObserver(Self);
  end;

  FPage := AValue;

  if Assigned(FPage) then
  begin
    FPage.FPOAttachObserver(Self);
  end;

  LoadFrameData;
end;

procedure TafmCTKEditorPageFrame.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if Page = ASender then
  begin
    case Operation of
      ooChange: LoadFrameData;
      ooFree: Page := nil;
      ooAddItem: ;
      ooDeleteItem: Page := nil;
      ooCustom: ;
    end;
  end;
end;

constructor TafmCTKEditorPageFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TafmCTKEditorPageFrame.Destroy;
begin
  if Assigned(Page) then
    Page.FPODetachObserver(Self);

  inherited Destroy;
end;

end.
