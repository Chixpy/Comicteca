unit uafCTKEditorTextFrame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  // Comicteca Core classes
  ucComictecaText,
  // Comicteca Editor abstract frames
  uafCTKEditorFrame;

type

  { TafmCTKEditorTextFrame }

  TafmCTKEditorTextFrame = class(TafmCTKEditorFrame, IFPObserver)
  private
    FCTKText: cComictecaText;
    procedure SetCTKText(AValue: cComictecaText);

  protected
    procedure DoLoadTextFrame; virtual; abstract;
    procedure DoClearTextFrame; virtual; abstract;

    procedure FPOObservedChanged(ASender: TObject;
      Operation: TFPObservedOperation; Data: Pointer); virtual;

  public
    property CTKText: cComictecaText read FCTKText write SetCTKText;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TafmCTKEditorTextFrame }

procedure TafmCTKEditorTextFrame.SetCTKText(AValue: cComictecaText);
begin
  if FCTKText = AValue then
    Exit;

  if Assigned(FCTKText) then
  begin
    FCTKText.FPODetachObserver(Self);
  end;

  FCTKText := AValue;

  if Assigned(FCTKText) then
  begin
    FCTKText.FPOAttachObserver(Self);
  end;

  DoLoadTextFrame;
end;

procedure TafmCTKEditorTextFrame.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);
begin
  if CTKText = ASender then
  begin
    case Operation of
      ooChange: DoLoadTextFrame;
      ooFree: CTKText := nil;
      ooAddItem: ;
      ooDeleteItem: CTKText := nil;
      ooCustom: ;
    end;
  end;
end;

constructor TafmCTKEditorTextFrame.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TafmCTKEditorTextFrame.Destroy;
begin
  if Assigned(CTKText) then
    CTKText.FPODetachObserver(Self);

  inherited Destroy;
end;

end.
