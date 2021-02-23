unit ufCTKEditorFrameList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls,
  // Comicteca Core classes;
  ucComictecaFrame,
  // Comicteca Editor abstract frames
  uafCTKEditorFrame;

type

  { TfmCTKEditorFrameList }

  TfmCTKEditorFrameList = class(TafmCTKEditorFrame)
    bAddframe: TButton;
    bMoveDown: TButton;
    bRemoveFrame: TButton;
    bSubir: TButton;
    gbxFrameList: TGroupBox;
    lvFrameList: TListView;
    pFrameListButtons: TPanel;
    procedure bAddframeClick(Sender: TObject);
    procedure bMoveDownClick(Sender: TObject);
    procedure bRemoveFrameClick(Sender: TObject);
    procedure bSubirClick(Sender: TObject);
    procedure lvFrameListSelectItem(Sender: TObject; Item: TListItem;
      Selected: boolean);

  private
    FOnFrameSelect: TCTKFrameObjProc;
    procedure SetOnFrameSelect(AValue: TCTKFrameObjProc);

  protected

    procedure DoLoadFrameData;
    procedure DoClearFrameData;

  public
    property OnFrameSelect: TCTKFrameObjProc
      read FOnFrameSelect write SetOnFrameSelect;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.lfm}

{ TfmCTKEditorFrameList }

procedure TfmCTKEditorFrameList.bSubirClick(Sender: TObject);
begin
  if not assigned(Comic) then
    Exit;

  if lvFrameList.ItemIndex < 1 then
    Exit;

  Comic.Frames.Exchange(lvFrameList.ItemIndex, lvFrameList.ItemIndex - 1);
  lvFrameList.Items.Exchange(lvFrameList.ItemIndex, lvFrameList.ItemIndex - 1);
end;

procedure TfmCTKEditorFrameList.lvFrameListSelectItem(Sender: TObject;
  Item: TListItem; Selected: boolean);
begin
  if not assigned(OnFrameSelect) then
    Exit;
  if Selected then
    OnFrameSelect(cComictecaFrame(Item.Data))
  else
    OnFrameSelect(nil);
end;

procedure TfmCTKEditorFrameList.SetOnFrameSelect(AValue: TCTKFrameObjProc);
begin
  if FOnFrameSelect = AValue then
    Exit;
  FOnFrameSelect := AValue;
end;

procedure TfmCTKEditorFrameList.DoLoadFrameData;
var
  i: integer;
begin
  ClearFrameData;

  Enabled := Assigned(Comic);

  if not Enabled then
    Exit;

  i := 0;
  while (i < Comic.Frames.Count) do
  begin
    lvFrameList.AddItem(IntToStr(i), Comic.Frames[i]);
    Inc(i);
  end;
end;

procedure TfmCTKEditorFrameList.DoClearFrameData;
begin
  lvFrameList.Clear;
end;

constructor TfmCTKEditorFrameList.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);

  OnLoadFrameData := @DoLoadFrameData;
  OnClearFrameData := @DoClearFrameData;
end;

destructor TfmCTKEditorFrameList.Destroy;
begin
  inherited Destroy;
end;

procedure TfmCTKEditorFrameList.bMoveDownClick(Sender: TObject);
begin
  if not assigned(Comic) then
    Exit;

  if (lvFrameList.ItemIndex < 0) or
    (lvFrameList.ItemIndex > (lvFrameList.Items.Count - 2)) then
    Exit;

  Comic.Frames.Exchange(lvFrameList.ItemIndex, lvFrameList.ItemIndex + 1);
  lvFrameList.Items.Exchange(lvFrameList.ItemIndex, lvFrameList.ItemIndex + 1);
end;

procedure TfmCTKEditorFrameList.bAddframeClick(Sender: TObject);
var
  aFrame: cComictecaFrame;
  aPos: integer;
begin
  if not assigned(Comic) then
    Exit;

  aFrame := cComictecaFrame.Create(nil);
  aPos := Comic.Frames.Add(aFrame);
  lvFrameList.AddItem(IntToStr(aPos), aFrame);
  lvFrameList.ItemIndex := lvFrameList.ItemIndex + 1;
end;

procedure TfmCTKEditorFrameList.bRemoveFrameClick(Sender: TObject);
begin
  if not assigned(Comic) then
    Exit;

  if lvFrameList.ItemIndex < 0 then
    Exit;

  Comic.Frames.Delete(lvFrameList.ItemIndex);
  lvFrameList.Items.Delete(lvFrameList.ItemIndex);
end;

end.
