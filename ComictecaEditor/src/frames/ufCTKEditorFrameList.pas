unit ufCTKEditorFrameList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls,
  // CHX units
  uCHXRecordHelpers,
  // Comicteca Core classes;
  ucComictecaFrame,
  // Comicteca Editor abstract frames
  uafCTKEditorFrame;

type

  { TfmCTKEditorFrameList }

  TfmCTKEditorFrameList = class(TafmCTKEditorFrame)
    bAddFrame: TButton;
    bMoveDown: TButton;
    bRemoveFrame: TButton;
    bSubir: TButton;
    gbxFrameList: TGroupBox;
    lbxFrameList: TListBox;
    pFrameListButtons: TPanel;
    procedure bAddFrameClick(Sender: TObject);
    procedure bMoveDownClick(Sender: TObject);
    procedure bRemoveFrameClick(Sender: TObject);
    procedure bSubirClick(Sender: TObject);
    procedure lbxFrameListSelectionChange(Sender: TObject; User: boolean);

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

  if lbxFrameList.ItemIndex < 1 then
    Exit;

  Comic.Frames.Exchange(lbxFrameList.ItemIndex, lbxFrameList.ItemIndex - 1);
  lbxFrameList.Items.Exchange(lbxFrameList.ItemIndex,
    lbxFrameList.ItemIndex - 1);

  lbxFrameList.ItemIndex := lbxFrameList.ItemIndex - 1;
end;

procedure TfmCTKEditorFrameList.lbxFrameListSelectionChange(Sender: TObject;
  User: boolean);
var
  aFrame: cComictecaFrame;
begin
  if lbxFrameList.ItemIndex < 0 then
    aFrame := nil
  else
    aFrame := cComictecaFrame(
      lbxFrameList.Items.Objects[lbxFrameList.ItemIndex]);

  if Assigned(OnFrameSelect) then
    OnFrameSelect(aFrame);
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
  aCaption: string;
begin
  ClearFrameData;

  Enabled := Assigned(Comic);

  if not Enabled then
    Exit;

  i := 0;
  while (i < Comic.Frames.Count) do
  begin
    aCaption := Comic.Frames[i].Page.FileName;
    if not Comic.Frames[i].ImgRect.IsEmpty then
      aCaption := aCaption + ' - ' + Comic.Frames[i].ImgRect.ToString;

    lbxFrameList.AddItem(aCaption, Comic.Frames[i]);
    Inc(i);
  end;
end;

procedure TfmCTKEditorFrameList.DoClearFrameData;
begin
  lbxFrameList.Clear;
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

  if (lbxFrameList.ItemIndex < 0) or (lbxFrameList.ItemIndex >
    (lbxFrameList.Items.Count - 2)) then
    Exit;

  Comic.Frames.Exchange(lbxFrameList.ItemIndex, lbxFrameList.ItemIndex + 1);
  lbxFrameList.Items.Exchange(lbxFrameList.ItemIndex,
    lbxFrameList.ItemIndex + 1);

  lbxFrameList.ItemIndex := lbxFrameList.ItemIndex + 1;
end;

procedure TfmCTKEditorFrameList.bAddFrameClick(Sender: TObject);
var
  aFrame: cComictecaFrame;
  aPos: integer;
begin
  if not assigned(Comic) then
    Exit;

  aPos := lbxFrameList.ItemIndex + 1;

  aFrame := cComictecaFrame.Create(nil);

  if (aPos < 1) or (aPos >= lbxFrameList.Count) then
  begin
    aPos := Comic.Frames.Add(aFrame);
    lbxFrameList.AddItem(IntToStr(aPos), aFrame);
  end
  else
  begin
    Comic.Frames.Insert(aPos, aFrame);
    lbxFrameList.Items.Insert(aPos, IntToStr(aPos));
    lbxFrameList.Items.Objects[aPos] := aFrame;
  end;

  lbxFrameList.ItemIndex := aPos;
end;

procedure TfmCTKEditorFrameList.bRemoveFrameClick(Sender: TObject);
var
  aPos: integer;
begin
  if not assigned(Comic) then
    Exit;

  if lbxFrameList.ItemIndex < 0 then
    Exit;

  aPos := lbxFrameList.ItemIndex;

  Comic.Frames.Delete(aPos);
  lbxFrameList.Items.Delete(aPos);

  if lbxFrameList.Count = 0 then
    OnFrameSelect(nil)
  else
  begin
    if aPos >= lbxFrameList.Count then
      lbxFrameList.ItemIndex := lbxFrameList.Count - 1
    else
      lbxFrameList.ItemIndex := aPos;
  end;
end;

end.
