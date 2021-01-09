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
    procedure bMoveDownClick(Sender: TObject);
    procedure bRemoveFrameClick(Sender: TObject);
    procedure bSubirClick(Sender: TObject);
    procedure lvFrameListSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);

  private
    FOnFrameSelect: TCTKFrameObjProc;
    procedure SetOnFrameSelect(AValue: TCTKFrameObjProc);

      protected

    procedure DoLoadFrameData;
    procedure DoClearFrameData;

  public
        property OnFrameSelect: TCTKFrameObjProc read FOnFrameSelect write SetOnFrameSelect;

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
  Item: TListItem; Selected: Boolean);
begin
    if not assigned(OnPageSelect) then
    Exit;
  if Selected then
    OnPageSelect(cComictecaFrame(Item.Data))
  else
    OnPageSelect(nil);
end;

procedure TfmCTKEditorFrameList.SetOnFrameSelect(AValue: TCTKFrameObjProc);
begin
  if FOnFrameSelect = AValue then Exit;
  FOnFrameSelect := AValue;
end;

procedure TfmCTKEditorFrameList.DoLoadFrameData;
begin

end;

procedure TfmCTKEditorFrameList.DoClearFrameData;
begin
  lvFileList.Clear;
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

  if not lvFrameList.ItemIndex in [0..lvFrameList.Items.Count - 1] then
    Exit;

  Comic.Frames.Exchange(lvFrameList.ItemIndex, lvFrameList.ItemIndex + 1);
  lvFrameList.Items.Exchange(lvFrameList.ItemIndex, lvFrameList.ItemIndex + 1);
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

