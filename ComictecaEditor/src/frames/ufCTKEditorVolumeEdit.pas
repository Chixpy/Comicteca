unit ufCTKEditorVolumeEdit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, LazFileUtils,
  // CHX units
  uCHXStrUtils,
  // Comicteca Editor units
  uCTKEditorConst,
  //
  uafCTKEditorFrame;

type

  { TfmCTKEditorVolumeEdit }

  TfmCTKEditorVolumeEdit = class(TafmCTKEditorFrame)
    cbxLangTitle: TComboBox;
    cbxLangSummary: TComboBox;
    cbxSerieTitle: TComboBox;
    chkRight2Left: TCheckBox;
    eFanSuber: TEdit;
    eTitle: TEdit;
    cbxEditor: TComboBox;
    cbxLanguage: TComboBox;
    lFanSub: TLabel;
    lSummary: TLabel;
    mSummary: TMemo;
    pLanguaje: TPanel;
    pSummary: TPanel;
    seSerieOrder: TSpinEdit;
    cbxPublisher: TComboBox;
    seTitleOrder: TSpinEdit;
    lEditor: TLabel;
    lLanguage: TLabel;
    lPublisher: TLabel;
    lSerie: TLabel;
    lTitle: TLabel;
    lSerieOrder: TLabel;
    lTitleOrder: TLabel;
    pTitle: TPanel;
    procedure chkRight2LeftEditingDone(Sender: TObject);
    procedure eFanSuberEditingDone(Sender: TObject);
    procedure eTitleEditingDone(Sender: TObject);
    procedure cbxEditorEditingDone(Sender: TObject);
    procedure cbxPublisherEditingDone(Sender: TObject);
    procedure mSummaryEditingDone(Sender: TObject);
    procedure seSerieOrderEditingDone(Sender: TObject);
    procedure cbxSerieTitleEditingDone(Sender: TObject);
    procedure seTitleOrderEditingDone(Sender: TObject);
    procedure cbxLanguageEditingDone(Sender: TObject);

    procedure UpdateLangCBX(Sender: TObject);

  private
    FDataFolder: string;
    procedure SetDataFolder(AValue: string);

  protected

    procedure LoadLists;
    procedure SaveLists;

    procedure LoadFrameData; override;
    procedure ClearFrameData; override;

  public
    property DataFolder: string read FDataFolder write SetDataFolder;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

  end;

implementation

{$R *.lfm}

{ TfmCTKEditorVolumeEdit }

procedure TfmCTKEditorVolumeEdit.cbxSerieTitleEditingDone(Sender: TObject);
begin
  if not Assigned(Comic) then
    Exit;

  Comic.Serie := cbxSerieTitle.Text;

  if cbxSerieTitle.Items.IndexOf(cbxSerieTitle.Text) = -1 then
    cbxSerieTitle.Items.Add(cbxSerieTitle.Text);
end;

procedure TfmCTKEditorVolumeEdit.chkRight2LeftEditingDone(Sender: TObject);
begin
  if not Assigned(Comic) then
    Exit;

  Comic.Right2Left := chkRight2Left.Checked;
end;

procedure TfmCTKEditorVolumeEdit.eFanSuberEditingDone(Sender: TObject);
begin
  if not Assigned(Comic) then
    Exit;

  Comic.FanSuber := eFanSuber.Text;
end;


procedure TfmCTKEditorVolumeEdit.eTitleEditingDone(Sender: TObject);
begin
  if not Assigned(Comic) then
    Exit;

  Comic.Title.CTMSetStr(cbxLangTitle.Text, eTitle.Text);
end;

procedure TfmCTKEditorVolumeEdit.cbxEditorEditingDone(Sender: TObject);
begin
  if not Assigned(Comic) then
    Exit;

  Comic.Editor := cbxEditor.Text;

  if cbxEditor.Items.IndexOf(cbxEditor.Text) = -1 then
    cbxEditor.Items.Add(cbxEditor.Text);
end;

procedure TfmCTKEditorVolumeEdit.cbxLanguageEditingDone(Sender: TObject);
begin
  if not Assigned(Comic) then
    Exit;

  Comic.Language := cbxLanguage.Text;

  UpdateLangCBX(Sender);
end;

procedure TfmCTKEditorVolumeEdit.cbxPublisherEditingDone(Sender: TObject);
begin
  if not Assigned(Comic) then
    Exit;

  Comic.Publisher := cbxPublisher.Text;

  if cbxPublisher.Items.IndexOf(cbxPublisher.Text) = -1 then
    cbxPublisher.Items.Add(cbxPublisher.Text);
end;

procedure TfmCTKEditorVolumeEdit.mSummaryEditingDone(Sender: TObject);
begin
  if not Assigned(Comic) then
    Exit;

  Comic.Summary.CTMSetSL(cbxLangSummary.Text, mSummary.Lines);
end;

procedure TfmCTKEditorVolumeEdit.seSerieOrderEditingDone(Sender: TObject);
begin
  if not Assigned(Comic) then
    Exit;

  Comic.SerieOrder := seSerieOrder.Value;
end;

procedure TfmCTKEditorVolumeEdit.seTitleOrderEditingDone(Sender: TObject);
begin
  if not Assigned(Comic) then
    Exit;

  Comic.TitleOrder := seTitleOrder.Value;
end;

procedure TfmCTKEditorVolumeEdit.SetDataFolder(AValue: string);
begin
  FDataFolder := SetAsFolder(AValue);

  LoadLists;
end;

procedure TfmCTKEditorVolumeEdit.LoadLists;
var
  SLLanguages: TStringList;
begin
  if not DirectoryExistsUTF8(DataFolder) then
    Exit;

  if FileExistsUTF8(DataFolder + kSeriesCBFile) then
    cbxSerieTitle.Items.LoadFromFile(DataFolder + kSeriesCBFile);

  if FileExistsUTF8(DataFolder + kPublishersCBFile) then
    cbxPublisher.Items.LoadFromFile(DataFolder + kPublishersCBFile);

  if FileExistsUTF8(DataFolder + kEditorsCBFile) then
    cbxEditor.Items.LoadFromFile(DataFolder + kEditorsCBFile);

  if FileExistsUTF8(DataFolder + kLanguageCBFile) then
  begin
    try
      SLLanguages := TStringList.Create;
      SLLanguages.LoadFromFile(DataFolder + kLanguageCBFile);

      cbxLanguage.Items.Assign(SLLanguages);
      cbxLangTitle.Items.Assign(SLLanguages);
      cbxLangSummary.Items.Assign(SLLanguages);
    finally
      SLLanguages.Free;
    end;
  end;
end;

procedure TfmCTKEditorVolumeEdit.SaveLists;
begin
  if not DirectoryExistsUTF8(DataFolder) then
    Exit;

  cbxSerieTitle.Items.SaveToFile(DataFolder + kSeriesCBFile);
  cbxPublisher.Items.SaveToFile(DataFolder + kPublishersCBFile);
  cbxEditor.Items.SaveToFile(DataFolder + kEditorsCBFile);
  cbxLanguage.Items.SaveToFile(DataFolder + kLanguageCBFile);
end;

procedure TfmCTKEditorVolumeEdit.UpdateLangCBX(Sender: TObject);
var
  CBX: TCustomComboBox;
begin
  if not (Sender is TCustomComboBox) then
    Exit;

  CBX := TCustomComboBox(Sender);

  if CBX.Items.IndexOf(CBX.Text) = -1 then
  begin
    cbxLanguage.Items.Add(CBX.Text);
    cbxLangTitle.Items.Add(CBX.Text);
    cbxLangSummary.Items.Add(CBX.Text);
  end;

  LoadFrameData;
end;

procedure TfmCTKEditorVolumeEdit.LoadFrameData;
var
  StrLst: TStringList;
begin
  inherited;

  Enabled := Assigned(Comic);

  if not Enabled then
  begin
    ClearFrameData;
    Exit;
  end;

  cbxSerieTitle.Text := Comic.Serie;
  seSerieOrder.Value := Comic.SerieOrder;
  eTitle.Text := Comic.Title.CTMGetStr(cbxLangTitle.Text);
  seTitleOrder.Value := Comic.TitleOrder;
  cbxPublisher.Text := Comic.Publisher;
  cbxEditor.Text := Comic.Editor;
  cbxLanguage.Text := Comic.Language;
  eFanSuber.Text := Comic.FanSuber;
  chkRight2Left.Checked := Comic.Right2Left;

  // Nil can't be assigned to TStringList...
  StrLst := Comic.Summary.CTMGetSL(cbxLangSummary.Text);
  if assigned(StrLst) then
    mSummary.Lines.Assign(StrLst)
  else
    mSummary.Clear;
end;

procedure TfmCTKEditorVolumeEdit.ClearFrameData;
begin
  inherited;

  cbxSerieTitle.Text := EmptyStr;
  seSerieOrder.Value := -1;
  cbxLangTitle.Text := EmptyStr;
  eTitle.Text := EmptyStr;
  seTitleOrder.Value := -1;
  cbxPublisher.Text := EmptyStr;
  cbxEditor.Text := EmptyStr;
  cbxLanguage.Text := EmptyStr;
  eFanSuber.Text := EmptyStr;
  chkRight2Left.Checked := False;
  cbxLangSummary.Text := EmptyStr;
  mSummary.Clear;
end;

constructor TfmCTKEditorVolumeEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
end;

destructor TfmCTKEditorVolumeEdit.Destroy;
begin
  SaveLists;

  inherited Destroy;
end;

end.
