unit mvLayersPropEditForm;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  CollectionPropEditForm, mvMapViewer, mvGpsObj, mvTypes;

type

  { TLayersPropertyEditForm }

  TLayersPropertyEditForm = class(TCollectionPropertyEditorForm)
    btnLoad: TBitBtn;
    btnSave: TBitBtn;
    Panel1: TPanel;
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure CollectionListBoxClick(Sender: TObject);
  private
    procedure UpdateVisuals;
    procedure Modified;
    procedure SaveToFile(AFileName: String; ALayer: TMapLayer);
    procedure LoadFromFile(AFileName: String; ALayer: TMapLayer);
  public
    procedure UpdateButtons;

  end;

var
  LayersPropertyEditForm: TLayersPropertyEditForm;

implementation

uses mvGPX, PropEdits;


{$R *.lfm}

{ TLayersPropertyEditForm }

procedure TLayersPropertyEditForm.CollectionListBoxClick(Sender: TObject);
begin
  inherited;
  UpdateVisuals;
end;

procedure TLayersPropertyEditForm.btnSaveClick(Sender: TObject);
var
  I: Integer;
  L: TMapLayer;
begin
  if not Assigned(Collection) then
    Exit;
  I := CollectionListBox.ItemIndex;
  L := Collection.Items[I] as TMapLayer;
  with TSaveDialog.Create(Nil) do
    try
      DefaultExt := '.gpx';
      Filter := 'GPX file|*.gpx|All files|*.*';
      Options := Options + [ofOverwritePrompt, ofEnableSizing];
      FileName := L.Caption;
      if Execute then
        SaveToFile(FileName, L);
    finally
      Free;
    end;
end;

procedure TLayersPropertyEditForm.btnLoadClick(Sender: TObject);
var
  I: Integer;
  L: TMapLayer;
begin
  if not Assigned(Collection) then
    Exit;
  I := CollectionListBox.ItemIndex;
  L := Collection.Items[I] as TMapLayer;
  with TOpenDialog.Create(Nil) do
    try
      DefaultExt := '.gpx';
      Filter := 'GPX file|*.gpx|All files|*.*';
      if Execute then
        LoadFromFile(FileName, L);
    finally
      Free;
    end;
end;

procedure TLayersPropertyEditForm.UpdateVisuals;
begin
  btnLoad.Enabled := (Collection <> Nil) {and btnLoad.Visible};
  btnSave.Enabled := (Collection <> Nil) {and btnSave.Visible};
end;

procedure TLayersPropertyEditForm.Modified;
begin
  if GlobalDesignHook <> nil then
    GlobalDesignHook.Modified(Self);
end;

procedure TLayersPropertyEditForm.SaveToFile(AFileName: String; ALayer: TMapLayer);
begin
  TGpxWriter.SaveToFile(AFileName, ALayer.ComboLayer);
end;

procedure TLayersPropertyEditForm.LoadFromFile(AFileName: String;
  ALayer: TMapLayer);
var
  List: TGpsObjectList;
  LBounds: TRealArea;
begin
  with TGpxReader.Create do
    try
      List := TGPSObjectList.Create;
      try
        LoadFromFile(AFileName, List, LBounds);
        ALayer.AssignFromGPSList(List);
      finally
        List.Free;
      end;
    finally
      Free;
    end;
  Modified;
end;

procedure TLayersPropertyEditForm.UpdateButtons;
begin
  inherited;  // Make it "public" in Laz 2.0.x
end;

end.

