unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Spin,
  mvMapViewer, mvMapProvider, mvPluginCore, mvMapScalePlugin;

type

  { TMainForm }

  TMainForm = class(TForm)
    cbScaleVisible: TCheckBox;
    gbZoomMin: TGroupBox;
    lblZoomMinInfo: TLabel;
    lblCurrentZoom: TLabel;
    MapView: TMapView;
    PluginManager: TMvPluginManager;
    ParamsPanel: TPanel;
    rgLengthUnits: TRadioGroup;
    rgScaleAlign: TRadioGroup;
    seZoomMin: TSpinEdit;
    procedure cbScaleVisibleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MapViewZoomChange(Sender: TObject);
    procedure rgLengthUnitsClick(Sender: TObject);
    procedure rgScaleAlignClick(Sender: TObject);
    procedure seZoomMinChange(Sender: TObject);
  private
    FScalePlugin: TMapScalePlugin;
    procedure UpdateZoomInfo;

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  P: TMapProvider;
  zoomMin, zoomMax: Integer;
begin
  MapView.Active := true;
  FScalePlugin := TMapScalePlugin.Create(PluginManager);

  P := MapView.Engine.MapProviderByName(MapView.MapProvider);
  P.GetZoomInfos(zoomMin, zoomMax);
  seZoomMin.MaxValue := zoomMax;
  seZoomMin.MinValue := zoomMin;
  seZoomMin.Value := FScalePlugin.ZoomMin;

  UpdateZoomInfo;
end;

procedure TMainForm.MapViewZoomChange(Sender: TObject);
begin
  UpdateZoomInfo;
end;

procedure TMainForm.cbScaleVisibleChange(Sender: TObject);
begin
  FScalePlugin.Enabled := cbScaleVisible.Checked;
end;

procedure TMainForm.rgLengthUnitsClick(Sender: TObject);
begin
  FScalePlugin.Imperial := rgLengthUnits.ItemIndex = 1;
end;

procedure TMainForm.rgScaleAlignClick(Sender: TObject);
var
  alignSet: TScaleAlignSet;
begin
  alignSet := [];
  case rgScaleAlign.ItemIndex of
    0: alignSet := [alLeft, alTop];
    1: alignSet := [alLeft, alRight, alTop];
    2: alignSet := [alRight, alTop];
    3: alignSet := [alLeft, alTop, alBottom];
    4: alignSet := [alLeft, alRight, alTop, alBottom];
    5: alignSet := [alRight, alTop, alBottom];
    6: alignSet := [alLeft, alBottom];
    7: alignSet := [alLeft, alRight, alBottom];
    8: alignSet := [alRight, alBottom];
  end;
  FScalePlugin.AlignSet := alignSet;
end;

procedure TMainForm.seZoomMinChange(Sender: TObject);
begin
  FScalePlugin.ZoomMin := seZoomMin.Value;
end;

procedure TMainForm.UpdateZoomInfo;
begin
  lblCurrentZoom.Caption := Format('Current zoom level %d', [MapView.Zoom]);
end;


end.

