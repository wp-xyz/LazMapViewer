unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, Spin,
  mvMapViewer, mvMapProvider, mvPluginCore, mvMapScalePlugin;

type

  { TForm1 }

  TForm1 = class(TForm)
    cbScaleVisible: TCheckBox;
    gbZoomMin: TGroupBox;
    Label1: TLabel;
    lblCurrentZoom: TLabel;
    MapView1: TMapView;
    MvPluginManager1: TMvPluginManager;
    Panel1: TPanel;
    rgLengthUnits: TRadioGroup;
    rgScaleAlign: TRadioGroup;
    seZoomMin: TSpinEdit;
    procedure cbScaleVisibleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MapView1ZoomChange(Sender: TObject);
    procedure rgLengthUnitsClick(Sender: TObject);
    procedure rgScaleAlignClick(Sender: TObject);
    procedure seZoomMinChange(Sender: TObject);
  private
    FScalePlugin: TMapScalePlugin;
    procedure UpdateZoomInfo;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  P: TMapProvider;
  zoomMin, zoomMax: Integer;
begin
  FScalePlugin := TMapScalePlugin.Create(MvPluginManager1);

  P := MapView1.Engine.MapProviderByName(MapView1.MapProvider);
  P.GetZoomInfos(zoomMin, zoomMax);
  seZoomMin.MaxValue := zoomMax;
  seZoomMin.MinValue := zoomMin;
  seZoomMin.Value := FScalePlugin.ZoomMin;

  UpdateZoomInfo;
end;

procedure TForm1.MapView1ZoomChange(Sender: TObject);
begin
  UpdateZoomInfo;
end;

procedure TForm1.cbScaleVisibleChange(Sender: TObject);
begin
  FScalePlugin.Enabled := cbScaleVisible.Checked;
end;

procedure TForm1.rgLengthUnitsClick(Sender: TObject);
begin
  FScalePlugin.Imperial := rgLengthUnits.ItemIndex = 1;
end;

procedure TForm1.rgScaleAlignClick(Sender: TObject);
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

procedure TForm1.seZoomMinChange(Sender: TObject);
begin
  FScalePlugin.ZoomMin := seZoomMin.Value;
end;

procedure TForm1.UpdateZoomInfo;
begin
  lblCurrentZoom.Caption := Format('Current zoom level %d', [MapView1.Zoom]);
end;


end.

