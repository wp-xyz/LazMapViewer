unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, DividerBevel, ExtCtrls, Spin, StdCtrls, SysUtils,
  Forms, Controls, Graphics, Dialogs, //LazLogger,
  mvMapViewer, mvTypes, mvEngine, mvPluginCore, mvPlugins;

type
  TForm1 = class(TForm)
    cbCyclic: TCheckBox;
    cbEnabled: TCheckBox;
    cbLeft: TCheckBox;
    cbTop: TCheckBox;
    cbRight: TCheckBox;
    cbBottom: TCheckBox;
    clbBackgroundColor: TColorButton;
    clbLabelTextColor: TColorButton;
    clbPenColor: TColorButton;
    cmbIncrement: TComboBox;
    divLines: TDividerBevel;
    divLabels: TDividerBevel;
    GroupBox1: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    lblLabelDistance: TLabel;
    lblIncrement: TLabel;
    lblOpacity: TLabel;
    MapView: TMapView;
    PluginManager: TMvPluginManager;
    ParamsPanel: TPanel;
    seLabelDistance: TSpinEdit;
    tbOpacity: TTrackBar;
    procedure cbEnabledChange(Sender: TObject);
    procedure cbCyclicChange(Sender: TObject);
    procedure clbLabelTextColorColorChanged(Sender: TObject);
    procedure LabelPositionChange(Sender: TObject);
    procedure clbBackgroundColorColorChanged(Sender: TObject);
    procedure clbPenColorColorChanged(Sender: TObject);
    procedure cmbIncrementChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure MapViewMouseUp(Sender: TObject; Button: TMouseButton; Shift:
      TShiftState; X, Y: Integer);
    procedure seLabelDistanceChange(Sender: TObject);
    procedure tbOpacityChange(Sender: TObject);
  private
    procedure AddPointOfInterest(X, Y: Integer);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Randomize;
  MapView.Zoom := 5;
  with TGridPlugin.Create(PluginManager) do
  begin
    clbBackgroundColor.ButtonColor := ColorToRGB(BackgroundColor);
    clbPenColor.ButtonColor := ColorToRGB(Pen.Color);
    clbLabelTextColor.ButtonColor := ColorToRGB(Font.Color);
    tbOpacity.Position := round(Opacity * 100);
  end;
end;

procedure TForm1.AddPointOfInterest(X, Y: Integer);
const
  DELTA = 4;
var
  layer: TMapLayer;
  poi: TPointOfInterest;
  RP: TRealPoint;
  area: TRealArea;
  list: TMapObjectList;
  i: Integer;
begin
  if MapView.Layers.Count = 0 then
    layer := TMapLayer(MapView.Layers.Add)
  else
    layer := MapView.Layers[0];

  RP := MapView.Engine.ScreenToLatLon(Point(X, Y));
  area := MapView.Engine.ScreenRectToRealArea(Rect(X-DELTA, Y-DELTA, X+DELTA, Y+DELTA));

  list := layer.PointsOfInterest.HitTest(area);
  if (list = nil) then
  begin
    poi := TPointOfInterest(layer.PointsOfInterest.Add);
    poi.Caption := 'Test ' + IntToStr(layer.PointsOfInterest.Count);
    poi.ImageIndex := Random(ImageList1.Count);
    poi.Longitude := RP.Lon;
    poi.Latitude := RP.Lat;
  end else
  begin
    for i := list.Count-1 downto 0 do
    begin
      if list[i] is TPointOfInterest then
      begin
        poi := TPointOfInterest(list[i]);
        poi.Free;
      end;
    end;
  end;
end;

procedure TForm1.cbCyclicChange(Sender: TObject);
begin
  MapView.Cyclic := cbCyclic.Checked;
end;

procedure TForm1.cbEnabledChange(Sender: TObject);
begin
  (PluginManager.PluginList[0] as TGridPlugin).Enabled := cbEnabled.Checked;
end;

procedure TForm1.clbBackgroundColorColorChanged(Sender: TObject);
begin
  (PluginManager.PluginList[0] as TGridPlugin).BackgroundColor := clbBackgroundColor.ButtonColor;
end;

procedure TForm1.clbLabelTextColorColorChanged(Sender: TObject);
begin
  (PluginManager.PluginList[0] as TGridPlugin).Font.Color := clbLabelTextColor.ButtonColor;
end;

procedure TForm1.clbPenColorColorChanged(Sender: TObject);
begin
  (PluginManager.PluginList[0] as TGridPlugin).Pen.Color := clbPenColor.ButtonColor;
end;

procedure TForm1.cmbIncrementChange(Sender: TObject);
var
  s: String;
  multiplier: Double;
  p: Integer;
begin
  if cmbIncrement.ItemIndex <= 0 then
    (PluginManager.PluginList[0] as TGridPlugin).Increment := 0
  else
  begin
    s := cmbIncrement.Items[cmbIncrement.ItemIndex];
    p := pos('°', s);
    if p > 0 then
      multiplier := 1.0
    else
    begin
      p := pos('''', s);
      if p > 0 then
        multiplier := 1.0/60
      else
      begin
        p := pos('"', s);
        if p > 0 then
          multiplier := 1.0/3600
        else
          exit;
      end;
    end;
    s := copy(s, 1, p-1);
    (PluginManager.PluginList[0] as TGridPlugin).Increment := StrToInt(s) * multiplier;
  end;
end;

procedure TForm1.LabelPositionChange(Sender: TObject);
begin
  with (PluginManager.PluginList[0] as TGridPlugin).GridLabels do
  begin
    if cbLeft.Checked then
      Position := Position + [glpLeft]
    else
      Position := Position - [glpLeft];

    if cbTop.Checked then
      Position := Position + [glpTop]
    else
      Position := Position - [glpTop];

    if cbRight.Checked then
      Position := Position + [glpRight]
    else
      Position := Position - [glpRight];

    if cbBottom.Checked then
      Position := Position + [glpBottom]
    else
      Position := Position - [glpBottom];
  end;
end;

procedure TForm1.MapViewMouseUp(Sender: TObject; Button: TMouseButton; Shift:
  TShiftState; X, Y: Integer);
begin
  if Button = mbRight then
    AddPointOfInterest(X, Y);
end;

procedure TForm1.seLabelDistanceChange(Sender: TObject);
begin
  (PluginManager.PluginList[0] as TGridPlugin).GridLabels.Distance := seLabelDistance.Value;
end;

procedure TForm1.tbOpacityChange(Sender: TObject);
begin
  (PluginManager.PluginList[0] as TGridPlugin).Opacity := tbOpacity.Position / 100;
end;

end.

