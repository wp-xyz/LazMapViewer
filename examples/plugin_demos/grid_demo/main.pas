unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ComCtrls, ExtCtrls, StdCtrls, SysUtils,
  Forms, Controls, Graphics, Dialogs, //LazLogger,
  mvMapViewer, mvEngine, mvPluginCore, mvPlugins;

type
  TForm1 = class(TForm)
    cbCyclic: TCheckBox;
    cbEnabled: TCheckBox;
    cbLeft: TCheckBox;
    cbTop: TCheckBox;
    cbRight: TCheckBox;
    cbBottom: TCheckBox;
    clbBackgroundColor: TColorButton;
    clbPenColor: TColorButton;
    cmbIncrement: TComboBox;
    GroupBox1: TGroupBox;
    lblIncrement: TLabel;
    lblOpacity: TLabel;
    MapView: TMapView;
    PluginManager: TMvPluginManager;
    ParamsPanel: TPanel;
    tbOpacity: TTrackBar;
    procedure cbEnabledChange(Sender: TObject);
    procedure cbCyclicChange(Sender: TObject);
    procedure LabelPositionChange(Sender: TObject);
    procedure clbBackgroundColorColorChanged(Sender: TObject);
    procedure clbPenColorColorChanged(Sender: TObject);
    procedure cmbIncrementChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure tbOpacityChange(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  MapView.Zoom := 5;
  with TGridPlugin.Create(PluginManager) do
  begin
    clbBackgroundColor.ButtonColor := BackgroundColor;
    clbPenColor.ButtonColor := Pen.Color;
    tbOpacity.Position := round(Opacity * 100);
  end;
end;

procedure TForm1.cbCyclicChange(Sender: TObject);
begin
  MapView.Cyclic := cbCyclic.Checked;
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

procedure TForm1.cbEnabledChange(Sender: TObject);
begin
  (PluginManager.PluginList[0] as TGridPlugin).Enabled := cbEnabled.Checked;
end;

procedure TForm1.clbBackgroundColorColorChanged(Sender: TObject);
begin
  (PluginManager.PluginList[0] as TGridPlugin).BackgroundColor := clbBackgroundColor.ButtonColor;
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

procedure TForm1.tbOpacityChange(Sender: TObject);
begin
  (PluginManager.PluginList[0] as TGridPlugin).Opacity := tbOpacity.Position / 100;
end;

end.

