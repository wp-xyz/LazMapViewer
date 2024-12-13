unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  mvMapViewer, mvPluginCore, mvMapScalePlugin;

type

  { TForm1 }

  TForm1 = class(TForm)
    cbScaleVisible: TCheckBox;
    MapView1: TMapView;
    MvPluginManager1: TMvPluginManager;
    Panel1: TPanel;
    rgLengthUnits: TRadioGroup;
    rgScaleAlign: TRadioGroup;
    procedure cbScaleVisibleChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure rgLengthUnitsClick(Sender: TObject);
    procedure rgScaleAlignClick(Sender: TObject);
  private
    FScalePlugin: TMapScalePlugin;

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FScalePlugin := TMapScalePlugin.Create(MvPluginManager1);
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

procedure TForm1.cbScaleVisibleChange(Sender: TObject);
begin
  FScalePlugin.Enabled := cbScaleVisible.Checked;
end;

end.

