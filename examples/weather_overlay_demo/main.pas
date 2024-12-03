unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, mvMapViewer,
  mvEngine;

type

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    MapView1: TMapView;
    RadioGroup1: TRadioGroup;
    procedure FormCreate(Sender: TObject);
    procedure RadioGroup1Click(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  MapView1.Layers[0].MapProvider := 'OpenWeatherMap Temperature';
  MapView1.Layers[1].MapProvider := 'OpenWeatherMap Precipitation';
  MapView1.Layers[2].MapProvider := 'OpenWeatherMap Wind';
  MapView1.Layers[3].MapProvider := 'OpenWeatherMap Clouds';
  MapView1.Active := true;
end;

procedure TForm1.RadioGroup1Click(Sender: TObject);
begin
  MapView1.Layers[0].Visible := RadioGroup1.ItemIndex = 1;
  MapView1.Layers[1].Visible := RadioGroup1.ItemIndex = 2;
  MapView1.Layers[2].Visible := RadioGroup1.ItemIndex = 3;
  MapView1.Layers[3].Visible := RadioGroup1.ItemIndex = 4;
end;

initialization
  OpenWeatherMap_APIKey := 'aebd53c0cfd56c938a0a161b2b8b064b';   // NICHT WEITERGEBEN!

end.

