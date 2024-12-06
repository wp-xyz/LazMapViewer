unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, mvMapViewer, mvPluginCore, mvPlugins, SysUtils, Forms,
  Controls, Graphics;

type
  TForm1 = class(TForm)
    MapView1: TMapView;
    MapView2: TMapView;
    MvPluginManager1: TMvPluginManager;
    MvPluginManager1LinkedMapsPlugin1: TLinkedMapsPlugin;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  MapView1.Active := true;
  MapView2.Active := true;
end;

end.

