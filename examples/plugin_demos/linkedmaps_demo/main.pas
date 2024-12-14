unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, mvMapViewer, mvPluginCore, mvPlugins, SysUtils, Forms,
  Controls, Graphics;

type
  TMainForm = class(TForm)
    MapView1: TMapView;
    MapView2: TMapView;
    PluginManager: TMvPluginManager;
    LinkedMapsPlugin: TLinkedMapsPlugin;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MapView1.Active := true;
  MapView2.Active := true;
end;

end.

