unit main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics, Controls, StdCtrls, ExtCtrls, Forms,
  mvMapViewer, mvPluginCore, mvPlugins;

type

  { TMainForm }

  TMainForm = class(TForm)
    Label1: TLabel;
    MapView1: TMapView;
    MapView2: TMapView;
    Panel1: TPanel;
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

