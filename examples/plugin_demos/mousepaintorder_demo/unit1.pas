unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, mvMapViewer,
  mvPluginCore, uDragColoredItemPlugin, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    MapView1: TMapView;
    MvPluginManager1: TMvPluginManager;
    Panel1: TPanel;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }
const
  PluginColors : array[0..8] of TColor = (
    clFuchsia,clRed,$ff8c00,clYellow,clLime,clGreen,clNavy,clBlue,clAqua
  );
procedure TForm1.FormCreate(Sender: TObject);
var
  lDragColoredItemPlugin : TDragColoredItemPlugin;
  i: Integer;
begin
  for i := 0 to High(PluginColors) do
  begin
    lDragColoredItemPlugin := TDragColoredItemPlugin.Create(Self);
    lDragColoredItemPlugin.Color := PluginColors[i];
    lDragColoredItemPlugin.MapView := MapView1;
    MvPluginManager1.AddPlugin(lDragColoredItemPlugin);
  end;
end;

end.

