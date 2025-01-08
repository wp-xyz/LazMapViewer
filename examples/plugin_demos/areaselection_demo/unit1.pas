unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  mvMapViewer, mvPluginCore, uAreaSelectionPlugin, mvTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    Label1: TLabel;
    lblSelArea: TLabel;
    MapView1: TMapView;
    MvPluginManager1: TMvPluginManager;
    Panel1: TPanel;
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    FAreaSelectionPlugin : TAreaSelectionPlugin;
    procedure OnSelectedAreaChanged(Sender : TObject);
    procedure OnSelectedAreaChanging(Sender : TObject; ANewArea : TRealArea; var Allow : Boolean);

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }
procedure TForm1.FormCreate(Sender: TObject);
begin
  FAreaSelectionPlugin := TAreaSelectionPlugin.Create(MvPluginManager1);
  FAreaSelectionPlugin.MapView := MapView1;
  FAreaSelectionPlugin.OnSelectedAreaChanged:= @OnSelectedAreaChanged;
  FAreaSelectionPlugin.OnSelectedAreaChanging:= @OnSelectedAreaChanging;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
end;

procedure TForm1.OnSelectedAreaChanged(Sender: TObject);
begin
  lblSelArea.Caption := Format('Left %1.2f, Top %1.2f, Right %1.2f, Bottom %1.2f',[
                            FAreaSelectionPlugin.SelectedArea.TopLeft.Lon,
                            FAreaSelectionPlugin.SelectedArea.TopLeft.Lat,
                            FAreaSelectionPlugin.SelectedArea.BottomRight.Lon,
                            FAreaSelectionPlugin.SelectedArea.BottomRight.Lat
                            ]);
end;

procedure TForm1.OnSelectedAreaChanging(Sender: TObject; ANewArea: TRealArea;
  var Allow: Boolean);
begin
  lblSelArea.Caption := Format('Left %1.2f, Top %1.2f, Right %1.2f, Bottom %1.2f',[
                            ANewArea.TopLeft.Lon,
                            ANewArea.TopLeft.Lat,
                            ANewArea.BottomRight.Lon,
                            ANewArea.BottomRight.Lat
                            ]);
end;

procedure TForm1.FormChangeBounds(Sender: TObject);
begin
  FAreaSelectionPlugin.SetupRectShifter;
end;

end.

