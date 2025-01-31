unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Forms, Controls, Graphics, StdCtrls, ExtCtrls, Dialogs,
  mvMapViewer, mvTypes, mvGeoMath, mvPluginCommon, mvAreaSelectionPlugin;

type

  { TMainForm }

  TMainForm = class(TForm)
    lblHeader: TStaticText;
    MapView: TMapView;
    Memo: TMemo;
    PluginManager: TMvPluginManager;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);

  private
    FAreaSelectionPlugin: TAreaSelectionPlugin;
    procedure OnSelectedAreaChanged(Sender: TObject);
    procedure OnSelectedAreaChanging(Sender: TObject; ANewArea: TRealArea; var Allow: Boolean);

  public

  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  MapView.Active := true;
  FAreaSelectionPlugin := TAreaSelectionPlugin.Create(PluginManager);
  FAreaSelectionPlugin.MapView := MapView;
  FAreaSelectionPlugin.OnSelectedAreaChanged := @OnSelectedAreaChanged;
  FAreaSelectionPlugin.OnSelectedAreaChanging := @OnSelectedAreaChanging;
  lblHeader.Caption := '                            Left     Top   Right  Bottom';
end;

procedure TMainForm.OnSelectedAreaChanged(Sender: TObject);
begin
  with FAreaSelectionPlugin.SelectedArea do
   Memo.Lines.Add('[%s]  Selected: %6.1f° %6.1f° %6.1f° %6.1f°',[
      FormatDateTime('hh:nn:ss.zzz', Now()),
      TopLeft.Lon,
      TopLeft.Lat,
      BottomRight.Lon,
      BottomRight.Lat
    ]);
end;

procedure TMainForm.OnSelectedAreaChanging(Sender: TObject; ANewArea: TRealArea;
  var Allow: Boolean);
begin
  with ANewArea do
    Memo.Lines.Add('[%s] Selecting: %6.1f° %6.1f° %6.1f° %6.1f°',[
      FormatDateTime('hh:nn:ss.zzz', Now()),
      TopLeft.Lon,
      TopLeft.Lat,
      BottomRight.Lon,
      BottomRight.Lat
    ]);
end;

end.

