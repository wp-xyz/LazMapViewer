unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, mvMapViewer, mvTypes, mvGPSObj, mvPluginCommon, mvGeoMath,
  mvPlugins, uGreatCirclePainterPlugin;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    MapView1: TMapView;
    MvPluginManager1: TMvPluginManager;
    MvPluginManager1DraggableMarkerPlugin1: TDraggableMarkerPlugin;
    Panel1: TPanel;
    Panel2: TPanel;
    TrackBar1: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MvPluginManager1DraggableMarkerPlugin1DraggableMarkerMovedEvent(
      Sender: TDraggableMarkerPlugin; AMarker: TGPSPoint;
      AOrgPosition: TRealPoint);
    procedure TrackBar1Change(Sender: TObject);
  private
    FGreatCirclePainterPlugin : TGreatCirclePainterPlugin;
    FStartMarker : TGpsPointOfInterest;
    FDestinationMarker : TGpsPointOfInterest;
    procedure OnGreatCirclePainterGetCoords(Sender : TGreatCirclePainterPlugin;
                                            var FStart, FDestination : TRealPoint);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }
procedure TForm1.FormCreate(Sender: TObject);

  function AddTraditionalMarker(const ALat, ALon: Double;
    ACaption: String) : TGpsPointOfInterest;
  const
    IMG_INDEX: Integer = 0;
  var
    gpsPt: TGpsPointOfInterest;
  begin
    Result := Nil;
    gpsPt := TGpsPointOfInterest.Create(ALon,ALat);
    try
      gpsPt.Name := ACaption;
{
      if AMapView = MapView_left then
        gpsPt.ImageIndex := -1
      else
      begin
        gpsPt.ImageIndex := IMG_INDEX;
        IMG_INDEX := (IMG_INDEX + 1) mod POI_Images.Count;
      end;
}
      MapView1.GPSItems.Add(gpsPt, 100);
      Result := gpsPt;
      gpsPt := Nil;
    finally
      if Assigned(gpsPt) then
        gpsPt.Free;
    end;
  end;

begin
  FGreatCirclePainterPlugin := TGreatCirclePainterPlugin.Create(MvPluginManager1);
  MvPluginManager1.PluginList.Add(FGreatCirclePainterPlugin);
  FGreatCirclePainterPlugin.ZOrder := gcpzCanvas;
  FGreatCirclePainterPlugin.OnGetStartAndDestinationCoords:=@OnGreatCirclePainterGetCoords;
  FGreatCirclePainterPlugin.MapView := MapView1;
  FGreatCirclePainterPlugin.GreatCircleLineColor:= clRed;
  FGreatCirclePainterPlugin.GreatCircleLineStyle:= psSolid;
  FGreatCirclePainterPlugin.GreatCircleLineWidth:= 1;
  FGreatCirclePainterPlugin.OrthodromeLineColor:= clBlack;
  FGreatCirclePainterPlugin.OrthodromeLineStyle:= psSolid;
  FGreatCirclePainterPlugin.OrthodromeLineWidth:= 3;
  FStartMarker := AddTraditionalMarker(41.1578,-8.6333, 'Start');
  FDestinationMarker := AddTraditionalMarker(10.6722,-61.5333, 'Destination');
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  MapView1.Cyclic := CheckBox1.Checked;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FStartMarker.MoveTo(0.0,10.0);
  FDestinationMarker.MoveTo(0.0,-40.0);
  MapView1.Invalidate;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FStartMarker.MoveTo(0.0,0.0);
  FDestinationMarker.MoveTo(100.0,0.0);
  MapView1.Invalidate;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FStartMarker.MoveTo(-8.6333,41.1578);
  FDestinationMarker.MoveTo(-61.5333,10.6722);
  MapView1.Invalidate;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  FStartMarker.MoveTo(DMSToDeg(66,40,0),DMSToDeg(25,17,0));
  FDestinationMarker.MoveTo(DMSToDeg(162,14,0),DMSToDeg(58,37,0));
  MapView1.Invalidate;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
    0 : FGreatCirclePainterPlugin.ZOrder := gcpzCanvas;
    1 : FGreatCirclePainterPlugin.ZOrder := gcpzInFrontOfMarkers;
    3 : FGreatCirclePainterPlugin.ZOrder := gcpzBehindMarkers;
  end;
  MapView1.Invalidate;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
end;

procedure TForm1.MvPluginManager1DraggableMarkerPlugin1DraggableMarkerMovedEvent
  (Sender: TDraggableMarkerPlugin; AMarker: TGPSPoint; AOrgPosition: TRealPoint
  );
begin
  FGreatCirclePainterPlugin.SetStartAndDestination(FStartMarker.RealPoint,
                                                   FDestinationMarker.RealPoint);
end;
const
  TruncLens : array[0..10] of Integer = (
     1,2,5,10,20,30,50,70,100,150,200
  );
procedure TForm1.TrackBar1Change(Sender: TObject);
begin
  FGreatCirclePainterPlugin.TruncLength := TruncLens[TrackBar1.Position];
  MapView1.Invalidate;
end;

procedure TForm1.OnGreatCirclePainterGetCoords(
  Sender: TGreatCirclePainterPlugin; var FStart, FDestination: TRealPoint);
begin
  FStart := FStartMarker.RealPoint;
  FDestination := FDestinationMarker.RealPoint;
end;

end.

