unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Menus, Spin, mvMapViewer, mvTypes, mvGPSObj, mvPluginCommon,
  mvGeoMath, mvPlugins, uGreatCirclePainterPlugin;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    CheckBox1: TCheckBox;
    CheckGroup1: TCheckGroup;
    ColorButton1: TColorButton;
    ColorButton2: TColorButton;
    ComboBox1: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MapView1: TMapView;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MvPluginManager1: TMvPluginManager;
    MvPluginManager1DraggableMarkerPlugin1: TDraggableMarkerPlugin;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PopupMenu1: TPopupMenu;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    StatusBar1: TStatusBar;
    TrackBar1: TTrackBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure CheckBox1Change(Sender: TObject);
    procedure CheckGroup1ItemClick(Sender: TObject; Index: integer);
    procedure ColorButton1ColorChanged(Sender: TObject);
    procedure ColorButton2ColorChanged(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PoleMenuItemClick(Sender: TObject);
    procedure MvPluginManager1DraggableMarkerPlugin1DraggableMarkerMovedEvent(
      Sender: TDraggableMarkerPlugin; AMarker: TGPSPoint;
      AOrgPosition: TRealPoint);
    procedure SpinEdit1Change(Sender: TObject);
    procedure SpinEdit2Change(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    FGreatCirclePainterPlugin : TGreatCirclePainterPlugin;
    FStartMarker : TGpsPointOfInterest;
    FDestinationMarker : TGpsPointOfInterest;
    procedure OnGreatCirclePainterGetCoords(Sender : TGreatCirclePainterPlugin;
                                            var FStart, FDestination : TRealPoint);
    procedure SetMarkerPositions(const AStartLat, AStartLon, ADestLat, ADestLon : Double);
    procedure OnGreatCirclePainterPluginChange(Sender : TObject);
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }
procedure TForm1.FormCreate(Sender: TObject);

  function AddTraditionalMarker(const ALat, ALon: Double;
    ACaption: String; const AImgIndex : Integer = -1) : TGpsPointOfInterest;
  var
    gpsPt: TGpsPointOfInterest;
  begin
    Result := Nil;
    gpsPt := TGpsPointOfInterest.Create(ALon,ALat);
    try
      gpsPt.Name := ACaption;
      gpsPt.ImageIndex := AImgIndex;
      MapView1.GPSItems.Add(gpsPt, 100);
      Result := gpsPt;
      gpsPt := Nil;
    finally
      if Assigned(gpsPt) then
        gpsPt.Free;
    end;
  end;

begin
  MapView1.Active := true;

  FGreatCirclePainterPlugin := TGreatCirclePainterPlugin.Create(MvPluginManager1);
  MvPluginManager1.PluginList.Add(FGreatCirclePainterPlugin);
  FGreatCirclePainterPlugin.ZOrder := gcpzCanvas;
  FGreatCirclePainterPlugin.OnGetStartAndDestinationCoords:=@OnGreatCirclePainterGetCoords;
  FGreatCirclePainterPlugin.OnChange := @OnGreatCirclePainterPluginChange;
  FGreatCirclePainterPlugin.MapView := MapView1;
  FGreatCirclePainterPlugin.GreatCirclePen.Color:= ColorButton2.ButtonColor;
  FGreatCirclePainterPlugin.GreatCirclePen.Style:= psSolid;
  FGreatCirclePainterPlugin.GreatCirclePen.Width:= SpinEdit2.Value;
  FGreatCirclePainterPlugin.OrthodromePen.Color:= ColorButton1.ButtonColor;
  FGreatCirclePainterPlugin.OrthodromePen.Style:= psSolid;
  FGreatCirclePainterPlugin.OrthodromePen.Width:= SpinEdit1.Value;
  FStartMarker := AddTraditionalMarker(41.1578,-8.6333, 'Start',0);
  FDestinationMarker := AddTraditionalMarker(10.6722,-61.5333, 'Destination',1);
end;

procedure TForm1.SetMarkerPositions(const AStartLat, AStartLon, ADestLat, ADestLon : Double);
begin
  FStartMarker.MoveTo(AStartLon, AStartLat);
  FDestinationMarker.MoveTo(ADestLon, ADestLat);
  MapView1.Invalidate;
end;

procedure TForm1.OnGreatCirclePainterPluginChange(Sender: TObject);
var
  s : String;
begin
  s := Format('%1.3f',[FGreatCirclePainterPlugin.OrthodromeDistance / 1000.0]);
  StatusBar1.Panels[1].Text := s;
  s := Format('%1.3f',[(EARTH_CIRCUMFERENCE-FGreatCirclePainterPlugin.OrthodromeDistance) / 1000.0]);
  StatusBar1.Panels[3].Text := s;
  s := Format('%1.6f:%1.6f',[FGreatCirclePainterPlugin.StartLat,FGreatCirclePainterPlugin.StartLon]);
  StatusBar1.Panels[5].Text := s;
  s := Format('%1.6f:%1.6f',[FGreatCirclePainterPlugin.DestinationLat,FGreatCirclePainterPlugin.DestinationLon]);
  StatusBar1.Panels[7].Text := s;
  s := Format('%1.2f°',[FGreatCirclePainterPlugin.InitialBearing]);
  StatusBar1.Panels[9].Text := s;
  StatusBar1.Panels[11].Text := IntToStr(FGreatCirclePainterPlugin.GreatCirclePointsCount);
end;


procedure TForm1.PoleMenuItemClick(Sender: TObject);

begin
  if Sender is TMenuItem then
  begin
    case TMenuItem(Sender).Tag of
      1 : SetMarkerPositions(42.496909,-7.026229,-42.496909,172.973771); // Antipodes Spain / New Zealand
      2 : SetMarkerPositions(90.0,20.0,-90.0,-90.0); // Both Poles, Start North
      3 : SetMarkerPositions(-90.0,40.0,90.0,-100.0); // Both Poles, Start South
      4 : SetMarkerPositions(90.0,60.0,48.8582300,2.2945500); // Start North, Dest free
      5 : SetMarkerPositions(-90.0,80.0,22.2708100,114.1497900); // Start South, Dest free
      6 : SetMarkerPositions(-33.8707000,151.2082800,90.0,-110.0); // Dest North, Start free
      7 : SetMarkerPositions(43.6439500,-79.3884000,-90.0,-120.0); // Dest South, Start free
    end;
  end;
end;

procedure TForm1.CheckBox1Change(Sender: TObject);
begin
  MapView1.Cyclic := CheckBox1.Checked;
  MapView1.Invalidate;
end;

procedure TForm1.CheckGroup1ItemClick(Sender: TObject; Index: integer);
var
  opt : TGreatCirclePainterOptions = [];
begin
  if CheckGroup1.Checked[0] then
    Include(opt,gcpoMarkStart);
  if CheckGroup1.Checked[1] then
    Include(opt,gcpoMarkCenter);
  if CheckGroup1.Checked[2] then
    Include(opt,gcpoMarkDestination);
  FGreatCirclePainterPlugin.Options := opt;
end;

procedure TForm1.ColorButton1ColorChanged(Sender: TObject);
begin
  FGreatCirclePainterPlugin.OrthodromePen.Color := ColorButton1.ButtonColor;
end;

procedure TForm1.ColorButton2ColorChanged(Sender: TObject);
begin
  FGreatCirclePainterPlugin.GreatCirclePen.Color := ColorButton2.ButtonColor;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  PopupMenu1.PopUp;
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

procedure TForm1.Button5Click(Sender: TObject);
begin
//  24◦33′ N, 118◦38′ E
// 37◦2′ N, 8◦55′ W
  FStartMarker.MoveTo(DMSToDeg(118,38,0),DMSToDeg(23,33,0));
  FDestinationMarker.MoveTo(-DMSToDeg(8,55,0),DMSToDeg(37,2,0));
  MapView1.Invalidate;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  case ComboBox1.ItemIndex of
    0 : FGreatCirclePainterPlugin.ZOrder := gcpzCanvas;
    1 : FGreatCirclePainterPlugin.ZOrder := gcpzInFrontOfMarkers;
    2 : FGreatCirclePainterPlugin.ZOrder := gcpzBehindMarkers;
  end;
  MapView1.Invalidate;
end;

procedure TForm1.MvPluginManager1DraggableMarkerPlugin1DraggableMarkerMovedEvent
  (Sender: TDraggableMarkerPlugin; AMarker: TGPSPoint; AOrgPosition: TRealPoint
  );
begin
  FGreatCirclePainterPlugin.SetStartAndDestination(FStartMarker.RealPoint,
                                                   FDestinationMarker.RealPoint);
end;

procedure TForm1.SpinEdit1Change(Sender: TObject);
begin
  FGreatCirclePainterPlugin.OrthodromePen.Width := SpinEdit1.Value;
end;

procedure TForm1.SpinEdit2Change(Sender: TObject);
begin
  FGreatCirclePainterPlugin.GreatCirclePen.Width := SpinEdit2.Value;
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

