unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, mvMapViewer, mvPluginCore, mvPlugins, SysUtils, Forms, Controls,
  Graphics, Dialogs, mvTypes;

type
  TForm1 = class(TForm)
    ImageList1: TImageList;
    MapView1: TMapView;
    MvPluginManager1: TMvPluginManager;
    MvPluginManager1LegalNoticePlugin1: TLegalNoticePlugin;
    MvPluginManager1LegalNoticePlugin2: TLegalNoticePlugin;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  L: TMapLayer;
  P: TMapPointOfInterest;
begin
  L := MapView1.Layers.Add as TMapLayer;

  P := L.PointsOfInterest.Add as TMapPointOfInterest;
  // or: P := TMapPointOfInterest.Create(L.PointsOfInterest);
  P.RealPoint := RealPoint(68, 111);
  P.ImageIndex := 0;
  P.ImageAnchorX := 0.0;
  P.ImageAnchorY := 1.0;
  P.TextPositionHor := tphLeft;
  P.Caption := 'Tilted Pin';

  P := L.PointsOfInterest.Add as TMapPointOfInterest;
  P.RealPoint := RealPoint(19, -45);
  P.ImageIndex := 2;
  P.Caption := 'Vertical Pin';

  P := L.PointsOfInterest.Add as TMapPointOfInterest;
  P.RealPoint := RealPoint(0.0, 0.0);
  P.ImageIndex := 1;
  P.Caption := 'Location';

  P := L.PointsOfInterest.Add as TMapPointOfInterest;
  P.RealPoint := RealPoint(0.0, -120.0);
  P.ImageIndex := 3;
  P.ImageAnchorX := 0.2;
  P.ImageAnchorY := 0;
  P.TextPositionHor := tphLeft;
  P.TextPositionVert := tpvAbove;
  P.Caption := 'Safety Pin';
end;

end.

