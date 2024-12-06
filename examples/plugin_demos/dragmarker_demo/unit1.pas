unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  mvMapViewer, mvPluginCore, mvPlugins,
  mvGPSObj;

type

  { TForm1 }

  TForm1 = class(TForm)
    MapView1: TMapView;
    MvPluginManager1: TMvPluginManager;
    MvPluginManager1DraggableMarkerPlugin1: TDraggableMarkerPlugin;
    MvPluginManager1LegalNoticePlugin1: TLegalNoticePlugin;
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
  procedure AddTraditionalMarker(const ALon, ALat : Double; ACaption : String);
  var
    gpsPt: TGpsPointOfInterest;
  begin
    gpsPt := TGpsPointOfInterest.Create(ALon,ALat);
    try
      gpsPt.Name := ACaption;
      gpsPt.ImageIndex := 0;
      MapView1.GPSItems.Add(gpsPt, 100);
      gpsPt := Nil;
    finally
      if Assigned(gpsPt) then
        gpsPt.Free;
    end;
  end;

begin
  AddTraditionalMarker(0.0, 51.4825766,'Greenwich');
  AddTraditionalMarker(2.2945500,48.8582300,'Tour d´Eiffel, Paris');
  AddTraditionalMarker(-79.3884000,43.6439500,'CN Tower, Toronto');
  AddTraditionalMarker(-157.7739800,21.2716900,'Kahala Avenue, Honolulu');
  AddTraditionalMarker(114.1497900,22.2708100,'The Peak, Hong Kong');
  AddTraditionalMarker(13.377778,52.516389,'Brandenburger Tor, Berlin');
  AddTraditionalMarker(-58.3722400,-34.6084700,'Pirámide de Mayo, Buenos Aires');
  AddTraditionalMarker(151.2082800,-33.8707000,'Sydney Tower Skywalk, Sydney');
  AddTraditionalMarker(139.7021800,35.6787500,'Meiji Jingu Shrine, Tokyo');
end;

end.

