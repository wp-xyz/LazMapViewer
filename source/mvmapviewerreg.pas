unit mvMapViewerReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure Register;

implementation

{$R mvmapviewer_icons.res}

uses
  mvTypes, mvGeoNames, mvMapViewer, mvDLEFpc, mvDLECache;

procedure Register;
begin
  RegisterComponents(PALETTE_PAGE, [TMapView]);
  RegisterComponents(PALETTE_PAGE, [TMvGeoNames]);
  RegisterComponents(PALETTE_PAGE, [TMvDEFpc, TMvDECache]);
end;

end.

