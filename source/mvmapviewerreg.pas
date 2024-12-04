unit mvMapViewerReg;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, PropEdits, ComponentEditors;

procedure Register;

implementation

{$R mvmapviewer_icons.res}

uses
  ImgList,
  mvTypes, mvGeoNames, mvMapViewer, mvDLEFpc, mvDLECache, mvPluginCore,
  mvMapViewerPropEdits, mvPluginEditors;

procedure Register;
begin
  RegisterComponents(PALETTE_PAGE, [TMapView]);
  RegisterComponents(PALETTE_PAGE, [TMvGeoNames]);
  RegisterComponents(PALETTE_PAGE, [TMvDEFpc, TMvDECache]);
  RegisterComponents(PALETTE_PAGE, [TMvPluginManager]);

  RegisterComponentEditor(TMapView, TMapViewComponentEditor);

  RegisterPropertyEditor(TypeInfo(TMapLayers),
    TMapView, 'Layers', TMapLayersPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TMapTrackPoints),
    TMapTrack, 'Points', TMapTrackPointsPropertyEditor);

  RegisterPropertyEditor(TypeInfo(String),
    TMapView,'MapProvider',TMapProviderPropertyEditor);
  RegisterPropertyEditor(TypeInfo(String),
    TMapLayer,'MapProvider',TMapProviderPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TImageIndex),
    TPointOfInterest, 'ImageIndex', TPointOfInterestImageIndexPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDateTime),
    TMapPoint, 'DateTime', TPointDateTimePropertyEditor);
  RegisterPropertyEditor(TypeInfo(Double),
    TMapPoint, 'Elevation', TPointElevationPropertyEditor);

  RegisterPropertyEditor(TypeInfo(Double),
    TMapPoint,'Latitude',TLatLonDMSPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Double),
    TMapPoint,'Longitude',TLatLonDMSPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Double),
    TMapCenter,'Latitude',TLatLonDMSPropertyEditor);
  RegisterPropertyEditor(TypeInfo(Double),
    TMapCenter,'Longitude',TLatLonDMSPropertyEditor);

  RegisterPropertyEditor(TypeInfo(TMvPluginList),
    TMvPluginManager, 'PluginList', TMvPluginListPropertyEditor);
  RegisterComponentEditor(TMvPluginManager, TMvPluginManagerComponentEditor);

end;

end.

