unit mvPlugins;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, Controls, mvMapViewer;

type
  TMvPluginManager = class;

  TMvPlugin = class(TComponent)
  private
    FMapView: TMapView;
    FPluginManager: TMvPluginManager;
    procedure SetPluginManager(AValue: TMvPluginManager);
  protected
    procedure MouseDown(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); virtual;
  public
    constructor Create(APluginManager: TMvPluginManager); virtual; reintroduce;
    property PluginManager: TMvPluginManager read FPluginManager write SetPluginManager;
  end;

  TMvPluginList = class(TFPList);

  TMvPluginManager = class(TMvCustomPluginManager)
  private
    FPluginList: TMvPluginList;
  protected
    procedure MouseDown(AMapView: TMapView; AButton: TMouseButton;
      AShift: TShiftState; X, Y: Integer; AUserEvent: TMouseEvent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PluginList: TMvPluginList read FPluginList;
  end;


implementation

{ TMvPlugin }

constructor TMvPlugin.Create(APluginManager: TMvPluginManager);
begin
  inherited Create(APluginManager);
  SetPluginManager(APluginManager);
end;

procedure TMvPlugin.MouseDown(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
begin
  Handled := false;
end;

procedure TMvPlugin.SetPluginManager(AValue: TMvPluginManager);
begin
  if FPluginManager = AValue then exit;
  if FPluginManager <> nil then
    FPluginManager.PluginList.Remove(Self);
  FPluginManager := AValue;
  if FPluginManager <> nil then
    FPluginManager.PluginList.Add(Self);
end;


{ TMvPluginManager }

constructor TMvPluginManager.Create(AOwner: TComponent);
begin
  inherited;
  FPluginList := TMvPluginList.Create;
end;

destructor TMvPluginManager.Destroy;
begin
  FPluginList.Free;
  inherited;
end;

procedure TMvPluginManager.MouseDown(AMapView: TMapView; AButton: TMouseButton;
  AShift: TShiftState; X, Y: Integer; AUserEvent: TMouseEvent);
var
  i: Integer;
  handled: Boolean;
begin
  handled := false;
  for i := 0 to FPluginList.Count-1 do
    TMvPlugin(FPluginList[i]).MouseDown(AMapView, AButton, AShift, X, Y, handled);
  if not handled then
    inherited;
end;

end.

