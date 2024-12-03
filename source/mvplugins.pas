unit mvPlugins;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, Controls, mvMapViewer;

type
  TMvPlugin = class;
  TMvPluginManager = class;

  TMvPlugin = class(TComponent)
  private
    FPluginManager: TMvPluginManager;
    procedure SetPluginManager(AValue: TMvPluginManager);
  protected
    procedure AfterDrawObjects(AMapView: TMapView; var Handled: Boolean); virtual;
    procedure AfterPaint(AMapView: TMapView; var Handled: Boolean); virtual;
    procedure BeforeDrawObjects(AMapView: TMapView; var Handled: Boolean); virtual;
    procedure MouseDown(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); virtual;
    procedure MouseEnter(AMapView: TMapView; var Handled: Boolean); virtual;
    procedure MouseLeave(AMapView: TMapView; var Handled: Boolean); virtual;
    procedure MouseMove(AMapView: TMapView; AShift: TShiftState; X,Y: Integer;
      var Handled: Boolean); virtual;
    procedure MouseUp(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); virtual;
    procedure Update; virtual;
  public
    constructor Create(APluginManager: TMvPluginManager); virtual; reintroduce;
    property PluginManager: TMvPluginManager read FPluginManager write SetPluginManager;
  end;

  TMvPluginList = class(TFPList)
  private
    function GetItem(AIndex: Integer): TMvPlugin;
    procedure SetItem(AIndex: Integer; AValue: TMvPlugin);
  public
    property Items[AIndex: Integer]: TMvPlugin read GetItem write SetItem; default;
  end;

  TMvPluginManager = class(TMvCustomPluginManager)
  private
    FPluginList: TMvPluginList;
    FMapList: TFPList;
  protected
    procedure AddMapView(AMapView: TMapView); override;
    procedure InvalidateMapViews;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RemoveMapView(AMapView: TMapView); override;
  protected
    // Dispatching events to be handled by the plugins
    procedure AfterDrawObjects(AMapView: TMapView; AMapEvent: TNotifyEvent); override;
    procedure AfterPaint(AMapView: TMapView; AMapEvent: TNotifyEvent); override;
    procedure BeforeDrawObjects(AMapView: TMapView; AMapEvent: TNotifyEvent); override;
    procedure MouseDown(AMapView: TMapView; AButton: TMouseButton;
      AShift: TShiftState; X, Y: Integer; AMapEvent: TMouseEvent); override;
    procedure MouseEnter(AMapView: TMapView; AMapEvent: TNotifyEvent); override;
    procedure MouseLeave(AMapView: TMapView; AMapEvent: TNotifyEvent); override;
    procedure MouseMove(AMapView: TMapView; AShift: TShiftState; X,Y: Integer;
      AMapEvent: TMouseMoveEvent); override;
    procedure MouseUp(AMapView: TMapView; AButton: TMouseButton;
      AShift: TShiftState; X, Y: Integer; AMapEvent: TMouseEvent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PluginList: TMvPluginList read FPluginList;
    property MapList: TFPList read FMapList;
  end;


implementation

{ TMvPlugin }

constructor TMvPlugin.Create(APluginManager: TMvPluginManager);
begin
  inherited Create(APluginManager);
  SetPluginManager(APluginManager);
end;

procedure TMvPlugin.AfterPaint(AMapView: TMapView; var Handled: Boolean);
begin
end;

procedure TMvPlugin.AfterDrawObjects(AMapView: TMapView; var Handled: Boolean);
begin
end;

procedure TMvPlugin.BeforeDrawObjects(AMapView: TMapView; var Handled: Boolean);
begin
end;

procedure TMvPlugin.MouseDown(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
begin
end;

procedure TMvPlugin.MouseEnter(AMapView: TMapView; var Handled: Boolean);
begin
end;

procedure TMvPlugin.MouseLeave(AMapView: TMapView; var Handled: Boolean);
begin
end;

procedure TMvPlugin.MouseMove(AMapView: TMapView; AShift: TShiftState;
  X, Y: Integer; var Handled: Boolean);
begin
end;

procedure TMvPlugin.MouseUp(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
begin
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

procedure TMvPlugin.Update;
begin
  if Assigned(FPluginManager) then
    FPluginManager.InvalidateMapViews;
end;


{ TMvPluginList }

function TMvPluginList.GetItem(AIndex: Integer): TMvPlugin;
begin
  Result := TMvPlugin(inherited Items[AIndex]);
end;

procedure TMvPluginList.SetItem(AIndex: Integer; AValue: TMvPlugin);
begin
  inherited Items[AIndex] := AValue;
end;


{ TMvPluginManager }

constructor TMvPluginManager.Create(AOwner: TComponent);
begin
  inherited;
  FPluginList := TMvPluginList.Create;
  FMapList := TFPList.Create;
end;

destructor TMvPluginManager.Destroy;
begin
  FMapList.Free;
  FPluginList.Free;
  inherited;
end;

procedure TMvPluginManager.AddMapView(AMapView: TMapView);
var
  idx: Integer;
begin
  idx := FMaplist.IndexOf(AMapView);
  if idx = -1 then
    FMapList.Add(AMapView);
end;

procedure TMvPluginManager.AfterDrawObjects(AMapView: TMapView; AMapEvent: TNotifyEvent);
var
  i: Integer;
  handled: Boolean;
begin
  handled := false;
  for i := 0 to FPluginList.Count-1 do
    FPluginList[i].AfterDrawObjects(AMapView, handled);
  if not handled then
    inherited AfterDrawObjects(AMapView, AMapEvent);
end;

procedure TMvPluginManager.AfterPaint(AMapView: TMapView; AMapEvent: TNotifyEvent);
var
  i: Integer;
  handled: Boolean;
begin
  handled := false;
  for i := 0 to FPluginList.Count-1 do
    FPluginList[i].AfterPaint(AMapView, handled);
  if not handled then
    inherited AfterPaint(AMapView, AMapEvent);
end;

procedure TMvPluginManager.BeforeDrawObjects(AMapView: TMapView; AMapEvent: TNotifyEvent);
var
  i: Integer;
  handled: Boolean;
begin
  handled := false;
  for i := 0 to FPluginList.Count-1 do
    FPluginList[i].BeforeDrawObjects(AMapView, handled);
  if not handled then
    inherited BeforeDrawObjects(AMapView, AMapEvent);
end;

procedure TMvPluginManager.InvalidateMapViews;
var
  i: Integer;
begin
  for i := 0 to FMapList.Count-1 do
    TMapView(FMapList[i]).Invalidate;
end;

procedure TMvPluginManager.MouseDown(AMapView: TMapView; AButton: TMouseButton;
  AShift: TShiftState; X, Y: Integer; AMapEvent: TMouseEvent);
var
  i: Integer;
  handled: Boolean;
begin
  handled := false;
  for i := 0 to FPluginList.Count-1 do
    FPluginList[i].MouseDown(AMapView, AButton, AShift, X, Y, handled);
  if (not handled) then
    inherited MouseDown(AMapView, AButton, AShift, X, Y, AMapEvent);
end;

procedure TMvPluginManager.MouseEnter(AMapView: TMapView; AMapEvent: TNotifyEvent);
var
  i: Integer;
  handled: Boolean;
begin
  handled := false;
  for i := 0 to FPluginList.Count-1 do
    FPluginList[i].MouseEnter(AMapView, handled);
  if not handled then
    inherited MouseEnter(AMapView, AMapEvent);
end;

procedure TMvPluginManager.MouseLeave(AMapView: TMapView; AMapEvent: TNotifyEvent);
var
  i: Integer;
  handled: Boolean;
begin
  handled := false;
  for i := 0 to FPluginList.Count-1 do
    FPluginList[i].MouseLeave(AMapView, handled);
  if not handled then
    inherited MouseLeave(AMapView, AMapEvent);
end;

procedure TMvPluginManager.MouseMove(AMapView: TMapView; AShift: TShiftState;
  X,Y: Integer; AMapEvent: TMouseMoveEvent);
var
  i: Integer;
  handled: Boolean;
begin
  handled := false;
  for i := 0 to FPluginList.Count-1 do
    FPluginList[i].MouseMove(AMapView, AShift, X, Y, handled);
  if (not handled) then
    inherited MouseMove(AMapView, AShift, X, Y, AMapEvent);
end;

procedure TMvPluginManager.MouseUp(AMapView: TMapView; AButton: TMouseButton;
  AShift: TShiftState; X, Y: Integer; AMapEvent: TMouseEvent);
var
  i: Integer;
  handled: Boolean;
begin
  handled := false;
  for i := 0 to FPluginList.Count-1 do
    FPluginList[i].MouseUp(AMapView, AButton, AShift, X, Y, handled);
  if not handled then
    inherited MouseUp(AMapView, AButton, AShift, X, Y, AMapEvent);
end;

procedure TMvPluginManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent is TMapView then
      RemoveMapView(TMapView(AComponent));
    if AComponent is TMvPlugin then
    begin
      AComponent.Free;
      FPluginList.Remove(AComponent);
    end;
  end;
end;

procedure TMvPluginManager.RemoveMapView(AMapView: TMapView);
begin
  FMapList.Remove(AMapView);
end;

end.

