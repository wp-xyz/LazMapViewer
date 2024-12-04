unit mvPlugins;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, contnrs,
  Controls, Dialogs,
  mvMapViewer;

type
  TMvPlugin = class;
  TMvPluginManager = class;

  TMvIndexedComponent = class(TComponent)
  strict protected
    function GetIndex: Integer; virtual; abstract;
    procedure SetIndex(AValue: Integer); virtual; abstract;
  public
    procedure ChangeNamePrefix(const AOld, ANew: String; var AFailed: String);
    property Index: Integer read GetIndex write SetIndex;
  end;

  TMvIndexedComponentList = class(TFPList)
  public
    procedure ChangeNamePrefix(const AOld, ANew: String);
  end;

  TMvPlugin = class(TMvIndexedComponent)
  private
    FPluginManager: TMvPluginManager;
    FActive: Boolean;
    procedure SetActive(AValue: Boolean);
    procedure SetPluginManager(AValue: TMvPluginManager);
  protected
    function GetIndex: Integer; override;
    procedure SetIndex(AValue: Integer); override;
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
  published
    property Active: Boolean read FActive write SetActive default true;
  end;

  TMvPluginClass = class of TMvPlugin;

  TMvPluginList = class(TMvIndexedComponentList)
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

procedure RegisterPluginClass(APluginClass: TMvPluginClass; const ACaption: String);


implementation

uses
  mvPluginRegistration;

{ TMvIndexedComponent, borrowed from TAChart }

procedure TMvIndexedComponent.ChangeNamePrefix(const AOld, ANew: String;
  var AFailed: String);
begin
  if AnsiStartsStr(AOld, Name) then
    try
      Name := ANew + Copy(Name, Length(AOld) + 1, Length(Name));
    except on EComponentError do
      AFailed += IfThen(AFailed = '', '', ', ') + Name;
    end;
end;


{ TMvIndexedComponentList, borrowed from TAChart }

procedure TMvIndexedComponentList.ChangeNamePrefix(const AOld, ANew: String);
var
  failed: String;
  i: Integer;
begin
  failed := '';
  for i := 0 to Count - 1 do
    TMvIndexedComponent(Items[i]).ChangeNamePrefix(AOld, ANew, failed);
  if (failed <> '') then
    ShowMessage(Format('Failed to rename components: %s', [failed]));
end;


{ TMvPlugin }

constructor TMvPlugin.Create(APluginManager: TMvPluginManager);
begin
  inherited Create(APluginManager);
  SetPluginManager(APluginManager);
  FActive := true;
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

function TMvPlugin.GetIndex: Integer;
begin
  if FPluginManager = nil then
    Result := -1
  else
    Result := FPluginManager.PluginList.IndexOf(Self);
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

procedure TMvPlugin.SetActive(AValue: Boolean);
begin
  if AValue <> FActive then
  begin
    FActive := AValue;
    Update;
  end;
end;

procedure TMvPlugin.SetIndex(AValue: Integer);
begin
  if AValue < 0 then
    AValue := 0
  else
  if AValue >= FPluginManager.PluginList.Count then
    AValue := FPluginManager.PluginList.Count - 1;
  FPluginManager.PluginList.Move(Index, AValue);
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
  while FPluginList.Count > 0 do
    FPluginList[FPluginList.Count - 1].Free;
  FPluginList.Free;
  FMapList.Free;
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
  plugin: TMvPlugin;
begin
  handled := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := FPluginList[i];
    if plugin.Active then
      plugin.AfterDrawObjects(AMapView, handled);
  end;
  if not handled then
    inherited AfterDrawObjects(AMapView, AMapEvent);
end;

procedure TMvPluginManager.AfterPaint(AMapView: TMapView; AMapEvent: TNotifyEvent);
var
  i: Integer;
  handled: Boolean;
  plugin: TMvPlugin;
begin
  handled := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := FPluginList[i];
    if plugin.Active then
      plugin.AfterPaint(AMapView, handled);
  end;
  if not handled then
    inherited AfterPaint(AMapView, AMapEvent);
end;

procedure TMvPluginManager.BeforeDrawObjects(AMapView: TMapView; AMapEvent: TNotifyEvent);
var
  i: Integer;
  handled: Boolean;
  plugin: TMvPlugin;
begin
  handled := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := FPluginList[i];
    if plugin.Active then
      plugin.BeforeDrawObjects(AMapView, handled);
  end;
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
  plugin: TMvPlugin;
begin
  handled := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := FPluginList[i];
    if plugin.Active then
      plugin.MouseDown(AMapView, AButton, AShift, X, Y, handled);
  end;
  if (not handled) then
    inherited MouseDown(AMapView, AButton, AShift, X, Y, AMapEvent);
end;

procedure TMvPluginManager.MouseEnter(AMapView: TMapView; AMapEvent: TNotifyEvent);
var
  i: Integer;
  handled: Boolean;
  plugin: TMvPlugin;
begin
  handled := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := FPluginList[i];
    if plugin.Active then
      plugin.MouseEnter(AMapView, handled);
  end;
  if not handled then
    inherited MouseEnter(AMapView, AMapEvent);
end;

procedure TMvPluginManager.MouseLeave(AMapView: TMapView; AMapEvent: TNotifyEvent);
var
  i: Integer;
  handled: Boolean;
  plugin: TMvPlugin;
begin
  handled := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := FPluginList[i];
    if plugin.Active then
      plugin.MouseLeave(AMapView, handled);
  end;
  if not handled then
    inherited MouseLeave(AMapView, AMapEvent);
end;

procedure TMvPluginManager.MouseMove(AMapView: TMapView; AShift: TShiftState;
  X,Y: Integer; AMapEvent: TMouseMoveEvent);
var
  i: Integer;
  handled: Boolean;
  plugin: TMvPlugin;
begin
  handled := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := FPluginList[i];
    if plugin.Active then
      plugin.MouseMove(AMapView, AShift, X, Y, handled);
  end;
  if (not handled) then
    inherited MouseMove(AMapView, AShift, X, Y, AMapEvent);
end;

procedure TMvPluginManager.MouseUp(AMapView: TMapView; AButton: TMouseButton;
  AShift: TShiftState; X, Y: Integer; AMapEvent: TMouseEvent);
var
  i: Integer;
  handled: Boolean;
  plugin: TMvPlugin;
begin
  handled := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := FPluginList[i];
    if plugin.Active then
      plugin.MouseUp(AMapView, AButton, AShift, X, Y, handled);
  end;
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


{ Plugin registration }

var
  PluginClassRegistry: TMvClassRegistry = nil;

procedure RegisterPluginClass(APluginClass: TMvPluginClass; const ACaption: String);
begin
  RegisterClass(APluginClass);
  if PluginClassRegistry.IndexOfClass(APluginClass) < 0 then
    PluginClassRegistry.Add(TMvClassRegistryItem.Create(APluginClass, ACaption));
end;

initialization
  PluginClassRegistry := TMvClassRegistry.Create;

finalization
  FreeAndNil(PluginClassRegistry);

end.

