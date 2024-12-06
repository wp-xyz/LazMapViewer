unit mvPluginCore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Math, LazLoggerBase,
  Controls, Dialogs,
  mvMapViewer, mvClassRegistration;

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
    FMapView: TMapView;
    FEnabled: Boolean;
    procedure SetEnabled(AValue: Boolean);
    procedure SetMapView(AValue: TMapView);
    procedure SetPluginManager(AValue: TMvPluginManager);
  protected
    function GetIndex: Integer; override;
    procedure ReadState(Reader: TReader); override;
    procedure SetIndex(AValue: Integer); override;
    procedure SetParentComponent(AParent: TComponent); override;
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
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
    property PluginManager: TMvPluginManager read FPluginManager write SetPluginManager;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default true;
    property MapView: TMapView read FMapView write SetMapView;
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
    function GetItem(AIndex: Integer): TMvPlugin;
  protected
    procedure AddMapView(AMapView: TMapView); override;
    function HandlePlugin(APlugin: TMvPlugin; AMapView: TMapView): Boolean;
    procedure InvalidateMapViews;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure RemoveMapView(AMapView: TMapView); override;
    procedure SetName(const AValue: TComponentName); override;
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
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure SetChildOrder(Child: TComponent; Order: Integer); override;
    property Item[AIndex: Integer]: TMvPlugin read GetItem; default;
    property MapList: TFPList read FMapList;
  published
    property PluginList: TMvPluginList read FPluginList;
  end;

procedure RegisterPluginClass(APluginClass: TMvPluginClass; const ACaption: String);

var
  PluginClassRegistry: TMvClassRegistry = nil;


implementation

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

constructor TMvPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TMvPluginManager then
    SetPluginManager(TMvPluginManager(AOwner));
  FEnabled := true;
end;

destructor TMvPlugin.Destroy;
begin
  SetPluginManager(nil);
  inherited;
end;

procedure TMvPlugin.Assign(Source: TPersistent);
begin
  if Source is TMvPlugin then
    FEnabled := TMvPlugin(Source).Enabled
  else
    inherited Assign(Source);
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

function TMvPlugin.GetParentComponent: TComponent;
begin
  Result := FPluginManager;
end;

function TMvPlugin.HasParent: Boolean;
begin
  Result := true;
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

procedure TMvPlugin.ReadState(Reader: TReader);
begin
  inherited ReadState(Reader);
  if Reader.Parent is TMvPluginManager then
    SetPluginManager(TMvPluginManager(Reader.Parent));
end;

procedure TMvPlugin.SetEnabled(AValue: Boolean);
begin
  if AValue <> FEnabled then
  begin
    FEnabled := AValue;
    Update;
  end;
end;

procedure TMvPlugin.SetIndex(AValue: Integer);
begin
  FPluginManager.PluginList.Move(Index, EnsureRange(AValue, 0, FPluginManager.PluginList.Count-1));
end;

procedure TMvPlugin.SetMapView(AValue: TMapView);
begin
  if FMapView <> AValue then
  begin
    FMapView := AValue;
    Update;
  end;
end;

procedure TMvPlugin.SetParentComponent(AParent: TComponent);
begin
  if not (csLoading in ComponentState) then
    SetPluginManager(AParent as TMvPluginManager);
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
{
  while FPluginList.Count > 0 do
    FPluginList[FPluginList.Count - 1].Free;
    }
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
    plugin := Item[i];
    if HandlePlugin(plugin, AMapView) then
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
    plugin := Item[i];
    if HandlePlugin(plugin, AMapView) then
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
    plugin := Item[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.BeforeDrawObjects(AMapView, handled);
  end;
  if not handled then
    inherited BeforeDrawObjects(AMapView, AMapEvent);
end;

procedure TMvPluginManager.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  plugin: TMvPlugin;
  i: Integer;
begin
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := Item[i];
    if plugin.Owner = Root then
      Proc(plugin);
  end;
end;

function TMvPluginManager.GetItem(AIndex: Integer): TMvPlugin;
begin
  Result := TMvPlugin(FPluginList.Items[AIndex]);
end;

function TMvPluginManager.HandlePlugin(APlugin: TMvPlugin; AMapView: TMapView): Boolean;
begin
  Result := APlugin.Enabled and ((APlugin.MapView = AMapView) or (APlugin.MapView = nil));
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
    plugin := Item[i];
    if HandlePlugin(plugin, AMapView) then
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
    plugin := Item[i];
    if HandlePlugin(plugin, AMapView) then
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
    plugin := Item[i];
    if HandlePlugin(plugin, AMapView) then
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
    plugin := Item[i];
    if HandlePlugin(plugin, AMapView) then
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
    plugin := Item[i];
    if HandlePlugin(plugin, AMapView) then
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

procedure TMvPluginManager.SetChildOrder(Child: TComponent; Order: Integer);
var
  i: Integer;
begin
  i := FPluginList.IndexOf(Child);
  if i >= 0 then
    FPluginList.Move(i, Order);
end;

procedure TMvPluginManager.SetName(const AValue: TComponentName);
var
  oldName: String;
begin
  if Name = AValue then exit;
  oldName := Name;
  inherited SetName(AValue);
  if csDesigning in ComponentState then
    PluginList.ChangeNamePrefix(oldName, AValue);
end;


{ Plugin registration }

procedure RegisterPluginClass(APluginClass: TMvPluginClass; const ACaption: String);
begin
  RegisterClass(APluginClass);
  if PluginClassRegistry.IndexOfClass(APluginClass) < 0 then
    PluginClassRegistry.Add(TMvClassRegistryItem.Create(APluginClass, ACaption));
end;

initialization
  if PluginClassRegistry = nil then
    PluginClassRegistry := TMvClassRegistry.Create;

finalization
  FreeAndNil(PluginClassRegistry);

end.

