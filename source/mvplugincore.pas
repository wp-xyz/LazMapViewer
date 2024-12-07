unit mvPluginCore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Math, LazLoggerBase,
  Controls, Dialogs, Contnrs,
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
    procedure CenterMove(AMapView: TMapView; var Handled: Boolean); virtual;
    procedure MouseDown(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); virtual;
    procedure MouseEnter(AMapView: TMapView; var Handled: Boolean); virtual;
    procedure MouseLeave(AMapView: TMapView; var Handled: Boolean); virtual;
    procedure MouseMove(AMapView: TMapView; AShift: TShiftState; X,Y: Integer;
      var Handled: Boolean); virtual;
    procedure MouseUp(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); virtual;
    procedure ZoomChange(AMapView: TMapView; var Handled: Boolean); virtual;
  //  procedure ZoomChanging(AMapView: TMapView; NewZoom: Integer; var Allow, Handled: Boolean); virtual;
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

  { TMvMultiMapsPluginData }

  TMvMultiMapsPluginData = class(TObject)
  private
    FMapView : TMapView;
    FData : array of Byte;
    function GetDataSize : Integer;
  public
    property DataSize : Integer read GetDataSize;
    procedure SetData(const AData; const ADataSize : Integer);
    function GetData(out AData; const AMaxDataSize : Integer) : Integer;
    function GetDataPtr : Pointer;
    property MapView : TMapView read FMapView write FMapView;
  end;

  { TMvMultiMapsPlugin }

  TMvMultiMapsPlugin = class(TMvPlugin)
  private
    FMapDataList : TObjectList;
    function GetMapViewDataIndex(Value : TMapView) : Integer;
    function GetMapViewDataSize(Value : TMapView) : Integer;
    function GetMapViewDataPtr(Value : TMapView) : Pointer;
  protected
    function CreateMultiMapsPluginData : TMvMultiMapsPluginData;virtual;
  public
    property MapViewDataIndex[AIndex : TMapView] : Integer read GetMapViewDataIndex;
    property MapViewDataSize[AIndex : TMapView] : Integer read GetMapViewDataSize;
    property MapViewDataPtr[AIndex : TMapView] : Pointer read GetMapViewDataPtr;
    function GetMapViewData(const AMapView : TMapView; out AData; const AMaxDataSize : Integer) : Integer;
    procedure SetMapViewData(const AMapView : TMapView; const AData; const ADataSize : Integer);
    constructor Create(AOwner : TComponent);override;
    destructor Destroy;override;
  end;

  TMvPluginClass = class of TMvPlugin;

  TMvPluginList = class(TMvIndexedComponentList)
  private
    function GetItem(AIndex: Integer): TMvPlugin;
    procedure SetItem(AIndex: Integer; AValue: TMvPlugin);
  public
    property Items[AIndex: Integer]: TMvPlugin read GetItem write SetItem; default;
  end;

  { TMvPluginManager }

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
    procedure AfterDrawObjects(AMapView: TMapView; AMapEvent: TNotifyEvent;
                               out Handled : Boolean); override;
    procedure AfterPaint(AMapView: TMapView; AMapEvent: TNotifyEvent;
                         out Handled : Boolean); override;
    procedure BeforeDrawObjects(AMapView: TMapView; AMapEvent: TNotifyEvent;
                                out Handled : Boolean); override;
    procedure CenterMove(AMapView: TMapView; AMapEvent: TNotifyEvent); override;
    procedure MouseDown(AMapView: TMapView; AButton: TMouseButton;
                        AShift: TShiftState; X, Y: Integer; AMapEvent: TMouseEvent;
                        out Handled : Boolean); override;
    procedure MouseEnter(AMapView: TMapView; AMapEvent: TNotifyEvent;
                         out Handled : Boolean); override;
    procedure MouseLeave(AMapView: TMapView; AMapEvent: TNotifyEvent;
                         out Handled : Boolean); override;
    procedure MouseMove(AMapView: TMapView; AShift: TShiftState; X,Y: Integer;
                        AMapEvent: TMouseMoveEvent; out Handled : Boolean); override;
    procedure MouseUp(AMapView: TMapView; AButton: TMouseButton;
                      AShift: TShiftState; X, Y: Integer; AMapEvent: TMouseEvent;
                      out Handled : Boolean); override;
    procedure ZoomChange(AMapView: TMapView; AMapEvent: TNotifyEvent); override;
//    procedure ZoomChanging(AMapView: TMapView; NewZoom: Integer; var Allow: Boolean; AMapEvent); override;
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

procedure TMvPlugin.CenterMove(AMapView: TMapView; var Handled: Boolean);
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

procedure TMvPlugin.ZoomChange(AMapView: TMapView; var Handled: Boolean);
begin
end;

{
procedure TMvPlugin.ZoomChanging(AMapView: TMapView; NewZoom: Integer;
  var Allow, Handled: Boolean);
begin
end;
}

{ TMvMultiMapsPluginData }

function TMvMultiMapsPluginData.GetDataSize: Integer;
begin
  Result := Length(FData);
end;

procedure TMvMultiMapsPluginData.SetData(const AData; const ADataSize: Integer);
begin
  SetLength(FData,ADataSize);
  if ADataSize > 0 then
    Move(AData,FData[0],ADataSize);
end;

function TMvMultiMapsPluginData.GetData(out AData; const AMaxDataSize: Integer
  ): Integer;
var
  ds : Integer;
begin
  Result := 0;
  ds := DataSize;
  if ds > AMaxDataSize then
    ds := AMaxDataSize;
  if ds <= 0 then Exit;
  Byte(AData) := 0; // Keep the compiler calm
  Move(FData[0],AData,ds);
  Result := ds;
end;

function TMvMultiMapsPluginData.GetDataPtr: Pointer;
begin
  Result := Nil;
  if Length(FData) > 0 then
    Result := @FData[0];
end;


{ TMvMultiMapsPlugin }

function TMvMultiMapsPlugin.GetMapViewDataIndex(Value: TMapView): Integer;
var
  i : Integer;
begin
  Result := -1;
  for i := 0 to FMapDataList.Count-1 do
    if TMvMultiMapsPluginData(FMapDataList.Items[i]).FMapView = Value then
      Exit(i);
end;

function TMvMultiMapsPlugin.GetMapViewDataSize(Value: TMapView): Integer;
var
  ndx : Integer;
begin
  Result := 0;
  ndx := MapViewDataIndex[Value];
  if ndx < 0 then Exit;
  Result := TMvMultiMapsPluginData(FMapDataList.Items[ndx]).DataSize;
end;

function TMvMultiMapsPlugin.GetMapViewDataPtr(Value: TMapView): Pointer;
var
  ndx : Integer;
  di : TMvMultiMapsPluginData;
begin
  Result := Nil;
  ndx := MapViewDataIndex[Value];
  if (ndx < 0) then Exit;
  di := TMvMultiMapsPluginData(FMapDataList.Items[ndx]);
  Result := di.GetDataPtr;
end;

function TMvMultiMapsPlugin.CreateMultiMapsPluginData: TMvMultiMapsPluginData;
begin
  Result := TMvMultiMapsPluginData.Create;
end;

function TMvMultiMapsPlugin.GetMapViewData(const AMapView: TMapView; out AData;
  const AMaxDataSize: Integer): Integer;
var
  ds : Integer;
  ndx : Integer;
  di : TMvMultiMapsPluginData;
begin
  Result := 0;
  ndx := MapViewDataIndex[AMapView];
  if ndx < 0 then Exit;
  di := TMvMultiMapsPluginData(FMapDataList.Items[ndx]);
  ds := di.DataSize;
  if ds > AMaxDataSize then
    ds := AMaxDataSize;
  if ds <= 0 then Exit;
  di.GetData(AData,ds);
  Result := ds;
end;

procedure TMvMultiMapsPlugin.SetMapViewData(const AMapView: TMapView;
  const AData; const ADataSize: Integer);
var
  ndx : Integer;
  di : TMvMultiMapsPluginData;
begin
  ndx := MapViewDataIndex[AMapView];
  if ndx < 0 then
  begin
    di := CreateMultiMapsPluginData;
    di.MapView := AMapView;
    ndx := FMapDataList.Add(di);
  end
  else
    di := TMvMultiMapsPluginData(FMapDataList.Items[ndx]);
  di.SetData(AData,ADataSize);
end;

constructor TMvMultiMapsPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMapDataList := TObjectList.Create(True);
end;

destructor TMvMultiMapsPlugin.Destroy;
begin
  if Assigned(FMapDataList) then
    FMapDataList.Free;
  inherited Destroy;
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

procedure TMvPluginManager.AfterDrawObjects(AMapView: TMapView;
  AMapEvent: TNotifyEvent; out Handled: Boolean);
var
  i: Integer;
  plugin: TMvPlugin;
  lHandled : Boolean;

begin
  Handled := False;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := Item[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.AfterDrawObjects(AMapView, Handled);
  end;
  // EDo muss weg, geht sonst nicht!!   if not handled then
    inherited AfterDrawObjects(AMapView, AMapEvent, lHandled);
end;

procedure TMvPluginManager.AfterPaint(AMapView: TMapView;
  AMapEvent: TNotifyEvent; out Handled: Boolean);
var
  i: Integer;
  plugin: TMvPlugin;
  lHandled : Boolean;

begin
  Handled := False;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := Item[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.AfterPaint(AMapView, Handled);
  end;
  // EDo muss weg, geht sonst nicht!!   if not handled then
    inherited AfterPaint(AMapView, AMapEvent, lHandled);
end;

procedure TMvPluginManager.BeforeDrawObjects(AMapView: TMapView;
  AMapEvent: TNotifyEvent; out Handled: Boolean);
var
  i: Integer;
  plugin: TMvPlugin;
  lHandled : Boolean;

begin
  handled := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := Item[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.BeforeDrawObjects(AMapView, Handled);
  end;
  // EDo muss weg, geht sonst nicht!!   if not handled then
    inherited BeforeDrawObjects(AMapView, AMapEvent, lHandled);
end;

procedure TMvPluginManager.CenterMove(AMapView: TMapView; AMapEvent: TNotifyEvent);
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
      plugin.CenterMove(AMapView, handled);
  end;
  if not handled then
    inherited CenterMove(AMapView, AMapEvent);
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
  AShift: TShiftState; X, Y: Integer; AMapEvent: TMouseEvent; out
  Handled: Boolean);
var
  i: Integer;
  plugin: TMvPlugin;
  lHandled : Boolean;

begin
  Handled := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := Item[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.MouseDown(AMapView, AButton, AShift, X, Y, Handled);
  end;
// EDo muss weg, geht sonst nicht!!     if (not handled) then
    inherited MouseDown(AMapView, AButton, AShift, X, Y, AMapEvent, lHandled);
end;

procedure TMvPluginManager.MouseEnter(AMapView: TMapView;
  AMapEvent: TNotifyEvent; out Handled: Boolean);
var
  i: Integer;
  plugin: TMvPlugin;
  lHandled : Boolean;

begin
  Handled := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := Item[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.MouseEnter(AMapView, handled);
  end;
  // EDo muss weg, geht sonst nicht!!   if not handled then
    inherited MouseEnter(AMapView, AMapEvent, lHandled);
end;

procedure TMvPluginManager.MouseLeave(AMapView: TMapView;
  AMapEvent: TNotifyEvent; out Handled: Boolean);
var
  i: Integer;
  plugin: TMvPlugin;
  lHandled : Boolean;

begin
  handled := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := Item[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.MouseLeave(AMapView, Handled);
  end;
  // EDo muss weg, geht sonst nicht!!   if not handled then
    inherited MouseLeave(AMapView, AMapEvent, lHandled);
end;

procedure TMvPluginManager.MouseMove(AMapView: TMapView; AShift: TShiftState;
  X, Y: Integer; AMapEvent: TMouseMoveEvent; out Handled: Boolean);
var
  i: Integer;
  plugin: TMvPlugin;
  lHandled : Boolean;
begin
  Handled := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := Item[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.MouseMove(AMapView, AShift, X, Y, Handled);
  end;
// EDo muss weg, geht sonst nicht!!     if (not handled) then
    inherited MouseMove(AMapView, AShift, X, Y, AMapEvent, lHandled);
end;

procedure TMvPluginManager.MouseUp(AMapView: TMapView; AButton: TMouseButton;
  AShift: TShiftState; X, Y: Integer; AMapEvent: TMouseEvent; out
  Handled: Boolean);
var
  i: Integer;
  plugin: TMvPlugin;
  lHandled : Boolean;

begin
  Handled := false;
  for i := 0 to FPluginList.Count-1 do
  begin
    plugin := Item[i];
    if HandlePlugin(plugin, AMapView) then
      plugin.MouseUp(AMapView, AButton, AShift, X, Y, Handled);
  end;
  // EDo muss weg, geht sonst nicht!!   if not handled then
    inherited MouseUp(AMapView, AButton, AShift, X, Y, AMapEvent, lHandled);
end;

procedure TMvPluginManager.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    if AComponent is TMapView then
      RemoveMapView(TMapView(AComponent));
      {
    if AComponent is TMvPlugin then
    begin
      AComponent.Free;
      FPluginList.Remove(AComponent);
    end;
    }
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

procedure TMvPluginManager.ZoomChange(AMapView: TMapView; AMapEvent: TNotifyEvent);
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
      plugin.ZoomChange(AMapView, handled);
  end;
  if not handled then
    inherited ZoomChange(AMapView, AMapEvent);
end;

                     (*
procedure TMvPluginManager.ZoomChanging(AMapView: TMapView; NewZoom: Integer;
  var Allow: Boolean; AMapEvent: TNotifyEvent);
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
      plugin.ZoomChanging(AMapView, NewZoom, Allow, handled);
  end;
  if not handled then
    inherited ZoomChanging(AMapView, NewZoom, Allow, AMapEvent);
end;                   *)


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

