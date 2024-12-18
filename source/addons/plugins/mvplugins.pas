unit mvPlugins;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  Graphics, Controls, LCLIntf, //LazLoggerBase,
  mvMapViewer, mvDrawingEngine, mvPluginCore, mvGPSObj, mvTypes;

type
  { TCenterMarkerPlugin - draws a cross in the map center }

  TCenterMarkerPlugin = class(TMvDrawPlugin)
  private
    const
      DEFAULT_MARKER_SIZE = 15;
  private
    FSize: Integer;
    procedure SetSize(AValue: Integer);
  protected
    procedure AfterDrawObjects(AMapView: TMapView; var {%H-}Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Pen;
    property Size: Integer read FSize write SetSize default DEFAULT_MARKER_SIZE;
  end;


  { TLinkedMapsPlugin - all linked maps use the same zoom and center point }

  TLinkedMapsPlugin = class(TMvPlugin)
  private
    FLocked: Integer;
  protected
    procedure CenterMove(AMapView: TMapView; var {%H-}Handled: Boolean); override;
    procedure ZoomChange(AMapView: TMapView; var {%H-}Handled: Boolean); override;
  end;


  { TLegalNoticePlugin - displays a clickable copyright text }

  TLegalNoticePosition = (lnpTopLeft, lnpTopRight, lnpBottomLeft, lnpBottomRight);

  TLegalNoticePlugin = class(TMvMultiMapsPlugin)
  private
    const
      DEFAULT_LEGALNOTICE_OPACITY = 0.55;
      DEFAULT_LEGALNOTICE_SPACING = 4;
  private
    FLegalNotice: String;
    FLegalNoticeURL: String;
    FBackgroundOpacity: Single;
    FPosition: TLegalNoticePosition;
    FFont: TFont;
    FSpacing: Integer;
    FBackgroundColor: TColor;
  private
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetBackgroundOpacity(AValue: Single);
    procedure SetFont(AValue: TFont);
    procedure SetLegalNotice(AValue: String);
    procedure SetLegalNoticeURL(AValue: String);
    procedure SetPosition(AValue: TLegalNoticePosition);
    procedure SetSpacing(AValue: Integer);
  protected
    procedure CalcClickableRect(AMapView: TMapView; out AClickableRect: TRect);
    procedure Changed(Sender: TObject);
  protected
    procedure AfterDrawObjects(AMapView: TMapView; var Handled: Boolean); override;
    procedure MouseDown(AMapView: TMapView; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); override;
    procedure MouseEnter(AMapView: TMapView; var Handled: Boolean); override;
    procedure MouseMove(AMapView: TMapView; {%H-}Shift: TShiftState; X, Y: Integer;
      var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clNone;
    property BackgroundOpacity: Single read FBackgroundOpacity write SetBackgroundOpacity default DEFAULT_LEGALNOTICE_OPACITY;  // 0..1
    property Font: TFont read FFont write SetFont;
    property LegalNotice: String read FLegalNotice write SetLegalNotice;
    property LegalNoticeURL: String read FLegalNoticeURL write SetLegalNoticeURL;
    property Position: TLegalNoticePosition read FPosition write SetPosition default lnpBottomRight;
    property Spacing: Integer read FSpacing write SetSpacing default DEFAULT_LEGALNOTICE_SPACING;
    // inherited properties
    property MapView;
  end;

  { TDraggableMarkerPlugin }

  TDraggableMarkerPlugin = class;
  TDraggableMarkerCanMoveEvent = function (Sender : TDraggableMarkerPlugin; AMarker : TGPSPoint) : Boolean of object;
  TDraggableMarkerMovedEvent = procedure (Sender : TDraggableMarkerPlugin; AMarker : TGPSPoint; AOrgPosition : TRealPoint) of object;

  { TDraggableMarkerData }
  PDraggableMarkerData = ^TDraggableMarkerData;
  TDraggableMarkerData = record
    FDraggableMarker : TGPSPoint;
    FOrgPosition : TRealPoint;
  end;

  TDraggableMarkerPlugin = class(TMvMultiMapsPlugin)
  private
    FDraggableMarkerCanMoveEvent : TDraggableMarkerCanMoveEvent;
    FDraggableMarkerMovedEvent : TDraggableMarkerMovedEvent;
    function GetFirstMarkerAtMousePos(const AMapView: TMapView; const AX, AY : Integer) : TGPSPoint;
  protected
    procedure MouseDown(AMapView: TMapView; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); override;
    procedure MouseMove(AMapView: TMapView; {%H-}AShift: TShiftState; X,Y: Integer;
      var Handled: Boolean); override;
    procedure MouseUp(AMapView: TMapView; {%H-}Button: TMouseButton; {%H-}Shift: TShiftState;
      {%H-}X, {%H-}Y: Integer; var Handled: Boolean); override;
  published
    property DraggableMarkerCanMoveEvent : TDraggableMarkerCanMoveEvent read FDraggableMarkerCanMoveEvent write FDraggableMarkerCanMoveEvent;
    property DraggableMarkerMovedEvent : TDraggableMarkerMovedEvent read FDraggableMarkerMovedEvent write FDraggableMarkerMovedEvent;
  public
//    property DraggableMarker : TGPSPoint read FDraggableMarker;
//    property OrgPosition : TRealPoint read FOrgPosition;
    procedure Assign(Source: TPersistent); override;
  end;

type
  TMvPluginCenterMovingEvent = procedure (Sender: TObject; AMapView: TMapView;
    var ANewCenter: TRealPoint; var Allow, Handled: Boolean) of object;

  TMvPluginDrawGPSPointEvent = procedure (Sender: TObject; AMapView: TMapView;
    ADrawingEngine: TMvCustomDrawingEngine; APoint: TGPSPoint; var Handled: Boolean) of object;

  TMvPluginDrawMissingTileEvent = procedure (Sender: TObject; AMapView: TMapView;
    ADrawingEngine: TMvCustomDrawingEngine; const ARect: TRect; var Handled: Boolean) of object;

  TMvPluginGPSItemsModifiedEvent = procedure (Sender: TObject; AMapView: TMapView;
    ChangedList: TGPSObjectList; ActualObjs: TGPSObjList; Adding: Boolean;
    var Handled: Boolean) of Object;

  TMvPluginNotifyEvent = procedure (Sender : TObject; AMapView: TMapView;
    var Handled: Boolean) of Object;

  TMvPluginMouseEvent = procedure (Sender : TObject; AMapView: TMapView; Button: TMouseButton;
    AShift: TShiftState; X, Y: Integer; var Handled: Boolean) of Object;

  TMvPluginMouseMoveEvent = procedure (Sender : TObject; AMapView: TMapView;
    AShift: TShiftState; X,Y: Integer; var Handled: Boolean) of Object;

  TMvPluginMouseWheelEvent = procedure (Sender: TObject; AMapView: TMapView;
    AShift: TShiftState; AWheelDelta: Integer; AMousePos: TPoint; var Handled: Boolean) of object;

  TMvPluginZoomChangingEvent = procedure (Sender: TObject; AMapView: TMapView;
    NewZoom: Integer; var Allow, Handled: Boolean) of object;

  { TUserDefinedPlugin }

  TUserDefinedPlugin = class(TMvCustomPlugin)
  private
    FAfterDrawObjectsEvent : TMvPluginNotifyEvent;
    FAfterPaintEvent : TMvPluginNotifyEvent;
    FBeforeDrawObjectsEvent : TMvPluginNotifyEvent;
    FCenterMoveEvent : TMvPluginNotifyEvent;
    FCenterMovingEvent: TMvPluginCenterMovingEvent;
    FDrawGPSPointEvent: TMvPluginDrawGPSPointEvent;
    FDrawMissingTileEvent: TMvPluginDrawMissingTileEvent;
    FGPSItemsModifiedEvent : TMvPluginGPSItemsModifiedEvent;
    FMouseDownEvent : TMvPluginMouseEvent;
    FMouseEnterEvent : TMvPluginNotifyEvent;
    FMouseLeaveEvent : TMvPluginNotifyEvent;
    FMouseMoveEvent : TMvPluginMouseMoveEvent;
    FMouseUpEvent : TMvPluginMouseEvent;
    FMouseWheelEvent : TMvPluginMouseWheelEvent;
    FZoomChangeEvent : TMvPluginNotifyEvent;
    FZoomChangingEvent : TMvPluginZoomChangingEvent;
  protected
    procedure AfterDrawObjects(AMapView: TMapView; var Handled: Boolean); override;
    procedure AfterPaint(AMapView: TMapView; var Handled: Boolean); override;
    procedure BeforeDrawObjects(AMapView: TMapView; var Handled: Boolean); override;
    procedure CenterMove(AMapView: TMapView; var Handled: Boolean); override;
    procedure CenterMoving(AMapView: TMapView; var NewCenter: TRealPoint;
      var Allow, Handled: Boolean); override;
    procedure DrawGPSPoint(AMapView: TMapView; ADrawingEngine: TMvCustomDrawingEngine;
      APoint: TGPSPoint; var Handled: Boolean); override;
    procedure DrawMissingTile(AMapView: TMapView; ADrawingEngine: TMvCustomDrawingEngine;
      const ARect: TRect; var Handled: Boolean); override;
    procedure GPSItemsModified(AMapView: TMapView; ChangedList: TGPSObjectList;
      ActualObjs: TGPSObjList; Adding: Boolean; var Handled: Boolean); override;
    procedure MouseDown(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); override;
    procedure MouseEnter(AMapView: TMapView; var Handled: Boolean); override;
    procedure MouseLeave(AMapView: TMapView; var Handled: Boolean); override;
    procedure MouseMove(AMapView: TMapView; AShift: TShiftState; X,Y: Integer;
      var Handled: Boolean); override;
    procedure MouseUp(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); override;
    procedure MouseWheel(AMapView: TMapView; AShift: TShiftState;
      AWheelDelta: Integer; AMousePos: TPoint; var Handled: Boolean); override;
    procedure ZoomChange(AMapView: TMapView; var Handled: Boolean); override;
    procedure ZoomChanging(AMapView: TMapView; NewZoom: Integer; var Allow, Handled: Boolean); override;
  public
  published
    property OnAfterDrawObjects : TMvPluginNotifyEvent read FAfterDrawObjectsEvent write FAfterDrawObjectsEvent;
    property OnAfterPaint : TMvPluginNotifyEvent read FAfterPaintEvent write FAfterPaintEvent;
    property OnBeforeDrawObjects : TMvPluginNotifyEvent read FBeforeDrawObjectsEvent write FBeforeDrawObjectsEvent;
    property OnCenterMove : TMvPluginNotifyEvent read FCenterMoveEvent write FCenterMoveEvent;
    property OnCenterMoving: TMvPluginCenterMovingEvent read FCenterMovingEvent write FCenterMovingEvent;
    property OnDrawGPSPoint: TMvPluginDrawGPSPointEvent read FDrawGPSPointEvent write FDrawGPSPointEvent;
    property OnGPSItemsModified : TMvPluginGPSItemsModifiedEvent read FGPSItemsModifiedEvent write FGPSItemsModifiedEvent;
    property OnMouseDown : TMvPluginMouseEvent read FMouseDownEvent write FMouseDownEvent;
    property OnMouseEnter : TMvPluginNotifyEvent read FMouseEnterEvent write FMouseEnterEvent;
    property OnMouseLeave : TMvPluginNotifyEvent read FMouseLeaveEvent write FMouseLeaveEvent;
    property OnMouseMove : TMvPluginMouseMoveEvent read FMouseMoveEvent write FMouseMoveEvent;
    property OnMouseUp : TMvPluginMouseEvent read FMouseUpEvent write FMouseUpEvent;
    property OnMouseWheel : TMvPluginMouseWheelEvent read FMouseWheelEvent write FMouseWheelEvent;
    property OnZoomChange : TMvPluginNotifyEvent read FZoomChangeEvent write FZoomChangeEvent;
    property OnZoomChanging: TMvPluginZoomChangingEvent read FZoomChangingEvent write FZoomChangingEvent;

    // inherited
    property Enabled;
    property MapView;
  end;

implementation

uses
  Types;

{ TCenterMargerPlugin }

constructor TCenterMarkerPlugin.Create(AOwner: TComponent);
begin
  inherited;
  FSize := DEFAULT_MARKER_SIZE;
end;

procedure TCenterMarkerPlugin.Assign(Source: TPersistent);
begin
  if Source is TCenterMarkerPlugin then
    FSize := TCenterMarkerPlugin(Source).Size;
  inherited;
end;

procedure TCenterMarkerPlugin.AfterDrawObjects(AMapView: TMapView;
  var Handled: Boolean);
var
  C: TPoint;
begin
  C := Point(AMapView.ClientWidth div 2, AMapView.ClientHeight div 2);
  AMapView.DrawingEngine.PenColor := Pen.Color;
  AMapView.DrawingEngine.PenStyle := Pen.Style;
  AMapView.DrawingEngine.PenWidth := Pen.Width;
  AMapView.DrawingEngine.Opacity := 1.0;
  AMapView.DrawingEngine.Line(C.X, C.Y - FSize, C.X, C.Y + FSize);
  AMapView.DrawingEngine.Line(C.X - FSize, C.Y, C.X + FSize, C.Y);
end;

procedure TCenterMarkerPlugin.SetSize(AValue: Integer);
begin
  if FSize <> AValue then
  begin
    FSize := AValue;
    Update;
  end;
end;


{ TLinkedMapsPlugin }

procedure TLinkedMapsPlugin.CenterMove(AMapView: TMapView; var Handled: Boolean);
var
  i: Integer;
  map: TMapView;
begin
  if FLocked > 0 then
    exit;
  inc(FLocked);
  try
    for i := 0 to PluginManager.MapList.Count-1 do
    begin
      map := TMapView(PluginManager.MapList[i]);
      if AMapView <> map then
        map.Center := AMapView.Center;
    end;
  finally
    dec(FLocked);
  end;
end;

procedure TLinkedMapsPlugin.ZoomChange(AMapView: TMapView; var Handled: Boolean);
var
  i: Integer;
  map: TMapView;
  zoomToCrs: Boolean;
begin
  if FLocked > 0 then
    exit;
  inc(FLocked);
  try
    for i := 0 to PluginManager.MapList.Count-1 do
    begin
      map := TMapView(PluginManager.MapList[i]);
      if AMapView <> map then
      begin
        zoomToCrs := map.ZoomToCursor;
        try
          map.ZoomToCursor := false;
          map.Zoom := AMapView.Zoom;
        finally
          map.ZoomToCursor := zoomToCrs;
        end;
      end;
    end;
  finally
    dec(FLocked);
  end;
end;

                  (*
procedure TLinkedMapsPlugin.ZoomChanging(AMapView: TMapView;
  var NewZoom, Handled: Boolean);
var
  i: integer;
  map: TMapView;
begin
  if FLocked > 0 then
    exit;
  inc(FLocked);
  try
    for i := = to PluginManager.MapList.Count-1 do
    begin
      map := TMapView(PluginManager.MapList[i]);
      if AMapView <> map then
        map.ZoomChanging(NewZoom, Allow);
        *)


{ TLegalNoticePlugin }

constructor TLegalNoticePlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBackgroundColor := clNone;
  FPosition := lnpBottomRight;
  FFont := TFont.Create;
  FFont.OnChange := @Changed;
  FBackgroundOpacity := DEFAULT_LEGALNOTICE_OPACITY;
  FSpacing := DEFAULT_LEGALNOTICE_SPACING;
end;

destructor TLegalNoticePlugin.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TLegalNoticePlugin.Assign(Source: TPersistent);
begin
  if Source is TLegalNoticePlugin then
  begin
    FBackgroundColor := TLegalNoticePlugin(Source).BackgroundColor;
    FBackgroundOpacity := TLegalNoticePlugin(Source).BackgroundOpacity;
    FFont.Assign(TLegalNoticePlugin(Source).Font);
    FLegalNotice := TLegalNoticePlugin(Source).LegalNotice;
    FLegalNoticeURL := TLegalNoticePlugin(Source).LegalNoticeURL;
    FPosition := TLegalNoticePlugin(Source).Position;
    FSpacing := TLegalNoticePlugin(Source).Spacing;
  end;
  inherited;
end;

procedure TLegalNoticePlugin.AfterDrawObjects(AMapView: TMapView; var Handled: Boolean);
var
  x, y: Integer;
  lClickableRect: TRect;
  lSavedFont: TMvFont;
  lSavedOpacity: Single;
begin
  if not Assigned(AMapView) then Exit;
  Handled := True;
  CalcClickableRect(AMapView,lClickableRect);
  x := lClickableRect.Left;
  y := lClickableRect.Top;
  lSavedFont := AMapView.DrawingEngine.GetFont;
  lSavedOpacity := AMapView.DrawingEngine.Opacity;
  try
    if FBackgroundColor <> clNone then
    begin
      AMapView.DrawingEngine.Opacity := FBackgroundOpacity;
      AMapView.DrawingEngine.BrushStyle := bsSolid;
      AMapView.DrawingEngine.BrushColor := ColorToRGB(FBackgroundColor);
      with lClickableRect do
        AMapView.DrawingEngine.FillRect(Left, Top, Right, Bottom);
    end;
    AMapView.DrawingEngine.BrushStyle := bsClear;
    AMapView.DrawingEngine.SetFont(FFont.Name, FFont.Size, FFont.Style, FFont.Color);
    AMapView.DrawingEngine.TextOut(x, y, FLegalNotice);
  finally
    AMapView.DrawingEngine.Opacity := lSavedOpacity;
    AMapView.DrawingEngine.SetFont(lSavedFont);
  end;
end;

procedure TLegalNoticePlugin.CalcClickableRect(AMapView: TMapView; out
  AClickableRect: TRect);
var
  sz: TSize;
  x, y: Integer;
  lSavedFont: TMvFont;
begin
  lSavedFont := AMapView.DrawingEngine.GetFont;
  try
    AMapView.DrawingEngine.SetFont(FFont.Name, FFont.Size, FFont.Style, FFont.Color);
    sz := AMapView.DrawingEngine.TextExtent(FLegalNotice);
    case FPosition of
      lnpTopLeft, lnpBottomLeft:
        x := FSpacing;
      lnpTopRight, lnpBottomRight:
        x := AMapView.Width - sz.CX - FSpacing;
    end;
    case FPosition of
      lnpTopLeft, lnpTopRight:
        y := FSpacing;
      lnpBottomLeft, lnpBottomRight:
        y := AMapView.Height - sz.CY - FSpacing;
    end;
    AClickableRect := Rect(x, y, x + sz.CX, y + sz.CY);
    SetMapViewData(AMapView,AClickableRect,SizeOf(AClickableRect));
  finally
    AMapView.DrawingEngine.SetFont(lSavedFont);
  end;
end;

procedure TLegalNoticePlugin.Changed(Sender: TObject);
begin
  Update;
end;

procedure TLegalNoticePlugin.MouseDown(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
var
  pt: TPoint;
  lClickableRect : TRect;
begin
  // The button down event is consumed by a different plugin, so do nothing here
  if Handled then Exit;
  pt.X := X;
  pt.Y := Y;
  if GetMapViewData(AMapView,lClickableRect,SizeOf(lClickableRect)) < SizeOf(lClickableRect) then
    CalcClickableRect(AMapView,lClickableRect);
  if PtInRect(lClickableRect, pt) and (FLegalNoticeURL <> '') then
  begin
    // The button down event is consumed by this plugin
    OpenURL(FLegalNoticeURL);
    Handled := True;
  end;
end;

procedure TLegalNoticePlugin.MouseEnter(AMapView: TMapView; var Handled: Boolean);
var
  lClickableRect : TRect;
begin
  inherited;
  CalcClickableRect(AMapView,lClickableRect);
end;

procedure TLegalNoticePlugin.MouseMove(AMapView: TMapView; Shift: TShiftState;
  X, Y: Integer; var Handled: Boolean);
var
  lClickableRect : TRect;
begin
  if GetMapViewData(AMapView,lClickableRect,SizeOf(lClickableRect)) < SizeOf(lClickableRect) then
    CalcClickableRect(AMapView, lClickableRect);

  if PtInRect(lClickableRect, Point(X, Y)) and (not AMapView.Engine.InDrag) and
     (FLegalNoticeURL <> '') then
  begin
    FFont.Style := [fsUnderline];
    AMapView.Cursor := crHandPoint;
    Handled := true;
  end else
  begin
    FFont.Style := [];
    if not Handled then
      AMapView.Cursor := crDefault;
  end;
  Update;
end;

procedure TLegalNoticePlugin.SetPosition(AValue: TLegalNoticePosition);
begin
  if FPosition = AValue then Exit;
  FPosition := AValue;
  Update;
end;

procedure TLegalNoticePlugin.SetLegalNotice(AValue: String);
begin
  if FLegalNotice = AValue then Exit;
  FLegalNotice := AValue;
  Update;
end;

procedure TLegalNoticePlugin.SetLegalNoticeURL(AValue: String);
begin
  if FLegalNoticeURL = AValue then Exit;
  FLegalNoticeURL := AValue;
  Update;
end;

procedure TLegalNoticePlugin.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor = AValue then Exit;
  FBackgroundColor := AValue;
  Update;
end;

procedure TLegalNoticePlugin.SetBackgroundOpacity(AValue: Single);
begin
  if FBackgroundOpacity = AValue then Exit;
  FBackgroundOpacity := AValue;
  Update;
end;

procedure TLegalNoticePlugin.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
  Update;
end;

procedure TLegalNoticePlugin.SetSpacing(AValue: Integer);
begin
  if FSpacing = AValue then Exit;
  FSpacing := AValue;
  Update;
end;

function TDraggableMarkerPlugin.GetFirstMarkerAtMousePos(const AMapView: TMapView;
  const AX, AY: Integer): TGPSPoint;
var
  pt : TPoint;
  aArea : TRealArea;
  cs2 : Integer;
  lGpsPt : TGpsPoint;
  gpsList: TGpsObjList;
  cnt : Integer;
  i : Integer;
begin
  Result := Nil;
  cs2 := 2;
  pt.X := AX-cs2;
  pt.Y := AY-cs2;
  aArea.TopLeft := AMapView.ScreenToLatLon(pt);
  pt.X := AX+cs2;
  pt.Y := AY+cs2;
  aArea.BottomRight := AMapView.ScreenToLatLon(pt);
  gpsList := AMapView.GPSItems.GetObjectsInArea(aArea);
  try
    cnt := gpsList.Count;
    if cnt < 1 then Exit;
    for i:=0 to gpsList.Count-1 do
    begin
      if (gpsList[i] is TGpsPoint)  then
      begin
        lGpsPt := TGpsPoint(gpsList[i]);
        if (not Assigned(FDraggableMarkerCanMoveEvent)) or
           FDraggableMarkerCanMoveEvent(Self,lGpsPt) then
        begin
          Result := lGpsPt;
          Exit;
        end;
      end;
    end;
  finally
    gpsList.Free;
  end;
end;

procedure TDraggableMarkerPlugin.MouseDown(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
var
  lDraggableMarkerData : TDraggableMarkerData;
begin
  if Handled then Exit;
  lDraggableMarkerData.FDraggableMarker := GetFirstMarkerAtMousePos(AMapView,X,Y);
  if Assigned(lDraggableMarkerData.FDraggableMarker) then
  begin
    lDraggableMarkerData.FOrgPosition.Lon:= lDraggableMarkerData.FDraggableMarker.Lon;
    lDraggableMarkerData.FOrgPosition.Lat:= lDraggableMarkerData.FDraggableMarker.Lat;
    SetMapViewData(AMapView,lDraggableMarkerData,SizeOf(lDraggableMarkerData));
    Handled := True;
  end;
end;

procedure TDraggableMarkerPlugin.MouseMove(AMapView: TMapView;
  AShift: TShiftState; X, Y: Integer; var Handled: Boolean);
var
  pt : TPoint;
  rpt : TRealPoint;
  ele : Double;
  dt : TDateTime;
  lDraggableMarkerData : TDraggableMarkerData;
  cnt : Integer;
begin
  cnt := GetMapViewData(AMapView,lDraggableMarkerData,SizeOf(lDraggableMarkerData));
  if (cnt >= SizeOf(lDraggableMarkerData)) and
     Assigned(lDraggableMarkerData.FDraggableMarker) then
  begin
    pt.X := X;
    pt.Y := Y;
    rpt := AMapView.ScreenToLatLon(pt);
    ele := lDraggableMarkerData.FDraggableMarker.Elevation;
    dt := lDraggableMarkerData.FDraggableMarker.DateTime;
    lDraggableMarkerData.FDraggableMarker.MoveTo(rpt.Lon, rpt.Lat,ele,dt);
    AMapView.Invalidate;
    Handled := True; // Prevent the dragging of the map!!
  end
  else
  begin
    if Assigned(GetFirstMarkerAtMousePos(AMapView,X,Y)) then
    begin
      AMapView.Cursor := crHandPoint;
      Handled := True;
    end
    else if not Handled then
      AMapView.Cursor := crDefault;
  end

end;

procedure TDraggableMarkerPlugin.MouseUp(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
var
  lpDraggableMarkerData : PDraggableMarkerData;
begin
  lpDraggableMarkerData := MapViewDataPtr[AMapView];
  if Assigned(lpDraggableMarkerData) and Assigned(lpDraggableMarkerData^.FDraggableMarker) then
  begin
    if Assigned(FDraggableMarkerMovedEvent) then
      FDraggableMarkerMovedEvent(Self,lpDraggableMarkerData^.FDraggableMarker,lpDraggableMarkerData^.FOrgPosition);
    Handled := True;
    lpDraggableMarkerData^.FDraggableMarker := Nil;
  end;
end;

procedure TDraggableMarkerPlugin.Assign(Source: TPersistent);
begin
  if Source is TDraggableMarkerPlugin then
  begin
    FDraggableMarkerCanMoveEvent := TDraggableMarkerPlugin(Source).DraggableMarkerCanMoveEvent;
    FDraggableMarkerMovedEvent := TDraggableMarkerPlugin(Source).DraggableMarkerMovedEvent;
  end;
  inherited;
end;

{ TMvCustomPlugin }

procedure TUserDefinedPlugin.AfterDrawObjects(AMapView: TMapView;
  var Handled: Boolean);
begin
  if Assigned(FAfterDrawObjectsEvent) then
    FAfterDrawObjectsEvent(Self, AMapView, Handled);
end;

procedure TUserDefinedPlugin.AfterPaint(AMapView: TMapView; var Handled: Boolean);
begin
  if Assigned(FAfterPaintEvent) then
    FAfterPaintEvent(Self, AMapView, Handled);
end;

procedure TUserDefinedPlugin.BeforeDrawObjects(AMapView: TMapView;
  var Handled: Boolean);
begin
  if Assigned(FBeforeDrawObjectsEvent) then
    FBeforeDrawObjectsEvent(Self, AMapView, Handled);
end;

procedure TUserDefinedPlugin.CenterMove(AMapView: TMapView; var Handled: Boolean);
begin
  if Assigned(FCenterMoveEvent) then
    FCenterMoveEvent(Self, AMapView, Handled);
end;

procedure TUserDefinedPlugin.CenterMoving(AMapView: TMapView;
  var NewCenter: TRealPoint; var Allow, Handled: Boolean);
begin
  if Assigned(FCenterMovingEvent) then
    FCenterMovingEvent(Self, AMapView, NewCenter, Allow, Handled);
end;

procedure TUserDefinedPlugin.DrawGPSPoint(AMapView: TMapView;
  ADrawingEngine: TMvCustomDrawingEngine; APoint: TGPSPoint; var Handled: Boolean);
begin
  if Assigned(FDrawGPSPointEvent) then
    FDrawGPSPointEvent(Self, AMapView, ADrawingEngine, APoint, Handled);
end;

procedure TUserDefinedPlugin.DrawMissingTile(AMapView: TMapView;
  ADrawingEngine: TMvCustomDrawingEngine; const ARect: TRect; var Handled: Boolean);
begin
  if Assigned(FDrawMissingTileEvent) then
    FDrawMissingTileEvent(Self, AMapView, ADrawingEngine, ARect, Handled);
end;

procedure TUserDefinedPlugin.GPSItemsModified(AMapView: TMapView;
  ChangedList: TGPSObjectList; ActualObjs: TGPSObjList; Adding: Boolean;
  var Handled: Boolean);
begin
  if Assigned(FGPSItemsModifiedEvent) then
    FGPSItemsModifiedEvent(Self, AMapView, ChangedList, ActualObjs, Adding, Handled);
end;

procedure TUserDefinedPlugin.MouseDown(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
begin
  if Assigned(FMouseDownEvent) then
    FMouseDownEvent(Self, AMapView, Button, Shift, X,Y, Handled);
end;

procedure TUserDefinedPlugin.MouseEnter(AMapView: TMapView; var Handled: Boolean);
begin
  if Assigned(FMouseEnterEvent) then
    FMouseEnterEvent(Self, AMapView, Handled);
end;

procedure TUserDefinedPlugin.MouseLeave(AMapView: TMapView; var Handled: Boolean);
begin
  if Assigned(FMouseLeaveEvent) then
    FMouseLeaveEvent(Self, AMapView, Handled);
end;

procedure TUserDefinedPlugin.MouseMove(AMapView: TMapView; AShift: TShiftState; X,
  Y: Integer; var Handled: Boolean);
begin
  if Assigned(FMouseMoveEvent) then
    FMouseMoveEvent(Self,AMapView, AShift, X, Y, Handled);
end;

procedure TUserDefinedPlugin.MouseUp(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
begin
  if Assigned(FMouseUpEvent) then
    FMouseUpEvent(Self, AMapView, Button, Shift, X, Y, Handled);
end;

procedure TUserDefinedPlugin.MouseWheel(AMapView: TMapView; AShift: TShiftState;
  AWheelDelta: Integer; AMousePos: TPoint; var Handled: Boolean);
begin
  if Assigned(FMouseWheelEvent) then
    FMouseWheelEvent(Self, AMapView, AShift, AWheelDelta, AMousePos, Handled);
end;

procedure TUserDefinedPlugin.ZoomChange(AMapView: TMapView; var Handled: Boolean);
begin
  if Assigned(FZoomChangeEvent) then
    FZoomChangeEvent(Self, AMapView, Handled);
end;

procedure TUserDefinedPlugin.ZoomChanging(AMapView: TMapView; NewZoom: Integer;
  var Allow, Handled: Boolean);
begin
  if Assigned(FZoomChangingEvent) then
    FZoomChangingEvent(Self, AMapView, NewZoom, Allow, Handled);
end;


initialization
  RegisterPluginClass(TCenterMarkerPlugin, 'Center marker');
  RegisterPluginClass(TLegalNoticePlugin, 'Legal notice');
  RegisterPluginClass(TLinkedMapsPlugin, 'Linked maps');
  RegisterPluginClass(TDraggableMarkerPlugin, 'Draggable marker');
  RegisterPluginClass(TUserDefinedPlugin, 'User-defined');

end.

