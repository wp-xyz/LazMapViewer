unit mvPlugins;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math,
  Graphics, Controls, LCLIntf, LazLoggerBase,
  mvMapViewer, mvDrawingEngine, mvPluginCore, mvGPSObj, mvGeoMath, mvTypes;

type
  { TCenterMarkerPlugin - draws a cross in the map center }

  TCenterMarkerPlugin = class(TMvPlugin)
  private
    const
      DEFAULT_MARKER_SIZE = 15;
  private
    FPen: TPen;
    FSize: Integer;
    procedure Changed(Sender: TObject);
    procedure SetPen(AValue: TPen);
    procedure SetSize(AValue: Integer);
  protected
    procedure AfterDrawObjects(AMapView: TMapView; var {%H-}Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Pen: TPen read FPen write SetPen;
    property Size: Integer read FSize write SetSize default DEFAULT_MARKER_SIZE;
  end;


  { TGridPlugin - draws a degrees grid }

  TGridPlugin = class;
  TMvGridLabelPosition = (glpLeft, glpTop, glpRight, glpBottom);
  TMvGridLabelPositions = set of TMvGridLabelPosition;

  TMvGridLabels = class(TPersistent)
  private
    FPlugin: TGridPlugin;
    FPosition: TMvGridLabelPositions;
    FDistance: Integer;
    procedure SetDistance(AValue: Integer);
    procedure SetPosition(AValue: TMvGridLabelpositions);
  protected
    procedure Update;
  public
    constructor Create(AOwner: TGridPlugin);
  published
    property Distance: Integer read FDistance write SetDistance default 0;
    property Position: TMvGridLabelPositions read FPosition write SetPosition default [glpLeft, glpTop];
  end;

  TGridPlugin = class(TMvPlugin)
  private
    const
      DEFAULT_MAX_DISTANCE = 200;
      DEFAULT_MIN_DISTANCE = 80;
    type
      TGridCoordType = (gctLatitude, gctLongitude);
  private
    FBackgroundColor: TColor;
    FFont: TFont;
    FIncrement: Double;
    FOpacity: Single;
    FPen: TPen;
    FGridLabels: TMvGridLabels;
    FMaxDistance: Integer;
    FMinDistance: Integer;
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetFont(AValue: TFont);
    procedure SetIncrement(AValue: Double);
    procedure SetMaxDistance(AValue: Integer);
    procedure SetMinDistance(AValue: Integer);
    procedure SetOpacity(AValue: Single);
    procedure SetPen(AValue: TPen);
  private
    procedure Changed(Sender: TObject);
  protected
    procedure AfterDrawObjects(AMapView: TMapView; var {%H-}Handled: Boolean); override;
    function CalcIncrement(AMapView: TMapView; Area: TRealArea): Double;
    function CalcVisibleArea(AMapView: TMapView): TRealArea;
    procedure DrawGridLine(ADrawingEngine: TMvCustomDrawingEngine;
      AValue: Double; P1, P2: TPoint; AMapRect: TRect);
    procedure DrawHorGridLines(AMapView: TMapView; Area: TRealArea; AMapRect: TRect; AIncrement: Double);
    procedure DrawVertGridlines(AMapView: TMapView; Area: TRealArea; AMapRect: TRect; AIncrement: Double);
    function GetLatLonAsString(AValue: Double; ACoordType: TGridCoordType): String; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clWhite;
    property Font: TFont read FFont write SetFont;
    property GridLabels: TMvGridLabels read FGridLabels write FGridLabels;
    property Increment: Double read FIncrement write SetIncrement; // 0 = automatic increment detection
    property MaxDistance: Integer read FMaxDistance write SetMaxDistance default DEFAULT_MAX_DISTANCE;
    property MinDistance: Integer read FMinDistance write SetMinDistance default DEFAULT_MIN_DISTANCE;
    property Opacity: Single read FOpacity write SetOpacity;
    property Pen: TPen read FPen write SetPen;
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
    FOpacity: Single;
    FPosition: TLegalNoticePosition;
    FFont: TFont;
    FSpacing: Integer;
    FBackgroundColor: TColor;
  private
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetFont(AValue: TFont);
    procedure SetLegalNotice(AValue: String);
    procedure SetLegalNoticeURL(AValue: String);
    procedure SetOpacity(AValue: Single);
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
    property Font: TFont read FFont write SetFont;
    property LegalNotice: String read FLegalNotice write SetLegalNotice;
    property LegalNoticeURL: String read FLegalNoticeURL write SetLegalNoticeURL;
    property Opacity: Single read FOpacity write SetOpacity default DEFAULT_LEGALNOTICE_OPACITY;
    property Position: TLegalNoticePosition read FPosition write SetPosition default lnpBottomRight;
    property Spacing: Integer read FSpacing write SetSpacing default DEFAULT_LEGALNOTICE_SPACING;
    // inherited properties
    property MapView;
  end;

  TDraggableMarkerPlugin = class;
  TDraggableMarkerCanMoveEvent = function (Sender : TDraggableMarkerPlugin; AMarker : TGPSPoint) : Boolean of object;
  TDraggableMarkerMovedEvent = procedure (Sender : TDraggableMarkerPlugin; AMarker : TGPSPoint; AOrgPosition : TRealPoint) of object;

  { TLegalNoticePluginData }
  PDraggableMarkerData = ^TDraggableMarkerData;
  TDraggableMarkerData = record
    FDraggableMarker : TGPSPoint;
    FOrgPosition : TRealPoint;
  end;

  { TDraggableMarkerPlugin }

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
  TMvPluginNotifyEvent = procedure (Sender : TObject; AMapView: TMapView; var Handled: Boolean) of Object;
  TMvPluginMouseEvent = procedure (Sender : TObject; AMapView: TMapView; Button: TMouseButton;
                                   Shift: TShiftState;
                                   X, Y: Integer; var Handled: Boolean) of Object;
  TMvPluginMouseMoveEvent = procedure (Sender : TObject; AMapView: TMapView; AShift: TShiftState;
                                       X,Y: Integer; var Handled: Boolean) of Object;
  TMvPluginGPSItemsModifiedEvent = procedure (Sender: TObject; AMapView: TMapView;
                                       ChangedList: TGPSObjectList;
                                       ActualObjs: TGPSObjList; Adding: Boolean;
                                       var Handled : Boolean) of Object;

  { TUserDefinedPlugin }

  TUserDefinedPlugin = class(TMvCustomPlugin)
  private
    FAfterDrawObjectsEvent : TMvPluginNotifyEvent;
    FAfterPaintEvent : TMvPluginNotifyEvent;
    FBeforeDrawObjectsEvent : TMvPluginNotifyEvent;
    FCenterMoveEvent : TMvPluginNotifyEvent;
    FMouseDownEvent : TMvPluginMouseEvent;
    FMouseEnterEvent : TMvPluginNotifyEvent;
    FMouseLeaveEvent : TMvPluginNotifyEvent;
    FMouseMoveEvent : TMvPluginMouseMoveEvent;
    FMouseUpEvent : TMvPluginMouseEvent;
    FZoomChangeEvent : TMvPluginNotifyEvent;
    FGPSItemsModifiedEvent : TMvPluginGPSItemsModifiedEvent;
  protected
    procedure AfterDrawObjects(AMapView: TMapView; var Handled: Boolean); override;
    procedure AfterPaint(AMapView: TMapView; var Handled: Boolean); override;
    procedure BeforeDrawObjects(AMapView: TMapView; var Handled: Boolean); override;
    procedure CenterMove(AMapView: TMapView; var Handled: Boolean); override;
    procedure MouseDown(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); override;
    procedure MouseEnter(AMapView: TMapView; var Handled: Boolean); override;
    procedure MouseLeave(AMapView: TMapView; var Handled: Boolean); override;
    procedure MouseMove(AMapView: TMapView; AShift: TShiftState; X,Y: Integer;
      var Handled: Boolean); override;
    procedure MouseUp(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); override;
    procedure ZoomChange(AMapView: TMapView; var Handled: Boolean); override;
    procedure GPSItemsModified(AMapView: TMapView; ChangedList: TGPSObjectList;
                               ActualObjs: TGPSObjList; Adding: Boolean;
                               var Handled : Boolean);override;
  public
  published
    property OnAfterDrawObjects : TMvPluginNotifyEvent read FAfterDrawObjectsEvent write FAfterDrawObjectsEvent;
    property OnAfterPaint : TMvPluginNotifyEvent read FAfterPaintEvent write FAfterPaintEvent;
    property OnBeforeDrawObjects : TMvPluginNotifyEvent read FBeforeDrawObjectsEvent write FBeforeDrawObjectsEvent;
    property OnCenterMove : TMvPluginNotifyEvent read FCenterMoveEvent write FCenterMoveEvent;
    property OnMouseDown : TMvPluginMouseEvent read FMouseDownEvent write FMouseDownEvent;
    property OnMouseEnter : TMvPluginNotifyEvent read FMouseEnterEvent write FMouseEnterEvent;
    property OnMouseLeave : TMvPluginNotifyEvent read FMouseLeaveEvent write FMouseLeaveEvent;
    property OnMouseMove : TMvPluginMouseMoveEvent read FMouseMoveEvent write FMouseMoveEvent;
    property OnMouseUp : TMvPluginMouseEvent read FMouseUpEvent write FMouseUpEvent;
    property OnZoomChange : TMvPluginNotifyEvent read FZoomChangeEvent write FZoomChangeEvent;
    property OnGPSItemsModified : TMvPluginGPSItemsModifiedEvent read FGPSItemsModifiedEvent write FGPSItemsModifiedEvent;

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
  FPen := TPen.Create;
  FPen.OnChange := @Changed;
  FSize := DEFAULT_MARKER_SIZE;
end;

destructor TCenterMarkerPlugin.Destroy;
begin
  FPen.Free;
  inherited;
end;

procedure TCenterMarkerPlugin.Assign(Source: TPersistent);
begin
  if Source is TCenterMarkerPlugin then
  begin
    FPen.Assign(TCenterMarkerPlugin(Source).Pen);
    FSize := TCenterMarkerPlugin(Source).Size;
  end;
  inherited;
end;

procedure TCenterMarkerPlugin.AfterDrawObjects(AMapView: TMapView;
  var Handled: Boolean);
var
  C: TPoint;
begin
  C := Point(AMapView.ClientWidth div 2, AMapView.ClientHeight div 2);
  AMapView.DrawingEngine.PenColor := FPen.Color;
  AMapView.DrawingEngine.PenStyle := FPen.Style;
  AMapView.DrawingEngine.PenWidth := FPen.Width;
  AMapView.DrawingEngine.Opacity := 1.0;
  AMapView.DrawingEngine.Line(C.X, C.Y - FSize, C.X, C.Y + FSize);
  AMapView.DrawingEngine.Line(C.X - FSize, C.Y, C.X + FSize, C.Y);
end;

procedure TCenterMarkerPlugin.Changed(Sender: TObject);
begin
  Update;
end;

procedure TCenterMarkerPlugin.SetPen(AValue: TPen);
begin
  FPen.Assign(AValue);
  Update;
end;

procedure TCenterMarkerPlugin.SetSize(AValue: Integer);
begin
  if FSize <> AValue then
  begin
    FSize := AValue;
    Update;
  end;
end;


{ TMvGridLabels }

constructor TMvGridLabels.Create(AOwner: TGridPlugin);
begin
  inherited Create;
  FPlugin := AOwner;
  FPosition := [glpLeft, glpTop];
end;

procedure TMvGridLabels.SetDistance(AValue: Integer);
begin
  if FDistance <> AValue then
  begin
    FDistance := AValue;
    FPlugin.Update;
  end;
end;

procedure TMvGridLabels.SetPosition(AValue: TMvGridLabelPositions);
begin
  if FPosition <> AValue then
  begin
    FPosition := AValue;
    Update;
  end;
end;

procedure TMvGridLabels.Update;
begin
  FPlugin.Update;
end;


{ TGridPlugin }

constructor TGridPlugin.Create(AOwner: TComponent);
begin
  inherited;
  FBackgroundColor := clWhite;
  FOpacity := 0.55;
  FFont := TFont.Create;
  FFont.OnChange := @Changed;
  FPen := TPen.Create;
  FPen.OnChange := @Changed;
  FGridLabels := TMvGridLabels.Create(Self);
  FMaxDistance := DEFAULT_MAX_DISTANCE;
  FMinDistance := DEFAULT_MIN_DISTANCE;
end;

destructor TGridPlugin.Destroy;
begin
  FGridLabels.Free;
  FPen.Free;
  FFont.Free;
  inherited;
end;

procedure TGridPlugin.AfterDrawObjects(AMapView: TMapView; var Handled: Boolean);
var
  area: TRealArea;
  coordType: TGridCoordType;
  incr: Double;
  mapRectPx: TRect;
begin
  area := CalcVisibleArea(AMapView);

  mapRectPx.TopLeft := AMapView.Engine.LatLonToScreen(area.TopLeft);
  mapRectPx.BottomRight := AMapView.Engine.LatLonToScreen(area.BottomRight);
  if AMapView.Cyclic then begin
    mapRectPx.Left := 0;
    mapRectPx.Right := AMapView.Width;
  end;

  if FIncrement <= 0.0 then
    incr := CalcIncrement(AMapView, area)
  else
    incr := FIncrement;

  AMapView.DrawingEngine.PenStyle := FPen.Style;
  AMapView.DrawingEngine.PenWidth := FPen.Width;
  AMapView.DrawingEngine.PenColor := FPen.Color;
  AMapView.DrawingEngine.BrushColor := FBackgroundColor;
  AMapView.DrawingEngine.FontName := FFont.Name;
  AMapView.DrawingEngine.FontSize := FFont.Size;
  AMapView.DrawingEngine.FontStyle := FFont.Style;
  AMapView.DrawingEngine.FontColor := FFont.Color;

  for coordType in TGridCoordType do
  begin
    case coordType of
      gctLatitude: DrawHorGridLines(AMapView, area, mapRectPx, incr);
      gctLongitude: DrawVertGridLines(AMapView, area, mapRectPx, incr);
    end;
  end;
end;

{ Calculates the increment of the grid lines. Since, due to the projection, the
  latitude lines have a non-constant spacing, their increment is taken from
  the equator value, i.e. is assumed to be equal to the longitude lines spacing. }
function TGridPlugin.CalcIncrement(AMapView: TMapView; Area: TRealArea): Double;
type
  TIncrement = (deg180, deg90, deg45, deg30, deg10, deg5, deg2, deg1,
    min30, min20, min10, min5, min2, min1,
    sec30, sec20, sec10, sec5, sec2, sec1);
const
  IncrementValues: array[TIncrement] of double = (
    180.0, 90.0, 45.0, 30.0, 10.0, 5.0, 2.5, 1.0,
    30.0/60, 20.0/60, 10.0/60, 5.0/60, 2.5/60, 1.0/60,
    30.0/3600, 20.0/3600, 10.0/3600, 5.0/3600, 2.5/3600, 1.0/3600);
var
  fullRange: Double;
  deg2px: Double;
  incr: TIncrement;
  incrementPx: Integer;
begin
  Result := 1E6;
  fullRange := abs(Area.BottomRight.Lon - Area.TopLeft.Lon);
  deg2px := fullRange / AMapView.ClientWidth;

  for incr in TIncrement do
  begin
    incrementPx := trunc(IncrementValues[incr] / deg2px);
    if (incrementPx >= MinDistance) and (incrementPx <= MaxDistance) then
    begin
      Result := IncrementValues[incr];
      break;
    end;
  end;
end;

{ Calculates the corner coordinates (in degrees) for the displayed map.
  The longitude values are expanded such that they increase monotonously
  from left to right. }
function TGridPlugin.CalcVisibleArea(AMapView: TMapView): TRealArea;
var
  worldWidth: Integer;
  numWorlds: Integer;
begin
  Result := AMapView.Engine.ScreenRectToRealArea(Rect(0, 0, AMapView.ClientWidth, AMapView.ClientHeight));
  if AMapView.Engine.CrossesDateline then
  begin
    if Result.BottomRight.Lon < Result.TopLeft.Lon then
      Result.BottomRight.Lon := Result.BottomRight.Lon + 360;
    worldWidth := mvGeoMath.ZoomFactor(AMapView.Zoom) * TileSize.CX;
    numWorlds := trunc(AMapView.ClientWidth / worldWidth);
    Result.BottomRight.Lon := Result.BottomRight.Lon + numWorlds * 360;
  end;
end;

procedure TGridPlugin.Changed(Sender: TObject);
begin
  Update;
end;

procedure TGridPlugin.DrawGridLine(ADrawingEngine: TMvCustomDrawingEngine;
  AValue: Double; P1, P2: TPoint; AMapRect: TRect);
var
  s: String;
  txtSize: TSize;
  txtRect: array[TMvGridLabelPosition] of TRect;
  glp: TMvGridLabelPosition;
  coordType: TGridCoordType;
begin
  if P1.Y = P2.Y then
  begin
    coordType := gctLatitude;
    if (P1.Y < AMapRect.Top) or (P2.Y > AMapRect.Bottom) then   // Pixel coordinates grow downwards
      exit;
  end else
  if P1.X = P2.X then
  begin
    coordType := gctLongitude;
    AValue := NormalizeLon(AValue);
    if (P1.X < AMapRect.Left) or (P2.X > AMapRect.Right) then
      exit;
  end else
    exit;

  // Calculate label text and position
  s := GetLatLonAsString(AValue, coordType);
  txtSize := ADrawingEngine.TextExtent(s);
  case coordType of
    gctLatitude:
      begin
        if (glpLeft in FGridLabels.Position) and InRange(P1.X, AMapRect.Left, AMapRect.Right) then
        begin
          txtRect[glpLeft] := Rect(0, 0, txtSize.CX, txtSize.CY);
          OffsetRect(txtRect[glpLeft], P1.X, P1.Y - txtSize.CY div 2);
        end else
          txtRect[glpLeft] := Rect(AMapRect.Left, P1.Y, AMapRect.Left, P1.Y);
        if (glpRight in FGridLabels.Position) and InRange(P1.X, AMapRect.Left, AMapRect.Right) then
        begin
          txtRect[glpRight] := Rect(0, 0, txtSize.CX, txtSize.CY);
          OffsetRect(txtRect[glpRight], P2.X - txtSize.CY, P1.Y - txtSize.CY div 2);
        end else
          txtRect[glpRight] := Rect(AMapRect.Right, P1.Y, AMapRect.Right, P1.Y);
      end;
    gctLongitude:
      begin
        if (glpTop in FGridLabels.Position) then
        begin
          txtRect[glpTop] := Rect(0, 0, txtSize.CX, txtSize.CY);
          OffsetRect(txtRect[glpTop], P1.X - txtSize.CX div 2, P1.Y);
        end else
          txtRect[glpTop] := Rect(P1.X, AMapRect.Top, P1.X, AMapRect.Top);
        if (glpBottom in FGridLabels.Position) then
        begin
          txtRect[glpBottom] := Rect(0, 0, txtSize.CX, txtSize.CY);
          OffsetRect(txtRect[glpBottom], P1.X - txtSize.CX div 2, P2.Y - txtSize.CY);
        end else
          txtRect[glpBottom] := Rect(P1.X, AMapRect.Bottom, P1.X, AMapRect.Bottom);
      end;
    end;
    for glp in TMvGridLabelPositions do
    begin
      if txtRect[glp].Top < AMapRect.Top then OffsetRect(txtRect[glp], 0, AMapRect.Top - txtRect[glp].Top);
      if txtRect[glp].Bottom > AMapRect.Bottom then OffsetRect(txtRect[glp], 0, AMapRect.Bottom - txtRect[glp].Bottom);
      if txtRect[glp].Left < AMapRect.Left then OffsetRect(txtRect[glp], AMapRect.Left - txtRect[glp].Left, 0);
      if txtRect[glp].Right > AMapRect.Right then OffsetRect(txtRect[glp], AMapRect.Right - txtRect[glp].Right, 0);
    end;

    // Draw latitude/longitude lines
    ADrawingEngine.Opacity := 1.0;
    case coordType of
      gctLatitude: ADrawingEngine.Line(txtRect[glpLeft].Right, P1.Y, txtRect[glpRight].Left, P2.Y);
      gctLongitude: ADrawingEngine.line(P1.X, txtRect[glpTop].Bottom, P2.X, txtRect[glpBottom].Top);
    end;

    // Draw (semi-transparent) background
    if FBackgroundColor <> clNone then
    begin
      ADrawingEngine.Opacity := FOpacity;
      ADrawingEngine.BrushStyle := bsSolid;
      case coordType of
        gctLatitude:
          begin
            if (glpLeft in FGridLabels.Position) then
              with txtRect[glpLeft] do
                ADrawingEngine.FillRect(Left, Top, Right, Bottom);
            if (glpRight in FGridLabels.Position) then
              with txtRect[glpRight] do
                ADrawingEngine.FillRect(Left, Top, Right, Bottom);
          end;
        gctLongitude:
          begin
            if (glpTop in FGridLabels.Position) then
              with txtRect[glpTop] do
                ADrawingEngine.FillRect(Left, top, Right, Bottom);
            if (glpBottom in FGridLabels.Position) then
              with txtRect[glpBottom] do
                ADrawingEngine.FillRect(Left, Top, Right, Bottom);
          end;
      end;
    end;

    // Draw grid coordinates as text
    ADrawingEngine.BrushStyle := bsClear;
    ADrawingEngine.Opacity := 1.0;
    case coordType of
      gctLatitude:
        begin
          if glpLeft in FGridlabels.Position then
            with txtRect[glpLeft] do
              ADrawingEngine.TextOut(Left, Top, s);
          if glpRight in FGridLabels.Position then
            with txtRect[glpRight] do
              ADrawingEngine.TextOut(Left, Top, s);
        end;
      gctLongitude:
        begin
          if glpTop in FGridLabels.Position then
            with txtRect[glpTop] do ADrawingEngine.TextOut(Left, Top, s);
          if glpBottom in FGridLabels.Position then
            with txtRect[glpBottom] do ADrawingEngine.TextOut(Left, Top, s);
        end;
    end;
end;

{ Draws horizontal grid lines (constant longitudes)
  * Area - Area covered by the map, in (monotonous) degrees
  * AMapRect - dto., in pixels.
  * AIncrement - distance between grid lines, in degrees. }
procedure TGridPlugin.DrawHorGridLines(AMapView: TMapView; Area: TRealArea;
  AMapRect: TRect; AIncrement: Double);
var
  rP1, rP2: TRealPoint;    // Top and bottom points of each gridline, in degrees
  P1, P2: TPoint;          // ... and in pixels
  lat: Double;
begin
  // Northern hemispere...
  lat := trunc(Area.BottomRight.Lat / AIncrement) * AIncrement;
  rP1 := RealPoint(lat, Area.TopLeft.Lon);
  rP2 := RealPoint(lat, Area.BottomRight.Lon);
  while (rP2.Lat <= Area.TopLeft.Lat) do
  begin
    P1 := AMapView.Engine.LatLonToScreen(rP1);
    P2 := AMapView.Engine.LatLonToScreen(rP2);
    if AMapView.Cyclic then
    begin
      P1.X := 0;
      P2.X := AMapView.ClientWidth;
    end;
    DrawGridLine(AMapView.DrawingEngine, rP1.Lat, P1, P2, AMapRect);
    rP1.Lat := rP1.Lat + AIncrement;
    rP2.Lat := rP2.Lat + AIncrement;
  end;

  // Southern hemisphere...
  rP1 := RealPoint(0.0, Area.TopLeft.Lon);
  rP2 := RealPoint(0.0, Area.BottomRight.Lon);
  while (rP2.Lat >= Area.BottomRight.Lat) do
  begin
    P1 := AMapView.Engine.LatLonToScreen(rP1);
    P2 := AMapView.Engine.LatLonToScreen(rP2);
    if AMapView.Cyclic then
    begin
      P1.X := 0;
      P2.X := AMapView.ClientWidth;
    end;
    DrawGridLine(AMapView.DrawingEngine, rP1.Lat, P1, P2, AMapRect);
    rP1.Lat := rP1.Lat - AIncrement;
    rP2.Lat := rP2.Lat - AIncrement;
  end;
end;

{ Draws vertical grid lines (constant longitude).
  * Area - Area covered by the map, in (monotonous) degrees
  * AMapRect - dto., in pixels.
  * AIncrement - distance between grid lines, in degrees. }
procedure TGridPlugin.DrawVertGridlines(AMapView: TMapView; Area: TRealArea;
  AMapRect: TRect; AIncrement: Double);
var
  rP1, rP2: TRealPoint;    // top and bottom points of each gridline, in degrees
  P1, P2: TPoint;          // ... and in pixels
  incrPx: integer;         // increment (distance between grid lines), in pixels
  lon: Double;             // longitude of grid line, in (monotonous) degrees
  lonPx, lonPxLeft, lonPxRight: Integer;  // longitude parameters and its limits, in pixels
  worldSize: Int64;
begin
  // Increment in pixels
  worldSize := mvGeoMath.ZoomFactor(AMapView.Zoom) * TileSize.CX;
  incrPx := round(AIncrement / 360 * worldSize);

  // Position of starting point
  lon := trunc(Area.TopLeft.Lon / AIncrement) * AIncrement;
  rP1 := RealPoint(Area.TopLeft.Lat, lon);
  rP2 := RealPoint(Area.BottomRight.Lat, lon);
  P1 := AMapView.Engine.LatLonToScreen(rP1);
  P2 := AMapView.Engine.LatLonToScreen(rP2);

  // Left and right pixel borders of the area to be handled.
  if AMapView.Cyclic then
  begin
    lonPxLeft := 0;
    lonPxRight := AMapView.Width;
  end else
  begin
    lonPxLeft := AMapRect.Left;
    lonPxRight := AMapRect.Right;
  end;

  // Find and draw vertical grid lines while going to the left
  lonPx := P1.X - incrPx;
  while lonPx > lonPxLeft do begin
    DrawGridLine(AMapView.DrawingEngine, lon, Point(lonPx, P1.Y), Point(lonPx, P2.Y), AMapRect);
    lon := lon - AIncrement;
    lonPx := lonPx - incrPx;
  end;

  // Find and draw vertical grid lines while going to the right
  lonPx := P1.X;
  while lonPx < lonPxRight do
  begin
    DrawGridLine(AMapView.DrawingEngine, lon, Point(lonPx, P1.Y), Point(lonPx, P2.Y), AMapRect);
    lon := lon + AIncrement;
    lonPx := lonPx + incrPx;
  end;
end;

function TGridPlugin.GetLatLonAsString(AValue: Double; ACoordType: TGridCoordType): String;
var
  degs, mins, secs: Double;
  secsZero: Boolean;
begin
  SplitGPS(AValue, degs, mins, secs);
  secsZero := SameValue(secs, 0.0, 2E-1);

  if (mins = 0) and secsZero then
    Result := Format('%.0f°', [abs(degs)])
  else
  if secsZero then
    Result := Format('%.0f° %.0f''', [abs(degs), mins])
  else
    Result := Format('%.0f° %.0f'' %.0f"', [abs(degs), mins, secs]);
  case ACoordType of
    gctLatitude:
      if AValue > 0 then
        Result := Result + ' N'
      else if AValue < 0 then
        Result := Result + ' S';
    gctLongitude:
      if (abs(AValue) <> 180) and (AValue <> 0) then
      begin
        if (AValue > 0) then
          Result := Result + ' E'
        else
          Result := Result + ' W';
      end;
  end;
end;

procedure TGridPlugin.SetBackgroundColor(AValue: TColor);
begin
  if FBackgroundColor <> AValue then
  begin
    FBackgroundColor := AValue;
    Update;
  end;
end;

procedure TGridPlugin.SetFont(AValue: TFont);
begin
  FFont := AValue;
  Update;
end;

procedure TGridPlugin.SetIncrement(AValue: Double);
begin
  if FIncrement <> AValue then
  begin
    FIncrement := AValue;
    Update;
  end;
end;

procedure TGridPlugin.SetMaxDistance(AValue: Integer);
begin
  if FMaxDistance <> AValue then
  begin
    FMaxDistance := AValue;
    Update;
  end;
end;

procedure TGridPlugin.SetMinDistance(AValue: Integer);
begin
  if FMinDistance <> AValue then
  begin
    FMinDistance := AValue;
    Update;
  end;
end;

procedure TGridPlugin.SetOpacity(AValue: Single);
begin
  if FOpacity <> AValue then
  begin
    FOpacity := AValue;
    Update;
  end;
end;

procedure TGridPlugin.SetPen(AValue: TPen);
begin
  FPen.Assign(AValue);
  Update;
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
  FOpacity := DEFAULT_LEGALNOTICE_OPACITY;
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
    FFont.Assign(TLegalNoticePlugin(Source).Font);
    FLegalNotice := TLegalNoticePlugin(Source).LegalNotice;
    FLegalNoticeURL := TLegalNoticePlugin(Source).LegalNoticeURL;
    FOpacity := TLegalNoticePlugin(Source).Opacity;
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
      AMapView.DrawingEngine.Opacity := FOpacity;
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
    CalcClickableRect(AMapView,lClickableRect);

  if PtInRect(lClickableRect, Point(X, Y)) and (not AMapView.Engine.InDrag) then
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

procedure TLegalNoticePlugin.SetOpacity(AValue: Single);
begin
  if FOpacity = AValue then Exit;
  FOpacity := AValue;
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

procedure TUserDefinedPlugin.MouseDown(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
begin
  if Assigned(FMouseDownEvent) then
    FMouseDownEvent(Self,AMapView, Button, Shift, X,Y, Handled);
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

procedure TUserDefinedPlugin.ZoomChange(AMapView: TMapView; var Handled: Boolean);
begin
  if Assigned(FZoomChangeEvent) then
    FZoomChangeEvent(Self, AMapView, Handled);
end;

procedure TUserDefinedPlugin.GPSItemsModified(AMapView: TMapView;
  ChangedList: TGPSObjectList; ActualObjs: TGPSObjList; Adding: Boolean;
  var Handled: Boolean);
begin
  if Assigned(FGPSItemsModifiedEvent) then
    FGPSItemsModifiedEvent(Self,AMapView, ChangedList, ActualObjs, Adding, Handled);
end;


initialization
  RegisterPluginClass(TCenterMarkerPlugin, 'Center marker');
  RegisterPluginClass(TLegalNoticePlugin, 'Legal notice');
  RegisterPluginClass(TLinkedMapsPlugin, 'Linked maps');
  RegisterPluginClass(TDraggableMarkerPlugin, 'Draggable marker');
  RegisterPluginClass(TUserDefinedPlugin, 'User-defined');

end.

