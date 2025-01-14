{
 uAreaSelectionPlugin

 Copyright (C) 2025 Ekkehard Domning (www.domis.de)

 License: modified LGPL with linking exception (like RTL, FCL and LCL)

 See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
 for details about the license.

 This plugin allows the use to select an area on the map.
 The Plugin can be used only on *one* MapView, so it is mandatory to assign a
 MapView to the MapView property.
 The plugin is able to deal with the Cyclic property of the MapView. This allows
 the selection of "WrapAround"-Areas, which are changing the Date-border.
 Those areas have the property that the
 BottomRight.Lon is *smaller* than the TopLeft.Lon!
 Example:
 The Area
   TopLeft.Lon     = 13.4 (Berlin)
   BottomRight.Lon = 0.0 (London)
 wraps around the Date-Border, while
 the Area
    TopLeft.Lon     = 0.0 (London)
    BottomRight.Lon = 13.4 (Berlin)
 does not!
 Two events are implemented. One is called if the selection of a new area is in
 progress, the other when the selection is finished.
 The user can change the rectangle by dragging some anchors with the mouse.
 Those anchors are the 4 lines, the 4 corners and an small area around the top line.
 The method of changing is indicated by the mouse pointer.
 Assigning a new SelectedArea while changing is not supported
}
unit uAreaSelectionPlugin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics,Contnrs,
  mvPluginCommon, mvPlugins, mvMapViewer, mvTypes, mvGeoMath, mvDrawingEngine;

type
  { class TMouseHitItem
    A helper class to hold a single detectable Item, Like corner, edge, etc.
    Not intended to be used outside this unit.
  }
  TMouseHitItem = class(TObject)
  private
    FTag : Integer;
    FZOrder : Integer;
    FActiveRect : TRect;
    FCursor : TCursor;
    FItemHit : Boolean;
    FMouseDownFlag : Boolean;
    FOrgX, FOrgY : Integer;
    FMouseDownX, FMouseDownY : Integer;
    FMouseDeltaX, FMouseDeltaY : Integer;
  public
    property Tag : Integer read FTag write FTag;
    property ZOrder : Integer read FZOrder write FZOrder;
    property ActiveRect : TRect read FActiveRect write FActiveRect;
    property Cursor : TCursor read FCursor  write FCursor;
    property OrgX : Integer read FOrgX;
    property OrgY : Integer read FOrgY;
    property ItemHit : Boolean read FItemHit;
    property MouseDownFlag : Boolean read FMouseDownFlag write FMouseDownFlag;
    property MouseDownX : Integer read FMouseDownX;
    property MouseDownY : Integer read FMouseDownY;
    property MouseDeltaX : Integer read FMouseDeltaX;
    property MouseDeltaY : Integer read FMouseDeltaY;
    procedure OnMouseDown(Shift: TShiftState; X, Y: Integer);
    procedure OnMouseMove(Shift: TShiftState; X,
      Y: Integer);
    procedure OnMouseUp(Shift: TShiftState; X, Y: Integer);
    constructor Create;
    constructor Create(const AActiveRect : TRect;
                       const AOrgX : Integer;
                       const AOrgY : Integer;
                       const AZOrder : Integer = 0;
                       const ACursor : TCursor = crSize;
                       const ATag : Integer = 0);

  end;
const
  DefaultAreaSelectionPluginPenColor = clNavy;
  DefaultAreaSelectionPluginPenStyle = psSolid;
  DefaultAreaSelectionPluginPenWidth = 3;
  DefaultAreaSelectionPluginSensitiveAreaInflation = 2; // this increases the pixels of the sensitive items


type
  TSelectedAreaChangingEvent = procedure (Sender : TObject; ANewArea : TRealArea; var Allow : Boolean) of Object;
  { TAreaSelectionPlugin }
  TAreaSelectionPlugin = class(TMvPlugin)
  private
    FMouseHitItems : TObjectList; // The list with the clickable items
    FMouseButton : TMouseButton;
    FShifterXInverseMode : Boolean; // Two Flags to ease the user interface for the flat and cylindrical world
    FShifterYInverseMode : Boolean;
    FSelectedArea : TRealArea;
    FLastMouseMoveHandled : Boolean;
    FPenColor : TColor;
    FPenStyle : TPenStyle;
    FPenWidth : Integer;
    FAreaInflation : Integer;
    FMouseMapCoords : TRealPoint;
    FSelectedAreaChangedEvent : TNotifyEvent;
    FSelectedAreaChangingEvent : TSelectedAreaChangingEvent;
    function GetCurrentItem : TMouseHitItem;
    function GetItemsCount: Integer;
    function GetItems(AIndex : Integer) : TMouseHitItem;
    procedure SetMouseButton(AValue: TMouseButton);
    procedure SetPenColor(Value : TColor);
    procedure SetPenWidth(Value : Integer);
    procedure SetPenStyle(Value : TPenStyle);
    procedure SetSensitiveAreaInflation(Value : Integer);
    procedure SetSelectedArea(Value : TRealArea);
  protected
    property ItemsCount : Integer read GetItemsCount;
    property Items[AIndex : Integer] : TMouseHitItem read GetItems;
    { property CurrentItem returns either the Item where the Mouse is down or the one where the mouse is hovering above }
    property CurrentItem : TMouseHitItem read GetCurrentItem;

    procedure AddSelectionArea(const ARect : TRect;
                           const AInflate : Integer);
    procedure AddSelectionAreaEx(const ARect : TRect;
                           const ARectParts : array of Integer;
                           const AInflate : Integer);
    procedure AfterPaint(AMapView: TMapView; var Handled: Boolean); override;
    procedure MouseMove(AMapView: TMapView; AShift: TShiftState; X,Y: Integer;
          var Handled: Boolean); override;
    procedure MouseUp(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
          X,Y: Integer; var Handled: Boolean); override;
    procedure MouseDown(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
          X,Y: Integer; var Handled: Boolean); override;
    procedure CenterMove(AMapView: TMapView; var Handled: Boolean); override;
    procedure ZoomChange(AMapView: TMapView; var Handled: Boolean); override;
    procedure Resize(AMapView: TMapView; var Handled: Boolean); override;
  published
    property MouseButton : TMouseButton read FMouseButton write SetMouseButton;
    property PenColor : TColor read FPenColor write SetPenColor default DefaultAreaSelectionPluginPenColor;
    property PenWidth : Integer read FPenWidth write SetPenWidth default DefaultAreaSelectionPluginPenWidth;
    property PenStyle : TPenStyle read FPenStyle write SetPenStyle default DefaultAreaSelectionPluginPenStyle;
    property SensitiveAreaInflation : Integer read FAreaInflation write SetSensitiveAreaInflation default DefaultAreaSelectionPluginSensitiveAreaInflation;
    property OnSelectedAreaChanged : TNotifyEvent read FSelectedAreaChangedEvent write FSelectedAreaChangedEvent;
    property OnSelectedAreaChanging : TSelectedAreaChangingEvent read FSelectedAreaChangingEvent write FSelectedAreaChangingEvent;
  public
    { property SelectedArea the selected area }
    property SelectedArea : TRealArea read FSelectedArea write SetSelectedArea;
    { procedure Clear the list of all items. Usually called in MouseUp and prior the rebuilding of a new setup }
    procedure Clear;
    { procedure SetupRectShifter creates the mouse shifting items, must be called if the display changes
      needed e.g.if the size of the MapView is altered}
    procedure SetupRectShifter;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
  end;

implementation
const
  tagRectArea        = 1;
  tagRectLeft        = 2;
  tagRectTop         = 3;
  tagRectRight       = 4;
  tagRectBottom      = 5;
  tagRectTopLeft     = 6;
  tagRectTopRight    = 7;
  tagRectBottomRight = 8;
  tagRectBottomLeft  = 9;

{function RealAreaWidth calculates the width of an area on earth.
   if the BottomRight.Lon is smaller than the TopLeft.Lon, the
   area is treated as wrapping around the date border}
function RealAreaWidth(const ARealArea : TRealArea) : Double;
begin
  Result := ARealArea.BottomRight.Lon-ARealArea.TopLeft.Lon;
  if ARealArea.BottomRight.Lon < ARealArea.TopLeft.Lon then
    Result := Result + 360.0;
end;

procedure RealAreaNormed(var ARealArea : TRealArea);
begin
  if ARealArea.TopLeft.Lat > 90.0 then
    ARealArea.TopLeft.Lat := 90.0
  else if ARealArea.TopLeft.Lat < -90.0 then
    ARealArea.TopLeft.Lat := -90.0;
  if ARealArea.TopLeft.Lon < -180.0 then
    ARealArea.TopLeft.Lon := -180.0
  else if ARealArea.TopLeft.Lon > 180.0 then
    ARealArea.TopLeft.Lon := 180.0;
  if ARealArea.BottomRight.Lat > 90.0 then
    ARealArea.BottomRight.Lat := 90.0
  else if ARealArea.BottomRight.Lat < -90.0 then
    ARealArea.BottomRight.Lat := -90.0;
  if ARealArea.BottomRight.Lon < -180.0 then
    ARealArea.BottomRight.Lon := -180.0
  else if ARealArea.BottomRight.Lon > 180.0 then
    ARealArea.BottomRight.Lon := 180.0;
end;

{ function IsInRectangle returns true, if the passed X and Y coords are inside the rectangle
   Parameter:
     ARect : The rectangle to test
     AX, AY : The coords to test
     AInflate : The amount of inflating the rectangle
   Remarks: The function works different from the definition in uLazQuadTreeGeometry.
     The rectangle here is treated having the right and bottom border as part of the
     rectangle! This is needed, because the right and bottom border must be hit for
     resizing.
}
function IsInRectangle(const ARect : TRect; const AX, AY : Integer; const AInflate : Integer = 0): Boolean;
var
  r : TRect;
begin
  r := ARect;
  if AInflate > 0 then
    r.Inflate(AInflate,AInflate);
  Result := (AX >= r.Left) and (AX <= r.Right) and
            (AY >= r.Top) and (AY <= r.Bottom);
end;

{ TMouseHitItem }

procedure TMouseHitItem.OnMouseDown(Shift: TShiftState; X,
  Y: Integer);
begin
  Unused(Shift);
  if IsInRectangle(FActiveRect, X, Y, 0) then
  begin
    FMouseDownFlag := True;
    FMouseDeltaX := 0;
    FMouseDeltaY := 0;
    FMouseDownX := X;
    FMouseDownY := Y;
  end;
end;

procedure TMouseHitItem.OnMouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  Unused(Shift);
  if FMouseDownFlag then
  begin
    FMouseDeltaX := X-FMouseDownX;
    FMouseDeltaY := Y-FMouseDownY;
  end
  else
  begin
    FMouseDeltaX := 0;
    FMouseDeltaY := 0;
    FItemHit := IsInRectangle(FActiveRect, X, Y, 0);
  end;
end;

procedure TMouseHitItem.OnMouseUp(Shift: TShiftState; X,
  Y: Integer);
begin
  Unused(Shift);
  if FMouseDownFlag then
  begin
    FMouseDeltaX := X-FMouseDownX;
    FMouseDeltaY := Y-FMouseDownY;
    FMouseDownFlag := False;
  end
  else
  begin
    FMouseDeltaX := 0;
    FMouseDeltaY := 0;
  end;
end;

constructor TMouseHitItem.Create;
begin
  inherited Create;
end;

constructor TMouseHitItem.Create(const AActiveRect: TRect;
  const AOrgX: Integer; const AOrgY: Integer; const AZOrder: Integer;
  const ACursor: TCursor; const ATag: Integer);
begin
  Create();
  FActiveRect := AActiveRect;
  FOrgX :=  AOrgX;
  FOrgY :=  AOrgY;
  FZOrder :=  AZOrder;
  FCursor :=  ACursor;
  FTag :=  ATag;
end;


{ TAreaSelectionPlugin }

function TAreaSelectionPlugin.GetCurrentItem: TMouseHitItem;
var
  i : Integer;
  lItem : TMouseHitItem;
begin
  Result := Nil;
  for i := 0 to ItemsCount-1 do
  begin
    lItem := TMouseHitItem(FMouseHitItems[i]);
    if lItem.FMouseDownFlag then
    begin
      if (not Assigned(Result)) or
         (Result.ZOrder < lItem.ZOrder) then
        Result := lItem;
    end;
  end;
  if Assigned(Result) then Exit;
  for i := 0 to ItemsCount-1 do
  begin
    lItem := TMouseHitItem(FMouseHitItems[i]);
    if lItem.ItemHit then
    begin
      if (not Assigned(Result)) or
         (Result.ZOrder < lItem.ZOrder) then
        Result := lItem;
    end;
  end;
end;

function TAreaSelectionPlugin.GetItemsCount: Integer;
begin
  Result := FMouseHitItems.Count;
end;

function TAreaSelectionPlugin.GetItems(AIndex: Integer): TMouseHitItem;
begin
  Result := TMouseHitItem(FMouseHitItems[AIndex]);
end;

procedure TAreaSelectionPlugin.SetMouseButton(AValue: TMouseButton);
begin
  if FMouseButton=AValue then Exit;
  FMouseButton:=AValue;
end;

procedure TAreaSelectionPlugin.SetPenColor(Value: TColor);
begin
  if FPenColor <> Value then
  begin
    FPenColor := Value;
    if Assigned(MapView) then
      MapView.Invalidate;
  end;
end;

procedure TAreaSelectionPlugin.SetPenWidth(Value: Integer);
begin
  if FPenWidth <> Value then
  begin
    FPenWidth := Value;
    if Assigned(MapView) then
      MapView.Invalidate;
  end;
end;

procedure TAreaSelectionPlugin.SetPenStyle(Value: TPenStyle);
begin
  if FPenStyle <> Value then
  begin
    FPenStyle := Value;
    if Assigned(MapView) then
      MapView.Invalidate;
  end;
end;

procedure TAreaSelectionPlugin.SetSensitiveAreaInflation(Value: Integer);
begin
  if FAreaInflation <> Value then
  begin
    FAreaInflation := Value;
    if Assigned(MapView) then
      MapView.Invalidate;
  end;
end;

procedure TAreaSelectionPlugin.SetSelectedArea(Value: TRealArea);
begin
  if not FSelectedArea.Equal(Value) then
  begin
    FSelectedArea.Init(Value.TopLeft,Value.BottomRight);
    MapView.Invalidate;
    SetupRectShifter;
  end;
end;

procedure TAreaSelectionPlugin.Clear;
begin
  FMouseHitItems.Clear;
end;

procedure TAreaSelectionPlugin.AddSelectionArea(const ARect: TRect;
  const AInflate: Integer);
begin
  AddSelectionAreaEx(ARect,
                 [tagRectArea,tagRectLeft, tagRectTop, tagRectRight, tagRectBottom,
                  tagRectTopLeft, tagRectTopRight, tagRectBottomRight, tagRectBottomLeft],
                 AInflate);
end;

procedure TAreaSelectionPlugin.AddSelectionAreaEx(const ARect: TRect;
  const ARectParts: array of Integer; const AInflate: Integer);



var
  lMaxExtentRect : TRect;
  lRect : TRect;
  i : Integer;
  llr : TRect;
  lItem : TMouseHitItem = Nil;
begin
  lMaxExtentRect.Left := 0;
  lMaxExtentRect.Top := 0;
  lMaxExtentRect.Right := MapView.Width;
  lMaxExtentRect.Bottom := MapView.Height;
  lRect := ARect;
  if ARect.Left < lMaxExtentRect.Left then
    lRect.Left := lMaxExtentRect.Left
  else if ARect.Left > lMaxExtentRect.Right then
    lRect.Left := lMaxExtentRect.Right;
  if ARect.Right < lMaxExtentRect.Left then
    lRect.Right := lMaxExtentRect.Left
  else if ARect.Right > lMaxExtentRect.Right then
    lRect.Right := lMaxExtentRect.Right;
  if ARect.Top < lMaxExtentRect.Top then
    lRect.Top := lMaxExtentRect.Top
  else if ARect.Top > lMaxExtentRect.Bottom then
    lRect.Top := lMaxExtentRect.Bottom;
  if ARect.Bottom < lMaxExtentRect.Top then
    lRect.Bottom := lMaxExtentRect.Top
  else if ARect.Bottom > lMaxExtentRect.Bottom then
    lRect.Bottom := lMaxExtentRect.Bottom;

  for i := 0 to High(ARectParts) do
  begin
    case ARectParts[i] of
      tagRectArea :
        begin
          llr.Left:=lRect.Left;
          llr.Top:=lRect.Top-3*AInflate;
          llr.Right:=lRect.Right;
          llr.Bottom:=lRect.Top+3*AInflate;
          lItem := TMouseHitItem.Create(llr,ARect.Left,ARect.Top,0,crSize,
                                        tagRectArea);
        end;
      tagRectLeft :
        begin
          // Left Border
          if (ARect.Left > lMaxExtentRect.Left-(2*AInflate)) and
             (ARect.Left < lMaxExtentRect.Right+(2*AInflate)) then
          begin
            llr.Left:=ARect.Left-AInflate;
            llr.Top:=lRect.Top;
            llr.Right:=ARect.Left+AInflate;
            llr.Bottom:=lRect.Bottom;
            lItem := TMouseHitItem.Create(llr,ARect.Left,ARect.Top,1,crSizeWE,
                                          tagRectLeft);
          end;
        end;
      tagRectTop :
        begin
          // Top Border
          if (ARect.Top > lMaxExtentRect.Top-(2*AInflate)) and
             (ARect.Top < lMaxExtentRect.Bottom-(2*AInflate)) then
          begin
            llr.Left:=lRect.Left;
            llr.Top:=ARect.Top-AInflate;
            llr.Right:=lRect.Right;
            llr.Bottom:=ARect.Top+AInflate;
            lItem := TMouseHitItem.Create(llr,ARect.Left,ARect.Top,1,crSizeNS,
                                          tagRectTop);
          end;
        end;
      tagRectRight :
        begin
          // Right Border
          if (ARect.Right > lMaxExtentRect.Left-(2*AInflate)) and
             (ARect.Right < lMaxExtentRect.Right+(2*AInflate)) then
          begin
            llr.Left:=ARect.Right-AInflate;
            llr.Top:=lRect.Top;
            llr.Right:=ARect.Right+AInflate;
            llr.Bottom:=lRect.Bottom;
            lItem := TMouseHitItem.Create(llr,ARect.Right,ARect.Top,1,crSizeWE,
                                          tagRectRight);
          end;
        end;
      tagRectBottom :
        begin
          // Bottom border
          if (ARect.Bottom > lMaxExtentRect.Top-(2*AInflate)) and
             (ARect.Bottom < lMaxExtentRect.Bottom-(2*AInflate)) then
          begin
            llr.Left:=lRect.Left;
            llr.Top:=ARect.Bottom-AInflate;
            llr.Right:=lRect.Right;
            llr.Bottom:=ARect.Bottom+AInflate;
            lItem := TMouseHitItem.Create(llr,ARect.Left,ARect.Bottom,1,crSizeNS,
                                          tagRectBottom);
          end;
        end;
      tagRectTopLeft :
        begin
          // Northwest edge
          if (ARect.Left > lMaxExtentRect.Left-(2*AInflate)) and
             (ARect.Left < lMaxExtentRect.Right+(2*AInflate)) and
             (ARect.Top > lMaxExtentRect.Top-(2*AInflate)) and
             (ARect.Top < lMaxExtentRect.Bottom-(2*AInflate)) then
          begin
            llr.Left := ARect.Left;
            llr.Top := ARect.Top;
            llr.Right := ARect.Left;
            llr.Bottom := ARect.Top;
            llr.Inflate(AInflate,AInflate);
            lItem := TMouseHitItem.Create(llr,ARect.Left,ARect.Top,2,crSizeNWSE,
                                          tagRectTopLeft);
         end;
        end;
      tagRectTopRight :
        begin
          // NorthEast edge
          if (ARect.Right > lMaxExtentRect.Left-(2*AInflate)) and
             (ARect.Right < lMaxExtentRect.Right+(2*AInflate)) and
             (ARect.Top > lMaxExtentRect.Top-(2*AInflate)) and
             (ARect.Top < lMaxExtentRect.Bottom-(2*AInflate)) then
          begin
            llr.Left := ARect.Right;
            llr.Top := ARect.Top;
            llr.Right := ARect.Right;
            llr.Bottom := ARect.Top;
            llr.Inflate(AInflate,AInflate);
            lItem := TMouseHitItem.Create(llr,ARect.Right,ARect.Top,2,crSizeNESW,
                                          tagRectTopRight);
          end;
        end;
      tagRectBottomRight :
        begin
          // SouthEast edge
          if (ARect.Right > lMaxExtentRect.Left-(2*AInflate)) and
             (ARect.Right < lMaxExtentRect.Right+(2*AInflate)) and
             (ARect.Bottom > lMaxExtentRect.Top-(2*AInflate)) and
             (ARect.Bottom < lMaxExtentRect.Bottom-(2*AInflate)) then
          begin
            llr.Left := ARect.Right;
            llr.Top := ARect.Bottom;
            llr.Right := ARect.Right;
            llr.Bottom := ARect.Bottom;
            llr.Inflate(AInflate,AInflate);
            lItem := TMouseHitItem.Create(llr,ARect.Right,ARect.Bottom,2,crSizeNWSE,
                                          tagRectBottomRight);
         end;
        end;
      tagRectBottomLeft :
        begin
          // SouthWest edge
          if (ARect.Left > lMaxExtentRect.Left-(2*AInflate)) and
             (ARect.Left < lMaxExtentRect.Right+(2*AInflate)) and
             (ARect.Bottom > lMaxExtentRect.Top-(2*AInflate)) and
             (ARect.Bottom < lMaxExtentRect.Bottom-(2*AInflate)) then
          begin
            llr.Left := ARect.Left;
            llr.Top := ARect.Bottom;
            llr.Right := ARect.Left;
            llr.Bottom := ARect.Bottom;
            llr.Inflate(AInflate,AInflate);
            lItem := TMouseHitItem.Create(llr,ARect.Left,ARect.Bottom,2,crSizeNESW,
                                          tagRectBottomLeft);
          end;
        end;
    end;
    if Assigned(lItem) then
      FMouseHitItems.Add(lItem);
    lItem := Nil;
  end;
end;

procedure TAreaSelectionPlugin.AfterPaint(AMapView: TMapView; var Handled: Boolean);
procedure IncRecToMinSize(var ARect : TRect);
begin
  if Abs(ARect.Left-ARect.Right) < FPenWidth then
    Inc(ARect.Right,FPenWidth);
  if Abs(ARect.Top-ARect.Bottom) < FPenWidth then
    Inc(ARect.Bottom,FPenWidth);
end;

var
  r0 : TRect;
  mapw : Int64;
  topLeftPt, bottomRightPt : TPoint;
  rectW : Integer;
  lRect : TRect;
  ptArr : TPointArray;
  pt : TPoint;
begin
  Unused(AMapView, Handled);

  topLeftPt := MapView.LatLonToScreen(FSelectedArea.TopLeft.Lat, FSelectedArea.TopLeft.Lon);
  lRect.Top := topLeftPt.Y;
  lRect.Left := topLeftPt.X;
  bottomRightPt := MapView.LatLonToScreen(FSelectedArea.BottomRight.Lat, FSelectedArea.BottomRight.Lon);
  lRect.Bottom := bottomRightPt.Y;
  lRect.Right := bottomRightPt.X;
  mapw := mvGeoMath.ZoomFactor(MapView.Zoom) * TileSize.CX;
  if lRect.Left > lRect.Right then
    lRect.Left := lRect.Left - mapw;
  rectW := lRect.Right-lRect.Left;

  MapView.Canvas.Brush.Style := bsClear;
  MapView.Canvas.Pen.Style := FPenStyle;
  MapView.Canvas.Pen.Color := FPenColor;
  MapView.Canvas.Pen.Width := FPenWidth;
  // Duplicate the rectangles to the dimmed doubled picture areas
  ptArr := MapView.CyclicPointsOf(topLeftPt);
  for pt in ptArr do
  begin
    r0.Left := pt.X;
    r0.Top := lRect.Top;
    r0.Right := pt.X+rectW;
    if r0.Right > MapView.Width+FPenWidth then
      r0.Right := MapView.Width+FPenWidth;
    r0.Bottom := lRect.Bottom;
    if r0.Bottom > MapView.Height+FPenWidth then
      r0.Bottom := MapView.Height+FPenWidth;
    IncRecToMinSize(r0);
    MapView.Canvas.Rectangle(r0);
  end;
  // Catch the case of the missing left rectangle (draw some rectangles again)
  ptArr := MapView.CyclicPointsOf(bottomRightPt);
  for pt in ptArr do
  begin
    r0.Left := pt.X-rectW;
    if r0.Left < -FPenWidth then
      r0.Left := -FPenWidth;
    r0.Top := lRect.Top;
    if r0.Top < -FPenWidth then
      r0.Top := -FPenWidth;
    r0.Right := pt.X;
    r0.Bottom := lRect.Bottom;
    if r0.Left = r0.Right then
      Inc(r0.Right);
    if r0.Top = r0.Bottom then
      Inc(r0.Bottom);
    IncRecToMinSize(r0);
    MapView.Canvas.Rectangle(r0);
  end;
end;

procedure TAreaSelectionPlugin.MouseMove(AMapView: TMapView; AShift: TShiftState;
  X, Y: Integer; var Handled: Boolean);
  // The following "PerformXXX" procedures, do the actial adjusting of the
  // side of the active rectangle.
  procedure PerformRightSide(const ANewX : Double; var ARect : TRealArea);
  begin
    if AMapView.Cyclic then
    begin
      ARect.BottomRight.Lon := ANewX;
      Exit;
    end;
    if FShifterXInverseMode then
    begin
      if (ANewX > ARect.BottomRight.Lon) then
        FShifterXInverseMode := False;
    end
    else
    begin
      if (ANewX < ARect.TopLeft.Lon) then
        FShifterXInverseMode := True;
    end;
    if FShifterXInverseMode then
      ARect.TopLeft.Lon := ANewX
    else
      ARect.BottomRight.Lon := ANewX;
  end;
  procedure PerformLeftSide(const ANewX : Double; var ARect : TRealArea);
  begin
    if AMapView.Cyclic then
    begin
      ARect.TopLeft.Lon := ANewX;
      Exit;
    end;
    if FShifterXInverseMode then
    begin
      if (ANewX < ARect.TopLeft.Lon) then
        FShifterXInverseMode := False;
    end
    else
    begin
      if (ANewX > ARect.BottomRight.Lon) then
        FShifterXInverseMode := True;
    end;
    if FShifterXInverseMode then
      ARect.BottomRight.Lon := ANewX
    else
      ARect.TopLeft.Lon := ANewX;
  end;
  procedure PerformTopSide(const ANewY : Double; var ARect : TRealArea);
  begin
    if FShifterYInverseMode then
    begin
      if (ANewY < ARect.TopLeft.Lat) then
        FShifterYInverseMode := False;
    end
    else
    begin
      if (ANewY > ARect.BottomRight.Lat) then
        FShifterYInverseMode := True;
    end;
    if FShifterYInverseMode then
      ARect.BottomRight.Lat := ANewY
    else
      ARect.TopLeft.Lat := ANewY;
  end;
  procedure PerformBottomSide(const ANewY : Double; var ARect : TRealArea);
  begin
    if FShifterYInverseMode then
    begin
      if (ANewY > ARect.BottomRight.Lat) then
        FShifterYInverseMode := False;
    end
    else
    begin
      if (ANewY < ARect.TopLeft.Lat) then
        FShifterYInverseMode := True;
    end;
    if FShifterYInverseMode then
      ARect.TopLeft.Lat := ANewY
    else
      ARect.BottomRight.Lat := ANewY;
  end;
var
  w, h : Double;
  lx,ly: Double;
  lRect0 : TRealArea;
  lHitItem : TMouseHitItem;
  ptR : TRealPoint;
  chgAllowed : Boolean;
var
  i : Integer;
  lItem : TMouseHitItem;
begin
  if Handled then Exit;
  for i := 0 to ItemsCount-1 do
  begin
    lItem := TMouseHitItem(FMouseHitItems[i]);
    lItem.OnMouseMove(AShift,X,Y);
  end;
  lHitItem := CurrentItem;
  Handled := Assigned(lHitItem);
  // Here we have to act carefully, since we should not change the Mouse Pointer
  // if we not had set him.
  if (not Handled) and FLastMouseMoveHandled then
    MapView.Cursor := crDefault;  // no hit, but hit previously, set the default cursor
  FLastMouseMoveHandled := Handled;
  if not Handled then Exit;
  // lHitItem will be assigned if one item is selected.
  MapView.Cursor := lHitItem.Cursor;  // Set the cursor to the hit items kind
  if lHitItem.MouseDownFlag then // if the mouse is down
  begin
    lRect0.Init(FSelectedArea.TopLeft,FSelectedArea.BottomRight);
    // Invert the axis to ease calculation
    lRect0.TopLeft.Lat := -lRect0.TopLeft.Lat;
    lRect0.BottomRight.Lat := -lRect0.BottomRight.Lat;
    FMouseMapCoords := MapView.ScreenToLatLon(Point(X,Y));
    ptR := MapView.ScreenToLatLon(Point(lHitItem.OrgX+lHitItem.MouseDeltaX,
                                        lHitItem.OrgY+lHitItem.MouseDeltaY));
    ptR.Lat := -ptR.Lat;
    lx := ptR.Lon;
    ly := ptR.Lat;
    // case of the several actions
    case (lHitItem.Tag and $F) of
      tagRectArea :
        begin
          // Special operation for the movement of the full rectangle.
          // Just locate the rectangle at the new position ...
          w := RealAreaWidth(lRect0);
          h := lRect0.BottomRight.Lat-lRect0.TopLeft.Lat;
          lRect0.TopLeft := ptR;
          lRect0.BottomRight.Lon := lRect0.TopLeft.Lon + w;
          lRect0.BottomRight.Lat := lRect0.TopLeft.Lat + h;
          // ... but disallow shifting beyond the borders
          if MapView.Cyclic then
          begin
            while lRect0.BottomRight.Lon >= 180.0 do
              lRect0.BottomRight.Lon := lRect0.BottomRight.Lon -360.0;
          end
          else
          begin
            // In the flat world model right and left are fixed borders
            if lRect0.BottomRight.Lon >= 180.0 then
              lRect0.TopLeft.Lon := 180.0 - w;
          end;
          // In the flat and cylindrical world model top and bottom are fixed borders
          lRect0.BottomRight.Lat := lRect0.TopLeft.Lat + h;
          if lRect0.BottomRight.Lat >= 90.0 then
            lRect0.TopLeft.Lat := 90.0 - h;
        end;
      tagRectLeft :
        begin
          PerformLeftSide(lx,lRect0);
        end;
      tagRectTop :
        begin
          PerformTopSide(ly,lRect0);
        end;
      tagRectRight :
        begin
          PerformRightSide(lx,lRect0);
        end;
      tagRectBottom :
        begin
          PerformBottomSide(ly,lRect0);
        end;
      tagRectTopLeft :
        begin
          PerformTopSide(ly,lRect0);
          PerformLeftSide(lx,lRect0);
        end;
      tagRectTopRight :
        begin
          PerformTopSide(ly,lRect0);
          PerformRightSide(lx,lRect0);
        end;
      tagRectBottomRight :
        begin
          PerformBottomSide(ly,lRect0);
          PerformRightSide(lx,lRect0);
        end;
      tagRectBottomLeft :
        begin
          PerformBottomSide(ly,lRect0);
          PerformLeftSide(lx,lRect0);
        end;
    end;
    // Now return adjust the changed coords and transform to the world coordinates
    // Invert the axis to ease calculation
    lRect0.TopLeft.Lat := -lRect0.TopLeft.Lat;
    lRect0.BottomRight.Lat := -lRect0.BottomRight.Lat;
    RealAreaNormed(lRect0);
    chgAllowed := True;
    if Assigned(FSelectedAreaChangingEvent) then
      FSelectedAreaChangingEvent(Self,lRect0,chgAllowed);
    if chgAllowed then
      FSelectedArea.Init(lRect0.TopLeft,lRect0.BottomRight);
    MapView.Invalidate; // redraw the map
  end;
end;


procedure TAreaSelectionPlugin.MouseUp(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
var
  i : Integer;
  lItem : TMouseHitItem;
begin
  Unused(AMapView);

  if Handled then Exit;
  if Button <> FMouseButton then Exit;
  for i := 0 to ItemsCount-1 do
  begin
    lItem := TMouseHitItem(FMouseHitItems[i]);
    lItem.OnMouseUp(Shift,X,Y);
  end;
  SetupRectShifter;  // Setup the HelperClass for the current setting
  Handled := True;
end;

procedure TAreaSelectionPlugin.MouseDown(AMapView: TMapView;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; var Handled: Boolean
  );
var
  i : Integer;
  lItem, lCurrItem : TMouseHitItem;
begin
  Unused(AMapView);

  if Handled then Exit;
  if Button <> FMouseButton then Exit;
  // Forward the MouseDown-Event to all Items
  for i := 0 to ItemsCount-1 do
  begin
    lItem := TMouseHitItem(FMouseHitItems[i]);
    lItem.OnMouseDown(Shift,X,Y);
  end;

  lCurrItem := GetCurrentItem;
  Handled := Assigned(lCurrItem); // Reserve this event for us (Prvent dragging the map)

  // Resetting all other Item's MouseDown-Flags but not from the current item.
  // Might not be necessary, but double tap ;-)
  if Assigned(lCurrItem) then
  begin
    for i := 0 to ItemsCount-1 do
    begin
      lItem := TMouseHitItem(FMouseHitItems[i]);
      if lItem = lCurrItem then Continue;
      if lItem.MouseDownFlag then
        lItem.FMouseDownFlag := False;
    end;
  end;
  // Reset the inverters
  FShifterXInverseMode := False;
  FShifterYInverseMode := False;
end;

procedure TAreaSelectionPlugin.CenterMove(AMapView: TMapView;
  var Handled: Boolean);
begin
  Unused(AMapView, Handled);

  SetupRectShifter;
end;

procedure TAreaSelectionPlugin.ZoomChange(AMapView: TMapView;
  var Handled: Boolean);
begin
  Unused(AMapView, Handled);

  SetupRectShifter;
end;

procedure TAreaSelectionPlugin.Resize(AMapView: TMapView; var Handled: Boolean);
begin
  SetupRectShifter;
end;

{ SetupRectShifter will fill the Helper Class for the mouse movement with the
    values, so that the mouse pointer will show sensitive items }
type
  TRectArr = array of TRect;

procedure TAreaSelectionPlugin.SetupRectShifter;
var
  r0 : TRect;
  mapw : Int64;
  topLeftPt, bottomRightPt : TPoint;
  rectW : Integer;
  pt : TPoint;
  lRect : TRect;
  ptArr : TPointArray;
  rArr : TRectArr = Nil;
  i : Integer;
  found : Boolean;
begin
  Clear;

  topLeftPt := MapView.LatLonToScreen(FSelectedArea.TopLeft.Lat, FSelectedArea.TopLeft.Lon);
  lRect.Top := topLeftPt.Y;
  lRect.Left := topLeftPt.X;
  bottomRightPt := MapView.LatLonToScreen(FSelectedArea.BottomRight.Lat, FSelectedArea.BottomRight.Lon);
  lRect.Bottom := bottomRightPt.Y;
  lRect.Right := bottomRightPt.X;
  mapw := mvGeoMath.ZoomFactor(MapView.Zoom) * TileSize.CX;
  if lRect.Left > lRect.Right then
    lRect.Left := lRect.Left - mapw;
  rectW := lRect.Right-lRect.Left;

  // Rectangles might be duplicated in the cyclic view
  ptArr := MapView.CyclicPointsOf(topLeftPt);
  SetLength(rArr,Length(ptArr));
  for i := 0 to High(ptArr) do
  begin
    pt := ptArr[i];
    r0.Left := pt.X;
    r0.Top := lRect.Top;
    r0.Right := pt.X+rectW;
    r0.Bottom := lRect.Bottom;
    rArr[i] := r0;
      AddSelectionArea(r0,FAreaInflation);
  end;

  // Catch the case of the missing left rectangle.
  ptArr := MapView.CyclicPointsOf(bottomRightPt);
  for pt in ptArr do
  begin
    r0.Left := pt.X-rectW;
    r0.Top := lRect.Top;
    r0.Right := pt.X;
    r0.Bottom := lRect.Bottom;
    found := False;
    for i := 0 to High(rArr) do
    begin
      if r0 = rArr[i] then
      begin
        found := True;
        Break;
      end;
    end;
    if not found then
      AddSelectionArea(r0,FAreaInflation);
  end;
end;

constructor TAreaSelectionPlugin.Create(AOwner: TComponent);
begin
  inherited;
  FMouseHitItems := TObjectList.Create(True);
  FSelectedArea.Init(-100,50,100,-50);
//  FSelectedArea.Init(0,0,0,0);
  FPenColor := DefaultAreaSelectionPluginPenColor;
  FPenWidth := DefaultAreaSelectionPluginPenWidth;
  FPenStyle := DefaultAreaSelectionPluginPenStyle;
  FAreaInflation := DefaultAreaSelectionPluginSensitiveAreaInflation;
end;

destructor TAreaSelectionPlugin.Destroy;
begin
  FreeAndNil(FMouseHitItems);
  inherited;
end;
initialization
  RegisterPluginClass(TAreaSelectionPlugin, 'Area selection');
end.

