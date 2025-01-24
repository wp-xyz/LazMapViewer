{ This unit implement an MapViewer-Plugin to display a great circle on the map.
  For details about great circles please see:
  https://en.wikipedia.org/wiki/Great_circle
  The great circle is defined by a Start and a Destination coordinate.
  If Start and Destination are equal, no greatcircle is displayed.
  If Start and Destination are 0 or 180 degrees separated, the circle around
  the two poles are displayed.
  The part containing the shortest distance between Start and Destination is
  called Orthodrome.
  The display Pen for the Great Circle and the Orthodrome could be independently
  choosen.
  The great circle on the map are approximated by short straight lines. The
  length of those segments could be set. In general, the segments should be
  smaller on larger scales (smaller zoom) and could be longer on smaller scales
  (bigger zoom), since the distortions are bigger on large scales, especially
  in polar regions.
  The Z-Order could be selected in three steps:
  On the native map behind or before the markers (this allows to save the
  great circle together with the map in a file) and on the Canvas.
  The calculated points are accessible. The contain the location, the distance
  (in meter) from Start and the kind of the point (wether it is start or destination,
  is part of the Orthodrome or none of the above). The distance starts at 0 at
  Start, increases in direction of the Destination an continues increasing until
  Start is reached again.
  The points are only available for the Longitudes visible in the map!
}
unit uGreatCirclePainterPlugin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics, Math, Contnrs, ClipBrd,
  mvPluginCommon, mvPlugins, mvMapViewer, mvTypes, mvGeoMath, mvDrawingEngine;

const
  DefaultTruncLength = 10; // 10 pixels for one trunc = line segment on screen
  MaxTruncLength     = 200; // maximum 200 pixels

type
  TGreatCirclePointKind = (gcpkStandard, gcpkOrthodrome, gcpkStart, gcpkDestination);

  { TGreatCirclePoint }

  TGreatCirclePoint = class(TObject)
  private
    FRealPoint : TRealPoint;
    FDistance : Double;
    FPointKind : TGreatCirclePointKind;
  public
    property RealPoint : TRealPoint read FRealPoint;
    property PointKind : TGreatCirclePointKind read FPointKind;
    property Distance : Double read FDistance;
    constructor Create(const ARealPoint : TRealPoint; const ADistance : Double;
                       const APointKind : TGreatCirclePointKind = gcpkStandard);
    constructor Create(const ARealPointLat, ARealPointLon : Double; const ADistance : Double;
                       const APointKind : TGreatCirclePointKind = gcpkStandard);
  end;

  TGreatCirclePainterZOrder = (gcpzCanvas,gcpzInFrontOfMarkers,gcpzBehindMarkers);
//  TGreatCirclePainterOption = (gcpoMarkStart, gcpoMarkDestination, gcpoMarkCenter);
//  TGreatCirclePainterOptions = set of TGreatCirclePainterOption;
  { TGreatCirclePainterPlugin }
  TGreatCirclePainterPlugin = class;
  TGreatCirclePainterGetCoordsEvent = procedure (Sender : TGreatCirclePainterPlugin;
                                                 var FStart, FDestination : TRealPoint) of Object;
  TGreatCirclePainterPlugin = class(TMvDrawPlugin)
  private
    FZOrder : TGreatCirclePainterZOrder;
    FGreatCircleLineColor : TColor;
    FGreatCircleLineStyle : TPenStyle;
    FGreatCircleLineWidth : Integer;
    FOrthodromeLineColor : TColor;
    FOrthodromeLineStyle : TPenStyle;
    FOrthodromeLineWidth : Integer;
    FStart : TRealPoint;
    FDestination : TRealPoint;
    FTruncLength : Integer;
    FOrthodromeDistance : Double;
    FInitialBearing : Double;
    FGetStartAndDestinationCoordsEvent : TGreatCirclePainterGetCoordsEvent;
    FGreatCircleLinePoints : TObjectList;
    function LimitLat(const ALat : Double) : Double;
    function LimitLon(const ALon : Double) : Double;
    procedure DoGetCoordEvent;
    procedure SetStartLat(Value : Double);
    procedure SetStartLon(Value : Double);
    procedure SetDestinationLat(Value : Double);
    procedure SetDestinationLon(Value : Double);
    function GetGreatCirclePointsCount : Integer;
    function GetGreatCirclePoints(AIndex : Integer) : TGreatCirclePoint;
    procedure SetTruncLength(Value : Integer);
    procedure PaintGreatCircleWithCanvas;
    procedure PaintGreatCircleWithDrawingEngine;
  protected
    procedure AfterDrawObjects(AMapView: TMapView; var Handled: Boolean); override;
    procedure AfterPaint(AMapView: TMapView; var Handled: Boolean); override;
    procedure BeforeDrawObjects(AMapView: TMapView; var Handled: Boolean); override;

    procedure CenterMove(AMapView: TMapView; var Handled: Boolean); override;
    procedure Resize(AMapView: TMapView; var Handled: Boolean); override;
    procedure ZoomChange(AMapView: TMapView; var Handled: Boolean); override;


  published
    property ZOrder : TGreatCirclePainterZOrder read FZOrder write FZOrder default gcpzCanvas;
    property StartLat : Double read FStart.Lat write SetStartLat;
    property StartLon : Double read FStart.Lon write SetStartLon;
    property DestinationLat : Double read FDestination.Lat write SetDestinationLat;
    property DestinationLon : Double read FDestination.Lon write SetDestinationLon;
    property GreatCircleLineColor : TColor read FGreatCircleLineColor write FGreatCircleLineColor;
    property GreatCircleLineStyle : TPenStyle read FGreatCircleLineStyle write FGreatCircleLineStyle;
    property GreatCircleLineWidth : Integer read FGreatCircleLineWidth write FGreatCircleLineWidth;
    property OrthodromeLineColor : TColor read FOrthodromeLineColor write FOrthodromeLineColor;
    property OrthodromeLineStyle : TPenStyle read FOrthodromeLineStyle write FOrthodromeLineStyle;
    property OrthodromeLineWidth : Integer read FOrthodromeLineWidth write FOrthodromeLineWidth;
    property TruncLength : Integer read FTruncLength write SetTruncLength default DefaultTruncLength;
    property OnGetStartAndDestinationCoords : TGreatCirclePainterGetCoordsEvent read FGetStartAndDestinationCoordsEvent write FGetStartAndDestinationCoordsEvent;

  public
    property GreatCirclePointsCount : Integer read GetGreatCirclePointsCount;
    property GreatCirclePoints[AIndex : Integer] : TGreatCirclePoint read GetGreatCirclePoints;
    property OrthodromeDistance : Double read FOrthodromeDistance;
    property InitialBearing : Double read FInitialBearing;
    procedure CalculateGreatCircle;
    procedure SetStartAndDestination(const AStartLat, AStartLon, ADestinationLat, ADestinationLon : Double);
    procedure SetStartAndDestination(const AStart, ADestination : TRealPoint);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy;override;
  end;

procedure VertexOfGreatCircle(const AStartLat, AStartLon, ADestinationLat, ADestinationLon : Double;
                              out AVertexLat, AVertexLon : Double);
procedure LatFromLonAtGreatCircle(const AStartLat, AStartLon, ADestinationLat, ADestinationLon : Double;
                                  const ASearchLon: Double; out AFoundLat: Double);


implementation
function NormalizeLon(const Lon: Double): Double;
begin
  if InRange(Lon, -180.0, 180.0) then
    Result := Lon
  else
    Result := FMod(Lon + 540.0, 360.0) - 180.0;
end;


procedure VertexOfGreatCircle(const AStartLat, AStartLon, ADestinationLat,
  ADestinationLon: Double; out AVertexLat, AVertexLon: Double);
var
  lStartLat, lStartLon, lDestinationLat, lDestinationLon : Double;
  lDestBearing, lDestBearingRad : Double;
  d : Double;
  lDestLatRad : Double;
  SecondLoop : Boolean = False;
begin
  AVertexLon := 0.0;
  lStartLat := AStartLat;
  lStartLon := AStartLon;
  lDestinationLat := ADestinationLat;
  lDestinationLon := ADestinationLon;
  // Problem: If the Latitude is 0.0 than the computation faild, we try than the opposite direction
  repeat
    lDestLatRad := DegToRad(lDestinationLat);
    lDestBearing := CalcBearing(lDestinationLat,lDestinationLon,lStartLat,lStartLon);
    if lDestBearing > 180.0 then
      lDestBearing := lDestBearing - 360.0;
    lDestBearingRad := DegToRad(lDestBearing);
    // Vertex Latidtude
    // Latitude of Vertex: cos phiS = sin alpha * cos phiA
    // cos φS = sin α · cos φPoS = sin 46,87° · cos 10,6722° = 0,7172; φS = 44,1762°
    d := ArcCos(Sin(lDestBearingRad) * Cos(lDestLatRad));
    AVertexLat := RadToDeg(d);
    // The latitude has a sign problem if the signs of DestinationLat and Bearing are differ
    if ((lDestinationLat > 0.0) and (lDestBearing < 0.0)) or
       ((lDestinationLat < 0.0) and (lDestBearing > 0.0)) then
      AVertexLat := -AVertexLat;
    // Vertex Longitude
    // Longitude of Vertex: tan(lamda A - lambda S) = 1 / (sin phi A * tan alpha)
    // tan (λS - λPoS) = cot α ⁄ sin φPoS =
    // = cot 46,87° ⁄ sin 10,6722° = 0,937 ⁄ 0,185 = 5,065;
    // ⇒ λS - λPoS = 78,83°, und daraus
    // λS = 78,83° + (-61,53°) = 17,3°
    if (lDestLatRad <> 0.0) and (lDestBearingRad <> 0.0) then
    begin
      d := RadToDeg(ArcTan(Cot(lDestBearingRad) / Sin(lDestLatRad)));
      AVertexLon := NormalizeLon(d + lDestinationLon);
      Break;
    end;
    if SecondLoop then Break;
    // If lDestLatRad is 0.0, than try the opposite way
    SecondLoop := True;
    lStartLat := ADestinationLat;
    lStartLon := ADestinationLon;
    lDestinationLat := AStartLat;
    lDestinationLon := AStartLon;
  until False;
end;


procedure LatFromLonAtGreatCircle(const AStartLat, AStartLon, ADestinationLat, ADestinationLon : Double;
                                  const ASearchLon: Double; out AFoundLat: Double);
var
  lVertexLat, lVertexLon : Double;
//  lDestBearing : Double;
begin
// tan φWP = tan φS · cos(λS - λWP)
// φS = Lat of Vertex
// λS = Lon of Vertex
// λWP = Lon of sdearched point
  VertexOfGreatCircle(AStartLat, AStartLon, ADestinationLat, ADestinationLon,lVertexLat, lVertexLon);
  AFoundLat := RadToDeg(ArcTan(Tan(DegToRad(lVertexLat)) * Cos(DegToRad(lVertexLon-ASearchLon))));
end;

{ TGreatCirclePoint }

constructor TGreatCirclePoint.Create(const ARealPoint: TRealPoint; const ADistance : Double;
  const APointKind: TGreatCirclePointKind);
begin
  inherited Create;
  FRealPoint := ARealPoint;
  FDistance := ADistance;
  if FDistance < 0.0 then
    FDistance := FDistance+EARTH_CIRCUMFERENCE;
  FPointKind := APointKind;
end;

constructor TGreatCirclePoint.Create(const ARealPointLat, ARealPointLon: Double;
  const ADistance : Double;
  const APointKind: TGreatCirclePointKind);
begin
  inherited Create;
  FRealPoint.InitLatLon(ARealPointLat, ARealPointLon);
  FDistance := ADistance;
  if FDistance < 0.0 then
    FDistance := FDistance+EARTH_CIRCUMFERENCE;
  FPointKind := APointKind;
end;


{ TGreatCirclePainterPlugin }
function GreatCircleLinePointsListSortCompare(Item1, Item2: Pointer): Integer;
var
  pt1 : TGreatCirclePoint absolute Item1;
  pt2 : TGreatCirclePoint absolute Item2;
begin
  Result := 0;
  if pt1.FRealPoint.Lon > pt2.FRealPoint.Lon then
    Result := 1
  else if pt1.FRealPoint.Lon < pt2.FRealPoint.Lon then
    Result := -1
  else if pt1.FDistance > pt2.FDistance then
    Result := 1
  else if pt1.FDistance < pt2.FDistance then
    Result := -1;
end;

procedure TGreatCirclePainterPlugin.CalculateGreatCircle;
  // Calculates the distance between start and the given point
  // including the direction, not the shorts way!
  function DistanceFromStart(const ALat, ALon : Double) : Double;
  var
    d, d0 : Double;
    refPt : TRealPoint;
  begin
    // First the absolute distance
    d := CalcGeoDistance(FStart.Lat, FStart.Lon, ALat, ALon, duMeters);
    // Now calculate the point in the given distance
    CalcLatLon(FStart.Lat, FStart.Lon, d, FInitialBearing, refPt.Lat, refPt.Lon);
    // now calculate the delta between the points
    d0 := CalcGeoDistance(refPt.Lat, refPt.Lon, ALat, ALon, duMeters);
    if Abs(d0) > 1.0 then // if not same
      d := EARTH_CIRCUMFERENCE-d; // the distance is on the other side
    Result := d;
  end;

  // PointKindFromDist
  // return gcpkOrthodrome if between start and destination else gcpkStandard
  function PointKindFromDist(const ADistFromStart : Double) : TGreatCirclePointKind;
  var
    d : Double;
  begin
    d := ADistFromStart;
    if d > EARTH_CIRCUMFERENCE then
      d := d - EARTH_CIRCUMFERENCE
    else if d < 0.0 then
      d := d + EARTH_CIRCUMFERENCE;
    if InRange(d,0.0,FOrthodromeDistance) then
      Result := gcpkOrthodrome
    else
      Result := gcpkStandard;
  end;

var
  lScreenArea : TRealArea;
  lMapCenter : TRealPoint;
  rPt, rPtSub : TRealPoint;
  d : Double;
  lStepLon, lStepLonSub : Double;
  lCurLon : Double;
  lStopLon : Double;
  curDist : Double;
  lMapViewWidth : Integer;
  lMapViewHeight : Integer;
  i : Integer;
  lStart, lDest : TRealPoint;
  lWorldSize : Int64;
  ptKind : TGreatCirclePointKind;
  lon0, lon1 : Double;
  lLastRPt : TrealPoint;
  cnt : Integer;
  lSuppressSort : Boolean = False;
  ndxDest : Integer;
  lFullSpan : Boolean;
  pt : TPoint;
  lDateBorderOnMap : Boolean;
begin
  FGreatCircleLinePoints.Clear;
  FOrthodromeDistance := 0.0;
  FInitialBearing := 0.0;
  if (FStart.Lat = FDestination.Lat) and
     (FStart.Lon = FDestination.Lon) then Exit;
  if not Assigned(MapView) then Exit;
  if not MapView.Visible then Exit;
  lMapViewWidth := MapView.Width;
  lMapViewHeight := MapView.Height;
  if (lMapViewWidth <= 0) or (lMapViewHeight <= 0) then Exit;
  try
    try
      lWorldSize := mvGeoMath.ZoomFactor(MapView.Zoom) * TileSize.CX;
      lFullSpan := ((lMapViewWidth + 2*FTruncLength) >= lWorldSize);
      // Set the ScreenArea (in Degree) from the visioble parts.
      // Take care to limit the area to the maximum width
      if lFullSpan then
      begin
        lScreenArea.TopLeft := MapView.ScreenToLatLon(Point(0,0-FTruncLength));
        lScreenArea.TopLeft.Lon := -180.0;
        lScreenArea.BottomRight := MapView.ScreenToLatLon(Point(lMapViewWidth,lMapViewHeight+FTruncLength));
        lScreenArea.BottomRight.Lon := 180.0;
        lMapViewWidth := lWorldSize;
      end
      else
      begin
        lScreenArea.TopLeft := MapView.ScreenToLatLon(Point(0-FTruncLength,0-FTruncLength));
        lScreenArea.BottomRight := MapView.ScreenToLatLon(Point(lMapViewWidth+FTruncLength,lMapViewHeight+FTruncLength));
      end;
      lMapCenter.InitLatLon((lScreenArea.TopLeft.Lat+lScreenArea.BottomRight.Lat) / 2.0,
                            (lScreenArea.TopLeft.Lon+lScreenArea.BottomRight.Lon) / 2.0);
      // Calculate the step distance (in fraction of longitude degree), by using the pixel distance
      lon0 := lScreenArea.TopLeft.Lon;
      lon1 := lScreenArea.BottomRight.Lon;
      if lon1 >= lon0 then // Normal case
        d := lon1-lon0
      else  // crossing date border
        d := lon1-lon0+360.0;
      lStepLon := d / lMapViewWidth * FTruncLength;
      lStopLon := lon0+d;
      // Now sort out two special cases vertical and horizontal
      FInitialBearing := CalcBearing(FStart.Lat, FStart.Lon, FDestination.Lat, FDestination.Lon);
      FOrthodromeDistance := CalcGeoDistance(FStart.Lat, FStart.Lon, FDestination.Lat, FDestination.Lon,duMeters);
      if (Abs(FStart.Lon-FDestination.Lon) = 0.0) or
         (Abs(FStart.Lon-FDestination.Lon) = 180.0) then
      begin // Straight North-South
        lSuppressSort := True; // We will insert the points as needed
        // We have to insert 6 points:
        //  North pole, South pole, South pole + 180, North pole + 180
        //  plus Start and Destination.
        // Start and destination may share the same Longitude or differ by 180
        for i := 0 to 4 do
        begin
          case i of
            0 : begin // North pole 0
                  rPt.Lat := 90.0; //lScreenArea.TopLeft.Lat;
                  rPt.Lon := FStart.Lon;
                  ptKind := gcpkStandard;
                end;
            1 : begin // Start
                  rPt.InitLatLon(FStart.Lat,FStart.Lon);
                  ptKind := gcpkStart;
                end;
            2 : begin // South pole 0
                  rPt.Lat := -90.0;
                  rPt.Lon := FStart.Lon;
                  ptKind := gcpkStandard;
                end;
            3 : begin // South pole 180
                  rPt.Lat := -90.0;
                  rPt.Lon := NormalizeLon(FStart.Lon + 180.0);
                  ptKind := gcpkStandard;
                end;
            4 : begin // North pole 180
                  rPt.Lat := 90.0;
                  rPt.Lon := NormalizeLon(FStart.Lon + 180.0);
                  ptKind := gcpkStandard;
                end;
          end;
          curDist := DistanceFromStart(rPt.Lat,rPt.Lon);
          FGreatCircleLinePoints.Add(TGreatCirclePoint.Create(rPt.Lat,rPt.Lon,curDist,ptKind));
        end;

        // Now insert the Destination
        rPt.InitLatLon(FDestination.Lat,FDestination.Lon);
        ptKind := gcpkDestination;
        if (Abs(FStart.Lon-FDestination.Lon) = 0.0) then
        begin
          if FStart.Lat > FDestination.Lat then
            ndxDest := 2
          else
            ndxDest := 1;
        end
        else
          ndxDest := 4;
        curDist := DistanceFromStart(rPt.Lat,rPt.Lon);
        FGreatCircleLinePoints.Insert(ndxDest, TGreatCirclePoint.Create(rPt.Lat,rPt.Lon,curDist,ptKind));
        if ndxDest = 4 then
        begin // The destination is on the other side, we need to decide which points are belonging to the orthodrome
          d := (90.0 - FStart.Lat) + (90.0 - FDestination.Lat);
          if d < 180.0 then
          begin // North pole is shorter
            TGreatCirclePoint(FGreatCircleLinePoints.Items[0]).FPointKind:= gcpkOrthodrome;
            TGreatCirclePoint(FGreatCircleLinePoints.Items[5]).FPointKind:= gcpkOrthodrome;
          end
          else
          begin // south pole is shorter
            TGreatCirclePoint(FGreatCircleLinePoints.Items[2]).FPointKind:= gcpkOrthodrome;
            TGreatCirclePoint(FGreatCircleLinePoints.Items[3]).FPointKind:= gcpkOrthodrome;
          end;
        end;
        // The North-South straight line has been processed
        Exit;
      end;
      // if the Start or Destination is on the screen add this point(s)
      FGreatCircleLinePoints.Add(TGreatCirclePoint.Create(FStart,0.0,gcpkStart));
      FGreatCircleLinePoints.Add(TGreatCirclePoint.Create(FDestination,FOrthodromeDistance,gcpkDestination));

      // Equator will be processed as normal
      // Now all other cases
      // First normalize the direction of the path, to run always eastern (rightwards).
      lStart := FStart;
      lDest := FDestination;

      // Now we travel along the part that may(!) visible on the screen.
      // We start far in the west (left)
      lCurLon := lScreenArea.TopLeft.Lon;
      lLastRPt.Lon := lCurLon;
      LatFromLonAtGreatCircle(lStart.Lat, lStart.Lon, lDest.Lat, lDest.Lon,
                              lLastRPt.Lon, lLastRPt.Lat);
      // Loop from left to right
      repeat
        // Increment the curDist (= distance on the circle to travel from normalized start)
        rPt.Lon := lCurLon;
        if rPt.Lon > 180.0 then
          rPt.Lon := rPt.Lon - 360.0;
        LatFromLonAtGreatCircle(lStart.Lat, lStart.Lon, lDest.Lat, lDest.Lon,
                                rPt.Lon, rPt.Lat);

        // Determine kind of point
        d := DistanceFromStart(rPt.Lat,rPt.Lon);
        ptKind := PointKindFromDist(d);
        FGreatCircleLinePoints.Add(TGreatCirclePoint.Create(rPt,d,ptKind));

        // Sub task. If the difference in the latitude is biger than wanted
        // we have to insert some intermediate points
        d := rPt.Lat-lLastRPt.Lat;
        if Abs(d) > lStepLon then
        begin
          cnt := Trunc(Abs(d) / lStepLon);
          lStepLonSub := lStepLon / cnt;
          for i := 1 to cnt-1 do
          begin
            rPtSub.Lon := lLastRPt.Lon + (i*lStepLonSub);
            if rPtSub.Lon > 180.0 then
              rPtSub.Lon := rPtSub.Lon - 360.0;
            LatFromLonAtGreatCircle(lStart.Lat, lStart.Lon, lDest.Lat, lDest.Lon,
                                    rPtSub.Lon, rPtSub.Lat);
            // Determine kind of point
            d := DistanceFromStart(rPtSub.Lat,rPtSub.Lon);
            ptKind := PointKindFromDist(d);
            FGreatCircleLinePoints.Add(TGreatCirclePoint.Create(rPtSub,d,ptKind));
          end;
        end;
        lLastRPt.InitLatLon(rPt.Lat,rPt.Lon); // save the current point as last point
        // terminate the loop if the right side is reached, or the full circle has been processed
        lCurLon := lCurLon + lStepLon;
      until lCurLon > lStopLon;

      // Add the points -180 and 180 if visible on map
      lDateBorderOnMap := lScreenArea.BottomRight.Lon < lScreenArea.TopLeft.Lon;
      for i := 0 to 1 do
      begin
        if i = 0 then
        begin
          rPt.Lon := -180.0;
          if not lDateBorderOnMap then
          begin
            pt := MapView.LatLonToScreen(lScreenArea.TopLeft.Lat,-180.0);
            if pt.X <= MapView.Engine.MapLeft then Continue;
          end;
        end
        else
        begin
          rPt.Lon := 180.0;
          if not lDateBorderOnMap then
          begin
            pt := MapView.LatLonToScreen(lScreenArea.TopLeft.Lat,180.0);
            if pt.X >= MapView.Engine.MapLeft+lWorldSize then Continue;
          end;
        end;
        LatFromLonAtGreatCircle(lStart.Lat, lStart.Lon, lDest.Lat, lDest.Lon,
                                rPt.Lon, rPt.Lat);
        d := DistanceFromStart(rPt.Lat,rPt.Lon);
        ptKind := PointKindFromDist(d);
        FGreatCircleLinePoints.Add(TGreatCirclePoint.Create(rPt,d,ptKind));
      end;

    except
      // Keep silence on any computation errors
    end;
  finally
    // Finally the found points must be sorted and (near) duplicates has to be removed
    if not lSuppressSort then
      FGreatCircleLinePoints.Sort(@GreatCircleLinePointsListSortCompare);
  end;
end;

function TGreatCirclePainterPlugin.LimitLat(const ALat: Double): Double;
begin
  if ALat > 90.0 then
    Result := 90.0
  else if ALat < -90.0 then
    Result := -90.0
  else
    Result := ALat;
end;

function TGreatCirclePainterPlugin.LimitLon(const ALon: Double): Double;
begin
  if ALon > 180.0 then
    Result := 180.0
  else if ALon < -180.0 then
    Result := -180.0
  else
    Result := ALon;
end;

procedure TGreatCirclePainterPlugin.DoGetCoordEvent;
var
  lSPt, lDPt : TRealPoint;
begin
  if Assigned(FGetStartAndDestinationCoordsEvent) then
  begin
    lSPt := FStart;
    lDPt := FDestination;
    FGetStartAndDestinationCoordsEvent(Self,lSPt, lDPt);
    SetStartAndDestination(lSPt, lDPt);
  end;
end;

procedure TGreatCirclePainterPlugin.SetStartLat(Value: Double);
var
  d : Double;
begin
  d := LimitLat(Value);
  if FStart.Lat = d then Exit;
  FStart.Lat := d;
  CalculateGreatCircle;
end;

procedure TGreatCirclePainterPlugin.SetStartLon(Value: Double);
var
  d : Double;
begin
  d := LimitLon(Value);
  if FStart.Lon = d then Exit;
  FStart.Lon := d;
  CalculateGreatCircle;
end;

procedure TGreatCirclePainterPlugin.SetDestinationLat(Value: Double);
var
  d : Double;
begin
  d := LimitLon(Value);
  if FDestination.Lat = d then Exit;
  FDestination.Lat := d;
  CalculateGreatCircle;
end;

procedure TGreatCirclePainterPlugin.SetDestinationLon(Value: Double);
var
  d : Double;
begin
  d := LimitLon(Value);
  if FDestination.Lon = d then Exit;
  FDestination.Lon := d;
  CalculateGreatCircle;
end;

function TGreatCirclePainterPlugin.GetGreatCirclePointsCount: Integer;
begin
  Result := FGreatCircleLinePoints.Count;
end;

function TGreatCirclePainterPlugin.GetGreatCirclePoints(AIndex: Integer
  ): TGreatCirclePoint;
begin
  Result := TGreatCirclePoint(FGreatCircleLinePoints.Items[AIndex]);
end;

procedure TGreatCirclePainterPlugin.SetTruncLength(Value: Integer);
var
  v : Integer;
begin
  if Value <= 0 then
    v := 1
  else if Value > MaxTruncLength then
    v := MaxTruncLength
  else
    v := Value;
  if FTruncLength = v then Exit;
  FTruncLength := v;
  CalculateGreatCircle;
end;

procedure TGreatCirclePainterPlugin.PaintGreatCircleWithCanvas;
var
  i : Integer;
  pt : TPoint;
  pt0, pt1 : TPoint;
  ptCyc: TPointArray;
  dpx, dpy : Integer;
  ptGC0 : TGreatCirclePoint;
  ptGC1 : TGreatCirclePoint;
  cnt : Integer;
  lWorldSize : Int64;
  topY, bottomY : Integer;
  ptOutArr : array of Boolean = Nil;
  isCyclic : Boolean;
begin
  cnt := FGreatCircleLinePoints.Count;
  if cnt > 1 then
  begin
    isCyclic := MapView.Cyclic;
    lWorldSize := mvGeoMath.ZoomFactor(MapView.Zoom) * TileSize.CX;
    pt0 := MapView.LatLonToScreen(90.0,0);
    topY := pt0.Y;
    pt0 := MapView.LatLonToScreen(-90.0,0);
    bottomY := pt0.Y;
    SetLength(ptOutArr,cnt);
    for i := 0 to cnt-1 do
    begin
      ptGC0 := TGreatCirclePoint(FGreatCircleLinePoints.Items[i]);
      pt0 := MapView.LatLonToScreen(ptGC0.RealPoint);
      ptOutArr[i] := (pt0.Y <= topY) or (pt0.Y >= bottomY);
    end;
    for i := 1 to cnt-1 do
    begin
      if not ptOutArr[i] then Continue;
      ptGC0 := TGreatCirclePoint(FGreatCircleLinePoints.Items[i-1]);
      pt0 := MapView.LatLonToScreen(ptGC0.RealPoint);
      ptGC1 := TGreatCirclePoint(FGreatCircleLinePoints.Items[i]);
      pt1 := MapView.LatLonToScreen(ptGC1.RealPoint);

      if ((pt0.Y <= topY) and (pt1.Y >= bottomY)) or
         ((pt0.Y >= bottomY) and (pt1.Y <= topY)) then
        ptOutArr[i] := False;
    end;

    MapView.Canvas.Brush.Style:= bsClear;
    MapView.Canvas.Pen.Style:= FGreatCircleLineStyle;
    MapView.Canvas.Pen.Width:= FGreatCircleLineWidth;
    MapView.Canvas.Pen.Color:= FGreatCircleLineColor;

    ptGC0 := TGreatCirclePoint(FGreatCircleLinePoints.Items[0]);
    for i := 1 to cnt-1 do
    begin
      ptGC1 := TGreatCirclePoint(FGreatCircleLinePoints.Items[i]);
      if (not ptOutArr[i-1]) or (not ptOutArr[i]) then
      begin
        pt0 := MapView.LatLonToScreen(ptGC0.RealPoint);
        pt1 := MapView.LatLonToScreen(ptGC1.RealPoint);

        if isCyclic then
        begin
          pt0.X := Trunc(Frac(pt0.X / lWorldSize)*lWorldSize);
          if pt0.X < 0 then
            pt0.X := pt0.X + lWorldSize
          else if pt0.X > lWorldSize then
            pt0.X := pt0.X - lWorldSize;
          pt1.X := Trunc(Frac(pt1.X / lWorldSize)*lWorldSize);
          if pt1.X < 0 then
            pt1.X := pt1.X + lWorldSize
          else if pt1.X > lWorldSize then
            pt1.X := pt1.X - lWorldSize;
        end;

        dpx := pt1.X-pt0.X;

        if Abs(dpx) > (lWorldSize div 2) then
        begin
          if dpx < 0 then
            dpx := dpx + lWorldSize
          else
            dpx := dpx - lWorldSize;
        end;

        dpy := pt1.Y-pt0.Y;

        if (ptGC0.FPointKind <> gcpkStandard) and
           (ptGC1.FPointKind <> gcpkStandard) then
        begin
          MapView.Canvas.Pen.Style:= FOrthodromeLineStyle;
          MapView.Canvas.Pen.Width:= FOrthodromeLineWidth;
          MapView.Canvas.Pen.Color:= FOrthodromeLineColor;
        end
        else
        begin
          MapView.Canvas.Pen.Style:= FGreatCircleLineStyle;
          MapView.Canvas.Pen.Width:= FGreatCircleLineWidth;
          MapView.Canvas.Pen.Color:= FGreatCircleLineColor;
        end;

        ptCyc := MapView.CyclicPointsOf(pt1);
        for pt in ptCyc do
        begin
          MapView.Canvas.Line(pt.X,pt.Y,pt.X-dpx,pt.Y-dpy);
{
          if (ptGC1.FPointKind = gcpkStart) or
             (ptGC1.FPointKind = gcpkDestination) then
          begin
            MapView.Canvas.Pen.Width:= 1;
            if (ptGC1.FPointKind = gcpkStart) then
              MapView.Canvas.Pen.Color:= clGreen
            else
              MapView.Canvas.Pen.Color:= clBlack;
            MapView.Canvas.Ellipse(pt.X-3,pt.Y-3,pt.X+4,pt.Y+4);
          end
          else
          begin
            MapView.Canvas.Pen.Width:= 1;
            MapView.Canvas.Pen.Color:= clBlack;
            MapView.Canvas.Ellipse(pt.X-3,pt.Y-3,pt.X+4,pt.Y+4);
          end;
}
        end;
      end;
      ptGC0 := ptGC1;
    end;
  end;
end;

procedure TGreatCirclePainterPlugin.PaintGreatCircleWithDrawingEngine;
var
  i : Integer;
  pt : TPoint;
  pt0, pt1 : TPoint;
  ptCyc: TPointArray;
  dpx, dpy : Integer;
  ptGC0 : TGreatCirclePoint;
  ptGC1 : TGreatCirclePoint;
  cnt : Integer;
  lWorldSize : Int64;
  topY, bottomY : Integer;
  ptOutArr : array of Boolean = Nil;
  lDrawingEngine : TMvCustomDrawingEngine;
  isCyclic : Boolean;
begin
  cnt := FGreatCircleLinePoints.Count;
  if cnt > 1 then
  begin
    isCyclic := MapView.Cyclic;
    lWorldSize := mvGeoMath.ZoomFactor(MapView.Zoom) * TileSize.CX;
    lDrawingEngine := MapView.DrawingEngine;
    pt0 := MapView.LatLonToScreen(90.0,0);
    topY := pt0.Y;
    pt0 := MapView.LatLonToScreen(-90.0,0);
    bottomY := pt0.Y;
    SetLength(ptOutArr,cnt);
    for i := 0 to cnt-1 do
    begin
      ptGC0 := TGreatCirclePoint(FGreatCircleLinePoints.Items[i]);
      pt0 := MapView.LatLonToScreen(ptGC0.RealPoint);
      ptOutArr[i] := (pt0.Y <= topY) or (pt0.Y >= bottomY);
    end;
    for i := 1 to cnt-1 do
    begin
      if not ptOutArr[i] then Continue;
      ptGC0 := TGreatCirclePoint(FGreatCircleLinePoints.Items[i-1]);
      pt0 := MapView.LatLonToScreen(ptGC0.RealPoint);
      ptGC1 := TGreatCirclePoint(FGreatCircleLinePoints.Items[i]);
      pt1 := MapView.LatLonToScreen(ptGC1.RealPoint);

      if ((pt0.Y <= topY) and (pt1.Y >= bottomY)) or
         ((pt0.Y >= bottomY) and (pt1.Y <= topY)) then
        ptOutArr[i] := False;
    end;

    lDrawingEngine.BrushStyle:= bsClear;
    lDrawingEngine.PenStyle:= FGreatCircleLineStyle;
    lDrawingEngine.PenWidth:= FGreatCircleLineWidth;
    lDrawingEngine.PenColor:= FGreatCircleLineColor;

    ptGC0 := TGreatCirclePoint(FGreatCircleLinePoints.Items[0]);
    for i := 1 to cnt-1 do
    begin
      ptGC1 := TGreatCirclePoint(FGreatCircleLinePoints.Items[i]);
      if (not ptOutArr[i-1]) or (not ptOutArr[i]) then
      begin
        pt0 := MapView.LatLonToScreen(ptGC0.RealPoint);
        pt1 := MapView.LatLonToScreen(ptGC1.RealPoint);
        if isCyclic then
        begin
          pt0.X := Trunc(Frac(pt0.X / lWorldSize)*lWorldSize);
          if pt0.X < 0 then
            pt0.X := pt0.X + lWorldSize
          else if pt0.X > lWorldSize then
            pt0.X := pt0.X - lWorldSize;
          pt1.X := Trunc(Frac(pt1.X / lWorldSize)*lWorldSize);
          if pt1.X < 0 then
            pt1.X := pt1.X + lWorldSize
          else if pt1.X > lWorldSize then
            pt1.X := pt1.X - lWorldSize;
        end;

        dpx := pt1.X-pt0.X;

        if Abs(dpx) > (lWorldSize div 2) then
        begin
          if dpx < 0 then
            dpx := dpx + lWorldSize
          else
            dpx := dpx - lWorldSize;
        end;

        dpy := pt1.Y-pt0.Y;

        if (ptGC0.FPointKind <> gcpkStandard) and
           (ptGC1.FPointKind <> gcpkStandard) then
        begin
          lDrawingEngine.PenStyle:= FOrthodromeLineStyle;
          lDrawingEngine.PenWidth:= FOrthodromeLineWidth;
          lDrawingEngine.PenColor:= FOrthodromeLineColor;
        end
        else
        begin
          lDrawingEngine.PenStyle:= FGreatCircleLineStyle;
          lDrawingEngine.PenWidth:= FGreatCircleLineWidth;
          lDrawingEngine.PenColor:= FGreatCircleLineColor;
        end;

        ptCyc := MapView.CyclicPointsOf(pt1);
        for pt in ptCyc do
        begin
          lDrawingEngine.Line(pt.X,pt.Y,pt.X-dpx,pt.Y-dpy);
{
          if (ptGC1.FPointKind = gcpkStart) or
             (ptGC1.FPointKind = gcpkDestination) then
          begin
            lDrawingEngine.PenWidth:= 1;
            if (ptGC1.FPointKind = gcpkStart) then
              lDrawingEngine.PenColor:= clGreen
            else
              lDrawingEngine.PenColor:= clBlack;
            lDrawingEngine.Ellipse(pt.X-3,pt.Y-3,pt.X+4,pt.Y+4);
          end
          else
          begin
            lDrawingEngine.PenWidth:= 1;
            lDrawingEngine.PenColor:= clBlack;
            lDrawingEngine.Ellipse(pt.X-3,pt.Y-3,pt.X+4,pt.Y+4);
          end;
}
        end;
      end;
      ptGC0 := ptGC1;
    end;
  end;
end;

procedure TGreatCirclePainterPlugin.AfterDrawObjects(AMapView: TMapView;
  var Handled: Boolean);
begin
  Unused(AMapView,Handled);
  if FZOrder <> gcpzInFrontOfMarkers then Exit;
  DoGetCoordEvent;
  PaintGreatCircleWithDrawingEngine;
end;

procedure TGreatCirclePainterPlugin.AfterPaint(AMapView: TMapView; var Handled: Boolean);
begin
  Unused(AMapView,Handled);
  if FZOrder <> gcpzCanvas then Exit;
  DoGetCoordEvent;
  PaintGreatCircleWithCanvas;
end;

procedure TGreatCirclePainterPlugin.BeforeDrawObjects(AMapView: TMapView;
  var Handled: Boolean);
begin
  Unused(AMapView,Handled);
  if FZOrder <> gcpzBehindMarkers then Exit;
  DoGetCoordEvent;
  PaintGreatCircleWithDrawingEngine;
end;

procedure TGreatCirclePainterPlugin.CenterMove(AMapView: TMapView;
  var Handled: Boolean);
begin
  Unused(AMapView,Handled);
  CalculateGreatCircle;
end;

procedure TGreatCirclePainterPlugin.Resize(AMapView: TMapView;
  var Handled: Boolean);
begin
  Unused(AMapView,Handled);
  CalculateGreatCircle;
end;

procedure TGreatCirclePainterPlugin.ZoomChange(AMapView: TMapView;
  var Handled: Boolean);
begin
  Unused(AMapView,Handled);
  CalculateGreatCircle;
end;

procedure TGreatCirclePainterPlugin.SetStartAndDestination(const AStartLat,
  AStartLon, ADestinationLat, ADestinationLon: Double);
var
  lSPt, sDPt : TRealPoint;
begin
  lSPt.InitLatLon(AStartLat,AStartLon);
  sDPt.InitLatLon(ADestinationLat, ADestinationLon);
  SetStartAndDestination(lSPt, sDPt);
end;

procedure TGreatCirclePainterPlugin.SetStartAndDestination(const AStart,
  ADestination: TRealPoint);
var
  lNewStart, lNewDest : TRealPoint;
begin
  lNewStart.Lat := LimitLat(AStart.Lat);
  lNewStart.Lon := LimitLon(AStart.Lon);
  lNewDest.Lat := LimitLat(ADestination.Lat);
  lNewDest.Lon := LimitLon(ADestination.Lon);
  if (FStart.Lat <>lNewStart.Lat) or
     (FStart.Lon <>lNewStart.Lon) or
     (FDestination.Lat <> lNewDest.Lat) or
     (FDestination.Lon <> lNewDest.Lon) then
  begin
    FStart.Lat := lNewStart.Lat;
    FStart.Lon := lNewStart.Lon;
    FDestination.Lat := lNewDest.Lat;
    FDestination.Lon := lNewDest.Lon;
    CalculateGreatCircle;
  end;
end;

constructor TGreatCirclePainterPlugin.Create(AOwner: TComponent);
begin
  inherited;
  FGreatCircleLinePoints := TObjectList.Create(True);
  FTruncLength := DefaultTruncLength;
end;

destructor TGreatCirclePainterPlugin.Destroy;
begin
  if Assigned(FGreatCircleLinePoints) then
    FGreatCircleLinePoints.Free;
  inherited;
end;

initialization
  RegisterPluginClass(TGreatCirclePainterPlugin, 'Great Circle Painter');
end.

