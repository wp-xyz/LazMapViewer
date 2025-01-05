unit uDragColoredItemPlugin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Controls, Graphics,
  mvPluginCore, mvPlugins, mvMapViewer, mvTypes, mvGPSObj, mvGeoMath;

type

  { TDragColoredItemPlugin }

  TDragColoredItemPlugin = class(TMvPlugin)
  private
    FMouseDown : Boolean;
    FCurrentCoord : TRealPoint;
    FRectSize : Integer;
    FColor : TColor;
    function IsAtMousePosition(const X,Y: Integer) : Boolean;
    procedure SetRectSize(Value : Integer);
  protected
    procedure AfterPaint(AMapView: TMapView; var Handled: Boolean); override;
    procedure MouseMove(AMapView: TMapView; AShift: TShiftState; X,Y: Integer;
          var Handled: Boolean); override;
    procedure MouseUp(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
          X,Y: Integer; var Handled: Boolean); override;
    procedure MouseDown(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
          X,Y: Integer; var Handled: Boolean); override;
    procedure MouseWheel(AMapView: TMapView; AShift: TShiftState;
      AWheelDelta: Integer; AMousePos: TPoint; var Handled: Boolean); override;
  public
    property Color : TColor read FColor write FColor;
    property RectSize : Integer read FRectSize write SetRectSize;
    constructor Create(AOwner: TComponent); override;
  end;

const
  MinRectSize = 6;
  MaxRectSize = 100;
  DefaultRectSize = 20;

implementation
{ TDragColoredItemPlugin }

function TDragColoredItemPlugin.IsAtMousePosition(const X, Y: Integer): Boolean;
var
  pt, ptc : TPoint;
  aArea, bArea : TRealArea;
  w2 : Integer;
begin
  Result := False;
  w2 := FRectSize div 2;
  pt.X := X-w2;
  pt.Y := Y-w2;
  aArea.TopLeft := MapView.ScreenToLatLon(pt);
  pt.X := X+w2;
  pt.Y := Y+w2;
  aArea.BottomRight := MapView.ScreenToLatLon(pt);

  ptc := MapView.LatLonToScreen(FCurrentCoord);
  pt.X := ptc.X-w2;
  pt.Y := ptc.Y-w2;
  bArea.TopLeft := MapView.ScreenToLatLon(pt);
  pt.X := ptc.X+w2;
  pt.Y := ptc.Y+w2;
  bArea.BottomRight := MapView.ScreenToLatLon(pt);
  Result := hasIntersectArea(aArea,bArea);
end;

procedure TDragColoredItemPlugin.SetRectSize(Value: Integer);
var
  newsz : Integer;
begin
  if Value < MinRectSize then
    newsz := MinRectSize
  else if Value > MaxRectSize then
    newsz := MaxRectSize
  else
    newsz := Value;
  if FRectSize <> newsz then
  begin
     FRectSize := newsz;
     MapView.Invalidate;
  end;
end;

procedure TDragColoredItemPlugin.AfterPaint(AMapView: TMapView; var Handled: Boolean);
var
  ptCurrentScreen : TPoint;
  s : String;
  w2 : Integer;
begin
  MapView.Canvas.Pen.Style:= psSolid;
  MapView.Canvas.Pen.Width:= 3;
  MapView.Canvas.Pen.Color:= FColor;
  ptCurrentScreen := MapView.LatLonToScreen(FCurrentCoord);
  w2 := (FRectSize div 2);
  MapView.Canvas.Rectangle(ptCurrentScreen.X-w2,ptCurrentScreen.Y-w2,
                           ptCurrentScreen.X+w2,ptCurrentScreen.Y+w2);
  s := Format('Index: %d',[Index]);
  MapView.Canvas.TextOut(ptCurrentScreen.X+w2, ptCurrentScreen.Y+w2, s);
end;

procedure TDragColoredItemPlugin.MouseMove(AMapView: TMapView; AShift: TShiftState;
  X, Y: Integer; var Handled: Boolean);
begin
  if FMouseDown then
  begin
    FCurrentCoord := MapView.ScreenToLatLon(Point(X,Y));
    MapView.Invalidate;
    Handled := True;
    MapView.Cursor := crDefault;
  end
  else if not Handled then
  begin
    if IsAtMousePosition(X,Y) then
    begin
      MapView.Cursor := crHandPoint;
      Handled := True;
    end
    else
      MapView.Cursor := crDefault;
  end;
end;

procedure TDragColoredItemPlugin.MouseUp(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
begin
  if Button <> mbRight then Exit;
  if FMouseDown then
    MapView.Invalidate; // This will remove the two circles and the text
  FMouseDown := False;
  MapView.Cursor := crDefault;
  Handled := True;
end;

procedure TDragColoredItemPlugin.MouseDown(AMapView: TMapView;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer; var Handled: Boolean
  );
begin
  if Button <> mbRight then Exit;
  if Handled then Exit;
  if IsAtMousePosition(X,Y) then
  begin
    FMouseDown := True;
    FCurrentCoord := MapView.ScreenToLatLon(Point(X,Y));
    Handled := True;
    MapView.Invalidate;
  end;
end;

procedure TDragColoredItemPlugin.MouseWheel(AMapView: TMapView;
  AShift: TShiftState; AWheelDelta: Integer; AMousePos: TPoint;
  var Handled: Boolean);
begin
  if Handled then Exit;
  if IsAtMousePosition(AMousePos.X,AMousePos.Y) then
  begin
    Handled := True;
    RectSize := RectSize + (AWheelDelta div 20);
  end;
end;

constructor TDragColoredItemPlugin.Create(AOwner: TComponent);
begin
  inherited;
  FRectSize := DefaultRectSize;
end;

end.

