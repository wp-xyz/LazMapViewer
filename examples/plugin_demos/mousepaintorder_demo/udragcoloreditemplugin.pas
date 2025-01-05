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
    FColor : TColor;
    function IsAtMousePosition(const X,Y: Integer) : Boolean;
  protected
    procedure AfterPaint(AMapView: TMapView; var Handled: Boolean); override;
    procedure MouseMove(AMapView: TMapView; AShift: TShiftState; X,Y: Integer;
          var Handled: Boolean); override;
    procedure MouseUp(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
          X,Y: Integer; var Handled: Boolean); override;
    procedure MouseDown(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
          X,Y: Integer; var Handled: Boolean); override;
  public
    property Color : TColor read FColor write FColor;

  end;

const
  RectWidth2 = 10;

implementation
{ TDragColoredItemPlugin }

function TDragColoredItemPlugin.IsAtMousePosition(const X, Y: Integer): Boolean;
var
  pt, ptc : TPoint;
  aArea, bArea : TRealArea;
begin
  Result := False;
  pt.X := X-RectWidth2;
  pt.Y := Y-RectWidth2;
  aArea.TopLeft := MapView.ScreenToLatLon(pt);
  pt.X := X+RectWidth2;
  pt.Y := Y+RectWidth2;
  aArea.BottomRight := MapView.ScreenToLatLon(pt);


  ptc := MapView.LatLonToScreen(FCurrentCoord);
  pt.X := ptc.X-RectWidth2;
  pt.Y := ptc.Y-RectWidth2;
  bArea.TopLeft := MapView.ScreenToLatLon(pt);
  pt.X := ptc.X+RectWidth2;
  pt.Y := ptc.Y+RectWidth2;
  bArea.BottomRight := MapView.ScreenToLatLon(pt);
  Result := hasIntersectArea(aArea,bArea);
end;

procedure TDragColoredItemPlugin.AfterPaint(AMapView: TMapView; var Handled: Boolean);
var
  ptCurrentScreen : TPoint;
  s : String;
begin
  MapView.Canvas.Pen.Style:= psSolid;
  MapView.Canvas.Pen.Width:= 3;
  ptCurrentScreen := MapView.LatLonToScreen(FCurrentCoord);
  MapView.Canvas.Pen.Color:= FColor;
  MapView.Canvas.Rectangle(ptCurrentScreen.X-RectWidth2,ptCurrentScreen.Y-RectWidth2,
                           ptCurrentScreen.X+RectWidth2,ptCurrentScreen.Y+RectWidth2);
  s := Format('Index: %d',[Index]);
  MapView.Canvas.TextOut(ptCurrentScreen.X+RectWidth2, ptCurrentScreen.Y+RectWidth2, s);
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

end.

