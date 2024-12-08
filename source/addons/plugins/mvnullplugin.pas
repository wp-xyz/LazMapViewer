unit mvNullPlugin;
interface
uses
  Classes, SysUtils, Controls,
  mvMapViewer, mvPluginCore;
type
  TMvPluginNotifyEvent = procedure (Sender : TObject; AMapView: TMapView; var Handled: Boolean) of Object;
  TMvPluginMouseEvent = procedure (Sender : TObject; AMapView: TMapView; Button: TMouseButton;
                                   Shift: TShiftState;
                                   X, Y: Integer; var Handled: Boolean) of Object;
  TMvPluginMouseMoveEvent = procedure (Sender : TObject; AMapView: TMapView; AShift: TShiftState;
                                       X,Y: Integer; var Handled: Boolean) of Object;

  { TMvNullPlugin }

  TMvNullPlugin = class(TMvCustomPlugin)
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
  end;


implementation

{ TMvCustomPlugin }

procedure TMvNullPlugin.AfterDrawObjects(AMapView: TMapView;
  var Handled: Boolean);
begin
  if Assigned(FAfterDrawObjectsEvent) then
    FAfterDrawObjectsEvent(Self, AMapView, Handled);
end;

procedure TMvNullPlugin.AfterPaint(AMapView: TMapView; var Handled: Boolean);
begin
  if Assigned(FAfterPaintEvent) then
    FAfterPaintEvent(Self, AMapView, Handled);
end;

procedure TMvNullPlugin.BeforeDrawObjects(AMapView: TMapView;
  var Handled: Boolean);
begin
  if Assigned(FBeforeDrawObjectsEvent) then
    FBeforeDrawObjectsEvent(Self, AMapView, Handled);
end;

procedure TMvNullPlugin.CenterMove(AMapView: TMapView; var Handled: Boolean);
begin
  if Assigned(FCenterMoveEvent) then
    FCenterMoveEvent(Self, AMapView, Handled);
end;

procedure TMvNullPlugin.MouseDown(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
begin
  if Assigned(FMouseDownEvent) then
    FMouseDownEvent(Self,AMapView, Button, Shift, X,Y, Handled);
end;

procedure TMvNullPlugin.MouseEnter(AMapView: TMapView; var Handled: Boolean);
begin
  if Assigned(FMouseEnterEvent) then
    FMouseEnterEvent(Self, AMapView, Handled);
end;

procedure TMvNullPlugin.MouseLeave(AMapView: TMapView; var Handled: Boolean);
begin
  if Assigned(FMouseLeaveEvent) then
    FMouseLeaveEvent(Self, AMapView, Handled);
end;

procedure TMvNullPlugin.MouseMove(AMapView: TMapView; AShift: TShiftState; X,
  Y: Integer; var Handled: Boolean);
begin
  if Assigned(FMouseMoveEvent) then
    FMouseMoveEvent(Self,AMapView, AShift, X, Y, Handled);
end;

procedure TMvNullPlugin.MouseUp(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
begin
  if Assigned(FMouseUpEvent) then
    FMouseUpEvent(Self, AMapView, Button, Shift, X, Y, Handled);
end;

procedure TMvNullPlugin.ZoomChange(AMapView: TMapView; var Handled: Boolean);
begin
  if Assigned(FZoomChangeEvent) then
    FZoomChangeEvent(Self, AMapView, Handled);
end;

initialization
  RegisterPluginClass(TMvNullPlugin, 'Null');
end.
