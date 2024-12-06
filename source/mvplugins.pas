unit mvPlugins;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types, LazLoggerBase,
  Graphics, Controls, LCLIntf,
  mvMapViewer, mvPluginCore;

type
  { TCenterMarkerPlugin - draws a cross in the map center }

  TCenterMarkerPlugin = class(TMvPlugin)
  private
    const
      DEFAULT_MARKER_SIZE = 15;
  private
    FPen: TPen;
    FSize: Integer;
    procedure SetPen(AValue: TPen);
    procedure SetSize(AValue: Integer);
  protected
    procedure AfterDrawObjects(AMapView: TMapView; var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Pen: TPen read FPen write SetPen;
    property Size: Integer read FSize write SetSize default DEFAULT_MARKER_SIZE;
  end;


  { TLegalNoticePlugin - displays a clickable copyright text }

  TLegalNoticePosition = (lnpTopLeft, lnpTopRight, lnpBottomLeft, lnpBottomRight);

  TLegalNoticePlugin = class(TMvPlugin)
  private
    const
      DEFAULT_LEGALNOTICE_SPACING = 4;
  private
    FClickableRect: TRect;
    FLegalNotice: String;
    FLegalNoticeURL: String;
    FPosition: TLegalNoticePosition;
    FFont: TFont;
    FSpacing: Integer;
    FBackgroundColor: TColor;
  private
    procedure SetBackgroundColor(AValue: TColor);
    procedure SetFont(AValue: TFont);
    procedure SetLegalNotice(AValue: String);
    procedure SetLegalNoticeURL(AValue: String);
    procedure SetPosition(AValue: TLegalNoticePosition);
    procedure SetSpacing(AValue: Integer);
  protected
    procedure CalcClickableRect(AMapView: TMapView);
  protected
    procedure AfterDrawObjects(AMapView: TMapView; var Handled: Boolean); override;
    procedure MouseDown(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); override;
    procedure MouseEnter(AMapView: TMapView; var Handled: Boolean); override;
    procedure MouseMove(AMapView: TMapView; Shift: TShiftState; X, Y: Integer;
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
    property Position: TLegalNoticePosition read FPosition write SetPosition default lnpBottomRight;
    property Spacing: Integer read FSpacing write SetSpacing default DEFAULT_LEGALNOTICE_SPACING;
  end;


implementation

{ TCenterMargerPlugin }

constructor TCenterMarkerPlugin.Create(AOwner: TComponent);
begin
  inherited;
  FPen := TPen.Create;
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
  AMapView.DrawingEngine.Line(C.X, C.Y - FSize, C.X, C.Y + FSize);
  AMapView.DrawingEngine.Line(C.X - FSize, C.Y, C.X + FSize, C.Y);
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


{ TLegalNoticePlugin }

constructor TLegalNoticePlugin.Create(AOwner: TComponent);
begin
  inherited;
  FBackgroundColor := clNone;
  FPosition := lnpBottomRight;
  FFont := TFont.Create;
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
    FPosition := TLegalNoticePlugin(Source).Position;
    FSpacing := TLegalNoticePlugin(Source).Spacing;
  end;
  inherited;
end;

procedure TLegalNoticePlugin.AfterDrawObjects(AMapView: TMapView; var Handled: Boolean);
var
  x,y : Integer;
begin
  if not Assigned(AMapView) then Exit;
  Handled := True;
  if FBackgroundColor = clNone then
    AMapView.DrawingEngine.BrushStyle := bsClear
  else begin
    AMapView.DrawingEngine.BrushStyle := bsSolid;
    AMapView.DrawingEngine.BrushColor := FBackgroundColor;
  end;
  CalcClickableRect(AMapView);
  x := FClickableRect.Left - FSpacing;
  y := FClickableRect.Top - FSpacing;
  AMapView.DrawingEngine.TextOut(x, y, FLegalNotice);
end;

procedure TLegalNoticePlugin.CalcClickableRect(AMapView: TMapView);
var
  sz: TSize;
  x, y: Integer;
begin
  AMapView.DrawingEngine.FontName := FFont.Name;
  AMapView.DrawingEngine.FontSize := FFont.Size;
  AMapView.DrawingEngine.FontStyle := FFont.Style;
  AMapView.DrawingEngine.FontColor := FFont.Color;
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
  FClickableRect := Rect(x + FSpacing, y + FSpacing, x + sz.CX, y + sz.CY);
end;

procedure TLegalNoticePlugin.MouseDown(AMapView: TMapView; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer; var Handled: Boolean);
var
  pt: TPoint;
begin
  // The button down event is consumed by a different plugin, so do nothing here
  if Handled then Exit;
  pt.X := X;
  pt.Y := Y;
  if PtInRect(FClickableRect, pt) and (FLegalNoticeURL <> '') then
  begin
    // The button down event is consumed by this plugin
    OpenURL(FLegalNoticeURL);
    Handled := True;
    Abort;     // No further handling of the event for dragging!
  end;
end;

procedure TLegalNoticePlugin.MouseEnter(AMapView: TMapView; var Handled: Boolean);
begin
  inherited;
  CalcClickableRect(AMapView);
end;

procedure TLegalNoticePlugin.MouseMove(AMapView: TMapView; Shift: TShiftState;
  X, Y: Integer; var Handled: Boolean);
begin
  if PtInRect(FClickableRect, Point(X, Y)) and (not AMapView.Engine.InDrag) then
  begin
    FFont.Style := [fsUnderline];
    AMapView.Cursor := crHandPoint;
  end else
  begin
    FFont.Style := [];
    AMapView.Cursor := crDefault;
  end;
  Handled := true;
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

initialization
  RegisterPluginClass(TCenterMarkerPlugin, 'Center marker');
  RegisterPluginClass(TLegalNoticePlugin, 'Legal notice');

end.

