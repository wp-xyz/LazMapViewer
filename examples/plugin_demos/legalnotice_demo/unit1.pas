unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Types,
  LCLIntf, Forms, Controls, Graphics, ExtCtrls, StdCtrls, Dialogs,
  TAGraph, TATools,
  mvMapViewer, mvPlugins;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Chart1: TChart;
    ChartToolset1: TChartToolset;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    Panel1: TPanel;
    procedure Button1Click(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FMapView: TMapView;
    FPluginManager: TMvPluginManager;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

type
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
    procedure AfterDrawObjects(AMapView: TMapView; var Handled: Boolean); override;
    procedure MouseDown(AMapView: TMapView; Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer; var Handled: Boolean); override;
    procedure MouseMove(AMapView: TMapView; Shift: TShiftState; X, Y: Integer;
      var Handled: Boolean); override;
  public
    constructor Create(APluginManager: TMvPluginManager); override;
    destructor Destroy; override;
  published
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clNone;
    property Font: TFont read FFont write SetFont;
    property LegalNotice: String read FLegalNotice write SetLegalNotice;
    property Position: TLegalNoticePosition read FPosition write SetPosition;
    property LegalNoticeURL: String read FLegalNoticeURL write SetLegalNoticeURL;
    property Spacing: Integer read FSpacing write SetSpacing default DEFAULT_LEGALNOTICE_SPACING;
  end;

constructor TLegalNoticePlugin.Create(APluginManager: TMvPluginManager);
begin
  inherited;
  FBackgroundColor := clNone;
  FPosition := lnpBottomRight;
  FFont := TFont.Create;
  {  --- das geht nicht, da hier lMapView nicht bekannt ist !!!
  if Assigned(lMapView) then
    FFont.Assign(lMapView.Font);
  }
  FSpacing := DEFAULT_LEGALNOTICE_SPACING;
end;

destructor TLegalNoticePlugin.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TLegalNoticePlugin.AfterDrawObjects(AMapView: TMapView; var Handled: Boolean);
var
  sz : TSize;
  x,y : Integer;
begin
  if not Assigned(AMapView) then Exit;
  Handled := True;
  {
  AMapView.Canvas.Font := FFont;
  AMapView.Canvas.Brush.Style := bsClear;
  sz := AMapView.Canvas.TextExtent(FLegalNotice);
  y := AMapView.ClientHeight - sz.cy - FSpacing;
  case FLegalNoticePosition of
    lnpBottomRight:
      x := AMapView.ClientWidth - sz.cx - FSpacing;
    else // lnpBottomLeft
      x := FSpacing;
  end;
  AMapView.Canvas.TextOut(x, y, FLegalNotice);
  }

  if FBackgroundColor = clNone then
    AMapView.DrawingEngine.BrushStyle := bsClear
  else begin
    AMapView.DrawingEngine.BrushStyle := bsSolid;
    AMapView.DrawingEngine.BrushColor := FBackgroundColor;
  end;
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
  AMapView.DrawingEngine.TextOut(x, y, FLegalNotice);
  FClickableRect := Rect(x + FSpacing, y + FSpacing, x + sz.cx, y + sz.cy);
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

procedure TLegalNoticePlugin.SetPosition(
  AValue: TLegalNoticePosition);
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


{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPluginManager := TMvPluginManager.Create(Self);

  FMapView := TMapView.Create(self);
  FMapView.Align := alClient;
  FMapView.Parent := Self;
  FMapView.MapProvider := 'OpenStreetMap Mapnik';
  FMapView.UseThreads := true;
  FMapView.Zoom := 4;
  FMapView.Active := true;
  FMapView.PluginManager := FPluginManager;

  with TLegalNoticePlugin.Create(FPluginManager) do
  begin
    LegalNotice := '(c) OpenStreetMap and contributors';
    LegalNoticeURL := 'https://www.openstreetmap.org/copyright';
    Spacing := 5;
    Font.Size := 8;
    Font.Color := clBlue;
    BackgroundColor := clWhite;

    Edit1.Text := LegalNotice;
  end;
end;

procedure TForm1.Edit1Change(Sender: TObject);
begin
  (FPluginManager.PluginList.Items[0] as TLegalNoticePlugin).LegalNotice := Edit1.Text;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FMapView.SaveToFile(TPortableNetworkGraphic, 'map.png');
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  (FPluginManager.PluginList.Items[0] as TLegalNoticePlugin).Position := TLegalNoticePosition(Combobox1.ItemIndex);
end;

end.

