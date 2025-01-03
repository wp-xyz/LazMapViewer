{ This unit implement an MapViewer-Plugin to modify the tiles prior painting.
  It could be used to grayscale tiles, exchange a color or adjust brightness and contrast.
  It works on all three currently implemented Drawing-Engines.
  The implementation alter only the background tiles, not the tiles from any
  separate layer.
  This is helpful to display the partly transparent OpenRailwayMap-Tiles on top
  of a grayscale OpenStreetMap
}
unit mvtilemodifyplugin;

{$mode ObjFPC}{$H+}

interface

{ Activate USE_EPIKTIMER to include Epiktimer (https://wiki.lazarus.freepascal.org/EpikTimer)
  for a precise timing measurement.
  If not activated the timing measurement is much less precise.}
{$define USE_EPIKTIMER}

{ Activate USE_BGRA if your application uses the mvDE_BGRA drawing engine.
  If not activated, the plugin is not able to perform on the tiles of this drawinmg engine.
  If activated, but the drawing engine is not used, a lot of unused code is compiled into
  the target application.
}
{$define USE_BGRA}
{ Activate USE_RGB32 if your application uses the mvDE_RGBGraphics drawing engine.
  If not activated, the plugin is not able to perform on the tiles of this drawinmg engine.
  If activated, but the drawing engine is not used, a lot of unused code is compiled into
  the target application.
}
{$define USE_RGB32}

uses
  Classes, SysUtils,
  Graphics, Controls, LCLIntf, //LazLoggerBase,
  mvMapViewer, mvPluginCore,  mvTypes,
  mvMapProvider, mvCache,
  FPImage, IntfGraphics, mvDE_IntfGraphics,
  uYCbCrTools
  {$ifdef USE_BGRA}, BGRABitmap, BGRABitmapTypes, mvDE_BGRA {$endif}
  {$ifdef USE_RGB32}, RGBGraphics, RGBTypes, mvDE_RGBGraphics {$endif}
  {$ifdef USE_EPIKTIMER}, epiktimer{$endif}
  ;
type

  { TTileModifyPlugin }
  TTileModifyMode = (tmmNone, tmmGrayScale, tmmColorExchange, tmmBrightnessContrast);
  TTileModifyPlugin = class(TMvCustomPlugin)
  private
    FModifyMode : TTileModifyMode;
    FOrgColor : TColor;
    FExchangeColor : TColor;
    FSameColorRange : Single;
    FBrightness : Single;
    FContrast : Single;
    FMilliSecondsPerTile : Single;
    FAvgTileCount : Integer;
    {$ifdef USE_EPIKTIMER}FEpikTimer : TEpikTimer;{$endif}

    function LimitToOne(Value : Single) : Single;
    procedure SetSameColorRange(Value : Single);
    procedure SetBrightness(Value : Single);
    procedure SetContrast(Value : Single);
    procedure ProcessTileLazIntfImg(const ALazIntfImg: TLazIntfImage);
    {$ifdef USE_BGRA}procedure ProcessTileBRGA(const ABGRABitmap : TBGRABitmap);{$endif}
    {$ifdef USE_RGB32}procedure ProcessTileRGB32(const ARGB32Bitmap : TRGB32Bitmap);{$endif}

  protected
    procedure TileAfterGetFromCache(AMapView: TMapView; ATileLayer: TGPSTileLayerBase;
      AMapProvider: TMapProvider; ATileID: TTileID; ATileImg: TPictureCacheItem;
      var Handled: Boolean); override;
  published
    property ModifyMode : TTileModifyMode read FModifyMode write FModifyMode;
    property MapView;
    // For the color exchange three values have to be set
    // The original color, the color to be painted instead
    // and the deviation (0.0 to 1.0) of the original color to be treated
    // as the same color.
    property OrgColor : TColor read FOrgColor write FOrgColor;
    property ExchangeColor : TColor read FExchangeColor write FExchangeColor;
    property SameColorRange : Single read FSameColorRange write SetSameColorRange;
    property Brightness : Single read FBrightness write SetBrightness;
    property Contrast : Single read FContrast write SetContrast;

  public
    property MilliSecondsPerTile : Single read FMilliSecondsPerTile;
    property AvgTileCount : Integer read FAvgTileCount write FAvgTileCount;
    procedure ResetMilliSecondsPerTile;
    constructor Create(AOwner: TComponent); override;
  end;

implementation
const
  TileCountMax = 128;

type
  TPluginRGBPixel = packed record
    blue, green, red, alpha: Byte;
  end;

function IsCloseToFPColor(const AColor, BColor : TFPColor; const ASameColorRange : Single) : Boolean;
var
  dw : Word;
begin
  dw := Trunc($FFFF * ASameColorRange);
  Result := (Abs(AColor.Red-BColor.Red) <= dw) and
            (Abs(AColor.Green-BColor.Green) <= dw) and
            (Abs(AColor.Blue-BColor.Blue) <= dw);
end;
function IsCloseToBGRAPixel(const AColor : TColor; const BColor : TPluginRGBPixel; const ASameColorRange : Single) : Boolean;
var
  dw : Word;
begin
  dw := Trunc($FF * ASameColorRange);
  Result := (Abs(Red(AColor)-BColor.Red) <= dw) and
            (Abs(Green(AColor)-BColor.Green) <= dw) and
            (Abs(Blue(AColor)-BColor.Blue) <= dw);
end;

{ TTileModifyPlugin }

function TTileModifyPlugin.LimitToOne(Value: Single): Single;
begin
  if Value < 0.0 then
    Result := 0.0
  else if Value > 1.0 then
    Result := 1.0
  else
    Result := Value;
end;

procedure TTileModifyPlugin.SetSameColorRange(Value: Single);
begin
  FSameColorRange := LimitToOne(Value);
end;

procedure TTileModifyPlugin.SetBrightness(Value: Single);
begin
  FBrightness := LimitToOne(Value);
end;

procedure TTileModifyPlugin.SetContrast(Value: Single);
begin
  FContrast := LimitToOne(Value);
end;

procedure TTileModifyPlugin.ProcessTileLazIntfImg(
  const ALazIntfImg: TLazIntfImage);
var
  x, y: Integer;
  lTmpFPClr: TFPColor;
  lTmpClr : TColor;
  Gray: Word;
  r,g,b : Double;
  yc,cr,cb : Double;
  rb, gb, bb : Byte;
  lOldClr, lNewClr : TFPColor;
begin
  case FModifyMode of
    tmmGrayScale :
      begin
        ALazIntfImg.BeginUpdate;
        try
          r := 0.30;
          g := 0.59;
          b := 0.11;
          for y := 0 to ALazIntfImg.Height - 1 do
            for x := 0 to ALazIntfImg.Width - 1 do
            begin
              lTmpFPClr := ALazIntfImg.Colors[x, y];

              Gray := Round(lTmpFPClr.Red * R + lTmpFPClr.Green * G + lTmpFPClr.Blue * B);
              lTmpFPClr.Red := Gray;
              lTmpFPClr.Green := Gray;
              lTmpFPClr.Blue := Gray;
              ALazIntfImg.Colors[x, y] := lTmpFPClr;
            end;
        finally
          ALazIntfImg.EndUpdate;
        end;
      end;
    tmmColorExchange :
      begin
        if FOrgColor = FExchangeColor then Exit;
        lOldClr := TColorToFPColor(FOrgColor);
        lNewClr := TColorToFPColor(FExchangeColor);
        ALazIntfImg.BeginUpdate;
        try
          for y := 0 to ALazIntfImg.Height - 1 do
            for x := 0 to ALazIntfImg.Width - 1 do
            begin
              lTmpFPClr := ALazIntfImg.Colors[x, y];
              if IsCloseToFPColor(lTmpFPClr,lOldClr,FSameColorRange) then
                lTmpFPClr := lNewClr;
              ALazIntfImg.Colors[x, y] := lTmpFPClr;
            end;
        finally
          ALazIntfImg.EndUpdate;
        end;
      end;
    tmmBrightnessContrast :
      begin
        if (FBrightness <> 0.5) or
           (FContrast <> 0.5) then
        begin
          ALazIntfImg.BeginUpdate;
          try
            for y := 0 to ALazIntfImg.Height - 1 do
              for x := 0 to ALazIntfImg.Width - 1 do
              begin
                lTmpFPClr := ALazIntfImg.Colors[x, y];
                lTmpClr := FPColorToTColor(lTmpFPClr);
                RGBToYCbCr(lTmpClr.Red,lTmpClr.Green,lTmpClr.Blue,yc,cr,cb);
                YCbCrContrastBrightness(yc,cr,cb,FContrast,FBrightness);
                YCbCrToRGB(yc,cr,cb,rb,gb,bb);
                lTmpClr := RGB(rb,gb,bb);
                lTmpFPClr := TColorToFPColor(lTmpClr);
                ALazIntfImg.Colors[x, y] := lTmpFPClr;
              end;
          finally
            ALazIntfImg.EndUpdate;
          end;
        end;
      end;
  else
    // tmmNone :;
  end;
end;
{$ifdef USE_BGRA}
procedure TTileModifyPlugin.ProcessTileBRGA(const ABGRABitmap: TBGRABitmap);
var
  plBGRAPixel: PBGRAPixel;
  lNewBGRAPixelClr : TPluginRGBPixel;
  n: integer;
  yc,cr,cb : Double;
begin
  case FModifyMode of
    tmmGrayScale :
      begin
        ABGRABitmap.InplaceGrayscale();
      end;
    tmmColorExchange :
      begin
        if FOrgColor = FExchangeColor then Exit;
        plBGRAPixel := ABGRABitmap.Data;
        for n := ABGRABitmap.NbPixels-1 downto 0 do
        begin
          lNewBGRAPixelClr.red:= plBGRAPixel^.red;
          lNewBGRAPixelClr.green:= plBGRAPixel^.green;
          lNewBGRAPixelClr.blue:= plBGRAPixel^.blue;
          if IsCloseToBGRAPixel(FOrgColor,lNewBGRAPixelClr,FSameColorRange) then
          begin
            plBGRAPixel^.red := Red(FExchangeColor);
            plBGRAPixel^.green := Green(FExchangeColor);
            plBGRAPixel^.blue := Blue(FExchangeColor);
          end;
          Inc(plBGRAPixel);
        end;
        ABGRABitmap.InvalidateBitmap;   // note that we have accessed pixels directly
      end;
    tmmBrightnessContrast :
      begin
        if (FBrightness <> 0.5) or
           (FContrast <> 0.5) then
        begin
          plBGRAPixel := ABGRABitmap.Data;
          for n := ABGRABitmap.NbPixels-1 downto 0 do
          begin
            RGBToYCbCr(plBGRAPixel^.red,plBGRAPixel^.green,plBGRAPixel^.blue,yc,cr,cb);
            YCbCrContrastBrightness(yc,cr,cb,FContrast,FBrightness);
            YCbCrToRGB(yc,cr,cb,plBGRAPixel^.red,plBGRAPixel^.green,plBGRAPixel^.blue);
            Inc(plBGRAPixel);
          end;
          ABGRABitmap.InvalidateBitmap;   // note that we have accessed pixels directly
        end;
      end;
  else
    // tmmNone :;
  end;
end;
{$endif}
{$ifdef USE_RGB32}
procedure TTileModifyPlugin.ProcessTileRGB32(const ARGB32Bitmap: TRGB32Bitmap);
var
  plBGRAPixel: PRGB32Pixel;
  lPixelClr : TPluginRGBPixel;
  lNewBGRAPixelClr : TPluginRGBPixel;
  n: integer;
  cnt : Integer;
  clr : TColor;
  yc,cr,cb : Double;
begin
  case FModifyMode of
    tmmGrayScale :
      begin
        ARGB32Bitmap.Grayscale;
      end;
    tmmColorExchange :
      begin
        if FOrgColor = FExchangeColor then Exit;
        lNewBGRAPixelClr.red := Red(FExchangeColor);
        lNewBGRAPixelClr.green := Green(FExchangeColor);
        lNewBGRAPixelClr.blue := Blue(FExchangeColor);

        plBGRAPixel := PRGB32Pixel(ARGB32Bitmap.Pixels);
        cnt := ARGB32Bitmap.Height * ARGB32Bitmap.RowPixelStride; // * lRGB32Bitmap.SizeOfPixel;
        for n := 0 to cnt-1 do
        begin
          clr := RGB32PixelToColor(plBGRAPixel^);
          lPixelClr.red := Red(clr);
          lPixelClr.green := Green(clr);
          lPixelClr.blue := Blue(clr);
          if IsCloseToBGRAPixel(FOrgColor,lPixelClr,FSameColorRange) then
          begin
            clr := RGB(lNewBGRAPixelClr.red,lNewBGRAPixelClr.green,lNewBGRAPixelClr.blue);
            plBGRAPixel^ := ColorToRGB32Pixel(clr);
          end;
          Inc(plBGRAPixel);
        end;
      end;
    tmmBrightnessContrast :
      begin
        if (FBrightness <> 0.5) or
           (FContrast <> 0.5) then
        begin
          plBGRAPixel := PRGB32Pixel(ARGB32Bitmap.Pixels);
          cnt := ARGB32Bitmap.Height * ARGB32Bitmap.RowPixelStride; // * lRGB32Bitmap.SizeOfPixel;
          for n := 0 to cnt-1 do
          begin
            clr := RGB32PixelToColor(plBGRAPixel^);
            lPixelClr.red := Red(clr);
            lPixelClr.green := Green(clr);
            lPixelClr.blue := Blue(clr);
            RGBToYCbCr(lPixelClr.red,lPixelClr.green,lPixelClr.blue,yc,cr,cb);
            YCbCrContrastBrightness(yc,cr,cb,FContrast,FBrightness);
            YCbCrToRGB(yc,cr,cb,lPixelClr.red,lPixelClr.green,lPixelClr.blue);
            clr := RGB(lPixelClr.red,lPixelClr.green,lPixelClr.blue);
            plBGRAPixel^ := ColorToRGB32Pixel(clr);
            Inc(plBGRAPixel);
          end;
        end;
      end;
  else
    // tmmNone :;
  end;
end;
{$endif}

procedure TTileModifyPlugin.TileAfterGetFromCache(AMapView: TMapView;
  ATileLayer: TGPSTileLayerBase; AMapProvider: TMapProvider; ATileID: TTileID;
  ATileImg: TPictureCacheItem; var Handled: Boolean);

// const
//  OSM = 'OPENSTREETMAP';

var
  {$ifndef USE_EPIKTIMER}
    t0, t1 : Int64;
  {$endif}
  ms : Double;
  partd : Double;
begin
  Unused(AMapView, AMapProvider);
  Unused(ATileID,Handled);

  if Assigned(ATileLayer) then Exit;
  if FModifyMode = tmmNone then Exit;

  {$ifdef USE_EPIKTIMER}
    FEpikTimer.Clear;
    FEpikTimer.Start;
  {$else}
    t0 := GetTickCount;
  {$endif}

  if ATileImg is TLazIntfImageCacheItem then
    ProcessTileLazIntfImg(TLazIntfImageCacheItem(ATileImg).Image)
  {$ifdef USE_BGRA}
  else if ATileImg is TBGRABitmapCacheItem then
    ProcessTileBRGA(TBGRABitmapCacheItem(ATileImg).Image)
  {$endif}
  {$ifdef USE_RGB32}
  else if ATileImg is TRGB32BitmapCacheItem then
    ProcessTileRGB32(TRGB32BitmapCacheItem(ATileImg).Image)
  {$endif}
  ;

  {$ifdef USE_EPIKTIMER}
    FEpikTimer.Stop;
    ms := FEpikTimer.Elapsed*1000.0;
  {$else}
    t1 := GetTickCount;
    ms := t1-t0;
  {$endif}
  partd := ((TileCountMax-FAvgTileCount)/TileCountMax);
  FMilliSecondsPerTile := (FMilliSecondsPerTile*(1.0-partd))+
                          (ms*partd);
  if FAvgTileCount < TileCountMax-1 then
    Inc(FAvgTileCount);
end;

procedure TTileModifyPlugin.ResetMilliSecondsPerTile;
begin
  FAvgTileCount := 0;
  FMilliSecondsPerTile := 0.0;
end;

constructor TTileModifyPlugin.Create(AOwner: TComponent);
begin
  inherited;
  {$ifdef USE_EPIKTIMER}FEpikTimer := TEpikTimer.Create(Self);{$endif}
end;

initialization
  RegisterPluginClass(TTileModifyPlugin, 'Tile Modify');

end.

