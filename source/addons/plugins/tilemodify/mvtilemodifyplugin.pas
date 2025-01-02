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

{$define USE_EPIKTIMER}

uses
  Classes, SysUtils,
  Graphics, Controls, LCLIntf, //LazLoggerBase,
  mvMapViewer, mvPluginCore,  mvTypes,
  mvMapProvider, mvCache,
  FPImage, IntfGraphics, mvDE_IntfGraphics,
  BGRABitmap, BGRABitmapTypes, mvDE_BGRA,
  RGBGraphics, mvDE_RGBGraphics
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

procedure TTileModifyPlugin.TileAfterGetFromCache(AMapView: TMapView;
  ATileLayer: TGPSTileLayerBase; AMapProvider: TMapProvider; ATileID: TTileID;
  ATileImg: TPictureCacheItem; var Handled: Boolean);
  function IsCloseToFPColor(const AColor, BColor : TFPColor) : Boolean;
  var
    dw : Word;
  begin
    dw := Trunc($FFFF * FSameColorRange);
    Result := (Abs(AColor.Red-BColor.Red) <= dw) and
              (Abs(AColor.Green-BColor.Green) <= dw) and
              (Abs(AColor.Blue-BColor.Blue) <= dw);
  end;
  function IsCloseToBGRAPixel(const AColor : TColor; BColor : PBGRAPixel) : Boolean;
  var
    dw : Word;
  begin
    dw := Trunc($FF * FSameColorRange);
    Result := (Abs(AColor.Red-BColor^.Red) <= dw) and
              (Abs(AColor.Green-BColor^.Green) <= dw) and
              (Abs(AColor.Blue-BColor^.Blue) <= dw);
  end;
  function ApplyBrightnessToFPColor(const AColor : TFPColor) : TFPColor;
  var
    sv, sr : Single;
  begin
    sv := 1.0+((FBrightness - 0.5)*2.0);
    sr := AColor.Red*sv;
    if sr < 0 then
      Result.Red := 0
    else if sr > $FFFF then
      Result.Red := $FFFF
    else
      Result.Red := Trunc(sr);
    sr := AColor.Green*sv;
    if sr < 0 then
      Result.Green := 0
    else if sr > $FFFF then
      Result.Green := $FFFF
    else
      Result.Green := Trunc(sr);
    sr := AColor.Blue*sv;
    if sr < 0 then
      Result.Blue := 0
    else if sr > $FFFF then
      Result.Blue := $FFFF
    else
      Result.Blue := Trunc(sr);
  end;
  function ApplyBrightnessToBGRAPixel(const AColor : PBGRAPixel) : TBGRAPixel;
  var
    sv, sr : Single;
  begin
    sv := 1.0+((FBrightness - 0.5)*2.0);
    sr := AColor^.Red*sv;
    if sr < 0 then
      Result.Red := 0
    else if sr > $FF then
      Result.Red := $FF
    else
      Result.Red := Trunc(sr);
    sr := AColor^.Green*sv;
    if sr < 0 then
      Result.Green := 0
    else if sr > $FF then
      Result.Green := $FF
    else
      Result.Green := Trunc(sr);
    sr := AColor^.Blue*sv;
    if sr < 0 then
      Result.Blue := 0
    else if sr > $FF then
      Result.Blue := $FF
    else
      Result.Blue := Trunc(sr);
  end;
  function ApplyContrastToFPColor(const AColor : TFPColor) : TFPColor;
  var
    sv, sr : Single;
    dev : Single;
  begin
    sv := ((FContrast-0.5)*2.0);
    dev := AColor.Red-$7FFF;
    sr := AColor.Red + dev*sv;
    if sr < 0 then
      Result.Red := 0
    else if sr > $FFFF then
      Result.Red := $FFFF
    else
      Result.Red := Trunc(sr);
    dev := AColor.Green-$7FFF;
    sr := AColor.Green + dev*sv;
    if sr < 0 then
      Result.Green := 0
    else if sr > $FFFF then
      Result.Green := $FFFF
    else
      Result.Green := Trunc(sr);
    dev := AColor.Blue-$7FFF;
    sr := AColor.Blue + dev*sv;
    if sr < 0 then
      Result.Blue := 0
    else if sr > $FFFF then
      Result.Blue := $FFFF
    else
      Result.Blue := Trunc(sr);
  end;
  function ApplyContrastToBGRAPixel(const AColor : PBGRAPixel) : TBGRAPixel;
  var
    sv, sr : Single;
    dev : Single;
  begin
    sv := ((FContrast-0.5)*2.0);
    dev := AColor^.Red-$7F;
    sr := AColor^.Red + dev*sv;
    if sr < 0 then
      Result.Red := 0
    else if sr > $FF then
      Result.Red := $FF
    else
      Result.Red := Trunc(sr);
    dev := AColor^.Green-$7F;
    sr := AColor^.Green + dev*sv;
    if sr < 0 then
      Result.Green := 0
    else if sr > $FF then
      Result.Green := $FF
    else
      Result.Green := Trunc(sr);
    dev := AColor^.Blue-$7F;
    sr := AColor^.Blue + dev*sv;
    if sr < 0 then
      Result.Blue := 0
    else if sr > $FF then
      Result.Blue := $FF
    else
      Result.Blue := Trunc(sr);
  end;




// const
//  OSM = 'OPENSTREETMAP';

var
  lBGRABitmap : TBGRABitmap;
  IntfImg: TLazIntfImage = nil;
  x, y: Integer;
  TempColor: TFPColor;
  Gray: Word;
  r,g,b : Single;
  lOldClr, lNewClr : TFPColor;
  plBGRAPixel: PBGRAPixel;
  lNewBGRAPixelClr : TBGRAPixel;
  n: integer;
  lRGB32Bitmap : TRGB32Bitmap;
  cnt : Integer;
  {$ifndef USE_EPIKTIMER}
    t0, t1 : Int64;
  {$endif}
  ms : Double;
  partd : Double;
begin
  if Assigned(ATileLayer) then Exit;

  {$ifdef USE_EPIKTIMER}
    FEpikTimer.Clear;
    FEpikTimer.Start;
  {$else}
    t0 := GetTickCount;
  {$endif}

  Unused(AMapView, AMapProvider);
  Unused(ATileID,Handled);
  case FModifyMode of
    tmmGrayScale :
      begin
        //  if UpperCase(Copy(AMapProvider.Name,1,Length(OSM))) = OSM then
        if not Assigned(ATileLayer) then
        begin // Modify only the underlying map tile
          if ATileImg is TLazIntfImageCacheItem then
          begin
            IntfImg := TLazIntfImageCacheItem(ATileImg).Image;
            IntfImg.BeginUpdate;
            try
              r := 0.30;
              g := 0.59;
              b := 0.11;
              for y := 0 to IntfImg.Height - 1 do
                for x := 0 to IntfImg.Width - 1 do
                begin
                  TempColor := IntfImg.Colors[x, y];

                  Gray := Round(TempColor.Red * R + TempColor.Green * G + TempColor.Blue * B);
                  TempColor.Red := Gray;
                  TempColor.Green := Gray;
                  TempColor.Blue := Gray;
                  IntfImg.Colors[x, y] := TempColor;
                end;
            finally
              IntfImg.EndUpdate;
            end;
          end
          else if ATileImg is TBGRABitmapCacheItem then
          begin
            TBGRABitmapCacheItem(ATileImg).Image.InplaceGrayscale();
          end
          else if ATileImg is TRGB32BitmapCacheItem then
          begin
            TRGB32BitmapCacheItem(ATileImg).Image.Grayscale;
          end;
        end;
      end;
    tmmColorExchange :
      begin
        if FOrgColor = FExchangeColor then Exit;
        //  if UpperCase(Copy(AMapProvider.Name,1,Length(OSM))) = OSM then
        if not Assigned(ATileLayer) then
        begin // Modify only the underlying map tile
          lOldClr := TColorToFPColor(FOrgColor);
          lNewClr := TColorToFPColor(FExchangeColor);


          if ATileImg is TLazIntfImageCacheItem then
          begin
            IntfImg := TLazIntfImageCacheItem(ATileImg).Image;
            IntfImg.BeginUpdate;
            try
              for y := 0 to IntfImg.Height - 1 do
                for x := 0 to IntfImg.Width - 1 do
                begin
                  TempColor := IntfImg.Colors[x, y];
                  if IsCloseToFPColor(TempColor,lOldClr) then
                    TempColor := lNewClr;
                  IntfImg.Colors[x, y] := TempColor;
                end;
            finally
              IntfImg.EndUpdate;
            end;
          end
          else if ATileImg is TBGRABitmapCacheItem then
          begin
            lNewBGRAPixelClr.red := Red(FExchangeColor);
            lNewBGRAPixelClr.green := Green(FExchangeColor);
            lNewBGRAPixelClr.blue := Blue(FExchangeColor);

            lBGRABitmap := TBGRABitmapCacheItem(ATileImg).Image;
            plBGRAPixel := lBGRABitmap.Data;
            for n := lBGRABitmap.NbPixels-1 downto 0 do
            begin
              if IsCloseToBGRAPixel(FOrgColor,plBGRAPixel) then
                plBGRAPixel^ := lNewBGRAPixelClr;
              Inc(plBGRAPixel);
            end;
            lBGRABitmap.InvalidateBitmap;   // note that we have accessed pixels directly
          end
          else if ATileImg is TRGB32BitmapCacheItem then
          begin
            lNewBGRAPixelClr.red := Red(FExchangeColor);
            lNewBGRAPixelClr.green := Green(FExchangeColor);
            lNewBGRAPixelClr.blue := Blue(FExchangeColor);

            lRGB32Bitmap := TRGB32BitmapCacheItem(ATileImg).Image;
            plBGRAPixel := PBGRAPixel(lRGB32Bitmap.Pixels);
            cnt := lRGB32Bitmap.Height * lRGB32Bitmap.RowPixelStride; // * lRGB32Bitmap.SizeOfPixel;
            for n := 0 to cnt-1 do
            begin
              if IsCloseToBGRAPixel(FOrgColor,plBGRAPixel) then
                plBGRAPixel^ := lNewBGRAPixelClr;
              Inc(plBGRAPixel);
            end;
          end;
        end;
      end;
    tmmBrightnessContrast :
      begin
        //  if UpperCase(Copy(AMapProvider.Name,1,Length(OSM))) = OSM then
        if not Assigned(ATileLayer) then
        begin // Modify only the underlying map tile
          // First the brightness
          if FBrightness <> 0.5 then
          begin
            if ATileImg is TLazIntfImageCacheItem then
            begin
              IntfImg := TLazIntfImageCacheItem(ATileImg).Image;
              IntfImg.BeginUpdate;
              try
                for y := 0 to IntfImg.Height - 1 do
                  for x := 0 to IntfImg.Width - 1 do
                  begin
                    TempColor := IntfImg.Colors[x, y];
                    TempColor := ApplyBrightnessToFPColor(TempColor);
                    IntfImg.Colors[x, y] := TempColor;
                  end;
              finally
                IntfImg.EndUpdate;
              end;
            end
            else if ATileImg is TBGRABitmapCacheItem then
            begin
              lBGRABitmap := TBGRABitmapCacheItem(ATileImg).Image;
              plBGRAPixel := lBGRABitmap.Data;
              for n := lBGRABitmap.NbPixels-1 downto 0 do
              begin
                lNewBGRAPixelClr := ApplyBrightnessToBGRAPixel(plBGRAPixel);
                plBGRAPixel^ := lNewBGRAPixelClr;
                Inc(plBGRAPixel);
              end;
              lBGRABitmap.InvalidateBitmap;   // note that we have accessed pixels directly
            end
            else if ATileImg is TRGB32BitmapCacheItem then
            begin
              lRGB32Bitmap := TRGB32BitmapCacheItem(ATileImg).Image;
              plBGRAPixel := PBGRAPixel(lRGB32Bitmap.Pixels);
              cnt := lRGB32Bitmap.Height * lRGB32Bitmap.RowPixelStride; // * lRGB32Bitmap.SizeOfPixel;
              for n := 0 to cnt-1 do
              begin
                lNewBGRAPixelClr := ApplyBrightnessToBGRAPixel(plBGRAPixel);
                plBGRAPixel^ := lNewBGRAPixelClr;
                Inc(plBGRAPixel);
              end;
            end;
          end;
          // second the contrast
          if FContrast <> 0.5 then
          begin
            if ATileImg is TLazIntfImageCacheItem then
            begin
              IntfImg := TLazIntfImageCacheItem(ATileImg).Image;
              IntfImg.BeginUpdate;
              try
                for y := 0 to IntfImg.Height - 1 do
                  for x := 0 to IntfImg.Width - 1 do
                  begin
                    TempColor := IntfImg.Colors[x, y];
                    TempColor := ApplyContrastToFPColor(TempColor);
                    IntfImg.Colors[x, y] := TempColor;
                  end;
              finally
                IntfImg.EndUpdate;
              end;
            end
            else if ATileImg is TBGRABitmapCacheItem then
            begin
              lBGRABitmap := TBGRABitmapCacheItem(ATileImg).Image;
              plBGRAPixel := lBGRABitmap.Data;
              for n := lBGRABitmap.NbPixels-1 downto 0 do
              begin
                lNewBGRAPixelClr := ApplyContrastToBGRAPixel(plBGRAPixel);
                plBGRAPixel^ := lNewBGRAPixelClr;
                Inc(plBGRAPixel);
              end;
              lBGRABitmap.InvalidateBitmap;   // note that we have accessed pixels directly
            end
            else if ATileImg is TRGB32BitmapCacheItem then
            begin
              lRGB32Bitmap := TRGB32BitmapCacheItem(ATileImg).Image;
              plBGRAPixel := PBGRAPixel(lRGB32Bitmap.Pixels);
              cnt := lRGB32Bitmap.Height * lRGB32Bitmap.RowPixelStride; // * lRGB32Bitmap.SizeOfPixel;
              for n := 0 to cnt-1 do
              begin
                lNewBGRAPixelClr := ApplyContrastToBGRAPixel(plBGRAPixel);
                plBGRAPixel^ := lNewBGRAPixelClr;
                Inc(plBGRAPixel);
              end;
            end;
          end;
        end;
      end;
  else
    // tmmNone :;
  end;
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

