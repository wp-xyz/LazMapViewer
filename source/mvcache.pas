{
  Picture cache manager
    (C) 2014 ti_dic@hotmail.com

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvCache;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IntfGraphics, syncObjs,
  mvMapProvider, mvTypes, FPImage;

Type
   { TPictureCacheItem }

   TPictureCacheItem = class(TObject)
   protected
     function GetImageObject: TObject; virtual;
     class function GetImageReader({%H-}AStream: TStream): TFPCustomImageReader;
   public
     constructor Create({%H-}AStream: TStream); virtual;
     destructor Destroy; override;
   end;

   TPictureCacheItemClass = class of TPictureCacheItem;

   { TPictureCache }

   TPictureCache = Class(TComponent)
   private
     FCacheItemClass: TPictureCacheItemClass;
     FMaxAge: Integer;
     FMemMaxElem: integer;
     Crit: TCriticalSection;
     Cache: TStringList;
     FBasePath: String;
     FUseDisk: Boolean;
     FUseThreads: Boolean;
     procedure SetCacheItemClass(AValue: TPictureCacheItemClass);
     procedure SetUseThreads(AValue: Boolean);
     Procedure EnterCrit;
     Procedure LeaveCrit;
   protected
     //function GetNewImgFor(aStream: TStream): TLazIntfImage;
     procedure ClearCache;
     Function MapProvider2FileName(MapProvider: TMapProvider): String;
     Function DiskCached(const aFileName: String): Boolean;
     procedure LoadFromDisk(const aFileName: String; out item: TPictureCacheItem);
     Function GetFileName(MapProvider: TMapProvider; const TileId: TTileId): String;
   public
     Procedure CheckCacheSize(Sender: TObject);
     constructor Create(aOwner: TComponent); override;
     destructor Destroy; override;
     Procedure Add(MapProvider: TMapProvider; const TileId: TTileId; Stream: TMemoryStream);
     Procedure GetFromCache(MapProvider: TMapProvider; const TileId: TTileId; out item: TPictureCacheItem);
     function GetPreviewFromCache(MapProvider: TMapProvider; var TileId: TTileId; out ARect: TRect): boolean;
     function InCache(MapProvider: TMapProvider; const TileId: TTileId): Boolean;
     procedure Prepare(MapProvider: TMapProvider);

     property UseDisk: Boolean read FUseDisk write FUseDisk;
     property BasePath: String read FBasePath write FBasePath;
     property UseThreads: Boolean read FUseThreads write SetUseThreads;
     property CacheItemClass: TPictureCacheItemClass read FCacheItemClass write SetCacheItemClass;
     property MaxAge: Integer read FMaxAge write FMaxAge; // in days
   end;


implementation

uses
  GraphType, DateUtils, FPReadJPEG;

const
  MEMCACHE_MAX = 64;       // Tiles kept in memory
  MEMCACHE_SWEEP_CNT = 10; // Max tiles to be swept at once
  FLAT_CACHE = false;      // all cache files in flat folder, or grouped by provider and zoom

function IsValidPNG(AStream: TStream): Boolean;
var
  s: string = '';
  y: Int64;
begin
  if Assigned(AStream) then
  begin
    SetLength(s, 3);
    y := AStream.Position;
    AStream.Position := 1;
    AStream.Read(s[1], 3);
    AStream.Position := y;
    Result := (s = 'PNG');
  end
  else
    Result := false;
end;

function IsValidJPEG(AStream: TStream): Boolean;
var
  s: string = '';
  y: Int64;
begin
  if Assigned(AStream) then
  begin
    SetLength(s, 4);
    y := AStream.Position;
    AStream.Position := 6;
    AStream.Read(s[1], 4);
    AStream.Position := y;
    Result := (s = 'JFIF') or (s = 'Exif');
  end
  else
    Result := false;
end;

{ TPictureCacheItem }

function TPictureCacheItem.GetImageObject: TObject;
begin
  Result := Nil;
end;

class function TPictureCacheItem.GetImageReader(AStream: TStream
  ): TFPCustomImageReader;
begin
  Result := Nil;
  if not Assigned(AStream) then
     Exit;
  if IsValidJPEG(AStream) then
    Result := TFPReaderJPEG.Create
  else if IsValidPNG(AStream) then
    Result := TLazReaderPNG.Create;
end;

constructor TPictureCacheItem.Create(AStream: TStream);
begin
  {empty}
end;

destructor TPictureCacheItem.Destroy;
begin
  inherited Destroy;
end;

constructor TPictureCache.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FCacheItemClass := TPictureCacheItem;
  FMemMaxElem := MEMCACHE_MAX;
  FMaxAge := MaxInt;
  Cache := TStringList.Create;
end;

destructor TPictureCache.Destroy;
begin
  inherited;
  ClearCache;
  Cache.Free;
  FreeAndNil(Crit);
end;

procedure TPictureCache.SetUseThreads(AValue: Boolean);
begin
  if FUseThreads = AValue then Exit;
  FUseThreads := AValue;
  if aValue then
    Crit := TCriticalSection.Create
  else
    FreeAndNil(Crit);
end;

procedure TPictureCache.SetCacheItemClass(AValue: TPictureCacheItemClass);
begin
  if FCacheItemClass = AValue then Exit;
  FCacheItemClass := AValue;
  ClearCache;
end;

procedure TPictureCache.EnterCrit;
begin
  if Assigned(Crit) then
    Crit.Enter;
end;

procedure TPictureCache.LeaveCrit;
begin
  if Assigned(Crit) then
    Crit.Leave;
end;

{
function TPictureCache.GetNewImgFor(aStream: TStream): TLazIntfImage;
var
  reader: TFPCustomImageReader;
  rawImg: TRawImage;
begin
  Result := nil;
  Reader := nil;
  if not Assigned(aStream) then
     exit;
  if IsValidJPEG(astream) then
    Reader := TFPReaderJPEG.create
  else
  if IsValidPNG(astream) then
    Reader  := TLazReaderPNG.create;
  if Assigned(reader) then
  begin
    try
      rawImg.Init;
      rawImg.Description.Init_BPP24_B8G8R8_BIO_TTB(TILE_SIZE, TILE_SIZE);
      Result := TLazIntfImage.Create(rawImg, true);
      try
         Result.LoadFromStream(aStream, reader);
      except
         FreeAndNil(Result);
      end;
    finally
      FreeAndNil(Reader)
    end;
  end;
end;
}

procedure TPictureCache.ClearCache;
var
  I: integer;
begin
  EnterCrit;
  try
    for I := 0 to Pred(Cache.Count) do
      Cache.Objects[I].Free;
    Cache.Clear;
  finally
    LeaveCrit;
  end;
end;

function TPictureCache.MapProvider2FileName(MapProvider: TMapProvider): String;
var
  i: integer;
begin
  Result := '';
  if Assigned(MapProvider) then
  begin
    Result := MapProvider.Name;
    for i := 1 to Length(Result) do
      if not (Result[i] in ['a'..'z', 'A'..'Z', '0'..'9', '_', '.']) then
        Result[i] := '-';
  end;
end;

function TPictureCache.DiskCached(const aFileName: String): Boolean;
var
  FullFileName: string;
  Age: TDateTime;
begin
  if UseDisk then
  begin
    FullFileName := BasePath + aFileName;
    Result := FileAge(fullFileName, Age) and (DaysBetween(Now, Age) <= FMaxAge);
  end
  else
    Result := False;
end;

procedure TPictureCache.LoadFromDisk(const aFileName: String; out
  item: TPictureCacheItem);
var
  FullFileName: String;
  lStream: TFileStream;
begin
  item := nil;
  if DiskCached(aFileName) then
  begin
    FullFileName := BasePath + aFileName;
    lStream := TFileStream.Create(FullFileName, fmOpenRead);
    try
      try
        item := FCacheItemClass.Create(lStream); //GetNewImgFor(lStream);
      except
        FreeAndNil(item);
      end;
      if Assigned(item) then
      begin
        EnterCrit;
        try
          Cache.AddObject(aFileName, item);
        finally
          LeaveCrit;
        end;
      end;
    finally
      lStream.Free;
    end;
  end;
end;

function TPictureCache.GetFileName(MapProvider: TMapProvider;
  const  TileId: TTileId): String;
var
  prov: String;
begin
  prov := MapProvider2FileName(MapProvider);
  if FLAT_CACHE then
    Result := Format('%s_%d_%d_%d', [prov, TileId.X, TileId.Y, TileId.Z])
  else
    Result := SetDirSeparators(Format('%s/%d/%d_%d', [prov, TileID.Z, TileID.X, TileID.Y]));
end;

procedure TPictureCache.CheckCacheSize(Sender: TObject);
var
  i, idx: integer;
begin
  EnterCrit;
  try
    if Cache.Count > FMemMaxElem then
    begin
      for i := 1 to MEMCACHE_SWEEP_CNT do
      begin
        idx := pred(Cache.Count);
        if idx > 1 then
        begin
          Cache.Objects[idx].Free;
          Cache.Delete(idx);
        end;
      end;
    end;
  finally
    LeaveCrit;
  end;
end;

procedure TPictureCache.Add(MapProvider: TMapProvider;
  const TileId: TTileId; Stream: TMemoryStream);
var
  FileName: String;
  item: TPictureCacheItem;
  lFile: TFileStream;
  idx: integer;
begin
  FileName := GetFileName(MapProvider, TileId);
  EnterCrit;
  try
    item := FCacheItemClass.Create(Stream); //GetNewImgFor(Stream);
    idx := Cache.IndexOf(FileName);
    if idx <> -1 then
      Cache.Objects[idx].Free
    else
    begin
      Cache.Insert(0, FileName);
      idx := 0;
    end;
    Cache.Objects[idx]:=item;
  finally
    LeaveCrit;
  end;

  if UseDisk then
  begin
    if Assigned(item) then
    begin
      FileName := BasePath + FileName;
      ForceDirectories(ExtractFileDir(FileName));    // <--- to be removed !!!
      lFile := TFileStream.Create(FileName, fmCreate);
      try
        Stream.Position := 0;
        lFile.CopyFrom(Stream, 0);
      finally
        FreeAndNil(lFile);
      end;
    end;
  end;
end;

procedure TPictureCache.GetFromCache(MapProvider: TMapProvider;
  const TileId: TTileId; out item: TPictureCacheItem);
var
  FileName: String;
  idx: integer;
begin
  item := nil;
  FileName := GetFileName(MapProvider, TileId);
  EnterCrit;
  try
    idx := Cache.IndexOf(FileName);
    if idx <> -1 then
    begin
      item := TPictureCacheItem(Cache.Objects[idx]);
      if Idx > FMemMaxElem div 2 then
      begin
        Cache.Delete(idx);
        Cache.Insert(0, FileName);
        Cache.Objects[0] := item;
      end;
    end;

  finally
    LeaveCrit;
  end;
  if idx = -1 then
  begin
    if UseDisk then
       LoadFromDisk(FileName, item);
  end;
end;

{ When TileId is not yet in the cache, the function decreases zoom level and
  returns the TileID of a tile which already is in the cache, and in ARect
  the rectangle coordinates to get an upscaled preview of the originally
  requested tile. The function returns true in this case.
  If the requested tile already is in the cache, or no containing tile is found
  the function returns false indicating that not preview image must be
  generated. }
function TPictureCache.GetPreviewFromCache(MapProvider: TMapProvider;
  var TileId: TTileId; out ARect: TRect): boolean;
var
  ltid: TTileId;
  xfrac, yfrac: Double;
  lDeltaZoom: Integer;
  w, px, py: Integer;
begin
  Result := false;
  ARect := Rect(0, 0, 0, 0);

  if (TileId.Z < 0) or
     (TileId.X < 0) or
     (TileId.Y < 0) then exit;

  if InCache(MapProvider, TileID) then
    exit;

  if TileId.Z <= 0 then
    exit; // The whole earth as a preview, is simply the earth

  // The "preview" is the part of the containing tile that covers the location of the wanted tile
  // Every decrement of Zoom reduces the tile area by 4 (half of x and y direction)
  // So incrementing Z and dividing X and Y in the Id will lead us to the containing tile
  // The fraction of the division points to the location of the preview
  // e.g 0.5 = right or lower half of the tile, when divided by 2
  ltid := TileId;
  lDeltaZoom := 1;
  w := TileSize.CX;
  repeat
    w := w shr 1;
    dec(ltid.Z);
    lDeltaZoom := lDeltaZoom shl 1;
    xfrac := TileId.X / lDeltaZoom; // xfrac, yfrac contains the tile number
    yfrac := TileId.Y / lDeltaZoom;
    ltid.X := Trunc(xfrac);
    ltid.Y := Trunc(yfrac);
    if InCache(MapProvider, ltid) then
    begin // We found a tile in the cache that contains the preview
      xfrac := xfrac - ltid.X; //xfrac and yfrac calculated for the position in the tile from the cache
      yfrac := yfrac - ltid.Y;
      px := Trunc(xfrac * TileSize.CX); //x and y are the percentage of the tile width
      py := Trunc(yfrac * TileSize.CY);
      ARect := Rect(px, py, px+w, py+w);
      TileID := ltid;
      Result := true;
      exit;
    end;
  until (w <= 1) or (ltid.Z <= 0);
end;

function TPictureCache.InCache(MapProvider: TMapProvider;
  const TileId: TTileId): Boolean;
var
  FileName: String;
  idx: integer;
begin
  FileName := GetFileName(MapProvider, TileId);
  EnterCrit;
  try
    idx := Cache.IndexOF(FileNAme);
  finally
    LeaveCrit;
  end;
  if idx <> -1 then
     Result := True
  else
     Result := DiskCached(FileName);
end;

{ Makes sure that the subfolders needed by the given map provider exist in
  the cache directory. }
procedure TPictureCache.Prepare(MapProvider: TMapProvider);
var
  prov, dir: String;
  zoom, zoomMin, zoomMax: Integer;
begin
  if (not FLAT_CACHE) or (MapProvider = nil) then
    exit;
  prov := MapProvider2FileName(MapProvider);
  dir := BasePath + prov;
  ForceDirectories(dir);
  MapProvider.GetZoomInfos(zoomMin, zoomMax);
  for zoom := zoomMin to zoomMax do
  begin
    if not DirectoryExists(dir + IntToStr(zoom)) then
      CreateDir(dir + IntToStr(zoom));
  end;
end;

end.

