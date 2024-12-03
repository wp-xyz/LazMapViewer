{ A Demo project for using the lazQuadTree in conjunction with the lazMapViewer
  Copyright (C) 2024 Ekkehard Domning (www.domis.de)

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Version Date        Change
  0.0.1   2023-05-31  First release
}
unit uLazMapViwerQuadTreeDemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Windows, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls,
  mvMapViewer, mvGpsObj, mvDrawingEngine,
  ulazQuadTree, uOSMQuadTree,
  mvGeoMath, mvTypes,
  mvEngine;
const
  // The sample file is located at the directory of the exe file
  SamplePOIFileName = 'POI.csv';
  // This defines how many group items will be displyed in one tile
  MaxItemsPerTileSqr = 4; // should be 2,4,8, other values will be low performant
  MaxItemsPerTile = MaxItemsPerTileSqr*MaxItemsPerTileSqr; // will 4, 16 or 64
  ZoomInPerGroup = MaxItemsPerTileSqr div 2 ; // The value should fit to MaxItemsPerTile
  //
  MinItemsPerGroup = 4; // If more than MinItemsPerGroup are in a specific area (default 32px) then those items are grouped

type
  { TGpsPointGroupMarker
     Is displayed as a circle with the amount of grouped items
   }
   TGpsPointGroupMarker = class(TGPSPoint)
   private
     FGroupedItemsCount : Integer;
   public
     property GroupedItemsCount : Integer read FGroupedItemsCount write FGroupedItemsCount;
   end;

  { TForm1 }

  TForm1 = class(TForm)
    lblSampleImportResult: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    lblListItemsCount: TLabel;
    lblPOIDesc: TLabel;
    MapView: TMapView;
    Panel1: TPanel;
    Panel2: TPanel;
    rgDefaultVsQuadTree: TRadioGroup;
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MapViewChange(Sender: TObject);
    procedure MapViewDrawGpsPoint(Sender: TObject;
      ADrawer: TMvCustomDrawingEngine; APoint: TGpsPoint);
    procedure MapViewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MapViewMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MapViewMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rgDefaultVsQuadTreeClick(Sender: TObject);
  private
    // Track of the Mouse movement
    FMapViewMouseDownX : Integer;
    FMapViewMouseDownY : Integer;
    // Track of the change in the map display
    FLastMapArea : TRealArea;
    FLastMapCenter : TRealPoint;
    // The QuadTree for the POIs
    FTestQuadTree : TOSMQuadTree;
    // The extracted POI values from the file
    FPOICoords : array of TLazQuadNodePoint;
    FPOICaptions : TStringList;
    // Events / CallBacks for the specific implementation
    // First, the Events/Callbacks for the QuadTree to get the Coords of the POI
    function OnTestQuadTreeGetItemCoord(Sender : TObject; AItem : TObject; var AX, AY : Double) : Boolean;
//    function OnTestQuadTreeGetItemAreaEvent(Sender : TObject; AItem : TObject; var AItemArea : TLazQuadNodeArea) : Boolean;
    function OnTestQuadTreeGetItemCaptionEvent(Sender : TObject; AItem : TObject; var AItemCaption : String) : Boolean;
    // Second, the MapView Event for displaying the POIs
    // If the MapViewer-Component is updated, this Event could be declared automaticly using the ObjectInspector
    procedure OnMapViewBeforeDrawObjects(Sender: TObject);
    // A Helper function to calculate the tile coords of the displayed map
    // will be very helpful for derived procets
    procedure CalculateMapPixelCoords(var ALeft,ATop,ARight,ABottom : Int64; var AOSMMapSize : Int64; var AOfsX, AOfsY : Integer);
    // Import the Sample POIs
    procedure SamplePOIImport;

    procedure DoBeforeDrawObjects;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.MapViewChange(Sender: TObject);
begin
  lblPOIDesc.Caption := '';
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  // To simplify the conversion of the Coords, this format ist fixed here
  DefaultFormatSettings.DecimalSeparator:='.';
  DefaultFormatSettings.ThousandSeparator:=#0;
  FTestQuadTree := TOSMQuadTree.Create(False); // The Tree does not owns the items
  FTestQuadTree.MaxQuadTreeLevel := MapView.ZoomMax; //Equal Tree and Map-Zoom levels
  FTestQuadTree.OnGetItemCoord := @OnTestQuadTreeGetItemCoord;
//  FTestQuadTree.OnGetItemArea := @OnTestQuadTreeGetItemAreaEvent;
  FTestQuadTree.OnGetItemCaption := @OnTestQuadTreeGetItemCaptionEvent;
  FLastMapArea.Init(0,0,0,0);
  // If the MapViewer-Component is updated, this Event could be assigned using the ObjectInspector
  MapView.OnBeforeDrawObjects:=@OnMapViewBeforeDrawObjects;
  FPOICaptions := TStringList.Create;
end;

procedure TForm1.FormActivate(Sender: TObject);
var
  oldCursor : TCursor;
begin
  OnActivate := Nil;
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    // Import the Sample POIs on activating
    SamplePOIImport;
  finally
    Screen.Cursor := oldCursor;
  end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  // Destroy created objects
  FPOICaptions.Free;
  FTestQuadTree.Free;
end;

{MapViewDrawGpsPoint draws the GpsPoints of the MapViewer
   The drawing separates the display of the GroupItems and the single POIs }
procedure TForm1.MapViewDrawGpsPoint(Sender: TObject;
  ADrawer: TMvCustomDrawingEngine; APoint: TGpsPoint);
var
  P: TPoint;
  Pt: TPoint;
  PtCyc: TPointArray;
  s : String;
  d: Integer;
  clr: TColor;
  extent: TSize;
  lMapView: TMapView;
  cnt : Integer;
begin
  lMapView := TMapView(Sender);
  // Screen coordinates of the GPS point
  P := lMapView.LatLonToScreen(APoint.RealPoint);
  PtCyc := MapView.CyclicPointsOf(P);
  // Presetting
  clr := clRed;
  d := 5;
  ADrawer.PenColor := clr;
  ADrawer.PenWidth:= 3;
  for Pt in PtCyc do
  begin
    // If the Point is our GroupMarker, then display a circle with the amount of
    // grouped Items in in, if 10.000 and more then display "10K+" instead
    if APoint is TGpsPointGroupMarker then
    begin
      ADrawer.PenColor := clNavy;
      ADrawer.BrushStyle := bsSolid;
      ADrawer.BrushColor := clWhite;
      d := 10;
      ADrawer.Ellipse(Pt.X - d, Pt.Y - d,
                      Pt.X + d, Pt.Y + d);
      cnt := TGpsPointGroupMarker(APoint).GroupedItemsCount;
      if cnt > 9999 then
        s := '10K+'
      else
        s := IntToStr(cnt);
      extent := ADrawer.TextExtent(s);
      // ... and font
      ADrawer.BrushStyle := bsClear;
      ADrawer.FontColor := clBlack;
      ADrawer.FontName := 'Arial';
      ADrawer.FontSize := 6;
      ADrawer.FontStyle := [];

      // Write the POI text
      ADrawer.TextOut(Pt.X - (extent.CX div 2), Pt.Y - (extent.CY div 2), s);

    end
    else // Al other objects ends in a "red cross" (optional with Caption)
    begin
      ADrawer.Line(Pt.X - d, Pt.Y, Pt.X + d, Pt.Y);
      ADrawer.Line(Pt.X, Pt.Y - d, Pt.X , Pt.Y + d);
{ Displaying the Captions of the single POI will looks ugly due to overlapping
  something to do better in the future. Here just left for testing

      // Prepare text output: background color...
      extent := ADrawer.TextExtent(APoint.Name);
      ADrawer.BrushStyle := bsClear;
      // ... and font
      ADrawer.FontColor := MapView.Font.Color;
      ADrawer.FontName := MapView.Font.Name;
      ADrawer.FontSize := 6; // MapView.Font.Size;
      ADrawer.FontStyle := MapView.Font.Style;


      // Write the POI text
      ADrawer.TextOut(Pt.X - extent.CX div 2, Pt.Y + d + 4, APoint.Name);
}
    end;
  end;
end;

procedure TForm1.MapViewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // Keep track of the mouse
  FMapViewMouseDownX := X;
  FMapViewMouseDownY := Y;
end;

// MouseMove finds the Map-GpsItems under the mouse pointer and display their
// captions in a label
procedure TForm1.MapViewMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
const
  DELTA = 3;
var
  rArea: TRealArea;
  gpsList: TGpsObjList;
  sl : TStringList;
  i: Integer;
  pt : TPoint;
  foundgroupmarker : Boolean;
begin
  foundgroupmarker := False;
  pt.X := X-DELTA;
  pt.Y := Y-DELTA;
  rArea.TopLeft := MapView.ScreenToLatLon(pt);
  pt.X := X+DELTA;
  pt.Y := Y+DELTA;
  rArea.BottomRight := MapView.ScreenToLatLon(pt);
  gpsList := MapView.GpsItems.GetObjectsInArea(rArea);
  try
    if gpsList.Count > 0 then
    begin
      sl := TStringList.Create;
      try
        for i:=0 to gpsList.Count-1 do
        begin
          if ((gpsList[i] is TGpsPointGroupMarker)) then
            foundgroupmarker := True
          else if (gpsList[i] is TGpsPoint) then
            sl.Add(TGpsPoint(gpsList[i]).Name);
        end;
        lblPOIDesc.Caption := sl.Text;
      finally
        sl.Free;
      end;
    end
    else
      lblPOIDesc.Caption := '';
  finally
    gpsList.Free;
  end;
  if foundgroupmarker then
     // if Groupmarker was found, change the pointer to indicate that the item could be clicked
    MapView.Cursor := crHandPoint
  else
    MapView.Cursor := crDefault;
end;

// MouseUp will perform the "ungroup"-Action, but only if the
// Mouse was'nt moved to far.
// Clicking on the GroupItems will set the center of the Map to the group and
// increase the zoom so that the desired area is using the area of the group.
procedure TForm1.MapViewMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
const
  DELTA = 12;
var
  rArea: TRealArea;
  gpsList: TGpsObjList;
  L: TStrings;
  i: Integer;
  pt : TPoint;
begin
  if (Abs(FMapViewMouseDownX-X) > 2) or
     (Abs(FMapViewMouseDownY-Y) > 2) then Exit; // Mouse was moved, no Group action
  // calculate the Area of the possible Group Item
  pt.X := X-DELTA;
  pt.Y := Y-DELTA;
  rArea.TopLeft := MapView.ScreenToLatLon(pt);
  pt.X := X+DELTA;
  pt.Y := Y+DELTA;
  rArea.BottomRight := MapView.ScreenToLatLon(pt);
  // Query for GPSItems in the MapView
  gpsList := MapView.GpsItems.GetObjectsInArea(rArea);
  try
    if gpsList.Count > 0 then
    begin
      L := TStringList.Create;
      try
        for i:=0 to gpsList.Count-1 do
        begin
          if gpsList[i] is TGpsPointGroupMarker then
          begin  // Found a GroupItem, perform the zoom and exit
            MapView.Center := TGpsPointGroupMarker(gpsList[i]).RealPoint;
            MapView.Zoom := MapView.Zoom + ZoomInPerGroup;
            Exit;
          end;
        end;
      finally
        L.Free;
      end;
    end;
  finally
    gpsList.Free;
  end;
end;

procedure TForm1.rgDefaultVsQuadTreeClick(Sender: TObject);
var
  i : Integer;
  gpsPt : TGpsPoint;
  oldCursor : TCursor;
begin
  oldCursor := Screen.Cursor;
  Screen.Cursor := crHourGlass;
  try
    if rgDefaultVsQuadTree.ItemIndex = 1 then
    begin
      // If the user wants to see the default behaviour of the mapview, simply
      // drop all items into the MapViewers GpsItem list
      MapView.GPSItems.BeginUpdate;
      try
        MapView.GPSItems.Clear(0);
        for i := 0 to High(FPOICoords) do
        begin
          // Create a GpsPoint and add the POI to the list of the MapView
          gpsPt := TGpsPoint.Create(FPOICoords[i].X, FPOICoords[i].Y);
          gpsPt.Name := FPOICaptions[i];
          MapView.GPSItems.Add(gpsPt,0);
        end;
      finally
        MapView.GPSItems.EndUpdate;
      end;
    end
    else // if the groped behaviour should be shown, simply empty the list
    begin
      FLastMapArea.Init(0,0,0,0); // Force Repaint
      MapView.GPSItems.BeginUpdate;
      try
        MapView.GPSItems.Clear(0); // Takes surprisingly long!!
      finally
        MapView.GPSItems.EndUpdate;
      end;
    end;
    MapView.Invalidate;
    MapView.Repaint;
  finally
    Screen.Cursor := oldCursor;
  end;
end;

{ OnTestQuadTreeGetItemCoord is called, when the QuadTree needs the coordinates of an Item }
function TForm1.OnTestQuadTreeGetItemCoord(Sender: TObject; AItem: TObject;
  var AX, AY: Double): Boolean;
var
  ndx : NativeInt;
begin
  Result := False;
  ndx := NativeInt(AItem)-1;
  if (ndx < 0) or
     (ndx >= Length(FPOICoords)) then Exit;
  AX := FPOICoords[ndx].X;
  AY := FPOICoords[ndx].Y;
  Result := True;
end;
{ OnTestQuadTreeGetItemCaptionEvent is called when the QuadTree needs the caption of an Item }
function TForm1.OnTestQuadTreeGetItemCaptionEvent(Sender: TObject;
  AItem: TObject; var AItemCaption: String): Boolean;
var
  ndx : NativeInt;
begin
  Result := False;
  ndx := NativeInt(AItem)-1;
  if (ndx < 0) or
     (ndx >= FPOICaptions.Count) then Exit;
  AItemCaption := FPOICaptions[ndx];
  Result := True;
end;

{SamplePOIImport Imports the POIs from the sample file
The file is comma separated values in three columns, first longitude,
second latitude (both full degree plus decimal fraction), third Caption
Example:
13.3777900,52.5162700,"[DE-10117] Brandenburger Tor [D-Berlin]"
}
procedure TForm1.SamplePOIImport;
var
  sl, line : TStringList;
  i : Integer;
  llat, llon : Double;
  lineerr, cnt0, cnt1 : Integer;
  fn : String;
  ndx : Integer;
begin
  if MapView.GPSItems.Count > 0 then
    MapView.GPSItems.Clear(0);

  FLastMapArea.Init(0,0,0,0); // Force Repaint
  FTestQuadTree.Clear; // Clear the Tree
  // Clear the imported data
  SetLength(FPOICoords,0);
  FPOICaptions.Clear;
  sl := Nil;
  line := Nil;
  try
    sl := TStringList.Create;
    line := TStringList.Create;
    line.Delimiter:=',';
    line.StrictDelimiter := True;
    line.QuoteChar:='"';
    fn := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName))+SamplePOIFileName;
    // Load the file
    sl.LoadFromFile(fn);
    cnt0 := sl.Count;
    SetLength(FPOICoords,cnt0);
    cnt1 := 0;
    lineerr := -1;
    ndx := 0;
    for i := 0 to sl.Count-1 do
    begin
      line.DelimitedText := sl[i]; // Split one line
      if line.Count < 3 then Continue; // To few information
      // Convert the data
      llon := StrToFloatDef(line[0],-9999);
      llat := StrToFloatDef(line[1],-9999);
      if (llat < OSMMinLatitude) or (llat > OSMMaxLatitude) or
         (llon < OSMMinLongitude) or (llon > OSMMaxLongitude) then
      begin // Track error
        if lineerr < 0 then
          lineerr := i;
        Continue;
      end;
      // Assign the imported data
      FPOICoords[ndx].X := llon;
      FPOICoords[ndx].Y := llat;
      FPOICaptions.Add(line[2]);
      Inc(ndx);
      // Insert the data in the tree
      // We "misuse" the Item : TObject parameter by casting it to an Index to the
      // FPOICoords and FPOICaptions POI-Data
      // Since 0 would be interpreted as Nil, we increment the used index by 1
      // reducing this in the actual usage
      if not Assigned(FTestQuadTree.InsertItem(TObject(NativeInt(i+1)),
                      True, False, llon,llat)) then  // Point is Valid, Area is unvalid, Coords
      begin
        if lineerr < 0 then
          lineerr := i;
      end
      else
        Inc(cnt1);
    end;
    if lineerr >= 0 then
      lblSampleImportResult.Caption := Format('%d lines imported into %d objects. Error in Line %d',[cnt0,cnt1,lineerr])
    else
      lblSampleImportResult.Caption := Format('%d lines imported into %d objects',[cnt0,cnt1]);
  finally
    if Assigned(line) then
      line.Free;
    if Assigned(sl) then
      sl.Free;
    SetLength(FPOICoords, FPOICaptions.Count);
  end;
end;

procedure TForm1.OnMapViewBeforeDrawObjects(Sender: TObject);
begin
  MapView.BeginUpdateObjects;
  try
    DoBeforeDrawObjects;
  finally
    MapView.EndUpdateObjects;
  end;
end;

{ OnMapViewBeforeDrawObjects
    This Event is new and has to be implemented in the
    TMapView.Class
    It will be called in the TMapView.Paint method
    (and there in the method local procedure FullRedraw).
    This point allows any modification in the GPSItems-List of the
    MapViewer without any further side effects, since after
    returning from the event, this list is drawn.

    The implementation here ist the central link between the
    QuadTree and the MapView.
    The displayed map (and some overlapping area) are fractioned into
    OSM-Tiles plus smaller parts of it (defined on the very top of
    this file). If not altered, each 256px Tile is divided into
    4 by 4 32px width rectangles.
    For each of those rectangle the QuadTree is asked how many objects
    are stored in this are. This estimation(!) is very fast, since no actual item
    coordinate is queried, instead the internal counter are fetched.
    If the number is bigger than (if not altered) 4 then the items are grouped
    into an special GroupMarker.
    If equal or less then the items are displayed as generic GpsPoints.
    This limits the total displayed items on a full screen map:
    (1920 x 1080px => 60 x 33 fields => 1980 items.
}
procedure TForm1.DoBeforeDrawObjects;
var
  gpsPt : TGpsPoint;
  lItemGroupMarker : TGpsPointGroupMarker;
  maparea : TRealArea;
  llon, llat : Double;
  x, y : Integer;
  ofsx, ofsy : Integer;
  pt1, pt0 : TPoint;
  i : Integer;
  rpt0, rpt1 : TRealPoint;
  lNodeArea : TLazQuadNodeArea;
  itemcnt : Integer;
  estimateditemcnt : Double;
  lList : TList;
  tilecntx, tilecnty : Integer;
  s : String;
  pixelsperitem : Integer;
  mapwidthpx : Int64;
  lLeft,lTop,lRight,lBottom : Int64;
begin
  // If the User wants to see ungrouped items than we dont have anything to do here
  if (rgDefaultVsQuadTree.ItemIndex <> 0) then
  begin
    // Update the Number of GpsItems in the list to the GUI
    lblListItemsCount.Caption := IntToStr(MapView.GPSItems.Count);
    Exit;
  end;
  // Check if the Map has changed or moved in the mean time.
  // If not, exit.
  maparea := MapView.GetVisibleArea;
  rpt0 := MapView.Center;
  if FLastMapArea.Equal(maparea) and
    (FLastMapCenter.Lat = rpt0.Lat) and
    (FLastMapCenter.Lon = rpt0.Lon)  then Exit;
  // Update the current position of the map
  FLastMapCenter.InitXY(rpt0.Lon,rpt0.Lat);
  FLastMapArea.Init(maparea.TopLeft,maparea.BottomRight);

  // Keep the compiler calm
  rpt1.Lat := 0.0;
  rpt1.Lon := 0.0;
  ofsx := 0;
  ofsy := 0;
  lLeft := 0;
  lTop := 0;
  lRight := 0;
  lBottom := 0;
  mapwidthpx := 0;
  s := '';
  llat := 0.0;
  llon := 0.0;


  // Calculate the tiles area
  // The method returns the coordinates in pixel for the current map, adjusted
  // to the full OSM-Tiles. OfsX and OfsY are the pixel shift between the
  // display and the tile upperleft edge.
  CalculateMapPixelCoords(lLeft,lTop,lRight,lBottom,mapwidthpx,ofsx,ofsy);
  tilecntx := (lRight-lLeft) div TILE_SIZE;
  tilecnty := (lBottom-lTop) div TILE_SIZE;

  // Clear all Marker-Items in the MapView
  // This could be done smarter, by keeping unchanged items
  // but for the ease of demonstration, we restart everytime from scratch.
  MapView.GPSItems.Clear(0);

  pixelsperitem := OSMTileSize div MaxItemsPerTileSqr; // calculate the distance between two items on the map (default = 32 pixel)
  // Run to all of the small 32x32 pixels size parts of the map in all (even partly) visible tiles
  for y := 1 to tilecnty*MaxItemsPerTileSqr do
  begin
    for x := 1 to tilecntx*MaxItemsPerTileSqr do
    begin
      // Calculate the top-left corner of the 32x32 areas
      pt0.X := lLeft+((x-1)*pixelsperitem);
      // In the case of a cycling map, negative values representing the left side
      // of the DateBorder (= eastern hemisphere)
      if pt0.X < 0 then
        pt0.X := pt0.X + mapwidthpx
      else if pt0.X > mapwidthpx then
        pt0.X := pt0.X - mapwidthpx;
      pt0.Y := lTop+((y-1)*pixelsperitem);
      // Converting the pixel on the map into Latitude and Longitude
      PixelsToDegreesOSM (pt0.X,pt0.Y,MapView.Zoom,rpt0.Lat,rpt0.Lon);
      // The bottom-right corner is simple, since it fits to the tile borders, no overflow could happen
      pt1.X := pt0.X + pixelsperitem;
      pt1.Y := pt0.Y + pixelsperitem;
      PixelsToDegreesOSM (pt1.X,pt1.Y,MapView.Zoom,rpt1.Lat,rpt1.Lon);
      // Assign the real world coordinates to the area record used in the QuadTree
      lNodeArea.Top:=rpt0.Lat;
      lNodeArea.Left:=rpt0.Lon;
      lNodeArea.Bottom:=rpt1.Lat;
      lNodeArea.Right:=rpt1.Lon;
      // Estimate the item count in this specific area. This calculation is very fast
      // since no item positions are used, instead the accumulated counts are combined.
      estimateditemcnt := FTestQuadTree.EstimatedCountOfAssignedItemsInArea(lNodeArea);
      // If the count is bigger than 4 build a group marker.
      // However, if the zoom is maxed out, no groups are created.
      if (estimateditemcnt > MinItemsPerGroup) and (MapView.Zoom < MapView.ZoomMax) then
      begin
        // Create a group marker and insert him to the MapView GpsItem-List
        lItemGroupMarker := TGpsPointGroupMarker.Create((rpt0.Lon+rpt1.Lon)/2,(rpt0.Lat+rpt1.Lat)/2.0);
        lItemGroupMarker.GroupedItemsCount := Round(estimateditemcnt);
        MapView.GPSItems.Add(lItemGroupMarker,0);
      end
      else
      begin
        // The estimation said that there are POI items in this area.
        // But the estimation is not precise, so now count the items in the area.
        // This is still very fast, but some items have to manually counted, so this
        // tooks a bit longer than the estimation
        itemcnt := FTestQuadTree.CountAssignedItemsInArea(lNodeArea);
        if itemcnt > 0 then
        begin // if items existing in this area, copy them into a temporary list
          lList := TList.Create;
          try
            FTestQuadTree.ListItemsInArea(lNodeArea,lList); // Collect all items in the list
            for i := 0 to lList.Count-1 do
            begin
              // Ask the Item for it's coordinates (we reuse the Event-Method here)
              if OnTestQuadTreeGetItemCoord(Self,TObject(lList.Items[i]),llon,llat) then
              begin
                // Create a GpsPoint and add the POI to the list of the MapView
                gpsPt := TGpsPoint.Create(llon,llat);
                if OnTestQuadTreeGetItemCaptionEvent(Self,TObject(lList.Items[i]),s) then
                  gpsPt.Name := s;
                MapView.GPSItems.Add(gpsPt,0);
              end;
            end;
          finally
            lList.Free;
          end;
        end;
      end;
    end;
  end;
  // Update the Number of GpsItems in the list to the GUI
  lblListItemsCount.Caption := IntToStr(MapView.GPSItems.Count);
end;

{ CalculateMapPixelCoords
  calculates the coordinates of the displayed map.
  This coordinates are adjusted to the full tiles, that are necesarry
  to cover the displayed map.
  This means:
  On a small zoom level (approx. < 5) than the full earth is returned.
  If the map is in the Cyclic mode, depending of the center position,
  the left-Coordinate may be negative, indicating that the map extends to the
  eastern Hemisphere. If the right coordinate is larger than the MapSize, this
  indicates that the map extends to the western hemishere.
  Example: If the procedure return
  -256,0,256,512
  than full earth is shown in Zoom-Level 1, the date border is in the middle, subsequently
  the eastern hemisphere is displayed left, the western hemisphere is displayed right.
}
procedure TForm1.CalculateMapPixelCoords(var ALeft, ATop, ARight,
  ABottom: Int64; var AOSMMapSize: Int64; var AOfsX, AOfsY: Integer);
var
  pt0 : TPoint;
  osmapsz : Int64;
  wpx,hpx : Int64;
  minx, maxx, miny, maxy : Int64;
  mcl : Double;
  d : Double;
begin
  wpx := MapView.Width;
  hpx := MapView.Height;
  osmapsz := round(mvGeoMath.ZoomFactor(MapView.Zoom)) * TILE_SIZE;
  AOSMMapSize := osmapsz;
  mcl := MapView.Center.Lon;
  // The pixelcoordinates of the MapView Center on the screen are always on the map, never outside
  pt0 := DegreesToPixelsOSM(MapView.Zoom,MapView.Center.Lat,mcl);
  // calculate the offset to the upper-left corner of the OSM-Tile that contains the center
  AOfsX := (pt0.X mod TILE_SIZE);
  AOfsY := (pt0.Y mod TILE_SIZE);

  // Calculate the number of tiles to the left border of the MapView
  d := ((wpx / 2)-AOfsX) / TILE_SIZE;
  minx := Trunc(d);
  if Frac(d) > 0 then // if a fraction exists, add the full tile
    Inc(minx);
  // calculate the pixel coord of the tile. This eleminates the offset for later use
  minx := minx * TILE_SIZE;
  minx := pt0.X - AOfsX - minx;

  // Same for the distance to the right border
  d := ((wpx / 2)+AOfsX) / TILE_SIZE;
  maxx := Trunc(d);
  if Frac(d) > 0 then
    Inc(maxx);
  maxx := maxx * TILE_SIZE;
  maxx := pt0.X - AOfsX + maxx;

  // Catch the extra behaviour of the map
  // if the map is not cyclic (means no crossing of the DateBorder could happen
  if not MapView.Cyclic then
  begin
    // Limit the left and the right side
    if minx < 0 then
      minx := 0;
    if maxx > osmapsz then
      maxx := osmapsz;
  end;
  // In all cases, if the window is wider than the OSM-Map, than took the Map width
  if (maxx-minx) > osmapsz then
  begin
    minx := 0;
    maxx := osmapsz;
  end;

  // Samme procedure with the vertical size
  d := ((hpx / 2)-AOfsY) / TILE_SIZE;
  miny := Trunc(d);
  if Frac(d) > 0 then
    Inc(miny);
  miny := miny * TILE_SIZE;
  miny := pt0.Y - AOfsY - miny;

  d := ((hpx / 2)+AOfsY) / TILE_SIZE;
  maxy := Trunc(d);
  if Frac(d) > 0 then
    Inc(maxy);
  maxy := maxy * TILE_SIZE;
  maxy := pt0.Y - AOfsY + maxy;

  // And also limit to the maximum extent
  if (maxy-miny) > osmapsz then
  begin
    miny := 0;
    maxy := osmapsz;
  end;
  if miny < 0 then
    miny := 0;
  if maxy > osmapsz then
    maxy := osmapsz;

  // Assign the return values
  ALeft := minx;
  ARight := maxx;
  ATop := miny;
  ABottom := maxy;
end;


end.

