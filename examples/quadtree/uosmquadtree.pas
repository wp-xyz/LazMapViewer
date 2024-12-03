{ a Quad-Tree for Objects specialized for the use with Openstreetmap
  Copyright (C) 2023 Ekkehard Domning (www.domis.de)

  License: modified LGPL with linking exception (like RTL, FCL and LCL)
  Contains minor parts from the LazMapViewer-Project, see details:
  https://wiki.lazarus.freepascal.org/LazMapViewer

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  Version Date        Change
  0.0.1   2024-05-24  First release
}
unit uOSMQuadTree;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Math,
  uLazQuadTree;
const
  OSMTileSize = 256;
  OSMMinLatitude = -85.05112878;
  OSMMaxLatitude = 85.05112878;
  OSMMinLongitude = -180;
  OSMMaxLongitude = 180;

type
  { TOSMQuadNode a specialized QuadNode for the usage with Openstreetmap.
    Each node represent one OSM-Tile.
    The zoom is equal to the count of parents = NodeLevel.
    The pixel coordinates defines the north-west edge of the tile.
    The tile size of 256 pixel is assumed, the coordinates are forced to be
    multiples of 256 (or zero).
  }
  TOSMQuadNode = class(TLazQuadNode)
  private
    FPixelX : Int64;
    FPixelY : Int64;
  protected
    { GetChildAreas returns the covered areas of the 4 possible children }
    function GetChildAreas(Index : TLazQuadTreeChildLocation) : TLazQuadNodeArea;override;
  public
    property PixelX : Int64 read FPixelX;
    property PixelY : Int64 read FPixelY;
    constructor Create(const AOwner : TLazQuadTree; const AParentNode : TLazQuadNode; const APixelX, APixelY : Int64);
  end;
  { TOSMQuadTree specialized QuadTree for the usage with Openstreetmap.
    The class divides Nodes exactly so that they are allways fit to tiles.
  }
  TOSMQuadTree = class(TLazQuadTree)
  private
  protected
    { InternalCreateQuadNode creates the spezialized ChildNodes }
    function InternalCreateQuadNode(const AParentNode : TLazQuadNode;
                                    const ANodeArea : TLazQuadNodeArea) : TLazQuadNode;override;
  public
    constructor Create(const AFreeObjects: Boolean = False;
                       const AQuadTreeCreateQuadNodeEvent : TLazQuadTreeCreateQuadNodeEvent = Nil);
  end;
{ DegreesToPixelsOSM returns the coordinates in Pixels that correspond to the
    Latitude and Longitude and the ZoomLevel. }
function DegreesToPixelsOSM(const AZoom: Integer; const ALatitude, ALongitude : Double): TPoint;
procedure PixelsToDegreesOSM(const AX, AY : Int64; const AZoom: Integer; var ALatitude, ALongitude : Double);

implementation

{ Coordinate calculation are derived from the LazMapViewer Project from the
  Units mvGeoMath and mvEngine. }

const
  TWO_PI = 2.0 * pi;

  _K = 1024;
  _M = _K*_K;
  _G = _K*_M;
  ZOOM_FACTOR: array[0..32] of Int64 = (
     1, 2, 4, 8, 16, 32, 64, 128, 256, 512,                             //  0..9
    _K, 2*_K, 4*_K, 8*_K, 16*_K, 32*_K, 64*_K, 128*_K, 256*_K, 512*_K,  // 10..19
    _M, 2*_M, 4*_M, 8*_M, 16*_M, 32*_M, 64*_M, 128*_M, 256*_M, 512*_M,  // 20..29
    _G, 2*_G, 4*_G                                                      // 30..32
  );

function ZoomFactor(AZoomLevel: Integer): Int64;
begin
  if (AZoomLevel >= Low(ZOOM_FACTOR)) and (AZoomLevel < High(ZOOM_FACTOR)) then
    Result := ZOOM_FACTOR[AZoomLevel]
  else
    Result := round(IntPower(2, AZoomLevel));
end;

function DegreesToPixelsOSM(const AZoom: Integer; const ALatitude, ALongitude : Double): TPoint;
var
  factor, px, py: Extended;
  lLat, lLon : Double;
begin
  lLat := Math.EnsureRange(ALatitude, OSMMinLatitude, OSMMaxLatitude);
  lLon := Math.EnsureRange(ALongitude, OSMMinLongitude, OSMMaxLongitude);

  factor := OSMTileSize / TWO_PI * ZoomFactor(AZoom);
  px := factor * (DegToRad(lLon) + pi);
  py := factor * (pi - ln( tan(pi/4 + DegToRad(lLat)/2) ));

  Result.x := Round(px);
  Result.y := Round(py);
end;

procedure PixelsToDegreesOSM(const AX, AY : Int64; const AZoom: Integer; var ALatitude, ALongitude : Double);
var
  zoomfac: Int64;
  llat, llon : Double;
begin
  zoomFac := ZoomFactor(AZoom);
  llon := ( AX / (( OSMTileSize / (2*pi)) * zoomFac) ) - pi;
  llat := arctan( sinh(pi - (AY/OSMTileSize) / zoomFac * pi*2) );

  ALatitude := Math.EnsureRange(RadToDeg(llat), OSMMinLatitude, OSMMaxLatitude);
  ALongitude := Math.EnsureRange(RadToDeg(llon), OSMMinLongitude, OSMMaxLongitude);
end;


{ TOSMQuadNode }

function TOSMQuadNode.GetChildAreas(Index: TLazQuadTreeChildLocation
  ): TLazQuadNodeArea;

var
  lLLA : TLazQuadNodeArea;
  lChildPixelX, lChildPixelY : Int64;
  centerLat, centerLon : Double;
begin
  if Assigned(ChildNodes[Index]) then
    Result := ChildNodes[Index].NodeArea
  else
  begin
    { Calculate the center coords of this tile/node by calculating the pixel coordinates
      in a deeper zoom level and recalculates the real world coordinates.
      This prevents incremental rounding errors if using real world coordinates
      as the leading values.}
    lChildPixelX := (FPixelX * 2)+OSMTileSize; // In the next level the pixel coords are double
    lChildPixelY := (FPixelY * 2)+OSMTileSize;
    // Keep the compiler calm
    centerLat := 0.0;
    centerLon := 0.0;
    PixelsToDegreesOSM(lChildPixelX,lChildPixelY,NodeLevel+1,centerLat,centerLon);
    lLLA := NodeArea;  // Use the node area as a start value, two values will be correct.
    // Depending of the desired sub area overwrite the two other values
    case Index of
      qclNE :
        begin
          lLLA.Left   := centerLon;
          lLLA.Bottom := centerLat;
        end;
      qclSE :
        begin
          lLLA.Left := centerLon;
          lLLA.Top  := centerLat;
        end;
      qclSW :
        begin
          lLLA.Right := centerLon;
          lLLA.Top   := centerLat;
        end;
      qclNW :
        begin
          lLLA.Right  := centerLon;
          lLLA.Bottom := centerLat;
        end;
    end;
    Result := lLLA;
  end;
end;

constructor TOSMQuadNode.Create(const AOwner: TLazQuadTree;
  const AParentNode: TLazQuadNode; const APixelX, APixelY: Int64);
var
  lNodeArea : TLazQuadNodeArea;
  lZoom : Integer = 0;
begin
  // Keep the compiler calm
  lNodeArea.Left := 0.0;
  lNodeArea.Top := 0.0;
  lNodeArea.Right := 0.0;
  lNodeArea.Bottom := 0.0;
  // Calculate the zoom level
  if Assigned(AParentNode) then
    lZoom := AParentNode.NodeLevel+1;
  // Force Coords to be aligned with the Tile size
  FPixelX := APixelX and $FFFFFFFFFFFFFF00;
  FPixelY := APixelY and $FFFFFFFFFFFFFF00;
  // Calculate the the NodeArea in world coordinates
  PixelsToDegreesOSM(FPixelX,FPixelY,lZoom,lNodeArea.Top,lNodeArea.Left);
  PixelsToDegreesOSM(FPixelX+OSMTileSize,FPixelY+OSMTileSize,lZoom,lNodeArea.Bottom,lNodeArea.Right);
  // inherted create the class
  inherited Create(AOwner,AParentNode,lNodeArea);
end;

{ TOSMQuadTree }

function TOSMQuadTree.InternalCreateQuadNode(const AParentNode: TLazQuadNode;
  const ANodeArea: TLazQuadNodeArea): TLazQuadNode;
var
  lOSMQuadNode : TOSMQuadNode;
  pt : TPoint;
  lZoom : Integer = 0;
begin
  Result := Nil;
  if Assigned(OnQuadTreeCreateQuadNode) then
    Result := OnQuadTreeCreateQuadNode(Self, AParentNode, ANodeArea);
  if not Assigned(Result) then
  begin
    // calculate the zoom level
    if Assigned(AParentNode) then
      lZoom := AParentNode.NodeLevel+1;
    // convert the passed real world coords to pixel coords
    pt := DegreesToPixelsOSM(lZoom, ANodeArea.Top, ANodeArea.Left);
    // Create the node
    lOSMQuadNode := TOSMQuadNode.Create(Self,AParentNode,pt.X,pt.Y);
    Result := lOSMQuadNode;
  end;
end;

constructor TOSMQuadTree.Create(const AFreeObjects: Boolean;
  const AQuadTreeCreateQuadNodeEvent: TLazQuadTreeCreateQuadNodeEvent);
var
  ws : TLazQuadNodeArea;
begin
  ws.Left := OSMMinLongitude;
  ws.Top := OSMMaxLatitude;
  ws.Right := OSMMaxLongitude;
  ws.Bottom := OSMMinLatitude;
  inherited Create(ws,AFreeObjects,True,qydUpPlus,AQuadTreeCreateQuadNodeEvent);
end;

end.

