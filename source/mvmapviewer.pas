{ Initial map viewer library:
    Copyright (C) 2011 Maciej Kaczkowski / keit.co

  Extensions:
    (C) 2014 ti_dic@hotmail.com
    (C) 2019 Werner Pamler (user wp at Lazarus forum https://forum.lazarus.freepascal.org)
    (C) 2023 Yuliyan Ivanov (user alpine at Lazarus forum https://forum.lazarus.freepascal.org)

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

// ToDo: Make Active work at designtime.

// "Deprecated" warnings:
// - function names containing "LonLat" were copied and named to contain "LatLon"
//   (will be removed in v1.0)

unit mvMapViewer;

{$MODE objfpc}{$H+}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
interface

uses
  Classes, SysUtils, Types, fgl, FPImage,
  Controls, GraphType, Graphics, IntfGraphics,
  Forms, ImgList, LCLVersion,
  mvTypes, mvGeoMath, mvGPSObj, mvDragObj, mvCache, mvExtraData,
  mvEngine, mvMapProvider, mvDownloadEngine, mvDrawingEngine;

Type

  TDrawGpsPointEvent = procedure (Sender: TObject;
    ADrawer: TMvCustomDrawingEngine; APoint: TGpsPoint) of object;

  TMapViewOption =
  (
    mvoEditorEnabled,       // Point/Track editor enabled
    mvoMouseDragging,       // Allow dragging of the map with the mouse
    mvoMouseZooming,        // Allow zooming into the map with the mouse
    mvoLatLonInDMS          // Use Degrees, Minutes and Seconds for Lat/Lon
  );

  TMapViewOptions = set of TMapViewOption;

  TCacheLocation = (clProfile, clTemp, clCustom);

const
  DefaultMapViewOptions = [mvoMouseDragging, mvoMouseZooming];

type
  TMapItem = class;
  TMapView = class;
  TMapPoint = class;
  TMapLayer = class;
  TMapLayers = class;
  TPointOfInterest = class;
  TPointsOfInterest = class;
  TMapTrack = class;
  TMapTracks = class;
  TMapArea = class;
  TMapAreas = class;
  TGPSTileLayer = class;
  TGPSComboLayer = class;
  TMapEditMark = class;
  TMvCustomPluginManager = class;

  TPointOfInterestDrawEvent = procedure(Sender: TObject;
    ADrawer: TMvCustomDrawingEngine; APoint: TPointOfInterest) of object;

  TMapTrackDrawEvent = procedure(Sender: TObject;
    ADrawer: TMvCustomDrawingEngine; ATrack: TMapTrack) of object;

  TMapAreaDrawEvent = procedure(Sender: TObject;
    ADrawer: TMvCustomDrawingEngine; AArea: TMapArea) of object;

  { TMapObjectList }

  TMapObjectList = class(specialize TFPGObjectList<TObject>)
  public
    constructor Create(ASingleObj: TObject = Nil); reintroduce;
    constructor Create(AList: TMapObjectList);
    class function AddListToResult(AList, AResult: TMapObjectList): TMapObjectList;
  end;

  { TMapItem }

  TMapItem = class(TCollectionItem)
  private
    FCaption: TCaption;
    FTag: PtrInt;
    FVisible: Boolean;
    function GetGPSObj: TGPSObj; virtual; abstract;
    function GetView: TMapView; virtual;
    function GetLayer: TMapLayer; virtual;
    procedure SetCaption(AValue: TCaption);
    procedure SetVisible(AValue: Boolean);
  protected
    function GetDisplayName: string; override;
    procedure SetIndex(Value: Integer); override;
    procedure ItemChanged; virtual; abstract;
  public
    function HitTest(constref Area: TRealArea): TMapObjectList; virtual; abstract;
    property View: TMapView read GetView;
    property Layer: TMapLayer read GetLayer;
    property GPSObj: TGPSObj read GetGPSObj;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Tag: PtrInt read FTag write FTag default 0;
  end;

  { TMapCollectionBase }

  TMapCollectionBase = class(TOwnedCollection)
  private
    FBaseZ: Integer;
  protected
    function GetView: TMapView; virtual;
    function GetLayer: TMapLayer; virtual;
    procedure FixOrder(APrevIndex, AIndex: Integer); virtual;
    procedure Update(Item: TCollectionItem); override;
  public
    property View: TMapView read GetView;
    property Layer: TMapLayer read GetLayer;
    property BaseZ: Integer read FBaseZ write FBaseZ;
  end;

  { TMapCollection }

  generic TMapCollection<T: TMapItem; OT: class> = class(TMapCollectionBase)
  private
    type
      TItemClass = T;
      TOwnerClass = OT;
  private
    FMCOwner: TOwnerClass;
    function GetFirst: TItemClass;
    function GetItems(Index: Integer): TItemClass;
    function GetLast: TItemClass;
    procedure SetItems(Index: Integer; AValue: TItemClass);
  public
    constructor Create(AOwner: OT; ABaseZ: Integer);
    function HitTest(constref Area: TRealArea): TMapObjectList; virtual;
    property MCOwner: TOwnerClass read FMCOwner;
    property First: TItemClass read GetFirst;
    property Last: TItemClass read GetLast;
    property Items[Index: Integer]: TItemClass read GetItems write SetItems; default;
  end;

  { TMapLayer }

  TMapLayer = class(TMapItem)
  strict private
    FComboLayer: TGPSComboLayer;
    FDrawMode: TItemDrawMode;
    FUseThreads: Boolean;
    FMapProvider: String;
    FOpacity: Single;
    FPointsOfInterest: TPointsOfInterest;
    FAreas: TMapAreas;
    FTracks: TMapTracks;
  private
    function GetAreas: TMapAreas;
    function GetGPSObj: TGPSObj; override;
    function GetView: TMapView; override;
    function GetLayer: TMapLayer; override;
    function GetMapProvider: String;
    function GetPointsOfInterest: TPointsOfInterest;
    function GetTracks: TMapTracks;
    function GetUseThreads: Boolean;
    procedure SetAreas(AValue: TMapAreas);
    procedure SetDrawMode(AValue: TItemDrawMode);
    procedure SetMapProvider(AValue: String);
    procedure SetOpacity(AValue: Single);
    procedure SetPointsOfInterest(AValue: TPointsOfInterest);
    procedure SetTracks(AValue: TMapTracks);
    procedure SetUseThreads(AValue: Boolean);
  protected
    procedure ItemChanged; override;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    function HitTest(constref Area: TRealArea): TMapObjectList; override;
    function AddPointOfInterest(APoint: TRealPoint; ACaption: String = ''): TPointOfInterest;
    procedure AssignFromGPSList(AList: TGPSObjectList);
    property ComboLayer: TGPSComboLayer read FComboLayer;
  published
    property MapProvider: String read GetMapProvider write SetMapProvider;
    property UseThreads: Boolean read GetUseThreads write SetUseThreads default True;
    property DrawMode: TItemDrawMode read FDrawMode write SetDrawMode default idmUseOpacity;
    property Opacity: Single read FOpacity write SetOpacity default 0.25;
    property PointsOfInterest: TPointsOfInterest read GetPointsOfInterest write SetPointsOfInterest;
    property Areas: TMapAreas read GetAreas write SetAreas;
    property Tracks: TMapTracks read GetTracks write SetTracks;
  end;

  { TMapLayers }

  TMapLayers = class(specialize TMapCollection<TMapLayer, TMapView>)
  protected
    function GetView: TMapView; override;
    function GetLayer: TMapLayer; override;
    procedure FixOrder(APrevIndex, AIndex: Integer); override;
  end;

  { TMapCenter }

  TMapCenter = class(TPersistent)
  private
    FLatitude: Double;
    FLongitude: Double;
    FView: TMapView;
    function GetLatLonInDMS: Boolean;
    procedure SetLatitude(AValue: Double);
    procedure SetLongitude(AValue: Double);
    procedure SetViewCenter;
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AView: TMapView);
    property LatLonInDMS: Boolean read GetLatLonInDMS;
  published
    property Longitude: Double read FLongitude write SetLongitude;
    property Latitude: Double read FLatitude write SetLatitude;
  end;

  { TMapPoint }

  TMapPoint = class(TMapItem)
  private
    FDateTime: TDateTime;
    FElevation: Double;
    FLatitude: Double;
    FLongitude: Double;
    FPoint: TGPSPoint;
    function GetLatLonInDMS: Boolean;
    function GetRealPoint: TRealPoint;
    function GetToScreen: TPoint;
    function IsDateTimeStored: Boolean;
    function IsElevationStored: Boolean;
    procedure SetDateTime(AValue: TDateTime);
    procedure SetElevation(AValue: Double);
    procedure SetLatitude(AValue: Double);
    procedure SetLongitude(AValue: Double);
    procedure SetRealPoint(AValue: TRealPoint);
    function GetGPSObj: TGPSObj; override;
  protected
    procedure ItemChanged; override;
    function CreatePoint: TGPSPoint; virtual;
    procedure DestroyPoint; virtual;
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
    function HitTest(constref Area: TRealArea): TMapObjectList; override;
    property LatLonInDMS: Boolean read GetLatLonInDMS;
    property RealPoint: TRealPoint read GetRealPoint write SetRealPoint;
    property ToScreen: TPoint read GetToScreen;
  published
    property Longitude: Double read FLongitude write SetLongitude;
    property Latitude: Double read FLatitude write SetLatitude;
    property Elevation: Double read FElevation write SetElevation stored IsElevationStored;
    property DateTime: TDateTime read FDateTime write SetDateTime stored IsDateTimeStored;
  end;

  { TMapTrackPoint }

  TMapTrackPoint = class(TMapPoint)
  private
    FMark: TSegmentExtraData.TSegmentMark;
    function MarkIsStored: Boolean;
    procedure SetMark(AValue: TSegmentExtraData.TSegmentMark);
  protected
    function GPSTrack: TGPSTrack;
    function CreatePoint: TGPSPoint; override;
    procedure DestroyPoint; override;
  public
    procedure AssignTo(Dest: TPersistent); override;
  published
    property Mark: TSegmentExtraData.TSegmentMark read FMark write SetMark stored MarkIsStored;
  end;

  { TMapTrackPoints }

  TMapTrackPoints = class(specialize TMapCollection<TMapTrackPoint, TMapTrack>)
  protected
    function GetLayer: TMapLayer; override;
    procedure FixOrder(APrevIndex, AIndex: Integer); override;
  end;

  { TMapAreaPoint }

  TMapAreaPoint = class(TMapPoint)
  protected
    function GPSArea: TGPSArea;
    function CreatePoint: TGPSPoint; override;
    procedure DestroyPoint; override;
  end;

  { TMapAreaPoints }

  TMapAreaPoints = class(specialize TMapCollection<TMapAreaPoint, TMapArea>)
  protected
    function GetLayer: TMapLayer; override;
    procedure FixOrder(APrevIndex, AIndex: Integer); override;
  end;

  { TPointOfInterest }

  TPointOfInterest = class(TMapPoint)
  private
    FImageIndex: TImageIndex;
    FOnDrawPoint: TPointOfInterestDrawEvent;
    procedure SetImageIndex(AValue: TImageIndex);
    procedure SetOnDrawPoint(AValue: TPointOfInterestDrawEvent);
  protected
    procedure DrawPoint(Sender: TObject; {%H-}AGPSObj: TGPSObj; {%H-}AArea: TRealArea);
    procedure ItemChanged; override;
    function CreatePoint: TGPSPoint; override;
    procedure DestroyPoint; override;
  public
    constructor Create(ACollection: TCollection); override;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property ImageIndex: TImageIndex read FImageIndex write SetImageIndex default -1;
    property OnDrawPoint: TPointOfInterestDrawEvent read FOnDrawPoint write SetOnDrawPoint;
  end;

  { TPointsOfInterest }

  TPointsOfInterest = class(specialize TMapCollection<TPointOfInterest, TMapLayer>)
  protected
    function GetLayer: TMapLayer; override;
  end;

  { TMapTrack }

  TMapTrack = class(TMapItem)
  private
    FConnectColor: TColor;
    FConnectWidth: Double;
    FLineColor: TColor;
    FLineWidth: Double;
    FOpacity: Single;
    FPoints: TMapTrackPoints;
    FTrack: TGPSTrack;
    FOnDrawTrack: TMapTrackDrawEvent;
    function GetGPSObj: TGPSObj; override;
    function GetPoints: TMapTrackPoints;
    procedure SetConnectColor(AValue: TColor);
    procedure SetConnectWidth(AValue: Double);
    procedure SetLineColor(AValue: TColor);
    procedure SetLineWidth(AValue: Double);
    procedure SetOnDrawTrack(AValue: TMapTrackDrawEvent);
    procedure SetOpacity(AValue: Single);
    procedure SetPoints(AValue: TMapTrackPoints);
  protected
    procedure DrawTrack(Sender: TObject; {%H-}AGPSObj: TGPSObj; {%H-}AArea: TRealArea);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure ItemChanged; override;
    function HitTest(constref Area: TRealArea): TMapObjectList; override;
  published
    property LineColor: TColor read FLineColor write SetLineColor default clDefault;
    property LineWidth: Double read FLineWidth write SetLineWidth;
    property ConnectColor: TColor read FConnectColor write SetConnectColor default clNone;
    property ConnectWidth: Double read FConnectWidth write SetConnectWidth;
    property Opacity: Single read FOpacity write SetOpacity default 1.0;
    property Points: TMapTrackPoints read GetPoints write SetPoints;
    property OnDrawTrack: TMapTrackDrawEvent read FOnDrawTrack write SetOnDrawTrack;
  end;

  { TMapTracks }

  TMapTracks = class(specialize TMapCollection<TMapTrack, TMapLayer>)
  protected
    function GetLayer: TMapLayer; override;
  end;

  TMapArea = class(TMapItem)
  private
    FFillColor: TColor;
    FLineColor: TColor;
    FLineWidth: Double;
    FOpacity: Single;
    FPoints: TMapAreaPoints;
    FArea: TGPSArea;
    FOnDrawArea: TMapAreaDrawEvent;
    function GetGPSObj: TGPSObj; override;
    function GetPoints: TMapAreaPoints;
    procedure SetFillColor(AValue: TColor);
    procedure SetLineColor(AValue: TColor);
    procedure SetLineWidth(AValue: Double);
    procedure SetOnDrawArea(AValue: TMapAreaDrawEvent);
    procedure SetOpacity(AValue: Single);
    procedure SetPoints(AValue: TMapAreaPoints);
  protected
    procedure DrawArea(Sender: TObject; {%H-}AGPSObj: TGPSObj; {%H-}AArea: TRealArea);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure ItemChanged; override;
    function HitTest(constref Area: TRealArea): TMapObjectList; override;
  published
    property LineColor: TColor read FLineColor write SetLineColor default clDefault;
    property LineWidth: Double read FLineWidth write SetLineWidth;
    property FillColor: TColor read FFillColor write SetFillColor default clNone;
    property Opacity: Single read FOpacity write SetOpacity default 1.0;
    property Points: TMapAreaPoints read GetPoints write SetPoints;
    property OnDrawArea: TMapAreaDrawEvent read FOnDrawArea write SetOnDrawArea;
  end;

  { TMapAreas }

  TMapAreas = class(specialize TMapCollection<TMapArea, TMapLayer>)
  protected
    function GetLayer: TMapLayer; override;
  public
    destructor Destroy; override;
  end;


  // Additional (ooCustom) map observer operations
  TMapObserverCustomOperation = (mooSelectionCompleted, mooStartDrag, mooDrag,
    mooEndDrag, mooIsDirty);
  PMapObserverCustomOperation = ^TMapObserverCustomOperation;

  { TMvCustomPluginManager }

  TMvCustomPluginManager = class(TComponent)
  private
  protected
    procedure AddMapView(AMapView: TMapView); virtual;
    procedure RemoveMapView(AMapView: TMapView); virtual;

    procedure DefaultMouseEvent(AMapView: TMapView; AButton: TMouseButton;
      AShift: TShiftState; X, Y: Integer; AMapEvent: TMouseEvent);
    procedure DefaultNotifyEventHandler(AMapView: TMapView; AMapEvent: TNotifyEvent);

    procedure AfterDrawObjects(AMapView: TMapView; AMapEvent: TNotifyEvent;
                               out Handled : Boolean); virtual;
    procedure AfterPaint(AMapView: TMapView; AMapEvent: TNotifyEvent;
                         out Handled : Boolean); virtual;
    procedure BeforeDrawObjects(AMapView: TMapView; AMapEvent: TNotifyEvent;
                                out Handled : Boolean); virtual;
    procedure CenterMove(AMapView: TMapView; AMapEvent: TNotifyEvent); virtual;
    procedure MouseDown(AMapView: TMapView; AButton: TMouseButton;
                        AShift: TShiftState; X, Y: Integer; AMapEvent: TMouseEvent;
                        out Handled : Boolean); virtual;
    procedure MouseEnter(AMapView: TMapView; AMapEvent: TNotifyEvent;
                         out Handled : Boolean); virtual;
    procedure MouseLeave(AMapView: TMapView; AMapEvent: TNotifyEvent;
                         out Handled : Boolean); virtual;
    procedure MouseMove(AMapView: TMapView; AShift: TShiftState; X,Y: Integer;
                        AMapEvent: TMouseMoveEvent; out Handled : Boolean); virtual;
    procedure MouseUp(AMapView: TMapView; AButton: TMouseButton;
                      AShift: TShiftState; X, Y: Integer; AMapEvent: TMouseEvent;
                      out Handled : Boolean); virtual;
    procedure ZoomChange(AMapView: TMapView; AMapEvent: TNotifyEvent); virtual;
  public
  end;

  { TMapView }

  TMapView = class(TCustomControl)
    private
      FCacheLocation: TCacheLocation;
      FCachePath, FCacheFullPath: String;
      FCanvasSize: TSize;         // Needed for calculation of cyclic points
      FCenter: TMapCenter;
      FDownloadEngine: TMvCustomDownloadEngine;
      FBuiltinDownloadEngine: TMvCustomDownloadEngine;
      FBuiltinPluginManager: TMvCustomPluginManager;
      FPluginManager: TMvCustomPluginManager;
      FOnEditDrag: TNotifyEvent;
      FOnEditEndDrag: TNotifyEvent;
      FOnEditIsDirty: TNotifyEvent;
      FOnEditSelectionCompleted: TNotifyEvent;
      FOnEditStartDrag: TNotifyEvent;
      FEngine: TMapViewerEngine;
      FBuiltinDrawingEngine: TMvCustomDrawingEngine;
      FDrawingEngine: TMvCustomDrawingEngine;
      FActive: boolean;
      FLayers: TMapLayers;
      FGPSItems: array [0..9] of TGPSObjectList;
      FSavedOnModifiedEvents: array of TModifiedEvent;
      FOptions: TMapViewOptions;
      FPOIImage: TCustomBitmap;
      FPOITextBgColor: TColor;
      FOnDrawGpsPoint: TDrawGpsPointEvent;
      FDebugTiles: Boolean;
      FDefaultTrackColor: TColor;
      FDefaultTrackWidth: Integer;
      FFont: TFont;
      FPOIImages: TCustomImageList;
      FPOIImagesWidth: Integer;
      FCacheOnDisk: Boolean;
      FZoomMax: Integer;
      FZoomMin: Integer;
      FOnCenterMove: TNotifyEvent;
      FOnZoomChange: TNotifyEvent;
      FBeforeDrawObjectsEvent: TNotifyEvent;
      FAfterDrawObjectsEvent: TNotifyEvent;
      FAfterPaintEvent: TNotifyEvent;
      FEditMark: TMapEditMark;
      FDragger: TDragObj;
      procedure CallAsyncInvalidate;
      procedure DoAsyncInvalidate({%H-}Data: PtrInt);
      procedure DrawObjects(const {%H-}TileId: TTileId; aLeft, aTop, aRight,aBottom: integer);
      procedure DrawGpsObj(const {%H-}Area: TRealArea; AObj: TGPSObj);
      function GetCacheMaxAge: Integer;
      function GetCacheOnDisk: boolean;
      function GetCenter: TRealPoint;
      function GetCyclic: Boolean;
      function GetDownloadEngine: TMvCustomDownloadEngine;
      function GetDrawingEngine: TMvCustomDrawingEngine;
      function GetDrawPreviewTiles: Boolean;
      function GetGPSItems: TGPSObjectList;
      function GetGPSLayer(Layer: Integer): TGPSObjectList;
      function GetInactiveColor: TColor;
      function GetLayers: TMapLayers;
      function GetMapProvider: String;
//      function GetOnCenterMove: TNotifyEvent;
      function GetOnCenterMoving: TCenterMovingEvent;
      function GetOnChange: TNotifyEvent;
//      function GetOnZoomChange: TNotifyEvent;
      function GetOnZoomChanging: TZoomChangingEvent;
      function GetPluginManager: TMvCustomPluginManager;
      function GetUseThreads: boolean;
      function GetZoom: integer;
      function GetZoomToCursor: Boolean;
      function IsCacheMaxAgeStored: Boolean;
      function IsCachePathStored: Boolean;
      function IsFontStored: Boolean;
      function IsLayersStored: Boolean;
      procedure SetActive(AValue: boolean);
      procedure SetCacheLocation(AValue: TCacheLocation);
      procedure SetCacheMaxAge(AValue: Integer);
      procedure SetCacheOnDisk(AValue: boolean);
      procedure SetCachePath(AValue: String);
      procedure SetCenter(AValue: TRealPoint);
      procedure SetCyclic(AValue: Boolean);
      procedure SetDebugTiles(AValue: Boolean);
      procedure SetDefaultTrackColor(AValue: TColor);
      procedure SetDefaultTrackWidth(AValue: Integer);
      procedure SetDownloadEngine(AValue: TMvCustomDownloadEngine);
      procedure SetDrawingEngine(AValue: TMvCustomDrawingEngine);
      procedure SetDrawPreviewTiles(AValue: Boolean);
      procedure SetFont(AValue: TFont);
      procedure SetInactiveColor(AValue: TColor);
      procedure SetLayers(const ALayers: TMapLayers);
      procedure SetMapProvider(AValue: String);
//      procedure SetOnCenterMove(AValue: TNotifyEvent);
      procedure SetOnCenterMoving(AValue: TCenterMovingEvent);
      procedure SetOnChange(AValue: TNotifyEvent);
//      procedure SetOnZoomChange(AValue: TNotifyEvent);
      procedure SetOnZoomChanging(AValue: TZoomChangingEvent);
      procedure SetOptions(AValue: TMapViewOptions);
      procedure SetPluginManager(AValue: TMvCustomPluginManager);
      procedure SetPOIImage(const AValue: TCustomBitmap);
      procedure SetPOIImages(const AValue: TCustomImageList);
      procedure SetPOIImagesWidth(AValue: Integer);
      procedure SetPOITextBgColor(AValue: TColor);
      procedure SetUseThreads(AValue: boolean);
      procedure SetZoom(AValue: integer);
      procedure SetZoomMax(AValue: Integer);
      procedure SetZoomMin(AValue: Integer);
      procedure SetZoomToCursor(AValue: Boolean);
      procedure UpdateFont(Sender: TObject);
      procedure UpdateImage(Sender: TObject);

    protected
      AsyncInvalidate : boolean;
      procedure ActivateEngine;
      procedure DblClick; override;
      procedure DoCenterMove(Sender: TObject);
      procedure DoDrawStretchedTile(const TileId: TTileID; X, Y: Integer; TileImg: TPictureCacheItem; const R: TRect);
      procedure DoDrawTile(const TileId: TTileId; X,Y: integer; TileImg: TPictureCacheItem);
      procedure DoDrawTileInfo(const {%H-}TileID: TTileID; X,Y: Integer);
      procedure DoEraseBackground(const R: TRect);
      procedure DoTileDownloaded(const {%H-}TileId: TTileId);
      function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
        MousePos: TPoint): Boolean; override;
      procedure DoOnResize; override;
      procedure DoZoomChange(Sender: TObject);
      function FindObjsAtScreenPt(X, Y: Integer; ATolerance: Integer; AVisibleOnly: Boolean): TGPSObjArray;
      function IsActive: Boolean; inline;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer); override;
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
        X, Y: Integer); override;
      procedure MouseMove(Shift: TShiftState; X,Y: Integer); override;
      procedure MouseEnter; override;
      procedure MouseLeave; override;
      procedure Notification(AComponent: TComponent; Operation: TOperation); override;
      procedure Paint; override;
      procedure OnGPSItemsModified(Sender: TObject; objs: TGPSObjList; Adding: boolean);

      function CreateLayers: TMapLayers; virtual;
      procedure UpdateLayers;

      procedure CreateEditor;
      procedure DoneEditor;

      function EditingEnabled: Boolean; inline;
      function DraggingEnabled: Boolean; inline;

      procedure DoEditSelectionCompleted(Sender: TObject);
      procedure DoEditStartDrag(Sender: TObject);
      procedure DoEditDrag(Sender: TObject);
      procedure DoEditEndDrag(Sender: TObject);
      procedure DoEditIsDirty(Sender: TObject);

      procedure ChangeCachePath(AOldLoc: TCacheLocation; ANewPath: String);
      class function CacheDirectory(ALoc: TCacheLocation; ACustomPath: String): String;

    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure BeginUpdateObjects;
      procedure EndUpdateObjects;
      function CyclicPointOf(APoint: TPoint; ARefX: LongInt; Eastwards: Boolean = True): TPoint;
      function CyclicPointsOf(APoint: TPoint): TPointArray;
      function TrackLineColor(AColor: TColor; ExtraData: TObject): TColor;
      function TrackLineWidth(AWidth: Double; ExtraData: TObject): Integer;
      procedure DrawPointOfInterest(const {%H-}Area: TRealArea; APt: TGPSPointOfInterest);
      procedure DrawPt(const {%H-}Area: TRealArea; APt: TGPSPoint);
      procedure DrawTrack(const {%H-}Area: TRealArea; trk: TGPSTrack);
      procedure DrawArea(const {%H-}Area: TRealArea; ar: TGPSArea);
      procedure ClearBuffer;
      procedure GetMapProviders(lstProviders: TStrings);
      function GetVisibleArea: TRealArea;
      function LatLonToScreen(aPt: TRealPoint): TPoint;
      function LatLonToScreen(Lat, Lon: Double): TPoint; overload;
      function LonLatToScreen(aPt: TRealPoint): TPoint; deprecated 'Use LatLonToScreen';
      function ObjsAtScreenPt(X, Y: Integer; ATolerance: Integer = -1): TGPSObjArray;
      function VisibleObjsAtScreenPt(X, Y: Integer; ATolerance: Integer = -1): TGPSObjArray;
      procedure SaveToFile(AClass: TRasterImageClass; const AFileName: String);
      function SaveToImage(AClass: TRasterImageClass): TRasterImage;
      procedure SaveToStream(AClass: TRasterImageClass; AStream: TStream);
      function ScreenToLatLon(aPt: TPoint): TRealPoint;
      function ScreenToLonLat(aPt: TPoint): TRealPoint; deprecated 'Use ScreenToLatLon';
      procedure CenterOnObj(obj: TGPSObj);
      procedure Redraw; inline;
      function UsesDefaultDownloadEngine: Boolean;
      function UsesDefaultDrawingEngine: Boolean;
      procedure ZoomOnArea(const aArea: TRealArea);
      procedure ZoomOnObj(obj: TGPSObj);
      procedure WaitEndOfRendering;
      procedure StartDragging(X, Y: Integer);
      procedure EndDragging(X, Y: Integer);
      procedure AbortDragging;

      property Center: TRealPoint read GetCenter write SetCenter;
      property Engine: TMapViewerEngine read FEngine;
      property GPSItems: TGPSObjectList read GetGPSItems;
      property GPSLayer[L: Integer]: TGPSObjectList read GetGPSLayer;
      property EditMark: TMapEditMark read FEditMark;
    published
      property Active: boolean read FActive write SetActive default false;
      property Align;
      property CacheOnDisk: boolean read GetCacheOnDisk write SetCacheOnDisk default true;
      property CacheLocation: TCacheLocation read FCacheLocation write SetCacheLocation default clProfile;
      property CachePath: String read FCachePath write SetCachePath stored IsCachePathStored;
      property CacheFullPath: String read FCacheFullPath stored False;
      property CacheMaxAge: Integer read GetCacheMaxAge write SetCacheMaxAge stored IsCacheMaxAgeStored;
      property Cyclic: Boolean read GetCyclic write SetCyclic default false;
      property DebugTiles: Boolean read FDebugTiles write SetDebugTiles default false;
      property DefaultTrackColor: TColor read FDefaultTrackColor write SetDefaultTrackColor default clRed;
      property DefaultTrackWidth: Integer read FDefaultTrackWidth write SetDefaultTrackWidth default 1;
      property DownloadEngine: TMvCustomDownloadEngine read GetDownloadEngine write SetDownloadEngine;
      property DrawingEngine: TMvCustomDrawingEngine read GetDrawingEngine write SetDrawingEngine;
      property DrawPreviewTiles: Boolean read GetDrawPreviewTiles write SetDrawPreviewTiles default true;
      property Options: TMapViewOptions read FOptions write SetOptions default DefaultMapViewOptions;
      property Layers: TMapLayers read GetLayers write SetLayers stored IsLayersStored;
      property Font: TFont read FFont write SetFont stored IsFontStored;
      property Height default 150;
      property InactiveColor: TColor read GetInactiveColor write SetInactiveColor default clWhite;
      property MapProvider: String read GetMapProvider write SetMapProvider;
      property MapCenter: TMapCenter read FCenter write FCenter;
      property PluginManager: TMvCustomPluginManager read GetPluginManager write SetPluginManager;
      property POIImage: TCustomBitmap read FPOIImage write SetPOIImage;
      property POIImages: TCustomImageList read FPOIImages write SetPOIImages;
      property POIImagesWidth: Integer read FPOIImagesWidth write SetPOIImagesWidth default 0;
      property POITextBgColor: TColor read FPOITextBgColor write SetPOITextBgColor default clNone;
      property PopupMenu;
      property UseThreads: boolean read GetUseThreads write SetUseThreads default false;
      property Width default 150;
      property Zoom: integer read GetZoom write SetZoom default 1;
      property ZoomMax: Integer read FZoomMax write SetZoomMax default 19;
      property ZoomMin: Integer read FZoomMin write SetZoomMin default 1;
      property ZoomToCursor: Boolean read GetZoomToCursor write SetZoomToCursor default True;
      property OnAfterDrawObjects: TNotifyEvent read FAfterDrawObjectsEvent write FAfterDrawObjectsEvent;
      property OnAfterPaint: TNotifyEvent read FAfterPaintEvent write FAfterPaintEvent;
      property OnBeforeDrawObjects: TNotifyEvent read FBeforeDrawObjectsEvent write FBeforeDrawObjectsEvent;
      property OnCenterMove: TNotifyEvent read {Get}FOnCenterMove write {Set}FOnCenterMove;
      property OnCenterMoving: TCenterMovingEvent read GetOnCenterMoving write SetOnCenterMoving;
      property OnZoomChange: TNotifyEvent read {Get}FOnZoomChange write {Set}FOnZoomChange;
      property OnZoomChanging: TZoomChangingEvent read GetOnZoomChanging write SetOnZoomChanging;
      property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
      property OnDrawGpsPoint: TDrawGpsPointEvent read FOnDrawGpsPoint write FOnDrawGpsPoint;
      property OnEditSelectionCompleted: TNotifyEvent read FOnEditSelectionCompleted write FOnEditSelectionCompleted;
      property OnEditStartDrag: TNotifyEvent read FOnEditStartDrag write FOnEditStartDrag;
      property OnEditDrag: TNotifyEvent read FOnEditDrag write FOnEditDrag;
      property OnEditEndDrag: TNotifyEvent read FOnEditEndDrag write FOnEditEndDrag;
      property OnEditIsDirty: TNotifyEvent read FOnEditIsDirty write FOnEditIsDirty;
      property OnMouseDown;
      property OnMouseEnter;
      property OnMouseLeave;
      property OnMouseMove;
      property OnMouseUp;

  end;

  { TGPSTileLayerBase }

  TGPSTileLayerBase = class(TGPSObj)
  private
    FDrawMode: TItemDrawMode;
    FMapProvider: String;
    FOpacity: Single;
    FParentView: TMapView;
    FEngine: TMapViewerEngine;
    FParentViewChanged: Boolean;
    function GetMapProvider: String;
    function GetUseThreads: Boolean;
    procedure DoTileDownloaded(const TileId: TTileId);
    procedure DoDrawTile(const TileId: TTileId; X, Y: Integer; TileImg: TPictureCacheItem);
    procedure SetDrawMode(AValue: TItemDrawMode);
    procedure SetMapProvider(AValue: String);
    procedure SetOpacity(AValue: Single);
    procedure SetUseThreads(AValue: Boolean);
    procedure SetParentView(AValue: TMapView);
  protected
    procedure TileDownloaded(const {%H-}TileId: TTileId); virtual;
    procedure DrawTile(const TileId: TTileId; X, Y: Integer; TileImg: TPictureCacheItem); virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetArea(out Area: TRealArea); override;
    procedure Draw(AView: TObject; {%H-}Area: TRealArea); override;
    procedure ParentViewChanged; virtual;

    property MapProvider: String read GetMapProvider write SetMapProvider;
    property UseThreads: Boolean read GetUseThreads write SetUseThreads;
    property DrawMode: TItemDrawMode read FDrawMode write SetDrawMode;
    property Opacity: Single read FOpacity write SetOpacity;
  end;

  { TGPSTileLayer }

  TGPSTileLayer = class(TGPSTileLayerBase)
  protected
    procedure DrawTile(const {%H-}TileId: TTileId; X, Y: Integer;
      TileImg: TPictureCacheItem); override;
  public
    procedure Draw(AView: TObject; Area: TRealArea); override;
    procedure TileDownloaded(const {%H-}TileId: TTileId); override;
  end;

  { TGPSTileLayerLabels }

  TGPSTileLayerLabels = class(TGPSTileLayerBase)
  protected
    procedure DrawTile(const {%H-}TileId: TTileId; {%H-}X, {%H-}Y: Integer;
      {%H-}TileImg: TPictureCacheItem); override;
  public
    procedure Draw(AView: TObject; Area: TRealArea); override;
  end;

  { TGPSComboLayer }

  TGPSComboLayer = class(TGPSObjectList)
    FTileLayer: TGPSTileLayer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure GetArea(out Area: TRealArea); override;
    procedure Draw(AView: TObject; Area: TRealArea); override;
    property TileLayer: TGPSTileLayer read FTileLayer;
  end;

  { TMapEditMark }

  TMapEditMark = class(TGPSObj)
  private
    FMapView: TMapView;
    FOnDirty: TNotifyEvent;
    FOnDrag: TNotifyEvent;
    FOnEndDrag: TNotifyEvent;
    FOnSelectionCompleted: TNotifyEvent;
    FOnStartDrag: TNotifyEvent;
    FRealPt: TRealPoint;
    FPt: TPoint;
    FSelection: TMapObjectList;
    FLit: TMapObjectList;
    FOrigins: TRealPointArray;
    FDragStarted: Boolean;
    FMarquee: Boolean;
    FMarqueeRect: TRect;
    FDirty: Boolean;
    FTruncSelection: Boolean;
    FClearSelection: Boolean;

    function GetCurrentArea: TMapArea;
    function GetCurrentPoint: TMapPoint;
    function GetCurrentTrack: TMapTrack;
    function GetCursorShape: TCursor;
    function GetHasSelection: Boolean;

    function AroundPt(X, Y: Integer; APt: TPoint): Boolean;
    procedure SelectFromMarquee;
    procedure MarkDirty;
  public
    constructor Create(AMapView: TMapView);
    destructor Destroy; override;

    procedure GetArea(out Area: TRealArea); override;
    procedure Draw({%H-}AView: TObject; {%H-}Area: TRealArea); override;
    procedure UpdateFrom(AObjs: TMapObjectList); virtual;

    function ClickableAt({%H-}X, {%H-}Y: Integer): Boolean;
    function ClickAt(X, Y: Integer) : Boolean;
    procedure ClearSelection;
    procedure CompleteSelection;
    procedure Select(APoint: TMapPoint; ClearFirst: Boolean = False);
    function IsSelected(AObj: TObject): Boolean;

    procedure DoStartDrag(Sender: TDragObj);
    procedure DoDrag(Sender: TDragObj);
    procedure DoEndDrag(Sender: TDragObj);

    property Lon: Double read FRealPt.Lon write FRealPt.Lon;
    property Lat: Double read FRealPt.Lat write FRealPt.Lat;
    property RealPt: TRealPoint read FRealPt write FRealPt;

    property CursorShape: TCursor read GetCursorShape;
    property HasSelection: Boolean read GetHasSelection;
    property CurrentPoint: TMapPoint read GetCurrentPoint;
    property CurrentTrack: TMapTrack read GetCurrentTrack;
    property CurrentArea: TMapArea read GetCurrentArea;
    property Selection: TMapObjectList read FSelection;
    property Dirty: Boolean read FDirty write FDirty;

    property OnStartDrag: TNotifyEvent read FOnStartDrag write FOnStartDrag;
    property OnDrag: TNotifyEvent read FOnDrag write FOnDrag;
    property OnEndDrag: TNotifyEvent read FOnEndDrag write FOnEndDrag;
    property OnDirty: TNotifyEvent read FOnDirty write FOnDirty;
    property OnSelectionCompleted: TNotifyEvent read FOnSelectionCompleted write FOnSelectionCompleted;
  end;

  { TMapEditorListFilterEnumerator }

  generic TMapEditorListFilterEnumerator<T: class> = class
  private
    type
      TItemClass = T;
  private
    FList: TMapObjectList;
    FCurrent: TItemClass;
    FIndex: Integer;
  public
    constructor Create(AList: TMapObjectList);
    function MoveNext: Boolean;
    property Current: TItemClass read FCurrent;
    function GetEnumerator: TMapEditorListFilterEnumerator;
    function Skip(ANum: Integer = 1): TMapEditorListFilterEnumerator;
  end;

  TMapEditorPointsFilterEnumerator = specialize TMapEditorListFilterEnumerator<TMapPoint>;
  TMapEditorTracksFilterEnumerator = specialize TMapEditorListFilterEnumerator<TMapTrack>;
  TMapEditorAreasFilterEnumerator = specialize TMapEditorListFilterEnumerator<TMapArea>;

  { TMapEditorList }

  TMapEditorList = class helper for TMapObjectList
  private
    function GetAreasOnly: TMapEditorAreasFilterEnumerator;
    function GetPointsOnly: TMapEditorPointsFilterEnumerator;
    function GetTracksOnly: TMapEditorTracksFilterEnumerator;
  public
    function IndexOfObj(const Item: TObject; out Index: Integer): Boolean;
    function DelIfPresent(const Item: TObject): Boolean;
    function AddIfNotPresent(const Item: TObject): Boolean; inline;

    property Points: TMapEditorPointsFilterEnumerator read GetPointsOnly;
    property Tracks: TMapEditorTracksFilterEnumerator read GetTracksOnly;
    property Areas: TMapEditorAreasFilterEnumerator read GetAreasOnly;
  end;


implementation

uses
  FileUtil, LazLoggerBase, Math,
  mvJobQueue,
  mvDLEFPC,
  {$IFDEF MSWINDOWS}
  mvDLEWin,
  {$ENDIF}
  mvDE_IntfGraphics;

const
  RANGE_Z = (High(Integer) div 4);
  BASE_Z_LAYER = Low(Integer);
  BASE_Z_AREA = Low(Integer);
  BASE_Z_TRACK = BASE_Z_AREA + RANGE_Z;
  BASE_Z_POI = BASE_Z_TRACK + RANGE_Z;

  POINT_DELTA = 3;       // Pixel tolerance to find a clicked GPS object

  _TILELAYERS_ID_ = -42; // OwnerIDs of the tile layers
  _MAPEDITOR_ID_ = -42;

{ Converts a length given in millimeters to screen pixels }
function mmToPx(AValue: Double): Integer;
begin
  Result := round(AValue / 25.4 * ScreenInfo.PixelsPerInchX);
end;

type

  { TDrawObjJob }

  TDrawObjJob = class(TJob)
  private
    AllRun: boolean;
    Viewer: TMapView;
    FRunning: boolean;
    FLst: TGPSObjList;
    FStates: Array of integer;
    FArea: TRealArea;
  protected
    function pGetTask: integer; override;
    procedure pTaskStarted(aTask: integer); override;
    procedure pTaskEnded(aTask: integer; aExcept: Exception); override;
  public
    procedure ExecuteTask(aTask: integer; {%H-}FromWaiting: boolean); override;
    function Running: boolean;override;
  public
    constructor Create(aViewer: TMapView; aLst: TGPSObjList; const aArea: TRealArea);
    destructor Destroy; override;
  end;

{ TMapAreaPoints }

function TMapAreaPoints.GetLayer: TMapLayer;
begin
  Result := MCOwner.Layer;
end;

procedure TMapAreaPoints.FixOrder(APrevIndex, AIndex: Integer);
var
  I, T, B: Integer;
  Area: TGPSArea;
  O: TGPSPoint;
begin
  if Assigned(MCOwner) and (MCOwner is TMapArea)
    then Area := (MCOwner as TMapArea).FArea
    else Exit;
  T := Min(APrevIndex, AIndex);
  B := Max(APrevIndex, AIndex);
  if APrevIndex < 0 then
  begin
    T := AIndex;
    B := Pred(Count);
  end;
  for I := T to B do
  begin
    O := TGPSPoint(TMapItem(Items[I]).GPSObj);
    if Area.Points.Extract(O) <> Nil then
      Area.Points.Insert(I, O);
  end;
end;

{ TMapAreas }

function TMapAreas.GetLayer: TMapLayer;
begin
  Result := MCOwner;
end;

destructor TMapAreas.Destroy;
begin
  inherited Destroy;
end;

{ TMapArea }

function TMapArea.GetGPSObj: TGPSObj;
begin
  Result := FArea;
end;

function TMapArea.GetPoints: TMapAreaPoints;
begin
  Result := FPoints;
end;

procedure TMapArea.SetFillColor(AValue: TColor);
begin
  if FFillColor=AValue then Exit;
  FFillColor:=AValue;
  ItemChanged;
end;

procedure TMapArea.SetLineColor(AValue: TColor);
begin
  if FLineColor = AValue then Exit;
  FLineColor := AValue;
  ItemChanged;
end;

procedure TMapArea.SetLineWidth(AValue: Double);
begin
  if FLineWidth = AValue then Exit;
  FLineWidth := AValue;
  ItemChanged;
end;

procedure TMapArea.SetOnDrawArea(AValue: TMapAreaDrawEvent);
begin
  if CompareMem(@FOnDrawArea, @AValue, SizeOf(TMethod)) then
    Exit;
  FOnDrawArea := AValue;
  if Assigned(FOnDrawArea)
    then FArea.OnDrawObj := @DrawArea
    else FArea.OnDrawObj := Nil;
  ItemChanged;
end;

procedure TMapArea.SetOpacity(AValue: Single);
begin
  AValue := EnsureRange(AValue, 0.0, 1.0);
  if FOpacity = AValue then
    Exit;
  FOpacity:=AValue;
  ItemChanged;
end;

procedure TMapArea.SetPoints(AValue: TMapAreaPoints);
begin
  FPoints.Assign(AValue);
end;

procedure TMapArea.DrawArea(Sender: TObject; AGPSObj: TGPSObj; AArea: TRealArea
  );
begin
  if Assigned(FOnDrawArea) then
    FOnDrawArea(Sender, (Collection as TMapAreas).GetView.DrawingEngine, Self);
end;

constructor TMapArea.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FOpacity := 1.0;
  FLineColor := clDefault;
  FLineWidth := -1;
  FFillColor := clNone;
  FVisible := True;
  FPoints := TMapAreaPoints.Create(Self, 0);
  FArea := TGPSArea.Create;
  Layer.ComboLayer.Add(FArea, Pred(_TILELAYERS_ID_), Self.Index + BASE_Z_AREA);
end;

destructor TMapArea.Destroy;
begin
  FPoints.Free;
  if Assigned(FArea) then
    Layer.ComboLayer.Delete(FArea);
  inherited Destroy;
end;

procedure TMapArea.ItemChanged;
begin
  FArea.Name := Caption;
  FArea.LineColor := LineColor;
  FArea.LineWidth := LineWidth;
  FArea.FillColor := FillColor;
  FArea.Opacity := Opacity;
  FArea.Visible := Visible;
  Changed(False);
end;

function TMapArea.HitTest(constref Area: TRealArea): TMapObjectList;
begin
  Result := Nil;
  if not Visible then
    Exit;
  Result := Points.HitTest(Area);
  if Assigned(Result) then
    Result.Add(Self);
end;

{ TMapAreaPoint }

function TMapAreaPoint.GPSArea: TGPSArea;
begin
  Result := Nil;
  if Assigned(Collection) and (Collection is TMapAreaPoints) then
    with (Collection as TMapAreaPoints) do
      if Assigned(MCOwner) and (MCOwner is TMapArea) then
        Result := (MCOwner as TMapArea).FArea;
end;

function TMapAreaPoint.CreatePoint: TGPSPoint;
var
  Area: TGPSArea;
begin
  Result := inherited CreatePoint;
  Area := GPSArea;
  if Assigned(Area)
    then Area.Points.Add(Result);
end;

procedure TMapAreaPoint.DestroyPoint;
var
  Area: TGPSArea;
begin
  Area := GPSArea;
  if Assigned(Area) and Assigned(FPoint)
    then Area.Points.Remove(FPoint);
end;

{ TMapObjectList }

constructor TMapObjectList.Create(ASingleObj: TObject);
begin
  inherited Create(False);
  if Assigned(ASingleObj) then
    Add(ASingleObj);
end;

constructor TMapObjectList.Create(AList: TMapObjectList);
begin
  inherited Create(False);
  if Assigned(AList) then
    AddList(AList);
end;

class function TMapObjectList.AddListToResult(AList, AResult: TMapObjectList): TMapObjectList;
begin
  Result := AResult;
  if Assigned(AList) then
    if Assigned(AResult) then
    begin
      Result.AddList(AList);
      AList.Free;
    end
    else
      Result := AList;
end;

{ TMapTrackPoint }

function TMapTrackPoint.CreatePoint: TGPSPoint;
var
  Trk: TGPSTrack;
begin
  Result := inherited CreatePoint;
  Trk := GPSTrack;
  if Assigned(Trk)
    then Trk.Points.Add(Result);
end;

procedure TMapTrackPoint.DestroyPoint;
var
  Trk: TGPSTrack;
begin
  Trk := GPSTrack;
  if Assigned(Trk) and Assigned(FPoint)
    then Trk.Points.Remove(FPoint);
end;

procedure TMapTrackPoint.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TMapTrackPoint then
    TMapTrackPoint(Dest).Mark := Self.Mark;
end;

function TMapTrackPoint.MarkIsStored: Boolean;
begin
  Result := FMark <> smNone;
end;

procedure TMapTrackPoint.SetMark(AValue: TSegmentExtraData.TSegmentMark);
begin
  if FMark = AValue then
    Exit;
  FMark := AValue;
  if (AValue = smNone) then
    FPoint.ExtraData.Free
  else
  begin
    if not Assigned(FPoint.ExtraData)
      then FPoint.ExtraData := TSegmentExtraData.Create(AValue)
      else TSegmentExtraData(FPoint.ExtraData).Mark := AValue;
  end;
end;

function TMapTrackPoint.GPSTrack: TGPSTrack;
begin
  Result := Nil;
  if Assigned(Collection) and (Collection is TMapTrackPoints) then
    with (Collection as TMapTrackPoints) do
      if Assigned(MCOwner) and (MCOwner is TMapTrack) then
        Result := (MCOwner as TMapTrack).FTrack;
end;

{ TMapCollectionBase }

function TMapCollectionBase.GetView: TMapView;
begin
  if Assigned(Layer)
    then Result := Layer.View
    else Result := Nil;
end;

function TMapCollectionBase.GetLayer: TMapLayer;
begin
  Result := Nil;
end;

procedure TMapCollectionBase.FixOrder(APrevIndex, AIndex: Integer);
var
  I, T, B: Integer;
begin
  T := Min(APrevIndex, AIndex);
  B := Max(APrevIndex, AIndex);
  if APrevIndex < 0 then
  begin
    T := AIndex;
    B := Pred(Count);
  end;
  for I := T to B do
    Layer.ComboLayer.ChangeZOrder(TMapItem(Items[I]).GPSObj, I + FBaseZ);
end;

procedure TMapCollectionBase.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(View) then
    View.Invalidate;
end;

{ TMapTrackPoints }

function TMapTrackPoints.GetLayer: TMapLayer;
begin
  Result := MCOwner.Layer;
end;

procedure TMapTrackPoints.FixOrder(APrevIndex, AIndex: Integer);
var
  I, T, B: Integer;
  Trk: TGPSTrack;
  O: TGPSPoint;
begin
  if Assigned(MCOwner) and (MCOwner is TMapTrack)
    then Trk := (MCOwner as TMapTrack).FTrack
    else Exit;
  T := Min(APrevIndex, AIndex);
  B := Max(APrevIndex, AIndex);
  if APrevIndex < 0 then
  begin
    T := AIndex;
    B := Pred(Count);
  end;
  for I := T to B do
  begin
    O := TGPSPoint(TMapItem(Items[I]).GPSObj);
    if Trk.Points.Extract(O) <> Nil then
      Trk.Points.Insert(I, O);
  end;
end;

{ TMapTrack }

procedure TMapTrack.SetOnDrawTrack(AValue: TMapTrackDrawEvent);
begin
  if CompareMem(@FOnDrawTrack, @AValue, SizeOf(TMethod)) then
    Exit;
  FOnDrawTrack := AValue;
  if Assigned(FOnDrawTrack)
    then FTrack.OnDrawObj := @DrawTrack
    else FTrack.OnDrawObj := Nil;
  ItemChanged;
end;

procedure TMapTrack.SetOpacity(AValue: Single);
begin
  AValue := EnsureRange(AValue, 0.0, 1.0);
  if FOpacity = AValue then
    Exit;
  FOpacity:=AValue;
  ItemChanged;
end;

function TMapTrack.GetGPSObj: TGPSObj;
begin
  Result := FTrack;
end;

function TMapTrack.GetPoints: TMapTrackPoints;
begin
  Result := FPoints;
end;

procedure TMapTrack.SetConnectColor(AValue: TColor);
begin
  if FConnectColor=AValue then Exit;
  FConnectColor:=AValue;
  ItemChanged;
end;

procedure TMapTrack.SetConnectWidth(AValue: Double);
begin
  if FConnectWidth=AValue then Exit;
  FConnectWidth:=AValue;
  ItemChanged;
end;

procedure TMapTrack.SetLineColor(AValue: TColor);
begin
  if FLineColor = AValue then Exit;
  FLineColor := AValue;
  ItemChanged;
end;

procedure TMapTrack.SetLineWidth(AValue: Double);
begin
  if FLineWidth = AValue then Exit;
  FLineWidth := AValue;
  ItemChanged;
end;

procedure TMapTrack.SetPoints(AValue: TMapTrackPoints);
begin
  FPoints.Assign(AValue);
end;

procedure TMapTrack.DrawTrack(Sender: TObject; AGPSObj: TGPSObj;
  AArea: TRealArea);
begin
  if Assigned(FOnDrawTrack) then
    FOnDrawTrack(Sender, (Collection as TMapTracks).GetView.DrawingEngine, Self);
end;

constructor TMapTrack.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FOpacity := 1.0;
  FLineColor := clDefault;
  FLineWidth := -1;
  FConnectColor := clNone;
  FConnectWidth := -1;
  FVisible := True;
  FPoints := TMapTrackPoints.Create(Self, 0);
  FTrack := TGPSTrack.Create;
  Layer.ComboLayer.Add(FTrack, Pred(_TILELAYERS_ID_), Self.Index + BASE_Z_TRACK);
end;

destructor TMapTrack.Destroy;
begin
  FPoints.Free;
  if Assigned(FTrack) then
    Layer.ComboLayer.Delete(FTrack);
  inherited Destroy;
end;

procedure TMapTrack.ItemChanged;
begin
  FTrack.Name := Caption;
  FTrack.LineColor := LineColor;
  FTrack.LineWidth := LineWidth;
  FTrack.ConnectColor := ConnectColor;
  FTrack.ConnectWidth := ConnectWidth;
  FTrack.Opacity := Opacity;
  FTrack.Visible := Visible;
  Changed(False);
end;

function TMapTrack.HitTest(constref Area: TRealArea): TMapObjectList;
begin
  Result := Nil;
  if not Visible then
    Exit;
  Result := Points.HitTest(Area);
  if Assigned(Result) then
    Result.Add(Self);
end;

{ TMapTracks }

function TMapTracks.GetLayer: TMapLayer;
begin
  Result := MCOwner;
end;

{ TMapItem }

function TMapItem.GetView: TMapView;
begin
  Result := Layer.View;
end;

function TMapItem.GetLayer: TMapLayer;
begin
  if Assigned(Collection)
    then Result := (Collection as TMapCollectionBase).Layer
    else Result := Nil;
end;

procedure TMapItem.SetCaption(AValue: TCaption);
begin
  if FCaption=AValue then Exit;
  FCaption:=AValue;
  ItemChanged;
end;

procedure TMapItem.SetVisible(AValue: Boolean);
begin
  if FVisible=AValue then Exit;
  FVisible:=AValue;
  ItemChanged;
end;

function TMapItem.GetDisplayName: string;
begin
  if FCaption <> ''
    then Result := FCaption
    else Result := ClassName;
  if not FVisible
    then Result := Result + ' (Invisible)';
end;

procedure TMapItem.SetIndex(Value: Integer);
var
  PrevIndex: Integer;
begin
  PrevIndex := Index;
  inherited SetIndex(Value);
  if (PrevIndex <> Index) and Assigned(Collection) then
    TMapCollectionBase(Collection).FixOrder(PrevIndex, Index);
end;

{ TMapCollection }

function TMapCollection.GetItems(Index: Integer): TItemClass;
begin
  Result := TItemClass(inherited GetItem(Index));
end;

function TMapCollection.GetFirst: TItemClass;
begin
  Result := GetItems(0);
end;

function TMapCollection.GetLast: TItemClass;
begin
  Result := GetItems(Pred(Count));
end;

procedure TMapCollection.SetItems(Index: Integer; AValue: TItemClass);
begin
  (GetItems(Index) as TPersistent).Assign(AValue);
end;

function TMapCollection.HitTest(constref Area: TRealArea): TMapObjectList;
var
  I: TCollectionItem;
begin
  Result := Nil;
  for I in Self do
    Result := TMapObjectList.AddListToResult(TItemClass(I).HitTest(Area), Result);
  if Assigned(Result) then
    Result.Add(Self);
end;

constructor TMapCollection.Create(AOwner: OT; ABaseZ: Integer);
begin
  inherited Create(AOwner, TItemClass);
  FMCOwner := AOwner;
  FBaseZ := ABaseZ;
end;

{ TGPSComboLayer }

procedure TGPSComboLayer.GetArea(out Area: TRealArea);
begin
  FTileLayer.GetArea(Area);
end;

constructor TGPSComboLayer.Create;
begin
  inherited Create;
  FTileLayer := TGPSTileLayer.Create;
end;

destructor TGPSComboLayer.Destroy;
begin
  FTileLayer.Free;
  inherited Destroy;
end;

procedure TGPSComboLayer.Draw(AView: TObject; Area: TRealArea);
var
  I: Integer;
  Objs: TGPSObjList;
begin
  inherited Draw(AView, Area);
  FTileLayer.Draw(AView, Area);
  Objs := GetObjectsInArea(Area);
  try
    if Objs.Count > 0 then
    begin
      for I := 0 to Pred(Objs.Count) do
        if Objs[I].Visible then
          Objs[I].Draw(AView, Area)
    end;
  finally
    FreeAndNil(Objs);
  end;
end;

{ TPointsOfInterest }

function TPointsOfInterest.GetLayer: TMapLayer;
begin
  Result := MCOwner;
end;

{ TMapPoint }

function TMapPoint.IsDateTimeStored: Boolean;
begin
  Result := not (FDateTime = NO_DATE);
end;

function TMapPoint.IsElevationStored: Boolean;
begin
  Result := not (FElevation = NO_ELE);
end;

procedure TMapPoint.SetDateTime(AValue: TDateTime);
begin
  if FDateTime=AValue then Exit;
  FDateTime:=AValue;
  ItemChanged;
end;

procedure TMapPoint.SetElevation(AValue: Double);
begin
  if FElevation=AValue then Exit;
  FElevation:=AValue;
  ItemChanged;
end;

function TMapPoint.GetLatLonInDMS: Boolean;
begin
  Result := Assigned(View) and (mvoLatLonInDMS in View.Options);
end;

function TMapPoint.GetRealPoint: TRealPoint;
begin
  Result := mvTypes.RealPoint(FLatitude, FLongitude);
end;

function TMapPoint.GetToScreen: TPoint;
begin
  Result := View.LatLonToScreen(GetRealPoint);
end;

procedure TMapPoint.SetLatitude(AValue: Double);
begin
  if FLatitude = AValue then Exit;
  FLatitude := AValue;
  ItemChanged;
end;

procedure TMapPoint.SetLongitude(AValue: Double);
begin
  if FLongitude = AValue then Exit;
  FLongitude := AValue;
  ItemChanged;
end;

procedure TMapPoint.SetRealPoint(AValue: TRealPoint);
begin
  if (FLatitude = AValue.Lat) and (FLongitude = AValue.Lon) then exit;
  FLatitude := AValue.Lat;
  FLongitude := AValue.Lon;
  ItemChanged;
end;

function TMapPoint.GetGPSObj: TGPSObj;
begin
  Result := FPoint;
end;

procedure TMapPoint.ItemChanged;
begin
  FPoint.Lon := Longitude;
  FPoint.Lat := Latitude;
  FPoint.Name := Caption;
  FPoint.Visible := Visible;
  FPoint.Elevation := Elevation;
  FPoint.DateTime := DateTime;
  Changed(False);
end;

function TMapPoint.HitTest(constref Area: TRealArea): TMapObjectList;
var
  BB: TRealArea;
begin
  Result := Nil;
  if not Visible then
    Exit;
  BB := Self.GPSObj.BoundingBox;
  if Area.ContainsPoint(BB.TopLeft) and Area.ContainsPoint(BB.BottomRight)
    then Result := TMapObjectList.Create(Self);
end;

function TMapPoint.CreatePoint: TGPSPoint;
begin
  Result := TGPSPoint.Create(FLongitude, FLatitude);
end;

procedure TMapPoint.DestroyPoint;
begin
end;

constructor TMapPoint.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FLongitude := View.Center.Lon;
  FLatitude := View.Center.Lat;
  FVisible := True;
  FElevation := NO_ELE;
  FDateTime := NO_DATE;
  FPoint := CreatePoint;
end;

destructor TMapPoint.Destroy;
begin
  DestroyPoint;
  inherited Destroy;
end;

procedure TMapPoint.AssignTo(Dest: TPersistent);
begin
  if Dest is TMapPoint then
    with TMapPoint(Dest) do
    begin
      Latitude := Self.Latitude;
      Longitude := Self.Longitude;
      Elevation := Self.Elevation;
      DateTime := Self.DateTime;
    end
  else
    inherited AssignTo(Dest);
end;

{ TPointOfInterest }

procedure TPointOfInterest.SetImageIndex(AValue: TImageIndex);
begin
  if FImageIndex = AValue then Exit;
  FImageIndex := AValue;
  ItemChanged;
end;

procedure TPointOfInterest.SetOnDrawPoint(AValue: TPointOfInterestDrawEvent);
begin
  if CompareMem(@FOnDrawPoint, @AValue, SizeOf(TMethod)) then
    Exit;
  FOnDrawPoint := AValue;
  if Assigned(FOnDrawPoint)
    then FPoint.OnDrawObj := @DrawPoint
    else FPoint.OnDrawObj := Nil;
  ItemChanged;
end;

procedure TPointOfInterest.DrawPoint(Sender: TObject; AGPSObj: TGPSObj;
  AArea: TRealArea);
begin
  if Assigned(FOnDrawPoint) then
    FOnDrawPoint(Sender, View.DrawingEngine, Self);
end;

procedure TPointOfInterest.ItemChanged;
begin
  TGPSPointOfInterest(FPoint).ImageIndex := FImageIndex;
  inherited ItemChanged;
end;

function TPointOfInterest.CreatePoint: TGPSPoint;
begin
  Result := TGPSPointOfInterest.Create(FLongitude, FLatitude);
  Layer.ComboLayer.Add(Result, Pred(_TILELAYERS_ID_), Self.Index + BASE_Z_POI);
end;

procedure TPointOfInterest.DestroyPoint;
begin
  if Assigned(FPoint) then
    Layer.ComboLayer.Delete(FPoint);
end;

constructor TPointOfInterest.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FImageIndex := -1;
end;

procedure TPointOfInterest.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if Dest is TPointOfInterest then
    TPointOfInterest(Dest).ImageIndex := Self.ImageIndex;
end;

{ TMapCenter }

procedure TMapCenter.SetLongitude(AValue: Double);
begin
  if FLongitude = AValue then Exit;
  FLongitude := AValue;
  SetViewCenter;
end;

procedure TMapCenter.SetLatitude(AValue: Double);
begin
  if FLatitude = AValue then Exit;
  FLatitude := AValue;
  SetViewCenter;
end;

function TMapCenter.GetLatLonInDMS: Boolean;
begin
  Result := Assigned(FView) and (mvoLatLonInDMS in FView.Options);
end;

procedure TMapCenter.SetViewCenter;
var
  R: TRealPoint;
begin
  R.InitLatLon(FLatitude, FLongitude);
  FView.SetCenter(R);
end;

function TMapCenter.GetOwner: TPersistent;
begin
  Result := FView;
end;

constructor TMapCenter.Create(AView: TMapView);
begin
  FView := AView;
end;

{ TMapLayer }

//function TMapLayer.GetMapView: TMapView;
//begin
//  if Collection is TMapLayers
//    then Result := (Collection as TMapLayers).MCOwner //MapView
//    else Result := Nil;
//end;

function TMapLayer.GetPointsOfInterest: TPointsOfInterest;
begin
  Result := FPointsOfInterest;
end;

function TMapLayer.GetTracks: TMapTracks;
begin
  Result := FTracks;
end;

function TMapLayer.GetAreas: TMapAreas;
begin
  Result := FAreas;
end;

function TMapLayer.GetGPSObj: TGPSObj;
begin
  Result := FComboLayer;
end;

function TMapLayer.GetView: TMapView;
begin
  if (Collection is TMapLayers)
    then Result := (Collection as TMapLayers).View
    else Result := Nil;
end;

function TMapLayer.GetLayer: TMapLayer;
begin
  Result := Self;
end;

function TMapLayer.GetMapProvider: String;
begin
  Result := ComboLayer.TileLayer.MapProvider
end;

function TMapLayer.GetUseThreads: Boolean;
begin
  Result := FUseThreads;
end;

procedure TMapLayer.SetAreas(AValue: TMapAreas);
begin
  FAreas.Assign(AValue);
end;

procedure TMapLayer.SetDrawMode(AValue: TItemDrawMode);
begin
  if FDrawMode=AValue then Exit;
  FDrawMode:=AValue;
  ItemChanged;
end;

procedure TMapLayer.SetMapProvider(AValue: String);
var
  P: TMapProvider;
  LPS, MPS: String;
begin
  if FMapProvider = AValue then
    Exit;
  // Check compat. of provider projection type against the base provider.
  if Assigned(View) then
  begin
    P := View.Engine.MapProviderByName(AValue);
    if Assigned(P) and (View.Engine.MapProjectionType <> P.ProjectionType) then
    begin
      WriteStr(LPS, View.Engine.MapProjectionType);
      WriteStr(MPS, P.ProjectionType);
      raise EArgumentException.CreateFmt(
        '%s has different projection type (%s) from the base map (%s).',
        [AValue, LPS, MPS]);
    end;
  end;
  FMapProvider := AValue;
  ItemChanged;
end;

procedure TMapLayer.SetOpacity(AValue: Single);
begin
  AValue := EnsureRange(AValue, 0.0, 1.0);
  if FOpacity = AValue then
    Exit;
  FOpacity:=AValue;
  ItemChanged;
end;

procedure TMapLayer.SetPointsOfInterest(AValue: TPointsOfInterest);
begin
  FPointsOfInterest.Assign(AValue);
end;

procedure TMapLayer.SetTracks(AValue: TMapTracks);
begin
  FTracks.Assign(AValue);
end;

procedure TMapLayer.SetUseThreads(AValue: Boolean);
begin
  if FUseThreads = AValue then
    Exit;
  FUseThreads := AValue;
  ItemChanged;
end;

procedure TMapLayer.ItemChanged;
begin
  if Assigned(FComboLayer) then
  begin
    FComboLayer.TileLayer.MapProvider := FMapProvider;
    FComboLayer.TileLayer.UseThreads := FUseThreads;
    FComboLayer.TileLayer.DrawMode := FDrawMode;
    FComboLayer.TileLayer.Opacity := FOpacity;
    FComboLayer.Visible := FVisible;
  end;
  Changed(False);
end;

constructor TMapLayer.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FUseThreads := True;
  FDrawMode := idmUseOpacity;
  FOpacity := 0.25;
  FVisible := True;
  FTag := 0;

  FPointsOfInterest := TPointsOfInterest.Create(Self, BASE_Z_POI);
  FAreas := TMapAreas.Create(Self, BASE_Z_AREA);
  FTracks := TMapTracks.Create(Self, BASE_Z_TRACK);
  FComboLayer := TGPSComboLayer.Create;
  View.GPSItems.Add(FComboLayer, _TILELAYERS_ID_, Self.Index + BASE_Z_LAYER);
end;

destructor TMapLayer.Destroy;
begin
  FPointsOfInterest.Free;
  FAreas.Free;
  FTracks.Free;
  if Assigned(FComboLayer) then
    View.GPSItems.Delete(FComboLayer);
  inherited Destroy;
end;

function TMapLayer.HitTest(constref Area: TRealArea): TMapObjectList;
begin
  Result := Nil;
  if not Visible then
    Exit;
  Result := PointsOfInterest.HitTest(Area);
  Result := TMapObjectList.AddListToResult(Tracks.HitTest(Area), Result);
  Result := TMapObjectList.AddListToResult(Areas.HitTest(Area), Result);
end;

function TMapLayer.AddPointOfInterest(APoint: TRealPoint; ACaption: String = ''): TPointOfInterest;
begin
  Result := PointsOfInterest.Add as TPointOfInterest;
  Result.RealPoint := APoint;
  Result.Caption := ACaption;
end;

procedure TMapLayer.AssignFromGPSList(AList: TGPSObjectList);

  procedure AddPoint(APoint: TGPSPoint);
  begin
    with PointsOfInterest.Add as TMapPoint do
    begin
      Caption := APoint.Name;
      Longitude := APoint.Lon;
      Latitude := APoint.Lat;
      Elevation := APoint.Elevation;
      DateTime := APoint.DateTime;
    end;
  end;

  procedure AddTrack(ATrack: TGPSTrack);
  var
    I: Integer;
    P: TGPSPoint;
  begin
    with Tracks.Add as TMapTrack do
    begin
      Caption := ATrack.Name;
      for I := 0 to Pred(ATrack.Points.Count) do
        with Points.Add as TMapTrackPoint do
        begin
          P := ATrack.Points[I];
          Caption := P.Name;
          Longitude := P.Lon;
          Latitude := P.Lat;
          Elevation := P.Elevation;
          DateTime := P.DateTime;
          if ATrack.Points[I].ExtraData is TSegmentExtraData then
            Mark := TSegmentExtraData(ATrack.Points[I].ExtraData).Mark;
        end;
      if ATrack.ExtraData is TTrackExtraData then
      begin
        LineWidth := TTrackExtraData(ATrack.ExtraData).Width;
        LineColor := TTrackExtraData(ATrack.ExtraData).Color;
      end;
    end;
  end;

var
  I: Integer;
begin
  if not Assigned(AList) then
    Exit;
  PointsOfInterest.Clear;
  Tracks.Clear;
  for I := 0 to Pred(AList.Count) do
  if AList[I] is TGPSPoint then
    AddPoint(TGPSPoint(AList[I]))
  else if AList[I] is TGPSTrack then
    AddTrack(TGPSTrack(AList[I]))
  else
    {TODO};
end;

{ TMapLayers }

function TMapLayers.GetView: TMapView;
begin
  Result := MCOwner;
end;

function TMapLayers.GetLayer: TMapLayer;
begin
  Result := Nil;
end;

procedure TMapLayers.FixOrder(APrevIndex, AIndex: Integer);
var
  I, T, B: Integer;
begin
  T := Min(APrevIndex, AIndex);
  B := Max(APrevIndex, AIndex);
  if APrevIndex < 0 then
  begin
    T := AIndex;
    B := Pred(Count);
  end;
  for I := T to B do
    View.GPSItems.ChangeZOrder(TMapItem(Items[I]).GPSObj, I + FBaseZ);
end;

{ TDrawObjJob }

function TDrawObjJob.pGetTask: integer;
var
  i: integer;
begin
  if not(AllRun) and not(Cancelled) then
  begin
    for i := Low(FStates) to High(FStates) do
      if FStates[i]=0 then
      begin
        result := i+1;
        Exit;
      end;
    AllRun:=True;
  end;

  Result := ALL_TASK_COMPLETED;
  for i := Low(FStates) to High(FStates) do
    if FStates[i]=1 then
    begin
      Result := NO_MORE_TASK;
      Exit;
    end;
end;

procedure TDrawObjJob.pTaskStarted(aTask: integer);
begin
  FRunning := True;
  FStates[aTask-1] := 1;
end;

procedure TDrawObjJob.pTaskEnded(aTask: integer; aExcept: Exception);
begin
  if Assigned(aExcept) then
    FStates[aTask-1] := 3
  else
    FStates[aTask-1] := 2;
end;

procedure TDrawObjJob.ExecuteTask(aTask: integer; FromWaiting: boolean);
var
  iObj: integer;
  Obj: TGpsObj;
begin
  iObj := aTask-1;
  Obj := FLst[iObj];
  Viewer.DrawGpsObj(FArea, Obj);
end;

function TDrawObjJob.Running: boolean;
begin
  Result := FRunning;
end;

constructor TDrawObjJob.Create(aViewer: TMapView; aLst: TGPSObjList;
  const aArea: TRealArea);
begin
  FArea := aArea;
  FLst := aLst;
  SetLength(FStates, FLst.Count);
  Viewer := aViewer;
  AllRun := false;
  Name := 'DrawObj';
end;

destructor TDrawObjJob.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FLst);
end;


{ TMapView }

procedure TMapView.SetActive(AValue: boolean);
begin
  if FActive = AValue then Exit;
  if AValue and (MapProvider = '') then
    // Raising an exception won't let the component to be loaded
    if not (csLoading in ComponentState) then
      raise Exception.Create('MapProvider is not selected.');
  FActive := AValue;
  if FActive then
    ActivateEngine
  else
    Engine.Active := false;
end;

procedure TMapView.SetCacheLocation(AValue: TCacheLocation);
var
  NewPath: String;
  OldLoc: TCacheLocation;
begin
  if FCacheLocation = AValue then
    Exit;
  OldLoc := FCacheLocation;
  FCacheLocation := AValue;
  NewPath := CacheDirectory(AValue, CachePath);
  if NewPath = Engine.CachePath then
    Exit;
  ChangeCachePath(OldLoc, NewPath);
end;

procedure TMapView.SetCacheMaxAge(AValue: Integer);
begin
  if Engine.CacheMaxAge = AValue then
    Exit;
  Engine.CacheMaxAge := AValue;
  UpdateLayers;
end;

function TMapView.GetCacheOnDisk: boolean;
begin
  Result := FCacheOnDisk; 
end;

function TMapView.GetCenter: TRealPoint;
begin
  Result := Engine.Center;
end;

function TMapView.GetCyclic: Boolean;
begin
  Result := Engine.Cyclic;
end;

function TMapView.GetDownloadEngine: TMvCustomDownloadEngine;
begin
  if FDownloadEngine = nil then
    Result := FBuiltinDownloadEngine
  else
    Result := FDownloadEngine;
end;

function TMapView.UsesDefaultDownloadEngine: Boolean;
begin
  Result := (FDownloadEngine = nil) or (FDownloadEngine = FBuiltinDownloadEngine);
end;

function TMapView.GetDrawingEngine: TMvCustomDrawingEngine;
begin
  if FDrawingEngine = nil then
    Result := FBuiltinDrawingEngine
  else
    Result := FDrawingEngine;
end;

function TMapView.UsesDefaultDrawingEngine: Boolean;
begin
  Result := (FDrawingEngine = nil) or (FDrawingEngine = FBuiltinDrawingEngine);
end;

function TMapView.GetDrawPreviewTiles: Boolean;
begin
  Result := Engine.DrawPreviewTiles;
end;

function TMapView.GetGPSItems: TGPSObjectList;
begin
  Result := GetGPSLayer(5);
end;

function TMapView.GetGPSLayer(Layer: Integer): TGPSObjectList;
begin
  Result := FGPSItems[Layer mod 10];
end;

function TMapView.GetInactiveColor: TColor;
begin
  Result := FPColorToTColor(Engine.BkColor);
end;

function TMapView.GetLayers: TMapLayers;
begin
  Result := FLayers;
end;

function TMapView.GetMapProvider: String;
begin
  result := Engine.MapProvider;
end;

{
function TMapView.GetOnCenterMove: TNotifyEvent;
begin
  Result := Engine.OnCenterMove;
end;
}

function TMapView.GetOnCenterMoving: TCenterMovingEvent;
begin
  Result := Engine.OnCenterMoving;
end;

procedure TMapView.DoZoomChange(Sender: TObject);
begin
  PluginManager.ZoomChange(Self, FOnZoomChange);
end;

function TMapView.GetOnChange: TNotifyEvent;
begin
  Result := Engine.OnChange;
end;

{
function TMapView.GetOnZoomChange: TNotifyEvent;
begin
  Result := Engine.OnZoomChange;
end;
}

function TMapView.GetOnZoomChanging: TZoomChangingEvent;
begin
  Result := Engine.OnZoomChanging;
end;

function TMapView.GetUseThreads: boolean;
begin
  Result := Engine.UseThreads;
end;

function TMapView.GetZoom: integer;
begin
  result := Engine.Zoom;
end;

function TMapView.GetZoomToCursor: Boolean;
begin
  Result := Engine.ZoomToCursor;
end;

function TMapView.IsCacheMaxAgeStored: Boolean;
begin
  Result := Engine.CacheMaxAge <> MaxInt;
end;

function TMapView.IsCachePathStored: Boolean;
begin
  Result := not SameText(CachePath, 'cache/');
end;

function TMapView.IsFontStored: Boolean;
begin
  Result := SameText(FFont.Name, 'default') and (FFont.Size = 0) and
    (FFont.Style = []) and (FFont.Color = clBlack);
end;

function TMapView.IsLayersStored: Boolean;
begin
  Result := True; 
end;

procedure TMapView.SetCacheOnDisk(AValue: boolean);
begin
  FCacheOnDisk := AValue;
  if csDesigning in ComponentState
    then Engine.CacheOnDisk := False
    else Engine.CacheOnDisk := AValue;
  UpdateLayers;
end;

procedure TMapView.SetCachePath(AValue: String);
var
  NewPath: String;
begin
  if FCachePath = AValue then
    Exit;
  FCachePath := AValue;
  NewPath := CacheDirectory(CacheLocation, AValue);
  if NewPath = Engine.CachePath then
    Exit;
  ChangeCachePath(CacheLocation, NewPath);
end;

procedure TMapView.SetCenter(AValue: TRealPoint);
begin
  Engine.Center := AValue;
  Invalidate;
end;

procedure TMapView.SetCyclic(AValue: Boolean);
begin
  Engine.Cyclic := AValue;
  UpdateLayers;
  Invalidate;
end;

procedure TMapView.SetDebugTiles(AValue: Boolean);
begin
  if FDebugTiles = AValue then exit;
  FDebugTiles := AValue;
  Invalidate;
end;

procedure TMapView.SetDefaultTrackColor(AValue: TColor);
begin
  if FDefaultTrackColor = AValue then exit;
  FDefaultTrackColor := AValue;
  Invalidate;
end;

procedure TMapView.SetDefaultTrackWidth(AValue: Integer);
begin
  if FDefaultTrackWidth = AValue then exit;
  FDefaultTrackWidth := AValue;
  Invalidate;
end;

procedure TMapView.SetDownloadEngine(AValue: TMvCustomDownloadEngine);
begin
  FDownloadEngine := AValue;
  FEngine.DownloadEngine := GetDownloadEngine;
  UpdateLayers;
end;

procedure TMapView.SetDrawingEngine(AValue: TMvCustomDrawingEngine);
begin
  FDrawingEngine := AValue;
  if AValue = nil then
  begin
    FBuiltinDrawingEngine.CreateBuffer(ClientWidth, ClientHeight);
    FEngine.CacheItemClass := FBuiltinDrawingEngine.GetCacheItemClass;
  end
  else begin
    FBuiltinDrawingEngine.CreateBuffer(0, 0);
    FDrawingEngine.CreateBuffer(ClientWidth, ClientHeight);
    FEngine.CacheItemClass := FDrawingEngine.GetCacheItemClass;
  end;
  UpdateFont(nil);
end;

procedure TMapView.SetDrawPreviewTiles(AValue: Boolean);
begin
  Engine.DrawPreviewTiles := AValue;
end;

procedure TMapView.SetFont(AValue: TFont);
begin
  FFont.Assign(AValue);
  UpdateFont(nil);
end;

procedure TMapView.SetInactiveColor(AValue: TColor);
begin
  Engine.BkColor := TColorToFPColor(AValue);
  if not IsActive then
    Invalidate;
end;

procedure TMapView.SetLayers(const ALayers: TMapLayers);
begin
  FLayers.Assign(ALayers);
end;

procedure TMapView.ActivateEngine;
begin
  Engine.SetSize(ClientWidth,ClientHeight);
  Engine.Active := IsActive;
end;

procedure TMapView.SetMapProvider(AValue: String);
begin
  //if AValue = '' then
  //  raise EArgumentException.Create('Empty map provider is not allowed.');
  Engine.MapProvider := AValue;
  if AValue = '' then
    Active := False;
  Invalidate;
end;
                                  {
procedure TMapView.SetOnCenterMove(AValue: TNotifyEvent);
begin
  Engine.OnCenterMove := AValue;
end;                               }

procedure TMapView.SetOnCenterMoving(AValue: TCenterMovingEvent);
begin
  Engine.OnCenterMoving := AValue;
end;

procedure TMapView.SetOnChange(AValue: TNotifyEvent);
begin
  Engine.OnChange := AValue;
end;

{
procedure TMapView.SetOnZoomChange(AValue: TNotifyEvent);
begin
  FOnZoomChange := AValue;
  Engine.OnZoomChange := @DoZoomChangeHandler;
end;
 }
procedure TMapView.SetOnZoomChanging(AValue: TZoomChangingEvent);
begin
  Engine.OnZoomChanging := AValue;
end;

procedure TMapView.SetOptions(AValue: TMapViewOptions);
begin
  if FOptions = AValue then Exit;
  FOptions := AValue;
  if Engine.InDrag and not (mvoMouseDragging in FOptions) then
  begin
    Engine.DragObj.AbortDrag;
    Invalidate;
  end;
end;

procedure TMapView.SetPluginManager(AValue: TMvCustomPluginManager);
begin
  if FPluginManager = AValue then
    exit;
  if FPluginManager <> nil then
    RemoveFreeNotification(FPluginManager);
  if AValue = nil then
    FPlugInManager.RemoveMapView(Self);

  FPluginManager := AValue;

  //FActiveToolIndex := -1;
  if FPluginManager <> nil then
  begin
    FreeNotification(FPluginManager);
    FPluginManager.AddMapView(Self);
  end;
end;

procedure TMapView.SetPOIImage(const AValue: TCustomBitmap);
var
  s: TStream;
begin
  if FPOIImage = AValue then exit;
  if AValue <> nil then
  begin
    s := TMemoryStream.Create;
    try
      AValue.SaveToStream(s);
      s.Position := 0;
      FPOIImage.Free;
      FPOIImage := TCustomBitmapClass(AValue.ClassType).Create;
      FPOIImage.OnChange := @UpdateImage;
      FPOIImage.LoadFromStream(s);
      // Is a stream the only way to retain the alpha channel?
    finally
      s.Free;
    end;
  end else
    FPOIImage.Clear;
  Invalidate;
end;

procedure TMapView.SetPOIImages(const AValue: TCustomImageList);
begin
  if FPOIImages = AValue then exit;
  FPOIImages := AValue;
  Invalidate;
end;

procedure TMapView.SetPOIImagesWidth(AValue: Integer);
begin
  if FPOIImagesWidth = AValue then exit;
  FPOIImagesWidth := AValue;
  Invalidate;
end;

procedure TMapView.SetPOITextBgColor(AValue: TColor);
begin
  if FPOITextBgColor = AValue then exit;
  FPOITextBgColor := AValue;
  Invalidate;
end;

procedure TMapView.SetUseThreads(AValue: boolean);
begin
  Engine.UseThreads := aValue;
end;

procedure TMapView.SetZoom(AValue: integer);
begin
  Engine.Zoom := EnsureRange(AValue, FZoomMin, FZoomMax);
  Invalidate;
end;

procedure TMapView.SetZoomMax(AValue: Integer);
begin
  if FZoomMax = AValue then Exit;
  FZoomMax := EnsureRange(AValue, FZoomMin, 19);
  Engine.ZoomMax := FZoomMax;
  Zoom := GetZoom;
end;

procedure TMapView.SetZoomMin(AValue: Integer);
begin
  if FZoomMin = AValue then Exit;
  FZoomMin := EnsureRange(AValue, 1, FZoomMax);
  Engine.ZoomMin := FZoomMin;
  Zoom := GetZoom;
end;

procedure TMapView.SetZoomToCursor(AValue: Boolean);
begin
  Engine.ZoomToCursor := AValue;
end;

function TMapView.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  Result:=inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if IsActive and (mvoMouseZooming in FOptions) then
  begin
    Engine.MouseWheel(self,Shift,WheelDelta,MousePos,Result);
    Invalidate;
  end;
end;

procedure TMapView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  savedOnMouseDown: TMouseEvent;
  lHandled : Boolean;
begin
  savedOnMouseDown := OnMouseDown;
  try
    OnMouseDown := nil;  // Avoid handling user OnMouseDown before the plugin manager
    inherited MouseDown(Button, Shift, X, Y);
  finally
    OnMouseDown := savedOnMouseDown;
  end;
  PluginManager.MouseDown(Self, Button, Shift, X, Y, OnMouseDown, lHandled);

  if EditingEnabled then
  begin
    if (Button = mbLeft) and FEditMark.ClickableAt(X, Y) then
    begin
      FEditMark.ClickAt(X, Y);
      if not lHandled then
        FDragger.MouseDown(FEditMark, X, Y);
    end
    // With editor enabled, dragging is with the middle button
    else if (Button = mbMiddle) and DraggingEnabled and
            (not lHandled) then
      StartDragging(X, Y);
  end
  else
    // With editor disabled, dragging is with the left button
    if IsActive and DraggingEnabled and
       (Button = mbLeft) and (not lHandled) then
      begin
        Engine.MouseDown(self,Button,Shift,X,Y);
        Invalidate;
      end;
end;

procedure TMapView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  savedOnMouseUp: TMouseEvent;
  lHandled : Boolean;
begin
  savedOnMouseUp := OnMouseUp;
  try
    OnMouseUp := nil;   // Avoid handling user OnMouseUp before the plugin manager
    inherited MouseUp(Button, Shift, X, Y);
  finally
    OnMouseUp := savedOnMouseUp;
  end;
  PluginManager.MouseUp(Self, Button, Shift, X, Y, OnMouseUp, lHandled);

  if IsActive then
    if Button = mbLeft then
    begin
      Engine.MouseUp(self,Button,Shift,X,Y);
      Engine.Redraw;
      Invalidate;
    end;

  if EditingEnabled then
  begin
    FDragger.MouseUp(X, Y);
    FEditMark.CompleteSelection;
    if (Button = mbMiddle)
      then EndDragging(X, Y)
      else AbortDragging;
  end;
end;

procedure TMapView.MouseMove(Shift: TShiftState; X, Y: Integer);

  procedure EditorMM;
  const
    DELTA = 5;
  var
    A: TRealArea;
    Hits: TMapObjectList;
  begin
    A.TopLeft := ScreenToLatLon(Point(X - DELTA, Y - DELTA));
    A.BottomRight := ScreenToLatLon(Point(X + DELTA, Y + DELTA));
    FDragger.MouseMove(X, Y);

    // Update hits
    if not FDragger.InDrag then
    begin
      Hits := Layers.HitTest(A);
      try
        FEditMark.UpdateFrom(Hits);
        Screen.Cursor := FEditMark.CursorShape;
      finally
        Hits.Free;
      end;
    end;
  end;

var
  savedOnMouseMove: TMouseMoveEvent;
  lHandled : Boolean;
begin
  savedOnMouseMove := OnMouseMove;
  try
    OnMouseMove := nil;  // Avoid handling user OnMouseMove before plugin manager
    inherited MouseMove(Shift, X, Y);
  finally
    OnMouseMove := savedOnMouseMove;
  end;
  PluginManager.MouseMove(Self, Shift, X, Y, OnMouseMove, lHandled);
  if IsActive then
  begin
    Engine.MouseMove(self,Shift,X,Y);
    if lHandled then
      AbortDragging;
    if Engine.InDrag then
      Invalidate;
  end;
  if EditingEnabled then
    EditorMM;
end;

procedure TMapView.MouseEnter;
var
  lHandled : Boolean;
begin
  PluginManager.MouseEnter(Self, OnMouseEnter,lHandled);
end;

procedure TMapView.MouseLeave;
var
  lHandled : Boolean;
begin
  PluginManager.MouseLeave(Self, OnMouseLeave,lHandled);
end;

procedure TMapView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) then
  begin
    if (AComponent = FPOIImages) then
      FPOIImages := nil;
    if (AComponent = FDownloadEngine) then
      DownloadEngine := nil;
    if (AComponent = FDrawingEngine) then
      DrawingEngine := nil;
    if (AComponent = FPluginManager) then
      PluginManager := nil;
  end;
end;

procedure TMapView.DblClick;
begin
  inherited DblClick;
  if IsActive then
  begin
    Engine.DblClick(self);
    Invalidate;
  end;
end;

procedure TMapView.DoOnResize;
begin
  inherited DoOnResize;
  //cancel all rendering threads
  Engine.CancelCurrentDrawing;
  DrawingEngine.CreateBuffer(ClientWidth, ClientHeight);
  if IsActive then
  begin
    Engine.SetSize(ClientWidth, ClientHeight);
    Invalidate;
  end;
  if (FCanvasSize.CX = 0) and (FCanvasSize.CY = 0) then
    FCanvasSize := Size(ClientWidth + 100, ClientHeight);   // will be updated correctly in Paint
end;

procedure TMapView.Paint;
var
  lHandled : Boolean;

const
  FREE_DRAG = 0; //(TILE_SIZE * TILE_SIZE) div 4;

  procedure DrawCenter;
  var
    C: TPoint;
  begin
    C := Point(ClientWidth div 2, ClientHeight div 2);
    Canvas.Pen.Color := clBlack;
    Canvas.Pen.Width := 1;
    Canvas.Line(C.X, C.Y - 15, C.X, C.Y + 15);
    Canvas.Line(C.X - 15, C.Y, C.X + 15, C.Y);
  end;

  procedure InactiveDraw;
  begin
    Canvas.Brush.Color := InactiveColor;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(0, 0, ClientWidth, ClientHeight);
  end;

  procedure FullRedraw;
  var
    W: Integer;
  begin
    Engine.Redraw;
    W := FCanvasSize.CX;
    if Cyclic then
      W := Min(1 shl Zoom * TileSize.CX, W);

    PluginManager.BeforeDrawObjects(Self, FBeforeDrawObjectsEvent,lHandled);
    DrawObjects(Default(TTileId), 0, 0, W - 1, FCanvasSize.CY);
    PluginManager.AfterDrawObjects(Self, FAfterDrawObjectsEvent,lHandled);

    DrawingEngine.PaintToCanvas(Canvas);
    if DebugTiles then
      DrawCenter;

    PluginManager.AfterPaint(Self, FAfterPaintEvent,lHandled);
  end;

  procedure DragDraw;
  var
    O: TPoint;
  begin
    O := Point(Engine.DragObj.OfsX, Engine.DragObj.OfsY);
    // Free drag up to half of the tile
    if ((O.X * O.X + O.Y * O.Y) < FREE_DRAG) then
    begin
      DrawingEngine.PaintToCanvas(Canvas);
      DrawingEngine.PaintToCanvas(Canvas, O);
      if DebugTiles then
        DrawCenter;
    end
    else
      FullRedraw;
  end;

begin
  inherited Paint;
  FCanvasSize := Size(Canvas.Width, Canvas.Height);
  if IsActive then
  begin
    if Engine.InDrag then
      DragDraw
    else
      FullRedraw;
  end else
    InactiveDraw;
end;

procedure TMapView.OnGPSItemsModified(Sender: TObject; objs: TGPSObjList;
  Adding: boolean);
var
  {%H-}Area, objArea, visArea: TRealArea;
begin
  if Adding and Assigned(Objs) then
  begin
    objArea := GetAreaOf(Objs);
    visArea := GetVisibleArea;
    if hasIntersectArea(objArea, visArea) then
    begin
      Area := IntersectArea(objArea, visArea);
      Invalidate;
    end
  end
  else
    Engine.Redraw;
end;

procedure TMapView.StartDragging(X, Y: Integer);
begin
  Engine.DragObj.MouseDown(Engine, X, Y);
end;

procedure TMapView.EndDragging(X, Y: Integer);
var
  Drag: Boolean;
begin
  Drag := Engine.InDrag;
  Engine.DragObj.MouseUp(X, Y);
  if Drag then
    Invalidate;
end;

procedure TMapView.AbortDragging;
begin
  Engine.DragObj.AbortDrag;
end;

function TMapView.TrackLineColor(AColor: TColor; ExtraData: TObject): TColor;
begin
  if AColor = clDefault then
  begin
    Result := ColorToRGB(FDefaultTrackColor);
    if (ExtraData <> Nil) and ExtraData.InheritsFrom(TDrawingExtraData) then
      Result := TDrawingExtraData(ExtraData).Color;
  end
  else
    Result := ColorToRGB(AColor);
end;

function TMapView.TrackLineWidth(AWidth: Double; ExtraData: TObject): Integer;
begin
  if AWidth = -1 then
  begin
    Result := FDefaultTrackWidth;
    if (ExtraData <> Nil) and ExtraData.InheritsFrom(TTrackExtraData) then
      Result := mmToPx(TTrackExtraData(ExtraData).Width);
  end
  else
    Result := mmToPx(AWidth);
  if Result < 1 then
    Result := 1;
end;

procedure TMapView.DrawTrack(const Area: TRealArea; trk: TGPSTrack);
var
  I, L, T, WSx, WSy: Integer;
  ClipRect: TRect;
  iPt1, iPt2, iPt3, iPt4: TPoint;
  ToEast, EndSegm, ConnSegm: Boolean;
  pt1, pt2: TRealPoint;
  trkColor, connColor: TColor;
  trkWidth, connWidth: Integer;
  OldOpacity: Single;
  OldPenStyle: TPenStyle;

  procedure ClipDrawLine(P1, P2: TPoint); inline;
  begin
    if not ClipLineToRect(ClipRect, P1, P2) then
      DrawingEngine.Line(P1.X, P1.Y, P2.X, P2.Y);
  end;

begin
  if not trk.Visible or (trk.Points.Count = 0) then
    exit;

  // Determine track color
  trkColor := TrackLineColor(trk.LineColor, trk.ExtraData);

  // Determine track width
  trkWidth := TrackLineWidth(trk.LineWidth, trk.ExtraData);

  ConnSegm := trk.ConnectColor <> clNone;
  if ConnSegm then
  begin
    if trk.ConnectColor = clDefault
      then connColor := trkColor
      else connColor := TrackLineColor(trk.ConnectColor, trk.ExtraData);
    if trk.ConnectWidth < 0.01
      then connWidth := trkWidth
      else connWidth := TrackLineWidth(trk.ConnectWidth, trk.ExtraData);
  end;

  OldOpacity := DrawingEngine.Opacity;
  OldPenStyle := DrawingEngine.PenStyle;
  try
    DrawingEngine.Opacity := trk.Opacity;

    DrawingEngine.PenColor := trkColor;
    DrawingEngine.PenWidth := trkWidth;
    DrawingEngine.PenStyle := psSolid;

    // Clipping rectangle
    if Cyclic then
      ClipRect := Rect(0, 0, ClientWidth, ClientHeight)
    else
    begin
      L := Max(0, Engine.MapLeft);
      T := Max(0, Engine.MapTop);
      WSx := mvGeoMath.ZoomFactor(Zoom) * TileSize.CX;
      WSy := mvGeoMath.ZoomFactor(Zoom) * TileSize.CY;
      ClipRect := Rect(L, T, Min(Engine.MapLeft + WSx, ClientWidth),
        Min(Engine.MapTop + WSy, ClientHeight));
    end;

    pt1 := trk.Points[0].RealPoint;
    iPt1 := Engine.LatLonToScreen(pt1);
    EndSegm := TSegmentExtraData.MarkOf(trk.Points[0].ExtraData) = smEnd;
    for I := 1 to Pred(trk.Points.Count) do
    begin
      pt2 := trk.Points[I].RealPoint;
      iPt2 := Engine.LatLonToScreen(pt2);
      ToEast := GoingEast(pt1.Lon, pt2.Lon); // Eastwards?
      iPt2 := CyclicPointOf(iPt2, iPt1.X, ToEast); // Nearest iPt2 to iPt1

      // Rightmost cyclic copy of the segment
      if ToEast then
      begin
        iPt3 := CyclicPointOf(iPt1, ClipRect.Right); // Left point
        iPt4 := (iPt2 - iPt1); // delta to the right point
      end
      else
      begin
        iPt3 := CyclicPointOf(iPt2, ClipRect.Right); // Left point
        iPt4 := (iPt1 - iPt2); // delta to the right point
      end;

      if EndSegm and ConnSegm then
      begin
        DrawingEngine.PenColor := connColor;
        DrawingEngine.PenWidth := connWidth;
      end;

      if not EndSegm or ConnSegm then
        // Draw all copies of the segment, right to left
        repeat
          ClipDrawLine(iPt3, iPt3 + iPt4);
          iPt3 := CyclicPointOf(iPt3, Pred(iPt3.X), False); // Next left cyclic iPt3
        until Max(iPt3.X, iPt3.X + iPt4.X) < ClipRect.Left;

      if EndSegm and ConnSegm then
      begin
        DrawingEngine.PenColor := trkColor;
        DrawingEngine.PenWidth := trkWidth;
      end;

      pt1 := pt2;
      iPt1 := iPt2;
      EndSegm := TSegmentExtraData.MarkOf(trk.Points[I].ExtraData) = smEnd;
    end;
  finally
    DrawingEngine.PenStyle := OldPenStyle;
    DrawingEngine.Opacity := OldOpacity;
  end;
end;

procedure TMapView.DrawArea(const Area: TRealArea; ar: TGPSArea);
var
  Pts: array of TPoint = nil;
  I, C: Integer;
  NoFill: Boolean;
  WS: Int64;
begin
  if not ar.Visible or (ar.Points.Count = 0)  then
    Exit;

  if Cyclic then
  begin
    WS := mvGeoMath.ZoomFactor(Zoom) * TileSize.CX;
    if (WS < ClientWidth) then
    begin
      {TODO Draw multiple copies of the area}
      Exit; // Not implemented, exit
    end;
  end;
  {TODO Fix drawing when the area crosses the date line, see DrawTrack}

  C := ar.Points.Count;
  NoFill := (ar.FillColor = clNone);
  if NoFill then
    Inc(C);

  SetLength(Pts, C);
  for I := 0 to Pred(ar.Points.Count) do
    Pts[I] := Engine.LatLonToScreen(ar.Points[I].RealPoint);
  if NoFill then
    Pts[Pred(C)] := Pts[0];

  DrawingEngine.Opacity := ar.Opacity;

  if ar.LineColor = clNone then
    DrawingEngine.PenStyle := psClear
  else
  begin
    DrawingEngine.PenStyle := psSolid;
    DrawingEngine.PenColor := TrackLineColor(ar.LineColor, ar.ExtraData);
    DrawingEngine.PenWidth := TrackLineWidth(ar.LineWidth, ar.ExtraData);
  end;
  if NoFill then
    DrawingEngine.Polyline(Pts)
  else
  begin
    DrawingEngine.BrushStyle := bsSolid;
    DrawingEngine.BrushColor := ar.FillColor;
    DrawingEngine.Polygon(Pts);
  end;
end;

procedure TMapView.DrawPointOfInterest(const Area: TRealArea; APt: TGPSPointOfInterest);
var
  pt: TPoint;
  ptCyc: TPointArray;
  ptColor: TColor;
  extent: TSize;
  s: String;
  bmp: TBitmap;
  w, h: Integer;
  OldOpacity: Single;
  OldPenStyle: TPenStyle;

  procedure DrawOne(pt: TPoint);
  begin
    if Assigned(bmp) then
      DrawingEngine.DrawBitmap(pt.X - w div 2, pt.Y - h, bmp, true)
    else
    begin
      // ... or as cross
      ptColor := clRed;
      if (APt.ExtraData <> nil) and APt.ExtraData.InheritsFrom(TDrawingExtraData) then
        ptColor := TDrawingExtraData(APt.ExtraData).Color;
      DrawingEngine.PenColor := ptColor;
      DrawingEngine.PenWidth := 3;
      DrawingEngine.Line(pt.X, pt.Y - 5, pt.X, pt.Y + 5);
      DrawingEngine.Line(pt.X - 5, pt.Y, pt.X + 5, pt.Y);
      pt.Y := pt.Y + 5;
    end;
    if FPOITextBgColor = clNone then
      DrawingEngine.BrushStyle := bsClear
    else
    begin
      DrawingEngine.BrushStyle := bsSolid;
      DrawingEngine.BrushColor := FPOITextBgColor;
    end;
    DrawingEngine.TextOut(pt.X - extent.CX div 2, pt.Y + 5, s);
  end;

begin
  pt := Engine.LatLonToScreen(APt.RealPoint);

  OldOpacity := DrawingEngine.Opacity;
  OldPenStyle := DrawingEngine.PenStyle;
  bmp := Nil;
  try
    DrawingEngine.Opacity := 1.0;
    DrawingEngine.PenStyle := psSolid;

    // Draw point as symbol from image list ...
    if Assigned(FPOIImages) and (APt.ImageIndex <> -1) and (APt.ImageIndex < FPOIImages.Count) then
    begin
      bmp := TBitmap.Create;
      FPOIImages.GetBitmap(APt.ImageIndex, bmp);
      {$IF LCL_FullVersion >= 2000000}
      w := FPOIImages.WidthForPPI[FPOIImagesWidth, Font.PixelsPerInch];
      h := FPOIImages.HeightForPPI[FPOIImagesWidth, Font.PixelsPerInch];
      {$ELSE}
      w := FPOIImages.Width;
      h := FPOIImages.Height;
      {$IFEND}
    end;

    // Draw point text
    s := APt.Name;
    if FPOITextBgColor <> clNone then
      s := ' ' + s + ' ';
    extent := DrawingEngine.TextExtent(s);
    if Cyclic then
    begin
      ptCyc := CyclicPointsOf(pt);
      for pt in ptCyc do
        DrawOne(pt);
    end
    else
      DrawOne(pt);
  finally
    bmp.Free;
    DrawingEngine.Opacity := OldOpacity;
    DrawingEngine.PenStyle := OldPenStyle;
  end;
end;

procedure TMapView.DrawPt(const Area: TRealArea; APt: TGPSPoint);
var
  Pt: TPoint;
  PtCyc: TPointArray;
  PtColor: TColor;
  extent: TSize;
  s: String;

  procedure DrawOne(Pt: TPoint);
  begin
    // Draw point marker
    if Assigned(FPOIImage) and not (FPOIImage.Empty) then
      DrawingEngine.DrawBitmap(Pt.X - FPOIImage.Width div 2, Pt.Y - FPOIImage.Height, FPOIImage, true)
    else begin
      DrawingEngine.PenColor := ptColor;
      DrawingEngine.PenWidth := 3;
      DrawingEngine.Line(Pt.X, Pt.Y - 5, Pt.X, Pt.Y + 5);
      DrawingEngine.Line(Pt.X - 5, Pt.Y, Pt.X + 5, Pt.Y);
      Pt.Y := Pt.Y + 5;
    end;

    // Draw point text
    s := APt.Name;
    if FPOITextBgColor = clNone then
      DrawingEngine.BrushStyle := bsClear
    else begin
      DrawingEngine.BrushStyle := bsSolid;
      DrawingEngine.BrushColor := FPOITextBgColor;
      s := ' ' + s + ' ';
    end;
    extent := DrawingEngine.TextExtent(s);
    DrawingEngine.Textout(Pt.X - extent.CX div 2, Pt.Y + 5, s);
  end;

begin
  if Assigned(FOnDrawGpsPoint) then begin
    FOnDrawGpsPoint(Self, DrawingEngine, APt);
    exit;
  end;

  Pt := Engine.LatLonToScreen(APt.RealPoint);
  PtColor := clRed;
  if APt.ExtraData <> nil then
  begin
    if APt.ExtraData.inheritsFrom(TDrawingExtraData) then
      PtColor := TDrawingExtraData(APt.ExtraData).Color;
  end;

  PtCyc := CyclicPointsOf(Pt);
  for Pt in PtCyc do
    DrawOne(Pt);
end;

procedure TMapView.DrawGpsObj(const Area: TRealArea; AObj: TGPSObj);
begin
  GPSItems.Lock;
  try
    AObj.Draw(Self, Area);
  finally
    GPSItems.Unlock;
  end;
end;

function TMapView.GetCacheMaxAge: Integer;
begin
  Result := Engine.CacheMaxAge;
end;

procedure TMapView.CallAsyncInvalidate;
Begin
  if not(AsyncInvalidate) then
  begin
    AsyncInvalidate := true;
    Engine.Jobqueue.QueueAsyncCall(@DoAsyncInvalidate, 0);
  end;
end;

procedure TMapView.DrawObjects(const TileId: TTileId;
  aLeft, aTop,aRight,aBottom: integer);
var
  Area: TRealArea;
  lst: TGPSObjList;
  I, J: Integer;
begin
  Area.TopLeft := Engine.ScreenToLatLon(Point(aLeft, aTop));
  Area.BottomRight := Engine.ScreenToLatLon(Point(aRight, aBottom));

  for J := 0 to High(FGPSItems) do
    if FGPSItems[J].Visible and (FGPSItems[J].Count > 0) then
    begin
      lst := FGPSItems[J].GetObjectsInArea(Area);
      try
        if lst.Count > 0 then
        begin
          for I := 0 to Pred(lst.Count) do
            if lst[I].Visible then
              DrawGpsObj(Area, lst[I]);
        end;
      finally
        FreeAndNil(Lst);
      end;
    end;
end;

procedure TMapView.DoAsyncInvalidate(Data: PtrInt);
Begin
  Invalidate;
  AsyncInvalidate := false;
end;

procedure TMapView.DoCenterMove(Sender: TObject);
begin
  PluginManager.CenterMove(Self, FOnCenterMove);
end;

procedure TMapView.DoDrawStretchedTile(const TileId: TTileID; X, Y: Integer;
  TileImg: TPictureCacheItem; const R: TRect);
begin
  if Assigned(TileImg) then
    DrawingEngine.DrawScaledCacheItem(Rect(X, Y, X + TileSize.CX, Y + TileSize.CY), R, TileImg)
  else
    DrawingEngine.FillPixels(X, Y, X + TileSize.CX, Y + TileSize.CY, InactiveColor);

  if FDebugTiles then
    DoDrawTileInfo(TileID, X, Y);

end;

procedure TMapView.DoDrawTile(const TileId: TTileId; X, Y: integer;
  TileImg: TPictureCacheItem);
begin
  if Assigned(TileImg) then
    DrawingEngine.DrawCacheItem(X, Y, TileImg)
  else
    DrawingEngine.FillPixels(X, Y, X + TileSize.CX, Y + TileSize.CY, InactiveColor);

  if FDebugTiles then
    DoDrawTileInfo(TileID, X, Y);

end;

procedure TMapView.DoDrawTileInfo(const TileID: TTileID; X, Y: Integer);
begin
  DrawingEngine.PenColor := clGray;
  DrawingEngine.PenWidth := 1;
  DrawingEngine.Line(X, Y, X, Y + TileSize.CY);
  DrawingEngine.Line(X, Y, X + TileSize.CX, Y);
  DrawingEngine.Line(X + TileSize.CX, Y, X + TileSize.CX, Y + TileSize.CY);
  DrawingEngine.Line(X, Y + TileSize.CY, X + TileSize.CX, Y + TileSize.CY);
end;

procedure TMapView.DoEraseBackground(const R: TRect);
begin
  DrawingEngine.FillPixels(R.Left, R.Top, R.Right, R.Bottom, InactiveColor);
end;

procedure TMapView.DoTileDownloaded(const TileId: TTileId);
begin
  // TODO: Include tile information to optimize redraw.
  CallAsyncInvalidate;
end;

function TMapView.IsActive: Boolean;
begin
  Result := FActive
end;

constructor TMapView.Create(AOwner: TComponent);
var
  I: Integer;
begin
  inherited Create(AOwner);

  Width := 150;
  Height := 150;

  FLayers := CreateLayers;

  FActive := false;
  FOptions := DefaultMapViewOptions;

  FDefaultTrackColor := clRed;
  FDefaultTrackWidth := 1;

  for I := 0 to High(FGPSItems) do
  begin
    FGPSItems[I] := TGPSObjectList.Create;
    FGPSItems[I].OnModified := @OnGPSItemsModified;
  end;

  {$IFDEF MSWindows}
  FBuiltinDownloadEngine := TMvDEWin.Create(self);
  {$ELSE}
  FBuiltinDownloadEngine := TMvDEFpc.Create(self);
  {$ENDIF}
  FBuiltinDownloadEngine.Name := 'BuiltInDLE';

  FCacheLocation := clProfile;

  FEngine := TMapViewerEngine.Create(self);
  FEngine.BkColor := colWhite;
  {FEngine.}CachePath := 'cache/';
  {FEngine.}CacheOnDisk := true;
  FEngine.OnCenterMove := @DoCenterMove;
  FEngine.OnDrawTile := @DoDrawTile;
  FEngine.OnDrawStretchedTile := @DoDrawStretchedTile;
  FEngine.OnEraseBackground := @DoEraseBackground;
  FEngine.OnTileDownloaded := @DoTileDownloaded;
  FEngine.OnZoomChange := @DoZoomChange;
  FEngine.DrawPreviewTiles := True;
  FEngine.DrawTitleInGuiThread := false;
  FEngine.DownloadEngine := FBuiltinDownloadEngine;
  FEngine.ZoomToCursor := True;

  FBuiltinDrawingEngine := TMvIntfGraphicsDrawingEngine.Create(self);
  FEngine.CacheItemClass := FBuiltinDrawingEngine.GetCacheItemClass;
  FBuiltinDrawingEngine.Name := 'BuiltInDE';
  FBuiltinDrawingEngine.CreateBuffer(Width, Height);

  FBuiltinPluginManager := TMvCustomPluginManager.Create(Self);
  FBuiltinPluginManager.Name := 'BuiltinPM';

  FFont := TFont.Create;
  FFont.Name := 'default';
  FFont.Size := 0;
  FFont.Style := [];
  FFont.Color := clBlack;
  FFont.OnChange := @UpdateFont;

  FPOIImage := TPortableNetworkGraphic.Create; //TBitmap.Create;
  FPOIImage.OnChange := @UpdateImage;
  FPOITextBgColor := clNone;

  FCenter := TMapCenter.Create(Self);
  FCenter.Longitude := 0.0;
  FCenter.Latitude := 0.0;

  FZoomMin := 1;
  FZoomMax := 19;
  Zoom := 1;

  CreateEditor;

end;

destructor TMapView.Destroy;
var
  I: Integer;
begin
  Active := False;
  DoneEditor;
  Engine.Jobqueue.RemoveAsyncCalls(Self);
  FFont.Free;
  FreeAndNil(FPOIImage);
  FLayers.Free;
  for I := 0 to High(FGPSItems) do
    FreeAndNil(FGPSItems[I]);
  FCenter.Free;
  inherited Destroy;
end;

{ This method, as well as EndUpdateItems, is not used anywhere by the MapViewer,
  but provides a way to safely interact with the GPS items in the
  OnBeforeDrawObjects event. }
procedure TMapView.BeginUpdateObjects;
var
  i: Integer;
begin
  SetLength(FSavedOnModifiedEvents, Length(FGPSItems));
  // Call BeginUpdate in all lists to prevent multiple drawings here.
  // Also remove the OnModified event of the lists, since everything
  // is up to date right here
  for i := 0 to High(FGPSItems) do
  begin
    FGPSItems[i].BeginUpdate;
    FSavedOnModifiedEvents[i] := FGPSItems[i].OnModified;
    FGPSItems[i].OnModified := Nil;
  end;
end;

procedure TMapView.EndUpdateObjects;
var
  i: Integer;
begin
  // EndUpdate and attach the original event method again to allow
  // the reflection of the changing in the GPSItems while drawing
  for i := 0 to High(FGPSItems) do
  begin
    FGPSItems[i].EndUpdate;
    FGPSItems[i].OnModified := FSavedOnModifiedEvents[i];
  end;
  SetLength(FSavedOnModifiedEvents, 0);
end;

function TMapView.CyclicPointOf(APoint: TPoint; ARefX: LongInt;
  Eastwards: Boolean): TPoint;
var
  WorldSize: Int64;
begin
  Result := APoint;
  WorldSize := mvGeoMath.ZoomFactor(Zoom) * TileSize.CX;
  if Eastwards then
  begin
    while Result.X < ARefX do
      Inc(Result.X, WorldSize);
    while Result.X >= ARefX + WorldSize do
      Dec(Result.X, WorldSize);
  end
  else
  begin
    while Result.X > ARefX do
      Dec(Result.X, WorldSize);
    while Result.X <= ARefX - WorldSize do
      Inc(Result.X, WorldSize);
  end;
end;

function TMapView.CyclicPointsOf(APoint: TPoint): TPointArray;
var
  I, R, L, WorldSize: LongInt;
begin
  Result := Default(TPointArray);
  if not Cyclic then
  begin
    SetLength(Result, 1);
    Result[0] := APoint;
  end
  else
  begin
    WorldSize := mvGeoMath.ZoomFactor(Zoom) * TileSize.CX;
    SetLength(Result, 1{APoint} + (1{Round} + FCanvasSize.CX div WorldSize));
    Result[0] := APoint;
    I := 1; R := APoint.X + WorldSize; L := APoint.X - WorldSize;
    while (R < FCanvasSize.CX) or (L >= 0) do
    begin
      if R < FCanvasSize.CX then
      begin
        Result[I].Y := APoint.Y;
        Result[I].X := R;
        Inc(I);
      end;
      if L >= 0 then
      begin
        Result[I].Y := APoint.Y;
        Result[I].X := L;
        Inc(I);
      end;
      Inc(R, WorldSize);
      Dec(L, WorldSize);
    end;
    if I < Length(Result) then
      SetLength(Result, I);
  end;
end;

procedure TMapView.SaveToFile(AClass: TRasterImageClass; const AFileName: String);
var
  stream: TFileStream;
begin
  stream := TFileStream.Create(AFileName, fmCreate + fmShareDenyNone);
  try
    SaveToStream(AClass, stream);
  finally
    stream.Free;
  end;
end;

function TMapView.SaveToImage(AClass: TRasterImageClass): TRasterImage;
begin
  Result := DrawingEngine.SaveToImage(AClass);
end;

procedure TMapView.SaveToStream(AClass: TRasterImageClass; AStream: TStream);
var
  img: TRasterImage;
begin
  img := SaveToImage(AClass);
  try
    img.SaveToStream(AStream);
  finally
    img.Free;
  end;
end;

function TMapView.ScreenToLatLon(aPt: TPoint): TRealPoint;
begin
  Result := Engine.ScreenToLatLon(aPt);
end;

function TMapView.ScreenToLonLat(aPt: TPoint): TRealPoint;
begin
  Result := Engine.ScreenToLatLon(aPt);
end;

function TMapView.LatLonToScreen(aPt: TRealPoint): TPoint;
begin
  Result := Engine.LatLonToScreen(aPt);
end;

function TMapView.LatLonToScreen(Lat, Lon: Double): TPoint;
begin
  Result := LatLonToScreen(RealPoint(Lat, Lon));
end;

function TMapView.LonLatToScreen(aPt: TRealPoint): TPoint;
begin
  Result := Engine.LatLonToScreen(aPt);
end;

procedure TMapView.GetMapProviders(lstProviders: TStrings);
var
  L: TStringList;
begin
  L := TStringList.Create;
  try
    Engine.GetMapProviders(L);
    L.Sort;
    lstProviders.Assign(L);
  finally
    L.Free;
  end;
//  Engine.GetMapProviders(lstProviders);
end;

function TMapView.GetPluginManager: TMvCustomPluginManager;
begin
  if Assigned(FPluginManager) then
    Result := FPluginManager
  else
    Result := FBuiltinPluginManager;
end;

procedure TMapView.WaitEndOfRendering;
begin
  Engine.Jobqueue.WaitAllJobTerminated(Engine);
end;

function TMapView.FindObjsAtScreenPt(X, Y: Integer; ATolerance: Integer; AVisibleOnly: Boolean): TGPSObjArray;
const
  BLOCK_SIZE = 32;
var
  rArea: TRealArea;
  gpsList: TGPSObjList;
  i, J: Integer;
  objsCount: Integer;
begin
  Result := nil;
  
  // Define area of +/-ATolerance pixels around the screen point
  rArea.TopLeft := ScreenToLatLon(Point(X-ATolerance, Y-ATolerance));
  rArea.BottomRight := ScreenToLatLon(Point(X+ATolerance, Y+ATolerance));

  // Collect Objects in this area
  objsCount := 0;
  for J := 0 to 9 do
  begin
    gpsList := FGPSItems[J].GetObjectsInArea(rArea);
    try
      for i := 0 to gpsList.Count-1 do
        if gpsList[i] is TGPSPoint then
        begin
          if AVisibleOnly and not gpsList[i].Visible then
            Continue;
          if objsCount mod BLOCK_SIZE = 0 then
            SetLength(Result, Length(Result) + BLOCK_SIZE);
          Result[objsCount] := gpsList[i];
          inc(objsCount);
        end;
        SetLength(Result, objsCount);
    finally
      gpsList.Free;
    end;
  end;
end;

function TMapView.ObjsAtScreenPt(X, Y: Integer; ATolerance: Integer = -1): TGPSObjArray;
begin
  if ATolerance = -1 then
    ATolerance := POINT_DELTA;
  Result := FindObjsAtScreenPt(X, Y, ATolerance, false);
end;

function TMapView.VisibleObjsAtScreenPt(X, Y: Integer; ATolerance: Integer = -1): TGPSObjArray;
begin
  if ATolerance = -1 then
    ATolerance := POINT_DELTA;
  Result := FindObjsAtScreenPt(X, Y, ATolerance, true);
end;


procedure TMapView.CenterOnObj(obj: TGPSObj);
var
  Area: TRealArea;
  Pt: TRealPoint;
begin
  obj.GetArea(Area);
  Pt.Lon := (Area.TopLeft.Lon + Area.BottomRight.Lon) /2;
  Pt.Lat := (Area.TopLeft.Lat + Area.BottomRight.Lat) /2;
  Center := Pt;
end;

procedure TMapView.ZoomOnObj(obj: TGPSObj);
var
  Area: TRealArea;
begin
  obj.GetArea(Area);
  Engine.ZoomOnArea(Area);
end;

procedure TMapView.ZoomOnArea(const aArea: TRealArea);
begin
  Engine.ZoomOnArea(aArea);
end;

procedure TMapView.Redraw;
begin
  Invalidate;
end;

function TMapView.GetVisibleArea: TRealArea;
var
  mapWidth: Int64;
begin
  Result.TopLeft := Engine.ScreenToLatLon(Point(0, 0));
  Result.BottomRight := Engine.ScreenToLatLon(Point(Width, Height));
  if Cyclic then
  begin
    mapWidth := mvGeoMath.ZoomFactor(Engine.Zoom) * TileSize.CX;
    if Width >= mapWidth then
    begin
      Result.TopLeft.Lon := -180;
      Result.BottomRight.Lon := 180;
    end;
  end;
end;

procedure TMapView.ClearBuffer;
begin
  DrawingEngine.CreateBuffer(ClientWidth, ClientHeight);       // ???
end;

procedure TMapView.UpdateFont(Sender: TObject);
var
  fd: TFontData;
begin
  fd := GetFontData(FFont.Handle);
  DrawingEngine.FontName := fd.Name;
  DrawingEngine.FontSize := abs(round(fd.Height / FFont.PixelsPerInch * 72));
  DrawingEngine.FontStyle := FFont.Style;
  DrawingEngine.FontColor := ColorToRGB(FFont.Color);
  Invalidate;
end;

procedure TMapView.UpdateImage(Sender: TObject);
begin
  Invalidate;
end;

function TMapView.CreateLayers: TMapLayers;
begin
  Result := TMapLayers.Create(Self, BASE_Z_LAYER);
end;

procedure TMapView.UpdateLayers;
var
  I: Integer;
begin
  for I := 0 to Pred(FLayers.Count) do
    FLayers[I].ComboLayer.TileLayer.ParentViewChanged;
end;

procedure TMapView.CreateEditor;
begin
  if Assigned(FEditMark) then
    DoneEditor;

  FEditMark := TMapEditMark.Create(Self);
  FEditMark.UpdateFrom(Nil);
  FGPSItems[High(FGPSItems)].Add(FEditMark, _MAPEDITOR_ID_, MaxInt);
  FEditMark.Visible := True;

  FEditMark.OnSelectionCompleted := @DoEditSelectionCompleted;
  FEditMark.OnStartDrag := @DoEditStartDrag;
  FEditMark.OnDrag := @DoEditDrag;
  FEditMark.OnEndDrag := @DoEditEndDrag;
  FEditMark.OnDirty := @DoEditIsDirty;

  FDragger := TDragObj.Create;
  FDragger.OnDrag := @FEditMark.DoDrag;
  FDragger.OnEndDrag := @FEditMark.DoEndDrag;
end;

procedure TMapView.DoneEditor;
begin
  if not Assigned(FEditMark) then
    Exit;
  FDragger.Free;
  FGPSItems[High(FGPSItems)].Delete(FEditMark);
  FEditMark := Nil;
end;

function TMapView.EditingEnabled: Boolean;
begin
  Result := IsActive and (mvoEditorEnabled in Options);
end;

function TMapView.DraggingEnabled: Boolean;
begin
  Result := IsActive and (mvoMouseDragging in FOptions);
end;

procedure TMapView.DoEditSelectionCompleted(Sender: TObject);
var
  What: TMapObserverCustomOperation = mooSelectionCompleted;
begin
  FPONotifyObservers(Self, ooCustom, @What);
  if Assigned(FOnEditSelectionCompleted) then
    FOnEditSelectionCompleted(Self);
end;

procedure TMapView.DoEditStartDrag(Sender: TObject);
var
  What: TMapObserverCustomOperation = mooStartDrag;
begin
  FPONotifyObservers(Self, ooCustom, @What);
  if Assigned(FOnEditStartDrag) then
    FOnEditStartDrag(Self);
end;

procedure TMapView.DoEditDrag(Sender: TObject);
var
  What: TMapObserverCustomOperation = mooDrag;
begin
  FPONotifyObservers(Self, ooCustom, @What);
  if Assigned(FOnEditDrag) then
    FOnEditDrag(Self);
end;

procedure TMapView.DoEditEndDrag(Sender: TObject);
var
  What: TMapObserverCustomOperation = mooEndDrag;
begin
  FPONotifyObservers(Self, ooCustom, @What);
  if Assigned(FOnEditEndDrag) then
    FOnEditEndDrag(Self);
end;

procedure TMapView.DoEditIsDirty(Sender: TObject);
var
  What: TMapObserverCustomOperation = mooIsDirty;
begin
  FPONotifyObservers(Self, ooCustom, @What);
  if Assigned(FOnEditIsDirty) then
    FOnEditIsDirty(Self);
end;

procedure TMapView.ChangeCachePath(AOldLoc: TCacheLocation; ANewPath: String);
var
  OldPath: String;
begin
  OldPath := Engine.CachePath;
  FCacheFullPath := ANewPath;
  //ForceDirectories(ANewPath);
  Engine.CachePath := ANewPath;
  UpdateLayers;
  if AOldLoc = clTemp then
    DeleteDirectory(OldPath, False);
end;

class function TMapView.CacheDirectory(ALoc: TCacheLocation;
  ACustomPath: String): String;
const
  LazMVCacheFolder: String = '.lazmapcache/';
begin
  case ALoc of
    clProfile: Result := Concat(GetUserDir, LazMVCacheFolder);
    clTemp: Result := Concat(GetTempDir(True), LazMVCacheFolder);
  otherwise
    Result := ACustomPath;
  end;
end;


{ TGPSTileLayerBase }

function TGPSTileLayerBase.GetMapProvider: String;
begin
  Result := FEngine.MapProvider;
end;

function TGPSTileLayerBase.GetUseThreads: Boolean;
begin
  Result := FEngine.UseThreads;
end;

procedure TGPSTileLayerBase.SetParentView(AValue: TMapView);
begin
  if (FParentView = AValue) and not FParentViewChanged then
    Exit;
  FEngine.Active := False;
  FParentView := AValue;
  if not Assigned(FParentView) then
    Exit;
  FEngine.DownloadEngine := FParentView.DownloadEngine;
  FEngine.CacheItemClass := FParentView.Engine.CacheItemClass;
  FEngine.CachePath := FParentView.Engine.CachePath;
  FEngine.CacheOnDisk := FParentView.Engine.CacheOnDisk;
  FEngine.CacheMaxAge := FParentView.Engine.CacheMaxAge;
  FEngine.Cyclic := FParentView.Engine.Cyclic;
  FEngine.CopyMapWindowFrom(FParentView.Engine);
  FEngine.Active := True;
end;

procedure TGPSTileLayerBase.DoTileDownloaded(const TileId: TTileId);
begin
  TileDownLoaded(TileId);
  if Assigned(FParentView) then
    FParentView.Redraw;
end;

procedure TGPSTileLayerBase.DoDrawTile(const TileId: TTileId; X, Y: Integer;
  TileImg: TPictureCacheItem);
begin
  DrawTile(TileId, X, Y, TileImg);
end;

procedure TGPSTileLayerBase.SetDrawMode(AValue: TItemDrawMode);
begin
  if FDrawMode = AValue then Exit;
  FDrawMode := AValue;
  if Assigned(FParentView) then
    FParentView.Redraw;
end;

procedure TGPSTileLayerBase.TileDownloaded(const TileId: TTileId);
begin
  ; // Intentionally empty
end;

procedure TGPSTileLayerBase.SetMapProvider(AValue: String);
begin
  if AValue = FMapProvider then
    Exit;
  FMapProvider := AValue;
  FEngine.MapProvider := FMapProvider;
end;

procedure TGPSTileLayerBase.SetOpacity(AValue: Single);
begin
  if FOpacity = AValue then Exit;
  FOpacity := AValue;
  if Assigned(FParentView) then
    FParentView.Redraw;
end;

procedure TGPSTileLayerBase.SetUseThreads(AValue: Boolean);
begin
  FEngine.UseThreads := AValue;
end;

constructor TGPSTileLayerBase.Create;
begin
  inherited;
  FEngine := TMapViewerEngine.Create(Nil);
  FMapProvider := FEngine.MapProvider;
  FEngine.OnTileDownloaded := @DoTileDownloaded;
  FEngine.OnDrawTile := @DoDrawTile;
end;

destructor TGPSTileLayerBase.Destroy;
begin
  FEngine.Free;
  inherited Destroy;
end;

procedure TGPSTileLayerBase.GetArea(out Area: TRealArea);
begin
  if Assigned(FParentView)
    then Area := FParentView.GetVisibleArea // Always over visible area
    else Area.Init(-180.0, 90.0, 180.0, -90.0); // Worldwide
end;

procedure TGPSTileLayerBase.Draw(AView: TObject; Area: TRealArea);
begin
  SetParentView(AView as TMapView);
  FEngine.CopyMapWindowFrom(FParentView.Engine);
end;

procedure TGPSTileLayerBase.ParentViewChanged;
begin
  FParentViewChanged := True;
end;

{ TGPSTileLayerLabels }

procedure TGPSTileLayerLabels.DrawTile(const TileId: TTileId; X, Y: Integer;
  TileImg: TPictureCacheItem);
begin
  ; // Intentionally blank
end;

procedure TGPSTileLayerLabels.Draw(AView: TObject; Area: TRealArea);
var
  V: TMapView;
  PtTL, PtBR, Pt0, Pt: TPoint;
  X, Y, Z, W, H: Integer;
  S: String;
  extent: TSize;
begin
  inherited Draw(AView, Area);
  V := FParentView;
  PtTL := V.Engine.LatLonToWorldScreen(Area.TopLeft);
  PtBR := V.Engine.LatLonToWorldScreen(Area.BottomRight);
  X := -PtTL.X div TileSize.CX;
  Y := -PtTL.Y div TileSize.CY;
  Pt0 := Point(V.Engine.MapLeft + X * TileSize.CX, V.Engine.MapTop + Y * TileSize.CY);
  Pt := Pt0;
  H := Y + (PtBR.Y - PtTL.Y) div TileSize.CY;
  while Y <= H do
  begin
    X := -PtTL.X div TileSize.CX;
    W := X + (PtBR.X - PtTL.X) div TileSize.CY;
    while X <= W do
    begin
      Z := V.Zoom;
      V.DrawingEngine.BrushStyle := bsSolid;
      V.DrawingEngine.BrushColor := clCream;
      S := Format(' %d-%d-%d ', [X, Y, Z]);
      extent := V.DrawingEngine.TextExtent(S);
      V.DrawingEngine.TextOut(Pt.X + (TileSize.CX - extent.CX) div 2,
        Pt.Y + (TileSize.CY - extent.CY) div 2, S);
      Inc(Pt.X, TileSize.CX);
      Inc(X);
    end;
    Pt.X := Pt0.X;
    Inc(Pt.Y, TileSize.CY);
    Inc(Y);
  end;
end;

{ TGPSTileLayer }

procedure TGPSTileLayer.DrawTile(const TileId: TTileId; X, Y: Integer;
  TileImg: TPictureCacheItem);
begin
  if not Assigned(FParentView) or not Assigned(TileImg) then
    Exit;
  FParentView.DrawingEngine.DrawCacheItem(X, Y, TileImg, FDrawMode, FOpacity);
end;

procedure TGPSTileLayer.Draw(AView: TObject; Area: TRealArea);
begin
  inherited Draw(AView, Area);
  FEngine.Redraw;
end;

procedure TGPSTileLayer.TileDownloaded(const TileId: TTileId);
begin
  FEngine.Redraw;
end;

{ TMapEditorListFilterEnumerator }

constructor TMapEditorListFilterEnumerator.Create(AList: TMapObjectList);
begin
  FList := AList;
  FIndex := -1;
end;

function TMapEditorListFilterEnumerator.MoveNext: Boolean;
begin
  repeat Inc(FIndex);
  until (FIndex >= FList.Count) or (FList[FIndex] is TItemClass);
  Result := FIndex < FList.Count;
  if Result then
    FCurrent := TItemClass(FList[FIndex]);
end;

function TMapEditorListFilterEnumerator.GetEnumerator: TMapEditorListFilterEnumerator;
begin
  Result := Self;
end;

function TMapEditorListFilterEnumerator.Skip(ANum: Integer
  ): TMapEditorListFilterEnumerator;
var
  I: Integer;
begin
  for I := 1 to ANum do
    MoveNext;
  Result := Self;
end;

{ TMapEditMark }

function TMapEditMark.GetCursorShape: TCursor;
begin
  if Assigned(FLit)
    then Result := crHandPoint
    else Result := crDefault;
end;

function TMapEditMark.GetCurrentPoint: TMapPoint;
begin
  if HasSelection
    then Result := TMapPoint(FSelection[0])
    else Result := Nil;
end;

function TMapEditMark.GetCurrentArea: TMapArea;
var
  A: TMapArea;
  Col: TCollection = Nil;
begin
  Result := Nil;
  if Assigned(GetCurrentPoint) then
    Col := GetCurrentPoint.Collection;
  for A in FSelection.Areas do
    if Col = A.Points then
      Exit(A);
end;

function TMapEditMark.GetCurrentTrack: TMapTrack;
var
  T: TMapTrack;
  Col: TCollection = Nil;
begin
  Result := Nil;
  if Assigned(GetCurrentPoint) then
    Col := GetCurrentPoint.Collection;
  for T in FSelection.Tracks do
    if Col = T.Points then
      Exit(T);
end;

function TMapEditMark.GetHasSelection: Boolean;
begin
  Result := (FSelection.Count > 0);
end;

procedure TMapEditMark.CompleteSelection;
var
  T: TObject;
begin
  if FClearSelection then
  begin
    FSelection.Clear;
    FClearSelection := False;
    FTruncSelection := False;
  end
  else if FTruncSelection and (FSelection.Count > 1) then
  begin
    // Find the container track/area for the first point
    T := CurrentTrack;
    if T = Nil then
      T := CurrentArea;

    // Delete everything except the first point
    FSelection.DeleteRange(1, FSelection.Count - 1);

    // Re-insert the container track/area
    if Assigned(T) then
      FSelection.Add(T);

    FTruncSelection := False;
  end;
  if Assigned(FOnSelectionCompleted) then
    FOnSelectionCompleted(Self);
end;

procedure TMapEditMark.Select(APoint: TMapPoint; ClearFirst: Boolean);
begin
  if ClearFirst then
    FSelection.Clear;
  FSelection.Insert(0, APoint);
  FMapView.Invalidate;
end;

function TMapEditMark.IsSelected(AObj: TObject): Boolean;
begin
  Result := FSelection.IndexOf(AObj) >= 0;
end;

function TMapEditMark.AroundPt(X, Y: Integer; APt: TPoint): Boolean;
begin
  Result := InRange(X, APt.X - 5, APt.X + 5) and
    InRange(Y, APt.Y - 5, APt.Y + 5);
end;

procedure TMapEditMark.SelectFromMarquee;
var
  Hits: TMapObjectList;
  RA: TRealArea;
  O: TObject;
begin
  RA.TopLeft := FMapView.ScreenToLatLon(FMarqueeRect.TopLeft);
  RA.BottomRight := FMapView.ScreenToLatLon(FMarqueeRect.BottomRight);
  Hits := FMapView.Layers.HitTest(RA);
  if Assigned(Hits) then
    try
      for O in Hits do
        FSelection.AddIfNotPresent(O);
      FMapView.Invalidate;
    finally
      Hits.Free;
    end;
end;

procedure TMapEditMark.MarkDirty;
begin
  if not FDirty then
  begin
    FDirty := True;
    if Assigned(FOnDirty) then
      FOnDirty(Self);
  end;
end;

constructor TMapEditMark.Create(AMapView: TMapView);
begin
  FMapView := AMapView;
  FSelection := TMapObjectList.Create;
end;

destructor TMapEditMark.Destroy;
begin
  FSelection.Free;
  inherited Destroy;
end;

procedure TMapEditMark.GetArea(out Area: TRealArea);
begin
  Area.Init(FRealPt, FRealPt);
end;

procedure TMapEditMark.Draw(AView: TObject; Area: TRealArea);
var
  View: TMapView;
  DE: TMvCustomDrawingEngine;
  Trk: TMapTrack;
  Ar: TMapArea;
  TrkPoint: TMapPoint;
  I: Integer;

  procedure MarkMP(P: TMapPoint);
  begin
    with View.LatLonToScreen(P.Latitude, P.Longitude) do
      DE.Ellipse(X - 4, Y - 4, X + 4, Y + 4);
  end;

begin
  View := TMapView(AView);
  DE := View.DrawingEngine;
  FPt := View.LatLonToScreen(RealPt);

  DE.PenStyle := psSolid;
  DE.PenColor := clRed;
  DE.PenWidth := 3;
  DE.BrushColor := clGray;
  DE.BrushStyle := bsSolid;

  if Assigned(FLit) then
    DE.Rectangle(FPt.X - 5, FPt.Y - 5, FPt.X + 5, FPt.Y + 5);

  if HasSelection then
  begin
    DE.PenWidth := 2;
    DE.BrushStyle := bsClear;

    for Trk in FSelection.Tracks do
      for I := 0 to Pred(Trk.Points.Count) do
        MarkMP(Trk.Points[I]);

    for Ar in FSelection.Areas do
      for I := 0 to Pred(Ar.Points.Count) do
        MarkMP(Ar.Points[I]);

    DE.PenWidth := 2;
    DE.BrushColor := clBlack;
    DE.BrushStyle := bsSolid;

    for TrkPoint in FSelection.Points.Skip(1) do
      with View.LatLonToScreen(TrkPoint.Latitude, TrkPoint.Longitude) do
        DE.Rectangle(X - 5, Y - 5, X + 5, Y + 5);

    DE.BrushColor := clLime;
    TrkPoint := (FSelection[0] as TMapPoint);
    with View.LatLonToScreen(TrkPoint.Latitude, TrkPoint.Longitude) do
        DE.Rectangle(X - 5, Y - 5, X + 5, Y + 5);
  end;

  if FMarquee then
  begin
    DE.PenStyle := psSolid;
    DE.PenWidth := 1;
    DE.PenColor := clGray;
    DE.BrushStyle := bsClear;

    with FMarqueeRect do
      DE.Rectangle(Left, Top, Right, Bottom);
  end;
end;

procedure TMapEditMark.UpdateFrom(AObjs: TMapObjectList);
begin
  if Assigned(AObjs) and (AObjs.Count > 0) and (AObjs[0] is TMapPoint) then
  begin
    // Same point?
    if Assigned(FLit) and (AObjs[0] = FLit[0]) then
      Exit;
    Lat := TMapPoint(AObjs[0]).Latitude;
    Lon := TMapPoint(AObjs[0]).Longitude;
    FLit.Free;
    FLit := TMapObjectList.Create(AObjs);
  end
  else
    FreeAndNil(FLit);
  FMapView.Invalidate;
end;

function TMapEditMark.ClickableAt(X, Y: Integer): Boolean;
begin
  Result := True; //Visible;
end;

function TMapEditMark.ClickAt(X, Y: Integer): Boolean;
var
  O: TObject;
  H: Boolean;
begin
  Result := True;
  if Assigned(FLit) and AroundPt(X, Y, FPt) then
  begin
    FTruncSelection := not (ssCtrl in GetKeyShiftState);
    H := FSelection.DelIfPresent(FLit[0]);
    FTruncSelection := not H and FTruncSelection;
    FSelection.Insert(0, FLit[0]);
    for O in FLit do
      FSelection.AddIfNotPresent(O);
    FreeAndNil(FLit);
  end
  else
    FClearSelection := not (ssCtrl in GetKeyShiftState);
end;

procedure TMapEditMark.ClearSelection;
begin
  FClearSelection := True;
  CompleteSelection;
end;

procedure TMapEditMark.DoStartDrag(Sender: TDragObj);
var
  I: Integer = 0;
  P: TMapPoint;
begin
  FLit := Nil;
  CompleteSelection;
  SetLength(FOrigins, FSelection.Count);
  for P in FSelection.Points do
  begin
    FOrigins[I] := RealPoint(P.Latitude, P.Longitude);
    Inc(I);
  end;
  SetLength(FOrigins, I); // why bother?
  FMarquee := not AroundPt(Sender.StartX, Sender.StartY, FPt);
  if FMarquee then
  begin
    FMarqueeRect := Rect(Sender.StartX, Sender.StartY, Sender.EndX, Sender.EndY);
    Self.RealPt := FMapView.Center; // keep it in view
    FMapView.Invalidate;
  end;
  FDragStarted := True;
  if Assigned(FOnStartDrag) then
    FOnStartDrag(Self);
end;

procedure TMapEditMark.DoDrag(Sender: TDragObj);
var
  I: Integer = 0;
  RPt: TRealPoint;
  MapPoint: TMapPoint;
begin
  if not FDragStarted then
    DoStartDrag(Sender);
  if FMarquee then
  begin
    FMarqueeRect := Rect(Sender.StartX, Sender.StartY, Sender.EndX, Sender.EndY);
    FMarqueeRect.NormalizeRect;
    FMapView.Invalidate;
    Exit;
  end;
  for MapPoint in FSelection.Points do
  begin
    MarkDirty;
    Rpt := FMapView.ScreenToLatLon(FMapView.LatLonToScreen(FOrigins[I]) +
      Point(Sender.OfsX, Sender.OfsY));
    MapPoint.Longitude := RPt.Lon;
    MapPoint.Latitude := RPt.Lat;
    Inc(I);
  end;
  //FMapView.Invalidate; // No need to
  if Assigned(FOnDrag) then
    FOnDrag(Self);
end;

procedure TMapEditMark.DoEndDrag(Sender: TDragObj);
begin
  if FMarquee then
  begin
    SelectFromMarquee;
    FMarquee := False;
  end;
  SetLength(FOrigins, 0);
  FDragStarted := False;
  if Assigned(FOnEndDrag) then
    FOnEndDrag(Self);
end;

{ TMapEditorList }

function TMapEditorList.GetAreasOnly: TMapEditorAreasFilterEnumerator;
begin
  Result := TMapEditorAreasFilterEnumerator.Create(Self);
end;

function TMapEditorList.GetPointsOnly: TMapEditorPointsFilterEnumerator;
begin
  Result := TMapEditorPointsFilterEnumerator.Create(Self);
end;

function TMapEditorList.GetTracksOnly: TMapEditorTracksFilterEnumerator;
begin
  Result := TMapEditorTracksFilterEnumerator.Create(Self);
end;

function TMapEditorList.IndexOfObj(const Item: TObject; out Index: Integer
  ): Boolean;
begin
  Index := IndexOf(Item);
  Result := Index >= 0;
end;

function TMapEditorList.DelIfPresent(const Item: TObject): Boolean;
var
  I: Integer;
begin
  Result := False;
  while IndexOfObj(Item, I) do
  begin
    Result := True;
    Delete(I);
  end;
end;

function TMapEditorList.AddIfNotPresent(const Item: TObject): Boolean;
begin
  Result := IndexOf(Item) < 0;
  if Result then
    Add(Item);
end;


{ TMvCustomPluginManager }

procedure TMvCustomPluginManager.AddMapView(AMapView: TMapView);
begin
//
end;

{ Just executes the handler assigned to the OnAfterDrawObjects event of the
  mapview. The descendant plugin manager will have to iterate over all plugins
  used by it. }
procedure TMvCustomPluginManager.AfterDrawObjects(AMapView: TMapView;
  AMapEvent: TNotifyEvent; out Handled: Boolean);
begin
  Handled := False;
  DefaultNotifyEventHandler(AMapView, AMapEvent);
end;

procedure TMvCustomPluginManager.AfterPaint(AMapView: TMapView;
  AMapEvent: TNotifyEvent; out Handled: Boolean);
begin
  DefaultNotifyEventHandler(AMapView, AMapEvent);
end;

procedure TMvCustomPluginManager.BeforeDrawObjects(AMapView: TMapView;
  AMapEvent: TNotifyEvent; out Handled: Boolean);
begin
  Handled := False;
  DefaultNotifyEventHandler(AMapView, AMapEvent);
end;

procedure TMvCustomPluginManager.CenterMove(AMapView: TMapView;
  AMapEvent: TNotifyEvent);
begin
  DefaultNotifyEventHandler(AMapView, AMapEvent);
end;

procedure TMvCustomPluginManager.DefaultMouseEvent(AMapView: TMapView;
  AButton: TMouseButton; AShift: TShiftState; X, Y: Integer; AMapEvent: TMouseEvent);
begin
  if Assigned(AMapEvent) then
    AMapEvent(AMapView, AButton, AShift, X, Y);
end;

procedure TMvCustomPluginManager.DefaultNotifyEventHandler(AMapView: TMapView;
  AMapEvent: TNotifyEvent);
begin
  if Assigned(AMapEvent) then
    AMapEvent(AMapView);
end;

procedure TMvCustomPluginManager.MouseDown(AMapView: TMapView;
  AButton: TMouseButton; AShift: TShiftState; X, Y: Integer;
  AMapEvent: TMouseEvent; out Handled: Boolean);
begin
  Handled := False;
  DefaultMouseEvent(AMapView, AButton, AShift, X, Y, AMapEvent);
end;

procedure TMvCustomPluginManager.MouseEnter(AMapView: TMapView;
  AMapEvent: TNotifyEvent; out Handled: Boolean);
begin
  Handled := False;
  DefaultNotifyEventHandler(AMapView, AMapEvent);
end;

procedure TMvCustomPluginManager.MouseLeave(AMapView: TMapView;
  AMapEvent: TNotifyEvent; out Handled: Boolean);
begin
  Handled := False;
  DefaultNotifyEventHandler(AMapView, AMapEvent);
end;

procedure TMvCustomPluginManager.MouseMove(AMapView: TMapView;
  AShift: TShiftState; X, Y: Integer; AMapEvent: TMouseMoveEvent; out
  Handled: Boolean);
begin
  Handled := False;
  if Assigned(AMapEvent) then
    AMapEvent(AMapView, AShift, X, Y);
end;

procedure TMvCustomPluginManager.MouseUp(AMapView: TMapView;
  AButton: TMouseButton; AShift: TShiftState; X, Y: Integer;
  AMapEvent: TMouseEvent; out Handled: Boolean);
begin
  Handled := False;
  DefaultMouseEvent(AMapView, AButton, AShift, X, Y, AMapEvent);
end;

procedure TMvCustomPluginManager.RemoveMapView(AMapView: TMapView);
begin
  //
end;

procedure TMvCustomPluginManager.ZoomChange(AMapView: TMapView; AMapEvent: TNotifyEvent);
begin
  DefaultNotifyEventHandler(AMapView, AMapEvent);
end;

end.

