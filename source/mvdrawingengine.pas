{ Mapviewer drawing engine
  (C) 2019 Werner Pamler (user wp at Lazarus forum https://forum.lazarus.freepascal.org)

  License: modified LGPL with linking exception (like RTL, FCL and LCL)

  See the file COPYING.modifiedLGPL.txt, included in the Lazarus distribution,
  for details about the license.

  See also: https://wiki.lazarus.freepascal.org/FPC_modified_LGPL
}

unit mvDrawingEngine;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, GraphMath, Types, IntfGraphics, mvCache;

type

  TItemDrawMode = (idmDraw, idmUseOpacity, idmUseSourceAlpha);
  TLineDrawProc = procedure(X1, Y1, X2, Y2: Integer) of Object;
  TPointArray = array of TPoint;

  TMvFont = record
    FontName: String;
    Size: Integer;
    Style: TFontStyles;
    Color: TColor;
  end;

  { TMvCustomDrawingEngine }

  TMvCustomDrawingEngine = class(TComponent)
  protected
    function GetBrushColor: TColor; virtual; abstract;
    function GetBrushStyle: TBrushStyle; virtual; abstract;
    function GetFontColor: TColor; virtual; abstract;
    function GetFontName: String; virtual; abstract;
    function GetFontSize: Integer; virtual; abstract;
    function GetFontStyle: TFontStyles; virtual; abstract;
    function GetPenColor: TColor; virtual; abstract;
    function GetPenStyle: TPenStyle; virtual; abstract;
    function GetPenWidth: Integer; virtual; abstract;
    function GetOpacity: Single; virtual; abstract;
    procedure SetOpacity(AValue: Single); virtual; abstract;
    procedure SetPenStyle(AValue: TPenStyle); virtual; abstract;
    procedure SetBrushColor(AValue: TColor); virtual; abstract;
    procedure SetBrushStyle(AValue: TBrushStyle); virtual; abstract;
    procedure SetFontColor(AValue: TColor); virtual; abstract;
    procedure SetFontName(AValue: String); virtual; abstract;
    procedure SetFontSize(AValue: Integer); virtual; abstract;
    procedure SetFontStyle(AValue: TFontStyles); virtual; abstract;
    procedure SetPenColor(AValue: TColor); virtual; abstract;
    procedure SetPenWidth(AValue: Integer); virtual; abstract;
    class procedure DoScanFill(APoly: array of TPoint; ALineDrawProc: TLineDrawProc);
    class procedure CalcBezier(APoints: array of TPoint; Continuous: Boolean; out APoly: TPointArray);
  public
    function GetCacheItemClass: TPictureCacheItemClass; virtual; abstract;
    procedure CreateBuffer(AWidth, AHeight: Integer); virtual; abstract;
    procedure DrawBitmap(X, Y: Integer; ABitmap: TCustomBitmap;
      UseAlphaChannel: Boolean); virtual; abstract;
    procedure DrawCacheItem(X, Y: Integer; AImg: TPictureCacheItem;
      ADrawMode: TItemDrawMode = idmDraw; AOpacity: Single = 1.0); virtual; abstract;
    procedure DrawScaledCacheItem(DestRect, SrcRect: TRect; AImg: TPictureCacheItem); virtual; abstract;
    procedure Ellipse(X1, Y1, X2, Y2: Integer); virtual; abstract;
    procedure FillPixels(X1, Y1, X2, Y2: Integer; AColor: TColor); virtual; abstract;
    procedure FillRect(X1, Y1, X2, Y2: Integer); virtual; abstract;
    function GetFont: TMvFont;
    procedure Line(X1, Y1, X2, Y2: Integer); virtual; abstract;
    procedure Polyline(const Points: array of TPoint); virtual; abstract;
    procedure Polygon(const Points: array of TPoint); virtual; abstract;
    procedure PolyBezier(const Points: array of TPoint; Filled: Boolean = False;
      Continuous: Boolean = True);  virtual; abstract;
    procedure PaintToCanvas(ACanvas: TCanvas); overload;
    procedure PaintToCanvas(ACanvas: TCanvas; Origin: TPoint); overload; virtual; abstract;
    procedure Rectangle(X1, Y1, X2, Y2: Integer); virtual; abstract;
    function SaveToImage(AClass: TRasterImageClass): TRasterImage; virtual; abstract;
    procedure SetFont(AFont: TMvFont);
    procedure SetFont(AFontName: String; AFontSize: Integer; AFontStyle: TFontStyles; AFontColor: TColor);
    function TextExtent(const AText: String): TSize; virtual; abstract;
    function TextHeight(const AText: String): Integer;
    procedure TextOut(X, Y: Integer; const AText: String); virtual; abstract;
    function TextWidth(const AText: String): Integer;

    property BrushColor: TColor read GetBrushColor write SetBrushColor;
    property BrushStyle: TBrushStyle read GetBrushStyle write SetBrushStyle;
    property FontColor: TColor read GetFontColor write SetFontColor;
    property FontName: String read GetFontName write SetFontName;
    property FontSize: Integer read GetFontSize write SetFontSize;
    property FontStyle: TFontStyles read GetFontStyle write SetFontStyle;
    property PenColor: TColor read GetPenColor write SetPenColor;
    property PenStyle: TPenStyle read GetPenStyle write SetPenStyle;
    property PenWidth: Integer read GetPenWidth write SetPenWidth;
    property Opacity: Single read GetOpacity write SetOpacity;
  end;

// Vector <MX, MY> orthogonal to a line <X1, Y1>, <X2, Y2>
function OrthoVec(X1, Y1, X2, Y2: Integer; out MX, MY: Double): Boolean;

// Intersection point between line segments <P1, P2> and <P3, P4>
// Returns:
//   0 - colinear line segments
//   1 - line segments intersect at PX
//   2 - colinear overlapping line segments, PX lies on both
//
function Intersect(P1, P2, P3, P4: TPoint; out PX: TPoint): Integer;

// Clip line segment <P1, P2> to rectangle ARect. Cohen-Sutherland algorithm.
// Returns True when line entirely clipped.
//
function ClipLineToRect(constref ARect: TRect; var P1, P2: TPoint): Boolean;

// Clip polygon to rectangle ARect. Sutherland-Hodgeman algorithm (sort of).
// Returns True when polygon entirely clipped. APoly will be modified in
// either case. Beware, there is no COW.
//
function ClipPolyToRect(constref ARect: TRect; var APoly: TPointArray;
  NumPts: Integer = MaxInt): Boolean;

// Polyline bounds
procedure PolyBounds(APoly: array of TPoint; out ABounds: TRect);

implementation

uses
  Math, LCLType, FPImage, fgl;

type
  TMvPointList = specialize TFPGList<TPoint>;

function Intersect(P1, P2, P3, P4: TPoint; out PX: TPoint): Integer;
var
  t, d, u: LongInt;
  f2: Boolean = False;
begin
  Result := 0;
  d := (P1.X - P2.X) * (P3.Y - P4.Y) - (P1.Y - P2.Y) * (P3.X - P4.X);
  if d = 0 then  // colinear?
  begin
    // P1 on line P3,P4?
    d := (P1.X - P3.X) * (P4.Y - P3.Y) - (P1.Y - P3.Y) * (P4.X - P3.X);
    if (P3 = P4) or (d <> 0) then
      Exit; // P1,P3,P4 not colinear
    // Trick the intersection by changing the second segment
    Dec(P3.Y);
    Inc(P4.Y);
    d := (P1.X - P2.X) * (P3.Y - P4.Y) - (P1.Y - P2.Y) * (P3.X - P4.X);
    if d > 0.0
      then f2 := True
      else Exit;
  end;
  t := (P1.X - P3.X) * (P3.Y - P4.Y) - (P1.Y - P3.Y) * (P3.X - P4.X);
  if (Sign(t) * Sign(d) < 0) or (Abs(t) > Abs(d)) then // 0 <= t/d <= 1
    Exit;
  u := (P1.X - P3.X) * (P1.Y - P2.Y) - (P1.Y - P3.Y) * (P1.X - P2.X);
  if (Sign(u) * Sign(d) < 0) or (Abs(u) > Abs(d)) then // 0 <= u/d <= 1
    Exit;
  PX.X := P1.X + Round(Double(t) * (P2.X - P1.X) / d);
  PX.Y := P1.Y + Round(Double(t) * (P2.Y - P1.Y) / d);
  if f2
    then Result := 2  // Second segment changed
    else Result := 1;
end;

procedure PolyBounds(APoly: array of TPoint; out ABounds: TRect);
var
  I, XMax, XMin, YMax, YMin: LongInt;
begin
  ABounds := Default(TRect);
  if Length(APoly) < 1 then
    Exit;
  XMax := APoly[0].X; XMin := XMax;
  YMax := APoly[0].Y; YMin := YMax;
  for I := 1 to High(APoly) do
  begin
    if APoly[I].X > XMax
      then XMax := APoly[I].X
      else if APoly[I].X < XMin
        then XMin := APoly[I].X;
    if APoly[I].Y > YMax
      then YMax := APoly[I].Y
      else if APoly[I].Y < YMin
        then YMin := APoly[I].Y;
  end;
  ABounds := Rect(XMin, YMin, XMax, YMax);
end;

function OrthoVec(X1, Y1, X2, Y2: Integer; out MX, MY: Double): Boolean;
var
  DX, DY: Integer;
  B: Double;

  // Inverted vector magnitude
  function InvMagn(X, Y: Double): Double; inline;
  begin
    Result := 1.0 / Sqrt(X * X + Y * Y);
  end;

begin
  if (Y1 = Y2) and (X1 = X2) then
    Exit(False);

  DX := X2 - X1;
  DY := Y2 - Y1;
  MX := 1.0;
  MY := 1.0;

  if DX = 0 then
    MY := 0.0 // <1.0, 0.0>
  else if DY = 0 then
    MX := 0.0 // <0.0, 1.0>
  else
  begin
    B := InvMagn(DX, DY);
    MX := DY * B;
    MY := -DX * B;
  end;
  Result := True;
end;

function ClipPolyToRect(constref ARect: TRect; var APoly: TPointArray;
  NumPts: Integer {=MaxInt}): Boolean;

var
  TmpPoly: TPointArray = nil;
  I: Integer;
  J: Integer = 0;

  procedure ClipLTRB(LTRB, Z: Integer; var SrcPts, DstPts: TPointArray;
    var SrcCnt, DstCnt: Integer);
  var
    I: Integer;
    P1, P2: TPoint;
    Out1: Boolean;

    procedure Add(P: TPoint); inline;
    begin
      DstPts[DstCnt] := P;
      Inc(DstCnt);
    end;

    procedure ClipTop(constref ARect: TRect; P1, P2: TPoint);
    begin
      if (P1.Y < Z) and (P2.Y < Z) then // both outside
      else if (P1.Y >= Z) and (P2.Y >= Z) then // both inside
        Add(P2)
      else
      begin
        Out1 := (P1.Y < Z) and (P2.Y >= Z); // 1st outside, 2nd inside
        ClipLineToRect(ARect, P1, P2);
        if Out1 then
          Add(P1);
        Add(P2);
      end
    end;

    procedure ClipBot(constref ARect: TRect; P1, P2: TPoint);
    begin
      if (P1.Y >= Z) and (P2.Y >= Z) then // both outside
      else if (P1.Y < Z) and (P2.Y < Z) then // both inside
        Add(P2)
      else
      begin
        Out1 := (P1.Y >= Z) and (P2.Y < Z); // 1st outside, 2nd inside
        ClipLineToRect(ARect, P1, P2);
        if Out1 then
          Add(P1);
        Add(P2);
      end
    end;

    procedure ClipLeft(constref ARect: TRect; P1, P2: TPoint);
    begin
      if (P1.X < Z) and (P2.X < Z) then // both outside
      else if (P1.X >= Z) and (P2.X >= Z) then // both inside
        Add(P2)
      else
      begin
        Out1 := (P1.X < Z) and (P2.X >= Z); // 1st outside, 2nd inside
        ClipLineToRect(ARect, P1, P2);
        if Out1 then
          Add(P1);
        Add(P2);
      end
    end;

    procedure ClipRight(constref ARect: TRect; P1, P2: TPoint);
    begin
      if (P1.X >= Z) and (P2.X >= Z) then // both outside
      else if (P1.X < Z) and (P2.X < Z) then // both inside
        Add(P2)
      else
      begin
        Out1 := (P1.X >= Z) and (P2.X < Z); // 1st outside, 2nd inside
        ClipLineToRect(ARect, P1, P2);
        if Out1 then;
          Add(P1);
        Add(P2);
      end
    end;

    procedure MuxLTRB(P1, P2: TPoint; LTRB: Integer); inline;
    begin
      case LTRB of
        1{L}: ClipLeft(Rect(Z, -MaxInt, MaxInt, MaxInt), P1, P2);
        2{T}: ClipTop(Rect(-MaxInt, Z, MaxInt, MaxInt), P1, P2);
        3{R}: ClipRight(Rect(-MaxInt, -MaxInt, Z, MaxInt), P1, P2);
        4{B}: ClipBot(Rect(-MaxInt, -MaxInt, MaxInt, Z), P1, P2);
      end;
    end;

  begin
    if SrcCnt < 3 then
      Exit;
    SetLength(DstPts, SrcCnt + SrcCnt div 2 + 1);
    DstCnt := 0;
    P1 := SrcPts[0];
    for I := 1 to Pred(SrcCnt) do
    begin
      P2 := SrcPts[I];
      MuxLTRB(P1, P2, LTRB);
      P1 := P2;
    end;
    P2 := SrcPts[0]; // closure
    MuxLTRB(P1, P2, LTRB);
  end;

begin
  I := Min(NumPts, Length(APoly));
  if I < 3 then
    Exit(True);

  // Clip to four sides of the lectangle
  ClipLTRB(1{L}, ARect.Left, APoly, TmpPoly, I, J);
  ClipLTRB(2{T}, ARect.Top, TmpPoly, APoly, J, I);
  ClipLTRB(3{R}, ARect.Right, APoly, TmpPoly, I, J);
  ClipLTRB(4{B}, ARect.Bottom, TmpPoly, APoly, J, I);

  Result := I > 2;
  if Result then
    SetLength(APoly, I);
end;

function ClipLineToRect(constref ARect: TRect; var P1, P2: TPoint): Boolean;

  function PtArea(P: TPoint): Integer; inline;
  begin
    Result := 0;
    if P.X < ARect.Left then
      Result := Result or 1{L}
    else if P.X > ARect.Right then
      Result := Result or 2{R};
    if P.Y < ARect.Top then
      Result := Result or 4{T}
    else if P.Y > ARect.Bottom then
      Result := Result or 8{B};
  end;

var
  A, A1, A2: Integer;
  P: TPoint;
begin
  A1 := PtArea(P1);
  A2 := PtArea(P2);
  while True do
  begin
    if (A1 = 0) and (A2 = 0) then
      Exit(False)
    else if (A1 and A2) <> 0 then
      Exit(True)
    else
    begin
      if A1 <> 0
        then A := A1
        else A := A2;
      if (A and 1{L}) <> 0 then
      begin
        P.X := ARect.Left;
        P.Y := P1.Y + ((P2.Y - P1.Y) * (ARect.Left - P1.X)) div (P2.X - P1.X);
      end
      else if (A and 2{R}) <> 0 then
      begin
        P.X := ARect.Right;
        P.Y := P1.Y + ((P2.Y - P1.Y) * (ARect.Right - P1.X)) div (P2.X - P1.X);
      end
      else if (A and 4{T}) <> 0 then
      begin
        P.X := P1.X + ((P2.X - P1.X) * (ARect.Top - P1.Y)) div (P2.Y - P1.Y);
        P.Y := ARect.Top;
      end
      else if (A and 8{B}) <> 0 then
      begin
        P.X := P1.X + ((P2.X - P1.X) * (ARect.Bottom - P1.Y)) div (P2.Y - P1.Y);
        P.Y := ARect.Bottom;
      end;
      if A = A1 then
      begin
        P1 := P;
        A1 := PtArea(P);
      end
      else
      begin
        P2 := P;
        A2 := PtArea(P);
      end;
    end;
  end;
end;

function ComparePoints_1(const L, R: TPoint
  ): Integer;
begin
  Result := L.X - R.X;
  if Result = 0 then
    Result := L.Y - R.Y;
end;

class procedure TMvCustomDrawingEngine.DoScanFill(APoly: array of TPoint;
  ALineDrawProc: TLineDrawProc);
var
  XI, YI: LongInt;
  NPoly: array of TPoint = Nil;
  Bounds: TRect;
  XPoints: TMvPointList;
  I, R, L: Integer;

  // Intersect NPoly with the scan line segment <A, B>. Result in XPoints.
  procedure ScanLineIntersect(const A, B: TPoint);
  var
    I, FirstI, LastI: Integer;
    X: TPoint;

    // Return next index with wrapping
    function Nxt(I: Integer): Integer; inline;
    begin
      if I = L
        then Result := 0
        else Result := Succ(I);
    end;

    // Return prior index with wrapping
    function Pri(I: Integer): Integer; inline;
    begin
      if I = 0
        then Result := L
        else Result := Pred(I);
    end;

    // Logic at a vertice, LI - prev index, RI - next index, PI - for deletion
    function Vertice(LI, RI, PI: Integer): Boolean;
    var
      S1, S2: TValueSign;
    begin
      repeat // Prior vertice which is above/below
        S1 := Sign(A.Y - NPoly[LI].Y);
        if S1 <> 0 then
          Break;
        LI := Pri(LI);
      until LI = RI;
      repeat // Next vertice which is above/below
        S2 := Sign(A.Y - NPoly[RI].Y);
        if S2 <> 0 then
          Break;
        RI := Nxt(RI);
      until RI = LI;
      // Both neighboring vertices are on the same side?
      Result := not ((S1 + S2 = 0) and (S1 <> 0));
      if Result then
        XPoints.Delete(PI); // Delete other (PI)
    end;

    // Add an intersection point, with a vertice logic
    procedure AddPoint(X: TPoint);
    begin
      if XPoints.Count = 0 then
      begin
        XPoints.Add(X); FirstI := I; LastI := I;
      end
      // Twice (on the vertice)?
      else if (Nxt(LastI) = I) and (XPoints.Last = X) then
        Vertice(LastI, Nxt(I), Pred(XPoints.Count))
      // Twice (on the vertice)? Last point.
      else if (Nxt(I) = FirstI) and (XPoints.First = X) then
        Vertice(I, Nxt(FirstI), 0)
      else
      begin
        XPoints.Add(X); LastI := I;
     end;
    end;

  begin
    XPoints.Clear;
    for I := 0 to L do
    begin
      R := Intersect(A, B, NPoly[I], NPoly[Nxt(I)], X);
      case R of
        1: // One intersection point, X
          AddPoint(X);
        2: // The current segment of the polyline is on the scan line
          begin
            AddPoint(NPoly[I]);
            AddPoint(NPoly[Nxt(I)]);
          end;
        otherwise ; // No intersection
      end;
    end;
    XPoints.Sort(@ComparePoints_1);
  end;

begin
  // Make a new polygon on a 2x grid with no zero length or horizontal lines
  SetLength(NPoly, Length(APoly));
  L := 0;
  I := 0;
  YI := MaxInt; // Y of the previous point
  while I < Length(APoly) do
  begin
    NPoly[L].X := APoly[I].X * 2; // X * 2
    R := APoly[I].Y * 2;
    if R = YI // Last Y was the same?
      then R := R + 1; // Make it non horizontal
    NPoly[L].Y := R; // Y * 2
    YI := R; // Keep Y for the next
    Inc(L);
    XI := Succ(I); // Scan for the next non zero length
    while (XI < Length(APoly)) and (APoly[XI] = APoly[I]) do
      Inc(XI);
    I := XI;
  end;
  Dec(L); // L must be at the last point
  if NPoly[0] = NPoly[L] then
    Dec(L); // Skip last if it is closed

  // Get bounds of the new polygon
  PolyBounds(NPoly, Bounds);

  XPoints := TMvPointList.Create;
  try
    // Scan each other horizontal line
    YI := Bounds.Top;
    while YI < Bounds.Bottom do
    begin
      // Intersect with the polygon
      ScanLineIntersect(Point(Bounds.Left, YI), Point(Bounds.Right, YI));
      // Draw lines, even - odd
      if XPoints.Count > 0 then
        for XI := 0 to XPoints.Count div 2 - 1 do
          ALineDrawProc(
            XPoints[XI * 2].X div 2, YI div 2,
            XPoints[XI * 2 + 1].X div 2, YI div 2);
      Inc(YI, 2);
    end;
  finally
    XPoints.Free;
  end;
end;

class procedure TMvCustomDrawingEngine.CalcBezier(APoints: array of TPoint;
  Continuous: Boolean; out APoly: TPointArray);
var
  NPoints: Integer;
  PtArray: PPoint;
  PtCount: LongInt = 0;
begin
  NPoints := Length(APoints);
  if NPoints < 4 then
    Exit; // Curve must have at least 4 points
  PtArray := Nil;
  APoly := Nil;
  try
    PolyBezier2Polyline(APoints, PtArray, PtCount, Continuous);
    if PtCount > 0 then
    begin
      SetLength(APoly, PtCount);
      Move(PtArray^, APoly[0], PtCount * SizeOf(TPoint));
    end;
  finally
    ReallocMem(PtArray, 0);
  end;
end;

function TMvCustomDrawingEngine.GetFont: TMvFont;
begin
  Result.FontName := FontName;
  Result.Size := FontSize;
  Result.Style := FontStyle;
  Result.Color := FontColor;
end;

procedure TMvCustomDrawingEngine.PaintToCanvas(ACanvas: TCanvas);
begin
  PaintToCanvas(ACanvas, Point(0, 0));
end;

procedure TMvCustomDrawingEngine.SetFont(AFont: TMvFont);
begin
  SetFont(AFont.FontName, AFont.Size, AFont.Style, AFont.Color);
end;

procedure TMvCustomDrawingEngine.SetFont(AFontName: String; AFontSize: Integer;
  AFontStyle: TFontStyles; AFontColor: TColor);
begin
  FontName := AFontName;
  FontSize := AFontSize;
  FontStyle := AFontStyle;
  FontColor := AFontColor;
end;

function TMvCustomDrawingEngine.TextHeight(const AText: String): Integer;
begin
  Result := TextExtent(AText).CX;
end;

function TMvCustomDrawingEngine.TextWidth(const AText: String): Integer;
begin
  Result := TextExtent(AText).CY;
end;


end.

