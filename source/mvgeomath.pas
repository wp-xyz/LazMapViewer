unit mvGeoMath;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math;

const
  EARTH_EQUATORIAL_RADIUS = 6378137;
  EARTH_POLAR_RADIUS = 6356752.3142;
  EARTH_CIRCUMFERENCE = 2 * pi * EARTH_EQUATORIAL_RADIUS;
  EARTH_ECCENTRICITY = sqrt(1 - sqr(EARTH_POLAR_RADIUS / EARTH_EQUATORIAL_RADIUS));
  EARTH_FLATTENING = 1.0 - EARTH_POLAR_RADIUS / EARTH_EQUATORIAL_RADIUS;

  DMS_Decimals: Integer = 1;

type
  TDistanceUnits = (duMeters, duKilometers, duMiles);
  TEarthShape = (esSphere, esEllipsoid);

function NormalizeLon(const Lon: Double): Double; inline;

function HaversineDist(Lat1, Lon1, Lat2, Lon2, Radius: Double): Double;
function LambertsDist(Lat1, Lon1, Lat2, Lon2, Radius, Flattening: Double): Double;
function CalcGeoDistance(Lat1, Lon1, Lat2, Lon2: double;
  AUnits: TDistanceUnits = duKilometers; AShape: TEarthShape = esSphere): double;

function CalcBearing(Lat1, Lon1, Lat2, Lon2: Double): Double;
procedure CalcLatLon(const Lat1, Lon1, ADist, ABearing: Double; out Lat2, Lon2: Double);
procedure CalcMidpoint(const Lat1, Lon1, Lat2, Lon2: Double; out Lat, Lon: Double);
procedure CalcIntermedPoint(const Lat1, Lon1, Lat2, Lon2, AFrac: Double; out Lat, Lon: Double);

function DMSToDeg(Deg, Min: Word; Sec: Double): Double;
function GPSToDMS(Angle: Double): string;
function GPSToDMS(Angle: Double; AFormatSettings: TFormatSettings): string;

function LatToStr(ALatitude: Double; DMS: Boolean): String;
function LatToStr(ALatitude: Double; DMS: Boolean; AFormatSettings: TFormatSettings): String;
function LonToStr(ALongitude: Double; DMS: Boolean): String;
function LonToStr(ALongitude: Double; DMS: Boolean; AFormatSettings: TFormatSettings): String;
function TryStrToGps(const AValue: String; out ADeg: Double): Boolean;
function TryStrDMSToDeg(const AValue: String; out ADeg: Double): Boolean;

procedure SplitGps(AValue: Double; out ADegs, AMins, ASecs: Double);

function ZoomFactor(AZoomLevel: Integer): Int64;


implementation

const
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

{ Calculation of distance on a sphere
   https://stackoverflow.com/questions/73608975/pascal-delphi-11-formula-for-distance-in-meters-between-two-decimal-gps-point
}
// angles in radians
function HaversineAngle(Lat1, Lon1, Lat2, Lon2: Double): Double;
var
  latFrom, latTo, lonDiff: Double;
  dx, dy, dz, arg: Double;
begin
  lonDiff := Lon1 - Lon2;
  latFrom := Lat1;
  latTo := Lat2;

  dz := sin(latFrom) - sin(latTo);
  dx := cos(lonDiff) * cos(latFrom) - cos(latTo);
  dy := sin(lonDiff) * cos(latFrom);

  arg := sqrt(sqr(dx) + sqr(dy) + sqr(dz)) / 2.0;
  if arg > 1.0 then
    Result := pi
  else
  if arg < -1.0 then
    Result := -pi
  else
    Result := arcsin(arg) * 2.0;
end;

// Angles in degrees
function HaversineDist(Lat1, Lon1, Lat2, Lon2, Radius: Double): Double;
begin
  Result := HaversineAngle(DegToRad(Lat1), DegToRad(Lon1), DegToRad(Lat2), DegToRad(Lon2)) * Radius;
end;

{ Calculation of distance on an ellipsoid
  https://en.wikipedia.org/wiki/Geographical_distance
}
// Angles in degrees
function LambertsDist(Lat1, Lon1, Lat2, Lon2, Radius, Flattening: Double): Double;
var
  reducedLat1, reducedLat2, sigma: Double;
  P, Q: Double;
  X, Y: Double;
  sinP, cosP, sinQ, cosQ: Double;
  sinSigma, sinSigma2, cosSigma2: Double;
begin
  Lat1 := DegToRad(Lat1);
  Lon1 := DegToRad(Lon1);
  Lat2 := DegToRad(Lat2);
  Lon2 := DegToRad(Lon2);
  reducedLat1 := arctan((1.0 - flattening) * tan(Lat1));
  reducedLat2 := arctan((1.0 - flattening) * tan(Lat2));
  sigma := HaversineAngle(reducedLat1, Lon1, reducedLat2, Lon2);
  P := (reducedLat1 + reducedLat2) * 0.5;
  Q := (reducedLat2 - reducedLat1) * 0.5;
  SinCos(P, sinP, cosP);
  SinCos(Q, sinQ, cosQ);
  SinCos(sigma*0.5, sinSigma2, cosSigma2);
  sinSigma := sin(sigma);
  X := (sigma - sinSigma) * sqr(sinP * cosQ / cosSigma2);
  Y := (sigma + sinSigma) * sqr(cosP * sinQ / sinSigma2);
  Result := (sigma - 0.5*Flattening*(X + Y)) * Radius;
end;


{ Returns the direct distance (air-line) between two geo coordinates
 If latitude is NOT between -90°..+90° and longitude is NOT between -180°..+180°
 the function returns NaN.

 Usage example:
   CalcGeoDistance(51.53323, -2.90130, 51.29442, -2.27275, duKilometers, esEllipsoid);
}
function CalcGeoDistance(Lat1, Lon1, Lat2, Lon2: double;
  AUnits: TDistanceUnits = duKilometers; AShape: TEarthShape = esSphere): double;
begin
  // Validate
  if (Lat1 < -90.0) or (Lat1 > 90.0) then exit(NaN);
  if (Lat2 < -90.0) or (Lat2 > 90.0) then exit(NaN);

  case AShape of
    esSphere:
      Result := HaversineDist(Lat1, Lon1, Lat2, Lon2, EARTH_EQUATORIAL_RADIUS);
    esEllipsoid:
      Result := LambertsDist(Lat1, Lon1, Lat2, Lon2, EARTH_EQUATORIAL_RADIUS, EARTH_FLATTENING);
  end;

  case AUnits of
    duMeters: ;
    duKilometers: Result := Result * 0.001;
    duMiles: Result := Result * 0.62137E-3;
  end;
end;

{ Calculate initial bearing (in °) from the start point Lat1,Lon1 to
  the end point Lat2,Lon2. No parameter checks, result normalized to 0°..360°.
}
function CalcBearing(Lat1, Lon1, Lat2, Lon2: Double): Double;
var
  latFrom, latTo, lonDiff: Double;
begin
  lonDiff := DegToRad(Lon2 - Lon1);
  latFrom := DegToRad(Lat1);
  latTo := DegToRad(Lat2);
  Result := ArcTan2(Sin(lonDiff) * Cos(latTo),
    Cos(latFrom) * Sin(latTo) - Sin(latFrom) * Cos(latTo) * Cos(lonDiff));
  Result := RadToDeg(Result);
  if Result < 0.0 then
    Result := Result + 360.0;
end;

{ Calculate end point Lat2,Lon2 by given start point Lat1,Lon1, distance
  ADist (in meters) and bearing ABearing (in °). No parameter checks,
  result Lon2 normalized to -180°..180°.
}
procedure CalcLatLon(const Lat1, Lon1, ADist, ABearing: Double; out Lat2,
  Lon2: Double);
var
  latFrom, lonFrom, brng, aD: Double;
begin
  latFrom := DegToRad(Lat1);
  lonFrom := DegToRad(Lon1);
  brng := DegToRad(ABearing);
  aD := ADist / EARTH_EQUATORIAL_RADIUS;
  Lat2 := ArcSin(Sin(latFrom) * Cos(aD) + Cos(latFrom) * Sin(aD) * Cos(brng));
  Lon2 := lonFrom + ArcTan2(Sin(brng) * Sin(aD) * Cos(latFrom),
    Cos(aD) - Sin(latFrom) * Sin(Lat2));
  Lat2 := RadToDeg(Lat2);
  Lon2 := NormalizeLon(RadToDeg(Lon2));
end;

{ Calculate midpoint Lat,Lon by given start point Lat1,Lon1 and end point
  Lat2,Lon2. No parameter checks, result Lon normalized to -180°..180°.
}
procedure CalcMidpoint(const Lat1, Lon1, Lat2, Lon2: Double; out Lat,
  Lon: Double);
var
  latFrom, lonDiff, latTo, lonTo, Bx, By: Double;
begin
  lonDiff := DegToRad(Lon2 - Lon1);
  latFrom := DegToRad(Lat1);
  latTo := DegToRad(Lat2);
  lonTo := DegToRad(Lon2);
  Bx := Cos(latTo) * Cos(lonDiff);
  By := Cos(latTo) * Sin(lonDiff);
  Lat := ArcTan2(Sin(latFrom) + Sin(latTo), Sqrt(Sqr(Cos(latFrom) + Bx) + Sqr(By)));
  Lon := lonTo + ArcTan2(By, Cos(latFrom) + By);
  Lat := RadToDeg(Lat);
  Lon := NormalizeLon(RadToDeg(Lon));
end;

{ Calculate intermediate point Lat,Lon by given start point Lat1,Lon1, end point
  Lat2,Lon2 and fraction AFrac (0.0-1.0). No parameter checks for
  Lat1,Lon1,Lat2 and Lon2. Result Lon normalized to -180°..180°.
}
procedure CalcIntermedPoint(const Lat1, Lon1, Lat2, Lon2, AFrac: Double; out
  Lat, Lon: Double);
var
  latFrom, lonFrom, latTo, lonTo: Double;
  A, B, aD, X, Y, Z: Double;
begin
  if (Lat1 = Lat2) and (Lon1 = Lon2) or (AFrac < 0.001) then
  begin
    Lat := Lat1;
    Lon := Lon1;
    Exit;
  end;
  if AFrac > 0.999 then
  begin
    Lat := Lat2;
    Lon := Lon2;
    Exit;
  end;
  latFrom := DegToRad(Lat1);
  lonFrom := DegToRad(Lon1);
  latTo := DegToRad(Lat2);
  lonTo := DegToRad(Lon2);
  aD := CalcGeoDistance(Lat1, Lon1, Lat2, Lon2, duMeters) / EARTH_EQUATORIAL_RADIUS;
  A := Sin((1.0 - AFrac) * aD) / Sin(aD);
  B := Sin(AFrac * aD) / Sin(aD);
  X := A * Cos(latFrom) * Cos(lonFrom) + B * Cos(latTo) * Cos(lonTo);
  Y := A * Cos(latFrom) * Sin(lonFrom) + B * Cos(latTo) * Sin(lonTo);
  Z := A * Sin(latFrom) + B * Sin(latTo);
  Lat := ArcTan2(Z, Sqrt(Sqr(X) + Sqr(Y)));
  Lon := ArcTan2(Y, X);
  Lat := RadToDeg(Lat);
  Lon := NormalizeLon(RadToDeg(Lon));
end;

function NormalizeLon(const Lon: Double): Double;
begin
  if InRange(Lon, -180.0, 180.0)
    then Result := Lon
    else Result := FMod(Lon + 540.0, 360.0) - 180.0;
end;

{ Converts an angle given as degrees, minutes and seconds to a single
  floating point degrees value. }
function DMSToDeg(Deg, Min: Word; Sec: Double): Double;
begin
  Result := Deg + Min/60.0 + Sec/3600.0;
end;

procedure SplitGps(AValue: Double; out ADegs, AMins: Double);
begin
  AValue := abs(AValue);
  AMins := frac(AValue) * 60;
  if abs(AMins - 60) < 1E-3 then
  begin
    AMins := 0;
    ADegs := trunc(AValue) + 1;
  end else
    ADegs := trunc(AValue);
  if AValue < 0 then
    ADegs := -ADegs;
end;

procedure SplitGps(AValue: Double; out ADegs, AMins, ASecs: Double);
begin
  SplitGps(AValue, ADegs, AMins);
  ASecs := frac(AMins) * 60;
  AMins := trunc(AMins);
  if abs(ASecs - 60) < 1E-3 then
  begin
    ASecs := 0;
    AMins := AMins + 1;
    if abs(AMins - 60) < 1e-3 then
    begin
      AMins := 0;
      ADegs := ADegs + 1;
    end;
  end;
  if AValue < 0 then
    ADegs := -ADegs;
end;

function GPSToDMS(Angle: Double): string;
begin
  Result := GPSToDMS(Angle, DefaultFormatSettings);
end;

function GPSToDMS(Angle: Double; AFormatSettings: TFormatSettings): string;
var
  deg, min, sec: Double;
begin
  SplitGPS(Angle, deg, min, sec);
  Result := Format('%.0f° %.0f'' %.*f"', [deg, min, DMS_Decimals, sec], AFormatSettings);
end;

function LatToStr(ALatitude: Double; DMS: Boolean): String;
begin
  Result := LatToStr(ALatitude, DMS, DefaultFormatSettings);
end;

function LatToStr(ALatitude: Double; DMS: Boolean; AFormatSettings: TFormatSettings): String;
begin
  if DMS then
    Result := GPSToDMS(abs(ALatitude), AFormatSettings)
  else
    Result := Format('%.6f°',[abs(ALatitude)], AFormatSettings);
  if ALatitude > 0 then
    Result := Result + ' N'
  else
  if ALatitude < 0 then
    Result := Result + ' S';
end;

function LonToStr(ALongitude: Double; DMS: Boolean): String;
begin
  Result := LonToStr(ALongitude, DMS, DefaultFormatSettings);
end;

function LonToStr(ALongitude: Double; DMS: Boolean; AFormatSettings: TFormatSettings): String;
begin
  if DMS then
    Result := GPSToDMS(abs(ALongitude), AFormatSettings)
  else
    Result := Format('%.6f°', [abs(ALongitude)], AFormatSettings);
  if ALongitude > 0 then
    Result := Result + ' E'
  else if ALongitude < 0 then
    Result := Result + ' W';
end;

{ Combines up to three parts of a GPS coordinate string (degrees, minutes, seconds)
  to a floating-point degree value. The parts are separated by non-numeric
  characters:

  three parts ---> d m s ---> d and m must be integer, s can be float
  two parts   ---> d m   ---> d must be integer, s can be float
  one part    ---> d     ---> d can be float

  Each part can exhibit a unit identifier, such as °, ', or ". BUT: they are
  ignored. This means that an input string 50°30" results in the output value 50.5
  although the second part is marked as seconds, not minutes!

  Hemisphere suffixes ('N', 'S', 'E', 'W') are supported at the end of the input string.
}
function TryStrToGps(const AValue: String; out ADeg: Double): Boolean;
const
  NUMERIC_CHARS = ['0'..'9', '.', ',', '-', '+'];
var
  mins, secs: Double;
  i, j, len: Integer;
  n: Integer;
  s: String = '';
  res: Integer;
  sgn: Double;
begin
  Result := false;

  ADeg := NaN;
  mins := 0;
  secs := 0;

  if AValue = '' then
    exit;

  len := Length(AValue);
  i := len;
  while (i >= 1) and (AValue[i] = ' ') do dec(i);
  sgn := 1.0;
  if (AValue[i] in ['S', 's', 'W', 'w']) then sgn := -1;

  // skip leading non-numeric characters
  i := 1;
  while (i <= len) and not (AValue[i] in NUMERIC_CHARS) do
    inc(i);

  // extract first value: degrees
  SetLength(s, len);
  j := 1;
  n := 0;
  while (i <= len) and (AValue[i] in NUMERIC_CHARS) do begin
    if AValue[i] = ',' then s[j] := '.' else s[j] := AValue[i];
    inc(i);
    inc(j);
    inc(n);
  end;
  if n > 0 then begin
    SetLength(s, n);
    val(s, ADeg, res);
    if res <> 0 then
      exit;
  end;

  // skip non-numeric characters between degrees and minutes
  while (i <= len) and not (AValue[i] in NUMERIC_CHARS) do
    inc(i);

  // extract second value: minutes
  SetLength(s, len);
  j := 1;
  n := 0;
  while (i <= len) and (AValue[i] in NUMERIC_CHARS) do begin
    if AValue[i] = ',' then s[j] := '.' else s[j] := AValue[i];
    inc(i);
    inc(j);
    inc(n);
  end;
  if n > 0 then begin
    SetLength(s, n);
    val(s, mins, res);
    if (res <> 0) or (mins < 0) then
      exit;
  end;

  // skip non-numeric characters between minutes and seconds
  while (i <= len) and not (AValue[i] in NUMERIC_CHARS) do
    inc(i);

  // extract third value: seconds
  SetLength(s, len);
  j := 1;
  n := 0;
  while (i <= len) and (AValue[i] in NUMERIC_CHARS) do begin
    if AValue[i] = ',' then s[j] := '.' else s[j] := AValue[i];
    inc(i);
    inc(j);
    inc(n);
  end;
  if n > 0 then begin
    SetLength(s, n);
    val(s, secs, res);
    if (res <> 0) or (secs < 0) then
      exit;
  end;

  // If the string contains seconds then minutes and deegrees must be integers
  if (secs <> 0) and ((frac(ADeg) > 0) or (frac(mins) > 0)) then
    exit;
  // If the string does not contain seconds then degrees must be integer.
  if (secs = 0) and (mins <> 0) and (frac(ADeg) > 0) then
    exit;

  // If the string contains minutes, but no seconds, then the degrees must be integer.
  Result := (mins >= 0) and (mins < 60) and (secs >= 0) and (secs < 60);

  // A similar check should be made for the degrees range, but since this is
  // different for latitude and longitude the check is skipped here.
  if Result then
    ADeg := sgn * (abs(ADeg) + mins / 60 + secs / 3600);
end;

{ Convert Lat/Lon string to degrees.

  Recognized formats:

    Degrees, Minutes and Seconds:
      DDD°MM'SS.S"
      32°18'23.1"N 122°36'52.5"W

    Degrees and Decimal Minutes:
      DDD° MM.MMM'
      32°18.385'N 122°36.875'W

    Decimal Degrees:
      DDD.DDDDD°
      32.30642°N 122.61458°W
      +32.30642 -122.61458
}
function TryStrDMSToDeg(const AValue: String; out ADeg: Double): Boolean;
const
  NUMERIC_CHARS = ['0'..'9', '.', ',', '-', '+'];
  WS_CHARS = [' '];
var
  I, Len, N: Integer;
  S: String;
  D, Minutes, Seconds: Double;
  R: Word;
  Last, Neg: Boolean;

  function EOL: Boolean; inline;
  begin
    Result := Len < I;
  end;

  procedure SkipWS; inline;
  begin
    while not EOL and (AValue[I] in WS_CHARS) do
      Inc(I);
  end;

  function NextNum: String;
  begin
    Result := '';
    SkipWS;
    while not EOL and (AValue[I] in NUMERIC_CHARS) do
    begin
      if AValue[I] = ','
        then Result := Result + '.'
        else Result := Result + AValue[I];
      Inc(I);
    end;
  end;

begin
  Result := False;
  ADeg := NaN;
  Len := Length(AValue);
  I := 1;

  // Degrees
  S := NextNum;
  if S = '' then
    Exit;
  Val(S, ADeg, R);
  if R > 0 then
    Exit;

  // It must be the only part if negative or fractional
  Neg := ADeg < 0.0;
  Last := Neg or (Frac(ADeg) > 0.0);

  // Eat the degree symbol if present
  if not EOL and (Utf8CodePointLen(@AValue[I], 2, False) = 2)
    and (AValue[I] = #$c2) and (AValue[I + 1] = #$b0)
  then
    Inc(I, 2);

  Minutes := 0.0;
  Seconds := 0.0;

  N := 1;
  while not (EOL or Last) and (N < 3) do
  begin
    S := NextNum;
    if S = '' then
      Break
    else
    begin
      Val(S, D, R);
      // Invalid part or negative one
      if (R > 0) or (D < 0.0) then
        Exit;
      // No more parts when fractional
      Last := Frac(D) > 0.0;
      if not EOL then
        case AValue[I] of
          '''': // Munutes suffix
            begin
              Minutes := D;
              Inc(I); // Eat the '
            end;
          '"': // Seconds suffix
            begin
              Seconds := D;
              Last := True; // Last part
              Inc(I); // Eat the "
            end;
          otherwise
            if N = 1
              then Minutes := D
              else Seconds := D;
        end;
    end;
    Inc(N);
  end;

  // Merge parts
  ADeg := ADeg + Minutes / 60 + Seconds / 3600;

  // Check for N-S and E-W designators
  SkipWS;
  if not (EOL or Neg) and (AValue[I] in ['S', 's', 'W', 'w', 'N', 'n', 'E', 'e']) then
  begin
    if AValue[I] in ['S', 's', 'W', 'w']
      then ADeg := -1 * ADeg;
    Inc(I);
  end;
  SkipWS;

  // It must be entirely consumed
  Result := EOL;
end;


end.

