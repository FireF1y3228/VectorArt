unit CoordSystems;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Math, Controls, Dialogs;

type
  TPointDouble = record
    x, y: real;
  end;

  TPointList = array of TPoint;

var
  GlobalScale: double;
  GlobalOffset: TPointDouble;
  CanvasWidth, CanvasHeight: integer;
  ScreenCoord: TPoint;
  WorldCoord: TPointDouble;

const
  MINSCALE = 0.01;
  MAXSCALE = 32;

function S2W(sx, sy: integer): TPointDouble;
function W2S(wx, wy: double): TPoint;
function S2W(p: TPoint): TPointDouble;
function W2S(p: TPointDouble): TPoint;
function PointDouble(X, Y: double): TPointDouble;
function SetScreenCoords(a: array of TPointDouble): TPointList;
function point_distance(x1, y1, x2, y2: double): double;
function point_in_rectangle(px, py, x1, y1, x2, y2: double): boolean;
function point_in_line(px, py, x1, y1, x2, y2: double; w: integer): boolean;

implementation
//TRANSFORM FUNCTIONS begin
function S2W(sx, sy: integer): TPointDouble;
begin
  Result.x := sx / GlobalScale + GlobalOffset.x;
  Result.y := sy / GlobalScale + GlobalOffset.y;
end;

function W2S(wx, wy: double): TPoint;
begin
  Result.x := round(wx * GlobalScale - GlobalOffset.x * GlobalScale);
  Result.y := round(wy * GlobalScale - GlobalOffset.y * GlobalScale);
end;

function S2W(p: TPoint): TPointDouble;
begin
  Result.x := p.x / GlobalScale + GlobalOffset.x;
  Result.y := p.y / GlobalScale + GlobalOffset.y;
end;

function W2S(p: TPointDouble): TPoint;
begin
  Result.x := round(p.x * GlobalScale - GlobalOffset.x * GlobalScale);
  Result.y := round(p.y * GlobalScale - GlobalOffset.y * GlobalScale);
end;

function PointDouble(X, Y: double): TPointDouble;
begin
  Result.x := x;
  Result.y := y;
end;

function SetScreenCoords(a: array of TPointDouble): TPointList;
var
  i: integer;
begin
  setlength(Result, 0);
  for i := 0 to length(a) - 1 do
  begin
    setlength(Result, length(Result) + 1);
    Result[i] := Point(w2s(a[i]).x, w2s(a[i]).y);
  end;
end;

function point_distance(x1, y1, x2, y2: double): double;
begin
  Result := sqrt((x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2));
end;

function point_in_rectangle(px, py, x1, y1, x2, y2: double): boolean;
var
  _x, _y: double;
begin
  if (x1 > x2) then
  begin
    _x := x1;
    x1 := x2;
    x2 := _x;
  end;
  if (y1 > y2) then
  begin
    _y := y1;
    y1 := y2;
    y2 := _y;
  end;
  if (px <= x2) and (px >= x1) and (py <= y2) and (py >= y1) then
    Result := True
  else
    Result := False;
end;

function point_in_line(px, py, x1, y1, x2, y2: double; w: integer): boolean;
var
  t, d2, d1, ap, a2, bp: double;
begin
  a2 := (py - y1) * (x2 - x1) - (px - x1) * (y2 - y1);
  ap := min(point_distance(px, py, x1, y1), point_distance(px, py, x2, y2));
  bp := max(point_distance(px, py, x1, y1), point_distance(px, py, x2, y2));
  d1 := sqrt((a2 * a2) / ((x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)));
  t := (px - x1) * (x2 - x1) + (py - y1) * (y2 - y1);
  if (t < 0) then
    d2 := ap
  else
  begin
    t := (x2 - px) * (x2 - x1) + (y2 - py) * (y2 - y1);
    if (t < 0) then
      d2 := bp
    else
      d2 := d1;
  end;
  Result := boolean(d2 < w + 5);
end;

//function point_in_ellipse
//TRANSFORM FUNCTIONS end
end.
