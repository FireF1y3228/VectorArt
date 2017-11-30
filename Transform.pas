unit CoordSystems;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  TPointDouble = record
    x, y: real;
  end;

  TPointList = array of TPoint;
//git
var
  GlobalScale: double;
  GlobalOffset: TPointDouble;
  CanvasWidth, CanvasHeight: integer;

const
  MINSCALE = 0.01;
  MAXSCALE = 32;

function S2W(sx, sy: integer): TPointDouble;
function W2S(wx, wy: double): TPoint;
function S2W(p: TPoint): TPointDouble;
function W2S(p: TPointDouble): TPoint;
function PointDouble(X, Y: double): TPointDouble;
function SetScreenCoords(a: array of TPointDouble): TPointList;

implementation
//TRANSFORM FUNCTIONS
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
//TRANSFORM FUNCTIONS
end.
