unit Figures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ExtCtrls, Graphics, Dialogs, Math, CoordSystems,
  Forms, Controls;

type
  TPointDoubleList = array of TPointDouble;

  TFigure = class
  protected
    procedure DrawOutlineRectangle(apoints: TPointDoubleList; acanvas: TCanvas);
  public
  var
    color: TColor;
    Width: integer;
    points: TPointDoubleList;
    selected: boolean;

    procedure Draw(acanvas: TCanvas); virtual; abstract;
    procedure MouseMove(x, y: integer); virtual; abstract;
    procedure MouseUp(x, y: integer); virtual;
    constructor Create(x, y: integer); virtual;
  end;

  TPen = class(TFigure)
  public
    procedure Draw(acanvas: TCanvas); override;
    procedure MouseMove(x, y: integer); override;
  end;

  TLine = class(TFigure)
  public
    procedure Draw(acanvas: TCanvas); override;
    procedure MouseMove(x, y: integer); override;
  end;

  TRectangle = class(TFigure)
    procedure Draw(acanvas: TCanvas); override;
    procedure MouseMove(x, y: integer); override;
  end;

  TEllipse = class(TFigure)
  public
    procedure Draw(acanvas: TCanvas); override;
    procedure MouseMove(x, y: integer); override;
  end;

  TRoundrect = class(TFigure)
  public
  var
    rx, ry: integer;

    procedure Draw(acanvas: TCanvas); override;
    procedure MouseMove(x, y: integer); override;
  end;

  TFigureClass = class of TFigure;

procedure ChangeDrawEdges(x, y: integer);

var
  FigureList: array of TFigure;
  pmin, pmax: tpointdouble;

implementation

constructor TFigure.Create(x, y: integer);
begin
  selected := False;
  setlength(points, 2);
  points[0] := s2w(x, y);
  points[1] := points[0];
  ScreenCoord.x := x;
  ScreenCoord.y := y;
  WorldCoord.x := s2w(x, 0).x;
  WorldCoord.y := s2w(0, y).y;
end;

procedure TFigure.MouseUp(x, y: integer);
begin
end;

//PEN
procedure TPen.Draw(acanvas: TCanvas);
var
  i: tpointdouble;
begin
  if (selected) then
  begin
    acanvas.pen.Width := Width + 10;
    acanvas.pen.color := clRed;
    acanvas.Polyline(SetScreenCoords(Points));
    acanvas.pen.Width := Width;
  end;
  acanvas.pen.Width := 3;
  acanvas.pen.color := clblue;
  for i in points do
    acanvas.ellipse(w2s(i.x, 0).x - 1, w2s(i.x, i.y).y - 1, w2s(i.x, 0).x +
      1, w2s(i.x, i.y).y + 1);
  acanvas.pen.color := color;
  acanvas.pen.Width := Width;
  acanvas.Polyline(SetScreenCoords(Points));
  if (selected) then
    DrawOutlineRectangle(points, acanvas);
end;

procedure TPen.MouseMove(x, y: integer);
begin
  setlength(points, length(points) + 1);
  points[high(points)] := S2W(x, y);
end;
//PEN

//LINE
procedure TLine.Draw(acanvas: TCanvas);
begin
  acanvas.line(w2s(points[0]), w2s(points[1]));
  if (selected) then
    DrawOutlineRectangle(points, acanvas);
end;

procedure TLine.MouseMove(x, y: integer);
begin
  points[1] := s2w(x, y);
end;
//LINE

//RECTANGLE
procedure TRectangle.Draw(acanvas: TCanvas);
begin
  //SetScreenCoords(points);
  acanvas.rectangle(
    w2s(points[0]).x, w2s(points[0]).y, w2s(points[1]).x, w2s(points[1]).y);
  if (selected) then
    DrawOutlineRectangle(points, acanvas);
end;

procedure TRectangle.MouseMove(x, y: integer);
begin
  points[1] := S2W(x, y);
end;
//RECTANGLE

//EllIPSE
procedure TEllipse.Draw(acanvas: TCanvas);
begin
  SetScreenCoords(points);
  acanvas.ellipse(w2s(points[0]).x, w2s(points[0]).y, w2s(points[1]).x,
    w2s(points[1]).y);
  if (selected) then
    DrawOutlineRectangle(points, acanvas);
end;

procedure TEllipse.MouseMove(x, y: integer);
begin
  points[1] := S2W(x, y);
end;
//ELLIPSE

//ROUNDRECT
procedure TRoundrect.Draw(acanvas: TCanvas);
begin
  acanvas.roundrect(w2s(points[0]).x, w2s(points[0]).y, w2s(points[1]).x,
    w2s(points[1]).y, rx, ry);
  if (selected) then
    DrawOutlineRectangle(points, acanvas);
end;

procedure TRoundrect.MouseMove(x, y: integer);
begin
  points[1] := S2W(x, y);
end;
//ROUNDRECT

procedure ChangeDrawEdges(x, y: integer);
begin
  if (S2W(x, 0).x < pmin.x) then
    pmin.x := S2W(x, 0).x;
  if (S2W(x, 0).x > pmax.x) then
    pmax.x := S2W(x, 0).x;
  if (S2W(0, y).y < pmin.y) then
    pmin.y := S2W(0, y).y;
  if (S2W(0, y).y > pmax.y) then
    pmax.y := S2W(0, y).y;
end;

procedure TFigure.DrawOutlineRectangle(apoints: TPointDoubleList; acanvas: TCanvas);
var
  x1, y1, x2, y2: double;
  i: TPointDouble;
begin
  x1 := 999999;
  y1 := 999999;
  x2 := -999999;
  y2 := -999999;
  for i in aPoints do
  begin
    if i.x < x1 then
      x1 := i.x;
    if i.y < y1 then
      y1 := i.y;
    if i.x > x2 then
      x2 := i.x;
    if i.y > y2 then
      y2 := i.y;
  end;
  aCanvas.Pen.Mode := pmNot;
  aCanvas.Pen.Style := psDash;
  aCanvas.Pen.Width := 3;
  aCanvas.Brush.Style := BsClear;
  acanvas.rectangle(w2s(x1 - 2, 0).x, w2s(0, y1 - 2).y, w2s(x2 + 2, 0).x,
    w2s(0, y2 + 2).y);
  aCanvas.Brush.Style := BsSolid;
  aCanvas.Pen.Style := psSolid;
  aCanvas.Pen.Mode := pmCopy;
  aCanvas.Pen.Width := Width;
end;

end.
