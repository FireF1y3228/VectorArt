unit TransformTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CoordSystems, Graphics, Dialogs, Math, Figures;

type
  TTransformTool = class
  public
  var
    points: array of TPointDouble;
    selected: boolean;

    procedure Draw(acanvas: TCanvas); virtual; abstract;
    procedure MouseMove(x, y: integer); virtual; abstract;
    procedure MouseUp(x, y: integer); virtual;
    constructor Create(x, y: integer); virtual;
  end;

  THand = class(TTransformTool)
  public
    procedure Draw(acanvas: TCanvas); override;
    procedure MouseMove(x, y: integer); override;
    procedure MouseUp(x, y: integer); override;
  end;

  TZoom = class(TTransformTool)
  public
    procedure Draw(acanvas: TCanvas); override;
    procedure MouseMove(x, y: integer); override;
    procedure MouseUp(x, y: integer); override;
  end;

  TZoomRect = class(TTransformTool)
  public
    procedure Draw(acanvas: TCanvas); override;
    procedure MouseMove(x, y: integer); override;
    procedure MouseUp(x, y: integer); override;
  end;

  TSelect = class(TTransformTool)
  public
    procedure Draw(acanvas: TCanvas); override;
    procedure MouseMove(x, y: integer); override;
    procedure MouseUp(x, y: integer); override;
  end;

  TTransformToolClass = class of TTransformTool;

var
  CurrentTTool: TTransformTool;
  TransformToolList: array of TTransformTool;
  mindistance: double;

implementation

constructor TTransformTool.Create(x, y: integer);
begin
  selected := False;
  setlength(points, 2);
  points[0] := s2w(x, y);
  points[1] := points[0];
  ScreenCoord.x := x;
  ScreenCoord.y := y;
  WorldCoord.x := s2w(x, 0).x;
  WorldCoord.y := s2w(0, y).y;
  currentttool := self;
end;

procedure TTransformTool.MouseUp(x, y: integer);
begin
end;

//HAND BEGIN
procedure THand.MouseMove(x, y: integer);
begin
  points[1] := S2W(x, y);
  GlobalOffset.x := GlobalOffset.x + points[0].x - points[1].x;
  GlobalOffset.y += points[0].y - points[1].y;
end;

procedure THand.Draw(acanvas: tcanvas);
begin
end;

procedure THand.MouseUp(x, y: integer);
begin
end;
//HAND END

//ZOOM BEGIN
procedure TZoom.MouseMove(x, y: integer);
var
  px0, px1: integer;
begin
  px0 := W2S(points[0]).x;
  px1 := x;
  points[1] := S2W(x, y);
  GlobalScale := min(MAXSCALE, max(MINSCALE, GlobalScale - (px0 - px1) / 200));
  GlobalOffset.x -= (ScreenCoord.x - W2S(WorldCoord.x, 0).x) / GlobalScale;
  GlobalOffset.y -= (ScreenCoord.y - W2S(0, WorldCoord.y).y) / GlobalScale;
  points[0] := S2W(x, y);
end;

procedure TZoom.Draw(acanvas: tcanvas);
begin
end;

procedure TZoom.MouseUp(x, y: integer);
begin
  GlobalOffset.x -= (ScreenCoord.x - W2S(WorldCoord.x, 0).x) / GlobalScale;
  GlobalOffset.y -= (ScreenCoord.y - W2S(0, WorldCoord.y).y) / GlobalScale;
end;
//ZOOM END

//ZOOM RECTANGLE BEGIN
procedure TZoomRect.Draw(acanvas: TCanvas);
var
  w: integer;
begin
  SetScreenCoords(points);
  w := acanvas.pen.Width;
  aCanvas.Pen.Style := psDot;
  aCanvas.Brush.Style := bsClear;
  aCanvas.Pen.Mode := pmNot;
  aCanvas.Pen.Width := 2;
  acanvas.rectangle(w2s(points[0]).x, w2s(points[0]).y, w2s(points[1]).x,
    w2s(points[1]).y);
  aCanvas.Pen.Style := psSolid;
  aCanvas.Brush.Style := bsSolid;
  aCanvas.Pen.Mode := pmCopy;
  acanvas.pen.Width := w;
end;

procedure TZoomRect.MouseMove(x, y: integer);
begin
  points[1] := S2W(x, y);
  ScreenCoord.x := W2S(points[1].x, 0).x + (W2S(points[0].x, 0).x -
    W2S(points[1].x, 0).x) div 2;
  WorldCoord.x := S2W(ScreenCoord.x, 0).x;
  ScreenCoord.y := W2S(0, points[1].y).y + (W2S(0, points[0].y).y -
    W2S(0, points[1].y).y) div 2;
  WorldCoord.y := S2W(0, ScreenCoord.y).y;
end;

procedure TZoomRect.MouseUp(x, y: integer);
begin
  GlobalScale :=
    min(max((CanvasWidth / abs(points[0].x - points[1].x)), MINSCALE), MAXSCALE);
  GlobalOffset.x -= (ScreenCoord.x - W2S(WorldCoord.x, 0).x) / GlobalScale;
  GlobalOffset.y -= (ScreenCoord.y - W2S(0, WorldCoord.y).y) / GlobalScale;
end;
//ZOOM RECTANGLE END

//SELECT begin
procedure TSelect.MouseMove(x, y: integer);
begin
  points[1] := S2W(x, y);
end;

procedure TSelect.Draw(acanvas: tcanvas);
var
  w: integer;
  cl: tcolor;
begin
  SetScreenCoords(points);
  w := acanvas.pen.Width;
  aCanvas.Pen.Style := psDash;
  aCanvas.Brush.Style := bsClear;
  aCanvas.Pen.Mode := pmCopy;
  cl := aCanvas.Pen.Color;
  aCanvas.Pen.Color := clWhite;
  aCanvas.Pen.Width := 3;
  acanvas.rectangle(w2s(points[0]).x, w2s(points[0]).y, w2s(points[1]).x,
    w2s(points[1]).y);
  aCanvas.Pen.Mode := pmNot;
  aCanvas.Pen.Width := 1;
  acanvas.rectangle(w2s(points[0]).x, w2s(points[0]).y, w2s(points[1]).x,
    w2s(points[1]).y);
  aCanvas.Pen.Color := cl;
  aCanvas.Pen.Style := psSolid;
  aCanvas.Brush.Style := bsSolid;
  aCanvas.Pen.Mode := pmCopy;
  acanvas.pen.Width := w;
end;

procedure TSelect.MouseUp(x, y: integer);
var
  i: TFigure;
  j: integer;
  p1, p2: TPointDouble;
  wx, wy: double;
begin
  wx := s2w(x, 0).x;
  wy := s2w(0, y).y;
  mindistance := 32200;
  for i in figurelist do
  begin
    case i.ClassName of
      'TLine':
      begin
        if (point_in_line(wx, wy, i.points[0].x, i.points[0].y,
          i.points[1].x, i.points[1].y, i.Width)) then
        begin
          i.selected := not i.selected;
        end;
      end;
      'TRectangle':
      begin
        if (point_in_rectangle(wx, wy, i.points[0].x, i.points[0].y,
          i.points[1].x, i.points[1].y)) then
        begin
          i.selected := not i.selected;
        end;
      end;
    end;
  end;
end;
//SELECT end
end.
