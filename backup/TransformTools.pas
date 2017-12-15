unit TransformTools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CoordSystems, Graphics, Dialogs, Math, Figures, Parameters;

type
  TransformModeType = (select, editpoints, move, resize);

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
  private
  var
    Mode: TransformModeType;
    sPoints, sFigures: array of integer;
    deltax, deltay: double;
    PointModified: boolean;

    function SelectedPointsCount(): integer;
    function point_in_figure(ax, ay: double; aFigure: TFigure): boolean;
    procedure RemoveAllSelection(afigurelist: TFigureList);
  public
    procedure Draw(acanvas: TCanvas); override;
    procedure MouseMove(x, y: integer); override;
    procedure MouseUp(x, y: integer); override;
    constructor Create(x, y: integer); override;
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
  UpdateScale;
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

//ZOOM RECTANGLE begin
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
  UpdateScale;
  GlobalOffset.x -= (ScreenCoord.x - W2S(WorldCoord.x, 0).x) / GlobalScale;
  GlobalOffset.y -= (ScreenCoord.y - W2S(0, WorldCoord.y).y) / GlobalScale;
end;
//ZOOM RECTANGLE end

//SELECT begin

constructor TSelect.Create(x, y: integer);
var
  i, f: TFigure;
  j, l: integer;
  wx, wy: double;
begin
  PointModified := False;
  wx := s2w(x, 0).x;
  wy := s2w(0, y).y;
  mode := select;
  for i in figurelist do
  begin
    if (i.selected) then
    begin
      for j := 0 to high(i.points) do
        if point_in_rectangle(wx, wy, i.points[j].x - 15, i.points[j].y -
          15, i.points[j].x + 15, i.points[j].y + 15) then
        begin
          mode := editpoints;
          if ((not ShiftButtonPressed) and ((SelectedPointsCount <= 1) or
            (not i.sPoints[j]))) or (CtrlButtonPressed) then
          begin
            RemoveAllSelection(figurelist);
            i.sPoints[j] := True;
            setlength(i.sPointList, 1);
            i.sPointList[0] := j;
            PointModified := True;
            break;
          end
          else
          begin
            if (not i.sPoints[j]) then
              PointModified := True;
            i.sPoints[j] := True;
            setlength(i.sPointList, Length(i.sPointList) + 1);
            i.sPointList[high(i.sPointList)] := j;
          end;
        end;
    end
    else
    begin

    end;
  end;
  inherited Create(x, y);
  deltax := points[0].x;
  deltay := points[0].y;
end;

procedure TSelect.MouseMove(x, y: integer);
var
  i: TFigure;
  j: integer;
begin
  points[1] := S2W(x, y);
  if (mode = editpoints) then
  begin
    for i in figurelist do
    begin
      case i.ClassName of
        'TLine', 'TRectangle', 'TRoundrect', 'TEllipse', 'TPen':
        begin
          for j := 0 to high(i.spoints) do
          begin
            if i.spoints[j] then
            begin
              i.points[j].x += -deltax + points[1].x;
              i.points[j].y += -deltay + points[1].y;
            end;
          end;
        end;
      end;
    end;
    deltax := points[1].x;
    deltay := points[1].y;
  end;
  if (mode = select) then
  begin

  end;
end;

procedure TSelect.Draw(acanvas: tcanvas);
var
  w: integer;
  cl: tcolor;
begin
  if (mode = select) then
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
end;

procedure TSelect.MouseUp(x, y: integer);
var
  i: TFigure;
  j, l: integer;
  p1, p2: TPointDouble;
  wx, wy: double;
begin
  wx := s2w(x, 0).x;
  wy := s2w(0, y).y;
  mindistance := 32200;
  if (mode = select) then
    for i in figurelist do
    begin
      if (point_in_figure(wx,wy,i)) then
         i.selected:=!i.selected;
    end;
  if (mode = editpoints) then     //and (not (PointModified))
  begin
    if (points[0].x = points[1].x) and (points[0].y = points[1].y) then
    begin
      for i in figurelist do
      begin
        if (i.selected) then
        begin
          for j := 0 to high(i.points) do
            if point_in_rectangle(wx, wy, i.points[j].x - 15,
              i.points[j].y - 15, i.points[j].x + 15, i.points[j].y + 15) then
            begin
              if (i.sPoints[j] = True) and (ShiftButtonPressed) and
                (not (PointModified)) then
              begin
                ArrayElementDelete(i.sPointList, j);
                i.sPoints[j] := False;
              end
              else
              begin
                i.sPoints[j] := True;
                setlength(i.sPointList, Length(i.sPointList) + 1);
                i.sPointList[high(i.sPointList)] := j;
              end;
              break;
            end;
        end;
      end;
    end;
  end;
end;

procedure TSelect.RemoveAllSelection(afigurelist: TFigureList);
var
  f: TFigure;
  l: integer;
begin
  for f in afigurelist do
  begin
    if (length(f.sPointList) > 0) then
    begin
      for l := 0 to high(f.sPointList) do
      begin
        f.sPoints[f.sPointList[l]] := False;
      end;
      Setlength(f.sPointList, 0);
    end;
  end;
end;

function TSelect.SelectedPointsCount(): integer;
var
  _i: TFigure;
begin
  Result := 0;
  for _i in figurelist do
  begin
    Result += length(_i.sPointList);
  end;
end;

function TSelect.point_in_figure(ax, ay: double; aFigure: TFigure): boolean;
begin
  case aFigure.ClassName of
    'TLine':
      Result := (point_in_line(ax, ay, aFigure.points[0].x, aFigure.points[0].y,
        aFigure.points[1].x, aFigure.points[1].y, aFigure.Width));

    'TRectangle':
      Result := (point_in_rectangle(ax, ay, aFigure.points[0].x,
        aFigure.points[0].y, aFigure.points[1].x, aFigure.points[1].y))
  end;
end;

//SELECT end
end.
