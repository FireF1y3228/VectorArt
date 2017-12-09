unit Tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, Math, CoordSystems, Parameters,
  Figures, TransformTools;

type
  GlobalModeType = (draw, transform);

  TTool = class
    procedure MouseDown(x, y: integer); virtual; abstract;
    constructor Create; virtual; abstract;
  end;

  TPenTool = class(TTool)
  private
  const
    params: array [0..0] of string = ('width');
  public
    constructor Create; override;
    procedure MouseDown(x, y: integer); override;
  end;

  TLineTool = class(TTool)
  private
  const
    params: array [0..0] of string = ('width');
  public
    constructor Create; override;
    procedure MouseDown(x, y: integer); override;
  end;

  TRectangleTool = class(TTool)
  private
  const
    params: array [0..0] of string = ('width');
  public
    constructor Create; override;
    procedure MouseDown(x, y: integer); override;
  end;

  TEllipseTool = class(TTool)
  private
  const
    params: array [0..0] of string = ('width');
  public
    constructor Create; override;
    procedure MouseDown(x, y: integer); override;
  end;

  TRoundrectTool = class(TTool)
  private
  const
    params: array [0..2] of string = ('width', 'radiusx', 'radiusy');
  public
    constructor Create; override;
    procedure MouseDown(x, y: integer); override;
  end;

  THandTool = class(TTool)
  private
  const
    params: array [0..0] of string = ('');
  public
    constructor Create; override;
    procedure MouseDown(x, y: integer); override;
  end;

  TZoomTool = class(TTool)
  private
  const
    params: array [0..0] of string = ('');
  public
    constructor Create; override;
    procedure MouseDown(x, y: integer); override;
  end;

  TZoomRectangleTool = class(TTool)
  private
  const
    params: array [0..0] of string = ('');
  public
    constructor Create; override;
    procedure MouseDown(x, y: integer); override;
  end;

  TSelectTool = class(TTool)
  private
  const
    params: array [0..0] of string = ('delete');
  public
    constructor Create; override;
    procedure MouseDown(x, y: integer); override;
  end;

  TToolClass = class of TTool;

var

  globalmode: globalmodetype;

const
  TToolClassList: array [0..8] of TToolClass =
    (TPenTool, TLineTool, TRectangleTool, TEllipseTool, TRoundrectTool,
    THandTool, TZoomTool, TZoomRectangleTool, TSelectTool);

implementation

constructor TPenTool.Create();
begin
  GlobalMode := Draw;
  (PCObj as ParameterCreator).CreateParameters(params);
end;

procedure TPenTool.MouseDown(x, y: integer);
begin
  setlength(FigureList, length(FigureList) + 1);
  FigureList[High(FigureList)] := TPen.Create(x, y);
  with FigureList[High(FigureList)] do
  begin
    Color := GlobalColor[0];
    Width := round(GlobalWidth);
  end;
end;

constructor TLineTool.Create();
begin
  GlobalMode := Draw;
  (PCObj as ParameterCreator).CreateParameters(params);
end;

procedure TLineTool.MouseDown(x, y: integer);
begin

  setlength(FigureList, length(FigureList) + 1);
  FigureList[High(FigureList)] := TLine.Create(x, y);
  with FigureList[High(FigureList)] do
  begin
    Color := GlobalColor[0];
    Width := round(GlobalWidth);
  end;
end;

constructor TRectangleTool.Create();
begin
  GlobalMode := Draw;
  (PCObj as ParameterCreator).CreateParameters(params);
end;

procedure TRectangleTool.MouseDown(x, y: integer);
begin
  setlength(FigureList, length(FigureList) + 1);
  FigureList[High(FigureList)] := TRectangle.Create(x, y);
  with FigureList[High(FigureList)] do
  begin
    Color := GlobalColor[0];
    Width := round(GlobalWidth);
  end;
end;

constructor TEllipseTool.Create();
begin
  GlobalMode := Draw;
  (PCObj as ParameterCreator).CreateParameters(params);
end;

procedure TEllipseTool.MouseDown(x, y: integer);
begin
  setlength(FigureList, length(FigureList) + 1);
  FigureList[High(FigureList)] := TEllipse.Create(x, y);
  with FigureList[High(FigureList)] do
  begin
    Color := GlobalColor[0];
    Width := round(GlobalWidth);
  end;
end;

constructor TRoundrectTool.Create();
begin
  GlobalMode := Draw;
  (PCObj as ParameterCreator).CreateParameters(params);
end;

procedure TRoundrectTool.MouseDown(x, y: integer);
begin
  setlength(FigureList, length(FigureList) + 1);
  FigureList[High(FigureList)] := TRoundrect.Create(x, y);
  with (FigureList[High(FigureList)] as TRoundrect) do
  begin
    Color := GlobalColor[0];
    Width := round(GlobalWidth);
    Rx := round(GlobalRadx);
    Ry := round(GlobalRady);
  end;
end;

constructor THandTool.Create();
begin
  GlobalMode := Transform;
  (PCObj as ParameterCreator).CreateParameters(params);
end;

procedure THandTool.MouseDown(x, y: integer);
begin
  THand.Create(x, y);
end;

constructor TZoomTool.Create();
begin
  GlobalMode := Transform;
  (PCObj as ParameterCreator).CreateParameters(params);
end;

procedure TZoomTool.MouseDown(x, y: integer);
begin
  TZoom.Create(x, y);
end;

constructor TZoomRectangleTool.Create();
begin
  GlobalMode := Transform;
  (PCObj as ParameterCreator).CreateParameters(params);
end;

procedure TZoomRectangleTool.MouseDown(x, y: integer);
begin
  TZoomRect.Create(x, y);
end;

constructor TSelectTool.Create();
begin
  GlobalMode := Transform;
  (PCObj as ParameterCreator).CreateParameters(params);
end;

procedure TSelectTool.MouseDown(x, y: integer);
begin
  TSelect.Create(x, y);
end;

end.
