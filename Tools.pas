unit Tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Controls, Dialogs, Math, CoordSystems, Parameters,
  Figures, TransformTools;

type

  TTool = class
  var
    Cursor: TCursor;
  public
    procedure MouseDown(x, y: integer); virtual; abstract;
    constructor Create; virtual; abstract;
  end;

  TPencilTool = class(TTool)
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
    params: array [0..2] of string = ('zup', 'zdown', 'delete');
  public
    constructor Create; override;
    procedure MouseDown(x, y: integer); override;
  end;

  TToolClass = class of TTool;

const
  TToolClassList: array [0..8] of TToolClass =
    (TPencilTool, TLineTool, TRectangleTool, TEllipseTool, TRoundrectTool,
    THandTool, TZoomTool, TZoomRectangleTool, TSelectTool);

implementation

constructor TPencilTool.Create();
begin
  Cursor := crCross;
  CreateParameters(params);
end;

procedure TPencilTool.MouseDown(x, y: integer);
begin
  setlength(FigureList, length(FigureList) + 1);
  FigureList[High(FigureList)] := TPencil.Create(x, y);
  with FigureList[High(FigureList)] do
  begin
    FillColor := GlobalColor[0];
    Width := round(GlobalWidth);
    Params := self.Params;
  end;
end;

constructor TLineTool.Create();
begin
  Cursor := crCross;
  CreateParameters(params);
end;

procedure TLineTool.MouseDown(x, y: integer);
begin
  setlength(FigureList, length(FigureList) + 1);
  FigureList[High(FigureList)] := TLine.Create(x, y);
  with FigureList[High(FigureList)] do
  begin
    FillColor := GlobalColor[0];
    Width := round(GlobalWidth);
    Params := self.Params;
  end;
end;

constructor TRectangleTool.Create();
begin
  Cursor := crCross;
  CreateParameters(params);
end;

procedure TRectangleTool.MouseDown(x, y: integer);
begin
  setlength(FigureList, length(FigureList) + 1);
  FigureList[High(FigureList)] := TRectangle.Create(x, y);
  with FigureList[High(FigureList)] do
  begin
    FillColor := GlobalColor[0];
    Width := round(GlobalWidth);
    Params := self.Params;
  end;
end;

constructor TEllipseTool.Create();
begin
  Cursor := crCross;
  CreateParameters(params);
end;

procedure TEllipseTool.MouseDown(x, y: integer);
begin
  setlength(FigureList, length(FigureList) + 1);
  FigureList[High(FigureList)] := TEllipse.Create(x, y);
  with FigureList[High(FigureList)] do
  begin
    FillColor := GlobalColor[0];
    Width := round(GlobalWidth);
    Params := self.Params;
  end;
end;

constructor TRoundrectTool.Create();
begin
  Cursor := crCross;
  CreateParameters(params);
end;

procedure TRoundrectTool.MouseDown(x, y: integer);
begin
  setlength(FigureList, length(FigureList) + 1);
  FigureList[High(FigureList)] := TRoundrect.Create(x, y);
  with (FigureList[High(FigureList)] as TRoundrect) do
  begin
    FillColor := GlobalColor[0];
    Width := round(GlobalWidth);
    RadX := round(GlobalRadx);
    RadY := round(GlobalRady);
    Params := self.Params;
  end;
end;

constructor THandTool.Create();
begin
  Cursor := crHandPoint;
  CreateParameters(params);
end;

procedure THandTool.MouseDown(x, y: integer);
begin
  THand.Create(x, y);
end;

constructor TZoomTool.Create();
begin
  Cursor := crSizeWE;
  CreateParameters(params);
end;

procedure TZoomTool.MouseDown(x, y: integer);
begin
  TZoom.Create(x, y);
  CurrentTTool.Params := Params;
end;

constructor TZoomRectangleTool.Create();
begin
  Cursor := crDrag;
  CreateParameters(params);
end;

procedure TZoomRectangleTool.MouseDown(x, y: integer);
begin
  TZoomRect.Create(x, y);
  CurrentTTool.Params := Params;
end;

constructor TSelectTool.Create();
begin
  Cursor := crDefault;
  CreateParameters(params);
end;

procedure TSelectTool.MouseDown(x, y: integer);
begin
  TSelect.Create(x, y);
  CurrentTTool.Params := Params;
  (CurrentTTool as TSelect).UpdateParameters(FigureList);
end;

end.
