unit Tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs, Math, CoordSystems;

type
  TTool = class
  public
  var
    points: array of TPointDouble;
    color: TColor;
    rx, ry, Width: integer;
    constructor Create(x, y: integer); virtual; abstract;
    procedure Draw(acanvas: TCanvas); virtual; abstract;
    procedure MouseMove(x, y: integer); virtual; abstract;
    procedure MouseUp(x, y: integer); virtual; abstract;
  end;

  TToolClass = class of TTool;
  TToolClassList=array of TToolClass;
var
  ToolList: array of TToolClass;

implementation

end.
