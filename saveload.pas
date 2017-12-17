unit SaveLoad;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Figures, Graphics, typinfo, CoordSystems;

const
  DEFAULT_FILE_NAME = 'Untitled';

var
  CurrentFileName: string;

procedure SaveProject(FileName: string);
procedure LoadProject(FileName: string);

implementation

procedure SaveProject(FileName: string);
var
  i: integer;
  a: TFigure;
  f: Text;
  s: string;
  p: TPointDouble;
begin
  AssignFile(f, FileName);/////////////////////////
  Rewrite(f);                               // vproj File Format:
  Writeln(f, length(FigureList));           // figure count
  for a in FigureList do                    // class name
  begin                                     // point count
    writeln(f, a.ClassName);                // points
    writeln(f, length(a.Points) * 2);       // parameter name
    for p in a.Points do                    // value
    begin                                   // #
      writeln(f, floattostr(p.x));          // class name
      writeln(f, floattostr(p.y));          // ...
    end;
    for s in a.Params do
    begin
      writeln(f, s);
      case s of
        'width':
          writeln(f, a.Width);
        'radiusx':
          writeln(f, a.RadX);
        'radiusy':
          writeln(f, a.RadY);
        'fillstyle':
          break;
      end;
    end;
    writeln(f, 'outlinecolor');
    writeln(f, red(a.OutlineColor));
    writeln(f, green(a.OutlineColor));
    writeln(f, blue(a.OutlineColor));
    writeln(f, 'fillcolor');
    writeln(f, red(a.FillColor));
    writeln(f, green(a.FillColor));
    writeln(f, blue(a.FillColor));
    writeln(f, '#');
  end;
  Reset(f);
  CloseFile(f);
  CurrentFileName := FileName;
end;

procedure LoadProject(FileName: string);
var
  i, n, j, block: integer;
  s: string;
  slist: array of string;
  f: Text;
  a: TFigure;
begin
  for i := 0 to high(FigureList) do
    FreeAndNil(FigureList[i]);
  SetLength(FigureList, 0);
  AssignFile(f, FileName);
  reset(f);
  readln(f, n);
  for i := 0 to n - 1 do
  begin
    block := 0;
    s := '';
    SetLength(sList, 0);
    while s <> '#' do
    begin
      readln(f, s);
      if (s <> '#') then
      begin
        setlength(sList, length(sList) + 1);
        sList[high(sList)] := s;
      end
      else
        break;
    end;
    case sList[0] of
      'TPencil':
        a := TPencil.Create(StrToFloat(sList[2]), StrToFloat(sList[3]));
      'TRectangle':
        a := TRectangle.Create(StrToFloat(sList[2]), StrToFloat(sList[3]));
      'TLine':
        a := TLine.Create(StrToFloat(sList[2]), StrToFloat(sList[3]));
      'TRoundrect':
        a := TRoundrect.Create(StrToFloat(sList[2]), StrToFloat(sList[3]));
      'TEllipse':
        a := TEllipse.Create(StrToFloat(sList[2]), StrToFloat(sList[3]));
    end;
    for j := 4 to (1 + StrToInt(sList[1])) do
    begin
      if (j mod 2 = 0) then
      begin
        if (j <> 4) then
          setlength(a.Points, length(a.Points) + 1);
        a.Points[high(a.Points)].x := StrToFloat(sList[j]);
      end
      else
        a.Points[high(a.Points)].y := StrToFloat(sList[j]);
    end;
    for j := (4 + StrToInt(sList[1])) to high(sList) do
    begin
      if (block > 0) then
      begin
        Dec(block);
        continue;
      end;
      case sList[j] of
        'width':
        begin
          a.Width := StrToInt(sList[j + 1]);
          block := 1;
        end;
        'radiusx':
        begin
          a.RadX := StrToInt(sList[j + 1]);
          block := 1;
        end;
        'radiusy':
        begin
          a.RadY := StrToInt(sList[j + 1]);
          block := 1;
        end;
        'outlinecolor':
        begin
          a.OutlineColor := RGBToColor(StrToInt(sList[j + 1]), StrToInt(
            sList[j + 2]), StrToInt(sList[j + 3]));
          block := 3;
        end;
        'fillcolor':
        begin
          a.FillColor := RGBToColor(StrToInt(sList[j + 1]), StrToInt(
            sList[j + 2]), StrToInt(sList[j + 3]));
          block := 3;
        end;
      end;
      setlength(a.Params, Length(a.Params) + 1);
      a.Params[high(a.Params)] := sList[j];
    end;
    setlength(FigureList, Length(FigureList) + 1);
    FigureList[high(FigureList)] := a;
  end;
  CloseFile(f);
end;

{function stod(s0: string): double;
var
  s1, s2: string;
  p: integer;
begin
  s1 := '';
  s2 := s1;
  p := pos('E', s0);
  s1 := copy(s0, 2, p - 2);
  s2 := copy(s0, p + 2, length(s0) - p + 2);
  for p := length(s1) downto 1 do
  begin
    if (s1[p] = '0') then
      Delete(s1, p, 1)
    else
      break;
  end;
  for p := length(s2) downto 1 do
  begin
    if (s2[p] = '0') then
      Delete(s2, p, 1)
    else
      break;
  end;
  Result := strtofloat(s1) * power(10, strtofloat(s2));
end;   }

end.
