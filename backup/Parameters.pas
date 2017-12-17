unit Parameters;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, FileUtil, Controls, Graphics, Math, StdCtrls,
  ExtCtrls, ComCtrls, CoordSystems, Buttons, Dialogs, Figures;

type
  DoublePointer = ^double;
  IntegerPointer = ^integer;
  ColorPointer = ^TColor;

  BaseParameter = class
  protected
  var
    left: integer;
  public
  var
    ObjectList: ParameterObjectListType;
  end;

  BaseParameterListType = array of BaseParameter;

  TextParameter = class(BaseParameter)
  private
  var
    min, max, increment, multiplier: double;
    pointer: ^double;

    procedure EditFormChange(Sender: TObject);
    procedure UpDownChangingEx(Sender: TObject; var AllowChange: boolean;
      NewValue: smallint; Direction: TUpDownDirection);
    procedure CreateEditForm(_cleft: integer; aCaption, def, aName: string);
  public
    constructor Create(_cleft: integer; aCaption, adef, aName: string;
      amin, amax, aincrement, amultiplier: double; apointer: DoublePointer);
    function GetText: string;
    procedure SetText(atext: string);
  end;


  ColorButtonParameter = class(BaseParameter)
  private
  var
    ButtonColor: TColor;
    pointer: ^TColor;

    procedure OnChangedColor(Sender: TObject);

  public
    constructor Create(aBorderWidth, aWidth, aHeight, aTop, aLeft: integer;
      aButtonColor: TColor; apointer: ColorPointer; atransparent: boolean);
  end;

  ButtonParameter = class(BaseParameter)
  private
    procedure SpeedButtonClick5(Sender: TObject);
    procedure SpeedButtonClick1(Sender: TObject);
    procedure SpeedButtonClick2(Sender: TObject);
    procedure SpeedButtonClick3(Sender: TObject);
    procedure SpeedButtonClick4(Sender: TObject);
  public
    constructor Create(aWidth, aHeight, aTop, aLeft: integer;
      aFlat: boolean; aCaption, aIconPath: string; aOnClick: integer);
  end;

procedure CreateParameters(aParamList: ParameterStringListType);
procedure UpdateScale;
procedure UpdateFigureProperties;

const
  SCALEPARAMDEFPOS = 1000;
  DEFAULTLEFT = 150;
  FormMainParams: array [0..4] of string =
    ('color2', 'color1', 'swapcolors', 'nocolor', 'scale');
  DEFAULT_PARAMETERS_COUNT = sizeof(formmainparams);
  DEFAULTCOLOR0 = ClBlack;
  DEFAULTCOLOR1 = ClWhite;

var
  BaseParameterList: BaseParameterListType;
  FormInstrumentPanel: ^TPanel;
  FormMainPaintBox: ^TPaintBox;
  GlobalColor: array[0..1] of TColor;
  GlobalWidth, GlobalRadX, GlobalRadY: double;
  CtrlButtonPressed, ShiftButtonPressed: boolean;

implementation

procedure CreateParameters(aParamList: ParameterStringListType);
var
  s: string;
  cleft, j, h, i: integer;
begin
  if (length(BaseParameterList) <> 0) then
  begin
    for i := DEFAULT_PARAMETERS_COUNT to high(BaseParameterList) do
    begin
      for j := 0 to high(BaseParameterList[i].ObjectList) do
        FreeAndNil(BaseParameterList[i].ObjectList[j]);
      FreeAndNil(BaseParameterList[i]);
    end;
    SetLength(BaseParameterList, DEFAULT_PARAMETERS_COUNT);
  end;
  for s in aParamList do
  begin
    if (length(BaseParameterList) > DEFAULT_PARAMETERS_COUNT) then
      cleft := BaseParameterList[high(BaseParameterList)].left
    else
      cleft := DEFAULTLEFT;
    setlength(BaseParameterList, length(BaseParameterList) + 1);
    h := high(BaseParameterList);
    case s of
      'width':
        BaseParameterList[h] :=
          TextParameter.Create(cleft, 'Width: ', floattostr(globalwidth),
          'Width', 1, 512, 1, 1, @GlobalWidth);
      'radiusx':
        BaseParameterList[h] :=
          TextParameter.Create(cleft, 'Radius X: ', floattostr(GlobalRadX),
          'RadX', 1, 512, 1, 1, @GlobalRadX);
      'radiusy':
        BaseParameterList[h] :=
          TextParameter.Create(cleft, 'Radius Y: ', floattostr(GlobalRadY),
          'RadY', 1, 512, 1, 1, @GlobalRadY);
      'scale':
        BaseParameterList[h] :=
          TextParameter.Create(SCALEPARAMDEFPOS, 'Scale: ',
          floattostr(GlobalScale * 100), 'CanvasScl', 1, 3200, 0.1, 0.01, @GlobalScale);
      'color2':
        BaseParameterList[h] :=
          ColorButtonParameter.Create(1, 40, 22, 4, 0, ClWhite, @globalcolor[1], False);
      'color1':
        BaseParameterList[h] :=
          ColorButtonParameter.Create(1, 39, 33, 0, 17, ClBlack, @globalcolor[0], False);
      'swapcolors':
        BaseParameterList[h] :=
          ButtonParameter.Create(20, 20, 7, 57, True, '-', 'swapcolors', 0);
      'nocolor':
        BaseParameterList[h] :=
          ButtonParameter.Create(20, 20, 7, 80, True, '-', 'nocolor', 1);
      'zup':
        BaseParameterList[h] :=
          ButtonParameter.Create(27, 27, 3, cleft, True, '-', 'zup', 2);
      'zdown':
        BaseParameterList[h] :=
          ButtonParameter.Create(27, 27, 3, cleft, True, '-', 'zdown', 3);
      'delete':
        BaseParameterList[h] :=
          ButtonParameter.Create(27, 27, 3, cleft, True, '-', 'delete', 4);
      else
        setlength(BaseParameterList, length(BaseParameterList) - 1);
    end;
  end;
end;

procedure TextParameter.CreateEditForm(_cleft: integer; aCaption, def, aName: string);
var
  h: integer;
begin
  left := _cleft;
  setlength(ObjectList, length(ObjectList) + 1);
  h := high(ObjectList);
  ObjectList[high(ObjectList)] := TEdit.Create(nil);
  with (ObjectList[high(ObjectList)] as TEdit) do
  begin
    BorderStyle := BsNone;
    Parent := FormInstrumentPanel^;
    Alignment := TaLeftJustify;
    AutoSize := False;
    Color := $00525252;
    AutoSelect := False;
    HideSelection := False;
    Font.Name := 'Tahoma';
    ParentFont := False;
    ParentColor := False;
    ParentBidiMode := False;
    Font.CharSet := 204;
    Font.Color := ClWhite;
    Font.Size := 8;
    Top := 7;
    Tag := h;
    Name := aName + 'Editz';
    TabStop := False;
    TabOrder := 0;
    Width := 32;
    Height := 16;
    Left := _cleft + 5 * length(aCaption);
    OnChange := @EditFormChange;
  end;

  setlength(ObjectList, length(ObjectList) + 1);
  ObjectList[high(ObjectList)] := TUpDown.Create(nil);
  with (ObjectList[high(ObjectList)] as TUpDown) do
  begin
    Parent := FormInstrumentPanel^;
    Tag := h;
    Top := 7;
    (objectlist[0] as TEdit).Text := Def;
    Left := _cleft + 32 + 5 * length(aCaption);
    Height := 18;
    Font.CharSet := 204;
    Min := 0;
    Max := 32767;
    increment := 0;
    Name := aName + 'UpDown';
    OnChangingEx := @UpDownChangingEx;
  end;

  setlength(ObjectList, length(ObjectList) + 1);
  ObjectList[high(ObjectList)] := TBevel.Create(nil);
  with (ObjectList[high(ObjectList)] as TBevel) do
  begin
    Parent := FormInstrumentPanel^;
    Tag := h;
    Shape := BsBottomLine;
    Left := _cleft + 5 * length(aCaption);
    Height := 1;
    Name := aName + 'Bevel';
    Top := 26;
  end;

  setlength(ObjectList, length(ObjectList) + 1);
  ObjectList[high(ObjectList)] := TLabel.Create(nil);
  with (ObjectList[high(ObjectList)] as TLabel) do
  begin
    Parent := FormInstrumentPanel^;
    Tag := h;
    Font.Charset := 204;
    Font.Color := ClSilver;
    Font.Name := 'Tahoma';
    Font.Size := 8;
    ParentColor := False;
    ParentFont := False;
    Caption := aCaption;
    Top := 8;
    Name := aName + 'Label';
    Height := 16;
    Left := _cleft;
  end;
  left += 128;
end;

procedure TextParameter.EditFormChange(Sender: TObject);
var
  txt, tempname: string;
  i, t: integer;
  _e: TEdit;
  tdouble: double;
begin
  _e := (Sender as TEdit);
  t := _e.Tag;
  txt := _e.Text;
  for i := 1 to length(txt) do
  begin
    if (not ((txt[i] in ['0'..'9']) or (txt[i] = ','))) then
    begin
      Delete(txt, i, 1);
    end;
  end;
  if ((txt = '') or (txt = '0')) then
    txt := floattostr(min);
  if (StrTofloat(txt) > max) then
    txt := floattostr(max);
  if (StrTofloat(txt) < min) then
    txt := floattostr(min);
  tdouble := strtofloat(txt);
  tdouble := trunc(frac(tdouble) * 100) / 100 + trunc(tdouble);
  _e.Text := floattostr(tdouble);
  pointer^ := StrTofloat(txt) * multiplier;
  tempname := _e.Name;
  if tempname[length(tempname)] = 'z' then
  begin
    Delete(tempname, length(tempname), 1);
    _e.Name := tempname;
  end
  else
    UpdateFigureProperties;
  FormMainPaintBox^.invalidate;
end;

procedure TextParameter.UpDownChangingEx(Sender: TObject; var AllowChange: boolean;
  NewValue: smallint; Direction: TUpDownDirection);
var
  _u: TUpDown;
  t: integer;
  _e: TEdit;
  val: double;
  s: string;
begin
  _u := Sender as TUpDown;
  _e := ObjectList[0] as TEdit;
  t := _e.tag;
  val := strtofloat(_e.Text);
  case Direction of
    UpdUp: val += increment;
    UpdDown: val -= increment;
  end;
  s := floattostr(val);
  _e.Text := floattostr(val);
  FormMainPaintBox^.invalidate;
end;

constructor TextParameter.Create(_cleft: integer; aCaption, adef, aName: string;
  amin, amax, aincrement, amultiplier: double; apointer: DoublePointer);
begin
  min := amin;
  max := amax;
  increment := aincrement;
  pointer := apointer;
  multiplier := amultiplier;
  CreateEditForm(_cleft, aCaption, aDef, aName);
end;

constructor ColorButtonParameter.Create(aBorderWidth, aWidth, aHeight,
  aTop, aLeft: integer; aButtonColor: TColor; apointer: ColorPointer;
  atransparent: boolean);
begin
  setlength(ObjectList, Length(ObjectList) + 1);
  ObjectList[high(ObjectList)] := TColorButton.Create(nil);
  with (ObjectList[high(ObjectList)] as TColorButton) do
  begin
    BorderWidth := aBorderWidth;
    Width := aWidth;
    Height := aHeight;
    Top := aTop;
    flat := True;
    Left := aLeft;
    ButtonColor := aButtonColor;
    Parent := FormInstrumentPanel^;
    Transparent := aTransparent;
    OnColorChanged := @OnChangedColor;
  end;
  Left := aLeft + aWidth + 15;
  Pointer := aPointer;
end;

constructor ButtonParameter.Create(aWidth, aHeight, aTop, aLeft: integer;
  aFlat: boolean; aCaption, aIconPath: string; aOnClick: integer);
var
  Picture: TPicture;
begin
  setlength(ObjectList, Length(ObjectList) + 1);
  ObjectList[high(ObjectList)] := TSpeedButton.Create(nil);
  with (ObjectList[high(ObjectList)] as TSpeedButton) do
  begin
    Width := aWidth;
    Height := aHeight;
    Top := aTop;
    Left := aLeft;
    if (acaption = '-') then
      ShowCaption := False
    else
      Caption := aCaption;
    Parent := FormInstrumentPanel^;
    case aOnClick of
      0: OnClick := @SpeedButtonClick1;
      1: OnClick := @SpeedButtonClick2;
      2: OnClick := @SpeedButtonClick3;
      3: OnClick := @SpeedButtonClick4;
      4: OnClick := @SpeedButtonClick5;
    end;
  end;
  Picture := TPicture.Create;
  Picture.LoadFromFile(getcurrentdir + '\Images\' + aIconPath + '.png');
  (ObjectList[high(ObjectList)] as TSpeedButton).Glyph := Picture.Bitmap;
  Left := aLeft + aWidth + 15;
end;

procedure ColorButtonParameter.OnChangedColor(Sender: TObject);
var
  tempColor: TColor;
  f: TFigure;
begin
  tempColor := GlobalColor[0];
  pointer^ := (Sender as TColorButton).ButtonColor;
  if (tempColor <> GlobalColor[0]) then
  begin
    for f in figurelist do
    begin
      if (f.selected) then
        f.color := GlobalColor[0];
    end;
  end;
  FormMainPaintBox^.Invalidate;
end;

procedure ButtonParameter.SpeedButtonClick1(Sender: TObject);
var
  tempcolor: TColor;
  _o1, _o2: TObject;
begin
  tempcolor := globalcolor[0];
  globalcolor[0] := globalcolor[1];
  globalcolor[1] := tempcolor;
  _o1 := (baseparameterlist[0] as ColorButtonParameter).ObjectList[0];
  _o2 := (baseparameterlist[1] as ColorButtonParameter).ObjectList[0];
  (_o1 as TColorButton).ButtonColor := globalcolor[1];
  (_o2 as TColorButton).ButtonColor := globalcolor[0];
end;

procedure UpdateScale;
begin
  (BaseParameterList[4] as TextParameter).SetText(floattostr(GlobalScale * 100));
end;

function TextParameter.GetText: string;
begin
  Result := (ObjectList[0] as TEdit).Text;
end;

procedure TextParameter.SetText(atext: string);
begin
  (ObjectList[0] as TEdit).Text := atext;
end;

procedure ButtonParameter.SpeedButtonClick2(Sender: TObject);
begin
  globalcolor[0] := clNone;
end;

procedure ButtonParameter.SpeedButtonClick3(Sender: TObject);
var
  i: integer;
  TempFigure: TFigure;
begin
  for i := (high(FigureList) - 1) downto 0 do
  begin
    if (FigureList[i].selected) and (not FigureList[i + 1].selected) then
    begin
      TempFigure := FigureList[i + 1];
      FigureList[i + 1] := FigureList[i];
      FigureList[i] := TempFigure;
    end;
  end;
  FormMainPaintBox^.Invalidate;
end;

procedure ButtonParameter.SpeedButtonClick4(Sender: TObject);
var
  i: integer;
  TempFigure: TFigure;
begin
  for i := 1 to high(FigureList) do
  begin
    if (FigureList[i].selected) and (not FigureList[i - 1].selected) then
    begin
      TempFigure := FigureList[i - 1];
      FigureList[i - 1] := FigureList[i];
      FigureList[i] := TempFigure;
    end;
  end;
  FormMainPaintBox^.Invalidate;
end;

procedure ButtonParameter.SpeedButtonClick5(Sender: TObject);
var
  i: integer;
  TempFigureList: array of TFigure;
begin
  for i := 0 to high(FigureList) do
  begin
    if (not FigureList[i].selected) then
    begin
      setlength(TempFigureList, length(TempFigureList) + 1);
      TempFigureList[high(TempFigureList)] := FigureList[i];
    end
    else
      FreeAndNil(FigureList[i]);
  end;
  FigureList := TempFigureList;
  FormMainPaintBox^.Invalidate;
end;


procedure UpdateFigureProperties();
var
  f: TFigure;
  j: integer;
  s: string;
begin
  for f in FigureList do
  begin
    if f.Selected then
    begin
      for s in f.Params do
      begin
        case s of
          'width':
            f.Width := round(GlobalWidth);
          'radx':
            f.RadX := round(GlobalRadX);
          'rady':
            f.RadY := round(GlobalRadY);
        end;
      end;
    end;
  end;
end;

end.
