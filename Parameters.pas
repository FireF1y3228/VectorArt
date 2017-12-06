unit Parameters;

{$mode objfpc}{$H+}
interface

uses
  Classes, SysUtils, FileUtil, Controls, Graphics, Math, StdCtrls,
  ExtCtrls, ComCtrls, CoordSystems, Buttons;

type
  DoublePointer = ^double;
  IntegerPointer = ^integer;
  ParameterStringList = array of string;

  ParameterProperties = record
    min, max, increment, multiplier: double;
    pointer: ^double;
  end;

  ParameterPropertiesList = array of ParameterProperties;

  ParameterObjectList = array of array of TObject;

  ParameterCreator = class
  private
    procedure creatEeditForm(_cleft: integer; aCaption, def, aName: string);
    function creatEeditFormAndProperties(_cleft: integer;
      aCaption, adef, aName: string; amin, amax, aincrement, amultiplier: double;
      apointer: DoublePointer): integer;
    procedure EditFormChange(Sender: TObject);
    procedure UpDownChangingEx(Sender: TObject; var AllowChange: boolean;
      NewValue: smallint; Direction: TUpDownDirection);
    function createbutton(_cleft: integer; aCaption: string): integer;
  var
    ParamObjList: ParameterObjectList;
    ParamPList: ParameterPropertiesList;
  const
    SCALEPARAMDEFPOS = 900;
  public
    procedure CreateParameters(aParamList: ParameterStringList);
  end;

  PersistentParameter = class

  end;

var
  FormInstrumentPanel: ^TPanel;
  FormMainPaintBox: ^TPaintBox;
  PCObj: ParameterCreator;
  GlobalColor: array[0..1] of TColor;
  GlobalWidth, GlobalRadX, GlobalRadY: double;

implementation

procedure ParameterCreator.CreateParameters(aParamList: ParameterStringList);
var
  s: string;
  cleft, i, j: integer;
begin
  for i := 1 to high(ParamObjList) do
    for j := 0 to high(ParamObjList[high(ParamObjList)]) do
      FreeAndNil(ParamObjList[i, j]);
  if (length(paramobjlist) <> 0) then
  begin
    setlength(ParamObjList, 1);
    setlength(ParamPlist, 1);
  end;
  Cleft := 150;
  for s in aParamList do
  begin
    case s of
      'width':
        cleft := creatEeditFormAndProperties(cleft, 'Width: ',
          floattostr(globalwidth), 'Width', 1, 512, 1, 1, @GlobalWidth);
      'radiusx':
        cleft := creatEeditFormAndProperties(cleft, 'Radius X: ',
          floattostr(GlobalRadX), 'RadX', 1, 512, 1, 1, @GlobalRadX);
      'radiusy':
        cleft := creatEeditFormAndProperties(cleft, 'Radius Y: ',
          floattostr(GlobalRadY), 'RadY', 1, 512, 1, 1, @GlobalRadY);
      'scale':
        creatEeditFormAndProperties(SCALEPARAMDEFPOS,
          'Scale: ', floattostr(GlobalScale * 100), 'CanvasScl', 1, 3200,
          0.1, 0.01, @GlobalScale);
      'color':
      begin

      end;
      'delete':
        cleft:=CreateButton(cleft, 'Delete');
    end;
  end;
end;

procedure ParameterCreator.CreateEditForm(_cleft: integer; aCaption, def, aName: string);
var
  h: integer;
begin
  setlength(ParamObjList, length(ParamObjList) + 1);
  h := high(ParamObjList);
  setlength(ParamObjList[h], length(ParamObjList[h]) + 1);
  paramobjlist[h, high(paramobjlist[h])] := TEdit.Create(nil);
  with (paramobjlist[h, high(ParamObjList[h])] as TEdit) do
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
    Name := aName + 'Edit';
    TabStop := False;
    TabOrder := 0;
    Width := 32;
    Height := 16;
    Left := _cleft;
    OnChange := @EditFormChange;
  end;

  setlength(ParamObjList[h], length(ParamObjList[h]) + 1);
  paramobjlist[h, high(paramobjlist[h])] := TUpDown.Create(nil);
  with (paramobjlist[h, high(ParamObjList[h])] as TUpDown) do
  begin
    Parent := FormInstrumentPanel^;
    Tag := h;
    Top := 7;
    (paramobjlist[h, 0] as TEdit).Text := Def;
    Left := _cleft + 32;
    Height := 18;
    Font.CharSet := 204;
    Min := 0;
    Max := 32767;
    increment := 0;
    Name := aName + 'UpDown';
    OnChangingEx := @UpDownChangingEx;
  end;

  setlength(ParamObjList[h], length(ParamObjList[h]) + 1);
  paramobjlist[h, high(paramobjlist[h])] := TBevel.Create(nil);
  with (paramobjlist[h, high(ParamObjList[h])] as TBevel) do
  begin
    Parent := FormInstrumentPanel^;
    Tag := h;
    Shape := BsBottomLine;
    Left := _cleft;
    Height := 1;
    Name := aName + 'Bevel';
    Top := 26;
  end;

  setlength(ParamObjList[h], length(ParamObjList[h]) + 1);
  paramobjlist[h, high(paramobjlist[h])] := TLabel.Create(nil);
  with (paramobjlist[h, high(ParamObjList[h])] as TLabel) do
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
    Left := _cleft - 5 * length(aCaption);
  end;
end;

function ParameterCreator.CreateButton(_cleft: integer; aCaption: string): integer;
var
  h: integer;
begin
  setlength(ParamObjList, length(ParamObjList) + 1);
  h := high(ParamObjList);
  setlength(ParamObjList[h], length(ParamObjList[h]) + 1);
  paramobjlist[h, high(paramobjlist[h])] := TSpeedButton.Create(nil);
  with (paramobjlist[h, high(ParamObjList[h])] as TSpeedButton) do
  begin
    Parent := FormInstrumentPanel^;
    left := _cleft;
    Caption := aCaption;
    //flat := True;
    color := clWindowFrame;
    Top := 4;
    Width := length(Caption) * 10;
    Height := 26;
  end;
  Result := _cleft + (paramobjlist[h, high(ParamObjList[h])] as TSpeedButton).Width;
end;

procedure ParameterCreator.EditFormChange(Sender: TObject);
var
  txt: string;
  i, t: integer;
  _e: TEdit;
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
    txt := floattostr(ParamPlist[t].min);
  if (StrTofloat(txt) > ParamPlist[t].max) then
    txt := floattostr(ParamPlist[t].max);
  if (StrTofloat(txt) < ParamPlist[t].min) then
    txt := floattostr(ParamPlist[t].min);
  _e.Text := txt;
  ParamPlist[t].pointer^ := StrTofloat(txt) * ParamPlist[t].multiplier;
  FormMainPaintBox^.invalidate;
end;

procedure ParameterCreator.UpDownChangingEx(Sender: TObject;
  var AllowChange: boolean; NewValue: smallint; Direction: TUpDownDirection);
var
  _u: TUpDown;
  t: integer;
  _e: TEdit;
  val: double;
  s: string;
begin
  _u := Sender as TUpDown;
  _e := ParamObjList[_u.tag, 0] as TEdit;
  t := _e.tag;
  val := strtofloat(_e.Text);
  case Direction of
    UpdUp: val += ParamPlist[t].increment;
    UpdDown: val -= ParamPlist[t].increment;
  end;
  s := floattostr(val);
  _e.Text := floattostr(val);
  FormMainPaintBox^.invalidate;
end;

function ParameterCreator.CreatEeditFormAndProperties(_cleft: integer;
  aCaption, adef, aName: string; amin, amax, aincrement, amultiplier: double;
  apointer: DoublePointer): integer;
begin
  setlength(ParamPlist, Length(ParamPlist) + 1);
  with (ParamPlist[High(ParamPlist)]) do
  begin
    min := amin;
    max := amax;
    increment := aincrement;
    pointer := apointer;
    multiplier := amultiplier;
  end;
  CreateEditForm(_cleft, aCaption, aDef, aName);
  Result := _cleft +128;
end;

end.
