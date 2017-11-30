unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, Spin, StdCtrls, EditBtn, ComCtrls, Menus, maskedit, ActnList,
  Figures, Math, Types, CoordSystems, Tools, TransformTools, Parameters;

type

  { TFormMain }

  TFormMain = class(TForm)
    ColorButton1: TColorButton;
    HorScrollBar: TScrollBar;
    MenuEdit: TMenuItem;
    MenuReset: TMenuItem;
    MenuResetScale: TMenuItem;
    MenuResetOffset: TMenuItem;
    MenuScaleToFit: TMenuItem;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    VerScrollBar: TScrollBar;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;
    StaticText4: TStaticText;
    StaticText5: TStaticText;
    StaticText6: TStaticText;
    StaticText7: TStaticText;
    StaticText8: TStaticText;
    PanelOutline2: TPanel;
    PanelOutline1: TPanel;
    ColorButton: TColorButton;
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuHelp: TMenuItem;
    MenuFileExit: TMenuItem;
    MenuHeloAbout: TMenuItem;
    MenuWindowACC: TMenuItem;
    MenuWindow: TMenuItem;
    PanelInstrument: TPanel;
    PaintBox: TPaintBox;
    PanelMain: TPanel;
    procedure ColorButtonColorChanged(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure HandPopupDefPosClick(Sender: TObject);
    procedure HorScrollBarChange(Sender: TObject);
    procedure MenuResetScaleClick(Sender: TObject);
    procedure MenuScaleToFitClick(Sender: TObject);
    procedure PaintBoxMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure PaintBoxMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
    procedure FormCreate(Sender: TObject);
    procedure MenuFileExitClick(Sender: TObject);
    procedure MenuHelpAboutClick(Sender: TObject);
    procedure PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure ToolButtonMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure RXEditEditingDone(Sender: TObject);
    procedure RYEditEditingDone(Sender: TObject);
    procedure ReselectButton(button: TObject);
    procedure VerScrollBarChange(Sender: TObject);
    procedure ZoomPopupDefPosClick(Sender: TObject);

  private
    { private declarations }
  public
    { public declarations }
  end;

var
  FormMain: TFormMain;
  FigureType: integer;
  Drawing, Transformed: boolean;
  FigureButtonTag, rx, ry, lastheight, lastwidth: integer;
  LastChosenButton: TSpeedButton;
  figuretemp: TFigure;
  ttemp: TTransformTool;
  ToolButtons: array of TSpeedButton;
  SelectedTool: TTool;
  ptptpt: tedit;

const
  FormMainParams: array [0..0] of string = ('scale');

implementation

{$R *.lfm}

//TFormMain

procedure TFormMain.FormCreate(Sender: TObject);
var
  i, j: integer;
  b: TSpeedButton;
  LoadedPicture: TPicture;
  s: string;
begin
  FormInstrumentPanel := @panelinstrument;
  FormMainPaintBox := @PaintBox;
  PCObj := ParameterCreator.Create;
  LastChosenButton := nil;
  GlobalOffset := PointDouble(0, 0);
  GlobalScale := 1;
  (PCObj as ParameterCreator).CreateParameters(formmainparams);
  for i := 0 to High(TToolClassList) do
  begin
    Transformed := False;
    b := TSpeedButton.Create(PanelMain);
    b.Parent := PanelMain;
    s := TToolClassList[i].ClassName;
    Delete(s, 1, 1);
    b.Name := s;
    LoadedPicture := TPicture.Create;
    LoadedPicture.LoadFromFile(getcurrentdir + '\Images\' +
      TToolClassList[i].ClassName + '.png');
    b.Glyph := LoadedPicture.Bitmap;
    LoadedPicture.Free;
    b.Height := 44;
    b.Width := 44;
    b.Top := 44 * i;
    b.Flat := True;
    b.Margin := 0;
    b.Left := 2;
    b.Tag := i;
    b.OnMouseDown := @ToolButtonMouseDown;
    setlength(toolbuttons, length(toolbuttons) + 1);
    ToolButtons[i] := b;
  end;
  globalwidth := 1;
  globalcolor[0] := clBlack;
  rx := 25;
  ry := 25;
  FigureButtonTag := 0;
  LastChosenButton := ToolButtons[0];
  ReselectButton(ToolButtons[0]);
  lastheight := PaintBox.Width;
  pmin.x := 0;
  pmin.y := 0;
  pmax.x := Width;
  pmax.y := Height;
  HorScrollBar.min := -Width div 2;
  HorScrollBar.max := Width div 2;
  VerScrollBar.min := -Height div 2;
  VerScrollBar.max := Height div 2;
  CanvasWidth := PaintBox.Width;
  CanvasHeight := PaintBox.Height;
  FormMain.DoubleBuffered := True;
end;

procedure TFormMain.ReselectButton(button: TObject);
var
  i: TObject;
  a: TNotifyEvent;
begin
  FigureButtonTag := (button as TSpeedButton).Tag;
  SelectedTool.Free;
  SelectedTool := TToolClassList[FigureButtonTag].Create;
  LastChosenButton.Enabled := True;
  LastChosenButton.Margin := 0;
  LastChosenButton := (button as TSpeedButton);
  (button as TSpeedButton).Down := True;
  (button as TSpeedButton).Margin := 0;
  (button as TSpeedButton).Enabled := False;
end;

procedure TFormMain.VerScrollBarChange(Sender: TObject);
begin
  GlobalOffset.y := VerScrollBar.position;
  paintbox.invalidate;
end;

procedure TFormMain.ZoomPopupDefPosClick(Sender: TObject);
begin
  GlobalScale := 1;
end;

procedure TFormMain.RXEditEditingDone(Sender: TObject);
begin
  rx := StrToInt(Name);
end;

procedure TFormMain.RYEditEditingDone(Sender: TObject);
begin
  ry := StrToInt(Name);
end;

procedure TFormMain.ColorButtonColorChanged(Sender: TObject);
begin
  globalcolor[0] := ColorButton.ButtonColor;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
  CanvasWidth := paintbox.Width;
  CanvasHeight := paintbox.Height;
end;

procedure TFormMain.HandPopupDefPosClick(Sender: TObject);
begin
  GlobalOffset.x := 0;
  GlobalOffset.y := 0;
  PaintBox.Invalidate;
end;

procedure TFormMain.HorScrollBarChange(Sender: TObject);
begin
  GlobalOffset.x := HorScrollBar.position;
  paintbox.invalidate;
end;

procedure TFormMain.MenuResetScaleClick(Sender: TObject);
begin
  globalscale := 1;
end;

procedure TFormMain.MenuScaleToFitClick(Sender: TObject);
begin
  globalscale :=
    min(paintbox.Width / (pmax.x - pmin.x), paintbox.Height / (pmax.y - pmin.y));
  paintbox.invalidate;
end;

procedure TFormMain.PaintBoxMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: boolean);
begin
  GlobalScale := min(MAXSCALE, max(MINSCALE, GlobalScale - 0.05));
  paintbox.invalidate;
end;

procedure TFormMain.PaintBoxMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: boolean);
var
  p: array[0..1] of tpointdouble;
begin
  GlobalScale := min(MAXSCALE, max(MINSCALE, GlobalScale + 0.05));
  //ScaleSpinEdit.Value := GlobalScale * 100;
  paintbox.invalidate;
end;


procedure TFormMain.MenuFileExitClick(Sender: TObject);
begin
  FormMain.Close;
end;

procedure TFormMain.MenuHelpAboutClick(Sender: TObject);
begin
  ShowMessage('Vector Graphics Editor v. 0.004 by Lesogorov Mihail (b8103a)');
end;

procedure TFormMain.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbLeft) then
  begin
    SelectedTool.MouseDown(x, y);
    PaintBox.Invalidate;
    drawing := True;
  end;
end;

procedure TFormMain.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if (Drawing = True) then
  begin
    case globalmode of
      Draw: FigureList[high(FigureList)].MouseMove(x, y);
      Transform: CurrentTTool.MouseMove(x, y);
    end;
    ChangeDrawEdges(x, y);
    HorScrollBar.min := round(pmin.x) - Width div 2;
    HorScrollBar.max := round(pmax.x) - Width div 2;
    VerScrollBar.min := round(pmin.y) - Height div 2;
    VerScrollBar.max := round(pmax.y) - Height div 2;
    //ScaleSpinEdit.Value := GlobalScale * 100;
    PaintBox.Invalidate;
  end;
  {StaticText1.Caption := 'tag: ' + floattostr(LastChosenButton.Tag);
  StaticText2.Caption := 'mx: ' + IntToStr(x);
  StaticText3.Caption := 'my: ' + IntToStr(y);}
  StaticText4.Caption := 'offx: ' + floattostr(GlobalOffset.x);
  StaticText5.Caption := 'offy: ' + floattostr(GlobalOffset.y);
  StaticText6.Caption := 'mdis: ' + floattostr(mindistance);
  StaticText7.Caption := 'fllength: ' + IntToStr(length(figurelist));
  StaticText8.Caption := 'tag: ' + IntToStr(FigureButtonTag);
  //scaleSpinEdit.Value := GlobalScale * 100;
end;

procedure TFormMain.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  i: TFigure;
begin
  if (Button = mbLeft) then
  begin
    case globalmode of
      Draw: FigureList[high(FigureList)].MouseUp(x, y);
      Transform: CurrentTTool.MouseUp(x, y);
    end;
    PaintBox.Invalidate;
    Drawing := False;
    Transformed := False;
  end;
  paintbox.cursor := crDefault;
  PaintBox.Invalidate;
  //scaleSpinEdit.Value := GlobalScale * 100;
end;

procedure TFormMain.PaintBoxPaint(Sender: TObject);
var
  i: TFigure;
begin
  PaintBox.Canvas.Pen.Color := clWhite;
  PaintBox.Canvas.Brush.Color := clWhite;
  PaintBox.Canvas.Rectangle(0, 0, PaintBox.Width, PaintBox.Height);
  for i in FigureList do
  begin
    PaintBox.Canvas.Pen.Color := i.color;
    PaintBox.Canvas.Brush.Color := i.color;
    PaintBox.Canvas.Pen.Width := max(1, round(i.Width * GlobalScale));
    i.Draw(PaintBox.Canvas);
  end;
  if (GlobalMode = transform) and (drawing) then
    CurrentTTool.Draw(Paintbox.Canvas);
end;

procedure TFormMain.SpeedButton1Click(Sender: TObject);
var
  tc: tcolor;
begin
  tc := globalcolor[0];
  globalcolor[0] := globalcolor[1];
  globalcolor[1] := tc;
end;

procedure TFormMain.ToolButtonMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  ReselectButton(Sender as TSpeedButton);
end;

end.
