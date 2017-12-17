unit Main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons,
  ExtCtrls, Spin, StdCtrls, EditBtn, ComCtrls, Menus, maskedit, ActnList,
  Figures, Math, Types, CoordSystems, Tools, TransformTools, Parameters,
  LCLType, SaveLoad;

type

  { TFormMain }

  TFormMain = class(TForm)
    HorScrollBar: TScrollBar;
    MenuEdit: TMenuItem;
    MenuClear: TMenuItem;
    MenuOpen: TMenuItem;
    MenuSave: TMenuItem;
    MenuSaveAs: TMenuItem;
    MenuReset: TMenuItem;
    MenuResetScale: TMenuItem;
    MenuResetOffset: TMenuItem;
    MenuScaleToFit: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
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
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormResize(Sender: TObject);
    procedure HandPopupDefPosClick(Sender: TObject);
    procedure HorScrollBarChange(Sender: TObject);
    procedure MenuClearClick(Sender: TObject);
    procedure MenuOpenClick(Sender: TObject);
    procedure MenuResetScaleClick(Sender: TObject);
    procedure MenuSaveAsClick(Sender: TObject);
    procedure MenuSaveClick(Sender: TObject);
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
    procedure ReselectButton(button: TObject; isTemporary: boolean);
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
  SelectedButtonTag, LastButtonTag, rx, ry, lastheight, lastwidth: integer;
  LastSelectedButton: TSpeedButton;
  ttemp: TTransformTool;
  ToolButtons: array of TSpeedButton;
  SelectedTool: TTool;

const
  DEFAULT_EXTENCION = 'vproj';

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
  CurrentFileName := DEFAULT_FILE_NAME;
  saveDialog.DefaultExt := DEFAULT_EXTENCION;
  FormInstrumentPanel := @panelinstrument;
  FormMainPaintBox := @PaintBox;
  LastSelectedButton := nil;
  GlobalOffset := PointDouble(0, 0);
  GlobalScale := 1;
  CreateParameters(FormMainParams);
  globalcolor[0] := DEFAULTCOLOR0;
  globalcolor[1] := DEFAULTCOLOR1;
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
    b.transparent := False;
    b.OnMouseDown := @ToolButtonMouseDown;
    setlength(toolbuttons, length(toolbuttons) + 1);
    ToolButtons[i] := b;
  end;
  globalwidth := 1;
  globalcolor[0] := clBlack;
  SelectedButtonTag := 0;
  LastButtonTag := -1;
  LastSelectedButton := ToolButtons[0];
  ReselectButton(ToolButtons[0], False);
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

procedure TFormMain.ReselectButton(button: TObject; isTemporary: boolean);
begin
  //if (SelectedButtonTag <> (button as TSpeedButton).Tag) then
  //begin
  if (not isTemporary) then
    LastButtonTag := SelectedButtonTag;
  SelectedButtonTag := (button as TSpeedButton).Tag;
  SelectedTool.Free;
  SelectedTool := TToolClassList[SelectedButtonTag].Create;
  LastSelectedButton.Enabled := True;
  LastSelectedButton.Margin := 0;
  LastSelectedButton := (button as TSpeedButton);
  (button as TSpeedButton).Down := True;
  (button as TSpeedButton).Margin := 0;
  (button as TSpeedButton).Enabled := False;
  //end;
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

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case key of
    vk_shift: ShiftButtonPressed := True;
    vk_control: CtrlButtonPressed := True;
    vk_q: ReselectButton(toolbuttons[0], False);
    vk_w: ReselectButton(toolbuttons[1], False);
    vk_e: ReselectButton(toolbuttons[2], False);
    vk_r: ReselectButton(toolbuttons[3], False);
    vk_t: ReselectButton(toolbuttons[4], False);
    vk_a: ReselectButton(toolbuttons[5], False);
    vk_s: ReselectButton(toolbuttons[6], False);
    vk_d: ReselectButton(toolbuttons[7], False);
    vk_f: ReselectButton(toolbuttons[8], False);
    vk_space: ReselectButton(toolbuttons[5], False);
  end;
end;

procedure TFormMain.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  case key of
    vk_shift: ShiftButtonPressed := False;
    vk_control: CtrlButtonPressed := False;
    vk_space: ReselectButton(toolbuttons[LastButtonTag], False);
  end;
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

procedure TFormMain.MenuClearClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to high(FigureList) do
    FreeAndNil(FigureList[i]);
  setlength(FigureList, 0);
  PaintBox.Invalidate;
  if CurrentTTool.ClassName = 'TSelect' then
    (CurrentTTool as TSelect).UpdateParameters(FigureList);
end;

procedure TFormMain.MenuOpenClick(Sender: TObject);
begin
  if (OpenDialog.Execute) then
    LoadProject(OpenDialog.FileName);
  PaintBox.Invalidate;
end;

procedure TFormMain.MenuResetScaleClick(Sender: TObject);
begin
  globalscale := 1;
end;

procedure TFormMain.MenuSaveAsClick(Sender: TObject);
begin
  if (SaveDialog.Execute) then
  begin
    SaveProject(SaveDialog.FileName);
  end;
end;

procedure TFormMain.MenuSaveClick(Sender: TObject);
begin
  if CurrentFileName = DEFAULT_FILE_NAME then
  begin
    MenuSaveAsClick(Sender);
    exit;
  end;
  Application.MessageBox('Successfully saved!', 'Info');
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
  paintbox.invalidate;
end;

procedure TFormMain.MenuFileExitClick(Sender: TObject);
begin
  FormMain.Close;
end;

procedure TFormMain.MenuHelpAboutClick(Sender: TObject);
begin
  ShowMessage('VectorArt v. 0.004 - Vector Graphics Editor by Lesogorov Mihail (b8103a)');
end;

procedure TFormMain.PaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if (Button = mbLeft) then
  begin
    SelectedTool.MouseDown(x, y);
    PaintBox.Cursor := SelectedTool.Cursor;
    PaintBox.Invalidate;
    drawing := True;
  end;
end;

procedure TFormMain.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: integer);
begin
  if (Drawing = True) and (length(FigureList) > 0) then
  begin
    case GlobalMode of
      DrawMode: FigureList[high(FigureList)].MouseMove(x, y);
      TransformMode: CurrentTTool.MouseMove(x, y);
    end;
    ChangeDrawEdges(x, y);
    HorScrollBar.min := round(pmin.x) - Width div 2;
    HorScrollBar.max := round(pmax.x) - Width div 2;
    VerScrollBar.min := round(pmin.y) - Height div 2;
    VerScrollBar.max := round(pmax.y) - Height div 2;
    PaintBox.Invalidate;
  end;
  {StaticText1.Caption := 'tag: ' + floattostr(LastSelectedButton.Tag);
  StaticText2.Caption := 'mx: ' + IntToStr(x);
  StaticText3.Caption := 'my: ' + IntToStr(y);}
  StaticText4.Caption := 'last tag: ' + floattostr(LastButtonTag);
  StaticText5.Caption := 'curr tag: ' + floattostr(SelectedButtonTag);
  StaticText6.Caption := 'mdis: ' + floattostr(mindistance);
  StaticText7.Caption := 'fllength: ' + IntToStr(length(figurelist));
  if (length(figurelist) > 0) then
    StaticText8.Caption := 'pointscount: ' + IntToStr(length(figurelist[0].points));
end;

procedure TFormMain.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  i: TFigure;
begin
  if (Button = mbLeft) and (length(FigureList) > 0) then
  begin
    case globalmode of
      DrawMode:
        FigureList[high(FigureList)].MouseUp(x, y);
      TransformMode: CurrentTTool.MouseUp(x, y);
    end;
    PaintBox.Invalidate;
    Drawing := False;
    Transformed := False;
  end;
  PaintBox.Cursor := crDefault;
  PaintBox.Invalidate;
end;

procedure TFormMain.PaintBoxPaint(Sender: TObject);
var
  i: TFigure;
  buffer: array of TFigure;
begin
  PaintBox.Canvas.Pen.Color := clWhite;
  PaintBox.Canvas.Brush.Color := clWhite;
  PaintBox.Canvas.Rectangle(0, 0, PaintBox.Width, PaintBox.Height);
  for i in FigureList do
  begin
    if (i.selected) then
    begin
      setlength(buffer, length(buffer) + 1);
      buffer[high(buffer)] := i;
    end;
    PaintBox.Canvas.Pen.Color := i.OutlineColor;
    PaintBox.Canvas.Brush.Color := i.FillColor;
    PaintBox.Canvas.Pen.Width := max(1, round(i.Width * GlobalScale));
    i.Draw(PaintBox.Canvas);
  end;
  for i in buffer do
  begin
    i.DrawOutlineRectangle(i.points, PaintBox.Canvas);
    i.DrawPoints(i.points, PaintBox.Canvas);
  end;
  if (GlobalMode = TransformMode) and (drawing) then
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
  ReselectButton(Sender as TSpeedButton, False);
end;

end.
