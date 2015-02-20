unit dpsMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Buttons, StdCtrls, ExtCtrls, ComCtrls, Printers, Registry, Mask, ToolEdit,
  Menus, RXShell, AppEvent, RxCombos, RXSpin, rpVersionInfo;

const
  regRoot   = '\Software\Apis\DPS';
  plCP852   = 0;
  plISO     = 1;
  plWinCE   = 2;
  plMazovia = 3;
  plNone    = 4;
  plUTF8    = 5;

type
  TMain = class(TForm)
    ToolBar: TPanel;
    Status: TPanel;
    tm: TTimer;
    Device: TComboBox;
    Path: TDirectoryEdit;
    TrayIcon: TRxTrayIcon;
    PopupMenu: TPopupMenu;
    Ustawienia: TMenuItem;
    Zakocz: TMenuItem;
    AppEvents: TAppEvents;
    FontSize: TRxSpinEdit;
    PathLabel: TLabel;
    DeviceLabel: TLabel;
    FontLabel: TLabel;
    Code: TComboBox;
    CodeLAbel: TLabel;
    Drukuj: TMenuItem;
    Panel1: TPanel;
    Buffer: TRichEdit;
    VersionInfo: TrpVersionInfo;
    FontName: TFontComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SetDefaults(Sender: TObject);
    procedure tmTimer(Sender: TObject);
    procedure CloseApp(Sender: TObject);
    procedure AppMinimize(Sender: TObject);
    procedure AppRestore(Sender: TObject);
    procedure ParseText(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure PrintBuffer(Sender: String);
    procedure FormActivate(Sender: TObject);
    procedure AppEventsDeactivate(Sender: TObject);
    procedure DeviceChange(Sender: TObject);
    procedure FontChange(Sender: TObject);
    procedure PathChange(Sender: TObject);
    procedure CodeChange(Sender: TObject);
  private
    { Private declarations }
    SourceText: TStrings;
    OnLoad: Boolean;
    FirstTime: Boolean;
    CopyCount: LongInt;
    procedure LoadSettings(Sender: TObject);
    procedure SaveSettings(Sender: TObject);
  public
    { Public declarations }
    procedure SetTimer(Printing: Boolean=False);
    procedure MatchPair(Find: String; Size: LongInt; Style: TFontStyles);
    function FindTag(Tag: String; Beg: LongInt): LongInt;
    function ReplaceText(Beg,Len: LongInt; Txt: String=''): String;
    function CurrText: TTextAttributes;
  end;

var
  Main: TMain;

implementation

{$R *.DFM}


function RegGetRoot(var Path: String): HKEY;
begin
  Result := HKEY_CURRENT_USER;
  if copy(Path,1,4)='HKLM' then
  begin
    Result := HKEY_LOCAL_MACHINE;
    Delete(Path,1,4);
  end else
    if copy(Path,1,4)='HKCU'
    then Delete(Path,1,4);
end;

function RegGetString(Section,Item,Default: String): String;
begin
  with TRegistry.Create do begin

    RootKey := RegGetRoot(Section);

    if OpenKey(Section,FALSE) and (GetDataType(Item)=rdString)
    then Result := ReadString(Item)
    else Result := Default; //Ini.ReadString(Section,Item,'');

    Free;

  end;
end;

function RegGetInteger(Section,Item: String; Default: LongInt): LongInt;
begin
  with TRegistry.Create do begin

    RootKey := RegGetRoot(Section);

    if OpenKey(Section,FALSE) and (GetDataType(Item)=rdInteger)
    then Result := ReadInteger(Item)
    else Result := Default; //Ini.ReadInteger(Section,Item,0);

    Free;

  end;
end;

procedure RegSetString(Section,Item,NewValue: String);
begin
  With TRegistry.Create do begin

    RootKey := RegGetRoot(Section);
    if OpenKey(Section,TRUE)
       then WriteString(Item,NewValue);
    Free;

  End;
end;

procedure RegSetInteger(Section,Item: String; NewValue: Integer);
begin
  With TRegistry.Create do begin

    RootKey := RegGetRoot(Section);
    if OpenKey(Section,TRUE)
       then WriteInteger(Item,NewValue);
    Free;

  End;
end;

function pl2pl(s: string; xIn, xTo: LongInt): string;
const
  aPL : array[plCP852..plUTF8] of string
      = ('§è®ù„‡óçΩ•Ü©à‰¢ò´æ',  // CP852
         '°∆ £—”¶¨Ø±ÊÍ≥nÛ∂ºø',  // ISO
         '•∆ £—”åèØπÊÍ≥nÛúüø',  // WinCE
         'èïêú•£ò†°Üçëí§¢û¶ß',  // Mazovia
         'ACELNOSZZacelnoszz',  // None
         'ACELNOSZZacelnoszz'); // UTF8!!!

  aSG : array[0..1] of string
      = ('ƒÕ∫…Àª» º','-=|*=**=*');
  aUTF: array[1..18] of string
      = ('¶‰','¶Ê','¶ú','+¸','+‚','+Ù','+‹','+¶','+¨','¶˘','¶Á','¶÷','+È','+‰','+-','+ç','+¶','+-');
       //'éÒ','éä','éù','+ÿ','+‘','+¢','+-','+é','+»','é®','é€','éÕ','+⁄','+Ò','+-','+è','+é','+-');
var
  i: LongInt;
begin

  Result := StringReplace(s,#255,#32,[rfReplaceAll]);

  for i := 0 to length(aSG[0])
  do Result := StringReplace(Result,aSG[0,i],aSG[1,i],[rfReplaceAll]);

  if (xIn=plNone) // jesli zrodlo bez polskich znaczkow
  or (xIn=xTo)    // lub zrodlo takie jak wynik
  or (xTo=plUTF8) // konwersji do UTF-8 na razie nie ma
  then Exit;      // to nie ma co robic

  if xIn=plUTF8 then
    for i := 1 to 16
    do Result := StringReplace(Result,aUTF[i],aPL[xTo,i],[rfReplaceAll])
  else
    for i := 0 to length(aPL[xIn])
    do Result := StringReplace(Result,aPL[xIn,i],aPL[xTo,i],[rfReplaceAll]);

end;

procedure TMain.SetTimer(Printing: Boolean);
begin
  if Printing then
  begin
    tm.Enabled := False;
    TrayIcon.Icon := TrayIcon.Icons[2];
  end else if (Path.Text='') or Active then
  begin
    tm.Enabled := False;
    TrayIcon.Icon := TrayIcon.Icons[0];
  end else
  begin
    tm.Enabled := True;
    TrayIcon.Icon := TrayIcon.Icons[1];
  end;
end;

procedure TMain.LoadSettings(Sender: TObject);
var
  Def: String;
begin

  OnLoad    := True;
  try

    Device.Items.Assign(Printer.Printers);

    with Path     do Text      := RegGetString(regRoot,Name,ExtractFilePath(Application.ExeName)+'spool');
    with FontName do ItemIndex := Items.IndexOf(RegGetString(regRoot,Name,Text));
    with FontSize do AsInteger := RegGetInteger(regRoot,Name,AsInteger);
    with Code     do ItemIndex := Items.IndexOf(RegGetString(regRoot,Name,Items[0]));

    if Code.ItemIndex<0 then Code.ItemIndex := 0;

    if Device.Items.Count>0 then
    begin

      if Printer.PrinterIndex<0
      then Def := Printer.Printers[0]
      else Def := Printer.Printers[Printer.PrinterIndex];

      Def := RegGetString(regRoot,Device.Name,Def);

      Device.ItemIndex := Device.Items.IndexOf(Def);
    end;
  finally
    OnLoad    := False;
    SaveSettings(Sender);
    SetDefaults(Sender);
  end;

end;

procedure TMain.SaveSettings(Sender: TObject);
begin
  if not OnLoad then
  begin
    with Device   do RegSetString( regRoot,Name,Text);
    with Path     do RegSetString( regRoot,Name,Text);
    with FontName do RegSetString( regRoot,Name,Text);
    with FontSize do RegSetInteger(regRoot,Name,AsInteger);
    with Code     do RegSetString( regRoot,Name,Text);
  end;
end;

procedure TMain.FormCreate(Sender: TObject);
begin

  FirstTime := True;
  //Caption := VersionInfo.FileDescription+' '+VersionInfo.FileVersion;
  SourceText := TStringList.Create;
  LoadSettings(Sender);

  try
    Buffer.Lines.LoadFromFile(ChangeFileExt(Application.ExeName,'.doc'));
  except
  end;

  SetTimer;

end;

procedure TMain.FormDestroy(Sender: TObject);
begin
  SourceText.Free;
end;

procedure TMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Application.Minimize;
  Abort;
//  SaveSettings(Sender);
end;

function TMain.CurrText: TTextAttributes;
begin
  if Buffer.SelLength > 0 then Result := Buffer.SelAttributes
  else Result := Buffer.DefAttributes;
end;

procedure TMain.DeviceChange(Sender: TObject);
begin
  SaveSettings(Sender);
  Printer.PrinterIndex := Device.ItemIndex;
end;

procedure TMain.FontChange(Sender: TObject);
begin
  SaveSettings(Sender);
  SetDefaults(Sender);
end;

procedure TMain.PathChange(Sender: TObject);
begin
  SaveSettings(Sender);
end;

procedure TMain.CodeChange(Sender: TObject);
begin
  SaveSettings(Sender);
  ParseText(Sender);
end;

procedure TMain.SetDefaults(Sender: TObject);
begin
  if not OnLoad then
  with Buffer do
  try
    SelLength := 0;
    SelStart  := 0;
    DefAttributes.Name  := FontName.Text;
    DefAttributes.Size  := FontSize.AsInteger;
    DefAttributes.Style := [];
    //Text := pl2pl(SourceText.Text,Code.ItemIndex,plWinCE);
    SelectAll;
    SelAttributes.Name  := FontName.Text;
    SelAttributes.Size  := FontSize.AsInteger;
    SelAttributes.Style := [];
    Text := pl2pl(SourceText.Text,Code.ItemIndex,plWinCE);
  finally
    MatchPair('I',0,[fsItalic]);
    MatchPair('B',0,[fsBold  ]);
    MatchPair('D',2,[        ]);
  end;
  //Buffer.SelectAll;

  //Buffer.SelLength := 0;
  //Buffer.Font.Assign(FontDialog.Font);
end;

procedure TMain.ParseText(Sender: TObject);
var x: LongInt;
begin
  Buffer.Text := pl2pl(SourceText.Text,Code.ItemIndex,plWinCE);
  SetDefaults(Sender);
  Status.Caption := format('%s(%d) | %s(%d)',[Buffer.SelAttributes.Name,Buffer.SelAttributes.Size
                                             ,Buffer.DefAttributes.Name,Buffer.DefAttributes.Size]);
  x := FindTag('Copy:',0);
  if x>=0 then CopyCount := StrToIntDef(ReplaceText(x,1),1);
  MatchPair('I',0,[fsItalic]);
  MatchPair('B',0,[fsBold  ]);
  MatchPair('D',2,[        ]);
end;

procedure TMain.PrintBuffer(Sender: String);
var
  x,y,xLine,xCol,xRow,xChar,pHeight: LongInt;

  procedure PageBreak;
  begin
    Printer.NewPage;
    xLine := 0;
    xChar := xChar+1; //#12
    xCol  := 0;
    xRow  := 0;
  end;

  procedure LineBreak;
  begin
    xLine := xLine+1;
    xChar := xChar+2; //#13#10
    xCol  := 0;
    xRow  := xRow+Printer.Canvas.TextWidth('jJ');
    if xRow>pHeight
    then PageBreak;
  end;

  procedure PrintChar(s: String);
  begin
    Printer.Canvas.Font.Assign(Buffer.SelAttributes);
    Printer.Canvas.TextOut(xCol,xRow,s);
    xCol  := xCol + Printer.Canvas.TextWidth(s);
    xChar := xChar+1;
  end;

begin
  Status.Caption       := format('Print %d copie(s) of %s',[CopyCount,Status.Caption]);

  Printer.PrinterIndex := Device.ItemIndex;
  Printer.Orientation  := poPortrait;
  Printer.Copies       := CopyCount;
  Printer.Title        := ExtractFileName(Sender);
  //Buffer.Print(Status.Caption);
  Printer.BeginDoc;
  Printer.Canvas.Font.Assign(Buffer.DefAttributes);

  xCol    := 0;
  xLine   := 0;
  xChar   := 0;
  pHeight := Printer.PageHeight-trunc(1.5*Printer.Canvas.TextHeight('jJ'));
  for y := 0 to Buffer.Lines.Count-1 do
  begin
    for x := 1 to length(Buffer.Lines[y]) do
    begin
      Buffer.SelStart  := xChar;
      Buffer.SelLength := 1;
      if Buffer.SelText=#12
      then PageBreak
      else PrintChar(Buffer.SelText);
    end;
    LineBreak;
  end;

  Printer.EndDoc;

end;

procedure TMain.tmTimer(Sender: TObject);
var SearchRec: TSearchRec;
    FileName: String;
begin
  SetTimer(True);
  if Path.Text<>'' then
  try
    if FindFirst(Path.Text+'\*.*', faArchive, SearchRec) = 0 then
    begin
      repeat
        FileName := Path.Text+'\'+SearchRec.Name;
        Status.Caption := FileName ;
        try
          SourceText.LoadFromFile(FileName);
          //ParseText(Sender);
          SetDefaults(Sender);
          PrintBuffer(FileName);
          DeleteFile(FileName);
        except
          //W razie b≥edÛw plik zostanie pominiÍty
          //bez komunikatu
        end
      until FindNext(SearchRec) <> 0;
    end;
  finally
    FindClose(SearchRec);
  end;
  SetTimer;
end;

procedure TMain.CloseApp(Sender: TObject);
begin
  Application.Terminate;
end;

procedure TMain.AppEventsDeactivate(Sender: TObject);
begin
  Application.Minimize;
end;

procedure TMain.FormActivate(Sender: TObject);
begin
  if FirstTime
  then Application.Minimize;
end;

procedure TMain.AppMinimize(Sender: TObject);
begin
  FirstTime := False;
  ShowWindow(Application.Handle, SW_HIDE);
  SetTimer;
end;

procedure TMain.AppRestore(Sender: TObject);
begin
  ShowWindow(Application.Handle, SW_SHOWNORMAL);
  SetTimer;
end;

function TMain.ReplaceText(Beg,Len: LongInt; Txt: String=''): String;
begin
  Buffer.SelStart  := Beg;
  Buffer.SelLength := Len;
  Result           := Buffer.SelText;
  Buffer.SelText   := Txt;
end;

function TMain.FindTag(Tag: String; Beg: LongInt): LongInt;
var
  Len,Ret: LongInt;
begin

  Len := length(Tag);
  Ret := Buffer.FindText(Tag, Beg, Length(Buffer.Text)-Beg, [stMatchCase]);
  if Ret <> -1 then ReplaceText(Ret,Len);
  Result := Ret;

end;

procedure TMain.MatchPair(Find: String; Size: LongInt; Style: TFontStyles);
var
  BegPos, EndPos: LongInt;
begin

  EndPos  := 0;

  repeat

    BegPos := FindTag('^'+UpperCase(Find), EndPos);
    if BegPos <> -1 then
    begin

      EndPos := FindTag('^'+LowerCase(Find), BegPos);
      if EndPos < 0
      then EndPos := length(Buffer.Text);

      Buffer.SelStart  := BegPos;
      Buffer.SelLength := EndPos-BegPos;
      CurrText.Style   := CurrText.Style + Style;
      CurrText.Size    := CurrText.Size  + Size;
    end;

    Buffer.SelLength := 0

  Until (BegPos=-1) or (EndPos=length(Buffer.Text));
end;


procedure TMain.FormResize(Sender: TObject);
begin
  with Path     do Width := ToolBar.Width-Left-4;
  with Device   do Width := ToolBar.Width-Left-4;
  with FontSize do Left  := ToolBar.Width-Width-4;
  with FontName do Width := FontSize.Left-Left-4;
  with Code     do Width := ToolBar.Width-Left-4;
end;

end.
