unit dp3Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Printers, Mask, ToolEdit, RxCombos, ActnList, RXSpin;

type
  TForm1 = class(TForm)
    memo: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    FilenameEdit1: TFilenameEdit;
    PrinterBox: TComboBox;
    FontBox: TFontComboBox;
    FontSize: TRxSpinEdit;
    Actions: TActionList;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
var
  x,i,l,h,lh: LongInt;
  s: String;

  procedure PageBreak;
  begin
    Printer.NewPage;
    l := 0;
  end;

  procedure PrintLn(s: String);
  begin
    Printer.Canvas.TextOut(0,l,s);
    l := l+lh;
  end;

begin
  Printer.BeginDoc;
  Printer.Canvas.Font.Assign(Memo.Font);
  l := 0;
  lh := trunc(1.5*Printer.Canvas.TextHeight('jJ'));
  h := Printer.PageHeight-lh;
  for i := 0 to memo.lines.count-1 do
  begin
    s := memo.lines[i];
    x := pos(#12,s);
    println(s);
    if (x>0) or (l>h) then PageBreak;
  end;

  Printer.EndDoc;
end;

end.
