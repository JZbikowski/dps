unit dpsEdit;

interface

uses
ComCtrls;

type
TdpsRichEdit = class(TRichEdit)
public
procedure Print(const ACaption: String); reintroduce;
end;

implementation

uses
Windows, RichEdit, Printers;

procedure TdpsRichEdit.Print(const ACaption: String);
begin
end;

end.