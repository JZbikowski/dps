program dps;

uses
  Forms,
  dpsMain in 'dpsMain.pas' {Main};

{$R *.RES}

begin
  Application.Initialize;
  Application.ShowMainForm := False;
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
