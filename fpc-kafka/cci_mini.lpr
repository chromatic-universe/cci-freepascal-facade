program cci_mini;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, rdkafka, cci_dev_info, cci_about, cci_king_console;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(Tfrm_cci_mini, frm_cci_mini);
  Application.CreateForm(Tfrm_dev_info, frm_dev_info);
  Application.CreateForm(Tfrm_about, frm_about);
  Application.Run;
end.

