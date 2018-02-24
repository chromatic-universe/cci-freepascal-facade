program cci_mini;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, cci_mini_kafka, cci_king_console, cci_dev_info;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(Tfrm_mini_kafka_main, frm_mini_kafka_main);
  Application.CreateForm(Tfrm_dev_info, frm_dev_info);
  Application.Run;
end.

