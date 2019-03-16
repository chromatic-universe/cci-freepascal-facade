program KafkaPas;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FormMainKafkaTest, frrm_message_lst;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMainKafkaTest, frmMainKafkaTest);
  Application.CreateForm(Tfrm_message_lst, frm_message_lst);
  Application.Run;
end.

