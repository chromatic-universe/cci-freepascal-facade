program KafkaPas;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, FormMainKafkaTest, frrm_message_lst, about, frm_post_up_one_partition;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TfrmMainKafkaTest, frmMainKafkaTest);
  Application.CreateForm(Tfrm_message_lst, frm_message_lst);
  Application.CreateForm(Tfrm_about, frm_about);
  Application.CreateForm(Tform_post_up_one_partition, form_post_up_one_partition
    );
  Application.Run;
end.

