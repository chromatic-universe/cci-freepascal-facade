unit cci_king_console;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    btn_kafka: TButton;
    tx_kafka_version: TEdit;
    procedure btn_kafkaClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;



var
  Form1: TForm1;

implementation

 uses
  cci_mini_kafka;

{$R *.lfm}

{ TForm1 }

procedure TForm1.btn_kafkaClick(Sender: TObject);
begin
         tx_kafka_version.text := cci_mini_kafka.rd_kafka_version_str;
end;

end.

