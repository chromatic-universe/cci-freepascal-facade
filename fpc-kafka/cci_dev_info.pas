unit cci_dev_info;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls , LCLType;

type

  { Tfrm_dev_info }

  Tfrm_dev_info = class(TForm)
    Button1: TButton;
    btn_test_conf: TButton;
    ed_debug_str: TEdit;
    ed_version: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    procedure btn_test_confClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormActivate(Sender: TObject);
  private

  public

  end;

var
  frm_dev_info: Tfrm_dev_info;

implementation

uses
     cci_mini_kafka;

{$R *.lfm}

{ Tfrm_dev_info }

procedure Tfrm_dev_info.Button1Click(Sender: TObject);
begin
         close;
end;

procedure Tfrm_dev_info.btn_test_confClick(Sender: TObject);
var
   conf_ptr : ptr_pas_rd_kafka_conf_t;
   reply : integer;
begin
     conf_ptr :=  cci_mini_kafka.rd_kafka_conf_new();
     if( conf_ptr <> nil ) then
       reply := Application.MessageBox( 'kafka conf object created..' ,
                                        'rdkafka', MB_ICONINFORMATION )
     else
         Application.MessageBox( 'could not create configuration object', 'rdkafka' , MB_ICONHAND );
end;

procedure Tfrm_dev_info.FormActivate(Sender: TObject);
begin
     ed_version.Text :=  cci_mini_kafka.rd_kafka_version_str;
     ed_debug_str.Text :=  cci_mini_kafka.rd_kafka_get_debug_contexts;


end;

end.

