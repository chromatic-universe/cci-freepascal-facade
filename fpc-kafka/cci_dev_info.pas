unit cci_dev_info;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls , LCLType;

type

  { Tfrm_dev_info }

  Tfrm_dev_info = class(TForm)
    btn_test_conf1: TButton;
    Button1: TButton;
    btn_test_conf: TButton;
    ed_debug_str: TEdit;
    ed_version: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    procedure btn_test_conf1Click(Sender: TObject);
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
     cci_mini_kafka  , ctypes;

{$R *.lfm}

{ Tfrm_dev_info }

procedure Tfrm_dev_info.Button1Click(Sender: TObject);
begin
         close;
end;

procedure Tfrm_dev_info.btn_test_confClick(Sender: TObject);
var
   conf_ptr : pas_ptr_rd_kafka_conf_t;
   reply : integer;
begin
     conf_ptr :=  cci_mini_kafka.rd_kafka_conf_new();
     if( conf_ptr <> nil ) then
       reply := Application.MessageBox( 'kafka conf object created..' ,
                                        'rdkafka', MB_ICONINFORMATION )
     else
         Application.MessageBox( 'could not create configuration object', 'rdkafka' , MB_ICONHAND );
end;

procedure Tfrm_dev_info.btn_test_conf1Click(Sender: TObject);
var
    desc_arr : array of pas_rd_kafka_err_desc;
    dw_arr   : ctypes.cuint64;
    desc      : longint;
    i        : integer;
begin
     try
             setlength( desc_arr , 256 );
             dw_arr := 16;
             cci_mini_kafka.rd_kafka_get_err_descs( desc_arr , dw_arr );
             for i := 0 to dw_arr - 1  do
             begin
                 desc :=  ord( desc_arr[i].code );
                 writeln( inttostr( desc ) );
             end;
     except
             writeln( '...failed....' );
     end;
     writeln( '..fini...' );
end;

procedure Tfrm_dev_info.FormActivate(Sender: TObject);
begin
     ed_version.Text :=  cci_mini_kafka.rd_kafka_version_str;
     ed_debug_str.Text :=  cci_mini_kafka.rd_kafka_get_debug_contexts;


end;

end.

