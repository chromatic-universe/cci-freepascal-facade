unit frm_post_up_one_partition;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls , Process , Kafka, KafkaClass , termio;

type

  { Tform_post_up_one_partition }

  Tform_post_up_one_partition = class(TForm)
          btn_post_up: TButton;
          edHost: TEdit;
          edPartition: TEdit;
          edPort: TEdit;
          Label1: TLabel;
          Label2: TLabel;
          Label3: TLabel;
          Memo1: TMemo;
          StatusBar1: TStatusBar;
          procedure btn_post_upClick(Sender: TObject);
          procedure FormCreate(Sender: TObject);
          procedure FormDestroy(Sender: TObject);
          procedure GroupBox1Click(Sender: TObject);
          //


        private

           brokers               : PChar;
           mode                  : PChar;
           debug                 : PChar;
           group                 : PChar;
           opt                   : integer;
           is_subscirption       : integer;
           err                   : Trd_kafka_resp_err_t;
           rk_ptr                : Prd_kafka_t;
           conf                  : Prd_kafka_conf_t;
           topic_conf            : Prd_kafka_topic_conf_t;
           quiet                 : integer;
           ptr_log_cb            : PProc_log_cb;

        public

           procedure kafka_init_vars;


  end;

  procedure proc_log_cb( rk: Prd_kafka_t; level: Int32; fac: PChar;buf: PChar ); cdecl;

var
  form_post_up_one_partition: Tform_post_up_one_partition;
const
  BUF_SIZE = 2048;
  SIGIO = 20;

implementation

{$R *.lfm}

{ Tform_post_up_one_partition }

procedure proc_log_cb( rk: Prd_kafka_t; level: Int32; fac: PChar;buf: PChar ); cdecl ;
begin

end;

procedure Tform_post_up_one_partition.GroupBox1Click(Sender: TObject);
begin

end;

procedure Tform_post_up_one_partition.btn_post_upClick(Sender: TObject);
begin
      try

      finally

      end;
end;

procedure Tform_post_up_one_partition.kafka_init_vars;
var
  proc : PProc_log_cb;
  s    : string;
begin
      try
          self.brokers := 'localhost:9092';
          self.mode := 'C';
          self.debug := nil;
          self.group := nil;
          self.quiet := not termio.IsATTY( 0 );
          new( proc );
          proc := PProc_log_cb( @proc_log_cb );

          //kafka configuration
          self.conf :=  rd_kafka_conf_new();
          //log callback
          rd_kafka_conf_set_log_cb( self.conf ,  proc );
          //quick termination
          fmtstr( s , '%d' , [SIGIO] );
          rd_kafka_conf_set( conf ,
                             'internal.termination.signal' ,
                             PChar( s ),
                             nil ,
                             0 );
          //topic configuration


       except
             on E: Exception do begin
               //
             end;
       end;

end;




procedure Tform_post_up_one_partition.FormCreate(Sender: TObject);
begin

     self.kafka_init_vars;

end;

procedure Tform_post_up_one_partition.FormDestroy(Sender: TObject);
begin
       if self.ptr_log_cb <> nil then
       begin
         dispose( self.ptr_log_cb );
       end;
end;




end.




