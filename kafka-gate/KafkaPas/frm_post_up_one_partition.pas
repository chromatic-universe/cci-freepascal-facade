unit frm_post_up_one_partition;

{$mode objfpc}{$H+}

interface

uses
  classes, sysUtils, fileUtil, forms, controls, graphics, dialogs, stdctrls,
  comctrls , process , kafka, kafkaclass , termio;

type

  { Tform_post_up_one_partition }

  Tform_post_up_one_partition = class(TForm)
          btn_post_up: TButton;
          edHost: TEdit;
          edPartition: TEdit;
          edGroup: TEdit;
          edPort: TEdit;
          Label1: TLabel;
          Label2: TLabel;
          Label3: TLabel;
          Label4: TLabel;
          memoPartition: TMemo;
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
           err_str               : Array of char;

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
  ret_val : integer;
begin
      try
          self.brokers := nil;
          self.mode := 'C';
          self.debug := 'broker,topic';
          self.group := 'cci_stream_ecosys';
          self.quiet := not termio.IsATTY( 0 );
          new( proc );
          proc := PProc_log_cb( @proc_log_cb );
          ret_val := 0;
          s := '';
          memoPartition.Lines.Add( '------------------------------------' );
          memoPartition.Lines.Add( Format ('%s: ',[DateTimeToStr(Now)]) );

          //kafka library configuration
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
          topic_conf := rd_kafka_topic_conf_new();
          //debug configuration
          setlength( self.err_str , 512 );
          ret_val := rd_kafka_conf_set(   conf ,
                                        'debug',
                                         debug ,
                                         PChar( self.err_str ) ,
                                         length( self.err_str ) * sizeof( self.err_str[0] ) );
          if ret_val <> RD_KAFKA_CONF_OK then
          begin
              Application.MessageBox( 'error..could not set debug configuation...,' ,
               'post up one parition' );
          end;

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




