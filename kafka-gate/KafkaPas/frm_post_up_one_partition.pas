unit frm_post_up_one_partition;

{$mode objfpc}{$H+}

interface

uses
  classes, sysUtils, fileUtil, forms, controls, graphics, dialogs, stdctrls,
  comctrls, ExtCtrls, Buttons , process , kafka, kafkaclass , termio;

type

  { Tform_post_up_one_partition }

  TLog_level = ( info , notice , error , critical );

  Tform_post_up_one_partition = class(TForm)
          edGroup: TEdit;
          edHost: TEdit;
          edPartition: TEdit;
          edPort: TEdit;
          il_log: TImageList;
          Label1: TLabel;
          Label2: TLabel;
          Label3: TLabel;
          Label4: TLabel;
          lv_log: TListView;
          memo_partition: TMemo;
          Panel1: TPanel;
          Panel2: TPanel;
          Panel3: TPanel;
          SpeedButton1: TSpeedButton;
          Splitter1: TSplitter;
          Splitter2: TSplitter;
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
           procedure post_atom_to_log_view( level : TLog_level; payload : string );
           //procedure kafka_preamble;


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


procedure Tform_post_up_one_partition.post_atom_to_log_view( level : TLog_level; payload : string );
var
  str : string;
  item : TListItem;
begin
        str := '';
        case level of
             info :
               str  := 'INFO';
             notice :
               str := 'NOTICE';
             error :
               str := 'ERROR';
             critical :
               str := 'CRITICAL';
        end;
        {memo_partition.Lines.Add( Format ( '%s: %s... %s...',
                                          [str ,
                                          DateTimeToStr(Now) ,
                                          payload ] ) ); }
        item := lv_log.Items.Add;
        item.caption :=  DateTimeToStr(Now);
        item.subitems.add( str );
        item.subitems.add( payload );
        item.ImageIndex := 2;
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
          memo_partition.Lines.Add( '------------------------------------' );


          //kafka library configuration
          self.conf :=  rd_kafka_conf_new();
          self.post_atom_to_log_view( info , 'kafka library configuation' );
          //log callback
          rd_kafka_conf_set_log_cb( self.conf ,  proc );
          self.post_atom_to_log_view( info , 'logging callback handler'  );
          //quick termination
          fmtstr( s , '%d' , [SIGIO] );
          rd_kafka_conf_set( conf ,
                             'internal.termination.signal' ,
                             PChar( s ),
                             nil ,
                             0 );
          self.post_atom_to_log_view( info , 'quick termination config' );
          //topic configuration
          topic_conf := rd_kafka_topic_conf_new();
          self.post_atom_to_log_view( info , 'new topic config' );
          //debug configuration
          setlength( self.err_str , 512 );
          ret_val := rd_kafka_conf_set(   conf ,
                                        'debug',
                                         debug ,
                                         PChar( self.err_str ) ,
                                         length( self.err_str ) * sizeof( self.err_str[0] ) );
          if ret_val <> RD_KAFKA_CONF_OK then
          begin
              self.post_atom_to_log_view( error, 'error..could not set debug configuation' );
          end
          else
           begin
                self.post_atom_to_log_view( info , 'debug configuation' );
           end;
       except
             on E: Exception do begin
              self.post_atom_to_log_view( error ,  E.Message );
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




