unit curlpp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,libpascurl,
  fpjson,cci_curl_pascal;

type

  { Tfrm_pascal_run }

  Tfrm_pascal_run = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;


var
    frm_pascal_run: Tfrm_pascal_run;
    h_curl : CURL;
    effectiveUrl, contentType, ip : PChar;
    responseCode, headerSize : Longint;
    contentLength, totalTime : Longword;
    ret :CURLcode;

implementation

{$R *.lfm}

{ Tfrm_pascal_run }

{DEFINE  MULTI_PERFORM_HANG_TIMEOUT  := 60 * 1000}


function write_function_callback( ptr : PChar; size : LongWord;
    nmemb : LongWord; data : Pointer )  : integer;
var
    strm : TStringStream;
begin

          strm :=  TStringStream( data^ );
          if Assigned( strm ) then
          begin
                strm.WriteString( string( ptr ) );
          end;
          result := size * nmemb;
end;

function debug_trace_callback ( curl : CURL;
                                typ : curl_infotype;
                                data : PCHAR ;
                                size : LongInt ;
                                userp : Pointer ) : integer;
begin
          writeln( string( data ) );
          result := 0;
end;

procedure Tfrm_pascal_run.Button1Click(Sender: TObject);
var
    jsn        : TJSONObject;
    cci_cp    : Tcci_curl_pas;
    endpoint  : string;
    h_curl    : CURL;
    ret :CURLcode;
 begin
          {try
            //
            //construct request
            //
            //init curl environment; once on main thread
            curl_global_init( CURL_GLOBAL_DEFAULT);
            //post data
            jsn := TJSONObject.Create( ['user','giron-d','password','Argentina1'] );
            //dsn - https by default
            endpoint := 'https://chromatic-universe-expansion:7080/mongo/imap2017/plain_text_auth';
            //instantiate                    dsn        debug   verif-peer verify-host  https=default
            cci_cp := Tcci_curl_pas.create(  endpoint , false , false , false );
            //call
            cci_cp.results_by_naked_param( jsn );
            //out
            writeln( cci_cp .stream() );
            //
          finally
            //
            //deinut
            //
            cci_cp.free;
            curl_global_cleanup;
          end;      }


          h_curl := curl_easy_init();
          curl_easy_setopt( h_curl, CURLOPT_USERNAME, 'william.kevin.johnson' );
          curl_easy_setopt(h_curl, CURLOPT_PASSWORD, 'Argentina1' );
          curl_easy_setopt(h_curl, CURLOPT_URL , 'imaps://localhost:993/INBOX/;UID=46' );
          curl_easy_setopt(h_curl, CURLOPT_SSL_VERIFYPEER, 0 );
          curl_easy_setopt(h_curl, CURLOPT_SSL_VERIFYHOST, 0 );
          curl_easy_setopt(h_curl, CURLOPT_VERBOSE, 1 );

          ret := curl_easy_perform( h_curl );
          if ret = CURLE_OK then
          begin
                writeln( 'ok' );
          end;

end;

end.

