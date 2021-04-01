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

    O : TJSONObject;
    curl_header_lst :  pcurl_slist;
    buffer : TStringStream;
begin
          //
          //construct request
          //
          curl_header_lst := nil;
          //init curl environment; once on main thread
          curl_global_init( CURL_GLOBAL_DEFAULT);
          //init local curl stack
          h_curl := curl_easy_init();
          //headers  data
          curl_header_lst := curl_slist_append( curl_header_lst ,  PChar( 'Content-Type: application/json' ) );
          curl_easy_setopt( h_curl, CURLOPT_HTTPHEADER , curl_header_lst );
          //post data
          O := TJSONObject.Create( ['user','william.kevin.johnson','password','Argentina1'] );
          curl_easy_setopt( h_curl, CURLOPT_POSTFIELDS , PChar( O.FormatJson ) );
          //destination url
          curl_easy_setopt( h_curl , CURLOPT_URL, PChar('https://localhost:7080/mongo/imap2017/plain_text_auth') );
          curl_easy_setopt( h_curl , CURLOPT_FAILONERROR , 1 );
          //set write callback
          curl_easy_setopt( h_curl , CURLOPT_WRITEFUNCTION, @write_function_callback );
          //write callback elastic buffer
          buffer := TStringStream.Create( '' );
          curl_easy_setopt( h_curl , CURLOPT_WRITEDATA, @buffer );
          //skip peer verification
          curl_easy_setopt( h_curl , CURLOPT_SSL_VERIFYPEER, 0 );
          //skip host verification
          curl_easy_setopt( h_curl , CURLOPT_SSL_VERIFYHOST, 0 );
          //debug( follow redirects )_
          curl_easy_setopt( h_curl , CURLOPT_VERBOSE,  1 );
          curl_easy_setopt( h_curl , CURLOPT_DEBUGFUNCTION, @debug_trace_callback );
          curl_easy_setopt( h_curl , CURLOPT_FOLLOWLOCATION, 1 );
          //
          //perform
          //
          try
                ret  :=  curl_easy_perform( h_curl );
                if ret = CURLE_OK then
                begin
                  New(effectiveUrl);
                  New(contentType);
                  New(ip);
                  //
                  //out
                  //
                  curl_easy_getinfo(h_curl, CURLINFO_EFFECTIVE_URL, @effectiveUrl);
                  curl_easy_getinfo(h_curl, CURLINFO_RESPONSE_CODE, @responseCode);
                  curl_easy_getinfo(h_curl, CURLINFO_HEADER_SIZE, @headerSize);
                  curl_easy_getinfo(h_curl, CURLINFO_CONTENT_TYPE, @contentType);
                  curl_easy_getinfo(h_curl, CURLINFO_CONTENT_LENGTH_DOWNLOAD_T, @contentLength);
                  curl_easy_getinfo(h_curl, CURLINFO_LOCAL_IP, @ip);
                  curl_easy_getinfo(h_curl, CURLINFO_TOTAL_TIME_T, @totalTime);

                  writeln('URL: ':20,                 effectiveUrl);
                  writeln('Response code: ':20,       responseCode);
                  writeln('Header size, kB: ':20,     FormatFloat('0.00', headerSize / 1024));
                  writeln('Content type: ',           contentType);
                  writeln('Content length, kB: ':20,  FormatFloat('0.00', contentLength / 1024));
                  writeln('IP: ':20,                  ip);
                  writeln('Total time, ms: ':20,      totalTime);
                  writeln('==== Content ====');
                  writeln(buffer.DataString);

                end;
          finally
              //deinit
              buffer.free;
              curl_slist_free_all( curl_header_lst );
              curl_global_cleanup;
          end;


end;

end.

