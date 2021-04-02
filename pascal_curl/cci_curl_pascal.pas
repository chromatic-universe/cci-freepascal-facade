// Tcci_curl_pascal     chromatic universe 2021    william k. johnson
unit cci_curl_pascal;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,libpascurl,
    fpjson;

    type
       Tcci_curl_pas = class

       private
           //attributes
           m_debug :         boolean;
           m_https :         boolean;
           m_verify_host :   boolean;
           m_verify_peer :   boolean;
           m_endpoint_dsn  : string;
           m_stream :        TStringStream;

       public
           //ctor
           constructor create( dsn : string;
                               debug : boolean = true;
                               verify_peer : boolean = true;
                               verify_host : boolean = true;
                               https : boolean = true );
           //dtor
           destructor destroy();  override;
           //mutators
           procedure debug( debug : boolean );
           procedure https( https : boolean );
           procedure verify_host( host : boolean );
           procedure verify_peer( peer : boolean );
           procedure endpoint_dsn( dsn  : string );
           //accessors-inspectors
           function debug(): boolean;
           function https(): boolean;
           function verify_host(): boolean;
           function verify_peer(): boolean;
           function endpoint_dsn(): string;
           function stream() : string;
           //services
           procedure results_by_naked_param(  const jsn : TJSONObject );
           //function imap_results_by_custom_request( const cr : string ) : boolean;



    end;
///
//static
//
function debug_trace_callback ( curl : CURL;
                                typ : curl_infotype;
                                data : PCHAR ;
                                size : LongInt ;
                                userp : Pointer ) : integer;
function write_function_callback( ptr : PChar; size : LongWord;
    nmemb : LongWord; data : Pointer )  : integer;

var

    buffer : TStringStream;
    h_curl : CURL;
    effectiveUrl, contentType, ip : PChar;
    responseCode, headerSize : Longint;
    contentLength, totalTime : Longword;
    ret :CURLcode;


implementation


//
//static
//
function debug_trace_callback ( curl : CURL;
                                typ : curl_infotype;
                                data : PCHAR ;
                                size : LongInt ;
                                userp : Pointer ) : integer;
begin
          writeln( string( data ) );
          result := 0;
end;

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
//
//runtime
//
constructor Tcci_curl_pas.create(  dsn : string;
                                   debug : boolean;
                                   verify_peer : boolean;
                                   verify_host : boolean;
                                   https : boolean );
begin
          m_debug := debug;
          m_endpoint_dsn := dsn;
          m_verify_peer := verify_peer;
          m_verify_host := verify_host;
          m_https := https;

end;

destructor Tcci_curl_pas.destroy();
begin
          inherited;
          if assigned( m_stream )then
          begin
               m_stream.free;
          end;
end;

procedure Tcci_curl_pas.results_by_naked_param(  const jsn : TJSONObject );
var
    curl_header_lst :  pcurl_slist;
begin
           if not assigned( m_stream )  then
           begin
                m_stream := TStringStream.create;
           end
           else
             begin
                m_stream.free;
                m_stream := TStringStream.create;
              end;
          //
          //construct request
          //
          curl_header_lst := nil;
          //init local curl stack
          h_curl := curl_easy_init();
          //headers  data
          curl_header_lst := curl_slist_append( curl_header_lst ,  PChar( 'Content-Type: application/json' ) );
          curl_easy_setopt( h_curl, CURLOPT_HTTPHEADER , curl_header_lst );
          //post data
          curl_easy_setopt( h_curl, CURLOPT_POSTFIELDS , PChar( jsn.FormatJson ) );
          //destination url
          curl_easy_setopt( h_curl , CURLOPT_URL, PChar( m_endpoint_dsn ) );
          curl_easy_setopt( h_curl , CURLOPT_FAILONERROR , 1 );
          //set write callback
          curl_easy_setopt( h_curl , CURLOPT_WRITEFUNCTION, @write_function_callback );
          //write callback elastic buffer
          curl_easy_setopt( h_curl , CURLOPT_WRITEDATA, @m_stream );
          //ssl
          if m_https = true then
          begin
            //skip peer verification
            if m_verify_peer = false  then curl_easy_setopt( h_curl , CURLOPT_SSL_VERIFYPEER, 0 );
            //skip host verification
            if m_verify_host = false then curl_easy_setopt( h_curl , CURLOPT_SSL_VERIFYHOST, 0 );
          end;
          //debug( follow redirects )
          if m_debug = true then
          begin
            curl_easy_setopt( h_curl , CURLOPT_VERBOSE,  1 );
            curl_easy_setopt( h_curl , CURLOPT_DEBUGFUNCTION, @debug_trace_callback );
            //curl_easy_setopt( h_curl , CURLOPT_FOLLOWLOCATION, 1 );
          end;
          //
          //perform
          //
          try
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
                  end;
                except
                  on e: EOutOfMemory do
                    writeln(' Out of memory!');
                  on e: EAccessViolation do
                    writeln(' Access violation! in Tcci_curl_pas.results_by_naked_param' );
                end;
          finally
              //deinit
              curl_slist_free_all( curl_header_lst );
          end;
end;

procedure Tcci_curl_pas.debug( debug : boolean );
begin
          m_debug := debug;
end;

function Tcci_curl_pas.debug()  : boolean;
begin
          result :=  m_debug;
end;

procedure Tcci_curl_pas.https( https : boolean );
begin
          m_https := https;
end;

function Tcci_curl_pas.https()  : boolean;
begin
          result :=  m_https;
end;

procedure Tcci_curl_pas.verify_host( host : boolean );
begin
          m_verify_host := host;
end;

function Tcci_curl_pas.verify_host(): boolean;
begin
          result := m_verify_host;
end;

procedure Tcci_curl_pas.verify_peer( peer : boolean );
begin
          m_verify_peer := peer;
end;

function Tcci_curl_pas.verify_peer(): boolean;
begin
          result := m_verify_peer;
end;

procedure Tcci_curl_pas.endpoint_dsn( dsn : string );
begin
          m_endpoint_dsn := dsn;
end;

function Tcci_curl_pas.endpoint_dsn(): string;
begin
          result := m_endpoint_dsn;
end;

function Tcci_curl_pas.stream() : string;
begin
          result := m_stream.DataString;
end;

end.

