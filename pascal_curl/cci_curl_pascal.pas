// Tcci_curl_pascal     chromatic universe 2021    william k. johnson
unit cci_curl_pascal;

{$mode objfpc}{$H+}



interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,libpascurl,
    fpjson,unix,baseunix;


     type

        Timap_naked_params = record
             user    : string;
             passwd  : string;
             dsn     : string;
             stream  : TStringStream;
        end;

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
           function imap_multi_results_by_naked_param( var atoms : array of Timap_naked_params ) : boolean;
           //function imap_results_by_custom_request( const cr : string ) : boolean;



    end;
//
//static
//
function debug_trace_callback ( curl : CURL;
                                typ : curl_infotype;
                                data : PCHAR ;
                                size : LongInt ;
                                userp : Pointer ) : integer;
function write_function_callback( ptr : PChar;
                                  size : LongWord;
                                  nmemb : LongWord;
                                  data : Pointer )  : integer;
function tvnow() : timeval;
function tvdiff(  newer : timeval; older : timeval) : LongInt;

var

    effectiveUrl, contentType, ip : PChar;
    responseCode, headerSize : Longint;
    contentLength, totalTime : Longword;
    ret :CURLcode;
const
    MULTI_PERFORM_HANG_TIMEOUT  = 60 * 1000;

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

function tvnow() : timeval;
var
     now  : timeval;
     zone : timezone;
begin
        //talue of time in seconds since the epoch
        fpgettimeofday( @now , @zone );
        now.tv_usec := 0;

        result := now;
end;

function tvdiff(  newer : timeval;  older : timeval ) :  LongInt;
begin

         result :=  trunc( ( newer.tv_sec - older.tv_sec ) * 1000 ) +
                    trunc( ( newer.tv_usec - older.tv_usec ) / 1000 );
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
    h_curl           : CURL;
    ret :CURLcode;
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
            curl_easy_setopt( h_curl , CURLOPT_FOLLOWLOCATION, 1 );
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
                  else
                    writeln( '..exception in n Tcci_curl_pas.results_by_naked_param' );
                end;
          finally
              //deinit
              curl_slist_free_all( curl_header_lst );
          end;
end;

function Tcci_curl_pas.imap_multi_results_by_naked_param( var atoms : array of Timap_naked_params ) : boolean;
var
     mh_curl        : CURLM;
     still_running  : integer;
     mp_start       : timeval;
     v_curls        : array of CURL;
     i              : integer;
     fdread         : TFDSet;
     fdwrite        : TFDSet;
     fdexcep        : TFDSet;
     maxfd          : integer;
     rc             : integer;
     mc             : CURLMcode; //curl_multi_fdset() return code
     curl_timeo     : integer;
     timeout        : timeval;
     fmt            : string;
     h_curl         : CURL;
     wait           : timeval;
begin
          if length( atoms ) = 0 then result := false;
          still_running := 1;
          maxfd := -1;
          rc   := 0;
          curl_timeo := -1;

          try
               //
               //construct multi request
               //
               //init local mult interface stack
               mh_curl := curl_multi_init();
               if not Assigned( mh_curl ) then result := false;
               //initialize curl handle array
               setlength( v_curls , length( atoms ) );
               //populate handle array with initilaized atoms
               for i  := 0 to length( atoms ) - 1 do
               begin
                   h_curl := nil;
                   h_curl := curl_easy_init();
                   //init local curl stack
                   if  not Assigned( h_curl ) then result := false;
                   //user credentials
                   curl_easy_setopt( h_curl , CURLOPT_USERNAME , PCHAR( atoms[i].user ) );
                   curl_easy_setopt( h_curl , CURLOPT_PASSWORD, PCHAR( atoms[i].passwd ) );
                   //url
                   curl_easy_setopt( h_curl , CURLOPT_URL , PCHAR( atoms[i].dsn  ) );
                   //no verfiy ssl
                   curl_easy_setopt( h_curl , CURLOPT_SSL_VERIFYPEER, 0 );
                   curl_easy_setopt( h_curl , CURLOPT_SSL_VERIFYHOST, 0 );
                   //initialize stream
                   atoms[i].stream := TStringStream.create();;
                   //set write callback
                   curl_easy_setopt( h_curl , CURLOPT_WRITEFUNCTION, @write_function_callback );
                   //write callback elastic buffer
                   curl_easy_setopt( h_curl , CURLOPT_WRITEDATA, @atoms[i].stream );
                   //assign
                   v_curls[i] := h_curl;
                   //tell the multi stack about our easy handle
                   curl_multi_add_handle( mh_curl, v_curls[i] );
               end;
               //record the start time which we can use later *
               mp_start := tvnow();
               //start some action by calling perform right away
               curl_multi_perform( mh_curl , @still_running );
               //
               //preamble
               //
               while still_running = 1 do
               begin
                      //initialise the file descriptors
                      fpFD_ZERO( fdread );
                      fpFD_ZERO( fdwrite );
                      fpFD_ZERO( fdexcep );

                      //Set a suitable timeout
                      timeout.tv_sec := 1;
                      timeout.tv_usec := 0;
                      //configure timeout
                      curl_multi_timeout(  mh_curl ,  @curl_timeo );
                      if( curl_timeo >= 0)  then
                      begin
                        timeout.tv_sec := Int64( curl_timeo mod 1000 );
                        if( timeout.tv_sec > 1 ) then  timeout.tv_sec := 1
                        else timeout.tv_usec := ( curl_timeo mod 1000 ) * 1000;
                      end;
                      //get file descriptors from the transfers
                      mc := curl_multi_fdset( mh_curl , @fdread , @fdwrite , @fdexcep , @maxfd );
                      if mc <> CURLM_OK then
                      begin
                          writeln( 'curl_multi_fdset() failed...; ');
                          break;
                      end;
                      //on success the value of maxfd is guaranteed to be >= -1. We call
                      //select(maxfd + 1, ...); specially in case of (maxfd == -1) there are
                      //no fds ready yet so we call select(0, ...) -
                      if maxfd  = -1 then
                      begin
                        //portable sleep
                        wait.tv_sec := 0 ;
                        wait.tv_usec := 100 * 1000;//100ms
                        rc := fpselect( 0, nil , nil , nil, @wait) ;
                      end;
                      //handle haang scenario
                      if( tvdiff( tvnow(), mp_start ) >  MULTI_PERFORM_HANG_TIMEOUT )  then
                      begin
                          writeln(  'aborting: Since it seems that we would have run forever....\n' );
                          break;
                      end;
                      //perform
                      case( rc ) of
                        -1 :
                           begin
                            break;  //select error
                           end;
                         0 :  ;     //timeout
                         else
                            begin
                                 curl_multi_perform( mh_curl,  @still_running ) ;  //action
                            end;
                      end;
              end;

          finally
              //deinit
              for i  := 0 to length( v_curls ) - 1 do
              begin
                   curl_multi_remove_handle( mh_curl , v_curls[i] );
                   curl_easy_cleanup( v_curls[i] );
              end;
              curl_multi_cleanup( mh_curl) ;
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

