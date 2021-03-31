unit curlpp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,libpascurl;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private

  public

  end;

var
    Form1: TForm1;
    buffer : TStringStream;
    h_curl : CURL;
    effectiveUrl, contentType, ip : PChar;
    responseCode, headerSize : Longint;
    contentLength, totalTime : Longword;
    ret :CURLcode;

implementation

{$R *.lfm}

{ TForm1 }
function WriteFunctionCallback (ptr : PChar; size : LongWord;
    nmemb : LongWord; data : Pointer)  : integer;
begin
    buffer.WriteString(string(ptr));
    //writeln( buffer.DataString );
    result := size * nmemb;
end;

procedure TForm1.Button1Click(Sender: TObject);
var
    fs : TFileStream;
begin
      curl_global_init(CURL_GLOBAL_DEFAULT);
      h_curl := curl_easy_init();
      curl_easy_setopt( h_curl , CURLOPT_URL, PChar('https://illwill.com/no-future') );

      buffer := TStringStream.Create('');
      fs := TFileStream.Create( '/home/wiljoh/foo.html', fmCreate );
      curl_easy_setopt( h_curl , CURLOPT_WRITEFUNCTION, @WriteFunctionCallback);
      //curl_easy_setopt( h_curl , CURLOPT_WRITEDATA, buffer);

        try
            ret  :=  curl_easy_perform( h_curl );
            if ret = CURLE_OK then
            begin
              New(effectiveUrl);
              New(contentType);
              New(ip);

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
              fs.WriteBuffer(Pointer(buffer.DataString)^, Length(buffer.DataString));
            end;
        finally
          buffer.free;
          fs.free;
        end;

      curl_global_cleanup;
end;

end.

