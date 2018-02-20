//cci_mini_kafka    chromatic unvierse 2018  william k .johnson
//librdkafka wrapper facade for free pascal

unit cci_mini_kafka;

{$mode objfpc}{$H+}
{$linklib rdkafka}
{$linklib cci_kafka_utils}
{$linklib c}


interface

uses CTypes , sysutils;

type
    {$IFDEF FPC}
    {$PACKRECORDS C}
    {$ENDIF}

    //enums
    pas_rdkakfa_type_t = (rd_kafka_producer , rd_kafka_consumer);


    procedure snacks; cdecl;
    //fetch kafka version string
    function rd_kafka_version_str:PAnsiChar ; cdecl;


implementation

procedure snacks; cdecl; external;
function rd_kafka_version_str:PAnsiChar ; cdecl;   external;

end.

