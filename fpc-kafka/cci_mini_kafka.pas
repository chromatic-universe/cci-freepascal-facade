// librdkafka - Apache Kafka C library
//
// Copyright (c) 2012-2013 Magnus Edenhill
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice,
//    this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
///
// @file rdkafka.h
// @brief Apache Kafka C/C++ consumer and producer client library.
//
// rdkafka.h contains the public API for librdkafka.
// The API is documented in this file as comments prefixing the function, type,
// enum, define, etc.
//
// @sa For the C++ interface see rdkafkacpp.h
//

//cci_mini_kafka    chromatic unviverse 2018  william k .johnson
//
//librdkafka wrapper facade for free pascal
//
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

    ////enums
    //
    pas_rd_kakfa_type_t = (
                           //producer cliebt
                           rd_kafka_producer ,
                           //consumer client
                           rd_kafka_consumer
                         );
    ////typedefs
    //
    ptr_pas_rd_kafka_t                             =  type pointer;
    //pas_rd_kafka_topic_s                       = ^pas_rd_kafka_topic_t;
    ptr_pas_rd_kafka_conf_t                        =  type pointer;
    //ptr_pas_rd_kafka_topic_conf_s                   = ^pas_rd_kafka_topic_conf_t;
    //ptr_pas_rd_kafka_queue_s                        = ^pas_rd_kafka_queue_t;


    //
    pss_rd_kafka_timestamp_type_t = (
                                      //timestamp not available
                                      rd_kafka_timestamp_not_available ,
                                      //message cration time
                                      rd_kafka_timestamp_create_time ,
                                      //log append time
                                      rd_kafka_timestamp_log_append_time
                                    );

    ////exports
    //
    // returns the librdkafka version as string.
    //
    // returns version string
    //
    function rd_kafka_version_str : PAnsiChar ; cdecl;
    // retrieve supported debug contexts for use with the \c \"debug\"
    //        configuration property. (runtime)
    //
    // returns comma-separated list of available debugging contexts.
    //
    function rd_kafka_get_debug_contexts : PAnsiChar ; cdecl;
    /////
    // create configuration object.
    //
    // When providing your own configuration to the \c rd_kafka_//_new_//() calls
    // the rd_kafka_conf_t objects needs to be created with this function
    // which will set up the defaults.
    // I.e.:
    // @code
    //   pas_rd_kafka_conf_t //myconf;
    //   pas_rd_kafka_conf_res_t res;
    //
    //   myconf = rd_kafka_conf_new();
    //   res = rd_kafka_conf_set(myconf, "socket.timeout.ms", "600",
    //                           errstr, sizeof(errstr));
    //   if (res != RD_KAFKA_CONF_OK)
    //      die("%s\n", errstr);
    //
    //   rk = rd_kafka_new(..., myconf);
    // @endcode
    //
    // Please see CONFIGURATION.md for the default settings or use
    // rd_kafka_conf_properties_show() to provide the information at runtime.
    //
    // The properties are identical to the Apache Kafka configuration properties
    // whenever possible.
    //
    // @returns A new rd_kafka_conf_t object with defaults set.
    //
    // @sa rd_kafka_conf_set(), rd_kafka_conf_destroy()
    //
    function  rd_kafka_conf_new : ptr_pas_rd_kafka_conf_t ;  cdecl;




implementation


function rd_kafka_version_str : PAnsiChar ; cdecl;   external;
//
function rd_kafka_get_debug_contexts : PAnsiChar ; cdecl;  external;
//
function rd_kafka_conf_new : ptr_pas_rd_kafka_conf_t ;  cdecl; external;

end.

