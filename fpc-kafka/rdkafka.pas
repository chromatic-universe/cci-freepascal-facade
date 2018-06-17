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

/////////////////////////////////////////////////////////////////////////////////
//                                                                             //
// rdkafka.pas    chromatic unviverse 2018  william k .johnson                 //
//                                                                             //
// librdkafka wrapper facade for free pascal                                   //
//                                                                             //
//                                                                             //
/////////////////////////////////////////////////////////////////////////////////
unit rdkafka;

{$mode objfpc}{$H+}
{$linklib rdkafka}
{$linklib cci_kafka_utils}
{$linklib c}
{$INLINE ON}

interface

uses ctypes , sysutils , sockets , unix;

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
    //
    pas_ptr_rd_kafka_timestamp_type_t =  ^pas_rd_kafka_timestamp_type_t;
    pas_rd_kafka_timestamp_type_t = (
                                      //timestamp not available
                                      rd_kafka_timestamp_not_available ,
                                      //message cration time
                                      rd_kafka_timestamp_create_time ,
                                      //log append time
                                      rd_kafka_timestamp_log_append_time
                                    );
     //
     //
     // @enum rd_kafka_conf_res_t
     // coonfiguration result type
     //
     pas_ptr_rd_kafka_conf_res_t = ^pas_rd_kafka_conf_res_t;
     pas_rd_kafka_conf_res_t =  (
                                      //unknown configuration name
	                              rd_kafka_conf_unknown = -2 ,
                                      //invalid configuration value
	                              rd__kafka_conf_invalid = -1 ,
                                      //configuration okay
	                              rd_kafka_conf_ok = 0
                                );


    //
    //
    // @enum rd_kafka_resp_err_t
    // @brief Error codes.
    //
    // The negative error codes delimited by two underscores
    // (\c RD_KAFKA_RESP_ERR__..) denotes errors internal to librdkafka and are
    // displayed as \c \"Local: \<error string..\>\", while the error codes
    // delimited by a single underscore (\c RD_KAFKA_RESP_ERR_..) denote broker
    // errors and are displayed as \c \"Broker: \<error string..\>\".
    //
    // @sa Use rd_kafka_err2str() to translate an error code a human readable string
    ///
    pas_rd_kafka_resp_err_t =      (
	                            //internal errors to rdkafka:
	                            //Begin internal error codes
	                            rd_kafka_resp_err__begin = -200,
	                            //received message is incorrect
	                            rd_kafka_resp_err__bad_msg = -199 ,
                                    //bad/unknown compression
	                            rd_kafka_resp_err__bad_compression = -198 ,
	                            //broker is going away
	                            rd_kafka_resp_err__destroy = -197 ,
	                            //generic failure
	                            rd_kafka_resp_err__fail = -196 ,
	                            //broker transport failure
	                            rd_kafka_resp_err__transport = -195 ,
	                            //critical system resource
	                            rd_kafka_resp_err__crit_sys_resource = -194 ,
	                            //failed to resolve broker
	                            rd_kafka_resp_err__resolve = -193 ,
	                            //produced message timed out
	                            rd_kafka_resp_err__msg_timed_out = -192 ,
	                            //reached the end of the topic+partition queue on
	                            //the broker. not really an error.
	                            rd_kafka_resp_err__partition_eof = -191 ,
	                            //permanent: partition does not exist in cluster.
	                            rd_kafka_resp_err__unknown_partition = -190 ,
	                            //file or filesystem error
	                            rd_kafka_resp_err__fs = -189 ,
	                            //permanent: topic does not exist in cluster.
	                            rd_kafka_resp_err__unknown_topic = -188 ,
	                            //all broker connections are down.
	                            rd_kafka_resp_err__all_brokers_down = -187 ,
	                            //invalid argument , or invalid configuration
	                            rd_kafka_resp_err__invalid_arg = -186 ,
	                            //operation timed out
	                            rd_kafka_resp_err__timed_out = -185 ,
	                            //queue is full
	                            rd_kafka_resp_err__queue_full = -184 ,
	                            //isr count < required.acks
                                    rd_kafka_resp_err__isr_insuff = -183 ,
	                            //broker node update
                                    rd_kafka_resp_err__node_update = -182 ,
	                            //ssl error
	                            rd_kafka_resp_err__ssl = -181 ,
	                            //waiting for coordinator to become available.
                                    rd_kafka_resp_err__wait_coord = -180 ,
	                            //unknown client group
                                    rd_kafka_resp_err__unknown_group = -179 ,
	                            //operation in progress
                                    rd_kafka_resp_err__in_progress = -178 ,
	                            //previous operation in progress , wait for it to finish.
                                    rd_kafka_resp_err__prev_in_progress = -177 ,
	                            //this operation would interfere with an existing subscription
                                    rd_kafka_resp_err__existing_subscription = -176 ,
	                            //assigned partitions (rebalance_cb)
                                    rd_kafka_resp_err__assign_partitions = -175 ,
	                            //revoked partitions (rebalance_cb)
                                    rd_kafka_resp_err__revoke_partitions = -174 ,
	                            //conflicting use
                                    rd_kafka_resp_err__conflict = -173 ,
	                            //wrong state
                                    rd_kafka_resp_err__state = -172 ,
	                            //unknown protocol
                                    rd_kafka_resp_err__unknown_protocol = -171 ,
	                            //not implemented
                                    rd_kafka_resp_err__not_implemented = -170 ,
	                            //authentication failure
	                            rd_kafka_resp_err__authentication = -169 ,
	                            //no stored offset
	                            rd_kafka_resp_err__no_offset = -168 ,
	                            //outdated
	                            rd_kafka_resp_err__outdated = -167 ,
	                            //timed out in queue
	                            rd_kafka_resp_err__timed_out_queue = -166 ,
                                    //feature not supported by broker
                                    rd_kafka_resp_err__unsupported_feature = -165 ,
                                    //awaiting cache update
                                    rd_kafka_resp_err__wait_cache = -164 ,
                                    //operation interrupted (e.g. , due to yield))
                                    rd_kafka_resp_err__intr = -163 ,
                                    //key serialization error
                                    rd_kafka_resp_err__key_serialization = -162 ,
                                    //value serialization error
                                    rd_kafka_resp_err__value_serialization = -161 ,
                                    //key deserialization error
                                    rd_kafka_resp_err__key_deserialization = -160 ,
                                    //value deserialization error
                                    rd_kafka_resp_err__value_deserialization = -159 ,
	                            //end internal error codes
	                            rd_kafka_resp_err__end = -100 ,
	                            //kafka broker errors:
	                            //unknown broker error
	                            rd_kafka_resp_err_unknown = -1 ,
	                            //success
	                            rd_kafka_resp_err_no_error = 0 ,
	                            //offset out of range
	                            rd_kafka_resp_err_offset_out_of_range = 1 ,
	                            //invalid message
	                            rd_kafka_resp_err_invalid_msg = 2 ,
	                            //unknown topic or partition
	                            rd_kafka_resp_err_unknown_topic_or_part = 3 ,
	                            //invalid message size
	                            rd_kafka_resp_err_invalid_msg_size = 4 ,
	                            //leader not available
	                            rd_kafka_resp_err_leader_not_available = 5 ,
	                            //not leader for partition
	                            rd_kafka_resp_err_not_leader_for_partition = 6 ,
	                            //request timed out
	                            rd_kafka_resp_err_request_timed_out = 7 ,
	                            //broker not available
	                            rd_kafka_resp_err_broker_not_available = 8 ,
	                            //replica not available
	                            rd_kafka_resp_err_replica_not_available = 9 ,
	                            //message size too large
	                            rd_kafka_resp_err_msg_size_too_large = 10 ,
	                            //stalecontrollerepochcode
	                            rd_kafka_resp_err_stale_ctrl_epoch = 11 ,
	                            //offset metadata string too large
	                            rd_kafka_resp_err_offset_metadata_too_large = 12 ,
	                            //broker disconnected before response received
	                            rd_kafka_resp_err_network_exception = 13 ,
	                            //group coordinator load in progress
                                    rd_kafka_resp_err_group_load_in_progress = 14 ,
	                             //group coordinator not available
                                    rd_kafka_resp_err_group_coordinator_not_available = 15 ,
	                            //not coordinator for group
                                    rd_kafka_resp_err_not_coordinator_for_group = 16 ,
	                            //invalid topic
                                    rd_kafka_resp_err_topic_exception = 17 ,
	                            //message batch larger than configured server segment size
                                    rd_kafka_resp_err_record_list_too_large = 18 ,
	                            //not enough in-sync replicas
                                    rd_kafka_resp_err_not_enough_replicas = 19 ,
	                            //message(s) written to insufficient number of in-sync replicas
                                    rd_kafka_resp_err_not_enough_replicas_after_append = 20 ,
	                            //invalid required acks value
                                    rd_kafka_resp_err_invalid_required_acks = 21 ,
	                            //specified group generation id is not valid
                                    rd_kafka_resp_err_illegal_generation = 22 ,
	                            //inconsistent group protocol
                                    rd_kafka_resp_err_inconsistent_group_protocol = 23 ,
	                            //invalid group.id
	                            rd_kafka_resp_err_invalid_group_id = 24 ,
	                            //unknown member
                                    rd_kafka_resp_err_unknown_member_id = 25 ,
	                            //invalid session timeout
                                    rd_kafka_resp_err_invalid_session_timeout = 26 ,
	                            //group rebalance in progress
	                            rd_kafka_resp_err_rebalance_in_progress = 27 ,
	                            //commit offset data size is not valid
                                    rd_kafka_resp_err_invalid_commit_offset_size = 28 ,
	                            //topic authorization failed
                                    rd_kafka_resp_err_topic_authorization_failed = 29 ,
	                            //group authorization failed
	                            rd_kafka_resp_err_group_authorization_failed = 30 ,
	                            //cluster authorization failed
	                            rd_kafka_resp_err_cluster_authorization_failed = 31 ,
	                            //invalid timestamp
	                            rd_kafka_resp_err_invalid_timestamp = 32 ,
	                            //unsupported sasl mechanism
	                            rd_kafka_resp_err_unsupported_sasl_mechanism = 33 ,
	                            //illegal sasl state
	                            rd_kafka_resp_err_illegal_sasl_state = 34 ,
	                            //unuspported version
	                            rd_kafka_resp_err_unsupported_version = 35 ,
	                            //topic already exists
	                            rd_kafka_resp_err_topic_already_exists = 36 ,
	                            //invalid number of partitions
	                            rd_kafka_resp_err_invalid_partitions = 37 ,
	                            //invalid replication factor
	                            rd_kafka_resp_err_invalid_replication_factor = 38 ,
	                            //invalid replica assignment
	                            rd_kafka_resp_err_invalid_replica_assignment = 39 ,
	                            //invalid config
	                            rd_kafka_resp_err_invalid_config = 40 ,
	                            //not controller for cluster
	                            rd_kafka_resp_err_not_controller = 41 ,
	                            //invalid request
	                            rd_kafka_resp_err_invalid_request = 42 ,
	                            //message format on broker does not support request
	                            rd_kafka_resp_err_unsupported_for_message_format = 43 ,
                                    //isolation policy volation
                                    rd_kafka_resp_err_policy_violation = 44 ,
                                    //broker received an out of order sequence number
                                    rd_kafka_resp_err_out_of_order_sequence_number = 45 ,
                                    //broker received a duplicate sequence number
                                    rd_kafka_resp_err_duplicate_sequence_number = 46 ,
                                    //producer attempted an operation with an old epoch
                                    rd_kafka_resp_err_invalid_producer_epoch = 47 ,
                                    //producer attempted a transactional operation in an invalid state
                                    rd_kafka_resp_err_invalid_txn_state = 48 ,
                                    //producer attempted to use a producer id which is not
                                    //currently assigned to its transactional id
                                    rd_kafka_resp_err_invalid_producer_id_mapping = 49 ,
                                    //transaction timeout is larger than the maximum
                                    //value allowed by the broker's max.transaction.timeout.ms
                                    rd_kafka_resp_err_invalid_transaction_timeout = 50 ,
                                    //producer attempted to update a transaction while another
                                    //concurrent operation on the same transaction was ongoing
                                    rd_kafka_resp_err_concurrent_transactions = 51 ,
                                    //indicates that the transaction coordinator sending a
                                    //writetxnmarker is no longer the current coordinator for a
                                    //given producer
                                    rd_kafka_resp_err_transaction_coordinator_fenced = 52 ,
                                    //transactional id authorization failed
                                    rd_kafka_resp_err_transactional_id_authorization_failed = 53 ,
                                    //security features are disabled
                                    rd_kafka_resp_err_security_disabled = 54 ,
                                    //operation not attempted
                                    rd_kafka_resp_err_operation_not_attempted = 55 ,
	                            rd_kafka_resp_err_end_all
                                   );
    ////typedefs
    //
    //private abi
    //
    pas_ptr_rd_kafka_t                             =  type pointer;
    pas_ptr_rd_kafka_headers_t                     =  type pointer;
    pas_rd_kafka_topic_t                           =  type pointer;
    pas_ptr_rd_kafka_conf_t                        =  type pointer;
    ptr_pas_rd_kafka_topic_conf_t                  =  type pointer;
    ptr_pas_rd_kafka_queue_t                       =  type pointer;
    //forward declarations
    pas_ptr_rd_kafka_topic_partition_list_t        =  ^pas_rd_kafka_topic_partition_list_t;
    pas_ptr_rd_kafka_message_t                     =  ^pas_rd_kafka_message_t;
    pas_ptr_ref_char_t                             =  ^PAnsiChar;
    ////function pointers & callbacks
    //
    pas_ptr_pas_t_compare_func = ^pas_t_compare_func;
    pas_t_compare_func = function( const a : pointer;
                                   const b : pointer;
                                   const opaque : pointer ) : ctypes.cint32; cdecl;
    //
    pas_ptr_pas_dr_msg_cb = ^pas_dr_msg_cb;
    pas_dr_msg_cb = procedure( rk : pas_ptr_rd_kafka_t;
                               rkmessage : pas_ptr_rd_kafka_message_t;
                               opaque : pointer );
    //
    pas_ptr_pas_rebalance_cb = ^pas_rebalance_cb;
    pas_rebalance_cb = procedure(  rk : pas_ptr_rd_kafka_t;
                                   err : pas_rd_kafka_resp_err_t;
                                   partitions : pas_ptr_rd_kafka_topic_partition_list_t;
                                   opaque : pointer );
    //
    pas_ptr_pas_consume_cb = ^pas_consume_cb;
    pas_consume_cb = procedure(  rk_message : pas_ptr_rd_kafka_message_t;
                                 opaque : pointer );
    //
    pas_ptr_pas_offset_commit_cb = ^pas_offset_commit_cb;
    pas_offset_commit_cb = procedure(  rk : pas_ptr_rd_kafka_t;
                                       err : pas_rd_kafka_resp_err_t;
                                       partitions : pas_ptr_rd_kafka_topic_partition_list_t;
                                       opaque : pointer );
    //
    pas_ptr_pas_error_cb  = ^pas_error_cb;
    pas_error_cb = procedure( rk : pas_ptr_rd_kafka_t;
                              err : ctypes.cuint64;
                              const reason : PAnsiChar;
                              opaque : pointer );
    //
    pas_ptr_pas_log_cb = ^pas_log_cb;
    pas_log_cb = procedure(  rk : pas_ptr_rd_kafka_t;
                             const fac : PAnsiChar;
                             const buf : PAnsiChar );
    //
    pas_ptr_pas_stats_cb = ^pas_stats_cb;
    pas_stats_cb = function( rk : pas_ptr_rd_kafka_t;
                             json : PAnsiChar;
                             json_len : ctypes.cuint64;
                             opaque : pointer ) : ctypes.cint64;
    //
    pas_ptr_pas_socket_cb = ^pas_socket_cb;
    pas_socket_cb = function( domain : ctypes.cint64;
                              typ : ctypes.cint64;
                              protocol : ctypes.cint64;
                              opaque : pointer ) : ctypes.cint64;
    //
    pas_ptr_pas_connect_cb = ^pas_connect_cb;
    pas_ptr_sock_unix_sockaddr  = ^TUnixSockAddr;
    pas_connect_cb = function( sockfd : ctypes.cint32;
                               const addr : pas_ptr_sock_unix_sockaddr;
                               addrlen : ctypes.cint64;
                               const id : PAnsiChar;
                               opaque : pointer ) : ctypes.cint64;
    //
    pas_ptr_pas_closesocket_cb = ^pas_closesocket_cb;
    pas_closesocket_cb = function( sockfd : ctypes.cint32;
                                   opaque : pointer ) : ctypes.cint64;
    //
    pas_ptr_open_cb = ^pas_open_cb;
    pas_open_cb = function( const pathname : PAnsiChar;
                            flags : cint32;
                            mode  : mode_t;
                            opaque : pointer ) : ctypes.cint64;
    //
    pas_ptr_topic_partition_cb = ^topic_partition_cb;
    topic_partition_cb = function( topic_conf : ptr_pas_rd_kafka_topic_conf_t;
				   const rkt : pas_rd_kafka_topic_t;
				   const keydata : pointer;
                                   keylen : ctypes.cint32;
				   partition_cnt : ctypes.cuint32;
                                   rkt_opaque : pointer;
                                   msg_opaque : pointer ) : ctypes.cint32;
    //
    pas_ptr_msg_order_cmp_func = ^msg_order_cmp_func;
    msg_order_cmp_func = function(  a :pas_ptr_rd_kafka_message_t;
                                    b :pas_ptr_rd_kafka_message_t ) : ctypes.cint32;


    ////records
    //
    //error code value, name and description.
    //typically for use with language bindings to automatically expose
    //the full set of librdkafka error codes.
    pas_ptr_rd_kafka_err_desc = ^pas_rd_kafka_err_desc;
    pas_rd_kafka_err_desc = record
       code : pas_rd_kafka_resp_err_t;
       name : PAnsiChar;
       desc : PAnsiChar;
    end;

     // topic partition place holder
     //
     // generic place holder for a Topic+Partition and its related information
     // used for multiple purposes:
     //   - consumer offset (see rd_kafka_commit(), et.al.)
     //   - group rebalancing callback (rd_kafka_conf_set_rebalance_cb())
     //   - offset commit result callback (rd_kafka_conf_set_offset_commit_cb())
     //
     // generic place holder for a specific Topic+Partition.
     //
     // rd_kafka_topic_partition_list_new()
     ///
     pas_ptr_rd_kafka_topic_partition_t  = ^pas_rd_kafka_topic_partition_t;
     pas_rd_kafka_topic_partition_t = record
            topic   : PAnsiChar;                  // topic name
            partition : ctypes.cint32;            // partition
	    offset   : ctypes.cint64;             // offset
            metadata :  pointer;                  // metadata
            metadata_size : ctypes.cuint64;       // metadata size
            opaque   : pointer;                   // application opaque
            err      : pas_rd_kafka_resp_err_t;   // error code, depending on use.
            _private : pointer;                   // INTERNAL USE ONLY,
     end;                                         // INITIALIZE TO ZERO, DO NOT TOUCH

     // a growable list of topic partitions.
     pas_rd_kafka_topic_partition_list_t = record
        cnt :  ctypes.cint32;                       //current number of elements
        size : ctypes.cint32;                       //vurrent allocated size
        elems : pas_ptr_rd_kafka_topic_partition_t; //element array[]
     end;


     // FIXME: this doesn't show up in docs for some reason
     // cCompound rd_kafka_message_t is not documented."
     // a kafka message as returned by the \c rd_kafka_consume//() family
     //        of functions as well as provided to the Producer \c dr_msg_cb().
     //
     // for the consumer this object has two purposes:
     //  - provide the application with a consumed message. (\c err == 0)
     //  - report per-topic+partition consumer errors (\c err != 0)
     //
     // the application must check \c err to decide what action to take.
     //
     // when the application is finished with a message it must call
     // rd_kafka_message_destroy() unless otherwise noted.
     //
     pas_rd_kafka_message_t  = record
	    err : pas_rd_kafka_resp_err_t;   /////< Non-zero for error signaling. ///
	    rkt : pas_ptr_rd_kafka_t;        /////< Topic ///
	    partition : ctypes.cint32;       /////< Partition ///
	    payload : pointer;               /////< Producer: original message payload.
				             // Consumer: Depends on the value of \c err :
				             // - \c err==0: Message payload.
				             // - \c err!=0: Error string ///
	    len : ctypes.cint64;             /////< Depends on the value of \c err :
	                                     // - \c err==0: Message payload length
				             // - \c err!=0: Error string length ///
	    key :  pointer;                  /////< Depends on the value of \c err :
				             // - \c err==0: Optional message key ///
	    key_len : ctypes.cint64;         /////< Depends on the value of \c err :
				             // - \c err==0: Optional message key length///
	    offset  : ctypes.cint64;         /////< Consume:
                                             // - Message offset (or offset for error
				             //   if \c err!=0 if applicable).
                                             // - dr_msg_cb:
                                             //   Message offset assigned by broker.
                                             //   If \c produce.offset.report is set then
                                             //   each message will have this field set,
                                             //   otherwise only the last message in
                                             //   each produced internal batch will
                                             //   have this field set, otherwise 0. ///
	    _private : pointer;              //_private;           /////< Consume:
				             //  - rdkafka private pointer: DO NOT MODIFY
				             //  - dr_msg_cb:
                                             //    msg_opaque from produce() call ///
    end;

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

    // returns the error code name (enum name).
    // param err Error code to translate
    function rd_kafka_err2name ( err : pas_rd_kafka_resp_err_t ) : PAnsiChar;  cdecl;

    // returns a human readable representation of a kafka error.
    // @param err Error code to translate
    function rd_kafka_err2str ( err : pas_rd_kafka_resp_err_t ) : PAnsiChar; cdecl;

     //  returns the last error code generated by a legacy API call
     //        in the current thread.
     //
     // The legacy APIs are the ones using errno to propagate error value, namely:
     //  - rd_kafka_topic_new()
     //  - rd_kafka_consume_start()
     //  - rd_kafka_consume_stop()
     //  - rd_kafka_consume()
     //  - rd_kafka_consume_batch()
     //  - rd_kafka_consume_callback()
     //  - rd_kafka_consume_queue()
     //  - rd_kafka_produce()
     //
     // The main use for this function is to avoid converting system \p errno
     // values to rd_kafka_resp_err_t codes for legacy APIs.
     //
     // The last error is stored per-thread, if multiple rd_kafka_t handles
     // are used in the same application thread the developer needs to
     // make sure rd_kafka_last_error() is called immediately after
     // a failed API call.
     //
     // errno propagation from librdkafka is not safe on Windows
     // and should not be used, use rd_kafka_last_error() instead.
     function rd_kafka_last_error : pas_rd_kafka_resp_err_t; cdecl;

     //returns the full list of error codes.
     procedure rd_kafka_get_err_descs( var errdescs : array of pas_rd_kafka_err_desc;
			               var cntp : ctypes.cuint64 ); cdecl;

     // destroy a rd_kafka_topic_partition_t.
     // this must not be called for elements in a topic partition list.
     //
     procedure rd_kafka_topic_partition_destroy ( rktpar : pas_ptr_rd_kafka_topic_partition_t ); cdecl;


     // create a new list/vector topic+partition container.
     //
     // @param size  initial allocated size used when the expected number of
     //              elements is known or can be estimated.
     //              Avoids reallocation and possibly relocation of the
     //              elems array.
     //
     // @returns a newly allocated topic+partition list.
     //
     // @remark use rd_kafka_topic_partition_list_destroy() to free all resources
     //         in use by a list and the list itself.
     // @sa     rd_kafka_topic_partition_list_add()
     //
     function rd_kafka_topic_partition_list_new ( size : ctypes.cint32 )
                                                 : pas_ptr_rd_kafka_topic_partition_list_t; cdecl;

     // free all resources used by the list and the list itself.
     //
     procedure rd_kafka_topic_partition_list_destroy ( rkparlist : pas_ptr_rd_kafka_topic_partition_list_t ); cdecl;

     // @brief add topic+partition to list
     //
     // @param rktparlist list to extend
     // @param topic      topic name (copied)
     // @param partition  partition id
     //
     // @returns The object which can be used to fill in additionals fields.
     function rd_kafka_topic_partition_list_add ( rkparlist : pas_ptr_rd_kafka_topic_partition_list_t;
                                                  const topic  : PAnsiChar;
                                                  partition : ctypes.cint32 ) : pas_ptr_rd_kafka_topic_partition_t; cdecl;

     // @brief add range of partitions from \p start to \p stop inclusive.
     //
     // @param rktparlist list to extend
     // @param topic      topic name (copied)
     // @param start      start partition of range
     // @param stop       last partition of range (inclusive)
     //
     procedure rd_kafka_topic_partition_list_add_range ( rktparlist : pas_ptr_rd_kafka_topic_partition_list_t;
                                                         const topic  : PAnsiChar;
                                                         start : ctypes.cint32;
                                                         stop  : ctypes.cint32 ); cdecl;

     // delete partition from list.
     //
     // @param rktparlist list to modify
     // @param topic      topic name to match
     // @param partition  partition to match
     //
     // @returns 1 if partition was found (and removed), else 0.
     //
     // @remark Any held indices to elems[] are unusable after this call returns 1.
     //
     function rd_kafka_topic_partition_list_del( rktparlist : pas_ptr_rd_kafka_topic_partition_list_t;
				                 const topic : PAnsichar;
                                                 partition : ctypes.cint32 ) : ctypes.cint32; cdecl;


     // delete partition from list by elems[] index.
     //
     // @returns 1 if partition was found (and removed), else 0.
     //
     // @sa rd_kafka_topic_partition_list_del()
     //
     function rd_kafka_topic_partition_list_del_by_idx ( rktparlist : pas_ptr_rd_kafka_topic_partition_list_t;
                                                         idx : ctypes.cint32 ) : ctypes.cint32; cdecl;



     // make a copy of an existing list.
     //
     // @param src   the existing list to copy.
     //
     // @returns a new list fully populated to be identical to \p src
     //
     function  rd_kafka_topic_partition_list_copy ( const src : pas_ptr_rd_kafka_topic_partition_list_t )
                                                 : pas_ptr_rd_kafka_topic_partition_list_t; cdecl;


     // set offset to \p offset for \p topic and \p partition
     //
     // @returns RD_KAFKA_RESP_ERR_NO_ERROR on success or
     //          RD_KAFKA_RESP_ERR__UNKNOWN_PARTITION if \p partition was not found
     //          in the list.
     //
     function rd_kafka_topic_partition_list_set_offset ( rktparlist : pas_ptr_rd_kafka_topic_partition_list_t;
	     	                                         const topic : PAnsiChar;
                                                         partition : ctypes.cint32;
                                                         offset : ctypes.cint64 ) : pas_rd_kafka_resp_err_t; cdecl;


     // find element by \p topic and \p partition.
     //
     // @returns a pointer to the first matching element, or NULL if not found.
     //
     function rd_kafka_topic_partition_list_find ( rktparlist : pas_ptr_rd_kafka_topic_partition_list_t;
				                   const topic : PAnsiChar;
                                                   partition : ctypes.cint32 ) : pas_ptr_rd_kafka_topic_partition_t; cdecl;


     // @brief sort list using comparator \p cmp.
     //
     // If \p cmp is NULL the default comparator will be used that
     // sorts by ascending topic name and partition.
     //
     procedure rd_kafka_topic_partition_list_sort( rktparlist : pas_ptr_rd_kafka_topic_partition_list_t;
                                                   cmp : pas_ptr_pas_t_compare_func;
                                                   opaque : pointer ); cdecl;



      // frees resources for \p rkmessage and hands ownership back to rdkafka.
      //
      procedure rd_kafka_message_destroy( rkmessage : pas_ptr_rd_kafka_message_t ); cdecl;


      // returns the error string for an errored rd_kafka_message_t or NULL if
      //        there was no error.
      //
      // @remark This function MUST NOT be used with the producer.
      //
      function  rd_kafka_message_errstr( const rkmessage : pas_ptr_rd_kafka_message_t ) : PAnsiChar ; inline;

      // returns the message timestamp for a consumed message.
      //
      // the timestamp is the number of milliseconds since the epoch (UTC).
      //
      // \p tstype (if not NULL) is updated to indicate the type of timestamp.
      //
      // @returns message timestamp, or -1 if not available.
      //
      // @remark message timestamps require broker version 0.10.0 or later.
      //
      function rd_kafka_message_timestamp( const rkmessage : pas_ptr_rd_kafka_message_t;
				           tstype : pas_ptr_rd_kafka_timestamp_type_t ) : ctypes.cint64; cdecl;


      // returns the latency for a produced message measured from
      //        the produce() call.
      //
      // @returns the latency in microseconds, or -1 if not available.
      //
      function rd_kafka_message_latency (const rkmessage : pas_ptr_rd_kafka_message_t ) : ctypes.cint64;  cdecl;


      // get the message header list.
      //
      // the returned pointer in \p //hdrsp is associated with the \p rkmessage and
      // must not be used after destruction of the message object or the header
      // list is replaced with rd_kafka_message_set_headers().
      //
      // @returns RD_KAFKA_RESP_ERR_NO_ERROR if headers were returned,
      //          RD_KAFKA_RESP_ERR__NOENT if the message has no headers,
      //          or another error code if the headers could not be parsed.
      //
      // @remark headers require broker version 0.11.0.0 or later.
      //
      // @remark as an optimization the raw protocol headers are parsed on
      //         the first call to this function.
      function  rd_kafka_message_headers ( const message : pas_ptr_rd_kafka_message_t;
                                           var hdrsp : pas_ptr_rd_kafka_headers_t ) : pas_rd_kafka_resp_err_t; cdecl;



       // get the message header list and detach the list from the message
       //        making the application the owner of the headers.
       //        The application must eventually destroy the headers using
       //        rd_kafka_headers_destroy().
       //        The message's headers will be set to NULL.
       //
       //        Otherwise same semantics as rd_kafka_message_headers()
       //
       // @sa rd_kafka_message_headers
       //
       function rd_kafka_message_detach_headers( message : pas_ptr_rd_kafka_message_t;
                                                 var hdrsp : pas_ptr_rd_kafka_headers_t ) : pas_rd_kafka_resp_err_t; cdecl;


       // replace the message's current headers with a new list.
       //
       // @param hdrs New header list. The message object assumes ownership of
       //             the list, the list will be destroyed automatically with
       //             the message object.
       //             The new headers list may be updated until the message object
       //             is passed or returned to librdkafka.
       //
       // @remark The existing headers object, if any, will be destroyed.
       ///
       procedure rd_kafka_message_set_headers ( rkmessage : pas_ptr_rd_kafka_message_t;
                                                hdrs : pas_ptr_rd_kafka_headers_t ) cdecl;


       // returns the number of header key/value pairs
       //
       // @param hdrs   Headers to count
       ///
       function rd_kafka_header_cnt ( const hdrs : pas_ptr_rd_kafka_headers_t )   : ctypes.cint64; cdecl;


       // vceate configuration object.
       //
       // when providing your own configuration to the \c rd_kafka_//_new_//() calls
       // the rd_kafka_conf_t objects needs to be created with this function
       // which will set up the defaults.
       // I.e.:
       // @code
       //   rd_kafka_conf_t //myconf;
       //   rd_kafka_conf_res_t res;
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
       // please see CONFIGURATION.md for the default settings or use
       // rd_kafka_conf_properties_show() to provide the information at runtime.
       //
       // the properties are identical to the Apache Kafka configuration properties
       // whenever possible.
       //
       // @returns A new rd_kafka_conf_t object with defaults set.
       //
       // @sa rd_kafka_conf_set(), rd_kafka_conf_destroy()
       //
       function rd_kafka_conf_new : pas_ptr_rd_kafka_conf_t; cdecl;


       //
       // dstroys a conf object.
       procedure rd_kafka_conf_destroy( conf : pas_ptr_rd_kafka_conf_t ); cdecl;

       // creates a copy/duplicate of configuration object \p conf
       //
       // @remark Interceptors are NOT copied to the new configuration object.
       // @sa rd_kafka_interceptor_f_on_conf_dup
       //
       function rd_kafka_conf_dup( const conf : pas_ptr_rd_kafka_conf_t ) : pas_ptr_rd_kafka_conf_t; cdecl;

       //
       //  same as rd_kafka_conf_dup() but with an array of property name
       //        prefixes to filter out (ignore) when copying.
       //
       function rd_kafka_conf_dup_filter ( const conf : pas_ptr_rd_kafka_conf_t;
                                           filter_cnt : ctypes.cuint64;
                                           var filter : PAnsiChar ) : pas_ptr_rd_kafka_conf_t; cdecl;

       //
       // sets a configuration property.
       //
       // \p conf must have been previously created with rd_kafka_conf_new().
       //
       // fallthrough:
       // Topic-level configuration properties may be set using this interface
       // in which case they are applied on the \c default_topic_conf.
       // If no \c default_topic_conf has been set one will be created.
       // Any sub-sequent rd_kafka_conf_set_default_topic_conf() calls will
       // replace the current default topic configuration.
       //
       // @returns \c rd_kafka_conf_res_t to indicate success or failure.
       // In case of failure \p errstr is updated to contain a human readable
       // error string.
       ///
       function rd_kafka_conf_set( const conf : pas_ptr_rd_kafka_conf_t;
				   const name : PAnsiChar;
				   const value : PAnsiChar;
				   const errstr : PAnsiChar;
                                   errstr_size : ctypes.cuint64 ) : pas_rd_kafka_conf_res_t ; cdecl;

       //
       // enable event sourcing.
       // events is a bitmask of RD_KAFKA_EVENT_* of events to enable
       // for consumption by `rd_kafka_queue_poll()`.
       //
       procedure rd_kafka_conf_set_events( conf : pas_ptr_rd_kafka_conf_t; events : ctypes.cuint64 ); cdecl;

       //
       // rd_kafka_conf_set_dr_cb
       // deprecated See rd_kafka_conf_set_dr_msg_cb()
       //
       //
       // deprecated ->skipped   <willian k. johnson>
       //

       //
       //  producer: Set delivery report callback in provided \p conf object.
       //
       // the delivery report callback will be called once for each message
       // accepted by rd_kafka_produce() (et.al) with \p err set to indicate
       // the result of the produce request.
       //
       // the callback is called when a message is succesfully produced or
       // if librdkafka encountered a permanent failure, or the retry counter for
       // temporary errors has been exhausted.
       //
       // an application must call rd_kafka_poll() at regular intervals to
       // serve queued delivery report callbacks.
       //
       procedure rd_kafka_conf_set_dr_msg_cb( conf : pas_ptr_rd_kafka_conf_t;
                                              msg_cb : pas_ptr_pas_dr_msg_cb ); cdecl;


       //
       //  consumer: set consume callback for use with rd_kafka_consumer_poll()
       //
       //
       procedure rd_kafka_conf_set_consume_cb ( conf : pas_ptr_rd_kafka_conf_t;
                                                consume_cb : pas_ptr_pas_consume_cb ); cdecl;



       //
       //  consumer: set rebalance callback for use with
       //                     coordinated consumer group balancing.
       //
       // the \p err field is set to either RD_KAFKA_RESP_ERR__ASSIGN_PARTITIONS
       // or RD_KAFKA_RESP_ERR__REVOKE_PARTITIONS and 'partitions'
       // contains the full partition set that was either assigned or revoked.
       //
       // registering a \p rebalance_cb turns off librdkafka's automatic
       // partition assignment/revocation and instead delegates that responsibility
       // to the application's \p rebalance_cb.
       //
       // The rebalance callback is responsible for updating librdkafka's
       // assignment set based on the two events: RD_KAFKA_RESP_ERR__ASSIGN_PARTITIONS
       // and RD_KAFKA_RESP_ERR__REVOKE_PARTITIONS but should also be able to handle
       // arbitrary rebalancing failures where \p err is neither of those.
       // @remark In this latter case (arbitrary error), the application must
       //         call rd_kafka_assign(rk, NULL) to synchronize state.
       //
       // without a rebalance callback this is done automatically by librdkafka
       // but registering a rebalance callback gives the application flexibility
       // in performing other operations along with the assinging/revocation,
       // such as fetching offsets from an alternate location (on assign)
       // or manually committing offsets (on revoke).
       //
       // @remark The \p partitions list is destroyed by librdkafka on return
       //         return from the rebalance_cb and must not be freed or
       //         saved by the application.
       //
       // the following example shows the application's responsibilities:
       // @code
       //    static void rebalance_cb (rd_kafka_t //rk, rd_kafka_resp_err_t err,
       //                              rd_kafka_topic_partition_list_t //partitions,
       //                              void //opaque) {
       //
       //        switch (err)
       //        {
       //          case RD_KAFKA_RESP_ERR__ASSIGN_PARTITIONS:
       //             // application may load offets from arbitrary external
       //             // storage here and update \p partitions
       //
       //             rd_kafka_assign(rk, partitions);
       //             break;
       //
       //          case RD_KAFKA_RESP_ERR__REVOKE_PARTITIONS:
       //             if (manual_commits) // Optional explicit manual commit
       //                 rd_kafka_commit(rk, partitions, 0); // sync commit
       //
       //             rd_kafka_assign(rk, NULL);
       //             break;
       //
       //          default:
       //             handle_unlikely_error(err);
       //             rd_kafka_assign(rk, NULL); // sync state
       //             break;
       //         }
       //    }
       // @endcode
       //
       procedure rd_kafka_conf_set_rebalance_cb ( conf : pas_ptr_rd_kafka_conf_t;
                                                  rebalance_cb : pas_ptr_pas_rebalance_cb ); cdecl;

       //
       // consumer: Set offset commit callback for use with consumer groups.
       //
       // the results of automatic or manual offset commits will be scheduled
       // for this callback and is served by rd_kafka_consumer_poll().
       //
       // if no partitions had valid offsets to commit this callback will be called
       // with \p err == RD_KAFKA_RESP_ERR__NO_OFFSET which is not to be considered
       // an error.
       //
       // the \p offsets list contains per-partition information:
       //   - \c offset: committed offset (attempted)
       //   - \c err:    commit error
       ///
       procedure rd_kafka_conf_set_offset_commit_cb( conf : pas_ptr_rd_kafka_conf_t;
                                                     offset_commit_cb : pas_ptr_pas_offset_commit_cb ); cdecl;


       //
       // set error callback in provided conf object.
       //
       // the error callback is used by librdkafka to signal critical errors
       // back to the application.
       //
       // if no error_cb is registered then the errors will be logged instead.
       //
       procedure rd_kafka_conf_set_error_cb( conf : pas_ptr_rd_kafka_conf_t;
				             error_cb : pas_ptr_pas_error_cb ); cdecl;


       //
       // set logger callback.
       //
       // the default is to print to stderr, but a syslog logger is also available,
       // see rd_kafka_log_print and rd_kafka_log_syslog for the builtin alternatives.
       // Alternatively the application may provide its own logger callback.
       // Or pass func as NULL to disable logging.
       //
       // This is the configuration alternative to the deprecated rd_kafka_set_logger()
       //
       // @remark The log_cb will be called spontaneously from librdkafka's internal
       //         threads unless logs have been forwarded to a poll queue through
       //         \c rd_kafka_set_log_queue().
       //         An application MUST NOT call any librdkafka APIs or do any prolonged
       //         work in a non-forwarded \c log_cb.
       //
       procedure rd_kafka_conf_set_log_cb( conf : pas_ptr_rd_kafka_conf_t;
			                   log_cb : pas_ptr_pas_log_cb ); cdecl;

       //
       // set statistics callback in provided conf object.
       //
       // the statistics callback is triggered from rd_kafka_poll() every
       // statistics.interval.ms (needs to be configured separately).
       // function arguments:
       //   - rk - Kafka handle
       //   - json - String containing the statistics data in JSON format
       //   - json_len - Length of json string.
       //   - opaque - application-provided opaque.
       //
       // if the application wishes to hold on to the json pointer and free
       // it at a later time it must return 1 from the stats_cb.
       // if the application returns 0 from the stats_cb then librdkafka
       // will immediately free the json pointer.
       //
       procedure  rd_kafka_conf_set_stats_cb( conf : pas_ptr_rd_kafka_conf_t;
                                              stats_cb : pas_ptr_pas_stats_cb ); cdecl;

       //
       // set socket callback.
       //
       // the socket callback is responsible for opening a socket
       // according to the supplied domain, type and protocol.
       // The socket shall be created with \c CLOEXEC set in a racefree fashion, if
       // possible.
       //
       // default:
       //  - on linux: racefree CLOEXEC
       //  - others  : non-racefree CLOEXEC
       //
       // @remark The callback will be called from an internal librdkafka thread.
       //
       procedure rd_kafka_conf_set_socket_cb( conf : pas_ptr_rd_kafka_conf_t;
                                              socket_cb : pas_ptr_pas_socket_cb ); cdecl;


       //
       // set connect callback.
       //
       // The connect callback is responsible for connecting socket \p sockfd
       // to peer address \p addr.
       // The id field contains the broker identifier.
       //
       // connect_cb shall return 0 on success (socket connected) or an error
       // number (errno) on error.
       //
       // @remark The callback will be called from an internal librdkafka thread.
       //
       procedure rd_kafka_conf_set_connect_cb( conf : pas_ptr_rd_kafka_conf_t;
                                               connect_cb :  pas_ptr_pas_connect_cb ); cdecl;

       //
       // set close socket callback.
       //
       // close a socket (optionally opened with socket_cb()).
       //
       // @remark The callback will be called from an internal librdkafka thread.
       //
       procedure rd_kafka_conf_set_closesocket_cb( conf : pas_ptr_rd_kafka_conf_t;
                                                   close_socket_cb :  pas_ptr_pas_closesocket_cb ); cdecl;

       {$IFNDEF _MSC_VER}
       //
       // set open callback.
       //
       // the open callback is responsible for opening the file specified by
       // pathname, flags and mode.
       // the file shall be opened with \c CLOEXEC set in a racefree fashion, if
       // possible.
       //
       // default:
       //  - on linux: racefree CLOEXEC
       //  - others  : non-racefree CLOEXEC
       //
       // @remark The callback will be called from an internal librdkafka thread.
       //
       procedure rd_kafka_conf_set_open_cb ( conf : pas_ptr_rd_kafka_conf_t;
                                             open_cb : pas_ptr_open_cb ); cdecl;

       {$ENDIF}


       //
       // sets the application's opaque pointer thai will be passed to callbacks
       //
       procedure rd_kafka_conf_set_opaque( conf : pas_ptr_rd_kafka_conf_t;
                                           opaque : pointer );  cdecl;

       //
       // retrieves the opaque pointer previously set with rd_kafka_conf_set_opaque()
       //
       function rd_kafka_opaque( const rk : pas_ptr_rd_kafka_t ) : pointer;  cdecl;

       //
       // Ssts the default topic configuration to use for automatically
       // subscribed topics (e.g., through pattern-matched topics).
       // the topic config object is not usable after this call.
       //
       procedure rd_kafka_conf_set_default_topic_conf( conf : pas_ptr_rd_kafka_conf_t;
                                                       tconf : ptr_pas_rd_kafka_topic_conf_t ); cdecl;


       //
       // retrieve configuration value for property \p name.
       //
       // if dest is non-NULL the value will be written to \p dest with at
       // most dest_size.
       //
       // dest_size is updated to the full length of the value, thus if
       // dest_size initially is smaller than the full length the application
       // may reallocate \p dest to fit the returned \p //dest_size and try again.
       //
       // if dest is NULL only the full length of the value is returned.
       //
       // fallthrough:
       // topic-level configuration properties from the \c default_topic_conf
       // may be retrieved using this interface.
       //
       // @returns \p RD_KAFKA_CONF_OK if the property name matched, else
       // RD_KAFKA_CONF_UNKNOWN.
       //
       function rd_kafka_conf_get( conf : pas_ptr_rd_kafka_conf_t;
                                   const name : PAnsiChar;
                                   dest : PAnsiChar;
                                   var dest_size : ctypes.cuint64 ) : pas_rd_kafka_conf_res_t; cdecl;


       //
       // dump the configuration properties and values of \p conf to an array
       //        with \"key\", \"value\" pairs.
       //
       // the number of entries in the array is returned in  //cntp.
       //
       // the dump must be freed with `rd_kafka_conf_dump_free()`.
       ///
       function rd_kafka_conf_dump( conf : pas_ptr_rd_kafka_conf_t;
                                    var cntp : ctypes.cuint32 ) : pas_ptr_ref_char_t ; cdecl;



       //
       // dump the topic configuration properties and values of \p conf
       //        to an array with \"key\", \"value\" pairs.
       //
       // the number of entries in the array is returned in \p //cntp.
       //
       // the dump must be freed with `rd_kafka_conf_dump_free()`.
       //
       function rd_kafka_topic_conf_dump( conf : ptr_pas_rd_kafka_topic_conf_t ;
                                         var cntp : ctypes.cuint32 ) : pas_ptr_ref_char_t ; cdecl;

       //
       // frees a configuration dump returned from `rd_kafka_conf_dump()` or
       //  rd_kafka_topic_conf_dump().
       //
       procedure rd_kafka_conf_dump_free( arr : pas_ptr_ref_char_t;
                                          cnt : ctypes.cuint64 ); cdecl;

       //
       //  prints a table to fp of all supported configuration properties,
       //  their default values as well as a description.
       //
       procedure  rd_kafka_conf_properties_show( fp : pointer ); cdecl;


       //
       // 7opic configuration
       // topic configuration property interface
       //
       //
       //
       // create topic configuration object
       //
       // @sa same semantics as for rd_kafka_conf_new().
       //
       function rd_kafka_topic_conf_new : ptr_pas_rd_kafka_topic_conf_t; cdecl;



       //
       // creates a copy/duplicate of topic configuration object \p conf.
       //
       function rd_kafka_topic_conf_dup( const conf : ptr_pas_rd_kafka_topic_conf_t )
                                                   : ptr_pas_rd_kafka_topic_conf_t; cdecl;

       //
       // creates a copy/duplicate of rk's default topic configuration
       // object.
       //
       function rd_kafka_default_topic_conf_dup ( rk : pas_ptr_rd_kafka_t )
                                                   : ptr_pas_rd_kafka_topic_conf_t; cdecl;

       //
       //  destroys a topic conf object.
       //
       procedure rd_kafka_topic_conf_destroy( topic_conf : ptr_pas_rd_kafka_topic_conf_t ); cdecl;

       //
       // producer: set partitioner callback in provided topic conf object.
       //
       // the partitioner may be called in any thread at any time,
       // it may be called multiple times for the same message/key.
       //
       // partitioner function constraints:
       //   - MUST NOT call any rd_kafka_//() functions except:
       //       rd_kafka_topic_partition_available()
       //   - MUST NOT block or execute for prolonged periods of time.
       //   - MUST return a value between 0 and partition_cnt-1, or the
       //     special \c RD_KAFKA_PARTITION_UA value if partitioning
       //     could not be performed.
       //
       procedure rd_kafka_topic_conf_set_partitioner_cb( topic_conf : ptr_pas_rd_kafka_topic_conf_t;
                                                         topic_partition_cb : pas_ptr_topic_partition_cb ); cdecl;


       //
       // producer: Set message queueing order comparator callback.
       //
       // the callback may be called in any thread at any time,
       // it may be called multiple times for the same message.
       //
       // ordering comparator function constraints:
       //   - MUST be stable sort (same input gives same output).
       //   - MUST NOT call any rd_kafka_//() functions.
       //   - MUST NOT block or execute for prolonged periods of time.
       //
       // the comparator shall compare the two messages and return:
       //  - < 0 if message \p a should be inserted before message \p b.
       //  - >=0 if message \p a should be inserted after message \p b.
       //
       // @remark Insert sorting will be used to enqueue the message in the
       //         correct queue position, this comes at a cost of O(n).
       //
       // @remark If `queuing.strategy=fifo` new messages are enqueued to the
       //         tail of the queue regardless of msg_order_cmp, but retried messages
       //         are still affected by msg_order_cmp.
       //
       // @warning THIS IS AN EXPERIMENTAL API, SUBJECT TO CHANGE OR REMOVAL,
       //          DO NOT USE IN PRODUCTION.
       //
       procedure rd_kafka_topic_conf_set_msg_order_cmp( topic_conf : ptr_pas_rd_kafka_topic_conf_t;
                                                        msg_oder_cmp : pas_ptr_msg_order_cmp_func ); cdecl;

       //
       // check if partition is available (has a leader broker).
       //
       // @returns 1 if the partition is available, else 0.
       //
       // @warning This function must only be called from inside a partitioner function
       //
       function rd_kafka_topic_partition_available( const rkt : pas_ptr_rd_kafka_t;
					            partition : ctypes.cint32 )  : ctypes.cint32; cdecl;

       //
       // random partitioner.
       //
       // will try not to return unavailable partitions.
       //
       // @returns a random partition between 0 and \p partition_cnt - 1.
       //
       //
       function rd_kafka_msg_partitioner_random( const rkt : pas_rd_kafka_topic_t;
					         const key : pointer;
                                                 keylen : ctypes.cuint64;
					         partition_cnt : ctypes.cuint64;
					         opaque : pointer;
                                                 msg_opaque : pointer ) : ctypes.cint32; cdecl;



         //
         // consistent partitioner.
         //
         // uses consistent hashing to map identical keys onto identical partitions.
         //
         // @returns a random partition between 0 and  partition_cnt - 1 based on
         //          the CRC value of the key
         //
         function rd_kafka_msg_partitioner_consistent ( const rkt : pas_rd_kafka_topic_t;
					                const key : pointer;
                                                        keylen : ctypes.cuint64;
					                partition_cnt : ctypes.cuint64;
					                opaque : pointer;
                                                        msg_opaque : pointer ) : ctypes.cint32; cdecl;

         //
         // cnsistent-random partitioner.
         //
         // this is the default partitioner.
         // uses consistent hashing to map identical keys onto identical partitions, and
         // messages without keys will be assigned via the random partitioner.
         //
         // @returns a random partition between 0 and \p partition_cnt - 1 based on
         //          the CRC value of the key (if provided)
         //
         function rd_kafka_msg_partitioner_consistent_random( const rkt : pas_rd_kafka_topic_t;
					                      const key : pointer;
                                                              keylen : ctypes.cuint64;
					                      partition_cnt : ctypes.cuint64;
					                      opaque : pointer;
                                                              msg_opaque : pointer ) : ctypes.cint32; cdecl;

          //
          // murmur2 partitioner (Java compatible).
          //
          // uses consistent hashing to map identical keys onto identical partitions
          // using Java-compatible Murmur2 hashing.
          //
          // @returns a partition between 0 and partition_cnt - 1.
          ///
          function rd_kafka_msg_partitioner_murmur2( const rkt : pas_rd_kafka_topic_t;
					             const key : pointer;
                                                     keylen : ctypes.cuint64;
					             partition_cnt : ctypes.cuint64;
					             opaque : pointer;
                                                     msg_opaque : pointer ) : ctypes.cint32; cdecl;

           //
           // consistent-Random Murmur2 partitioner (Java compatible).
           //
           // uses consistent hashing to map identical keys onto identical partitions
           // using Java-compatible Murmur2 hashing.
           // messages without keys will be assigned via the random partitioner.
           //
           // @returns a partition between 0 and \p partition_cnt - 1.
           //
           function rd_kafka_msg_partitioner_murmur2_random(   const rkt : pas_rd_kafka_topic_t;
					                       const key : pointer;
                                                               keylen : ctypes.cuint64;
					                       partition_cnt : ctypes.cuint64;
					                       opaque : pointer;
                                                               msg_opaque : pointer ) : ctypes.cint32; cdecl;

           ///
           // creates a new Kafka handle and starts its operation according to the
           //        specified \p type (\p RD_KAFKA_CONSUMER or \p RD_KAFKA_PRODUCER).
           //
           // conf is an optional struct created with `rd_kafka_conf_new()` that will
           // be used instead of the default configuration.
           // The \p conf object is freed by this function on success and must not be used
           // or destroyed by the application sub-sequently.
           // See `rd_kafka_conf_set()` et.al for more information.
           //
           //  errstr must be a pointer to memory of at least size  errstr_size where
           // `rd_kafka_new()` may write a human readable error message in case the
           // creation of a new handle fails. In which case the function returns NULL.
           //
           // @remark \b RD_KAFKA_CONSUMER: When a new \p RD_KAFKA_CONSUMER
           //           rd_kafka_t handle is created it may either operate in the
           //           legacy simple consumer mode using the rd_kafka_consume_start()
           //           interface, or the High-level KafkaConsumer API.
           // @remark An application must only use one of these groups of APIs on a given
           //         rd_kafka_t RD_KAFKA_CONSUMER handle.
           //
           // @returns The Kafka handle on success or NULL on error (see \p errstr)
           //
           // @sa To destroy the Kafka handle, use rd_kafka_destroy().
           //
           function  rd_kafka_new(  typ :  pas_rd_kakfa_type_t;
                                    conf : pas_ptr_rd_kafka_conf_t;
			            errstr : PAnsiChar;
                                    errstr_size : ctypes.cuint64 )  : pas_ptr_rd_kafka_t; cdecl;


           //
           // destroy Kafka handle.
           //
           // this is a blocking operation.
           //
           procedure rd_kafka_destroy( rk : pas_ptr_rd_kafka_t );  cdecl;


           //
           // returns Kafka handle name.
           ///
           function rd_kafka_name( const rkt : pas_ptr_rd_kafka_t ) : PAnsiChar; cdecl;


           //
           // returns Kafka handle type.
           //
           function rd_kafka_type( const rkt : pas_ptr_rd_kafka_t ) : pas_rd_kakfa_type_t; cdecl;


           ///
           // returns this client's broker-assigned group member id
           //
           // @remark this currently requires the high-level KafkaConsumer
           //
           // @returns an allocated string containing the current broker-assigned group
           //          member id, or NULL if not available.
           //          The application must free the string with \p free() or
           //          rd_kafka_mem_free()
           ///
           function rd_kafka_memberid( const rkt : pas_ptr_rd_kafka_t ) : PAnsiChar; cdecl;

           //
           // @brief returns the ClusterId as reported in broker metadata.
           //
           // @param timeout_ms If there is no cached value from metadata retrieval
           //                   then this specifies the maximum amount of time
           //                   (in milliseconds) the call will block waiting
           //                   for metadata to be retrieved.
           //                   Use 0 for non-blocking calls.

           // @remark requires broker version >=0.10.0 and api.version.request=true.
           //
           // @remark the application must free the returned pointer
           //         using rd_kafka_mem_free().
           //
           // @returns a newly allocated string containing the ClusterId, or NULL
           //          if no ClusterId could be retrieved in the allotted timespan.
           ///
           function rd_kafka_clusterid(  rkt : pas_ptr_rd_kafka_t;
                                         timeout_ms  : ctypes.cint32 ) : PAnsiChar; cdecl;

           //
           // creates a new topic handle for topic named \p topic.
           //
           // \p conf is an optional configuration for the topic created with
           // `rd_kafka_topic_conf_new()` that will be used instead of the default
           // topic configuration.
           // the \p conf object is freed by this function and must not be used or
           // destroyed by the application sub-sequently.
           // see `rd_kafka_topic_conf_set()` et.al for more information.
           //
           // topic handles are refcounted internally and calling rd_kafka_topic_new()
           // again with the same topic name will return the previous topic handle
           // without updating the original handle's configuration.
           // applications must eventually call rd_kafka_topic_destroy() for each
           // succesfull call to rd_kafka_topic_new() to clear up resources.
           //
           // @returns the new topic handle or NULL on error (use rd_kafka_errno2err()
           //          to convert system \p errno to an rd_kafka_resp_err_t error code.
           //
           // @sa rd_kafka_topic_destroy()
           //
           function rd_kafka_topic_new( rkt : pas_ptr_rd_kafka_t;
                                        topic : PAnsiChar;
				        conf : pas_ptr_rd_kafka_conf_t )  : pas_rd_kafka_topic_t;  cdecl;

           //
           //  loose application's topic handle refcount as previously created
           //        with `rd_kafka_topic_new()`.
           //
           //  @remark since topic objects are refcounted (both internally and for the app)
           //         the topic object might not actually be destroyed by this call,
           //         but the application must consider the object destroyed.
           ///
           procedure rd_kafka_topic_destroy( rkt : pas_rd_kafka_topic_t ); cdecl;

           //
           // returns the topic name.
           //
           function rd_kafka_topic_name( const rk : pas_rd_kafka_topic_t ) : PAnsiChar; cdecl;


           //
           // get the rkt_opaque pointer that was set in the topic configuration.
           //
           procedure rd_kafka_topic_opaque ( const rk : pas_rd_kafka_topic_t) ;  cdecl;

           //
           // polls the provided kafka handle for events.
           //
           // events will cause application provided callbacks to be called.
           //
           // the timeout_ms argument specifies the maximum amount of time
           // (in milliseconds) that the call will block waiting for events.
           // for non-blocking calls, provide 0 as \p timeout_ms.
           // to wait indefinately for an event, provide -1.
           //
           // an application should make sure to call poll() at regular
           // intervals to serve any queued callbacks waiting to be called.
           //
           // events:
           //   - delivery report callbacks  (if dr_cb/dr_msg_cb is configured) [producer]
           //   - error callbacks (rd_kafka_conf_set_error_cb()) [all]
           //   - stats callbacks (rd_kafka_conf_set_stats_cb()) [all]
           //   - throttle callbacks (rd_kafka_conf_set_throttle_cb()) [all]
           //
           // @returns the number of events served.
           //
           function rd_kafka_poll( rkt : pas_ptr_rd_kafka_t; timeout_ms :  ctypes.cint64 ) : ctypes.cint64; cdecl;

           //
           // cancels the current callback dispatcher (rd_kafka_poll(),
           // rd_kafka_consume_callback(), etc).
           //
           // a callback may use this to force an immediate return to the calling
           // code (caller of e.g. rd_kafka_poll()) without processing any further
           // events.
           //
           // @remark This function MUST ONLY be called from within a librdkafka callback.
           ///
           procedure rd_kafka_yield( rkt : pas_ptr_rd_kafka_t ); cdecl;

           //
           // pause producing or consumption for the provided list of partitions.
           //
           // success or error is returned per-partition \p err in the \p partitions list.
           //
           // @returns RD_KAFKA_RESP_ERR_NO_ERROR
           //
           function  rd_kafka_pause_partitions( rkt : pas_ptr_rd_kafka_t;
	                       partitions : pas_ptr_rd_kafka_topic_partition_list_t ) : pas_rd_kafka_resp_err_t; cdecl;

           //
           // resume producing consumption for the provided list of partitions.
           //
           // success or error is returned per-partition \p err in the \p partitions list.
           //
           // @returns RD_KAFKA_RESP_ERR_NO_ERROR
           //
           function rd_kafka_resume_partitions( rkt : pas_ptr_rd_kafka_t;
	                       partitions : pas_ptr_rd_kafka_topic_partition_list_t ) : pas_rd_kafka_resp_err_t; cdecl;



           //
           // query broker for low (oldest/beginning) and high (newest/end) offsets
           //        for partition.
           //
           // offsets are returned in low and high respectively.
           //
           // @returns RD_KAFKA_RESP_ERR_NO_ERROR on success or an error code on failure.
           //
           function rd_kafka_query_watermark_offsets( rkt : pas_ptr_rd_kafka_t;
	                                              const topic : PAnsiChar;
                                                      partition : ctypes.cint32;
		                                      low : ctypes.cint64;
                                                      high : ctypes.cint64;
                                                      timeout_ms : ctypes.cint64 ) : pas_rd_kafka_resp_err_t; cdecl;

           //
           // get last known low (oldest/beginning) and high (newest/end) offsets
           //        for partition.
           //
           // the low offset is updated periodically (if statistics.interval.ms is set)
           // while the high offset is updated on each fetched message set from the broker.
           //
           // if there is no cached offset (either low or high, or both) then
           // RD_KAFKA_OFFSET_INVALID will be returned for the respective offset.
           //
           // offsets are returned in \p //low and \p //high respectively.
           //
           // @returns RD_KAFKA_RESP_ERR_NO_ERROR on success or an error code on failure.
           //
           // @remark shall only be used with an active consumer instance.
           ///
           function rd_kafka_get_watermark_offsets( kt : pas_ptr_rd_kafka_t;
				           partition : ctypes.cint32;
		                           var low : ctypes.cint64;
                                           var high : ctypes.cint64 ) : pas_rd_kafka_resp_err_t; cdecl;





implementation


function rd_kafka_version_str : PAnsiChar ; cdecl;   external;
//
function rd_kafka_get_debug_contexts : PAnsiChar ; cdecl;  external;
//
function rd_kafka_err2name ( err : pas_rd_kafka_resp_err_t ) : PAnsiChar;  cdecl;   external;
//
function rd_kafka_err2str ( err : pas_rd_kafka_resp_err_t ) : PAnsiChar; cdecl;  external;
//
function rd_kafka_last_error : pas_rd_kafka_resp_err_t; cdecl; external;
//
function rd_kafka_topic_partition_list_new ( size : ctypes.cint32 )
                                                 : pas_ptr_rd_kafka_topic_partition_list_t; cdecl; external;
//
function rd_kafka_topic_partition_list_add ( rkparlist : pas_ptr_rd_kafka_topic_partition_list_t;
                                             const topic  : PAnsiChar;
                                             partition : ctypes.cint32 ) : pas_ptr_rd_kafka_topic_partition_t; cdecl;  external;
//
function rd_kafka_topic_partition_list_del( rktparlist : pas_ptr_rd_kafka_topic_partition_list_t;
				            const topic : PAnsichar;
                                            partition : ctypes.cint32 ) : ctypes.cint32; cdecl; external;
function rd_kafka_topic_partition_list_del_by_idx ( rktparlist : pas_ptr_rd_kafka_topic_partition_list_t;
                                                    idx : ctypes.cint32 ) : ctypes.cint32; cdecl;  external;
//
function  rd_kafka_topic_partition_list_copy ( const src : pas_ptr_rd_kafka_topic_partition_list_t )
                                                : pas_ptr_rd_kafka_topic_partition_list_t; cdecl;  external;
//
function rd_kafka_topic_partition_list_set_offset ( rktparlist : pas_ptr_rd_kafka_topic_partition_list_t;
	     	                                    const topic : PAnsiChar;
                                                    partition : ctypes.cint32;
                                                    offset : ctypes.cint64 ) : pas_rd_kafka_resp_err_t; cdecl; external;
//
function rd_kafka_topic_partition_list_find ( rktparlist : pas_ptr_rd_kafka_topic_partition_list_t;
				                   const topic : PAnsiChar;
                                                   partition : ctypes.cint32 ) : pas_ptr_rd_kafka_topic_partition_t; cdecl;  external;
//
procedure rd_kafka_get_err_descs( var errdescs : array of pas_rd_kafka_err_desc;
       		                  var cntp : ctypes.cuint64 )  cdecl;    external;
//
procedure rd_kafka_topic_partition_destroy ( rktpar : pas_ptr_rd_kafka_topic_partition_t ); cdecl; external;
//
procedure rd_kafka_topic_partition_list_destroy ( rkparlist : pas_ptr_rd_kafka_topic_partition_list_t ); cdecl;  external;
//
procedure rd_kafka_topic_partition_list_add_range ( rktparlist : pas_ptr_rd_kafka_topic_partition_list_t;
                                                    const topic  : PAnsiChar;
                                                    start : ctypes.cint32;
                                                    stop  : ctypes.cint32 ); cdecl; external;
procedure rd_kafka_topic_partition_list_sort( rktparlist : pas_ptr_rd_kafka_topic_partition_list_t;
                                                  cmp : pas_ptr_pas_t_compare_func;
                                                  opaque : pointer ); cdecl; external;
//
function rd_kafka_message_timestamp( const rkmessage : pas_ptr_rd_kafka_message_t;
				           tstype : pas_ptr_rd_kafka_timestamp_type_t ) : ctypes.cint64; cdecl; external;
//
procedure rd_kafka_message_destroy( rkmessage : pas_ptr_rd_kafka_message_t ); cdecl;  external;
//
function rd_kafka_message_latency (const rkmessage : pas_ptr_rd_kafka_message_t ) : ctypes.cint64;  cdecl; external;
//                                                                                               rk : pas_ptr_rd_kafka_t
function rd_kafka_message_headers ( const message : pas_ptr_rd_kafka_message_t;
                                    var hdrsp : pas_ptr_rd_kafka_headers_t ) : pas_rd_kafka_resp_err_t; cdecl;  external;
//
function rd_kafka_message_detach_headers( message : pas_ptr_rd_kafka_message_t;
                                          var hdrsp : pas_ptr_rd_kafka_headers_t ) : pas_rd_kafka_resp_err_t; cdecl;  external;
//
procedure rd_kafka_message_set_headers ( rkmessage : pas_ptr_rd_kafka_message_t;
                                                hdrs : pas_ptr_rd_kafka_headers_t ) cdecl; external;
//
function rd_kafka_header_cnt ( const hdrs : pas_ptr_rd_kafka_headers_t )   : ctypes.cint64; cdecl; external;
//
function rd_kafka_conf_new : pas_ptr_rd_kafka_conf_t; cdecl;  external;
//
procedure rd_kafka_conf_destroy( conf : pas_ptr_rd_kafka_conf_t ); cdecl;  external;
//
function rd_kafka_conf_dup( const conf : pas_ptr_rd_kafka_conf_t ) : pas_ptr_rd_kafka_conf_t; cdecl; external;
//
function rd_kafka_conf_dup_filter ( const conf : pas_ptr_rd_kafka_conf_t;
                                    filter_cnt : ctypes.cuint64;
                                    var filter : PAnsiChar ) : pas_ptr_rd_kafka_conf_t; cdecl;  external;
//
function rd_kafka_conf_set(  const conf : pas_ptr_rd_kafka_conf_t;
			     const name : PAnsiChar;
			     const value : PAnsiChar;
			     const errstr : PAnsiChar;
                             errstr_size : ctypes.cuint64 ) : pas_rd_kafka_conf_res_t ; cdecl;  external;
//
procedure rd_kafka_conf_set_events( conf : pas_ptr_rd_kafka_conf_t; events : ctypes.cuint64 ); cdecl; external;
//
procedure rd_kafka_conf_set_dr_msg_cb( conf : pas_ptr_rd_kafka_conf_t;
                                        msg_cb : pas_ptr_pas_dr_msg_cb ); cdecl; external;
//
procedure rd_kafka_conf_set_rebalance_cb ( conf : pas_ptr_rd_kafka_conf_t;
                                           rebalance_cb : pas_ptr_pas_rebalance_cb ); cdecl; external;
//
procedure rd_kafka_conf_set_consume_cb ( conf : pas_ptr_rd_kafka_conf_t;
                                         consume_cb : pas_ptr_pas_consume_cb ); cdecl;  external;
//
procedure rd_kafka_conf_set_offset_commit_cb( conf : pas_ptr_rd_kafka_conf_t;
                                              offset_commit_cb : pas_ptr_pas_offset_commit_cb ); cdecl; external;
//
procedure rd_kafka_conf_set_error_cb( conf : pas_ptr_rd_kafka_conf_t;
				             error_cb : pas_ptr_pas_error_cb ); cdecl; external;
//
procedure rd_kafka_conf_set_log_cb( conf : pas_ptr_rd_kafka_conf_t;
			                   log_cb : pas_ptr_pas_log_cb ); cdecl;  external;
//
procedure  rd_kafka_conf_set_stats_cb( conf : pas_ptr_rd_kafka_conf_t;
                                              stats_cb : pas_ptr_pas_stats_cb ); cdecl; external;
//
procedure rd_kafka_conf_set_socket_cb( conf : pas_ptr_rd_kafka_conf_t;
                                              socket_cb : pas_ptr_pas_socket_cb ) cdecl;  external;
//
procedure rd_kafka_conf_set_connect_cb( conf : pas_ptr_rd_kafka_conf_t;
                                        connect_cb :  pas_ptr_pas_connect_cb ); cdecl;  external;
//
procedure rd_kafka_conf_set_closesocket_cb( conf : pas_ptr_rd_kafka_conf_t;
                                            close_socket_cb :  pas_ptr_pas_closesocket_cb ); cdecl;  external;
{$IFNDEF _MSC_VER}
//
procedure rd_kafka_conf_set_open_cb ( conf : pas_ptr_rd_kafka_conf_t;
                                             open_cb : pas_ptr_open_cb ); cdecl;  external;
{$ENDIF}
//
procedure rd_kafka_conf_set_opaque( conf : pas_ptr_rd_kafka_conf_t;
                                    opaque : pointer );  cdecl;  external;
//
function rd_kafka_opaque( const rk : pas_ptr_rd_kafka_t ) : pointer;  cdecl;  external;
//
procedure rd_kafka_conf_set_default_topic_conf( conf : pas_ptr_rd_kafka_conf_t;
                                                tconf : ptr_pas_rd_kafka_topic_conf_t ); cdecl; external;
//
function rd_kafka_conf_get( conf : pas_ptr_rd_kafka_conf_t;
                            const name : PAnsiChar;
                            dest : PAnsiChar;
                            var dest_size : ctypes.cuint64 ) : pas_rd_kafka_conf_res_t; cdecl;  external;
//
function rd_kafka_conf_dump( conf : pas_ptr_rd_kafka_conf_t;
                             var cntp : ctypes.cuint32 ) :  pas_ptr_ref_char_t; cdecl; external;
//
function rd_kafka_topic_conf_dump( conf : ptr_pas_rd_kafka_topic_conf_t;
                                   var cntp : ctypes.cuint32 ) : pas_ptr_ref_char_t ; cdecl;  external;
//
procedure rd_kafka_conf_dump_free( arr : pas_ptr_ref_char_t;
                                          cnt : ctypes.cuint64 ); cdecl; external;
//
procedure rd_kafka_conf_properties_show( fp : pointer ); cdecl;  external;
//
function rd_kafka_topic_conf_new : ptr_pas_rd_kafka_topic_conf_t; cdecl;  external;
//
function rd_kafka_topic_conf_dup( const conf : ptr_pas_rd_kafka_topic_conf_t )
                                                   : ptr_pas_rd_kafka_topic_conf_t; cdecl; external;
//
function rd_kafka_default_topic_conf_dup ( rk : pas_ptr_rd_kafka_t )
                                              : ptr_pas_rd_kafka_topic_conf_t; cdecl;  external;
//
procedure rd_kafka_topic_conf_destroy( topic_conf : ptr_pas_rd_kafka_topic_conf_t ); cdecl;  external;

procedure rd_kafka_topic_conf_set_partitioner_cb( topic_conf : ptr_pas_rd_kafka_topic_conf_t;
                                                  topic_partition_cb : pas_ptr_topic_partition_cb ); cdecl; external;
//
procedure rd_kafka_topic_conf_set_msg_order_cmp( topic_conf : ptr_pas_rd_kafka_topic_conf_t;
                                                        msg_oder_cmp : pas_ptr_msg_order_cmp_func ); cdecl;  external;
//
function rd_kafka_topic_partition_available( const rkt : pas_ptr_rd_kafka_t;
					     partition : ctypes.cint32 )  : ctypes.cint32;   cdecl; external;
//
function rd_kafka_msg_partitioner_random( const rkt : pas_rd_kafka_topic_t;
					  const key : pointer;
                                          keylen : ctypes.cuint64;
					  partition_cnt : ctypes.cuint64;
					  opaque : pointer;
                                          msg_opaque : pointer ) : ctypes.cint32; cdecl;  external;
//
function rd_kafka_msg_partitioner_consistent ( const rkt : pas_rd_kafka_topic_t;
					       const key : pointer;
                                               keylen : ctypes.cuint64;
					       partition_cnt : ctypes.cuint64;
					       opaque : pointer;
                                               msg_opaque : pointer ) : ctypes.cint32; cdecl;  external;
//
function rd_kafka_msg_partitioner_consistent_random(  const rkt : pas_rd_kafka_topic_t;
					              const key : pointer;
                                                      keylen : ctypes.cuint64;
					              partition_cnt : ctypes.cuint64;
					              opaque : pointer;
                                                      msg_opaque : pointer ) : ctypes.cint32; cdecl; external;
//
function rd_kafka_msg_partitioner_murmur2( const rkt : pas_rd_kafka_topic_t;
					             const key : pointer;
                                                     keylen : ctypes.cuint64;
					             partition_cnt : ctypes.cuint64;
					             opaque : pointer;
                                                     msg_opaque : pointer ) : ctypes.cint32; cdecl;  external;
//
function rd_kafka_msg_partitioner_murmur2_random(   const rkt : pas_rd_kafka_topic_t;
       				                    const key : pointer;
                                                    keylen : ctypes.cuint64;
       				                    partition_cnt : ctypes.cuint64;
       				                    opaque : pointer;
                                                    msg_opaque : pointer ) : ctypes.cint32; cdecl;  external;
//
function  rd_kafka_new(   typ :  pas_rd_kakfa_type_t;
                          conf : pas_ptr_rd_kafka_conf_t;
			  errstr : PAnsiChar;
                          errstr_size : ctypes.cuint64 )  : pas_ptr_rd_kafka_t; cdecl; external;
//
function rd_kafka_name( const rkt : pas_ptr_rd_kafka_t ) : PAnsiChar; cdecl; external;
//
procedure rd_kafka_destroy( rk : pas_ptr_rd_kafka_t );  cdecl; external;
//
function rd_kafka_type( const rkt : pas_ptr_rd_kafka_t ) : pas_rd_kakfa_type_t; cdecl; external;
//
function rd_kafka_memberid( const rkt : pas_ptr_rd_kafka_t ) : PAnsiChar; cdecl; external;
//
function rd_kafka_clusterid(  rkt : pas_ptr_rd_kafka_t;
                              timeout_ms  : ctypes.cint32 ) : PAnsiChar; cdecl;   external;
//
function rd_kafka_topic_new( rkt : pas_ptr_rd_kafka_t;
                             topic : PAnsiChar;
			     conf : pas_ptr_rd_kafka_conf_t )  : pas_rd_kafka_topic_t;  cdecl; external;
//
procedure rd_kafka_topic_destroy( rkt : pas_rd_kafka_topic_t ); cdecl;  external;
//
function rd_kafka_topic_name( const rk : pas_rd_kafka_topic_t ) : PAnsiChar; cdecl; external;
//
procedure rd_kafka_topic_opaque ( const rk : pas_rd_kafka_topic_t ) ;  cdecl; external;
//
function rd_kafka_poll( rkt : pas_ptr_rd_kafka_t; timeout_ms :  ctypes.cint64 ) : ctypes.cint64; cdecl; external;
//
procedure rd_kafka_yield ( rkt : pas_ptr_rd_kafka_t ); cdecl; external;
//
function  rd_kafka_pause_partitions( rkt : pas_ptr_rd_kafka_t;
	                       partitions : pas_ptr_rd_kafka_topic_partition_list_t ) : pas_rd_kafka_resp_err_t; cdecl;  external;
//
function rd_kafka_resume_partitions ( rkt : pas_ptr_rd_kafka_t;
                             partitions : pas_ptr_rd_kafka_topic_partition_list_t ) : pas_rd_kafka_resp_err_t; cdecl;  external;
//
function rd_kafka_query_watermark_offsets( rkt : pas_ptr_rd_kafka_t;
                                           const topic : PAnsiChar;
                                           partition : ctypes.cint32;
       	                                   low : ctypes.cint64;
                                           high : ctypes.cint64;
                                           timeout_ms : ctypes.cint64 ) : pas_rd_kafka_resp_err_t; cdecl;   external;
//
function rd_kafka_get_watermark_offsets( kt : pas_ptr_rd_kafka_t;
      			                 partition : ctypes.cint32;
      	                                 var low : ctypes.cint64;
                                         var high : ctypes.cint64) : pas_rd_kafka_resp_err_t; cdecl; external;
//
function rd_kafka_message_errstr( const rkmessage : pas_ptr_rd_kafka_message_t ) : PAnsiChar ; inline;
var
    err_msg : pas_rd_kafka_message_t;
begin
       err_msg := rkmessage^;
       if ( integer( err_msg.err ) = 0 ) then
       begin
	      result := nil;
       end;

       if ( err_msg.payload <> nil ) then
       begin
	     result := PAnsiChar( err_msg.payload );
       end;

       result := rd_kafka_err2str( err_msg.err );
end;




end.

