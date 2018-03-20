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
//
unit cci_mini_kafka;

{$mode objfpc}{$H+}
{$linklib rdkafka}
{$linklib cci_kafka_utils}
{$linklib c}
{$INLINE ON}

interface

uses ctypes , sysutils;

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
    pas_ptr_pas_t_compare_func = ^pas_t_compare_func;
    pas_t_compare_func = function( const a : pointer;
                                   const b : pointer;
                                   const opaque : pointer ) : ctypes.cint32; cdecl;
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
    /////
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
    //pas_rd_kafka_topic_s                       = ^pas_rd_kafka_topic_t;
    pas_ptr_rd_kafka_conf_t                        =  type pointer;
    //ptr_pas_rd_kafka_topic_conf_s                   = ^pas_rd_kafka_topic_conf_t;
    //ptr_pas_rd_kafka_queue_s                        = ^pas_rd_kafka_queue_t;

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


     // Topic+Partition place holder
     //
     // Generic place holder for a Topic+Partition and its related information
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
     pas_ptr_rd_kafka_topic_partition_list_t = ^pas_rd_kafka_topic_partition_list_t;
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
     pas_ptr_rd_kafka_message_t = ^pas_rd_kafka_message_t;
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
	    key :  pointer;                   /////< Depends on the value of \c err :
				             // - \c err==0: Optional message key ///
	    key_len : ctypes.cint64;         /////< Depends on the value of \c err :
				             // - \c err==0: Optional message key length///
	    offset  : ctypes.cint64;          /////< Consume:
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
                                                  topic  : PAnsiChar;
                                                  partition : ctypes.cint32 ) : pas_ptr_rd_kafka_topic_partition_t; cdecl;

     // @brief add range of partitions from \p start to \p stop inclusive.
     //
     // @param rktparlist list to extend
     // @param topic      topic name (copied)
     // @param start      start partition of range
     // @param stop       last partition of range (inclusive)
     //
     procedure rd_kafka_topic_partition_list_add_range ( rktparlist : pas_ptr_rd_kafka_topic_partition_list_t;
                                                         topic  : PAnsiChar;
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
				                 topic : PAnsichar;
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
	     	                                         topic : PAnsiChar;
                                                         partition : ctypes.cint32;
                                                         offset : ctypes.cint64 ) : pas_rd_kafka_resp_err_t; cdecl;


     // find element by \p topic and \p partition.
     //
     // @returns a pointer to the first matching element, or NULL if not found.
     //
     function rd_kafka_topic_partition_list_find ( rktparlist : pas_ptr_rd_kafka_topic_partition_list_t;
				                   topic : PAnsiChar;
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
                                             topic  : PAnsiChar;
                                             partition : ctypes.cint32 ) : pas_ptr_rd_kafka_topic_partition_t; cdecl;  external;
//
function rd_kafka_topic_partition_list_del( rktparlist : pas_ptr_rd_kafka_topic_partition_list_t;
				            topic : PAnsichar;
                                            partition : ctypes.cint32 ) : ctypes.cint32; cdecl; external;
function rd_kafka_topic_partition_list_del_by_idx ( rktparlist : pas_ptr_rd_kafka_topic_partition_list_t;
                                                    idx : ctypes.cint32 ) : ctypes.cint32; cdecl;  external;
//
function  rd_kafka_topic_partition_list_copy ( const src : pas_ptr_rd_kafka_topic_partition_list_t )
                                                : pas_ptr_rd_kafka_topic_partition_list_t; cdecl;  external;
//
function rd_kafka_topic_partition_list_set_offset ( rktparlist : pas_ptr_rd_kafka_topic_partition_list_t;
	     	                                         topic : PAnsiChar;
                                                         partition : ctypes.cint32;
                                                         offset : ctypes.cint64 ) : pas_rd_kafka_resp_err_t; cdecl; external;
//
function rd_kafka_topic_partition_list_find ( rktparlist : pas_ptr_rd_kafka_topic_partition_list_t;
				                   topic : PAnsiChar;
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
                                                    topic  : PAnsiChar;
                                                    start : ctypes.cint32;
                                                    stop  : ctypes.cint32 ); cdecl; external;
procedure rd_kafka_topic_partition_list_sort( rktparlist : pas_ptr_rd_kafka_topic_partition_list_t;
                                                  cmp : pas_ptr_pas_t_compare_func;
                                                  opaque : pointer ); cdecl; external;
//                                                                               procedure rd_kafka_conf_destroy( conf : pas_ptr_rd_kafka_conf_t ); cdecl;
function rd_kafka_message_timestamp( const rkmessage : pas_ptr_rd_kafka_message_t;
				           tstype : pas_ptr_rd_kafka_timestamp_type_t ) : ctypes.cint64; cdecl; external;
//
procedure rd_kafka_message_destroy( rkmessage : pas_ptr_rd_kafka_message_t ); cdecl;  external;
//
function rd_kafka_message_latency (const rkmessage : pas_ptr_rd_kafka_message_t ) : ctypes.cint64;  cdecl; external;
//
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

