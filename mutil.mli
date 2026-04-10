(* $Id: mutil.mli,v 1.13 2017/12/28 10:50:13 deraugla Exp $ *)

open Mcomm;

value pi : float;
value round : float -> int;

value string_of_time : float -> string;
value single_unix_read : Unix.file_descr -> bytes -> int -> int -> int;
value input_value : Unix.file_descr -> option 'a;
value sprint_result : result 'a -> string;
value map_result : ('a -> 'b) -> result 'a -> result 'b;
value map_option : ('a -> 'b) -> option 'a -> option 'b;
value int_of_file_descr : Unix.file_descr -> int;
value file_descr_of_int : int -> Unix.file_descr;
value trace_oc : unit -> out_channel;

value service_of_string : string -> Unix.sockaddr;
value socket_and_bind : Unix.sockaddr -> Unix.file_descr;
