(* $Id: mutil.ml,v 1.25 2017/12/28 10:50:13 deraugla Exp $ *)

open Mcomm;
open Printf;

value round f = truncate (f +. if f < 0.0 then -0.5 else 0.5);

value string_of_time tm =
  let tms = round tm in
  let mm = tms / 60 in
  let ss = tms mod 60 in
  let hh = mm / 60 in
  let mm = mm mod 60 in
  let dd = hh / 24 in
  if dd > 1 then
    let hh = hh mod 24 in
    if mm > 0 then sprintf "%dd%dh%dmin" dd hh mm
    else if hh > 0 then sprintf "%dd%dh" dd hh
    else sprintf "%dd" dd
  else if hh > 0 then
    if ss > 0 then sprintf "%dh%dmin%ds" hh mm ss
    else if mm > 0 then sprintf "%dh%dmin" hh mm
    else sprintf "%dh" hh
  else if mm > 0 then
    if ss > 0 then sprintf "%dmin%ds" mm ss
    else sprintf "%dmin" mm
  else
    sprintf "%gs" tm
;

value single_unix_read s b ofs len =
  loop 0 ofs len where rec loop nrd ofs len =
    match
      try Some (Unix.read s b ofs len) with
      [ Unix.Unix_error Unix.ECONNRESET _ _ -> None ]
    with
    [ Some 0 | None -> 0
    | Some n ->
        if n = len then nrd + n
        else loop (nrd + n) (ofs + n) (len - n) ]
;

value buff = ref (Bytes.create (Marshal.header_size + 10000));

value input_value s =
  let len = single_unix_read s buff.val 0 Marshal.header_size in
  if len = 0 then None
  else if len <> Marshal.header_size then None
  else do {
    let size = Marshal.data_size buff.val 0 in
    let len = Marshal.header_size + size in
    if Bytes.length buff.val < len then do {
      let new_buff = Bytes.create len in
      Bytes.blit buff.val 0 new_buff 0 Marshal.header_size;
      buff.val := new_buff;
    }
    else ();
    let len = single_unix_read s buff.val Marshal.header_size size in
    if len <> size then None
    else Some (Marshal.from_string (Bytes.to_string buff.val) 0)
  }
;

value sprint_result =
  fun
  [ Result n -> string_of_int n
  | LimitReached _ _ _ -> "lim" ]
;

value map_result f =
  fun
  [ Result x -> Result x
  | LimitReached xn yn p -> LimitReached (f xn) (f yn) p ]
;

value map_option f =
  fun
  [ Some x -> Some (f x)
  | None -> None ]
;

value int_of_file_descr (s : Unix.file_descr) = (Obj.magic s : int);
value file_descr_of_int (i : int) = (Obj.magic i : Unix.file_descr);

value service_of_string s =
  match try Some (String.index s ':') with [ Not_found -> None ] with
  [ Some i ->
      let name = String.sub s 0 i in
      let p = String.sub s (i + 1) (String.length s - i - 1) in
      match
        try Some (int_of_string p) with [ Failure _ -> None ]
      with
      [ Some port ->
          match
            try Some (Unix.gethostbyname name) with
            [ Not_found -> None ]
          with
          [ Some h -> Unix.ADDR_INET h.Unix.h_addr_list.(0) port
          | None -> failwith (sprintf "unknown host \"%s\"\n" name) ]
      | None -> failwith (sprintf "invalid address \"%s\"" s) ]
  | None ->
      Unix.ADDR_UNIX s ]
;

value socket_and_bind addr = do {
  let s = Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  try do {
    Unix.setsockopt s Unix.SO_REUSEADDR True;
    Unix.bind s addr;
    Unix.listen s 1;
  }
  with e -> do {
    Unix.close s;
    raise e
  };
  s
};

value pi = 3.1415926535897932384626;

value tracing = ref True;
value trace_oc =
  let oc = ref None in
  fun () ->
    match oc.val with
    [ Some oc -> oc
    | None -> do {
        let new_oc =
          open_out (if tracing.val then "mlbrot.log" else "/dev/null")
        in
        oc.val := Some new_oc;
        new_oc
      } ]
;

value fprintf_date oc tm =
  fprintf oc "%4d-%02d-%02d %02d:%02d:%02d" (1900 + tm.Unix.tm_year)
    (succ tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec
;

value trace fmt = do {
  let oc = trace_oc () in
  flush oc;
(*
  if tracing.val then do {
    let tm = Unix.localtime (Unix.gettimeofday ()) in
    fprintf_date oc tm;
    fprintf oc " "
  }
  else ();
*)
  fprintf oc fmt;
};
