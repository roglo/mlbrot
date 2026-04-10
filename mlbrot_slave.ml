(* $Id: mlbrot_slave.ml,v 1.106 2018/02/02 02:08:35 deraugla Exp $ *)

open Mcomm;
open Printf;

value quiet = ref False;
value slave_alias = ref "";
value hired_slave_name = ref "";
value test_no_answer = ref 0;

value prompt () = do {
  eprintf "[mlbrot_slave]";
  let name = hired_slave_name.val in
  if name = "" && slave_alias.val = "" then ()
  else do {
    eprintf " [%s" name;
    if slave_alias.val <> "" then eprintf " %s" slave_alias.val else ();
    eprintf "]";
  };
  eprintf " ";
};

value mprintf fmt = do {
  prompt ();
  eprintf fmt
};

value init_hiring addr =
  let s = Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  match
    try Some (Unix.connect s addr) with
    [ Unix.Unix_error err _ _ as exc -> do {
        match err with
        [ Unix.ENOENT -> 
            match addr with
            [ Unix.ADDR_UNIX name ->
                mprintf "Socket file '%s' does not exist\n%!" name
            | Unix.ADDR_INET _ _ -> () ]
        | Unix.ETIMEDOUT | Unix.ECONNREFUSED ->
            mprintf "%s\n%!" (Unix.error_message err)
        | _ ->
            raise exc ];
        None
      } ]
  with
  [ Some () -> Some s
  | None -> None ]
;

type context =
  { c_mandelbrot_point : Mfloat.mandelbrot_fun Mfloat.M.t;
    c_prec : int;
    c_fctx : function_context Mfloat.Best.t }
;

value send_answer s answer =
  match
    try Some (Unix.write s answer 0 (Bytes.length answer)) with
    [ Unix.Unix_error Unix.EPIPE _ _ -> None ]
  with
  [ Some len ->
      if len <> Bytes.length answer then failwith "error on write"
      else True
  | None -> do {
      if quiet.val then ()
      else do {
        mprintf "master stopped\n%!";
      };
      False
    } ]
;

value apply_request ctx verbose s =
  fun
  [ Set_environ env -> do {
      IFDEF MPFR THEN
        if env.e_with_mpfr then ()
        else do {
          mprintf "incompatibility between master and slave\n";
          mprintf "slave with mpfr library and master without\n%!";
          raise Exit
        }
      ELSE
        if not env.e_with_mpfr then ()
        else do {
          mprintf "incompatibility between master and slave\n";
          mprintf "master with mpfr library and slave without\n%!";
          raise Exit
        }
      END;
      if verbose then do {
        mprintf "reduc %s xc %s yc %s\n"
          env.e_reduc.s_value env.e_xc.s_value env.e_yc.s_value;
        mprintf "prec reduc %d xc %d yc %d\n%!"
          env.e_reduc.s_prec env.e_xc.s_prec env.e_yc.s_prec;
      }
      else ();
      let reduc = Mfloat.Best.deserialize env.e_reduc in
(*
eprintf "bef reduc '%s' (%d)\n" (snd env.e_reduc) (fst env.e_reduc);
eprintf "aft reduc '%s'\n" (Mfloat.Best.to_string reduc);
flush stderr;
*)
      let xc = Mfloat.Best.deserialize env.e_xc in
      let yc = Mfloat.Best.deserialize env.e_yc in
      let julia =
        Mutil.map_option
          (fun (xc, yc) ->
             (Mfloat.Best.deserialize xc, Mfloat.Best.deserialize yc))
          env.e_julia
      in
      if verbose then do {
        mprintf "reduc %s xc %s yc %s\n%!"
          (Mfloat.Best.to_string reduc) (Mfloat.Best.to_string xc)
          (Mfloat.Best.to_string yc);
      }
      else ();
      Mfloat.M.set_num env.e_num_type;
      let prec = env.e_level + 10 in
      Mfloat.M.set_default_prec prec;
      if verbose then do {
        mprintf "reduc %s xc %s yc %s w %d h %d\n%!"
          (Mfloat.Best.to_string reduc) (Mfloat.Best.to_string xc)
          (Mfloat.Best.to_string yc) env.e_w env.e_h;
      }
      else ();
      let mandelbrot_point =
        if env.e_lambda then Mfloat.M.lambda_point
        else
          match env.e_formula with
          [ 2 -> Mfloat.M.mandelbrot_point
          | 3 -> Mfloat.M.mandelbrot_point3
          | n -> Mfloat.M.mandelbrot_point_m n ]
      in
      let rot = Mfloat.trigo_of_rot env.e_rot in
      let fctx =
        {f_w = env.e_w; f_h = env.e_h; f_rot = rot; f_reduc = reduc;
         f_xc = xc; f_yc = yc; f_nb_it = env.e_nb_it;
         f_invert = env.e_invert; f_julia = julia}
      in
      let ctx =
        {c_mandelbrot_point = mandelbrot_point; c_prec = prec; c_fctx = fctx}
      in
      Some ctx
    }
  | Set_nb_it nb_it -> do {
      if verbose then do {
        mprintf "nb_it %d\n%!" nb_it;
      }
      else ();
      Some {(ctx) with c_fctx = {(ctx.c_fctx) with f_nb_it = nb_it}}
    }
  | Compute_values ija -> do {
      if verbose then do {
        mprintf "ijl";
        Array.iter
          (fun q -> do {
             eprintf " (%d, %d" q.q_i q.q_j;
             if q.q_len > 1 then eprintf ", di=%d, len=%d" q.q_di q.q_len
             else ();
             eprintf ")";
           })
          ija;
        eprintf "\n%!";
      }
      else ();
      if test_no_answer.val > 0 then do {
        decr test_no_answer;
        if test_no_answer.val = 0 then do {
          mprintf "test no answering\n%!";
          Unix.sleep 10;
        }
        else ();
      }
      else ();
      let rev_vl =
        Array.fold_left
          (fun rev_vl q ->
             let start =
               Mutil.map_option
                 (fun (xn, yn) ->
                    (Mfloat.M.of_string ctx.c_prec xn,
                     Mfloat.M.of_string ctx.c_prec yn))
                 q.q_start
             in
             let vl =
               loop q.q_len q.q_i where rec loop len i =
                 if len = 0 then []
                 else
                   let r =
                     Mfloat.compute_point_with_fun ctx.c_mandelbrot_point
                       ctx.c_fctx i q.q_j start
                   in
                   let v =
                     Mutil.map_result
                       (fun xn ->
                          (Mfloat.M.get_prec xn, Mfloat.M.to_string xn))
                       r
                   in
                   [v :: loop (len - 1) (i + q.q_di)]
             in
             List.rev_append vl rev_vl)
          [] ija
      in
      let va = Array.of_list (List.rev rev_vl) in
      if verbose then do {
        mprintf "answer";
        Array.iter (fun v -> eprintf " %s" (Mutil.sprint_result v)) va;
        eprintf "\n%!";
      }
      else ();
      let answer =
        Marshal.to_bytes
          (Answ_values (va : array (result (int * string))))
          [Marshal.No_sharing]
      in
      if send_answer s answer then Some ctx else None
    } ]
;

value run_slave verbose s =
  let reduc = Mfloat.Best.of_float 1. in
  let xc = Mfloat.Best.of_float 0. in
  let yc = Mfloat.Best.of_float 0. in
  let fctx =
    {f_w = 1; f_h = 1; f_reduc = reduc; f_rot = None; f_xc = xc; f_yc = yc;
     f_nb_it = 1; f_invert = False; f_julia = None}
  in
  let ctx =
    {c_mandelbrot_point = Mfloat.M.mandelbrot_point; c_prec = 53;
     c_fctx = fctx}
  in
  let b = Bytes.make (String.length Mmagic.comm_magic) ' ' in
  let len = Mutil.single_unix_read s b 0 (Bytes.length b) in
  if len = 0 then
    if quiet.val then ()
    else do {
      mprintf "master stopped\n%!";
    }
  else if Bytes.to_string b <> Mmagic.comm_magic then do {
    mprintf "master and slave have incompatible versions\n%!";
  }
  else
    loop ctx where rec loop ctx =
      match Mutil.input_value s with
      [ Some req ->
          match apply_request ctx verbose s req with
          [ Some ctx -> loop ctx
          | None -> () ]
      | None -> () ]
;

value verbose = ref False;
value master_hiring = ref "";

value speclist =
  Arg.align
    [("-n", Arg.Set_string slave_alias,
      " Give a name to the slave (for traces)");
     ("-q", Arg.Set quiet, " Don't display connecting messages");
     ("-v", Arg.Set verbose, " Verbose");
     ("-t", Arg.Set_int test_no_answer,
      "<int> Test no answer (for debugging)")]
;

value anonfun s = master_hiring.val := s;

value usage =
  "Usage: " ^ Sys.argv.(0) ^ " [option]... <addr>
<addr> is master address: <host:port> or <filename>

Options:"
;

value get_slave_type () =
  if master_hiring.val = "" then do {
    mprintf "Address required.\n";
    mprintf "Use option -help for usage.\n%!";
    exit 2
  }
  else master_hiring.val
;

value terminate addr =
  if quiet.val then ()
  else do {
    mprintf "terminated\n%!";
  }
;

value main () = do {
  Arg.parse speclist anonfun usage;
  let addr = get_slave_type () in
  Sys.catch_break True;
  let _ : Sys.signal_behavior =
    Sys.signal Sys.sigpipe Sys.Signal_ignore
  in ();
  Random.self_init ();
  try do {
    let addr = Mutil.service_of_string addr in
    let hname =
      match addr with
      [ Unix.ADDR_UNIX name -> name
      | Unix.ADDR_INET _ _ -> Unix.gethostname () ]
    in
    hired_slave_name.val := hname;
    match init_hiring addr with
    [ Some s -> do {
        if quiet.val then ()
        else do {
          mprintf "connected...\n%!";
        };
        let sl_name =
          if slave_alias.val = "" then hname
          else hname ^ " " ^ slave_alias.val
        in
        let give_name =
          Marshal.to_bytes (Answ_set_name sl_name) [Marshal.No_sharing]
        in
        if send_answer s give_name then run_slave verbose.val s else ()
      }
    | None -> () ];
    terminate addr
  }
  with e -> do {
    match e with
    [ Sys.Break | Exit -> ()
    | Unix.Unix_error e fn txt ->
        mprintf "Unix error \"%s\" on '%s'\n%!" (Unix.error_message e) fn
    | e ->
        mprintf "%s\n%!" (Printexc.to_string e) ];
    terminate addr;
    flush stderr;
  }
};

main ();
