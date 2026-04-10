(* $Id: compute.ml,v 1.80 2017/12/28 10:50:13 deraugla Exp $ *)

open Mcomm;
open Mdef;
open Mprintf;
open Printf;

module Fifo :
  sig
    type t 'a = 'abstract;
    value empty : t 'a;
    value put : t 'a -> 'a -> t 'a;
    value take : t 'a -> option ('a * t 'a);
    value fold : ('a -> 'b -> 'a) -> 'a -> t 'b -> 'a;
  end =
  struct
    type t 'a = {head : list 'a; tail : list 'a};
    value empty = {head = []; tail = []};
    value put f x = {(f) with tail = [x :: f.tail]};
    value take f =
      match f.head with
      [ [x :: l] -> Some (x, {(f) with head = l})
      | [] ->
          match List.rev f.tail with
          [ [x :: l] -> Some (x, {head = l; tail = []})
          | [] -> None ] ]
    ;
    value fold f accu fifo =
      let accu = List.fold_left f accu fifo.head in
      List.fold_left f accu fifo.tail
    ;
  end
;

type simple_question = (int * int * option (Mfloat.M.t * Mfloat.M.t));

type t =
  { pending : Fifo.t (simple_question * result Mfloat.M.t);
    to_do : list simple_question;
    to_do_len : int;
    slaves : Mfd.Map.t (list (question Mfloat.M.t));
    ready_slaves : (float * list Unix.file_descr);
    had_slaves : bool }
;

type pop_result =
  [ PR_some of int and int and result Mfloat.M.t
  | PR_none
  | PR_x_event ]
;

value empty =
  {pending = Fifo.empty; to_do = []; to_do_len = 0; slaves = Mfd.Map.empty;
   ready_slaves = (0.0, []); had_slaves = False}
;

value compute_point ctx i j start =
  Mfloat.compute_point_with_fun ctx.c_function ctx.c_fctx i j start
;

value slave_name info s =
  match Mfd.map_find s info.slave_name with
  [ Some sl_name -> sl_name
  | None -> sprintf "socket %d" (Mutil.int_of_file_descr s) ]
;

value unix_select_read minfo comp sl tmout =
  try
    let (sl, _, _) = Unix.select sl [] [] tmout in
    sl
  with e -> do {
    mflush ();
    match e with
    [ Sys.Break -> raise e
    | _ -> () ];
    eprintf "\nUnix.select in error\n";
    eprintf "Dumping context:\n";
    let ob_sl = List.sort compare (Mfd.Set.elements minfo.obsolete_slaves) in
    eprintf "- %d obsolete slaves:\n" (List.length ob_sl);
    List.iter
      (fun s ->
         eprintf "  * %d: %s\n" (Mutil.int_of_file_descr s)
           (slave_name minfo s))
      ob_sl;
    let csl =
      List.sort compare
        (Mfd.Map.fold (fun s ijl sl -> [(s, ijl) :: sl]) comp.slaves [])
    in
    eprintf "- %d computing slaves:\n" (List.length csl);
    List.iter
      (fun (s, ijl) ->
         eprintf "  * %d: %s (%d questions)\n" (Mutil.int_of_file_descr s)
           (slave_name minfo s) (List.length ijl))
      csl;
    let sl = List.sort compare sl in
    eprintf "- %d parameters of Unix.select\n" (List.length sl);
    List.iter
      (fun s ->
         eprintf "  * %d: %s\n" (Mutil.int_of_file_descr s)
           (slave_name minfo s))
      sl;
    eprintf "\n";
    flush stderr;
    raise e
  }
;

value unix_write s b ofs len =
(*
  let (_, sl, _) = Unix.select [] [s] [] 0.0 in
  if List.length sl <> 1 then
    failwith "Compute.unix_write"
  else
*)
    try Unix.write s b ofs len with
    [ Unix.Unix_error (Unix.EPIPE | Unix.ECONNRESET) _ _ -> 0
    | Unix.Unix_error e fn _ -> do {
        mprintf "Unix error \"%s\" on '%s'\n" (Unix.error_message e) fn;
        0
      } ]
;

value internal_error name v = do {
  mprintf "internal error in '%s'\n" name;
  mprintf "trying to continue...\n";
  v
};

value expand_question sql q =
  loop q.q_len q.q_i where rec loop len i =
    if len = 0 then sql
    else
      let sq = (i, q.q_j, q.q_start) in
      [sq :: loop (len - 1) (i + q.q_di)]
;

value shrink_question ql (i, j, start) =
  match ql with
  [ [q :: ql] when
        start = None && j = q.q_j && (q.q_di = 0 || q.q_i - i = q.q_di) ->
      let q = {(q) with q_i = i; q_di = q.q_i - i; q_len = q.q_len + 1} in
      [q :: ql]
  | _ ->
      let q = {q_i = i; q_j = j; q_start = start; q_di = 0; q_len = 1} in
      [q :: ql] ]
;

value give_up_slave_not_running ctx s = do {
  let w = ctx.c_isc.work in
  let info = ctx.c_info in
  let sl_name = slave_name info s in
  mprintf "giving up slave '%s'\n" sl_name;
  if Mfd.Set.mem s info.busy_slaves then do {
    info.busy_slaves := Mfd.Set.remove s info.busy_slaves;
    let n =
      try Mfd.Map.find s w.w_active_slaves with
      [ Not_found -> {ps_nb = []} ]
    in
    w.w_term_slaves := [(n, sl_name) :: w.w_term_slaves];
  }
  else ();
  info.free_slaves := Mfd.Set.remove s info.free_slaves;
  info.obsolete_slaves := Mfd.Set.remove s info.obsolete_slaves;
  w.w_active_slaves := Mfd.Map.remove s w.w_active_slaves;
  try Unix.close s with [ Unix.Unix_error _ _ _ -> () ];
};

value give_up_slave ctx comp s = do {
  give_up_slave_not_running ctx s;
  match Mfd.map_find s comp.slaves with
  [ Some ijl ->
      let to_do = List.fold_left expand_question comp.to_do (List.rev ijl) in
      {(comp) with to_do = to_do; slaves = Mfd.Map.remove s comp.slaves}
  | None -> 
      comp ]     
};

value gen_send_request s sl_name request =
  let len = Bytes.length request in
  let wlen = unix_write s request 0 len in
  if wlen <> len then do {
    mprintf "slave '%s' not working (1)\n" sl_name;
    False
  }
  else True
;

value check_slave_version s sl_name =
  let request = Mmagic.comm_magic in
  if gen_send_request s sl_name (Bytes.of_string request) then True
  else do {
    mprintf "giving up slave '%s'\n" sl_name;
    False
  }
;

value send_request ctx s sl_name request =
  if gen_send_request s sl_name request then ()
  else give_up_slave_not_running ctx s
;

value serialize = Mfloat.Best.serialize;

value set_environ_for_socket ctx s = do {
  let fctx = ctx.c_fctx in
  let request =
    Marshal.to_string
      (Set_environ
         {e_level = Mutil.round ctx.c_level;
          e_formula =
            match ctx.c_info.mlb with
            [ Some m -> m
            | None -> if ctx.c_info.m3 then 3 else 2 ];
          e_lambda = ctx.c_info.lambda;
          e_num_type = ctx.c_isc.num_type;
          e_rot = ctx.c_isc.rot;
          e_reduc = serialize fctx.f_reduc;
          e_xc = serialize fctx.f_xc;
          e_yc = serialize fctx.f_yc;
          e_nb_it = fctx.f_nb_it; e_w = fctx.f_w; e_h = fctx.f_h;
          e_invert = fctx.f_invert;
          e_julia =
            Mutil.map_option (fun (xc, yc) -> (serialize xc, serialize yc))
              fctx.f_julia;
          e_with_mpfr = IFDEF MPFR THEN True ELSE False END})
      [Marshal.No_sharing]
  in
  let sl_name = slave_name ctx.c_info s in
  send_request ctx s sl_name (Bytes.of_string request)
};

value set_nb_it_for_socket ctx s = do {
  let request =
    Marshal.to_string (Set_nb_it ctx.c_fctx.f_nb_it) [Marshal.No_sharing]
  in
  let sl_name = slave_name ctx.c_info s in
  send_request ctx s sl_name (Bytes.of_string request)
};

value set_environ ctx =
  Mfd.Set.iter (set_environ_for_socket ctx) ctx.c_info.free_slaves
;

value set_nb_it ctx =
  Mfd.Set.iter (set_nb_it_for_socket ctx) ctx.c_info.free_slaves
;

value map_question f q =
  {(q) with
   q_start = Mutil.map_option (fun (xn, yn) -> (f xn, f yn)) q.q_start}
;

value lose_patience = 4;
value stream_computing_time = 4.0;
value first_slave_work = 1;
value minimum_slave_work = 10;

value give_work_to_slave ctx comp how_many s tm_end_prev = do {
(*
let ini_how_many = how_many in
*)
  let info = ctx.c_info in
  let max_for_slave =
    match Mfd.map_find s info.slave_work with
    [ Some (pts, tm_start) -> do {
        let computed_time = tm_end_prev -. tm_start in
        if False && computed_time >= stream_computing_time then do {
          let sl_name = slave_name info s in
          mprintf "slave '%s' computed %d points in %s\n" sl_name pts
            (Mutil.string_of_time computed_time);
        }
        else ();
        let fpts = float pts /. computed_time *. stream_computing_time in
        let m = if fpts >= float max_int then max_int else truncate fpts in
        let (speed, speed_hist) =
          try Mfd.Map.find s info.slave_speed with
          [ Not_found -> (0, []) ]
        in
        let speed_hist =
          loop 9 [m :: speed_hist] where rec loop rest list =
            if rest = 0 then []
            else
              match list with
              [ [x :: l] -> [x :: loop (rest - 1) l]
              | [] -> [] ]
        in
        let speed =
          let len = List.length speed_hist in
          (2 * List.fold_left \+ 0 speed_hist + len) / (2 * len)
        in
        info.slave_speed :=
          Mfd.Map.add s (speed, speed_hist) info.slave_speed;
        max m first_slave_work
      }
    | None -> first_slave_work ]
  in
  let how_many = min how_many max_for_slave in
  let tm_now = Unix.gettimeofday () in
  info.slave_work := Mfd.Map.add s (how_many, tm_now) info.slave_work;
  let (ijl, to_do) =
    loop [] how_many comp.to_do where rec loop rev_ijl n =
      fun
      [ [ij :: to_do] ->
          if n > 0 then loop [ij :: rev_ijl] (n - 1) to_do
          else (List.rev rev_ijl, [ij :: to_do])
      | [] ->
          (List.rev rev_ijl, []) ]
  in
  let ql = List.fold_left shrink_question [] (List.rev ijl) in
(*
  Mutil.trace "socket %d: giving %d points (%d)\n"
    (Mutil.int_of_file_descr s) how_many ini_how_many;
*)
  let ija =
    Array.of_list (List.map (map_question Mfloat.M.to_string) ql)
  in
  let req = Marshal.to_bytes (Compute_values ija) [Marshal.No_sharing] in
  let len = Bytes.length req in
  if unix_write s req 0 len <> len then do {
    let sl_name = slave_name info s in
    mprintf "slave '%s' not working\n" sl_name;
    (give_up_slave ctx comp s, False)
  }
  else
    let comp =
      {(comp) with to_do = to_do; slaves = Mfd.Map.add s ql comp.slaves;
       had_slaves = True}
    in
    (comp, True)
};

value push ctx comp (i, j, start) =
  {(comp) with to_do = [(i, j, start) :: comp.to_do];
   to_do_len = comp.to_do_len + 1}
;

value stack_len comp = comp.to_do_len;

value how_many_by_slave ctx comp =
  let nslaves =
    Mfd.set_length ctx.c_info.free_slaves +
    Mfd.set_length ctx.c_info.busy_slaves
  in
  let how_many = List.length comp.to_do / (nslaves + 1) / 2 in
  max minimum_slave_work how_many
;

value launch_slaves ctx comp =
  if not (Mfd.Set.is_empty ctx.c_info.free_slaves) then do {
    let tm_end_prev = Unix.gettimeofday () in
    let how_many = how_many_by_slave ctx comp in
    let available_slaves = Mfd.Set.elements ctx.c_info.free_slaves in
    let slaves_by_speed =
      let minfo = ctx.c_info in
      List.sort
        (fun s1 s2 ->
           let (sp1, _) =
             try Mfd.Map.find s1 minfo.slave_speed with
             [ Not_found -> (0, []) ]
           in
           let (sp2, _) =
             try Mfd.Map.find s2 minfo.slave_speed with
             [ Not_found -> (0, []) ]
           in
           compare sp2 sp1)
        available_slaves
    in
    let (comp, free_sl, busy_sl) =
      List.fold_left
        (fun (comp, free_sl, busy_sl) s ->
           if comp.to_do = [] then
             (comp, Mfd.Set.add s free_sl, busy_sl)
           else
             let (comp, success) =
               give_work_to_slave ctx comp how_many s tm_end_prev
             in
             let busy_sl =
               if success then Mfd.Set.add s busy_sl else busy_sl
             in
             (comp, free_sl, busy_sl))
        (comp, Mfd.Set.empty, Mfd.Set.empty) slaves_by_speed
    in
    ctx.c_info.free_slaves := free_sl;
    ctx.c_info.busy_slaves := busy_sl;
    comp
  }
  else comp
;

value give_more_work ctx comp s tm_end_prev =
  if comp.to_do = [] then
    if Mfd.Set.mem s ctx.c_info.busy_slaves then do {
      ctx.c_info.free_slaves := Mfd.Set.add s ctx.c_info.free_slaves;
      ctx.c_info.busy_slaves := Mfd.Set.remove s ctx.c_info.busy_slaves;
      comp
    }
    else do {
      let comp = internal_error "give_more_work" comp in
      if Mfd.Set.mem s ctx.c_info.free_slaves then do {
        mprintf "strange: already in free_slaves\n";
      }
      else ();
      if Mfd.Set.mem s ctx.c_info.obsolete_slaves then do {
        mprintf "strange: in obsolete_slaves\n";
      }
      else ();
      comp
    }
  else
    let how_many = how_many_by_slave ctx comp in
    fst (give_work_to_slave ctx comp how_many s tm_end_prev)
;

value eq_result x y =
  match (x, y) with
  [ (Result a, Result b) -> a = b
  | (LimitReached x1 y1 p1, LimitReached x2 y2 p2) -> True
  | _ -> False ]
;

value optional_check_answer ctx s i j start n =
  let nb_attempts = 5 in
  if ctx.c_info.check_slaves_answers &&
     ctx.c_isc.nb_diff_answ < nb_attempts
  then
    let na = compute_point ctx i j start in
    if not (eq_result na n) then do {
      ctx.c_isc.nb_diff_answ := ctx.c_isc.nb_diff_answ + 1;
      if ctx.c_isc.nb_diff_answ = nb_attempts then do {
        mprintf "...\n";
      }
      else do {
        let sl_name = slave_name ctx.c_info s in
        mprintf "different answers from %s\n" sl_name;
        mprintf "- slave answer (%d, %d) = %s\n" i j (Mutil.sprint_result n);
        mprintf "- program answer = %s\n" (Mutil.sprint_result na);
      }
    }
    else ()
  else ()
;

value pop_to_do ctx comp =
  if ctx.c_isc.master_lazy && not (Mfd.Map.is_empty comp.slaves) then
    (None, comp)
  else
    match comp.to_do with
    [ [(i, j, start) :: to_do] -> do {
        if start = None then
          ctx.c_isc.work.w_master := ctx.c_isc.work.w_master + 1
        else ();
        let r = compute_point ctx i j start in
        let comp = {(comp) with to_do = to_do} in
        (Some (i, j, r), comp)
      }
    | [] ->
        (None, comp) ]
;

value hire_slave_not_working info s =
  match
    try Some (Unix.accept s) with [ Unix.Unix_error Unix.EAGAIN _ _ -> None ]
  with
  [ Some (s, addr) -> do {
      let sl_name = sprintf "socket %d" (Mutil.int_of_file_descr s) in
      if check_slave_version s sl_name then do {
        info.free_slaves := Mfd.Set.add s info.free_slaves;
        Some s
      }
      else do {
        try Unix.close s with [ Unix.Unix_error _ _ _ -> () ];
        None
      }
    }
  | None -> None ]
;

value hire_slave ctx s =
  match hire_slave_not_working ctx.c_info s with
  [ Some s -> do {
      set_environ_for_socket ctx s;
      Some s
    }
  | None -> None ]
;

value give_work_to_new_slave ctx comp s =
  if ctx.c_use_slaves && comp.to_do <> [] then do {
    let tm_now = Unix.gettimeofday () in
    let info = ctx.c_info in
    info.free_slaves := Mfd.Set.remove s info.free_slaves;
    info.busy_slaves := Mfd.Set.add s info.busy_slaves;
    fst (give_work_to_slave ctx comp first_slave_work s tm_now)
  }
  else comp
;

value adding_slave info sl_name s = do {      
  let len = Mfd.set_length info.free_slaves in
  let len = len + Mfd.set_length info.busy_slaves in
  let len = len + Mfd.set_length info.obsolete_slaves in
  mprintf "... ADDING SLAVE '%s' (%d SLAVE%s NOW)\n" sl_name len
    (if len < 2 then "" else "S");
  mflush ();
  info.slave_name := Mfd.Map.add s sl_name info.slave_name;
};

value obsolete_slave ctx info s = do {
  info.obsolete_slaves := Mfd.Set.remove s info.obsolete_slaves;
  info.busy_slaves := Mfd.Set.remove s info.busy_slaves;
  info.free_slaves := Mfd.Set.add s info.free_slaves;
  set_environ_for_socket ctx s;
};

value list_rev_combine =
  loop [] where rec loop accu l1 l2 =
     match (l1, l2) with
     [ ([], []) -> accu
     | ([a1 :: l1], [a2 :: l2]) -> loop [(a1, a2) :: accu] l1 l2
     | (_, _) -> invalid_arg "list_rev_combine" ]
;

value one_socket_answer ctx comp tm_end s =
  let info = ctx.c_info in
  match Mutil.input_value s with
  [ None -> do {
      mprintf "slave '%s' terminated\n" (slave_name info s);
      give_up_slave ctx comp s
    }
  | Some (Answ_set_name sl_name) -> do {
      adding_slave info sl_name s;
      comp
    }
  | Some (Answ_values (na : array (result (int * string)))) ->
      match Mfd.map_find s comp.slaves with
      [ Some ql ->
          let ijl = List.fold_left expand_question [] (List.rev ql) in
          if Array.length na = List.length ijl then do {
            let na =
              Array.map
                (Mutil.map_result
                   (fun (prec, x) -> Mfloat.M.of_string prec x))
                na
            in
            let comp = {(comp) with slaves = Mfd.Map.remove s comp.slaves} in
            let comp = give_more_work ctx comp s tm_end in
            let len = Array.length na in
(*
            let ijnl = List.rev (list_rev_combine ijl (Array.to_list na)) in
            if ctx.c_info.check_slaves_answers then
              List.iter
                (fun ((i, j, start), n) ->
                   optional_check_answer ctx s i j start n)
                ijnl
            else ();
*)
            match ijl with
            [ [(_, _, None) :: _] ->
                let work = ctx.c_isc.work in
                let len =
                  match
                    try Some (Mfd.Map.find s work.w_active_slaves) with
                    [ Not_found -> None ]
                  with
                  [ Some w -> {ps_nb = [len :: w.ps_nb]}
                  | None -> {ps_nb = [len]} ]
                in
                work.w_active_slaves := Mfd.Map.add s len work.w_active_slaves
            | [(_, _, Some _) :: _] | [] -> () ];
            let (pending, _) =
              List.fold_left
                (fun (fifo, k) ij -> (Fifo.put fifo (ij, na.(k)), k+1))
                (comp.pending, 0) ijl
            in
            {(comp) with pending = pending}
          }
          else do {
            mprintf "slave '%s' bad answer\n" (slave_name info s);
            mprintf "sent %d pts received %d pts\n"
              (List.length ijl) (Array.length na);
            Mfd.Map.iter
              (fun s1 ql ->
                 if s = s1 then do {
                   let ijl = List.fold_left expand_question [] ql in
                   mprintf "- found request of size %d\n"
                     (List.length ijl);
                 }
                 else ())
              comp.slaves;
            give_up_slave ctx comp s
          }
      | None ->
          if Mfd.Set.mem s info.obsolete_slaves then do {
            obsolete_slave ctx info s;
            give_work_to_new_slave ctx comp s
          }
          else do {
            internal_error "socket_answer 2" ();
            comp
          } ] ]
;

value rec socket_answer ctx comp tm_end =
  fun
  [ [s :: sl] ->
      let comp = {(comp) with ready_slaves = (tm_end, sl)} in
      let comp = one_socket_answer ctx comp tm_end s in
      match Fifo.take comp.pending with
      [ Some (((i, j, _), r), pending) ->
          (Some (i, j, r), {(comp) with pending = pending})
      | None ->
          socket_answer ctx comp tm_end sl ]
  | [] ->
      let (r, comp) = pop_to_do ctx comp in
      match r with
      [ Some (i, j, r) -> (Some (i, j, r), comp)
      | None -> (None, comp) ] ]
;

value give_up_too_long ctx comp sl =
  List.fold_left
    (fun comp s -> do {
       let sl_name = slave_name ctx.c_info s in
       mprintf "answer too long to come from '%s'\n" sl_name;
       give_up_slave ctx comp s
     })
  comp sl
;

value flush_remaining_answers ctx comp = do {
  let info = ctx.c_info in
  info.obsolete_slaves := Mfd.Set.union info.busy_slaves info.obsolete_slaves;
  info.busy_slaves := Mfd.Set.empty
};

value pop_result (r, comp) =
  match r with
  [ Some (i, j, r) -> (PR_some i j r, comp)
  | None -> (PR_none, comp) ]
;

value rec pop_not_pending ctx comp =
  let info = ctx.c_info in
  if not ctx.c_use_slaves then pop_result (pop_to_do ctx comp)
  else
    let (r, comp) =
      if not (Mfd.Map.is_empty comp.slaves) ||
         not (Mfd.Set.is_empty info.obsolete_slaves)
      then
        if ctx.c_pending_events () then (PR_x_event, comp)
        else if snd comp.ready_slaves <> [] then
          let tm_end = fst comp.ready_slaves in
          let ready_slaves = snd comp.ready_slaves in
          pop_result (socket_answer ctx comp tm_end ready_slaves)
        else
          let sl = Mfd.Set.elements info.obsolete_slaves in
          let sl = Mfd.Map.fold (fun s _ sl -> [s :: sl]) comp.slaves sl in
          let sl = unix_select_read info comp sl 0.0 in
          let tm_end = Unix.gettimeofday () in
          if sl = [] then
            let (r, comp) = pop_to_do ctx comp in
            match r with
            [ Some (i, j, r) -> (PR_some i j r, comp)
            | None -> (PR_none, comp) ]
          else
            pop_result (socket_answer ctx comp tm_end sl)
      else (PR_none, comp)
    in
    match r with
    [ PR_some _ _ _ | PR_x_event -> (r, comp)
    | PR_none ->
        let (r, comp) = pop_to_do ctx comp in
        if r <> None then pop_result (r, comp)
        else if not (Mfd.Map.is_empty comp.slaves) then do {
          let sl = Mfd.Map.fold (fun s _ sl -> [s :: sl]) comp.slaves [] in
          let wait_sec = lose_patience in
          loop_wait wait_sec where rec loop_wait n =
            if ctx.c_pending_events () then do {
              if n = wait_sec || ctx.c_isc.master_lazy then ()
              else eprintf " event\n%!";
              (PR_x_event, comp)
            }
            else do {
              let slr_opt =
                loop 9 where rec loop m =
                  let slr = unix_select_read info comp sl 0.1 in
                  if slr <> [] || m = 0 then Some slr
                  else if ctx.c_pending_events () then None
                  else loop (m - 1)
              in
              match slr_opt with
              | None → (PR_x_event, comp)
              | Some slr →
                  let tm_end = Unix.gettimeofday () in
                  if slr = [] then
                    if n > 0 then do {
                      if ctx.c_isc.master_lazy then ()
                      else if n = wait_sec then do {
                        mprintf "waiting for";
                        match sl with
                        [ [s] ->
                            let sl_name = slave_name info s in
                            eprintf " '%s' (%d pts)" sl_name
                              (try fst (Mfd.Map.find s info.slave_work) with
                               [ Not_found -> 0 ])
                        | [] | [_ :: _] ->
                            eprintf " %d slaves" (List.length sl) ];
                        eprintf "...%!";
                      }
                      else eprintf " %d...%!" n;
                      loop_wait (n - 1)
                    }
                    else do {
                      if ctx.c_isc.master_lazy then ()
                      else eprintf " nothing\n%!";
                      let comp = give_up_too_long ctx comp sl in
                      let comp = launch_slaves ctx comp in
                      pop_not_pending ctx comp
                    }
                  else do {
                    if ctx.c_isc.master_lazy then ()
                    else if n < wait_sec then do {
                      match sl with
                      [ [_] -> eprintf " ok"
                      | [] | [_ :: _] ->
                          let len = List.length slr in
                          eprintf " ok (%d %s)" len
                            (if len = 1 then "answer" else "answers") ];
                      eprintf "\n";
                      flush stderr;
                    }
                    else ();
                    let (r, comp) = socket_answer ctx comp tm_end slr in
                    match r with
                    [ Some (i, j, r) -> (PR_some i j r, comp)
                    | None -> pop_not_pending ctx comp ]
                  }
              end
            }
        }
        else (PR_none, comp) ]
;

value pop ctx comp =
  match Fifo.take comp.pending with
  [ Some (((i, j, _), r), pending) ->
      (PR_some i j r, {(comp) with pending = pending})
  | None ->
      pop_not_pending ctx comp ]
;

value first_missing_line ctx comp =
  if comp.had_slaves then
    let (i, j) =
      Fifo.fold
        (fun (ifirst, jfirst) ((i, j, _), _) ->
           if j < jfirst then (i, j) else (ifirst, jfirst))
        (0, ctx.c_fctx.f_h) comp.pending
    in
    let (i, j) =
      Mfd.Map.fold
        (fun _ ql (ifirst, jfirst) ->
           List.fold_left
             (fun (ifirst, jfirst) q ->
                if q.q_j < jfirst then (q.q_i, q.q_j) else (ifirst, jfirst))
             (ifirst, jfirst) ql)
        comp.slaves (i, j)
    in
    Some (i, j)
  else
    match comp.to_do with
    [ [(i, j, _) :: _] -> Some (i, j)
    | [] -> None ]
;
