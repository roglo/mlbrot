(* $Id: info.ml,v 1.60 2014/04/06 02:51:56 deraugla Exp $ *)

open Info_def;
open Mcomm;
open Mdef;
open Mprintf;
open Palette_def;
open Printf;

value list_find f =
  loop where rec loop =
    fun
    [ [x :: l] -> if f x then Some x else loop l
    | [] -> None ]
;

value rec list_first n =
  fun
  [ [x :: l] -> if n <= 0 then [] else [x :: list_first (n - 1) l]
  | [] -> [] ]
;

value gen_refresh_pixmap_fast info show_fast =
  match info.g_info with
  [ Some (xinfo, gfun) ->
      gfun.g_refresh_pixmap_fast xinfo info.m_info show_fast
  | None -> () ]
;

value gen_make_window_visible info =
  match info.g_info with
  [ Some (xinfo, gfun) -> gfun.g_make_window_visible xinfo
  | None -> () ]
;

value gen_unselect_cursor info =
  match info.g_info with
  [ Some (xinfo, gfun) -> gfun.g_unselect_cursor xinfo
  | None -> () ]
;

value gen_pending_events info =
  match info.g_info with
  [ Some (xinfo, gfun) -> gfun.g_pending_events xinfo
  | None ->
      match info.m_info.slave_hiring with
      [ Some s ->
          let (sl, _, _) = Unix.select [s] [] [] 0.0 in
          if sl = [] then False else True
      | None -> False ] ]
;

value check_user_intervention info =
  if gen_pending_events info then
    loop () where rec loop () =
      let ev_opt =
        match info.g_info with
        [ Some (xinfo, gfun) -> gfun.g_get_pending_event info xinfo
        | None ->
            match info.m_info.slave_hiring with
            [ Some s -> Some (ME_hiring_ready s)
            | None -> None ] ]
      in
      match ev_opt with
      [ Some ev -> Some ev
      | None -> if gen_pending_events info then loop () else None ]
  else None
;

value set_times_and_nb_it ctx = do {
  let info = ctx.c_info in
  let isc = ctx.c_isc in
  let (exposing_user_time, exposing_total_user_time) =
    Mmisc.image_exposing_user_time info isc (Mmisc.user_time ())
  in
  isc.exposing_user_time := Some exposing_user_time;
  info.total_user_time := exposing_total_user_time;
  let (exposing_real_time, exposing_total_real_time) =
    Mmisc.image_exposing_real_time info isc (Mmisc.real_time ())
  in
  isc.exposing_real_time := Some exposing_real_time;
  info.total_real_time := exposing_total_real_time;
  isc.nb_it := ctx.c_fctx.f_nb_it;
};

value pause ctx info into_win = do {
  match info.g_info with
  [ Some (xinfo, gfun) -> do {
      gfun.g_select_cursor_pause xinfo;
      mprintf "pause\n";
      mflush ();
      loop_pause () where rec loop_pause () =
        match gfun.g_get_pending_event info xinfo with
        [ Some ev ->
            match ev with
            [ ME_gevent ev -> do {
                mprintf "resume\n";
                mflush ();
                gfun.g_select_cursor xinfo into_win;
                Some ev
              }
            | ME_pause -> do {
                mprintf "resume\n";
                mflush ();
                gfun.g_select_cursor xinfo into_win;
                None
              }
            | ME_hiring_ready s ->
                let _ : option _ = Compute.hire_slave ctx s in
                loop_pause ()
            | ME_environ_to_set s -> do {
                Compute.set_environ_for_socket ctx s;
                loop_pause ()
              } ]
        | None -> loop_pause () ]
    }
  | None -> None ]
};

value check_user_intervention_and_stop ctx info comp into_win =
  match check_user_intervention info with
  [ Some (ME_gevent ev) -> do {
      Compute.flush_remaining_answers ctx comp;
      set_times_and_nb_it ctx;
      (Some ev, comp)
    }
  | Some ME_pause ->
      match pause ctx info into_win with
      [ Some ev -> do {
          Compute.flush_remaining_answers ctx comp;
          set_times_and_nb_it ctx;
          (Some ev, comp)
        }
      | None -> (None, comp) ]
  | Some (ME_hiring_ready s) ->
      let comp =
        match Compute.hire_slave ctx s with
        [ Some s -> Compute.give_work_to_new_slave ctx comp s
        | None -> comp ]
      in
      (None, comp)
  | Some (ME_environ_to_set s) -> do {
      Compute.set_environ_for_socket ctx s;
      let comp = Compute.give_work_to_new_slave ctx comp s in
      (None, comp)
    }
  | None ->
      (None, comp) ]
;

value check_user_intervention_before_running ctx info into_win =
  match check_user_intervention info with
  [ Some (ME_gevent ev) -> do {
      if ctx.c_use_slaves then set_times_and_nb_it ctx else ();
      Some ev
    }
  | Some ME_pause -> do {
      if ctx.c_use_slaves then set_times_and_nb_it ctx else ();
      pause ctx info into_win
    }
  | Some (ME_hiring_ready s) -> do {
      if ctx.c_use_slaves then
        let _ : option Unix.file_descr = Compute.hire_slave ctx s in
        ()
      else ();
      None;
    }
  | Some (ME_environ_to_set s) -> do {
      if ctx.c_use_slaves then Compute.set_environ_for_socket ctx s else ();
      None
    }
  | None -> None ]
;

value periodic_check_user ctx info comp nb_computed eip into_win =
  match info.g_info with
  [ Some (xinfo, gfun) -> do {
      let minfo = info.m_info in
      minfo.pts_bef_test := minfo.pts_bef_test - 1;
      if minfo.pts_bef_test <= 0 then do {
        minfo.pts_bef_test := minfo.points_between_check_user;
        if into_win then do {
          let n = minfo.ymax - minfo.ymin + eip.eip_chunk + 1 in
          let fctx = ctx.c_fctx in
          let y = minfo.ymin - eip.eip_chunk/2 in
          if y >= 0 && n >= 0 then
            let nb_total = Compute.stack_len comp in
            let frac = float nb_computed /. float nb_total in
            gfun.g_make_area_visible xinfo eip frac (0, y, fctx.f_w, n)
          else ();
          minfo.ymin := fctx.f_h;
          minfo.ymax := 0;
        }
        else ();
        check_user_intervention_and_stop ctx info comp into_win
      }
      else
        (None, comp)
    }
  | None -> (None, comp) ]
;

value update_point_in_rect info isc eip rect i j count into_win = do {
  let x = rect.r_x in
  let y = rect.r_y in
  let w = rect.r_w in
  let h = rect.r_h in
  for k = 0 to eip.eip_chunk - 1 do {
    for l = 0 to eip.eip_chunk - 1 do {
      let i = i - eip.eip_chunk / 2 + k in
      let j = j - eip.eip_chunk / 2 + l in
      if i >= x && j >= y && i < x + w && j < y + h then
        Array2dim.set isc.counts_array i j count
      else ()
    }
  };
  match info.g_info with
  [ Some (xinfo, gfun) ->
      if into_win then
        gfun.update_point_in_pixmap xinfo eip.eip_chunk rect (i, j) count
      else ()
  | None -> () ]
};

value in_rect i j a =
  i >= a.r_x && i < a.r_x + a.r_w && j >= a.r_y && j < a.r_y + a.r_h
;

value update_point info isc eip i j count into_win =
  match list_find (in_rect i j) eip.eip_rect with
  [ Some a -> update_point_in_rect info isc eip a i j count into_win
  | None -> () ]
;

value refine_black_borders ctx info eip into_win = do {
  let minfo = info.m_info in
  let isc = ctx.c_isc in
  let nb_it = ctx.c_fctx.f_nb_it in
  let nb_pts_reach_lim = List.length isc.lim_reach in
  if nb_pts_reach_lim = 0 then (nb_it, None)
  else
    loop_nb nb_it nb_it nb_pts_reach_lim
    where rec loop_nb accum_nb_it nb_it old_nb_pts_reach_lim = do {
      let ctx = {(ctx) with c_fctx = {(ctx.c_fctx) with f_nb_it = nb_it}} in
      let comp =
        List.fold_left
          (fun comp (i, j, xn, yn) ->
             Compute.push ctx comp (i, j, Some (xn, yn)))
          Compute.empty isc.lim_reach
      in

      info.m_info.pts_bef_test := max 1 (info.m_info.pts_bef_test / 2);
      info.m_info.points_between_check_user :=
        max 1 (minfo.points_between_check_user / 2);

      let comp =
        if ctx.c_use_slaves then Compute.launch_slaves ctx comp else comp
      in
      loop nb_it comp 0 []
      where rec loop nb_it comp nb_computed lim_reach = do {
        let (r, comp) = Compute.pop ctx comp in
        match r with
        [ Compute.PR_some i j r -> do {
            let nb_computed = nb_computed + 1 in
            match r with
            [ Result n -> do {
                let n = n + accum_nb_it in
                update_point info isc eip i j (Some n) into_win;
                minfo.ymin := min minfo.ymin j;
                minfo.ymax := max minfo.ymax j;
                let (ev_opt, comp) =
                  periodic_check_user ctx info comp nb_computed eip into_win
                in
                let nb_it = max n nb_it in
                match ev_opt with
                [ Some ev -> (nb_it, Some ev)
                | None -> loop nb_it comp nb_computed lim_reach ]
              }
            | LimitReached xn yn _ -> do {
                let (ev_opt, comp) =
                  periodic_check_user ctx info comp nb_computed eip into_win
                in
                match ev_opt with
                [ Some ev -> (nb_it, Some ev)
                | None ->
                    loop nb_it comp nb_computed
                      [(i, j, xn, yn) :: lim_reach] ]
              } ]
          }
        | Compute.PR_none -> do {
            Compute.flush_remaining_answers ctx comp;
            isc.lim_reach := lim_reach;
            let nb_pts_reach_lim = List.length isc.lim_reach in
            if into_win then gen_make_window_visible info else ();
            if nb_pts_reach_lim <> 0 &&
               (old_nb_pts_reach_lim - nb_pts_reach_lim) * 100 /
                 old_nb_pts_reach_lim > 10
            then
              loop_nb (accum_nb_it + nb_it) (nb_it * 2) nb_pts_reach_lim
            else
              (nb_it, None)
          }
        | Compute.PR_x_event ->
            let (ev_opt, comp) =
              check_user_intervention_and_stop ctx info comp into_win
            in
            match ev_opt with
            [ Some ev -> (nb_it, Some ev)
            | None -> loop nb_it comp nb_computed lim_reach ] ]
      }
    }
};

value update_curr_y_in_rect eip i j =
  List.map (fun a -> if in_rect i j a then {(a) with r_curr_y = j} else a)
    eip.eip_rect
;

value compute_points ctx eip info comp update_nb_it into_win n_not_in_set
    dist_to_nb_it = do {
  let minfo = ctx.c_info in
  let isc = ctx.c_isc in
  let nb_it = ctx.c_fctx.f_nb_it in
  let ok_to_use_slaves = ctx.c_use_slaves in
  let comp =
    if ok_to_use_slaves then Compute.launch_slaves ctx comp else comp
  in
  loop 0 comp 0 where rec loop max_nb_it_found comp nb_computed =
    let (r, comp) = Compute.pop ctx comp in
    match r with
    [ Compute.PR_some i j r -> do {
        let nb_computed = nb_computed + 1 in
        let (count, max_nb_it_found) =
          match r with
          [ Result n -> do {
              incr n_not_in_set;
              dist_to_nb_it.val := dist_to_nb_it.val +. float (nb_it - n);
              (Some n, max max_nb_it_found n)
            }
          | LimitReached xn yn p -> do {
              if update_nb_it then
                isc.lim_reach := [(i, j, xn, yn) :: isc.lim_reach]
              else ();
              (if p = 0 then None else Some (16 * p), max_nb_it_found)
            } ]
        in
        update_point info isc eip i j count into_win;
        minfo.ymin := min minfo.ymin j;
        minfo.ymax := max minfo.ymax j;
        let rect = update_curr_y_in_rect eip i j in
        let eip = {(eip) with eip_rect = rect} in
        isc.exposing_state := ES_in_progress eip;
        let (ev_opt, comp) =
          periodic_check_user ctx info comp nb_computed eip into_win
        in
        match ev_opt with
        [ Some ev ->
            (Some (ev, Compute.first_missing_line ctx comp), max_nb_it_found)
        | None -> loop max_nb_it_found comp nb_computed ]
      }
    | Compute.PR_none -> do {
        Compute.flush_remaining_answers ctx comp;
        (None, max_nb_it_found)
      }
    | Compute.PR_x_event -> do {
        let (ev_opt, comp) =
          check_user_intervention_and_stop ctx info comp into_win
        in
        match ev_opt with
        [ Some ev ->
            (Some (ev, Compute.first_missing_line ctx comp), max_nb_it_found)
        | None -> loop max_nb_it_found comp nb_computed ]
      } ]
};

value first_chunk = 16;
value next_chunk chunk_size = chunk_size / 2;
value prev_chunk chunk_size = chunk_size * 2;

value end_chunk ctx info eip nb_it update_nb_it nb_try n_not_in_set
    dist_to_nb_it = do {
  let isc = ctx.c_isc in
  let minfo = info.m_info in
  let chunk_size = eip.eip_chunk in
  if chunk_size = 1 then do {
    let us_tm_now = Mmisc.user_time () in
    let (us_tm, tot_us_tm) =
      Mmisc.image_exposing_user_time info.m_info isc us_tm_now
    in
    let re_tm_now = Mmisc.real_time () in
    let (re_tm, tot_re_tm) =
      Mmisc.image_exposing_real_time info.m_info isc re_tm_now
    in
    isc.exposing_user_time := Some us_tm;
    isc.exposing_real_time := Some re_tm;
    minfo.total_user_time := tot_us_tm;
    minfo.total_real_time := tot_re_tm;
    let h = sprintf "level %g " ctx.c_level in
    mprintf "%s user time: %s (total time: %s)\n" h
      (Mutil.string_of_time us_tm)
      (Mutil.string_of_time minfo.total_user_time);
    mprintf "%s real time: %s (total time: %s)\n"
      (String.make (String.length h) ' ')
      (Mutil.string_of_time re_tm)
      (Mutil.string_of_time minfo.total_real_time);
    None
  }
  else
    let (chunk_size, nb_it) =
      if chunk_size = first_chunk &&
         isc.exposing_user_time = None && update_nb_it
      then do {
        incr nb_try;
        let av =
          if n_not_in_set = 0 then 0
          else Mutil.round (dist_to_nb_it /. float n_not_in_set)
        in
        if nb_try.val < 5 && av < 200 then do {
          isc.nb_it := nb_it * 2;
          (chunk_size, nb_it * 2)
        }
        else do {
          if av > 1500 then isc.nb_it := nb_it / 2 else ();
          (next_chunk chunk_size, nb_it)
        }
      }
      else (next_chunk chunk_size, nb_it)
    in
    Some (chunk_size, nb_it)
};

value set_auto_float minfo isc = do {
  minfo.auto_float := True;
  let n =
    IFDEF MPFR THEN
      if isc.level <= Mmisc.lim_flo minfo.lambda then N_flo else N_mpf
    ELSIFDEF MPZ THEN
      if isc.level <= Mmisc.lim_flo minfo.lambda then N_flo else N_mpz
    ELSE N_flo END
  in
  Mfloat.M.set_num n;
  isc.num_type := n;
};

value all_neighbours_same rect counts_array w h parent_i parent_j (di, dj) =
  parent_i - di >= rect.r_x && parent_i + di < rect.r_x + rect.r_w &&
  parent_j - dj >= rect.r_y && parent_j + dj < rect.r_y + rect.r_h &&
  let v = Array2dim.get counts_array parent_i parent_j in
  parent_i >= di && parent_j >= dj &&
  parent_i + di < w && parent_j + dj < h &&
  Array2dim.get counts_array (parent_i-di) (parent_j-dj) = v &&
  Array2dim.get counts_array (parent_i-di) (parent_j) = v &&
  Array2dim.get counts_array (parent_i-di) (parent_j+dj) = v &&
  Array2dim.get counts_array (parent_i) (parent_j-dj) = v &&
  Array2dim.get counts_array (parent_i) (parent_j+dj) = v &&
  Array2dim.get counts_array (parent_i+di) (parent_j-dj) = v &&
  Array2dim.get counts_array (parent_i+di) (parent_j) = v &&
  Array2dim.get counts_array (parent_i+di) (parent_j+dj) = v
;

value set_computing_in_rect ctx eip w h ini_chunk comp rect =
  let exposing_y = rect.r_curr_y in
  let wmax = rect.r_x + rect.r_w - 1 in
  let hmax = rect.r_y + rect.r_h - 1 in
  let isc = ctx.c_isc in
  let prev_chunk_size = prev_chunk eip.eip_chunk in
  loop_j comp hmax where rec loop_j comp j =
    if j < exposing_y then comp
    else
      let parent_j =
        if j mod prev_chunk_size = 0 then j else j - eip.eip_chunk
      in
      loop_i comp wmax where rec loop_i comp i =
        if i < rect.r_x then loop_j comp (j - eip.eip_chunk)
        else
          let parent_i =
            if i mod prev_chunk_size = 0 then i else i - eip.eip_chunk
          in
          let do_evaluate =
            if eip.eip_chunk = ini_chunk then True
            else if i = parent_i && j = parent_j then False
            else if
              all_neighbours_same rect isc.counts_array w h
                parent_i parent_j (prev_chunk_size, prev_chunk_size)
            then False
            else True
          in
          if not do_evaluate then do {
            let count =
              Array2dim.get isc.counts_array parent_i parent_j
            in
            Array2dim.set isc.counts_array i j count;
            loop_i comp (i - eip.eip_chunk)
          }
          else
            let comp = Compute.push ctx comp (i, j, None) in
            loop_i comp (i - eip.eip_chunk)
;

value set_computing ctx eip w h ini_chunk =
  List.fold_left (set_computing_in_rect ctx eip w h ini_chunk) Compute.empty
    eip.eip_rect
;

value trace_level minfo isc = do {
  mprintf "level %g type %s nb_it %d rot %s magn %s%s\n" isc.level
    (Mfloat.M.string_of_num_type ()) isc.nb_it (Int10.to_string isc.rot)
    (let reduc = Mfloat.Best.to_float isc.reduc in
     let er = 1. /. minfo.extra_reduc in
     if Mmisc.magn_of_reduc_overflows er reduc then
        "big" (* todo: try to display something better *)
     else
        sprintf "%g" (Mmisc.magn_of_reduc er reduc))
    (if isc.julia <> None then " julia" else "");
};

value set_num minfo isc n = do {
  minfo.auto_float := False;
  Mfloat.M.set_num n;
  isc.num_type := n;
  trace_level minfo isc;
};

value answering_interval = 0.2;

value estimate_speed mandelbrot_point fctx start_user_time =
  loop 1 where rec loop nb = do {
    let i = Random.int fctx.f_w in
    let j = Random.int fctx.f_h in
    match Mfloat.compute_point_with_fun mandelbrot_point fctx i j None with
    [ Result _ -> ()
    | LimitReached _ _ _ -> () ];
    if nb = 1 then loop (nb + 1)
    else
      let tm = Mmisc.user_time () -. start_user_time in
      let v = answering_interval *. float nb /. tm +. 0.5 in
      if v >= float max_int || tm < 0.03 then loop (nb + 1)
      else (max 1 (Mutil.round v), nb, tm)
  }
;

value expose_rect_to_chunk to_excl_chunk info isc eip w h update_nb_it
    into_win verbose = do {
  let ini_chunk = eip.eip_chunk in
  let minfo = info.m_info in
  match info.g_info with
  [ Some (xinfo, gfun) -> gfun.g_select_cursor xinfo into_win
  | None -> () ];
  let nb_it = max 1 isc.nb_it in
  let level = isc.level in
  let prec = Mutil.round level + if isc.julia <> None then 40 else 10 in
  if minfo.auto_float then set_auto_float minfo isc
  else Mfloat.M.set_num isc.num_type;
  if verbose then trace_level minfo isc else ();
  Mfloat.M.set_default_prec prec;
  if minfo.max_prec > prec then ()
  else do {
    isc.xc := Mfloat.Best.with_prec prec isc.xc;
    isc.yc := Mfloat.Best.with_prec prec isc.yc;
    isc.reduc := Mfloat.Best.with_prec prec isc.reduc;
    match isc.julia with
    [ None -> ()
    | Some (xc, yc) ->
        let xc = Mfloat.Best.with_prec prec xc in
        let yc = Mfloat.Best.with_prec prec yc in
        isc.julia := Some (xc, yc) ];
  };

  minfo.max_prec := max prec minfo.max_prec;
  minfo.start_user_time := Mmisc.user_time ();
  minfo.start_real_time := Mmisc.real_time ();
  let rot = Mfloat.trigo_of_rot isc.rot in
  let fctx =
    {f_w = w; f_h = h; f_rot = rot; f_reduc = isc.reduc;
     f_xc = isc.xc; f_yc = isc.yc; f_nb_it = nb_it;
     f_invert = minfo.invert; f_julia = isc.julia}
  in
  let mandelbrot_point =
    match minfo.mlb with
    [ Some m -> Mfloat.M.mandelbrot_point_m m
    | None ->
        if minfo.m3 then Mfloat.M.mandelbrot_point3
        else if minfo.lambda then Mfloat.M.lambda_point
        else Mfloat.M.mandelbrot_point ]
  in
  let (points_between_check_user, nb_pts, tm_for_nb_pts) =
    estimate_speed mandelbrot_point fctx minfo.start_user_time
  in
  let eval_tm =
    let surface =
      List.fold_left
        (fun surface rect -> surface +. float rect.r_w *. float rect.r_h)
        0. eip.eip_rect
    in
    tm_for_nb_pts *. surface /. float nb_pts
  in
  if verbose && into_win then do {
    mprintf "estimated time %s\n" (Mutil.string_of_time eval_tm);
  }
  else ();
  let ok_to_use_slaves =
    minfo.force_using_slaves = Always ||
(*
    minfo.force_using_slaves = IfNeeded && points_between_check_user < 2000
*)
    minfo.force_using_slaves = IfNeeded && level > 8.0 && eval_tm > 20.0
(**)
  in
  let ctx =
    {c_info = minfo; c_isc = isc; c_level = level;
     c_fctx = fctx; c_function = mandelbrot_point;
     c_pending_events _ = gen_pending_events info;
     c_use_slaves = ok_to_use_slaves}
  in
  if verbose && into_win then do {
    let has_slaves =
      not (Mfd.Set.is_empty minfo.free_slaves &&
           Mfd.Set.is_empty minfo.busy_slaves &&
           Mfd.Set.is_empty minfo.obsolete_slaves)
    in
    if has_slaves && minfo.force_using_slaves <> Never then do {
      if ok_to_use_slaves then
        let len = Mfd.set_length minfo.free_slaves in
        let len = len + Mfd.set_length minfo.obsolete_slaves in
        mprintf "... USING %s SLAVE%s\n"
          (if len = 0 then "NO" else string_of_int len)
          (if len < 2 then "" else "S")
      else
        mprintf "... it's fast: not needing slaves\n";
      mflush ();
    }
    else ()
  }
  else ();
  minfo.points_between_check_user := points_between_check_user;
  minfo.pts_bef_test := points_between_check_user;
  minfo.ymin := h;
  minfo.ymax := 0;
  Compute.set_environ ctx;
  minfo.slave_work := Mfd.Map.empty;
  let nb_try = ref 0 in
  loop_chunk eip nb_it where rec loop_chunk eip nb_it = do {

    let ctx = {(ctx) with c_fctx = {(ctx.c_fctx) with f_nb_it = nb_it}} in
    Compute.set_nb_it ctx;

    let n_not_in_set = ref 0 in
    let dist_to_nb_it = ref 0.0 in

    isc.exposing_state := ES_in_progress eip;

    let (ev_opt, max_nb_it_found_opt) =
      if not eip.eip_refining then do {
        let comp = set_computing ctx eip w h ini_chunk in
        isc.lim_reach := [];
        match check_user_intervention_before_running ctx info into_win with
        [ Some ev -> (Some ev, None)
        | None ->
            let (opt, max_nb_it_found) =
              compute_points ctx eip info comp update_nb_it into_win
                n_not_in_set dist_to_nb_it
            in
            let ev_opt =
              match opt with
              [ Some (ev, opt_ij) -> do {
                  let eip =
                    match opt_ij with
                    [ Some (i, j) ->
                        let a = update_curr_y_in_rect eip i j in
                        {(eip) with eip_rect = a}
                    | None ->
                        {(eip) with eip_refining = True} ]
                  in
                  isc.exposing_state := ES_in_progress eip;
                  Some ev
                }
              | None -> None ]
            in
            (ev_opt, Some max_nb_it_found) ]
      }
      else
        (None, None)
    in

    if update_nb_it then do {
      match max_nb_it_found_opt with
      [ Some max_nb_it_found -> do {
          let nb_it = isc.nb_it in
          let nb_it = (9 * nb_it + max_nb_it_found) / 10 in (* pif *)
(*
let _ = do { mprintf "updated nb_it %d (start %d)\n" nb_it isc.nb_it_at_start; flush stderr } in
*)
          isc.nb_it := nb_it
        }
      | None -> () ]
    }
    else ();

    match ev_opt with
    [ Some ev -> Some ev
    | None -> do {
        let eip = {(eip) with eip_refining = True} in
        isc.exposing_state := ES_in_progress eip;
        let (nb_it, ev_opt) =
          if update_nb_it then refine_black_borders ctx info eip into_win
          else (ctx.c_fctx.f_nb_it, None)
        in
        if into_win then gen_make_window_visible info else ();
        match ev_opt with
        [ Some ev -> Some ev
        | None ->
            let r =
              end_chunk ctx info eip nb_it update_nb_it nb_try
                n_not_in_set.val dist_to_nb_it.val
            in
            match r with
            [ Some (chunk_size, nb_it) ->
                if chunk_size = to_excl_chunk then do {
                  gen_unselect_cursor info;
                  isc.exposing_state := ES_terminated;
                  None
                }
                else do {
                  let eip =
                    {eip_chunk = chunk_size;
                     eip_rect =
                       List.map (fun a -> {(a) with r_curr_y = a.r_y})
                         eip.eip_rect;
                     eip_refining = False}
                  in
                  loop_chunk eip nb_it
                }
            | None -> do {
                gen_unselect_cursor info; 
                isc.exposing_state := ES_terminated;
                None
              } ] ]
      } ]
  }
};

value expose_rect info = expose_rect_to_chunk 0 info;

value finish_list info w h =
  loop False where rec loop has_worked =
    fun
    [ [isc :: iscl] ->
        match isc.exposing_state with
        [ ES_in_progress eip -> do {
            if not has_worked then do {
              mprintf "finishing history levels...\n";
            }
            else ();
            isc.counts_array := Array2dim.uncompress isc.counts_array;
            let r =
              try expose_rect info isc eip w h True False False with e -> do {
                isc.counts_array := Array2dim.compress isc.counts_array;
                raise e
              }
            in
            isc.counts_array := Array2dim.compress isc.counts_array;
            match r with
            [ Some ev -> Some ev
            | None -> loop True iscl ]
          }
        | ES_terminated -> loop has_worked iscl ]
    | [] -> do {
        if has_worked then do {
          mprintf "ok\n";
          let minfo = info.m_info in
          trace_level minfo minfo.state.cur;
        }
        else ();
        None
      } ]
;

value finish_exposing_remaining_levels info w h =
  match finish_list info w h info.m_info.state.bef with
  [ Some ev -> Some ev
  | None -> finish_list info w h info.m_info.state.aft ]
;

value expose_mandel_kont info eip w h update_nb_it verbose = do {
  let isc = info.m_info.state.cur in
  match expose_rect info isc eip w h update_nb_it True verbose with
  [ Some ev -> Some ev
  | None -> finish_exposing_remaining_levels info w h ]
};

value init_state_vars isc = do {
  isc.work :=
    {w_active_slaves = Mfd.Map.empty; w_term_slaves = []; w_master = 0};
  isc.nb_diff_answ := 0;
};

value expose_mandel_from_chunk ini_chunk info w h update_nb_it verbose = do {
  let isc = info.m_info.state.cur in
  let eip =
    {eip_chunk = ini_chunk;
     eip_rect = [{r_x = 0; r_y = 0; r_w = w; r_h = h; r_curr_y = 0}];
     eip_refining = False}
  in
  isc.exposing_user_time := None;
  isc.exposing_real_time := None;
  init_state_vars isc;
  isc.nb_it_at_start := min (isc.nb_it_at_start * 120 / 100) isc.nb_it;
  isc.nb_it := isc.nb_it_at_start;
  try expose_mandel_kont info eip w h update_nb_it verbose with
  [ Failure s -> do {
      mprintf "Failed: %s\n" s;
      None
    } ]
};

value expose_mandel info = expose_mandel_from_chunk first_chunk info;

value expose_fast info isc w h f g = do {
  let nb_it = isc.nb_it in
  let n_bef_it = ref 0 in
  let dist_to_nb_it = ref 0.0 in
  let c =
    Array2dim.init (Array2dim.dim1 isc.counts_array)
      (Array2dim.dim2 isc.counts_array)
      (fun i j -> do {
         let i = f i in
         let j = g j in
         let n = Array2dim.get isc.counts_array i j in
         match n with
         [ Some n -> do {
             incr n_bef_it;
             dist_to_nb_it.val := dist_to_nb_it.val +. float (nb_it - n);
           }
         | None -> () ];
         n
       })
  in
(**)
  let av =
    if n_bef_it.val = 0 then 0
    else Mutil.round (dist_to_nb_it.val /. float n_bef_it.val)
  in
  if av < 200 then isc.nb_it := nb_it * 2 else ();
(**)
  Array2dim.blit c isc.counts_array;
  gen_refresh_pixmap_fast info True;
  let eip =
    {eip_chunk = 1;
     eip_rect = [{r_x = 0; r_y = 0; r_w = w; r_h = h; r_curr_y = 0}];
     eip_refining = False}
  in
  isc.exposing_user_time := None;
  isc.exposing_real_time := None;
  init_state_vars isc;
  expose_rect info isc eip w h True True True
};

value push_state info = do {
  if info.hist_size = 0 then ()
  else do {
    let isc = info.state.cur in
    let copied_cur =
      {(isc) with counts_array = Array2dim.compress isc.counts_array}
    in
    let bef = list_first (info.hist_size - 1) info.state.bef in
    info.state := {(info.state) with bef = [copied_cur :: bef]; aft = []};
    Gc.compact ();
  };
  init_state_vars info.state.cur;
};

value expose_slow info isc w h = do {
  let eip =
    {eip_chunk = first_chunk;
     eip_rect = [{r_x = 0; r_y = 0; r_w = w; r_h = h; r_curr_y = 0}];
     eip_refining = False}
  in
  isc.exposing_user_time := None;
  isc.exposing_real_time := None;
  init_state_vars isc;
  expose_rect info isc eip w h True True True
};


value expose_optim info x y w h = do {
  let isc = info.m_info.state.cur in
  let optim =
    if x > w / 4 && x < 3 * w / 4 && y > h / 4 && y < 3 * h / 4 then
      match isc.exposing_state with
      [ ES_in_progress eip ->
          if isc.lim_reach = [] && eip.eip_chunk = 1 then
            if eip.eip_refining then False
            else
              List.for_all (fun rect -> rect.r_curr_y > y + h / 4)
                eip.eip_rect
          else False
      | ES_terminated -> True ]
    else False
  in
  if optim then
    (* optimization: using what was already computed *)
    match
      expose_fast info isc w h
        (fun i -> i / 2 + x - w / 4) (fun j -> j / 2 + y - h / 4)
    with
    [ Some ev -> Some ev
    | None -> finish_exposing_remaining_levels info w h ]
  else
    (* normal case *)
    expose_mandel info w h True True
};

value expose_move info dx dy w h first_chunk_for_glob = do {
  let isc = info.m_info.state.cur in
  let f i = i + dx in
  let g j = j + dy in
  let c =
    Array2dim.init (Array2dim.dim1 isc.counts_array)
      (Array2dim.dim2 isc.counts_array)
      (fun i1 j1 ->
         let i = f i1 in
         let j = g j1 in
         if i < 0 || i >= w || j < 0 || j >= h then None
         else Array2dim.get isc.counts_array i j)
  in
  Array2dim.blit c isc.counts_array;
  match info.g_info with
  [ Some (xinfo, gfun) ->
      let (x, w, x_dest) =
        if dx >= 0 then (dx, w - dx, 0) else (0, w + dx, -dx)
      in
      let (y, h, y_dest) =
        if dy >= 0 then (dy, h - dy, 0) else (0, h + dy, -dy)
      in
      gfun.g_copy_area xinfo info.m_info (x, y, w, h) (x_dest, y_dest)
  | None -> () ];
  let (x1, y1, w1, h1, x2, y2, w2, h2) =
    let x1 = 0 in
    let y1 = if dy >= 0 then h - dy else 0 in
    let w1 = w in
    let h1 = abs dy in
    let x2 = if dx >= 0 then w - dx else 0 in
    let y2 = if dy < 0 then - dy else 0 in
    let w2 = abs dx in
    let h2 = h - dy in
    let x1 = max 0 (min w x1) in
    let y1 = max 0 (min h y1) in
    let w1 = max 0 (min (w - x1) w1) in
    let h1 = max 0 (min (h - y1) h1) in
    let x2 = max 0 (min w x2) in
    let y2 = max 0 (min h y2) in
    let w2 = max 0 (min (w - x2) w2) in
    let h2 = max 0 (min (h - y2) h2) in
    if y1 < y2 then (x1, y1, w1, h1, x2, y2, w2, h2)
    else (x2, y2, w2, h2, x1, y1, w1, h1)
  in
  let (x1, y1, w1, h1) =
    (x1 - first_chunk_for_glob, y1 - first_chunk_for_glob,
     w1 + 2 * first_chunk_for_glob, h1 + 2 * first_chunk_for_glob)
  in
  let (x2, y2, w2, h2) =
    (x2 - first_chunk_for_glob, y2 - first_chunk_for_glob,
     w2 + 2 * first_chunk_for_glob, h2 + 2 * first_chunk_for_glob)
  in
  isc.exposing_user_time := None;
  isc.exposing_real_time := None;
  init_state_vars isc;
  let a1 = {r_x = x1; r_y = y1; r_w = w1; r_h = h1; r_curr_y = y1} in
  let a2 = {r_x = x2; r_y = y2; r_w = w2; r_h = h2; r_curr_y = y2} in
  let eip =
    {eip_chunk = first_chunk; eip_rect = [a2; a1]; eip_refining = False}
  in
  if first_chunk_for_glob = 0 then do {
    expose_rect_to_chunk 0 info isc eip w h True True True
  }
  else do {
    let nb_it_before_move = isc.nb_it in
    match
      if first_chunk_for_glob = first_chunk then None
      else
        expose_rect_to_chunk first_chunk_for_glob info isc eip w h True True
          False
    with
    [ Some ev -> Some ev
    | None -> do {
(*
        printf "nb_it (%d) := nb_it_before_move (%d)\n" isc.nb_it
          nb_it_before_move;
        flush stdout;
*)
        isc.nb_it := nb_it_before_move;
        expose_mandel_from_chunk first_chunk_for_glob info w h True False
      } ]
  }
};

value get_pixel_value_fun minfo w h =
  let ica = minfo.state.cur.counts_array in
  if minfo.half_size then
    let f i j =
      let c =
        List.fold_left
          (fun c (di, dj) ->
             match Array2dim.get ica (2*i+di) (2*j+dj) with
             [ None -> c
             | Some c1 -> c + c1 + 1 ])
          0 [(0, 0); (0, 1); (1, 0); (1, 1)]
      in
      c / 4
    in
    (f, w / 2, h / 2)
  else
    let f i j =
      match Array2dim.get ica i j with
      [ Some c -> c + 1
      | None -> 0 ]
    in
    (f, w, h)
;

value get_pixel_color_fun minfo w h =
  let ict = minfo.c_pal.c_tab in
  let (get_pixel_value, w, h) = get_pixel_value_fun minfo w h in
  (get_pixel_value, Mmisc.color_of_nb_iter ict, w, h)
;

value refresh_period = 8;
value deg_by_image = 1.0;
value pixel_shift = 10.0;
value zoom_coeff = 0.985;

(*
value pixel_shift = 5.0;
value deg_by_image = 0.5;
value zoom_coeff = 0.993;
*)

value zoom_coeff_sqrt_2 = zoom_coeff ** (sqrt 2. /. 2.);

value apply_zoom_unzoom zooming info w h reduc_opt slow_down ppm_from = do {
  let minfo = info.m_info in
  let slow_speed =
    if slow_down then
      match reduc_opt with
      [ Some cr -> do {
          let dr = Mfloat.Best.to_float minfo.state.cur.reduc in
          let n = (log cr -. log dr) /. log zoom_coeff in
          printf "n = %g\n" n;
          flush stdout;
          let n = 2. *. n in
          let log_ss =
            (log cr -. log dr -. n *. log zoom_coeff) /.
               (n *. (n +. 1.) /. 2.)
          in
          exp log_ss
        }
      | None -> 1.0 ]
    else 1.0
  in
  if slow_down then do {
    printf "slow speed %g\n" slow_speed;
    flush stdout;
  }
  else ();
  let (ev_opt, start_expose) =
    if minfo.ppm_cnt >= ppm_from - refresh_period - 1 then
      (expose_slow info minfo.state.cur w h, True)
    else
      (None, minfo.ppm_cnt >= ppm_from)
  in
  loop zoom_coeff start_expose ev_opt 1 where rec
  loop zoom_coeff start_expose ev_opt cnt =
    match ev_opt with
    [ Some ev -> Some ev
    | None ->
        let stop =
          match reduc_opt with
          [ Some r -> do {
              let cr = Mfloat.Best.to_float info.m_info.state.cur.reduc in
let _ = if True || minfo.ppm_cnt >= ppm_from - 1 then do { mprintf "curr magn %g (dest magn %g)\n" (Mmisc.magn_of_reduc minfo.extra_reduc cr) (Mmisc.magn_of_reduc minfo.extra_reduc r); mflush () } else () in
              if zooming then cr <= r else cr >= r
            }
          | None -> False ]
        in
        if stop then None
        else do {
          let get_pixel_color = get_pixel_color_fun minfo in
          Mmisc.output_image minfo ppm_from (get_pixel_color w h);
          push_state minfo;
          let isc = minfo.state.cur in
          let new_reduc =
            (if zooming then Mfloat.Best.mult else Mfloat.Best.div)
              isc.reduc (Mfloat.Best.of_float zoom_coeff)
          in
          isc.reduc := new_reduc;
          let new_level = Mmisc.level_of_reduc minfo.extra_reduc new_reduc in
          isc.level := new_level;
          if minfo.auto_float then
            IFDEF MPFR THEN
              if new_level > Mmisc.lim_flo minfo.lambda then
                isc.num_type := N_mpf
              else ()
            ELSIFDEF MPZ THEN
              if new_level > Mmisc.lim_flo minfo.lambda then
                isc.num_type := N_mpz
              else ()
            ELSE () END
          else ();
          let start_expose =
            start_expose ||
            minfo.ppm_cnt > ppm_from - refresh_period - 1 && cnt = 0
          in
(*
mprintf "cnt %d minfo.ppm_cnt %d ppm_from %d\n" cnt minfo.ppm_cnt ppm_from;
mprintf "start_expose %b\n" start_expose;
*)
          let ev_opt =
            if not start_expose then None
            else if zooming && cnt <> 0 then
              (* optimization: using what was already computed *)
              let di = (1. -. zoom_coeff) *. float (w / 2) in
              let dj = (1. -. zoom_coeff) *. float (h / 2) in
              expose_fast info isc w h
                (fun i -> Mutil.round (zoom_coeff *. float i +. di))
                (fun j -> Mutil.round (zoom_coeff *. float j +. dj))
            else
              (* normal case (or every 'refresh_period' times, because
                 degradation) *)
              expose_slow info isc w h
          in
          let cnt = (cnt + 1) mod refresh_period in
          let zoom_coeff =
            if slow_down then zoom_coeff *. slow_speed else zoom_coeff
          in
          if slow_down then do {
            printf "zoom coeff %g\n" zoom_coeff;
            flush stdout
          }
          else ();
          loop zoom_coeff start_expose ev_opt cnt
        } ]
};

value apply_zoom info = apply_zoom_unzoom True info;
value apply_unzoom info = apply_zoom_unzoom False info;

(* set angle between -179 et 180 *)
value normalize_angle a =
  Int10.sub_int (Int10.mod_int (Int10.add_int a (360 + 179)) 360) 179
;

value smooth_move_zoom = True;

value apply_move_rotate_zoom info w h xy_opt rot_opt with_zoom direct
    ppm_from = do {
  let minfo = info.m_info in
  let isc = minfo.state.cur in
  let prec_xy_opt =
    match xy_opt with
    [ Some (x, y) -> do {
        let prec = Mmisc.max_prec_of_level_and_coord isc.level x y in
        minfo.max_prec := max prec minfo.max_prec;
        Mfloat.M.set_default_prec prec;
        let x_dest = Mfloat.Best.of_string prec x in
        let y_dest = Mfloat.Best.of_string prec y in
        match isc.julia with
        [ Some _ -> isc.julia := Some (x_dest, y_dest)
        | None -> () ];
        Some (prec, x_dest, y_dest)
      }
    | None -> None ]
  in

  let (n1, move_opt) =
    match prec_xy_opt with
    [ Some (prec, x_dest, y_dest) -> do {
        let ddx = Mfloat.Best.sub x_dest isc.xc in
        let ddy = Mfloat.Best.sub y_dest isc.yc in
        let di =
          Mutil.round (Mfloat.Best.to_float (Mfloat.Best.div ddx isc.reduc))
        in
        let dj =
          Mutil.round (Mfloat.Best.to_float (Mfloat.Best.div ddy isc.reduc))
        in
        let sqr x = x *. x in
        let d = sqrt (sqr (float di) +. sqr (float dj)) in
        let n =
          let pixels_by_image = pixel_shift /. minfo.extra_reduc in
          Mutil.round (d /. pixels_by_image)
        in
        (max 1 n, Some (prec, ddx, ddy, x_dest, y_dest))
      }
    | None -> (0, None) ]
  in

  let (n2, rotate_opt) =
    match rot_opt with
    [ Some rot ->
        let src_rot = isc.rot in
        let dest_rot = normalize_angle rot in
        let diff = normalize_angle (Int10.sub dest_rot src_rot) in
        let n =
          Int10.truncate
            (Int10.of_float (Int10.to_float (Int10.abs diff) /. deg_by_image))
        in
        (n, Some (src_rot, dest_rot, diff))
    | None -> (0, None) ]
  in

  let n =
    if direct then 0
    else
      let n = max n1 n2 in
      if with_zoom then Mutil.round (float n *. sqrt 2.) else n
  in
let _ = do { if direct then () else mprintf "%d steps\n" n } in

  let move_opt =
    match move_opt with
    [ Some (prec, ddx, ddy, x_dest, y_dest) ->
        if n = 0 then Some (ddx, ddy, x_dest, y_dest)
        else
          let nn = Mfloat.Best.of_string prec (string_of_int n) in
          let nn =
            if with_zoom then
              let c =
                if smooth_move_zoom then Mfloat.Best.of_float zoom_coeff
                else Mfloat.Best.of_float zoom_coeff_sqrt_2
              in
              loop 0 (Mfloat.Best.of_float 0.0) (Mfloat.Best.of_float 1.0)
              where rec loop k sum_zi zi =
                if k = n then sum_zi
                else
                  let zik =
                    if smooth_move_zoom then
                      let v = if k <= n / 2 then 4 * k else 4 * (n - k) in
                      let fn = Mfloat.Best.of_float (float_of_int n) in
                      Mfloat.Best.div (Mfloat.Best.mult_int zi v) fn
                    else zi
                  in
                  loop (k + 1) (Mfloat.Best.add sum_zi zik)
                    (Mfloat.Best.mult zi c)
            else nn
          in
          let dx = Mfloat.Best.div ddx nn in
          let dy = Mfloat.Best.div ddy nn in
          Some (dx, dy, x_dest, y_dest)
    | None -> None ]
  in

  let rot = Mfloat.trigo_of_rot isc.rot in

  loop 0 isc.xc isc.yc (Mfloat.Best.of_float 1.0)
  where rec loop k xc yc zi = do {
let _ = if True || minfo.ppm_cnt >= ppm_from - 1 then do { if direct then () else mprintf "step %d/%d\n" k n } else () in
    let isc = minfo.state.cur in
    let prev_xc = isc.xc in
    let prev_yc = isc.yc in
    match move_opt with
    [ Some (dx, dy, x_dest, y_dest) -> do {
        isc.xc := if k = n then x_dest else xc;
        isc.yc := if k = n then y_dest else yc;
      }
    | None -> () ];
    match rotate_opt with
    [ Some (src_rot, dest_rot, diff) ->
        let a =
          if k = n then
            dest_rot
          else if n = 0 then
            src_rot
          else
            Int10.add src_rot (Int10.div_int (Int10.mul_int diff k) n)
        in
        isc.rot := normalize_angle a
    | None -> () ];

    if with_zoom then do {
      let new_reduc =
        let c =
          if n = 0 then
            if smooth_move_zoom then zoom_coeff
            else zoom_coeff_sqrt_2
          else
            if smooth_move_zoom then
              let v = if k <= n/2 then n - 2 * k else 2 * k - n in
              1.0 +. (zoom_coeff -. 1.0) *. float v /. float n
            else zoom_coeff_sqrt_2
        in
        Mfloat.Best.mult isc.reduc (Mfloat.Best.of_float c)
      in
      isc.reduc := new_reduc;
      let new_level = Mmisc.level_of_reduc minfo.extra_reduc new_reduc in
      isc.level := new_level;
      if minfo.auto_float then
        IFDEF MPFR THEN
          if new_level > Mmisc.lim_flo minfo.lambda then isc.num_type := N_mpf
          else ()
        ELSIFDEF MPZ THEN
          if new_level > Mmisc.lim_flo minfo.lambda then isc.num_type := N_mpz
          else ()
        ELSE () END
      else ();
    }
    else ();

    if direct then None
    else
      let ev_opt =
        match (move_opt, rotate_opt) with
        [ (Some (_, _, x_dest, y_dest), None) -> do {
            let (i, j) =
              Mfloat.pos_of_coord rot isc.reduc (prev_xc, prev_yc)
                (isc.xc, isc.yc) (w, h)
            in
            if k < n then do {
              (* adjust the center according to (i, j); not sure it is the
                 good thing to do but it is the only way I found to make
                 'expose_move' work correctly *)
              let (xc, yc) =
                Mfloat.coord_of_pos rot isc.reduc (prev_xc, prev_yc) (i, j)
                  (w, h)
              in
              isc.xc := xc;
              isc.yc := yc;
            }
            else ();
            if minfo.ppm_cnt < ppm_from - 1 then
              if minfo.ppm_cnt >= ppm_from - refresh_period - 1 then
                expose_slow info minfo.state.cur w h
              else
                None
            else if
              k = n || with_zoom && k mod refresh_period = refresh_period - 1
            then expose_slow info isc w h
            else do {
              let di = (2 * i - w) / 2 in
              let dj = (2 * j - h) / 2 in
              match expose_move info di dj w h 0 with
              [ Some ev -> Some ev
              | None ->
                  if with_zoom then
                    let c =
                      if smooth_move_zoom then
                        let v = if k <= n/2 then n - 2 * k else 2 * k - n in
                        1.0 +. (zoom_coeff -. 1.0) *. float v /. float n
                      else zoom_coeff_sqrt_2
                    in
                    let di = (1. -. c) *. float (w / 2) in
                    let dj = (1. -. c) *. float (h / 2) in
                    expose_fast info isc w h
                      (fun i -> Mutil.round (c *. float i +. di))
                      (fun j -> Mutil.round (c *. float j +. dj))
                  else None ]
            }
          }
        | _ ->
            if minfo.ppm_cnt < ppm_from - 1 then None
            else expose_slow info isc w h ]
      in
      match ev_opt with
      [ Some ev ->
          Some ev
      | None -> do {
          let get_pixel_color = get_pixel_color_fun minfo in
          Mmisc.output_image minfo ppm_from (get_pixel_color w h);
          if k = n then
            None
          else
            let (xc, yc) =
              match move_opt with
              [ Some (dx, dy, x_dest, y_dest) ->
                  if with_zoom then
                    let zi =
                      if smooth_move_zoom then
                        let v = if k <= n / 2 then 4 * k else 4 * (n - k) in
                        let fn = Mfloat.Best.of_float (float_of_int n) in
                        Mfloat.Best.div (Mfloat.Best.mult_int zi v) fn
                      else zi
                    in
                    let xc = Mfloat.Best.add xc (Mfloat.Best.mult dx zi) in
                    let yc = Mfloat.Best.add yc (Mfloat.Best.mult dy zi) in
                    (xc, yc)
                  else
                    let xc = Mfloat.Best.add xc dx in
                    let yc = Mfloat.Best.add yc dy in
                    (xc, yc)
              | None -> (xc, yc) ]
            in
            let zi =
              if with_zoom then
                let c =
                  if smooth_move_zoom then Mfloat.Best.of_float zoom_coeff
                  else Mfloat.Best.of_float zoom_coeff_sqrt_2
                in
                Mfloat.Best.mult zi c
              else zi
            in
            loop (k + 1) xc yc zi
        } ]
  }
};

value install_palette info pd = do {
  let minfo = info.m_info in
  let max_ds = minfo.c_pal.max_ds in
  let max_dv = minfo.c_pal.max_dv in
  let pal = Palette.of_palette_def max_ds max_dv pd in
  minfo.c_pal.c_tab := pal;
  match info.g_info with
  [ Some (xinfo, gfun) -> gfun.g_set_palette xinfo pal
  | None -> () ]
};

value push_palette info pd pdl = do {
  let minfo = info.m_info in
  minfo.pal_defs :=
    {bef = [minfo.pal_defs.cur :: minfo.pal_defs.bef]; cur = pd;
     aft = pdl};
  minfo.c_pal_def := pd;
  install_palette info pd
};

value pop_palette info pd pdl = do {
  let minfo = info.m_info in
  minfo.pal_defs :=
    {bef = pdl; cur = pd;
     aft = [minfo.pal_defs.cur :: minfo.pal_defs.aft]};
  minfo.c_pal_def := pd;
  install_palette info pd
};

value apply_palette info pd w h direct ppm_from = do {
  let minfo = info.m_info in
  push_state minfo;
  if direct then ()
  else do {
    let (max_ds, max_dv) =
      match info.g_info with
      [ Some (xinfo, _) ->
          let max_ds = minfo.c_pal.max_ds in
          let max_dv = minfo.c_pal.max_dv in
          (max_ds, max_dv)
      | None -> (0, 0) ]
    in
    let src_pal = minfo.c_pal.c_tab in
    let dst_pal = Palette.of_palette_def max_ds max_dv pd in
    let nsteps = 30 in
    let (get_pixel_value, w, h) = get_pixel_value_fun minfo w h in
    let color_of_nb_iter n c =
      if c = 0 then (0, 0, 0)
      else
        let c = c - 1 in
        let (r1, g1, b1) = src_pal.(c mod Array.length src_pal).rgb in
        let (r2, g2, b2) = dst_pal.(c mod Array.length dst_pal).rgb in
        (r1 + (r2 - r1) * n / nsteps,
         g1 + (g2 - g1) * n / nsteps,
         b1 + (b2 - b1) * n / nsteps)
    in
    for n = 0 to nsteps do {
      Mmisc.output_image minfo ppm_from
        (get_pixel_value, color_of_nb_iter n, w, h)
    }
  };
  push_palette info pd [];
  match info.g_info with
  [ Some (xinfo, gfun) ->
      if minfo.ppm_cnt < ppm_from then ()
      else gfun.g_refresh_pixmap_fast xinfo minfo False
  | None -> () ];
  None
};

type scenario =
  [ SC_direct of action
  | SC_by_steps of action ]

and action =
  [ SA_zoom of option float and bool
  | SA_unzoom of float
  | SA_move of string and string
  | SA_pause of int
  | SA_rotate of Int10.t
  | SA_move_rotate of string and string and Int10.t
  | SA_move_zoom of string and string
  | SA_iterations of int
  | SA_palette of Palette_def.t ]
;

value apply_scenario_action info w h direct sc_it ppm_from =
  let minfo = info.m_info in
  let (w, h) =
    match info.g_info with
    [ Some (xinfo, gfun) -> gfun.g_widget_size xinfo
    | None -> (w, h) ]
  in
  match sc_it with
  [ SA_zoom m_opt slow_down -> do {
      mprintf "*** scenario%s zoom%s" (if direct then " direct" else "")
        (if slow_down then " slow down" else "");
      match m_opt with
      [ Some f -> eprintf " to %g" f
      | None -> () ];
      eprintf "\n";
      mflush ();
let isc = minfo.state.cur in
mprintf "isc.xc %s isc.yc %s\n" (Mfloat.Best.to_string isc.xc) (Mfloat.Best.to_string isc.yc); mflush ();
      let r_opt =
        match m_opt with
        [ Some m -> Some (Mmisc.reduc_of_magn minfo.extra_reduc m)
        | None -> None ]
      in
      if direct then do {
        match r_opt with
        [ Some r -> do {
            let isc = minfo.state.cur in
            let new_reduc = Mfloat.Best.of_float r in
            isc.reduc := new_reduc;
            isc.level := Mmisc.level_of_reduc minfo.extra_reduc new_reduc;
          }
        | None -> () ];
        None
      }
      else
        apply_zoom info w h r_opt slow_down ppm_from
    }
  | SA_unzoom m -> do {
      mprintf "*** scenario%s unzoom to %g\n"
        (if direct then " direct" else "") m;
      mflush ();
      let r = Mmisc.reduc_of_magn minfo.extra_reduc m in
      if direct then do {
        let isc = minfo.state.cur in
        let new_reduc = Mfloat.Best.of_float r in
        isc.reduc := new_reduc;
        isc.level := Mmisc.level_of_reduc minfo.extra_reduc new_reduc;
        None
      }
      else
        apply_unzoom info w h (Some r) False ppm_from
    }
  | SA_move x y -> do {
      let isc = minfo.state.cur in
      mprintf "*** scenario%s move (from %s %s) to %s %s\n"
        (if direct then " direct" else "")
        (Mfloat.Best.to_string isc.xc) (Mfloat.Best.to_string isc.yc) x y;
      mflush ();
      apply_move_rotate_zoom info w h (Some (x, y)) None False direct ppm_from
    }
  | SA_pause n -> do {
      mprintf "*** scenario pause %d images\n" n;
      mflush ();
      let get_pixel_color = get_pixel_color_fun minfo in
      for i = 0 to n - 1 do {
        Mmisc.output_image minfo ppm_from (get_pixel_color w h)
      };
      None
    }
  | SA_rotate r -> do {
      let isc = minfo.state.cur in
      mprintf "*** scenario rotate (from %s) to %s\n"
        (Int10.to_string isc.rot) (Int10.to_string r);
      mflush ();
      apply_move_rotate_zoom info w h None (Some r) False direct ppm_from
    }
  | SA_move_rotate x y r -> do {
      let isc = minfo.state.cur in
      mprintf "*** scenario%s move rotate\n"
        (if direct then " direct" else "");
      mprintf "    moving (from %s %s) to %s %s\n"
        (Mfloat.Best.to_string isc.xc) (Mfloat.Best.to_string isc.yc)
        x y;
      mprintf "    rotating (from %s) to %s\n" (Int10.to_string isc.rot)
        (Int10.to_string r);
      mflush ();
      apply_move_rotate_zoom info w h (Some (x, y)) (Some r) False direct
        ppm_from
    }
  | SA_move_zoom x y -> do {
      let isc = minfo.state.cur in
      mprintf "*** scenario%s move zoom\n"
        (if direct then " direct" else "");
      mprintf "    moving (from %s %s) to %s %s\n"
        (Mfloat.Best.to_string isc.xc) (Mfloat.Best.to_string isc.yc)
        x y;
      mprintf "    and zooming\n";
      mflush ();
      apply_move_rotate_zoom info w h (Some (x, y)) None True direct ppm_from
    }
  | SA_iterations i -> do {
      mprintf "*** scenario set iterations %d\n" i;
      mflush ();
      minfo.state.cur.nb_it := max 1 i;
      None
    }
  | SA_palette pd -> do {
      mprintf "*** scenario set palette %s\n" (Palette.def_to_string pd);
      mflush ();
      apply_palette info pd w h direct ppm_from
    }]
;

value apply_scenario_item info w h sc_it ppm_from last_was_direct =
  let (direct, action) =
    match sc_it with
    [ SC_direct a -> (True, a)
    | SC_by_steps a ->
        match (a, info.m_info.state.cur.julia) with
        [ (SA_move_zoom x y, Some _) -> (True, SA_move x y)
        | _ -> (False, a) ] ]
  in
  let ev_opt =
    if not direct &&
      (last_was_direct && info.m_info.ppm_cnt > ppm_from ||
       info.m_info.ppm_cnt = ppm_from)
    then
      expose_slow info info.m_info.state.cur w h
    else None
  in
  match ev_opt with
  [ Some _ -> (ev_opt, direct)
  | None -> (apply_scenario_action info w h direct action ppm_from, direct) ]
;

value apply_scenario info w h scenario ppm_from =
  loop False scenario where rec loop last_was_direct =
    fun
    [ [sc_it :: scen] -> do {
        let (ev_opt, was_direct) =
          apply_scenario_item info w h sc_it ppm_from last_was_direct
        in
        match ev_opt with
        [ Some ev -> Some ev
        | None -> loop was_direct scen ]
      }
    | [] ->
        if info.m_info.ppm_cnt >= ppm_from && last_was_direct then
          expose_slow info info.m_info.state.cur w h
        else None ]
;

#load "pa_extend.cmo";

value scenario_lex = Plexer.gmake ();
value scenario_gram = Grammar.gcreate scenario_lex;
value scenario_eof = Grammar.Entry.create scenario_gram "scenario";

EXTEND
  GLOBAL: scenario_eof;
  scenario_eof: [ [ l = LIST0 scenario; EOI -> l ] ];
  scenario:
    [ [ "direct"; a = action; ";" -> SC_direct a
      | a = action; ";" -> SC_by_steps a ] ]
  ;
  action:
    [ [ "zoom"; m = FLOAT -> SA_zoom (Some (float_of_string m)) False
      | "zoom" -> SA_zoom None False
      | "zoom_slow_down"; m = FLOAT -> SA_zoom (Some (float_of_string m)) True
      | "unzoom"; m = FLOAT -> SA_unzoom (float_of_string m)
      | "move"; x = signed_float; y = signed_float -> SA_move x y
      | "pause"; x = INT -> SA_pause (int_of_string x)
      | "rotate"; x = signed_int -> SA_rotate (Int10.of_int (int_of_string x))
      | "move_rotate"; x = signed_float; y = signed_float; r = signed_int ->
          SA_move_rotate x y (Int10.of_int (int_of_string r))
      | "move_zoom"; x = signed_float; y = signed_float -> SA_move_zoom x y
      | "iterations"; i = INT -> SA_iterations (int_of_string i)
      | "palette"; pd = palette_def -> SA_palette pd ] ]
  ;
  palette_def:
    [ [ hl = LIST1 INT SEP ","; "/"; permut = INT; ","; ca = color_algo;
        "/"; sat = INT; ","; ds = INT; "/"; bri = INT; ","; db = INT ->
          let hl = List.map int_of_string hl in
          let permut = int_of_string permut in
          let sat = int_of_string sat in
          let ds = int_of_string ds in
          let bri = int_of_string bri in
          let db = int_of_string db in
          {hues = hl; permutation = permut; saturation = sat;
           delta_saturation = ds; brightness = bri; delta_brightness  = db;
           color_algo = ca}
     | pd = STRING -> Palette.def_of_string pd ] ]
  ;
  color_algo:
    [ [ "b" -> CA_start_with_black
      | "w" -> CA_start_with_white
      | "n" -> CA_no_gradation ] ]
  ;
  signed_float:
    [ [ f = FLOAT -> f
      | "-"; f = FLOAT -> "-" ^ f ] ]
  ;
  signed_int:
    [ [ i = INT -> i
      | "-"; i = INT -> "-" ^ i ] ]
  ;
END;

value parse_scenario cs = Grammar.Entry.parse scenario_eof cs;

value parse_scenario_file fname =
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic -> do {
      let r =
        try parse_scenario (Stream.of_channel ic) with
        [ Ploc.Exc loc e -> do {
            let err_line = Ploc.line_nb loc in
            let c1 = Ploc.first_pos loc - Ploc.bol_pos loc in
            let c2 = Ploc.last_pos loc - Ploc.bol_pos loc in
            eprintf "File \"%s\", line %d, characters %d-%d\n"
              fname err_line c1 c2;
            flush stderr;
            match e with
            [ Stream.Error m -> do {
                eprintf "Syntax error: %s\n" m;
                flush stderr;
                exit 2
              }
            | _ -> raise e ]
          }]
      in
      close_in ic;
      r
    }
  | None -> do {
      eprintf "Cannot open '%s'\n" fname;
      flush stderr;
      exit 2
    } ]
;

value common_init () = do {
  Sys.catch_break True;
  (* to prevent being killed in case of write on a dead slave;
     will raise the exception Unix_error EPIPE instead. *)
  let _ : Sys.signal_behavior =
    Sys.signal Sys.sigpipe Sys.Signal_ignore
  in ();
  let scenario_opt =
    match Mmisc.arg_scenario_file.val with
    [ Some fname -> Some (parse_scenario_file fname)
    | None -> None ]
  in
  let slave_hiring =
    match Mmisc.arg_slave_hiring.val with
    [ Some str -> do {
        let addr =
          try Unix.ADDR_INET Unix.inet_addr_any (int_of_string str) with
          [ Failure _ ->
              if Sys.file_exists str then do {
                mprintf "error: file \'%s\' exists.\n" str;
                exit 2
              }
              else Unix.ADDR_UNIX str ]
        in
        let s = Mutil.socket_and_bind addr in
        (* nonblock because 'accept' may block even after a 'select' saying
           there is something to read. See manual page 'accept(2)' *)
        Unix.set_nonblock s;
        Some (s, Mmisc.arg_master_lazy.val)
      }
    | None -> None ]
  in
  let (w, h, p) =
    let (sz, p) = Mmisc.arg_geometry.val in
    let (w, h) =
      match sz with
      [ Some (w, h) -> (w, h)
      | None -> (Mmisc.default_init_wid, Mmisc.default_init_hei) ]
    in
    (w, h, p)
  in
  let rs =
    let a =
      match Mmisc.arg_seed.val with
      [ Some s -> [| s |]
      | None -> [| 0; 1 |] ]
    in
    Random.State.make a
  in
  let c_pal_def =
    match Mmisc.arg_palette.val with
    [ Some palette_def -> palette_def
    | None -> Palette.def_of_string "rainbow" ]
  in
  let c_pal = Palette.make c_pal_def in
  (w, h, p, rs, c_pal_def, c_pal, slave_hiring, scenario_opt)
};

value batch () = do {
  let (w, h, pos, rs, c_pal_def, c_pal, slave_hiring, scenario_opt) =
    common_init ()
  in
  let minfo = Mmisc.make_minfo rs w h c_pal_def c_pal slave_hiring in
  let info = {m_info = minfo; g_info = None} in
  try
    match scenario_opt with
    [ Some scenario ->
        let _ : option _ =
          apply_scenario info w h scenario Mmisc.arg_ppm_from.val
        in
        ()
    | None ->
        let _ : option _ = expose_slow info minfo.state.cur w h in
        let _ : option _ =
          apply_zoom info w h None False Mmisc.arg_ppm_from.val
        in
        () ]
  with
  [ Sys.Break -> do {
      mflush ();
      Mmisc.print_command minfo w h None;
    } ]
};

value display_interesting_points () = do {
  let (w, h, pos, rs, c_pal_def, c_pal, slave_hiring, scenario_opt) =
    common_init ()
  in
  let minfo = Mmisc.make_minfo rs w h c_pal_def c_pal slave_hiring in
  let info = {m_info = minfo; g_info = None} in
  let _ : option _ = expose_slow info minfo.state.cur w h in
  let int_pts =
    Mmisc.iter_points minfo w h 0
      (fun cl x _ _ ->
         if List.length cl > 6 then Some (x + 1, True) else None)
  in
  printf "%d\n" int_pts;
  flush stdout;
};
