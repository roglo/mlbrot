(* $Id: mmisc.ml,v 1.44 2017/12/28 10:50:13 deraugla Exp $ *)

(* maximum of things not dependent on the graphic toolkit *)

open Mcomm;
open Mdef;
open Mprintf;
open Palette_def;
open Printf;

value option_map f =
  fun
  [ Some x -> Some (f x)
  | None -> None ]
;

value rec merge_uniq x =
  fun
  [ [y :: l] -> if y = x then [y :: l] else [y :: merge_uniq x l]
  | [] -> [x] ]
;

value user_time () =
  let tm = Unix.times () in
  tm.Unix.tms_utime +. tm.Unix.tms_stime
;

value real_time () = Unix.gettimeofday ();

value color_of_nb_iter ict n =
  if n = 0 then (0, 0, 0)
  else ict.((n - 1) mod Array.length ict).rgb
;

value output_ppm oc (nb_iter_of_pixel, color_of_nb_iter, w, h) = do {
  fprintf oc "P6\n";
  fprintf oc "%d %d\n" w h;
  fprintf oc "255\n";
  for j = 0 to h - 1 do {
    for i = 0 to w - 1 do {
      let (r, g, b) = color_of_nb_iter (nb_iter_of_pixel i j) in
      output_byte oc r;
      output_byte oc g;
      output_byte oc b;
    }
  };
};

value output_ppm_file minfo pfx get_pixel_color = do {
  let fn = sprintf "%s%05d.ppm" pfx minfo.ppm_cnt in
  mprintf "output file %s\n" fn;
  let oc = open_out_bin pfx in
  output_ppm oc get_pixel_color;
  close_out oc;
  try Sys.remove fn with [ Sys_error _ -> () ];
  Sys.rename pfx fn;
};

value string_of_current_palette_def minfo pd =
  let c_pal = minfo.c_pal in
  let sat = max 0 (min 255 (c_pal.ini_max_s + c_pal.max_ds)) in
  let bri = max 0 (min 255 (c_pal.ini_max_v + c_pal.max_dv)) in
  let pd = {(pd) with saturation = sat; brightness = bri} in
  Palette.def_to_string pd
;

value output_mdb minfo oc (nb_iter_of_pixel, color_of_nb_iter, w, h) = do {
  fprintf oc "MDB1\n";
  fprintf oc "%d %d\n" w h;
  fprintf oc "%s\n" (string_of_current_palette_def minfo minfo.c_pal_def);
  let col_tab_len = Array.length minfo.c_pal.c_tab in
  let len = w * h in
  let s = Bytes.create len in
  loop 0 (h - 1) 0 where rec loop i j k =
    if k = len then output_bytes oc s
    else if i = w then loop 0 (j - 1) k
    else do {
      let c = nb_iter_of_pixel i j in
      Bytes.set s k (Char.chr (if c = 0 then 0 else c mod col_tab_len + 1));
      loop (i + 1) j (k + 1)
    };
};

value output_mdb_file minfo pfx get_pixel_color = do {
  if Array.length minfo.c_pal.c_tab < 256 then do {
    let fn = sprintf "%s%05d.mdb" pfx minfo.ppm_cnt in
    mprintf "output file %s\n" fn;
    let oc = open_out_bin pfx in
    output_mdb minfo oc get_pixel_color;
    close_out oc;
    try Sys.remove fn with [ Sys_error _ -> () ];
    Sys.rename pfx fn;
  }
  else ()
};

value image_exposing_user_time info isc tm_now =
  let prev_time =
    match isc.exposing_user_time with
    [ Some t -> t
    | None -> 0. ]
  in
  let inc_time = tm_now -. info.start_user_time in
  (prev_time +. inc_time, info.total_user_time +. inc_time)
;

value image_exposing_real_time info isc tm_now =
  let prev_time =
    match isc.exposing_real_time with
    [ Some t -> t
    | None -> 0. ]
  in
  let inc_time = tm_now -. info.start_real_time in
  (prev_time +. inc_time, info.total_real_time +. inc_time)
;

value string_of_circumstance =
  fun
  [ Always -> "always"
  | IfNeeded -> "if needed"
  | Never -> "never" ]
;

value glop = 1200.0;
value magn_of_reduc er reduc = 1. /. (reduc *. glop *. er);
value reduc_of_magn er magn = 1. /. (magn *. glop) *. er;

value magn_of_reduc_overflows er reduc =
  max_float *. (reduc *. glop *. er) < 1.0;

value default_init_reduc er = reduc_of_magn er 0.14;

value log_magn_of_reduc er reduc =
  -. Mfloat.Best.log reduc -. log (glop *. er)
;

value level_of_reduc er reduc =
  (log (default_init_reduc er) -.
      log (Mfloat.Best.to_float reduc *. er)) /. log 2.
;

(*
value reduc_of_level er level =
  Mfloat.Best.of_float (default_init_reduc er *. 2.0 ** (-. level))
;
*)

value reduc_of_level er level =
  let c = 1. /. (0.14 *. 1200.0) *. er in
  Mfloat.Best.of_float (c *. 2.0 ** (-. level))
;
value reduc_of_level er level =
  let c = Mfloat.Best.of_float (1. /. (0.14 *. 1200.0) *. er) in
  let e = Mfloat.Best.of_float (-. level *. log 2.) in
  Mfloat.Best.mult c (Mfloat.Best.exp e)
;

value lim_flo is_lam = if is_lam then 43.0 else 45.0;

value apply_point info w h pred accu i j =
  let ica = info.state.cur.counts_array in
  let dd =
    match info.state.cur.exposing_state with
    [ ES_in_progress eip -> eip.eip_chunk
    | ES_terminated -> 1 ]
  in
  if i < dd || i >= w - dd || j < dd || j >= h - dd then None
  else
    match Array2dim.get ica i j with
    [ Some c ->
        loop [c] (-dd) (-dd) where rec loop cl di dj =
          match pred cl accu i j with
          [ Some accu -> Some accu
          | None ->
              if di = 2 * dd then None
              else if dj = 2 * dd then loop cl (di+dd) (-dd)
              else
                let c =
                  match Array2dim.get ica (i+di) (j+dj) with
                  [ Some c -> c
                  | None -> 0 ]
                in
                let new_cl = merge_uniq c cl in
                loop new_cl di (dj+dd) ]
    | None -> None ]
;

value iter_points info w h accu pred =
  loop_ij accu 1 1 where rec loop_ij accu i j =
    if j = h - 1 then accu
    else if i = w - 1 then loop_ij accu 1 (j + 1)
    else
      match apply_point info w h pred accu i j with
      [ Some (accu, cont) -> if cont then loop_ij accu (i + 1) j else accu
      | None -> loop_ij accu (i + 1) j ]
;

value code_of_digit d =
  if d < 10 then Char.code '0' + d
  else Char.code 'A' + (d - 10)
;

value to_string_sep_base sep base x =
  let digits = loop [] x
    where rec loop d x =
      if x = 0 then d
      else loop [x mod base :: d] (x / base)
  in
  let digits = if digits = [] then [0] else digits in
  let len = List.length digits in
  let slen = String.length sep in
  let s = Bytes.create (len + (len - 1) / 3 * slen) in
  let _ =
    List.fold_left
       (fun (i, j) d ->
          do {
            Bytes.set s j (Char.chr (code_of_digit d));
            if i < len - 1 && (len - 1 - i) mod 3 = 0 then do {
              String.blit sep 0 s (j + 1) slen;
              (i + 1, j + 1 + slen)
            }
            else (i + 1, j + 1)
          })
       (0, 0) digits
  in
  Bytes.to_string s
;

value string_thousand_of_int n =
  let s = to_string_sep_base "," 10 (abs n) in
  if n < 0 then "-" ^ s else s
;

value print_time oc info isc total_pts normal_total_pts
    (name, time, image_exposing_time, exposing_time, total_time) = do {
  let tm_now = time () in
  let (tm, tot_tm) =
    if isc.exposing_state = ES_terminated then
      let tm =
        match exposing_time with
        [ Some t -> t
        | None -> 0. ]
      in
      (tm, total_time)
    else
      image_exposing_time info isc tm_now
  in
  fprintf oc "%s time %s" name (Mutil.string_of_time tm);
  if isc.exposing_state <> ES_terminated then do {
    if total_pts <> 0 then do {
      let est_rem_tm =
        floor
          (tm /. float total_pts *.
             float (normal_total_pts - total_pts) +. 0.5)
      in
      fprintf oc " + estim rem %s" (Mutil.string_of_time est_rem_tm);
      fprintf oc " = estim tot %s" (Mutil.string_of_time (tm +. est_rem_tm));
    }
    else ();
  }
  else ();
  fprintf oc " (sum: %s)\n" (Mutil.string_of_time tot_tm);
};

value trace_slave_points oc minfo isc w h = do {
  fprintf oc "\n";
  let wo = isc.work in
  let slaves_list =
    let list =
      Mfd.Map.fold
        (fun s w list ->
           let sl_name = Compute.slave_name minfo s in
           [(w, Some s, sl_name, True) :: list])
        wo.w_active_slaves []
    in
    let list =
      List.rev_append
        (List.rev_map (fun (w, sl_name) -> (w, None, sl_name, False))
           wo.w_term_slaves)
        list
    in
    let compare_ps w1 w2 =
      compare (List.fold_left \+ 0 w1.ps_nb) (List.fold_left \+ 0 w2.ps_nb)
    in
    List.sort
      (fun (w1, _, _, _) (w2, _, _, _) -> compare_ps w2 w1)
      list
  in
  let total_pts =
    List.fold_left
      (fun accu (w, _, _, _) -> List.fold_left \+ accu w.ps_nb)
      wo.w_master slaves_list
  in
  let total_pack =
    List.fold_left (fun accu (w, _, _, _) -> accu + List.length w.ps_nb)
      0(*wo.w_master*) slaves_list
  in
  let normal_total_pts = w * h in
  if slaves_list <> [] then do {
    fprintf oc "points:\n";
    List.iter
      (fun (w, s, addr, active) ->
         let nb = List.fold_left \+ 0 w.ps_nb in
         fprintf oc "  - %s computed by '%s' in %d packets%s%s%s%s\n"
           (string_thousand_of_int nb) addr (List.length w.ps_nb)
(*
           (List.fold_left (fun s1 nb -> sprintf "%s %d" s1 nb) " [" w.ps_nb ^
            " ] ")
*)
""
(**)
(**)
           (match s with
            [ Some s ->
                let (speed, speed_list) =
                  try Mfd.Map.find s minfo.slave_speed with
                  [ Not_found -> (0, []) ]
                in
                sprintf " (speed %d [%s ])" speed
                  (List.fold_left (fun s1 nb -> sprintf "%s %d" s1 nb)
                     "" speed_list)
           | None -> "" ])
(*
""
*)
           (if isc.exposing_state = ES_terminated then ""
            else
              match s with
              [ Some s ->
                  match Mfd.map_find s minfo.slave_work with
                  [ Some (m, _) -> sprintf " (max/req %d)" m
                  | None -> "" ]
              | None -> "" ])
           (if not active then " (terminated)" else ""))
      slaves_list;
    fprintf oc "  - %s computed by the program\n"
      (string_thousand_of_int wo.w_master);
    fprintf oc "total: %s points / %s" (string_thousand_of_int total_pts)
      (string_thousand_of_int normal_total_pts);
    if isc.exposing_state = ES_terminated then
      let n = normal_total_pts - total_pts in
      if n < 0 then
        fprintf oc " (too many %s)" (string_thousand_of_int (-n))
      else
        fprintf oc " (not needed %s)" (string_thousand_of_int n)
    else ();
    fprintf oc " - %d slaves" (List.length slaves_list);
    fprintf oc " (%d packets)" total_pack;
    fprintf oc "\n";
    fprintf oc "\n";
    fprintf oc "slaves speeds:\n";
    let slaves_speed_list =
      List.map
        (fun (_, s, addr, _) ->
           match s with
           [ Some s ->
               let (speed, _) =
                 try Mfd.Map.find s minfo.slave_speed with
                 [ Not_found -> (0, []) ]
               in
               (speed, addr)
           | None -> (0, addr) ])
        slaves_list
    in
    let slaves_speed_list =
      List.sort (fun a b -> compare b a) slaves_speed_list
    in
    fprintf oc " ";
    List.iter (fun (speed, addr) -> fprintf oc " \"%s\" (%d)" addr speed)
      slaves_speed_list;
    fprintf oc "\n";
  }
  else do {
    fprintf oc "computed points: %s" (string_thousand_of_int total_pts);
    fprintf oc " / %s" (string_thousand_of_int normal_total_pts);
    if isc.exposing_state = ES_terminated then
      fprintf oc " (not needed %s)"
        (string_thousand_of_int (normal_total_pts - total_pts))
    else ();
    fprintf oc "\n";
  };
  fprintf oc "\n";
  fprintf oc "level %g" isc.level;
  fprintf oc " rot %s" (Int10.to_string isc.rot);
  fprintf oc " magn %g"
    (magn_of_reduc minfo.extra_reduc (Mfloat.Best.to_float isc.reduc));
  fprintf oc " type %s" (Mfloat.M.string_of_num_type ());
  match minfo.mlb with
  [ Some i -> fprintf oc " formula z^%d+c" i
  | None -> if minfo.m3 then fprintf oc " formula z^3+c" else () ];
  if minfo.lambda then fprintf oc " lam" else ();
  if minfo.invert then fprintf oc " inv" else ();
  if isc.julia <> None then fprintf oc " julia" else ();
  let nb_slaves =
    Mfd.set_length minfo.free_slaves + Mfd.set_length minfo.busy_slaves +
    Mfd.set_length minfo.obsolete_slaves
  in
  if nb_slaves > 0 && minfo.force_using_slaves <> Never then do {
    fprintf oc " %d slaves" nb_slaves
  }
  else ();
  fprintf oc "\n";
  fprintf oc "xc %s\n"
    (Mfloat.Best.to_string (Mfloat.Best.mult_int isc.xc 1));
  fprintf oc "yc %s\n"
    (Mfloat.Best.to_string (Mfloat.Best.mult_int isc.yc 1));
  let c = minfo.c_pal in
  let s = max 0 (min 255 (c.ini_max_s + c.max_ds)) in
  let v = max 0 (min 255 (c.ini_max_v + c.max_dv)) in
  fprintf oc "nb_it %d nb col %d sat %d bri %d" isc.nb_it
    (Array.length c.c_tab) s v;
  fprintf oc "\n";

  List.iter (print_time oc minfo isc total_pts normal_total_pts)
    [("us", user_time, image_exposing_user_time,
      isc.exposing_user_time, minfo.total_user_time);
     ("re", real_time, image_exposing_real_time,
      isc.exposing_real_time, minfo.total_real_time)];

  match isc.exposing_state with
  [ ES_in_progress eip -> do {
      fprintf oc "exposing chunk %dx%d" eip.eip_chunk eip.eip_chunk;
      if eip.eip_refining then fprintf oc " refinement"
      else do {
        List.iter
          (fun a -> fprintf oc " y %d/%d" a.r_curr_y (a.r_y + a.r_h))
          eip.eip_rect;
      };
      fprintf oc "\n";
    }
  | ES_terminated -> () ];
  flush oc;
};

value mprint_palette_def sat bri pd =
  let pd = {(pd) with saturation = sat; brightness = bri} in
  mprintf "palette %s\n" (Palette.def_to_string pd)
;

value mprint_current_palette_def minfo pd =
  mprintf "palette %s\n" (string_of_current_palette_def minfo pd)
;

value make_random_palette_def rs sat bri = do {
  let ca =
    if Random.State.int rs 2 = 0 then CA_start_with_white
    else CA_start_with_black
  in
  let nb_hues = 8 in
  let hlist =
    loop [] nb_hues where rec loop hlist n =
      if n = 0 then hlist
      else
        let h = Random.State.int rs 256 in
        loop [h :: hlist] (n - 1)
  in
  let _ =
    (* historical reason: to get the same color palette than previous
       versions *)
    Random.State.int rs 2
  in
  let permut = Random.State.int rs 352 in
  let pd =
    {hues = hlist; permutation = permut; saturation = sat;
     delta_saturation = 8; brightness = bri; delta_brightness = 8;
     color_algo = ca}
  in
  mprint_palette_def sat bri pd;
  pd
};

value make_very_random_palette_def rs = do {
  let ca =
    if Random.State.int rs 2 = 0 then CA_start_with_white
    else CA_start_with_black
  in
  let nb_hues = 2 + 2 * Random.State.int rs 7 in
  let hlist =
    loop [] nb_hues where rec loop hlist n =
      if n = 0 then hlist
      else
        let h = Random.State.int rs 256 in
        loop [h :: hlist] (n - 1)
  in
  let permut = Random.State.int rs 1000 in
  let ds = 1 + Random.State.int rs 16 in
  let db = 1 + Random.State.int rs 16 in
  let sat = 255 in
  let bri = 224 in
  let pd =
    {hues = hlist; permutation = permut; saturation = sat;
     delta_saturation = ds; brightness = bri; delta_brightness = db;
     color_algo = ca}
  in
  mprint_palette_def sat bri pd;
  pd
};

value tempo () = let _ = Unix.select [] [] [] 0.0005 in ();

value recenter minfo x y w h =
  let isc = minfo.state.cur in
  let cs = Mfloat.trigo_of_rot isc.rot in
  match isc.julia with
  [ Some (xc, yc) ->
      let (xc, yc) =
        Mfloat.coord_of_pos cs isc.reduc (xc, yc) (x, y) (w, h)
      in
      isc.julia := Some (xc, yc)
  | None -> do {
      let (xc, yc) =
        Mfloat.coord_of_pos cs isc.reduc (isc.xc, isc.yc) (x, y) (w, h)
      in
      isc.xc := xc;
      isc.yc := yc;
    } ]
;

value default_init_wid = 768;
value default_init_hei = 432;

value string_of_geometry (sz, p) =
  sprintf "%s%s"
    (match sz with
     [ Some (w, h) -> sprintf "%dx%d" w h
     | None -> sprintf "%dx%d" default_init_wid default_init_hei ])
    (match p with
     [ None -> ""
     | Some (x, y) ->
         sprintf "%s%d%s%d" (if x >= 0 then "+" else "-") (abs x)
           (if y >= 0 then "+" else "-") (abs y) ])
;

value print_command_args oc minfo w h pos_opt = do {
  let state = minfo.state.cur in
  let c_pal = minfo.c_pal in
  let sat = max 0 (min 255 (c_pal.ini_max_s + c_pal.max_ds)) in
  let bri = max 0 (min 255 (c_pal.ini_max_v + c_pal.max_dv)) in
  let a = Mfloat.M.string_of_num_type () in
  fprintf oc "%s-z %s %s%s -l %g%s -i %d%s%s%s%s%s%s%s%s%s%s%s%s"
    (if state.julia <> None then "-julia " else "")
    (Mfloat.Best.to_string state.xc) (Mfloat.Best.to_string state.yc)
    (if state.rot = Int10.zero then ""
     else sprintf " -rot %g" (Int10.to_float state.rot))
    state.level
(*
    (magn_of_reduc (1. /. minfo.extra_reduc)
       (Mfloat.Best.to_float state.reduc))
*)
    (if minfo.extra_reduc = 1.0 then ""
     else sprintf " -er %g" minfo.extra_reduc)
    state.nb_it (if minfo.auto_float then "" else sprintf " -a %s" a)
    (if minfo.m3 then " -m3" else "")
    (match minfo.mlb with
     [ Some m -> sprintf " -mlb %d" m
     | None -> "" ])
    (if minfo.lambda then " -lam" else "")
    (if minfo.invert then " -inv" else "")
    (sprintf " -g %dx%d" w h)
    (match pos_opt with
     [ Some (x, y) -> sprintf "+%d+%d" x y
     | None ->  "" ])
    (if minfo.pal_defs.bef = [] then
       match minfo.init_seed with
       [ Some s -> sprintf " -s %d" s
       | None ->
           match minfo.init_palette with
           [ Some _ ->
              let pd = minfo.c_pal_def in
              let pd = {(pd) with saturation = sat; brightness = bri} in
               sprintf " -p %s" (Palette.def_to_string pd)
           | None ->
               "" ] ]
     else
       let pd = minfo.c_pal_def in
       let pd = {(pd) with saturation = sat; brightness = bri} in
       sprintf " -p %s" (Palette.def_to_string pd))
    (match minfo.init_hiring with
     [ Some a -> sprintf " -hiring %s" a
     | None -> "" ])
    (if minfo.init_master_lazy then " -mlazy" else "")
    (match minfo.hist_size with
     [ 0 -> ""
     | n -> sprintf " -h %d" n ])
    (match minfo.mode with
     [ M_gtk -> IFDEF GTK AND (RT OR OPENGL) THEN " -gtk" ELSE "" END
     | M_open_gl -> " -opengl"
     | M_rt -> " -rt"
     | M_batch_animate | M_display_interesting_points -> "" ])
};

value print_command minfo w h pos_opt = do {
  let c_pal = minfo.c_pal in
  let sat = max 0 (min 255 (c_pal.ini_max_s + c_pal.max_ds)) in
  let bri = max 0 (min 255 (c_pal.ini_max_v + c_pal.max_dv)) in
  let pd = minfo.c_pal_def in
  let pd = {(pd) with saturation = sat; brightness = bri} in
  eprintf "%s\n" (Palette.def_to_string pd);
  eprintf "%s " Sys.argv.(0);
  print_command_args stderr minfo w h pos_opt;
  eprintf "\n";
  flush stderr;
};

value help_txt_1 = "
    Zooming
      mouse button 1: zoom centered at mouse position
      mouse button 2: center at mouse position
      mouse button 3: unzoom
      mouse wheel: rotate 1 degree; +shift: rotate 10 degrees
      z: zoom; Z: zoom ten times; x: unzoom; X: unzoom ten times
      s: zoom a little
      a: animate (loop zooming a little and outputing ppm)
      space: pause/resume
    Color palette
      r: change to random color palette; R very random
      left-arrow: previous color palette
      right-arrow: next color palette
      [ ] =: decrease/increase/medium saturation (ctrl: spreading)
      { } -: decrease/increase/medium brightness (ctrl: spreading)
      ( ): colors permutation (ctrl: fast)
    Iterations
      d: decrease number of iterations (faster)
      u: increase number of iterations (less black)
    Information
      .: draw half sized rectangle around center
      c: display color palette
      i: display interesting points in white
      /: erase information
      shift + mouse button 1: draw iteration points
    Computations
      1: computations with integers
      2: computations with integers 64 bits
      3: computations with floats (default)
      4: computations with big integers (bignum)"
;

value help_txt_2_1 = "
      5: computations with multiple precision floats (mpfr)
      6: computations with multiple precision ints (mpz)
      0: computations with 3 or 5 depending on level"
;

value help_txt_2_2 = "
      6: computations with multiple precision ints (mpz)
      0: computations with 3 or 6 depending on level"
;

value help_txt_3 = "
    History
      up-arrow: previous in history
      down-arrow: next in history
    Misc
      j: toggle Julia/Mandelbrot set
      ,: center on a random interesting point
      f: toggle force using slaves, if any
      k: toggle check slaves answers, if any
      o: print command to start current vision
      p: save current image on file 'mlbrot.ppm'
      l: redraw
      t: trace
      h: help
      ctl-q: quit

    Type <escape> to remove this window."
;

value help_txt () =
  match I18n.ptransl "~help txt 1" with
  [ Some s -> s
  | None -> help_txt_1 ] ^
  IFDEF MPFR THEN
    match I18n.ptransl "~help txt 2.1" with
    [ Some s -> s
    | None -> help_txt_2_1 ]
  ELSIFDEF MPZ THEN
    match I18n.ptransl "~help txt 2.2" with
    [ Some s -> s
    | None -> help_txt_2_2 ]
  ELSE "" END ^
  match I18n.ptransl "~help txt 3" with
  [ Some s -> s
  | None -> help_txt_3 ]
;

value stdout_is_a_tty () = Unix.isatty Unix.stdout;

value output_image minfo ppm_from get_pixel_color = do {
  minfo.ppm_cnt := minfo.ppm_cnt + 1;
  match minfo.ppm_dir with
  [ None ->
      if not (stdout_is_a_tty ()) then do {
        output_ppm stdout get_pixel_color;
        flush stdout;
      }
      else ()
  | Some dir ->
      if minfo.ppm_cnt >= ppm_from then do {
        output_ppm_file minfo (Filename.concat dir "im_") get_pixel_color;
        output_mdb_file minfo (Filename.concat dir "im_") get_pixel_color;
        let oc =
          open_out_gen [Open_wronly; Open_append; Open_creat; Open_text] 0o666
            (Filename.concat dir "info.txt")
        in
        fprintf oc "%d " minfo.ppm_cnt;
        let (_, _, w, h) = get_pixel_color in
        print_command_args oc minfo w h None;
        fprintf oc "\n";
        close_out oc;
      }
      else () ]
};

value open_connection addr = do {
  let s = Unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  Unix.connect s addr;
  s
};

value parse_digit =
  fparser
  [ [: `('0'..'9' as c) :] -> Char.code c - Char.code '0' ]
;

value rec parse_int_kont i =
  fparser
  [ [: d = parse_digit; i = parse_int_kont (10 * i + d) :] -> i
  | [: :] -> i ]
;

value parse_int =
  fparser
  [ [: d = parse_digit; i = parse_int_kont d :] -> i ]
;

value parse_sign =
  fparser
  [ [: `'+' :] -> 1
  | [: `'-' :] -> -1 ]
;

value parse_pos =
  fparser
  [ [: sx = parse_sign; x = parse_int; sy = parse_sign; y = parse_int :] ->
      Some (sx * x, sy * y)
  | [: :] ->
      None ]
;

value parse_geometry =
  fparser
  [ [: w = parse_int; `'x'; h = parse_int; p = parse_pos;
       _ = Fstream.empty :] ->
         (Some (w, h), p)
  | [: p = parse_pos; _ = Fstream.empty :] -> (None, p) ]
;

value geometry_of_string s =
  match parse_geometry (Fstream.of_string s) with
  [ Some (r, _) -> r
  | None -> failwith (sprintf "bad geometry: \"%s\"" s) ]
;

value get_sockaddr t =
  let s =
    match try Some (String.index t ' ') with [ Not_found -> None ] with
    [ Some i -> String.sub t 0 i
    | None -> t ]
  in
  try Some (Mutil.service_of_string s) with
  [ Failure s -> do {
      mprintf "%s\n" s;
      None
    } ]
;

value arg_xc = ref "0";
value arg_yc = ref "0";
value arg_rot = ref Int10.zero;
value arg_magn = ref None;
value arg_julia = ref False;

value arg_mode =
  ref
    (IFDEF GTK THEN M_gtk
     ELSIFDEF RT THEN M_rt
     ELSIFDEF OPENGL THEN M_open_gl
     ELSE M_rt END)
;

value arg_extra_reduc = ref 1.0;
value arg_level = ref None;
value arg_nb_it = ref 64;
value arg_geometry = ref (None, None);
value arg_hist_size = ref 0;
value arg_float_arith = ref "aut";
value arg_m3 = ref False;
value arg_mlb = ref None;
value arg_lambda = ref False;
value arg_invert = ref False;
value arg_force_using_slaves = ref IfNeeded;
value arg_scenario_file = ref None;
value arg_seed = ref None;
value arg_check_slaves_answers = ref False;
value arg_slave_hiring = ref None;
value arg_master_lazy = ref False;
value arg_palette = ref None;
value arg_ppm_dir = ref None;
value arg_ppm_from = ref 0;
value arg_half_size = ref False;

value superseded_lam by =
  if arg_lambda.val then do {
    eprintf "Warning: option -lam ignored (superseded by '%s')\n" by;
    flush stderr;
    arg_lambda.val := False;
  }
  else ()
;

value superseded_m3 by =
  if arg_m3.val then do {
    eprintf "Warning: option -m3 ignored (superseded by '%s')\n" by;
    flush stderr;
    arg_m3.val := False;
  }
  else ()
;

value superseded_mlb by =
  if arg_mlb.val <> None then do {
    eprintf "Warning: option -mlb ignored (superseded by '%s')\n" by;
    flush stderr;
    arg_mlb.val := None
  }
  else ()
;

value speclist =
  Arg.align
    ([("-a", Arg.Set_string arg_float_arith,
      let txt1 = "<str> Set arith: int, i64, flo, big" in
      let txt2 = IFDEF MPFR THEN ", mpf, aut" ELSE "" END in
      let txt3 = " (default: " ^ arg_float_arith.val ^ ")" in
      txt1 ^ txt2 ^ txt3);
     ("-ba",
      Arg.Unit (fun () -> arg_mode.val := M_batch_animate),
      " Animate in batch");
     ("-check_sl", Arg.Set arg_check_slaves_answers,
      " Check slaves answers");
     ("-d", Arg.String (fun s -> arg_ppm_dir.val := Some s),
      "<dir> When animate or scenario, put ppm files on that directory");
     ("-di",
      Arg.Unit (fun () -> arg_mode.val := M_display_interesting_points),
      " Display number of interesting points and exit");
     ("-er",
      Arg.Float (fun f -> if f <> 0. then arg_extra_reduc.val := f else ()),
      sprintf "<flo> Extra reduction (default: %.1f)" arg_extra_reduc.val);
     ("-force_sl", Arg.Unit (fun () -> arg_force_using_slaves.val := Always),
      " Always use slaves even if not necessary");
     ("-g",
      Arg.String
        (fun s ->
           try arg_geometry.val := geometry_of_string s with
           [ Failure s -> raise (Arg.Bad s) ]),
      sprintf "<geom> Set geometry (default: %s)"
        (string_of_geometry arg_geometry.val))] @
    IFDEF GTK AND (RT OR OPENGL) THEN
       [("-gtk", Arg.Unit (fun () -> arg_mode.val := M_gtk),
         " Drawing with GTK")]
    ELSE [] END @
    [("-h", Arg.Set_int arg_hist_size,
      sprintf "<int> Set history size (default: %d)" arg_hist_size.val);
     ("-hs", Arg.Set arg_half_size,
      " Scenarios generate images with half size by averaging pixels");
     ("-hiring", Arg.String (fun s -> arg_slave_hiring.val := Some s),
      "<addr> Slaves hiring service (<port> or <filename>)");
     ("-mlazy", Arg.Set arg_master_lazy,
      " Do nothing when slaves are working");
     ("-i", Arg.Set_int arg_nb_it,
      sprintf "<int> Set initial number of iterations (default: %d)"
        arg_nb_it.val);
     ("-inv", Arg.Set arg_invert, " Invert mandelbrot function");
     ("-julia", Arg.Set arg_julia, " Julia fractal");
     ("-l",
      Arg.Float
        (fun f -> do {
           arg_level.val := Some f;
           arg_magn.val := None
         }),
      sprintf "<m> Set initial level (default: %g)"
        (level_of_reduc arg_extra_reduc.val
           (Mfloat.Best.of_float (default_init_reduc arg_extra_reduc.val))));
     ("-lam",
      Arg.Unit
        (fun () -> do {
           arg_lambda.val := True;
           superseded_m3 "-lam";
           superseded_mlb "-lam";
         }),
      " Mandelbrot lambda function");
     ("-m",
      Arg.Float
        (fun f ->
           if f <> 0. then do {
             arg_level.val := None;
             arg_magn.val := Some f;
           }
           else ()),
      sprintf "<m> Set initial magnification (default: %.1f)"
        (magn_of_reduc arg_extra_reduc.val
           (default_init_reduc arg_extra_reduc.val)));
     ("-m3",
      Arg.Unit
        (fun () -> do {
           arg_m3.val := True;
           superseded_lam "-m3";
           superseded_mlb "-m3";
         }),
      " Set mandelbrot with formula z^3+c");
     ("-mlb",
      Arg.Int
        (fun i -> do {
           arg_mlb.val := Some i;
           superseded_lam "-mlb";
           superseded_m3 "-mlb";
         }),
      "<int> Set mandelbrot with formula z^n+c")] @
    IFDEF OPENGL AND (GTK OR RT) THEN
      [("-opengl", Arg.Unit (fun () -> arg_mode.val := M_open_gl),
        " Drawing with OpenGL")]
    ELSE [] END @
    [("-p",
      Arg.String
        (fun s -> do {
           try arg_palette.val := Some (Palette.def_of_string s) with
           [ Failure s -> do {
               eprintf "\
Palette syntax:

hue,hue,...,hue/permut,bw/sat,ds/bri,db

  hue: color hue between 0 and 255; 0 = red; 85 = green; 170 = blue
  permut: palette circular permutation (a number)
  bw: if 'b', first hue starts with black, if 'w' it starts with white
  sat: maximum saturation between 0 and 255
  ds: saturation delta in colors gradations (a small positive number)
  bri: maximum brightness between 0 and 255
  db: brightness delta in colors gradations (a small positive number)

Example: 96,86,102,244,17,54,160,197/308,b/128,8/224,8

Some keywords are also usable:
  rainbow, reverse_rainbow, shifted_rainbow, random\n";
               flush stderr;
               exit 2
             } ];
           arg_seed.val := None;
         }),
      "<str> Set color palette");
     ("-rot", Arg.Float (fun f -> arg_rot.val := Int10.of_float f),
      sprintf "<flo> Set initial rotation (default %g)"
        (Int10.to_float arg_rot.val))] @
     IFDEF RT AND (GTK OR OPENGL) THEN
       [("-rt", Arg.Unit (fun () -> arg_mode.val := M_rt),
         " Drawing with RT")]
     ELSE [] END @
     [("-s",
      Arg.Int
        (fun i -> do {
           arg_palette.val := None;
           arg_seed.val := Some i
         }),
      "<int> Set random seed for color palettes");
     ("-scen", Arg.String (fun s -> arg_scenario_file.val := Some s),
      "<file> Apply scenario file");
     ("-start", Arg.Set_int arg_ppm_from,
      sprintf "<int> Start image number (default %d)" arg_ppm_from.val);
     ("-z",
      Arg.Tuple [Arg.Set_string arg_xc; Arg.Set_string arg_yc],
      sprintf "<xc><yc> Set initial position (default: %s %s)"
        arg_xc.val arg_yc.val)])
;

value usage = "Usage: " ^ Sys.argv.(0) ^ " [option]... <addr>\n\nOptions:";

value anonfun s = raise (Arg.Bad ("don't know what to do with " ^ s));

value max_prec_of_level_and_coord level x y =
  Mutil.round
    (max (level +. 10.0)
       (float (max (String.length x) (String.length y)) *.
        log 10. /. log 2.))
;

value make_minfo rs w h c_pal_def c_pal slave_hiring = do {
  let rs =
    match arg_seed.val with
    [ Some _ -> rs
    | None -> Random.State.make_self_init () ]
  in
  let level =
    match arg_level.val with
    [ Some l -> l
    | None -> 0.0 ]
  in
  let (slave_hiring, master_lazy) =
    match slave_hiring with
    | Some (sh, ml) → (Some sh, ml)
    | None → (None, False)
    end
  in
  let max_prec = max_prec_of_level_and_coord level arg_xc.val arg_yc.val in
  Mfloat.M.set_default_prec max_prec;
  let (reduc, level) =
    match arg_level.val with
    [ Some l -> (reduc_of_level arg_extra_reduc.val l, l)
    | None ->
        match arg_magn.val with
        [ Some m ->
            let r = reduc_of_magn arg_extra_reduc.val m in
            let r = Mfloat.Best.of_float r in
            (r, level_of_reduc arg_extra_reduc.val r)
        | None ->
            if arg_invert.val && not arg_lambda.val ||
               not arg_invert.val && arg_lambda.val
            then
              (* ad hoc *)
              (reduc_of_level arg_extra_reduc.val (-1.0), -1.0)
            else
              let r =
                Mfloat.Best.of_float (default_init_reduc arg_extra_reduc.val)
              in
              (r, level_of_reduc arg_extra_reduc.val r) ] ]
  in
  let num_type =
    match arg_float_arith.val with
    [ "int" -> N_int
    | "i64" -> N_i64
    | "flo" -> N_flo
    | "big" -> N_big
    | IFDEF MPFR THEN
      "mpf" -> N_mpf
      END
    | IFDEF MPZ THEN
      "mpz" -> N_mpz
      END
    | "aut" ->
        IFDEF MPFR THEN
          if level <= lim_flo arg_lambda.val then N_flo else N_mpf
        ELSE IFDEF MPZ THEN
          if level <= lim_flo arg_lambda.val then N_flo else N_mpz
        ELSE
          N_flo
        END END
    | s -> do {
        mprintf "%s: bad arith type \"%s\"\n" Sys.argv.(0) s;
        exit 2
      } ]
  in
  Mfloat.M.set_num num_type;
  let auto_float =
    IFDEF MPFR OR MPZ THEN
      if arg_float_arith.val = "aut" then True else False
    ELSE False END
  in
  let nb_it = arg_nb_it.val in
  let reduc = Mfloat.Best.with_prec max_prec reduc in
  let xc = Mfloat.Best.of_string max_prec arg_xc.val in
  let yc = Mfloat.Best.of_string max_prec arg_yc.val in
  let rot = arg_rot.val in

  let counts_array = Array2dim.create w h None in
  let hist_size =
    if arg_mode.val = M_batch_animate then 0 else arg_hist_size.val
  in
  let c_pal =
    {c_tab = c_pal; ini_max_s = c_pal_def.saturation;
     ini_max_v = c_pal_def.brightness; max_ds = 0; max_dv = 0}
  in
  let julia = if arg_julia.val then Some (xc, yc) else None in
  {state =
     {bef = [];
      cur =
        {level = level; num_type = num_type; reduc = reduc;
         rot = rot; xc = xc; yc = yc; nb_it = nb_it; nb_it_at_start = nb_it;
         julia = julia; counts_array = counts_array;
         exposing_state = ES_terminated; exposing_user_time = None;
         exposing_real_time = None;
         work =
           {w_active_slaves = Mfd.Map.empty; w_term_slaves = [];
            w_master = 0};
         master_lazy = master_lazy;
         nb_diff_answ = 0; lim_reach = []};
      aft = []};
   auto_float = auto_float;
   hist_size = hist_size; start_user_time = 0.; start_real_time = 0.;
   total_user_time = 0.; total_real_time = 0.; max_prec = max_prec;
   slave_name = Mfd.Map.empty; slave_speed = Mfd.Map.empty;
   free_slaves = Mfd.Set.empty; busy_slaves = Mfd.Set.empty;
   slave_work = Mfd.Map.empty; obsolete_slaves = Mfd.Set.empty;
   check_slaves_answers = arg_check_slaves_answers.val;
   pal_defs = {bef = []; cur = c_pal_def; aft = []};
   c_pal_def = c_pal_def; random_for_colors = rs; mode = arg_mode.val;

   pts_bef_test = 0; points_between_check_user = 0;
   ymin = 0; ymax = 0;

   m3 = arg_m3.val; mlb = arg_mlb.val; lambda = arg_lambda.val;
   invert = arg_invert.val; extra_reduc = arg_extra_reduc.val;
   slave_hiring = slave_hiring;
   force_using_slaves = arg_force_using_slaves.val;
   init_seed = arg_seed.val; init_palette = arg_palette.val;
   init_geometry = arg_geometry.val; init_hiring = arg_slave_hiring.val;
   init_master_lazy = arg_master_lazy.val;
   c_pal = c_pal; ppm_dir = arg_ppm_dir.val; ppm_cnt = 0;
   half_size = arg_half_size.val}
};
