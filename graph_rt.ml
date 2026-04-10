(* $Id: graph_rt.ml,v 1.27 2017/12/28 10:50:13 deraugla Exp $ *)

open Mcomm;
open Info_def;
open Mdef;
open Mmisc;
open Mprintf;
open Palette_def;
open Printf;
open Rt;

type x_info =
  { xargs : Rt.xargs;
    widget : Rt.widget;
    pixmap : Rt.pixmap;
    font : Rt.font;
    c_red : Rt.color;
    c_green : Rt.color;
    c_blue : Rt.color;
    c_gray : Rt.color;
    c_black : Rt.color;
    c_white : Rt.color;
    last_col : mutable Rt.color;
    cursor_busy : Rt.mouse;
    cursor_pause : Rt.mouse;
    cursor_finish : Rt.mouse;
    x_pal : mutable array Rt.color;
    check_event : mutable bool;
    pending_event : mutable option (Info_def.mlbrot_event Rt.raw_event);
    no_main_loop : mutable bool }
;

value (set_info, get_info) = user_info "info" (ref UNone);

value vt_newline_mode = "\027[20h";
value vt_hide_cursor = "\027[?35h";

value return_event xinfo ev = do {
  xinfo.pending_event := Some (ME_gevent ev);
  None
};

value center_of_subwidget main_wid wid =
  let x = widget_x main_wid in
  let y = widget_y main_wid in
  let w = widget_width main_wid in
  let h = widget_height main_wid in
  (x + w / 2 - widget_width wid / 2, y + h / 2 - widget_height wid / 2)
;

value display_help wid = do {
  let txt = help_txt () in
  let (txt, nrow, ncol) =
    let tab =
      loop 1 where rec loop i =
        if i < String.length txt && txt.[i] = ' ' then loop (i + 1)
        else i
    in
    let b = Buffer.create 1 in
    loop_i 0 0 0 0 where rec loop_i i nrow ccol ncol =
      if i = String.length txt then
        let s = Buffer.contents b in
        (String.sub s 1 (String.length s - 1), nrow, ncol)
      else do {
        Buffer.add_char b txt.[i];
        let (i, nrow, ccol, ncol) =
          if txt.[i] = '\n' then
            let i = if txt.[i+1] = ' ' then i + tab else i + 1 in
            (i, nrow + 1, 0, max ccol ncol)
          else
            (i + 1, nrow, ccol + 1, ncol)
        in
        loop_i i nrow ccol ncol
      }
  in
  let sub_wid =
    rt_create_transient_widget wid "help mlbrot"
      (Some rt_destroy_widget)
      (term_desc [] (nrow, ncol, 0)
         (fun wid ->
            fun
            [ TermEvKeyPress {item = K_Escape} -> rt_destroy_widget wid
            | _ -> () ]))
  in
  term_send sub_wid (vt_newline_mode ^ vt_hide_cursor);
  term_send sub_wid txt;
  let (pos_x, pos_y) = center_of_subwidget wid sub_wid in
  rt_map_transient_widget sub_wid pos_x pos_y
};

value no_action _ _ = ();

value alert wid xinfo txt = do {
  let xa = xinfo.xargs in
  let xd = xdata_of_widget wid in
  let txt = sprintf " %s " txt in
  let info_wid = rt_create_popup_widget xd (title_desc [] txt no_action) in
  let (x, y) = center_of_subwidget wid info_wid in
  rt_map_popup_widget info_wid x y 0;
  rt_set_timeout xa txt (rt_current_time xa + 2000)
    (fun () -> rt_destroy_widget info_wid)
};

value x_select_color xinfo count =
  let c =
    match count with
    [ Some count -> xinfo.x_pal.(count mod Array.length xinfo.x_pal)
    | None -> xinfo.c_black ]
  in
  if color_pixel c = color_pixel xinfo.last_col then ()
  else do {
    rt_select_color c;
    xinfo.last_col := c;
  }
;

value refresh_pixel drw minfo xinfo i j =
  let ica = minfo.state.cur.counts_array in
  if i < Array2dim.dim1 ica && j < Array2dim.dim2 ica then do {
    let count = Array2dim.get ica i j in
    x_select_color xinfo count;
    rt_draw_point drw (i, j);
  }
  else ()
;

value refresh_pixmap_fast wid xinfo minfo rect_opt show_fast = do {
  let (x, y, w, h) =
    match rect_opt with
    [ Some r -> r
    | None -> (0, 0, widget_width wid, widget_height wid) ]
  in
  if minfo.state.cur.exposing_state = ES_terminated then ()
  else rt_select_mouse wid xinfo.cursor_busy;
  rt_select_color xinfo.last_col;
  let drw = PixmapDr xinfo.pixmap in
  loop x y where rec loop i j =
    if j = y + h then
      if show_fast then () else rt_clear_widget wid
    else if i = x + w then do {
      if True || show_fast then rt_clear_area wid (x, j, w, 1) else ();
      loop x (j + 1)
    }
    else do {
      refresh_pixel drw minfo xinfo i j;
      loop (i + 1) j
    };
  if minfo.state.cur.exposing_state = ES_terminated then ()
  else rt_unselect_mouse wid;
};

value refresh_pixmap_with_random_effect2 wid info xinfo = do {
  let minfo = info.m_info in
  let w = widget_width wid in
  let h = widget_height wid in
  rt_select_color xinfo.last_col;
  let drw = PixmapDr xinfo.pixmap in
  if Random.int 3 = 0 then
    (* random filling by chunks *)
    let chunk_size = max 1 (w / 24) in
    let n_wid = (w + chunk_size - 1) / chunk_size in
    let n_hei = (h + chunk_size - 1) / chunk_size in
    let filled = Array.init n_wid (fun _ -> Array.make n_hei False) in
    loop (n_wid * n_hei) where rec loop rest =
      if rest = 0 then None
      else do {
        let n = Random.int rest in
        let (i, j) =
          loop n 0 0 where rec loop n i j =
            if j = n_hei then assert False
            else if i = n_wid then loop n 0 (j + 1)
            else if filled.(i).(j) then loop n (i + 1) j
            else if n = 0 then (i, j)
            else loop (n - 1) (i + 1) j
        in
        filled.(i).(j) := True;
        let i = i * chunk_size in
        let j = j * chunk_size in
        loop 0 0 where rec loop k l =
          if l = chunk_size then ()
          else if k = chunk_size then loop 0 (l + 1)
          else do {
            let i = i + k in
            let j = j + l in
            refresh_pixel drw minfo xinfo i j;
            loop (k + 1) l
          };
        rt_clear_area wid (i, j, chunk_size, chunk_size);
        tempo ();
        match Info.check_user_intervention info with
        [ Some (ME_gevent ev) -> do {
            refresh_pixmap_fast wid xinfo minfo None False;
            Some ev
          }
        | Some _ | None -> loop (rest - 1) ]
      }
  else if Random.int 2 = 1 then
    (* slanted paths *)
    let refresh_rate = max 1 (w / 100) in
    let ord_ij = Random.int 2 in
    let dir = Random.int 2 in
    if ord_ij = 0 then
      let (min_imj, max_imj, inc_imj) =
        if dir = 0 then (w-1, -h+1, -1) else (-h+1, w-1, 1)
      in
      loop_imj min_imj min_imj 0 where rec loop_imj imj i j =
        if imj = max_imj + inc_imj then do {
          rt_clear_widget wid;
          None
        }
        else if i = w + h then do {
          if imj mod refresh_rate = 0 then do {
            rt_clear_widget wid;
            tempo ();
          }
          else ();
          loop_imj (imj + inc_imj) (imj + inc_imj) 0;
        }
        else do {
          if i >= 0 && j >= 0 then refresh_pixel drw minfo xinfo i j
          else ();
          loop_imj imj (i + 1) (j + 1)
        }
    else
      let (min_ipj, max_ipj, inc_ipj) =
        if dir = 0 then (0, w+h-1, 1) else (w+h-1, 0, -1)
      in
      loop_ipj min_ipj 0 min_ipj where rec loop_ipj ipj i j =
        if ipj = max_ipj + inc_ipj then do {
          rt_clear_widget wid;
          None
        }
        else if j = 0 then do {
          if ipj mod refresh_rate = 0 then do {
            rt_clear_widget wid;
            tempo ();
          }
          else ();
          loop_ipj (ipj + inc_ipj) 0 (ipj + inc_ipj);
        }
        else do {
          if i < w && j < h then refresh_pixel drw minfo xinfo i j else ();
          loop_ipj ipj (i + 1) (j - 1)
        }
  else do {
    (* horizontal or vertical paths *)
    let refresh_rate = max 1 (w / 100) in
    let ord_ij = Random.int 2 in
    let dir = Random.int 2 in
    let (min_i, max_i, inc_i) =
     if dir = 0 then (0, w-1, 1) else (w-1, 0, -1)
    in
    let (min_j, max_j, inc_j) =
      if dir = 0 then (0, h-1, 1) else (h-1, 0, -1)
    in
    loop 0 min_i min_j where rec loop cnt i j =
      if ord_ij = 0 && j = max_j + inc_j ||
         ord_ij = 1 && i = max_i + inc_i
      then do {
        rt_clear_widget wid;
        None
      }
      else if ord_ij = 0 && i = max_i + inc_i then do {
        if cnt = refresh_rate - 1 then do {
          rt_clear_widget wid;
          tempo ();
        }
        else ();
        loop ((cnt + 1) mod refresh_rate) min_i (j + inc_j)
      }
      else if ord_ij = 1 && j = max_j + inc_j then do {
        if cnt = refresh_rate - 1 then do {
          rt_clear_widget wid;
          tempo ();
        }
        else ();
        loop ((cnt + 1) mod refresh_rate) (i + inc_i) min_j
      }
      else do {
        refresh_pixel drw minfo xinfo i j;
        if ord_ij = 0 then loop cnt (i + inc_i) j else loop cnt i (j + inc_j)
      }
  }
};

value refresh_pixmap_with_random_effect wid info xinfo = do {
  if info.m_info.state.cur.exposing_state <> ES_terminated then ()
  else rt_select_mouse wid xinfo.cursor_busy;
  let r = refresh_pixmap_with_random_effect2 wid info xinfo in
  if info.m_info.state.cur.exposing_state <> ES_terminated then ()
  else rt_unselect_mouse wid;
  r
};

value change_saturation xd wid info xinfo inc =
  let c_pal = info.c_pal in
  let x_pal = xinfo.x_pal in
  let ini_s = c_pal.ini_max_s in
  let old_max_ds = c_pal.max_ds in
  let old_max_s = max 0 (min 255 (ini_s + old_max_ds)) in
  let new_max_ds = if inc = 0 then 0 else old_max_ds + inc * 16 in
  let new_max_s = max 0 (min 255 (ini_s + new_max_ds)) in
  if new_max_s <> old_max_s then do {
    mprintf "saturation %d\n" new_max_s;
    c_pal.max_ds := new_max_ds;
    let ini_v = c_pal.ini_max_v in
    let max_v = max 0 (min 255 (ini_v + c_pal.max_dv)) in
    for i = 0 to Array.length x_pal - 1 do {
      let c = x_pal.(i) in
      let (h, s, v) = c_pal.c_tab.(i).hsv in
      rt_free_color c;
      let new_s = if ini_s <= 0 then new_max_s else s * new_max_s / ini_s in
      let new_v = if ini_v <= 0 then max_v else v * max_v / ini_v in
      let rgb = rgb_of_hsv (h, new_s, new_v) in
      let c =  rt_create_color xd rgb in
      x_pal.(i) := c;
      c_pal.c_tab.(i) := {rgb = rgb; hsv = (h, s, v)};
    };
    refresh_pixmap_fast wid xinfo info None False;
  }
  else ()
;

value change_saturation_or_spreading xd wid minfo xinfo kev inc =
  if kev.control then
    let old_ds = minfo.c_pal_def.delta_saturation in
    let new_ds = max 1 (min 32 (if inc = 0 then 8 else old_ds + inc)) in
    if new_ds <> old_ds then do {
      let new_pd = {(minfo.c_pal_def) with delta_saturation = new_ds} in
      minfo.c_pal_def := new_pd;
      Info.install_palette {m_info = minfo; g_info = Some xinfo} new_pd;
      refresh_pixmap_fast wid (fst xinfo) minfo None False;
      mprintf "delta saturation %d\n" new_ds;
    }
    else ()
  else
    change_saturation xd wid minfo (fst xinfo) inc
;

value change_brightness xd wid info xinfo inc =
  let c_pal = info.c_pal in
  let x_pal = xinfo.x_pal in
  let ini_v = c_pal.ini_max_v in
  let old_max_dv = c_pal.max_dv in
  let old_max_v = max 0 (min 255 (ini_v + old_max_dv)) in
  let new_max_dv = if inc = 0 then 0 else old_max_dv + inc * 16 in
  let new_max_v = max 0 (min 255 (ini_v + new_max_dv)) in
  if new_max_v <> old_max_v then do {
    mprintf "brightness %d\n" new_max_v;
    c_pal.max_dv := new_max_dv;
    let ini_s = c_pal.ini_max_s in
    let max_s = max 0 (min 255 (ini_s + c_pal.max_ds)) in
    for i = 0 to Array.length x_pal - 1 do {
      let c = x_pal.(i) in
      let (h, s, v) = c_pal.c_tab.(i).hsv in
      rt_free_color c;
      let new_s = if ini_s <= 0 then max_s else s * max_s / ini_s in
      let new_v = if ini_v <= 0 then new_max_v else v * new_max_v / ini_v in
      assert (new_v < 256);
      let rgb = rgb_of_hsv (h, new_s, new_v) in
      let c = rt_create_color xd rgb in
      x_pal.(i) := c;
      c_pal.c_tab.(i) := {rgb = rgb; hsv = (h, s, v)};
    };
    refresh_pixmap_fast wid xinfo info None False;
  }
  else ()
;

value change_brightness_or_spreading xd wid minfo xinfo kev inc =
  if kev.control then
    let old_dv = minfo.c_pal_def.delta_brightness in
    let new_dv = max 1 (min 32 (if inc = 0 then 8 else old_dv + inc)) in
    if new_dv <> old_dv then do {
      let new_pd = {(minfo.c_pal_def) with delta_brightness = new_dv} in
      minfo.c_pal_def := new_pd;
      Info.install_palette {m_info = minfo; g_info = Some xinfo} new_pd;
      refresh_pixmap_fast wid (fst xinfo) minfo None False;
      mprintf "delta brightness %d\n" new_dv;
    }
    else ()
  else
    change_brightness xd wid minfo (fst xinfo) inc
;

value change_permutation xd wid minfo xinfo kev inc = do {
  let len = Array.length minfo.c_pal.c_tab in
  let old_p = minfo.c_pal_def.permutation in
  let inc = if kev.control then 10 * inc else inc in
  let new_p = (old_p + inc + len) mod len in
  let new_pd = {(minfo.c_pal_def) with permutation = new_p} in
  minfo.c_pal_def := new_pd;
  Info.install_palette {m_info = minfo; g_info = Some xinfo} new_pd;
  refresh_pixmap_fast wid (fst xinfo) minfo None False;
  mprintf "color permutation %d\n" new_p;
};

value draw_internal_rectangle xd wid xinfo = do {
  let w = widget_width wid in
  let h = widget_height wid in
  let drw = WidgetDr wid in
  let r = 5 in
  rt_set_line_width xd 1;
  rt_select_color xinfo.c_black;
  rt_draw_rectangle drw (w/4, h/4, w/2, h/2);
  rt_draw_arc drw (w/2-r, h/2-r, 2*r, 2*r, 0, 360*64);
  rt_select_color xinfo.c_white;
  rt_draw_rectangle drw (w/4-1, h/4-1, w/2+2, h/2+2);
  rt_draw_arc drw (w/2-r-1, h/2-r-1, 2*r+2, 2*r+2, 0, 360*64);
};

value draw_iteration_points wid minfo xinfo x y w h = do {
  let isc = minfo.state.cur in
  let rot = Mfloat.trigo_of_rot isc.rot in
  let drw = WidgetDr wid in
  let reduc = Mfloat.Best.of_float 0.005 in
  rt_select_color xinfo.c_gray;
  rt_fill_rectangle drw (0, 0, w, h);
  let ica = minfo.state.cur.counts_array in
  rt_select_color xinfo.c_black;
  loop 0 0 where rec loop i j =
    if j = h then ()
    else if i = w then loop 0 (j + 1)
    else do {
      let count = Array2dim.get ica i j in
      if count = None then rt_draw_point drw (i, j) else ();
      loop (i + 1) j
    };
  rt_select_color xinfo.c_blue;
  let bof = Mfloat.Best.of_float in
  let zero = bof 0.0 in
  let conv rot x y =
    Mfloat.pos_of_coord rot reduc (zero, zero) (bof x, bof y) (w, h)
  in
  List.iter
    (fun ((x1, y1), (x2, y2)) ->
       let (x1, y1) = conv rot x1 y1 in
       let (x2, y2) = conv rot x2 y2 in
       rt_draw_line drw (x1, y1) (x2, y2))
     [((-2.0, 0.0), (2.0, 0.0)); ((0.0, -2.0), (0.0, 2.0))];
  let (xc, yc) = conv rot 0.0 0.0 in
  let r1 = fst (conv None 1.0 0.0) - xc in
  let r2 = fst (conv None 2.0 0.0) - xc in
  rt_draw_arc drw (xc-r1, yc-r1, 2*r1, 2*r1, 0, 360*64);
  rt_draw_arc drw (xc-r2, yc-r2, 2*r2, 2*r2, 0, 360*64);
  let fctx =
    {f_w = w; f_h = h; f_rot = rot; f_reduc = isc.reduc;
     f_xc = isc.xc; f_yc = isc.yc; f_nb_it = isc.nb_it;
     f_invert = minfo.invert; f_julia = isc.julia}
  in
  let fp x y = do {
    let (i, j) = conv rot x y in
    rt_draw_point drw (i, j);
    rt_draw_point drw (i+1, j);
    rt_draw_point drw (i, j+1);
    rt_draw_point drw (i+1, j+1);
  }
  in
  rt_select_color xinfo.c_red;
  let restore_ctx =
    let cur_num = Mfloat.M.get_num () in
    fun () -> do {
      Mfloat.fp.val := None;
      Mfloat.M.set_num cur_num;
    }
  in
  Mfloat.fp.val := Some fp;
  Mfloat.M.set_num N_flo;
  try do {
    match
      Mfloat.compute_point_with_fun Mfloat.M.mandelbrot_point fctx x y None
    with
    [ Result n -> mprintf "result %d\n" n
    | LimitReached _ _ _ -> mprintf "limit (nb_it %d)\n" (fctx.f_nb_it) ];
  }
  with e -> do { restore_ctx (); raise e };
  restore_ctx ();
};

value action_mandel2 wid info (xinfo, xfun) ev = do {
  let xd = xdata_of_widget wid in
  let minfo = info.m_info in
  let isc = minfo.state.cur in
  let w = widget_width wid in
  let h = widget_height wid in
  match ev with
  [ RawEvKeyPress kev ->
      match kev.item with
      [ K_Ascii ' ' -> do {
          if xinfo.check_event then
            xinfo.pending_event := Some ME_pause
          else ();
          None
        }
      | K_Ascii 'a' ->
          if xinfo.check_event then return_event xinfo ev
          else Info.apply_zoom info w h None False 0
| K_Ascii 'b' -> do {
    List.iter
      (fun (a, s) -> do {
         let b = Mfloat.Best.serialize a in
         eprintf "1. %s (%d,\"%s\")\n" s b.s_prec (String.escaped b.s_value);
         let b = Mfloat.Best.serialize (Mfloat.Best.deserialize b) in
         eprintf "2. %s (%d,\"%s\")\n" s b.s_prec (String.escaped b.s_value);
         let b = Mfloat.Best.serialize (Mfloat.Best.deserialize b) in
         eprintf "3. %s (%d,\"%s\")\n" s b.s_prec (String.escaped b.s_value);
         flush stderr;
       })
      [(isc.xc, "xc"); (isc.yc, "yc"); (isc.reduc, "rd")];
    flush stderr;
    None
  }
      | K_Ascii 'c' -> do {
          let nb_hues = List.length minfo.c_pal_def.hues in
          let c_pal = minfo.c_pal.c_tab in
          let x_pal = xinfo.x_pal in
          let nb_col = Array.length x_pal in
          mprintf "%d hues %d colors\n" nb_hues nb_col;
          let reduc = float w /. float nb_col *. 0.66 in
          let xw i =
            let x = Mutil.round (float i *. reduc) in
            (x, Mutil.round (float (i + 1) *. reduc) - x)
          in
          let drw = WidgetDr wid in
          for i = 0 to 5 do {
            rt_select_color
              (if i mod 2 = 0 then xinfo.c_black else xinfo.c_white);
            rt_fill_rectangle drw (i, 0, 1, 100);
          };
          for i = 0 to nb_col - 1 do {
            rt_select_color x_pal.(i);
            let (x, w) = xw i in
            rt_fill_rectangle drw (6 + x, 0, w, 100);
          };
          let (x, _) = xw nb_col in
          for i = 0 to 5 do {
            rt_select_color
              (if i mod 2 = 0 then xinfo.c_black else xinfo.c_white);
            rt_fill_rectangle drw (6 + x + i, 0, 1, 100);
          };
          let start_hue =
            let (first_hue, _, _) = c_pal.(0).hsv in
            let (last_hue, _, _) = c_pal.(nb_col-1).hsv in
            if first_hue = last_hue then first_hue else -1
          in
          loop start_hue 0 where rec loop last_hue i =
            if i = nb_col then ()
            else do {
              let col = c_pal.(i) in
              let (hue, _, _) = col.hsv in
              if hue <> last_hue then do {
                let (x, w) = xw (i + 6) in
                rt_select_color xinfo.c_black;
                rt_draw_string drw (x, 100) (string_of_int hue);
                rt_select_color xinfo.c_white;
                rt_draw_string drw (x+1, 101) (string_of_int hue);
              }
              else ();
              loop hue (i + 1)
            };
          rt_select_color xinfo.c_black;
          let x = Mutil.round (float w *. 0.66) + 50 in
          rt_erase_draw_string drw (x, 35)
            (sprintf " %d hues %d colors " nb_hues nb_col);
          let ds = minfo.c_pal_def.delta_saturation in
          rt_erase_draw_string drw (x, 50) (sprintf " delta sat %d " ds);
          let db = minfo.c_pal_def.delta_brightness in
          rt_erase_draw_string drw (x, 65) (sprintf " delta bri %d " db);
          mprint_current_palette_def minfo minfo.c_pal_def;
          None
        }
      | K_Ascii 'd' ->
          if xinfo.check_event then return_event xinfo ev
          else do {
            isc.nb_it := max 1 (isc.nb_it / 3 * 2);
            mprintf "level %g nb_it %d\n" isc.level isc.nb_it;
            Info.expose_mandel info w h False True
          }
      | K_Ascii 'e' -> do {
          let info =
            let btf = Mfloat.Best.to_float in
            let zoom = magn_of_reduc 0.18 (btf isc.reduc) in
            {Psbrot.w = w; h = h; xc = btf isc.xc; yc = btf isc.yc;
             zoom = zoom; max_pq = 50; show_pq = 1; verbose = False;
             c_black = xinfo.c_white; c_white = xinfo.c_white;
             c_green = xinfo.c_green; c_blue = xinfo.c_blue;
             c_red = xinfo.c_red; font = xinfo.font}
          in
          Psbrot.draw_pseudobrot (WidgetDr wid) info;
          None
        }
      | K_Ascii 'f' -> do {
          minfo.force_using_slaves :=
            match minfo.force_using_slaves with
            [ Always -> Never
            | IfNeeded -> Always
            | Never -> IfNeeded ];
          mprintf "force slaves: %s\n"
            (string_of_circumstance minfo.force_using_slaves);
          None
        }
      | K_Ascii 'g' -> do {
          rt_select_color xinfo.c_white;
          let drw = WidgetDr wid in
          let trigo = Mfloat.trigo_of_rot isc.rot in
          let f i j =
            Mfloat.coord_of_pos trigo isc.reduc (isc.xc, isc.yc) (i, j) (w, h)
          in
let (x1, y2) = f 0 0 in
let (x2, y1) = f w h in
mprintf "x1 %s y1 %s x2 %s y2 %s\n" (Mfloat.Best.to_string x1) (Mfloat.Best.to_string y1) (Mfloat.Best.to_string x2) (Mfloat.Best.to_string y2);
mprintf "dx %s dy %s\n" (Mfloat.Best.to_string (Mfloat.Best.sub x2 x1)) (Mfloat.Best.to_string (Mfloat.Best.sub y2 y1));
let d = abs_float (Mfloat.Best.to_float (Mfloat.Best.sub y2 y1)) in
let (dman, dexp) =
  loop d 1.0 where rec loop d exp =
    if d < 1.0 then loop (d *. 10.0) (exp *. 10.0)
    else (d, exp)
in
mprintf "mant %g exp %g\n" dman dexp;

mprintf "x1 %.15f x2 %.15f\n" (Mfloat.Best.to_float x1 -. mod_float (Mfloat.Best.to_float x1) (1./.dexp)) (Mfloat.Best.to_float x2 -. mod_float (Mfloat.Best.to_float x2) (1./.dexp));
mprintf "y1 %.15f y2 %.15f\n" (Mfloat.Best.to_float y1 -. mod_float (Mfloat.Best.to_float y1) (1./.dexp)) (Mfloat.Best.to_float y2 -. mod_float (Mfloat.Best.to_float y2) (1./.dexp));

let ix1 = Mfloat.Best.to_float x1 -. mod_float (Mfloat.Best.to_float x1) (1./.dexp) in
let iy1 = Mfloat.Best.to_float y1 -. mod_float (Mfloat.Best.to_float y1) (1./.dexp) in
let ix2 = Mfloat.Best.to_float x2 -. mod_float (Mfloat.Best.to_float x2) (1./.dexp) in
let iy2 = Mfloat.Best.to_float y2 -. mod_float (Mfloat.Best.to_float y2) (1./.dexp) in
          let g a b =
            let (i, j) =
              Mfloat.pos_of_coord trigo isc.reduc (isc.xc, isc.yc) (a, b)
                (w, h)
            in
            (max ~-32768 (min 32767 i), max ~-32768 (min 32767 j))
          in

loop ix1 (-1, -1) where rec loop ix (prev_i, prev_j) =
  if ix > ix2 then ()
  else
   let (i, j) = g (Mfloat.Best.of_float ix) y1 in
   if i = prev_i && j = prev_j then ()
   else  do {
     rt_draw_line drw (i, j) (g (Mfloat.Best.of_float ix) y2);
      loop (ix +. 1./.dexp) (i, j)
    };

loop iy1 (-1, -1) where rec loop iy (prev_i, prev_j) =
  if iy > iy2 then ()
  else
    let (i, j) = g x1 (Mfloat.Best.of_float iy) in
    if i = prev_i && j = prev_j then ()
    else do {
      rt_draw_line drw (i, j) (g x2 (Mfloat.Best.of_float iy));
      loop (iy +. 1./.dexp) (i, j)
    };

(*
          rt_draw_line drw (g ~-.2.0 0.0) (g 2.0 0.0);
          rt_draw_line drw (g 0.0 ~-.2.0) (g 0.0 2.0);
*)
          None
        }
      | K_Ascii 'h' -> do { display_help wid; None }
      | K_Ascii 'i' -> do {
          let drw = WidgetDr wid in
          rt_select_color xinfo.c_white;
          iter_points minfo w h ()
            (fun cl x i j ->
               if List.length cl > 8 then do {
                 rt_draw_point drw (i, j);
                 Some (x, True)
               }
               else None);
          None
        }
      | K_Ascii 'j' ->
          if xinfo.check_event then return_event xinfo ev
          else do {
            Info.push_state minfo;
            isc.julia :=
              match isc.julia with
              [ Some (xc, yc) -> None
              | None -> Some (isc.xc, isc.yc) ];
            let txt = if isc.julia = None then "Mandelbrot" else "Julia" in
            alert wid xinfo txt;
            Info.expose_mandel info w h True True
          }
      | K_Ascii 'k' -> do {
          minfo.check_slaves_answers := not minfo.check_slaves_answers;
          mprintf "check slaves answers: %b\n" minfo.check_slaves_answers;
          isc.nb_diff_answ := 0;
          None
        }
      | K_Ascii 'l' ->
          if xinfo.check_event then return_event xinfo ev
          else Info.expose_mandel info w h True True
      | K_Ascii 'm' -> do {
          (* transformation du photomaton *)
          let ica = minfo.state.cur.counts_array in
          let dim1 = Array2dim.dim1 ica in
          let dim2 = Array2dim.dim2 ica in
          let old_ica = Array2dim.init dim1 dim2 (Array2dim.get ica) in
          loop 0 0 where rec loop i j =
            if i + 1 >= w then ()
            else if j + 1 >= h then loop (i + 2) 0
            else do {
              Array2dim.set ica (i / 2) (j / 2)
                (Array2dim.get old_ica i j);
              Array2dim.set ica (i / 2) ((j + h) / 2)
                (Array2dim.get old_ica i (j + 1));
              Array2dim.set ica ((i + w) / 2) (j / 2)
                (Array2dim.get old_ica (i + 1) j);
              Array2dim.set ica ((i + w) / 2) ((j + h) / 2)
                (Array2dim.get old_ica (i + 1) (j + 1));
              loop i (j + 2)
            };
          refresh_pixmap_fast wid xinfo minfo None True;
          None
        }
      | K_Ascii 'o' -> do {
          print_command minfo w h (Some (widget_x wid, widget_y wid));
          None
        }
      | K_Ascii 'p' -> do {
          let get_pixel_color = Info.get_pixel_color_fun minfo in
          let fname = "mlbrot.ppm" in
          let oc = open_out fname in
          output_ppm oc (get_pixel_color w h);
          close_out oc;
          mprintf "file '%s' saved\n" fname;
          if not (stdout_is_a_tty ()) then
            output_ppm stdout (get_pixel_color w h)
          else ();
          None
        }
      | K_Ascii 'q' ->
          if not kev.control then None
          else if xinfo.check_event then return_event xinfo ev
          else do {
            xinfo.no_main_loop := True;
            rt_stop_main_loop xinfo.xargs;
            None
          }
      | K_Ascii ('r' | 'R' as g) -> do {
          let rs = minfo.random_for_colors in
          let pd =
            if g = 'R' then
              make_very_random_palette_def rs
            else
              let c_pal = minfo.c_pal in
              let sat = max 0 (min 255 (c_pal.ini_max_s + c_pal.max_ds)) in
              let bri = max 0 (min 255 (c_pal.ini_max_v + c_pal.max_dv)) in
              make_random_palette_def rs sat bri
          in
          Info.push_palette info pd [];
          refresh_pixmap_with_random_effect wid info xinfo
        }
      | K_Ascii 's' ->
          if xinfo.check_event then return_event xinfo ev
          else do {
            let new_reduc =
              Mfloat.Best.mult isc.reduc
                (Mfloat.Best.of_float Info.zoom_coeff)
            in
            Info.push_state minfo;
            isc.reduc := new_reduc;
            let new_level = level_of_reduc minfo.extra_reduc new_reduc in
            isc.level := new_level;
            if isc.exposing_state = ES_terminated then
              (* optimization: using what was already computed *)
              let di = (1. -. Info.zoom_coeff) *. float (w / 2) +. 0.5 in
              let dj = (1. -. Info.zoom_coeff) *. float (h / 2) +. 0.5 in
              match
                Info.expose_fast info isc w h
                  (fun i -> Mutil.round (Info.zoom_coeff *. float i +. di))
                  (fun j -> Mutil.round (Info.zoom_coeff *. float j +. dj))
              with
              [ Some ev -> Some ev
              | None -> Info.finish_exposing_remaining_levels info w h ]
            else
              (* normal case *)
              Info.expose_mandel info w h True True
          }
      | K_Ascii 't' -> do {
          trace_slave_points stderr minfo isc w h;
          None
        }
      | K_Ascii 'u' ->
          if xinfo.check_event then return_event xinfo ev
          else do {
            isc.nb_it := (max 2 isc.nb_it) * 3 / 2;
            mprintf "level %g nb_it %d\n" isc.level isc.nb_it;
            Info.expose_mandel info w h False True
          }
      | K_Ascii 'x' ->
          if xinfo.check_event then return_event xinfo ev
          else do {
            Info.push_state minfo;
            isc.reduc := Mfloat.Best.twice isc.reduc;
            isc.level := isc.level -. 1.0;
            Info.expose_mandel info w h True True
          }
      | K_Ascii 'X' ->
          if xinfo.check_event then return_event xinfo ev
          else do {
            Info.push_state minfo;
            for i = 1 to 10 do { isc.reduc := Mfloat.Best.twice isc.reduc };
            isc.level := isc.level -. 10.0;
            Info.expose_mandel info w h True True
          }
      | K_Ascii 'z' ->
          if xinfo.check_event then return_event xinfo ev
          else do {
            Info.push_state minfo;
            isc.reduc := Mfloat.Best.half isc.reduc;
            isc.level := isc.level +. 1.0;
            let x = w / 2 in
            let y = h / 2 in
            Info.expose_optim info x y w h
          }
      | K_Ascii 'Z' ->
          if xinfo.check_event then return_event xinfo ev
          else do {
            Info.push_state minfo;
            for i = 1 to 10 do { isc.reduc := Mfloat.Best.half isc.reduc };
            isc.level := isc.level +. 10.0;
            Info.expose_mandel info w h True True
          }
      | IFDEF MPFR OR MPZ THEN
        K_Ascii '0' | K_KP_N 0 -> do {
          Info.set_auto_float minfo isc;
          Info.trace_level minfo isc;
          None
        }
        END
      | K_Ascii '1' | K_KP_N 1 -> do { Info.set_num minfo isc N_int; None }
      | K_Ascii '2' | K_KP_N 2 -> do { Info.set_num minfo isc N_i64; None }
      | K_Ascii '3' | K_KP_N 3 -> do { Info.set_num minfo isc N_flo; None }
      | K_Ascii '4' | K_KP_N 4 -> do { Info.set_num minfo isc N_big; None }
      | IFDEF MPFR THEN
        K_Ascii '5' | K_KP_N 5 -> do { Info.set_num minfo isc N_mpf; None }
        END
      | IFDEF MPZ THEN
        K_Ascii '6' | K_KP_N 6 -> do { Info.set_num minfo isc N_mpz; None }
        END
      | K_Ascii '[' -> do {
          change_saturation_or_spreading xd wid minfo (xinfo, xfun) kev (-1);
          None
        }
      | K_Ascii ']' -> do {
          change_saturation_or_spreading xd wid minfo (xinfo, xfun) kev 1;
          None
        }
      | K_Ascii '=' -> do {
          change_saturation_or_spreading xd wid minfo (xinfo, xfun) kev 0;
          None
        }
      | K_Ascii '{' -> do {
          change_brightness_or_spreading xd wid minfo (xinfo, xfun) kev (-1);
          None
        }
      | K_Ascii '}' -> do {
          change_brightness_or_spreading xd wid minfo (xinfo, xfun) kev 1;
          None
        }
      | K_Ascii '-' -> do {
          change_brightness_or_spreading xd wid minfo (xinfo, xfun) kev 0;
          None
        }
      | K_Ascii '(' -> do {
          change_permutation xd wid minfo (xinfo, xfun) kev ~-1;
          None
        }
      | K_Ascii ')' -> do {
          change_permutation xd wid minfo (xinfo, xfun) kev 1;
          None
        }
      | K_Ascii '/' -> do { rt_clear_widget wid; None }
      | K_Ascii '.' -> do { draw_internal_rectangle xd wid xinfo; None }
      | K_Ascii ',' ->
          if xinfo.check_event then return_event xinfo ev
          else
            let cnt =
              iter_points minfo w h 0
                (fun cl cnt _ _ ->
                   if List.length cl > 6 then Some (cnt + 1, True) else None)
            in
            if cnt = 0 then None
            else do {
              let cnt = Random.int cnt in
              let (i, j, _) =
                iter_points minfo w h (0, 0, cnt)
                  (fun cl (_, _, cnt) i j ->
                     if List.length cl > 6 then
                       if cnt = 0 then Some ((i, j, 0), False)
                       else Some ((0, 0, cnt - 1), True)
                     else None)
              in
              Info.push_state minfo;
              recenter minfo i j w h;
              Info.expose_mandel info w h True True
            }
      | K_Ascii '~' -> do {
          let wid =
            let expose_cnt = ref 0 in
            let stat_event wid =
              fun
              [ RawEvExpose _ _ _ _ | RawEvConfigureNotify _ _ -> do {
                  incr expose_cnt;
                  try
                    while rt_pending_events xinfo.xargs do {
                      rt_treat_one_event xinfo.xargs
                    }
                  with e -> do { decr expose_cnt; raise e };
                  decr expose_cnt;
                  if expose_cnt.val = 0 then do {
                    let max_nb_it =
                      iter_points minfo w h 0
                        (fun cl max_nb_it i j ->
                           match cl with
                           [ [c] -> Some (max c max_nb_it, True)
                           | [] | [_ :: _] -> failwith "error" ])
                    in
                    let t = Array.make (max_nb_it + 1) 0 in
                    iter_points minfo w h ()
                      (fun cl x i j ->
                         match cl with
                         [ [c] -> do {
                             t.(c) := t.(c) + 1;
                             Some (x, True)
                           }
                         | [] | [_ :: _] -> failwith "error" ]);
                    let min_y = 0 in
                    let max_y =
                      loop 0 0 where rec loop max_y i =
                        if i = Array.length t then max_y
                        else loop (max max_y t.(i)) (i + 1)
                    in
  let _ = do { mprintf "max x %d max y %d\n" max_nb_it max_y } in
                    let min_x =
                      loop 0 where rec loop i =
                        if t.(i) < max_y / 100 then loop (i + 1)
                        else i
                    in
                    let max_x =
                      loop max_nb_it where rec loop i =
                        if t.(i) < max_y / 100 then loop (i - 1)
                        else i
                    in
  let _ = do { mprintf "between %d and %d\n" min_x max_x } in
                    let sw = widget_width wid in
                    let sh = widget_height wid in
                    let drw = WidgetDr wid in
                    rt_clear_widget wid;
                    rt_select_color xinfo.c_black;
                    let fx x =
                      (x - min_x) * (sw - 20) / (max_x - min_x) + 10
                    in
                    let fy y =
                      (y - min_y) * (sh - 20) / (max_y - min_y) + 10
                    in
                    for i = min_x + 1 to max_x do {
                      let x1 = fx (i-1) in
                      let y1 = fy t.(i-1) in
                      let x2 = fx i in
                      let y2 = fy t.(i) in
                      rt_draw_line drw (x1, sh - y1) (x2, sh - y2)
                    }
                  }
                  else ();
                }
              | RawEvEnterNotify _ _ _ _ | RawEvLeaveNotify -> ()
              | RawEvFocusIn | RawEvFocusOut -> ()
              | RawEvKeyPress _ -> ()
              | ev ->
                  failwith
                    (sprintf "stat event %s\n"
                       (if Obj.is_int (Obj.repr ev) then
                          sprintf "(int %d)" (Obj.magic ev)
                        else
                          sprintf "(tag %d)" (Obj.tag (Obj.repr ev)))) ]
            in
            rt_create_widget xd "stat" "stat" AutoPosition
              (Some rt_destroy_widget)
              (raw_desc [] (1000, 300, 0, [SelExposure; SelStructureNotify])
                 stat_event)
          in
          rt_map_widget wid;
          None
        }
      | K_Left ->
          match minfo.pal_defs.bef with
          [ [] -> None
          | [pd :: pdl] -> do {
              mprint_current_palette_def minfo pd;
              Info.pop_palette info pd pdl;
              refresh_pixmap_fast wid xinfo minfo None False;
              None
            } ]
      | K_Right ->
          match minfo.pal_defs.aft with
          [ [] -> None
          | [pd :: pdl] -> do {
              mprint_current_palette_def minfo pd;
              Info.push_palette info pd pdl;
              refresh_pixmap_fast wid xinfo minfo None False;
              None
            } ]
      | K_Up ->
          if xinfo.check_event && minfo.state.bef <> [] then
            return_event xinfo ev
          else
            let st = minfo.state in
            match st.bef with
            [ [] -> None
            | [s :: sl] -> do {
                minfo.state := {bef = sl; cur = s; aft = [st.cur :: st.aft]};
                st.cur.counts_array :=
                  Array2dim.compress st.cur.counts_array;
                s.counts_array := Array2dim.uncompress s.counts_array;
                refresh_pixmap_fast wid xinfo minfo None False;
                Gc.compact ();
                match s.exposing_state with
                [ ES_in_progress eip ->
                    Info.expose_mandel_kont (get_info wid) eip w h True True
                | ES_terminated -> do {
                    Info.trace_level minfo s;
                    Info.finish_exposing_remaining_levels info w h
                  } ]
              } ]
      | K_Down ->
          if xinfo.check_event && minfo.state.aft <> [] then
            return_event xinfo ev
          else
            let st = minfo.state in
            match st.aft with
            [ [] -> None
            | [s :: sl] -> do {
                minfo.state := {bef = [st.cur :: st.bef]; cur = s; aft = sl};
                st.cur.counts_array :=
                  Array2dim.compress st.cur.counts_array;
                s.counts_array := Array2dim.uncompress s.counts_array;
                refresh_pixmap_fast wid xinfo minfo None False;
                Gc.compact ();
                match s.exposing_state with
                [ ES_in_progress eip ->
                    Info.expose_mandel_kont (get_info wid) eip w h True True
                | ES_terminated -> do {
                    Info.trace_level minfo s;
                    Info.finish_exposing_remaining_levels info w h
                  } ]
              } ]
      | K_Escape -> do { rt_clear_widget wid; None }
      | _ -> None ]
  | RawEvButtonPress x y _ _ k ->
      match k.item with
      [ 1 ->
          if k.shiftMod then do {
            draw_iteration_points wid minfo xinfo x y w h;
            None
          }
          else if k.control then do {
            let trigo = Mfloat.trigo_of_rot isc.rot in
            let (x, y) =
              Mfloat.coord_of_pos trigo isc.reduc (isc.xc, isc.yc) (x, y)
                (w, h)
            in
            mprintf "position %s %s\n" (Mfloat.Best.to_string x)
              (Mfloat.Best.to_string y);
            None
          }
          else if xinfo.check_event then return_event xinfo ev
          else do {
            let new_reduc = Mfloat.Best.half isc.reduc in
            Info.push_state minfo;
            recenter minfo x y w h;
            isc.reduc := new_reduc;
            isc.level := isc.level +. 1.0;
            Info.expose_optim info x y w h;
          }
      | 2 ->
          if xinfo.check_event then return_event xinfo ev
          else do {
            recenter minfo x y w h;
(**)
            if isc.exposing_state = ES_terminated then
              let dx = (2 * x - w) / 2 in
              let dy = (2 * y - h) / 2 in
              Info.expose_move info dx dy w h 0
            else
(**)
            Info.expose_mandel info w h True True
          }
      | 3 ->
          if xinfo.check_event then return_event xinfo ev
          else do {
            Info.push_state minfo;
            isc.reduc := Mfloat.Best.twice isc.reduc;
            isc.level := isc.level -. 1.0;
            Info.expose_mandel info w h True True
          }
      | 4 | 5 as b ->
          if xinfo.check_event then return_event xinfo ev
          else do {
            Info.push_state minfo;
            let new_rot =
              let d = if k.shiftMod then 10 else 1 in
              if b = 4 then Int10.add_int isc.rot d
              else Int10.sub_int isc.rot d
            in
            isc.rot := Info.normalize_angle new_rot;
            Info.expose_mandel info w h True True
          }
      | _ -> None ]
  | RawEvExpose _ _ _ _ -> do { rt_clear_widget wid; None }
  | RawEvConfigureNotify (ow, oh, _) (w, h, _) ->
      if xinfo.check_event && (w <> ow || h <> oh) then
        return_event xinfo ev
      else if w <> ow || h <> oh then do {
        minfo.state := {(minfo.state) with bef = []; aft = []};
        Info.expose_mandel info w h True False
      }
      else None
  | _ ->
      None ]
};

value action_mandel wid ev =
  let info = get_info wid in
  match info.g_info with
  [ Some (xinfo, xfun) ->
      loop ev where rec loop ev = do {
        let r = action_mandel2 wid info (xinfo, xfun) ev in
        match ev with
        [ RawEvKeyPress _ | RawEvButtonPress _ _ _ _ _ -> mflush ()
        | _ -> () ];
        match r with
        [ Some ev -> loop ev
        | None -> () ]
      }
  | None -> () ]
;

value x_init init_pos init_wid init_hei c_pal_def c_pal = do {
  let xd = rt_initialize "" in
  rt_select_char_set xd Utf_8;
  let xa = rt_args [xd] in
  let init_pos =
    match init_pos with
    [ Some (x, y) -> UserPosition x y
    | None -> AutoPosition ]
  in
  let wid =
    let name = Filename.basename Sys.argv.(0) in
    rt_create_widget xd name name init_pos
      (Some (fun _ -> rt_stop_main_loop xa))
      (raw_desc []
         (init_wid, init_hei, 0,
          [SelExposure; SelKeyPress; SelStructureNotify; SelButtonPress])
         action_mandel)
  in
  let scr_w = screen_width xd in
  let scr_h = screen_height xd in
  let font = rt_load_query_font xd "-*-*-bold-r-normal-*-24-*-*-*-*-*-*-*" in
  rt_select_font font;
  let w = max init_wid scr_w in
  let h = max init_hei scr_h in
  let pixmap = rt_create_pixmap xd w h in
  let c_red = rt_create_color xd (255, 0, 0) in
  let c_green = rt_create_color xd (0, 255, 0) in
  let c_blue = rt_create_color xd (0, 0, 255) in
  let c_gray = rt_create_color xd (196, 196, 196) in
  let c_black = rt_black_color xd in
  let c_white = rt_white_color xd in
  let x_pal = Array.map (fun p -> rt_create_color xd p.rgb) c_pal in
  let c_start = rt_create_color xd (0xd1, 0xe0, 0xa7) in
  rt_select_color c_start;
  rt_fill_rectangle (PixmapDr pixmap) (0, 0, w, h);
  let font = rt_load_query_font xd "-*-*-bold-*-*-*-13-*-*-*-*-*-iso8859-*" in
  rt_select_font font;
  let xinfo =
    {xargs = xa; widget = wid; pixmap = pixmap; font = font;
     c_red = c_red; c_green = c_green; c_blue = c_blue;
     c_gray = c_gray; c_black = c_black; c_white = c_white;
     last_col = c_red;
     cursor_busy = rt_create_font_mouse xd xC_watch;
     cursor_pause = rt_create_font_mouse xd xC_center_ptr;
     cursor_finish = rt_create_font_mouse xd xC_tcross;
     x_pal = x_pal; check_event = False; pending_event = None;
     no_main_loop = False}
  in
  let w = max init_wid scr_w in
  let h = max init_hei scr_h in
  (xinfo, w, h)
};

value update_point_in_pixmap xinfo chunk area (i, j) count = do {
  let drw = PixmapDr xinfo.pixmap in
  x_select_color xinfo count;
  if chunk = 1 then
    rt_draw_point drw (i, j)
  else do {
    let x = area.r_x in
    let y = area.r_y in
    let w = area.r_w in
    let h = area.r_h in
    let x1 = max x (i - chunk / 2) in
    let y1 = max y (j - chunk / 2) in
    let x2 = min (x + w) (i + chunk / 2) in
    let y2 = min (y + h) (j + chunk / 2) in
    if x2 > x1 && y2 > y1 then
      rt_fill_rectangle drw (x1, y1, x2 - x1, y2 - y1)
    else ()
  }
};

value x_pending_events xinfo = Rt.rt_pending_events xinfo.xargs;

value x_make_area_visible xinfo eip frac (x, y, w, h) =
  let wid = xinfo.widget in
  rt_clear_area wid (x, y, w, h)
;

value x_refresh_pixmap_fast xinfo minfo show_fast =
  refresh_pixmap_fast xinfo.widget xinfo minfo None show_fast
;

value x_copy_area ginfo minfo (x, y, w, h) (x_dest, y_dest) = do {
  let drw = PixmapDr ginfo.pixmap in
  rt_copy_area drw drw (x, y, w, h) (x_dest, y_dest)
};

value x_make_window_visible xinfo = rt_clear_widget xinfo.widget;

value x_select_cursor xinfo into_win =
  rt_select_mouse xinfo.widget
    (if into_win then xinfo.cursor_busy else xinfo.cursor_finish)
;

value x_select_cursor_pause xinfo =
  rt_select_mouse xinfo.widget xinfo.cursor_pause
;

value x_set_palette xinfo pal = do {
  let wid = xinfo.widget in
  let xd = xdata_of_widget wid in
  Array.iter rt_free_color xinfo.x_pal;
  xinfo.x_pal := Array.map (fun p -> rt_create_color xd p.rgb) pal
};

value x_get_pending_event info xinfo = do {
  xinfo.check_event := True;
  xinfo.pending_event := None;
  rt_treat_one_event xinfo.xargs;
  let r = xinfo.pending_event in
  xinfo.check_event := False;
  xinfo.pending_event := None;
  r
};

value x_unselect_cursor xinfo = rt_unselect_mouse xinfo.widget;

value x_widget_size xinfo =
  let w = widget_width xinfo.widget in
  let h = widget_height xinfo.widget in
  (w, h)
;

value xfun =
  {update_point_in_pixmap = update_point_in_pixmap;
   g_copy_area = x_copy_area;
   g_get_pending_event = x_get_pending_event;
   g_make_area_visible = x_make_area_visible;
   g_make_window_visible = x_make_window_visible;
   g_pending_events = x_pending_events;
   g_refresh_pixmap_fast = x_refresh_pixmap_fast;
   g_select_cursor = x_select_cursor;
   g_select_cursor_pause = x_select_cursor_pause;
   g_set_palette = x_set_palette;
   g_unselect_cursor = x_unselect_cursor;
   g_widget_size = x_widget_size}
;

value ask_for_connection info i = do {
  let s = Mutil.file_descr_of_int i in
  match Compute.hire_slave_not_working info.m_info s with
  [ Some s ->
      match info.g_info with
      [ Some (xinfo, _) -> xinfo.pending_event := Some (ME_environ_to_set s)
      | None -> () ]
  | None -> () ]
};

value interactive () = do {
  let (w, h, pos, rs, c_pal_def, c_pal, slave_hiring, scenario_opt) =
    Info.common_init ()
  in
  let (xinfo, scr_w, scr_h) = x_init pos w h c_pal_def c_pal in
  let minfo = make_minfo rs scr_w scr_h c_pal_def c_pal slave_hiring in
  let info = {m_info = minfo; g_info = Some (xinfo, xfun)} in

  let xa = xinfo.xargs in
  let wid = xinfo.widget in
  let pixmap = xinfo.pixmap in

  match slave_hiring with
  [ Some (s, _) ->
      rt_select_file_descr xa (Mutil.int_of_file_descr s)
        (ask_for_connection info)
  | None -> () ];
  rt_set_user_info wid (set_info info);
  rt_change_background wid (PixmapPn pixmap);
  rt_map_widget wid;
  alert wid xinfo "type h for help";
  if try Sys.getenv "MLBROT" = "q" with [ Not_found -> False ] then
    xinfo.no_main_loop := True
  else ();
  let wid_w = widget_width wid in
  let wid_h = widget_height wid in
  match Info.expose_mandel info wid_w wid_h True True with
  [ Some ev -> action_mandel wid ev
  | None -> () ];
  if xinfo.no_main_loop then ()
  else do {
    match scenario_opt with
    [ Some scenario ->
        match Info.apply_scenario info w h scenario arg_ppm_from.val with
        [ Some ev -> action_mandel wid ev
        | None -> () ]
    | None -> () ];
    if xinfo.no_main_loop then () else rt_main_loop xa;
  };
  let wid_w = widget_width wid in
  let wid_h = widget_height wid in
  let wid_x = widget_x wid in
  let wid_y = widget_y wid in
  print_command minfo wid_w wid_h (Some (wid_x, wid_y));
};
