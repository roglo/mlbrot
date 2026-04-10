(* $Id: graph_gtk.ml,v 1.134 2014/06/01 08:56:35 deraugla Exp $ *)

open Info_def;
open Mdef;
open Mprintf;
open Palette_def;
open Printf;

type ginfo =
  { root : Gtk.obj Gtk.window;
    window : Gtk.obj Gtkn.window;
    window_gc : Gdk.gc;
    pixmap : Gtk.obj Gtkn.pixmap;
    pixmap_gc : Gdk.gc;
    menu_bar_noteb : Gtk.obj Gtk.notebook;
    menu_bar : mutable Gtk.obj Gtk.menu_bar;
    width : mutable int;
    height : mutable int;
    colormap : Gdk.colormap;
    black : Gdk.color;
    white : Gdk.color;
    palette : mutable array Gdk.color;
    status : Gtk.obj Gtk.label;
    progress_bar : Gtk.obj Gtk.progress_bar;
    button_table : Hashtbl.t string (Gtk.obj Gtk.menu_item);
    check_table : Hashtbl.t string (Gtk.obj Gtk.check_menu_item);
    cursor_watch : Gdk.cursor;
    cursor_cross : Gdk.cursor;
    cursor_pause : Gdk.cursor;
    cursor_default : Gdk.cursor;
    check_event : mutable bool;
    pending_event : mutable option (mlbrot_event event);
    rectangle_drawn : mutable bool;
    first_configure : mutable bool;
    no_main_loop : mutable bool }
and event = { ev_fun : unit -> option event };

value g_draw_internal_rectangle ginfo = do {
  let w = ginfo.width in
  let h = ginfo.height in
  let r = 5 in
  let dr = Gtkn.WindowDr ginfo.window in
  let gc = ginfo.window_gc in
  Gdk.GC.set_foreground gc ginfo.black;
  Gtkn.draw_rectangle dr gc False (w/4) (h/4) (w/2) (h/2);
  Gtkn.draw_arc dr gc False (w/2-r) (h/2-r) (2*r) (2*r) 0.0 360.0;
  Gtkn.draw_line dr gc (0, h/2) (w/4-1, h/2);
  Gtkn.draw_line dr gc (3*w/4+1, h/2) (w, h/2);
  Gtkn.draw_line dr gc (w/2, 0) (w/2, h/4-1);
  Gtkn.draw_line dr gc (w/2, 3*h/4+1) (w/2, h);
  Gdk.GC.set_foreground gc ginfo.white;
  Gtkn.draw_rectangle dr gc False (w/4-1) (h/4-1) (w/2+2) (h/2+2);
  Gtkn.draw_arc dr gc False (w/2-r-1) (h/2-r-1) (2*r+2) (2*r+2) 0.0 360.0;
  Gtkn.draw_line dr gc (0, h/2-1) (w/4-1, h/2-1);
  Gtkn.draw_line dr gc (3*w/4+1, h/2-1) (w, h/2-1);
  Gtkn.draw_line dr gc (w/2-1, 0) (w/2-1, h/4-1);
  Gtkn.draw_line dr gc (w/2-1, 3*h/4+1) (w/2-1, h);
};

value g_select_color ginfo count = do {
  let gc = ginfo.pixmap_gc in
  let color =
    match count with
    [ Some count -> ginfo.palette.(count mod Array.length ginfo.palette)
    | None -> ginfo.black ]
  in
  Gdk.GC.set_foreground gc color
};

value update_point_in_pixmap ginfo chunk area (i, j) count = do {
  g_select_color ginfo count;
  if chunk = 1 then do {
    let dr = Gtkn.PixmapDr ginfo.pixmap in
    let gc = ginfo.pixmap_gc in
    Gtkn.draw_point dr gc i j;
  }
  else do {
    let x = area.r_x in
    let y = area.r_y in
    let w = area.r_w in
    let h = area.r_h in
    let x1 = max x (i - chunk / 2) in
    let y1 = max y (j - chunk / 2) in
    let x2 = min (x + w) (i + chunk / 2) in
    let y2 = min (y + h) (j + chunk / 2) in
    if x2 > x1 && y2 > y1 then do {
      let dr = Gtkn.PixmapDr ginfo.pixmap in
      let gc = ginfo.pixmap_gc in
      Gtkn.draw_rectangle dr gc True x1 y1 (x2 - x1) (y2 - y1);
    }
    else ()
  }
};

value g_get_event info ginfo = do {
  ginfo.check_event := True;
  ginfo.pending_event := None;
  try let _ : bool = Glib.Main.iteration True in () with e -> do {
    ginfo.check_event := False;
    raise e
  };
  let r = ginfo.pending_event in
  ginfo.check_event := False;
  ginfo.pending_event := None;
  r
};

value g_make_area_visible ginfo eip frac (x, y, w, h) = do {
  let dr = Gtkn.WindowDr ginfo.window in
  let gc = ginfo.window_gc in
  Gtkn.draw_pixmap dr gc ginfo.pixmap x y x y w h;
  if ginfo.rectangle_drawn then g_draw_internal_rectangle ginfo else ();
  Gtkn.progress_bar_set_fraction ginfo.progress_bar frac;
  let txt =
    if eip.eip_chunk = 1 then sprintf "%s" (I18n.transl "pixels")
    else
      sprintf "%s %dx%d" (I18n.transl "squares") eip.eip_chunk eip.eip_chunk
  in
  let txt =
    if eip.eip_refining then txt ^ " " ^ I18n.transl "refining" else txt
  in
  Gtkn.progress_bar_set_text ginfo.progress_bar txt;
};

value g_make_window_visible ginfo = do {
  Gtkn.window_clear ginfo.window;
  if ginfo.rectangle_drawn then g_draw_internal_rectangle ginfo else ();
  Gtkn.progress_bar_set_fraction ginfo.progress_bar 1.0;
};

value g_pending_events ginfo = do {
  Glib.Main.pending ()
};

value refresh_pixel minfo ginfo i j =
  let ica = minfo.state.cur.counts_array in
  if i < Array2dim.dim1 ica && j < Array2dim.dim2 ica then do {
    let count = Array2dim.get ica i j in
    g_select_color ginfo count;
    let dr = Gtkn.PixmapDr ginfo.pixmap in
    let gc = ginfo.pixmap_gc in
    Gtkn.draw_point dr gc i j;
  }
  else ()
;

value g_select_cursor ginfo into_win = do {
  Gtkn.set_cursor ginfo.window
    (if into_win then ginfo.cursor_watch else ginfo.cursor_cross);
  if into_win then Gtkn.widget_show (Gtkn.ProgressBarWid ginfo.progress_bar)
  else Gtkn.widget_hide (Gtkn.ProgressBarWid ginfo.progress_bar);
};

value g_select_cursor_pause ginfo = do {
  Gtkn.set_cursor ginfo.window ginfo.cursor_pause
};

value g_unselect_cursor ginfo = do {
  Gtkn.set_cursor ginfo.window ginfo.cursor_default;
  Gtkn.widget_hide (Gtkn.ProgressBarWid ginfo.progress_bar);
};

value refresh_pixmap_fast ginfo minfo rect_opt show_fast = do {
  let (x, y, w, h) =
    match rect_opt with
    [ Some r -> r
    | None -> (0, 0, ginfo.width, ginfo.height) ]
  in
  loop x y where rec loop i j =
    if j = y + h then
      if show_fast then () else g_make_window_visible ginfo
    else if i = x + w then do {
      if (*True ||*) show_fast then do {
        let dr = Gtkn.WindowDr ginfo.window in
        let gc = ginfo.window_gc in
        Gtkn.draw_pixmap dr gc ginfo.pixmap x j x j w 1;
      }
      else ();
      loop x (j + 1)
    }
    else do {
      refresh_pixel minfo ginfo i j;
      loop (i + 1) j
    };
};

value g_refresh_pixmap_fast ginfo minfo show_fast = do {
  if minfo.state.cur.exposing_state = ES_terminated then ()
  else g_select_cursor ginfo True;
  refresh_pixmap_fast ginfo minfo None show_fast;
  if minfo.state.cur.exposing_state = ES_terminated then ()
  else g_unselect_cursor ginfo;
};

value g_copy_area ginfo minfo (x, y, w, h) (x_dest, y_dest) = do {
  Gtkn.draw_pixmap (Gtkn.PixmapDr ginfo.pixmap) ginfo.pixmap_gc ginfo.pixmap
    x y x_dest y_dest w h;
};

value gtk_color_of_color colormap (r, g, b) =
  Gdk.Color.alloc colormap (`RGB (256 * r) (256 * g) (256 * b))
;

value g_set_palette ginfo pal = do {
  ginfo.palette :=
    Array.map (fun p -> gtk_color_of_color ginfo.colormap p.rgb) pal;
};

value gfun =
  {update_point_in_pixmap = update_point_in_pixmap;
   g_copy_area = g_copy_area;
   g_get_pending_event = g_get_event;
   g_make_area_visible = g_make_area_visible;
   g_make_window_visible = g_make_window_visible;
   g_pending_events = g_pending_events;
   g_refresh_pixmap_fast = g_refresh_pixmap_fast;
   g_select_cursor = g_select_cursor;
   g_select_cursor_pause = g_select_cursor_pause;
   g_set_palette = g_set_palette;
   g_unselect_cursor = g_unselect_cursor;
   g_widget_size = fun []}
;

value return_event ginfo ev = do {
  ginfo.pending_event := Some (ME_gevent ev);
  None
};

value rec pause info ginfo = do {
  if ginfo.check_event then ginfo.pending_event := Some ME_pause
  else ();
  None
};

value rec lazy_master info ginfo = do {
  if ginfo.check_event then
    info.m_info.state.cur.master_lazy := not info.m_info.state.cur.master_lazy
  else ();
  None
};

value rec quit info ginfo = do {
  if ginfo.check_event then return_event ginfo {ev_fun () = quit info ginfo}
  else do {
    ginfo.no_main_loop := True;
    GMain.Main.quit ();
    None
  }
};

value set_sensitive ginfo label b =
  try
    let mi = Hashtbl.find ginfo.button_table label in
    Gtkn.widget_set_sensitive (Gtkn.MenuItemWid mi) b
  with
  [ Not_found -> () ]
;

value set_sensitive_history minfo ginfo = do {
  set_sensitive ginfo "prev_hist" (minfo.state.bef <> []);
  set_sensitive ginfo "next_hist" (minfo.state.aft <> []);
};

value set_sensitive_palette minfo ginfo = do {
  set_sensitive ginfo "prev_pal" (minfo.pal_defs.bef <> []);
  set_sensitive ginfo "next_pal" (minfo.pal_defs.aft <> []);
};

value set_julia_button ginfo v =
  let cmi = Hashtbl.find ginfo.check_table "julia" in
  Gtkn.check_menu_item_set_active cmi v
;

value julia_button_is_set ginfo =
  let cmi = Hashtbl.find ginfo.check_table "julia" in
  Gtkn.check_menu_item_get_active cmi
;

value update_status info ginfo = do {
  let isc = info.m_info.state.cur in
  let txt = if isc.julia = None then "Mandelbrot" else "Julia" in
  let txt = sprintf "%s, zoom = 2^%d" txt (Mutil.round isc.level) in
  let txt =
    if isc.rot = Int10.zero then txt
    else sprintf "%s, rot = %s" txt (Int10.to_string isc.rot)
  in
  set_julia_button ginfo (isc.julia <> None);
  Gtkn.label_set_text ginfo.status txt;
};

value rec toggle_julia info ginfo = do {
  let minfo = info.m_info in
  let isc = minfo.state.cur in
  let is_set = julia_button_is_set ginfo in
  if is_set && isc.julia <> None || not is_set && isc.julia = None then None
  else if ginfo.check_event then do {
    return_event ginfo {ev_fun () = toggle_julia info ginfo}
  }
  else do {
    let w = ginfo.width in
    let h = ginfo.height in
    Info.push_state minfo;
    set_sensitive_history minfo ginfo;
    isc.julia :=
      match isc.julia with
      [ Some (xc, yc) -> None
      | None -> Some (isc.xc, isc.yc) ];
    update_status info ginfo;
    Info.expose_mandel info w h True True
  }
};

value rec redraw info ginfo = do {
  if ginfo.check_event then do {
    return_event ginfo {ev_fun () = redraw info ginfo}
  }
  else do {
    let w = ginfo.width in
    let h = ginfo.height in
    Info.expose_mandel info w h True True
  }
};

value rec less_iterations info ginfo = do {
  if ginfo.check_event then do {
    return_event ginfo {ev_fun () = less_iterations info ginfo}
  }
  else do {
    let minfo = info.m_info in
    let isc = minfo.state.cur in
    let w = ginfo.width in
    let h = ginfo.height in
    isc.nb_it := max 1 (isc.nb_it / 3 * 2);
    mprintf "level %g nb_it %d\n" isc.level isc.nb_it;
    Info.expose_mandel info w h False True
  }
};

value rec more_iterations info ginfo = do {
  if ginfo.check_event then do {
    return_event ginfo {ev_fun () = more_iterations info ginfo}
  }
  else do {
    let minfo = info.m_info in
    let isc = minfo.state.cur in
    let w = ginfo.width in
    let h = ginfo.height in
    isc.nb_it := (max 2 isc.nb_it) * 3 / 2;
    mprintf "level %g nb_it %d\n" isc.level isc.nb_it;
    Info.expose_mandel info w h False True
  }
};

value rec undo info ginfo = do {
  if ginfo.check_event then do {
    return_event ginfo {ev_fun () = undo info ginfo}
  }
  else do {
    mprintf "undo not yet implented\n";
    None
  }
};

value rec redo info ginfo = do {
  if ginfo.check_event then do {
    return_event ginfo {ev_fun () = redo info ginfo}
  }
  else do {
    mprintf "redo not yet implented\n";
    None
  }
};

value rec random_interesting_point info ginfo = do {
  if ginfo.check_event then do {
    return_event ginfo {ev_fun () = random_interesting_point info ginfo}
  }
  else do {
    let minfo = info.m_info in
    let isc = minfo.state.cur in
    let w = ginfo.width in
    let h = ginfo.height in
    let cnt =
      Mmisc.iter_points minfo w h 0
        (fun cl cnt _ _ ->
           if List.length cl > 6 then Some (cnt + 1, True) else None)
    in
    if cnt = 0 then None
    else do {
      let cnt = Random.int cnt in
      let (x, y, _) =
        Mmisc.iter_points minfo w h (0, 0, cnt)
          (fun cl (_, _, cnt) i j ->
             if List.length cl > 6 then
               if cnt = 0 then Some ((i, j, 0), False)
               else Some ((0, 0, cnt - 1), True)
             else None)
      in
      Info.push_state minfo;
      set_sensitive_history minfo ginfo;
      Mmisc.recenter minfo x y w h;
      if isc.exposing_state = ES_terminated then do {
        let dx = (2 * x - w) / 2 in
        let dy = (2 * y - h) / 2 in
        Info.expose_move info dx dy w h 0
      }
      else do {
        Info.expose_mandel info w h True True
      }
    }
  }
};

value rec unzoom info ginfo = do {
  if ginfo.check_event then return_event ginfo {ev_fun () = unzoom info ginfo}
  else do {
    let minfo = info.m_info in
    let isc = minfo.state.cur in
    let w = ginfo.width in
    let h = ginfo.height in
    Info.push_state minfo;
    set_sensitive_history minfo ginfo;
    isc.reduc := Mfloat.Best.twice isc.reduc;
    isc.level := isc.level -. 1.0;
    update_status info ginfo;
    Info.expose_mandel info w h True True
  }
};

value rec unzoom_10 info ginfo = do {
  if ginfo.check_event then do {
    return_event ginfo {ev_fun () = unzoom_10 info ginfo}
  }
  else do {
    let minfo = info.m_info in
    let isc = minfo.state.cur in
    let w = ginfo.width in
    let h = ginfo.height in
    Info.push_state minfo;
    set_sensitive_history minfo ginfo;
    for i = 1 to 10 do { isc.reduc := Mfloat.Best.twice isc.reduc };
    isc.level := isc.level -. 10.0;
    update_status info ginfo;
    Info.expose_mandel info w h True True
  }
};

value rec zoom info ginfo = do {
  if ginfo.check_event then return_event ginfo {ev_fun () = zoom info ginfo}
  else do {
    let minfo = info.m_info in
    let isc = minfo.state.cur in
    let w = ginfo.width in
    let h = ginfo.height in
    Info.push_state minfo;
    set_sensitive_history minfo ginfo;
    isc.reduc := Mfloat.Best.half isc.reduc;
    isc.level := isc.level +. 1.0;
    let x = w / 2 in
    let y = h / 2 in
    update_status info ginfo;
    Info.expose_optim info x y w h
  }
};

value rec zoom_10 info ginfo = do {
  if ginfo.check_event then do {
    return_event ginfo {ev_fun () = zoom_10 info ginfo}
  }
  else do {
    let minfo = info.m_info in
    let isc = minfo.state.cur in
    let w = ginfo.width in
    let h = ginfo.height in
    Info.push_state minfo;
    set_sensitive_history minfo ginfo;
    for i = 1 to 10 do { isc.reduc := Mfloat.Best.half isc.reduc };
    isc.level := isc.level +. 10.0;
    update_status info ginfo;
    Info.expose_mandel info w h True True
  }
};

value rec zoom_a_little info ginfo = do {
  if ginfo.check_event then do {
    return_event ginfo {ev_fun () = zoom_a_little info ginfo}
  }
  else do {
    let minfo = info.m_info in
    let isc = minfo.state.cur in
    let w = ginfo.width in
    let h = ginfo.height in
    Info.push_state minfo;
    set_sensitive_history minfo ginfo;
    let new_reduc =
      Mfloat.Best.mult isc.reduc
        (Mfloat.Best.of_float Info.zoom_coeff)
    in
    isc.reduc := new_reduc;
    let new_level = Mmisc.level_of_reduc minfo.extra_reduc new_reduc in
    isc.level := new_level;
    update_status info ginfo;
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
    else do {
      (* normal case *)
      Info.expose_mandel info w h True True
    }
  }
};

value random_palette very info ginfo = do {
  let minfo = info.m_info in
  let rs = minfo.random_for_colors in
  let pd =
    if very then Mmisc.make_very_random_palette_def rs
    else do {
      let c_pal = minfo.c_pal in
      let sat = max 0 (min 255 (c_pal.ini_max_s + c_pal.max_ds)) in
      let bri = max 0 (min 255 (c_pal.ini_max_v + c_pal.max_dv)) in
      Mmisc.make_random_palette_def rs sat bri
    }
  in
  Info.push_palette info pd [];
  set_sensitive_palette minfo ginfo;
(*
  refresh_pixmap_with_random_effect info ginfo;
*)
  refresh_pixmap_fast ginfo minfo None True;
(**)
  None
};

value some_palette pname info ginfo = do {
  let minfo = info.m_info in
  let pd = Palette.def_of_string pname in
  Mmisc.mprint_current_palette_def minfo pd;
  Info.push_palette info pd [];
  set_sensitive_palette minfo ginfo;
  refresh_pixmap_fast ginfo minfo None True;
  None
};

value color_permutation is_control is_right_rot info ginfo = do {
  let minfo = info.m_info in
  let inc = if is_right_rot then 1 else -1 in
  let len = Array.length minfo.c_pal.c_tab in
  let old_p = minfo.c_pal_def.permutation in
  let inc = if is_control then 10 * inc else inc in
  let new_p = (old_p + inc + len) mod len in
  let new_pd = {(minfo.c_pal_def) with permutation = new_p} in
  minfo.c_pal_def := new_pd;
  Info.install_palette info new_pd;
  refresh_pixmap_fast ginfo minfo None False;
  eprintf "color permutation %d\n" new_p;
  flush stderr;
  None
};

value change_saturation inc info ginfo = do {
  let minfo = info.m_info in
  let c_pal = minfo.c_pal in
  let g_pal = ginfo.palette in
  let ini_s = c_pal.ini_max_s in
  let old_max_ds = c_pal.max_ds in
  let old_max_s = max 0 (min 255 (ini_s + old_max_ds)) in
  let new_max_ds = if inc = 0 then 0 else old_max_ds + inc * 16 in
  let new_max_s = max 0 (min 255 (ini_s + new_max_ds)) in
  if new_max_s <> old_max_s then do {
    eprintf "saturation %d\n" new_max_s;
    flush stderr;
    c_pal.max_ds := new_max_ds;
    let ini_v = c_pal.ini_max_v in
    let max_v = max 0 (min 255 (ini_v + c_pal.max_dv)) in
    for i = 0 to Array.length g_pal - 1 do {
      let (h, s, v) = c_pal.c_tab.(i).hsv in
      let new_s = if ini_s <= 0 then new_max_s else s * new_max_s / ini_s in
      let new_v = if ini_v <= 0 then max_v else v * max_v / ini_v in
      let rgb = Palette.rgb_of_hsv (h, new_s, new_v) in
      let c = gtk_color_of_color ginfo.colormap rgb in
      g_pal.(i) := c;
      c_pal.c_tab.(i) := {rgb = rgb; hsv = (h, s, v)};
    };
    refresh_pixmap_fast ginfo minfo None False;
  }
  else ();
  None
};

value change_saturation_spreading inc info ginfo = do {
  let minfo = info.m_info in
  let old_ds = minfo.c_pal_def.delta_saturation in
  let new_ds = max 1 (min 32 (if inc = 0 then 8 else old_ds + inc)) in
  if new_ds <> old_ds then do {
    let new_pd = {(minfo.c_pal_def) with delta_saturation = new_ds} in
    minfo.c_pal_def := new_pd;
    Info.install_palette {m_info = minfo; g_info = Some (ginfo, gfun)} new_pd;
    refresh_pixmap_fast ginfo minfo None False;
    eprintf "delta saturation %d\n" new_ds;
    flush stderr;
  }
  else ();
  None
};

value change_brightness inc info ginfo = do {
  let minfo = info.m_info in
  let c_pal = minfo.c_pal in
  let g_pal = ginfo.palette in
  let ini_v = c_pal.ini_max_v in
  let old_max_dv = c_pal.max_dv in
  let old_max_v = max 0 (min 255 (ini_v + old_max_dv)) in
  let new_max_dv = if inc = 0 then 0 else old_max_dv + inc * 16 in
  let new_max_v = max 0 (min 255 (ini_v + new_max_dv)) in
  if new_max_v <> old_max_v then do {
    eprintf "brighness %d\n" new_max_v;
    flush stderr;
    c_pal.max_dv := new_max_dv;
    let ini_s = c_pal.ini_max_s in
    let max_s = max 0 (min 255 (ini_s + c_pal.max_ds)) in
    for i = 0 to Array.length g_pal - 1 do {
      let (h, s, v) = c_pal.c_tab.(i).hsv in
      let new_s = if ini_s <= 0 then max_s else s * max_s / ini_s in
      let new_v = if ini_v <= 0 then new_max_v else v * new_max_v / ini_v in
      let rgb = Palette.rgb_of_hsv (h, new_s, new_v) in
      let c = gtk_color_of_color ginfo.colormap rgb in
      g_pal.(i) := c;
      c_pal.c_tab.(i) := {rgb = rgb; hsv = (h, s, v)};
    };
    refresh_pixmap_fast ginfo minfo None False;
  }
  else ();
  None
};

value prev_palette info ginfo = do {
  let minfo = info.m_info in
  match minfo.pal_defs.bef with
  [ [] -> None
  | [pd :: pdl] -> do {
      Mmisc.mprint_current_palette_def minfo pd;
      Info.pop_palette info pd pdl;
      set_sensitive_palette minfo ginfo;
      refresh_pixmap_fast ginfo minfo None False;
      None
    } ]
};

value next_palette info ginfo = do {
  let minfo = info.m_info in
  match minfo.pal_defs.aft with
  [ [] -> None
  | [pd :: pdl] -> do {
      Mmisc.mprint_current_palette_def minfo pd;
      Info.push_palette info pd pdl;
      set_sensitive_palette minfo ginfo;
      refresh_pixmap_fast ginfo minfo None False;
      None
    } ]
};

value draw_internal_rectangle info ginfo = do {
  if ginfo.rectangle_drawn then do {
    Gtkn.window_clear ginfo.window;
    ginfo.rectangle_drawn := False;
  }
  else do {
    g_draw_internal_rectangle ginfo;
    ginfo.rectangle_drawn := True;
  };
  None
};

value erase_info info ginfo = do {
  Gtkn.window_clear ginfo.window;
  ginfo.rectangle_drawn := False;
  None
};

value history_kont info ginfo s st = do {
  g_select_cursor ginfo True;
  let minfo = info.m_info in
  let w = ginfo.width in
  let h = ginfo.height in
  set_sensitive_history minfo ginfo;
  update_status info ginfo;
  st.cur.counts_array := Array2dim.compress st.cur.counts_array;
  s.counts_array := Array2dim.uncompress s.counts_array;
  g_refresh_pixmap_fast ginfo minfo False;
(**)
  Gc.compact ();
(**)
  g_unselect_cursor ginfo;
  match s.exposing_state with
  [ ES_in_progress eip ->
      Info.expose_mandel_kont info eip w h True True
  | ES_terminated -> do {
      Info.trace_level minfo s;
      Info.finish_exposing_remaining_levels info w h
    } ]
};

value rec history_prev info ginfo = do {
  let minfo = info.m_info in
  if ginfo.check_event && minfo.state.bef <> [] then do {
    return_event ginfo {ev_fun () = history_prev info ginfo}
  }
  else do {
    let st = minfo.state in
    match st.bef with
    [ [] -> None
    | [s :: sl] -> do {
        minfo.state := {bef = sl; cur = s; aft = [st.cur :: st.aft]};
        history_kont info ginfo s st
      } ]
  }
};

value rec history_next info ginfo = do {
  let minfo = info.m_info in
  if ginfo.check_event && minfo.state.aft <> [] then do {
    return_event ginfo {ev_fun () = history_next info ginfo}
  }
  else do {
    let st = minfo.state in
    match st.aft with
    [ [] -> None
    | [s :: sl] -> do {
        minfo.state := {bef = [st.cur :: st.bef]; cur = s; aft = sl};
        history_kont info ginfo s st
      } ]
   }
};

value rec key_pressed info ginfo ev = do {
  let key = GdkEvent.Key.string ev in
  let w = ginfo.width in
  let h = ginfo.height in
  match key with
  [ "o" -> do {
      Mmisc.print_command info.m_info w h None;
      None
    }
  | "a" -> do {
      (* try to find the center of symmetry... experiments... *)
      let minfo = info.m_info in
      let isc = info.m_info.state.cur in
      let w = ginfo.width in
      let h = ginfo.height / 2 * 2 in
      let set_ij i j =
        let aij = Array2dim.get isc.counts_array i j in
        let bij = Array2dim.get isc.counts_array (w - i) (h - j) in
         match (aij, bij) with
         | (None, None) -> ()
         | (None, Some b) -> ()
         | (Some a, None) -> ()
         | (Some a, Some b) -> do {
              Array2dim.set isc.counts_array i j (Some (abs(a-b)/2*2));
              Array2dim.set isc.counts_array (w-i) (h-j) (Some (abs(a-b)/2*2));
            }
         end
      in
      for i = 0 to w do { for j = 0 to h/2-1 do { set_ij i j }; };
      let j = h/2 in
      for i = 0 to w/2 do { set_ij i j };
      g_refresh_pixmap_fast ginfo minfo False;
      None
    }
  | "0" -> do {
      if ginfo.check_event then do {
        return_event ginfo {ev_fun () = key_pressed info ginfo ev}
      }
      else do {
        let isc = info.m_info.state.cur in
        let (x, y) = (isc.xc, Mfloat.Best.of_float 0.0) in
        let (i, j) =
          Mfloat.pos_of_coord None isc.reduc (isc.xc, isc.yc) (x, y) (w, h)
        in
        isc.yc := y;
        let first_chunk =
          match isc.exposing_state with
          [ ES_in_progress eip -> eip.eip_chunk
          | ES_terminated -> 0 ]
        in
        let dx = (2 * i - w) / 2 in
        let dy = (2 * j - h) / 2 in
        Info.expose_move info dx dy w h first_chunk
      }
    }
  | _ -> None ]
};

value rec button_pressed info ginfo ev = do { 
  let minfo = info.m_info in
  let isc = minfo.state.cur in
  let x = int_of_float (GdkEvent.Button.x ev) in
  let y = int_of_float (GdkEvent.Button.y ev) in
  let w = ginfo.width in
  let h = ginfo.height in
  match GdkEvent.Button.button ev with
  [ 1 -> do {
      if ginfo.check_event then do {
        return_event ginfo {ev_fun () = button_pressed info ginfo ev}
      }
      else do {
        let new_reduc = Mfloat.Best.half isc.reduc in
        Info.push_state minfo;
        set_sensitive_history minfo ginfo;
        Mmisc.recenter minfo x y w h;
        isc.reduc := new_reduc;
        isc.level := isc.level +. 1.0;
        update_status info ginfo;
        Info.expose_optim info x y w h
      }
    }
  | 2 -> do {
      if ginfo.check_event then do {
        return_event ginfo {ev_fun () = button_pressed info ginfo ev}
      }
      else do {
        Mmisc.recenter minfo x y w h;
        let first_chunk =
          match isc.exposing_state with
          [ ES_in_progress eip -> eip.eip_chunk
          | ES_terminated -> 0 ]
        in
        let dx = (2 * x - w) / 2 in
        let dy = (2 * y - h) / 2 in
        Info.expose_move info dx dy w h first_chunk
      }
    }
  | 3 -> do {
      if ginfo.check_event then do {
        return_event ginfo {ev_fun () = button_pressed info ginfo ev}
      }
      else do {
        Info.push_state minfo;
        set_sensitive_history minfo ginfo;
        isc.reduc := Mfloat.Best.twice isc.reduc;
        isc.level := isc.level -. 1.0;
        update_status info ginfo;
        Info.expose_mandel info w h True True
      }
    }
  | _ -> None ]
};
   
value rec rotate is_shift is_down info ginfo = do {
  if ginfo.check_event then do {
    return_event ginfo {ev_fun () = rotate is_shift is_down info ginfo}
  }
  else do {
    let minfo = info.m_info in
    let isc = minfo.state.cur in
    let w = ginfo.width in
    let h = ginfo.height in
    Info.push_state minfo;
    set_sensitive_history minfo ginfo;
    let new_rot =
      let d = if is_shift then 10 else 1 in
      if is_down then Int10.add_int isc.rot d else Int10.sub_int isc.rot d
    in
    isc.rot := Info.normalize_angle new_rot;
    update_status info ginfo;
    Info.expose_mandel info w h True True
  }
};

value scroll info ginfo ev = do { 
  let modf = Gdk.Convert.modifier (GdkEvent.Scroll.state ev) in
  let is_shift = List.mem `SHIFT modf in
  let is_down = GdkEvent.Scroll.direction ev = `DOWN in
  rotate is_shift is_down info ginfo
};

value expose info ginfo ev = do {
  update_status info ginfo;
  Gtkn.window_clear ginfo.window;
  None
};

value rec configure info ginfo ev = do {
  if ginfo.check_event then do {
    return_event ginfo {ev_fun () = configure info ginfo ev}
  }
  else do {
    let w = GdkEvent.Configure.width ev in
    let h = GdkEvent.Configure.height ev in
    if w <> ginfo.width || h <> ginfo.height then do {
      let minfo = info.m_info in
      ginfo.width := w;
      ginfo.height := h;
      minfo.state := {(minfo.state) with bef = []; aft = []};
      update_status info ginfo;
      Info.expose_mandel info w h True False
    }
    else None
  }
};

value configure_root pos info ginfo ev = do {
  if ginfo.first_configure then do {
    ginfo.first_configure := False;
    let scr_w = Gdk.Screen.width () in
    let scr_h = Gdk.Screen.height () in
    let (x, y) =
      match pos with
      [ Some (x, y) -> do {
          let x = if x < 0 then scr_w - x else x in
          let y = if y < 0 then scr_h - y else y in
          (x, y)
        }
      | None -> do {
         let w = GdkEvent.Configure.width ev in
         let h = GdkEvent.Configure.height ev in
         ((scr_w - w)/2, (scr_h - h)/2)
        } ]
     in
     Gtkn.window_move ginfo.root x y;
  }
  else ();
  False
};

value loop_ev info ginfo ev =
  loop (ME_gevent ev) where rec loop ev = do {
    let r =
      match ev with
      [ ME_gevent ev ->
          let ev = ev.ev_fun () in
          Mmisc.option_map (fun ev -> ME_gevent ev) ev
      | ME_pause -> failwith "not impl pause"
      | ME_hiring_ready s -> failwith "not impl hiring ready"
      | ME_environ_to_set s -> failwith "not impl environ to set" ]
    in
    match r with
    [ Some ev -> loop ev
    | None -> () ]
  }
;

value callb f info ginfo () = do {
  loop_ev info ginfo {ev_fun () = f info ginfo}
};

value callb2 f info ginfo ev = do {
  loop_ev info ginfo {ev_fun () = f info ginfo ev};
  False
};

value rec ask_for_connection info s _ = do {
  match Compute.hire_slave_not_working info.m_info s with
  [ Some s ->
      match info.g_info with
      [ Some (ginfo, _) -> ginfo.pending_event := Some (ME_environ_to_set s)
      | None -> () ]
  | None -> () ];
  let _ : GMain.Io.id =
    GMain.Io.add_watch [`IN] (ask_for_connection info s)
      (GMain.Io.channel_of_descr s)
  in
  False
};
 
value rec mlbrot_menus info ginfo =
  let minfo = info.m_info in
  let is_julia = minfo.state.cur.julia <> None in
  let is_lazy_master = minfo.state.cur.master_lazy in
  [(I18n.transl "Application",
    [Gtkn.M_check "julia" is_julia "Julia" (Some ([], Char.code 'j'))
       (callb toggle_julia info ginfo);
     Gtkn.M_separator;
     Gtkn.M_button "en" "English" None (change_lang info ginfo "en");
     Gtkn.M_button "fr" "Français" None (change_lang info ginfo "fr");
     Gtkn.M_separator;
     Gtkn.M_button "" (I18n.transl "Pause/Resume") (Some ([], Char.code ' '))
       (callb pause info ginfo) ::
     if minfo.init_master_lazy then
       [Gtkn.M_check "lazy_master" is_lazy_master
          (I18n.transl "Lazy Master") None
          (callb lazy_master info ginfo);
        Gtkn.M_button "" (I18n.transl "Quit") (Some ([`CONTROL], Char.code 'q'))
          (callb quit info ginfo)]
     else
       [Gtkn.M_button "" (I18n.transl "Quit") (Some ([`CONTROL], Char.code 'q'))
          (callb quit info ginfo)]]);
   (I18n.transl "Display",
    [Gtkn.M_button "" (I18n.transl "Redraw") (Some ([], Char.code 'l'))
       (callb redraw info ginfo);
     Gtkn.M_separator;
     Gtkn.M_button "" (I18n.transl "Zoom Area") (Some ([], Char.code '.'))
       (callb draw_internal_rectangle info ginfo);
     Gtkn.M_button "" (I18n.transl "Erase Info") (Some ([], Char.code '/'))
       (callb erase_info info ginfo)]);
   (I18n.transl "Exploration",
    [Gtkn.M_button "" (I18n.transl "Use Mouse Buttons!") None
       (fun () -> ());
     Gtkn.M_separator;
     Gtkn.M_button "" (I18n.transl "Zoom") (Some ([], Char.code 'z'))
       (callb zoom info ginfo);
     Gtkn.M_button "" (I18n.transl "Unzoom") (Some ([], Char.code 'x'))
       (callb unzoom info ginfo);
     Gtkn.M_button "" (I18n.transl "Zoom 10 times")
       (Some ([`SHIFT], Char.code 'z')) (callb zoom_10 info ginfo);
     Gtkn.M_button "" (I18n.transl "Unzoom 10 times")
       (Some ([`SHIFT], Char.code 'x')) (callb unzoom_10 info ginfo);
     Gtkn.M_button "" (I18n.transl "Zoom A Little") (Some ([], Char.code 's'))
       (callb zoom_a_little info ginfo);
     Gtkn.M_separator;
     Gtkn.M_button "" (I18n.transl "Rotate ↺") None
       (callb (rotate False False) info ginfo);
     Gtkn.M_button "" (I18n.transl "Rotate ↻") None
       (callb (rotate False True) info ginfo);
     Gtkn.M_button "" (I18n.transl "Rotate ↺ 10°") None
       (callb (rotate True False) info ginfo);
     Gtkn.M_button "" (I18n.transl "Rotate ↻ 10°") None
       (callb (rotate True True) info ginfo);
     Gtkn.M_separator;
     Gtkn.M_button "" (I18n.transl "Random Interesting Point")
       (Some ([], Char.code ','))
       (callb random_interesting_point info ginfo)]);
   (I18n.transl "Colours",
    [Gtkn.M_button "" (I18n.transl "Random Palette")
       (Some ([], Char.code 'r'))
       (callb (random_palette False) info ginfo);
     Gtkn.M_button "" (I18n.transl "Very Random Palette")
       (Some ([`SHIFT], Char.code 'r'))
       (callb (random_palette True) info ginfo);
     Gtkn.M_separator;
     Gtkn.M_button "prev_pal" (I18n.etransl ("@(p)" ^ I18n.transl "Previous"))
       (Some ([`SHIFT], GdkKeysyms._Left))
       (callb prev_palette info ginfo);
     Gtkn.M_button "next_pal" (I18n.etransl ("@(p)" ^ I18n.transl "Next"))
       (Some ([`SHIFT], GdkKeysyms._Right))
       (callb next_palette info ginfo);
     Gtkn.M_separator;
     Gtkn.M_button "" (I18n.transl "Rainbow") None
       (callb (some_palette "rainbow") info ginfo);
     Gtkn.M_button "" (I18n.transl "Shifted Rainbow") None
       (callb (some_palette "shifted_rainbow") info ginfo);
     Gtkn.M_button "" (I18n.transl "Reverse Rainbow") None
       (callb (some_palette "reverse_rainbow") info ginfo);
     Gtkn.M_button "" (I18n.transl "Grey") None
       (callb (some_palette "grey") info ginfo);
     Gtkn.M_button "" (I18n.transl "Black & White") None
       (callb (some_palette "black_and_white") info ginfo);
     Gtkn.M_separator;
     Gtkn.M_submenu (I18n.transl "Saturation")
       [Gtkn.M_button "" (I18n.transl "Decrease")
          (Some ([], Char.code '['))
          (callb (change_saturation (-1)) info ginfo);
        Gtkn.M_button "" (I18n.transl "Increase")
          (Some ([], Char.code ']'))
          (callb (change_saturation 1) info ginfo);
        Gtkn.M_button "" (I18n.etransl ("@(s)" ^ I18n.transl "Initial"))
          (Some ([], Char.code '='))
          (callb (change_saturation 0) info ginfo);
        Gtkn.M_button "" (I18n.transl "Decrease Spreading")
          (Some ([`CONTROL], Char.code '['))
          (callb (change_saturation_spreading (-1)) info ginfo);
        Gtkn.M_button "" (I18n.transl "Increase Spreading")
          (Some ([`CONTROL], Char.code ']'))
          (callb (change_saturation_spreading 1) info ginfo);
        Gtkn.M_button "" (I18n.transl "Initial Spreading")
          (Some ([`CONTROL], Char.code '='))
          (callb (change_saturation_spreading 0) info ginfo)];
     Gtkn.M_submenu (I18n.transl "Brightness")
       [Gtkn.M_button "" (I18n.transl "Decrease")
          (Some ([], Char.code '{'))
          (callb (change_brightness (-1)) info ginfo);
        Gtkn.M_button "" (I18n.transl "Increase")
          (Some ([], Char.code '}'))
          (callb (change_brightness 1) info ginfo);
        Gtkn.M_button "" (I18n.etransl ("@(b)" ^ I18n.transl "Initial"))
          (Some ([], Char.code '-'))
          (callb (change_brightness 0) info ginfo)];
     Gtkn.M_submenu (I18n.transl "Permutation")
       [Gtkn.M_button "" (I18n.transl "Rotation" ^ " ↺")
          (Some ([], Char.code '('))
          (callb (color_permutation False False) info ginfo);
        Gtkn.M_button "" (I18n.transl "Rotation" ^ " ↻")
          (Some ([], Char.code ')'))
          (callb (color_permutation False True) info ginfo);
        Gtkn.M_button "" (I18n.transl "Rotation" ^ " ↺ x10")
          (Some ([`CONTROL], Char.code '('))
          (callb (color_permutation True False) info ginfo);
        Gtkn.M_button "" (I18n.transl "Rotation" ^ " ↻ x10")
          (Some ([`CONTROL], Char.code ')'))
          (callb (color_permutation True True) info ginfo)]]);
   (I18n.transl "Iterations",
    [Gtkn.M_button ""
       (sprintf "%s (⇒ %s)" (I18n.transl "Increase")
          (I18n.transl "less black"))
       (Some ([], Char.code 'u'))
       (callb more_iterations info ginfo);
     Gtkn.M_button ""
       (sprintf "%s (⇒ %s)" (I18n.transl "Decrease") (I18n.transl "faster"))
       (Some ([], Char.code 'd'))
       (callb less_iterations info ginfo)]);
   (I18n.transl "Way",
    [Gtkn.M_button ""
       (I18n.transl "Undo") None
       (callb undo info ginfo);
     Gtkn.M_button ""
       (I18n.transl "Redo") None
       (callb redo info ginfo)])] @
   if minfo.hist_size = 0 then []
   else
     [(I18n.transl "History",
        [Gtkn.M_button "prev_hist" (I18n.etransl (I18n.transl "Previous"))
           (Some ([], GdkKeysyms._Page_Up))
           (callb history_prev info ginfo);
         Gtkn.M_button "next_hist" (I18n.etransl (I18n.transl "Next"))
           (Some ([], GdkKeysyms._Page_Down))
           (callb history_next info ginfo)])]
and change_lang info ginfo lang () = do {
  I18n.clear_lexicon ();
  I18n.lang.val := lang;
  let menu_bar = Gtkn.menu_bar_new () in
  let n =
    Gtkn.notebook_append_page ginfo.menu_bar_noteb (Gtkn.MenuBarWid menu_bar)
      (Gtkn.LabelWid (Gtkn.label lang 0 0))
  in
  Hashtbl.clear ginfo.button_table;
  Hashtbl.clear ginfo.check_table;
  let accel_group = GtkData.AccelGroup.create () in
  Gtkn.window_add_accel_group ginfo.root accel_group;
  let menu_buttons =
    List.map
      (Gtkn.make_one_menu ginfo.button_table ginfo.check_table
         (Hashtbl.create 1) accel_group)
      (mlbrot_menus info ginfo)
  in
  List.iter (Gtkn.menu_shell_append menu_bar) menu_buttons;

  Gtkn.notebook_set_current_page ginfo.menu_bar_noteb n;
  Gtkn.notebook_remove_page ginfo.menu_bar_noteb 0;
  Gtkn.object_destroy ginfo.menu_bar;
  ginfo.menu_bar := menu_bar;
  let minfo = info.m_info in
  set_sensitive_history minfo ginfo;
  set_sensitive_palette minfo ginfo;
  set_sensitive ginfo lang False;
};

value interactive () = do {
  let (w, h, pos, rs, c_pal_def, c_pal, slave_hiring, scenario_opt) =
    Info.common_init ()
  in

  let _ : string = GtkMain.Main.init () in
  let scr_w = Gdk.Screen.width () in
  let scr_h = Gdk.Screen.height () in

  let root = Gtkn.window "mlbrot" in
  let vbox = Gtkn.box `VERTICAL in
  Gtkn.container_add root (Gtkn.BoxWid vbox);

  let hbox = Gtkn.box `HORIZONTAL in

  let menu_bar_noteb = Gtkn.notebook [] in
  Gtkn.notebook_set_show_tabs menu_bar_noteb False;
  Gtkn.container_add hbox (Gtkn.NotebookWid menu_bar_noteb);

  let menu_bar = Gtkn.menu_bar_new () in
  let _ : int =
    Gtkn.notebook_append_page menu_bar_noteb (Gtkn.MenuBarWid menu_bar)
      (Gtkn.LabelWid (Gtkn.label "" 0 0))
  in

  Gtkn.pack_start vbox (Gtkn.BoxWid hbox);
  let draw = Gtkn.drawing_area w h in
  Gtkn.container_add vbox (Gtkn.DrawingAreaWid draw);
  Gtkn.widget_realize (Gtkn.DrawingAreaWid draw);
  let bottom_bar = Gtkn.box `HORIZONTAL in
  let status = Gtkn.label "" 0 0 in
  Gtkn.pack_start bottom_bar (Gtkn.LabelWid status);
  let progress_align = Gtkn.alignment (1.0, 0.1) (0.0, 1.0) 0 1 in
  Gtkn.container_add bottom_bar (Gtkn.AlignmentWid progress_align);
  let progress_bar = Gtkn.progress_bar () in
  Gtkn.container_add progress_align (Gtkn.ProgressBarWid progress_bar);
  Gtkn.pack_start vbox (Gtkn.BoxWid bottom_bar);
  let pixmap = Gtkn.pixmap_new scr_w scr_h in
  Gtkn.set_back_pixmap (Gtkn.widget_window (Gtkn.DrawingAreaWid draw)) pixmap;

  let pixmap_dr = Gtkn.PixmapDr pixmap in
  let window_dr =
    Gtkn.WindowDr (Gtkn.widget_window (Gtkn.DrawingAreaWid draw))
  in
  let window_gc = Gtkn.create_gc window_dr in
  let pixmap_gc = Gtkn.create_gc pixmap_dr in
(*
  let window_dr = (Gtkn.widget_window (Gtkn.DrawingAreaWid window)) in
  let pixmap_dr = pixmap in
*)
  let window = Gtkn.widget_window (Gtkn.DrawingAreaWid draw) in

  let minfo = Mmisc.make_minfo rs scr_w scr_h c_pal_def c_pal slave_hiring in
  let colormap = Gdk.Color.get_system_colormap () in
  let black = Gdk.Color.alloc colormap `BLACK in
  let white = Gdk.Color.alloc colormap `WHITE in
  let palette =
    Array.map (fun p -> gtk_color_of_color colormap p.rgb) c_pal
  in
  let button_table = Hashtbl.create 1 in
  let check_table = Hashtbl.create 1 in
  let ginfo =
    {root = root; window = window; window_gc = window_gc;
     pixmap = pixmap; pixmap_gc = pixmap_gc;
     menu_bar_noteb = menu_bar_noteb; menu_bar = menu_bar;
     width = w; height = h; colormap = colormap;
     black = black; white = white; palette = palette;
     status = status; progress_bar = progress_bar;
     button_table = button_table; check_table = check_table;
     cursor_watch = Gdk.Cursor.create `WATCH;
     cursor_cross = Gdk.Cursor.create `CROSSHAIR;
     cursor_pause = Gdk.Cursor.create `CENTER_PTR;
     cursor_default = Gdk.Cursor.create `TOP_LEFT_ARROW;
     check_event = False; pending_event = None;
     rectangle_drawn = False; first_configure = True;
     no_main_loop = False}
  in
  let info = {m_info = minfo; g_info = Some (ginfo, gfun)} in

  g_select_color ginfo (Some 0);
  Gtkn.draw_rectangle pixmap_dr pixmap_gc True 0 0 scr_w scr_h;

  match slave_hiring with
  [ Some (s, _) ->
      let _ : GMain.Io.id =
        GMain.Io.add_watch [`IN] (ask_for_connection info s)
          (GMain.Io.channel_of_descr s)
      in
      ()
  | None -> () ];

  Gtkn.signal_connect root GtkBase.Object.S.destroy (callb quit info ginfo);

  let accel_group = GtkData.AccelGroup.create () in
  Gtkn.window_add_accel_group root accel_group;

  let menu_buttons =
    List.map
      (Gtkn.make_one_menu button_table check_table (Hashtbl.create 1)
         accel_group)
      (mlbrot_menus info ginfo)
  in
  List.iter (Gtkn.menu_shell_append menu_bar) menu_buttons;

  set_sensitive_palette minfo ginfo;
  set_sensitive_history minfo ginfo;
  let lang =
    let len = String.length I18n.lang.val in
    if len >= 2 then String.sub I18n.lang.val 0 2 else "en"
  in
  set_sensitive ginfo lang False;

  Gtkn.signal_connect root GtkBase.Widget.Signals.Event.configure
    (configure_root pos info ginfo);
  Gtkn.signal_connect root GtkBase.Widget.Signals.Event.key_press
    (callb2 key_pressed info ginfo);

  Gtkn.signal_connect draw GtkBase.Widget.Signals.Event.expose
    (callb2 expose info ginfo);
  Gtkn.signal_connect draw GtkBase.Widget.Signals.Event.configure
    (callb2 configure info ginfo);
  Gtkn.signal_connect draw GtkBase.Widget.Signals.Event.button_press
    (callb2 button_pressed info ginfo);
  Gtkn.signal_connect draw GtkBase.Widget.Signals.Event.scroll
    (callb2 scroll info ginfo);
  Gtkn.add_events (Gtkn.DrawingAreaWid draw) [`EXPOSURE; `BUTTON_PRESS];

  update_status info ginfo;

  Gtkn.widget_show (Gtkn.WindowWid root);

  (* I had to add a small temporisation here because sometimes the
     drawing stops in the very beginning, I don't know why; perhaps
     a problem of gtk somewhere, or a problem of using gtk... I
     should investigate one day *)
  ignore (Unix.select [] [] [] 0.1);

  match Info.expose_mandel info w h True True with
  [ Some ev -> loop_ev info ginfo ev
  | None -> () ];
  if ginfo.no_main_loop then () else GMain.Main.main ();

  let wid_w = ginfo.width in
  let wid_h = ginfo.height in
  Mmisc.print_command minfo wid_w wid_h pos
};
