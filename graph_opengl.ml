(* $Id: graph_opengl.ml,v 1.4 2010/12/07 09:02:23 deraugla Exp $ *)

open Info_def;
open Mdef;
open Mprintf;
open Palette_def;
open Printf;

type event =
  [ EvExpose of int and int
  | EvKey of char and int and int
  | EvMouse of Glut.mouse_button and Glut.mouse_button_state and int and int ]
;

type ginfo =
  { width : int;
    height : int;
    first_expose : mutable bool;
    g_pal : mutable array (char * char * char);
    check_event : mutable bool;
    pending_event : mutable option event }
;

exception Event;

value g_select_color ginfo count = do {
  let (r, g, b) =
    match count with
    [ Some count -> ginfo.g_pal.(count mod Array.length ginfo.g_pal)
    | None -> ('\000', '\000', '\000') ]
  in
  GL.glColor3c r g b
};

value coord x w = (2. *. float x -. float w) /. float w;

value update_point_in_pixmap ginfo chunk area (i, j) count = do {
  g_select_color ginfo count;
  if chunk = 1 then do {
    let w = ginfo.width in
    let h = ginfo.height in
    GL.glBegin GL.GL_POINTS;
    GL.glVertex2 (coord i w) (coord (h - j) h);
    GL.glEnd ();
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
      let w = ginfo.width in
      let h = ginfo.height in
      GL.glBegin GL.GL_QUADS;
      GL.glVertex2 (coord x1 w) (coord (h - y1) h);
      GL.glVertex2 (coord x1 w) (coord (h - y2) h);
      GL.glVertex2 (coord x2 w) (coord (h - y2) h);
      GL.glVertex2 (coord x2 w) (coord (h - y1) h);
      GL.glEnd ();
    }
    else ()
  }
};

value g_select_cursor ginfo into_win = do {
  Glut.glutSetCursor
    (if into_win then Glut.GLUT_CURSOR_WAIT else Glut.GLUT_CURSOR_CROSSHAIR);
};

value g_unselect_cursor ginfo = do {
  Glut.glutSetCursor Glut.GLUT_CURSOR_INHERIT;
};

value return_event ginfo ev_opt = do {
  ginfo.pending_event := ev_opt;
  raise Event
};

value normal_key2 info ginfo key x y = do {
  let ev = EvKey key x y in
  let minfo = info.m_info in
  let isc = minfo.state.cur in
  let w = ginfo.width in
  let h = ginfo.height in
  match key with
  [ '\017' -> do {
      (* ctrl-q *)
      raise Exit
    }
  | 'u' -> do {
      if ginfo.check_event then return_event ginfo (Some ev)
      else do {
        isc.nb_it := (max 2 isc.nb_it) * 3 / 2;
        mprintf "level %g nb_it %d\n" isc.level isc.nb_it;
        Info.expose_mandel info w h False True
      }
    }
  | 'x' -> do {
      if ginfo.check_event then return_event ginfo (Some ev)
      else do {
        Info.push_state minfo;
        isc.reduc := Mfloat.Best.twice isc.reduc;
        isc.level := isc.level -. 1.0;
        Info.expose_mandel info w h True True
      }
    }
  | 'z' -> do {
      if ginfo.check_event then return_event ginfo (Some ev)
      else do {
        Info.push_state minfo;
        isc.reduc := Mfloat.Best.half isc.reduc;
        isc.level := isc.level +. 1.0;
        let x = w / 2 in
        let y = h / 2 in
        Info.expose_optim info x y w h
      }
    }
  | _ -> None ]
};

value mouse_func2 info ginfo button state x y = do {
  let ev = EvMouse button state x y in
  let minfo = info.m_info in
  let isc = minfo.state.cur in
  let w = ginfo.width in
  let h = ginfo.height in
  match state with
  [ Glut.GLUT_DOWN -> do {
      match button with
      [ Glut.GLUT_LEFT_BUTTON -> do {
          if ginfo.check_event then return_event ginfo (Some ev)
          else do {
            let new_reduc = Mfloat.Best.half isc.reduc in
            Info.push_state minfo;
            Mmisc.recenter minfo x y w h;
            isc.reduc := new_reduc;
            isc.level := isc.level +. 1.0;
            Info.expose_optim info x y w h;
          }
        }
      | Glut.GLUT_MIDDLE_BUTTON -> do {
          if ginfo.check_event then return_event ginfo (Some ev)
          else do {
            Mmisc.recenter minfo x y w h;
            if isc.exposing_state = ES_terminated then
              let dx = (2 * x - w) / 2 in
              let dy = (2 * y - h) / 2 in
              Info.expose_move info dx dy w h 0
            else
              Info.expose_mandel info w h True True
          }
        }
      | Glut.GLUT_RIGHT_BUTTON -> do {
          None
        }
      | _ -> None ];
    }
  | Glut.GLUT_UP -> None ]
};

value g_pending_events ginfo = do {
  let idle () = return_event ginfo None in
  ginfo.check_event := True;
  Glut.glutIdleFunc idle;
  try
    let _ = Glut.glutMainLoop () in
    failwith "internal error: main loop in pending_event"
  with
  [ Event -> () ];
  Glut.glutRemoveIdleFunc ();
  ginfo.check_event := False;
  ginfo.pending_event <> None
};

value g_make_area_visible ginfo eip frac (x, y, w, h) = do {
  Glut.glutSwapBuffers ();
};

value refresh_pixel minfo ginfo i j = do {
  let ica = minfo.state.cur.counts_array in
  let w = ginfo.width in
  let h = ginfo.height in
  if i < Array2dim.dim1 ica && j < Array2dim.dim2 ica then do {
    let count = Array2dim.get ica i j in
    g_select_color ginfo count;
    GL.glBegin GL.GL_POINTS;
    GL.glVertex2 (coord i w) (coord (h - j) h);
    GL.glEnd ();
  }
  else ()
};

value refresh_pixmap_fast ginfo minfo rect_opt show_fast = do {
  let (x, y, w, h) =
    match rect_opt with
    [ Some r -> r
    | None -> (0, 0, ginfo.width, ginfo.height) ]
  in
  loop x y where rec loop i j =
    if j = y + h then
      if show_fast then () else Glut.glutSwapBuffers ()
    else if i = x + w then do {
      if True || show_fast then Glut.glutSwapBuffers () else ();
      loop x (j + 1)
    }
    else do {
      refresh_pixel minfo ginfo i j;
      loop (i + 1) j
    };
};

value g_refresh_pixmap_fast ginfo minfo show_fast = do {
  if minfo.state.cur.exposing_state = ES_terminated then ()
  else Glut.glutSetCursor Glut.GLUT_CURSOR_WAIT;
  refresh_pixmap_fast ginfo minfo None show_fast;
  if minfo.state.cur.exposing_state = ES_terminated then ()
  else Glut.glutSetCursor Glut.GLUT_CURSOR_INHERIT;
};

value g_copy_area ginfo minfo (x, y, w, h) (x_dest, y_dest) = do {
  (* likely a much faster version if there is a 'copy_area' function
     somewhere in OpenGL: compare with -rt version! *)
  let rect_opt = Some (x_dest, y_dest, w, h) in
  refresh_pixmap_fast ginfo minfo rect_opt False;
};

value g_make_window_visible ginfo = do {
  Glut.glutSwapBuffers ();
};

value g_get_pending_event info ginfo = do {
  match ginfo.pending_event with
  [ Some ev -> do {
      ginfo.pending_event := None;
      Some (ME_gevent ev)
    }
  | None -> assert False ];
};

value gfun =
  {update_point_in_pixmap = update_point_in_pixmap;
   g_copy_area = g_copy_area;
   g_get_pending_event = g_get_pending_event;
   g_make_area_visible = g_make_area_visible;
   g_make_window_visible = g_make_window_visible;
   g_pending_events = g_pending_events;
   g_refresh_pixmap_fast = g_refresh_pixmap_fast;
   g_select_cursor = g_select_cursor;
   g_select_cursor_pause = fun [];
   g_set_palette = fun [];
   g_unselect_cursor = g_unselect_cursor;
   g_widget_size = fun []}
;

value display2 info ginfo w h = do {
  GL.glClear [];
  if ginfo.first_expose then do {
    ginfo.first_expose := False;
    info.m_info.state := {(info.m_info.state) with bef = []; aft = []};
    Info.expose_mandel info w h True False
  }
  else do {
    g_refresh_pixmap_fast ginfo info.m_info True;
    Glut.glutSwapBuffers ();
    None
  }
};

value loop_ev info ginfo =
  loop where rec loop ev = do {
    let r =
      match ev with
      [ EvExpose w h ->
          display2 info ginfo w h
      | EvKey key x y ->
          normal_key2 info ginfo key x y
      | EvMouse button state x y ->
          mouse_func2 info ginfo button state x y ]
    in
    match r with
    [ Some ev -> loop ev
    | None -> () ]
  }
;

value display info ginfo w h () =
  loop_ev info ginfo (EvExpose w h)
;

value normal_key info ginfo ~{key} ~{x} ~{y} =
  loop_ev info ginfo (EvKey key x y)
;

value mouse_func info ginfo ~{button} ~{state} ~{x} ~{y} =
  loop_ev info ginfo (EvMouse button state x y)
;

value color_char_of_color {rgb = (r, g, b)} =
  (Char.chr r, Char.chr g, Char.chr b)
;

value interactive () = do {
  let (w, h, pos, rs, c_pal_def, c_pal, slave_hiring, scenario_opt) =
    Info.common_init ()
  in
  let (scr_w, scr_h) = (w, h) in
  let g_pal = Array.map color_char_of_color c_pal in
  let ginfo =
    {width = scr_w; height = scr_h; first_expose = True; g_pal = g_pal;
     check_event = False; pending_event = None}
  in
  let minfo = Mmisc.make_minfo rs scr_w scr_h c_pal_def c_pal slave_hiring in
  let info = {m_info = minfo; g_info = Some (ginfo, gfun)} in
  let _ : array string = Glut.glutInit Sys.argv in
  Glut.glutInitWindowSize w h;
  let _ : Glut.window_id = Glut.glutCreateWindow "mlbrot" in
  Glut.glutDisplayFunc (display info ginfo w h);
  Glut.glutKeyboardFunc (normal_key info ginfo);
  Glut.glutMouseFunc (mouse_func info ginfo);
  try Glut.glutMainLoop () with [ Exit -> () ];
  let wid_w = w in
  let wid_h = h in
  let wid_x = 0 in
  let wid_y = 0 in
  Mmisc.print_command minfo wid_w wid_h (Some (wid_x, wid_y));
};
