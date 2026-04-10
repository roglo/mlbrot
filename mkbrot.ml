(* $Id: mkbrot.ml,v 1.54 2009/08/08 17:44:32 deraugla Exp $ *)

open Rt;
open Psbrot;
open Printf;

value not_impl txt x =
  let desc =
    if Obj.is_block (Obj.repr x) then
      "tag = " ^ string_of_int (Obj.tag (Obj.repr x))
    else "int_val = " ^ string_of_int (Obj.magic x)
  in
  failwith ("mkbrot: " ^ txt ^ ", not impl: " ^ desc)
;

value draw_in_pixmap pixmap info = do {
  let drw = PixmapDr pixmap in
  rt_select_color info.c_white;
  rt_fill_rectangle drw (0, 0, info.w, info.h);
  draw_pseudobrot drw info;
};

value vt_erase_from_cursor = "\027[J";

value action_mkbrot xa pixmap info wid ev =
  match ev with
  [ RawEvButtonPress x y x_root y_root mbut ->
      match mbut.item with
      [ 1 | 2 | 3 -> do {
          if mbut.item <> 3 then do {
            let (xc, yc) = xy_of_ij info x y in
            info.xc := xc;
            info.yc := yc;
          }
          else ();
          if mbut.item = 1 then info.zoom := 2. *. info.zoom
          else if mbut.item = 3 then info.zoom := 0.5 *. info.zoom
          else ();
          draw_in_pixmap pixmap info;
          rt_clear_widget wid;
        }
      | _ -> do {
          eprintf "button press %d %d %d %d %d\n" x y x_root y_root mbut.item;
          flush stderr;
        } ]
  | RawEvConfigureNotify (ow, oh, ob) (nw, nh, nb) -> do {
      if nw <> ow || nh <> oh then do {
        info.w := nw;
        info.h := nh;
        draw_in_pixmap pixmap info;
        rt_clear_widget wid;
      }
      else ();
    }
  | RawEvEnterNotify _ _ _ _ -> ()
  | RawEvExpose x y w h -> rt_clear_widget wid
  | RawEvFocusIn -> ()
  | RawEvFocusOut -> ()
  | RawEvKeyPress ksym ->
      match ksym.item with
      [ K_Ascii 'd' -> do {
          if info.max_pq > 1 then do {
            info.max_pq := info.max_pq - 1;
            eprintf "%d%s\r" info.max_pq vt_erase_from_cursor;
            flush stderr;
            draw_in_pixmap pixmap info;
            rt_clear_widget wid
          }
          else ()
        }
      | K_Ascii 'p' -> do {
          info.show_pq := (info.show_pq + 1) mod 3;
          draw_in_pixmap pixmap info;
          rt_clear_widget wid
        }
      | K_Ascii 'q' -> if ksym.control then rt_stop_main_loop xa else ()
      | K_Ascii 'r' -> do {
          info.zoom := 1. /. 1.05 *. info.zoom;
          draw_in_pixmap pixmap info;
          rt_clear_widget wid;
        }
      | K_Ascii 's' -> do {
          info.zoom := 1.05 *. info.zoom;
          draw_in_pixmap pixmap info;
          rt_clear_widget wid;
        }
      | K_Ascii 't' -> do {
          printf "w %d h %d\n" info.w info.h;
          printf "xc %g yc %g\n" info.xc info.yc;
          printf "zoom %g\n" info.zoom;
          printf "max_pq %d\n" info.max_pq;
          flush stdout;
        }
      | K_Ascii 'u' ->  do {
          info.max_pq := info.max_pq + 1;
          eprintf "%d\r" info.max_pq;
          flush stderr;
          draw_in_pixmap pixmap info;
          rt_clear_widget wid
        }
      | K_Ascii 'v' -> do {
          info.verbose := not info.verbose;
          printf "======\n%s\n" (if info.verbose then "verbose" else "quiet");
          flush stdout;
          draw_in_pixmap pixmap info;
          rt_clear_widget wid
        }
      | K_Ascii 'x' -> do {
          info.zoom := 0.5 *. info.zoom;
          draw_in_pixmap pixmap info;
          rt_clear_widget wid;
        }
      | K_Ascii 'z' -> do {
          info.zoom := 2. *. info.zoom;
          draw_in_pixmap pixmap info;
          rt_clear_widget wid;
        }
      | K_Ascii '.' -> do {
          let drw = WidgetDr wid in
          let w = info.w in
          let h = info.h in
          let r = 5 in
          rt_select_color info.c_blue;
          rt_draw_rectangle drw (w/4, h/4, w/2, h/2);
          rt_draw_arc drw (w/2-r, h/2-r, 2*r, 2*r, 0, 360*64);
        }
      | K_Control_L | K_Control_R -> ()
      | _ -> do {
          eprintf "key press\n";
          flush stderr
        } ]
  | RawEvLeaveNotify -> ()
  | ev -> not_impl "action_mkbrot ev" ev ]
;

value arg_xc = ref 0.0;
value arg_yc = ref 0.0;
value arg_zoom = ref 1.0;
value arg_max_pq = ref 30;
value arg_show_pq = ref 1;

value usage = "Usage: " ^ Sys.argv.(0) ^ " [option]... <addr>\n\nOptions:";

value speclist =
  Arg.align
    [("-center",
      Arg.Tuple [Arg.Set_float arg_xc; Arg.Set_float arg_yc],
      sprintf "<xc><yc> Set initial center position (default: %g %g)"
        arg_xc.val arg_yc.val);
     ("-max_pq", Arg.Set_int arg_max_pq,
      sprintf "<int> max p & q of cardioid bulbs (default: %d)"
        arg_max_pq.val);
     ("-show_pq", Arg.Set_int arg_show_pq,
      sprintf "<int> 0=no show; 1=show pq; 2=show level (default: %d)"
        arg_show_pq.val);
     ("-zoom", Arg.Set_float arg_zoom,
      sprintf "<flo> Set initial zoom (default: %g)" arg_zoom.val)]
;

value anonfun s = raise (Arg.Bad ("don't know what to do with " ^ s));

value main () = do {
  Arg.parse speclist anonfun usage;
  let xd = rt_initialize "" in
  let xa = rt_args [xd] in
  let w = 768 in
  let h = 432 in
  let pixmap = rt_create_pixmap xd (screen_width xd) (screen_height xd) in
  let c_black = rt_black_color xd in
  let c_white = rt_white_color xd in
  let c_red = rt_create_color xd (255, 0, 0) in
  let c_green = rt_create_color xd (0, 255, 0) in
  let c_blue = rt_create_color xd (0, 0, 255) in
  let font = rt_load_query_font xd "fixed" in
  rt_select_font font;
  let info =
    {w = w; h = h; xc = arg_xc.val; yc = arg_yc.val; zoom = arg_zoom.val;
     max_pq = arg_max_pq.val; show_pq = arg_show_pq.val; verbose = False;
     c_black = c_black; c_white = c_white; c_red = c_red; c_green = c_green;
     c_blue = c_blue; font = font}
  in
  let wid =
    let name = "mkbrot" in
    rt_create_widget xd name name AutoPosition
      (Some (fun _ -> rt_stop_main_loop xa))
      (raw_desc []
         (w, h, 0,
          [SelExposure; SelKeyPress; SelStructureNotify; SelButtonPress])
         (action_mkbrot xa pixmap info))
  in
  draw_in_pixmap pixmap info;
  rt_change_background wid (PixmapPn pixmap);
  rt_map_widget wid;
  printf "commands: u/d, z/x, s/r, p, t, ctl-q and mouse buttons.\n";
  flush stdout;
  rt_main_loop xa;
  printf "%s -center %.15g %.15g -zoom %g -max_pq %d -show_pq %d\n"
    Sys.argv.(0) info.xc info.yc info.zoom info.max_pq info.show_pq;
  flush stdout;
};

main ();
