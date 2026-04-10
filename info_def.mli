(* $Id: info_def.mli,v 1.18 2010/12/05 09:43:16 deraugla Exp $ *)

open Mdef;

type info 'a 'b =
  { m_info : m_info;
    g_info : option ('a * xfun 'a 'b)}
and xfun 'a 'b =
  { update_point_in_pixmap :
      'a -> int -> rect -> (int * int) -> option int -> unit;
    g_copy_area :
      'a -> m_info -> (int * int * int * int) -> (int * int) -> unit;
    g_get_pending_event : info 'a 'b -> 'a -> option (mlbrot_event 'b);
    g_make_area_visible :
      'a -> exposing_in_progress -> float -> (int * int * int * int) -> unit;
    g_make_window_visible : 'a -> unit;
    g_pending_events : 'a -> bool;
    g_refresh_pixmap_fast : 'a -> m_info -> bool -> unit;
    g_select_cursor : 'a -> bool -> unit;
    g_select_cursor_pause : 'a -> unit;
    g_set_palette : 'a -> array Palette_def.color -> unit;
    g_unselect_cursor : 'a -> unit;
    g_widget_size : 'a -> (int * int) }
and mlbrot_event 'a =
  [ ME_gevent of 'a
  | ME_pause
  | ME_hiring_ready of Unix.file_descr
  | ME_environ_to_set of Unix.file_descr ]
;
