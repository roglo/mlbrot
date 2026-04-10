(* $Id: info.mli,v 1.8 2014/01/25 10:52:45 deraugla Exp $ *)

open Info_def;
open Mdef;

value trace_level : m_info -> state -> unit;

value check_user_intervention : info 'a 'b -> option (mlbrot_event 'b);

value set_auto_float : m_info -> state -> unit;
value set_num : m_info -> state -> Mcomm.num_type -> unit;

value finish_exposing_remaining_levels :
  info 'a 'b -> int -> int -> option 'b;
value expose_mandel_kont :
  info 'a 'b -> exposing_in_progress -> int -> int -> bool -> bool ->
    option 'b;
value expose_mandel : info 'a 'b -> int -> int -> bool -> bool -> option 'b;
value expose_fast :
  info 'a 'b -> state -> int -> int -> (int -> int) -> (int -> int) ->
    option 'b;

value expose_optim : info 'a 'b -> int -> int -> int -> int -> option 'b;
value expose_move :
  info 'a 'b -> int -> int -> int -> int -> int -> option 'b;
value first_chunk : int;

value push_state : m_info -> unit;

value get_pixel_color_fun :
  m_info -> int -> int ->
    (int -> int -> int * int -> (int * int * int) * int * int);

value zoom_coeff : float;

value normalize_angle : Int10.t -> Int10.t;

value install_palette : info 'a 'b -> Palette_def.t -> unit;
value push_palette :
  info 'a 'b -> Palette_def.t -> list Palette_def.t -> unit;
value pop_palette :
  info 'a 'b -> Palette_def.t -> list Palette_def.t -> unit;

type scenario = 'abstract;

value apply_zoom :
  info 'a 'b -> int -> int -> option float -> bool -> int -> option 'b;
value apply_scenario :
  info 'a 'b -> int -> int -> list scenario -> int -> option 'b;

value common_init :
  unit ->
    (int * int * option (int * int) * Random.State.t * Palette_def.t *
     array Palette_def.color * option (Unix.file_descr * bool) *
     option (list scenario));

value batch : unit -> unit;
value display_interesting_points : unit -> unit;
