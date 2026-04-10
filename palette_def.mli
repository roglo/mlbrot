(* $Id: palette_def.mli,v 1.2 2010/10/18 09:41:10 deraugla Exp $ *)

type t =
  { hues : list int;
    permutation : int;
    saturation : int;
    delta_saturation : int;
    brightness : int;
    delta_brightness : int;
    color_algo : color_algo }
and color_algo =
  [ CA_start_with_white
  | CA_start_with_black
  | CA_no_gradation ]
;

type color =
  { rgb : (int * int * int);
    hsv : (int * int * int) }
;
