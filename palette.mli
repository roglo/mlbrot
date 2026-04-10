(* $Id: palette.mli,v 1.4 2010/11/11 23:20:10 deraugla Exp $ *)

value def_of_string : string -> Palette_def.t;
value def_to_string : Palette_def.t -> string;
value of_palette_def : int -> int -> Palette_def.t -> array Palette_def.color;
value make : Palette_def.t -> array Palette_def.color;

value rgb_of_hsv : (int * int * int) -> (int * int * int);
