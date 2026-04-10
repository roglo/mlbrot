(* $Id: int10.mli,v 1.2 2009/05/03 18:49:07 deraugla Exp $ *)

type t = 'abstract;

value zero : t;

value abs : t -> t;
value add : t -> t -> t;
value sub : t -> t -> t;
value add_int : t -> int -> t;
value sub_int : t -> int -> t;
value mul_int : t -> int -> t;
value div_int : t -> int -> t;
value mod_int : t -> int -> t;
value truncate : t -> int;
value of_int : int -> t;
value of_float : float -> t;
value of_string : string -> t;
value to_float : t -> float;
value to_string : t -> string;
