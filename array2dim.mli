(* $Id: array2dim.mli,v 1.1 2009/02/10 18:49:29 deraugla Exp $ *)

type t 'a = 'abstract;

value create : int -> int -> 'a -> t 'a;
value init : int -> int -> (int -> int -> 'a) -> t 'a;
value get : t 'a -> int -> int -> 'a;
value set : t 'a -> int -> int -> 'a -> unit;
value blit : t 'a -> t 'a -> unit;
value compress : t 'a -> t 'a;
value uncompress : t 'a -> t 'a;
value dim1 : t 'a -> int;
value dim2 : t 'a -> int;
