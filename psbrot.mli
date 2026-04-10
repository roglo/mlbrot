(* $Id: psbrot.mli,v 1.1 2009/08/08 16:10:51 deraugla Exp $ *)

type info =
  { w : mutable int;
    h : mutable int;
    xc : mutable float;
    yc : mutable float;
    zoom : mutable float;
    max_pq : mutable int;
    show_pq : mutable int;
    verbose : mutable bool;
    c_black : Rt.color;
    c_white : Rt.color;
    c_red : Rt.color;
    c_green : Rt.color;
    c_blue : Rt.color;
    font : Rt.font }
;

value xy_of_ij : info -> int -> int -> (float * float);
value draw_circle : Rt.drawable -> info -> float -> float -> float -> unit;

value draw_pseudobrot : Rt.drawable -> info -> unit;
