(* $Id: mfloat.mli,v 1.30 2014/02/16 10:09:14 deraugla Exp $ *)

open Mcomm;

IFDEF MPFR THEN
  type best_float = Mpfr.t;
ELSIFDEF MPZ THEN
  type best_float = mpzp_t
(*
  and mpzp_t = 'abstract
*)
  and mpzp_t =
    { mpz : Mpz.t;
      prec : int }
  ;
(**)
ELSE
  type best_float = float;
END;

type mandelbrot_fun 't = 't -> 't -> int -> option ('t * 't) -> result 't;

module type Mfloat =
  sig
    type t = 'abstract;
    value string_of_num_type : unit -> string;
    value neg : t -> t;
    value twice : t -> t;
    value half : t -> t;
    value add : t -> t -> t;
    value sub : t -> t -> t;
    value mult : t -> t -> t;
    value mult_int : t -> int -> t;
    value sqr : t -> t;
    value div : t -> t -> t;
    value exp : t -> t;
    value cos : t -> t;
    value sin : t -> t;
    value log : t -> float;
    value to_string : t -> string;
    value of_string : int -> string -> t;
    value to_float : t -> float;
    value of_float : float -> t;
    value of_best_float : best_float -> t;
    value get_prec : t -> int;
    value mandelbrot_point : mandelbrot_fun t;
    value mandelbrot_point3 : mandelbrot_fun t;
    value mandelbrot_point_m : int -> mandelbrot_fun t;
    value lambda_point : mandelbrot_fun t;
    value set_num : num_type -> unit;
    value get_num : unit -> num_type;
    value set_default_prec : int -> unit;
    value with_prec : int -> t -> t;
    value serialize : t -> Mcomm.serialized;
    value deserialize : Mcomm.serialized -> t;
  end
;

module M : Mfloat;
module Best : Mfloat with type t = best_float;

value trigo_of_rot : Int10.t -> option (Best.t * Best.t);

value coord_of_pos :
  option (Best.t * Best.t) -> Best.t -> (Best.t * Best.t) ->
    (int * int) -> (int * int) -> (Best.t * Best.t)
;

value pos_of_coord :
  option (Best.t * Best.t) -> Best.t -> (Best.t * Best.t) ->
    (Best.t * Best.t) -> (int * int) -> (int * int)
;

value compute_point_with_fun :
  mandelbrot_fun M.t -> Mcomm.function_context Best.t ->
    int -> int -> option (M.t * M.t) -> Mcomm.result M.t;

value fp : ref (option (float -> float -> unit));
