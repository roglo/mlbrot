(* $Id: mpfr.mli,v 1.10 2009/04/04 17:05:15 deraugla Exp $ *)

open Mcomm;

type t = 'abstract;

external neg : t -> t = "ml_mpfr_neg";
external add : t -> t -> t = "ml_mpfr_add";
external sub : t -> t -> t = "ml_mpfr_sub";
external mul : t -> t -> t = "ml_mpfr_mul";
external mul_si : t -> int -> t = "ml_mpfr_mul_si";
external div : t -> t -> t = "ml_mpfr_div";
external div_si : t -> int -> t = "ml_mpfr_div_si";
external sqr : t -> t = "ml_mpfr_sqr";
external cos : t -> t = "ml_mpfr_cos";
external sin : t -> t = "ml_mpfr_sin";
external to_string : int -> t -> (string * int) = "ml_mpfr_to_string";
external of_string : int -> string -> t = "ml_mpfr_of_string";
external to_float : t -> float = "ml_mpfr_to_float";
external of_float : float -> t = "ml_mpfr_of_float";
external set_default_prec : int -> unit = "ml_mpfr_set_default_prec";
external get_prec : t -> int = "ml_mpfr_get_prec";
external with_prec : int -> t -> t = "ml_mpfr_with_prec";

external mandelbrot_point :
  t -> t -> int -> option (t * t) -> result t =
    "ml_mpfr_mandelbrot_point";
external mandelbrot_point3 :
  t -> t -> int -> option (t * t) -> result t =
    "ml_mpfr_mandelbrot_point3";
external mandelbrot_point_m :
  int -> t -> t -> int -> option (t * t) -> result t =
    "ml_mpfr_mandelbrot_point_m";
external lambda_point :
  t -> t -> int -> option (t * t) -> result t =
    "ml_mpfr_lambda_point";
