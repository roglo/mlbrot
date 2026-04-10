(* $Id: mpz.ml,v 1.3 2016/10/31 14:33:59 deraugla Exp $ *)

open Mcomm;

type t = 'abstract;
type randstate = 'abstract;

external abs : t -> t = "ml_mpz_abs";
external neg : t -> t = "ml_mpz_neg";
external add : t -> t -> t = "ml_mpz_add";
external add_ui : t -> int -> t = "ml_mpz_add_ui";
external sub : t -> t -> t = "ml_mpz_sub";
external sub_ui : t -> int -> t = "ml_mpz_sub_ui";
external mul : t -> t -> t = "ml_mpz_mul";
external mul_si : t -> int -> t = "ml_mpz_mul_si";
external mul_2exp : t -> int -> t = "ml_mpz_mul_2exp";
external div_q : t -> t -> t = "ml_mpz_div_q";
external div_q_ui : t -> int -> t = "ml_mpz_div_q_ui";
external div_r : t -> t -> t = "ml_mpz_div_r";
external div_r_ui : t -> int -> int = "ml_mpz_div_r_ui";
external div_qr : t -> t -> (t * t) = "ml_mpz_div_qr";
external div_qr_ui : t -> int -> (t * int) = "ml_mpz_div_qr_ui";
external div_q_2exp : t -> int -> t = "ml_mpz_div_q_2exp";
external div_r_2exp : t -> int -> t = "ml_mpz_div_r_2exp";
external addmul : t -> t -> t -> t = "ml_mpz_addmul";
external addmul_ui : t -> t -> int -> t = "ml_mpz_addmul_ui";
external submul : t -> t -> t -> t = "ml_mpz_submul";
external submul_ui : t -> t -> int -> t = "ml_mpz_submul_ui";
external pow_ui : t -> int -> t = "ml_mpz_pow_ui";
external ui_pow_ui : int -> int -> t = "ml_mpz_ui_pow_ui";
external pow_mod : t -> t -> t -> t = "ml_mpz_pow_mod";
external pow_mod_ui : t -> int -> t -> t = "ml_mpz_pow_mod_ui";
external sqrt : t -> t = "ml_mpz_sqrt";
external gcd : t -> t -> t = "ml_mpz_gcd";
external l_and : t -> t -> t = "ml_mpz_and";
external l_ior : t -> t -> t = "ml_mpz_ior";
external l_xor : t -> t -> t = "ml_mpz_xor";
external of_int : int -> t = "ml_mpz_of_int";
external to_int : t -> int = "ml_mpz_to_int";
external of_string : int -> string -> t = "ml_mpz_of_string";
external to_string : int -> t -> string = "ml_mpz_to_string";
external of_float : float -> t = "ml_mpz_of_float";
external to_float : t -> float = "ml_mpz_to_float";
external compare : t -> t -> int = "ml_mpz_compare";
external compare_si : t -> int -> int = "ml_mpz_compare_si";
external randstate_init : t -> randstate = "ml_mpz_randstate_init";
external random : randstate -> t -> t = "ml_mpz_random";
external size_in_base : t -> int -> int = "ml_mpz_size_in_base";
external probab_prime_p : t -> int -> int = "ml_mpz_probab_prime_p";

external mandelbrot_point :
  int -> t -> t -> int -> option (t * t) -> result t =
    "ml_mpz_mandelbrot_point";
external mandelbrot_point3 :
  int -> t -> t -> int -> option (t * t) -> result t =
    "ml_mpz_mandelbrot_point3";
(**)
external mandelbrot_point_m :
  int -> int -> t -> t -> int -> option (t * t) -> result t =
    "ml_mpz_mandelbrot_point_m_bytecode"
    "ml_mpz_mandelbrot_point_m_native";
(**)
external lambda_point :
  int -> t -> t -> int -> option (t * t) -> result t =
    "ml_mpz_lambda_point";
