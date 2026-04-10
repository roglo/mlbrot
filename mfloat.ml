(* $Id: mfloat.ml,v 1.120 2013/03/22 19:41:13 deraugla Exp $ *)

open Mcomm;
open Printf;

IFDEF MPZ THEN
  type mpzp_t =
    { mpz : Mpz.t;
      prec : int }
  ;
END;

IFDEF MPFR THEN
  type best_float = Mpfr.t;
ELSIFDEF MPZ THEN
  type best_float = mpzp_t;
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

IFDEF MPZ THEN
  value mpz_to_float x = ldexp (Mpz.to_float x.mpz) (-x.prec);
END;

module Mfloat_int : Mfloat =
  struct
    type t = int;
    value string_of_num_type () = "int";
    value m = 32768;
    value fm = float m;
    value add = \+;
    value sub = \-;
    value mult_int = \*;
    value mult x y =
      if x = 0 || y = 0 then 0
      else
        let x1 = x / m in
        let x2 = x mod m in
        let y1 = y / m in
        let y2 = y mod m in
        x1 * y1 * m + x1 * y2 + x2 * y1 + (x2 * y2) / m
    ;
    value sqr x = mult x x;
    value div x y = truncate (float x *. fm /. float y);
    value neg x = - x;
    value twice x = 2 * x;
    value half x = x / 2;
    value to_float x = float x /. fm;
    value of_float f = Mutil.round (f *. fm);
    value exp x = of_float (exp (to_float x));
    value cos x = failwith "not impl int.cos";
    value sin x = failwith "not impl int.sin";
    value log x = failwith "not impl int.log";
    value to_string x = sprintf "%.8g" (float x /. float m);
    value of_string _ s = Mutil.round (float_of_string s *. fm);
    value of_best_float f =
      IFDEF MPFR THEN Mutil.round (Mpfr.to_float f *. fm)
      ELSE IFDEF MPZ THEN Mutil.round (mpz_to_float f *. fm)
      ELSE of_float f END END
    ;
    value get_prec _ = 0;
    value one_half = m / 2;
    value four = 4 * m;
    value xy_of_start =
      fun
      [ Some (xn, yn) -> (xn, yn)
      | None -> (0, 0) ]
    ;
    value mandelbrot_point a b nb_it start =
      let (xn, yn) = xy_of_start start in
      loop 0 xn yn (mult xn xn) (mult yn yn)
      where rec loop it xn yn sqr_xn sqr_yn =
        if it >= nb_it then LimitReached xn yn 0
        else
          let u = mult xn yn in
          let yn = u + u + b in
          let xn = sqr_xn - sqr_yn + a in
          let sqr_yn = mult yn yn in
          let sqr_xn = mult xn xn in
          if sqr_xn + sqr_yn >= four then Result it
          else loop (it + 1) xn yn sqr_xn sqr_yn
    ;
    value mandelbrot_point3 a b nb_it start =
      let (xn, yn) = xy_of_start start in
      loop 0 xn yn (mult xn xn) (mult yn yn)
      where rec loop it xn yn sqr_xn sqr_yn =
        if it >= nb_it then LimitReached xn yn 0
        else
          let x = xn and y = yn in
          let xn = mult x (sqr_xn - mult_int 3 sqr_yn) + a in
          let yn = mult y (mult_int 3 sqr_xn - sqr_yn) + b in
          let sqr_yn = mult yn yn in
          let sqr_xn = mult xn xn in
          if sqr_xn + sqr_yn >= four then Result it
          else loop (it + 1) xn yn sqr_xn sqr_yn
    ;
    value mandelbrot_point_m m a b nb_it start =
      failwith "not impl Mfloat_int.mandelbrot_point_m"
    ;
    value lambda_point a b nb_it start =
      failwith "not impl Mfloat_int.lamda_point"
    ;
    value set_num _ = ();
    value get_num () = failwith "get_num";
    value set_default_prec _ = ();
    value with_prec _ x = x;
    value serialize x = {s_prec = get_prec x; s_value = to_string x};
    value deserialize s = of_string s.s_prec s.s_value;
  end
;

module Mfloat_i64 : Mfloat =
  struct
    type t = int64;
    value string_of_num_type () = "i64";
    value mf = ldexp 1. 30;
    value m64 = Int64.of_float mf;
    value add = Int64.add;
    value sub = Int64.sub;
    value mult_int x y = Int64.mul x (Int64.of_int y);
    value mult x y =
      if x = Int64.zero || y = Int64.zero then Int64.zero
      else
        let x1 = Int64.div x m64 in
        let x2 = Int64.rem x m64 in
        let y1 = Int64.div y m64 in
        let y2 = Int64.rem y m64 in
        Int64.add
          (Int64.add
             (Int64.add
                (Int64.mul
                   (Int64.mul x1 y1)
                   m64)
                (Int64.mul x1 y2))
             (Int64.mul x2 y1))
          (Int64.div (Int64.mul x2 y2) m64)
    ;
    value sqr x = mult x x;
    value div x y = Int64.div (Int64.mul x m64) y;
    value int64_two = Int64.of_int 2;
    value neg = Int64.neg;
    value twice x = Int64.mul x int64_two;
    value half x = Int64.div x int64_two;
    value exp x = failwith "not impl i64.exp";
    value cos x = failwith "not impl i64.cos";
    value sin x = failwith "not impl i64.sin";
    value log x = failwith "not impl i64.log";
    value to_string x = sprintf "%.16g" (Int64.to_float x /. mf);
    value of_string _ s = Int64.of_float (float_of_string s *. mf);
    value to_float x = Int64.to_float x /. mf;
    value of_float f = Int64.of_float (f *. mf +. 0.5);
    value of_best_float f =
      IFDEF MPFR THEN Int64.of_float (Mpfr.to_float f *. mf +. 0.5)
      ELSE IFDEF MPZ THEN Int64.of_float (mpz_to_float f *. mf +. 0.5)
      ELSE of_float f END END
    ;
    value get_prec _ = 0;
    value one_half = Int64.div m64 (Int64.of_int 2);
    value four = Int64.mul (Int64.of_int 4) m64;
    value xy_of_start =
      fun
      [ Some (xn, yn) -> (xn, yn)
      | None -> (Int64.zero, Int64.zero) ]
    ;
    value mandelbrot_point a b nb_it start =
      let (xn, yn) = xy_of_start start in
      loop 0 xn yn (mult xn xn) (mult yn yn)
      where rec loop it xn yn sqr_xn sqr_yn =
        if it >= nb_it then LimitReached xn yn 0
        else
          let u = mult xn yn in
          let yn = Int64.add (Int64.add u u) b in
          let xn = Int64.add (Int64.sub sqr_xn sqr_yn) a in
          let sqr_yn = mult yn yn in
          let sqr_xn = mult xn xn in
          if Int64.add sqr_xn sqr_yn >= four then Result it
          else loop (it + 1) xn yn sqr_xn sqr_yn
    ;
    value mandelbrot_point3 a b nb_it start =
      failwith "not impl Mfloat_i64.mandelbrot_point3"
    ;
    value mandelbrot_point_m m a b nb_it start =
      failwith "not impl Mfloat_i64.mandelbrot_point_m"
    ;
    value lambda_point a b nb_it start =
      failwith "not impl Mfloat_i64.lamda_point"
    ;
    value set_num _ = ();
    value get_num () = failwith "get_num";
    value set_default_prec _ = ();
    value with_prec _ x = x;
    value serialize x = {s_prec = get_prec x; s_value = to_string x};
    value deserialize s = of_string s.s_prec s.s_value;
  end
;

value fp = ref None;

module Mfloat_flo : Mfloat with type t = float =
  struct
    type t = float;
    value string_of_num_type () = "flo";
    value neg x = -. x;
    value twice x = 2. *. x;
    value half x = x /. 2.;
    value add = \+.;
    value sub = \-.;
    value mult_int x y = x *. float y;
    value mult x y = x *. y;
    value sqr x = x *. x;
    value div x y = x /. y;
    value exp = Pervasives.exp;
    value cos = Pervasives.cos;
    value sin = Pervasives.sin;
    value log = Pervasives.log;
    value to_string x = sprintf "%.16g" x;
    value of_string _ = float_of_string;
    value to_float x = x;
    value of_float x = x;
    value get_prec _ = 0;
    value of_best_float x =
      IFDEF MPFR THEN Mpfr.to_float x
      ELSE IFDEF MPZ THEN mpz_to_float x
      ELSE x END END
    ;
    value xy_of_start def_x =
      fun
      [ Some (xn, yn) -> (xn, yn)
      | None -> (def_x, 0.0) ]
    ;
    value slow_mandelbrot_point a b nb_it start =
      let (xn, yn) = xy_of_start 0.0 start in
      loop 0 xn yn (xn *. xn) (yn *. yn)
      where rec loop it xn yn sqr_xn sqr_yn =
        if it >= nb_it then LimitReached xn yn 0
        else do {
          let u = xn *. yn in
          let yn = u +. u +. b in
          let xn = sqr_xn -. sqr_yn +. a in
          let sqr_xn = xn *. xn in
          let sqr_yn = yn *. yn in
          match fp.val with
          [ None -> ()
          | Some f -> f xn yn ];
          if sqr_xn +. sqr_yn >= 4. then Result it
          else loop (it + 1) xn yn sqr_xn sqr_yn
        }
    ;
    external fast_mandelbrot_point :
      float -> float -> int -> option (float * float) -> result float
      = "ml_flo_mandelbrot_point"
    ;
    value mandelbrot_point a b nb_it start =
      if fp.val = None then fast_mandelbrot_point a b nb_it start
      else slow_mandelbrot_point a b nb_it start
    ;
    value lambda_point a b nb_it start =
      let (xn, yn) = xy_of_start 0.5 start in
      loop 0 xn yn (xn *. xn) (yn *. yn)
      where rec loop it xn yn sqr_xn sqr_yn =
        if it >= nb_it then LimitReached xn yn 0
        else
          let u = xn -. sqr_xn +. sqr_yn in
          let v = yn *. (1.0 -. xn -. xn) in
          let xn = a *. u -. b *. v in
          let yn = a *. v +. b *. u in
          let sqr_xn = xn *. xn in
          let sqr_yn = yn *. yn in
          if sqr_xn +. sqr_yn >= 4. then Result it
          else loop (it + 1) xn yn sqr_xn sqr_yn
    ;
    value mandelbrot_point3 a b nb_it start =
      let (xn, yn) = xy_of_start 0.0 start in
      loop 0 xn yn (xn *. xn) (yn *. yn)
      where rec loop it xn yn sqr_xn sqr_yn =
        if it >= nb_it then LimitReached xn yn 0
        else
          let xn = xn *. (sqr_xn -. 3. *. sqr_yn) +. a in
          let yn = yn *. (3. *. sqr_xn -. sqr_yn) +. b in
          let sqr_xn = xn *. xn in
          let sqr_yn = yn *. yn in
          if sqr_xn +. sqr_yn >= 4. then Result it
          else loop (it + 1) xn yn sqr_xn sqr_yn
    ;
    value power x y m =
      loop 1. 0. m where rec loop xi yi m =
        if m <= 0 then (xi, yi)
        else loop (x *. xi -. y *. yi) (x *. yi +. y *. xi) (m - 1)
    ;
    value mandelbrot_point_m m a b nb_it start =
      let (xn, yn) = xy_of_start 0.0 start in
      loop 0 xn yn where rec loop it xn yn =
        if it >= nb_it then LimitReached xn yn 0
        else
          let (xn, yn) = power xn yn m in
          let xn = xn +. a in
          let yn = yn +. b in
          if xn *. xn +. yn *. yn >= 4. then Result it
          else loop (it + 1) xn yn
    ;
    value set_num _ = ();
    value get_num () = failwith "get_num";
    value set_default_prec _ = ();
    value with_prec _ x = x;
    value serialize x = {s_prec = get_prec x; s_value = to_string x};
    value deserialize s = of_string s.s_prec s.s_value;
  end
;

value string_to_end s i = String.sub s i (String.length s - i);

value sign_int_frac_exp_of_string s =
  let (sign, s) =
    if s.[0] = '-' then (True, String.sub s 1 (String.length s - 1))
    else (False, s)
  in
  let (mant, exp) =
    match try Some (String.index s 'e') with [ Not_found -> None ] with
    [ Some i ->
        (String.sub s 0 i, int_of_string (string_to_end s (i + 1)))
    | None ->
        (s, 0) ]
  in
  let (int, frac) =
    match try Some (String.index mant '.') with [ Not_found -> None ] with
    [ Some i -> (String.sub mant 0 i, string_to_end mant (i + 1))
    | None -> (mant, "") ]
  in
  (sign, int, frac, exp)
;

IFDEF MPFR THEN
value int_frac_exp_of_mpfr base x =
  let (m, e) = Mpfr.to_string base x in
  let (is_neg, m) =
    if m.[0] = '-' then (True, String.sub m 1 (String.length m - 1))
    else (False, m)
  in
  let (int, frac, e) =
    if e > 0 then
      if e >= String.length m then
        (String.make 1 m.[0], String.sub m 1 (String.length m - 1), e - 1)
      else
        (String.sub m 0 e, String.sub m e (String.length m - e), 0)
    else if e < 0 then
      (String.make 1 m.[0], String.sub m 1 (String.length m - 1), e - 1)
    else
      ("0", m, e)
  in
  (is_neg, int, frac, e)
;
END;

module Mfloat_big : Mfloat =
  struct
    open Big_int;
    type t = big_int;
    type prec 't =
      { e : mutable int;
        m : mutable 't;
        half_m : mutable 't;
        one : mutable 't;
        b_four : mutable 't;
        one_half : mutable 't }
    ;
    value string_of_num_type () = "big";
    value two_big_int = big_int_of_int 2;
    value s =
      {e = 0; m = zero_big_int; half_m = zero_big_int; one = zero_big_int;
       b_four = zero_big_int; one_half = zero_big_int}
    ;
    value set_default_prec e = do {
      s.e := e;
      s.m := power_int_positive_int 2 e;
      s.half_m := power_int_positive_int 2 (e - 1);
      s.one := s.m;
      s.b_four := mult_int_big_int 4 (square_big_int s.m);
      s.one_half := div_big_int s.one two_big_int;
    };
    set_default_prec 53;

    value neg = minus_big_int;
    value twice x = mult_int_big_int 2 x;
    value half x = div_big_int x two_big_int;
    value add = add_big_int;
    value sub = sub_big_int;
    value mult_int x y = mult_int_big_int y x;
    value mult x y = div_big_int (mult_big_int x y) s.m;
    value sqr x = mult x x;
    value div x y = div_big_int (mult_big_int x s.m) y;
    value exp x = failwith "not impl big.exp";
    value cos x = failwith "not impl big.cos";
    value sin x = failwith "not impl big.sin";
    value log x = failwith "not impl big.log";
    value to_string x = sprintf "%.16g" (ldexp (float_of_big_int x) (-s.e));
    value of_sign_int_frac_exp is_neg int frac exp =
      let r = mult_big_int (big_int_of_string (int ^ frac)) s.m in
      let e = String.length frac - exp in
      let r =
        if e > 0 then div_big_int r (power_int_positive_int 10 e)
        else if e < 0 then mult_big_int r (power_int_positive_int 10 (-e))
        else r
      in
      if is_neg then minus_big_int r else r
    ;
    value of_string _ t =
      let (is_neg, int, frac, exp) = sign_int_frac_exp_of_string t in
      of_sign_int_frac_exp is_neg int frac exp
    ;
    value to_float x = ldexp (float_of_big_int x) (-s.e);
    value of_float f =
      let str = sprintf "%.0f" (ldexp f s.e) in
      big_int_of_string str
    ;
    value get_prec _ = 0;
    IFDEF MPFR THEN
      value of_mpfr f =
        let (is_neg, int, frac, exp) = int_frac_exp_of_mpfr 10 f in
        of_sign_int_frac_exp is_neg int frac exp
      ;
    END;
    value of_best_float f =
      IFDEF MPFR THEN of_mpfr f
      ELSIFDEF MPZ THEN failwith "big.of_best_float"
      ELSE of_float f END
    ;
    value xy_of_start =
      fun
      [ Some (xn, yn) -> (xn, yn)
      | None -> (zero_big_int, zero_big_int) ]
    ;
    value mandelbrot_point a b nb_it start = 
      let (xn, yn) = xy_of_start start in
      loop 0 xn yn (square_big_int xn) (square_big_int yn)
      where rec loop it xn yn b_sqr_xn b_sqr_yn =
        if it >= nb_it then LimitReached xn yn 0
        else
          let yn =
            add_big_int (div_big_int (mult_big_int xn yn) s.half_m) b
          in
          let xn =
            add_big_int (div_big_int (sub_big_int b_sqr_xn b_sqr_yn) s.m) a
          in
          let b_sqr_yn = square_big_int yn in
          let b_sqr_xn = square_big_int xn in
          if ge_big_int (add_big_int b_sqr_xn b_sqr_yn) s.b_four then
            Result it
          else
            loop (it + 1) xn yn b_sqr_xn b_sqr_yn
    ;
    value mandelbrot_point3 a b nb_it _ =
      failwith "not impl Mfloat_big.mandelbrot_point3"
    ;
    value mandelbrot_point_m m a b nb_it _ =
      failwith "not impl Mfloat_big.mandelbrot_point_m"
    ;
    value lambda_point a b nb_it _ =
      failwith "not impl Mfloat_big.lamda_point"
    ;
    value set_num _ = ();
    value get_num () = failwith "get_num";
    value with_prec _ x = x;
    value serialize x = {s_prec = get_prec x; s_value = to_string x};
    value deserialize s = of_string s.s_prec s.s_value;
  end
;

IFDEF MPFR THEN
module Mfloat_mpf : Mfloat with type t = Mpfr.t =
  struct
    type t = Mpfr.t;
    value string_of_num_type () = "mpf";
    value neg = Mpfr.neg;
    value twice x = Mpfr.mul_si x 2;
    value half x = Mpfr.div_si x 2;
    value add = Mpfr.add;
    value sub = Mpfr.sub;
    value mult = Mpfr.mul;
    value mult_int = Mpfr.mul_si;
    value to_float = Mpfr.to_float;
    value of_float = Mpfr.of_float;
    value sqr = Mpfr.sqr;
    value div = Mpfr.div;
    value cos = Mpfr.cos;
    value sin = Mpfr.sin;
    value exp x = of_float (exp (to_float x));
    value log x = failwith "not impl mpf.log";
    value to_string x =
      let (is_neg, int, frac, exp) = int_frac_exp_of_mpfr 10 x in
      sprintf "%s%s.%s%s" (if is_neg then "-" else "") int frac
        (if exp = 0 then "" else "e" ^ string_of_int exp)
    ;
    value of_string = Mpfr.of_string;
    value get_prec = Mpfr.get_prec;
    value of_best_float x = x;
    value mandelbrot_point = Mpfr.mandelbrot_point;
    value mandelbrot_point3 = Mpfr.mandelbrot_point3;
    value mandelbrot_point_m = Mpfr.mandelbrot_point_m;
    value lambda_point = Mpfr.lambda_point;
    value set_num _ = ();
    value set_default_prec = Mpfr.set_default_prec;
    value get_num () = failwith "Mpfr.get_num";
    value with_prec = Mpfr.with_prec;
    value serialize _ = failwith "Mpfr.serialize";
    value deserialize _ = failwith "Mpfr.deserialize";
  end
;
END;

IFDEF MPZ THEN
module Mfloat_mpz : Mfloat with type t = mpzp_t =
  struct
    type t = mpzp_t;
    value zero = Mpz.of_int 0;
    type prec = { e : mutable int };
    value s = {e = 0};
    value set_default_prec e =
      let e = max e 53 in
      let e = e + 50 in
      s.e := e
    ;
    set_default_prec 53;
    value with_prec p x =
      let p = max p 53 in
      if p > x.prec then
        {mpz = Mpz.mul_2exp x.mpz (p - x.prec); prec = p}
      else if p < x.prec then
        {mpz = Mpz.div_q_2exp x.mpz (x.prec - p); prec = p}
      else x
    ;

    value string_of_num_type () = "mpz";
    value to_string x =
      let (is_neg, n) =
        if Mpz.compare x.mpz zero < 0 then (True, Mpz.neg x.mpz)
        else (False, x.mpz)
      in
      let int = Mpz.to_int (Mpz.div_q_2exp n x.prec) in
      let n_signif_digits =
        Mutil.round (float x.prec /. log 10. *. log 2.) + 1
      in
      let frac = Mpz.div_r_2exp n x.prec in
      let (int, dl, exp) =
        loop n_signif_digits [] frac 0 where rec loop ndec rev_dl frac exp =
          let frac = Mpz.mul_si frac 10 in
          let d = Mpz.to_int (Mpz.div_q_2exp frac x.prec) in
          if ndec = 0 then
            if d >= 5 then
              let (carry, dl) =
                List.fold_left
                  (fun (carry, dl) d ->
                     let d = d + carry in
                     let (carry, d) = if d = 10 then (1, 0) else (0, d) in
                     (carry, [d :: dl]))
                  (1, []) rev_dl
              in
              if carry = 1 then (int + 1, dl, exp) else (int, dl, exp)
            else
              (int, List.rev rev_dl, exp)
          else
            let (ndec, rev_dl, exp) =
              if ndec = n_signif_digits && int = 0 && d = 0 &&
                 Mpz.compare x.mpz zero <> 0
              then (ndec, rev_dl, exp - 1)
              else (ndec - 1, [d :: rev_dl], exp)
            in
            loop ndec rev_dl (Mpz.div_r_2exp frac x.prec) exp
      in
      let (int, dl, exp) =
        if exp = 0 then (int, dl, exp)
        else
          match dl with
          [ [d :: dl] -> (d, dl, exp - 1)
          | [] -> (int, dl, exp) ]
      in
      let frac =
        String.concat ""
          (List.map (fun d -> String.make 1 (Char.chr (Char.code '0' + d)))
             dl)
      in
      sprintf "%s%d.%s%s" (if is_neg then "-" else "") int frac
        (if exp <> 0 then "e" ^ string_of_int exp else "")
    ;

    value neg x = with_prec s.e {mpz = Mpz.neg x.mpz; prec = x.prec};
    value twice x = with_prec s.e {mpz = Mpz.mul_si x.mpz 2; prec = x.prec};
    value half x = with_prec s.e {mpz = Mpz.div_q_ui x.mpz 2; prec = x.prec};
    value add x y =
      let x = with_prec s.e x in
      let y = with_prec s.e y in
      {mpz = Mpz.add x.mpz y.mpz; prec = s.e}
    ;
    value sub x y =
      let x = with_prec s.e x in
      let y = with_prec s.e y in
      {mpz = Mpz.sub x.mpz y.mpz; prec = s.e}
    ;
    value mult x y =
      with_prec s.e {mpz = Mpz.mul x.mpz y.mpz; prec = x.prec + y.prec}
    ;
    value mult_int x i =
      with_prec s.e {mpz = Mpz.mul_si x.mpz i; prec = x.prec}
    ;
    value sqr x = mult x x;
    value div x y =
      if Mpz.compare y.mpz zero = 0 then
        let r = Mpz.mul_2exp (Mpz.of_int 4) s.e in
        {mpz = r; prec = s.e}
      else
        let x = with_prec s.e x in
        let y = with_prec s.e y in
        let r = Mpz.div_q (Mpz.mul_2exp x.mpz s.e) y.mpz in
        {mpz = r; prec = s.e}
    ;
    value exp x =
      let x = with_prec s.e x in
      let one = Mpz.mul_2exp (Mpz.of_int 1) s.e in
      sum one 0 one where rec sum r n xn_on_fn =
        if Mpz.compare xn_on_fn zero = 0 then {mpz = r; prec = s.e}
        else
          let xn1_on_fn1 =
            Mpz.div_q_2exp (Mpz.div_q_ui (Mpz.mul xn_on_fn x.mpz) (n + 1)) s.e
          in
          sum (Mpz.add r xn1_on_fn1) (n + 1) xn1_on_fn1
    ;
    value cos x =
      let x = with_prec s.e x in
      let x2 = Mpz.mul x.mpz x.mpz in
      let one = Mpz.mul_2exp (Mpz.of_int 1) s.e in
      sum one 0 True one where rec sum r n neg x2n_on_f2n =
        if Mpz.compare x2n_on_f2n zero = 0 then {mpz = r; prec = s.e}
        else
          let x2n_on_f2n =
            Mpz.div_q_2exp
              (Mpz.div_q_ui (Mpz.mul x2n_on_f2n x2)
                 ((2 * n + 1) * (2 * n + 2)))
              (2 * s.e)
          in
          let c = if neg then Mpz.neg x2n_on_f2n else x2n_on_f2n in
          sum (Mpz.add r c) (n + 1) (not neg) x2n_on_f2n
    ;
    value sin x =
      let x = with_prec s.e x in
      let (ax, is_neg) =
        if Mpz.compare x.mpz zero < 0 then (Mpz.neg x.mpz, True)
        else (x.mpz, False)
      in
      let x2 = Mpz.mul x.mpz x.mpz in
      sum ax 0 True ax where rec sum r n neg x2n1_on_f2n1 =
        if Mpz.compare x2n1_on_f2n1 zero = 0 then
          let r = if is_neg then Mpz.neg r else r in
          {mpz = r; prec = s.e}
        else
          let x2n1_on_f2n1 =
            Mpz.div_q_2exp
              (Mpz.div_q_ui (Mpz.mul x2n1_on_f2n1 x2)
                 ((2 * n + 2) * (2 * n + 3)))
              (2 * s.e)
          in
          let c = if neg then Mpz.neg x2n1_on_f2n1 else x2n1_on_f2n1 in
          sum (Mpz.add r c) (n + 1) (not neg) x2n1_on_f2n1
    ;
    value to_float x = mpz_to_float x;
    value of_float f =
      let fexp = - snd (frexp f) in
      let prec = fexp + 50 in
      let str = sprintf "%.0f" (ldexp f prec) in
      {mpz = Mpz.of_string 10 str; prec = prec}
    ;
    value log x =
      if Mpz.compare x.mpz zero <= 0 then invalid_arg "mpz.log"
      else do {
        let very_small = with_prec s.e (of_float 1e-100) in
        loop 0.0 x where rec loop r x =
          let x = with_prec s.e x in
          if Mpz.compare x.mpz very_small.mpz < 0 then
            loop (r -. log 2.0) (twice x)
          else r +. log (to_float x)
      }
    ;
    value of_sign_int_frac_exp base p is_neg int frac exp =
      let e = String.length frac - exp in
      let m = Mpz.mul_2exp (Mpz.of_string base (int ^ frac)) p in
      let r =
        if e > 0 then Mpz.div_q m (Mpz.ui_pow_ui base e)
        else if e < 0 then Mpz.mul m (Mpz.ui_pow_ui base (-e))
        else m
      in
      if is_neg then Mpz.neg r else r
    ;
    value of_string p t =
      let (is_neg, int, frac, exp) = sign_int_frac_exp_of_string t in
      {mpz = of_sign_int_frac_exp 10 p is_neg int frac exp; prec = p}
    ;
    IFDEF MPFR THEN
      value of_mpfr f =
        let (is_neg, int, frac, exp) = int_frac_exp_of_mpfr 2 f in
        let mpz = of_sign_int_frac_exp 2 s.e is_neg int frac exp in
        {mpz = mpz; prec = s.e}
      ;
    END;
    value of_best_float f =
      IFDEF MPFR THEN of_mpfr f
      ELSE with_prec s.e f END
    ;
    value get_prec x = x.prec;
    value gen_mandelbrot_point func a b nb_it start =
      let p = s.e - 50 in
      let a = with_prec p a in
      let b = with_prec p b in
      if a.prec = p && b.prec = p then
        let start =
          Mutil.map_option
            (fun (x, y) -> ((with_prec p x).mpz, (with_prec p y).mpz))
            start
        in
        let r = func p a.mpz b.mpz nb_it start in
        Mutil.map_result (fun x -> {mpz = x; prec = p}) r
      else failwith "mpz.gen_mandelbrot_point"
    ;
    value mandelbrot_point = gen_mandelbrot_point Mpz.mandelbrot_point;
    value mandelbrot_point3 = gen_mandelbrot_point Mpz.mandelbrot_point3;
(**)
    value mandelbrot_point_m m =
      gen_mandelbrot_point (Mpz.mandelbrot_point_m m)
    ;
(*
    value mandelbrot_point_m m = failwith "mpz.mandelbrot_point_m";
*)
    value lambda_point = gen_mandelbrot_point Mpz.lambda_point;
    value set_num _ = failwith "mpz.set_num";
    value get_num () = failwith "get_num";
    value serialize x =
      {s_prec = get_prec x; s_value = Mpz.to_string 62 x.mpz}
    ;
    value deserialize s =
      {prec = s.s_prec; mpz = Mpz.of_string 62 s.s_value}
    ;
  end
;
END;

module M : Mfloat =
  struct
    type comb =
      [ F_int of Mfloat_int.t
      | F_i64 of Mfloat_i64.t
      | F_flo of Mfloat_flo.t
      | F_big of Mfloat_big.t
      | IFDEF MPFR THEN
        F_mpf of Mfloat_mpf.t
        END
      | IFDEF MPZ THEN
        F_mpz of Mfloat_mpz.t
        END ]
    ;
    type t = { a : mutable comb };
    value gnt = ref N_flo;
    value string_of_num_type () =
      match gnt.val with
      [ N_int -> Mfloat_int.string_of_num_type ()
      | N_i64 -> Mfloat_i64.string_of_num_type ()
      | N_flo -> Mfloat_flo.string_of_num_type ()
      | N_big -> Mfloat_big.string_of_num_type ()
      | IFDEF MPFR THEN
        N_mpf -> Mfloat_mpf.string_of_num_type ()
        END
      | IFDEF MPZ THEN
        N_mpz -> Mfloat_mpz.string_of_num_type ()
        END ]
    ;
    value int_value x =
      match x.a with
      [ F_int a -> a
      | F_i64 a -> Mfloat_int.of_float (Mfloat_i64.to_float a)
      | F_flo a -> Mfloat_int.of_float (Mfloat_flo.to_float a)
      | F_big a -> Mfloat_int.of_float (Mfloat_big.to_float a)
      | IFDEF MPFR THEN
        F_mpf a -> Mfloat_int.of_float (Mfloat_mpf.to_float a)
        END
      | IFDEF MPZ THEN
        F_mpz a -> Mfloat_int.of_float (Mfloat_mpz.to_float a)
        END ]
    ;
    value i64_value x =
      match x.a with
      [ F_int a -> Mfloat_i64.of_float (Mfloat_int.to_float a)
      | F_i64 a -> a
      | F_flo a -> Mfloat_i64.of_float (Mfloat_flo.to_float a)
      | F_big a -> Mfloat_i64.of_float (Mfloat_big.to_float a)
      | IFDEF MPFR THEN
        F_mpf a -> Mfloat_i64.of_float (Mfloat_mpf.to_float a)
        END
      | IFDEF MPZ THEN
        F_mpz a -> Mfloat_i64.of_float (Mfloat_mpz.to_float a)
        END ]
    ;
    value flo_value x =
      match x.a with
      [ F_int a -> Mfloat_flo.of_float (Mfloat_int.to_float a)
      | F_i64 a -> Mfloat_flo.of_float (Mfloat_i64.to_float a)
      | F_flo a -> a
      | F_big a -> Mfloat_flo.of_float (Mfloat_big.to_float a)
      | IFDEF MPFR THEN
        F_mpf a -> Mfloat_flo.of_float (Mfloat_mpf.to_float a)
        END
      | IFDEF MPZ THEN
        F_mpz a -> Mfloat_flo.of_float (Mfloat_mpz.to_float a)
        END ]
    ;
    value big_value x =
      match x.a with
      [ F_int a -> Mfloat_big.of_float (Mfloat_int.to_float a)
      | F_i64 a -> Mfloat_big.of_float (Mfloat_i64.to_float a)
      | F_flo a -> Mfloat_big.of_float (Mfloat_flo.to_float a)
      | F_big a -> a
      | IFDEF MPFR THEN
        F_mpf a -> Mfloat_big.of_float (Mfloat_mpf.to_float a)
        END
      | IFDEF MPZ THEN
        F_mpz a -> Mfloat_big.of_float (Mfloat_mpz.to_float a)
        END ]
    ;
    IFDEF MPFR THEN
    value mpf_value x =
      match x.a with
      [ F_int a -> Mfloat_mpf.of_float (Mfloat_int.to_float a)
      | F_i64 a -> Mfloat_mpf.of_float (Mfloat_i64.to_float a)
      | F_flo a -> Mfloat_mpf.of_float (Mfloat_flo.to_float a)
      | F_big a -> Mfloat_mpf.of_float (Mfloat_big.to_float a)
      | F_mpf a -> a
      | IFDEF MPZ THEN
        F_mpz a -> Mfloat_mpf.of_float (Mfloat_mpz.to_float a)
        END ]
    ;
    END;
    IFDEF MPZ THEN
    value mpz_value x =
      match x.a with
      [ F_int a -> Mfloat_mpz.of_float (Mfloat_int.to_float a)
      | F_i64 a -> Mfloat_mpz.of_float (Mfloat_i64.to_float a)
      | F_flo a -> Mfloat_mpz.of_float (Mfloat_flo.to_float a)
      | F_big a -> Mfloat_mpz.of_float (Mfloat_big.to_float a)
      | IFDEF MPFR THEN
        F_mpf a -> Mfloat_mpz.of_float (Mfloat_mpf.to_float a)
        END
      | F_mpz a -> a ]
    ;
    END; 
    value to_int x =
      match x.a with
      [ F_int a -> a
      | _ -> do {
          let a = int_value x in
          x.a := F_int a;
          a
        } ]
    ;
    value to_i64 x =
      match x.a with
      [ F_i64 a -> a
      | _ -> do {
          let a = i64_value x in
          x.a := F_i64 a;
          a
        } ]
    ;
    value to_flo x =
      match x.a with
      [ F_flo a -> a
      | _ -> do {
          let a = flo_value x in
          x.a := F_flo a;
          a
        } ]
    ;
    value to_big x =
      match x.a with
      [ F_big a -> a
      | _ -> do {
          let a = big_value x in
          x.a := F_big a;
          a
        } ]
    ;
    IFDEF MPFR THEN
    value to_mpf x =
      match x.a with
      [ F_mpf a -> a
      | _ -> do {
          let a = mpf_value x in
          x.a := F_mpf a;
          a
        } ]
    ;
    END;
    IFDEF MPZ THEN
    value to_mpz x =
      match x.a with
      [ F_mpz a -> a
      | _ -> do {
          let a = mpz_value x in
          x.a := F_mpz a;
          a
        } ]
    ;
    END;
    value neg x =
      match x.a with
      [ F_int a -> {a = F_int (Mfloat_int.neg a)}
      | F_i64 a -> {a = F_i64 (Mfloat_i64.neg a)}
      | F_flo a -> {a = F_flo (Mfloat_flo.neg a)}
      | F_big a -> {a = F_big (Mfloat_big.neg a)}
      | IFDEF MPFR THEN
        F_mpf a -> {a = F_mpf (Mfloat_mpf.neg a)}
        END
      | IFDEF MPZ THEN
        F_mpz a -> {a = F_mpz (Mfloat_mpz.neg a)}
        END ]
    ;
    value twice x =
      match gnt.val with
      [ N_int -> {a = F_int (Mfloat_int.twice (to_int x))}
      | N_i64 -> {a = F_i64 (Mfloat_i64.twice (to_i64 x))}
      | N_flo -> {a = F_flo (Mfloat_flo.twice (to_flo x))}
      | N_big -> {a = F_big (Mfloat_big.twice (to_big x))}
      | IFDEF MPFR THEN
        N_mpf -> {a = F_mpf (Mfloat_mpf.twice (to_mpf x))}
        END
      | IFDEF MPZ THEN
        N_mpz -> {a = F_mpz (Mfloat_mpz.twice (to_mpz x))}
        END ]
    ;
    value half x =
      match gnt.val with
      [ N_int -> {a = F_int (Mfloat_int.half (to_int x))}
      | N_i64 -> {a = F_i64 (Mfloat_i64.half (to_i64 x))}
      | N_flo -> {a = F_flo (Mfloat_flo.half (to_flo x))}
      | N_big -> {a = F_big (Mfloat_big.half (to_big x))}
      | IFDEF MPFR THEN
        N_mpf -> {a = F_mpf (Mfloat_mpf.half (to_mpf x))}
        END
      | IFDEF MPZ THEN
        N_mpz -> {a = F_mpz (Mfloat_mpz.half (to_mpz x))}
        END ]
    ;
    value add x y =
      match gnt.val with
      [ N_int -> {a = F_int (Mfloat_int.add (to_int x) (to_int y))}
      | N_i64 -> {a = F_i64 (Mfloat_i64.add (to_i64 x) (to_i64 y))}
      | N_flo -> {a = F_flo (Mfloat_flo.add (to_flo x) (to_flo y))}
      | N_big -> {a = F_big (Mfloat_big.add (to_big x) (to_big y))}
      | IFDEF MPFR THEN
        N_mpf -> {a = F_mpf (Mfloat_mpf.add (to_mpf x) (to_mpf y))}
        END
      | IFDEF MPZ THEN
        N_mpz -> {a = F_mpz (Mfloat_mpz.add (to_mpz x) (to_mpz y))}
        END ]
    ;
    value sub x y =
      match gnt.val with
      [ N_int -> {a = F_int (Mfloat_int.sub (to_int x) (to_int y))}
      | N_i64 -> {a = F_i64 (Mfloat_i64.sub (to_i64 x) (to_i64 y))}
      | N_flo -> {a = F_flo (Mfloat_flo.sub (to_flo x) (to_flo y))}
      | N_big -> {a = F_big (Mfloat_big.sub (to_big x) (to_big y))}
      | IFDEF MPFR THEN
        N_mpf -> {a = F_mpf (Mfloat_mpf.sub (to_mpf x) (to_mpf y))}
        END
      | IFDEF MPZ THEN
        N_mpz -> {a = F_mpz (Mfloat_mpz.sub (to_mpz x) (to_mpz y))}
        END ]
    ;
    value mult_int x y =
      match gnt.val with
      [ N_int -> {a = F_int (Mfloat_int.mult_int (to_int x) y)}
      | N_i64 -> {a = F_i64 (Mfloat_i64.mult_int (to_i64 x) y)}
      | N_flo -> {a = F_flo (Mfloat_flo.mult_int (to_flo x) y)}
      | N_big -> {a = F_big (Mfloat_big.mult_int (to_big x) y)}
      | IFDEF MPFR THEN
        N_mpf -> {a = F_mpf (Mfloat_mpf.mult_int (to_mpf x) y)}
        END
      | IFDEF MPZ THEN
        N_mpz -> {a = F_mpz (Mfloat_mpz.mult_int (to_mpz x) y)}
        END ]
    ;
    value mult x y =
      match gnt.val with
      [ N_int -> {a = F_int (Mfloat_int.mult (to_int x) (to_int y))}
      | N_i64 -> {a = F_i64 (Mfloat_i64.mult (to_i64 x) (to_i64 y))}
      | N_flo -> {a = F_flo (Mfloat_flo.mult (to_flo x) (to_flo y))}
      | N_big -> {a = F_big (Mfloat_big.mult (to_big x) (to_big y))}
      | IFDEF MPFR THEN
        N_mpf -> {a = F_mpf (Mfloat_mpf.mult (to_mpf x) (to_mpf y))}
        END
      | IFDEF MPZ THEN
        N_mpz -> {a = F_mpz (Mfloat_mpz.mult (to_mpz x) (to_mpz y))}
        END ]
    ;
    value sqr x =
      match gnt.val with
      [ N_int -> {a = F_int (Mfloat_int.sqr (to_int x))}
      | N_i64 -> {a = F_i64 (Mfloat_i64.sqr (to_i64 x))}
      | N_flo -> {a = F_flo (Mfloat_flo.sqr (to_flo x))}
      | N_big -> {a = F_big (Mfloat_big.sqr (to_big x))}
      | IFDEF MPFR THEN
        N_mpf -> {a = F_mpf (Mfloat_mpf.sqr (to_mpf x))}
        END
      | IFDEF MPZ THEN
        N_mpz -> {a = F_mpz (Mfloat_mpz.sqr (to_mpz x))}
        END ]
    ;
    value div x y =
      match gnt.val with
      [ N_int -> {a = F_int (Mfloat_int.div (to_int x) (to_int y))}
      | N_i64 -> {a = F_i64 (Mfloat_i64.div (to_i64 x) (to_i64 y))}
      | N_flo -> {a = F_flo (Mfloat_flo.div (to_flo x) (to_flo y))}
      | N_big -> {a = F_big (Mfloat_big.div (to_big x) (to_big y))}
      | IFDEF MPFR THEN
        N_mpf -> {a = F_mpf (Mfloat_mpf.div (to_mpf x) (to_mpf y))}
        END
      | IFDEF MPZ THEN
        N_mpz -> {a = F_mpz (Mfloat_mpz.div (to_mpz x) (to_mpz y))}
        END ]
    ;
    value exp x = failwith "not impl m.exp";
    value cos x = failwith "not impl m.cos";
    value sin x = failwith "not impl m.sin";
    value log x = failwith "not impl m.log";
    value to_string x =
      match gnt.val with
      [ N_int -> Mfloat_int.to_string (to_int x)
      | N_i64 -> Mfloat_i64.to_string (to_i64 x)
      | N_flo -> Mfloat_flo.to_string (to_flo x)
      | N_big -> Mfloat_big.to_string (to_big x)
      | IFDEF MPFR THEN
        N_mpf -> Mfloat_mpf.to_string (to_mpf x)
        END
      | IFDEF MPZ THEN
        N_mpz -> Mfloat_mpz.to_string (to_mpz x)
        END ]
    ;
    value of_string prec s =
      match gnt.val with
      [ N_int -> {a = F_int (Mfloat_int.of_string prec s)}
      | N_i64 -> {a = F_i64 (Mfloat_i64.of_string prec s)}
      | N_flo -> {a = F_flo (Mfloat_flo.of_string prec s)}
      | N_big -> {a = F_big (Mfloat_big.of_string prec s)}
      | IFDEF MPFR THEN
        N_mpf -> {a = F_mpf (Mfloat_mpf.of_string prec s)}
        END
      | IFDEF MPZ THEN
        N_mpz -> {a = F_mpz (Mfloat_mpz.of_string prec s)}
        END ]
    ;
    value to_float x =
      match x.a with
      [ F_int a -> Mfloat_int.to_float a
      | F_i64 a -> Mfloat_i64.to_float a
      | F_flo a -> Mfloat_flo.to_float a
      | F_big a -> Mfloat_big.to_float a
      | IFDEF MPFR THEN
        F_mpf a -> Mfloat_mpf.to_float a
        END
      | IFDEF MPZ THEN
        F_mpz a -> Mfloat_mpz.to_float a
        END ]
    ;
    value of_float f =
      match gnt.val with
      [ N_int -> {a = F_int (Mfloat_int.of_float f)}
      | N_i64 -> {a = F_i64 (Mfloat_i64.of_float f)}
      | N_flo -> {a = F_flo (Mfloat_flo.of_float f)}
      | N_big -> {a = F_big (Mfloat_big.of_float f)}
      | IFDEF MPFR THEN
        N_mpf -> {a = F_mpf (Mfloat_mpf.of_float f)}
        END
      | IFDEF MPZ THEN
        N_mpz -> {a = F_mpz (Mfloat_mpz.of_float f)}
        END ]
    ;
    value of_best_float f =
      match gnt.val with
      [ N_int -> {a = F_int (Mfloat_int.of_best_float f)}
      | N_i64 -> {a = F_i64 (Mfloat_i64.of_best_float f)}
      | N_flo -> {a = F_flo (Mfloat_flo.of_best_float f)}
      | N_big -> {a = F_big (Mfloat_big.of_best_float f)}
      | IFDEF MPFR THEN
        N_mpf -> {a = F_mpf (Mfloat_mpf.of_best_float f)}
        END
      | IFDEF MPZ THEN
        N_mpz -> {a = F_mpz (Mfloat_mpz.of_best_float f)}
        END ]
    ;
    value get_prec x =
      match gnt.val with
      [ N_int -> Mfloat_int.get_prec (to_int x)
      | N_i64 -> Mfloat_i64.get_prec (to_i64 x)
      | N_flo -> Mfloat_flo.get_prec (to_flo x)
      | N_big -> Mfloat_big.get_prec (to_big x)
      | IFDEF MPFR THEN
        N_mpf -> Mfloat_mpf.get_prec (to_mpf x)
        END
      | IFDEF MPZ THEN
        N_mpz -> Mfloat_mpz.get_prec (to_mpz x)
        END ]
    ;
    value with_prec p x =
      match gnt.val with
      [ N_int -> {a = F_int (Mfloat_int.with_prec p (to_int x))}
      | N_i64 -> {a = F_i64 (Mfloat_i64.with_prec p (to_i64 x))}
      | N_flo -> {a = F_flo (Mfloat_flo.with_prec p (to_flo x))}
      | N_big -> {a = F_big (Mfloat_big.with_prec p (to_big x))}
      | IFDEF MPFR THEN
        N_mpf -> {a = F_mpf (Mfloat_mpf.with_prec p (to_mpf x))}
        END
      | IFDEF MPZ THEN
        N_mpz -> {a = F_mpz (Mfloat_mpz.with_prec p (to_mpz x))}
        END ]
    ;
    value int_f = (fun x -> {a = F_int x}, to_int);
    value i64_f = (fun x -> {a = F_i64 x}, to_i64);
    value flo_f = (fun x -> {a = F_flo x}, to_flo);
    value big_f = (fun x -> {a = F_big x}, to_big);
    IFDEF MPFR THEN
    value mpf_f = (fun x -> {a = F_mpf x}, to_mpf);
    END;
    IFDEF MPZ THEN
    value mpz_f = (fun x -> {a = F_mpz x}, to_mpz);
    END;
    value f_gen f a b start (from_typ, to_typ) nb_it =
      let start =
        Mutil.map_option (fun (xn, yn) -> (to_typ xn, to_typ yn)) start
      in
      Mutil.map_result from_typ (f (to_typ a) (to_typ b) nb_it start)
    ;
    value mandelbrot_point a b nb_it start =
      match gnt.val with
      [ N_int -> f_gen Mfloat_int.mandelbrot_point a b start int_f nb_it
      | N_i64 -> f_gen Mfloat_i64.mandelbrot_point a b start i64_f nb_it
      | N_flo -> f_gen Mfloat_flo.mandelbrot_point a b start flo_f nb_it
      | N_big -> f_gen Mfloat_big.mandelbrot_point a b start big_f nb_it
      | IFDEF MPFR THEN
        N_mpf -> f_gen Mfloat_mpf.mandelbrot_point a b start mpf_f nb_it
        END
      | IFDEF MPZ THEN
        N_mpz -> f_gen Mfloat_mpz.mandelbrot_point a b start mpz_f nb_it
        END ]
    ;
    value mandelbrot_point3 a b nb_it start =
      match gnt.val with
      [ N_int -> f_gen Mfloat_int.mandelbrot_point3 a b start int_f nb_it
      | N_i64 -> f_gen Mfloat_i64.mandelbrot_point3 a b start i64_f nb_it
      | N_flo -> f_gen Mfloat_flo.mandelbrot_point3 a b start flo_f nb_it
      | N_big -> f_gen Mfloat_big.mandelbrot_point3 a b start big_f nb_it
      | IFDEF MPFR THEN
        N_mpf -> f_gen Mfloat_mpf.mandelbrot_point3 a b start mpf_f nb_it
        END
      | IFDEF MPZ THEN
        N_mpz -> f_gen Mfloat_mpz.mandelbrot_point3 a b start mpz_f nb_it
        END ]
    ;
    value mandelbrot_point_m m a b nb_it start =
      match gnt.val with
      [ N_int -> f_gen (Mfloat_int.mandelbrot_point_m m) a b start int_f nb_it
      | N_i64 -> f_gen (Mfloat_i64.mandelbrot_point_m m) a b start i64_f nb_it
      | N_flo -> f_gen (Mfloat_flo.mandelbrot_point_m m) a b start flo_f nb_it
      | N_big -> f_gen (Mfloat_big.mandelbrot_point_m m) a b start big_f nb_it
      | IFDEF MPFR THEN
        N_mpf -> f_gen (Mfloat_mpf.mandelbrot_point_m m) a b start mpf_f nb_it
        END
      | IFDEF MPZ THEN
        N_mpz -> f_gen (Mfloat_mpz.mandelbrot_point_m m) a b start mpz_f nb_it
        END ]
    ;
    value lambda_point a b nb_it start =
      match gnt.val with
      [ N_int -> f_gen Mfloat_int.lambda_point a b start int_f nb_it
      | N_i64 -> f_gen Mfloat_i64.lambda_point a b start i64_f nb_it
      | N_flo -> f_gen Mfloat_flo.lambda_point a b start flo_f nb_it
      | N_big -> f_gen Mfloat_big.lambda_point a b start big_f nb_it
      | IFDEF MPFR THEN
        N_mpf -> f_gen Mfloat_mpf.lambda_point a b start mpf_f nb_it
        END
      | IFDEF MPZ THEN
        N_mpz -> f_gen Mfloat_mpz.lambda_point a b start mpz_f nb_it
        END ]
    ;
    value set_num nt = gnt.val := nt;
    value get_num () = gnt.val;
    value set_default_prec p = do {
      match gnt.val with
      [ N_int -> Mfloat_int.set_default_prec p
      | N_i64 -> Mfloat_i64.set_default_prec p
      | N_flo -> Mfloat_flo.set_default_prec p
      | N_big -> Mfloat_big.set_default_prec p
      | IFDEF MPFR THEN
        N_mpf -> Mfloat_mpf.set_default_prec p
        END
      | IFDEF MPZ THEN
        N_mpz -> Mfloat_mpz.set_default_prec p
        END ];
      IFDEF MPFR THEN
        if gnt.val <> N_mpf then Mfloat_mpf.set_default_prec p else ()
      ELSE IFDEF MPZ THEN
        if gnt.val <> N_mpz then Mfloat_mpz.set_default_prec p else ()
      ELSE () END END;
    };
    value serialize x =
      match gnt.val with
      [ N_int -> Mfloat_int.serialize (to_int x)
      | N_i64 -> Mfloat_i64.serialize (to_i64 x)
      | N_flo -> Mfloat_flo.serialize (to_flo x)
      | N_big -> Mfloat_big.serialize (to_big x)
      | IFDEF MPFR THEN
        N_mpf -> Mfloat_mpf.serialize (to_mpf x)
        END
      | IFDEF MPZ THEN
        N_mpz -> Mfloat_mpz.serialize (to_mpz x)
        END ]
    ;
    value deserialize s =
      match gnt.val with
      [ N_int -> {a = F_int (Mfloat_int.deserialize s)}
      | N_i64 -> {a = F_i64 (Mfloat_i64.deserialize s)}
      | N_flo -> {a = F_flo (Mfloat_flo.deserialize s)}
      | N_big -> {a = F_big (Mfloat_big.deserialize s)}
      | IFDEF MPFR THEN
        N_mpf -> {a = F_mpf (Mfloat_mpf.deserialize s)}
        END
      | IFDEF MPZ THEN
        N_mpz -> {a = F_mpz (Mfloat_mpz.deserialize s)}
        END ]
    ;
  end
;

IFDEF MPFR THEN
  module Best = Mfloat_mpf;
ELSIFDEF MPZ THEN
  module Best = Mfloat_mpz;
ELSE
  module Best = Mfloat_flo;
END;

value trigo_of_rot rot =
  if rot = Int10.zero then None
  else
    try
      let r = Best.of_float (Int10.to_float rot /. 180. *. Mutil.pi) in
      let cosr = Best.cos r in
      let sinr = Best.sin r in
      Some (cosr, sinr)
    with
    [ Failure s -> do {
        eprintf "rotation not implemented: %s\n" s;
        flush stderr;
        None
      } ]
;

value coord_of_pos rot reduc (xc, yc) (i, j) (w, h) =
  let (xc, yc) =
    match rot with
    [ None -> (xc, yc)
    | Some (cosr, sinr) ->
        (Best.add (Best.mult xc cosr) (Best.mult yc sinr),
         Best.sub (Best.mult yc cosr) (Best.mult xc sinr)) ]
  in
  let xf = Best.add xc (Best.mult_int reduc (2 * i - w)) in
  let yf = Best.add yc (Best.mult_int reduc (h - 2 * j)) in
  match rot with
  [ None -> (xf, yf)
  | Some (cosr, sinr) ->
      (Best.sub (Best.mult xf cosr) (Best.mult yf sinr),
       Best.add (Best.mult yf cosr) (Best.mult xf sinr)) ]
;

value pos_of_coord rot reduc (xc, yc) (x, y) (w, h) =
  let (xc, yc) =
    match rot with
    [ None -> (xc, yc)
    | Some (cosr, sinr) ->
        (Best.add (Best.mult xc cosr) (Best.mult yc sinr),
         Best.sub (Best.mult yc cosr) (Best.mult xc sinr)) ]
  in
  let (x, y) =
    match rot with
    [ None -> (x, y)
    | Some (cosr, sinr) ->
        (Best.add (Best.mult x cosr) (Best.mult y sinr),
         Best.sub (Best.mult y cosr) (Best.mult x sinr)) ]
  in
  let a = Best.div (Best.sub x xc) reduc in
  let b = Best.div (Best.sub y yc) reduc in
  let i = Mutil.round ((float w +. Best.to_float a) *. 0.5) in
  let j = Mutil.round ((float h -. Best.to_float b) *. 0.5) in
  (i, j)
;

value compute_point_with_fun mandelbrot_point fctx i j start =
  let (a, b) =
    let center = (fctx.f_xc, fctx.f_yc) in
    match fctx.f_julia with
    [ None ->
        coord_of_pos fctx.f_rot fctx.f_reduc center (i, j)
          (fctx.f_w, fctx.f_h)
    | Some _ ->
        center ]
  in
  let a = M.of_best_float a in
  let b = M.of_best_float b in
  let (a, b) =
    if fctx.f_invert then
      let t = M.add (M.sqr a) (M.sqr b) in
      (M.div a t, M.neg (M.div b t))
    else
      (a, b)
  in
  let start =
    match fctx.f_julia with
    [ None -> start
    | Some (xc, yc) ->
        match start with
        [ Some _ -> start
        | None ->
            let (xn, yn) =
              coord_of_pos fctx.f_rot fctx.f_reduc (xc, yc) (i, j)
                (fctx.f_w, fctx.f_h)
            in
            Some (M.of_best_float xn, M.of_best_float yn) ] ]
  in
  mandelbrot_point a b fctx.f_nb_it start
;
