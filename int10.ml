(* $Id: int10.ml,v 1.3 2009/07/14 08:30:13 deraugla Exp $ *)

type t = int;

value base = 100;
value ln10base = 2;

value zero = 0;

value abs = Pervasives.abs;
value add = \+;
value sub = \-;
value add_int x i = x + base * i;
value sub_int x i = x - base * i;
value mul_int x i = x * i;
value div_int x i = x / i;
value mod_int x i = x mod (i * base);

value of_int i = i * base;
value of_float f =
  truncate (f *. float base +. if f < 0.0 then -0.5 else 0.5)
;
value of_string s = int_of_string s * base;

value to_float x = float x /. float base;
value to_string x =
  let sign = if x < 0 then "-" else "" in
  let x = Pervasives.abs x in
  let frac =
    let s = Printf.sprintf "%08d" (x mod base) in
    String.sub s (String.length s - ln10base) ln10base
  in
  sign ^ string_of_int (x / base) ^ "." ^ frac
;

value truncate x = x / base;
