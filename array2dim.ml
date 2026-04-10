(* $Id: array2dim.ml,v 1.7 2016/10/31 14:18:16 deraugla Exp $ *)

type t 'a =
  { a : array (array 'a);
    d1 : int;
    d2 : int;
    s : string }
;

value create d1 d2 v =
  {a = Array.init d1 (fun _ -> Array.make d2 v); d1 = d1 ; d2 = d2; s = ""}
;

value init d1 d2 f =
  {a = Array.init d1 (fun i -> Array.init d2 (fun j -> f i j)); d1 = d1;
   d2 = d2; s = ""}
;

value get {a = a; d1 = d1; d2 = d2} i j =
  if i < 0 || i >= d1 || j < 0 || j >= d2 then a.(0).(0)
  else a.(i).(j)
;

value set {a = a; d1 = d1; d2 = d2} i j v =
  if i < 0 || i >= d1 || j < 0 || j >= d2 then ()
  else a.(i).(j) := v
;

(*
type t 'a = (array 'a * int * int * string);

value create d1 d2 v = (Array.create (d1 * d2) v, d1, d2, "");

value init d1 d2 f =
  (Array.init (d1 * d2) (fun i -> f (i / d2) (i mod d2)), d1, d2, "")
;

value get (a, d1, d2, _) i j =
  if i < 0 || i >= d1 || j < 0 || j >= d2 then a.(0)
  else a.(i * d2 + j)
;

value set (a, d1, d2, _) i j v =
  if i < 0 || i >= d1 || j < 0 || j >= d2 then ()
  else a.(i * d2 + j) := v
;
*)

value blit {a = a1; d1 = d11; d2 = d12} {a = a2; d1 = d21; d2 = d22} =
  if d11 <> d21 || d12 <> d22 then failwith "Array2dim.blit"
  else Array.blit a1 0 a2 0 (Array.length a1)
;

value compress {a = a; d1 = d1; d2 = d2} =
  {a = [| |]; d1 = d1; d2 = d2; s = Marshal.to_string a [Marshal.No_sharing]}
;

value uncompress {d1 = d1; d2 = d2; s = s} =
  if s = "" then invalid_arg "Array2dim.uncompress"
  else {a = Marshal.from_string s 0; d1 = d1; d2 = d2; s = ""}
;

value dim1 a = a.d1;
value dim2 a = a.d2;
