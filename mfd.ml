(* $Id: mfd.ml,v 1.2 2009/08/25 09:28:33 deraugla Exp $ *)

module F = struct type t = Unix.file_descr; value compare = compare; end;

module Set = Set.Make F;
module Map = Map.Make F;

value set_length = Set.cardinal;
value map_find s m = try Some (Map.find s m) with [ Not_found -> None ];
