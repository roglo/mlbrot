(* $Id: palette.ml,v 1.13 2010/11/20 20:05:02 deraugla Exp $ *)

open Palette_def;
open Printf;

value cyclic_permutation n cl =
  rotate_cl (n mod List.length cl) [] cl where rec rotate_cl n rev_beg_cl =
    fun
    [ [c :: cl] ->
        if n = 0 then [c :: cl] @ List.rev rev_beg_cl
        else rotate_cl (n - 1) [c :: rev_beg_cl] cl
    | [] ->
        assert False ]
;

value nround x y =
  if x > 0 && y > 0 || x < 0 && y < 0 then (2 * x + y) / (2 * y)
  else (2 * x - y) / (2 * y)
;

value mX = 255;

value rgb_of_hsv (h, s, v) =
  let h = max 0 (min mX h) in
  let s = max 0 (min mX s) in
  let v = max 0 (min mX v) in
  let h = h * 6 in
  let i = h / mX * mX in
  let f = h - i in
  let m = v * (mX - s) / mX
  and n = v * (mX - s * f / mX) / mX
  and k = v * (mX - s * (mX - f) / mX) / mX in
  (nround
     (mX *
      (match i / mX with
       [ 0 | 6 -> v
       | 1 -> n
       | 2 -> m
       | 3 -> m
       | 4 -> k
       | 5 -> v
       | _ -> failwith "red_of_hsv" ]))
     mX,
   nround
     (mX *
      (match i / mX with
       [ 0 | 6 -> k
       | 1 -> v
       | 2 -> v
       | 3 -> n
       | 4 -> m
       | 5 -> m
       | _ -> failwith "green_of_hsv" ]))
     mX,
   nround
     (mX *
      (match i / mX with
       [ 0 | 6 -> m
       | 1 -> m
       | 2 -> k
       | 3 -> v
       | 4 -> v
       | 5 -> n
       | _ -> failwith "blue_of_hsv" ]))
     mX)
;

value create_mcolor s2 max_s v2 max_v h s v =
  let rgb =
    let s = if s2 <= 0 then max_s else s * max_s / s2 in
    let v = if v2 <= 0 then max_v else v * max_v / v2 in
    rgb_of_hsv (h, s, v)
  in
  {rgb = rgb; hsv = (h, s, v)}
;

value of_palette_def max_ds max_dv pal_def = do {
  let s1 = 0 in
  let s2 = pal_def.saturation in
  let v1 = 0 in
  let v2 = pal_def.brightness in
  let inc_s = max 1 pal_def.delta_saturation in
  let inc_v = max 1 pal_def.delta_brightness in
  let max_s = max 0 (min 255 (s2 + max_ds)) in
  let max_v = max 0 (min 255 (v2 + max_dv)) in
  loop_hue_pair [] pal_def.color_algo (List.rev pal_def.hues)
  where rec loop_hue_pair cl color_algo =
    fun
    [ [] ->
        let permut = pal_def.permutation mod (List.length cl) in
        let cl = cyclic_permutation permut cl in
        Array.of_list cl
    | [h :: hlist] ->
        match color_algo with
        [ CA_start_with_white ->
            let cl =
              (* white to color by saturation increasing *)
              let v = v2 in
              loop cl s1 where rec loop cl s =
                if s >= s2 then cl
                else
                  let c = create_mcolor s2 max_s v2 max_v h s v in
                  loop [c :: cl] (s + inc_s)
            in
            let cl =
              (* color to white by brightness decreasing *)
              let s = s2 in
              loop cl v2 where rec loop cl v =
                if v <= v1 then cl
                else
                  let c = create_mcolor s2 max_s v2 max_v h s v in
                  loop [c :: cl] (v - inc_v)
            in
            loop_hue_pair cl CA_start_with_black hlist
        | CA_start_with_black ->
            let cl =
              (* black to color by brightness increasing *)
              let s = s2 in
              loop cl v1 where rec loop cl v =
                if v >= v2 then cl
                else
                  let c = create_mcolor s2 max_s v2 max_v h s v in
                  loop [c :: cl] (v + inc_v)
            in
            let cl =
              (* color to white by saturation decreasing *)
              let v = v2 in
              loop cl s2 where rec loop cl s =
                if s <= s1 then cl
                else
                  let c = create_mcolor s2 max_s v2 max_v h s v in
                  loop [c :: cl] (s - inc_s)
            in
            loop_hue_pair cl CA_start_with_white hlist
        | CA_no_gradation ->
            let c = create_mcolor s2 max_s v2 max_v h s2 v2 in
            loop_hue_pair [c :: cl] color_algo hlist ] ]
};

value make = of_palette_def 0 0;

value parse_digit =
  fparser
  [ [: `('0'..'9' as c) :] -> Char.code c - Char.code '0' ]
;

value rec parse_int_kont i =
  fparser
  [ [: d = parse_digit; i = parse_int_kont (10 * i + d) :] -> i
  | [: :] -> i ]
;

value parse_int =
  fparser
  [ [: d = parse_digit; i = parse_int_kont d :] -> i ]
;

value rec parse_int_list =
  fparser
  [ [: `','; c = parse_int; cl = parse_int_list :] -> [c :: cl]
  | [: :] -> [] ]
;

value parse_opt_comma_bw =
  fparser
  [ [: `','; `'b' :] -> Some CA_start_with_black
  | [: `','; `'w' :] -> Some CA_start_with_white
  | [: `','; `'n' :] -> Some CA_no_gradation
  | [: :] -> None ]
;

value parse_palette_def =
  fparser
  [ [: h = parse_int; hl = parse_int_list; `'/'; permut = parse_int;
       ca = parse_opt_comma_bw; `'/'; sat = parse_int; `',';
       dsat = parse_int; `'/'; bri = parse_int; `','; dbri = parse_int;
       _ = Fstream.empty :] ->
      let ca =
        match ca with
        [ Some ca -> ca
        | None -> CA_start_with_black ]
      in
      {hues = [h :: hl]; permutation = permut; saturation = sat;
       delta_saturation = dsat; brightness = bri; delta_brightness  = dbri;
       color_algo = ca} ]
;

value random_palette (nb_hues, ds, dv) = do {
  let sat = 255 in
  let bri = 224 in
  let ca = if Random.int 2 = 0 then 'w' else 'b' in
  let hlist =
    loop [] nb_hues where rec loop hlist n =
      if n = 0 then hlist
      else
        let h = Random.int 256 in
        loop [h :: hlist] (n - 1)
  in
  let hues = String.concat "," (List.map string_of_int hlist) in
  let permut = Random.int 1000 in
  sprintf "%s/%d,%c/%d,%d/%d,%d" hues permut ca sat ds bri dv
};

value def_of_string s =
  let s =
    match s with
    [ "rainbow" -> "0,42,84,126,168,210/0,w/255,12/224,12"
    | "shifted_rainbow" -> "21,63,105,147,189,231/0,w/255,12/224,12"
    | "reverse_rainbow" -> "210,168,126,84,42,0/0,w/255,12/224,12"
    | "black_and_white" -> "0/0,w/0,0/255,255"
    | "grey" | "gray" -> "0,0/0,w/0,0/255,8"
    | "random" -> random_palette (8, 8, 8)
    | s ->
        match
          try
            Some
             (Scanf.sscanf s "random/%d/%d/%d%!"
                (fun d1 d2 d3 -> (d1, d2, d3)))
          with
          [ Scanf.Scan_failure _ | End_of_file -> None ]
        with
        [ Some x -> random_palette x
        | None -> s ] ]
  in
  match parse_palette_def (Fstream.of_string s) with
  [ Some (r, _) -> r
  | None -> failwith (sprintf "bad color palette: \"%s\"" s) ]
;

value def_to_string pd =
  sprintf "%s/%d,%s/%d,%d/%d,%d"
    (String.concat "," (List.map string_of_int pd.hues))
    pd.permutation
    (match pd.color_algo with
     [ CA_start_with_white -> "w"
     | CA_start_with_black -> "b"
     | CA_no_gradation -> "n" ])
    pd.saturation pd.delta_saturation pd.brightness pd.delta_brightness
;
