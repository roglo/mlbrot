(* $Id: psbrot.ml,v 1.4 2010/12/13 02:41:32 deraugla Exp $ *)

open Rt;
open Printf;

type info =
  { w : mutable int;
    h : mutable int;
    xc : mutable float;
    yc : mutable float;
    zoom : mutable float;
    max_pq : mutable int;
    show_pq : mutable int;
    verbose : mutable bool;
    c_black : color;
    c_white : color;
    c_red : color;
    c_green : color;
    c_blue : color;
    font : font }
;

value pi = 3.1415926535897932384626;
value two_pi = 6.2831853071795864769252;

value round f = truncate (f +. if f < 0.0 then -0.5 else 0.5);
value sqr x = x *. x;
value rec gcd p q = if q = 0 then p else gcd q (p mod q);

value fmin_int = -32768.0;
value fmax_int = 32767.0;

value ij_of_xy info x y =
  let xc = info.xc in
  let yc = info.yc in
  let w = info.w in
  let h = info.h in
  let zoom = info.zoom in
  let mwh = min w h in
  let dx = float (mwh / 2) *. (x -. xc) *. 0.5 *. zoom in
  let dy = float (mwh / 2) *. (y -. yc) *. 0.5 *. zoom in
  let i = w / 2 + round dx in
  let j = h / 2 - round dy in
  (i, j)
;

value xy_of_ij info i j =
  let w = info.w in
  let h = info.h in
  let mwh = min w h in
  let x = info.xc +. (float ((i - w / 2) * 4) /. float mwh /. info.zoom) in
  let y = info.yc +. (float ((h / 2 - j) * 4) /. float mwh /. info.zoom) in
  (x, y)
;

value intersect len i1 j1 i2 j2 j =
  if j1 = j2 then None
  else
    let fj = float j in
    let fi1 = float i1 in
    let fj1 = float j1 in
    let fi2 = float i2 in
    let fj2 = float j2 in
    let fi = fi1 +. (fj -. fj1) *. (fi2 -. fi1) /. (fj2 -. fj1) in
    if fi < fmin_int || fi >= fmax_int then None
    else
      let i = round fi in
      if i < 0 || i > len then None
      else Some (i, j)
;

value intersect_horiz info ij2 i1 j1 i2 j2 j =
  if ij2 <> None then ij2
  else
    match intersect info.w i1 j1 i2 j2 j with
    [ Some (i, j) -> Some (i, j)
    | None -> None ]
;

value intersect_vertic info ij2 i1 j1 i2 j2 i =
  if ij2 <> None then ij2
  else
    match intersect info.h j1 i1 j2 i2 i with
    [ Some (j, i) -> Some (i, j)
    | None -> None ]
;

value clip_segment info (i1, j1) (i2, j2) =
  if i1 <= 0 && i2 <= 0 || i1 >= info.w && i2 >= info.w ||
     j1 <= 0 && j2 <= 0 || j1 >= info.h && j2 >= info.h
  then
    None
  else
    let pt_1_in_rect = i1 >= 0 && i1 <= info.w && j1 >= 0 && j1 <= info.h in
    let pt_2_in_rect = i2 >= 0 && i2 <= info.w && j2 >= 0 && j2 <= info.h in
    let ij1 =
      if pt_1_in_rect then Some (i1, j1)
      else
        let ij1 =
          intersect_horiz info None i2 j2 i1 j1
            (if j2 < j1 then info.h else 0)
        in
        intersect_vertic info ij1 i2 j2 i1 j1 (if i2 < i1 then info.w else 0)
    in
    let ij2 =
      if pt_2_in_rect then Some (i2, j2)
      else
        let ij2 =
          intersect_horiz info None i1 j1 i2 j2
            (if j2 < j1 then 0 else info.h)
        in
        intersect_vertic info ij2 i1 j1 i2 j2 (if i2 < i1 then 0 else info.w)
    in
    match (ij1, ij2) with
    [ (Some (i1, j1), Some (i2, j2)) -> Some ((i1, j1), (i2, j2))
    | (None, None) -> None
    | (Some _, None) | (None, Some _) -> None (*assert False*) ]
;

value clip_rectangle info (i1, j1, i2, j2) =
  if i1 = i2 || j1 = j2 then
    None
  else if
    i1 - (i2 - i1) >= info.w ||
    i2 + (i2 - i1) <= 0 ||
    j1 - (j2 - j1) >= info.h ||
    j2 + (j2 - j1) <= 0
  then
    None
  else
    (* simplified version *)
    Some (i1, j1, i2, j2)
;

value draw_line drw info (i1, j1) (i2, j2) = do {
  if info.verbose then do {
    printf "rt_draw_line (%d, %d) (%d, %d)\n" i1 j1 i2 j2;
    flush stdout;
  }
  else ();
  rt_draw_line drw (i1, j1) (i2, j2)
};

value draw_point drw info (i, j) = do {
  if info.verbose then do {
    printf "rt_draw_point (%d, %d)\n" i j;
    flush stdout;
  }
  else ();
  rt_draw_point drw (i, j)
};

value draw_circle drw info r x0 y0 =
  let dtheta = 0.01 /. (r *. info.zoom) in
  let turn = two_pi +. dtheta in
  loop 0.0 0 0 where rec loop theta prev_i prev_j =
    if theta >= turn then ()
    else do {
      let x = x0 +. r *. cos theta in
      let y = y0 -. r *. sin theta in
      let (i, j) = ij_of_xy info x y in
      if theta = 0.0 then ()
      else 
        match clip_segment info (prev_i, prev_j) (i, j) with
        [ Some ((i1, j1), (i2, j2)) -> draw_line drw info (i1, j1) (i2, j2)
        | None -> () ];
      loop (theta +. dtheta) i j
    }
;

value draw_pq drw info level x y p q theta =
  if info.show_pq > 0 then
    let (i, j) = ij_of_xy info x y in
    if i >= 0 && i < info.w && j >= 0 && j < info.h then do {
      let sr = 10. /. info.zoom /. float info.w in
      rt_select_color info.c_red;
      draw_circle drw info sr x y;
      let txt =
        if info.show_pq = 1 then sprintf "%d/%d" p q
        else sprintf "%d" level
      in
      let txt_wid = rt_text_width info.font txt in
      let txt_hei = rt_font_size info.font in
      let (di, dj) =
        if theta < -. pi +. pi /. 4. then (- txt_wid - 5, txt_hei / 3 + 1)
        else if theta < -. pi /. 4. then (- txt_wid / 2, txt_hei + 2)
        else if theta < pi /. 4. then (5, 5)
        else if theta < pi -. pi /. 4. then (- txt_wid / 2, -5)
        else (- txt_wid - 5, txt_hei / 3 + 1)
      in
      rt_draw_string drw (i+di, j+dj) txt;
      rt_select_color info.c_black;
    }
    else ()
  else ()
;

value rec draw_circles_around drw info level xc yc theta0 f dnx dny =
  loop_q [] 2 where rec loop_q pq_list q =
    if q > info.max_pq then do {
      let list =
        List.sort (fun (p1, q1) (p2, q2) -> compare (p1 * q2) (p2 * q1))
          pq_list
      in
      loop list where rec loop =
        fun
        [ [(p1, q1); (p2, q2) :: list] -> do {
            let theta1 = theta0 +. two_pi *. float p1 /. float q1 in
            let theta2 = theta0 +. two_pi *. float p2 /. float q2 in
            let rho1 = f (cos theta1) in
            let rho2 = f (cos theta2) in
            let x1 = xc +. rho1 *. cos theta1 in
            let y1 = yc +. rho1 *. sin theta1 in
            let x2 = xc +. rho2 *. cos theta2 in
            let y2 = yc +. rho2 *. sin theta2 in
            let (i1, j1) = ij_of_xy info x1 y1 in
            let (i2, j2) = ij_of_xy info x2 y2 in
            if i1 = i2 && j1 = j2 then ()
            else
              match clip_segment info (i1, j1) (i2, j2) with
              [ Some (pt1, pt2) -> draw_line drw info pt1 pt2
              | None -> () ];
            loop [(p2, q2) :: list]
          }
        | [_] | [] -> () ]
    }
    else
      let pq_list =
        loop_p pq_list 1 where rec loop_p pq_list p =
          if p >= q then pq_list
          else
            let pq_list =
              if gcd p q = 1 then do {
                (* tangent point *)
                let theta = theta0 +. two_pi *. float p /. float q in
                let st = sin theta in
                let ct = cos theta in
                let rho = f ct in
                let x = xc +. rho *. ct in
                let y = yc +. rho *. st in
                (* slope of the curve normal *)
                let xn = x -. xc +. dnx st ct in
                let yn = y -. yc +. dny st ct in
                (* center and radius of the tangent circle *)
                (* note: I forgot to divide by sqrt(xn²+yn²) but the result
                    seems OK with the Mandelbrot Set; have I been lucky? *)
                let xc = x +. xn /. float (q * q) in
                let yc = y +. yn /. float (q * q) in
                let r = sqrt (sqr (x -. xc) +. sqr (y -. yc)) in
                let (i1, j1) = ij_of_xy info (xc -. r) (yc +. r) in
                let (i2, j2) = ij_of_xy info (xc +. r) (yc -. r) in
                if i1 = i2 && j1 = j2 then
                  if i1 >= 0 && i1 < info.w && j1 >= 0 && j1 < info.h then
                    draw_point drw info (i1, j1)
                  else ()
                else
                  match clip_rectangle info (i1, j1, i2, j2) with
                  [ Some _ ->
                      if abs (i2 - i1) < 10 then
                        draw_circle drw info r xc yc
                      else do {
                        let theta0 = atan2 (-. yn) (-. xn) in
(*
                        let theta1 =
                          if (level, p, q) = (0, 1, 3) then theta0 -. 0.1
                          else theta0
                        in
*)
                        let theta1 = theta0 in
(**)
                        draw_circles_around drw info (level + 1) xc yc theta1
                          (fun _ -> r) (fun _ _ -> 0.0) (fun _ _ -> 0.0);
                        draw_pq drw info level x y p q theta0;
                      }
                  | None -> () ];
                [(p, q) :: pq_list]
              }
              else pq_list
            in
            loop_p pq_list (p + 1)
      in
      loop_q pq_list (q + 1)
;

value draw_pseudobrot drw info = do {
  rt_select_color info.c_black;
  let sr = 10. /. info.zoom /. float info.w in
  draw_circle drw info sr 0.0 0.0;
  let f ct = 0.5 *. (1.0 -. ct) in
  let dnx st ct = 0.5 *. st *. st in
  let dny st ct = -0.5 *. st *. ct in
  draw_circles_around drw info 0 0.25 0.0 0.0 f dnx dny
};
