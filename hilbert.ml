(* $Id: hilbert.ml,v 1.17 2010/12/16 04:44:43 deraugla Exp $ *)

open Printf;

value size = 256 + 100;

value xconv h (x, y) = (x, h - y);

value color colormap r g b =
  Gdk.Color.alloc colormap (`RGB (256 * r) (256 * g) (256 * b))
;

value draw_path draw gc len list =
  ignore
    (List.fold_left
       (fun prev_xy (direct, (x, y), a) -> do {
          let x = 50 + x * len + a * len / 4 in
          let y = 50 + y * len + a * len / 4 in
          let a = a * len / 2 in
          let list =
            if direct then
              [(x, y); (x + a, y); (x + a, y + a); (x, y + a)]
            else
              [(x, y); (x, y + a); (x + a, y + a); (x + a, y)]
          in
          let list = List.map (xconv size) list in
          let (x, y) = List.hd list in
(*
          Gdk.Draw.arc draw gc
            ~{x = x - 5; y = y - 5; filled = True; width = 10; height = 10;
              start = 0.; angle = 360.} ();
*)
          match prev_xy with
          [ Some (prev_x, prev_y) ->
              Gdk.Draw.line draw gc ~{x = prev_x; y = prev_y; x; y}
          | None -> () ];
          Gdk.Draw.lines draw gc list;
          Some (List.hd (List.rev list))
        })
     None list : option (int * int))
;

value expose_hilbert draw n = do {
  let draw = GtkBase.Widget.window draw in
  let gc = Gdk.GC.create draw in
  let colormap = Gdk.Color.get_system_colormap () in
  let black = Gdk.Color.alloc colormap `BLACK in

  let len = 256 in
  Gdk.GC.set_foreground gc black;
  draw_path draw gc len [(False, (0, 0), 1)];

  let len = len / 2 in
  Gdk.GC.set_foreground gc (color colormap 255 0 0);
  draw_path draw gc len
    [(True, (0, 0), 1); (False, (0, 1), 1);
     (False, (1, 1), 1); (True, (2, 1), -1)];

  let len = len / 2 in
  Gdk.GC.set_foreground gc (color colormap 0 0 255);
  draw_path draw gc len
    [(False, (0, 0), 1); (True, (1, 0), 1);
     (True, (1, 1), 1); (False, (1, 2), -1);

     (True, (0, 2), 1); (False, (0, 3), 1);
     (False, (1, 3), 1); (True, (2, 3), -1);

     (True, (2, 2), 1); (False, (2, 3), 1);
     (False, (3, 3), 1); (True, (4, 3), -1);

     (False, (4, 2), -1); (True, (3, 2), -1);
     (True, (3, 1), -1); (False, (3, 0), 1)]
};

value hilbert n = do {
  ignore (GtkMain.Main.init () : string);
  let root =
    GtkWindow.Window.create [Gobject.param GtkWindow.Window.P.title "hilbert"]
  in
  let draw = GtkMisc.DrawingArea.create [] in
  GtkMisc.DrawingArea.size draw ~{width = size; height = size};
  GtkBase.Widget.show draw;
  GtkBase.Container.add root draw;
  ignore
    (GtkSignal.connect draw
        ~{sgn = GtkBase.Widget.Signals.Event.expose;
          callback ev = do { expose_hilbert draw n; False} } : GtkSignal.id);
  ignore
    (GtkSignal.connect root
        ~{sgn = GtkBase.Widget.Signals.Event.key_press;
          callback ev = do {
            let key = GdkEvent.Key.string ev in
            if String.length key = 1 &&
               Char.code key.[0] = Char.code 'Q' - 64
            then
              GMain.Main.quit ()
            else ();
            False
          }} : GtkSignal.id);
  GtkBase.Widget.show root;
  GMain.Main.main ();
};

value main () = hilbert 3;

main ();
