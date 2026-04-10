(* $Id: mlbrot.ml,v 1.739 2010/11/22 09:09:13 deraugla Exp $ *)

open Mmisc;
open Mprintf;

value main () = do {
  Random.self_init ();
  Arg.parse speclist anonfun usage;
  match arg_mode.val with
  [ Mdef.M_display_interesting_points ->
      Info.display_interesting_points ()
  | Mdef.M_batch_animate ->
      Info.batch ()
  | Mdef.M_open_gl ->
      IFDEF OPENGL THEN Graph_opengl.interactive () ELSE () END
  | Mdef.M_gtk ->
      IFDEF GTK THEN Graph_gtk.interactive () ELSE () END
  | Mdef.M_rt ->
      IFDEF RT THEN Graph_rt.interactive () ELSE () END ]
};

value terminate () =
  match arg_slave_hiring.val with
  [ Some str ->
      match try Some (int_of_string str) with [ Failure _ -> None ] with
      [ Some _ -> ()
      | None -> try Sys.remove str with [ Sys_error _ -> () ] ]
  | None -> () ]
;

try main () with e -> do {
  terminate ();
  match e with
  [ Unix.Unix_error e fn _ -> do {
      mprintf "Unix error \"%s\" on '%s'\n" (Unix.error_message e) fn;
      exit 1
    }
  | e -> do {
      mprintf "%s\n" (Printexc.to_string e);
      exit 1
    } ];
};

terminate ();
