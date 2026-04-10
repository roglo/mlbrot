(* $Id: mprintf.mli,v 1.3 2009/03/06 09:57:36 deraugla Exp $ *)

value mprintf : format4 'a unit string unit -> 'a;
value mflush : unit -> unit;
