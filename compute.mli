(* $Id: compute.mli,v 1.14 2010/11/12 11:00:25 deraugla Exp $ *)

open Mdef;

type t = 'abstract;

type pop_result =
  [ PR_some of int and int and Mcomm.result Mfloat.M.t
  | PR_none
  | PR_x_event ]
;

type simple_question = (int * int * option (Mfloat.M.t * Mfloat.M.t));

value empty : t;

value check_slave_version : Unix.file_descr -> string -> bool;

value set_environ : context -> unit;
value set_environ_for_socket : context -> Unix.file_descr -> unit;
value set_nb_it : context -> unit;

value push : context -> t -> simple_question -> t;
value pop : context -> t -> (pop_result * t);
value stack_len : t -> int;

value launch_slaves : context -> t -> t;
value hire_slave : context -> Unix.file_descr -> option Unix.file_descr;
value hire_slave_not_working :
  m_info -> Unix.file_descr -> option Unix.file_descr;
value give_work_to_new_slave : context -> t -> Unix.file_descr -> t;
value flush_remaining_answers : context -> t -> unit;
value slave_name : m_info -> Unix.file_descr -> string;
value one_socket_answer : context -> t -> float -> Unix.file_descr -> t;
value first_missing_line : context -> t -> option (int * int);
