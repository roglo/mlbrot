(* $Id: mprintf.ml,v 1.11 2016/10/31 14:18:17 deraugla Exp $ *)

open Printf;

value last_len = 6;
value last_mess = Array.make last_len "";
value last_mess_i = ref 0;
value last_len = Array.length last_mess;
value nb_rep = Array.make last_len 0;

value push_mess s = do {
  last_mess.(last_mess_i.val) := s;
  last_mess_i.val := (last_mess_i.val + 1) mod last_len;
};
value mess_ind i = (last_mess_i.val + 2 * last_len - i - 1) mod last_len;
value get_mess i = last_mess.(mess_ind i);
value set_mess i v = last_mess.(mess_ind i) := v;
value ntimes_txt =
  fun
  [ 1 -> "once"
  | 2 -> "twice"
  | n -> sprintf "%d times" n ]
;
value pfx = "[mlbrot]";

value flush_n n sh = do {
  let ntm = nb_rep.(n-1) / n in
  if ntm > 0 then
    if n = 1 then
      if get_mess (1 + sh) = "\n" || ntm = 1 then
        for i = 1 to ntm do { eprintf "%s %s" pfx (get_mess (1 + sh)) }
      else
        eprintf "%s last message repeated %s\n" pfx (ntimes_txt ntm)
    else
      if ntm = 1 then
        for i = n downto 1 do { eprintf "%s %s" pfx (get_mess (i + sh)) }
      else
        eprintf "%s last %d messages repeated %s\n" pfx n (ntimes_txt ntm)
  else ();
  let r = nb_rep.(n-1) mod n in
  for i = r downto 1 do { eprintf "%s %s" pfx (get_mess (i + sh)) };
  if ntm > 0 then for i = 1 to n do { set_mess (i + sh) "" } else ();
  nb_rep.(n-1) := 0;
};

value kmprintf s = do {
  push_mess s;
  let len = String.length s in
  if s = "" || s.[len-1] <> '\n' then do {
    for i = last_len downto 1 do { flush_n i 0 };
    eprintf "%s %s" pfx s;
    flush stderr;
  }
  else
    loop 1 where rec loop i =
      if i = last_len then do {
        for i = last_len downto 1 do { flush_n i 0 };
        eprintf "%s %s" pfx s;
        flush stderr;
      }
      else if s = get_mess i then do {
        for j = last_len downto 1 do { if j <> i then flush_n j 0 else () };
        if s = get_mess i then nb_rep.(i-1) := nb_rep.(i-1) + 1
        else do {
          eprintf "%s %s" pfx s;
          flush stderr
        }
      }
      else loop (i + 1)
};

value mprintf fmt = ksprintf kmprintf fmt;

value mflush () = do {
  for i = last_len downto 1 do { flush_n i (-1) };
  flush stderr
};
