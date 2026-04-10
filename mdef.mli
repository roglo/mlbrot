(* $Id: mdef.mli,v 1.37 2014/01/25 10:59:59 deraugla Exp $ *)

open Mcomm;

type deque 'a =
  { bef : list 'a;
    cur : 'a;
    aft : list 'a }
;

type circumstance = [ Always | IfNeeded | Never ];

type m_info =
  { state : mutable deque state;
    auto_float : mutable bool;
    hist_size : mutable int;
    start_user_time : mutable float;
    start_real_time : mutable float;
    total_user_time : mutable float;
    total_real_time : mutable float;
    max_prec : mutable int;
    slave_name : mutable Mfd.Map.t string;
    slave_speed : mutable Mfd.Map.t (int * list int);
    free_slaves : mutable Mfd.Set.t;
    busy_slaves : mutable Mfd.Set.t;
    slave_work : mutable Mfd.Map.t (int * float);
    obsolete_slaves : mutable Mfd.Set.t;
    force_using_slaves : mutable circumstance;
    check_slaves_answers : mutable bool;
    pal_defs : mutable deque Palette_def.t;
    c_pal_def : mutable Palette_def.t;
    random_for_colors : Random.State.t;
    mode : mode;

    pts_bef_test : mutable int;
    points_between_check_user : mutable int;
    ymin : mutable int;
    ymax : mutable int;

    m3 : bool;
    mlb : option int;
    lambda : bool;
    invert : bool;
    extra_reduc : float;
    slave_hiring : option Unix.file_descr;
    init_geometry : (option (int * int) * option (int * int));
    init_seed : option int;
    init_palette : option Palette_def.t;
    init_hiring : option string;
    init_master_lazy : bool;
    c_pal : palette;
    ppm_dir : option string;
    ppm_cnt : mutable int;
    half_size : bool }
and state =
  { level : mutable float;
    num_type : mutable num_type;
    rot : mutable Int10.t;
    reduc : mutable Mfloat.Best.t;
    xc : mutable Mfloat.Best.t;
    yc : mutable Mfloat.Best.t;
    nb_it : mutable int;
    nb_it_at_start : mutable int;
    julia : mutable option (Mfloat.Best.t * Mfloat.Best.t);
    counts_array : mutable Array2dim.t (option int);
    exposing_state : mutable exposing_state;
    exposing_user_time : mutable option float;
    exposing_real_time : mutable option float;
    work : mutable work;
    master_lazy : mutable bool;
    nb_diff_answ : mutable int;
    lim_reach : mutable list (int * int * Mfloat.M.t * Mfloat.M.t) }
and exposing_state =
  [ ES_in_progress of exposing_in_progress
  | ES_terminated ]
and exposing_in_progress =
  { eip_chunk : int;
    eip_rect : list rect;
    eip_refining : bool }
and rect =
  { r_x : int; r_y : int; r_w : int; r_h : int; r_curr_y : int }
and work =
  { w_active_slaves : mutable Mfd.Map.t pts_strm;
    w_term_slaves : mutable list (pts_strm * string);
    w_master : mutable int }
and pts_strm =
  { ps_nb : mutable list int }
and palette =
  { c_tab : mutable array Palette_def.color;
    ini_max_s : int;
    ini_max_v : int;
    max_ds : mutable int;
    max_dv : mutable int }
and mode =
  [ M_display_interesting_points
  | M_batch_animate
  | M_gtk
  | M_open_gl
  | M_rt ]
;

type context =
  { c_info : m_info;
    c_isc : state;
    c_level : float;
    c_function : Mfloat.mandelbrot_fun Mfloat.M.t;
    c_pending_events : unit -> bool;
    c_fctx : function_context Mfloat.Best.t;
    c_use_slaves : bool }
;
