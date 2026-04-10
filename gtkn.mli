(* $Id: gtkn.mli,v 1.23 2010/12/06 20:37:52 deraugla Exp $ *)

type pixmap = [ = `drawable | `gdkpixmap ];
type window = [ = `drawable | `gdkwindow ];

type widget =
  [ AlignmentWid of Gtk.obj Gtk.alignment
  | BoxWid of Gtk.obj Gtk.box
  | ButtonWid of Gtk.obj Gtk.button
  | ButtonBoxWid of Gtk.obj Gtk.button_box
  | CheckMenuItemWid of Gtk.obj Gtk.check_menu_item
  | DrawingAreaWid of Gtk.obj Gtk.drawing_area
  | EntryWid of Gtk.obj Gtk.entry
  | FrameWid of Gtk.obj Gtk.frame
  | LabelWid of Gtk.obj Gtk.label
  | MenuBarWid of Gtk.obj Gtk.menu_bar
  | MenuItemWid of Gtk.obj Gtk.menu_item
  | NotebookWid of Gtk.obj Gtk.notebook
  | ProgressBarWid of Gtk.obj Gtk.progress_bar
  | RadioMenuItemWid of Gtk.obj Gtk.radio_menu_item
  | SpinButtonWid of Gtk.obj Gtk.spin_button
  | WindowWid of Gtk.obj Gtk.window ]
;

type drawable =
  [ PixmapDr of Gtk.obj pixmap
  | WindowDr of Gtk.obj window ]
;

type menu_item =
  [ M_button of string and  string and
      option (list Gdk.Tags.modifier * Gdk.keysym) and unit -> unit
  | M_check of string and bool and string and
      option (list Gdk.Tags.modifier * Gdk.keysym) and unit -> unit
  | M_radiobuttons of string and list (string * unit -> unit)
  | M_separator
  | M_submenu of string and list menu_item ]
;

value window : string -> Gtk.obj Gtk.window;

value window_move : Gtk.obj Gtk.window -> int -> int -> unit;
value window_add_accel_group : Gtk.obj Gtk.window -> Gtk.accel_group -> unit;

value widget_hide : widget -> unit;
value widget_realize : widget -> unit;
value widget_show : widget -> unit;
value widget_set_sensitive : widget -> bool -> unit;

value object_destroy : Gtk.obj [ > `gtk ] -> unit;

value container_add : Gtk.obj [ > `container ] -> widget -> unit;
value pack_start : Gtk.obj Gtk.box -> widget -> unit;
value pack_end : Gtk.obj Gtk.box -> widget -> unit;

value alignment :
  (float * float) -> (float * float) -> int -> int -> Gtk.obj Gtk.alignment;

value box : Gtk.Tags.orientation -> Gtk.obj Gtk.box;

value drawing_area : int -> int -> Gtk.obj Gtk.drawing_area;

value frame : string -> Gtk.obj Gtk.frame;

value label : string -> int -> int -> Gtk.obj Gtk.label;
value label_set_text : Gtk.obj Gtk.label -> string -> unit;

value menu_new : unit -> Gtk.obj Gtk.menu;
value menu_bar_new : unit -> Gtk.obj Gtk.menu_bar;
value menu_item_new_with_label : string -> Gtk.obj Gtk.menu_item;
value check_menu_item_new_with_label : string -> Gtk.obj Gtk.check_menu_item;
value radio_menu_item_new_with_label :
  option (Gtk.group Gtk.radio_menu_item) -> string ->
    Gtk.obj Gtk.radio_menu_item;
value separator_menu_item_new : unit -> Gtk.obj Gtk.menu_item;

value check_menu_item_get_active :
  Gtk.obj Gtk.check_menu_item -> bool;
value check_menu_item_set_active :
  Gtk.obj Gtk.check_menu_item -> bool -> unit;

value menu_shell_append : Gtk.obj [ > `menushell ] -> widget -> unit;
value menu_set_submenu : Gtk.obj Gtk.menu_item -> Gtk.obj [ > `menu ] -> unit;

value notebook : list (Gobject.param Gtk.notebook) -> Gtk.obj Gtk.notebook;
value notebook_append_page : Gtk.obj Gtk.notebook -> widget -> widget -> int;
value notebook_remove_page : Gtk.obj Gtk.notebook -> int -> unit;
value notebook_set_current_page : Gtk.obj Gtk.notebook -> int -> unit;
value notebook_set_show_tabs : Gtk.obj Gtk.notebook -> bool -> unit;

value progress_bar : unit -> Gtk.obj Gtk.progress_bar;
value progress_bar_set_text : Gtk.obj Gtk.progress_bar -> string -> unit;
value progress_bar_set_fraction : Gtk.obj Gtk.progress_bar -> float -> unit;

value group : Gtk.obj Gtk.radio_menu_item -> Gtk.group Gtk.radio_menu_item;

value signal_connect : Gtk.obj 'a -> GtkSignal.t 'a 'b -> 'b -> unit;

value widget_add_accelerator :
  Gtk.obj [ > `menuitem | `widget ] -> Gtk.accel_group ->
    list Gdk.Tags.modifier -> Gdk.keysym -> unit;
value widget_remove_accelerator :
  Gtk.obj [ > `menuitem | `widget ] -> Gtk.accel_group ->
    list Gdk.Tags.modifier -> Gdk.keysym -> unit;

value create_gc : drawable -> Gdk.gc;

value set_back_pixmap : Gtk.obj window -> Gtk.obj pixmap -> unit;

value widget_window : widget -> Gtk.obj window;

value pixmap_new : int -> int -> Gtk.obj pixmap;

value draw_rectangle :
  drawable -> Gdk.gc -> bool -> int -> int -> int -> int -> unit;
value draw_arc :
  drawable -> Gdk.gc -> bool -> int -> int -> int -> int -> float -> float ->
    unit;
value draw_line : drawable -> Gdk.gc -> (int * int) -> (int * int) -> unit;
value draw_lines : drawable -> Gdk.gc -> list (int * int) -> unit;
value draw_pixmap :
  drawable -> Gdk.gc -> Gtk.obj pixmap -> int -> int -> int -> int ->
    int -> int -> unit;
value draw_point :
  drawable -> Gdk.gc -> int -> int -> unit;
value draw_string :
  drawable -> Gdk.font -> Gdk.gc -> int -> int -> string -> unit;

value window_clear : Gtk.obj window -> unit;

value set_cursor : Gtk.obj window -> Gdk.cursor -> unit;

value add_events : widget -> list Gdk.Tags.event_mask -> unit;

value make_one_menu :
  Hashtbl.t string (Gtk.obj Gtk.menu_item) ->
    Hashtbl.t string (Gtk.obj Gtk.check_menu_item) ->
      Hashtbl.t string (list (Gtk.obj Gtk.radio_menu_item)) ->
        Gtk.accel_group -> (string * list menu_item) -> widget;
