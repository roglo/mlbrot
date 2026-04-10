(* $Id: gtkn.ml,v 1.27 2014/04/06 02:51:56 deraugla Exp $ *)

(* gtk without objects *)

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

value window title = do {
  GtkWindow.Window.create [Gobject.param GtkWindow.Window.P.title title]
};

value window_move w x y = GtkWindow.Window.move w ~{x; y};
value window_add_accel_group = GtkWindow.Window.add_accel_group;

value object_destroy = GtkBase.Object.destroy;

value obj_of_wid : widget -> Gtk.obj _ =
  fun
  [ AlignmentWid w -> Obj.magic w
  | BoxWid w -> Obj.magic w
  | ButtonWid w -> Obj.magic w
  | ButtonBoxWid w -> Obj.magic w
  | CheckMenuItemWid w -> Obj.magic w
  | DrawingAreaWid w -> Obj.magic w
  | EntryWid w -> Obj.magic w
  | FrameWid w -> Obj.magic w
  | LabelWid w -> Obj.magic w
  | MenuBarWid w -> Obj.magic w
  | MenuItemWid w -> Obj.magic w
  | NotebookWid w -> Obj.magic w
  | ProgressBarWid w -> Obj.magic w
  | RadioMenuItemWid w -> Obj.magic w
  | SpinButtonWid w -> Obj.magic w
  | WindowWid w -> Obj.magic w ]
;

value obj_of_draw : drawable -> Gtk.obj _ =
  fun
  [ WindowDr w -> Obj.magic w
  | PixmapDr p -> Obj.magic p ]
;

value widget_hide w = GtkBase.Widget.hide (obj_of_wid w);
value widget_realize w = GtkBase.Widget.realize (obj_of_wid w);
value widget_show w = GtkBase.Widget.show (obj_of_wid w);
value widget_set_sensitive w =
  Gobject.set GtkBase.Widget.P.sensitive (obj_of_wid w);

value container_add item w = GtkBase.Container.add item (obj_of_wid w);
value pack_start obj w =
  GtkPack.Box.pack obj (obj_of_wid w) ~{from = `START}
    ?{expand = None} ?{fill = None} ?{padding = None}
;
value pack_end obj w =
  GtkPack.Box.pack obj (obj_of_wid w) ~{from = `\END}
    ?{expand = None} ?{fill = None} ?{padding = None}
;

value alignment (xalign, xscale) (yalign, yscale) width height = do {
  let pl =
    [Gobject.param GtkBin.Alignment.P.xalign xalign;
     Gobject.param GtkBin.Alignment.P.xscale xscale;
     Gobject.param GtkBin.Alignment.P.yalign yalign;
     Gobject.param GtkBin.Alignment.P.yscale yscale]
  in
  let pl =
    if width <> 0 then
      [Gobject.param GtkBase.Widget.P.width_request width :: pl]
    else pl
  in
  let pl =
    if height <> 0 then
      [Gobject.param GtkBase.Widget.P.height_request height :: pl]
    else pl
  in
  let w = GtkBin.Alignment.create pl in
  GtkBase.Widget.show w;
  w
};

value box dir = do {
  let w = GtkPack.Box.create dir [] in
  GtkBase.Widget.show w;
  w
};

value drawing_area width height = do {
  let w = GtkMisc.DrawingArea.create [] in
  if width <> 0 || height <> 0 then
    GtkMisc.DrawingArea.size w ~{width; height}
  else ();
  GtkBase.Widget.show w;
  w
};

value frame text = do {
  let w =
    GtkBin.Frame.create
      [Gobject.param GtkBin.Frame.P.label (Some text);
       Gobject.param GtkBin.Frame.P.label_xalign 1.0]
  in
  GtkBase.Widget.show w;
  w
};

value label text xpad ypad = do {
  let w =
    GtkMisc.Label.create
      [Gobject.param GtkMisc.Label.P.label text;
       Gobject.param GtkMisc.Misc.P.xpad xpad;
       Gobject.param GtkMisc.Misc.P.ypad ypad]
  in
  GtkBase.Widget.show w;
  w
};

value label_set_text = GtkMisc.Label.set_text;

value menu_new () = GtkMenu.Menu.create [];

value menu_bar_new () = do {
  let w = GtkMenu.MenuBar.create [] in
  GtkBase.Widget.show w;
  w
};

value menu_item_new_with_label label = do {
  let w = GtkMenu.MenuItem.create ~{label} () in
  GtkBase.Widget.show w;
  w
};

value check_menu_item_new_with_label label = do {
  let w = GtkMenu.CheckMenuItem.create ~{label} () in
  GtkBase.Widget.show w;
  w
};

value check_menu_item_get_active = Gobject.get GtkMenu.CheckMenuItem.P.active;
value check_menu_item_set_active = Gobject.set GtkMenu.CheckMenuItem.P.active;

value radio_menu_item_new_with_label group label = do {
  let w = GtkMenu.RadioMenuItem.create ?{group} ~{label} () in
  GtkBase.Widget.show w;
  w
};

value separator_menu_item_new () = do {
  let w = GtkMenu.MenuItem.separator_create () in
  GtkBase.Widget.show w;
  w
};

value menu_shell_append item_menu w =
  GtkMenu.MenuShell.insert item_menu (obj_of_wid w) ~{pos = -1}
;

value menu_set_submenu = GtkMenu.MenuItem.set_submenu;

value notebook pl = do {
  let w = GtkPack.Notebook.create pl in
  GtkBase.Widget.show w;
  w
};

value notebook_append_page noteb child tab_label = do {
  GtkPack.Notebook.insert_page_menu noteb (obj_of_wid child)
    ~{tab_label = Gpointer.may_box (Some (obj_of_wid tab_label)) ~{f x = x};
      menu_label = Gpointer.optboxed None}
    ?{pos = None}
};

value notebook_remove_page = GtkPack.Notebook.remove_page;
value notebook_set_current_page = Gobject.set GtkPack.Notebook.P.page;
value notebook_set_show_tabs = Gobject.set GtkPack.Notebook.P.show_tabs;

value progress_bar () = do {
  let w = GtkRange.ProgressBar.create [] in
  GtkBase.Widget.show w;
  w
};

value progress_bar_set_text = Gobject.set GtkRange.ProgressBar.P.text;
value progress_bar_set_fraction = Gobject.set GtkRange.ProgressBar.P.fraction;

value group w = Some w;

value sgn_activate = GtkMenu.MenuItem.S.activate;

value widget_add_accelerator item accel_group modi key = do {
  GtkBase.Widget.add_accelerator item ~{sgn = sgn_activate} accel_group
    ~{key; modi; flags = [`VISIBLE]}
};

value widget_remove_accelerator item accel_group modi key = do {
  GtkBase.Widget.remove_accelerator item accel_group ~{key; modi}
};

value signal_connect item sgn callback =
  let _ : GtkSignal.id = GtkSignal.connect ~{sgn; callback} item in
  ()
;

value create_gc w = Gdk.GC.create (obj_of_draw w);

value set_back_pixmap w p = Gdk.Window.set_back_pixmap w (`PIXMAP p);

value widget_window w = GtkBase.Widget.window (obj_of_wid w);

value pixmap_new width height =
  let depth =
    let colormap = GtkBase.Widget.get_default_colormap () in
    Gdk.Visual.depth (Gdk.Color.get_visual colormap)
  in
  Gdk.Pixmap.create ?{window = None} ~{width; height; depth} ()
;

value draw_rectangle dr gc filled x y width height =
  Gdk.Draw.rectangle (obj_of_draw dr) gc ~{filled; x; y; width; height} ()
;

value draw_arc dr gc filled x y width height start angle =
  Gdk.Draw.arc (obj_of_draw dr) gc
    ~{x; y; filled; width; height; start; angle} ()
;

value draw_line dr gc (x1, y1) (x2, y2) =
  Gdk.Draw.line (obj_of_draw dr) gc ~{x = x1; y = y1; x = x2; y = y2}
;

value draw_lines dr gc list = Gdk.Draw.lines (obj_of_draw dr) gc list;

value draw_pixmap dr gc pixmap xsrc ysrc xdest ydest width height =
  Gdk.Draw.pixmap (obj_of_draw dr) gc
    ~{xsrc; ysrc; xdest; ydest; width; height} pixmap
;

value draw_point dr gc x y = Gdk.Draw.point (obj_of_draw dr) gc ~{x; y};

value draw_string drw font gc x y str =
  Gdk.Draw.string (obj_of_draw drw) font gc x y str
;

value window_clear = Gdk.Window.clear;

value set_cursor = Gdk.Window.set_cursor;

value add_events w el = GtkBase.Widget.add_events (obj_of_wid w) el;

value rec make_one_menu htb htc htr accel_group (label, menu_item_list) = do {
  let start_button = menu_item_new_with_label label in
  let item_menu = menu_new () in
  List.iter
    (fun
     [ M_button name label accel callback -> do {
         let item = GtkMenu.MenuItem.create ~{label} () in
         GtkBase.Widget.show item;
         signal_connect item sgn_activate callback;
         if name <> "" then Hashtbl.add htb name item else ();
         match accel with
         [ Some (modi, ksym) -> do {
             widget_add_accelerator item accel_group modi ksym;
           }
         | None -> () ];
         menu_shell_append item_menu (MenuItemWid item)
       }
     | M_check name active label accel callback -> do {
         let item = check_menu_item_new_with_label label in
         check_menu_item_set_active item active;
         signal_connect item sgn_activate callback;
         if name <> "" then Hashtbl.add htc name item else ();
         match accel with
         [ Some (modi, ksym) -> do {
             widget_add_accelerator item accel_group modi ksym
           }
         | None -> () ];
         menu_shell_append item_menu (CheckMenuItemWid item)
       }
     | M_radiobuttons name lcl -> do {
         match lcl with
         [ [(label, callback) :: lcl] -> do {
             let item = radio_menu_item_new_with_label None label in
             signal_connect item sgn_activate callback;
             menu_shell_append item_menu (RadioMenuItemWid item);
             let group = group item in
             let item_list =
               List.map
                 (fun (label, callback) -> do {
                    let item =
                      radio_menu_item_new_with_label (Some group) label
                    in
                    signal_connect item sgn_activate callback;
                    menu_shell_append item_menu (RadioMenuItemWid item);
                    item
                  })
               lcl
             in
             if name <> "" then Hashtbl.add htr name [item :: item_list]
             else ();
           }
         | [] -> do {
             let item = radio_menu_item_new_with_label None "" in
             menu_shell_append item_menu (RadioMenuItemWid item)
           } ]
       }
     | M_separator -> do {
         let item = separator_menu_item_new () in
         menu_shell_append item_menu (MenuItemWid item)
       }
     | M_submenu label list -> do {
         let menu_item =
           make_one_menu htb htc htr accel_group (label, list)
         in
         menu_shell_append item_menu menu_item
       } ])
    menu_item_list;
  menu_set_submenu start_button item_menu;
  MenuItemWid start_button
};
