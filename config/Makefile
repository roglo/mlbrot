# Makefile.config

OCAMLLIB=/home/pps/deraugla/.opam/ocaml-4.06/lib/ocaml
OPENGL_INCL=
OPENGL_OBJS=
OPENGL_LIB=
GTK_INCL=
GTK_OBJS=
GTK_LIB=
RT_INCL=-I ../../olibrt/master/rt
RT_OBJS=$(RT_OBJS_OPT)
RT_LIB=$(RT_LIB_OPT)
MKBROT=mkbrot
MPZ_DIR=/usr/include/i386-linux-gnu
MPZ_OBJS=ml_mpz.o
MPFR_OBJS=
C5DEF= -DRT -DMPZ
LINKFLAGS= -cclib ../../olibrt/master/rt/../Xlib/libx.a -cclib -L/usr/lib/i386-linux-gnu -cclib -lX11 -cclib -L$(MPZ_DIR)/usr/lib -cclib -Wl,-rpath,$(MPZ_DIR)/usr/lib -cclib -lgmp
