# $Id: Makefile,v 1.92 2018/08/21 16:00:52 deraugla Exp $

OCAMLC=ocamlc.opt -warn-error A-3 -g
OCAMLOPT=ocamlopt.opt -warn-error A-3
C5FLAGS=pa_macro.cmo pa_fstream.cmo $(C5DEF)
MPFR_INCLUDE=-I$(MPFR_DIR)/usr/include
MPZ_INCLUDE=-I$(MPZ_DIR)/usr/include
PLUS_CAMLP5=-I $$(camlp5 -where)

OBJS_COMM=mmagic.cmx ml_float.o $(MPZ_OBJS) $(MPFR_OBJS) i18n.cmx mutil.cmx int10.cmx mpz.cmx mfloat.cmx mprintf.cmx array2dim.cmx mfd.cmx compute.cmx palette.cmx mmisc.cmx info.cmx

GTK_OBJS_OPT=gtkn.cmx graph_gtk.cmx
GTK_LIB_OPT=lablgtk.cmxa

OPENGL_OBJS_OPT=graph_opengl.cmx
OPENGL_LIB_OPT=GL.cmxa Glut.cmxa

RT_OBJS_OPT=psbrot.cmx graph_rt.cmx
RT_LIB_OPT=librt.cmxa

OBJS=$(OBJS_COMM) $(OPENGL_OBJS) $(GTK_OBJS) $(RT_OBJS) mlbrot.cmx

OBJS_SLAVE=mmagic.cmx ml_float.o $(MPZ_OBJS) $(MPFR_OBJS) mutil.cmx int10.cmx mpz.cmx mfloat.cmx mlbrot_slave.cmx
OBJS_MKBROT=psbrot.cmx mkbrot.cmx

TARGETS=mlbrot mlbrot_slave $(MKBROT)

TEST_DIR=test `basename "$<"` = "$<" || { echo "File \"$<\" needs to be recompiled."; echo "Please run 'make' in directory '$$(dirname "$<")' first."; exit 1; }

include config/Makefile

all: $(TARGETS)

out: mlbrot.out

top: ocaml_mpz.out

ocaml_mpz.out: ml_mpz.o mpz.cmi
	cp mpz.mli mpz.ml
	ocamlc -c -pp camlp5r mpz.ml
	ocamlmktop -custom -ccopt ml_mpz.o -cclib -lgmp mpz.cmo -o $@

mmagic.ml: mcomm.mli
	camlp5r pr_r.cmo $(C5FLAGS) mcomm.mli | grep -v '$$Id' > tmpfile
	echo "value comm_magic = \"`echo "prerr_endline (Digest.to_hex (Digest.file \\\"tmpfile\\\"));;" | ocaml -noprompt 2>&1 >/dev/null`\";" > mmagic.ml

clean:
	rm -f mlbrot mlbrot_slave mkbrot mmagic.ml *.cm[iox] *.ppo *.o *.out

depend:
	export LC_ALL=C; for i in $$(ls *.ml *.mli); do camlp5r pr_depend.cmo $(C5FLAGS) $$i; done > .depend.new
	mv .depend.new .depend

mlbrot.out: $(OBJS:.cmx=.cmo)
	$(OCAMLC) -custom -g $(XLIBOPT) $(PLUS_CAMLP5) gramlib.cma nums.cma unix.cma $(OPENGL_INCL) $(OPENGL_LIB:.cmxa=.cma) $(GTK_INCL) $(GTK_LIB:.cmxa=.cma) $(RT_INCL) $(RT_LIB:.cmxa=.cma) $(OBJS:.cmx=.cmo) $(LINKFLAGS) -o $@

mlbrot: $(OBJS)
	$(OCAMLOPT) $(XLIBOPT) $(PLUS_CAMLP5) gramlib.cmxa nums.cmxa unix.cmxa $(OPENGL_INCL) $(OPENGL_LIB) $(GTK_INCL) $(GTK_LIB) $(RT_INCL) $(RT_LIB) $(OBJS) $(LINKFLAGS) -o $@

mlbrot_slave: $(OBJS_SLAVE)
	$(OCAMLOPT) nums.cmxa unix.cmxa $(OBJS_SLAVE) $(LINKFLAGS) -o $@

mkbrot: $(OBJS_MKBROT)
	$(OCAMLOPT) $(XLIBOPT) $(RT_INCL) $(RT_LIB) $(OBJS_MKBROT) $(LINKFLAGS) -o $@

ml_mpfr.o: ml_mpfr.c
	$(CC) -c -I$(OCAMLLIB) $(MPFR_INCLUDE) $<

ml_mpz.o: ml_mpz.c
	$(CC) -c -I$(OCAMLLIB) $(MPZ_INCLUDE) $<

ml_float.o: ml_float.c
	$(CC) -O3 -c -I$(OCAMLLIB) $<

.SUFFIXES: .mli .ml .cmi .cmo .cmx

.mli.cmi:
	@$(TEST_DIR)
	$(OCAMLC) -pp "camlp5r $(C5FLAGS)" $(PLUS_CAMLP5) $(RT_INCL) $(GTK_INCL) -c $<

.ml.cmo:
	@$(TEST_DIR)
	camlp5r $(C5FLAGS) $< -o $*.ppo
	$(OCAMLC) $(PLUS_CAMLP5) $(RT_INCL) $(OPENGL_INCL) $(GTK_INCL) -c -impl $*.ppo

.ml.cmx:
	@$(TEST_DIR)
	camlp5r $(C5FLAGS) $< -o $*.ppo
	$(OCAMLOPT) $(PLUS_CAMLP5) $(RT_INCL) $(OPENGL_INCL) $(GTK_INCL) -c -impl $*.ppo

.cmo.out:
	@$(TEST_DIR)
	$(OCAMLC) -custom -g $(GTK_INCL) $(GTK_LIB:.cmxa=.cma) $< $(LINKFLAGS) -o $@

include .depend
