/* $Id: ml_float.c,v 1.6 2010/11/19 13:40:57 deraugla Exp $ */

#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/alloc.h>

static value Result(long it)
{
  CAMLparam0();
  CAMLlocal1(r_v);
  r_v = caml_alloc(1, 0);
  Store_field(r_v, 0, Val_long(it));
  CAMLreturn(r_v);
}

static value LimitReached(double xn, double yn)
{
  CAMLparam0();
  CAMLlocal3(r_v, xn_v, yn_v);
  r_v = caml_alloc(3, 1);
  xn_v = caml_copy_double(xn);
  yn_v = caml_copy_double(yn);
  Store_field(r_v, 0, xn_v);
  Store_field(r_v, 1, yn_v);
  Store_field(r_v, 2, Val_long(0));
  CAMLreturn(r_v);
}

/*
      let (xn, yn) = xy_of_start 0.0 start in
      loop 0 xn yn (xn *. xn) (yn *. yn) (*False*)True
      where rec loop it xn yn sqr_xn sqr_yn testing_lim_per =
        if it >= nb_it then LimitReached xn yn 0
        else do {
          let u = xn *. yn in
          let yn = u +. u +. b in
          let xn = sqr_xn -. sqr_yn +. a in
          let sqr_xn = xn *. xn in
          let sqr_yn = yn *. yn in
          match fp.val with
          [ None -> ()
          | Some f -> f xn yn ];
          if sqr_xn +. sqr_yn >= 4. then Result it
          else loop (it + 1) xn yn sqr_xn sqr_yn testing_lim_per
*/

#define body \
    if (it >= nb_it) {\
      r_v = LimitReached(xn, yn);\
      CAMLreturn(r_v);\
    }\
    yn = xn * yn + xn * yn + b;\
    xn = sqr_xn - sqr_yn + a;\
    sqr_xn = xn * xn;\
    sqr_yn = yn * yn;\
    if (sqr_xn + sqr_yn >= 4.0) {\
      r_v = Result(it);\
      CAMLreturn(r_v);\
    }\
    it++;

value ml_flo_mandelbrot_point(value a_v, value b_v, value nb_it_v,
                              value start_v)
{
  CAMLparam4(a_v, b_v, nb_it_v, start_v);
  CAMLlocal1(r_v);
  long nb_it, it;
  double a, b, xn, yn, sqr_xn, sqr_yn;
  value xy_v;

  a = Double_val(a_v);
  b = Double_val(b_v);
  nb_it = Long_val(nb_it_v);
  it = 0;
  if (Is_block(start_v)) {
    xy_v = Field(start_v, 0);
    xn = Double_val(Field(xy_v, 0));
    yn = Double_val(Field(xy_v, 1));
  }
  else {
    xn = 0.0;
    yn = 0.0;
  }
  sqr_xn = xn * xn;
  sqr_yn = yn * yn;
  for (;;) {
    body body body body;
  }
}
