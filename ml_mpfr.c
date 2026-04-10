/* $Id: ml_mpfr.c,v 1.42 2009/07/21 20:01:15 deraugla Exp $ */

#include <stdio.h>
#include <string.h>
#include <mpfr.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/intext.h>
#include <caml/alloc.h>

#define Mpfr_val(v) (*((mpfr_t **) Data_custom_val(v)))

static mpfr_t *alloc_mpfr(void)
{
  mpfr_t *x = (mpfr_t *)malloc(sizeof(mpfr_t));
  mpfr_init(*x);
  return x;
}

static mpfr_t *alloc_mpfr_prec(int prec)
{
  mpfr_t *x = (mpfr_t *)malloc(sizeof(mpfr_t));
  if (prec == 0) prec = mpfr_get_default_prec();
  mpfr_init2(*x, prec);
  return x;
}

static void finalize_mpfr(value v)
{
  mpfr_t *x = Mpfr_val(v);
  mpfr_clear(*x);
  free(x);
}

static struct custom_operations custom_mpfr_ops = {
  "mpfr",
  finalize_mpfr,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static value caml_alloc_mpfr(mpfr_t *x)
{
  value v = alloc_custom(&custom_mpfr_ops, sizeof(mpfr_t *), 0, 1);
  Mpfr_val(v) = x;
  return v;
}

value ml_mpfr_with_prec(value prec_v, value x_v)
{
  CAMLparam2(prec_v, x_v);
  CAMLlocal1(r_v);
  int prec;
  mpfr_t *r, *x;
  prec = Long_val(prec_v);
  x = Mpfr_val(x_v);
  r = alloc_mpfr_prec(prec);
  mpfr_set(*r, *x, GMP_RNDN);
  r_v = caml_alloc_mpfr(r);
  CAMLreturn(r_v);
}

value ml_mpfr_neg(value x_v)
{
  CAMLparam1(x_v);
  CAMLlocal1(r_v);
  mpfr_t *r, *x;
  x = Mpfr_val(x_v);
  r = alloc_mpfr();
  mpfr_neg(*r, *x, GMP_RNDN);
  r_v = caml_alloc_mpfr(r);
  CAMLreturn(r_v);
}

value ml_mpfr_add(value x_v, value y_v)
{
  CAMLparam2(x_v, y_v);
  CAMLlocal1(r_v);
  mpfr_t *r, *x, *y;
  x = Mpfr_val(x_v);
  y = Mpfr_val(y_v);
  r = alloc_mpfr();
  mpfr_add(*r, *x, *y, GMP_RNDN);
  r_v = caml_alloc_mpfr(r);
  CAMLreturn(r_v);
}

value ml_mpfr_sub(value x_v, value y_v)
{
  CAMLparam2(x_v, y_v);
  CAMLlocal1(r_v);
  mpfr_t *r, *x, *y;
  x = Mpfr_val(x_v);
  y = Mpfr_val(y_v);
  r = alloc_mpfr();
  mpfr_sub(*r, *x, *y, GMP_RNDN);
  r_v = caml_alloc_mpfr(r);
  CAMLreturn(r_v);
}

value ml_mpfr_mul(value x_v, value y_v)
{
  CAMLparam2(x_v, y_v);
  CAMLlocal1(r_v);
  mpfr_t *r, *x, *y;
  x = Mpfr_val(x_v);
  y = Mpfr_val(y_v);
  r = alloc_mpfr();
  mpfr_mul(*r, *x, *y, GMP_RNDN);
  r_v = caml_alloc_mpfr(r);
  CAMLreturn(r_v);
}

value ml_mpfr_mul_si(value x_v, value i_v)
{
  CAMLparam2(x_v, i_v);
  CAMLlocal1(r_v);
  mpfr_t *r, *x;
  x = Mpfr_val(x_v);
  r = alloc_mpfr();
  mpfr_mul_si(*r, *x, Long_val(i_v), GMP_RNDN);
  r_v = caml_alloc_mpfr(r);
  CAMLreturn(r_v);
}

value ml_mpfr_sqr(value x_v)
{
  CAMLparam1(x_v);
  CAMLlocal1(r_v);
  mpfr_t *r, *x;
  x = Mpfr_val(x_v);
  r = alloc_mpfr();
  mpfr_sqr(*r, *x, GMP_RNDN);
  r_v = caml_alloc_mpfr(r);
  CAMLreturn(r_v);
}

value ml_mpfr_div(value x_v, value y_v)
{
  CAMLparam2(x_v, y_v);
  CAMLlocal1(r_v);
  mpfr_t *r, *x, *y;
  x = Mpfr_val(x_v);
  y = Mpfr_val(y_v);
  r = alloc_mpfr();
  mpfr_div(*r, *x, *y, GMP_RNDN);
  r_v = caml_alloc_mpfr(r);
  CAMLreturn(r_v);
}

value ml_mpfr_div_si(value x_v, value i_v)
{
  CAMLparam2(x_v, i_v);
  CAMLlocal1(r_v);
  mpfr_t *r, *x;
  x = Mpfr_val(x_v);
  r = alloc_mpfr();
  mpfr_div_si(*r, *x, Long_val(i_v), GMP_RNDN);
  r_v = caml_alloc_mpfr(r);
  CAMLreturn(r_v);
}

value ml_mpfr_cos(value x_v)
{
  CAMLparam1(x_v);
  CAMLlocal1(r_v);
  mpfr_t *r, *x;
  x = Mpfr_val(x_v);
  r = alloc_mpfr();
  mpfr_cos(*r, *x, GMP_RNDN);
  r_v = caml_alloc_mpfr(r);
  CAMLreturn(r_v);
}

value ml_mpfr_sin(value x_v)
{
  CAMLparam1(x_v);
  CAMLlocal1(r_v);
  mpfr_t *r, *x;
  x = Mpfr_val(x_v);
  r = alloc_mpfr();
  mpfr_sin(*r, *x, GMP_RNDN);
  r_v = caml_alloc_mpfr(r);
  CAMLreturn(r_v);
}

value ml_mpfr_to_string(value base_v, value x_v)
{
  CAMLparam1(x_v);
  CAMLlocal1(r_v);
  int base;
  mpfr_t *x;
  mp_exp_t exp;
  char *s;
  base = Long_val(base_v);
  x = Mpfr_val(x_v);
  s = mpfr_get_str(0, &exp, base, 0, *x, GMP_RNDN);
  r_v = caml_alloc(2, 0);
  Store_field(r_v, 0, caml_copy_string(s));
  Store_field(r_v, 1, Val_long(exp));
  mpfr_free_str(s);
  CAMLreturn(r_v);
}

value ml_mpfr_of_string(value prec_v, value s_v)
{
  CAMLparam2(s_v, prec_v);
  CAMLlocal1(r_v);
  mpfr_t *r;
  int prec = Long_val(prec_v);
  r = alloc_mpfr_prec(prec);
  mpfr_set_str(*r, String_val(s_v), 10, GMP_RNDN);
  r_v = caml_alloc_mpfr(r);
  CAMLreturn(r_v);
}

value ml_mpfr_to_float(value x_v)
{
  CAMLparam1(x_v);
  CAMLlocal1(r_v);
  mpfr_t *x;
  double r;
  x = Mpfr_val(x_v);
  r = mpfr_get_d(*x, GMP_RNDN);
  r_v = caml_copy_double(r);
  CAMLreturn(r_v);
}

value ml_mpfr_of_float(value f_v)
{
  CAMLparam1(f_v);
  CAMLlocal1(r_v);
  mpfr_t *r;
  r = alloc_mpfr();
  mpfr_set_d(*r, Double_val(f_v), GMP_RNDN);
  r_v = caml_alloc_mpfr(r);
  CAMLreturn(r_v);
}

static int vars_initialized = 0;
static mpfr_t xn, yn, xi, yi, sqr_xn, sqr_yn, u, v, w;
static mpfr_t *vars[] = {&xn, &yn, &xi, &yi, &sqr_xn, &sqr_yn, &u, &v, &w, 0};

value ml_mpfr_set_default_prec(value prec_v)
{
  CAMLparam1(prec_v);
  CAMLlocal1(x_v);
  int prec = Long_val(prec_v);
  mpfr_t **p, *x;
  if (prec >= MPFR_PREC_MIN && prec <= MPFR_PREC_MAX) {
    mpfr_set_default_prec(prec);
    if (vars_initialized) {
      for (p = &vars[0]; *p; p++) mpfr_clear(**p);
      mpfr_free_cache();
      vars_initialized = 0;
    }
  }
  CAMLreturn(0);
}

value ml_mpfr_get_prec(value x_v)
{
  CAMLparam1(x_v);
  mpfr_t *x = Mpfr_val(x_v);
  int prec = mpfr_get_prec(*x);
  CAMLreturn(Val_long(prec));
}

static void initialize_mpfr_vars(void)
{
  mpfr_t **p = &vars[0];
  while (*p) mpfr_init(**p++);
  vars_initialized = 1;
}

static value Result(int it)
{
  CAMLparam0();
  CAMLlocal1(r_v);
  r_v = caml_alloc(1, 0);
  Store_field(r_v, 0, Val_long(it));
  CAMLreturn(r_v);
}

static value LimitReached(mpfr_t *xn, mpfr_t *yn)
{
  CAMLparam0();
  CAMLlocal3(r_v, xn_v, yn_v);
  mpfr_t *xn_c, *yn_c;
  xn_c = alloc_mpfr_prec(mpfr_get_prec(*xn));
  yn_c = alloc_mpfr_prec(mpfr_get_prec(*yn));
  mpfr_set(*xn_c, *xn, GMP_RNDN);
  mpfr_set(*yn_c, *yn, GMP_RNDN);
  r_v = caml_alloc(3, 1);
  xn_v = caml_alloc_mpfr(xn_c);
  yn_v = caml_alloc_mpfr(yn_c);
  Store_field(r_v, 0, xn_v);
  Store_field(r_v, 1, yn_v);
  Store_field(r_v, 2, Val_long(0));
  CAMLreturn(r_v);
}

/*
  fun
  [ Some (xn, yn) -> (xn, yn)
  | None -> (0.0, 0.0) ]
*/
static void xy_of_start(value start_v)
{
  value xy_v;
  mpfr_t *xnp, *ynp;
  if (Is_block(start_v)) {
    xy_v = Field(start_v, 0);
    xnp = Mpfr_val(Field(xy_v, 0));
    ynp = Mpfr_val(Field(xy_v, 1));
    mpfr_set(xn, *xnp, GMP_RNDN);
    mpfr_set(yn, *ynp, GMP_RNDN);
  }
  else {
    mpfr_set_ui(xn, 0, GMP_RNDN);
    mpfr_set_ui(yn, 0, GMP_RNDN);
  }
}

/*
  let (xn, yn) = xy_of_start start in
  loop 0 xn yn (xn *. xn) (yn *. yn)
  where rec loop it xn yn sqr_xn sqr_yn =
    if it >= nb_it then LimitReached xn yn
    else
      let u = xn *. yn in
      let xn = sqr_xn -. sqr_yn +. a in
      let yn = u +. u +. b in
      let sqr_xn = xn *. xn in
      let sqr_yn = yn *. yn in
      if sqr_xn +. sqr_yn >= 4. then Result it
      else loop (it + 1) xn yn sqr_xn sqr_yn
*/
value ml_mpfr_mandelbrot_point(value a_v, value b_v, value nb_it_v,
                               value start_v)
{
  CAMLparam4(a_v, b_v, nb_it_v, start_v);
  CAMLlocal1(r_v);
  int nb_it, it;
  mpfr_t *a, *b;
  if (! vars_initialized) initialize_mpfr_vars();

  a = Mpfr_val(a_v);
  b = Mpfr_val(b_v);
  nb_it = Long_val(nb_it_v);
  it = 0;
  xy_of_start(start_v); 

  mpfr_sqr(sqr_xn, xn, GMP_RNDN);
  mpfr_sqr(sqr_yn, yn, GMP_RNDN);
  while (1) {
    if (it >= nb_it) {
      r_v = LimitReached(&xn, &yn);
      break;
    }
    else {
      mpfr_mul(u, xn, yn, GMP_RNDN);
      mpfr_sub(xn, sqr_xn, sqr_yn, GMP_RNDN);
      mpfr_add(xn, xn, *a, GMP_RNDN);
      mpfr_mul_2ui(yn, u, 1, GMP_RNDN);
      mpfr_add(yn, yn, *b, GMP_RNDN);
      mpfr_sqr(sqr_xn, xn, GMP_RNDN);
      mpfr_sqr(sqr_yn, yn, GMP_RNDN);
      mpfr_add(u, sqr_xn, sqr_yn, GMP_RNDN);

      if (mpfr_get_exp(u) >= 3) {
        r_v = Result(it);
        break;
      }
      else it++;
    }
  }

  CAMLreturn(r_v);
}

/*
  let (xn, yn) =
    match start with
    [ Some (xn, yn) -> (xn, yn)
    | None -> (0.5, 0.0) ]
  in
  loop 0 xn yn (xn *. xn) (yn *. yn)
  where rec loop it xn yn sqr_xn sqr_yn =
    if it >= nb_it then LimitReached xn yn
    else
      let u = xn -. sqr_xn +. sqr_yn in
      let v = yn *. (1.0 -. xn -. xn) in
      let xn = a *. u -. b *. v in
      let yn = a *. v +. b *. u in
      let sqr_xn = xn *. xn in
      let sqr_yn = yn *. yn in
      if sqr_xn +. sqr_yn >= 4. then Result it
      else loop (it + 1) xn yn sqr_xn sqr_yn
*/
value ml_mpfr_lambda_point(value a_v, value b_v, value nb_it_v, value start_v)
{
  CAMLparam4(a_v, b_v, nb_it_v, start_v);
  CAMLlocal1(r_v);
  value xy_v;
  int nb_it, it;
  mpfr_t *a, *b, *xnp, *ynp;
  if (! vars_initialized) initialize_mpfr_vars();

  a = Mpfr_val(a_v);
  b = Mpfr_val(b_v);
  nb_it = Long_val(nb_it_v);
  it = 0;
  if (Is_block(start_v)) {
    xy_v = Field(start_v, 0);
    xnp = Mpfr_val(Field(xy_v, 0));
    ynp = Mpfr_val(Field(xy_v, 1));
    mpfr_set(xn, *xnp, GMP_RNDN);
    mpfr_set(yn, *ynp, GMP_RNDN);
  }
  else {
    mpfr_set_d(xn, 0.5, GMP_RNDN);
    mpfr_set_d(yn, 0.0, GMP_RNDN);
  }

  mpfr_sqr(sqr_xn, xn, GMP_RNDN);
  mpfr_sqr(sqr_yn, yn, GMP_RNDN);
  while (1) {
    if (it >= nb_it) {
      r_v = LimitReached(&xn, &yn);
      break;
    }
    else {
      mpfr_sub(u, xn, sqr_xn, GMP_RNDN);
      mpfr_add(u, u, sqr_yn, GMP_RNDN);
      mpfr_set_d(v, 1.0, GMP_RNDN);
      mpfr_sub(v, v, xn, GMP_RNDN);
      mpfr_sub(v, v, xn, GMP_RNDN);
      mpfr_mul(v, yn, v, GMP_RNDN);
      mpfr_mul(xn, *a, u, GMP_RNDN);
      mpfr_mul(w, *b, v, GMP_RNDN);
      mpfr_sub(xn, xn, w, GMP_RNDN);
      mpfr_mul(yn, *a, v, GMP_RNDN);
      mpfr_mul(w, *b, u, GMP_RNDN);
      mpfr_add(yn, yn, w, GMP_RNDN);
      mpfr_sqr(sqr_xn, xn, GMP_RNDN);
      mpfr_sqr(sqr_yn, yn, GMP_RNDN);
      mpfr_add(u, sqr_xn, sqr_yn, GMP_RNDN);
      mpfr_add(u, sqr_xn, sqr_yn, GMP_RNDN);

      if (mpfr_get_exp(u) >= 3) {
        r_v = Result(it);
        break;
      }
      else it++;
    }
  }

  CAMLreturn(r_v);
}

/*
  let (xn, yn) = xy_of_start start in
  loop 0 xn yn (xn *. xn) (yn *. yn)
  where rec loop it xn yn sqr_xn sqr_yn =
    if it >= nb_it then LimitReached xn yn
    else
      let xn = xn *. (sqr_xn -. 3. *. sqr_yn) +. a in
      let yn = yn *. (3. *. sqr_xn -. sqr_yn) +. b in
      let sqr_xn = xn *. xn in
      let sqr_yn = yn *. yn in
      if sqr_xn +. sqr_yn >= 4. then Result it
      else loop (it + 1) xn yn sqr_xn sqr_yn
*/

value ml_mpfr_mandelbrot_point3(value a_v, value b_v, value nb_it_v,
                                value start_v)
{
  CAMLparam4(a_v, b_v, nb_it_v, start_v);
  CAMLlocal1(r_v);
  int nb_it, it;
  mpfr_t *a, *b;
  if (! vars_initialized) initialize_mpfr_vars();

  a = Mpfr_val(a_v);
  b = Mpfr_val(b_v);
  nb_it = Long_val(nb_it_v);
  it = 0;
  xy_of_start(start_v);

  mpfr_sqr(sqr_xn, xn, GMP_RNDN);
  mpfr_sqr(sqr_yn, yn, GMP_RNDN);
  while (1) {
    if (it >= nb_it) {
      r_v = LimitReached(&xn, &yn);
      break;
    }
    else {
      mpfr_mul_ui(u, sqr_yn, 3, GMP_RNDN);
      mpfr_sub(u, sqr_xn, u, GMP_RNDN);
      mpfr_mul(u, xn, u, GMP_RNDN);
      mpfr_add(xn, u, *a, GMP_RNDN);
      mpfr_mul_ui(u, sqr_xn, 3, GMP_RNDN);
      mpfr_sub(u, u, sqr_yn, GMP_RNDN);
      mpfr_mul(u, yn, u, GMP_RNDN);
      mpfr_add(yn, u, *b, GMP_RNDN);
      mpfr_sqr(sqr_xn, xn, GMP_RNDN);
      mpfr_sqr(sqr_yn, yn, GMP_RNDN);
      mpfr_add(u, sqr_xn, sqr_yn, GMP_RNDN);

      if (mpfr_get_exp(u) >= 3) {
        r_v = Result(it);
        break;
      }
      else it++;
    }
  }

  CAMLreturn(r_v);
}

/*
  value power x y m =
    loop 1. 0. m where rec loop xi yi m =
      if m <= 0 then (xi, yi)
      else loop (x *. xi -. y *. yi) (x *. yi +. y *. xi) (m - 1)
  ;
  value mandelbrot_point_m m a b nb_it start =
    let (xn, yn) = xy_of_start start in
    loop 0 xn yn where rec loop it xn yn =
      if it >= nb_it then LimitReached xn yn
      else
        let (xn, yn) = power xn yn m in
        let xn = xn +. a in
        let yn = yn +. b in
        if xn *. xn +. yn *. yn >= 4. then Result it
        else loop (it + 1) xn yn
  ;
*/

value ml_mpfr_mandelbrot_point_m(value m_v, value a_v, value b_v,
                                 value nb_it_v, value start_v)
{
  CAMLparam5(m_v, a_v, b_v, nb_it_v, start_v);
  CAMLlocal1(r_v);
  int m, nb_it, it, i;
  mpfr_t *a, *b;
  if (! vars_initialized) initialize_mpfr_vars();

  m = Long_val(m_v);
  a = Mpfr_val(a_v);
  b = Mpfr_val(b_v);
  nb_it = Long_val(nb_it_v);
  it = 0;
  xy_of_start(start_v);
  while (1) {
    if (it >= nb_it) {
      r_v = LimitReached(&xn, &yn);
      break;
    }
    else {
      mpfr_set_ui(xi, 1, GMP_RNDN);
      mpfr_set_ui(yi, 0, GMP_RNDN);
      for (i = m; i > 0; i--) {
        mpfr_mul(u, xn, xi, GMP_RNDN);
        mpfr_mul(v, yn, yi, GMP_RNDN);
        mpfr_sub(w, u, v, GMP_RNDN);
        mpfr_mul(u, xn, yi, GMP_RNDN);
        mpfr_mul(v, yn, xi, GMP_RNDN);
        mpfr_add(yi, u, v, GMP_RNDN);
        mpfr_set(xi, w, GMP_RNDN);
      };
      mpfr_add(xn, xi, *a, GMP_RNDN);
      mpfr_add(yn, yi, *b, GMP_RNDN);
      mpfr_sqr(u, xn, GMP_RNDN);
      mpfr_sqr(v, yn, GMP_RNDN);
      mpfr_add(u, u, v, GMP_RNDN);

      if (mpfr_get_exp(u) >= 3) {
        r_v = Result(it);
        break;
      }
      else it++;
    }
  }

  CAMLreturn(r_v);
}
