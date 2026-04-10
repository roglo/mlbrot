/* $Id: ml_mpz.c,v 1.50 2017/12/28 10:53:01 deraugla Exp $ */

#include <stdio.h>
#include <gmp.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/custom.h>
#include <caml/alloc.h>
#include <caml/fail.h>

#define Mpz_val(v) (*((mpz_t **) Data_custom_val(v)))
#define Mpzp_mant(x) (Field(x,0))
#define Randstate_val(v) (*((gmp_randstate_t **) Data_custom_val(v)))

static mpz_t *alloc_mpz(void)
{
  mpz_t *x = (mpz_t *)malloc(sizeof(mpz_t));
  mpz_init(*x);
  return x;
}

static gmp_randstate_t *alloc_mpz_randstate(void)
{
  gmp_randstate_t *x = (gmp_randstate_t *)malloc(sizeof(gmp_randstate_t));
  return x;
}

static void finalize_mpz(value v)
{
  mpz_t *x = Mpz_val(v);
  mpz_clear(*x);
  free(x);
}

static struct custom_operations custom_mpz_ops = {
  "mpz",
  finalize_mpz,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static void finalize_mpz_randstate(value v)
{
  gmp_randstate_t *x = Randstate_val(v);
  gmp_randclear(*x);
  free(x);
}

static struct custom_operations custom_mpz_randstate_ops = {
  "mpz",
  finalize_mpz_randstate,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default
};

static value caml_alloc_mpz(mpz_t *x)
{
  value v = alloc_custom(&custom_mpz_ops, sizeof(mpz_t *), 0, 1);
  Mpz_val(v) = x;
  return v;
}

static value caml_alloc_mpz_randstate(gmp_randstate_t *x)
{
  value v = alloc_custom(&custom_mpz_randstate_ops,
                         sizeof(gmp_randstate_t *), 0, 1);
  Randstate_val(v) = x;
  return v;
}

#define value_ml_mpz_t_t_t(ml,mpz) value ml(value x_v, value y_v)\
{\
  CAMLparam2(x_v, y_v);\
  CAMLlocal1(r_v);\
  mpz_t *x, *y, *r;\
  x = Mpz_val(x_v);\
  y = Mpz_val(y_v);\
  r = alloc_mpz();\
  mpz(*r, *x, *y);\
  r_v = caml_alloc_mpz(r);\
  CAMLreturn(r_v);\
}

#define value_ml_mpz_t_t(ml,mpz) value ml(value x_v)\
{\
  CAMLparam1(x_v);\
  CAMLlocal1(r_v);\
  mpz_t *r, *x;\
  x = Mpz_val(x_v);\
  r = alloc_mpz();\
  mpz(*r, *x);\
  r_v = caml_alloc_mpz(r);\
  CAMLreturn(r_v);\
}

#define value_ml_mpz_t_int_t(ml,mpz) value ml(value x_v, value y_v)\
{\
  CAMLparam2(x_v, y_v);\
  CAMLlocal1(r_v);\
  mpz_t *x, *r;\
  long y;\
  x = Mpz_val(x_v);\
  y = Long_val(y_v);\
  r = alloc_mpz();\
  mpz(*r, *x, y);\
  r_v = caml_alloc_mpz(r);\
  CAMLreturn(r_v);\
}

value_ml_mpz_t_t(ml_mpz_abs, mpz_abs)
value_ml_mpz_t_t(ml_mpz_neg, mpz_neg)
value_ml_mpz_t_t_t(ml_mpz_add, mpz_add)
value_ml_mpz_t_int_t(ml_mpz_add_ui, mpz_add_ui)
value_ml_mpz_t_t_t(ml_mpz_sub, mpz_sub)
value_ml_mpz_t_int_t(ml_mpz_sub_ui, mpz_sub_ui)
value_ml_mpz_t_t_t(ml_mpz_mul, mpz_mul)
value_ml_mpz_t_int_t(ml_mpz_mul_si, mpz_mul_si)
value_ml_mpz_t_int_t(ml_mpz_mul_2exp, mpz_mul_2exp)
value_ml_mpz_t_t_t(ml_mpz_div_q, mpz_fdiv_q)
value_ml_mpz_t_t_t(ml_mpz_div_r, mpz_fdiv_r)

value_ml_mpz_t_int_t(ml_mpz_div_q_ui, mpz_fdiv_q_ui)

value_ml_mpz_t_int_t(ml_mpz_div_q_2exp, mpz_fdiv_q_2exp)
value_ml_mpz_t_int_t(ml_mpz_div_r_2exp, mpz_fdiv_r_2exp)

value_ml_mpz_t_int_t(ml_mpz_pow_ui, mpz_pow_ui)

value_ml_mpz_t_t(ml_mpz_sqrt, mpz_sqrt)
value_ml_mpz_t_t_t(ml_mpz_gcd, mpz_gcd)
value_ml_mpz_t_t_t(ml_mpz_and, mpz_and)
value_ml_mpz_t_t_t(ml_mpz_ior, mpz_ior)
value_ml_mpz_t_t_t(ml_mpz_xor, mpz_xor)

value ml_mpz_div_r_ui(value x_v, value i_v)
{
  CAMLparam2(x_v, i_v);
  mpz_t *x, phony;
  long i, r;
  x = Mpz_val(x_v);
  i = Long_val(i_v);
  mpz_init(phony);
  r = mpz_fdiv_r_ui(phony, *x, i);
  mpz_clear(phony);
  CAMLreturn(Val_long(r));
}

value ml_mpz_div_qr(value x_v, value y_v)
{
  CAMLparam2(x_v, y_v);
  CAMLlocal3(q_v, r_v, result_v);
  mpz_t *x, *y, *q, *r;
  x = Mpz_val(x_v);
  y = Mpz_val(y_v);
  q = alloc_mpz();
  r = alloc_mpz();
  mpz_fdiv_qr(*q, *r, *x, *y);
  q_v = caml_alloc_mpz(q);
  r_v = caml_alloc_mpz(r);
  result_v = caml_alloc(2, 0);
  Store_field(result_v, 0, q_v);
  Store_field(result_v, 1, r_v);
  CAMLreturn(result_v);
}

value ml_mpz_div_qr_ui(value x_v, value i_v)
{
  CAMLparam2(x_v, i_v);
  CAMLlocal2(q_v, result_v);
  mpz_t *x, *q, phony;
  long i, r;
  x = Mpz_val(x_v);
  i = Long_val(i_v);
  q = alloc_mpz();
  mpz_init(phony);
  r = mpz_fdiv_qr_ui(*q, phony, *x, i);
  mpz_clear(phony);
  q_v = caml_alloc_mpz(q);
  result_v = caml_alloc(2, 0);
  Store_field(result_v, 0, q_v);
  Store_field(result_v, 1, Val_long(r));
  CAMLreturn(result_v);
}

value ml_mpz_addmul(value x_v, value y_v, value z_v)
{
  CAMLparam3(x_v, y_v, z_v);
  CAMLlocal1(r_v);
  mpz_t *x, *y, *z, *r;
  x = Mpz_val(x_v);
  y = Mpz_val(y_v);
  z = Mpz_val(z_v);
  r = alloc_mpz();
  mpz_set(*r, *x);
  mpz_addmul(*r, *y, *z);
  r_v = caml_alloc_mpz(r);
  CAMLreturn(r_v);
}

value ml_mpz_addmul_ui(value x_v, value y_v, value i_v)
{
  CAMLparam3(x_v, y_v, i_v);
  CAMLlocal1(r_v);
  mpz_t *x, *y, *r;
  long i;
  x = Mpz_val(x_v);
  y = Mpz_val(y_v);
  i = Long_val(i_v);
  r = alloc_mpz();
  mpz_set(*r, *x);
  mpz_addmul_ui(*r, *y, i);
  r_v = caml_alloc_mpz(r);
  CAMLreturn(r_v);
}

value ml_mpz_submul(value x_v, value y_v, value z_v)
{
  CAMLparam3(x_v, y_v, z_v);
  CAMLlocal1(r_v);
  mpz_t *x, *y, *z, *r;
  x = Mpz_val(x_v);
  y = Mpz_val(y_v);
  z = Mpz_val(z_v);
  r = alloc_mpz();
  mpz_set(*r, *x);
  mpz_submul(*r, *y, *z);
  r_v = caml_alloc_mpz(r);
  CAMLreturn(r_v);
}

value ml_mpz_submul_ui(value x_v, value y_v, value i_v)
{
  CAMLparam3(x_v, y_v, i_v);
  CAMLlocal1(r_v);
  mpz_t *x, *y, *r;
  long i;
  x = Mpz_val(x_v);
  y = Mpz_val(y_v);
  i = Long_val(i_v);
  r = alloc_mpz();
  mpz_set(*r, *x);
  mpz_submul_ui(*r, *y, i);
  r_v = caml_alloc_mpz(r);
  CAMLreturn(r_v);
}

value ml_mpz_ui_pow_ui(value base_v, value exp_v)
{
  CAMLparam2(base_v, exp_v);
  CAMLlocal1(r_v);
  long base, exp;
  mpz_t *r;
  base = Long_val(base_v);
  exp = Long_val(exp_v);
  r = alloc_mpz();
  mpz_ui_pow_ui(*r, base, exp);
  r_v = caml_alloc_mpz(r);
  CAMLreturn(r_v);
}

value ml_mpz_pow_mod(value base_v, value exp_v, value mod_v)
{
  CAMLparam3(base_v, exp_v, mod_v);
  CAMLlocal1(r_v);
  mpz_t *base, *exp, *mod, *r;
  base = Mpz_val(base_v);
  exp = Mpz_val(exp_v);
  mod = Mpz_val(mod_v);
  r = alloc_mpz();
  mpz_powm(*r, *base, *exp, *mod);
  r_v = caml_alloc_mpz(r);
  CAMLreturn(r_v);
}

value ml_mpz_pow_mod_ui(value base_v, value exp_v, value mod_v)
{
  CAMLparam3(base_v, exp_v, mod_v);
  CAMLlocal1(r_v);
  mpz_t *base, *mod, *r;
  long exp;
  base = Mpz_val(base_v);
  exp = Long_val(exp_v);
  mod = Mpz_val(mod_v);
  r = alloc_mpz();
  mpz_powm_ui(*r, *base, exp, *mod);
  r_v = caml_alloc_mpz(r);
  CAMLreturn(r_v);
}

value ml_mpz_of_int(value x_v)
{
  CAMLparam1(x_v);
  CAMLlocal1(r_v);
  long x;
  mpz_t *r;
  x = Long_val(x_v);
  r = alloc_mpz();
  mpz_set_si(*r, x);
  r_v = caml_alloc_mpz(r);
  CAMLreturn(r_v);
}

value ml_mpz_to_int(value x_v)
{
  CAMLparam1(x_v);
  CAMLlocal1(r_v);
  mpz_t *x;
  long r;
  x = Mpz_val(x_v);
  r = mpz_get_si(*x);
  r_v = Val_long(r);
  CAMLreturn(r_v);
}

value ml_mpz_of_string(value base_v, value str_v)
{
  CAMLparam2(base_v, str_v);
  CAMLlocal1(r_v);
  mpz_t *r;
  long base = Long_val(base_v);
  char *str = String_val(str_v);
  if (base < 2 || base > 62)
    caml_invalid_argument("Mpz.of_string");
  else {
    r = alloc_mpz();
    mpz_set_str(*r, str, base);
    r_v = caml_alloc_mpz(r);
    CAMLreturn(r_v);
  }
}

value ml_mpz_to_string(value base_v, value x_v)
{
  CAMLparam2(base_v, x_v);
  CAMLlocal1(r_v);
  long base = Long_val(base_v);
  mpz_t *x;
  char *r;
  if (base > 62 || base < -36 || base > -2 && base < 2)
    caml_invalid_argument("Mpz.to_string");
  else {
    x = Mpz_val(x_v);
    r = mpz_get_str(0, base, *x);
    r_v = caml_copy_string(r);
    free(r);
    CAMLreturn(r_v);
  }
}

value ml_mpz_of_float(value x_v)
{
  CAMLparam1(x_v);
  CAMLlocal1(r_v);
  mpz_t *r;
  double x = Double_val(x_v);
  r = alloc_mpz();
  mpz_set_d(*r, x);
  r_v = caml_alloc_mpz(r);
  CAMLreturn(r_v);
}

value ml_mpz_to_float(value x_v)
{
  CAMLparam1(x_v);
  CAMLlocal1(r_v);
  mpz_t *x;
  double r;
  x = Mpz_val(x_v);
  r = mpz_get_d(*x);
  r_v = caml_copy_double(r);
  CAMLreturn(r_v);
}

value ml_mpz_compare(value x_v, value y_v)
{
  CAMLparam2(x_v, y_v);
  CAMLlocal1(r_v);
  mpz_t *x, *y;
  long r;
  x = Mpz_val(x_v);
  y = Mpz_val(y_v);
  r = mpz_cmp(*x, *y);
  r_v = Val_long(r);
  CAMLreturn(r_v);
}

value ml_mpz_compare_si(value x_v, value i_v)
{
  CAMLparam2(x_v, i_v);
  CAMLlocal1(r_v);
  mpz_t *x;
  long i, r;
  x = Mpz_val(x_v);
  i = Long_val(i_v);
  r = mpz_cmp_si(*x, i);
  r_v = Val_long(r);
  CAMLreturn(r_v);
}

value ml_mpz_size_in_base(value x_v, value base_v)
{
  CAMLparam2(x_v, base_v);
  CAMLlocal1(r_v);
  mpz_t *x;
  long base, r;
  x = Mpz_val(x_v);
  base = Long_val(base_v);
  r = mpz_sizeinbase(*x, base);
  r_v = Val_long(r);
  CAMLreturn(r_v);
}

value ml_mpz_probab_prime_p(value x_v, value reps_v)
{
  CAMLparam2(x_v, reps_v);
  CAMLlocal1(r_v);
  mpz_t *x;
  long reps, r;
  x = Mpz_val(x_v);
  reps = Long_val(reps_v);
  r = mpz_probab_prime_p(*x, reps);
  r_v = Val_long(r);
  CAMLreturn(r_v);
}

value ml_mpz_randstate_init(value seed_v)
{
  CAMLparam1(seed_v);
  CAMLlocal1(r_v);
  mpz_t *seed;
  gmp_randstate_t *r;
  seed = Mpz_val(seed_v);
  r = alloc_mpz_randstate();
  gmp_randinit_default(*r);
  gmp_randseed(*r, *seed);
  r_v = caml_alloc_mpz_randstate(r);
  CAMLreturn(r_v);
}

value ml_mpz_random(value state_v, value n_v)
{
  CAMLparam2(state_v, n_v);
  CAMLlocal1(r_v);
  gmp_randstate_t *state;
  mpz_t *n, *r;
  state = Randstate_val(state_v);
  n = Mpz_val(n_v);
  r = alloc_mpz();
  mpz_urandomm(*r, *state, *n);
  r_v = caml_alloc_mpz(r);
  CAMLreturn(r_v);
}

static int vars_initialized = 0;
static mpz_t xn, yn, xi, yi, sqr_xn, sqr_yn, u, v, w, four;
static mpz_t *vars[] =
  {&xn, &yn, &xi, &yi, &sqr_xn, &sqr_yn, &u, &v, &w, &four, 0};

static void initialize_mpz_vars(void)
{
  mpz_t **p = &vars[0];
  while (*p) mpz_init(**p++);
  vars_initialized = 1;
}

/*
  fun
  [ Some (xn, yn) -> (xn, yn)
  | None -> (def_x, 0.0) ]
*/
static void xy_of_start(long prec, long def_x_is_one_half, value start_v)
{
  value xy_v;
  mpz_t *xnp, *ynp;
  if (Is_block(start_v)) {
    xy_v = Field(start_v, 0);
    xnp = Mpz_val(Field(xy_v, 0));
    ynp = Mpz_val(Field(xy_v, 1));
    mpz_set(xn, *xnp);
    mpz_set(yn, *ynp);
  }
  else {
    if (def_x_is_one_half) {
      mpz_set_ui(xn, 1);
      mpz_mul_2exp(xn, xn, prec-1);
    }
    else mpz_set_ui(xn, 0);
    mpz_set_ui(yn, 0);
  }
}

static value Result(long it)
{
  CAMLparam0();
  CAMLlocal1(r_v);
  r_v = caml_alloc(1, 0);
  Store_field(r_v, 0, Val_long(it));
  CAMLreturn(r_v);
}

static value LimitReached(mpz_t *xn, mpz_t *yn)
{
  CAMLparam0();
  CAMLlocal3(r_v, xn_v, yn_v);
  mpz_t *xn_c, *yn_c;
  xn_c = alloc_mpz();
  yn_c = alloc_mpz();
  mpz_set(*xn_c, *xn);
  mpz_set(*yn_c, *yn);
  r_v = caml_alloc(3, 1);
  xn_v = caml_alloc_mpz(xn_c);
  yn_v = caml_alloc_mpz(yn_c);
  Store_field(r_v, 0, xn_v);
  Store_field(r_v, 1, yn_v);
  Store_field(r_v, 2, Val_long(0));
  CAMLreturn(r_v);
}

/*
  let (xn, yn) = xy_of_start 0.0 start in
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
value ml_mpz_mandelbrot_point(value prec_v, value a_v, value b_v,
                              value nb_it_v, value start_v)
{
  CAMLparam5(prec_v, a_v, b_v, nb_it_v, start_v);
  CAMLlocal1(r_v);
  long prec, nb_it, it;
  mpz_t *a, *b;
  if (! vars_initialized) initialize_mpz_vars();

  prec = Long_val(prec_v);
  a = Mpz_val(a_v);
  b = Mpz_val(b_v);
  nb_it = Long_val(nb_it_v);
  it = 0;
  xy_of_start(prec, 0, start_v);

  mpz_set_ui(four, 4);
  mpz_mul_2exp(four, four, prec*2);
  mpz_mul(sqr_xn, xn, xn);
  mpz_mul(sqr_yn, yn, yn);
  while (1) {
    if (it >= nb_it) {
      r_v = LimitReached(&xn, &yn);
      break;
    }
    else {
      /* yn := 2 xn yn + b */
      mpz_mul(u, xn, yn);
      mpz_fdiv_q_2exp(yn, u, prec-1);
      mpz_add(yn, yn, *b);
      /* xn := xn^2 - yn^2 + a */
      mpz_sub(xn, sqr_xn, sqr_yn);
      mpz_fdiv_q_2exp(xn, xn, prec);
      mpz_add(xn, xn, *a);
      /* u = xn^2 + yn^2 */
      mpz_mul(sqr_xn, xn, xn);
      mpz_mul(sqr_yn, yn, yn);
      mpz_add(u, sqr_xn, sqr_yn);

      if (mpz_cmp(u, four) >= 0) {
        r_v = Result(it);
        break;
      }
      else it++;
    }
  }

  CAMLreturn(r_v);
}

/*
  let (xn, yn) = xy_of_start 0.0 start in
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
value ml_mpz_mandelbrot_point3(value prec_v, value a_v, value b_v,
                               value nb_it_v, value start_v)
{
  CAMLparam5(prec_v, a_v, b_v, nb_it_v, start_v);
  CAMLlocal1(r_v);
  long prec, nb_it, it;
  mpz_t *a, *b;
  if (! vars_initialized) initialize_mpz_vars();

  prec = Long_val(prec_v);
  a = Mpz_val(a_v);
  b = Mpz_val(b_v);
  nb_it = Long_val(nb_it_v);
  it = 0;
  xy_of_start(prec, 0, start_v);

  mpz_set_ui(four, 4);
  mpz_mul_2exp(four, four, prec*2);
  mpz_mul(sqr_xn, xn, xn);
  mpz_mul(sqr_yn, yn, yn);
  while (1) {
    if (it >= nb_it) {
      r_v = LimitReached(&xn, &yn);
      break;
    }
    else {
      /* xn := xn *. (sqr_xn -, 3. *. sqr_yn) +. a */
      mpz_mul_si(u, sqr_yn, -3);
      mpz_add(u, sqr_xn, u);
      mpz_mul(u, xn, u);
      mpz_fdiv_q_2exp(u, u, 2*prec);
      mpz_add(xn, u, *a);
      /* yn = yn *. (3. *. sqr_xn -. sqr_yn) +. b */
      mpz_mul_si(u, sqr_xn, 3);
      mpz_sub(u, u, sqr_yn);
      mpz_mul(u, yn, u);
      mpz_fdiv_q_2exp(u, u, 2*prec);
      mpz_add(yn, u, *b);
      /* u = xn^2 + yn^2 */
      mpz_mul(sqr_xn, xn, xn);
      mpz_mul(sqr_yn, yn, yn);
      mpz_add(u, sqr_xn, sqr_yn);

      if (mpz_cmp(u, four) >= 0) {
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

  let (xn, yn) = xy_of_start 0.0 start in
  loop 0 xn yn where rec loop it xn yn =
    if it >= nb_it then LimitReached xn yn
    else
      let (xn, yn) = power xn yn m in
      let xn = xn +. a in
      let yn = yn +. b in
      if xn *. xn +. yn *. yn >= 4. then Result it
      else loop (it + 1) xn yn
*/
value ml_mpz_mandelbrot_point_m_native(value m_v, value prec_v, value a_v,
                                       value b_v, value nb_it_v, value start_v)
{
  CAMLparam5(m_v, prec_v, a_v, b_v, nb_it_v);
  CAMLxparam1(start_v);
  CAMLlocal1(r_v);
  long m, prec, nb_it, it, i;
  mpz_t *a, *b;
  if (! vars_initialized) initialize_mpz_vars();

  m = Long_val(m_v);
  prec = Long_val(prec_v);
  a = Mpz_val(a_v);
  b = Mpz_val(b_v);
  nb_it = Long_val(nb_it_v);
  it = 0;
  xy_of_start(prec, 0, start_v);

  mpz_set_ui(four, 4);
  mpz_mul_2exp(four, four, prec*2);
  while (1) {
    if (it >= nb_it) {
      r_v = LimitReached(&xn, &yn);
      break;
    }
    else {
      /* (xn, yn) = power xn yn m */
      mpz_set_ui(xi, 1);
      mpz_mul_2exp(xi, xi, prec);
      mpz_set_ui(yi, 0);
      for (i = m; i > 0; i--) {
        mpz_mul(u, xn, xi);
        mpz_mul(v, yn, yi);
        mpz_sub(w, u, v);
        mpz_mul(u, xn, yi);
        mpz_mul(v, yn, xi);
        mpz_add(yi, u, v);
        mpz_fdiv_q_2exp(yi, yi, prec);
        mpz_set(xi, w);
        mpz_fdiv_q_2exp(xi, xi, prec);
      };
      /*  xn := xn +. a */
      mpz_add(xn, xi, *a);
      /* yn = yn +. b */
      mpz_add(yn, yi, *b);
      /* u = xn^2 + yn^2 */
      mpz_mul(u, xn, xn);
      mpz_mul(v, yn, yn);
      mpz_add(u, u, v);

      if (mpz_cmp(u, four) >= 0) {
        r_v = Result(it);
        break;
      }
      else it++;
    }
  }

  CAMLreturn(r_v);
}

value ml_mpz_mandelbrot_point_m_bytecode(value *argv, int argn)
{
  ml_mpz_mandelbrot_point_m_native(argv[0], argv[1], argv[2],
                                   argv[3], argv[4], argv[5]);
}

/*
  let (xn, yn) = xy_of_start 0.5 start in
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
value ml_mpz_lambda_point(value prec_v, value a_v, value b_v,
                          value nb_it_v, value start_v)
{
  CAMLparam5(prec_v, a_v, b_v, nb_it_v, start_v);
  CAMLlocal1(r_v);
  long prec, nb_it, it;
  mpz_t *a, *b;
  if (! vars_initialized) initialize_mpz_vars();

  prec = Long_val(prec_v);
  a = Mpz_val(a_v);
  b = Mpz_val(b_v);
  nb_it = Long_val(nb_it_v);
  it = 0;
  xy_of_start(prec, 1, start_v);

  mpz_set_ui(four, 4);
  mpz_mul_2exp(four, four, prec*2);
  mpz_mul(sqr_xn, xn, xn);
  mpz_mul(sqr_yn, yn, yn);
  while (1) {
    if (it >= nb_it) {
      r_v = LimitReached(&xn, &yn);
      break;
    }
    else {
      /*  u = xn -. sqr_xn +. sqr_yn */
      mpz_sub(u, sqr_yn, sqr_xn);
      mpz_fdiv_q_2exp(u, u, prec);
      mpz_add(u, xn, u);
      /* v = yn *. (1.0 -. xn -. xn) */
      /* v = yn -. 2.0 *. xn *. yn */
      mpz_mul(v, xn, yn);
      mpz_fdiv_q_2exp(v, v, prec-1);
      mpz_sub(v, yn, v);
      /* xn = a *. u -. b *. v */
      mpz_mul(xn, *a, u);
      mpz_mul(w, *b, v);
      mpz_sub(xn, xn, w);
      mpz_fdiv_q_2exp(xn, xn, prec);
      /* yn = a *. v +. b *. u */
      mpz_mul(yn, *a, v);
      mpz_mul(w, *b, u);
      mpz_add(yn, yn, w);
      mpz_fdiv_q_2exp(yn, yn, prec);
      /* u = xn^2 + yn^2 */
      mpz_mul(sqr_xn, xn, xn);
      mpz_mul(sqr_yn, yn, yn);
      mpz_add(u, sqr_xn, sqr_yn);

      if (mpz_cmp(u, four) >= 0) {
        r_v = Result(it);
        break;
      }
      else it++;
    }
  }

  CAMLreturn(r_v);
}
