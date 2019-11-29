#include "erl_nif.h"
#include <mcl/bn_c384_256.h>
#include <string.h>

static
int enif_mcl_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
  return mclBn_init(MCL_BLS12_381, MCLBN_COMPILED_TIME_VAR);
}

#define FP_SIZE 48
#define FP2_SIZE (2 * FP_SIZE)
#define FR_SIZE 32
#define G1_SIZE 1024
#define G2_SIZE 1024
#define GT_SIZE 2048

#define FX_MODE 10
#define GX_MODE 16

#define CHECK_BINARY_ARG(X) \
  if((argc != 1) || (!enif_inspect_binary(env, argv[0], &X))){\
    return enif_make_badarg(env); \
  }

#define CHECK_BINARY_ARGS(X, Y) \
  if((argc != 2) || (!enif_inspect_binary(env, argv[0], &X)) \
                 || (!enif_inspect_binary(env, argv[1], &Y))){ \
    return enif_make_badarg(env); \
  }

#define MCL_BN_IS_X(TYPE, GET, IS_X) \
  TYPE a; \
  \
  if(argc != 1 || !GET(env, argv[0], &a)) \
    return enif_make_badarg(env); \
  \
  if(IS_X(&a)) \
    return enif_make_atom(env, "true"); \
  else \
    return enif_make_atom(env, "false");

#define MCL_BN_IS_EQUAL(TYPE, GET, IS_EQUAL) \
  TYPE a, b; \
  \
  if(argc != 2 || !GET(env, argv[0], &a) || !GET(env, argv[1], &b)) \
    return enif_make_badarg(env); \
  \
  if(IS_EQUAL(&a, &b)) \
    return enif_make_atom(env, "true"); \
  else \
    return enif_make_atom(env, "false");

static ERL_NIF_TERM mk_atom(ErlNifEnv *env, const char *str){
  ERL_NIF_TERM atom;
  if(!enif_make_existing_atom(env, str, &atom, ERL_NIF_LATIN1)){
    atom = enif_make_atom(env, str);
  }
  return atom;
}

static
ERL_NIF_TERM error_tuple(ErlNifEnv *env, char *error_atom) {
  return enif_make_tuple2(env, mk_atom(env, "error"), mk_atom(env, error_atom));
}

static
ERL_NIF_TERM ok_tuple(ErlNifEnv *env, ERL_NIF_TERM t) {
  return enif_make_tuple2(env, mk_atom(env, "ok"), t);
}

static ERL_NIF_TERM maybe_ok_tag(int ok, ErlNifEnv *env, ERL_NIF_TERM t){
  if(ok){
    return ok_tuple(env, t);
  }
  return t;
}

static ERL_NIF_TERM enif_return_fx(int ok, ErlNifEnv *env, const char *x,
                                   const char *tag, unsigned int size) {
  ErlNifBinary bin;

  if(!enif_alloc_binary(size, &bin)){
    return enif_raise_exception(env, enif_make_atom(env, "out_of_memory"));
  }

  memcpy(bin.data, x, bin.size);

  ERL_NIF_TERM res = enif_make_tuple2(env, mk_atom(env, tag),
                                           enif_make_binary(env, &bin));

  return maybe_ok_tag(ok, env, res);
}


static ERL_NIF_TERM enif_return_fr(int ok, ErlNifEnv *env, const mclBnFr *x){
  return enif_return_fx(ok, env, (const char *)x->d, "fr", FR_SIZE);
}

static ERL_NIF_TERM enif_return_fp(int ok, ErlNifEnv *env, const mclBnFp *x){
  return enif_return_fx(ok, env, (const char *)x->d, "fp", FP_SIZE);
}

static ERL_NIF_TERM enif_return_fp2(int ok, ErlNifEnv *env, const mclBnFp2 *x){
  ERL_NIF_TERM res = enif_make_tuple3(env, mk_atom(env, "fp2"),
                                           enif_return_fp(0, env, &x->d[0]),
                                           enif_return_fp(0, env, &x->d[1]));

  return maybe_ok_tag(ok, env, res);
}

static ERL_NIF_TERM enif_return_g1(int ok, ErlNifEnv *env, const mclBnG1 *x){
  ERL_NIF_TERM res = enif_make_tuple4(env, mk_atom(env, "g1"),
                                           enif_return_fp(0, env, &x->x),
                                           enif_return_fp(0, env, &x->y),
                                           enif_return_fp(0, env, &x->z));

  return maybe_ok_tag(ok, env, res);
}

static ERL_NIF_TERM enif_return_g2(int ok, ErlNifEnv *env, const mclBnG2 *x){
  ERL_NIF_TERM res = enif_make_tuple4(env, mk_atom(env, "g2"),
                                           enif_return_fp2(0, env, &x->x),
                                           enif_return_fp2(0, env, &x->y),
                                           enif_return_fp2(0, env, &x->z));

  return maybe_ok_tag(ok, env, res);
}

static ERL_NIF_TERM enif_return_gt(int ok, ErlNifEnv *env, const mclBnGT *x){
  ERL_NIF_TERM arr[13];

  arr[0] = mk_atom(env, "gt");
  for(int i = 0; i < 12; i++){
    arr[i + 1] = enif_return_fp(0, env, &x->d[i]);
  }

  ERL_NIF_TERM res = enif_make_tuple_from_array(env, arr, 13);

  return maybe_ok_tag(ok, env, res);
}

static int check_tag(ErlNifEnv *env, ERL_NIF_TERM atom, const char *tag, unsigned int sz){
  char buf[sz + 1];
  if(!enif_get_atom(env, atom, buf, sz + 1, ERL_NIF_LATIN1)
     || strncmp(tag, buf, sz)) return 0;

  return 1;
}

static int get_fr(ErlNifEnv *env, ERL_NIF_TERM arg, mclBnFr *x){
  const ERL_NIF_TERM *array;
  ErlNifBinary bin;
  int tsize;

  if(!enif_get_tuple(env, arg, &tsize, &array) || tsize != 2
      || !check_tag(env, array[0], "fr", 2)
      || !enif_inspect_binary(env, array[1], &bin)) {
    return 0;
  }

  memcpy(x->d, bin.data, bin.size);

  return 1;
}

static int get_fp(ErlNifEnv *env, ERL_NIF_TERM arg, mclBnFp *x){
  const ERL_NIF_TERM *array;
  ErlNifBinary bin;
  int tsize;

  if(!enif_get_tuple(env, arg, &tsize, &array) || tsize != 2
      || !check_tag(env, array[0], "fp", 2)
      || !enif_inspect_binary(env, array[1], &bin)) {
    return 0;
  }

  memcpy(x->d, bin.data, bin.size);

  return 1;
}

static int get_fp2(ErlNifEnv *env, ERL_NIF_TERM arg, mclBnFp2 *x){
  const ERL_NIF_TERM *array;
  int tsize;

  if(!enif_get_tuple(env, arg, &tsize, &array) || tsize != 3
      || !check_tag(env, array[0], "fp2", 3)
      || !get_fp(env, array[1], &x->d[0])
      || !get_fp(env, array[2], &x->d[1])){
    return 0;
  }

  return 1;
}

static int get_g1(ErlNifEnv *env, ERL_NIF_TERM arg, mclBnG1 *x){
  const ERL_NIF_TERM *array;
  int tsize;

  if(!enif_get_tuple(env, arg, &tsize, &array) || tsize != 4
      || !check_tag(env, array[0], "g1", 2)
      || !get_fp(env, array[1], &x->x)
      || !get_fp(env, array[2], &x->y)
      || !get_fp(env, array[3], &x->z)){
    return 0;
  }

  return 1;
}

static int get_g2(ErlNifEnv *env, ERL_NIF_TERM arg, mclBnG2 *x){
  const ERL_NIF_TERM *array;
  int tsize;

  if(!enif_get_tuple(env, arg, &tsize, &array) || tsize != 4
      || !check_tag(env, array[0], "g2", 2)
      || !get_fp2(env, array[1], &x->x)
      || !get_fp2(env, array[2], &x->y)
      || !get_fp2(env, array[3], &x->z)){
    return 0;
  }

  return 1;
}

static int get_gt(ErlNifEnv *env, ERL_NIF_TERM arg, mclBnGT *x){
  const ERL_NIF_TERM *array;
  int tsize;
  if(!enif_get_tuple(env, arg, &tsize, &array) || tsize != 13
      || !check_tag(env, array[0], "gt", 2)){
    return 0;
  }

  for(int i = 0; i < 12; i++){
    if(!get_fp(env, array[i+1], &x->d[i])) return 0;
  }

  return 1;
}

static
ERL_NIF_TERM binary_from_buf_len(ErlNifEnv *env, const char *buf, int len){
  ErlNifBinary bin;

  if (!enif_alloc_binary(len, &bin)) {
    return(error_tuple(env, "alloc_failed"));
  }

  memcpy(bin.data, buf, bin.size);

  return ok_tuple(env, enif_make_binary(env, &bin));
}

static
ERL_NIF_TERM binary_from_buf(ErlNifEnv *env, const char *buf){
  return binary_from_buf_len(env, buf, strlen(buf));
}

// -----
// Fr arithmetic functions
// -----
#define MCL_BN_UNOP(TYPE, GET, OP, RETURN) \
  TYPE x, z; \
  if(!GET(env, argv[0], &x)) \
    return enif_make_badarg(env); \
  \
  OP(&z, &x); \
  \
  return RETURN(0, env, &z);

#define MCL_BN_BINOP(TYPE, GET, OP, RETURN) \
  TYPE x, y, z; \
  if(!GET(env, argv[0], &x) || !GET(env, argv[1], &y)) \
    return enif_make_badarg(env); \
  \
  OP(&z, &x, &y); \
  \
  return RETURN(0, env, &z);

#define MCL_BN_FR_UNOP(OP)  MCL_BN_UNOP(mclBnFr, get_fr, OP, enif_return_fr)
#define MCL_BN_FR_BINOP(OP) MCL_BN_BINOP(mclBnFr, get_fr, OP, enif_return_fr)

static
ERL_NIF_TERM enif_mcl_bn_fr_neg(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FR_UNOP(mclBnFr_neg)
}

static
ERL_NIF_TERM enif_mcl_bn_fr_inv(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FR_UNOP(mclBnFr_inv)
}

static
ERL_NIF_TERM enif_mcl_bn_fr_sqr(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FR_UNOP(mclBnFr_sqr)
}

static
ERL_NIF_TERM enif_mcl_bn_fr_sqrt(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  mclBnFr x, z;

  if(!get_fr(env, argv[0], &x))
    return enif_make_badarg(env);

  if(!mclBnFr_squareRoot(&z, &x))
    return enif_return_fr(1, env, &z);
  else
    return error_tuple(env, "no_sqrt");
}

static
ERL_NIF_TERM enif_mcl_bn_fr_add(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FR_BINOP(mclBnFr_add)
}

static
ERL_NIF_TERM enif_mcl_bn_fr_sub(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FR_BINOP(mclBnFr_sub)
}

static
ERL_NIF_TERM enif_mcl_bn_fr_mul(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FR_BINOP(mclBnFr_mul)
}

static
ERL_NIF_TERM enif_mcl_bn_fr_div(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FR_BINOP(mclBnFr_div)
}

static
ERL_NIF_TERM enif_mcl_bn_fr_is_equal(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_EQUAL(mclBnFr, get_fr, mclBnFr_isEqual)
}

static
ERL_NIF_TERM enif_mcl_bn_fr_is_zero(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_X(mclBnFr, get_fr, mclBnFr_isZero)
}

static
ERL_NIF_TERM enif_mcl_bn_fr_is_one(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_X(mclBnFr, get_fr, mclBnFr_isOne)
}

static
ERL_NIF_TERM enif_mcl_bn_fr_is_odd(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_X(mclBnFr, get_fr, mclBnFr_isOdd)
}

static
ERL_NIF_TERM enif_mcl_bn_fr_is_valid(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_X(mclBnFr, get_fr, mclBnFr_isValid)
}

static
ERL_NIF_TERM enif_mcl_bn_fr_is_negative(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_X(mclBnFr, get_fr, mclBnFr_isNegative)
}

static
ERL_NIF_TERM enif_mcl_bn_fr_lagrange_interpolation(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  // We have ensured on the Erlang side that these are non-empty lists of
  // equal length.
  if(argc != 2 || !enif_is_list(env, argv[0]) || !enif_is_list(env, argv[1])){
    return enif_make_badarg(env);
  }

  unsigned int k, i = 0;
  if(!enif_get_list_length(env, argv[0], &k)) {
    return enif_make_badarg(env);
  }

  mclBnFr xs[k], ys[k], res;
  ERL_NIF_TERM exs = argv[0], ex, eys = argv[1], ey;

  while(enif_get_list_cell(env, exs, &ex, &exs) && enif_get_list_cell(env, eys, &ey, &eys)){
    if(!get_fr(env, ex, &xs[i]) || !get_fr(env, ey, &ys[i]))
      return enif_make_badarg(env);

    i++;
  }

  if(i != k) return enif_make_badarg(env);

  if(!mclBn_FrLagrangeInterpolation(&res, xs, ys, k))
    return enif_return_fr(1, env, &res);
  else
    return error_tuple(env, "bad_input");
}

static
ERL_NIF_TERM enif_mcl_bn_fr_eval_polynomial(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  mclBnFr x;

  // We have ensured on the Erlang side that these are non-empty lists of
  // equal length.
  if(argc != 2 || !enif_is_list(env, argv[0]) || !get_fr(env, argv[1], &x)){
    return enif_make_badarg(env);
  }

  unsigned int k, i = 0;
  if(!enif_get_list_length(env, argv[0], &k)) {
    return enif_make_badarg(env);
  }

  mclBnFr cs[k], res;
  ERL_NIF_TERM ecs = argv[0], ec;

  while(enif_get_list_cell(env, ecs, &ec, &ecs)){
    if(!get_fr(env, ec, &cs[i++])){
      return enif_make_badarg(env);
    }
  }

  if(i != k) return enif_make_badarg(env);

  if(!mclBn_FrEvaluatePolynomial(&res, cs, k, &x))
    return enif_return_fr(1, env, &res);
  else
    return error_tuple(env, "bad_input");
}

// -----
// Fp arithmetic functions
// -----
#define MCL_BN_FP_UNOP(OP)  MCL_BN_UNOP(mclBnFp, get_fp, OP, enif_return_fp)
#define MCL_BN_FP_BINOP(OP) MCL_BN_BINOP(mclBnFp, get_fp, OP, enif_return_fp)

static
ERL_NIF_TERM enif_mcl_bn_fp_neg(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FP_UNOP(mclBnFp_neg)
}

static
ERL_NIF_TERM enif_mcl_bn_fp_inv(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FP_UNOP(mclBnFp_inv)
}

static
ERL_NIF_TERM enif_mcl_bn_fp_sqr(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FP_UNOP(mclBnFp_sqr)
}

static
ERL_NIF_TERM enif_mcl_bn_fp_sqrt(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  mclBnFp x, z;

  if(!get_fp(env, argv[0], &x))
    return enif_make_badarg(env);

  if(!mclBnFp_squareRoot(&z, &x))
    return enif_return_fp(1, env, &z);
  else
    return error_tuple(env, "no_sqrt");
}

static
ERL_NIF_TERM enif_mcl_bn_fp_add(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FP_BINOP(mclBnFp_add)
}

static
ERL_NIF_TERM enif_mcl_bn_fp_sub(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FP_BINOP(mclBnFp_sub)
}

static
ERL_NIF_TERM enif_mcl_bn_fp_mul(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FP_BINOP(mclBnFp_mul)
}

static
ERL_NIF_TERM enif_mcl_bn_fp_div(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FP_BINOP(mclBnFp_div)
}

static
ERL_NIF_TERM enif_mcl_bn_fp_is_equal(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_EQUAL(mclBnFp, get_fp, mclBnFp_isEqual)
}

static
ERL_NIF_TERM enif_mcl_bn_fp_is_zero(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_X(mclBnFp, get_fp, mclBnFp_isZero)
}

static
ERL_NIF_TERM enif_mcl_bn_fp_is_one(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_X(mclBnFp, get_fp, mclBnFp_isOne)
}

static
ERL_NIF_TERM enif_mcl_bn_fp_is_odd(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_X(mclBnFp, get_fp, mclBnFp_isOdd)
}

static
ERL_NIF_TERM enif_mcl_bn_fp_is_valid(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_X(mclBnFp, get_fp, mclBnFp_isValid)
}

static
ERL_NIF_TERM enif_mcl_bn_fp_is_negative(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_X(mclBnFp, get_fp, mclBnFp_isNegative)
}

// -----
// Fp2 arithmetic functions
// -----
#define MCL_BN_FP2_UNOP(OP)  MCL_BN_UNOP(mclBnFp2, get_fp2, OP, enif_return_fp2)
#define MCL_BN_FP2_BINOP(OP) MCL_BN_BINOP(mclBnFp2, get_fp2, OP, enif_return_fp2)

static
ERL_NIF_TERM enif_mcl_bn_fp2_neg(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FP2_UNOP(mclBnFp2_neg)
}

static
ERL_NIF_TERM enif_mcl_bn_fp2_inv(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FP2_UNOP(mclBnFp2_inv)
}

static
ERL_NIF_TERM enif_mcl_bn_fp2_sqr(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FP2_UNOP(mclBnFp2_sqr)
}

static
ERL_NIF_TERM enif_mcl_bn_fp2_sqrt(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  mclBnFp2 x, z;

  if(!get_fp2(env, argv[0], &x))
    return enif_make_badarg(env);

  if(!mclBnFp2_squareRoot(&z, &x))
    return enif_return_fp2(1, env, &z);
  else
    return error_tuple(env, "no_sqrt");
}

static
ERL_NIF_TERM enif_mcl_bn_fp2_add(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FP2_BINOP(mclBnFp2_add)
}

static
ERL_NIF_TERM enif_mcl_bn_fp2_sub(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FP2_BINOP(mclBnFp2_sub)
}

static
ERL_NIF_TERM enif_mcl_bn_fp2_mul(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FP2_BINOP(mclBnFp2_mul)
}

static
ERL_NIF_TERM enif_mcl_bn_fp2_div(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FP2_BINOP(mclBnFp2_div)
}

static
ERL_NIF_TERM enif_mcl_bn_fp2_is_equal(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_EQUAL(mclBnFp2, get_fp2, mclBnFp2_isEqual)
}

static
ERL_NIF_TERM enif_mcl_bn_fp2_is_zero(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_X(mclBnFp2, get_fp2, mclBnFp2_isZero)
}

static
ERL_NIF_TERM enif_mcl_bn_fp2_is_one(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_X(mclBnFp2, get_fp2, mclBnFp2_isOne)
}

// -----
// G1 arithmetic functions
// -----
#define MCL_BN_G1_UNOP(OP)  MCL_BN_UNOP(mclBnG1, get_g1, OP, enif_return_g1)
#define MCL_BN_G1_BINOP(OP) MCL_BN_BINOP(mclBnG1, get_g1, OP, enif_return_g1)

static
ERL_NIF_TERM enif_mcl_bn_g1_neg(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_G1_UNOP(mclBnG1_neg)
}

static
ERL_NIF_TERM enif_mcl_bn_g1_dbl(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_G1_UNOP(mclBnG1_dbl)
}

static
ERL_NIF_TERM enif_mcl_bn_g1_normalize(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_G1_UNOP(mclBnG1_normalize)
}

static
ERL_NIF_TERM enif_mcl_bn_g1_add(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_G1_BINOP(mclBnG1_add)
}

static
ERL_NIF_TERM enif_mcl_bn_g1_sub(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_G1_BINOP(mclBnG1_sub)
}

static
ERL_NIF_TERM enif_mcl_bn_g1_mul(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  mclBnFr b;
  mclBnG1 a, ab;

  if(!get_g1(env, argv[0], &a) || !get_fr(env, argv[1], &b))
    return enif_make_badarg(env);

  mclBnG1_mul(&ab, &a, &b);

  return enif_return_g1(0, env, &ab);
}

static
ERL_NIF_TERM enif_mcl_bn_g1_mul_vec(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  ERL_NIF_TERM g1s, g1;
  ERL_NIF_TERM frs, fr;
  mclBnFr b;
  mclBnG1 a, ab, res;

  // We have ensured on the Erlang side that these are non-empty lists of
  // equal length.
  if(argc != 2 || !enif_is_list(env, argv[0]) || !enif_is_list(env, argv[1])){
    return enif_make_badarg(env);
  }

  if(!enif_get_list_cell(env, argv[0], &g1, &g1s)
      || !enif_get_list_cell(env, argv[1], &fr, &frs)){
    return enif_make_badarg(env);
  }

  if(!get_g1(env, g1, &a) || !get_fr(env, fr, &b))
    return enif_make_badarg(env);

  mclBnG1_mul(&res, &a, &b);

  while(enif_get_list_cell(env, g1s, &g1, &g1s) && enif_get_list_cell(env, frs, &fr, &frs)){
    if(!get_g1(env, g1, &a) || !get_fr(env, fr, &b))
      return enif_make_badarg(env);

    mclBnG1_mul(&ab, &a, &b);
    mclBnG1_add(&res, &ab, &res);
  }

  return enif_return_g1(1, env, &res);
}

static
ERL_NIF_TERM enif_mcl_bn_g1_hash_and_map_to(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  ErlNifBinary in;
  mclBnG1 res;

  CHECK_BINARY_ARG(in);

  if(mclBnG1_hashAndMapTo(&res, in.data, in.size)) {
    return error_tuple(env, "hash_and_map_to_failed");
  }

  return enif_return_g1(1, env, &res);
}

static
ERL_NIF_TERM enif_mcl_bn_g1_is_equal(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_EQUAL(mclBnG1, get_g1, mclBnG1_isEqual)
}

static
ERL_NIF_TERM enif_mcl_bn_g1_is_zero(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_X(mclBnG1, get_g1, mclBnG1_isZero)
}

static
ERL_NIF_TERM enif_mcl_bn_g1_is_valid(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_X(mclBnG1, get_g1, mclBnG1_isValid)
}

static
ERL_NIF_TERM enif_mcl_bn_g1_is_valid_order(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_X(mclBnG1, get_g1, mclBnG1_isValidOrder)
}

static
ERL_NIF_TERM enif_mcl_bn_g1_lagrange_interpolation(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  // We have ensured on the Erlang side that these are non-empty lists of
  // equal length.
  if(argc != 2 || !enif_is_list(env, argv[0]) || !enif_is_list(env, argv[1])){
    return enif_make_badarg(env);
  }

  unsigned int k, i = 0;
  if(!enif_get_list_length(env, argv[0], &k)) {
    return enif_make_badarg(env);
  }

  mclBnFr xs[k];
  mclBnG1 ys[k], res;
  ERL_NIF_TERM exs = argv[0], ex, eys = argv[1], ey;

  while(enif_get_list_cell(env, exs, &ex, &exs) && enif_get_list_cell(env, eys, &ey, &eys)){
    if(!get_fr(env, ex, &xs[i]) || !get_g1(env, ey, &ys[i])){
      return enif_make_badarg(env);
    }

    i++;
  }

  if(i != k) return enif_make_badarg(env);

  if(!mclBn_G1LagrangeInterpolation(&res, xs, ys, k))
    return enif_return_g1(1, env, &res);
  else
    return error_tuple(env, "bad_input");
}

static
ERL_NIF_TERM enif_mcl_bn_g1_eval_polynomial(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  mclBnFr x;

  // We have ensured on the Erlang side that these are non-empty lists of
  // equal length.
  if(argc != 2 || !enif_is_list(env, argv[0]) || !get_fr(env, argv[1], &x)){
    return enif_make_badarg(env);
  }

  unsigned int k, i = 0;
  if(!enif_get_list_length(env, argv[0], &k)) {
    return enif_make_badarg(env);
  }

  mclBnG1 cs[k], res;
  ERL_NIF_TERM ecs = argv[0], ec;

  while(enif_get_list_cell(env, ecs, &ec, &ecs)){
    if(!get_g1(env, ec, &cs[i++])){
      return enif_make_badarg(env);
    }
  }

  if(i != k) return enif_make_badarg(env);

  if(!mclBn_G1EvaluatePolynomial(&res, cs, k, &x))
    return enif_return_g1(1, env, &res);
  else
    return error_tuple(env, "bad_input");
}

// -----
// G2 arithmetic functions
// -----
#define MCL_BN_G2_UNOP(OP)  MCL_BN_UNOP(mclBnG2, get_g2, OP, enif_return_g2)
#define MCL_BN_G2_BINOP(OP) MCL_BN_BINOP(mclBnG2, get_g2, OP, enif_return_g2)

static
ERL_NIF_TERM enif_mcl_bn_g2_neg(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_G2_UNOP(mclBnG2_neg)
}

static
ERL_NIF_TERM enif_mcl_bn_g2_dbl(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_G2_UNOP(mclBnG2_dbl)
}

static
ERL_NIF_TERM enif_mcl_bn_g2_normalize(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_G2_UNOP(mclBnG2_normalize)
}

static
ERL_NIF_TERM enif_mcl_bn_g2_add(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_G2_BINOP(mclBnG2_add)
}

static
ERL_NIF_TERM enif_mcl_bn_g2_sub(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_G2_BINOP(mclBnG2_sub)
}

static
ERL_NIF_TERM enif_mcl_bn_g2_mul(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  mclBnFr b;
  mclBnG2 a, ab;

  if(!get_g2(env, argv[0], &a) || !get_fr(env, argv[1], &b))
    return enif_make_badarg(env);

  mclBnG2_mul(&ab, &a, &b);

  return enif_return_g2(0, env, &ab);
}

static
ERL_NIF_TERM enif_mcl_bn_g2_mul_vec(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  ERL_NIF_TERM g2s, g2;
  ERL_NIF_TERM frs, fr;
  mclBnFr b;
  mclBnG2 a, ab, res;

  // We have ensured on the Erlang side that these are non-empty lists of
  // equal length.
  if(argc != 2 || !enif_is_list(env, argv[0]) || !enif_is_list(env, argv[1])){
    return enif_make_badarg(env);
  }

  if(!enif_get_list_cell(env, argv[0], &g2, &g2s)
      || !enif_get_list_cell(env, argv[1], &fr, &frs)){
    return enif_make_badarg(env);
  }

  if(!get_g2(env, g2, &a) || !get_fr(env, fr, &b))
    return enif_make_badarg(env);

  mclBnG2_mul(&res, &a, &b);

  while(enif_get_list_cell(env, g2s, &g2, &g2s) && enif_get_list_cell(env, frs, &fr, &frs)){
    if(!get_g2(env, g2, &a) || !get_fr(env, fr, &b))
      return enif_make_badarg(env);

    mclBnG2_mul(&ab, &a, &b);
    mclBnG2_add(&res, &ab, &res);
  }

  return enif_return_g2(1, env, &res);
}

static
ERL_NIF_TERM enif_mcl_bn_g2_hash_and_map_to(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  ErlNifBinary in;
  mclBnG2 res;

  CHECK_BINARY_ARG(in);

  if(mclBnG2_hashAndMapTo(&res, in.data, in.size)) {
    return error_tuple(env, "hash_and_map_to_failed");
  }

  return enif_return_g2(1, env, &res);
}

static
ERL_NIF_TERM enif_mcl_bn_g2_is_equal(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_EQUAL(mclBnG2, get_g2, mclBnG2_isEqual)
}

static
ERL_NIF_TERM enif_mcl_bn_g2_is_zero(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_X(mclBnG2, get_g2, mclBnG2_isZero)
}

static
ERL_NIF_TERM enif_mcl_bn_g2_is_valid(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_X(mclBnG2, get_g2, mclBnG2_isValid)
}

static
ERL_NIF_TERM enif_mcl_bn_g2_is_valid_order(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_X(mclBnG2, get_g2, mclBnG2_isValidOrder)
}

static
ERL_NIF_TERM enif_mcl_bn_g2_lagrange_interpolation(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  // We have ensured on the Erlang side that these are non-empty lists of
  // equal length.
  if(argc != 2 || !enif_is_list(env, argv[0]) || !enif_is_list(env, argv[1])){
    return enif_make_badarg(env);
  }

  unsigned int k, i = 0;
  if(!enif_get_list_length(env, argv[0], &k)) {
    return enif_make_badarg(env);
  }

  mclBnFr xs[k];
  mclBnG2 ys[k], res;
  ERL_NIF_TERM exs = argv[0], ex, eys = argv[1], ey;

  while(enif_get_list_cell(env, exs, &ex, &exs) && enif_get_list_cell(env, eys, &ey, &eys)){
    if(!get_fr(env, ex, &xs[i]) || !get_g2(env, ey, &ys[i])){
      return enif_make_badarg(env);
    }

    i++;
  }

  if(i != k) return enif_make_badarg(env);

  if(!mclBn_G2LagrangeInterpolation(&res, xs, ys, k))
    return enif_return_g2(1, env, &res);
  else
    return error_tuple(env, "bad_input");
}

static
ERL_NIF_TERM enif_mcl_bn_g2_eval_polynomial(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  mclBnFr x;

  // We have ensured on the Erlang side that these are non-empty lists of
  // equal length.
  if(argc != 2 || !enif_is_list(env, argv[0]) || !get_fr(env, argv[1], &x)){
    return enif_make_badarg(env);
  }

  unsigned int k, i = 0;
  if(!enif_get_list_length(env, argv[0], &k)) {
    return enif_make_badarg(env);
  }

  mclBnG2 cs[k], res;
  ERL_NIF_TERM ecs = argv[0], ec;

  while(enif_get_list_cell(env, ecs, &ec, &ecs)){
    if(!get_g2(env, ec, &cs[i++])){
      return enif_make_badarg(env);
    }
  }

  if(i != k) return enif_make_badarg(env);

  if(!mclBn_G2EvaluatePolynomial(&res, cs, k, &x))
    return enif_return_g2(1, env, &res);
  else
    return error_tuple(env, "bad_input");
}

// -----
// Gt arithmetic functions
// -----
#define MCL_BN_GT_UNOP(OP)  MCL_BN_UNOP(mclBnGT, get_gt, OP, enif_return_gt)
#define MCL_BN_GT_BINOP(OP) MCL_BN_BINOP(mclBnGT, get_gt, OP, enif_return_gt)

static
ERL_NIF_TERM enif_mcl_bn_gt_neg(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_GT_UNOP(mclBnGT_neg)
}

static
ERL_NIF_TERM enif_mcl_bn_gt_inv(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_GT_UNOP(mclBnGT_inv)
}

static
ERL_NIF_TERM enif_mcl_bn_gt_inv_generic(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_GT_UNOP(mclBnGT_invGeneric)
}

static
ERL_NIF_TERM enif_mcl_bn_gt_sqr(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_GT_UNOP(mclBnGT_sqr)
}

static
ERL_NIF_TERM enif_mcl_bn_gt_add(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_GT_BINOP(mclBnGT_add)
}

static
ERL_NIF_TERM enif_mcl_bn_gt_sub(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_GT_BINOP(mclBnGT_sub)
}

static
ERL_NIF_TERM enif_mcl_bn_gt_mul(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_GT_BINOP(mclBnGT_mul)
}

static
ERL_NIF_TERM enif_mcl_bn_gt_div(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_GT_BINOP(mclBnGT_div)
}

static
ERL_NIF_TERM enif_mcl_bn_gt_pow(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  mclBnFr b;
  mclBnGT a, ab;

  if(!get_gt(env, argv[0], &a) || !get_fr(env, argv[1], &b))
    return enif_make_badarg(env);

  mclBnGT_pow(&ab, &a, &b);

  return enif_return_gt(0, env, &ab);
}

static
ERL_NIF_TERM enif_mcl_bn_gt_pow_vec(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  ERL_NIF_TERM gts, gt;
  ERL_NIF_TERM frs, fr;
  mclBnFr b;
  mclBnGT a, ab, res;

  // We have ensured on the Erlang side that these are non-empty lists of
  // equal length.
  if(argc != 2 || !enif_is_list(env, argv[0]) || !enif_is_list(env, argv[1])){
    return enif_make_badarg(env);
  }

  if(!enif_get_list_cell(env, argv[0], &gt, &gts)
      || !enif_get_list_cell(env, argv[1], &fr, &frs)){
    return enif_make_badarg(env);
  }

  if(!get_gt(env, gt, &a) || !get_fr(env, fr, &b))
    return enif_make_badarg(env);

  mclBnGT_pow(&res, &a, &b);

  while(enif_get_list_cell(env, gts, &gt, &gts) && enif_get_list_cell(env, frs, &fr, &frs)){
    if(!get_gt(env, gt, &a) || !get_fr(env, fr, &b))
      return enif_make_badarg(env);

    mclBnGT_pow(&ab, &a, &b);
    mclBnGT_mul(&res, &ab, &res);
  }

  return enif_return_gt(1, env, &res);
}

static
ERL_NIF_TERM enif_mcl_bn_gt_is_equal(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_EQUAL(mclBnGT, get_gt, mclBnGT_isEqual)
}

static
ERL_NIF_TERM enif_mcl_bn_gt_is_zero(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_X(mclBnGT, get_gt, mclBnGT_isZero)
}

static
ERL_NIF_TERM enif_mcl_bn_gt_is_one(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_IS_X(mclBnGT, get_gt, mclBnGT_isOne)
}

// -----
// Pairing
// -----
static
ERL_NIF_TERM enif_mcl_bn_miller_loop(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  mclBnG1 a;
  mclBnG2 b;
  mclBnGT e;

  if(argc != 2 || !get_g1(env, argv[0], &a) || !get_g2(env, argv[1], &b))
    return enif_make_badarg(env);

  mclBn_millerLoop(&e, &a, &b);

  return enif_return_gt(0, env, &e);
}

static
ERL_NIF_TERM enif_mcl_bn_miller_loop_vec(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  ERL_NIF_TERM g1s, g1;
  ERL_NIF_TERM g2s, g2;
  mclBnG1 a;
  mclBnG2 b;
  mclBnGT e, res;

  // We have ensured on the Erlang side that these are non-empty lists of
  // equal length.
  if(argc != 2 || !enif_is_list(env, argv[0]) || !enif_is_list(env, argv[1])){
    return enif_make_badarg(env);
  }

  if(!enif_get_list_cell(env, argv[0], &g1, &g1s) || !enif_get_list_cell(env, argv[1], &g2, &g2s)){
    return enif_make_badarg(env);
  }

  if(!get_g1(env, g1, &a) || !get_g2(env, g2, &b))
    return enif_make_badarg(env);

  mclBn_pairing(&res, &a, &b);

  while(enif_get_list_cell(env, g1s, &g1, &g1s) && enif_get_list_cell(env, g2s, &g2, &g2s)){
    if(!get_g1(env, g1, &a) || !get_g2(env, g2, &b))
      return enif_make_badarg(env);

    mclBn_pairing(&e, &a, &b);
    mclBnGT_mul(&res, &e, &res);
  }

  return enif_return_gt(1, env, &res);
}

static
ERL_NIF_TERM enif_mcl_bn_final_exp(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  mclBnGT p;
  mclBnGT e;

  if(argc != 1 || !get_gt(env, argv[0], &p))
    return enif_make_badarg(env);

  mclBn_finalExp(&e, &p);

  return enif_return_gt(0, env, &e);
}

static
ERL_NIF_TERM enif_mcl_bn_pairing(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  mclBnG1 a;
  mclBnG2 b;
  mclBnGT e;

  if(argc != 2 || !get_g1(env, argv[0], &a) || !get_g2(env, argv[1], &b))
    return enif_make_badarg(env);

  mclBn_pairing(&e, &a, &b);

  return enif_return_gt(0, env, &e);
}

// -----
// hash_of/map_to
// -----
static
ERL_NIF_TERM enif_mcl_bn_fr_hash_of(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  ErlNifBinary in;
  mclBnFr x;

  CHECK_BINARY_ARG(in);

  mclBnFr_setHashOf(&x, in.data, in.size);

  return enif_return_fr(0, env, &x);
}

static
ERL_NIF_TERM enif_mcl_bn_fp_hash_of(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  ErlNifBinary in;
  mclBnFp x;

  CHECK_BINARY_ARG(in);

  mclBnFp_setHashOf(&x, in.data, in.size);

  return enif_return_fp(0, env, &x);
}

static
ERL_NIF_TERM enif_mcl_bn_fp2_hash_of(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  ErlNifBinary in;
  mclBnFp2 x;

  CHECK_BINARY_ARG(in);

  mclBnFp_setHashOf(&x.d[0], in.data, in.size);
  mclBnFp_clear(&x.d[1]);

  return enif_return_fp2(0, env, &x);
}

static
ERL_NIF_TERM enif_mcl_bn_fp_map_to_g1(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  mclBnG1 g1;
  mclBnFp fp;

  if(!get_fp(env, argv[0], &fp))
    return enif_make_badarg(env);

  if(mclBnFp_mapToG1(&g1, &fp)) {
    return error_tuple(env, "map_to_failed");
  }

  return enif_return_g1(1, env, &g1);
}

static
ERL_NIF_TERM enif_mcl_bn_fp2_map_to_g2(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  mclBnG2 g2;
  mclBnFp2 fp2;

  if(!get_fp2(env, argv[0], &fp2))
    return enif_make_badarg(env);

  if(mclBnFp2_mapToG2(&g2, &fp2)) {
    return error_tuple(env, "map_to_failed");
  }

  return enif_return_g2(1, env, &g2);
}

// -----
// To "string"
// -----
#define MCL_BN_TO_STR(TYPE, SIZE, GET, FUN, MODE) \
  TYPE x; \
  char buf[SIZE]; \
  \
  if(argc != 1 || !GET(env, argv[0], &x)){ \
    return enif_make_badarg(env); \
  } \
  \
  if(!FUN(buf, SIZE, &x, MODE)){ \
    return error_tuple(env, "bad_value"); \
  } \
  \
  return binary_from_buf(env, buf);

static
ERL_NIF_TERM enif_mcl_bn_fr_to_str(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_TO_STR(mclBnFr, FR_SIZE * 2, get_fr, mclBnFr_getStr, FX_MODE)
}

static
ERL_NIF_TERM enif_mcl_bn_fp_to_str(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_TO_STR(mclBnFp, FP_SIZE * 2, get_fp, mclBnFp_getStr, FX_MODE)
}

static
ERL_NIF_TERM enif_mcl_bn_g1_to_str(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_TO_STR(mclBnG1, G1_SIZE, get_g1, mclBnG1_getStr, GX_MODE)
}

static
ERL_NIF_TERM enif_mcl_bn_g2_to_str(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_TO_STR(mclBnG2, G2_SIZE, get_g2, mclBnG2_getStr, GX_MODE)
}

static
ERL_NIF_TERM enif_mcl_bn_gt_to_str(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_TO_STR(mclBnGT, GT_SIZE, get_gt, mclBnGT_getStr, GX_MODE)
}

// -----
// From "string"
// -----
#define MCL_BN_FROM_STR(TYPE, FUN, MODE, RETURN) \
  ErlNifBinary in; \
  TYPE x; \
  \
  CHECK_BINARY_ARG(in); \
  \
  if(FUN(&x, (const char *)in.data, in.size, MODE)) \
    return error_tuple(env, "bad_string"); \
  \
  return RETURN(1, env, &x);

static
ERL_NIF_TERM enif_mcl_bn_fr_from_str(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FROM_STR(mclBnFr, mclBnFr_setStr, FX_MODE, enif_return_fr)
}

static
ERL_NIF_TERM enif_mcl_bn_fp_from_str(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FROM_STR(mclBnFp, mclBnFp_setStr, FX_MODE, enif_return_fp)
}

static
ERL_NIF_TERM enif_mcl_bn_g1_from_str(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FROM_STR(mclBnG1, mclBnG1_setStr, GX_MODE, enif_return_g1)
}

static
ERL_NIF_TERM enif_mcl_bn_g2_from_str(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FROM_STR(mclBnG2, mclBnG2_setStr, GX_MODE, enif_return_g2)
}

static
ERL_NIF_TERM enif_mcl_bn_gt_from_str(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_FROM_STR(mclBnGT, mclBnGT_setStr, GX_MODE, enif_return_gt)
}

// -----
// Compression
// -----
#define MCL_BN_COMPRESS(TYPE, SIZE, GET, FUN) \
  TYPE x; \
  char buf[SIZE]; \
  \
  if(!GET(env, argv[0], &x)) \
    return enif_make_badarg(env); \
  \
  if(!FUN(buf, SIZE, &x)) \
    return error_tuple(env, "compression_failed"); \
  \
  return binary_from_buf_len(env, buf, SIZE);

static
ERL_NIF_TERM enif_mcl_bn_fr_compress(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_COMPRESS(mclBnFr, FR_SIZE, get_fr, mclBnFr_serialize)
}

static
ERL_NIF_TERM enif_mcl_bn_fp_compress(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_COMPRESS(mclBnFp, FP_SIZE, get_fp, mclBnFp_serialize)
}

static
ERL_NIF_TERM enif_mcl_bn_fp2_compress(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_COMPRESS(mclBnFp2, FP2_SIZE, get_fp2, mclBnFp2_serialize)
}

static
ERL_NIF_TERM enif_mcl_bn_g1_compress(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_COMPRESS(mclBnG1, FP_SIZE, get_g1, mclBnG1_serialize)
}

static
ERL_NIF_TERM enif_mcl_bn_g2_compress(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_COMPRESS(mclBnG2, FP2_SIZE, get_g2, mclBnG2_serialize)
}

static
ERL_NIF_TERM enif_mcl_bn_gt_compress(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_COMPRESS(mclBnGT, (12 * FP_SIZE), get_gt, mclBnGT_serialize)
}

// -----
// Decompression
// -----
#define MCL_BN_DECOMPRESS(TYPE, FUN, RETURN)\
  ErlNifBinary bin; \
  TYPE x; \
  \
  CHECK_BINARY_ARG(bin); \
  \
  if(!FUN(&x, bin.data, bin.size)) \
    return error_tuple(env, "decompression_failed"); \
  \
  return RETURN(1, env, &x);

static
ERL_NIF_TERM enif_mcl_bn_fr_decompress(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_DECOMPRESS(mclBnFr, mclBnFr_deserialize, enif_return_fr)
}

static
ERL_NIF_TERM enif_mcl_bn_fp_decompress(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_DECOMPRESS(mclBnFp, mclBnFp_deserialize, enif_return_fp)
}

static
ERL_NIF_TERM enif_mcl_bn_fp2_decompress(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_DECOMPRESS(mclBnFp2, mclBnFp2_deserialize, enif_return_fp2)
}

static
ERL_NIF_TERM enif_mcl_bn_g1_decompress(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_DECOMPRESS(mclBnG1, mclBnG1_deserialize, enif_return_g1)
}

static
ERL_NIF_TERM enif_mcl_bn_g2_decompress(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_DECOMPRESS(mclBnG2, mclBnG2_deserialize, enif_return_g2)
}

static
ERL_NIF_TERM enif_mcl_bn_gt_decompress(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
  MCL_BN_DECOMPRESS(mclBnGT, mclBnGT_deserialize, enif_return_gt)
}

/* Tie the knot to the Erlang world */
static ErlNifFunc nif_funcs[] = {
  {"mcl_bn_miller_loop", 2, enif_mcl_bn_miller_loop},
  {"mcl_bn_miller_loop_vec", 2, enif_mcl_bn_miller_loop_vec},
  {"mcl_bn_final_exp", 1, enif_mcl_bn_final_exp},
  {"mcl_bn_pairing", 2, enif_mcl_bn_pairing},

  {"mcl_bn_fr_neg",  1, enif_mcl_bn_fr_neg},
  {"mcl_bn_fr_inv",  1, enif_mcl_bn_fr_inv},
  {"mcl_bn_fr_sqr",  1, enif_mcl_bn_fr_sqr},
  {"mcl_bn_fr_sqrt", 1, enif_mcl_bn_fr_sqrt},
  {"mcl_bn_fr_add",  2, enif_mcl_bn_fr_add},
  {"mcl_bn_fr_sub",  2, enif_mcl_bn_fr_sub},
  {"mcl_bn_fr_mul",  2, enif_mcl_bn_fr_mul},
  {"mcl_bn_fr_div",  2, enif_mcl_bn_fr_div},
  {"mcl_bn_fr_is_equal", 2, enif_mcl_bn_fr_is_equal},
  {"mcl_bn_fr_is_zero", 1, enif_mcl_bn_fr_is_zero},
  {"mcl_bn_fr_is_one", 1, enif_mcl_bn_fr_is_one},
  {"mcl_bn_fr_is_odd", 1, enif_mcl_bn_fr_is_odd},
  {"mcl_bn_fr_is_valid", 1, enif_mcl_bn_fr_is_valid},
  {"mcl_bn_fr_is_negative", 1, enif_mcl_bn_fr_is_negative},
  {"mcl_bn_fr_lagrange_interpolation", 2, enif_mcl_bn_fr_lagrange_interpolation},
  {"mcl_bn_fr_eval_polynomial", 2, enif_mcl_bn_fr_eval_polynomial},

  {"mcl_bn_fp_neg",  1, enif_mcl_bn_fp_neg},
  {"mcl_bn_fp_inv",  1, enif_mcl_bn_fp_inv},
  {"mcl_bn_fp_sqr",  1, enif_mcl_bn_fp_sqr},
  {"mcl_bn_fp_sqrt", 1, enif_mcl_bn_fp_sqrt},
  {"mcl_bn_fp_add",  2, enif_mcl_bn_fp_add},
  {"mcl_bn_fp_sub",  2, enif_mcl_bn_fp_sub},
  {"mcl_bn_fp_mul",  2, enif_mcl_bn_fp_mul},
  {"mcl_bn_fp_div",  2, enif_mcl_bn_fp_div},
  {"mcl_bn_fp_is_equal", 2, enif_mcl_bn_fp_is_equal},
  {"mcl_bn_fp_is_zero", 1, enif_mcl_bn_fp_is_zero},
  {"mcl_bn_fp_is_one", 1, enif_mcl_bn_fp_is_one},
  {"mcl_bn_fp_is_odd", 1, enif_mcl_bn_fp_is_odd},
  {"mcl_bn_fp_is_valid", 1, enif_mcl_bn_fp_is_valid},
  {"mcl_bn_fp_is_negative", 1, enif_mcl_bn_fp_is_negative},

  {"mcl_bn_fp2_neg",  1, enif_mcl_bn_fp2_neg},
  {"mcl_bn_fp2_inv",  1, enif_mcl_bn_fp2_inv},
  {"mcl_bn_fp2_sqr",  1, enif_mcl_bn_fp2_sqr},
  {"mcl_bn_fp2_sqrt", 1, enif_mcl_bn_fp2_sqrt},
  {"mcl_bn_fp2_add",  2, enif_mcl_bn_fp2_add},
  {"mcl_bn_fp2_sub",  2, enif_mcl_bn_fp2_sub},
  {"mcl_bn_fp2_mul",  2, enif_mcl_bn_fp2_mul},
  {"mcl_bn_fp2_div",  2, enif_mcl_bn_fp2_div},
  {"mcl_bn_fp2_is_equal", 2, enif_mcl_bn_fp2_is_equal},
  {"mcl_bn_fp2_is_zero", 1, enif_mcl_bn_fp2_is_zero},
  {"mcl_bn_fp2_is_one", 1, enif_mcl_bn_fp2_is_one},

  {"mcl_bn_g1_neg", 1, enif_mcl_bn_g1_neg},
  {"mcl_bn_g1_dbl", 1, enif_mcl_bn_g1_dbl},
  {"mcl_bn_g1_normalize", 1, enif_mcl_bn_g1_normalize},
  {"mcl_bn_g1_add", 2, enif_mcl_bn_g1_add},
  {"mcl_bn_g1_sub", 2, enif_mcl_bn_g1_sub},
  {"mcl_bn_g1_mul", 2, enif_mcl_bn_g1_mul},
  {"mcl_bn_g1_mul_vec", 2, enif_mcl_bn_g1_mul_vec},
  {"mcl_bn_g1_hash_and_map_to", 1, enif_mcl_bn_g1_hash_and_map_to},
  {"mcl_bn_g1_is_equal", 2, enif_mcl_bn_g1_is_equal},
  {"mcl_bn_g1_is_zero", 1, enif_mcl_bn_g1_is_zero},
  {"mcl_bn_g1_is_valid", 1, enif_mcl_bn_g1_is_valid},
  {"mcl_bn_g1_is_valid_order", 1, enif_mcl_bn_g1_is_valid_order},
  {"mcl_bn_g1_lagrange_interpolation", 2, enif_mcl_bn_g1_lagrange_interpolation},
  {"mcl_bn_g1_eval_polynomial", 2, enif_mcl_bn_g1_eval_polynomial},

  {"mcl_bn_g2_neg", 1, enif_mcl_bn_g2_neg},
  {"mcl_bn_g2_dbl", 1, enif_mcl_bn_g2_dbl},
  {"mcl_bn_g2_normalize", 1, enif_mcl_bn_g2_normalize},
  {"mcl_bn_g2_add", 2, enif_mcl_bn_g2_add},
  {"mcl_bn_g2_sub", 2, enif_mcl_bn_g2_sub},
  {"mcl_bn_g2_mul", 2, enif_mcl_bn_g2_mul},
  {"mcl_bn_g2_mul_vec", 2, enif_mcl_bn_g2_mul_vec},
  {"mcl_bn_g2_hash_and_map_to", 1, enif_mcl_bn_g2_hash_and_map_to},
  {"mcl_bn_g2_is_equal", 2, enif_mcl_bn_g2_is_equal},
  {"mcl_bn_g2_is_zero", 1, enif_mcl_bn_g2_is_zero},
  {"mcl_bn_g2_is_valid", 1, enif_mcl_bn_g2_is_valid},
  {"mcl_bn_g2_is_valid_order", 1, enif_mcl_bn_g2_is_valid_order},
  {"mcl_bn_g2_lagrange_interpolation", 2, enif_mcl_bn_g2_lagrange_interpolation},
  {"mcl_bn_g2_eval_polynomial", 2, enif_mcl_bn_g2_eval_polynomial},

  {"mcl_bn_gt_neg", 1, enif_mcl_bn_gt_neg},
  {"mcl_bn_gt_inv", 1, enif_mcl_bn_gt_inv},
  {"mcl_bn_gt_inv_generic", 1, enif_mcl_bn_gt_inv_generic},
  {"mcl_bn_gt_sqr", 1, enif_mcl_bn_gt_sqr},
  {"mcl_bn_gt_add", 2, enif_mcl_bn_gt_add},
  {"mcl_bn_gt_sub", 2, enif_mcl_bn_gt_sub},
  {"mcl_bn_gt_mul", 2, enif_mcl_bn_gt_mul},
  {"mcl_bn_gt_div", 2, enif_mcl_bn_gt_div},
  {"mcl_bn_gt_pow", 2, enif_mcl_bn_gt_pow},
  {"mcl_bn_gt_pow_vec", 2, enif_mcl_bn_gt_pow_vec},
  {"mcl_bn_gt_is_equal", 2, enif_mcl_bn_gt_is_equal},
  {"mcl_bn_gt_is_zero", 1, enif_mcl_bn_gt_is_zero},
  {"mcl_bn_gt_is_one", 1, enif_mcl_bn_gt_is_one},

  {"mcl_bn_fr_to_str",  1, enif_mcl_bn_fr_to_str},
  {"mcl_bn_fp_to_str",  1, enif_mcl_bn_fp_to_str},
  {"mcl_bn_g1_to_str",  1, enif_mcl_bn_g1_to_str},
  {"mcl_bn_g2_to_str",  1, enif_mcl_bn_g2_to_str},
  {"mcl_bn_gt_to_str",  1, enif_mcl_bn_gt_to_str},

  {"mcl_bn_fr_from_str",  1, enif_mcl_bn_fr_from_str},
  {"mcl_bn_fp_from_str",  1, enif_mcl_bn_fp_from_str},
  {"mcl_bn_g1_from_str",  1, enif_mcl_bn_g1_from_str},
  {"mcl_bn_g2_from_str",  1, enif_mcl_bn_g2_from_str},
  {"mcl_bn_gt_from_str",  1, enif_mcl_bn_gt_from_str},

  {"mcl_bn_fr_compress",  1, enif_mcl_bn_fr_compress},
  {"mcl_bn_fp_compress",  1, enif_mcl_bn_fp_compress},
  {"mcl_bn_fp2_compress",  1, enif_mcl_bn_fp2_compress},
  {"mcl_bn_g1_compress",  1, enif_mcl_bn_g1_compress},
  {"mcl_bn_g2_compress",  1, enif_mcl_bn_g2_compress},
  {"mcl_bn_gt_compress",  1, enif_mcl_bn_gt_compress},

  {"mcl_bn_fr_decompress",  1, enif_mcl_bn_fr_decompress},
  {"mcl_bn_fp_decompress",  1, enif_mcl_bn_fp_decompress},
  {"mcl_bn_fp2_decompress",  1, enif_mcl_bn_fp2_decompress},
  {"mcl_bn_g1_decompress",  1, enif_mcl_bn_g1_decompress},
  {"mcl_bn_g2_decompress",  1, enif_mcl_bn_g2_decompress},
  {"mcl_bn_gt_decompress",  1, enif_mcl_bn_gt_decompress},

  {"mcl_bn_fr_hash_of", 1, enif_mcl_bn_fr_hash_of},
  {"mcl_bn_fp_hash_of", 1, enif_mcl_bn_fp_hash_of},
  {"mcl_bn_fp2_hash_of", 1, enif_mcl_bn_fp2_hash_of},
  {"mcl_bn_fp_map_to_g1", 1, enif_mcl_bn_fp_map_to_g1},
  {"mcl_bn_fp2_map_to_g2", 1, enif_mcl_bn_fp2_map_to_g2}

};

ERL_NIF_INIT(emcl_nif, nif_funcs, enif_mcl_load, NULL, NULL, NULL);
