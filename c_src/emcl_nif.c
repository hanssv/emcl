#include "erl_nif.h"
#include <mcl/bn_c384_256.h>
#include <string.h>

#define FP_SIZE 48
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


static
ERL_NIF_TERM error_tuple(ErlNifEnv *env, char *error_atom) {
  return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, error_atom));
}

static
ERL_NIF_TERM ok_tuple(ErlNifEnv *env, ERL_NIF_TERM t) {
  return enif_make_tuple2(env, enif_make_atom(env, "ok"), t);
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

static
int enif_mcl_load(ErlNifEnv *env, void **priv_data, ERL_NIF_TERM load_info) {
    return mclBn_init(MCL_BLS12_381, MCLBN_COMPILED_TIME_VAR);
}

// -----
// Fr arithmetic functions
// -----
static
ERL_NIF_TERM enif_mcl_bn_fr_unop(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[],
                                 void (*f) (mclBnFr *, const mclBnFr *)) {
    ErlNifBinary in;
    mclBnFr x, z;
    char buf[FR_SIZE];

    CHECK_BINARY_ARG(in);

    mclBnFr_deserialize(&x, in.data, in.size);
    f(&z, &x);

    return binary_from_buf_len(env, buf, mclBnFr_serialize(buf, FR_SIZE, &z));
}

static
ERL_NIF_TERM enif_mcl_bn_fr_binop(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[],
                                  void (*f) (mclBnFr *, const mclBnFr *, const mclBnFr *)) {
    ErlNifBinary in_x, in_y;
    mclBnFr x, y, z;
    char buf[FR_SIZE];

    CHECK_BINARY_ARGS(in_x, in_y);

    mclBnFr_deserialize(&x, in_x.data, in_x.size);
    mclBnFr_deserialize(&y, in_y.data, in_y.size);
    f(&z, &x, &y);

    return binary_from_buf_len(env, buf, mclBnFr_serialize(buf, FR_SIZE, &z));
}

static
ERL_NIF_TERM enif_mcl_bn_fr_neg(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    return enif_mcl_bn_fr_unop(env, argc, argv, mclBnFr_neg);
}

static
ERL_NIF_TERM enif_mcl_bn_fr_inv(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    return enif_mcl_bn_fr_unop(env, argc, argv, mclBnFr_inv);
}

static
ERL_NIF_TERM enif_mcl_bn_fr_sqr(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    return enif_mcl_bn_fr_unop(env, argc, argv, mclBnFr_sqr);
}

static
ERL_NIF_TERM enif_mcl_bn_fr_add(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    return enif_mcl_bn_fr_binop(env, argc, argv, mclBnFr_add);
}

static
ERL_NIF_TERM enif_mcl_bn_fr_sub(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    return enif_mcl_bn_fr_binop(env, argc, argv, mclBnFr_sub);
}

static
ERL_NIF_TERM enif_mcl_bn_fr_mul(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    return enif_mcl_bn_fr_binop(env, argc, argv, mclBnFr_mul);
}

static
ERL_NIF_TERM enif_mcl_bn_fr_div(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    return enif_mcl_bn_fr_binop(env, argc, argv, mclBnFr_div);
}

static
ERL_NIF_TERM enif_mcl_bn_g1_mul(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    ErlNifBinary in_a, in_b;
    mclBnFr b;
    mclBnG1 a, ab;
    char buf[G1_SIZE];

    CHECK_BINARY_ARGS(in_a, in_b);

    mclBnG1_deserialize(&a, in_a.data, in_a.size);
    mclBnFr_deserialize(&b, in_b.data, in_b.size);
    mclBnG1_mul(&ab, &a, &b);

    return binary_from_buf_len(env, buf, mclBnG1_serialize(buf, G1_SIZE, &ab));
}

static
ERL_NIF_TERM enif_mcl_bn_g2_mul(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    ErlNifBinary in_a, in_b;
    mclBnFr b;
    mclBnG2 a, ab;
    char buf[G2_SIZE];

    CHECK_BINARY_ARGS(in_a, in_b);

    mclBnG2_deserialize(&a, in_a.data, in_a.size);
    mclBnFr_deserialize(&b, in_b.data, in_b.size);
    mclBnG2_mul(&ab, &a, &b);

    return binary_from_buf_len(env, buf, mclBnG2_serialize(buf, G2_SIZE, &ab));
}

static
ERL_NIF_TERM enif_mcl_bn_gt_pow(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    ErlNifBinary in_a, in_b;
    mclBnFr b;
    mclBnGT a, ab;
    char buf[GT_SIZE];

    CHECK_BINARY_ARGS(in_a, in_b);

    mclBnGT_deserialize(&a, in_a.data, in_a.size);
    mclBnFr_deserialize(&b, in_b.data, in_b.size);
    mclBnGT_pow(&ab, &a, &b);

    return binary_from_buf_len(env, buf, mclBnGT_serialize(buf, GT_SIZE, &ab));
}

static
ERL_NIF_TERM enif_mcl_bn_gt_equal(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    ErlNifBinary in_a, in_b;
    mclBnGT a, b;

    CHECK_BINARY_ARGS(in_a, in_b);

    mclBnGT_deserialize(&a, in_a.data, in_a.size);
    mclBnGT_deserialize(&b, in_b.data, in_b.size);

    if(mclBnGT_isEqual(&a, &b))
        return(ok_tuple(env, enif_make_atom(env, "true")));
    else
        return(ok_tuple(env, enif_make_atom(env, "false")));
}

static
ERL_NIF_TERM enif_mcl_bn_g1_hash_and_map_to(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    ErlNifBinary in;
    mclBnG1 res;
    char buf[G1_SIZE];

    CHECK_BINARY_ARG(in);

    if(mclBnG1_hashAndMapTo(&res, in.data, in.size)) {
        return error_tuple(env, "hash_and_map_to_failed");
    }

    return binary_from_buf_len(env, buf, mclBnG1_serialize(buf, G1_SIZE, &res));
}

static
ERL_NIF_TERM enif_mcl_bn_g2_hash_and_map_to(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    ErlNifBinary in;
    mclBnG2 res;
    char buf[G2_SIZE];

    CHECK_BINARY_ARG(in);

    if(mclBnG2_hashAndMapTo(&res, in.data, in.size)) {
        return error_tuple(env, "hash_and_map_to_failed");
    }

    return binary_from_buf_len(env, buf, mclBnG2_serialize(buf, G2_SIZE, &res));
}

static
ERL_NIF_TERM enif_mcl_bn_pairing(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    ErlNifBinary in_a, in_b;
    mclBnG1 a;
    mclBnG2 b;
    mclBnGT ab;
    char buf[GT_SIZE];

    CHECK_BINARY_ARGS(in_a, in_b);

    mclBnG1_deserialize(&a, in_a.data, in_a.size);
    mclBnG2_deserialize(&b, in_b.data, in_b.size);
    mclBn_pairing(&ab, &a, &b);

    return binary_from_buf_len(env, buf, mclBnGT_serialize(buf, GT_SIZE, &ab));
}

// -----
// To "string"
// -----
static
ERL_NIF_TERM enif_mcl_bn_fr_to_str(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    ErlNifBinary in;
    mclBnFr x;
    char buf[FR_SIZE];

    CHECK_BINARY_ARG(in);

    mclBnFr_deserialize(&x, in.data, in.size);
    mclBnFr_getStr(buf, FR_SIZE, &x, FX_MODE);

    return binary_from_buf(env, buf);
}

static
ERL_NIF_TERM enif_mcl_bn_fp_to_str(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    ErlNifBinary in;
    mclBnFp x;
    char buf[FP_SIZE];

    CHECK_BINARY_ARG(in);

    mclBnFp_deserialize(&x, in.data, in.size);
    mclBnFp_getStr(buf, FP_SIZE, &x, FX_MODE);

    return binary_from_buf(env, buf);
}

static
ERL_NIF_TERM enif_mcl_bn_g1_to_str(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    ErlNifBinary in;
    mclBnG1 x;
    char buf[G1_SIZE];

    CHECK_BINARY_ARG(in);

    mclBnG1_deserialize(&x, in.data, in.size);
    mclBnG1_getStr(buf, G1_SIZE, &x, GX_MODE);

    return binary_from_buf(env, buf);
}

static
ERL_NIF_TERM enif_mcl_bn_g2_to_str(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    ErlNifBinary in;
    mclBnG2 x;
    char buf[G2_SIZE];

    CHECK_BINARY_ARG(in);

    mclBnG2_deserialize(&x, in.data, in.size);
    mclBnG2_getStr(buf, G2_SIZE, &x, GX_MODE);

    return binary_from_buf(env, buf);
}

static
ERL_NIF_TERM enif_mcl_bn_gt_to_str(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    ErlNifBinary in;
    mclBnGT x;
    char buf[GT_SIZE];

    CHECK_BINARY_ARG(in);

    mclBnGT_deserialize(&x, in.data, in.size);
    mclBnGT_getStr(buf, GT_SIZE, &x, GX_MODE);

    return binary_from_buf(env, buf);
}

// -----
// From "string"
// -----
static
ERL_NIF_TERM enif_mcl_bn_fr_from_str(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    ErlNifBinary in;
    mclBnFr x;
    char buf[FR_SIZE];

    CHECK_BINARY_ARG(in);

    mclBnFr_setStr(&x, in.data, in.size, FX_MODE);

    return binary_from_buf_len(env, buf, mclBnFr_serialize(buf, FR_SIZE, &x));
}

static
ERL_NIF_TERM enif_mcl_bn_fp_from_str(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    ErlNifBinary in;
    mclBnFp x;
    char buf[FP_SIZE];

    CHECK_BINARY_ARG(in);

    mclBnFp_setStr(&x, in.data, in.size, FX_MODE);

    return binary_from_buf_len(env, buf, mclBnFp_serialize(buf, FP_SIZE, &x));
}

static
ERL_NIF_TERM enif_mcl_bn_g1_from_str(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    ErlNifBinary in;
    mclBnG1 x;
    char buf[G1_SIZE];

    CHECK_BINARY_ARG(in);

    mclBnG1_setStr(&x, in.data, in.size, GX_MODE);

    return binary_from_buf_len(env, buf, mclBnG1_serialize(buf, G1_SIZE, &x));
}

static
ERL_NIF_TERM enif_mcl_bn_g2_from_str(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    ErlNifBinary in;
    mclBnG2 x;
    char buf[G2_SIZE];

    CHECK_BINARY_ARG(in);

    mclBnG2_setStr(&x, in.data, in.size, GX_MODE);

    return binary_from_buf_len(env, buf, mclBnG2_serialize(buf, G2_SIZE, &x));
}

static
ERL_NIF_TERM enif_mcl_bn_gt_from_str(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    ErlNifBinary in;
    mclBnGT x;
    char buf[GT_SIZE];

    CHECK_BINARY_ARG(in);

    mclBnGT_setStr(&x, in.data, in.size, GX_MODE);

    return binary_from_buf_len(env, buf, mclBnGT_serialize(buf, GT_SIZE, &x));
}


// -----
// Random generation
// -----
static
ERL_NIF_TERM enif_mcl_bn_fr_random(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    mclBnFr x;
    char buf[FR_SIZE];

    mclBnFr_setByCSPRNG(&x);

    return binary_from_buf_len(env, buf, mclBnFr_serialize(buf, FR_SIZE, &x));
}

static
ERL_NIF_TERM enif_mcl_bn_fp_random(ErlNifEnv *env, int argc, ERL_NIF_TERM const argv[]) {
    mclBnFp x;
    char buf[FP_SIZE];

    mclBnFp_setByCSPRNG(&x);

    return binary_from_buf_len(env, buf, mclBnFp_serialize(buf, FP_SIZE, &x));
}

/* Tie the knot to the Erlang world */
static ErlNifFunc nif_funcs[] = {
    {"mcl_bn_g1_mul", 2, enif_mcl_bn_g1_mul},
    {"mcl_bn_g2_mul", 2, enif_mcl_bn_g2_mul},
    {"mcl_bn_g1_hash_and_map_to", 1, enif_mcl_bn_g1_hash_and_map_to},
    {"mcl_bn_g2_hash_and_map_to", 1, enif_mcl_bn_g2_hash_and_map_to},
    {"mcl_bn_gt_pow", 2, enif_mcl_bn_gt_pow},
    {"mcl_bn_gt_equal", 2, enif_mcl_bn_gt_equal},
    {"mcl_bn_pairing", 2, enif_mcl_bn_pairing},

    {"mcl_bn_fr_neg", 1, enif_mcl_bn_fr_neg},
    {"mcl_bn_fr_inv", 1, enif_mcl_bn_fr_inv},
    {"mcl_bn_fr_sqr", 1, enif_mcl_bn_fr_sqr},
    {"mcl_bn_fr_add", 2, enif_mcl_bn_fr_add},
    {"mcl_bn_fr_sub", 2, enif_mcl_bn_fr_sub},
    {"mcl_bn_fr_mul", 2, enif_mcl_bn_fr_mul},
    {"mcl_bn_fr_div", 2, enif_mcl_bn_fr_div},

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

    {"mcl_bn_fr_random", 0, enif_mcl_bn_fr_random},
    {"mcl_bn_fp_random", 0, enif_mcl_bn_fp_random}
};

ERL_NIF_INIT(emcl_nif, nif_funcs, enif_mcl_load, NULL, NULL, NULL);
