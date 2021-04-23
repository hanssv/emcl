-module(emcl).

%% API exports
-export([ mk_Fr/1
        , mk_Fp/1
        , mk_Fp2/2
        , mk_G1/3
        , mk_G2/3
        , mk_G2/6
        , mk_Gt/12

        , rnd_Fr/0
        , rnd_Fp/0
        , rnd_Fp2/0
        , rnd_G1/0
        , rnd_G2/0
        , rnd_Gt/0

        , compress_Fr/1
        , compress_Fp/1
        , compress_Fp2/1
        , compress_G1/1
        , compress_G2/1
        , compress_Gt/1

        , decompress_Fr/1
        , decompress_Fp/1
        , decompress_Fp2/1
        , decompress_G1/1
        , decompress_G2/1
        , decompress_Gt/1

        , bnFr_neg/1
        , bnFr_inv/1
        , bnFr_sqr/1
        , bnFr_sqrt/1
        , bnFr_add/2
        , bnFr_sub/2
        , bnFr_mul/2
        , bnFr_div/2
        , bnFr_is_equal/2
        , bnFr_is_zero/1
        , bnFr_is_one/1
        , bnFr_is_odd/1
        , bnFr_is_valid/1
        , bnFr_is_negative/1
        , bnFr_lagrange_interpolation/2
        , bnFr_eval_polynomial/2
        , bnFr_to_int/1
        , bnFr_to_bin/1
        , bnFr_from_str/1
        , bnFr_hash_of/1

        , bnFp_neg/1
        , bnFp_inv/1
        , bnFp_sqr/1
        , bnFp_sqrt/1
        , bnFp_add/2
        , bnFp_sub/2
        , bnFp_mul/2
        , bnFp_div/2
        , bnFp_is_equal/2
        , bnFp_is_zero/1
        , bnFp_is_one/1
        , bnFp_is_odd/1
        , bnFp_is_valid/1
        , bnFp_is_negative/1
        , bnFp_to_int/1
        , bnFp_to_bin/1
        , bnFp_from_str/1
        , bnFp_hash_of/1
        , bnFp_map_to_G1/1

        , bnFp2_neg/1
        , bnFp2_inv/1
        , bnFp2_sqr/1
        , bnFp2_sqrt/1
        , bnFp2_add/2
        , bnFp2_sub/2
        , bnFp2_mul/2
        , bnFp2_div/2
        , bnFp2_is_equal/2
        , bnFp2_is_zero/1
        , bnFp2_is_one/1
        , bnFp2_to_int/1
        , bnFp2_to_bin/1
        , bnFp2_hash_of/1
        , bnFp2_map_to_G2/1

        , bnG1_neg/1
        , bnG1_dbl/1
        , bnG1_normalize/1
        , bnG1_add/2
        , bnG1_sub/2
        , bnG1_mul/2
        , bnG1_mul_vec/2
        , bnG1_is_equal/2
        , bnG1_is_zero/1
        , bnG1_is_valid/1
        , bnG1_is_valid_order/1
        , bnG1_lagrange_interpolation/2
        , bnG1_eval_polynomial/2
        , bnG1_to_int/1
        , bnG1_to_bin/1
        , bnG1_from_str/1
        , bnG1_hash_and_map_to/1

        , bnG2_neg/1
        , bnG2_dbl/1
        , bnG2_normalize/1
        , bnG2_add/2
        , bnG2_sub/2
        , bnG2_mul/2
        , bnG2_mul_vec/2
        , bnG2_is_equal/2
        , bnG2_is_zero/1
        , bnG2_is_valid/1
        , bnG2_is_valid_order/1
        , bnG2_lagrange_interpolation/2
        , bnG2_eval_polynomial/2
        , bnG2_to_int/1
        , bnG2_to_bin/1
        , bnG2_from_str/1
        , bnG2_hash_and_map_to/1

        , bnGt_neg/1
        , bnGt_inv/1
        , bnGt_inv_generic/1
        , bnGt_sqr/1
        , bnGt_add/2
        , bnGt_sub/2
        , bnGt_mul/2
        , bnGt_div/2
        , bnGt_pow/2
        , bnGt_pow_vec/2
        , bnGt_is_equal/2
        , bnGt_is_zero/1
        , bnGt_is_one/1
        , bnGt_to_int/1
        , bnGt_to_bin/1
        , bnGt_from_str/1

        , bn_miller_loop/2
        , bn_miller_loop_vec/2
        , bn_final_exp/1
        , bn_pairing/2
        ]).

-export([pp/1, is_eq/2]).

-ifdef(TEST).
-export([random_int/1]).
-endif.

-define(FR_SIZE, 256). %% 4 * 64
-define(FP_SIZE, 384). %% 6 * 64
-define(FR_ZERO, <<0:?FP_SIZE>>).
-define(FP_ZERO, <<0:?FP_SIZE>>).

-type fr_bin() :: <<_:?FR_SIZE>>.
-type fp_bin() :: <<_:?FP_SIZE>>.

-record(fr, {d = ?FR_ZERO :: fr_bin()}).
-record(fp, {d = ?FP_ZERO :: fp_bin()}).
-record(fp2, {d1 = #fp{} :: #fp{},
              d2 = #fp{} :: #fp{}}).
-record(g1, {x = #fp{} :: #fp{},
             y = #fp{} :: #fp{},
             z = #fp{} :: #fp{}}).
-record(g2, {x = #fp2{} :: #fp2{},
             y = #fp2{} :: #fp2{},
             z = #fp2{} :: #fp2{}}).
-record(gt, {d1  = #fp{} :: #fp{},
             d2  = #fp{} :: #fp{},
             d3  = #fp{} :: #fp{},
             d4  = #fp{} :: #fp{},
             d5  = #fp{} :: #fp{},
             d6  = #fp{} :: #fp{},
             d7  = #fp{} :: #fp{},
             d8  = #fp{} :: #fp{},
             d9  = #fp{} :: #fp{},
             d10 = #fp{} :: #fp{},
             d11 = #fp{} :: #fp{},
             d12 = #fp{} :: #fp{}}).

-type mcl_bnFr()  :: #fr{}.
-type mcl_bnFp()  :: #fp{}.
-type mcl_bnFp2() :: #fp2{}.
-type mcl_bnG1()  :: #g1{}.
-type mcl_bnG2()  :: #g2{}.
-type mcl_bnGt()  :: #gt{}.

-opaque g1() :: mcl_bnG1().
-opaque g2() :: mcl_bnG2().
-opaque fr() :: mcl_bnFr().

-export_type([g1/0, g2/0, fr/0]).

%%====================================================================
%% API functions
%%====================================================================

%%%%%
%% mk_X
%%%%%
-spec mk_Fr(X :: integer() | fr_bin()) -> mcl_bnFr().
mk_Fr(X) when is_integer(X) ->
  {ok, Fr} = emcl_nif:mcl_bn_fr_from_str(integer_to_binary(X)),
  Fr;
mk_Fr(X = <<_:?FR_SIZE>>) ->
  #fr{ d = X }.

-spec mk_Fp(X :: integer() | fp_bin()) -> mcl_bnFp().
mk_Fp(X) when is_integer(X) ->
  {ok, Fp} = emcl_nif:mcl_bn_fp_from_str(integer_to_binary(X)),
  Fp;
mk_Fp(X = <<_:?FP_SIZE>>) ->
  #fp{ d = X }.

-spec mk_Fp2(X1 :: integer() | mcl_bnFp(),
             X2 :: integer() | mcl_bnFp()) -> mcl_bnFp2().
mk_Fp2(X1, X2) when is_integer(X1), is_integer(X2) ->
  #fp2{ d1 = mk_Fp(X1), d2 = mk_Fp(X2) };
mk_Fp2(X1 = #fp{}, X2 = #fp{}) ->
  #fp2{ d1 = X1, d2 = X2 }.

-spec mk_G1(X :: integer() | mcl_bnFp(),
            Y :: integer() | mcl_bnFp(),
            Z :: integer() | mcl_bnFp()) -> mcl_bnG1().
mk_G1(X, Y, Z) when is_integer(X), is_integer(Y), is_integer(Z) ->
  #g1{ x = mk_Fp(X), y = mk_Fp(Y), z = mk_Fp(Z) };
mk_G1(X = #fp{}, Y = #fp{}, Z = #fp{}) ->
  #g1{ x = X, y = Y, z = Z }.

-spec mk_G2(X :: mcl_bnFp2(), Y :: mcl_bnFp2(), Z :: mcl_bnFp2()) -> mcl_bnG2().
mk_G2(X = #fp2{}, Y = #fp2{}, Z = #fp2{}) ->
  #g2{ x = X, y = Y, z = Z }.

-spec mk_G2(X1 :: integer(), X2 :: integer(),
            Y1 :: integer(), Y2 :: integer(),
            Z1 :: integer(), Z2 :: integer()) -> mcl_bnG2().
mk_G2(X1, X2, Y1, Y2, Z1, Z2) when is_integer(X1), is_integer(X2),
                                   is_integer(Y1), is_integer(Y2),
                                   is_integer(Z1), is_integer(Z2)  ->
  #g2{ x = mk_Fp2(X1, X2), y = mk_Fp2(Y1, Y2), z = mk_Fp2(Z1, X2) }.

-spec mk_Gt(D1 :: integer() | mcl_bnFp(), D2 :: integer() | mcl_bnFp(),
            D3 :: integer() | mcl_bnFp(), D4 :: integer() | mcl_bnFp(),
            D5 :: integer() | mcl_bnFp(), D6 :: integer() | mcl_bnFp(),
            D7 :: integer() | mcl_bnFp(), D8 :: integer() | mcl_bnFp(),
            D9 :: integer() | mcl_bnFp(), D10 :: integer() | mcl_bnFp(),
            D11 :: integer() | mcl_bnFp(), D12 :: integer() | mcl_bnFp()) -> mcl_bnGt().
mk_Gt(D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12)
    when is_integer(D1), is_integer(D2), is_integer(D3), is_integer(D4),
         is_integer(D5), is_integer(D6), is_integer(D7), is_integer(D8),
         is_integer(D9), is_integer(D10), is_integer(D11), is_integer(D12) ->
  #gt{ d1 = mk_Fp(D1), d2 = mk_Fp(D2), d3 = mk_Fp(D3), d4 = mk_Fp(D4),
       d5 = mk_Fp(D5), d6 = mk_Fp(D6), d7 = mk_Fp(D7), d8 = mk_Fp(D8),
       d9 = mk_Fp(D9), d10 = mk_Fp(D10), d11 = mk_Fp(D11), d12 = mk_Fp(D12) };
mk_Gt(D1 = #fp{}, D2 = #fp{}, D3 = #fp{}, D4 = #fp{}, D5 = #fp{}, D6 = #fp{},
      D7 = #fp{}, D8 = #fp{}, D9 = #fp{}, D10 = #fp{}, D11 = #fp{}, D12 = #fp{}) ->
  #gt{ d1 = D1, d2 = D2, d3 = D3, d4 = D4, d5 = D5, d6 = D6,
       d7 = D7, d8 = D8, d9 = D9, d10 = D10, d11 = D11, d12 = D12 }.

%%%%%
%% rnd_X
%%%%%
-spec rnd_Fr() -> mcl_bnFr().
rnd_Fr() ->
  mk_Fr(random_fr()).

-spec rnd_Fp() -> mcl_bnFp().
rnd_Fp() ->
  mk_Fp(random_fp()).

-spec rnd_Fp2() -> mcl_bnFp2().
rnd_Fp2() ->
  #fp2{ d1 = rnd_Fp(), d2 = rnd_Fp() }.

-spec rnd_G1() -> mcl_bnG1().
rnd_G1() ->
  #g1{ x = rnd_Fp(), y = rnd_Fp(), z = rnd_Fp() }.

-spec rnd_G2() -> mcl_bnG2().
rnd_G2() ->
  #g2{ x = rnd_Fp2(), y = rnd_Fp2(), z = rnd_Fp2() }.

-spec rnd_Gt() -> mcl_bnGt().
rnd_Gt() ->
  #gt{ d1 = rnd_Fp(), d2 = rnd_Fp(),  d3 = rnd_Fp(),  d4 = rnd_Fp(),
       d5 = rnd_Fp(), d6 = rnd_Fp(),  d7 = rnd_Fp(),  d8 = rnd_Fp(),
       d9 = rnd_Fp(), d10 = rnd_Fp(), d11 = rnd_Fp(), d12 = rnd_Fp() }.

%%%%%
%% compress_X
%%%%%
-spec compress_Fr(X :: mcl_bnFr()) -> {ok, binary()} | {error, term()}.
compress_Fr(Fr) ->
  emcl_nif:mcl_bn_fr_compress(Fr).

-spec compress_Fp(X :: mcl_bnFp()) -> {ok, binary()} | {error, term()}.
compress_Fp(Fp) ->
  emcl_nif:mcl_bn_fp_compress(Fp).

-spec compress_Fp2(X :: mcl_bnFp2()) -> {ok, binary()} | {error, term()}.
compress_Fp2(Fp2) ->
  emcl_nif:mcl_bn_fp2_compress(Fp2).

-spec compress_G1(X :: mcl_bnG1()) -> {ok, binary()} | {error, term()}.
compress_G1(G1) ->
  emcl_nif:mcl_bn_g1_compress(G1).

-spec compress_G2(X :: mcl_bnG2()) -> {ok, binary()} | {error, term()}.
compress_G2(G2) ->
  emcl_nif:mcl_bn_g2_compress(G2).

-spec compress_Gt(X :: mcl_bnGt()) -> {ok, binary()} | {error, term()}.
compress_Gt(Gt) ->
  emcl_nif:mcl_bn_gt_compress(Gt).

%%%%%
%% decompress_X
%%%%%
-spec decompress_Fr(X :: binary()) -> {ok, mcl_bnFr()} | {error, term()}.
decompress_Fr(Fr) ->
  emcl_nif:mcl_bn_fr_decompress(Fr).

-spec decompress_Fp(X :: binary()) -> {ok, mcl_bnFp()} | {error, term()}.
decompress_Fp(Fp) ->
  emcl_nif:mcl_bn_fp_decompress(Fp).

-spec decompress_Fp2(X :: binary()) -> {ok, mcl_bnFp2()} | {error, term()}.
decompress_Fp2(Fp2) ->
  emcl_nif:mcl_bn_fp2_decompress(Fp2).

-spec decompress_G1(X :: binary()) -> {ok, mcl_bnG1()} | {error, term()}.
decompress_G1(G1) ->
  emcl_nif:mcl_bn_g1_decompress(G1).

-spec decompress_G2(X :: binary()) -> {ok, mcl_bnG2()} | {error, term()}.
decompress_G2(G2) ->
  emcl_nif:mcl_bn_g2_decompress(G2).

-spec decompress_Gt(X :: binary()) -> {ok, mcl_bnGt()} | {error, term()}.
decompress_Gt(Gt) ->
  emcl_nif:mcl_bn_gt_decompress(Gt).

%% Fr arithmetic
-spec bnFr_neg(X :: mcl_bnFr()) -> mcl_bnFr().
bnFr_neg(X = #fr{}) ->
  emcl_nif:mcl_bn_fr_neg(X).

-spec bnFr_inv(X :: mcl_bnFr()) -> mcl_bnFr().
bnFr_inv(X = #fr{}) ->
  emcl_nif:mcl_bn_fr_inv(X).

-spec bnFr_sqr(X :: mcl_bnFr()) -> mcl_bnFr().
bnFr_sqr(X = #fr{}) ->
  emcl_nif:mcl_bn_fr_sqr(X).

-spec bnFr_sqrt(X :: mcl_bnFr()) -> {ok, mcl_bnFr()} | {error, term()}.
bnFr_sqrt(X = #fr{}) ->
  emcl_nif:mcl_bn_fr_sqrt(X).

-spec bnFr_add(X :: mcl_bnFr(), Y :: mcl_bnFr()) -> mcl_bnFr().
bnFr_add(X = #fr{}, Y = #fr{}) ->
  emcl_nif:mcl_bn_fr_add(X, Y).

-spec bnFr_sub(X :: mcl_bnFr(), Y :: mcl_bnFr()) -> mcl_bnFr().
bnFr_sub(X = #fr{}, Y = #fr{}) ->
  emcl_nif:mcl_bn_fr_sub(X, Y).

-spec bnFr_mul(X :: mcl_bnFr(), Y :: mcl_bnFr()) -> mcl_bnFr().
bnFr_mul(X = #fr{}, Y = #fr{}) ->
  emcl_nif:mcl_bn_fr_mul(X, Y).

-spec bnFr_div(X :: mcl_bnFr(), Y :: mcl_bnFr()) -> mcl_bnFr().
bnFr_div(X = #fr{}, Y = #fr{}) ->
  emcl_nif:mcl_bn_fr_div(X, Y).

-spec bnFr_is_equal(X :: mcl_bnFr(), Y :: mcl_bnFr()) -> boolean().
bnFr_is_equal(X = #fr{}, Y = #fr{}) ->
  emcl_nif:mcl_bn_fr_is_equal(X, Y).

-spec bnFr_is_zero(X :: mcl_bnFr()) -> boolean().
bnFr_is_zero(X = #fr{}) ->
  emcl_nif:mcl_bn_fr_is_zero(X).

-spec bnFr_is_one(X :: mcl_bnFr()) -> boolean().
bnFr_is_one(X = #fr{}) ->
  emcl_nif:mcl_bn_fr_is_one(X).

-spec bnFr_is_odd(X :: mcl_bnFr()) -> boolean().
bnFr_is_odd(X = #fr{}) ->
  emcl_nif:mcl_bn_fr_is_odd(X).

-spec bnFr_is_valid(X :: mcl_bnFr()) -> boolean().
bnFr_is_valid(X = #fr{}) ->
  emcl_nif:mcl_bn_fr_is_valid(X).

-spec bnFr_is_negative(X :: mcl_bnFr()) -> boolean().
bnFr_is_negative(X = #fr{}) ->
  emcl_nif:mcl_bn_fr_is_negative(X).

-spec bnFr_lagrange_interpolation(Xs :: [mcl_bnFr()], Ys :: [mcl_bnFr()]) ->
    {ok, mcl_bnFr()} | {error, term()}.
bnFr_lagrange_interpolation([], _) -> {error, empty_list};
bnFr_lagrange_interpolation(Xs, Ys) when length(Xs) == length(Ys) ->
  emcl_nif:mcl_bn_fr_lagrange_interpolation(Xs, Ys);
bnFr_lagrange_interpolation(_, _) -> {error, different_sized_lists}.

-spec bnFr_eval_polynomial(Cs :: [mcl_bnFr()], X :: mcl_bnFr()) ->
    {ok, mcl_bnFr()} | {error, term()}.
bnFr_eval_polynomial([], _) -> {error, empty_list};
bnFr_eval_polynomial(Cs, X = #fr{}) ->
  emcl_nif:mcl_bn_fr_eval_polynomial(Cs, X).

-spec bnFr_to_int(X :: mcl_bnFr()) -> integer().
bnFr_to_int(Fr) ->
  {ok, BinInt} = emcl_nif:mcl_bn_fr_to_str(Fr),
  binary_to_integer(BinInt).

%% The binary is an integer in Montgomery representation
-spec bnFr_to_bin(X :: mcl_bnFr()) -> binary().
bnFr_to_bin(#fr{ d = Bin }) ->
  Bin.

%% Fp arithmetic
-spec bnFp_neg(X :: mcl_bnFp()) -> mcl_bnFp().
bnFp_neg(X = #fp{}) ->
  emcl_nif:mcl_bn_fp_neg(X).

-spec bnFp_inv(X :: mcl_bnFp()) -> mcl_bnFp().
bnFp_inv(X = #fp{}) ->
  emcl_nif:mcl_bn_fp_inv(X).

-spec bnFp_sqr(X :: mcl_bnFp()) -> mcl_bnFp().
bnFp_sqr(X = #fp{}) ->
  emcl_nif:mcl_bn_fp_sqr(X).

-spec bnFp_sqrt(X :: mcl_bnFp()) -> {ok, mcl_bnFp()} | {error, term()}.
bnFp_sqrt(X = #fp{}) ->
  emcl_nif:mcl_bn_fp_sqrt(X).

-spec bnFp_add(X :: mcl_bnFp(), Y :: mcl_bnFp()) -> mcl_bnFp().
bnFp_add(X = #fp{}, Y = #fp{}) ->
  emcl_nif:mcl_bn_fp_add(X, Y).

-spec bnFp_sub(X :: mcl_bnFp(), Y :: mcl_bnFp()) -> mcl_bnFp().
bnFp_sub(X = #fp{}, Y = #fp{}) ->
  emcl_nif:mcl_bn_fp_sub(X, Y).

-spec bnFp_mul(X :: mcl_bnFp(), Y :: mcl_bnFp()) -> mcl_bnFp().
bnFp_mul(X = #fp{}, Y = #fp{}) ->
  emcl_nif:mcl_bn_fp_mul(X, Y).

-spec bnFp_div(X :: mcl_bnFp(), Y :: mcl_bnFp()) -> mcl_bnFp().
bnFp_div(X = #fp{}, Y = #fp{}) ->
  emcl_nif:mcl_bn_fp_div(X, Y).

-spec bnFp_is_equal(X :: mcl_bnFp(), Y :: mcl_bnFp()) -> boolean().
bnFp_is_equal(X = #fp{}, Y = #fp{}) ->
  emcl_nif:mcl_bn_fp_is_equal(X, Y).

-spec bnFp_is_zero(X :: mcl_bnFp()) -> boolean().
bnFp_is_zero(X = #fp{}) ->
  emcl_nif:mcl_bn_fp_is_zero(X).

-spec bnFp_is_one(X :: mcl_bnFp()) -> boolean().
bnFp_is_one(X = #fp{}) ->
  emcl_nif:mcl_bn_fp_is_one(X).

-spec bnFp_is_odd(X :: mcl_bnFp()) -> boolean().
bnFp_is_odd(X = #fp{}) ->
  emcl_nif:mcl_bn_fp_is_odd(X).

-spec bnFp_is_valid(X :: mcl_bnFp()) -> boolean().
bnFp_is_valid(X = #fp{}) ->
  emcl_nif:mcl_bn_fp_is_valid(X).

-spec bnFp_is_negative(X :: mcl_bnFp()) -> boolean().
bnFp_is_negative(X = #fp{}) ->
  emcl_nif:mcl_bn_fp_is_negative(X).

-spec bnFp_to_int(X :: mcl_bnFp()) -> integer().
bnFp_to_int(Fp) ->
  {ok, BinInt} = emcl_nif:mcl_bn_fp_to_str(Fp),
  binary_to_integer(BinInt).

%% The binary is an integer in Montgomery representation
-spec bnFp_to_bin(X :: mcl_bnFp()) -> binary().
bnFp_to_bin(#fp{ d = Bin }) ->
  Bin.

%% Fp2 arithmetic
-spec bnFp2_neg(X :: mcl_bnFp2()) -> mcl_bnFp2().
bnFp2_neg(X = #fp2{}) ->
  emcl_nif:mcl_bn_fp2_neg(X).

-spec bnFp2_inv(X :: mcl_bnFp2()) -> mcl_bnFp2().
bnFp2_inv(X = #fp2{}) ->
  emcl_nif:mcl_bn_fp2_inv(X).

-spec bnFp2_sqr(X :: mcl_bnFp2()) -> mcl_bnFp2().
bnFp2_sqr(X = #fp2{}) ->
  emcl_nif:mcl_bn_fp2_sqr(X).

-spec bnFp2_sqrt(X :: mcl_bnFp2()) -> {ok, mcl_bnFp2()} | {error, term()}.
bnFp2_sqrt(X = #fp2{}) ->
  emcl_nif:mcl_bn_fp2_sqrt(X).

-spec bnFp2_add(X :: mcl_bnFp2(), Y :: mcl_bnFp2()) -> mcl_bnFp2().
bnFp2_add(X = #fp2{}, Y = #fp2{}) ->
  emcl_nif:mcl_bn_fp2_add(X, Y).

-spec bnFp2_sub(X :: mcl_bnFp2(), Y :: mcl_bnFp2()) -> mcl_bnFp2().
bnFp2_sub(X = #fp2{}, Y = #fp2{}) ->
  emcl_nif:mcl_bn_fp2_sub(X, Y).

-spec bnFp2_mul(X :: mcl_bnFp2(), Y :: mcl_bnFp2()) -> mcl_bnFp2().
bnFp2_mul(X = #fp2{}, Y = #fp2{}) ->
  emcl_nif:mcl_bn_fp2_mul(X, Y).

-spec bnFp2_div(X :: mcl_bnFp2(), Y :: mcl_bnFp2()) -> mcl_bnFp2().
bnFp2_div(X = #fp2{}, Y = #fp2{}) ->
  emcl_nif:mcl_bn_fp2_div(X, Y).

-spec bnFp2_is_equal(X :: mcl_bnFp2(), Y :: mcl_bnFp2()) -> boolean().
bnFp2_is_equal(X = #fp2{}, Y = #fp2{}) ->
  emcl_nif:mcl_bn_fp2_is_equal(X, Y).

-spec bnFp2_is_zero(X :: mcl_bnFp2()) -> boolean().
bnFp2_is_zero(X = #fp2{}) ->
  emcl_nif:mcl_bn_fp2_is_zero(X).

-spec bnFp2_is_one(X :: mcl_bnFp2()) -> boolean().
bnFp2_is_one(X = #fp2{}) ->
  emcl_nif:mcl_bn_fp2_is_one(X).

-spec bnFp2_to_int(X :: mcl_bnFp2()) -> {integer(), integer()}.
bnFp2_to_int(#fp2{ d1 = D1, d2 = D2 }) ->
  {bnFp_to_int(D1), bnFp_to_int(D2)}.

%% The binary is an integer in Montgomery representation
-spec bnFp2_to_bin(X :: mcl_bnFp2()) -> {binary(), binary()}.
bnFp2_to_bin(#fp2{ d1 = D1, d2 = D2 }) ->
  {bnFp_to_bin(D1), bnFp_to_bin(D2)}.

%% G1 arithmetic
-spec bnG1_neg(X :: mcl_bnG1()) -> mcl_bnG1().
bnG1_neg(X = #g1{}) ->
  emcl_nif:mcl_bn_g1_neg(X).

-spec bnG1_dbl(X :: mcl_bnG1()) -> mcl_bnG1().
bnG1_dbl(X = #g1{}) ->
  emcl_nif:mcl_bn_g1_dbl(X).

-spec bnG1_normalize(X :: mcl_bnG1()) -> mcl_bnG1().
bnG1_normalize(X = #g1{}) ->
  emcl_nif:mcl_bn_g1_normalize(X).

-spec bnG1_add(X :: mcl_bnG1(), Y :: mcl_bnG1()) -> mcl_bnG1().
bnG1_add(X = #g1{}, Y = #g1{}) ->
  emcl_nif:mcl_bn_g1_add(X, Y).

-spec bnG1_sub(X :: mcl_bnG1(), Y :: mcl_bnG1()) -> mcl_bnG1().
bnG1_sub(X = #g1{}, Y = #g1{}) ->
  emcl_nif:mcl_bn_g1_sub(X, Y).

-spec bnG1_mul(A :: mcl_bnG1(), B :: mcl_bnFr()) -> mcl_bnG1().
bnG1_mul(A = #g1{}, B = #fr{}) ->
  emcl_nif:mcl_bn_g1_mul(A, B).

-spec bnG1_mul_vec(X :: [mcl_bnG1()], Frs :: [mcl_bnFr()]) -> {ok, mcl_bnG1()} | {error, term()}.
bnG1_mul_vec([], _) -> {error, empty_list};
bnG1_mul_vec(G1s, Frs) when length(G1s) == length(Frs) ->
  emcl_nif:mcl_bn_g1_mul_vec(G1s, Frs);
bnG1_mul_vec(_, _) -> {error, different_sized_lists}.

-spec bnG1_hash_and_map_to(X :: binary()) -> {ok, mcl_bnG1()} | {error, term()}.
bnG1_hash_and_map_to(X) when is_binary(X) ->
  emcl_nif:mcl_bn_g1_hash_and_map_to(X).

-spec bnG1_is_equal(A :: mcl_bnG1(), B :: mcl_bnG1()) -> boolean().
bnG1_is_equal(A = #g1{}, B = #g1{}) ->
  emcl_nif:mcl_bn_g1_is_equal(A, B).

-spec bnG1_is_zero(X :: mcl_bnG1()) -> boolean().
bnG1_is_zero(X = #g1{}) ->
  emcl_nif:mcl_bn_g1_is_zero(X).

-spec bnG1_is_valid(X :: mcl_bnG1()) -> boolean().
bnG1_is_valid(X = #g1{}) ->
  emcl_nif:mcl_bn_g1_is_valid(X).

-spec bnG1_is_valid_order(X :: mcl_bnG1()) -> boolean().
bnG1_is_valid_order(X = #g1{}) ->
  emcl_nif:mcl_bn_g1_is_valid_order(X).

-spec bnG1_lagrange_interpolation(Xs :: [mcl_bnFr()], Ys :: [mcl_bnG1()]) ->
    {ok, mcl_bnG1()} | {error, term()}.
bnG1_lagrange_interpolation([], _) -> {error, empty_list};
bnG1_lagrange_interpolation(Xs, Ys) when length(Xs) == length(Ys) ->
  emcl_nif:mcl_bn_g1_lagrange_interpolation(Xs, Ys);
bnG1_lagrange_interpolation(_, _) -> {error, different_sized_lists}.

-spec bnG1_eval_polynomial(Cs :: [mcl_bnG1()], X :: mcl_bnFr()) ->
    {ok, mcl_bnG1()} | {error, term()}.
bnG1_eval_polynomial([], _) -> {error, empty_list};
bnG1_eval_polynomial(Cs, X = #fr{}) ->
  emcl_nif:mcl_bn_g1_eval_polynomial(Cs, X).

-spec bnG1_to_int(X :: mcl_bnG1()) -> {integer(), integer(), integer()}.
bnG1_to_int(#g1{ x = X, y = Y, z = Z}) ->
  {bnFp_to_int(X), bnFp_to_int(Y), bnFp_to_int(Z)}.

%% The binary is an integer in Montgomery representation
-spec bnG1_to_bin(X :: mcl_bnG1()) -> {binary(), binary(), binary()}.
bnG1_to_bin(#g1{ x = X, y = Y, z = Z}) ->
  {bnFp_to_bin(X), bnFp_to_bin(Y), bnFp_to_bin(Z)}.

%% G2 arithmetic
-spec bnG2_neg(X :: mcl_bnG2()) -> mcl_bnG2().
bnG2_neg(X = #g2{}) ->
  emcl_nif:mcl_bn_g2_neg(X).

-spec bnG2_dbl(X :: mcl_bnG2()) -> mcl_bnG2().
bnG2_dbl(X = #g2{}) ->
  emcl_nif:mcl_bn_g2_dbl(X).

-spec bnG2_normalize(X :: mcl_bnG2()) -> mcl_bnG2().
bnG2_normalize(X = #g2{}) ->
  emcl_nif:mcl_bn_g2_normalize(X).

-spec bnG2_add(X :: mcl_bnG2(), Y :: mcl_bnG2()) -> mcl_bnG2().
bnG2_add(X = #g2{}, Y = #g2{}) ->
  emcl_nif:mcl_bn_g2_add(X, Y).

-spec bnG2_sub(X :: mcl_bnG2(), Y :: mcl_bnG2()) -> mcl_bnG2().
bnG2_sub(X = #g2{}, Y = #g2{}) ->
  emcl_nif:mcl_bn_g2_sub(X, Y).

-spec bnG2_mul(A :: mcl_bnG2(), B :: mcl_bnFr()) -> mcl_bnG2().
bnG2_mul(A = #g2{}, B = #fr{}) ->
  emcl_nif:mcl_bn_g2_mul(A, B).

-spec bnG2_mul_vec(X :: [mcl_bnG2()], Frs :: [mcl_bnFr()]) -> {ok, mcl_bnG2()} | {error, term()}.
bnG2_mul_vec([], _) -> {error, empty_list};
bnG2_mul_vec(G2s, Frs) when length(G2s) == length(Frs) ->
  emcl_nif:mcl_bn_g2_mul_vec(G2s, Frs);
bnG2_mul_vec(_, _) -> {error, different_sized_lists}.

-spec bnG2_hash_and_map_to(X :: binary()) -> {ok, mcl_bnG2()} | {error, term()}.
bnG2_hash_and_map_to(X) ->
  emcl_nif:mcl_bn_g2_hash_and_map_to(X).

-spec bnG2_is_equal(A :: mcl_bnG2(), B :: mcl_bnG2()) -> boolean().
bnG2_is_equal(A = #g2{}, B = #g2{}) ->
  emcl_nif:mcl_bn_g2_is_equal(A, B).

-spec bnG2_is_zero(X :: mcl_bnG2()) -> boolean().
bnG2_is_zero(X = #g2{}) ->
  emcl_nif:mcl_bn_g2_is_zero(X).

-spec bnG2_is_valid(X :: mcl_bnG2()) -> boolean().
bnG2_is_valid(X = #g2{}) ->
  emcl_nif:mcl_bn_g2_is_valid(X).

-spec bnG2_is_valid_order(X :: mcl_bnG2()) -> boolean().
bnG2_is_valid_order(X = #g2{}) ->
  emcl_nif:mcl_bn_g2_is_valid_order(X).

-spec bnG2_lagrange_interpolation(Xs :: [mcl_bnFr()], Ys :: [mcl_bnG2()]) ->
    {ok, mcl_bnG2()} | {error, term()}.
bnG2_lagrange_interpolation([], _) -> {error, empty_list};
bnG2_lagrange_interpolation(Xs, Ys) when length(Xs) == length(Ys) ->
  emcl_nif:mcl_bn_g2_lagrange_interpolation(Xs, Ys);
bnG2_lagrange_interpolation(_, _) -> {error, different_sized_lists}.

-spec bnG2_eval_polynomial(Cs :: [mcl_bnG2()], X :: mcl_bnFr()) ->
    {ok, mcl_bnG2()} | {error, term()}.
bnG2_eval_polynomial([], _) -> {error, empty_list};
bnG2_eval_polynomial(Cs, X = #fr{}) ->
  emcl_nif:mcl_bn_g2_eval_polynomial(Cs, X).

-spec bnG2_to_int(X :: mcl_bnG2()) -> {{integer(), integer()},
                                       {integer(), integer()},
                                       {integer(), integer()}}.
bnG2_to_int(#g2{ x = X, y = Y, z = Z}) ->
  {bnFp2_to_int(X), bnFp2_to_int(Y), bnFp2_to_int(Z)}.

%% The binary is an integer in Montgomery representation
-spec bnG2_to_bin(X :: mcl_bnG2()) -> {{binary(), binary()},
                                       {binary(), binary()},
                                       {binary(), binary()}}.
bnG2_to_bin(#g2{ x = X, y = Y, z = Z}) ->
  {bnFp2_to_bin(X), bnFp2_to_bin(Y), bnFp2_to_bin(Z)}.

%% Gt arithmetic
-spec bnGt_neg(X :: mcl_bnGt()) -> mcl_bnGt().
bnGt_neg(X = #gt{}) ->
  emcl_nif:mcl_bn_gt_neg(X).

-spec bnGt_inv(X :: mcl_bnGt()) -> mcl_bnGt().
bnGt_inv(X = #gt{}) ->
  emcl_nif:mcl_bn_gt_inv(X).

-spec bnGt_inv_generic(X :: mcl_bnGt()) -> mcl_bnGt().
bnGt_inv_generic(X = #gt{}) ->
  emcl_nif:mcl_bn_gt_inv_generic(X).

-spec bnGt_sqr(X :: mcl_bnGt()) -> mcl_bnGt().
bnGt_sqr(X = #gt{}) ->
  emcl_nif:mcl_bn_gt_sqr(X).

-spec bnGt_add(X :: mcl_bnGt(), Y :: mcl_bnGt()) -> mcl_bnGt().
bnGt_add(X = #gt{}, Y = #gt{}) ->
  emcl_nif:mcl_bn_gt_add(X, Y).

-spec bnGt_sub(X :: mcl_bnGt(), Y :: mcl_bnGt()) -> mcl_bnGt().
bnGt_sub(X = #gt{}, Y = #gt{}) ->
  emcl_nif:mcl_bn_gt_sub(X, Y).

-spec bnGt_mul(X :: mcl_bnGt(), Y :: mcl_bnGt()) -> mcl_bnGt().
bnGt_mul(X = #gt{}, Y = #gt{}) ->
  emcl_nif:mcl_bn_gt_mul(X, Y).

-spec bnGt_div(X :: mcl_bnGt(), Y :: mcl_bnGt()) -> mcl_bnGt().
bnGt_div(X = #gt{}, Y = #gt{}) ->
  emcl_nif:mcl_bn_gt_div(X, Y).

-spec bnGt_pow(A :: mcl_bnGt(), B :: mcl_bnFr()) -> mcl_bnGt().
bnGt_pow(A = #gt{}, B = #fr{}) ->
  emcl_nif:mcl_bn_gt_pow(A, B).

-spec bnGt_pow_vec(X :: [mcl_bnGt()], Frs :: [mcl_bnFr()]) -> {ok, mcl_bnGt()} | {error, term()}.
bnGt_pow_vec([], _) -> {error, empty_list};
bnGt_pow_vec(Gts, Frs) when length(Gts) == length(Frs) ->
  emcl_nif:mcl_bn_gt_pow_vec(Gts, Frs);
bnGt_pow_vec(_, _) -> {error, different_sized_lists}.

-spec bnGt_is_equal(A :: mcl_bnGt(), B :: mcl_bnGt()) -> boolean().
bnGt_is_equal(A = #gt{}, B = #gt{}) ->
  emcl_nif:mcl_bn_gt_is_equal(A, B).

-spec bnGt_is_zero(X :: mcl_bnGt()) -> boolean().
bnGt_is_zero(X = #gt{}) ->
  emcl_nif:mcl_bn_gt_is_zero(X).

-spec bnGt_is_one(X :: mcl_bnGt()) -> boolean().
bnGt_is_one(X = #gt{}) ->
  emcl_nif:mcl_bn_gt_is_one(X).

-spec bnGt_to_int(X :: mcl_bnGt()) -> {integer(), integer(), integer(), integer(),
                                       integer(), integer(), integer(), integer(),
                                       integer(), integer(), integer(), integer()}.
bnGt_to_int(Gt = #gt{}) ->
  [gt | Fps ] = tuple_to_list(Gt),
  list_to_tuple([bnFp_to_int(Fp) || Fp <- Fps]).

%% The binary is an integer in Montgomery representation
-spec bnGt_to_bin(X :: mcl_bnGt()) -> {binary(), binary(), binary(), binary(),
                                       binary(), binary(), binary(), binary(),
                                       binary(), binary(), binary(), binary()}.
bnGt_to_bin(Gt = #gt{}) ->
  [gt | Fps ] = tuple_to_list(Gt),
  list_to_tuple([bnFp_to_bin(Fp) || Fp <- Fps]).

%% Pairing operations
-spec bn_miller_loop(X :: mcl_bnG1(), Y :: mcl_bnG2()) -> mcl_bnGt().
bn_miller_loop(X = #g1{}, Y = #g2{}) ->
  emcl_nif:mcl_bn_miller_loop(X, Y).

-spec bn_miller_loop_vec(X :: [mcl_bnG1()], Frs :: [mcl_bnG2()]) ->
    {ok, mcl_bnGt()} | {error, term()}.
bn_miller_loop_vec([], _) -> {error, empty_list};
bn_miller_loop_vec(G1s, G2s) when length(G1s) == length(G2s) ->
  emcl_nif:mcl_bn_miller_loop_vec(G1s, G2s);
bn_miller_loop_vec(_, _) -> {error, different_sized_lists}.

-spec bn_final_exp(X :: mcl_bnGt()) -> mcl_bnGt().
bn_final_exp(X = #gt{}) ->
  emcl_nif:mcl_bn_final_exp(X).

-spec bn_pairing(X :: mcl_bnG1(), Y :: mcl_bnG2()) -> mcl_bnGt().
bn_pairing(X = #g1{}, Y = #g2{}) ->
  emcl_nif:mcl_bn_pairing(X, Y).

-spec bnFr_from_str(X :: binary()) -> mcl_bnFr().
bnFr_from_str(X) ->
  {ok, Fr} = emcl_nif:mcl_bn_fr_from_str(X),
  Fr.

-spec bnFp_from_str(X :: binary()) -> mcl_bnFp().
bnFp_from_str(X) ->
  {ok, Fp} = emcl_nif:mcl_bn_fp_from_str(X),
  Fp.

-spec bnG1_from_str(X :: binary()) -> mcl_bnG1().
bnG1_from_str(X) ->
  {ok, G1} = emcl_nif:mcl_bn_g1_from_str(X),
  G1.

-spec bnG2_from_str(X :: binary()) -> mcl_bnG2().
bnG2_from_str(X) ->
  {ok, G2} = emcl_nif:mcl_bn_g2_from_str(X),
  G2.

-spec bnGt_from_str(X :: binary()) -> mcl_bnGt().
bnGt_from_str(X) ->
  {ok, Gt} = emcl_nif:mcl_bn_gt_from_str(X),
  Gt.

-spec bnFr_hash_of(X :: binary()) -> mcl_bnFr().
bnFr_hash_of(X) ->
  emcl_nif:mcl_bn_fr_hash_of(X).

-spec bnFp_hash_of(X :: binary()) -> mcl_bnFp().
bnFp_hash_of(X) ->
  emcl_nif:mcl_bn_fp_hash_of(X).

-spec bnFp2_hash_of(X :: binary()) -> mcl_bnFp2().
bnFp2_hash_of(X) ->
  emcl_nif:mcl_bn_fp2_hash_of(X).

-spec bnFp_map_to_G1(X :: mcl_bnFp()) -> {ok, mcl_bnG1()} | {error, term()}.
bnFp_map_to_G1(X = #fp{}) ->
  emcl_nif:mcl_bn_fp_map_to_g1(X).

-spec bnFp2_map_to_G2(X :: mcl_bnFp2()) -> {ok, mcl_bnG2()} | {error, term()}.
bnFp2_map_to_G2(X = #fp2{}) ->
  emcl_nif:mcl_bn_fp2_map_to_g2(X).

-spec pp(term()) -> iolist().
pp(X = #fr{}) -> {ok, Y} = emcl_nif:mcl_bn_fr_to_str(X), Y;
pp(X = #fp{}) -> {ok, Y} = emcl_nif:mcl_bn_fp_to_str(X), Y;
pp(X = #g1{}) -> {ok, Y} = emcl_nif:mcl_bn_g1_to_str(X), Y;
pp(X = #g2{}) -> {ok, Y} = emcl_nif:mcl_bn_g2_to_str(X), Y;
pp(X = #gt{}) -> {ok, Y} = emcl_nif:mcl_bn_gt_to_str(X), Y.

-spec is_eq(term(), term()) -> boolean().
is_eq(A = #fr{}, B = #fr{}) -> bnFr_is_equal(A, B);
is_eq(A = #fp{}, B = #fp{}) -> bnFp_is_equal(A, B);
is_eq(A = #fp2{}, B = #fp2{}) -> bnFp2_is_equal(A, B);
is_eq(A = #g1{}, B = #g1{}) -> bnG1_is_equal(A, B);
is_eq(A = #g2{}, B = #g2{}) -> bnG2_is_equal(A, B);
is_eq(A = #gt{}, B = #gt{}) -> bnGt_is_equal(A, B).

%%====================================================================
%% Internal functions
%%====================================================================
-define(MAX_FR, 16#73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001).
-define(MAX_FP, 16#1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab).
random_fr() ->
  random_int(?MAX_FR).

random_fp() ->
  random_int(?MAX_FP).

random_int(MaxInt) ->
  <<X:48/unsigned-integer-unit:8>> = crypto:strong_rand_bytes(48),
  X rem MaxInt.
