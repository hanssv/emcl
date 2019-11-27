-module(emcl_tests).

-include_lib("eunit/include/eunit.hrl").

mk_test() ->
  ?assertEqual(fr,  element(1, emcl:mk_Fr(123))),
  ?assertEqual(fp,  element(1, emcl:mk_Fp(123))),
  ?assertEqual(fp2, element(1, emcl:mk_Fp2(123, 456))),
  ?assertEqual(g1,  element(1, emcl:mk_G1(123, 456, 789))),
  ?assertEqual(g2,  element(1, emcl:mk_G2(123, 234, 456, 567, 789, 890))),
  ?assertEqual(gt,  element(1, emcl:mk_Gt(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))),

  ok.

rnd_test() ->
  ?assertEqual(fr,  element(1, emcl:rnd_Fr())),
  ?assertEqual(fp,  element(1, emcl:rnd_Fp())),
  ?assertEqual(fp2, element(1, emcl:rnd_Fp2())),
  ?assertEqual(g1,  element(1, emcl:rnd_G1())),
  ?assertEqual(g2,  element(1, emcl:rnd_G2())),
  ?assertEqual(gt,  element(1, emcl:rnd_Gt())),

  ok.

compress_test() ->
  {ok, _} = emcl:compress_Fr(emcl:rnd_Fr()),
  {ok, _} = emcl:compress_Fp(emcl:rnd_Fp()),
  {ok, _} = emcl:compress_Fp2(emcl:rnd_Fp2()),
  {ok, _} = emcl:compress_G1(emcl:rnd_G1()),
  {ok, _} = emcl:compress_G2(emcl:rnd_G2()),
  {ok, _} = emcl:compress_Gt(emcl:rnd_Gt()),

  ok.

decompress_test() ->
  {ok, _} = emcl:decompress_Fr(element(2, emcl:compress_Fr(emcl:rnd_Fr()))),
  {ok, _} = emcl:decompress_Fp(element(2, emcl:compress_Fp(emcl:rnd_Fp()))),
  {ok, _} = emcl:decompress_Fp2(element(2, emcl:compress_Fp2(emcl:rnd_Fp2()))),
  {ok, G1} = emcl:bnG1_hash_and_map_to(<<"xyz">>),
  {ok, _} = emcl:decompress_G1(element(2, emcl:compress_G1(G1))),
  {ok, G2} = emcl:bnG2_hash_and_map_to(<<"xyz">>),
  {ok, _} = emcl:decompress_G2(element(2, emcl:compress_G2(G2))),
  {ok, _} = emcl:decompress_Gt(element(2, emcl:compress_Gt(emcl:rnd_Gt()))),

  ok.

simple_test() ->
  A  = emcl:mk_Fr(123),
  B  = emcl:mk_Fr(456),
  AB = emcl:bnFr_mul(A, B),

  io:format("\nA = ~s; B = ~s\nA * B = ~s\n", [emcl:pp(A), emcl:pp(B), emcl:pp(AB)]),

  {ok, P} = emcl:bnG1_hash_and_map_to(<<"this">>),
  {ok, Q} = emcl:bnG2_hash_and_map_to(<<"that">>),

  io:format("P = ~s\nQ = ~s\n", [emcl:pp(P), emcl:pp(Q)]),

  AP = emcl:bnG1_mul(P, A),
  BQ = emcl:bnG2_mul(Q, B),
  io:format("AP = ~s\nBQ = ~s\n", [emcl:pp(AP), emcl:pp(BQ)]),


  E = emcl:bn_pairing(P, Q),
  io:format("E = ~s\n", [emcl:pp(E)]),

  E1 = emcl:bnGt_pow(E, A),
  E2 = emcl:bn_pairing(AP, Q),

  ?assertEqual(true, emcl:bnGt_is_equal(E1, E2)),

  E3 = emcl:bnGt_pow(E, B),
  E4 = emcl:bn_pairing(P, BQ),

  ?assertEqual(true, emcl:bnGt_is_equal(E3, E4)),

  ok.

fr_test() ->
  X = emcl:rnd_Fr(),
  Y = emcl:rnd_Fr(),

  _ = emcl:bnFr_neg(X),
  _ = emcl:bnFr_inv(X),
  _ = emcl:bnFr_sqr(X),
  _ = emcl:bnFr_add(X, Y),
  _ = emcl:bnFr_sub(X, Y),
  _ = emcl:bnFr_mul(X, Y),
  _ = emcl:bnFr_div(X, Y),

  ok.

fp_test() ->
  X = emcl:rnd_Fp(),
  Y = emcl:rnd_Fp(),

  _ = emcl:bnFp_neg(X),
  _ = emcl:bnFp_inv(X),
  _ = emcl:bnFp_sqr(X),
  _ = emcl:bnFp_add(X, Y),
  _ = emcl:bnFp_sub(X, Y),
  _ = emcl:bnFp_mul(X, Y),
  _ = emcl:bnFp_div(X, Y),

  ok.

fp2_test() ->
  X = emcl:rnd_Fp2(),
  Y = emcl:rnd_Fp2(),

  _ = emcl:bnFp2_neg(X),
  _ = emcl:bnFp2_inv(X),
  _ = emcl:bnFp2_sqr(X),
  _ = emcl:bnFp2_add(X, Y),
  _ = emcl:bnFp2_sub(X, Y),
  _ = emcl:bnFp2_mul(X, Y),
  _ = emcl:bnFp2_div(X, Y),

  ok.

sqrt_test() ->
    Fr  = emcl:rnd_Fr(),
    Fp  = emcl:rnd_Fp(),
    Fp2 = emcl:rnd_Fp2(),

    _ = emcl:bnFr_sqrt(Fr),
    _ = emcl:bnFp_sqrt(Fp),
    _ = emcl:bnFp2_sqrt(Fp2),

    ok.

g1_test() ->
  {ok, X} = emcl:bnG1_hash_and_map_to(<<"abc">>),
  {ok, Y} = emcl:bnG1_hash_and_map_to(<<"def">>),

  _ = emcl:bnG1_neg(X),
  _ = emcl:bnG1_dbl(X),
  _ = emcl:bnG1_normalize(X),
  _ = emcl:bnG1_add(X, Y),
  _ = emcl:bnG1_sub(X, Y),

  ok.

g2_test() ->
  {ok, X} = emcl:bnG2_hash_and_map_to(<<"abc">>),
  {ok, Y} = emcl:bnG2_hash_and_map_to(<<"def">>),

  _ = emcl:bnG2_neg(X),
  _ = emcl:bnG2_dbl(X),
  _ = emcl:bnG2_normalize(X),
  _ = emcl:bnG2_add(X, Y),
  _ = emcl:bnG2_sub(X, Y),

  ok.

gt_test() ->
  X = emcl:bnGt_from_str(<<"1 2 3 4 5 6 7 8 9 10 11 12">>),
  Y = emcl:bnGt_from_str(<<"-1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12">>),

  _ = emcl:bnGt_neg(X),
  _ = emcl:bnGt_inv(X),
  _ = emcl:bnGt_inv_generic(X),
  _ = emcl:bnGt_sqr(X),
  _ = emcl:bnGt_add(X, Y),
  _ = emcl:bnGt_sub(X, Y),
  _ = emcl:bnGt_mul(X, Y),
  _ = emcl:bnGt_div(X, Y),

  ok.

mul_vec_test() ->
  Frs      = [ emcl:rnd_Fr() || _ <- lists:seq(1, 5) ],
  {ok, G1} = emcl:bnG1_hash_and_map_to(<<"abc">>),
  {ok, G2} = emcl:bnG2_hash_and_map_to(<<"abc">>),
  GT       = emcl:bnGt_from_str(<<"1 2 3 4 5 6 7 8 9 10 11 12">>),

  {ok, _} = emcl:bnG1_mul_vec(lists:duplicate(5, G1), Frs),
  {ok, _} = emcl:bnG2_mul_vec(lists:duplicate(5, G2), Frs),
  {ok, _} = emcl:bnGt_pow_vec(lists:duplicate(5, GT), Frs),

  ok.

hash_map_test() ->
  G1a = emcl:bnG1_hash_and_map_to(<<"this">>),
  G2a = emcl:bnG2_hash_and_map_to(<<"that">>),

  G1b = emcl:bnFp_map_to_G1(emcl:bnFp_hash_of(<<"this">>)),
  G2b = emcl:bnFp2_map_to_G2(emcl:bnFp2_hash_of(<<"that">>)),

  _ = emcl:bnFr_hash_of(<<"Test">>),

  ?assertEqual(G1a, G1b),
  ?assertEqual(G2a, G2b),

  ok.

pairing_test() ->
  {ok, G1} = emcl:bnG1_hash_and_map_to(<<"abc">>),
  {ok, G2} = emcl:bnG2_hash_and_map_to(<<"def">>),

  {ok, G1b} = emcl:bnG1_hash_and_map_to(<<"ABC">>),
  {ok, G2b} = emcl:bnG2_hash_and_map_to(<<"DEF">>),

  E1 = emcl:bn_pairing(G1, G2),
  E2 = emcl:bn_final_exp(emcl:bn_miller_loop(G1, G2)),

  ?assertEqual(E1, E2),

  {ok, Ex} = emcl:bn_miller_loop_vec([G1, G1b], [G2, G2b]),
  Ey = emcl:bnGt_mul(emcl:bn_pairing(G1, G2), emcl:bn_pairing(G1b, G2b)),

  ?assertEqual(Ex, Ey),

  ok.

equal_test() ->
  Fr  = emcl:rnd_Fr(),
  Fp  = emcl:rnd_Fp(),
  Fp2 = emcl:rnd_Fp2(),

  {ok, G1} = emcl:bnG1_hash_and_map_to(<<"abc">>),
  {ok, G2} = emcl:bnG2_hash_and_map_to(<<"def">>),
  GT       = emcl:bnGt_from_str(<<"1 2 3 4 5 6 7 8 9 10 11 12">>),

  true = emcl:bnFr_is_equal(Fr, Fr),
  true = emcl:bnFp_is_equal(Fp, Fp),
  true = emcl:bnFp2_is_equal(Fp2, Fp2),
  true = emcl:bnG1_is_equal(G1, G1),
  true = emcl:bnG2_is_equal(G2, G2),
  true = emcl:bnGt_is_equal(GT, GT),

  ok.

is_x_test() ->
  Fr  = emcl:rnd_Fr(),
  Fp  = emcl:rnd_Fp(),
  Fp2 = emcl:rnd_Fp2(),

  {ok, G1} = emcl:bnG1_hash_and_map_to(<<"abc">>),
  {ok, G2} = emcl:bnG2_hash_and_map_to(<<"def">>),
  GT       = emcl:bnGt_from_str(<<"1 2 3 4 5 6 7 8 9 10 11 12">>),

  ?assertEqual(true, is_boolean(emcl:bnFr_is_zero(Fr))),
  ?assertEqual(true, is_boolean(emcl:bnFr_is_one(Fr))),
  ?assertEqual(true, is_boolean(emcl:bnFr_is_odd(Fr))),
  ?assertEqual(true, is_boolean(emcl:bnFr_is_valid(Fr))),
  ?assertEqual(true, is_boolean(emcl:bnFr_is_negative(Fr))),

  ?assertEqual(true, is_boolean(emcl:bnFp_is_zero(Fp))),
  ?assertEqual(true, is_boolean(emcl:bnFp_is_one(Fp))),
  ?assertEqual(true, is_boolean(emcl:bnFp_is_odd(Fp))),
  ?assertEqual(true, is_boolean(emcl:bnFp_is_valid(Fp))),
  ?assertEqual(true, is_boolean(emcl:bnFp_is_negative(Fp))),

  ?assertEqual(true, is_boolean(emcl:bnFp2_is_zero(Fp2))),
  ?assertEqual(true, is_boolean(emcl:bnFp2_is_one(Fp2))),

  ?assertEqual(true, is_boolean(emcl:bnG1_is_zero(G1))),
  ?assertEqual(true, is_boolean(emcl:bnG1_is_valid(G1))),
  ?assertEqual(true, is_boolean(emcl:bnG1_is_valid_order(G1))),

  ?assertEqual(true, is_boolean(emcl:bnG2_is_zero(G2))),
  ?assertEqual(true, is_boolean(emcl:bnG2_is_valid(G2))),
  ?assertEqual(true, is_boolean(emcl:bnG2_is_valid_order(G2))),

  ?assertEqual(true, is_boolean(emcl:bnGt_is_zero(GT))),
  ?assertEqual(true, is_boolean(emcl:bnGt_is_one(GT))),

  ok.

lagrange_test() ->
  Frs  = [ emcl:rnd_Fr() || _ <- lists:seq(1, 5) ],
  Frs2 = [ emcl:rnd_Fr() || _ <- lists:seq(1, 5) ],
  G1s  = [ element(2, emcl:bnG1_hash_and_map_to(integer_to_binary(I))) || I <- lists:seq(1, 5) ],
  G2s  = [ element(2, emcl:bnG2_hash_and_map_to(integer_to_binary(I))) || I <- lists:seq(1, 5) ],

  {ok, _} = emcl:bnFr_lagrange_interpolation(Frs, Frs2),
  {ok, _} = emcl:bnG1_lagrange_interpolation(Frs, G1s),
  {ok, _} = emcl:bnG2_lagrange_interpolation(Frs, G2s),

  Fr      = emcl:rnd_Fr(),
  {ok, _} = emcl:bnFr_eval_polynomial(Frs2, Fr),
  {ok, _} = emcl:bnG1_eval_polynomial(G1s, Fr),
  {ok, _} = emcl:bnG2_eval_polynomial(G2s, Fr),

  ok.

construct_test() ->
  I1 = 18590416872094568093245860,
  I2 = 832110056234918438930585923958,
  Fr = emcl:mk_Fr(I1),
  Fp = emcl:mk_Fp(I2),

  ?assertEqual(I1, emcl:bnFr_to_int(Fr)),
  ?assertEqual(I2, emcl:bnFp_to_int(Fp)),

  G1 = emcl:mk_G1(I1, I1, I1),
  G2 = emcl:mk_G2(I1, I2, I1, I2, I1, I2),

  ?assertEqual({I1, I1, I1}, emcl:bnG1_to_int(G1)),
  ?assertEqual({{I1, I2}, {I1, I2}, {I1, I2}}, emcl:bnG2_to_int(G2)),

  Gt = emcl:mk_Gt(I1, I2, I1, I2, I1, I2, I1, I2, I1, I2, I1, I2),
  ?assertEqual({I1, I2, I1, I2, I1, I2, I1, I2, I1, I2, I1, I2},
               emcl:bnGt_to_int(Gt)),

  B1 = emcl:bnFp_to_bin(emcl:mk_Fp(I1)),
  B2 = emcl:bnFp_to_bin(emcl:mk_Fp(I2)),

  ?assertEqual({B1, B1, B1}, emcl:bnG1_to_bin(G1)),
  ?assertEqual({{B1, B2}, {B1, B2}, {B1, B2}}, emcl:bnG2_to_bin(G2)),
  ?assertEqual({B1, B2, B1, B2, B1, B2, B1, B2, B1, B2, B1, B2}, emcl:bnGt_to_bin(Gt)),

  ok.
