-module(emcl_eqc).
-compile([export_all, nowarn_export_all]).

-include_lib("eqc/include/eqc.hrl").

%%
%% Fr tests
%%
i2fr(I) -> emcl:mk_Fr(I).

prop_fr_add() ->
  ?FORALL({X, Y}, {gen_number(), gen_number()},
    is_eq(i2fr(X + Y), emcl:bnFr_add(i2fr(X), i2fr(Y)))).

prop_fr_sub() ->
  ?FORALL({X, Y}, {gen_number(), gen_number()},
    is_eq(i2fr(X - Y), emcl:bnFr_sub(i2fr(X), i2fr(Y)))).

prop_fr_mul() ->
  ?FORALL({X, Y}, {gen_number(), gen_number()},
    is_eq(i2fr(X * Y), emcl:bnFr_mul(i2fr(X), i2fr(Y)))).

prop_fr_div() ->
  ?FORALL({X, Y}, {gen_number(), gen_number()},
    is_eq(i2fr(X), emcl:bnFr_div(emcl:bnFr_mul(i2fr(Y), i2fr(X)), i2fr(Y)))).

prop_fr_sqr() ->
  ?FORALL(X, gen_number(),
    is_eq(i2fr(X * X), emcl:bnFr_sqr(i2fr(X)))).

prop_fr_inv() ->
  ?FORALL(X, gen_number(),
    is_eq(i2fr(X), emcl:bnFr_inv(emcl:bnFr_inv(i2fr(X))))).

prop_fr_neg() ->
  ?FORALL(X, gen_number(),
    is_eq(i2fr(X), emcl:bnFr_neg(emcl:bnFr_neg(i2fr(X))))).

prop_fr_add_commutative() ->
  ?FORALL({X, Y}, {gen_fr(), gen_fr()},
    is_eq(emcl:bnFr_add(X, Y), emcl:bnFr_add(Y, X))).

prop_fr_add_associative() ->
  ?FORALL({X, Y, Z}, {gen_fr(), gen_fr(), gen_fr()},
    is_eq(emcl:bnFr_add(X, emcl:bnFr_add(Y, Z)),
          emcl:bnFr_add(emcl:bnFr_add(X, Y), Z))).

prop_fr_mul_commutative() ->
  ?FORALL({X, Y}, {gen_fr(), gen_fr()},
    is_eq(emcl:bnFr_mul(X, Y), emcl:bnFr_mul(Y, X))).

prop_fr_mul_associative() ->
  ?FORALL({X, Y, Z}, {gen_fr(), gen_fr(), gen_fr()},
    is_eq(emcl:bnFr_mul(X, emcl:bnFr_mul(Y, Z)),
          emcl:bnFr_mul(emcl:bnFr_mul(X, Y), Z))).

%%
%% Fp tests
%%
i2fp(I) -> emcl:mk_Fp(I).

prop_fp_add() ->
  ?FORALL({X, Y}, {gen_number(), gen_number()},
    is_eq(i2fp(X + Y), emcl:bnFp_add(i2fp(X), i2fp(Y)))).

prop_fp_sub() ->
  ?FORALL({X, Y}, {gen_number(), gen_number()},
    is_eq(i2fp(X - Y), emcl:bnFp_sub(i2fp(X), i2fp(Y)))).

prop_fp_mul() ->
  ?FORALL({X, Y}, {gen_number(), gen_number()},
    is_eq(i2fp(X * Y), emcl:bnFp_mul(i2fp(X), i2fp(Y)))).

prop_fp_div() ->
  ?FORALL({X, Y}, {gen_number(), gen_number()},
    is_eq(i2fp(X), emcl:bnFp_div(emcl:bnFp_mul(i2fp(Y), i2fp(X)), i2fp(Y)))).

prop_fp_sqr() ->
  ?FORALL(X, gen_number(),
    is_eq(i2fp(X * X), emcl:bnFp_sqr(i2fp(X)))).

prop_fp_inv() ->
  ?FORALL(X, gen_number(),
    is_eq(i2fp(X), emcl:bnFp_inv(emcl:bnFp_inv(i2fp(X))))).

prop_fp_neg() ->
  ?FORALL(X, gen_number(),
    is_eq(i2fp(X), emcl:bnFp_neg(emcl:bnFp_neg(i2fp(X))))).

prop_fp_add_commutative() ->
  ?FORALL({X, Y}, {gen_fp(), gen_fp()},
    is_eq(emcl:bnFp_add(X, Y), emcl:bnFp_add(Y, X))).

prop_fp_add_associative() ->
  ?FORALL({X, Y, Z}, {gen_fp(), gen_fp(), gen_fp()},
    is_eq(emcl:bnFp_add(X, emcl:bnFp_add(Y, Z)),
          emcl:bnFp_add(emcl:bnFp_add(X, Y), Z))).

prop_fp_mul_commutative() ->
  ?FORALL({X, Y}, {gen_fp(), gen_fp()},
    is_eq(emcl:bnFp_mul(X, Y), emcl:bnFp_mul(Y, X))).

prop_fp_mul_associative() ->
  ?FORALL({X, Y, Z}, {gen_fp(), gen_fp(), gen_fp()},
    is_eq(emcl:bnFp_mul(X, emcl:bnFp_mul(Y, Z)),
          emcl:bnFp_mul(emcl:bnFp_mul(X, Y), Z))).

%%
%% Fp2 tests
%%
i2fp2(I) -> emcl:mk_Fp2(I, 0).

prop_fp2_add() ->
  ?FORALL({X, Y}, {gen_number(), gen_number()},
    is_eq(i2fp2(X + Y), emcl:bnFp2_add(i2fp2(X), i2fp2(Y)))).

prop_fp2_sub() ->
  ?FORALL({X, Y}, {gen_number(), gen_number()},
    is_eq(i2fp2(X - Y), emcl:bnFp2_sub(i2fp2(X), i2fp2(Y)))).

prop_fp2_mul() ->
  ?FORALL({X, Y}, {gen_number(), gen_number()},
    is_eq(i2fp2(X * Y), emcl:bnFp2_mul(i2fp2(X), i2fp2(Y)))).

prop_fp2_div() ->
  ?FORALL({X, Y}, {gen_number(), gen_number()},
    is_eq(i2fp2(X), emcl:bnFp2_div(emcl:bnFp2_mul(i2fp2(Y), i2fp2(X)), i2fp2(Y)))).

prop_fp2_sqr() ->
  ?FORALL(X, gen_number(),
    is_eq(i2fp2(X * X), emcl:bnFp2_sqr(i2fp2(X)))).

prop_fp2_inv() ->
  ?FORALL(X, gen_number(),
    is_eq(i2fp2(X), emcl:bnFp2_inv(emcl:bnFp2_inv(i2fp2(X))))).

prop_fp2_neg() ->
  ?FORALL(X, gen_number(),
    is_eq(i2fp2(X), emcl:bnFp2_neg(emcl:bnFp2_neg(i2fp2(X))))).

prop_fp2_add_commutative() ->
  ?FORALL({X, Y}, {gen_fp2(), gen_fp2()},
    is_eq(emcl:bnFp2_add(X, Y), emcl:bnFp2_add(Y, X))).

prop_fp2_add_associative() ->
  ?FORALL({X, Y, Z}, {gen_fp2(), gen_fp2(), gen_fp2()},
    is_eq(emcl:bnFp2_add(X, emcl:bnFp2_add(Y, Z)),
          emcl:bnFp2_add(emcl:bnFp2_add(X, Y), Z))).

prop_fp2_mul_commutative() ->
  ?FORALL({X, Y}, {gen_fp2(), gen_fp2()},
    is_eq(emcl:bnFp2_mul(X, Y), emcl:bnFp2_mul(Y, X))).

prop_fp2_mul_associative() ->
  ?FORALL({X, Y, Z}, {gen_fp2(), gen_fp2(), gen_fp2()},
    is_eq(emcl:bnFp2_mul(X, emcl:bnFp2_mul(Y, Z)),
          emcl:bnFp2_mul(emcl:bnFp2_mul(X, Y), Z))).

%%
%% G1 tests
%%
i2g1(I) -> emcl:mk_G1(I, 0, 0).

prop_g1_add_commutative() ->
  ?FORALL({X, Y}, {gen_g1(), gen_g1()},
    is_eq(emcl:bnG1_add(X, Y), emcl:bnG1_add(Y, X))).

prop_g1_add_associative() ->
  ?FORALL([X, Y, Z], vector(3, gen_g1()),
    is_eq(emcl:bnG1_add(X, emcl:bnG1_add(Y, Z)),
          emcl:bnG1_add(emcl:bnG1_add(X, Y), Z))).

prop_g1_neg() ->
  ?FORALL(X, gen_g1(), emcl:bnG1_is_zero(emcl:bnG1_add(X, emcl:bnG1_neg(X)))).

prop_g1_dbl() ->
  ?FORALL(X, gen_g1(), is_eq(emcl:bnG1_add(X, X), emcl:bnG1_dbl(X))).

prop_g1_mul2() ->
  ?FORALL(G, gen_g1(),
    is_eq(emcl:bnG1_mul(G, i2fr(2)), emcl:bnG1_add(G, G))).

prop_g1_mul3() ->
  ?FORALL(G, gen_g1(),
    is_eq(emcl:bnG1_mul(G, i2fr(3)), emcl:bnG1_add(G, emcl:bnG1_add(G, G)))).

prop_g1_mul4() ->
  ?FORALL(G, gen_g1(),
    is_eq(emcl:bnG1_mul(G, i2fr(4)), emcl:bnG1_add(emcl:bnG1_add(G, G), emcl:bnG1_add(G, G)))).

prop_g1_add_sub() ->
  ?FORALL([X, Y], vector(2, gen_g1()),
    is_eq(X, emcl:bnG1_sub(emcl:bnG1_add(X, Y), Y))).

%%
%% G2 tests
%%
i2g2(I) -> emcl:mk_G2(I, 0, 0).

prop_g2_add_commutative() ->
  ?FORALL({X, Y}, {gen_g2(), gen_g2()},
    is_eq(emcl:bnG2_add(X, Y), emcl:bnG2_add(Y, X))).

prop_g2_add_associative() ->
  ?FORALL([X, Y, Z], vector(3, gen_g2()),
    is_eq(emcl:bnG2_add(X, emcl:bnG2_add(Y, Z)),
          emcl:bnG2_add(emcl:bnG2_add(X, Y), Z))).

prop_g2_neg() ->
  ?FORALL(X, gen_g2(), emcl:bnG2_is_zero(emcl:bnG2_add(X, emcl:bnG2_neg(X)))).

prop_g2_dbl() ->
  ?FORALL(X, gen_g2(), is_eq(emcl:bnG2_add(X, X), emcl:bnG2_dbl(X))).

prop_g2_mul2() ->
  ?FORALL(G, gen_g2(),
    is_eq(emcl:bnG2_mul(G, i2fr(2)), emcl:bnG2_add(G, G))).

prop_g2_mul3() ->
  ?FORALL(G, gen_g2(),
    is_eq(emcl:bnG2_mul(G, i2fr(3)), emcl:bnG2_add(G, emcl:bnG2_add(G, G)))).

prop_g2_mul4() ->
  ?FORALL(G, gen_g2(),
    is_eq(emcl:bnG2_mul(G, i2fr(4)), emcl:bnG2_add(emcl:bnG2_add(G, G), emcl:bnG2_add(G, G)))).

prop_g2_add_sub() ->
  ?FORALL([X, Y], vector(2, gen_g2()),
    is_eq(X, emcl:bnG2_sub(emcl:bnG2_add(X, Y), Y))).

%%
%% Gt tests
%%
prop_gt_add_commutative() ->
  ?FORALL({X, Y}, {gen_gt(), gen_gt()},
    is_eq(emcl:bnGt_add(X, Y), emcl:bnGt_add(Y, X))).

prop_gt_add_associative() ->
  ?FORALL([X, Y, Z], vector(3, gen_gt()),
    is_eq(emcl:bnGt_add(X, emcl:bnGt_add(Y, Z)),
          emcl:bnGt_add(emcl:bnGt_add(X, Y), Z))).

prop_gt_mul_commutative() ->
  ?FORALL({X, Y}, {gen_gt(), gen_gt()},
    is_eq(emcl:bnGt_mul(X, Y), emcl:bnGt_mul(Y, X))).

prop_gt_mul_associative() ->
  ?FORALL({X, Y, Z}, {gen_gt(), gen_gt(), gen_gt()},
    is_eq(emcl:bnGt_mul(X, emcl:bnGt_mul(Y, Z)),
          emcl:bnGt_mul(emcl:bnGt_mul(X, Y), Z))).

prop_gt_neg() ->
  ?FORALL(X, gen_gt(), emcl:bnGt_is_zero(emcl:bnGt_add(X, emcl:bnGt_neg(X)))).

prop_gt_pow2() ->
  ?FORALL(G, gen_gt(),
    is_eq(emcl:bnGt_pow(G, i2fr(2)), emcl:bnGt_mul(G, G))).

prop_gt_mul3() ->
  ?FORALL(G, gen_gt(),
    is_eq(emcl:bnGt_pow(G, i2fr(3)), emcl:bnGt_mul(G, emcl:bnGt_mul(G, G)))).

prop_gt_mul4() ->
  ?FORALL(G, gen_gt(),
    is_eq(emcl:bnGt_pow(G, i2fr(4)), emcl:bnGt_mul(emcl:bnGt_mul(G, G), emcl:bnGt_mul(G, G)))).

prop_gt_add_sub() ->
  ?FORALL([X, Y], vector(2, gen_gt()),
    is_eq(X, emcl:bnGt_sub(emcl:bnGt_add(X, Y), Y))).

%%
%% Misc pairing
%%

prop_pairing_check() ->
  ?FORALL({G1, G2}, {gen_g1(), gen_g2()},
    begin
      E1 = emcl:bn_pairing(G1, G2),
      E2 = emcl:bn_pairing(emcl:bnG1_neg(G1), G2),
      emcl:bnGt_is_one(emcl:bnGt_mul(E1, E2))
    end).

prop_pairing_symmetry() ->
  ?FORALL({P, Q, A, B}, {gen_g1(), gen_g2(), gen_fr(), gen_fr()},
    begin
      AP = emcl:bnG1_mul(P, A),
      BQ = emcl:bnG2_mul(Q, B),
      E  = emcl:bn_pairing(P, Q),

      E1 = emcl:bnGt_pow(E, A),
      E2 = emcl:bn_pairing(AP, Q),

      E3 = emcl:bnGt_pow(E, B),
      E4 = emcl:bn_pairing(P, BQ),

      emcl:is_eq(E1, E2) andalso emcl:is_eq(E3, E4)
    end).

prop_multi_pairing() ->
  ?FORALL({[G1a, G1b], [G2a, G2b]}, {vector(2, gen_g1()), vector(2, gen_g2())},
    begin
      {ok, E1} = emcl:bn_miller_loop_vec([G1a, G1b], [G2a, G2b]),
      E2       = emcl:bnGt_mul(emcl:bn_pairing(G1a, G2a), emcl:bn_pairing(G1b, G2b)),

      is_eq(E1, E2)
    end).

prop_measure() ->
  ?FORALL({P1, Q1, P2, Q2, K, I}, {gen_g1(), gen_g1(), gen_g2(), gen_g2(),
                                   gen_fr(), choose(0, 16#ffffffffffffffffffffffffffffffffff)},
  begin
    {T1_1, _} = timer:tc(fun() -> emcl:bnG1_neg(P1) end),
    {T1_2, _} = timer:tc(fun() -> emcl:bnG1_normalize(P1) end),
    {T1_3, _} = timer:tc(fun() -> emcl:bnG1_is_valid(P1) end),
    {T1_4, _} = timer:tc(fun() -> emcl:bnG1_is_zero(P1) end),
    {T1_5, _} = timer:tc(fun() -> emcl:bnG1_add(P1, Q1) end),
    {T1_6, _} = timer:tc(fun() -> emcl:bnG1_mul(P1, K) end),

    {T2_1, _} = timer:tc(fun() -> emcl:bnG2_neg(P2) end),
    {T2_2, _} = timer:tc(fun() -> emcl:bnG2_normalize(P2) end),
    {T2_3, _} = timer:tc(fun() -> emcl:bnG2_is_valid(P2) end),
    {T2_4, _} = timer:tc(fun() -> emcl:bnG2_is_zero(P2) end),
    {T2_5, _} = timer:tc(fun() -> emcl:bnG2_add(P2, Q2) end),
    {T2_6, _} = timer:tc(fun() -> emcl:bnG2_mul(P2, K) end),

    Gt  = emcl:bn_pairing(P1, P2),
    Gt2 = emcl:bn_pairing(Q1, Q2),

    {TT_1, _} = timer:tc(fun() -> emcl:bnGt_inv(Gt) end),
    {TT_2, _} = timer:tc(fun() -> emcl:bnGt_add(Gt, Gt2) end),
    {TT_3, _} = timer:tc(fun() -> emcl:bnGt_mul(Gt, Gt2) end),
    {TT_4, _} = timer:tc(fun() -> emcl:bnGt_pow(Gt, K) end),
    {TT_5, _} = timer:tc(fun() -> emcl:bnGt_is_one(Gt) end),
    {TT_6, _} = timer:tc(fun() -> emcl:bn_pairing(P1, P2) end),
    {TT_7, _} = timer:tc(fun() -> emcl:bn_miller_loop(P1, P2) end),
    {TT_8, _} = timer:tc(fun() -> emcl:bn_final_exp(Gt) end),

    {TC_1, Fr} = timer:tc(fun() -> emcl:mk_Fr(I) end),
    {TC_2, Fp} = timer:tc(fun() -> emcl:mk_Fp(I) end),
    {TC_3, _} = timer:tc(fun() ->  emcl:pp(Fr) end),
    {TC_4, _} = timer:tc(fun() ->  emcl:pp(Fp) end),

    measure(g1_neg__, T1_1,
    measure(g1_norm_, T1_2,
    measure(g1_valid, T1_3,
    measure(g1_zero_, T1_4,
    measure(g1_add__, T1_5,
    measure(g1_mul__, T1_6,
    measure(g2_neg__, T2_1,
    measure(g2_norm_, T2_2,
    measure(g2_valid, T2_3,
    measure(g2_zero_, T2_4,
    measure(g2_add__, T2_5,
    measure(g2_mul__, T2_6,
    measure(gt_inv__, TT_1,
    measure(gt_add__, TT_2,
    measure(gt_mul__, TT_3,
    measure(gt_pow__, TT_4,
    measure(gt_one__, TT_5,
    measure(gt_pair_, TT_6,
    measure(gt_mill_, TT_7,
    measure(gt_final, TT_8,
    measure(c_i2fr__, TC_1,
    measure(c_i2fp__, TC_2,
    measure(c_fr2i__, TC_3,
    measure(c_fp2i__, TC_4,
            true))))))))))))))))))))))))
    end).




%%
%% Compress/Decompress
%%

prop_compress_g1() ->
  ?FORALL(G1, gen_g1(),
    begin
      {ok, CG1} = emcl:compress_G1(G1),
      is_binary(CG1)
    end).

prop_decompress_g1() ->
  ?FORALL(G1, gen_g1(),
    begin
      {ok, CG1} = emcl:compress_G1(G1),
      {ok, G1b} = emcl:decompress_G1(CG1),
      is_eq(G1, G1b)
    end).

%%
%% Generators
%%

gen_number() ->
  weighted_default({5, choose(1, 20)}, {1, choose(1, 10000000)}).

gen_fr() ->
  weighted_default({2, ?LET(X, choose(1, 500), i2fr(X))},
                   {1, return(emcl:rnd_Fr())}).

gen_fp() ->
  weighted_default({2, ?LET(X, choose(1, 500), i2fp(X))},
                   {1, return(emcl:rnd_Fp())}).

gen_fp2() ->
  weighted_default({2, ?LET(X, choose(1, 500), i2fp2(X))},
                   {1, return(emcl:rnd_Fp2())}).


g1_infinity() -> emcl:bnG1_from_str(<<"0">>).

gen_g1() ->
  frequency([{1, g1_infinity()},
             {50,  ?SUCHTHAT(G1, ?LET(X, binary(6), return(element(2, emcl:bnG1_hash_and_map_to(X)))),
                             emcl:bnG1_is_valid(G1))}]).
gen_g2() ->
  ?SUCHTHAT(G1, ?LET(X, binary(6), return(element(2, emcl:bnG2_hash_and_map_to(X)))),
            emcl:bnG2_is_valid(G1)).

gen_gt() -> ?LET({X, Y}, {gen_g1(), gen_g2()}, return(emcl:bn_pairing(X, Y))).

is_eq(A, B) ->
  ?WHENFAIL(eqc:format("~140p \n /= ~140p\n<==> ~s /= ~s\n", [A, B, emcl:pp(A), emcl:pp(B)]),
            emcl:is_eq(A, B)).
