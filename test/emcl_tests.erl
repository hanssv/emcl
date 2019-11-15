-module(emcl_tests).

-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    A  = emcl:int_to_bnFr(123),
    B  = emcl:int_to_bnFr(456),
    AB = emcl:bnFr_mul(A, B),

    io:format("\nA = ~s; B = ~s\nA * B = ~s\n", [emcl:pp(A), emcl:pp(B), emcl:pp(AB)]),

    P = emcl:bnG1_hash_and_map_to(<<"this">>),
    Q = emcl:bnG2_hash_and_map_to(<<"that">>),

    io:format("P = ~s\nQ = ~s\n", [emcl:pp(P), emcl:pp(Q)]),

    AP = emcl:bnG1_mul(P, A),
    BQ = emcl:bnG2_mul(Q, B),
    io:format("AP = ~s\nBQ = ~s\n", [emcl:pp(AP), emcl:pp(BQ)]),


    E = emcl:bn_pairing(P, Q),
    io:format("E = ~s\n", [emcl:pp(E)]),

    E1 = emcl:bnGt_pow(E, A),
    E2 = emcl:bn_pairing(AP, Q),

    ?assertEqual(true, emcl:bnGt_equal(E1, E2)),

    E3 = emcl:bnGt_pow(E, B),
    E4 = emcl:bn_pairing(P, BQ),

    ?assertEqual(true, emcl:bnGt_equal(E3, E4)),

    %% ?assertEqual(A, B),

    _ = emcl:bnFp_random(),
    _ = emcl:bnFr_random(),

    ok.

fr_test() ->
    X = emcl:bnFr_random(),
    Y = emcl:bnFr_random(),

    _ = emcl:bnFr_neg(X),
    _ = emcl:bnFr_inv(X),
    _ = emcl:bnFr_sqr(X),
    _ = emcl:bnFr_add(X, Y),
    _ = emcl:bnFr_sub(X, Y),
    _ = emcl:bnFr_mul(X, Y),
    _ = emcl:bnFr_div(X, Y),

    ok.
