-module(emcl).

%% API exports
-export([ int_to_bnFr/1
        , bnFr_random/0
        , bnFp_random/0

        , bnFr_neg/1
        , bnFr_inv/1
        , bnFr_sqr/1
        , bnFr_add/2
        , bnFr_sub/2
        , bnFr_mul/2
        , bnFr_div/2

        , bnG1_mul/2
        , bnG2_mul/2

        , bnGt_pow/2
        , bnGt_equal/2

        , bn_pairing/2

        , bnG1_hash_and_map_to/1
        , bnG2_hash_and_map_to/1]).

-export([pp/1]).

-type mcl_bnFp()  :: {fp,  binary()}.
-type mcl_bnFr()  :: {fr,  binary()}.
-type mcl_bnFp2() :: {fp2, binary()}.
-type mcl_bnG1()  :: {g1,  binary()}.
-type mcl_bnG2()  :: {g2,  binary()}.
-type mcl_bnGt()  :: {gt,  binary()}.

%%====================================================================
%% API functions
%%====================================================================
-spec int_to_bnFr(X :: integer()) -> mcl_bnFr().
int_to_bnFr(X) when is_integer(X) ->
  {ok, Fr} = emcl_nif:mcl_bn_fr_from_str(integer_to_binary(X)),
  {fr, Fr}.

-spec bnFr_random() -> mcl_bnFr().
bnFr_random() ->
  {ok, Fr} = emcl_nif:mcl_bn_fr_random(),
  {fr, Fr}.

-spec bnFp_random() -> mcl_bnFp().
bnFp_random() ->
  {ok, Fp} = emcl_nif:mcl_bn_fp_random(),
  {fp, Fp}.

%% Fr arithmetic
-spec bnFr_neg(X :: mcl_bnFr()) -> mcl_bnFr().
bnFr_neg({fr, X}) ->
  {ok, Z} = emcl_nif:mcl_bn_fr_neg(X),
  {fr, Z}.

-spec bnFr_inv(X :: mcl_bnFr()) -> mcl_bnFr().
bnFr_inv({fr, X}) ->
  {ok, Z} = emcl_nif:mcl_bn_fr_inv(X),
  {fr, Z}.

-spec bnFr_sqr(X :: mcl_bnFr()) -> mcl_bnFr().
bnFr_sqr({fr, X}) ->
  {ok, Z} = emcl_nif:mcl_bn_fr_sqr(X),
  {fr, Z}.

-spec bnFr_add(X :: mcl_bnFr(), Y :: mcl_bnFr()) -> mcl_bnFr().
bnFr_add({fr, X}, {fr, Y}) ->
  {ok, Z} = emcl_nif:mcl_bn_fr_add(X, Y),
  {fr, Z}.

-spec bnFr_sub(X :: mcl_bnFr(), Y :: mcl_bnFr()) -> mcl_bnFr().
bnFr_sub({fr, X}, {fr, Y}) ->
  {ok, Z} = emcl_nif:mcl_bn_fr_sub(X, Y),
  {fr, Z}.

-spec bnFr_mul(X :: mcl_bnFr(), Y :: mcl_bnFr()) -> mcl_bnFr().
bnFr_mul({fr, X}, {fr, Y}) ->
  {ok, Z} = emcl_nif:mcl_bn_fr_mul(X, Y),
  {fr, Z}.

-spec bnFr_div(X :: mcl_bnFr(), Y :: mcl_bnFr()) -> mcl_bnFr().
bnFr_div({fr, X}, {fr, Y}) ->
  {ok, Z} = emcl_nif:mcl_bn_fr_div(X, Y),
  {fr, Z}.

-spec bnG1_mul(A :: mcl_bnG1(), B :: mcl_bnFr()) -> mcl_bnG1().
bnG1_mul({g1, A}, {fr, B}) ->
  {ok, AB} = emcl_nif:mcl_bn_g1_mul(A, B),
  {g1, AB}.

-spec bnG1_hash_and_map_to(X :: binary()) -> mcl_bnG1().
bnG1_hash_and_map_to(X) ->
  {ok, G1} = emcl_nif:mcl_bn_g1_hash_and_map_to(X),
  {g1, G1}.

-spec bnG2_mul(A :: mcl_bnG2(), B :: mcl_bnFr()) -> mcl_bnG2().
bnG2_mul({g2, A}, {fr, B}) ->
  {ok, AB} = emcl_nif:mcl_bn_g2_mul(A, B),
  {g2, AB}.

-spec bnG2_hash_and_map_to(X :: binary()) -> mcl_bnG2().
bnG2_hash_and_map_to(X) ->
  {ok, G2} = emcl_nif:mcl_bn_g2_hash_and_map_to(X),
  {g2, G2}.

-spec bnGt_pow(A :: mcl_bnGt(), B :: mcl_bnFr()) -> mcl_bnGt().
bnGt_pow({gt, A}, {fr, B}) ->
  {ok, AB} = emcl_nif:mcl_bn_gt_pow(A, B),
  {gt, AB}.

-spec bnGt_equal(A :: mcl_bnGt(), B :: mcl_bnGt()) -> boolean().
bnGt_equal({gt, A}, {gt, B}) ->
  {ok, Res} = emcl_nif:mcl_bn_gt_equal(A, B),
  Res.

-spec bn_pairing(X :: mcl_bnG1(), Y :: mcl_bnG2()) -> mcl_bnGt().
bn_pairing({g1, X}, {g2, Y}) ->
  {ok, Z} = emcl_nif:mcl_bn_pairing(X, Y),
  {gt, Z}.

-spec pp(term()) -> iolist().
pp({fr, X}) -> {ok, Y} = emcl_nif:mcl_bn_fr_to_str(X), Y;
pp({fp, X}) -> {ok, Y} = emcl_nif:mcl_bn_fp_to_str(X), Y;
pp({g1, X}) -> {ok, Y} = emcl_nif:mcl_bn_g1_to_str(X), Y;
pp({g2, X}) -> {ok, Y} = emcl_nif:mcl_bn_g2_to_str(X), Y;
pp({gt, X}) -> {ok, Y} = emcl_nif:mcl_bn_gt_to_str(X), Y.

%%====================================================================
%% Internal functions
%%====================================================================

