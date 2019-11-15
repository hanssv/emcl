-module(emcl_nif).

-compile([export_all, nowarn_export_all]).

-on_load(init/0).

init() ->
    Dir = case code:priv_dir(emcl) of
              {error, bad_name} ->
                  filename:join(
                    filename:dirname(
                      filename:dirname(
                        code:which(?MODULE))), "priv");
              D -> D
          end,
    SoName = filename:join(Dir, atom_to_list(?MODULE)),
    erlang:load_nif(SoName, 0).

mcl_bn_g1_mul(_A, _B) -> erlang:nif_error(nif_not_loaded).
mcl_bn_g2_mul(_A, _B) -> erlang:nif_error(nif_not_loaded).
mcl_bn_g1_hash_and_map_to(_X) -> erlang:nif_error(nif_not_loaded).
mcl_bn_g2_hash_and_map_to(_X) -> erlang:nif_error(nif_not_loaded).
mcl_bn_pairing(_A, _B) -> erlang:nif_error(nif_not_loaded).
mcl_bn_gt_pow(_A, _B) -> erlang:nif_error(nif_not_loaded).
mcl_bn_gt_equal(_A, _B) -> erlang:nif_error(nif_not_loaded).

mcl_bn_fr_neg(_A)     -> erlang:nif_error(nif_not_loaded).
mcl_bn_fr_inv(_A)     -> erlang:nif_error(nif_not_loaded).
mcl_bn_fr_sqr(_A)     -> erlang:nif_error(nif_not_loaded).
mcl_bn_fr_add(_A, _B) -> erlang:nif_error(nif_not_loaded).
mcl_bn_fr_sub(_A, _B) -> erlang:nif_error(nif_not_loaded).
mcl_bn_fr_mul(_A, _B) -> erlang:nif_error(nif_not_loaded).
mcl_bn_fr_div(_A, _B) -> erlang:nif_error(nif_not_loaded).

mcl_bn_fr_to_str(_A) -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_to_str(_A) -> erlang:nif_error(nif_not_loaded).
mcl_bn_g1_to_str(_A) -> erlang:nif_error(nif_not_loaded).
mcl_bn_g2_to_str(_A) -> erlang:nif_error(nif_not_loaded).
mcl_bn_gt_to_str(_A) -> erlang:nif_error(nif_not_loaded).

mcl_bn_fr_from_str(_A) -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_from_str(_A) -> erlang:nif_error(nif_not_loaded).
mcl_bn_g1_from_str(_A) -> erlang:nif_error(nif_not_loaded).
mcl_bn_g2_from_str(_A) -> erlang:nif_error(nif_not_loaded).
mcl_bn_gt_from_str(_A) -> erlang:nif_error(nif_not_loaded).

mcl_bn_fr_random() -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_random() -> erlang:nif_error(nif_not_loaded).

