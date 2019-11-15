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

mcl_bn_miller_loop(_A, _B)     -> erlang:nif_error(nif_not_loaded).
mcl_bn_miller_loop_vec(_A, _B) -> erlang:nif_error(nif_not_loaded).
mcl_bn_final_exp(_A)           -> erlang:nif_error(nif_not_loaded).
mcl_bn_pairing(_A, _B)         -> erlang:nif_error(nif_not_loaded).

mcl_bn_fr_neg(_A)         -> erlang:nif_error(nif_not_loaded).
mcl_bn_fr_inv(_A)         -> erlang:nif_error(nif_not_loaded).
mcl_bn_fr_sqr(_A)         -> erlang:nif_error(nif_not_loaded).
mcl_bn_fr_sqrt(_A)        -> erlang:nif_error(nif_not_loaded).
mcl_bn_fr_add(_A, _B)     -> erlang:nif_error(nif_not_loaded).
mcl_bn_fr_sub(_A, _B)     -> erlang:nif_error(nif_not_loaded).
mcl_bn_fr_mul(_A, _B)     -> erlang:nif_error(nif_not_loaded).
mcl_bn_fr_div(_A, _B)     -> erlang:nif_error(nif_not_loaded).
mcl_bn_fr_is_equal(_A, _B) -> erlang:nif_error(nif_not_loaded).
mcl_bn_fr_is_zero(_A)     -> erlang:nif_error(nif_not_loaded).
mcl_bn_fr_is_one(_A)      -> erlang:nif_error(nif_not_loaded).
mcl_bn_fr_is_odd(_A)      -> erlang:nif_error(nif_not_loaded).
mcl_bn_fr_is_valid(_A)    -> erlang:nif_error(nif_not_loaded).
mcl_bn_fr_is_negative(_A) -> erlang:nif_error(nif_not_loaded).
mcl_bn_fr_lagrange_interpolation(_A, _B) -> erlang:nif_error(nif_not_loaded).
mcl_bn_fr_eval_polynomial(_A, _B)        -> erlang:nif_error(nif_not_loaded).

mcl_bn_fp_neg(_A)         -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_inv(_A)         -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_sqr(_A)         -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_sqrt(_A)        -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_add(_A, _B)     -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_sub(_A, _B)     -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_mul(_A, _B)     -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_div(_A, _B)     -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_is_equal(_A, _B) -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_is_zero(_A)     -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_is_one(_A)      -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_is_odd(_A)      -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_is_valid(_A)    -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_is_negative(_A) -> erlang:nif_error(nif_not_loaded).

mcl_bn_fp2_neg(_A)       -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp2_inv(_A)       -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp2_sqr(_A)       -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp2_sqrt(_A)      -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp2_add(_A, _B)   -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp2_sub(_A, _B)   -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp2_mul(_A, _B)   -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp2_div(_A, _B)   -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp2_is_equal(_A, _B) -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp2_is_zero(_A)   -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp2_is_one(_A)    -> erlang:nif_error(nif_not_loaded).

mcl_bn_g1_neg(_A)             -> erlang:nif_error(nif_not_loaded).
mcl_bn_g1_dbl(_A)             -> erlang:nif_error(nif_not_loaded).
mcl_bn_g1_normalize(_A)       -> erlang:nif_error(nif_not_loaded).
mcl_bn_g1_add(_A, _B)         -> erlang:nif_error(nif_not_loaded).
mcl_bn_g1_sub(_A, _B)         -> erlang:nif_error(nif_not_loaded).
mcl_bn_g1_mul(_A, _B)         -> erlang:nif_error(nif_not_loaded).
mcl_bn_g1_mul_vec(_A, _B)     -> erlang:nif_error(nif_not_loaded).
mcl_bn_g1_hash_and_map_to(_X) -> erlang:nif_error(nif_not_loaded).
mcl_bn_g1_is_equal(_A, _B)       -> erlang:nif_error(nif_not_loaded).
mcl_bn_g1_is_zero(_A)         -> erlang:nif_error(nif_not_loaded).
mcl_bn_g1_is_valid(_A)        -> erlang:nif_error(nif_not_loaded).
mcl_bn_g1_is_valid_order(_A)  -> erlang:nif_error(nif_not_loaded).
mcl_bn_g1_lagrange_interpolation(_A, _B) -> erlang:nif_error(nif_not_loaded).
mcl_bn_g1_eval_polynomial(_A, _B)        -> erlang:nif_error(nif_not_loaded).

mcl_bn_g2_neg(_A)             -> erlang:nif_error(nif_not_loaded).
mcl_bn_g2_dbl(_A)             -> erlang:nif_error(nif_not_loaded).
mcl_bn_g2_normalize(_A)       -> erlang:nif_error(nif_not_loaded).
mcl_bn_g2_add(_A, _B)         -> erlang:nif_error(nif_not_loaded).
mcl_bn_g2_sub(_A, _B)         -> erlang:nif_error(nif_not_loaded).
mcl_bn_g2_mul(_A, _B)         -> erlang:nif_error(nif_not_loaded).
mcl_bn_g2_mul_vec(_A, _B)     -> erlang:nif_error(nif_not_loaded).
mcl_bn_g2_hash_and_map_to(_X) -> erlang:nif_error(nif_not_loaded).
mcl_bn_g2_is_equal(_A, _B)       -> erlang:nif_error(nif_not_loaded).
mcl_bn_g2_is_zero(_A)         -> erlang:nif_error(nif_not_loaded).
mcl_bn_g2_is_valid(_A)        -> erlang:nif_error(nif_not_loaded).
mcl_bn_g2_is_valid_order(_A)  -> erlang:nif_error(nif_not_loaded).
mcl_bn_g2_lagrange_interpolation(_A, _B) -> erlang:nif_error(nif_not_loaded).
mcl_bn_g2_eval_polynomial(_A, _B)        -> erlang:nif_error(nif_not_loaded).

mcl_bn_gt_neg(_A)     -> erlang:nif_error(nif_not_loaded).
mcl_bn_gt_inv(_A)     -> erlang:nif_error(nif_not_loaded).
mcl_bn_gt_inv_generic(_A)     -> erlang:nif_error(nif_not_loaded).
mcl_bn_gt_sqr(_A)     -> erlang:nif_error(nif_not_loaded).
mcl_bn_gt_add(_A, _B) -> erlang:nif_error(nif_not_loaded).
mcl_bn_gt_sub(_A, _B) -> erlang:nif_error(nif_not_loaded).
mcl_bn_gt_mul(_A, _B) -> erlang:nif_error(nif_not_loaded).
mcl_bn_gt_div(_A, _B) -> erlang:nif_error(nif_not_loaded).
mcl_bn_gt_pow(_A, _B) -> erlang:nif_error(nif_not_loaded).
mcl_bn_gt_pow_vec(_A, _B) -> erlang:nif_error(nif_not_loaded).
mcl_bn_gt_is_equal(_A, _B) -> erlang:nif_error(nif_not_loaded).
mcl_bn_gt_is_zero(_A)   -> erlang:nif_error(nif_not_loaded).
mcl_bn_gt_is_one(_A)    -> erlang:nif_error(nif_not_loaded).

mcl_bn_fr_to_str(_A) -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_to_str(_A) -> erlang:nif_error(nif_not_loaded).
mcl_bn_g1_to_str(_A) -> erlang:nif_error(nif_not_loaded).
mcl_bn_g2_to_str(_A) -> erlang:nif_error(nif_not_loaded).
mcl_bn_gt_to_str(_A) -> erlang:nif_error(nif_not_loaded).

mcl_bn_fr_from_str(_A)  -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_from_str(_A)  -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp2_from_str(_A) -> erlang:nif_error(nif_not_loaded).
mcl_bn_g1_from_str(_A)  -> erlang:nif_error(nif_not_loaded).
mcl_bn_g2_from_str(_A)  -> erlang:nif_error(nif_not_loaded).
mcl_bn_gt_from_str(_A)  -> erlang:nif_error(nif_not_loaded).

mcl_bn_fr_random() -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_random() -> erlang:nif_error(nif_not_loaded).

mcl_bn_fr_hash_of(_A)    -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_hash_of(_A)    -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp2_hash_of(_A)   -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_map_to_g1(_A)  -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp2_map_to_g2(_A) -> erlang:nif_error(nif_not_loaded).

mcl_bn_fr_compress(_A)  -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_compress(_A)  -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp2_compress(_A) -> erlang:nif_error(nif_not_loaded).
mcl_bn_g1_compress(_A)  -> erlang:nif_error(nif_not_loaded).
mcl_bn_g2_compress(_A)  -> erlang:nif_error(nif_not_loaded).
mcl_bn_gt_compress(_A)  -> erlang:nif_error(nif_not_loaded).

mcl_bn_fr_decompress(_A)  -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp_decompress(_A)  -> erlang:nif_error(nif_not_loaded).
mcl_bn_fp2_decompress(_A) -> erlang:nif_error(nif_not_loaded).
mcl_bn_g1_decompress(_A)  -> erlang:nif_error(nif_not_loaded).
mcl_bn_g2_decompress(_A)  -> erlang:nif_error(nif_not_loaded).
mcl_bn_gt_decompress(_A)  -> erlang:nif_error(nif_not_loaded).
