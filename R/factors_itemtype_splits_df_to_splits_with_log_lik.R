factors_itemtype_splits_df_to_splits_with_log_lik <-
	function(factors, itemtype, splits_df, n_cycles, verbose, the_method){
		splits_df %>%
			mutate(
				model =
					splits %>%
					map(
						~ list(
							data = training(.),
							model = fix_factors(factors),
							itemtype = itemtype,
							TOL = 0.0002, # could crank
							technical = list(theta_lim = c(-6, 6), NCYCLES = n_cycles),
							verbose = verbose,
							method = the_method
						)
					) %>%
					map(~ do.call(mirt, .)),
				log_lik_test =
					map2_dbl(
						splits,
						model,
						~ calc_log_lik_ghq2(.y, testing(.x))
					)
			)
	}
