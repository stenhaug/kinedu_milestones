bifactor_to_specific <- function(x){
	out <- NA
	if(x == "bifact_all"){
		out <- as.numeric(as.factor(areas$area))
	}
	else if(x == "bifact_phys"){
		out <- (item_area == "Physical") + 1
	}
	out
}

bifactor_factors_splits_df_to_splits_with_log_lik <-
	function(factors, splits_df, n_cycles, verbose){
		splits_df %>%
			mutate(
				model =
					splits %>%
					map(
						~ list(
							data = training(.),
							model = bifactor_to_specific(factors),
							TOL = 0.0002, # could increase
							technical = list(theta_lim = c(-6, 6), NCYCLES = n_cycles),
							verbose = verbose
						)
					) %>%
					map(~ do.call(bfactor, .)),
				log_lik_test =
					map2_dbl(
						splits,
						model,
						~ calc_log_lik_ghq2(.y, testing(.x))
					)
			)
	}
