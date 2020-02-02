x <- models_exploratory$model_full[[5]]@Model$Theta %>% as_tibble()

x$V1 %>% unique()

9 * 9 * 9 * 9

factors <- 5
itemtype = "2PL"
splits_df <- splits_response_matrix

verbose = TRUE

n_cycles <- 10

the_method <- "MCEM"

x <- splits_df %>%
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
			map(~ do.call(mirt, .)))

splits <- x$splits[[1]]
model <- x$model[[1]]

data <- testing(splits)

calc_log_lik_ghq2(model, testing(splits))

x %>%
	mutate(
		log_lik_test =
			map2_dbl(
				splits,
				model,
				~ calc_log_lik_ghq2(.y, testing(.x))
			)
	)

if(!is.null(model@Internals$bfactor$specific)){

	print("using bifactor loglik")

	mirt:::Estep.bfactor(
		pars=model@ParObjects$pars,
		tabdata=make_fulldata(data),
		freq=rep(1, nrow(data)),
		Theta=model@Model$Theta,
		prior=model@Internals$bfactor$prior[[1]],
		Priorbetween=model@Internals$bfactor$Priorbetween[[1]],
		specific=model@Internals$bfactor$specific,
		sitems=model@Internals$bfactor$sitems,
		itemloc=model@Model$itemloc,
		CUSTOM.IND=model@Internals$CUSTOM.IND,
		Etable=TRUE
	)$expected %>%
		log() %>%
		sum(na.rm = TRUE)
} else {

	print("using mirt loglik")

	mirt:::Estep.mirt(
		pars = model@ParObjects$pars,
		tabdata = make_fulldata(data),
		freq = rep(1, nrow(data)),
		CUSTOM.IND = model@Internals$CUSTOM.IND,
		Theta = model@Model$Theta,
		prior = model@Internals$Prior[[1]],
		itemloc = model@Model$itemloc,
		full = FALSE,
		Etable = TRUE
	)$expected %>%
		log() %>%
		sum(na.rm = TRUE)
}

