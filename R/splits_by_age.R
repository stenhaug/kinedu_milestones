split_model_to_oos <- function(split, model){
	tibble(
		id = row.names(testing(split)),
		oos = calc_log_lik_ghq2_student(model, testing(split))
	)
}

splits_with_ll_to_oos <- function(splits_with_log_lik) {
	splits_with_log_lik %>%
		mutate(
			oos = map2(splits, model, split_model_to_oos)
		) %>%
		pull(oos) %>%
		bind_rows() %>%
		left_join(by_age)
}
