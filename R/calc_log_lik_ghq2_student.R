make_fulldata <- function(data){
	wrong <- 1 - data
	right <- data
	colnames(wrong) <- glue::glue("Item.{1:ncol(data)}_1")
	colnames(right) <- glue::glue("Item.{1:ncol(data)}_2")
	cbind(wrong, right)[, order(c(seq(ncol(wrong)), seq(ncol(right))))]
}

calc_log_lik_ghq2_student <- function(model, data){
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
		)$expected
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
		)$expected
	}
}

# examples
# data <- expand.table(LSAT6) %>% as.matrix()
# model <- mirt(data, 1, "3PL")
# calc_log_lik_ghq2(model, data)
# calc_log_lik_ghq(model, data, 50, 6)
