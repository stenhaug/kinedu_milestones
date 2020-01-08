fix_factors <- function(x){
    out <- NA
    if(x == "phys"){
        out <-
            mirt.model(
                glue::glue(
                    "Phys = {paste(which(item_area == 'Physical'), collapse = ',')}
                NonPhys = {paste(which(item_area != 'Physical'), collapse = ',')}
                COV = Phys*NonPhys"
                )
            )
    }
    else if(x == "phys_ling"){
        out <-
            mirt.model(
                glue::glue(
                    "Phys = {paste(which(item_area == 'Physical'), collapse = ',')}
                Ling = {paste(which(item_area == 'Linguistic'), collapse = ',')}
                Other = {paste(which(!item_area %in% c('Physical', 'Linguistic')), collapse = ',')}"
                )
            )
    }
    else if(x == "interaction"){
        out <-
            mirt.model(
                "F1 = 1-414
                 F2 = 1-414
                 (F1*F2) = 1-414"
            )
    }
    else{
        out <- as.numeric(x)
    }
    out
}

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

bifactor_to_specific <- function(x){
    out <- NA
    if(x == "bifact_all"){
        out <- as.numeric(as.factor(item_area))
    }
    else if(x == "bifact_phys"){
        out <- (item_area == "Physical") + 1
    }
    out
}

BBBfactors_splits_df_to_splits_with_log_lik <-
    function(factors, splits_df, n_cycles, verbose){
        splits_df %>%
            mutate(
                model =
                    splits %>%
                    map(
                        ~ list(
                            data = training(.),
                            model = bifactor_to_specific(factors),
                            TOL = 0.0002, # could crank
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
