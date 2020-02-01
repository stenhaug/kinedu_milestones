library(tidyverse)
library(mirt)

d_mat <- read_rds(here::here("data-clean/d_mat.rds"))

mirts <- read_rds(here::here("02_mirts_2_add_5.rds"))

bifactor <- read_rds(here::here("02_bifactors.rds"))

full <-
    bind_rows(mirts, bifactor) %>%
    rename(out_of_sample = ll_person_item) %>%
    mutate(
        in_sample = exp(log_lik / nrow(d_mat))^(1/ncol(d_mat))
    ) %>%
    mutate(
        oos =
            splits_with_log_lik %>% map_dbl(~ sum(.$log_lik_test))
    ) %>%
    select(
        factors, itemtype, in_log_lik = log_lik, in_p = in_sample,
        out_log_lik = oos, out_p = out_of_sample, model_full, fscores, splits_with_log_lik)


summary(full$model_full[[4]], rotate = "none")

summary(full$model_full[[4]], rotate = "varimax")

summary(full$model_full[[4]], rotate = "oblimin")

get_prop_var <- function(object){
    F <- object@Fit$F
    SS <- apply(F^2, 2, sum)
    SS / nrow(F)
}

mirts2$model_full[[1]] %>%
    get_prop_var() %>%
    enframe() %>%
    ggplot(aes(x = name, y = value)) +
    geom_col() +
    labs(
        x = "factor",
        y = "Proportion Var",
        title = "4 factor model"
    )
