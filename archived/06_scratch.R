# are we ignoring something temporal?
# d has gestation kinder diagnosis etc
# add in age

milestones <-
    readxl::read_excel("data/norming2/0-48 milestones y abreviaciones (viejos vs nuevos).xlsx") %>%
    select(id = 1, area = 5, abbrev = 10, month = 6, eng = 4)

library(tidyverse)
library(mirt)
models <- read_rds("6_mirts_2.rds")

midcut <- function(x, from, to, by){
    x <- cut(x,seq(from,to,by),include.lowest=T)
    vec <- seq(from+by/2,to-by/2,by)
    names(vec) <- levels(x)
    unname(vec[x])
}

# look at models ----------------------------------------------------------
models %>%
    select_if(Negate(is.list)) %>%
    View()

models %>%
    ggplot(aes(x = n_pars, y = log_lik, label = paste0(factors, "_", itemtype))) +
    geom_point() +
    ggrepel::geom_text_repel()

models %>%
    ggplot(aes(x = n_pars, y = ll_person_item, label = paste0(factors, "_", itemtype))) +
    geom_point() +
    ggrepel::geom_text_repel()

# how can we look at one model? -------------------------------------------
a_model <- models$model_full[[7]]

a_model@Model$factorNames

kids <-
    fscores(a_model) %>%
    as_tibble() %>%
    mutate(id = rownames(d_mat)) %>%
    left_join(d %>% group_by(id) %>% summarize(age = age[1])) %>%
    select(id, age, everything())

names(kids)[str_starts(names(kids), "F\\d")] <-
    paste0("f_", a_model@Model$factorNames)

items <-
    coef(a_model, simplify = TRUE)$items %>%
    as_tibble() %>%
    mutate(id = parse_number(colnames(d_mat))) %>%
    left_join(milestones) %>%
    select(id:abbrev, everything())

names(items)[str_starts(names(items), "a\\d")] <-
    paste0("a_", a_model@Model$factorNames)

# kids
kids %>%
    mutate(age = midcut(age, 0, 60, 5)) %>%
    gather(var, val, -id, -age) %>%
    group_by(age, var) %>%
    summarize(mean = mean(val)) %>%
    ggplot(aes(x = age, y = mean)) +
    geom_point() +
    facet_wrap(~ var)

ggpairs(
    select(kids, -id),
    aes(color = midcut(age, 0, 60, 10) %>% as.factor(), alpha = 0.5),
    title = "Kids"
)

# items
items %>%
    select(area, abbrev, starts_with("a_"), d, g) %>%
    gather(var, val, -area, -abbrev) %>%
    group_by(area, var) %>%
    summarize(mean = mean(val)) %>%
    ggplot(aes(x = area, y = mean)) +
    geom_col() +
    facet_wrap(~ var, scales = "free_y") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

ggpairs(
    select(items, -id, -abbrev, -u, -month, -eng),
    aes(alpha = 0.25, color = area),
    title = "Items",
    lower = list(continuous = wrap("smooth"))
)
