get_kids <- function(fscores){
    fscores %>%
        as_tibble() %>%
        mutate(id = rownames(d_mat)) %>%
        left_join(d %>% group_by(id) %>% summarize(age = age[1])) %>%
        select(id, age, everything())
}

get_items <- function(model){
    coef(model, simplify = TRUE)$items %>%
        as_tibble() %>%
        mutate(id = parse_number(colnames(d_mat))) %>%
        left_join(milestones) %>%
        select(id:abbrev, everything())
}

midcut <- function(x, from, to, by){
    x <- cut(x,seq(from,to,by),include.lowest=T)
    vec <- seq(from+by/2,to-by/2,by)
    names(vec) <- levels(x)
    unname(vec[x])
}
