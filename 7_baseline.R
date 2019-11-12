# peek
d_mat[1:10, 1:3]

# verify with coin flip ---------------------------------------------------
tmp <- d_mat
tmp[ , ] <- 0.5
log_lik <- sum(rowSums(log(tmp)))
exp(log_lik / nrow(d_mat))^(1/ncol(d_mat))

# use the percent correct for each item -----------------------------------
mat <- matrix(colMeans(d_mat), nrow = nrow(d_mat), ncol = ncol(d_mat), byrow = TRUE)
log_lik <- sum(rowSums(log(mat)))
exp(log_lik / nrow(d_mat))^(1/ncol(d_mat))

# use percent correct conditional on age ----------------------------------
# get a feel
d %>%
    filter(code %in% sample(unique(d$code), 6)) %>%
    group_by(age = midcut(age, 0, 100, 5), code) %>%
    summarize(p = mean(response), n = n()) %>%
    ggplot(aes(x = age, y = p, size = n)) +
    geom_point() +
    facet_wrap(~ code)

# calculate
p_for_age <-
    d %>%
    group_by(midcut_age = midcut(age, 0, 100, 5), code) %>%
    summarize(p = mean(response))

p_for_age2 <-
    d %>%
    mutate(midcut_age = midcut(age, 0, 100, 5)) %>%
    left_join(p_for_age) %>%
    select(id, code, p) %>%
    spread(code, p)

# check
all(p_for_age2$id == row.names(d_mat))
all(names(p_for_age2)[-1] == colnames(d_mat))

# use p(data)
mat <- ifelse(d_mat == 1, as.matrix(select(p_for_age2, -id)), 1 - as.matrix(select(p_for_age2, -id)))

log_lik <- sum(rowSums(log(mat)))
exp(log_lik / nrow(d_mat))^(1/ncol(d_mat))

