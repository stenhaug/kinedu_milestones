library(tidyverse)

areas <- read_rds("data-clean/areas.rds")
d <- read_rds("data-clean/d.rds")
d_mat <- read_rds("data-clean/d_mat.rds")
item_area <- areas$area[match(colnames(d_mat), areas$paste)]

ages <-
    d %>%
    group_by(id) %>%
    summarize(age = age[1]) %>%
    filter(age > 1)

d_mat <- d_mat[row.names(d_mat) %in% ages$id, ]

d <- d %>% filter(age > 1)

d_mat %>% apply(1, function(x) mean(is.na(x))) %>% table()

d_mat %>% apply(2, function(x) mean(is.na(x))) %>% table()

ms <- milestones %>% mutate(code = str_remove(code, " "))

areas %>%
    filter(paste %in% colnames(d_mat)) %>%
    group_by(area) %>%
    mutate(count = n()) %>%
    slice(1) %>%
    ungroup() %>%
    left_join(ms, by = c("paste" = "code")) %>%
    select(group = area, count, spanish = short_name) %>%
    arrange(desc(count)) %>%
    mutate(english = c(
        "Can stand on their toes",
        "Can find an object on the floor",
        "Babbles as if imitating conversations",
        "Complains when their activity is interrupted"
    )) %>%
    View()

mod3 <- mirt(d_mat, 3, "2PL")

d_mat
