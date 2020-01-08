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

milestones <-
    d_raw %>%
    select(abs_183:color_679) %>%
    slice(3) %>%
    gather(code, name) %>%
    mutate(short_name = str_sub(name, start = 0, end = 40),
           code = str_replace(code, "^d_","d"),
           code = str_replace(code, "^e_","")) %>%
    mutate(code2 = code) %>%
    separate(code2, into = c("category","number")) %>%
    select(-number)

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

mod3 %>% write_rds("scratch_mod3.rds")

x <- summary(mod3, rotate = "varimax")

ms

what <-
    x$rotF %>%
    as_tibble() %>%
    mutate(paste = row.names(x$rotF)) %>%
    left_join(areas)

what %>%
    select(Group = area, F1, F2, F3) %>%
    gather(var, val, -Group) %>%
    mutate(val = -val) %>%
    ggplot(aes(x = val, fill = Group)) +
    ggridges::geom_density_line(alpha = 0.5) +
    facet_wrap(~ var, ncol = 1) +
    labs(
        x = "Item discrimination (or slope)",
        y = "Density",
        title = "Milestone loadings by factor"
    )

f <- fscores(mod3, rotate = "varimax")

f %>%
    as_tibble() %>%
    mutate(age = ages$age) %>%
    gather(var, val, -age) %>%
    mutate(val = -val) %>%
    ggplot(aes(x = age, y = val)) +
    geom_point(alpha = 0.1) +
    facet_wrap(~ var, ncol = 1) +
    geom_smooth(method = "lm") +
    labs(
        x = "Age (in months)",
        y = "Factor score",
        title = "The first factor is highly associated with age"
    )

mean(row.names(d_mat) == ages$id)
