a <- results %>% select(factors, itemtype, model_full, fscores)

model_fscores_item_to_proba <- function(model, fscores, item){
	tibble(
		proba = probtrace(extract.item(model, item), fscores)[, 2],
		outcome = responses_wide[, -1][[item]]
	) %>%
		mutate(
			pred = as.numeric(proba >= 0.5)
		) %>%
		summarize(mean(outcome == pred)) %>%
		pull()
}

out <- list()

for (i in 1:414){
	out[[i]] <-
		a %>%
		mutate(
			acc = map2_dbl(model_full, fscores, model_fscores_item_to_proba, i)
		) %>%
		select(factors, itemtype, acc)
}

out %>%
	bind_rows(.id = "item") %>%
	group_by(factors, itemtype) %>%
	summarize(mean(acc))

out

first <- function(model, fscores, item){
	tibble(
		proba = probtrace(extract.item(model, item), fscores)[, 2],
		outcome = responses_wide[, -1][[item]]
	)
}

b <-
	a %>%
	mutate(
		acc = map2(model_full, fscores, first, 411)
	)

bind_rows(b$acc[[2]] %>% mutate(person = row_number()), b$acc[[3]] %>% mutate(person = row_number()), .id = "model") %>%
	spread(model, proba) %>%
	rename("simple" = `1`, "complex" = `2`) %>%
	mutate(
		simple_correct = outcome == (simple > 0.5),
		complex_correct = outcome == (complex > 0.5)
	) %>%
	filter(simple_correct != complex_correct) %>%
	count(simple_correct, complex_correct)

# why person 69 so wild
coef(a$model_full[[2]], simplify = TRUE)$items[411, ]
a$fscores[[2]][14, ]
boot::inv.logit(7.175 * 0.2 + -2.27)

coef(a$model_full[[3]], simplify = TRUE)$items[411, ]
a$fscores[[3]][14, ]
boot::inv.logit(0 * -6.03 + -1.55 * -0.812 + -0.0232)

bind_rows(b$acc[[1]] %>% mutate(person = row_number()), b$acc[[2]] %>% mutate(person = row_number()), .id = "model") %>%
	spread(model, proba) %>%
	rename("rasch" = `1`, "complex" = `2`) %>%
	ggplot(aes(x = rasch, y = complex)) +
	geom_point()

bind_rows(b$acc[[1]] %>% mutate(person = row_number()), b$acc[[2]] %>% mutate(person = row_number()), .id = "model") %>%
	spread(model, proba) %>%
	rename("rasch" = `1`, "complex" = `2`) %>%
	gather(var, val, -outcome, -person) %>%
	ggplot(aes(x = val)) +
	geom_histogram() +
	facet_wrap(~ var)
