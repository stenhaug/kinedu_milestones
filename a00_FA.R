library(psych)

bfi_data <- bfi

bfi_data <- bfi_data[complete.cases(bfi_data),] %>% as_tibble()


bfi_cor <- cor(bfi_data)

#Factor analysis of the data
factors_data <- fa(r = bfi_cor, nfactors = 6)

factors_data



fit <- factanal(bfi_data, 3, rotation = "varimax")


load <- fit$loadings[ , 1:2]

plot(load, type = "n")
text(load,labels=names(bfi_data),cex=.7)


solution <- fa(r = cor(bfi_data), nfactors = 2, rotate = "oblimin")

plot(solution, labels=names(bfi_data), cex=.7, ylim=c(-.1,1))

solution

fit$correlation

?scree

scree(cor(bfi_data), pc = FALSE)


install.packages("nFactors")

library(nFactors)

ev <- eigen(cor(bfi_data)) # get eigenvalues
ap <- parallel(subject=nrow(bfi_data),var=ncol(bfi_data), rep=100, cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS)



