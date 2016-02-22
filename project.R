library(rethinking)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source("processing.R")

xtabs(~error,liquidsAll)

modelL <- map2stan(
  alist(
    error ~ dcategorical(softmax(N,r,n,w)),
    N <- a[1] + bC[1]*TARGET + bL[1]*LIQUID + bLang[1]*lang,
    r <- a[2] + bC[2]*TARGET + bL[2]*LIQUID + bLang[2]*lang,
    n <- a[3] + bC[3]*TARGET + bL[3]*LIQUID + bLang[3]*lang,
    w <- a[4] + bC[4]*TARGET + bL[4]*LIQUID + bLang[4]*lang,
    # Priors:
    a ~ dnorm(0,5),
    bC ~ dnorm(0,5),
    bL ~ dnorm(0,5),
    bLang ~ dnorm(0,5)
  ),
  data = liquidsL
)
