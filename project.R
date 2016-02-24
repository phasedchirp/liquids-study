library(rethinking)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

source("processing.R")

prop.table(xtabs(~CHOICE,liquidsAll))


modelL <- map2stan(
  alist(
    CHOICE ~ dcategorical(softmax(0,r,n,w)),
    #l <- a_l ,
    n <- a_n ,
    r <- a_r ,
    w <- a_w ,
    #l <- a[1] + bC[1]*TARGET + bL[1]*LIQUID + bLang[1]*lang,
    #r <- a[2] + bC[2]*TARGET + bL[2]*LIQUID + bLang[2]*lang,
    #n <- a[3] + bC[3]*TARGET + bL[3]*LIQUID + bLang[3]*lang,
    #w <- a[4] + bC[4]*TARGET + bL[4]*LIQUID + bLang[4]*lang,
    # Priors:
    # Intercept:
    c(a_l,a_n,a_r,a_w) ~ dnorm(0,5)
    # Consonant (actually going to need to be more complex):
#     bC ~ dnorm(0,5),
    # Liquid:
#     bL_l ~ dnorm(0,5),
#     bL_r ~ dnorm(0,5),
#     bL_n ~ dnorm(0,5),
#     bL_w ~ dnorm(0,5),
#     # Vowel:
#     bV[VOWEL] ~ dnorm(0,5),
#     bV_i ~ dnorm(0,5),
#     bV_i ~ dnorm(0,5),
#     # Position:
#     bP ~ dnorm(0,5),
#     # Language
#     bLang ~ dnorm(0,5)
  ),
  data = list(CHOICE = as.numeric(liquidsAll$CHOICE)),cores=4,chains=4
)
