source("processing.R")
library(rethinking)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())


round(prop.table(xtabs(~CHOICE+VOWEL,liquidsAll)),2)


counts <- liquidsAll %>% mutate(vowelID=as.numeric(VOWEL),
                                langID = ifelse(lang=="Cantonese",1,2)) %>%
  group_by(vowelID,langID) %>%
  summarize(l=sum(CHOICE=='l'),
            n=sum(CHOICE=='n'),
            r=sum(CHOICE=='r'),
            w=sum(CHOICE=='n'),total=sum(l,n,r,w)) %>%
  as.data.frame

modelL <- map2stan(
  alist(
    l ~ dpois(lambda_l) ,
    n ~ dpois(lambda_n) ,
    r ~ dpois(lambda_r) ,
    w ~ dpois(lambda_w) ,
    log(lambda_l) <-  bV_l[vowelID]+bL_l[langID],
    log(lambda_n) <-  bV_n[vowelID]+bL_n[langID],
    log(lambda_r) <-  bV_r[vowelID]+bL_r[langID],
    log(lambda_w) <-  bV_w[vowelID]+bL_w[langID],
    # Priors:
    # Intercept:
    #c(a_l,a_n,a_r,a_w) ~ dnorm(0,10)#,
    bV_l[vowelID] ~ dnorm(0,10),
    bV_n[vowelID] ~ dnorm(0,10),
    bV_r[vowelID] ~ dnorm(0,10),
    bV_w[vowelID] ~ dnorm(0,10),
    bL_l[langID] ~ dnorm(0,10),
    bL_n[langID] ~ dnorm(0,10),
    bL_r[langID] ~ dnorm(0,10),
    bL_w[langID] ~ dnorm(0,10)
  ),
  data = counts,iter=5000,cores=2,chains=4#, max_treedepth=15
)





# modelL <- map2stan(
#   alist(
#     CHOICE ~ dcategorical(softmax(0,r,n,w)),
#     #l <- a_l ,
#     n <- a_n ,
#     r <- a_r ,
#     w <- a_w ,
#     #l <- a[1] + bC[1]*TARGET + bL[1]*LIQUID + bLang[1]*lang,
#     #r <- a[2] + bC[2]*TARGET + bL[2]*LIQUID + bLang[2]*lang,
#     #n <- a[3] + bC[3]*TARGET + bL[3]*LIQUID + bLang[3]*lang,
#     #w <- a[4] + bC[4]*TARGET + bL[4]*LIQUID + bLang[4]*lang,
#     # Priors:
#     # Intercept:
#     c(a_l,a_n,a_r,a_w) ~ dnorm(0,5)
#     # Consonant (actually going to need to be more complex):
#     #     bC ~ dnorm(0,5),
#     # Liquid:
#     #     bL_l ~ dnorm(0,5),
#     #     bL_r ~ dnorm(0,5),
#     #     bL_n ~ dnorm(0,5),
#     #     bL_w ~ dnorm(0,5),
#     #     # Vowel:
#     #     bV[VOWEL] ~ dnorm(0,5),
#     #     bV_i ~ dnorm(0,5),
#     #     bV_i ~ dnorm(0,5),
#     #     # Position:
#     #     bP ~ dnorm(0,5),
#     #     # Language
#     #     bLang ~ dnorm(0,5)
#   ),
#   data = list(CHOICE = as.numeric(liquidsAll$CHOICE)),cores=4,chains=4
# )