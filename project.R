source("processing.R")
library(rethinking)
#rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())


#round(prop.table(xtabs(~CHOICE+VOWEL,liquidsAll)),2)

#round(xtabs(~CHOICE+VOWEL+lang,liquidsAll),2)


counts <- liquidsAll %>% mutate(vowelID = as.numeric(VOWEL),
                                consID = as.numeric(cons),
                                lID = as.numeric(LIQUID)) %>%
  group_by(lID) %>%
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
    log(lambda_l) <-  bL_l[lID], #bV_l[vowelID] + 
    log(lambda_n) <-  bL_n[lID], #bV_n[vowelID] +
    log(lambda_r) <-  bL_r[lID], #bV_r[vowelID] + 
    log(lambda_w) <-  bL_w[lID], #bV_w[vowelID] + 
    # Priors:
    # Intercept:
    #c(a_l,a_n,a_r,a_w) ~ dnorm(0,10),
    # liquid in stimulus:
    bL_l[lID] ~ dnorm(0,10),
    bL_n[lID] ~ dnorm(0,10),
    bL_r[lID] ~ dnorm(0,10),
    bL_w[lID] ~ dnorm(0,10)
    # vowel
#     bV_l[vowelID] ~ dnorm(0,10),
#     bV_n[vowelID] ~ dnorm(0,10),
#     bV_r[vowelID] ~ dnorm(0,10),
#     bV_w[vowelID] ~ dnorm(0,10),
#     # consonant
#     bC_l[consID] ~ dnorm(0,10),
#     bC_n[consID] ~ dnorm(0,10),
#     bC_r[consID] ~ dnorm(0,10),
#     bC_w[consID] ~ dnorm(0,10)
  ),
  data = counts,iter=5000,cores=4,chains=4, control=list(max_treedepth=15,adapt_delta=0.85)
)

precis(modelL,depth=2)
plot(precis(modelL,depth=2))

newDat <- data.frame(lID = c(1,2,3,4))
p <- link(modelL,newDat)
lapply(lapply(p,apply,2,mean),prop.table)


counts <- liquidsAll %>% 
  group_by(VOWEL,cons) %>%
  summarize(l=sum(CHOICE=='l'),
            n=sum(CHOICE=='n'),
            r=sum(CHOICE=='r'),
            w=sum(CHOICE=='n'),total=sum(l,n,r,w)) %>%
  as.data.frame