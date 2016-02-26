source("processing.R")
library(rethinking)
#rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())


#round(prop.table(xtabs(~CHOICE+VOWEL,liquidsAll)),2)

#round(xtabs(~CHOICE+VOWEL+lang,liquidsAll),2)


counts <- liquidsAll %>% mutate(init = ifelse(POSITION=="initial",1,0),
                                med = ifelse(POSITION=="medial",1,0),
                                consID = as.numeric(cons),
                                lID = as.numeric(LIQUID),
                                Man = ifelse(lang=="Mandarin",1,0)) %>%
  group_by(lID,Man,init,med) %>%
  summarize(l=sum(CHOICE=='l'),
            n=sum(CHOICE=='n'),
            r=sum(CHOICE=='r'),
            w=sum(CHOICE=='w'),total=sum(l,n,r,w)) %>%
  as.data.frame

modelL <- map2stan(
  alist(
    l ~ dpois(lambda_l) ,
    n ~ dpois(lambda_n) ,
    r ~ dpois(lambda_r) ,
    w ~ dpois(lambda_w) ,
    # Linear models:
    log(lambda_l) <-  liq_l + pos_l, # + bMan_l*Man
    log(lambda_n) <-  liq_n + pos_n, # + bMan_n*Man
    log(lambda_r) <-  liq_r + pos_r, # + bMan_r*Man
    log(lambda_w) <-  liq_w + pos_w, # + bMan_w*Man
    # Construct liquid:
    liq_l <- bL_l[lID] + iL_l[lID]*Man,
    liq_n <- bL_n[lID] + iL_n[lID]*Man,
    liq_r <- bL_r[lID] + iL_r[lID]*Man,
    liq_w <- bL_w[lID] + iL_w[lID]*Man,
    # Construct position:
    pos_l <- bInit_l*init + bMed_l*med + iInit_l*Man*init + iMed_l*Man*med,
    pos_n <- bInit_n*init + bMed_n*med + iInit_n*Man*init + iMed_n*Man*med,
    pos_r <- bInit_r*init + bMed_r*med + iInit_r*Man*init + iMed_r*Man*med,
    pos_w <- bInit_w*init + bMed_w*med + iInit_w*Man*init + iMed_w*Man*med,
    # Priors:
    # Intercept:
    #c(a_l,a_n,a_r,a_w) ~ dnorm(0,10),
    # liquid in stimulus:
    bL_l[lID] ~ dnorm(0,10),
    bL_n[lID] ~ dnorm(0,10),
    bL_r[lID] ~ dnorm(0,10),
    bL_w[lID] ~ dnorm(0,10),
    # main effects for language:
    bMan_l ~ dnorm(0,10),
    bMan_n ~ dnorm(0,10),
    bMan_r ~ dnorm(0,10),
    bMan_w ~ dnorm(0,10),
    # initial:
    bInit_l ~ dnorm(0,10),
    bInit_n ~ dnorm(0,10),
    bInit_r ~ dnorm(0,10),
    bInit_w ~ dnorm(0,10),
    # medial:
    bMed_l ~ dnorm(0,10),
    bMed_n ~ dnorm(0,10),
    bMed_r ~ dnorm(0,10),
    bMed_w ~ dnorm(0,10),
    # language:position interactions:
    iInit_l ~ dnorm(0,10),
    iInit_n ~ dnorm(0,10),
    iInit_r ~ dnorm(0,10),
    iInit_w ~ dnorm(0,10),
    iMed_l ~ dnorm(0,10),
    iMed_n ~ dnorm(0,10),
    iMed_r ~ dnorm(0,10),
    iMed_w ~ dnorm(0,10),
    # language:liquid interaction:
    iL_l[lID] ~ dnorm(0,10),
    iL_n[lID] ~ dnorm(0,10),
    iL_r[lID] ~ dnorm(0,10),
    iL_w[lID] ~ dnorm(0,10)
    # vowel
#     bV_l[vowelID] ~ dnorm(0,10),
#     bV_n[vowelID] ~ dnorm(0,10),
#     bV_r[vowelID] ~ dnorm(0,10),
#     bV_w[vowelID] ~ dnorm(0,10)
  ),
  data = counts,iter=5000,cores=4,chains=4, control=list(max_treedepth=15,adapt_delta=0.9)
)

precis(modelL,depth=2)
plot(precis(modelL,depth=2))

newDat <- expand.grid(lID = c(1,2,3,4),Man=c(0,1),pos=c(0,1)) %>% arrange(lID,Man)
p <- link(modelL,newDat)
mu <- lapply(p,apply,2,mean)
intervals <- lapply(p,apply,2,HPDI)

plotFrame <- with(liquidsAll,expand.grid(liquid=levels(LIQUID),
                                         lang = levels(lang),
                                         position=levels())

