source("processing.R")
library(tidyr)
library(rethinking)

countsL <- liquidsL %>% mutate(init = ifelse(POSITION=="initial",1,0),
                              med = ifelse(POSITION=="medial",1,0),
                              consID = as.numeric(cons),
                              Man = ifelse(lang=="Mandarin",1,0),
                              pID = as.numeric(SUBJECT),
                              vI = ifelse(VOWEL=="i",1,0),
                              vU = ifelse(VOWEL=="u",1,0)) %>%
  group_by(Man,init,med,vI,vU) %>%
  summarize(correct=as.integer(sum(CHOICE=='l')),
            error=sum(CHOICE!='l'),
            total=as.integer(sum(correct,error)),
            prop = correct/total) %>%
  as.data.frame



modelL.1 <- map2stan(
  alist(
    correct ~ dbinom(total,theta) ,
    
    # Linear models:
    logit(theta) <-  a + bL*Man + pos,
    # submodel for position
    pos <- bInit*init + bMed*med,
    
    # Priors:
    # Intercept:
    a ~ dnorm(0,50),
    # language Effect
    bL ~ dnorm(0,10),
    # position effects:
    bInit ~ dnorm(0,10),
    bMed ~ dnorm(0,10)
  ),
  data = countsL,iter=5000,cores=4,chains=4, control=list(max_treedepth=15,adapt_delta=0.9)
)

modelL.2 <- map2stan(
  alist(
    correct ~ dbinom(total,theta) ,
    
    # Linear models:
    logit(theta) <-  a + bL*Man + pos,
    # submodel for position
    pos <- bInit*init + iInit*init*Man + bMed*med +iMed*med*Man,
    
    # Priors:
    # Intercept:
    a ~ dnorm(0,50),
    # language Effect
    bL ~ dnorm(0,10),
    # position effects:
    bInit ~ dnorm(0,10),
    bMed ~ dnorm(0,10),
    # position:language interaction:
    iInit ~ dnorm(0,10),
    iMed ~ dnorm(0,10)
  ),
  data = countsL,iter=5000,cores=4,chains=4, control=list(max_treedepth=15,adapt_delta=0.9)
)


modelL.3 <- map2stan(
  alist(
    correct ~ dbinom(total,theta) ,
    
    # Linear models:
    logit(theta) <-  a + bL*Man + pos + vowel,
    # submodels for position, vowel
    pos <- bInit*init + iInit*init*Man + bMed*med +iMed*med*Man,
    vowel <- bI*vI + bU*vU ,
    # Priors:
    # Intercept:
    a ~ dnorm(0,50),
    # language Effect
    bL ~ dnorm(0,10),
    # position effects:
    bInit ~ dnorm(0,10),
    bMed ~ dnorm(0,10),
    # position:language interaction:
    iInit ~ dnorm(0,10),
    iMed ~ dnorm(0,10),
    # vowel main effects:
    bI ~ dnorm(0,10),
    bU ~ dnorm(0,10)
  ),
  data = countsL,iter=5000,cores=4,chains=4, control=list(max_treedepth=15,adapt_delta=0.9)
)


modelL.4 <- map2stan(
  alist(
    correct ~ dbinom(total,theta) ,
    
    # Linear models:
    logit(theta) <-  a + bL*Man + pos + vowel,
    # submodels for position, vowel
    pos <- bInit*init + iInit*init*Man + bMed*med +iMed*med*Man,
    vowel <- bI*vI + iI*vI*Man + bU*vU +iU*vU*Man ,
    # Priors:
    # Intercept:
    a ~ dnorm(0,50),
    # language Effect
    bL ~ dnorm(0,10),
    # position effects:
    bInit ~ dnorm(0,10),
    bMed ~ dnorm(0,10),
    # position:language interaction:
    iInit ~ dnorm(0,10),
    iMed ~ dnorm(0,10),
    # vowel main effects:
    bI ~ dnorm(0,10),
    bU ~ dnorm(0,10),
    # vowel:language interaction:
    iI ~ dnorm(0,10),
    iU ~ dnorm(0,10)
  ),
  data = countsL,iter=5000,cores=4,chains=4, control=list(max_treedepth=15,adapt_delta=0.9)
)


compare(modelL.1,modelL.2,modelL.3,modelL.4)



# precis(modelL1)
# 
# # newDat <- countsL[,c("Man","init","med","vI","vU")] %>% distinct(lID,Man,init,med,vI,vU) %>% arrange(lID,Man,med,vI)
# newDat <- countsL[,c("Man","init","med")] %>% arrange(Man,init,med)
# p <- link(modelL1,newDat,n=5000)["theta"]
# mu <- apply(p$theta,2,mean)
# intervals <- apply(p$theta,2,HPDI)