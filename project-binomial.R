source("processing.R")
library(tidyr)
library(rethinking)

liquidsL <- liquidsL %>% mutate(init = ifelse(POSITION=="initial",1,0),
                              med = ifelse(POSITION=="medial",1,0),
                              consID = as.numeric(cons),
                              Man = ifelse(lang=="Mandarin",1,0),
                              pID = as.numeric(SUBJECT),
                              vI = ifelse(VOWEL=="i",1,0),
                              vU = ifelse(VOWEL=="u",1,0)) %>%
  mutate(correct=ifelse(CHOICE=='l',1,0)) %>%
  as.data.frame

countsL <- liquidsL %>% 
  group_by(lang,POSITION,VOWEL,Man,init,med,vI,vU) %>% 
  summarize(correct=sum(CHOICE=='l'),
            error=sum(CHOICE!='l'),
            total = sum(correct,error),
            prop = correct/total) %>% as.data.frame %>%
  select(Man,init,med,vI,vU,correct,error,total)

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
  data = countsL,iter=5000,cores=4,chains=4, control=list(max_treedepth=15,adapt_delta=0.95)
)

precis(modelL.4)


plotFrame <- liquidsL %>% mutate(init = ifelse(POSITION=="initial",1,0),
                                 med = ifelse(POSITION=="medial",1,0),
                                 consID = as.numeric(cons),
                                 Man = ifelse(lang=="Mandarin",1,0),
                                 pID = as.numeric(SUBJECT),
                                 vI = ifelse(VOWEL=="i",1,0),
                                 vU = ifelse(VOWEL=="u",1,0)) %>%
  group_by(lang,POSITION,VOWEL,Man,init,med,vI,vU) %>% 
  summarize(correct=sum(CHOICE=='l'),
            error=sum(CHOICE!='l'),
            total = sum(correct,error),
            prop = correct/total) %>%
  as.data.frame

#newDat <- plotFrame[,c("Man","init","med")]

newDat <- plotFrame[,c("Man","init","med","vI","vU")]

p <- link(modelL.4,data=newDat,n=5000)["theta"]
mu <- apply(p$theta,2,mean)
intervals <- apply(p$theta,2,HPDI)



plotFrame$est <- mu
plotFrame$upper <- intervals[2,]
plotFrame$lower <- intervals[1,]

ggplot(data=plotFrame,aes(y=prop,x=POSITION,color=VOWEL,linetype=lang))+
  geom_point(position=position_dodge(width=0.5),size=4,alpha=0.75)+ #observed
  geom_point(aes(y=est),position=position_dodge(width=0.5),size=2)+
  geom_errorbar(aes(ymin=upper,ymax=lower),position=position_dodge(width=0.5),size=1,width=0)+
  theme_bw()+ggtitle("Response patterns for initial liquids")



modelL.5 <- map2stan(
  alist(
    correct ~ dbinom(total,theta) ,
    
    # Linear models:
    logit(theta) <-  a + bL*Man + pos + vowel + vPos,
    # submodels for position, vowel, interactions
    pos <- bInit*init + iInit*init*Man + bMed*med +iMed*med*Man,
    vowel <- bI*vI + iI*vI*Man + bU*vU +iU*vU*Man ,
    vPos <- iIinit*vI*init + iUinit*vU*init + iImed*vI*med + iUmed*vU*med,
    # Priors:
    # Intercept:
    a ~ dnorm(0,50),
    # language Effect
    bL ~ dnorm(0,5),
    # position effects:
    bInit ~ dnorm(0,5),
    bMed ~ dnorm(0,5),
    # position:language interaction:
    iInit ~ dnorm(0,5),
    iMed ~ dnorm(0,5),
    # vowel main effects:
    bI ~ dnorm(0,5),
    bU ~ dnorm(0,5),
    # vowel:language interaction:
    iI ~ dnorm(0,5),
    iU ~ dnorm(0,5),
    # position:vowel interaction:
    iIinit ~ dnorm(0,5),
    iUinit ~ dnorm(0,5),
    iImed ~ dnorm(0,5),
    iUmed ~ dnorm(0,5)
  ),
  data = countsL,iter=5000,cores=4,chains=4, control=list(max_treedepth=15,adapt_delta=0.9)
)

compare(modelL.5,modelL.4)


plotFrame <- liquidsL %>% mutate(init = ifelse(POSITION=="initial",1,0),
                                 med = ifelse(POSITION=="medial",1,0),
                                 consID = as.numeric(cons),
                                 Man = ifelse(lang=="Mandarin",1,0),
                                 pID = as.numeric(SUBJECT),
                                 vI = ifelse(VOWEL=="i",1,0),
                                 vU = ifelse(VOWEL=="u",1,0)) %>%
  group_by(lang,POSITION,VOWEL,Man,init,med,vI,vU) %>% 
  summarize(correct=as.integer(sum(CHOICE=='l')),
            error=sum(CHOICE!='l'),
            total=as.integer(sum(correct,error)),
            prop = correct/total) %>%
  as.data.frame

#newDat <- plotFrame[,c("Man","init","med")]

newDat <- plotFrame[,c("Man","init","med","vI","vU")]

p <- link(modelL.5,countsL,n=5000)["theta"]
mu <- apply(p$theta,2,mean)
intervals <- apply(p$theta,2,HPDI)



plotFrame$est <- mu
plotFrame$upper <- intervals[2,]
plotFrame$lower <- intervals[1,]

ggplot(data=plotFrame,aes(y=prop,x=POSITION,color=VOWEL,linetype=lang))+
  geom_hline(yintercept=0.5,color="red",alpha=0.5,linetype=2,size=1)+
  geom_point(position=position_dodge(width=0.5),size=4,alpha=0.75)+ #observed
  geom_point(aes(y=est),position=position_dodge(width=0.5),size=2)+
  geom_errorbar(aes(ymin=upper,ymax=lower),position=position_dodge(width=0.5),size=1,width=0)+
  scale_y_continuous(breaks = seq(from=0,to=1,by=0.1),limits=c(0.3,1))+
  labs(x="Position",y="Proportion correct")+
  theme_bw()+ggtitle("Proportion correct for /l/ by position, vowel, and language")

################################################################################
# Alternative model specifications and comparison using WAIC                   #
################################################################################
# 
# modelL.1 <- map2stan(
#   alist(
#     correct ~ dbinom(total,theta) ,
#     
#     # Linear models:
#     logit(theta) <-  a + bL*Man + pos,
#     # submodel for position
#     pos <- bInit*init + bMed*med,
#     
#     # Priors:
#     # Intercept:
#     a ~ dnorm(0,50),
#     # language Effect
#     bL ~ dnorm(0,10),
#     # position effects:
#     bInit ~ dnorm(0,10),
#     bMed ~ dnorm(0,10)
#   ),
#   data = countsL,iter=5000,cores=4,chains=4, control=list(max_treedepth=15,adapt_delta=0.9)
# )
# 
# modelL.2 <- map2stan(
#   alist(
#     correct ~ dbinom(total,theta) ,
#     
#     # Linear models:
#     logit(theta) <-  a + bL*Man + pos,
#     # submodel for position
#     pos <- bInit*init + iInit*init*Man + bMed*med +iMed*med*Man,
#     
#     # Priors:
#     # Intercept:
#     a ~ dnorm(0,50),
#     # language Effect
#     bL ~ dnorm(0,10),
#     # position effects:
#     bInit ~ dnorm(0,10),
#     bMed ~ dnorm(0,10),
#     # position:language interaction:
#     iInit ~ dnorm(0,10),
#     iMed ~ dnorm(0,10)
#   ),
#   data = countsL,iter=5000,cores=4,chains=4, control=list(max_treedepth=15,adapt_delta=0.9)
# )
# 
# 
# modelL.3 <- map2stan(
#   alist(
#     correct ~ dbinom(total,theta) ,
#     
#     # Linear models:
#     logit(theta) <-  a + bL*Man + pos + vowel,
#     # submodels for position, vowel
#     pos <- bInit*init + iInit*init*Man + bMed*med +iMed*med*Man,
#     vowel <- bI*vI + bU*vU ,
#     # Priors:
#     # Intercept:
#     a ~ dnorm(0,50),
#     # language Effect
#     bL ~ dnorm(0,10),
#     # position effects:
#     bInit ~ dnorm(0,10),
#     bMed ~ dnorm(0,10),
#     # position:language interaction:
#     iInit ~ dnorm(0,10),
#     iMed ~ dnorm(0,10),
#     # vowel main effects:
#     bI ~ dnorm(0,10),
#     bU ~ dnorm(0,10)
#   ),
#   data = countsL,iter=5000,cores=4,chains=4, control=list(max_treedepth=15,adapt_delta=0.9)
# )
# 
# compare(modelL.1,modelL.2,modelL.3,modelL.4)



