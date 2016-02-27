source("processing.R")
library(tidyr)
library(rethinking)

#rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())


#round(prop.table(xtabs(~CHOICE+VOWEL,liquidsAll)),2)

#round(xtabs(~CHOICE+VOWEL+lang,liquidsAll),2)


counts <- liquidsAll %>% mutate(init = ifelse(POSITION=="initial",1,0),
                                med = ifelse(POSITION=="medial",1,0),
                                consID = as.numeric(cons),
                                lID = as.numeric(LIQUID),
                                Man = ifelse(lang=="Mandarin",1,0),
                                pID = as.numeric(SUBJECT),
                                vI = ifelse(VOWEL=="i",1,0),
                                vU = ifelse(VOWEL=="u",1,0)) %>%
  group_by(lID,Man,init,med,vI,vU) %>%
  summarize(l=sum(CHOICE=='l'),
            n=sum(CHOICE=='n'),
            r=sum(CHOICE=='r'),
            w=sum(CHOICE=='w'),total=sum(l,n,r,w)) %>%
  as.data.frame

modelL1 <- map2stan(
  alist(
    l ~ dpois(lambda_l) ,
    n ~ dpois(lambda_n) ,
    r ~ dpois(lambda_r) ,
    w ~ dpois(lambda_w) ,
    # Linear models:
    log(lambda_l) <-  liq_l + pos_l + v_l, #
    log(lambda_n) <-  liq_n + pos_n + v_n, #
    log(lambda_r) <-  liq_r + pos_r + v_r, #
    log(lambda_w) <-  liq_w + pos_w + v_w, #
    # Intercepts by liquid in stimulus (also main effect(s) of language):
    liq_l <- bL_l[lID] + iL_l[lID]*Man,
    liq_n <- bL_n[lID] + iL_n[lID]*Man,
    liq_r <- bL_r[lID] + iL_r[lID]*Man,
    liq_w <- bL_w[lID] + iL_w[lID]*Man,
    # Position effects and interactions with language (final is intercept):
    pos_l <- bInit_l*init + bMed_l*med + iInit_l*Man*init + iMed_l*Man*med,
    pos_n <- bInit_n*init + bMed_n*med + iInit_n*Man*init + iMed_n*Man*med,
    pos_r <- bInit_r*init + bMed_r*med + iInit_r*Man*init + iMed_r*Man*med,
    pos_w <- bInit_w*init + bMed_w*med + iInit_w*Man*init + iMed_w*Man*med,
    # Vowel effects and interactions with language:
    v_l <- bI_l*vI + bU_l*vU + iI_l*vI*Man + iU_l*vU*Man,
    v_n <- bI_n*vI + bU_n*vU + iI_n*vI*Man + iU_n*vU*Man,
    v_r <- bI_r*vI + bU_r*vU + iI_r*vI*Man + iU_r*vU*Man,
    v_w <- bI_w*vI + bU_w*vU + iI_w*vI*Man + iU_w*vU*Man,
    # Priors:
    # Intercepts by liquid:
    bL_l[lID] ~ dnorm(0,50),
    bL_n[lID] ~ dnorm(0,50),
    bL_r[lID] ~ dnorm(0,50),
    bL_w[lID] ~ dnorm(0,50),
    # language main effects by liquid:
    iL_l[lID] ~ dnorm(0,20),
    iL_n[lID] ~ dnorm(0,20),
    iL_r[lID] ~ dnorm(0,20),
    iL_w[lID] ~ dnorm(0,20),
    # Position:
    c(bInit_l, bInit_n, bInit_r, bInit_w) ~ dnorm(0,10),
    c(bMed_l,bMed_n,bMed_r,bMed_w) ~ dnorm(0,10),
    
    # language:position interactions:
    c(iInit_l, iInit_n, iInit_r, iInit_w) ~ dnorm(0,10),
    c(iMed_l,iMed_n,iMed_r,iMed_w) ~ dnorm(0,10),

    # vowel
    c(bI_l, bI_n, bI_r, bI_w) ~ dnorm(0,10),
    c(bU_l, bU_n, bU_r, bU_w) ~ dnorm(0,10),
    
    # language:vowel interactions:
    c(iI_l, iI_n, iI_r, iI_w) ~ dnorm(0,10),
    c(iU_l, iU_n, iU_r, iU_w) ~ dnorm(0,10)
    
    # position:vowel interactions:
  ),
  data = counts,iter=5000,cores=4,chains=4, control=list(max_treedepth=15,adapt_delta=0.9)
)

precis(modelL,depth=2)
plot(precis(modelL,depth=2))

newDat <- counts[,c("lID","Man","init","med","vI","vU")] %>% distinct(lID,Man,init,med,vI,vU) %>% arrange(lID,Man,med,vI)
p <- link(modelL1,newDat)[c("lambda_l","lambda_n","lambda_r","lambda_w")]

# See McElreath (2016) p.327
pSums <- with(p,lambda_l+lambda_n+lambda_r+lambda_w)
props <- lapply(p,function(x){x/pSums})
mu <- lapply(props,apply,2,median)
intervals <- lapply(props,apply,2,function(x){HPDI(x,prob=0.99)})

plotFrame <- liquidsAll %>% mutate(init = ifelse(POSITION=="initial",1,0),
                                    med = ifelse(POSITION=="medial",1,0),
                                    consID = as.numeric(cons),
                                    lID = as.numeric(LIQUID),
                                    Man = ifelse(lang=="Mandarin",1,0)) %>%
  group_by(LIQUID,lang,POSITION,VOWEL) %>%
  summarize(l=sum(CHOICE=='l'),
            n=sum(CHOICE=='n'),
            r=sum(CHOICE=='r'),
            w=sum(CHOICE=='w'),total=n()) %>%
  gather(choice,count,l:w) %>%
  mutate(obs = count/total)%>%
  as.data.frame

plotFrame$est <- c(mu$lambda_l, mu$lambda_n, mu$lambda_r, mu$lambda_w)
plotFrame$upper <- with(intervals,c(lambda_l[2,], lambda_n[2,], lambda_r[2,], lambda_w[2,]))
plotFrame$lower <- with(intervals,c(lambda_l[1,], lambda_n[1,], lambda_r[1,], lambda_w[1,]))

initial <- plotFrame %>% subset(POSITION=="initial")
ggplot(data=initial,aes(y=obs,x=VOWEL,col=choice))+
  geom_point(position=position_dodge(width=0.5),size=4,alpha=0.75)+ #observed
  geom_point(aes(y=est),position=position_dodge(width=0.5),size=2)+
  geom_errorbar(aes(ymin=upper,ymax=lower),position=position_dodge(width=0.5),size=1,width=0)+
  theme_bw()+facet_grid(LIQUID~lang)+ggtitle("Response patterns for initial liquids")

medial <- plotFrame %>% subset(POSITION=="medial")
ggplot(data=medial,aes(y=obs,x=VOWEL,col=choice))+
  geom_point(position=position_dodge(width=0.5),size=4,alpha=0.75)+ #observed
  geom_point(aes(y=est),position=position_dodge(width=0.5),size=2)+
  geom_errorbar(aes(ymin=upper,ymax=lower),position=position_dodge(width=0.5),size=1,width=0)+
  theme_bw()+facet_grid(LIQUID~lang)+ggtitle("Response patterns for medial liquids")

final <- plotFrame %>% subset(POSITION=="final")
ggplot(data=final,aes(y=obs,x=VOWEL,col=choice))+
  geom_point(position=position_dodge(width=0.5),size=4,alpha=0.75)+ #observed
  geom_point(aes(y=est),position=position_dodge(width=0.5),size=2)+
  geom_errorbar(aes(ymin=upper,ymax=lower),position=position_dodge(width=0.5),size=1,width=0)+
  theme_bw()+facet_grid(LIQUID~lang)+ggtitle("Response patterns for final liquids")




modelL2 <- map2stan(
  alist(
    l ~ dpois(lambda_l) ,
    n ~ dpois(lambda_n) ,
    r ~ dpois(lambda_r) ,
    w ~ dpois(lambda_w) ,
    # Linear models:
    log(lambda_l) <-  liq_l + pos_l, #
    log(lambda_n) <-  liq_n + pos_n, #
    log(lambda_r) <-  liq_r + pos_r, #
    log(lambda_w) <-  liq_w + pos_w, #
    # Intercepts by liquid in stimulus (also main effect(s) of language):
    liq_l <- bL_l[lID] + iL_l[lID]*Man,
    liq_n <- bL_n[lID] + iL_n[lID]*Man,
    liq_r <- bL_r[lID] + iL_r[lID]*Man,
    liq_w <- bL_w[lID] + iL_w[lID]*Man,
    # Position effects and interactions with language (final is intercept):
    pos_l <- bInit_l*init + bMed_l*med + iInit_l*Man*init + iMed_l*Man*med,
    pos_n <- bInit_n*init + bMed_n*med + iInit_n*Man*init + iMed_n*Man*med,
    pos_r <- bInit_r*init + bMed_r*med + iInit_r*Man*init + iMed_r*Man*med,
    pos_w <- bInit_w*init + bMed_w*med + iInit_w*Man*init + iMed_w*Man*med,
    # Priors:
    # Intercepts by liquid:
    bL_l[lID] ~ dnorm(0,10),
    bL_n[lID] ~ dnorm(0,10),
    bL_r[lID] ~ dnorm(0,10),
    bL_w[lID] ~ dnorm(0,10),
    # language main effects by liquid:
    iL_l[lID] ~ dnorm(0,10),
    iL_n[lID] ~ dnorm(0,10),
    iL_r[lID] ~ dnorm(0,10),
    iL_w[lID] ~ dnorm(0,10),
    # Position:
    c(bInit_l, bInit_n, bInit_r, bInit_w) ~ dnorm(0,10),
    c(bMed_l,bMed_n,bMed_r,bMed_w) ~ dnorm(0,10),
    
    # language:position interactions:
    c(iInit_l, iInit_n, iInit_r, iInit_w) ~ dnorm(0,10),
    c(iMed_l,iMed_n,iMed_r,iMed_w) ~ dnorm(0,10)
  ),
  data = counts,iter=5000,cores=4,chains=4, control=list(max_treedepth=15,adapt_delta=0.9)
)

newDat <- counts[,c("lID","Man","init","med")] %>% distinct(lID,Man,init,med) %>% arrange(lID,Man,med)
p <- link(modelL2,newDat)[c("lambda_l","lambda_n","lambda_r","lambda_w")]

# See McElreath (2016) p.327
pSums <- with(p,lambda_l+lambda_n+lambda_r+lambda_w)
props <- lapply(p,function(x){x/pSums})
mu <- lapply(props,apply,2,median)
intervals <- lapply(props,apply,2,function(x){HPDI(x,prob=0.99)})


mu <- lapply(p,apply,2,median)
pSums <- with(mu,lambda_l+lambda_n+lambda_r+lambda_w)
mu <- lapply(mu,function(x){x/pSums})
intervals <- lapply(p,apply,2,function(x){HPDI(x,prob=0.99)})
pSums <- with(intervals,lambda_l+lambda_n+lambda_r+lambda_w)
intervals <- lapply(intervals,function(x){x/pSums})

plotFrame <- liquidsAll %>% mutate(init = ifelse(POSITION=="initial",1,0),
                                   med = ifelse(POSITION=="medial",1,0),
                                   consID = as.numeric(cons),
                                   lID = as.numeric(LIQUID),
                                   Man = ifelse(lang=="Mandarin",1,0)) %>%
  group_by(LIQUID,lang,POSITION) %>%
  summarize(l=sum(CHOICE=='l'),
            n=sum(CHOICE=='n'),
            r=sum(CHOICE=='r'),
            w=sum(CHOICE=='w'),total=n()) %>%
  gather(choice,count,l:w) %>%
  mutate(obs = count/total)%>%
  as.data.frame

plotFrame$est <- c(mu$lambda_l, mu$lambda_n, mu$lambda_r, mu$lambda_w)
plotFrame$upper <- with(intervals,c(lambda_l[2,], lambda_n[2,], lambda_r[2,], lambda_w[2,]))
plotFrame$lower <- with(intervals,c(lambda_l[1,], lambda_n[1,], lambda_r[1,], lambda_w[1,]))

ggplot(data=plotFrame,aes(y=obs,x=lang,col=choice))+
  geom_point(position=position_dodge(width=0.5),size=4,alpha=0.75)+ #observed
  geom_point(aes(y=est),position=position_dodge(width=0.5),size=2)+
  geom_errorbar(aes(ymin=upper,ymax=lower),position=position_dodge(width=0.5),size=1,width=0)+
  theme_bw()+facet_grid(LIQUID~POSITION)