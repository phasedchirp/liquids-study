source("processing.R")
library(tidyr)
library(rethinking)

#rstan_options(auto_write = TRUE)
#options(mc.cores = parallel::detectCores())


#round(prop.table(xtabs(~CHOICE+VOWEL,liquidsAll)),2)

#round(xtabs(~CHOICE+VOWEL+lang,liquidsAll),2)


counts <- liquidsAll %>%
  filter(LIQUID == "L") %>%
  mutate(init = ifelse(POSITION=="initial",1,0),
         med = ifelse(POSITION=="medial",1,0),
         lID = as.numeric(LIQUID),
         Man = ifelse(lang=="Mandarin",1,0),
         pID = as.numeric(SUBJECT),
         vI = ifelse(VOWEL=="i",1,0),
         vU = ifelse(VOWEL=="u",1,0)) %>%
  group_by(Man,init,med) %>%
  summarize(l=sum(CHOICE=='l'),
            n=sum(CHOICE=='n'),
            r=sum(CHOICE=='r'),
            w=sum(CHOICE=='w'),total=sum(l,n,r,w)) %>%
  as.data.frame

model.1 <- map2stan(
  alist(
    l ~ dpois(lambda_l) ,
    n ~ dpois(lambda_n) ,
    r ~ dpois(lambda_r) ,
    w ~ dpois(lambda_w) ,
    # Linear models:
    log(lambda_l) <-  a_l + pos_l + bMan_l*Man, #+ v_l, #
    log(lambda_n) <-  a_n + pos_n + bMan_n*Man , #+ v_n, #
    log(lambda_r) <-  a_r + pos_r + bMan_r*Man , #+ v_r, #
    log(lambda_w) <-  a_w + pos_w + bMan_w*Man , #+ v_w, #
    # Position:
    pos_l <- bInit_l*init + bMed_l*med,
    pos_n <- bInit_n*init + bMed_n*med,
    pos_r <- bInit_r*init + bMed_r*med,
    pos_w <- bInit_w*init + bMed_w*med,
    # Intercepts:
    c(a_l,a_n,a_r,a_w) ~ dnorm(0,50),
    # Position effects:
    c(bInit_l, bInit_n, bInit_r, bInit_w) ~ dnorm(0,5),
    c(bMed_l, bMed_n, bMed_r, bMed_w) ~ dnorm(0,5),
    # Language effects:
    c(bMan_l, bMan_n, bMan_r, bMan_w) ~ dnorm(0,5)
    
  ),
  data = counts,iter=5000,cores=4,chains=4, control=list(max_treedepth=15,adapt_delta=0.9)
)


precis(model.1,depth=2)
plotFrame <- liquidsAll %>%
  filter(LIQUID=="L") %>%
  mutate(init = ifelse(POSITION=="initial",1,0),
         med = ifelse(POSITION=="medial",1,0),
         consID = as.numeric(cons),
         lID = as.numeric(LIQUID),
         Man = ifelse(lang=="Mandarin",1,0)) %>%
  group_by(lang,POSITION,Man,init,med) %>%#,POSITION,VOWEL) %>%
  summarize(l=sum(CHOICE=='l'),
            n=sum(CHOICE=='n'),
            r=sum(CHOICE=='r'),
            w=sum(CHOICE=='w'),total=n()) %>%
  as.data.frame


newDat <- plotFrame[,c("Man","init","med")]
p <- link(modelL,newDat,n=5000)[c("lambda_l","lambda_n","lambda_r","lambda_w")]

# See McElreath (2016) p.327
pSums <- with(p,lambda_l+lambda_n+lambda_r+lambda_w)
props <- lapply(p,function(x){x/pSums})
mu <- lapply(props,apply,2,median)
intervals <- lapply(props,apply,2,function(x){HPDI(x,prob=0.99)})

plotFrame <- plotFrame %>%
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
  theme_bw()+facet_grid(.~POSITION)