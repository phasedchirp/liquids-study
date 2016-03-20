# Check for required packages, if not, run setup:
is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
# function found in this thread: http://r.789695.n4.nabble.com/test-if-a-package-is-installed-td1750671.html
if(!is.installed("rethinking")){
  deps <- c("coda","mvtnorm","devtools")
  if(!all(is.installed(deps))){
    install.packages(deps[-is.installed(deps)])
    library(devtools)
    devtools::install_github("rmcelreath/rethinking")
  }
}

library(dplyr)


cols <- c("SUBJECT","NUMBER","BL","ANSWER","RESP","CHOICE","C1C2","LEXICAL","LIQUID","POSITION","VOWEL","CHOICE1","CHOICE2","CHOICE3","CHOICE4","NAME","TARGET")

CNT_sg_err <- read.csv("CNT_ERR_SG.csv")[,cols]
CNT_sg_cor <- read.csv("CNT_COR_SG.csv")[,cols]
CNT_sg_err$CHOICE <- CNT_sg_err$CHOICE %>% as.character %>% tolower %>% as.factor
CNT_sg_cor$CHOICE <- CNT_sg_cor$CHOICE %>% as.character %>% tolower %>% as.factor
CNT <- rbind(CNT_sg_err,CNT_sg_cor)
CNT$lang <- "Cantonese"

MAN_sg_err <- read.csv("MAN_ERR_SG.csv")[,cols]
MAN_sg_cor <- read.csv("MAN_COR_SG.csv")[,cols]
MAN_sg_err$CHOICE <- MAN_sg_err$CHOICE %>% as.character %>% tolower %>% as.factor
MAN_sg_cor$CHOICE <- MAN_sg_cor$CHOICE %>% as.character %>% tolower %>% as.factor
MAN <- rbind(MAN_sg_err,MAN_sg_cor)
MAN$lang <- "Mandarin"


liquidsAll <- rbind(CNT,MAN)
liquidsAll$CHOICE[liquidsAll$CHOICE=="miss"] <- NA

liquidsAll <- liquidsAll %>%
  filter(TARGET%in%c("control","target")) %>%
  mutate(cons = ifelse(POSITION=="initial",substr(C1C2,3,3),tolower(substr(C1C2,1,1)))) %>%
  droplevels %>% na.omit

liquidsAll$cons <- as.factor(liquidsAll$cons)
liquidsAll$lang <- as.factor(liquidsAll$lang)



liquidsL <- liquidsAll %>% filter(LIQUID=="L") %>% droplevels
liquidsR <- liquidsAll %>% filter(LIQUID=="R") %>% droplevels


counts.L <- liquidsAll %>%
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

counts.subj.L <- liquidsAll %>%
  filter(LIQUID == "L") %>%
  mutate(init = ifelse(POSITION=="initial",1,0),
         med = ifelse(POSITION=="medial",1,0),
         lID = as.numeric(LIQUID),
         Man = ifelse(lang=="Mandarin",1,0),
         pID = as.numeric(SUBJECT),
         vI = ifelse(VOWEL=="i",1,0),
         vU = ifelse(VOWEL=="u",1,0)) %>%
  group_by(Man,init,med,pID) %>%
  summarize(l=sum(CHOICE=='l'),
            n=sum(CHOICE=='n'),
            r=sum(CHOICE=='r'),
            w=sum(CHOICE=='w'),total=sum(l,n,r,w)) %>%
  as.data.frame

counts.R <- liquidsAll %>%
  filter(LIQUID == "R") %>%
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

counts.subj.R <- liquidsAll %>%
  filter(LIQUID == "R") %>%
  mutate(init = ifelse(POSITION=="initial",1,0),
         med = ifelse(POSITION=="medial",1,0),
         lID = as.numeric(LIQUID),
         Man = ifelse(lang=="Mandarin",1,0),
         pID = as.numeric(SUBJECT),
         vI = ifelse(VOWEL=="i",1,0),
         vU = ifelse(VOWEL=="u",1,0)) %>%
  group_by(Man,init,med,pID) %>%
  summarize(l=sum(CHOICE=='l'),
            n=sum(CHOICE=='n'),
            r=sum(CHOICE=='r'),
            w=sum(CHOICE=='w'),total=sum(l,n,r,w)) %>%
  as.data.frame

counts.W <- liquidsAll %>%
  filter(LIQUID == "W") %>%
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

counts.subj.W <- liquidsAll %>%
  filter(LIQUID == "W") %>%
  mutate(init = ifelse(POSITION=="initial",1,0),
         med = ifelse(POSITION=="medial",1,0),
         lID = as.numeric(LIQUID),
         Man = ifelse(lang=="Mandarin",1,0),
         pID = as.numeric(SUBJECT),
         vI = ifelse(VOWEL=="i",1,0),
         vU = ifelse(VOWEL=="u",1,0)) %>%
  group_by(Man,init,med,pID) %>%
  summarize(l=sum(CHOICE=='l'),
            n=sum(CHOICE=='n'),
            r=sum(CHOICE=='r'),
            w=sum(CHOICE=='w'),total=sum(l,n,r,w)) %>%
  as.data.frame

counts.N <- liquidsAll %>%
  filter(LIQUID == "N") %>%
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

counts.subj.N <- liquidsAll %>%
  filter(LIQUID == "N") %>%
  mutate(init = ifelse(POSITION=="initial",1,0),
         med = ifelse(POSITION=="medial",1,0),
         lID = as.numeric(LIQUID),
         Man = ifelse(lang=="Mandarin",1,0),
         pID = as.numeric(SUBJECT),
         vI = ifelse(VOWEL=="i",1,0),
         vU = ifelse(VOWEL=="u",1,0)) %>%
  group_by(Man,init,med,pID) %>%
  summarize(l=sum(CHOICE=='l'),
            n=sum(CHOICE=='n'),
            r=sum(CHOICE=='r'),
            w=sum(CHOICE=='w'),total=sum(l,n,r,w)) %>%
  as.data.frame


# stringDiff <- function(x,y){
#   xs <- strsplit(x,'')[[1]]
#   ys <- strsplit(y,'')[[1]]
#   diff <- setdiff(ys,xs)
#   ifelse(x==y,"none",diff)
# }