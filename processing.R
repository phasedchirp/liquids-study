# Check for required packages, if not, run setup:
is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
# function found in this thread: http://r.789695.n4.nabble.com/test-if-a-package-is-installed-td1750671.html
if(!is.installed("rethinking")){
  deps <- c("coda","mvtnorm","devtools")
  if(!all(is.installed(deps))){
    install.packages(deps[-is.installed(deps)])
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
liquidsAll$lang <- as.factor(liquidsAll$lang)
liquidsAll$CHOICE[liquidsAll$CHOICE=="miss"] <- NA

liquidsAll <- liquidsAll %>% filter(TARGET%in%c("control","target")) %>% droplevels %>% na.omit

# liquidsL <- liquidsAll %>% filter(LIQUID=="L") %>% droplevels
# liquidsR <- liquidsAll %>% filter(LIQUID=="R") %>% droplevels


counts <- liquidsAll %>% group_by(LIQUID,C1C2,VOWEL,POSITION,LEXICAL,lang)%>%summarize(count=n()) %>% as.data.frame

counts1 <- liquidsAll %>% group_by(LIQUID,CHOICE) %>% summarize(count=n()) %>% as.data.frame

counts2 <- liquidsAll %>% group_by(LIQUID,CHOICE,C1C2) %>% summarize(count=n()) %>% as.data.frame

counts3 <- liquidsAll %>% group_by(LIQUID,CHOICE,VOWEL) %>% summarize(count=n()) %>% as.data.frame

counts4 <- liquidsAll %>% group_by(LIQUID,CHOICE,POSITION) %>% summarize(count=n()) %>% as.data.frame


# stringDiff <- function(x,y){
#   xs <- strsplit(x,'')[[1]]
#   ys <- strsplit(y,'')[[1]]
#   diff <- setdiff(ys,xs)
#   ifelse(x==y,"none",diff)
# }