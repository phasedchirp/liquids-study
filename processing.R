# Check for required packages, if not, run setup:
is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
if(!is.installed(rethinking)){
  deps <- c("coda","mvtnorm","devtools")
  if(!all(is.installed(deps))){
    install.packages(deps[-is.installed(deps)])
    devtools::install_github("rmcelreath/rethinking")
  }
}

library(dplyr)


cols_err <- c("SUBJECT","ANSWER","RESP","C1C2","CL_ONS","LEXICAL","LIQUID","POSITION","Running","TARGET","TYPE","VOWEL","CHOICE1","CHOICE2","CHOICE3","CHOICE4","CHOICE5")
cols_cor <- c("SUBJECT","ANSWER","RESP","C1C2","CL_ONS","LEXICAL","LIQUID","POSITION","Running","TARGET","TYPE","VOWEL","CHOICE1","CHOICE2","CHOICE3","CHOICE4","CHOICE5")

CNT_sg_err <- read.csv("CNT_ERR_SG.csv")[,cols_err]
CNT_sg_cor <- read.csv("CNT_ERR_SG.csv")[,cols_cor]
CNT_sg_cor$choice <- CNT_sg_cor$choice %>% as.character %>% tolower %>% as.factor
CNT <- rbind(CNT_sg_err,CNT_sg_cor)
CNT$lang <- "Cantonese"

CNT_sg_err <- read.csv("CNT_ERR_SG.csv")[,cols_err]
CNT_sg_cor <- read.csv("CNT_ERR_SG.csv")[,cols_cor]
CNT_sg_cor$choice <- CNT_sg_cor$choice %>% as.character %>% as.factor
CNT <- rbind(CNT_sg_err,CNT_sg_cor)
MAN$lang <- "Mandarin"

liquidsAll <- rbind(CNT,MAN)
liquidsAll$lang <- as.factor(liquidsAll$lang)

liquidsAll <- liquidsAll %>% filter(TARGET%in%c("control","target"))

liquidsL <- liquidsAll %>% filter(LIQUID=="L")
liquidsR <- liquidsAll %>% filter(LIQUID=="R")


# stringDiff <- function(x,y){
#   xs <- strsplit(x,'')[[1]]
#   ys <- strsplit(y,'')[[1]]
#   diff <- setdiff(ys,xs)
#   ifelse(x==y,"none",diff)
# }