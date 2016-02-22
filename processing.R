# install.packages(c("coda","mvtnorm","devtools"))
# library(devtools)
# 
# devtools::install_github("rmcelreath/rethinking")
library(dplyr)
cols <- c("SUBJECT","ANSWER","RESP","C1C2","CL_ONS","LEXICAL","LIQUID","POSITION","Running","TARGET","TYPE","VOWEL","CHOICE1","CHOICE2","CHOICE3","CHOICE4","CHOICE5")

CNT <- read.csv("CNT_ALL.csv")[,cols]
CNT$lang <- "Cantonese"
MAN <- read.csv("MAN_ALL.csv")[,cols]
MAN$lang <- "Mandarin"

liquidsAll <- rbind(CNT,MAN)
liquidsAll$lang <- as.factor(liquidsAll$lang)

liquidsAll <- liquidsAll %>% filter(TARGET%in%c("control","target")) %>%
  mutate(correct = ifelse(ANSWER==1,as.character(CHOICE1),
                          ifelse(ANSWER==2,as.character(CHOICE2),
                                 ifelse(ANSWER==3,as.character(CHOICE3),
                                        ifelse(ANSWER==4,as.character(CHOICE4),
                                               ifelse(ANSWER==5,as.character(CHOICE5),NA))))),
         choice = ifelse(RESP==1,as.character(CHOICE1),
                         ifelse(RESP==2,as.character(CHOICE2),
                                ifelse(RESP==3,as.character(CHOICE3),
                                       ifelse(RESP==4,as.character(CHOICE4),
                                              ifelse(RESP==5,as.character(CHOICE5),NA)))))) %>%
  mutate(change=stringDiff(correct,choice))

liquidsL <- liquidsAll %>% filter(LIQUID=="L")
liquidsR <- liquidsAll %>% filter(LIQUID=="R")