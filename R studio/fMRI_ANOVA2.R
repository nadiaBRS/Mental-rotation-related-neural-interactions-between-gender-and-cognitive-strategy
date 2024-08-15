library(rio)
library(plyr)
library(car)
library(dbplyr)
library(dplyr)
library(dtplyr)
library(effects)
library(emmeans)
library(ggeffects)
library(ggplot2)
library(ggpubr)
library(grid)
library(lattice)
library(magrittr)
library(multcomp)
library(PairedData)
library(plyr)
library(rio)
library(rstatix)
library(tibble)
library(tidyverse)
library(ggstatsplot)
library(lme4)
library(car)
library(MuMIn)
library(multimode)

getwd()
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/SISSA/R/MRlibrary-master/datasets")
data <- read.delim('fMRI_beh_filtered.txt', header = T)
summary(data)

# for some reason there is a bunch of rows that we don't need here. Let's remonve them 

data <- data[-(64:104),]

data_fem <- subset(data, Gender == 'F')
summary(data_fem)
sd(data_fem$Age)
data_males <- subset(data, Gender == 'M')
summary(data_males)
sd(data_males$Age)


names(data)[names(data) == 'rt_ctrl'] <- 'Control'
names(data)[names(data) == 'rt_ego'] <- 'EBS'
names(data)[names(data) == 'rt_allo'] <- 'OBS'
data <-data %>% gather(key = "condition", value = "Mean_RT" , "Control", "EBS", "OBS")
data$Mean_RT<- gsub(",",".",data$Mean_RT)
data$Mean_RT<-as.numeric(data$Mean_RT)
data$Sub <- as.factor(data$Sub)
data$TST <- as.factor(data$TST)
data$Gender <- as.factor(data$Gender)

str(data$Mean_RT)
mod <- lm(Mean_RT ~ Gender + condition + Gender : condition, data)
anova(mod)

rt <- ggplot(data,aes(condition, Mean_RT, fill = Gender))+
  geom_boxplot() + theme_bw() +
  ggtitle("RTs according to Condition and Gender")+
  xlab("Condition") + ylab("Mean RT")

rt + theme(panel.border = element_blank())

rt + theme_light()

rt + theme_bw() + theme(panel.border = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

rt + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                        axis.text.x=element_text(size=11),axis.text.y=element_text(size=11))
# there is an effect of gender ! Which we didn't have in the first 2 parts.

# in the next lines I kept the code to investigate an effect of condition. Ignore this and go to the lines that focus on gender.

dataCtrl<- subset(data,condition=="control")
#View(dataCtrl)

dataEBS<- subset(data,condition=="EBS") 
#View(dataEgo)

dataOBS<- subset(data,condition=="OBS")
#View(dataOBS)

t.test(dataCtrl$Mean_RT,dataOBS$Mean_RT)
t.test(dataCtrl$Mean_RT,dataEBS$Mean_RT)      
t.test(dataOBS$Mean_RT,dataEBS$Mean_RT)
str(data)

# Investigating gender


dataMenCtrl<- subset(dataCtrl, Gender=="M")
dataWomenCtrl<- subset(dataCtrl, Gender=="F")

dataMenOBS<- subset(dataOBS, Gender=="M")
dataWomenOBS<- subset(dataOBS, Gender=="F")

dataMenEBS<- subset(dataEBS, Gender=="M")
dataWomenEBS<- subset(dataEBS, Gender=="F")

summary(dataMenCtrl)

t.test(dataMenCtrl$Mean_RT,dataWomenCtrl$Mean_RT)
t.test(dataMenOBS$Mean_RT,dataWomenOBS$Mean_RT)
t.test(dataMenEBS$Mean_RT,dataWomenEBS$Mean_RT)    # this one doesn't survive. Interesting. Let's plot ittttt

library(emmeans)

emmeans(mod, list(pairwise ~ condition|Gender), adjust="tukey")
emmeans(mod, list(pairwise ~ Gender|condition), adjust="tukey")

##DOING ANOVA ON SCORES

getwd()
setwd("~/Library/Mobile Documents/com~apple~CloudDocs/SISSA/R/MRlibrary-master/datasets")
data <- read.delim('fMRI_beh_filtered.txt', header = T)
data <- data[-(64:104),]
summary(data)


names(data)[names(data) == 'acc_ctrl'] <- 'Control'
names(data)[names(data) == 'acc_ego'] <- 'EBS'
names(data)[names(data) == 'acc_allo'] <- 'OBS'
data <- data%>% gather(key = "condition", value = "Accuracy" , "Control", "EBS", "OBS")
data$Accuracy<-as.numeric(data$Accuracy)
data$Gender <- as.factor(data$Gender)
data$Order <- as.factor(data$Order)
data %>% group_by(condition)
data %>% get_summary_stats(Accuracy, type = "mean_sd")
data$condition <- factor(data$condition)
mod <- lm(Accuracy ~ Gender + condition + Order + Gender : condition + Order : condition, data)
anova(mod)

# there is an effect of Gender and an effect of condition. Let's plot it.

acc <- ggplot(data,aes(condition, Accuracy, fill=Gender))+
  geom_boxplot() + theme_bw() +
  ggtitle("Accuracy according to Condition and Gender")+
  xlab("Condition") + ylab("Accuracy")

acc + theme(panel.border = element_blank())

acc + theme_light()

acc + theme_bw() + theme(panel.border = element_blank(),
                        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))

acc + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                         panel.background = element_blank(),
                         axis.line = element_line(colour = "black"), axis.text.x=element_text(size=12),
                         axis.text.y=element_text(size=12))

acc + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                         axis.text.x=element_text(size=11),axis.text.y=element_text(size=11))

#let's plot this differently for each gender

data_women <- subset(data, Gender == "F")
data_men <- subset(data, Gender == "M")


acc_women <- ggplot(data_women,aes(condition, Accuracy, fill=condition))+
  geom_boxplot() + theme_bw() +
  xlab("Condition") + ylab("Accuracy")

acc_women + theme_bw() + theme(panel.border = element_blank())+
  scale_fill_manual(values = c('#FC4E07', '#48B242', '#3DB5FF'))+
  ylim(0.3,1)

acc_women + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                         panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


acc_women + theme_bw() + theme(panel.border = element_blank(),
                             panel.grid.minor = element_blank())+
  scale_fill_manual(values = c('yellow1', 'magenta1', 'cyan3')) + ylim(0.3,1)


acc_men <- ggplot(data_men,aes(condition, Accuracy, fill=condition))+
  geom_boxplot() + theme_bw() +
  xlab("Condition") + ylab("Accuracy")

acc_men

acc_men + theme_bw() + theme(panel.border = element_blank())+
  scale_fill_manual(values = c('#FC4E07', '#48B242', '#3DB5FF'))+
  ylim(0.3,1)


acc_men + theme_bw() + theme(panel.border = element_blank(),
                               panel.grid.minor = element_blank())+
  scale_fill_manual(values = c('yellow1', 'magenta1', 'cyan3')) + ylim(0.3,1)


acc_men + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


# Let's see what's significant


dataCtrl<- subset(data,condition=="Control")
#View(dataCtrl)
dataEBS<- subset(data,condition=="EBS")
#View(dataEgo)
dataOBS<- subset(data,condition=="OBS")
#View(dataOBS)
t.test(dataCtrl$Accuracy,dataOBS$Accuracy)
t.test(dataCtrl$Accuracy,dataEBS$Accuracy)
t.test(dataOBS$Accuracy,dataEBS$Accuracy)
str(data)

dataCtrlMen <- subset(dataCtrl, Gender =="M")
dataCtrlWomen <- subset(dataCtrl, Gender =="F")

dataOBSMen <- subset(dataOBS, Gender =="M")
dataOBSWomen <- subset(dataOBS, Gender =="F")

dataEBSMen <- subset(dataEBS, Gender =="M")
dataEBSWomen <- subset(dataEBS, Gender =="F")

library(emmeans)

emmeans(mod, list(pairwise ~ condition|Gender), adjust="tukey")
emmeans(mod, list(pairwise ~ Gender|condition), adjust="tukey")

t.test(dataCtrlMen$Accuracy,dataCtrlWomen$Accuracy)
t.test(dataOBSMen$Accuracy,dataOBSWomen$Accuracy)
t.test(dataEBSMen$Accuracy,dataEBSWomen$Accuracy)

t.test(dataCtrlWomen$Accuracy,dataEBSWomen$Accuracy)

t.test(dataCtrlWomen$Accuracy,dataOBSWomen$Accuracy)
t.test(dataCtrlWomen$Accuracy,dataOBSWomen$Accuracy)
t.test(dataEBSWomen$Accuracy,dataOBSWomen$Accuracy)



# Let's check the effect in order, in the OBScentric condition

dataCtrlA <- subset(dataCtrl, Order == 'A')
dataCtrlB <- subset(dataCtrl, Order == 'B')

dataOBSA <- subset(dataOBS, Order == 'A')
dataOBSB <- subset(dataOBS, Order == 'B')

dataEBSA <- subset(dataEBS, Order == 'A')
dataEBSB <- subset(dataEBS, Order == 'B')

t.test(dataCtrlA$Accuracy, dataCtrlB$Accuracy)
t.test(dataOBSA$Accuracy, dataOBSB$Accuracy)
t.test(dataEgoA$Accuracy, dataEgoB$Accuracy)


####### invetigating the order of the task ##########

emmeans(mod, list(pairwise ~ condition|Order), adjust="tukey")
emmeans(mod, list(pairwise ~ Order|condition), adjust="tukey")

inter_ver_cond <- ggplot(data,aes(Order, Accuracy, color=condition))+
  facet_wrap(~condition)+
  stat_summary(aes(group=condition, color=condition), fun="mean", geom="point", position= position_dodge(0.5)) +
  stat_summary(aes(group=condition, color = condition), fun = "mean", geom="line",position= position_dodge(0.5)) +              
  stat_summary(aes(group=condition, color=condition), fun.data = "mean_cl_normal",                  
               geom="errorbar", width=0.1,position= position_dodge(0.5))+
  theme_bw() + theme( panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), 
                     axis.text.x=element_text(size=11),axis.text.y=element_text(size=11))

inter_ver_cond + coord_cartesian(ylim = c(0.60, 0.9)) + ggtitle("Interaction version*condition")

                                                                                                                   