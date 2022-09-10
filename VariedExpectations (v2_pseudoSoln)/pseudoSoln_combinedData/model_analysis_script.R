########################################################
########################################################
########################################################
#############                              #############
#############     ORDER ANALYSIS SCRIPT    #############
#############                              #############
########################################################
########################################################
########################################################
# LOAD ALL RELEVANT LIBRARIES:
library(lme4)
library(nlme)
library(boot)
library(car) 
library(reshape2)
library(ggplot2)
library(ez)
library(ggsignif)
library(lsr)
library(sjmisc)
library(sjstats)
library(BayesFactor)
library(foreign)
library(plyr)
library(dplyr)
library(lattice)
library(openxlsx)
library(sjPlot)
library(xtable)
library(ggforce)
options(scipen=9999)

######################
# data preprocessing #
######################
# load data
D = read.csv(file.choose(), header = TRUE)
D = na.omit(D)
#nrow(D[rowSums(is.na(D)) < 2, ])
#D = D[rowSums(is.na(D)) < 2, ]

# get the structure of the data
str(D)

# create a test_choice variable
D$test_choice = rep(0,nrow(D))
for(i in 1:nrow(D)){
  if(D$test_activity_correct[i]>0.5 && D$test_activity_incorrect[i]<0.5){
    D$test_choice[i] = 1
  }else{
    D$test_choice[i] = 0
  }
}
# 0 - incorrect; 1 - correct

D$direction_choice = rep(0, nrow(D))
for(i in 1:nrow(D)){
  if(D$direction_activity_SOCconsistent[i]>0.5 && D$direction_activity_inverse[i]<0.5){
    D$direction_choice[i] = 1
  }else if(D$direction_activity_SOCconsistent[i]>0.5 && D$direction_activity_inverse[i]>0.5){
    D$direction_choice[i] = 0
  }else if(D$direction_activity_SOCconsistent[i]<0.5 && D$direction_activity_inverse[i]>0.5){
    D$direction_choice[i] = 2
  }else{
    D$direction_choice[i] = 999
  }
}
# 0 - both; 1 - SOC consistent; 2 - Inverse choice; 999 - flag

D$control_choice = rep(0, nrow(D))
for(i in 1:nrow(D)){
  if(D$control_activity_ordered[i]>0.5 && D$control_activity_inverse[i]<0.5){
    D$control_choice[i] = 1
  }else if(D$control_activity_ordered[i]>0.5 && D$control_activity_inverse[i]>0.5){
    D$control_choice[i] = 0
  }else if(D$control_activity_ordered[i]<0.5 && D$control_activity_inverse[i]>0.5){
    D$control_choice[i] = 2
  }else{
    D$control_choice[i] = 999
  }
}
# 0 - both; 1 - Order consistent; 2 - Inverse choice; 999 - flag

#####################################
# preliminary analyses and plotting #
#####################################
D$model_type = as.factor(D$model_type)
D$model_type = ordered(D$model_type, levels = c("no_inverse", "weak_inverse", "split", "strong_inverse"))
#D$model_type = ordered(D$model_type, levels = c("no_inverse","split"))
levels(D$model_type)
# get distribution of successes and failure 
# test_choice
table(D$test_choice)
table(D[D$model_type=="no_inverse",]$test_choice)
table(D[D$model_type=="weak_inverse",]$test_choice)
table(D[D$model_type=="split",]$test_choice)
table(D[D$model_type=="strong_inverse",]$test_choice)
baseline_test_sucess_prob = table(D$test_choice)[[2]]/(table(D$test_choice)[[1]]+table(D$test_choice)[[2]])
baseline_test_sucess_prob
baseline_test_success_odds = baseline_test_sucess_prob/(1-baseline_test_sucess_prob) # this is what will be shown
baseline_test_success_odds 

# direction_choice
table(D$direction_choice)
table(D[D$model_type=="no_inverse",]$direction_choice)
table(D[D$model_type=="weak_inverse",]$direction_choice)
table(D[D$model_type=="split",]$direction_choice)
table(D[D$model_type=="strong_inverse",]$direction_choice)
baseline_direction_sucess_prob = table(D$direction_choice)[[2]]/(table(D$direction_choice)[[1]]+table(D$direction_choice)[[2]])
baseline_direction_sucess_prob
baseline_direction_success_odds = baseline_direction_sucess_prob/(1-baseline_direction_sucess_prob) # this is what will be shown
baseline_direction_success_odds

# control_choice
table(D$control_choice)
table(D[D$model_type=="no_inverse",]$control_choice)
table(D[D$model_type=="weak_inverse",]$control_choice)
table(D[D$model_type=="split",]$control_choice)
table(D[D$model_type=="strong_inverse",]$control_choice)
baseline_control_sucess_prob = table(D$control_choice)[[2]]/(table(D$control_choice)[[1]]+table(D$control_choice)[[2]])
baseline_control_sucess_prob
baseline_control_success_odds = baseline_control_sucess_prob/(1-baseline_control_sucess_prob) # this is what will be shown
baseline_control_success_odds

#################
# main analysis #
#################
# test_choice_main_analysis

# no inverse Model #
# soc
glm.fit.soc_ni = glm(test_choice[D$model_type=="no_inverse"]~1, 
                     data = D, 
                     family = "binomial")
summary(glm.fit.soc_ni)

# direction
glm.fit.dir_ni = glm(direction_choice[D$model_type=="no_inverse"]~1,
                     data = D, 
                     family = "binomial")
summary(glm.fit.dir_ni)

# control
glm.fit.cont_ni = glm(control_choice[D$model_type=="no_inverse"]~1,
                      data = D, 
                      family = "binomial")
summary(glm.fit.cont_ni)

# split Model #
# soc
glm.fit.soc_split = glm(test_choice[D$model_type=="split"]~1, 
                        data = D, 
                        family = "binomial")
summary(glm.fit.soc_split)

# direction
glm.fit.dir_split = glm(direction_choice[D$model_type=="split"]~1,
                        data = D, 
                        family = "binomial")
summary(glm.fit.dir_split)

# control
glm.fit.cont_split = glm(control_choice[D$model_type=="split"]~1,
                         data = D, 
                         family = "binomial")
summary(glm.fit.cont_split)

# activity level analysis 
D$test_activityDiff = D$test_activity_correct - D$test_activity_incorrect
D$direction_activityDiff = D$direction_activity_SOCconsistent - D$direction_activity_inverse
D$control_activityDiff = D$control_activity_ordered - D$control_activity_inverse

t.test(D$test_activityDiff[D$model_type=="no_inverse"], D$test_activityDiff[D$model_type=="split"])
t.test(D$direction_activityDiff[D$model_type=="no_inverse"], D$direction_activityDiff[D$model_type=="split"])
t.test(D$control_activityDiff[D$model_type=="no_inverse"], D$control_activityDiff[D$model_type=="split"])

# t-test Figure #
ggplot(D) +
  geom_bar(aes(x = model_type, y = control_activityDiff, fill = model_type), stat = "summary") +
  theme_bw() +
  scale_fill_manual(values= c("#FF9999","black")) +
  xlab("Model Type") +
  ylab("Difference in Control Test Activity") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none") +
  scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0, 0.7))

# FIGURES #
D$test_choice = revalue(x = as.factor(D$test_choice), 
                             c("0" = "Incorrect", "1"="SOC Consistent"))
D$test_choice = factor(D$test_choice, levels = c("Incorrect", "SOC Consistent"), ordered = TRUE)
test_choice_plot = ggplot(D, aes(test_choice, fill = test_choice)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
test_choice_plot+geom_bar() + theme_bw() + # remove the gray background
  scale_fill_manual(values= c("black","#FF9999")) +
  xlab("SOC Consistent & SOC Inconsistent") +
  facet_wrap_paginate(~model_type, nrow = 2, page = 4) +
  #facet_wrap(~model_type) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        legend.key.size = unit(4,'mm')) +
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 25))

D$direction_choice = revalue(x = as.factor(D$direction_choice), 
                             c("0" = "Both", "1"="SOC Consistent","2"="Order Inconsistent", "999"="NA"))
#D$direction_choice = revalue(x = as.factor(D$direction_choice),c("0" = "Both", "1"="SOC Consistent"))
D$direction_choice = factor(D$direction_choice, levels = c("SOC Consistent", "Both", "Order Inconsistent", "NA"), ordered = TRUE)
direction_choice_plot = ggplot(D, aes(direction_choice, fill = direction_choice)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
direction_choice_plot+geom_bar() + theme_bw() + # remove the gray background
  scale_fill_manual(values= c("#FF9999","black","#438987","#FF0000")) +
  xlab("SOC Consistent & Inverse Construction") +
  facet_wrap_paginate(~model_type, nrow = 2, page = 4) +
  #facet_wrap(~model_type) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        legend.key.size = unit(4,'mm')) +
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 25))

D$control_choice = revalue(x = as.factor(D$control_choice), 
                           c("1"="Order Consistent", "0" = "Both","2"="Order Inconsistent", "999"="NA"))
#D$control_choice = revalue(x = as.factor(D$control_choice), 
#                           c("1"="Order Consistent", "0" = "Both"))
D$control_choice = factor(D$control_choice, levels = c("Order Consistent", "Both", "Order Inconsistent", "NA"), ordered = TRUE)
control_choice_plot = ggplot(D, aes(control_choice, fill = control_choice)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
control_choice_plot+geom_bar() + theme_bw() + # remove the gray background
  scale_fill_manual(values= c("#FF9999","black","#438987","#FF0000")) +
  xlab("Order Consistent & Order Inconsistent") +
  facet_wrap_paginate(~model_type, nrow = 2, page = 4) +
  #facet_wrap(~model_type) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), 
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.text = element_text(size = 6),
        legend.title = element_text(size = 7),
        legend.key.size = unit(4,'mm')) +
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 25))

