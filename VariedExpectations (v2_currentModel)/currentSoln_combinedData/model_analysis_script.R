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
library(nnet)
options(scipen=9999)

######################
# data preprocessing #
######################
# load data
D = read.csv(file.choose(), header = TRUE)
D = na.omit(D)

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
table(D$test_choice)
D$test_choice = revalue(x = as.factor(D$test_choice), c("0" = "SOC Inconsistent", "1"="SOC Consistent"))
# 0 - incorrect; 1 - correct
table(D$model_type, D$test_choice)

## create a direction_choice_lib variable ##
D$direction_choice_lib = rep(0, nrow(D))
for(i in 1:nrow(D)){
  if(D$direction_activity_SOCconsistent[i]>0.5 && D$direction_activity_inverse[i]<0.5){
    D$direction_choice_lib[i] = 1 #soc-consistent / order-consistent
  }else if(D$direction_activity_SOCconsistent[i]>0.5 && D$direction_activity_inverse[i]>0.5){
    D$direction_choice_lib[i] = 0
  }else if(D$direction_activity_SOCconsistent[i]<0.5 && D$direction_activity_inverse[i]>0.5){
    D$direction_choice_lib[i] = 0 #chose inverse but not soc_con
  }else{
    D$direction_choice_lib[i] = 0 #neither
  }
}

# name the levels of direction_choice_lib
D$direction_choice_lib = revalue(x = as.factor(D$direction_choice_lib), 
                                 c("0" = "Other", "1"="Order Consistent"))

#  Note that this corresponds to a "liberal" coding of adults' responses 
# to the direction trials. 

# get the distribution of "order consistent" and "other" choices
# for the Direction test trial
table(D$model_type, D$direction_choice_lib)

## create a direction_choice_conserv variable ##
D$direction_choice_conserv = rep(0, nrow(D))
for(i in 1:nrow(D)){
  if(D$direction_activity_SOCconsistent[i]>0.5 && D$direction_activity_inverse[i]<0.5){
    D$direction_choice_conserv[i] = 1 #soc-consistent / order-consistent
  }else if(D$direction_activity_SOCconsistent[i]>0.5 && D$direction_activity_inverse[i]>0.5){
    D$direction_choice_conserv[i] = 0
  }else if(D$direction_activity_SOCconsistent[i]<0.5 && D$direction_activity_inverse[i]>0.5){
    D$direction_choice_conserv[i] = 2 #chose inverse but not soc_con
  }else{
    D$direction_choice_conserv[i] = 3 #neither
  }
}
# 0 - both; 1 - SOC_consistent; 2 - inverse but not soc_con; 3 - neither

# name the levels of direction_choice_conserv
D$direction_choice_conserv = revalue(x = as.factor(D$direction_choice_conserv), 
                                     c("0" = "Both", "1"="Order Consistent",
                                       "2" = "Inverse", "3"= "Neither"))

# get the distribution of choices for the conservative coding of
# direction
table(D$model_type, D$direction_choice_conserv)

## create a control_choice_lib variable ##
D$control_choice_lib = rep(0, nrow(D))
for(i in 1:nrow(D)){
  if(D$control_activity_ordered[i]>0.5 && D$control_activity_inverse[i]<0.5){
    D$control_choice_lib[i] = 1 #order-consistent 
  }else if(D$control_activity_ordered[i]>0.5 && D$control_activity_inverse[i]>0.5){
    D$control_choice_lib[i] = 0 #both
  }else if(D$control_activity_ordered[i]<0.5 && D$control_activity_inverse[i]>0.5){
    D$control_choice_lib[i] = 0 #chose inverse but not order_con
  }else{
    D$control_choice_lib[i] = 0 #neither
  }
}

# name the levels of control_choice_lib
D$control_choice_lib = revalue(x = as.factor(D$control_choice_lib), 
                               c("0" = "Other", "1"="Order Consistent"))
table(D$model_type, D$control_choice_lib)

## create a control_choice_conserv variable ##
D$control_choice_conserv = rep(0, nrow(D))
for(i in 1:nrow(D)){
  if(D$control_activity_ordered[i]>0.5 && D$control_activity_inverse[i]<0.5){
    D$control_choice_conserv[i] = 1 #order-consistent 
  }else if(D$control_activity_ordered[i]>0.5 && D$control_activity_inverse[i]>0.5){
    D$control_choice_conserv[i] = 0 #both
  }else if(D$control_activity_ordered[i]<0.5 && D$control_activity_inverse[i]>0.5){
    D$control_choice_conserv[i] = 2 #chose inverse but not order_con
  }else{
    D$control_choice_conserv[i] = 3 #neither
  }
}
# 0 - both; 1 - order_consistent; 2 - inverse but not order_consistent; 3 - neither

# name the levels of direction_choice_conserv
D$control_choice_conserv = revalue(x = as.factor(D$control_choice_conserv), 
                                   c("0" = "Both", "1"="Order Consistent",
                                     "2" = "Inverse Only", "3" = "Neither"))
# get distributional data
table(D$model_type, D$control_choice_conserv)

#####################################
# preliminary analyses and plotting #
#####################################
D$model_type = as.factor(D$model_type)
D$model_type = ordered(D$model_type, levels = c("no_inverse", "weak_inverse", "split", "strong_inverse"))
levels(D$model_type)

# get distribution of successes and failure for each model

##################
### no_inverse ###
##################
# test_choice
table(D$test_choice[D$model_type=="no_inverse"])
noInverse_test_sucess_prob = table(D$test_choice[D$model_type=="no_inverse"])[[2]]/(table(D$test_choice[D$model_type=="no_inverse"])[[1]]+table(D$test_choice[D$model_type=="no_inverse"])[[2]])
noInverse_test_sucess_prob
noInverse_test_success_odds = noInverse_test_sucess_prob/(1-noInverse_test_sucess_prob) # this is what will be shown
noInverse_test_success_odds

# direction_choice
table(D$direction_choice_lib[D$model_type=="no_inverse"])
noInverse_direction_ordered_lib_prob = table(D$direction_choice_lib[D$model_type=="no_inverse"])[[2]]/(table(D$direction_choice_lib[D$model_type=="no_inverse"])[[1]]+table(D$direction_choice_lib[D$model_type=="no_inverse"])[[2]])
noInverse_direction_ordered_lib_prob 
noInverse_direction_ordered_lib_odds = noInverse_direction_ordered_lib_prob/(1-noInverse_direction_ordered_lib_prob) # this is what will be shown
noInverse_direction_ordered_lib_odds

table(D$direction_choice_conserv[D$model_type=="no_inverse"])
noInverse_direction_ordered_conserv_prob = table(D$direction_choice_conserv[D$model_type=="no_inverse"])[[2]]/(table(D$direction_choice_conserv[D$model_type=="no_inverse"])[[1]]+
                                                                                    table(D$direction_choice_conserv[D$model_type=="no_inverse"])[[2]]+
                                                                                    table(D$direction_choice_conserv[D$model_type=="no_inverse"])[[3]]+
                                                                                    table(D$direction_choice_conserv[D$model_type=="no_inverse"])[[4]])
noInverse_direction_ordered_conserv_prob 
noInverse_direction_ordered__conserv_odds = noInverse_direction_ordered_conserv_prob/(1-noInverse_direction_ordered_conserv_prob) # this is what will be shown
noInverse_direction_ordered__conserv_odds


# control_choice
table(D$control_choice_lib[D$model_type=="no_inverse"])
noInverse_control_ordered_lib_prob = table(D$control_choice_lib[D$model_type=="no_inverse"])[[2]]/(table(D$control_choice_lib[D$model_type=="no_inverse"])[[1]]+table(D$control_choice_lib[D$model_type=="no_inverse"])[[2]])
noInverse_control_ordered_lib_prob
noInverse_control_ordered_lib_odds = noInverse_control_ordered_lib_prob/(1-noInverse_control_ordered_lib_prob) # this is what will be shown
noInverse_control_ordered_lib_odds


table(D$control_choice_conserv[D$model_type=="no_inverse"])
noInverse_control_ordered_conserv_prob = table(D$control_choice_conserv[D$model_type=="no_inverse"])[[2]]/(table(D$control_choice_conserv[D$model_type=="no_inverse"])[[1]]+
                                                                                table(D$control_choice_conserv[D$model_type=="no_inverse"])[[2]]+
                                                                                table(D$control_choice_conserv[D$model_type=="no_inverse"])[[3]])
                                                                                #+table(D$control_choice_conserv[D$model_type=="no_inverse"])[[4]])
noInverse_control_ordered_conserv_prob
noInverse_control_ordered_conserv_odds = noInverse_control_ordered_conserv_prob/(1-noInverse_control_ordered_conserv_prob) # this is what will be shown
noInverse_control_ordered_conserv_odds

####################
### weak_inverse ###
####################
# test_choice
table(D$test_choice[D$model_type=="weak_inverse"])
weakInverse_test_sucess_prob = table(D$test_choice[D$model_type=="weak_inverse"])[[2]]/(table(D$test_choice[D$model_type=="weak_inverse"])[[1]]+table(D$test_choice[D$model_type=="weak_inverse"])[[2]])
weakInverse_test_sucess_prob
weakInverse_test_success_odds = weakInverse_test_sucess_prob/(1-weakInverse_test_sucess_prob) # this is what will be shown
weakInverse_test_success_odds 

# direction_choice
table(D$direction_choice_lib[D$model_type=="weak_inverse"])
weakInverse_direction_ordered_lib_prob = table(D$direction_choice_lib[D$model_type=="weak_inverse"])[[2]]/(table(D$direction_choice_lib[D$model_type=="weak_inverse"])[[1]]+table(D$direction_choice_lib[D$model_type=="weak_inverse"])[[2]])
weakInverse_direction_ordered_lib_prob 
weakInverse_direction_ordered_lib_odds = weakInverse_direction_ordered_lib_prob/(1-weakInverse_direction_ordered_lib_prob) # this is what will be shown
weakInverse_direction_ordered_lib_odds

table(D$direction_choice_conserv[D$model_type=="weak_inverse"])
weakInverse_direction_ordered_conserv_prob = table(D$direction_choice_conserv[D$model_type=="weak_inverse"])[[2]]/(table(D$direction_choice_conserv[D$model_type=="weak_inverse"])[[1]]+
                                                                                                                 table(D$direction_choice_conserv[D$model_type=="weak_inverse"])[[2]]+
                                                                                                                 table(D$direction_choice_conserv[D$model_type=="weak_inverse"])[[3]]+
                                                                                                                 table(D$direction_choice_conserv[D$model_type=="weak_inverse"])[[4]])
weakInverse_direction_ordered_conserv_prob 
weakInverse_direction_ordered__conserv_odds = weakInverse_direction_ordered_conserv_prob/(1-weakInverse_direction_ordered_conserv_prob) # this is what will be shown
weakInverse_direction_ordered__conserv_odds


# control_choice
table(D$control_choice_lib[D$model_type=="weak_inverse"])
weakInverse_control_ordered_lib_prob = table(D$control_choice_lib[D$model_type=="weak_inverse"])[[2]]/(table(D$control_choice_lib[D$model_type=="weak_inverse"])[[1]]+table(D$control_choice_lib[D$model_type=="weak_inverse"])[[2]])
weakInverse_control_ordered_lib_prob
weakInverse_control_ordered_lib_odds = weakInverse_control_ordered_lib_prob/(1-weakInverse_control_ordered_lib_prob) # this is what will be shown
weakInverse_control_ordered_lib_odds


table(D$control_choice_conserv[D$model_type=="weak_inverse"])
weakInverse_control_ordered_conserv_prob = table(D$control_choice_conserv[D$model_type=="weak_inverse"])[[2]]/(table(D$control_choice_conserv[D$model_type=="weak_inverse"])[[1]]+
                                                                                                             table(D$control_choice_conserv[D$model_type=="weak_inverse"])[[2]]+
                                                                                                             table(D$control_choice_conserv[D$model_type=="weak_inverse"])[[3]]+
                                                                                                             table(D$control_choice_conserv[D$model_type=="weak_inverse"])[[4]])
weakInverse_control_ordered_conserv_prob
weakInverse_control_ordered_conserv_odds = weakInverse_control_ordered_conserv_prob/(1-weakInverse_control_ordered_conserv_prob) # this is what will be shown
weakInverse_control_ordered_conserv_odds

#####################
####### split #######
#####################
# test_choice
table(D$test_choice[D$model_type=="split"])
split_test_sucess_prob = table(D$test_choice[D$model_type=="split"])[[2]]/(table(D$test_choice[D$model_type=="split"])[[1]]+table(D$test_choice[D$model_type=="split"])[[2]])
split_test_sucess_prob
split_test_success_odds = split_test_sucess_prob/(1-split_test_sucess_prob) # this is what will be shown
split_test_success_odds 

# direction_choice
table(D$direction_choice_lib[D$model_type=="split"])
split_direction_ordered_lib_prob = table(D$direction_choice_lib[D$model_type=="split"])[[2]]/(table(D$direction_choice_lib[D$model_type=="split"])[[1]]+table(D$direction_choice_lib[D$model_type=="split"])[[2]])
split_direction_ordered_lib_prob 
split_direction_ordered_lib_odds = split_direction_ordered_lib_prob/(1-split_direction_ordered_lib_prob) # this is what will be shown
split_direction_ordered_lib_odds

table(D$direction_choice_conserv[D$model_type=="split"])
split_direction_ordered_conserv_prob = table(D$direction_choice_conserv[D$model_type=="split"])[[2]]/(table(D$direction_choice_conserv[D$model_type=="split"])[[1]]+
                                                                                                                     table(D$direction_choice_conserv[D$model_type=="split"])[[2]]+
                                                                                                                     table(D$direction_choice_conserv[D$model_type=="split"])[[3]]+
                                                                                                                     table(D$direction_choice_conserv[D$model_type=="split"])[[4]])
split_direction_ordered_conserv_prob 
split_direction_ordered__conserv_odds = split_direction_ordered_conserv_prob/(1-split_direction_ordered_conserv_prob) # this is what will be shown
split_direction_ordered__conserv_odds


# control_choice
table(D$control_choice_lib[D$model_type=="split"])
split_control_ordered_lib_prob = table(D$control_choice_lib[D$model_type=="split"])[[2]]/(table(D$control_choice_lib[D$model_type=="split"])[[1]]+table(D$control_choice_lib[D$model_type=="split"])[[2]])
split_control_ordered_lib_prob
split_control_ordered_lib_odds = split_control_ordered_lib_prob/(1-split_control_ordered_lib_prob) # this is what will be shown
split_control_ordered_lib_odds


table(D$control_choice_conserv[D$model_type=="split"])
split_control_ordered_conserv_prob = table(D$control_choice_conserv[D$model_type=="split"])[[2]]/(table(D$control_choice_conserv[D$model_type=="split"])[[1]]+
                                                                                                                 table(D$control_choice_conserv[D$model_type=="split"])[[2]])
                                                                                                                 #+table(D$control_choice_conserv[D$model_type=="split"])[[3]]
                                                                                                                 #+table(D$control_choice_conserv[D$model_type=="split"])[[4]])
split_control_ordered_conserv_prob
split_control_ordered_conserv_odds = split_control_ordered_conserv_prob/(1-split_control_ordered_conserv_prob) # this is what will be shown
split_control_ordered_conserv_odds

######################
### strong_inverse ###
######################
# test_choice
table(D$test_choice[D$model_type=="strong_inverse"])
strongInverse_test_sucess_prob = table(D$test_choice[D$model_type=="strong_inverse"])[[2]]/(table(D$test_choice[D$model_type=="strong_inverse"])[[1]]+table(D$test_choice[D$model_type=="strong_inverse"])[[2]])
strongInverse_test_sucess_prob
strongInverse_test_success_odds = strongInverse_test_sucess_prob/(1-strongInverse_test_sucess_prob) # this is what will be shown
strongInverse_test_success_odds 

# direction_choice
table(D$direction_choice_lib[D$model_type=="strong_inverse"])
strongInverse_direction_ordered_lib_prob = table(D$direction_choice_lib[D$model_type=="strong_inverse"])[[2]]/(table(D$direction_choice_lib[D$model_type=="strong_inverse"])[[1]]+table(D$direction_choice_lib[D$model_type=="strong_inverse"])[[2]])
strongInverse_direction_ordered_lib_prob 
strongInverse_direction_ordered_lib_odds = strongInverse_direction_ordered_lib_prob/(1-strongInverse_direction_ordered_lib_prob) # this is what will be shown
strongInverse_direction_ordered_lib_odds

table(D$direction_choice_conserv[D$model_type=="strong_inverse"])
strongInverse_direction_ordered_conserv_prob = table(D$direction_choice_conserv[D$model_type=="strong_inverse"])[[2]]/(table(D$direction_choice_conserv[D$model_type=="strong_inverse"])[[1]]+
                                                                                                                     table(D$direction_choice_conserv[D$model_type=="strong_inverse"])[[2]]+
                                                                                                                     table(D$direction_choice_conserv[D$model_type=="strong_inverse"])[[3]]+
                                                                                                                     table(D$direction_choice_conserv[D$model_type=="strong_inverse"])[[4]])
strongInverse_direction_ordered_conserv_prob 
strongInverse_direction_ordered__conserv_odds = strongInverse_direction_ordered_conserv_prob/(1-strongInverse_direction_ordered_conserv_prob) # this is what will be shown
strongInverse_direction_ordered__conserv_odds


# control_choice
table(D$control_choice_lib[D$model_type=="strong_inverse"])
strongInverse_control_ordered_lib_prob = table(D$control_choice_lib[D$model_type=="strong_inverse"])[[2]]/(table(D$control_choice_lib[D$model_type=="strong_inverse"])[[1]]+table(D$control_choice_lib[D$model_type=="strong_inverse"])[[2]])
strongInverse_control_ordered_lib_prob
strongInverse_control_ordered_lib_odds = strongInverse_control_ordered_lib_prob/(1-strongInverse_control_ordered_lib_prob) # this is what will be shown
strongInverse_control_ordered_lib_odds


table(D$control_choice_conserv[D$model_type=="strong_inverse"])
strongInverse_control_ordered_conserv_prob = table(D$control_choice_conserv[D$model_type=="strong_inverse"])[[2]]/(table(D$control_choice_conserv[D$model_type=="strong_inverse"])[[1]]+
                                                                                                                 table(D$control_choice_conserv[D$model_type=="strong_inverse"])[[2]]+
                                                                                                                 table(D$control_choice_conserv[D$model_type=="strong_inverse"])[[3]]+
                                                                                                                 table(D$control_choice_conserv[D$model_type=="strong_inverse"])[[4]])
strongInverse_control_ordered_conserv_prob
strongInverse_control_ordered_conserv_odds = strongInverse_control_ordered_conserv_prob/(1-strongInverse_control_ordered_conserv_prob) # this is what will be shown
strongInverse_control_ordered_conserv_odds

#################
# main analysis #
#################

### no inverse Model ###
# test_choice
glm.fit.soc_ni = glm(test_choice[D$model_type=="no_inverse"]~1, 
                     data = D, 
                     family = "binomial")
summary(glm.fit.soc_ni)
#estimate = coefficient -- log odds - so if you exponentiate it, you'll get real odds
exp(0.9383) #odds of correcting SOC consistent in test choice.
confint(glm.fit.soc_ni)

# Figure
noInverse_test_choice_barplot = ggplot(D[D$model_type=="no_inverse",], aes(test_choice, fill = test_choice)) 
noInverse_test_choice_barplot + 
  geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Second-Order Correlation Test Simulation: No Inverse Model") +
  ylab("Number of Choice") + 
  coord_cartesian(ylim=c(0, 30)) +
  scale_fill_manual(values=c("black","#ffc857")) +
  theme_bw() +
  theme(legend.position = "none")

# direction liberal
glm.fit.dir_ni = glm(direction_choice_lib[D$model_type=="no_inverse"]~1, 
                           data = D, family = "binomial")
summary(glm.fit.dir_ni)
#estimate = coefficient -- log odds - so if you exponentiate it, you'll get real odds
exp(0.9383) #odds of correcting SOC consistent in test choice.

# liberal figure
noInverse_direction_choice_liberal_barplot = ggplot(D[D$model_type=="no_inverse",], aes(direction_choice_lib, fill = direction_choice_lib)) 
noInverse_direction_choice_liberal_barplot + geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Relational Order Test Simulation: No Inverse Model") +
  ylab("Number of Choice") + 
  coord_cartesian(ylim=c(0, 30)) +
  scale_fill_manual(values=c("black","#ffc857")) +
  theme_bw() +
  theme(legend.position = "none")

## conservative analyses ##
table(D$direction_choice_conserv[D$model_type=="no_inverse"])
noInverse_multinom_direction_choice = multinom(direction_choice_conserv[D$model_type=="no_inverse"] ~ 1, data = D)
summary(noInverse_multinom_direction_choice)

noInverse_multinom_direction_choice_z <- summary(noInverse_multinom_direction_choice)$coefficients/summary(noInverse_multinom_direction_choice)$standard.errors
noInverse_multinom_direction_choice_z
noInverse_multinom_direction_choice_p <- (1 - pnorm(abs(noInverse_multinom_direction_choice_z), 0, 1)) * 2
noInverse_multinom_direction_choice_p

# conservative figure
direction_choice_conserv_barplot = ggplot(D[D$model_type=="no_inverse",], aes(direction_choice_conserv, fill = direction_choice_conserv)) 
direction_choice_conserv_barplot + geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 20)) +
  scale_fill_manual(values=c("black","#ffc857", "darkblue", "#9933FF")) +
  theme_bw()

# control
glm.fit.cont_ni = glm(control_choice_lib[D$model_type=="no_inverse"]~1,
                      data = D, 
                      family = "binomial")
summary(glm.fit.cont_ni)
exp(1.2730)
confint(glm.fit.cont_ni)

# liberal figure
noInverse_control_choice_liberal_barplot = ggplot(D[D$model_type=="no_inverse",], aes(control_choice_lib, fill = control_choice_lib)) 
noInverse_control_choice_liberal_barplot + geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Controlled Relational Order Test Simulation: No Inverse Model") +
  ylab("Number of Choice") + 
  coord_cartesian(ylim=c(0, 30)) +
  scale_fill_manual(values=c("black","#ffc857")) +
  theme_bw() +
  theme(legend.position = "none")

## conservative analyses ##
noInverse_multinom_control_choice = multinom(control_choice_conserv[D$model_type=="no_inverse"] ~ 1, data = D)
summary(noInverse_multinom_control_choice)

noInverse_multinom_control_choice_z <- summary(noInverse_multinom_control_choice)$coefficients/summary(noInverse_multinom_control_choice)$standard.errors
noInverse_multinom_control_choice_z
noInverse_multinom_control_choice_p <- (1 - pnorm(abs(noInverse_multinom_control_choice_z), 0, 1)) * 2
noInverse_multinom_control_choice_p

# conservative figure
noInverse_control_choice_conserv_barplot = ggplot(D[D$model_type=="no_inverse",], aes(control_choice_conserv, fill = control_choice_conserv)) 
noInverse_control_choice_conserv_barplot + geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Controlled Relational Order Test Simulation: No Inverse Model") +
  ylab("Number of Choice") + 
  coord_cartesian(ylim=c(0, 20)) +
  scale_fill_manual(values=c("black","#ffc857", "darkblue", "#9933FF")) +
  theme_bw() +
  theme(legend.position = "none")

### weak inverse Model ###
# test_choice
glm.fit.soc_wi = glm(test_choice[D$model_type=="weak_inverse"]~1, 
                     data = D, 
                     family = "binomial")
summary(glm.fit.soc_wi)
#estimate = coefficient -- log odds - so if you exponentiate it, you'll get real odds
exp(1.6094)

# Figure
weakInverse_test_choice_barplot = ggplot(D[D$model_type=="weak_inverse",], aes(test_choice, fill = test_choice)) 
weakInverse_test_choice_barplot + 
  geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 30)) +
  scale_fill_manual(values=c("black","#ffc857")) +
  theme_bw()

# direction liberal
glm.fit.dir_wi = glm(direction_choice_lib[D$model_type=="weak_inverse"]~1, 
                     data = D, family = "binomial")
summary(glm.fit.dir_wi)
exp(0.5465)

# liberal figure
weakInverse_direction_choice_liberal_barplot = ggplot(D[D$model_type=="weak_inverse",], aes(direction_choice_lib, fill = direction_choice_lib)) 
weakInverse_direction_choice_liberal_barplot + geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 30)) +
  scale_fill_manual(values=c("black","#ffc857")) +
  theme_bw()

## conservative analyses ##
weakInverse_multinom_direction_choice = multinom(direction_choice_conserv[D$model_type=="weak_inverse"] ~ 1, data = D)
summary(weakInverse_multinom_direction_choice)

weakInverse_multinom_direction_choice_z <- summary(weakInverse_multinom_direction_choice)$coefficients/summary(weakInverse_multinom_direction_choice)$standard.errors
weakInverse_multinom_direction_choice_z
weakInverse_multinom_direction_choice_p <- (1 - pnorm(abs(weakInverse_multinom_direction_choice_z), 0, 1)) * 2
weakInverse_multinom_direction_choice_p

# conservative figure
direction_choice_conserv_barplot = ggplot(D[D$model_type=="weak_inverse",], aes(direction_choice_conserv, fill = direction_choice_conserv)) 
direction_choice_conserv_barplot + geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 20)) +
  scale_fill_manual(values=c("black","#ffc857", "darkblue", "#9933FF")) +
  theme_bw()

# control
glm.fit.cont_wi = glm(control_choice_lib[D$model_type=="weak_inverse"]~1,
                      data = D, 
                      family = "binomial")
summary(glm.fit.cont_wi)
exp(0.6931)

# liberal figure
weakInverse_control_choice_liberal_barplot = ggplot(D[D$model_type=="weak_inverse",], aes(control_choice_lib, fill = control_choice_lib)) 
weakInverse_control_choice_liberal_barplot + geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 30)) +
  scale_fill_manual(values=c("black","#ffc857")) +
  theme_bw()

## conservative analyses ##
weakInverse_multinom_control_choice = multinom(control_choice_conserv[D$model_type=="weak_inverse"] ~ 1, data = D)
summary(weakInverse_multinom_control_choice)

weakInverse_multinom_control_choice_z <- summary(weakInverse_multinom_control_choice)$coefficients/summary(weakInverse_multinom_control_choice)$standard.errors
weakInverse_multinom_control_choice_z
weakInverse_multinom_control_choice_p <- (1 - pnorm(abs(weakInverse_multinom_control_choice_z), 0, 1)) * 2
weakInverse_multinom_control_choice_p

# conservative figure
weakInverse_control_choice_conserv_barplot = ggplot(D[D$model_type=="weak_inverse",], aes(control_choice_conserv, fill = control_choice_conserv)) 
weakInverse_control_choice_conserv_barplot + geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 20)) +
  scale_fill_manual(values=c("black","#ffc857", "darkblue", "#9933FF")) +
  theme_bw()

### split Model ###
# test_choice
table(D$test_choice[D$model_type=="split"])
D$test_choice = relevel(D$test_choice, ref="SOC Inconsistent")
glm.fit.soc_split = glm(test_choice[D$model_type=="split"]~1, 
                     data = D, 
                     family = "binomial")
summary(glm.fit.soc_split)
#estimate = coefficient -- log odds - so if you exponentiate it, you'll get real odds
exp(-25.57)
confint(glm.fit.soc_split)

# Figure
split_test_choice_barplot = ggplot(D[D$model_type=="split",], aes(test_choice, fill = test_choice)) 
split_test_choice_barplot + 
  geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Second-Order Correlation Test Simulation: Split Model") +
  ylab("Number of Choice") + 
  coord_cartesian(ylim=c(0, 30)) + 
  scale_fill_manual(values=c("black","#ffc857")) +
  theme_bw() +
  theme(legend.position = "none")

# direction liberal
glm.fit.dir_split = glm(direction_choice_lib[D$model_type=="split"]~1, 
                     data = D, family = "binomial")
summary(glm.fit.dir_split)
exp(-0.9383)
confint(glm.fit.dir_split)

# liberal figure
split_direction_choice_liberal_barplot = ggplot(D[D$model_type=="split",], aes(direction_choice_lib, fill = direction_choice_lib)) 
split_direction_choice_liberal_barplot + geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Relational Order Test Simulation: Split Model") +
  ylab("Number of Choice") + 
  coord_cartesian(ylim=c(0, 30)) +
  scale_fill_manual(values=c("black","#ffc857")) +
  theme_bw() +
  theme(legend.position = "none")

## conservative analyses ##
split_multinom_direction_choice = multinom(direction_choice_conserv[D$model_type=="split"] ~ 1, data = D)
summary(split_multinom_direction_choice)

split_multinom_direction_choice_z <- summary(split_multinom_direction_choice)$coefficients/summary(split_multinom_direction_choice)$standard.errors
split_multinom_direction_choice_z
split_multinom_direction_choice_p <- (1 - pnorm(abs(split_multinom_direction_choice_z), 0, 1)) * 2
split_multinom_direction_choice_p

# conservative figure
split_direction_choice_conserv_barplot = ggplot(D[D$model_type=="split",], aes(direction_choice_conserv, fill = direction_choice_conserv)) 
split_direction_choice_conserv_barplot + geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Relational Order Test Simulation: Split Model") +
  ylab("Number of Choice") + 
  coord_cartesian(ylim=c(0, 20)) +
  scale_fill_manual(values=c("black","#ffc857", "darkblue", "#9933FF")) +
  theme_bw() +
  theme(legend.position = "none")

# control
glm.fit.cont_split = glm(control_choice_lib[D$model_type=="split"]~1,
                      data = D, 
                      family = "binomial")
summary(glm.fit.cont_split)
exp(-0.7885)
confint(glm.fit.cont_split)

# liberal figure
split_control_choice_liberal_barplot = ggplot(D[D$model_type=="split",], aes(control_choice_lib, fill = control_choice_lib)) 
split_control_choice_liberal_barplot + geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Controlled Relational Order Test Simulation: Split Model") +
  ylab("Number of Choice") + 
  coord_cartesian(ylim=c(0, 30)) +
  scale_fill_manual(values=c("black","#ffc857")) +
  theme_bw() +
  theme(legend.position = "none")

## conservative analyses ##
split_multinom_control_choice = multinom(control_choice_conserv[D$model_type=="split"] ~ 1, data = D)
summary(split_multinom_control_choice)

split_multinom_control_choice_z <- summary(split_multinom_control_choice)$coefficients/summary(split_multinom_control_choice)$standard.errors
split_multinom_control_choice_z
split_multinom_control_choice_p <- (1 - pnorm(abs(split_multinom_control_choice_z), 0, 1)) * 2
split_multinom_control_choice_p

# conservative figure
split_control_choice_conserv_barplot = ggplot(D[D$model_type=="split",], aes(control_choice_conserv, fill = control_choice_conserv)) 
split_control_choice_conserv_barplot + geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  xlab("Controlled Relational Order Test Simulation: Split Model") +
  ylab("Number of Choice") + 
  coord_cartesian(ylim=c(0, 20)) +
  scale_fill_manual(values=c("black","#ffc857", "darkblue", "#9933FF")) +
  theme_bw() +
  theme(legend.position = "none")

### strong inverse Model ###
# test_choice
glm.fit.soc_wi = glm(test_choice[D$model_type=="strong_inverse"]~1, 
                     data = D, 
                     family = "binomial")
summary(glm.fit.soc_wi)
#estimate = coefficient -- log odds - so if you exponentiate it, you'll get real odds
exp(-1.3863)

# Figure
strongInverse_test_choice_barplot = ggplot(D[D$model_type=="strong_inverse",], aes(test_choice, fill = test_choice)) 
strongInverse_test_choice_barplot + 
  geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 30)) +
  scale_fill_manual(values=c("black","#ffc857")) +
  theme_bw()

# direction liberal
glm.fit.dir_wi = glm(direction_choice_lib[D$model_type=="strong_inverse"]~1, 
                     data = D, family = "binomial")
summary(glm.fit.dir_wi)
exp(-25.57)

# liberal figure
strongInverse_direction_choice_liberal_barplot = ggplot(D[D$model_type=="strong_inverse",], aes(direction_choice_lib, fill = direction_choice_lib)) 
strongInverse_direction_choice_liberal_barplot + geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 30)) +
  scale_fill_manual(values=c("black","#ffc857")) +
  theme_bw()

## conservative analyses ##
strongInverse_multinom_direction_choice = multinom(direction_choice_conserv[D$model_type=="strong_inverse"] ~ 1, data = D)
summary(strongInverse_multinom_direction_choice)

strongInverse_multinom_direction_choice_z <- summary(strongInverse_multinom_direction_choice)$coefficients/summary(strongInverse_multinom_direction_choice)$standard.errors
strongInverse_multinom_direction_choice_z
strongInverse_multinom_direction_choice_p <- (1 - pnorm(abs(strongInverse_multinom_direction_choice_z), 0, 1)) * 2
strongInverse_multinom_direction_choice_p

# conservative figure
direction_choice_conserv_barplot = ggplot(D[D$model_type=="strong_inverse",], aes(direction_choice_conserv, fill = direction_choice_conserv)) 
direction_choice_conserv_barplot + geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 20)) +
  scale_fill_manual(values=c("black","#ffc857", "darkblue", "#9933FF")) +
  theme_bw()

# control
glm.fit.cont_wi = glm(control_choice_lib[D$model_type=="strong_inverse"]~1,
                      data = D, 
                      family = "binomial")
summary(glm.fit.cont_wi)
exp(-25.57)

# liberal figure
strongInverse_control_choice_liberal_barplot = ggplot(D[D$model_type=="strong_inverse",], aes(control_choice_lib, fill = control_choice_lib)) 
strongInverse_control_choice_liberal_barplot + geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 30)) +
  scale_fill_manual(values=c("black","#ffc857")) +
  theme_bw()

## conservative analyses ##
strongInverse_multinom_control_choice = multinom(control_choice_conserv[D$model_type=="strong_inverse"] ~ 1, data = D)
summary(strongInverse_multinom_control_choice)

strongInverse_multinom_control_choice_z <- summary(strongInverse_multinom_control_choice)$coefficients/summary(strongInverse_multinom_control_choice)$standard.errors
strongInverse_multinom_control_choice_z
strongInverse_multinom_control_choice_p <- (1 - pnorm(abs(strongInverse_multinom_control_choice_z), 0, 1)) * 2
strongInverse_multinom_control_choice_p

# conservative figure
strongInverse_control_choice_conserv_barplot = ggplot(D[D$model_type=="strong_inverse",], aes(control_choice_conserv, fill = control_choice_conserv)) 
strongInverse_control_choice_conserv_barplot + geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 20)) +
  scale_fill_manual(values=c("black","#ffc857", "darkblue", "#9933FF")) +
  theme_bw()

## Follow-up t-tests ##

## assess whether networks were more likely to provide higher (or even lower) ##
## ratings for the correct and incorrect objects during the test trial ##

##### no inverse #####

# correct activitys
mean_test_activity_correct_NI = mean(D$test_activity_correct[D$model_type == "no_inverse"])
mean_test_activity_correct_NI
sd_test_activity_correct_NI = sd(D$test_activity_correct[D$model_type == "no_inverse"])
sd_test_activity_correct_NI

# incorrect activitys
mean_test_activity_incorrect_NI = mean(D$test_activity_incorrect[D$model_type == "no_inverse"])
mean_test_activity_incorrect_NI
sd_test_activity_incorrect_NI = sd(D$test_activity_incorrect[D$model_type == "no_inverse"])
sd_test_activity_incorrect_NI

t_test_test_trial_activity_NI = t.test(D$test_activity_correct[D$model_type == "no_inverse"],
                                   D$test_activity_incorrect[D$model_type == "no_inverse"], paired = TRUE,
                                   alternative = "two.sided")
t_test_test_trial_activity_NI

# Examine whether there is an effect of whether participants chose the SOC object during
# the replication trial (i.e., D$test_choice) on their ratings of the correct (i.e., D$test_rating_correct)
# & incorrect objects.
aov_test_choice_correct_NI = aov(test_activity_correct[D$model_type == "no_inverse"]~test_choice[D$model_type == "no_inverse"],
                              data = D)
summary(aov_test_choice_correct_NI)

# activity of correct object for those who chose correctly
mean(D$test_activity_correct[D$test_choice=="Correct"])
sd(D$test_activity_correct[D$test_choice=="Correct"])

# activity of correct object for those who chose incorrectly
mean(D$test_activity_correct[D$test_choice=="Incorrect"])
sd(D$test_activity_correct[D$test_choice=="Incorrect"])

# Figure
test_activity_correct_barplot = ggplot(D, aes(test_choice, test_activity_correct, fill = test_choice)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
test_activity_correct_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("activity") + # change the label of the y-axis
  xlab("SOC Test Trial Object activities of Correct Responses") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 1)) +
  scale_fill_manual(values=c("black","#ffc857")) +
  theme_bw() +
  theme(legend.position = "none")


aov_test_choice_incorrect = aov(test_activity_incorrect~test_choice,
                                data = D)
summary(aov_test_choice_incorrect)

# activity of incorrect object for those who chose correctly
mean(D$test_activity_incorrect[D$test_choice=="Correct"])
sd(D$test_activity_incorrect[D$test_choice=="Correct"])

# activity of incorrect object for those who chose incorrectly
mean(D$test_activity_incorrect[D$test_choice=="Incorrect"])
sd(D$test_activity_incorrect[D$test_choice=="Incorrect"])


# Figure
test_activity_incorrect_barplot = ggplot(D, aes(test_choice, test_activity_incorrect, fill = test_choice)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
test_activity_incorrect_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("activity") + # change the label of the y-axis
  xlab("SOC Test Trial Object activities of Incorrect Responses") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 1)) +
  scale_fill_manual(values=c("black","#ffc857")) +
  theme_bw() +
  theme(legend.position = "none")


# Examine whether there is an effect of whether participants chose the direction-consistent object during
# the direction trial (i.e., D$direction_choice_lib or D$direction_choice_conserv) for both 
# the liberal and conservative codings
# on their ratings of the direction consistent and inverse objects (D$direction_rating_SOCconsistent or 
# D$direction_rating_inverse)
# & incorrect objects.


table(D$direction_choice_lib)
aov_direction_choice_lib_correct = aov(D$direction_rating_SOCconsistent~D$direction_choice_lib,
                                       data = D)
summary(aov_direction_choice_lib_correct)

# CONCLUSION: Participants who chose the order consistent object were not more confident in their
# ratings of the direction-consistent test object than those who responded with some other pattern.


table(D$direction_choice_conserv)
aov_direction_choice_conserv_correct = aov(D$direction_rating_SOCconsistent~D$direction_choice_conserv,
                                           data = D)
summary(aov_direction_choice_conserv_correct)

# both
mean(D$direction_rating_SOCconsistent[D$direction_choice_conserv=="Both"])
sd(D$direction_rating_SOCconsistent[D$direction_choice_conserv=="Both"])

# order consistent
mean(D$direction_rating_SOCconsistent[D$direction_choice_conserv=="Order Consistent"])
sd(D$direction_rating_SOCconsistent[D$direction_choice_conserv=="Order Consistent"])

# inverse 
mean(D$direction_rating_SOCconsistent[D$direction_choice_conserv=="Inverse"])
sd(D$direction_rating_SOCconsistent[D$direction_choice_conserv=="Inverse"])

# neither 
mean(D$direction_rating_SOCconsistent[D$direction_choice_conserv=="Neither"])
sd(D$direction_rating_SOCconsistent[D$direction_choice_conserv=="Neither"])

# Figure
direction_rating_SOCconsistent_barplot = ggplot(D, aes(direction_choice_conserv,direction_rating_SOCconsistent, fill = direction_choice_conserv)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
direction_rating_SOCconsistent_barplot + 
  stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Rating") + # change the label of the y-axis
  xlab("Order Test Trial Object Ratings for Order Consistent Responses") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 1)) +
  scale_fill_manual(values=c("black","#ffc857", "dark blue", "purple")) +
  theme_bw() +
  theme(legend.position = "none")

# follow-up t-tests
both_direction_choice_correct = D$direction_rating_SOCconsistent[D$direction_choice_conserv=="Both"]
order_consistent_direction_choice_correct = D$direction_rating_SOCconsistent[D$direction_choice_conserv=="Order Consistent"]
neither_direction_choice_correct = D$direction_rating_SOCconsistent[D$direction_choice_conserv=="Neither"]

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded by choosing the order-consistent test object.
both_vs_order_consistent_t_test = t.test(both_direction_choice_correct,
                                         order_consistent_direction_choice_correct,
                                         alternative = "two.sided") 
both_vs_order_consistent_t_test

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded "neither.
both_vs_neither_t_test = t.test(both_direction_choice_correct,
                                neither_direction_choice_correct,
                                alternative = "two.sided")
both_vs_neither_t_test


# compare the confidence ratings for the order consistent test object between those who responded by choosing the 
# directionally consistent object and those
# who responded "neither.
direction_consistent_vs_neither_t_test = t.test(order_consistent_direction_choice_correct,
                                                neither_direction_choice_correct,
                                                alternative = "two.sided")
direction_consistent_vs_neither_t_test


# CONCLUSION: Those who chose both objects or the order consistent one were more confident in their ratings than those who chose
# the inverse object or neither object. We can't run analyses on those who chose the inverse because so few of them (N = 1) responded 
# this way.



# liberal coding
table(D$direction_choice_lib)
aov_direction_choice_lib_inverse = aov(D$direction_rating_inverse~D$direction_choice_lib,
                                       data = D)
summary(aov_direction_choice_lib_inverse)

mean(D$direction_rating_inverse[D$direction_choice_lib=="Other"])
sd(D$direction_rating_inverse[D$direction_choice_lib=="Other"])

mean(D$direction_rating_inverse[D$direction_choice_lib=="Order Consistent"])
sd(D$direction_rating_inverse[D$direction_choice_lib=="Order Consistent"])

# CONCLUSION: Participants who chose the order consistent object were much less confident in their
# ratings of the inverse object test than those who chose the inverse object.


table(D$direction_choice_conserv)
aov_direction_choice_conserv_inverse = aov(D$direction_rating_inverse~D$direction_choice_conserv,
                                           data = D)
summary(aov_direction_choice_conserv_inverse)

# both
mean(D$direction_rating_inverse[D$direction_choice_conserv=="Both"])
sd(D$direction_rating_inverse[D$direction_choice_conserv=="Both"])

# order consistent
mean(D$direction_rating_inverse[D$direction_choice_conserv=="Order Consistent"])
sd(D$direction_rating_inverse[D$direction_choice_conserv=="Order Consistent"])

# inverse 
mean(D$direction_rating_inverse[D$direction_choice_conserv=="Inverse"])
sd(D$direction_rating_inverse[D$direction_choice_conserv=="Inverse"])

# neither 
mean(D$direction_rating_inverse[D$direction_choice_conserv=="Neither"])
sd(D$direction_rating_inverse[D$direction_choice_conserv=="Neither"])

# Figure
direction_rating_inverse_barplot = ggplot(D, aes(direction_choice_conserv,direction_rating_inverse, fill = direction_choice_conserv)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
direction_rating_inverse_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Rating") + # change the label of the y-axis
  xlab("Order Test Trial Object Ratings for Order Inconsistent Responses") + 
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 1)) +
  scale_fill_manual(values=c("black","#ffc857", "dark blue", "purple")) +
  theme_bw() +
  theme(legend.position = "none")

# follow-up t-tests
both_direction_choice_inverse = D$direction_rating_inverse[D$direction_choice_conserv=="Both"]
order_consistent_direction_choice_inverse = D$direction_rating_inverse[D$direction_choice_conserv=="Order Consistent"]
neither_direction_choice_inverse = D$direction_rating_inverse[D$direction_choice_conserv=="Neither"]

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded by choosing the order-consistent test object.
both_vs_order_consistent_inverse_t_test = t.test(both_direction_choice_inverse,
                                                 order_consistent_direction_choice_inverse,
                                                 alternative = "two.sided") 
both_vs_order_consistent_inverse_t_test

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded "neither.
both_vs_neither_inverse_t_test = t.test(both_direction_choice_inverse,
                                        neither_direction_choice_inverse,
                                        alternative = "two.sided")
both_vs_neither_inverse_t_test


# compare the confidence ratings for the order consistent test object between those who responded by choosing the 
# directionally consistent object and those
# who responded "neither.
direction_consistent_vs_neither_inverse_t_test = t.test(order_consistent_direction_choice_inverse,
                                                        neither_direction_choice_correct,
                                                        alternative = "two.sided")
direction_consistent_vs_neither_inverse_t_test


# CONCLUSION: Those who chose both objects  were more confident in their ratings of the inverse object than those who
# chose the order consistent object than neither object.
# the inverse object or neither object. We can't run analyses on those who chose the inverse because so few of them (N = 1) responded 
# this way.



# Examine whether there is an effect of whether participants chose the direction-consistent object during
# the control trial for both the liberal and conservative codings
# on their ratings of the control_ordered and inverse object.
table(D$control_choice_lib)
aov_control_choice_lib_correct = aov(D$control_rating_ordered~D$control_choice_lib,
                                     data = D)
summary(aov_control_choice_lib_correct)

mean(D$control_rating_ordered[D$control_choice_lib=="Order Consistent"])
sd(D$control_rating_ordered[D$control_choice_lib=="Order Consistent"])
mean(D$control_rating_ordered[D$control_choice_lib=="Other"])
sd(D$control_rating_ordered[D$control_choice_lib=="Other"])

# CONCLUSION: Participants who chose the order consistent object were more confident in their
# ratings of the direction-consistent test object than those who responded with some other pattern.


table(D$control_choice_conserv)
aov_control_choice_conserv_correct = aov(D$control_rating_ordered~D$control_choice_conserv,
                                         data = D)
summary(aov_control_choice_conserv_correct)

# both
mean(D$control_rating_ordered[D$control_choice_conserv=="Both"])
sd(D$control_rating_ordered[D$control_choice_conserv=="Both"])

# order consistent
mean(D$control_rating_ordered[D$control_choice_conserv=="Order Consistent"])
sd(D$control_rating_ordered[D$control_choice_conserv=="Order Consistent"])

# inverse 
mean(D$control_rating_ordered[D$control_choice_conserv=="Inverse"])
sd(D$control_rating_ordered[D$control_choice_conserv=="Inverse"])

# neither 
mean(D$control_rating_ordered[D$control_choice_conserv=="Neither"])
sd(D$control_rating_ordered[D$control_choice_conserv=="Neither"])

# Figure
control_rating_ordered_barplot = ggplot(D, aes(control_choice_conserv,control_rating_ordered, fill = control_choice_conserv)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
control_rating_ordered_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Rating") + # change the label of the y-axis
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 1)) +
  scale_fill_manual(values=c("black","#ffc857", "dark blue", "purple")) +
  theme_bw() +
  theme(legend.position = "none")

# follow-up t-tests
both_control_choice_correct = D$control_rating_ordered[D$control_choice_conserv=="Both"]
order_control_direction_choice_correct = D$control_rating_ordered[D$control_choice_conserv=="Order Consistent"]
neither_control_choice_correct = D$control_rating_ordered[D$control_choice_conserv=="Neither"]

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded by choosing the order-consistent test object.
control_both_vs_order_consistent_t_test = t.test(both_control_choice_correct,
                                                 order_control_direction_choice_correct,
                                                 alternative = "two.sided") 
control_both_vs_order_consistent_t_test

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded "neither.
control_both_vs_neither_t_test = t.test(both_control_choice_correct,
                                        neither_control_choice_correct,
                                        alternative = "two.sided")
control_both_vs_neither_t_test


# compare the confidence ratings for the order consistent test object between those who responded by choosing the 
# directionally consistent object and those
# who responded "neither.
control_direction_consistent_vs_neither_t_test = t.test(order_control_direction_choice_correct,
                                                        neither_control_choice_correct,
                                                        alternative = "two.sided")
control_direction_consistent_vs_neither_t_test


# CONCLUSION: Those who chose both objects or the order consistent one were more confident in their ratings of the order
# consistent test object than those who chose neither object. 



# liberal coding
table(D$control_choice_lib)
aov_control_choice_lib_inverse = aov(D$control_rating_inverse~D$control_choice_lib,
                                     data = D)
summary(aov_control_choice_lib_inverse)

mean(D$control_rating_inverse[D$control_choice_lib=="Other"])
sd(D$control_rating_inverse[D$control_choice_lib=="Other"])

mean(D$control_rating_inverse[D$control_choice_lib=="Order Consistent"])
sd(D$control_rating_inverse[D$control_choice_lib=="Order Consistent"])

# CONCLUSION: Participants who chose the order consistent object were much less confident in their
# ratings of the inverse object test than those who chose the inverse object. Makes sense.


table(D$control_choice_conserv)
aov_control_choice_conserv_inverse = aov(D$control_rating_inverse~D$control_choice_conserv,
                                         data = D)
summary(aov_control_choice_conserv_inverse)

# both
mean(D$control_rating_inverse[D$control_choice_conserv=="Both"])
sd(D$control_rating_inverse[D$control_choice_conserv=="Both"])

# order consistent
mean(D$control_rating_inverse[D$control_choice_conserv=="Order Consistent"])
sd(D$control_rating_inverse[D$control_choice_conserv=="Order Consistent"])

# inverse 
mean(D$control_rating_inverse[D$control_choice_conserv=="Inverse"])
sd(D$control_rating_inverse[D$control_choice_conserv=="Inverse"])

# neither 
mean(D$control_rating_inverse[D$control_choice_conserv=="Neither"])
sd(D$control_rating_inverse[D$control_choice_conserv=="Neither"])

# Figure
control_rating_inverse_barplot = ggplot(D, aes(control_choice_conserv,control_rating_inverse, fill = control_choice_conserv)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
control_rating_inverse_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Rating") + # change the label of the y-axis
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 1)) +
  scale_fill_manual(values=c("black","#ffc857", "dark blue", "purple")) +
  theme_bw()+
  theme(legend.position = "none")

# follow-up t-tests
both_control_choice_inverse = D$control_rating_inverse[D$control_choice_conserv=="Both"]
order_consistent_control_choice_inverse = D$control_rating_inverse[D$control_choice_conserv=="Order Consistent"]
neither_control_choice_inverse = D$control_rating_inverse[D$control_choice_conserv=="Neither"]

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded by choosing the order-consistent test object.
control_both_vs_order_consistent_inverse_t_test = t.test(both_control_choice_inverse,
                                                         order_consistent_control_choice_inverse,
                                                         alternative = "two.sided") 
control_both_vs_order_consistent_inverse_t_test

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded "neither.
control_both_vs_neither_inverse_t_test = t.test(both_control_choice_inverse,
                                                neither_control_choice_inverse,
                                                alternative = "two.sided")
control_both_vs_neither_inverse_t_test


# CONCLUSION: Those who chose both objects  were more confident in their ratings of the inverse object than those who
# chose the order consistent object or those who chose neither object.
