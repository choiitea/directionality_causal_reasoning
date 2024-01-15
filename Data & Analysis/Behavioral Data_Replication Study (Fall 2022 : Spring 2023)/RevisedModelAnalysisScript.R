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
    D$test_choice[i] = 1 # soc-consistent
  }else if(D$test_activity_correct[i]>0.5 && D$test_activity_incorrect[i]>0.5){
    D$test_choice[i] = 2 # both
  }else if(D$test_activity_correct[i]<0.5 && D$test_activity_incorrect[i]>0.5){
    D$test_choice[i] = 3 # soc-inconsistent only
  }
  else if(D$test_activity_correct[i]<0.5 && D$test_activity_incorrect[i]<0.5){
    D$test_choice[i] = 4 # neither
  }
}
table(D$test_choice)
D$test_choice = revalue(x = as.factor(D$test_choice), c("4" = "Neither", "1"="SOC Consistent"))
# 0 - incorrect; 1 - correct

table(D$model_type, D$test_choice)

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

##################
### no_inverse ###
##################
D_NI <- D[D$model_type == "no_inverse", ]
#test_choice
# do networks prefer the SOC-consistent test object? 23 of 32
D_NI$SOC_SOCchoices <- ifelse(D_NI$test_choice %in% c("Both", "SOC Consistent"), 1, 0)
D_NI$SOC_SOCchoices = as.factor(D_NI$SOC_SOCchoices)
D_NI$SOC_SOCchoices = relevel(D_NI$SOC_SOCchoices, ref="0")

glm_NI_test_SOCchoice = glm(SOC_SOCchoices~1, 
                      data = D_NI, 
                      family = "binomial")
summary(glm_NI_test_SOCchoice)
exp(glm_NI_test_SOCchoice$coefficients)
exp(confint(glm_NI_test_SOCchoice))

# of the networks that choose the SOC-consistent test object, do they reliably reject the SOC-inconsistent test object?
# 23/23 reject the SOC-inconsistent

# do networks choose the SOC-inconsistent test object?
# no. 0/32

# NI networks registered the SOC from training. 

# direction choice
# do networks choose the SOC-consistent test object? 
# BOTH + SOC-consistent / ALL = 23 of 32
D_NI$dir_SOCchoices <- ifelse(D_NI$direction_choice_conserv %in% c("Both", "Order Consistent"), 1, 0)
D_NI$dir_SOCchoices = as.factor(D_NI$dir_SOCchoices)
D_NI$dir_SOCchoices = relevel(D_NI$dir_SOCchoices, ref="0")

glm_NI_dir_SOCchoice = glm(dir_SOCchoices~1,
                           data = D_NI,
                           family = "binomial")
summary(glm_NI_dir_SOCchoice)
exp(glm_NI_dir_SOCchoice$coefficients)
exp(confint(glm_NI_dir_SOCchoice))

# of the networks that choose the SOC-consistent test object, how many also accept the Order-Inconsistent test object?
# 0 of 32 - they all reliably reject the order-inconsistent test object -- indicating registering the SOC AND the relational order
D_NI$dir_BOTH <- ifelse(D_NI$direction_choice_conserv %in% c("Both"), 1, 0)
D_NI$dir_BOTH = as.factor(D_NI$dir_BOTH)
D_NI$dir_BOTH = relevel(D_NI$dir_BOTH, ref="0")

glm_NI_dir_SOC_BOTH = glm(dir_BOTH~1,
                          data = D_NI[D_NI$direction_choice_conserv == "Both" | D_NI$direction_choice_conserv == "Order Consistent",],
                          family = "binomial")
summary(glm_NI_dir_SOC_BOTH)
exp(glm_NI_dir_SOC_BOTH$coefficients)
exp(confint(glm_NI_dir_SOC_BOTH))

# do networks choose the order-inconsistent test object?
D_NI$dir_INCONSchoices <- ifelse(D_NI$direction_choice_conserv %in% c("Both", "Order Inconsistent"), 1, 0)
D_NI$dir_INCONSchoices = as.factor(D_NI$dir_INCONSchoices)
D_NI$dir_INCONSchoices = relevel(D_NI$dir_INCONSchoices, ref = "0")

glm_NI_dir_INCONSchoices = glm(dir_INCONSchoices~1,
                               data = D_NI,
                               family = "binomial")
summary(glm_NI_dir_INCONSchoices)
exp(glm_NI_dir_INCONSchoices$coefficients)
exp(confint(glm_NI_dir_INCONSchoices))

D_NI$dir_NEITHER <- ifelse(D_NI$direction_choice_conserv %in% c("Neither"), 1, 0)
D_NI$dir_NEITHER = as.factor(D_NI$dir_NEITHER)
D_NI$dir_NEITHER = relevel(D_NI$dir_NEITHER, ref = "0")

glm_NI_dir_NEITHER = glm(dir_NEITHER~1,
                         data = D_NI,
                         family = "binomial")
summary(glm_NI_dir_NEITHER)
exp(glm_NI_dir_NEITHER$coefficients)
exp(confint(glm_NI_dir_NEITHER))

# control choice 
# do networks choose the order-consistent test object? 
# BOTH + order-consistent / ALL = 25 of 32
D_NI$con_SOCchoices <- ifelse(D_NI$control_choice_conserv %in% c("Both", "Order Consistent"), 1, 0)
D_NI$con_SOCchoices = as.factor(D_NI$con_SOCchoices)
D_NI$con_SOCchoices = relevel(D_NI$con_SOCchoices, ref="0")

glm_NI_con_SOCchoice = glm(con_SOCchoices~1,
                           data = D_NI,
                           family = "binomial")
summary(glm_NI_con_SOCchoice)
exp(glm_NI_con_SOCchoice$coefficients)
exp(confint(glm_NI_con_SOCchoice))

# of the networks choosing the order-consistent test object, how many also accept the order-inconsistent? 
# 0 of 25

# do networks choose the order-inconsistent test object?
# 0 of 32

D_NI$con_NEITHER = ifelse(D_NI$control_choice_conserv %in% c("Neither"), 1, 0)
D_NI$con_NEITHER = as.factor(D_NI$con_NEITHER)
D_NI$con_NEITHER = relevel(D_NI$con_NEITHER, ref="0")

glm_NI_con_NEITHER = glm(con_NEITHER~1,
                         data = D_NI,
                         family = "binomial")
summary(glm_NI_con_NEITHER)
exp(glm_NI_con_NEITHER$coefficients)
exp(confint(glm_NI_con_NEITHER))

#####################
####### split #######
#####################
D_Split <- D[D$model_type == "split", ]
#test_choice
# do networks prefer the SOC-consistent test object? 32 of 32
D_Split$SOC_SOCchoices <- ifelse(D_Split$test_choice %in% c("Both", "SOC Consistent"), 1, 0)
D_Split$SOC_SOCchoices = as.factor(D_Split$SOC_SOCchoices)
D_Split$SOC_SOCchoices = relevel(D_Split$SOC_SOCchoices, ref="0")

glm_Split_test_SOCchoice = glm(SOC_SOCchoices~1, 
                            data = D_Split, 
                            family = "binomial")
summary(glm_Split_test_SOCchoice)
exp(glm_Split_test_SOCchoice$coefficients)
exp(confint(glm_Split_test_SOCchoice))

# of the networks that choose the SOC-consistent test object, do they reliably reject the SOC-inconsistent test object?
# 32/32 reject the SOC-inconsistent

# NI networks registered the SOC from training. 

# direction choice
# do networks choose the SOC-consistent test object? 
# BOTH + SOC-consistent / ALL = 32 of 32
D_Split$dir_SOCchoices <- ifelse(D_Split$direction_choice_conserv %in% c("Both", "Order Consistent"), 1, 0)
D_Split$dir_SOCchoices = as.factor(D_Split$dir_SOCchoices)
D_Split$dir_SOCchoices = relevel(D_Split$dir_SOCchoices, ref="0")

glm_Split_dir_SOCchoice = glm(dir_SOCchoices~1,
                           data = D_Split,
                           family = "binomial")
summary(glm_Split_dir_SOCchoice)
exp(glm_Split_dir_SOCchoice$coefficients)
exp(confint(glm_Split_dir_SOCchoice))

# of the networks that choose the SOC-consistent test object, how many also accept the Order-Inconsistent test object?
# 23 of 32
D_Split$dir_BOTH <- ifelse(D_Split$direction_choice_conserv %in% c("Both"), 1, 0)
D_Split$dir_BOTH = as.factor(D_Split$dir_BOTH)
D_Split$dir_BOTH = relevel(D_Split$dir_BOTH, ref="0")

glm_Split_dir_SOC_BOTH = glm(dir_BOTH~1,
                          data = D_Split[D_Split$direction_choice_conserv == "Both" | D_Split$direction_choice_conserv == "Order Consistent",],
                          family = "binomial")
summary(glm_Split_dir_SOC_BOTH)
exp(glm_Split_dir_SOC_BOTH$coefficients)
exp(confint(glm_Split_dir_SOC_BOTH))

# do networks choose the order-inconsistent test object? 23 of 32
D_Split$dir_INCONSchoices <- ifelse(D_Split$direction_choice_conserv %in% c("Both", "Order Inconsistent"), 1, 0)
D_Split$dir_INCONSchoices = as.factor(D_Split$dir_INCONSchoices)
D_Split$dir_INCONSchoices = relevel(D_Split$dir_INCONSchoices, ref = "0")

glm_Split_dir_INCONSchoices = glm(dir_INCONSchoices~1,
                               data = D_Split,
                               family = "binomial")
summary(glm_Split_dir_INCONSchoices)
exp(glm_Split_dir_INCONSchoices$coefficients)
exp(confint(glm_Split_dir_INCONSchoices))

# do networks that choose the order-inconsistent test object reject the order-consistent? 0 of 32

# control choice
# do networks choose the order-consistent test object? 
# BOTH + order-consistent / ALL = 32 of 32
D_Split$con_SOCchoices <- ifelse(D_Split$control_choice_conserv %in% c("Both", "Order Consistent"), 1, 0)
D_Split$con_SOCchoices = as.factor(D_Split$con_SOCchoices)
D_Split$con_SOCchoices = relevel(D_Split$con_SOCchoices, ref="0")

glm_Split_con_SOCchoice = glm(con_SOCchoices~1,
                           data = D_Split,
                           family = "binomial")
summary(glm_Split_con_SOCchoice)
exp(glm_Split_con_SOCchoice$coefficients)
exp(confint(glm_Split_con_SOCchoice))

# of the networks choosing the order-consistent test object, how many also accept the order-inconsistent? 
# 22 of 32

D_Split$con_INCON <- ifelse(D_Split$control_choice_conserv %in% c("Both", "Order Inconsistent"), 1, 0)
D_Split$con_INCON = as.factor(D_Split$con_INCON)
D_Split$con_INCON = relevel(D_Split$con_INCON, ref="0")

glm_Split_con_INCON = glm(con_INCON~1,
                              data = D_Split,
                              family = "binomial")
summary(glm_Split_con_INCON)
exp(glm_Split_con_INCON$coefficients)
exp(confint(glm_Split_con_INCON))

D_Split$dir_BOTH <- ifelse(D_Split$direction_choice_conserv %in% c("Both"), 1, 0)
D_Split$dir_BOTH = as.factor(D_Split$dir_BOTH)
D_Split$dir_BOTH = relevel(D_Split$dir_BOTH, ref="0")

glm_Split_dir_SOC_BOTH = glm(dir_BOTH~1,
                             data = D_Split[D_Split$direction_choice_conserv == "Both" | D_Split$direction_choice_conserv == "Order Consistent",],
                             family = "binomial")
summary(glm_Split_dir_SOC_BOTH)
exp(glm_Split_dir_SOC_BOTH$coefficients)
exp(confint(glm_Split_dir_SOC_BOTH))

# do networks choose only the order-inconsistent test object?
# 0 of 32
