########################################################
########################################################
########################################################
#############                              #############
#############     ADULT ANALYSIS SCRIPT    #############
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
library(plyr)
library(ggsignif)
library(lsr)
library(sjmisc)
library(sjstats)
library(BayesFactor)
library(foreign)
library(dplyr)
library(lattice)
library(openxlsx)
library(sjPlot)
library(pwr)
library(nnet)
options(scipen=9999)

######################
# data preprocessing #
######################
## load data ##
D = read.csv(file.choose(), header = TRUE)

## check to see if there're any "NAs" ##
is.na(D)

## get the structure of the data ##
str(D)

## get the names of the columns ##
names(D)

## demographic summaries
table(D$gender)
mean(D$age)
min(D$age)
max(D$age)

## create a test_choice variable ##
D$test_choice = rep(0,nrow(D))
for(i in 1:nrow(D)){
  if(D$correct_soc_choice[i]==D$test_soc_choice[i]){
    D$test_choice[i] = "Correct"
  }else{
    D$test_choice[i] = "Incorrect"
  }
}

## get the distribution of correct and incorrect choices ##
table(D$test_choice)

## create a direction_choice_conserv variable ##
D$direction_choice_conserv = rep(0, nrow(D))
for(i in 1:nrow(D)){
  if(D$test_dir_choice[i]==D$ordered_dir_choice[i]){
    D$direction_choice_conserv[i] = 1 #soc-consistent / order-consistent
  }else if(D$test_dir_choice[i]=="both"){
    D$direction_choice_conserv[i] = 0
  }else if(D$test_dir_choice[i]=="left"&&D$ordered_dir_choice[i]=="right"){
    D$direction_choice_conserv[i] = 2 #chose inverse but not soc_con
  }else if(D$test_dir_choice[i]=="right"&&D$ordered_dir_choice[i]=="left"){
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
table(D$direction_choice_conserv)

## create a control_choice_conserv variable ##
D$control_choice_conserv = rep(0, nrow(D))
for(i in 1:nrow(D)){
  if(D$ordered_contrDir_choice[i]==D$test_contrDir_choice[i]){
    D$control_choice_conserv[i] = 1 #order-consistent 
  }else if(D$test_contrDir_choice[i]=="both"){
    D$control_choice_conserv[i] = 0
  }else if(D$test_contrDir_choice[i]=="left"&&D$ordered_contrDir_choice[i]=="right"){
    D$control_choice_conserv[i] = 2 #chose inverse but not order_con
  }else if(D$test_contrDir_choice[i]=="right"&&D$ordered_contrDir_choice[i]=="left"){
    D$control_choice_conserv[i] = 2 #chose inverse but not order_con
  }else{
    D$control_choice_conserv[i] = 3 #neither
  }
}
# 0 - both; 1 - order_consistent; 2 - inverse but not order_consistent; 3 - neither



# name the levels of direction_choice_conserv
D$control_choice_conserv = revalue(x = as.factor(D$control_choice_conserv), 
                                   c("0" = "Both", "1" = "Order Consistent", 
                                     "2"="Order Inconsistent",
                                     "3"= "Neither"))

D$control_choice_conserv <- factor(D$control_choice_conserv, levels = c("Both", "Order Consistent",
                                                                        "Order Inconsistent", "Neither"))

# get distributional data
table(D$control_choice_conserv)

# remove unnecessary columns
D = D[,-c(1,10:15)]
D$ID = c(1:nrow(D))
D$row.names = NULL

D$class_year = as.factor(D$class_year)
D$gender = as.factor(D$gender)
D$static_con = as.factor(D$static_con)
D$causal_con = as.factor(D$causal_con)
D$soc_con = as.factor(D$soc_con)
D$dir_con = as.factor(D$dir_con)
D$contrDir_con = as.factor(D$contrDir_con)
D$memory_check = as.factor(D$memory_check)
D$test_choice = as.factor(D$test_choice)
D$direction_choice = as.factor(D$direction_choice)
D$control_choice = as.factor(D$control_choice)

#################
# main analysis #
#################
# test_choice FULL analysis

D$test_choice = relevel(D$test_choice, ref="Incorrect")
glm_test_choice_full = glm(test_choice~static_con+causal_con+soc_con+dir_con+contrDir_con, 
                           data = D, 
                           family = "binomial")
summary(glm_test_choice_full)
# This analysis tell us that there is no effect of our counterbalancing conditions,
# which is a *great* thing.

# first, do participants tend to pick the SOC-consistent object?
table(D$test_choice)
glm_test_choice = glm(test_choice~1, 
                      data = D, 
                      family = "binomial")
summary(glm_test_choice)
exp(glm_test_choice$coefficients)
exp(confint(glm_test_choice))
#estimate = coefficient -- log odds - so if you exponentiate it, you'll get real odds

# of the participants who pick the SOC-consistent, how many ONLY pick the SOC-consistent? 
glm_test_choice_correctonly = glm(test_choice~1, 
                      data = D[D$test_choice == "Correct", ], 
                      family = "binomial")
summary(glm_test_choice_correctonly)
exp(glm_test_choice_correctonly$coefficients)
exp(confint(glm_test_choice_correctonly))

# Figure
test_choice_barplot = ggplot(D, aes(test_choice, fill = test_choice)) 
test_choice_barplot + geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 30)) +
  scale_fill_manual(values=c("black","#ffc857")) +
  xlab("Second-Order Correlation Test Choice") +
  ylab("Number of Responses") +
  theme_bw() +
  theme(legend.position = "none")

# direction_choice FULL analysis
table(D$direction_choice_conserv)
# how many participants are choosing the SOC-consistent test object in Trial 2?
# 20 (SOC-consistent + Both) of 32
D$dir_SOCchoice <- ifelse(D$direction_choice_conserv %in% c("Both", "Order Consistent"), 1, 0)
D$dir_SOCchoice = as.factor(D$dir_SOCchoice)
D$dir_SOCchoice = relevel(D$dir_SOCchoice, ref="0")

glm_dir_SOCchoice = glm(dir_SOCchoice~1,
                        data = D,
                        family = "binomial")
summary(glm_dir_SOCchoice)
exp(glm_dir_SOCchoice$coefficients)
exp(confint(glm_dir_SOCchoice))

# of these 20 participants, how many ALSO accept the INVERSE test object?
D$dir_BOTHchoice <- ifelse(D$direction_choice_conserv %in% c("Both"), 1, 0)
D$dir_BOTHchoice = as.factor(D$dir_BOTHchoice)
D$dir_BOTHchoice = relevel(D$dir_BOTHchoice, ref="0")

glm_dir_SOC_AND_INVERSEchoice = glm(dir_BOTHchoice~1,
                                    data = D[D$direction_choice_conserv=="Both" | D$direction_choice_conserv=="Order Consistent",],
                                    family = "binomial")
summary(glm_dir_SOC_AND_INVERSEchoice)
exp(glm_dir_SOC_AND_INVERSEchoice$coefficients)
exp(confint(glm_dir_SOC_AND_INVERSEchoice))

# this gives us evidence that participants are in fact registering the SOC but still choosing the inverse object
# but it remains, why they are choosing the inverse object? is it due to the SOC in the inverse or due to the FOC 
# that is confounded with the inverse? 

# how many accept the inverse?
D$dir_INCONchoice <- ifelse(D$direction_choice_conserv %in% c("Both", "Order Inconsistent"), 1, 0)
D$dir_INCONchoice = as.factor(D$dir_INCONchoice)
D$dir_INCONchoice = relevel(D$dir_INCONchoice, ref="0")

glm_dir_INCONchoice = glm(dir_INCONchoice~1,
                        data = D,
                        family = "binomial")
summary(glm_dir_INCONchoice)
exp(glm_dir_INCONchoice$coefficients)
exp(confint(glm_dir_INCONchoice))

glm_dir_INVERSE_AND_BOTHchoice = glm(dir_BOTHchoice~1,
                                     data=D[D$direction_choice_conserv=="Both" | D$direction_choice_conserv=="Inverse",],
                                     family = "binomial")
summary(glm_dir_INVERSE_AND_BOTHchoice)
exp(glm_dir_INVERSE_AND_BOTHchoice$coefficients)
exp(confint(glm_dir_INVERSE_AND_BOTHchoice))

# this provides evidence that choosing BOTH is a response based on the learnt SOC rather than on the basis of the FOC
# that is confounded with the inverse object

# however, Trial 2 itself does not provide evidence that this is due to insensitivity to relational order

# how many participants are choosing the order-consistent test object? 19 of 32
table(D$control_choice_conserv)
D$contr_CONSISTENTchoice <- ifelse(D$control_choice_conserv %in% c("Both", "Order Consistent"), 1, 0)
D$contr_CONSISTENTchoice = as.factor(D$contr_CONSISTENTchoice)
D$contr_CONSISTENTchoice = relevel(D$contr_CONSISTENTchoice, ref="0")

glm_contr_CONSISTENTchoice <- glm(contr_CONSISTENTchoice~1,
                                  data=D,
                                  family = "binomial")
summary(glm_contr_CONSISTENTchoice)
exp(glm_contr_CONSISTENTchoice$coefficients)
exp(confint(glm_contr_CONSISTENTchoice))

# of these 19 who choose the order-consistent test object, how many ONLY accept the order-consistent? 4 of 19
D$contr_onlyCONSISTENTchoice <- ifelse(D$control_choice_conserv %in% c("Order Consistent"), 1, 0)
D$contr_onlyCONSISTENTchoice = as.factor(D$contr_onlyCONSISTENTchoice)
D$contr_onlyCONSISTENTchoice = relevel(D$contr_onlyCONSISTENTchoice, ref="0")

glm_contr_onlyCONSISTENTchoice <- glm(contr_onlyCONSISTENTchoice~1,
                                      data=D[D$control_choice_conserv=="Both" | D$control_choice_conserv=="Order Consistent", ],
                                      family = "binomial")
summary(glm_contr_onlyCONSISTENTchoice)
exp(glm_contr_onlyCONSISTENTchoice$coefficients)
exp(confint(glm_contr_onlyCONSISTENTchoice))

# of these 19 who choose the order-consistent test object, how many ONLY accept the order-consistent? 15 of 19
D$contr_BOTHCONSISTENT <- ifelse(D$control_choice_conserv %in% c("Both"), 1, 0)
D$contr_BOTHCONSISTENT = as.factor(D$contr_BOTHCONSISTENT)
D$contr_BOTHCONSISTENT = relevel(D$contr_BOTHCONSISTENT, ref="0")

glm_contr_BOTHCONSISTENT <- glm(contr_BOTHCONSISTENT~1,
                                      data=D[D$control_choice_conserv=="Both" | D$control_choice_conserv=="Order Consistent", ],
                                      family = "binomial")
summary(glm_contr_BOTHCONSISTENT)
exp(glm_contr_BOTHCONSISTENT$coefficients)
exp(confint(glm_contr_BOTHCONSISTENT))

# this shows that for those who have registered SOC, it is significantly less likely for learners to ONLY choose the 
# order-consistent test object than choose BOTH test objects

D$con_INCONchoice <- ifelse(D$control_choice_conserv %in% c("Both", "Order Inconsistent"), 1, 0)
D$con_INCONchoice = as.factor(D$con_INCONchoice)
D$con_INCONchoice = relevel(D$con_INCONchoice, ref="0")

glm_con_INCONchoice = glm(con_INCONchoice~1,
                          data = D,
                          family = "binomial")
summary(glm_con_INCONchoice)
exp(glm_con_INCONchoice$coefficients)
exp(confint(glm_con_INCONchoice))

# additionally, we need to test if there are those who choose only the order-inconsistent test object conditional on making an 
# order-inconsistent choice. 0 of 15 
D$contr_onlyINCONSISTENTchoice <- ifelse(D$control_choice_conserv %in% c("Order Inconsistent"), 1, 0)
D$contr_onlyINCONSISTENTchoice = as.factor(D$contr_onlyINCONSISTENTchoice)
D$contr_onlyINCONSISTENTchoice = relevel(D$contr_onlyINCONSISTENTchoice, ref="0")

glm_contr_onlyINCONSISTENTchoice <- glm(contr_onlyINCONSISTENTchoice~1,
                                        data=D[D$control_choice_conserv=="Both" | D$control_choice_conserv=="Order Inconsistent", ],
                                        family = "binomial")
summary(glm_contr_onlyINCONSISTENTchoice)
exp(glm_contr_onlyINCONSISTENTchoice$coefficients)
exp(confint(glm_contr_onlyINCONSISTENTchoice))

# this provides evidence that an order-inconsistent choice is a function of learning the SOC but being INSENSITIVE to relational order
