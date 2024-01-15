####################################################################
####################################################################
####################################################################
#############                                          #############
#############     Replication Study ANALYSIS SCRIPT    #############
#############                                          #############
####################################################################
####################################################################
####################################################################
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
D = read.csv(file.choose(), header = TRUE, na.strings=c(""," ","NA"))

## check to see if there're any "NAs" ##
is.na(D)

## exclude participants who did not complete the tasks
# 1. exclude those who did not complete training or engage in the full range of test trials
D <- D[!is.na(D$static_con) & !is.na(D$causal_con) & !is.na(D$soc_con) & !is.na(D$dir_con) & !is.na(D$contrDir_con), ]

## get the structure of the data ##
str(D)

## get the names of the columns ##
names(D)

## demographic summaries
table(D$gender)
mean(D$age)
min(D$age)
max(D$age)

## create a test_choice variable -- before intuit
D$test_choice = rep(0,nrow(D))
for(i in 1:nrow(D)){
  if(D$correct_soc_choice[i]==D$test_soc_choice[i]){
    D$test_choice[i] = "Correct"
  }else if (D$test_soc_choice[i] == "unsure"){
    D$test_choice[i] = "Unsure"
  }else{
    D$test_choice[i] = "Incorrect"
  }
}

## get the distribution of choices before intuit
table(D$test_choice)

## test_choice_conserv -- before intuit
D$test_choice_conserv = rep(0,nrow(D))
for(i in 1:nrow(D)){
  if(D$correct_soc_choice[i]==D$test_soc_choice[i]){
    D$test_choice_conserv[i] = "SOC-consistent"
  }else if(D$test_soc_choice[i]=="both"){
    D$test_choice_conserv[i] = "Both"
  }else if(D$test_soc_choice[i]=="neither"){
    D$test_choice_conserv[i] = "Neither"
  }else if(D$test_soc_choice[i]=="left"&&D$correct_soc_choice[i]=="right"){
    D$test_choice_conserv[i] = "SOC-inconsistent"
  }else if(D$test_soc_choice[i]=="right"&&D$correct_soc_choice[i]=="left"){
    D$test_choice_conserv[i] = "SOC-inconsistent"
  }
  else{
    D$test_choice_conserv[i] = "Unsure"
  }
}
table(D$test_choice_conserv)
## create a final_test_choice variable -- after intuit
D$final_test_choice = rep(0,nrow(D))
for(i in 1:nrow(D)){
  if(D$correct_soc_choice[i]==D$final_soc_choice[i]){
    D$final_test_choice[i] = "Correct"
  }else{
    D$final_test_choice[i] = "Incorrect"
  }
}

## get the distribution of correct and incorrect after intuit
table(D$final_test_choice)

## create a final_test_choice_conserv
D$final_test_choice_conserv = rep(0,nrow(D))
for(i in 1:nrow(D)){
  if(D$correct_soc_choice[i]==D$final_soc_choice[i]){
    D$final_test_choice_conserv[i] = "SOC-consistent"
  }else if(D$final_soc_choice[i]=="both"){
    D$final_test_choice_conserv[i] = "Both"
  }else if(D$final_soc_choice[i]=="neither"){
    D$final_test_choice_conserv[i] = "Neither"
  }else if(D$final_soc_choice[i]=="left"&&D$correct_soc_choice[i]=="right"){
    D$final_test_choice_conserv[i] = "SOC-inconsistent"
  }else if(D$final_soc_choice[i]=="right"&&D$correct_soc_choice[i]=="left"){
    D$final_test_choice_conserv[i] = "SOC-inconsistent"
  }
  else{
    D$final_test_choice_conserv[i] = "Neither"
  }
}
table(D$final_test_choice_conserv)

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
  }else if(D$test_dir_choice[i]=="unsure"){
    D$direction_choice_conserv[i] = 4 #unsure response
  }else{
    D$direction_choice_conserv[i] = 3 #neither
  }
}
# 0 - both; 1 - SOC_consistent; 2 - inverse but not soc_con; 3 - neither

# name the levels of direction_choice_conserv
D$direction_choice_conserv = revalue(x = as.factor(D$direction_choice_conserv), 
                                     c("0" = "Both", "1"="Order Consistent",
                                       "2" = "Inverse", "3"= "Neither", "4" = "Unsure"))

# get the distribution of choices for the conservative coding of
# direction
table(D$direction_choice_conserv)

## create a final_direction_choice_conserv vairable
D$final_direction_choice_conserv = rep(0, nrow(D))
for(i in 1:nrow(D)){
  if(D$final_dir_choice[i]==D$ordered_dir_choice[i]){
    D$final_direction_choice_conserv[i] = 1 #soc-consistent / order-consistent
  }else if(D$final_dir_choice[i]=="both"){
    D$final_direction_choice_conserv[i] = 0
  }else if(D$final_dir_choice[i]=="left"&&D$ordered_dir_choice[i]=="right"){
    D$final_direction_choice_conserv[i] = 2 #chose inverse but not soc_con
  }else if(D$final_dir_choice[i]=="right"&&D$ordered_dir_choice[i]=="left"){
    D$final_direction_choice_conserv[i] = 2 #chose inverse but not soc_con
  }else if(D$final_dir_choice[i]=="unsure"){
    D$final_direction_choice_conserv[i] = 4 #unsure response
  }else{
    D$final_direction_choice_conserv[i] = 3 #neither
  }
}

D$final_direction_choice_conserv = revalue(x = as.factor(D$final_direction_choice_conserv), 
                                     c("0" = "Both", "1"="Order Consistent",
                                       "2" = "Inverse", "3"= "Neither", "4" = "Unsure"))

table(D$final_direction_choice_conserv)

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
  }else if(D$test_contrDir_choice[i]=="unsure"){
    D$control_choice_conserv[i] = 4 #unsure
  }else{
    D$control_choice_conserv[i] = 3 #neither
  }
}
# 0 - both; 1 - order_consistent; 2 - inverse but not order_consistent; 3 - neither; 4 - unsure

# name the levels of direction_choice_conserv
D$control_choice_conserv = revalue(x = as.factor(D$control_choice_conserv), 
                                   c("0" = "Both", "1" = "Order Consistent", 
                                     "2"="Order Inconsistent",
                                     "3"= "Neither",
                                     "4" = "Unsure"))

D$control_choice_conserv <- factor(D$control_choice_conserv, levels = c("Both", "Order Consistent",
                                                                        "Order Inconsistent", "Neither", "Unsure"))

# get distributional data
table(D$control_choice_conserv)

## create a final_control_choice_conserv variable ##
D$final_control_choice_conserv = rep(0, nrow(D))
for(i in 1:nrow(D)){
  if(D$ordered_contrDir_choice[i]==D$final_contrDir_choice[i]){
    D$final_control_choice_conserv[i] = 1 #order-consistent 
  }else if(D$final_contrDir_choice[i]=="both"){
    D$final_control_choice_conserv[i] = 0
  }else if(D$final_contrDir_choice[i]=="left"&&D$ordered_contrDir_choice[i]=="right"){
    D$final_control_choice_conserv[i] = 2 #chose inverse but not order_con
  }else if(D$final_contrDir_choice[i]=="right"&&D$ordered_contrDir_choice[i]=="left"){
    D$final_control_choice_conserv[i] = 2 #chose inverse but not order_con
  }else if(D$final_contrDir_choice[i]=="unsure"){
    D$final_control_choice_conserv[i] = 4 #unsure
  }else{
    D$final_control_choice_conserv[i] = 3 #neither
  }
}
# 0 - both; 1 - order_consistent; 2 - inverse but not order_consistent; 3 - neither; 4 - unsure

# name the levels of direction_choice_conserv
D$final_control_choice_conserv = revalue(x = as.factor(D$final_control_choice_conserv), 
                                         c("0" = "Both", "1" = "Order Consistent", 
                                           "2"="Order Inconsistent",
                                           "3"= "Neither"))

D$final_control_choice_conserv <- factor(D$final_control_choice_conserv, levels = c("Both", "Order Consistent",
                                                                                    "Order Inconsistent", "Neither"))

# get distributional data
table(D$final_control_choice_conserv)

# convert variables that need to be factors into factors
D$class_year = as.factor(D$class_year)
D$gender = as.factor(D$gender)
D$static_con = as.factor(D$static_con)
D$causal_con = as.factor(D$causal_con)
D$soc_con = as.factor(D$soc_con)
D$dir_con = as.factor(D$dir_con)
D$contrDir_con = as.factor(D$contrDir_con)
D$memory_check = as.factor(D$memory_check)
D$test_choice = as.factor(D$test_choice)
D$final_test_choice = as.factor(D$final_test_choice)
D$direction_choice_conserv = as.factor(D$direction_choice_conserv)
D$final_direction_choice_conserv = as.factor(D$final_direction_choice_conserv)
D$control_choice_conserv = as.factor(D$control_choice_conserv)
D$final_control_choice_conserv = as.factor(D$final_control_choice_conserv)

# check for failed memory check
table(D$memory_check)
D <- D[!(D$memory_check=="2"),]

# Create a logical vector indicating whether each row contains a null value in intuit columns
no_intuit_vector <- is.na(D$intuit_soc_choice) & is.na(D$intuit_dir_choice) & is.na(D$intuit_contrDir_choice)

## D_intuit - participants who were reprompted to intuit at least once
D_HASintuit <- D[!no_intuit_vector, ]

# # remove unnecessary columns
# D = D[,-c(1,10:21)]
# D$ID = c(1:nrow(D))
# D$row.names = NULL
# 
# D_HASintuit = D_HASintuit[,-c(1,10:21)]
# D_HASintuit$ID = c(1:nrow(D_HASintuit))
# D_HASintuit$row.names = NULL
# 
# D_NOintuit = D_NOintuit[,-c(1,10:21)]
# D_NOintuit$ID = c(1:nrow(D_NOintuit))
# D_NOintuit$row.names = NULL

D$final_test_choice = relevel(D$final_test_choice, ref="Incorrect")
glm_final_test_choice_full = glm(final_test_choice~static_con+causal_con+soc_con+dir_con+contrDir_con, 
                                 data = D, 
                                 family = "binomial")
summary(glm_final_test_choice_full)
# tells us there is no effect of our counterbalancing conditions

table(D$final_test_choice_conserv)
# how many participants are choosing the SOC-consistent test object in Trial 1?
# SOC-consistent + Both / ALL = 19/31
# Ex 4 - 22 / 31
D$SOC_SOCchoice <- ifelse(D$final_test_choice_conserv %in% c("Both", "SOC-consistent"), 1, 0)
D$SOC_SOCchoice = as.factor(D$SOC_SOCchoice)
D$SOC_SOCchoice = relevel(D$SOC_SOCchoice, ref="0")

glm_SOC_SOCchoice = glm(SOC_SOCchoice~1,
                        data = D,
                        family = "binomial")
summary(glm_SOC_SOCchoice)
exp(glm_SOC_SOCchoice$coefficients)
exp(confint(glm_SOC_SOCchoice))

# of these 19 participants, how many reliably reject the SOC-inconsistent test object?
# SOC-consistent ONLY / SOC-consistent + BOTH = 14/19
# Ex 4 - 19 / 22
D$SOC_SOConly <- ifelse(D$final_test_choice_conserv %in% c("SOC-consistent"), 1, 0)
D$SOC_SOConly = as.factor(D$SOC_SOConly)
D$SOC_SOConly = relevel(D$SOC_SOConly, ref="0")

glm_SOC_SOConly = glm(SOC_SOConly~1,
                      data = D[D$final_test_choice_conserv=="Both" | D$final_test_choice_conserv=="SOC-consistent",],
                      family="binomial")
summary(glm_SOC_SOConly)
exp(glm_SOC_SOConly$coefficients)
exp(confint(glm_SOC_SOConly))

# how many participants chose the SOC-inconsistent test object in Trial 1?
# SOC-inconsitent + both / all = 10/31
# Ex 4 - 5 / 31
D$SOC_Inconsistent <- ifelse(D$final_test_choice_conserv %in% c("Both", "SOC-inconsistent"), 1, 0)
D$SOC_Inconsistent = as.factor(D$SOC_Inconsistent)
D$SOC_Inconsistent = relevel(D$SOC_Inconsistent, ref="0")

glm_SOC_inconsistent = glm(SOC_Inconsistent~1,
                           data = D,
                           family = "binomial")
summary(glm_SOC_inconsistent)
exp(glm_SOC_inconsistent$coefficients)
exp(confint(glm_SOC_inconsistent))

# given a SOC-inconsistent choice, how many also chose SOC-consistent
# 5/10
# ex 4 - 3 / 5
D$SOC_both <- ifelse(D$final_test_choice_conserv %in% c("Both"), 1, 0)
D$SOC_both <- as.factor(D$SOC_both)
D$SOC_both <- relevel(D$SOC_both, ref="0")

glm_SOC_inconsistent_both = glm(SOC_both~1,
                                data = D[D$final_test_choice_conserv=="Both" | D$final_test_choice_conserv=="SOC-inconsistent", ],
                                family = "binomial")
summary(glm_SOC_inconsistent_both)
exp(glm_SOC_inconsistent_both$coefficients)
exp(confint(glm_SOC_inconsistent_both))

# direction_choice FULL analysis
table(D$final_direction_choice_conserv)
# how many participants are choosing the SOC-consistent test object in Trial 2?
# 13 (SOC-consistent + Both) of 31
# Ex4 - 14 of 31
D$dir_SOCchoice <- ifelse(D$final_direction_choice_conserv %in% c("Both", "Order Consistent"), 1, 0)
D$dir_SOCchoice = as.factor(D$dir_SOCchoice)
D$dir_SOCchoice = relevel(D$dir_SOCchoice, ref="0")

glm_dir_SOCchoice = glm(dir_SOCchoice~1,
                        data = D,
                        family = "binomial")
summary(glm_dir_SOCchoice)
exp(glm_dir_SOCchoice$coefficients)
exp(confint(glm_dir_SOCchoice))

# of these 13 participants, how many reliably reject the inverse test object? 7 of 13
# ex 4 - 4 / 14
D$dir_SOConly <- ifelse(D$final_direction_choice_conserv %in% c("Order Consistent"), 1, 0)
D$dir_SOConly = as.factor(D$dir_SOConly)
D$dir_SOConly = relevel(D$dir_SOConly, ref="0")

glm_dir_SOC_AND_INVERSEchoice = glm(dir_SOConly~1,
                                    data = D[D$final_direction_choice_conserv=="Both" | D$final_direction_choice_conserv=="Order Consistent",],
                                    family = "binomial")
summary(glm_dir_SOC_AND_INVERSEchoice)
exp(glm_dir_SOC_AND_INVERSEchoice$coefficients)
exp(confint(glm_dir_SOC_AND_INVERSEchoice))

# this suggests that given an SOC-consistent choice in T2, adults are at chance in rejecting the inverse test object

# how many participants are accepting the inverse test object? 
# Inverse + Both / ALL = 20 of 31
# exp 4 - 23 / 31
D$dir_INCONSISTENTchoice <- ifelse(D$final_direction_choice_conserv %in% c("Both", "Inverse"), 1, 0)
D$dir_INCONSISTENTchoice = as.factor(D$dir_INCONSISTENTchoice)
D$dir_INCONSISTENTchoice = relevel(D$dir_INCONSISTENTchoice, ref="0")

glm_dir_INCONSISTENTchoice = glm(dir_INCONSISTENTchoice~1,
                                 data = D,
                                 family = "binomial")
summary(glm_dir_INCONSISTENTchoice)
exp(glm_dir_INCONSISTENTchoice$coefficients)
exp(confint(glm_dir_INCONSISTENTchoice))

# of the 20 participants who accepted the inverse test object, how many are also accepting the SOC-consistent test object?
# 6 of 20
# ex 4 - 10/23
D$dir_INCONSISTENT_SOCalso <- ifelse(D$final_direction_choice_conserv %in% c("Both"), 1, 0)
D$dir_INCONSISTENT_SOCalso = as.factor(D$dir_INCONSISTENT_SOCalso)
D$dir_INCONSISTENT_SOCalso = relevel(D$dir_INCONSISTENT_SOCalso, ref = "0")

glm_dir_INCONSISTENT_SOCalso = glm(dir_INCONSISTENT_SOCalso~1,
                                   data=D[D$final_direction_choice_conserv=="Both" | D$final_direction_choice_conserv=="Inverse",],
                                   family = "binomial")
summary(glm_dir_INCONSISTENT_SOCalso)
exp(glm_dir_INCONSISTENT_SOCalso$coefficients)
exp(confint(glm_dir_INCONSISTENT_SOCalso))

# this provides evidence that choosing BOTH is a response based on the FOC that is confounded with the inverse object
# rather than the learnt SOC

# how many participants are choosing the order-consistent test object in Trial 3? 
# Order-Consistent + Both / ALL = 14 of 31
# ex4 - 18 / 31
table(D$final_control_choice_conserv)

D$contr_CONSISTENTchoice <- ifelse(D$final_control_choice_conserv %in% c("Both", "Order Consistent"), 1, 0)
D$contr_CONSISTENTchoice = as.factor(D$contr_CONSISTENTchoice)
D$contr_CONSISTENTchoice = relevel(D$contr_CONSISTENTchoice, ref="0")

glm_contr_CONSISTENTchoice <- glm(contr_CONSISTENTchoice~1,
                                  data=D,
                                  family = "binomial")
summary(glm_contr_CONSISTENTchoice)
exp(glm_contr_CONSISTENTchoice$coefficients)
exp(confint(glm_contr_CONSISTENTchoice))

# of these 14 who choose the order-consistent test object, how many also accept the order-inconsistent? 6 of 14
# ex 4 - 11 of 18
D$contr_BOTH <- ifelse(D$final_control_choice_conserv %in% c("Both"), 1, 0)
D$contr_BOTH = as.factor(D$contr_BOTH)
D$contr_BOTH = relevel(D$contr_BOTH, ref="0")

glm_contr_BOTH <- glm(contr_BOTH~1,
                      data=D[D$final_control_choice_conserv=="Both" | D$final_control_choice_conserv=="Order Consistent", ],
                      family = "binomial")
summary(glm_contr_BOTH)
exp(glm_contr_BOTH$coefficients)
exp(confint(glm_contr_BOTH))

# this shows that for those who have registered SOC, participants were at chance in accepting the order-inconsistent test object

# additionally, we need to test participants who are choosing the order-inconsistent test object in Trial 3
# Order-Inconsistent + BOTH / ALL = 10/31
# ex4 - 14 of 31
D$contr_INCONSISTENTchoice <- ifelse(D$final_control_choice_conserv %in% c("Both", "Order Inconsistent"), 1, 0)
D$contr_INCONSISTENTchoice = as.factor(D$contr_INCONSISTENTchoice)
D$contr_INCONSISTENTchoice = relevel(D$contr_INCONSISTENTchoice, ref="0")

glm_contr_INCONSISTENTchoice <- glm(contr_INCONSISTENTchoice~1,
                                    data=D,
                                    family = "binomial")
summary(glm_contr_INCONSISTENTchoice)
exp(glm_contr_INCONSISTENTchoice$coefficients)
exp(confint(glm_contr_INCONSISTENTchoice))

# participants were not likely to indicate the order-inconsistent test object as a blicket

# test if those who do choose the order-inconsistent test object, also accept the order-consistent test object
# BOTH / BOTH + Order-Inconsistent = 6 of 10
# ex4 - 11 of 14
D$contr_INCONSISTENT_both <- ifelse(D$final_control_choice_conserv %in% c("Both"), 1, 0)
D$contr_INCONSISTENT_both = as.factor(D$contr_INCONSISTENT_both)
D$contr_INCONSISTENT_both = relevel(D$contr_INCONSISTENT_both, ref="0")

glm_contr_INCONSISTENT_BOTH <- glm(contr_INCONSISTENT_both~1,
                                   data=D[D$final_control_choice_conserv=="Both" | D$final_control_choice_conserv=="Order Inconsistent", ],
                                   family = "binomial")
summary(glm_contr_INCONSISTENT_BOTH)
exp(glm_contr_INCONSISTENT_BOTH$coefficients)
exp(confint(glm_contr_INCONSISTENT_BOTH))

# participants are at chance in accepting the order-consistent test object given choosing an order-inconsistent test object

######### REPROMPTED PARTICIPANTS - 20 REPROMPTED participants total
### test_choice -- 9 people were reprompted / ex4 - 10
table(D_HASintuit[!is.na(D_HASintuit$intuit_soc_choice)]$test_choice)
D_SOC_intuit <- D_HASintuit[!is.na(D_HASintuit$intuit_soc_choice), ]
table(D_SOC_intuit$final_test_choice_conserv)
# how many participants are choosing the SOC-consistent test object in Trial 1?
# SOC-consistent + Both / ALL = 5 of 10
D_SOC_intuit$SOC_SOCchoice <- ifelse(D_SOC_intuit$final_test_choice_conserv %in% c("Both", "SOC-consistent"), 1, 0)
D_SOC_intuit$SOC_SOCchoice = as.factor(D_SOC_intuit$SOC_SOCchoice)
D_SOC_intuit$SOC_SOCchoice = relevel(D_SOC_intuit$SOC_SOCchoice, ref="0")

glm_intuitSOC_SOCchoice = glm(SOC_SOCchoice~1,
                              data = D_SOC_intuit,
                              family = "binomial")
summary(glm_intuitSOC_SOCchoice)
exp(glm_intuitSOC_SOCchoice$coefficients)
exp(confint(glm_intuitSOC_SOCchoice))

# of these 6 participants, how many reliably reject the SOC-inconsistent test object?
# SOC-consistent ONLY / SOC-consistent + BOTH = 5 of 5
D_SOC_intuit$SOC_SOConly <- ifelse(D_SOC_intuit$final_test_choice_conserv %in% c("SOC-consistent"), 1, 0)
D_SOC_intuit$SOC_SOConly = as.factor(D_SOC_intuit$SOC_SOConly)
D_SOC_intuit$SOC_SOConly = relevel(D_SOC_intuit$SOC_SOConly, ref="0")

glm_intuitSOC_SOConly = glm(SOC_SOConly~1,
                            data = D_SOC_intuit[D_SOC_intuit$final_test_choice_conserv=="Both" | D_SOC_intuit$final_test_choice_conserv=="SOC-consistent",],
                            family="binomial")
summary(glm_intuitSOC_SOConly)
exp(glm_intuitSOC_SOConly$coefficients)
exp(confint(glm_intuitSOC_SOConly))

# how many participants chose the SOC-inconsistent test object in Trial 1?
# SOC-inconsistent + Both / ALL = 2 / 9
# ex4 - 2 of 10
D_SOC_intuit$SOC_inconsistent <- ifelse(D_SOC_intuit$final_test_choice_conserv %in% c("Both", "SOC-inconsistent"), 1, 0)
D_SOC_intuit$SOC_inconsistent = as.factor(D_SOC_intuit$SOC_inconsistent)
D_SOC_intuit$SOC_inconsistent = relevel(D_SOC_intuit$SOC_inconsistent, ref="0")

glm_intuitSOC_inconsistent = glm(SOC_inconsistent~1,
                                 data = D_SOC_intuit,
                                 family = "binomial")
summary(glm_intuitSOC_inconsistent)
exp(glm_intuitSOC_inconsistent$coefficients)
exp(confint(glm_intuitSOC_inconsistent))

# of those who choose the SOC-inconsistent test object, how many also accept the SOC-consistent?
# BOTH / BOTH + SOC-inconsistent = 1/2
# ex 4 - 0 of 2
D_SOC_intuit$SOC_BOTH <- ifelse(D_SOC_intuit$final_test_choice_conserv %in% c("Both"), 1, 0)
D_SOC_intuit$SOC_BOTH = as.factor(D_SOC_intuit$SOC_BOTH)
D_SOC_intuit$SOC_BOTH = relevel(D_SOC_intuit$SOC_BOTH, ref="0")

glm_intuitSOC_inconsistentBOTH = glm(SOC_BOTH~1,
                                     data = D_SOC_intuit[D_SOC_intuit$final_test_choice_conserv=="Both" | D_SOC_intuit$final_test_choice_conserv=="SOC-inconsistent",],
                                     family = "binomial")
summary(glm_intuitSOC_inconsistentBOTH)
exp(glm_intuitSOC_inconsistentBOTH$coefficients)
exp(confint(glm_intuitSOC_inconsistentBOTH))

### direction choice -- 8 re-prompted
D_dir_intuit <- D_HASintuit[!is.na(D_HASintuit$intuit_dir_choice), ]
table(D_dir_intuit$final_direction_choice_conserv)

# of these 8 participants, how many are choosing the SOC-consistent test object in Trial 2?
# 4 of 8
# ex4 - 4 / 12
D_dir_intuit$dir_SOCchoice <- ifelse(D_dir_intuit$final_direction_choice_conserv %in% c("Both", "Order Consistent"), 1, 0)
D_dir_intuit$dir_SOCchoice = as.factor(D_dir_intuit$dir_SOCchoice)
D_dir_intuit$dir_SOCchoice = relevel(D_dir_intuit$dir_SOCchoice, ref="0")

glm_intuitDIR_SOCchoice = glm(dir_SOCchoice~1,
                              data = D_dir_intuit,
                              family = "binomial")
summary(glm_intuitDIR_SOCchoice)
exp(glm_intuitDIR_SOCchoice$coefficients)
exp(confint(glm_intuitDIR_SOCchoice))

# participants are at chance in choosing the SOC-consistent test object
# of these 4, do participants reliably reject the inverse test object?
# 3 of 4
# ex4 - 1 of 4
D_dir_intuit$dir_SOConly <- ifelse(D_dir_intuit$final_direction_choice_conserv %in% c("Order Consistent"), 1, 0)
D_dir_intuit$dir_SOConly = as.factor(D_dir_intuit$dir_SOConly)
D_dir_intuit$dir_SOConly = relevel(D_dir_intuit$dir_SOConly, ref="0")

glm_intuitDIR_SOConly = glm(dir_SOConly~1,
                            data = D_dir_intuit[D_dir_intuit$final_direction_choice_conserv=="Both" | D_dir_intuit$final_direction_choice_conserv=="Order Consistent", ],
                            family = "binomial")
summary(glm_intuitDIR_SOConly)
exp(glm_intuitDIR_SOConly$coefficients)
exp(confint(glm_intuitDIR_SOConly))

# how many are accepting the inverse test object? 5 of 8
# ex4 - 10 of 12
D_dir_intuit$dir_INVERSEchoice <- ifelse(D_dir_intuit$final_direction_choice_conserv %in% c("Inverse", "Both"), 1, 0)
D_dir_intuit$dir_INVERSEchoice = as.factor(D_dir_intuit$dir_INVERSEchoice)
D_dir_intuit$dir_INVERSEchoice = relevel(D_dir_intuit$dir_INVERSEchoice, ref="0")

glm_intuitDIR_INVERSEchoice = glm(dir_INVERSEchoice~1,
                                  data = D_dir_intuit,
                                  family="binomial")
summary(glm_intuitDIR_INVERSEchoice)
exp(glm_intuitDIR_INVERSEchoice$coefficients)
exp(confint(glm_intuitDIR_INVERSEchoice))

# of the 5 who are accepting the inverse test object, how many also accept the SOC-consistent test object?
# 1 of 5
#ex 4 - 3 of 10
D_dir_intuit$dir_INVERSE_Both <- ifelse(D_dir_intuit$final_direction_choice_conserv %in% c("Both"), 1, 0)
D_dir_intuit$dir_INVERSE_Both = as.factor(D_dir_intuit$dir_INVERSE_Both)
D_dir_intuit$dir_INVERSE_Both = relevel(D_dir_intuit$dir_INVERSE_Both, ref="0")

glm_intuitDIR_Inverse_Both <- glm(dir_INVERSE_Both~1,
                                  data = D_dir_intuit[D_dir_intuit$final_direction_choice_conserv=="Inverse" | D_dir_intuit$final_direction_choice_conserv == "Both", ],
                                  family = "binomial")
summary(glm_intuitDIR_Inverse_Both)
exp(glm_intuitDIR_Inverse_Both$coefficients)
exp(confint(glm_intuitDIR_Inverse_Both))

### control choice -- 11 re-prompted
D_con_intuit <- D_HASintuit[!is.na(D_HASintuit$intuit_contrDir_choice), ]
table(D_con_intuit$final_control_choice_conserv)

# of those re-prompted, how many participants choose the order-consistent test object?
# order-consistent + both / all = 5 of 11
# 1 of 6
D_con_intuit$con_ORDEREDchoice <- ifelse(D_con_intuit$final_control_choice_conserv %in% c("Order Consistent","Both"), 1, 0)
D_con_intuit$con_ORDEREDchoice = as.factor(D_con_intuit$con_ORDEREDchoice)
D_con_intuit$con_ORDEREDchoice = relevel(D_con_intuit$con_ORDEREDchoice, ref="0")

glm_intuitCON_ORDEREDchoice = glm(con_ORDEREDchoice~1,
                                  data = D_con_intuit,
                                  family = "binomial")
summary(glm_intuitCON_ORDEREDchoice)
exp(glm_intuitCON_ORDEREDchoice$coefficients)
exp(confint(glm_intuitCON_ORDEREDchoice))

# of the 5 who do choose the order-consistent test object, how many accept the order-inconsistent as well?
# 1 of 5 
# 1 of 1
D_con_intuit$con_ORDERED_BOTH <- ifelse(D_con_intuit$final_control_choice_conserv %in% c("Both"), 1, 0)
D_con_intuit$con_ORDERED_BOTH = as.factor(D_con_intuit$con_ORDERED_BOTH)
D_con_intuit$con_ORDERED_BOTH = relevel(D_con_intuit$con_ORDERED_BOTH, ref="0")

glm_intuitCON_ORDERED_BOTH = glm(con_ORDERED_BOTH~1,
                                 data=D_con_intuit[D_con_intuit$final_control_choice_conserv=="Both" | D_con_intuit$final_control_choice_conserv=="Order Consistent", ],
                                 family="binomial")
summary(glm_intuitCON_ORDERED_BOTH)
exp(glm_intuitCON_ORDERED_BOTH$coefficients)
exp(confint(glm_intuitCON_ORDERED_BOTH))

# of those re-prompted, how many participants chose the order-inconsistent test object?
# order-inconsistent + both / all = 3 / 11
# 3 / 6
D_con_intuit$con_INCONSchoice <- ifelse(D_con_intuit$final_control_choice_conserv %in% c("Order Inconsistent","Both"), 1, 0)
D_con_intuit$con_INCONSchoice = as.factor(D_con_intuit$con_INCONSchoice)
D_con_intuit$con_INCONSchoice = relevel(D_con_intuit$con_INCONSchoice, ref="0")

glm_intuitCON_INCONSchoice = glm(con_INCONSchoice~1,
                                 data = D_con_intuit,
                                 family = "binomial")
summary(glm_intuitCON_INCONSchoice)
exp(glm_intuitCON_INCONSchoice$coefficients)
exp(confint(glm_intuitCON_INCONSchoice))

# of the 3 who chose the order-inconsistent test object, how many also accept the order-consistent test object?
# 1 of 3
# 1 of 3
D_con_intuit$con_INCONS_BOTH <- ifelse(D_con_intuit$final_control_choice_conserv %in% c("Both"), 1, 0)
D_con_intuit$con_INCONS_BOTH = as.factor(D_con_intuit$con_INCONS_BOTH)
D_con_intuit$con_INCONS_BOTH = relevel(D_con_intuit$con_INCONS_BOTH, ref="0")

glm_intuitCON_INCONS_BOTH = glm(con_INCONS_BOTH~1,
                               data = D_con_intuit[D_con_intuit$final_control_choice_conserv=="Both" | D_con_intuit$final_control_choice_conserv=="Order Inconsistent", ],
                               family = "binomial")
summary(glm_intuitCON_INCONS_BOTH)
exp(glm_intuitCON_INCONS_BOTH$coefficients)
exp(confint(glm_intuitCON_INCONS_BOTH))

######### NO REPROMPT PARTICIPANTS

### No Reprompt Trial 1 - 22 total no reprompt
D_SOC_NOintuit <- D[is.na(D$intuit_soc_choice), ]
table(D_SOC_NOintuit$final_test_choice_conserv)

# how many participants are choosing the SOC-consistent test object in Trial 1?
# SOC-consistent + Both / ALL = 13/22
# ex4 - 17 / 21 
D_SOC_NOintuit$SOC_SOCchoice <- ifelse(D_SOC_NOintuit$final_test_choice_conserv %in% c("Both", "SOC-consistent"), 1, 0)
D_SOC_NOintuit$SOC_SOCchoice = as.factor(D_SOC_NOintuit$SOC_SOCchoice)
D_SOC_NOintuit$SOC_SOCchoice = relevel(D_SOC_NOintuit$SOC_SOCchoice, ref="0")

glm_noIntuit_SOC_SOCchoice = glm(SOC_SOCchoice~1,
                                 data = D_SOC_NOintuit,
                                 family = "binomial")
summary(glm_noIntuit_SOC_SOCchoice)
exp(glm_noIntuit_SOC_SOCchoice$coefficients)
exp(confint(glm_noIntuit_SOC_SOCchoice))

# of these 13 participants, how many reliably reject the SOC-inconsistent test object?
# SOC-consistent ONLY / SOC-consistent + BOTH = 9/13
# ex4 - 14 / 17
D_SOC_NOintuit$SOC_SOConly <- ifelse(D_SOC_NOintuit$final_test_choice_conserv %in% c("SOC-consistent"), 1, 0)
D_SOC_NOintuit$SOC_SOConly = as.factor(D_SOC_NOintuit$SOC_SOConly)
D_SOC_NOintuit$SOC_SOConly = relevel(D_SOC_NOintuit$SOC_SOConly, ref="0")

glm_NOintuit_SOC_SOConly = glm(SOC_SOConly~1,
                               data = D_SOC_NOintuit[D_SOC_NOintuit$final_test_choice_conserv=="Both" | D_SOC_NOintuit$final_test_choice_conserv=="SOC-consistent",],
                               family="binomial")
summary(glm_NOintuit_SOC_SOConly)
exp(glm_NOintuit_SOC_SOConly$coefficients)
exp(confint(glm_NOintuit_SOC_SOConly))

# how many participants are choosing the SOC-inconsistent test object in Trial 1?
# SOC-inconsistent + Both / ALL = 8/22
# ex4 - 3 / 21
D_SOC_NOintuit$SOC_inconsistent <- ifelse(D_SOC_NOintuit$final_test_choice_conserv %in% c("SOC-inconsistent", "Both"), 1, 0)
D_SOC_NOintuit$SOC_inconsistent = as.factor(D_SOC_NOintuit$SOC_inconsistent)
D_SOC_NOintuit$SOC_inconsistent = relevel(D_SOC_NOintuit$SOC_inconsistent, ref="0")

glm_NOintuit_SOC_inconsistent = glm(SOC_inconsistent~1,
                                    data = D_SOC_NOintuit,
                                    family = "binomial")
summary(glm_NOintuit_SOC_inconsistent)
exp(glm_NOintuit_SOC_inconsistent$coefficients)
exp(confint(glm_NOintuit_SOC_inconsistent))

# how many participants who choose the SOC-inconsistent test object also choose the soc-consistent test object?
# 4/8
# ex 4 - 3 of 3
D_SOC_NOintuit$SOC_inconsistentBOTH <- ifelse(D_SOC_NOintuit$final_test_choice_conserv %in% c("Both"), 1, 0)
D_SOC_NOintuit$SOC_inconsistentBOTH = as.factor(D_SOC_NOintuit$SOC_inconsistentBOTH)
D_SOC_NOintuit$SOC_inconsistentBOTH = relevel(D_SOC_NOintuit$SOC_inconsistentBOTH , ref="0")

glm_NOintuit_SOC_inconsistent_BOTH = glm(SOC_inconsistentBOTH~1,
                                         data = D_SOC_NOintuit[D_SOC_NOintuit$final_test_choice_conserv=="Both" | D_SOC_NOintuit$final_test_choice_conserv=="SOC-inconsistent",],
                                         family = "binomial")
summary(glm_NOintuit_SOC_inconsistent_BOTH)
exp(glm_NOintuit_SOC_inconsistent_BOTH$coefficients)
exp(confint(glm_NOintuit_SOC_inconsistent_BOTH))

### No Reprompt Trial 2
D_DIR_NOintuit <- D[is.na(D$intuit_dir_choice), ]
table(D_DIR_NOintuit$final_direction_choice_conserv)
# how many are choosing the SOC-consistent test object in Trial 2?
# 9 of 23
# 10 of 19
D_DIR_NOintuit$dir_SOCchoice <- ifelse(D_DIR_NOintuit$final_direction_choice_conserv %in% c("Both", "Order Consistent"), 1, 0)
D_DIR_NOintuit$dir_SOCchoice = as.factor(D_DIR_NOintuit$dir_SOCchoice)
D_DIR_NOintuit$dir_SOCchoice = relevel(D_DIR_NOintuit$dir_SOCchoice, ref="0")

glm_NOintuitDIR_SOCchoice = glm(dir_SOCchoice~1,
                                data = D_DIR_NOintuit,
                                family = "binomial")
summary(glm_NOintuitDIR_SOCchoice)
exp(glm_NOintuitDIR_SOCchoice$coefficients)
exp(confint(glm_NOintuitDIR_SOCchoice))

# participants are at chance in choosing the SOC-consistent test object
# of these 9, do participants reliably reject the inverse test object?
# 4 of 9
# 3 of 10
D_DIR_NOintuit$dir_SOConly <- ifelse(D_DIR_NOintuit$final_direction_choice_conserv %in% c("Order Consistent"), 1, 0)
D_DIR_NOintuit$dir_SOConly = as.factor(D_DIR_NOintuit$dir_SOConly)
D_DIR_NOintuit$dir_SOConly = relevel(D_DIR_NOintuit$dir_SOConly, ref="0")

glm_NOintuitDIR_SOConly = glm(dir_SOConly~1,
                            data = D_DIR_NOintuit[D_DIR_NOintuit$final_direction_choice_conserv=="Both" | D_DIR_NOintuit$final_direction_choice_conserv=="Order Consistent", ],
                            family = "binomial")
summary(glm_NOintuitDIR_SOConly)
exp(glm_NOintuitDIR_SOConly$coefficients)
exp(confint(glm_NOintuitDIR_SOConly))
# they do not significantly reject the inverse. 

# how many are accepting the inverse test object? 15 of 23
# 13 of 19
D_DIR_NOintuit$dir_INVERSEchoice <- ifelse(D_DIR_NOintuit$final_direction_choice_conserv %in% c("Inverse", "Both"), 1, 0)
D_DIR_NOintuit$dir_INVERSEchoice = as.factor(D_DIR_NOintuit$dir_INVERSEchoice)
D_DIR_NOintuit$dir_INVERSEchoice = relevel(D_DIR_NOintuit$dir_INVERSEchoice, ref="0")

glm_NOintuitDIR_INVERSEchoice = glm(dir_INVERSEchoice~1,
                                  data = D_DIR_NOintuit,
                                  family="binomial")
summary(glm_NOintuitDIR_INVERSEchoice)
exp(glm_NOintuitDIR_INVERSEchoice$coefficients)
exp(confint(glm_NOintuitDIR_INVERSEchoice))
# at chance. 
# of the 15 who are accepting the inverse test object, how many also accept the SOC-consistent test object?
# 5 of 15
# 7 of 13
D_DIR_NOintuit$dir_INVERSE_Both <- ifelse(D_DIR_NOintuit$final_direction_choice_conserv %in% c("Both"), 1, 0)
D_DIR_NOintuit$dir_INVERSE_Both = as.factor(D_DIR_NOintuit$dir_INVERSE_Both)
D_DIR_NOintuit$dir_INVERSE_Both = relevel(D_DIR_NOintuit$dir_INVERSE_Both, ref="0")

glm_NOintuitDIR_Inverse_Both <- glm(dir_INVERSE_Both~1,
                                  data = D_DIR_NOintuit[D_DIR_NOintuit$final_direction_choice_conserv=="Inverse" | D_DIR_NOintuit$final_direction_choice_conserv == "Both", ],
                                  family = "binomial")
summary(glm_NOintuitDIR_Inverse_Both)
exp(glm_NOintuitDIR_Inverse_Both$coefficients)
exp(confint(glm_NOintuitDIR_Inverse_Both))

### No Reprompt Trial 3
D_CON_NOintuit <- D[is.na(D$intuit_contrDir_choice), ]
table(D_CON_NOintuit$final_control_choice_conserv) # 20

# how many participants choose the order-consistent test object?
# order-consistent + both / all = 9 of 20
# 17 / 25
D_CON_NOintuit$con_ORDEREDchoice <- ifelse(D_CON_NOintuit$final_control_choice_conserv %in% c("Order Consistent","Both"), 1, 0)
D_CON_NOintuit$con_ORDEREDchoice = as.factor(D_CON_NOintuit$con_ORDEREDchoice)
D_CON_NOintuit$con_ORDEREDchoice = relevel(D_CON_NOintuit$con_ORDEREDchoice, ref="0")

glm_NOintuitCON_ORDEREDchoice = glm(con_ORDEREDchoice~1,
                                  data = D_CON_NOintuit,
                                  family = "binomial")
summary(glm_NOintuitCON_ORDEREDchoice)
exp(glm_NOintuitCON_ORDEREDchoice$coefficients)
exp(confint(glm_NOintuitCON_ORDEREDchoice))
# at chance. 
# of the 9 who do choose the order-consistent test object, how many accept the order-inconsistent as well?
# 5 of 9
# 10 of 17
D_CON_NOintuit$con_ORDERED_BOTH <- ifelse(D_CON_NOintuit$final_control_choice_conserv %in% c("Both"), 1, 0)
D_CON_NOintuit$con_ORDERED_BOTH = as.factor(D_CON_NOintuit$con_ORDERED_BOTH)
D_CON_NOintuit$con_ORDERED_BOTH = relevel(D_CON_NOintuit$con_ORDERED_BOTH, ref="0")

glm_NOintuitCON_ORDERED_BOTH = glm(con_ORDERED_BOTH~1,
                                 data=D_CON_NOintuit[D_CON_NOintuit$final_control_choice_conserv=="Both" | D_CON_NOintuit$final_control_choice_conserv=="Order Consistent", ],
                                 family="binomial")
summary(glm_NOintuitCON_ORDERED_BOTH)
exp(glm_NOintuitCON_ORDERED_BOTH$coefficients)
exp(confint(glm_NOintuitCON_ORDERED_BOTH))

# how many participants chose the order-inconsistent test object?
# order-inconsistent + both / all = 7 / 20
# 11 of 25
D_CON_NOintuit$con_INCONSchoice <- ifelse(D_CON_NOintuit$final_control_choice_conserv %in% c("Order Inconsistent","Both"), 1, 0)
D_CON_NOintuit$con_INCONSchoice = as.factor(D_CON_NOintuit$con_INCONSchoice)
D_CON_NOintuit$con_INCONSchoice = relevel(D_CON_NOintuit$con_INCONSchoice, ref="0")

glm_NOintuitCON_INCONSchoice = glm(con_INCONSchoice~1,
                                 data = D_CON_NOintuit,
                                 family = "binomial")
summary(glm_NOintuitCON_INCONSchoice)
exp(glm_NOintuitCON_INCONSchoice$coefficients)
exp(confint(glm_NOintuitCON_INCONSchoice))
# at chance
# of the 7 who chose the order-inconsistent test object, how many accept the order-consistent test object?
# 5 of 7
# 10 of 11
D_CON_NOintuit$con_INCON_BOTH <- ifelse(D_CON_NOintuit$final_control_choice_conserv %in% c("Both"), 1, 0)
D_CON_NOintuit$con_INCON_BOTH = as.factor(D_CON_NOintuit$con_INCON_BOTH)
D_CON_NOintuit$con_INCON_BOTH = relevel(D_CON_NOintuit$con_INCON_BOTH, ref="0")

glm_NOintuitCON_INCON_BOTH = glm(con_INCON_BOTH~1,
                               data = D_CON_NOintuit[D_CON_NOintuit$final_control_choice_conserv=="Both" | D_CON_NOintuit$final_control_choice_conserv=="Order Inconsistent", ],
                               family = "binomial")
summary(glm_NOintuitCON_INCON_BOTH)
exp(glm_NOintuitCON_INCON_BOTH$coefficients)
exp(confint(glm_NOintuitCON_INCON_BOTH))

######## FOR EXPERIMENT 4 (REPLICATION 2) w/ "What is a blicket object?" open-ended item
D_CorrectBlicketness <- D[D$blicketness=="1",]
D_CorrectBlicketness <- subset(D_CorrectBlicketness, !is.na(D_CorrectBlicketness$blicketness))
D_OtherBlicketness <- D[D$blicketness=="2",]
D_OtherBlicketness <- subset(D_OtherBlicketness, !is.na(D_OtherBlicketness$blicketness))

table(D_CorrectBlicketness$final_test_choice_conserv)
# how many participants are choosing the SOC-consistent test object in Trial 1?
# SOC-consistent + Both / ALL = 10 of 10
D_CorrectBlicketness$SOC_SOCchoice <- ifelse(D_CorrectBlicketness$final_test_choice_conserv %in% c("Both", "SOC-consistent"), 1, 0)
D_CorrectBlicketness$SOC_SOCchoice = as.factor(D_CorrectBlicketness$SOC_SOCchoice)
D_CorrectBlicketness$SOC_SOCchoice = relevel(D_CorrectBlicketness$SOC_SOCchoice, ref="0")

glm_SOC_SOCchoice = glm(SOC_SOCchoice~1,
                        data = D_CorrectBlicketness,
                        family = "binomial")
summary(glm_SOC_SOCchoice)
exp(glm_SOC_SOCchoice$coefficients)
exp(confint(glm_SOC_SOCchoice))

# of these 10 participants, how many reliably reject the SOC-inconsistent test object?
# SOC-consistent ONLY / SOC-consistent + BOTH = 10 of 10
D_CorrectBlicketness$SOC_SOConly <- ifelse(D_CorrectBlicketness$final_test_choice_conserv %in% c("SOC-consistent"), 1, 0)
D_CorrectBlicketness$SOC_SOConly = as.factor(D_CorrectBlicketness$SOC_SOConly)
D_CorrectBlicketness$SOC_SOConly = relevel(D_CorrectBlicketness$SOC_SOConly, ref="0")

glm_SOC_SOConly = glm(SOC_SOConly~1,
                      data = D_CorrectBlicketness[D_CorrectBlicketness$final_test_choice_conserv=="Both" | D_CorrectBlicketness$final_test_choice_conserv=="SOC-consistent",],
                      family="binomial")
summary(glm_SOC_SOConly)
exp(glm_SOC_SOConly$coefficients)
exp(confint(glm_SOC_SOConly))

# how many participants chose the SOC-inconsistent test object in Trial 1?
# SOC-inconsitent + both / all = 0 of 10

# given a SOC-inconsistent choice, how many also chose SOC-consistent
# 0 of 0

table(D_CorrectBlicketness$final_direction_choice_conserv)
# how many participants are choosing the SOC-consistent test object in Trial 2?
# 8 (SOC-consistent + Both) of 10

D_CorrectBlicketness$dir_SOCchoice <- ifelse(D_CorrectBlicketness$final_direction_choice_conserv %in% c("Both", "Order Consistent"), 1, 0)
D_CorrectBlicketness$dir_SOCchoice = as.factor(D_CorrectBlicketness$dir_SOCchoice)
D_CorrectBlicketness$dir_SOCchoice = relevel(D_CorrectBlicketness$dir_SOCchoice, ref="0")

glm_dir_SOCchoice = glm(dir_SOCchoice~1,
                        data = D_CorrectBlicketness,
                        family = "binomial")
summary(glm_dir_SOCchoice)
exp(glm_dir_SOCchoice$coefficients)
exp(confint(glm_dir_SOCchoice))

# of these 13 participants, how many reliably reject the inverse test object? 1 / 8
D_CorrectBlicketness$dir_SOConly <- ifelse(D_CorrectBlicketness$final_direction_choice_conserv %in% c("Order Consistent"), 1, 0)
D_CorrectBlicketness$dir_SOConly = as.factor(D_CorrectBlicketness$dir_SOConly)
D_CorrectBlicketness$dir_SOConly = relevel(D_CorrectBlicketness$dir_SOConly, ref="0")

glm_dir_SOC_AND_INVERSEchoice = glm(dir_SOConly~1,
                                    data = D_CorrectBlicketness[D_CorrectBlicketness$final_direction_choice_conserv=="Both" | D_CorrectBlicketness$final_direction_choice_conserv=="Order Consistent",],
                                    family = "binomial")
summary(glm_dir_SOC_AND_INVERSEchoice)
exp(glm_dir_SOC_AND_INVERSEchoice$coefficients)
exp(confint(glm_dir_SOC_AND_INVERSEchoice))

# how many participants are accepting the inverse test object? 
# Inverse + Both / ALL = 9 of 10
D_CorrectBlicketness$dir_INCONSISTENTchoice <- ifelse(D_CorrectBlicketness$final_direction_choice_conserv %in% c("Both", "Inverse"), 1, 0)
D_CorrectBlicketness$dir_INCONSISTENTchoice = as.factor(D_CorrectBlicketness$dir_INCONSISTENTchoice)
D_CorrectBlicketness$dir_INCONSISTENTchoice = relevel(D_CorrectBlicketness$dir_INCONSISTENTchoice, ref="0")

glm_dir_INCONSISTENTchoice = glm(dir_INCONSISTENTchoice~1,
                                 data = D_CorrectBlicketness,
                                 family = "binomial")
summary(glm_dir_INCONSISTENTchoice)
exp(glm_dir_INCONSISTENTchoice$coefficients)
exp(confint(glm_dir_INCONSISTENTchoice))

# of the 9 participants who accepted the inverse test object, how many are also accepting the SOC-consistent test object?
# 7 of 9
D_CorrectBlicketness$dir_INCONSISTENT_SOCalso <- ifelse(D_CorrectBlicketness$final_direction_choice_conserv %in% c("Both"), 1, 0)
D_CorrectBlicketness$dir_INCONSISTENT_SOCalso = as.factor(D_CorrectBlicketness$dir_INCONSISTENT_SOCalso)
D_CorrectBlicketness$dir_INCONSISTENT_SOCalso = relevel(D_CorrectBlicketness$dir_INCONSISTENT_SOCalso, ref = "0")

glm_dir_INCONSISTENT_SOCalso = glm(dir_INCONSISTENT_SOCalso~1,
                                   data=D_CorrectBlicketness[D_CorrectBlicketness$final_direction_choice_conserv=="Both" | D_CorrectBlicketness$final_direction_choice_conserv=="Inverse",],
                                   family = "binomial")
summary(glm_dir_INCONSISTENT_SOCalso)
exp(glm_dir_INCONSISTENT_SOCalso$coefficients)
exp(confint(glm_dir_INCONSISTENT_SOCalso))

# how many participants are choosing the order-consistent test object in Trial 3? 
# Order-Consistent + Both / ALL = 8 of 10
table(D_CorrectBlicketness$final_control_choice_conserv)

D_CorrectBlicketness$contr_CONSISTENTchoice <- ifelse(D_CorrectBlicketness$final_control_choice_conserv %in% c("Both", "Order Consistent"), 1, 0)
D_CorrectBlicketness$contr_CONSISTENTchoice = as.factor(D_CorrectBlicketness$contr_CONSISTENTchoice)
D_CorrectBlicketness$contr_CONSISTENTchoice = relevel(D_CorrectBlicketness$contr_CONSISTENTchoice, ref="0")

glm_contr_CONSISTENTchoice <- glm(contr_CONSISTENTchoice~1,
                                  data=D_CorrectBlicketness,
                                  family = "binomial")
summary(glm_contr_CONSISTENTchoice)
exp(glm_contr_CONSISTENTchoice$coefficients)
exp(confint(glm_contr_CONSISTENTchoice))

# of these 8 who choose the order-consistent test object, how many also accept the order-inconsistent? 7 of 8
D_CorrectBlicketness$contr_BOTH <- ifelse(D_CorrectBlicketness$final_control_choice_conserv %in% c("Both"), 1, 0)
D_CorrectBlicketness$contr_BOTH = as.factor(D_CorrectBlicketness$contr_BOTH)
D_CorrectBlicketness$contr_BOTH = relevel(D_CorrectBlicketness$contr_BOTH, ref="0")

glm_contr_BOTH <- glm(contr_BOTH~1,
                      data=D_CorrectBlicketness[D_CorrectBlicketness$final_control_choice_conserv=="Both" | D_CorrectBlicketness$final_control_choice_conserv=="Order Consistent", ],
                      family = "binomial")
summary(glm_contr_BOTH)
exp(glm_contr_BOTH$coefficients)
exp(confint(glm_contr_BOTH))

# additionally, we need to test participants who are choosing the order-inconsistent test object in Trial 3
# Order-Inconsistent + BOTH / ALL = 8/10
D_CorrectBlicketness$contr_INCONSISTENTchoice <- ifelse(D_CorrectBlicketness$final_control_choice_conserv %in% c("Both", "Order Inconsistent"), 1, 0)
D_CorrectBlicketness$contr_INCONSISTENTchoice = as.factor(D_CorrectBlicketness$contr_INCONSISTENTchoice)
D_CorrectBlicketness$contr_INCONSISTENTchoice = relevel(D_CorrectBlicketness$contr_INCONSISTENTchoice, ref="0")

glm_contr_INCONSISTENTchoice <- glm(contr_INCONSISTENTchoice~1,
                                    data=D_CorrectBlicketness,
                                    family = "binomial")
summary(glm_contr_INCONSISTENTchoice)
exp(glm_contr_INCONSISTENTchoice$coefficients)
exp(confint(glm_contr_INCONSISTENTchoice))

# participants were not likely to indicate the order-inconsistent test object as a blicket

# test if those who do choose the order-inconsistent test object, also accept the order-consistent test object
# BOTH / BOTH + Order-Inconsistent = 7 of 8
D_CorrectBlicketness$contr_INCONSISTENT_both <- ifelse(D_CorrectBlicketness$final_control_choice_conserv %in% c("Both"), 1, 0)
D_CorrectBlicketness$contr_INCONSISTENT_both = as.factor(D_CorrectBlicketness$contr_INCONSISTENT_both)
D_CorrectBlicketness$contr_INCONSISTENT_both = relevel(D_CorrectBlicketness$contr_INCONSISTENT_both, ref="0")

glm_contr_INCONSISTENT_BOTH <- glm(contr_INCONSISTENT_both~1,
                                   data=D_CorrectBlicketness[D_CorrectBlicketness$final_control_choice_conserv=="Both" | D_CorrectBlicketness$final_control_choice_conserv=="Order Inconsistent", ],
                                   family = "binomial")
summary(glm_contr_INCONSISTENT_BOTH)
exp(glm_contr_INCONSISTENT_BOTH$coefficients)
exp(confint(glm_contr_INCONSISTENT_BOTH))

### wrong Blicket

table(D_OtherBlicketness$final_test_choice_conserv)
# how many participants are choosing the SOC-consistent test object in Trial 1?
# SOC-consistent + Both / ALL = 10 of 15
D_OtherBlicketness$SOC_SOCchoice <- ifelse(D_OtherBlicketness$final_test_choice_conserv %in% c("Both", "SOC-consistent"), 1, 0)
D_OtherBlicketness$SOC_SOCchoice = as.factor(D_OtherBlicketness$SOC_SOCchoice)
D_OtherBlicketness$SOC_SOCchoice = relevel(D_OtherBlicketness$SOC_SOCchoice, ref="0")

glm_SOC_SOCchoice = glm(SOC_SOCchoice~1,
                        data = D_OtherBlicketness,
                        family = "binomial")
summary(glm_SOC_SOCchoice)
exp(glm_SOC_SOCchoice$coefficients)
exp(confint(glm_SOC_SOCchoice))

# of these 10 participants, how many reliably reject the SOC-inconsistent test object?
# SOC-consistent ONLY / SOC-consistent + BOTH = 7 of 10
D_OtherBlicketness$SOC_SOConly <- ifelse(D_OtherBlicketness$final_test_choice_conserv %in% c("SOC-consistent"), 1, 0)
D_OtherBlicketness$SOC_SOConly = as.factor(D_OtherBlicketness$SOC_SOConly)
D_OtherBlicketness$SOC_SOConly = relevel(D_OtherBlicketness$SOC_SOConly, ref="0")

glm_SOC_SOConly = glm(SOC_SOConly~1,
                      data = D_OtherBlicketness[D_OtherBlicketness$final_test_choice_conserv=="Both" | D_OtherBlicketness$final_test_choice_conserv=="SOC-consistent",],
                      family="binomial")
summary(glm_SOC_SOConly)
exp(glm_SOC_SOConly$coefficients)
exp(confint(glm_SOC_SOConly))

# how many participants chose the SOC-inconsistent test object in Trial 1?
# SOC-inconsitent + both / all = 4 of 15
D_OtherBlicketness$SOC_Inconsistent <- ifelse(D_OtherBlicketness$final_test_choice_conserv %in% c("Both", "SOC-inconsistent"), 1, 0)
D_OtherBlicketness$SOC_Inconsistent = as.factor(D_OtherBlicketness$SOC_Inconsistent)
D_OtherBlicketness$SOC_Inconsistent = relevel(D_OtherBlicketness$SOC_Inconsistent, ref="0")

glm_SOC_inconsistent = glm(SOC_Inconsistent~1,
                           data = D_OtherBlicketness,
                           family = "binomial")
summary(glm_SOC_inconsistent)
exp(glm_SOC_inconsistent$coefficients)
exp(confint(glm_SOC_inconsistent))

# given a SOC-inconsistent choice, how many also chose SOC-consistent
# 3 of 4
D_OtherBlicketness$SOC_both <- ifelse(D_OtherBlicketness$final_test_choice_conserv %in% c("Both"), 1, 0)
D_OtherBlicketness$SOC_both <- as.factor(D_OtherBlicketness$SOC_both)
D_OtherBlicketness$SOC_both <- relevel(D_OtherBlicketness$SOC_both, ref="0")

glm_SOC_inconsistent_both = glm(SOC_both~1,
                                data = D_OtherBlicketness[D_OtherBlicketness$final_test_choice_conserv=="Both" | D_OtherBlicketness$final_test_choice_conserv=="SOC-inconsistent", ],
                                family = "binomial")
summary(glm_SOC_inconsistent_both)
exp(glm_SOC_inconsistent_both$coefficients)
exp(confint(glm_SOC_inconsistent_both))

table(D_OtherBlicketness$final_direction_choice_conserv)
# how many participants are choosing the SOC-consistent test object in Trial 2?
# 5 (SOC-consistent + Both) of 15

D_OtherBlicketness$dir_SOCchoice <- ifelse(D_OtherBlicketness$final_direction_choice_conserv %in% c("Both", "Order Consistent"), 1, 0)
D_OtherBlicketness$dir_SOCchoice = as.factor(D_OtherBlicketness$dir_SOCchoice)
D_OtherBlicketness$dir_SOCchoice = relevel(D_OtherBlicketness$dir_SOCchoice, ref="0")

glm_dir_SOCchoice = glm(dir_SOCchoice~1,
                        data = D_OtherBlicketness,
                        family = "binomial")
summary(glm_dir_SOCchoice)
exp(glm_dir_SOCchoice$coefficients)
exp(confint(glm_dir_SOCchoice))

# of these 5 participants, how many reliably reject the inverse test object? 2 / 5
D_OtherBlicketness$dir_SOConly <- ifelse(D_OtherBlicketness$final_direction_choice_conserv %in% c("Order Consistent"), 1, 0)
D_OtherBlicketness$dir_SOConly = as.factor(D_OtherBlicketness$dir_SOConly)
D_OtherBlicketness$dir_SOConly = relevel(D_OtherBlicketness$dir_SOConly, ref="0")

glm_dir_SOC_AND_INVERSEchoice = glm(dir_SOConly~1,
                                    data = D_OtherBlicketness[D_OtherBlicketness$final_direction_choice_conserv=="Both" | D_OtherBlicketness$final_direction_choice_conserv=="Order Consistent",],
                                    family = "binomial")
summary(glm_dir_SOC_AND_INVERSEchoice)
exp(glm_dir_SOC_AND_INVERSEchoice$coefficients)
exp(confint(glm_dir_SOC_AND_INVERSEchoice))

# how many participants are accepting the inverse test object? 
# Inverse + Both / ALL = 10 of 15
D_OtherBlicketness$dir_INCONSISTENTchoice <- ifelse(D_OtherBlicketness$final_direction_choice_conserv %in% c("Both", "Inverse"), 1, 0)
D_OtherBlicketness$dir_INCONSISTENTchoice = as.factor(D_OtherBlicketness$dir_INCONSISTENTchoice)
D_OtherBlicketness$dir_INCONSISTENTchoice = relevel(D_OtherBlicketness$dir_INCONSISTENTchoice, ref="0")

glm_dir_INCONSISTENTchoice = glm(dir_INCONSISTENTchoice~1,
                                 data = D_OtherBlicketness,
                                 family = "binomial")
summary(glm_dir_INCONSISTENTchoice)
exp(glm_dir_INCONSISTENTchoice$coefficients)
exp(confint(glm_dir_INCONSISTENTchoice))

# of the 10 participants who accepted the inverse test object, how many are also accepting the SOC-consistent test object?
# 3 of 10
D_OtherBlicketness$dir_INCONSISTENT_SOCalso <- ifelse(D_OtherBlicketness$final_direction_choice_conserv %in% c("Both"), 1, 0)
D_OtherBlicketness$dir_INCONSISTENT_SOCalso = as.factor(D_OtherBlicketness$dir_INCONSISTENT_SOCalso)
D_OtherBlicketness$dir_INCONSISTENT_SOCalso = relevel(D_OtherBlicketness$dir_INCONSISTENT_SOCalso, ref = "0")

glm_dir_INCONSISTENT_SOCalso = glm(dir_INCONSISTENT_SOCalso~1,
                                   data=D_OtherBlicketness[D_OtherBlicketness$final_direction_choice_conserv=="Both" | D_OtherBlicketness$final_direction_choice_conserv=="Inverse",],
                                   family = "binomial")
summary(glm_dir_INCONSISTENT_SOCalso)
exp(glm_dir_INCONSISTENT_SOCalso$coefficients)
exp(confint(glm_dir_INCONSISTENT_SOCalso))

# how many participants are choosing the order-consistent test object in Trial 3? 
# Order-Consistent + Both / ALL = 9 of 15
table(D_OtherBlicketness$final_control_choice_conserv)

D_OtherBlicketness$contr_CONSISTENTchoice <- ifelse(D_OtherBlicketness$final_control_choice_conserv %in% c("Both", "Order Consistent"), 1, 0)
D_OtherBlicketness$contr_CONSISTENTchoice = as.factor(D_OtherBlicketness$contr_CONSISTENTchoice)
D_OtherBlicketness$contr_CONSISTENTchoice = relevel(D_OtherBlicketness$contr_CONSISTENTchoice, ref="0")

glm_contr_CONSISTENTchoice <- glm(contr_CONSISTENTchoice~1,
                                  data=D_OtherBlicketness,
                                  family = "binomial")
summary(glm_contr_CONSISTENTchoice)
exp(glm_contr_CONSISTENTchoice$coefficients)
exp(confint(glm_contr_CONSISTENTchoice))

# of these 9 who choose the order-consistent test object, how many also accept the order-inconsistent? 4 of 9
D_OtherBlicketness$contr_BOTH <- ifelse(D_OtherBlicketness$final_control_choice_conserv %in% c("Both"), 1, 0)
D_OtherBlicketness$contr_BOTH = as.factor(D_OtherBlicketness$contr_BOTH)
D_OtherBlicketness$contr_BOTH = relevel(D_OtherBlicketness$contr_BOTH, ref="0")

glm_contr_BOTH <- glm(contr_BOTH~1,
                      data=D_OtherBlicketness[D_OtherBlicketness$final_control_choice_conserv=="Both" | D_OtherBlicketness$final_control_choice_conserv=="Order Consistent", ],
                      family = "binomial")
summary(glm_contr_BOTH)
exp(glm_contr_BOTH$coefficients)
exp(confint(glm_contr_BOTH))

# additionally, we need to test participants who are choosing the order-inconsistent test object in Trial 3
# Order-Inconsistent + BOTH / ALL = 4/15
D_OtherBlicketness$contr_INCONSISTENTchoice <- ifelse(D_OtherBlicketness$final_control_choice_conserv %in% c("Both", "Order Inconsistent"), 1, 0)
D_OtherBlicketness$contr_INCONSISTENTchoice = as.factor(D_OtherBlicketness$contr_INCONSISTENTchoice)
D_OtherBlicketness$contr_INCONSISTENTchoice = relevel(D_OtherBlicketness$contr_INCONSISTENTchoice, ref="0")

glm_contr_INCONSISTENTchoice <- glm(contr_INCONSISTENTchoice~1,
                                    data=D_OtherBlicketness,
                                    family = "binomial")
summary(glm_contr_INCONSISTENTchoice)
exp(glm_contr_INCONSISTENTchoice$coefficients)
exp(confint(glm_contr_INCONSISTENTchoice))

# test if those who do choose the order-inconsistent test object, also accept the order-consistent test object
# BOTH / BOTH + Order-Inconsistent = 4 of 4
D_OtherBlicketness$contr_INCONSISTENT_both <- ifelse(D_OtherBlicketness$final_control_choice_conserv %in% c("Both"), 1, 0)
D_OtherBlicketness$contr_INCONSISTENT_both = as.factor(D_OtherBlicketness$contr_INCONSISTENT_both)
D_OtherBlicketness$contr_INCONSISTENT_both = relevel(D_OtherBlicketness$contr_INCONSISTENT_both, ref="0")

glm_contr_INCONSISTENT_BOTH <- glm(contr_INCONSISTENT_both~1,
                                   data=D_OtherBlicketness[D_OtherBlicketness$final_control_choice_conserv=="Both" | D_OtherBlicketness$final_control_choice_conserv=="Order Inconsistent", ],
                                   family = "binomial")
summary(glm_contr_INCONSISTENT_BOTH)
exp(glm_contr_INCONSISTENT_BOTH$coefficients)
exp(confint(glm_contr_INCONSISTENT_BOTH))
