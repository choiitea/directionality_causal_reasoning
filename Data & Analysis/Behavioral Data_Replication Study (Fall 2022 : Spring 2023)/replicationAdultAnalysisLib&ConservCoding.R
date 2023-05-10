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

# ## create a final_test_choice_conserv
# D$final_test_choice_conserv = rep(0,nrow(D))
# for(i in 1:nrow(D)){
#   if(D$correct_soc_choice[i]==D$final_soc_choice[i]){
#     D$final_test_choice_conserv[i] = "SOC-consistent"
#   }else if(D$final_soc_choice[i]=="both"){
#     D$final_test_choice_conserv[i] = "Both"
#   }else if(D$final_soc_choice[i]=="neither"){
#     D$final_test_choice_conserv[i] = "Neither"
#   }else if(D$final_soc_choice[i]=="left"&&D$correct_soc_choice[i]=="right"){
#     D$final_test_choice_conserv[i] = "SOC-inconsistent"
#   }else if(D$final_soc_choice[i]=="right"&&D$correct_soc_choice[i]=="left"){
#     D$final_test_choice_conserv[i] = "SOC-inconsistent"
#   }
#   else{
#     D$final_test_choice_conserv[i] = "Neither"
#   }
# }
# table(D$final_test_choice_conserv)

## create a direction_choice_lib variable ##
D$direction_choice_lib = rep(0, nrow(D))
for(i in 1:nrow(D)){
  if(D$test_dir_choice[i]==D$ordered_dir_choice[i]){
    D$direction_choice_lib[i] = 1 #soc-consistent / order-consistent
  }else if(D$test_dir_choice[i]=="both"){
    D$direction_choice_lib[i] = 0
  }else if(D$test_dir_choice[i]=="left"&&D$ordered_dir_choice[i]=="right"){
    D$direction_choice_lib[i] = 0 #chose inverse but not soc_con
  }else if(D$test_dir_choice[i]=="right"&&D$ordered_dir_choice[i]=="left"){
    D$direction_choice_lib[i] = 0 #chose inverse but not soc_con
  }else if(D$test_dir_choice[i]=="unsure"){
    D$direction_choice_lib[i] = 0 #unsure response
  }else{
    D$direction_choice_lib[i] = 0 #neither
  }
}
# 0 - both; 1 - SOC_consistent; 2 - inverse but not soc_con; 3 - neither

# name the levels of direction_choice_lib
D$direction_choice_lib = revalue(x = as.factor(D$direction_choice_lib), 
                                 c("0" = "Other", "1"="Order Consistent"))

#  Note that this corresponds to a "liberal" coding of adults' responses 
# to the direction trials. 

# get the distribution of "order consistent" and "other" choices
# for the Direction test trial
table(D$direction_choice_lib)

# create final_direction_choice_lib
D$final_direction_choice_lib = rep(0, nrow(D))
for(i in 1:nrow(D)){
  if(D$final_dir_choice[i]==D$ordered_dir_choice[i]){
    D$final_direction_choice_lib[i] = 1 #soc-consistent / order-consistent
  }else if(D$final_dir_choice[i]=="both"){
    D$final_direction_choice_lib[i] = 0
  }else if(D$final_dir_choice[i]=="left"&&D$ordered_dir_choice[i]=="right"){
    D$final_direction_choice_lib[i] = 0 #chose inverse but not soc_con
  }else if(D$final_dir_choice[i]=="right"&&D$ordered_dir_choice[i]=="left"){
    D$final_direction_choice_lib[i] = 0 #chose inverse but not soc_con
  }else if(D$final_dir_choice[i]=="unsure"){
    D$final_direction_choice_lib[i] = 0 #unsure response
  }else{
    D$final_direction_choice_lib[i] = 0 #neither
  }
}

D$final_direction_choice_lib = revalue(x = as.factor(D$final_direction_choice_lib), 
                                 c("0" = "Other", "1"="Order Consistent"))

# Here it's clear that participants are much less likely to choose the "order consistent" object
# compared to "other" choices. 

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
# 0 - both; 1 - SOC_consistent; 2 - inverse but not soc_con; 3 - neither; 4 - unsure

# name the levels of direction_choice_conserv
D$final_direction_choice_conserv = revalue(x = as.factor(D$final_direction_choice_conserv), 
                                     c("0" = "Both", "1"="Order Consistent",
                                       "2" = "Inverse", "3"= "Neither", "4" = "Unsure"))

# get the distribution of choices for the conservative coding of direction after intuit
table(D$final_direction_choice_conserv)

## create a control_choice_lib variable ##
D$control_choice_lib = rep(0, nrow(D))
for(i in 1:nrow(D)){
  if(D$ordered_contrDir_choice[i]==D$test_contrDir_choice[i]){
    D$control_choice_lib[i] = 1 #order-consistent 
  }else if(D$test_contrDir_choice[i]=="both"){
    D$control_choice_lib[i] = 0
  }else if(D$test_contrDir_choice[i]=="left"&&D$ordered_contrDir_choice[i]=="right"){
    D$control_choice_lib[i] = 0 #chose inverse but not order_con
  }else if(D$test_contrDir_choice[i]=="right"&&D$ordered_contrDir_choice[i]=="left"){
    D$control_choice_lib[i] = 0 #chose inverse but not order_con
  }else if(D$test_contrDir_choice[i]=="unsure"){
    D$control_choice_lib[i] = 0 #unsure
  }else{
    D$control_choice_lib[i] = 0 #neither
  }
}

# name the levels of control_choice_lib
D$control_choice_lib = revalue(x = as.factor(D$control_choice_lib), 
                               c("0" = "Other", "1"="Order Consistent"))
table(D$control_choice_lib)

### create final_control_choice_lib ###

D$final_control_choice_lib = rep(0, nrow(D))
for(i in 1:nrow(D)){
  if(D$ordered_contrDir_choice[i]==D$final_contrDir_choice[i]){
    D$final_control_choice_lib[i] = 1 #order-consistent 
  }else if(D$final_contrDir_choice[i]=="both"){
    D$final_control_choice_lib[i] = 0
  }else if(D$final_contrDir_choice[i]=="left"&&D$ordered_contrDir_choice[i]=="right"){
    D$final_control_choice_lib[i] = 0 #chose inverse but not order_con
  }else if(D$final_contrDir_choice[i]=="right"&&D$ordered_contrDir_choice[i]=="left"){
    D$final_control_choice_lib[i] = 0 #chose inverse but not order_con
  }else if(D$final_contrDir_choice[i]=="unsure"){
    D$final_control_choice_lib[i] = 0 #unsure
  }else{
    D$final_control_choice_lib[i] = 0 #neither
  }
}

# name the levels of final_control_choice_lib
D$final_control_choice_lib = revalue(x = as.factor(D$final_control_choice_lib), 
                               c("0" = "Other", "1"="Order Consistent"))

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
D$direction_choice_lib = as.factor(D$direction_choice_lib)
D$final_direction_choice_lib = as.factor(D$final_direction_choice_lib)
D$direction_choice_conserv = as.factor(D$direction_choice_conserv)
D$final_direction_choice_conserv = as.factor(D$final_direction_choice_conserv)
D$control_choice_lib = as.factor(D$control_choice_lib)
D$final_control_choice_lib = as.factor(D$final_control_choice_lib)
D$control_choice_conserv = as.factor(D$control_choice_conserv)
D$final_control_choice_conserv = as.factor(D$final_control_choice_conserv)

 #table(D$memory_check)
 #D <- D[!(D$memory_check=="2"),]

# Create a logical vector indicating whether each row contains a null value in intuit columns
no_intuit_vector <- is.na(D$intuit_soc_choice) & is.na(D$intuit_dir_choice) & is.na(D$intuit_contrDir_choice)

## D_NOintuit - participants who were not reprompted to intuit
D_NOintuit <- D[no_intuit_vector, ]

## D_intuit - participants who were reprompted to intuit at least once
D_HASintuit <- D[!no_intuit_vector, ]

# remove unnecessary columns
D = D[,-c(1,10:21)]
D$ID = c(1:nrow(D))
D$row.names = NULL

D_HASintuit = D_HASintuit[,-c(1,10:21)]
D_HASintuit$ID = c(1:nrow(D_HASintuit))
D_HASintuit$row.names = NULL

D_NOintuit = D_NOintuit[,-c(1,10:21)]
D_NOintuit$ID = c(1:nrow(D_NOintuit))
D_NOintuit$row.names = NULL

### cleaned csv export ###
#write.csv(D,"cleaned_replication_data_wRating_wUnsure2.csv", row.names = FALSE)

#####################################
# preliminary analyses and plotting #
#####################################
#### memory check failures
table(D$memory_check)

# get distribution of successes and failure 
# test_choice
table(D$test_choice)
baseline_test_sucess_prob = table(D$test_choice)[[2]]/(table(D$test_choice)[[1]]+table(D$test_choice)[[2]]+table(D$test_choice)[[3]])
baseline_test_sucess_prob
baseline_test_success_odds = baseline_test_sucess_prob/(1-baseline_test_sucess_prob) # this is what will be shown
baseline_test_success_odds 

# final_test_choice
table(D$final_test_choice)
baseline_test_sucess_prob = table(D$final_test_choice)[[1]]/(table(D$final_test_choice)[[1]]+table(D$final_test_choice)[[2]])
baseline_test_sucess_prob
baseline_test_success_odds = baseline_test_sucess_prob/(1-baseline_test_sucess_prob) # this is what will be shown
baseline_test_success_odds

# direction_choice
table(D$direction_choice_lib)
baseline_direction_ordered_lib_prob = table(D$direction_choice_lib)[[2]]/(table(D$direction_choice_lib)[[1]]+table(D$direction_choice_lib)[[2]])
baseline_direction_ordered_lib_prob 
baseline_direction_ordered_lib_odds = baseline_direction_ordered_lib_prob/(1-baseline_direction_ordered_lib_prob) # this is what will be shown
baseline_direction_ordered_lib_odds

table(D$direction_choice_conserv)
baseline_direction_ordered_conserv_prob = table(D$direction_choice_conserv)[[2]]/(table(D$direction_choice_conserv)[[1]]+
                                                                                    table(D$direction_choice_conserv)[[2]]+
                                                                                    table(D$direction_choice_conserv)[[3]]+
                                                                                    table(D$direction_choice_conserv)[[4]]+
                                                                                    table(D$direction_choice_conserv)[[5]])
baseline_direction_ordered_conserv_prob 
baseline_direction_ordered__conserv_odds = baseline_direction_ordered_conserv_prob/(1-baseline_direction_ordered_conserv_prob) # this is what will be shown
baseline_direction_ordered__conserv_odds

table(D$final_direction_choice_conserv)
final_baseline_direction_ordered_conserv_prob = table(D$final_direction_choice_conserv)[[2]]/(table(D$final_direction_choice_conserv)[[1]]+
                                                                                    table(D$final_direction_choice_conserv)[[2]]+
                                                                                    table(D$final_direction_choice_conserv)[[3]]+
                                                                                    table(D$final_direction_choice_conserv)[[4]])
final_baseline_direction_ordered_conserv_prob 
final_baseline_direction_ordered__conserv_odds = final_baseline_direction_ordered_conserv_prob/(1-final_baseline_direction_ordered_conserv_prob) # this is what will be shown
final_baseline_direction_ordered__conserv_odds

# control_choice
table(D$control_choice_lib)
baseline_control_ordered_lib_prob = table(D$control_choice_lib)[[2]]/(table(D$control_choice_lib)[[1]]+table(D$control_choice_lib)[[2]])
baseline_control_ordered_lib_prob
baseline_control_ordered_lib_odds = baseline_control_ordered_lib_prob/(1-baseline_control_ordered_lib_prob) # this is what will be shown
baseline_control_ordered_lib_odds


table(D$control_choice_conserv)
baseline_control_ordered_conserv_prob = table(D$control_choice_conserv)[[2]]/(table(D$control_choice_conserv)[[1]]+
                                                                                table(D$control_choice_conserv)[[2]]+
                                                                                table(D$control_choice_conserv)[[3]]+
                                                                                table(D$control_choice_conserv)[[4]]+
                                                                                table(D$control_choice_conserv)[[5]])
baseline_control_ordered_conserv_prob
baseline_control_ordered_conserv_odds = baseline_control_ordered_conserv_prob/(1-baseline_control_ordered_conserv_prob) # this is what will be shown
baseline_control_ordered_conserv_odds

table(D$final_control_choice_conserv)
baseline_final_control_ordered_conserv_prob = table(D$final_control_choice_conserv)[[2]]/(table(D$final_control_choice_conserv)[[1]]+
                                                                                table(D$final_control_choice_conserv)[[2]]+
                                                                                table(D$final_control_choice_conserv)[[3]]+
                                                                                table(D$final_control_choice_conserv)[[4]])
baseline_final_control_ordered_conserv_prob
baseline_final_control_ordered_conserv_odds = baseline_final_control_ordered_conserv_prob/(1-baseline_final_control_ordered_conserv_prob) # this is what will be shown
baseline_final_control_ordered_conserv_odds
#################
# main analysis #
#################
# test_choice FULL analysis
D$final_test_choice = relevel(D$final_test_choice, ref="Incorrect")
D$test_choice = relevel(D$test_choice, ref="Incorrect")
glm_test_choice_full = glm(test_choice~static_con+causal_con+soc_con+dir_con+contrDir_con, 
                           data = D, 
                           family = "binomial")
summary(glm_test_choice_full)
glm_final_test_choice_full = glm(final_test_choice~static_con+causal_con+soc_con+dir_con+contrDir_con, 
                           data = D, 
                           family = "binomial")
summary(glm_final_test_choice_full)
# This analysis tell us that there is no effect of our counterbalancing conditions

glm_test_choice = glm(test_choice~1, 
                      data = D, 
                      family = "binomial")
summary(glm_test_choice)
exp(glm_test_choice$coefficients)
confint(glm_test_choice)
table(D$test_choice) # 14 of 32

glm_final_test_choice = glm(final_test_choice~1, 
                      data = D, 
                      family = "binomial")
summary(glm_final_test_choice)
exp(glm_final_test_choice$coefficients)
exp(confint(glm_final_test_choice))
#estimate = coefficient -- log odds - so if you exponentiate it, you'll get real odds
table(D$final_test_choice) # 20 of 32

# Figure
test_choice_barplot = ggplot(D, aes(final_test_choice, fill = final_test_choice)) 
test_choice_barplot + geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 30)) +
  scale_fill_manual(values=c("black","#ffc857")) +
  xlab("Second-Order Correlation Test Choice") +
  ylab("Number of Responses") +
  theme_bw() +
  theme(legend.position = "none")

## liberal analyses ##
# direction_choice_lib main analysis
table(D$direction_choice_lib)
glm_direction_choice_full = glm(direction_choice_lib~static_con+causal_con+soc_con+dir_con+contrDir_con, 
                                data = D, family = "binomial")
summary(glm_direction_choice_full)

#### commented out for now - cmd shift c (because I always forget how to comment out block.)
table(D$direction_choice_lib)
D$direction_choice_lib = relevel(D$direction_choice_lib, ref="Other")
glm_direction_choice_lib = glm(direction_choice_lib~1,
                               data = D, family = "binomial")
summary(glm_direction_choice_lib)
exp(glm_direction_choice_lib$coefficients)
exp(confint(glm_direction_choice_lib))
# 
# # liberal figure
# direction_choice_liberal_barplot = ggplot(D, aes(direction_choice_lib, fill = direction_choice_lib))
# direction_choice_liberal_barplot + geom_bar(position="dodge") +
#   scale_y_continuous(expand = c(0, 0)) +
#   coord_cartesian(ylim=c(0, 30)) +
#   scale_fill_manual(values=c("black","#ffc857")) +
#   xlab("Relational Order Test Choice") +
#   ylab("Number of Responses") +
#   theme_bw() +
#   theme(legend.position = "none")

## conservative analyses ##
table(D$final_direction_choice_conserv)
multinom_direction_choice = multinom(final_direction_choice_conserv ~ 1, data = D)
summary(multinom_direction_choice)

multinom_direction_choice_z <- summary(multinom_direction_choice)$coefficients/summary(multinom_direction_choice)$standard.errors
multinom_direction_choice_z
multinom_direction_choice_p <- (1 - pnorm(abs(multinom_direction_choice_z), 0, 1)) * 2
multinom_direction_choice_p


# conservative figure
direction_choice_conserv_barplot = ggplot(D, aes(final_direction_choice_conserv, fill = final_direction_choice_conserv)) 
direction_choice_conserv_barplot + geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 20)) +
  scale_fill_manual(values=c("black","#ffc857", "darkblue", "#9933FF")) +
  xlab("Relational Order Test Choice") +
  ylab("Number of Responses") + 
  theme_bw() +
  theme(legend.position = "none")

# Conclusion for the liberal analysis: 

# Conclusion for the conservative analysis: 


## liberal analyses ## 
# control_choice_main_analysis
table(D$control_choice_lib)
glm_control_choice_full = glm(control_choice_lib ~ static_con+causal_con+soc_con+dir_con+contrDir_con, 
                              data = D, family = "binomial")
summary(glm_control_choice_full)

##### commented out for now.
D$control_choice_lib = relevel(D$control_choice_lib, ref="Other")
glm_control_choice = glm(control_choice_lib ~ 1,
                         data = D, family = "binomial")
summary(glm_control_choice)
exp(glm_control_choice$coefficients)
exp(confint(glm_control_choice))

# # liberal figure
# control_choice_lib_barplot = ggplot(D, aes(x=reorder(control_choice_lib, control_choice_lib, function(x)-length(x)), fill = control_choice_lib)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
# control_choice_lib_barplot + geom_bar(position="dodge") +
#   scale_y_continuous(expand = c(0, 0)) +
#   coord_cartesian(ylim=c(0, 30)) +
#   scale_fill_manual(values=c("#ffc857","black")) +
#   xlab("Controlled Relational Order Test Choice") +
#   ylab("Number of Responses") + 
#   theme_bw() +
#   theme(legend.position = "none")


## conservative analyses ##
table(D$final_control_choice_conserv)
multinom_control_choice = multinom(final_control_choice_conserv ~ 1, data = D)
summary(multinom_control_choice)

multinom_control_choice_z <- summary(multinom_control_choice)$coefficients/summary(multinom_control_choice)$standard.errors
multinom_control_choice_z
multinom_control_choice_p <- (1 - pnorm(abs(multinom_control_choice_z), 0, 1)) * 2
multinom_control_choice_p

# conservative figure
control_choice_conserv_barplot = ggplot(D, aes(final_control_choice_conserv, fill = final_control_choice_conserv)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
control_choice_conserv_barplot + geom_bar(position="dodge") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 20)) +
  scale_fill_manual(values=c("black","#ffc857", "#9933FF","darkblue")) +
  scale_x_discrete(drop=FALSE) +
  xlab("Controlled Relational Order Test Choice") +
  ylab("Number of Responses") + 
  theme_bw() +
  theme(legend.position = "none")

# Conclusion for the liberal analysis: 

# Conclusion for the conservative analysis: 


## Follow-up t-tests ##

## assess whether participants were more likely to provide higher (or even lower) ##
## ratings for the correct and incorrect objects during the test trial ##

# correct ratings
mean_test_rating_correct = mean(D$test_rating_correct, na.rm = TRUE)
mean_test_rating_correct
sd_test_rating_correct = sd(D$test_rating_correct, na.rm = TRUE)
sd_test_rating_correct

# incorrect ratings
mean_test_rating_incorrect = mean(D$test_rating_incorrect, na.rm = TRUE)
mean_test_rating_incorrect
sd_test_rating_incorrect = sd(D$test_rating_incorrect, na.rm = TRUE)
sd_test_rating_incorrect

t_test_test_trial_ratings = t.test(D$test_rating_correct,
                                   D$test_rating_incorrect, paired = TRUE,
                                   alternative = "two.sided")
t_test_test_trial_ratings

D_test_rating_tall = reshape(D, varying = 14:15, v.names = "rating", timevar = "final_test_trial_rating", idvar = "ID", 
                             new.row.names = 1:62, 
                             direction = "long")
test_rating_barplot = ggplot(D_test_rating_tall, aes(final_test_trial_rating, rating)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
test_rating_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Rating") + # change the label of the y-axis  +
  xlab("SOC Test Trial") +
  scale_y_continuous(expand = c(0, 1)) +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_manual(values = c("black", "azure3")) +
  theme_bw()

# Examine whether there is an effect of whether participants chose the SOC object during
# the replication trial (i.e., D$test_choice) on their ratings of the correct (i.e., D$test_rating_correct)
# & incorrect objects.
aov_test_choice_correct = aov(test_rating_correct~final_test_choice,
                              data = D)
summary(aov_test_choice_correct)

# rating of correct object for those who chose correctly
mean(D$test_rating_correct[D$final_test_choice=="Correct"], na.rm = TRUE)
sd(D$test_rating_correct[D$final_test_choice=="Correct"], na.rm = TRUE)

# rating of correct object for those who chose incorrectly
mean(D$test_rating_correct[D$final_test_choice=="Incorrect"], na.rm = TRUE)
sd(D$test_rating_correct[D$final_test_choice=="Incorrect"], na.rm = TRUE)

test_rating_correct_barplot = ggplot(D, aes(final_test_choice, final_test_rating_correct, fill = final_test_choice)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
test_rating_correct_barplot + 
  stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Rating") + # change the label of the y-axis
  xlab("SOC Test Trial Object Ratings of Correct Responses") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_manual(values=c("black","#ffc857")) +
  theme_bw() +
  theme(legend.position = "none")

# CONCLUSION:
# 

aov_test_choice_incorrect = aov(test_rating_incorrect~final_test_choice,
                                data = D)
summary(aov_test_choice_incorrect)

# rating of incorrect object for those who chose correctly
mean(D$test_rating_incorrect[D$final_test_choice=="Correct"], na.rm = TRUE)
sd(D$test_rating_incorrect[D$final_test_choice=="Correct"], na.rm = TRUE)

# rating of incorrect object for those who chose incorrectly
mean(D$test_rating_incorrect[D$final_test_choice=="Incorrect"], na.rm = TRUE)
sd(D$test_rating_incorrect[D$final_test_choice=="Incorrect"], na.rm = TRUE)

test_rating_incorrect_barplot = ggplot(D, aes(final_test_choice, final_test_rating_incorrect, fill = final_test_choice)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
test_rating_incorrect_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Rating") + # change the label of the y-axis
  xlab("SOC Test Trial Object Ratings of Incorrect Responses") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_manual(values=c("black","#ffc857")) +
  theme_bw() +
  theme(legend.position = "none")

# CONCLUSION:

# Examine whether there is an effect of whether participants chose the direction-consistent object during
# the direction trial (i.e., D$direction_choice_lib or D$direction_choice_conserv) for both 
# the liberal and conservative codings
# on their ratings of the direction consistent and inverse objects (D$direction_rating_SOCconsistent or 
# D$direction_rating_inverse)
# & incorrect objects.

table(D$direction_choice_lib)
aov_direction_choice_lib_correct = aov(D$direction_rating_SOCconsistent~D$dir_SOCchoice,
                                       data = D)
summary(aov_direction_choice_lib_correct)

# other
mean(D$direction_rating_SOCconsistent[D$direction_choice_lib=="Other"], na.rm = TRUE)
sd(D$direction_rating_SOCconsistent[D$direction_choice_lib=="Other"], na.rm = TRUE)

# order consistent
mean(D$direction_rating_SOCconsistent[D$direction_choice_lib=="Order Consistent"], na.rm = TRUE)
sd(D$direction_rating_SOCconsistent[D$direction_choice_lib=="Order Consistent"], na.rm = TRUE)

direction_rating_SOCconsistent_barplot = ggplot(D, aes(final_direction_choice_conserv,final_direction_rating_SOCconsistent, fill = final_direction_choice_conserv)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
direction_rating_SOCconsistent_barplot + 
  stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Rating") + # change the label of the y-axis
  xlab("Order Test Trial Object Ratings for Order Consistent Responses") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_manual(values=c("black","#ffc857", "dark blue", "purple")) +
  theme_bw() +
  theme(legend.position = "none")

# CONCLUSION: 


table(D$final_direction_choice_conserv)
aov_direction_choice_conserv_correct = aov(D$direction_rating_SOCconsistent~D$final_direction_choice_conserv,
                                           data = D)
summary(aov_direction_choice_conserv_correct)

# both
mean(D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Both"], na.rm = TRUE)
sd(D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Both"], na.rm = TRUE)

# order consistent
mean(D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Order Consistent"], na.rm = TRUE)
sd(D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Order Consistent"], na.rm = TRUE)

# inverse 
mean(D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Inverse"], na.rm = TRUE)
sd(D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Inverse"], na.rm = TRUE)

# neither 
mean(D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Neither"], na.rm = TRUE)
sd(D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Neither"], na.rm = TRUE)

direction_rating_inverse_barplot = ggplot(D, aes(final_direction_choice_conserv,final_direction_rating_inverse, fill = final_direction_choice_conserv)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
direction_rating_inverse_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Rating") + # change the label of the y-axis
  xlab("Order Test Trial Object Ratings for Order Inconsistent Responses") + 
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_manual(values=c("black","#ffc857", "dark blue", "purple")) +
  theme_bw() +
  theme(legend.position = "none")

# follow-up t-tests
both_direction_choice_correct = D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Both"]
order_consistent_direction_choice_correct = D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Order Consistent"]
inverse_direction_choice_correct = D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Inverse"]
neither_direction_choice_correct = D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Neither"]

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded by choosing the order-consistent test object.
both_vs_order_consistent_t_test = t.test(both_direction_choice_correct,
                                         order_consistent_direction_choice_correct,
                                         alternative = "two.sided") 
both_vs_order_consistent_t_test

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded by choosing the order-inconsistent test object
both_vs_inverse_t_test = t.test(both_direction_choice_correct, 
                                           inverse_direction_choice_correct,
                                           alternative = "two.sided")
both_vs_inverse_t_test

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


# CONCLUSION: 

# liberal coding
table(D$direction_choice_lib)
aov_direction_choice_lib_inverse = aov(D$direction_rating_inverse~D$dir_INCONSISTENTchoice,
                                       data = D)
summary(aov_direction_choice_lib_inverse)

mean(D$direction_rating_inverse[D$direction_choice_lib=="Other"], na.rm = TRUE)
sd(D$direction_rating_inverse[D$direction_choice_lib=="Other"], na.rm = TRUE)

mean(D$direction_rating_inverse[D$direction_choice_lib=="Order Consistent"], na.rm = TRUE)
sd(D$direction_rating_inverse[D$direction_choice_lib=="Order Consistent"], na.rm = TRUE)

# CONCLUSION: 


table(D$final_direction_choice_conserv)
aov_direction_choice_conserv_inverse = aov(D$direction_rating_inverse~D$final_direction_choice_conserv,
                                           data = D)
summary(aov_direction_choice_conserv_inverse)

# both
mean(D$direction_rating_inverse[D$final_direction_choice_conserv=="Both"], na.rm = TRUE)
sd(D$direction_rating_inverse[D$final_direction_choice_conserv=="Both"], na.rm = TRUE)

# order consistent
mean(D$direction_rating_inverse[D$final_direction_choice_conserv=="Order Consistent"])
sd(D$direction_rating_inverse[D$final_direction_choice_conserv=="Order Consistent"])

# inverse 
mean(D$direction_rating_inverse[D$final_direction_choice_conserv=="Inverse"])
sd(D$direction_rating_inverse[D$final_direction_choice_conserv=="Inverse"])

# neither 
mean(D$direction_rating_inverse[D$final_direction_choice_conserv=="Neither"])
sd(D$direction_rating_inverse[D$final_direction_choice_conserv=="Neither"])

control_rating_ordered_barplot = ggplot(D, aes(final_control_choice_conserv,final_control_rating_ordered, fill = final_control_choice_conserv)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
control_rating_ordered_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Rating") + # change the label of the y-axis
  xlab("Control Test Trial Object Ratings for Order Consistent Responses") + 
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_manual(values=c("black","#ffc857", "dark blue", "purple")) +
  theme_bw() +
  theme(legend.position = "none")

# follow-up t-tests
both_direction_choice_inverse = D$direction_rating_inverse[D$final_direction_choice_conserv=="Both"]
order_consistent_direction_choice_inverse = D$direction_rating_inverse[D$final_direction_choice_conserv=="Order Consistent"]
inverse_direction_choice_inverse = D$direction_rating_inverse[D$final_direction_choice_conserv=="Inverse"]
neither_direction_choice_inverse = D$direction_rating_inverse[D$final_direction_choice_conserv=="Neither"]

# compare the confidence ratings for the order inconsistent test object between those who responded "both" and those
# who responded by choosing the order-consistent test object.
both_vs_order_consistent_inverse_t_test = t.test(both_direction_choice_inverse,
                                                 order_consistent_direction_choice_inverse,
                                                 alternative = "two.sided") 
both_vs_order_consistent_inverse_t_test

# compare the confidence ratings for the order inconsistent test object between those who responded "both" and those
# who responded by choosing the order-inconsistent test object
both_vs_inverse_inverse_t_test = t.test(both_direction_choice_inverse, 
                                inverse_direction_choice_inverse,
                                alternative = "two.sided")
both_vs_inverse_inverse_t_test

# compare the confidence ratings for the order inconsistent test object between those who responded "both" and those
# who responded "neither.
both_vs_neither_inverse_t_test = t.test(both_direction_choice_inverse,
                                        neither_direction_choice_inverse,
                                        alternative = "two.sided")
both_vs_neither_inverse_t_test


# compare the confidence ratings for the order consistent test object between those who responded by choosing the 
# directionally consistent object and those
# who responded "neither.
direction_consistent_vs_inverse_inverse_t_test = t.test(order_consistent_direction_choice_inverse,
                                                        inverse_direction_choice_inverse,
                                                        alternative = "two.sided")
direction_consistent_vs_inverse_inverse_t_test

# compare the confidence ratings for the order consistent test object between those who responded by choosing the 
# directionally consistent object and those
# who responded "neither.
direction_consistent_vs_neither_inverse_t_test = t.test(order_consistent_direction_choice_inverse,
                                                        neither_direction_choice_inverse,
                                                        alternative = "two.sided")
direction_consistent_vs_neither_inverse_t_test


# CONCLUSION: 


# Examine whether there is an effect of whether participants chose the direction-consistent object during
# the control trial for both the liberal and conservative codings
# on their ratings of the control_ordered and inverse object.
table(D$control_choice_lib)
aov_control_choice_lib_correct = aov(D$control_rating_ordered~D$contr_CONSISTENTchoice,
                                     data = D)
summary(aov_control_choice_lib_correct)

mean(D$control_rating_ordered[D$control_choice_lib=="Order Consistent"], na.rm = TRUE)
sd(D$control_rating_ordered[D$control_choice_lib=="Order Consistent"], na.rm = TRUE)
mean(D$control_rating_ordered[D$control_choice_lib=="Other"], na.rm = TRUE)
sd(D$control_rating_ordered[D$control_choice_lib=="Other"], na.rm = TRUE)

# CONCLUSION: Participants who chose the order consistent object were more confident in their
# ratings of the direction-consistent test object than those who responded with some other pattern.

table(D$final_control_choice_conserv)
aov_control_choice_conserv_correct = aov(D$control_rating_ordered~D$final_control_choice_conserv,
                                         data = D)
summary(aov_control_choice_conserv_correct)

# both
mean(D$control_rating_ordered[D$final_control_choice_conserv=="Both"])
sd(D$control_rating_ordered[D$final_control_choice_conserv=="Both"])

# order consistent
mean(D$control_rating_ordered[D$final_control_choice_conserv=="Order Consistent"], na.rm = TRUE)
sd(D$control_rating_ordered[D$final_control_choice_conserv=="Order Consistent"], na.rm = TRUE)

# inverse 
mean(D$control_rating_ordered[D$final_control_choice_conserv=="Order Inconsistent"], na.rm = TRUE)
sd(D$control_rating_ordered[D$final_control_choice_conserv=="Order Inconsistent"], na.rm = TRUE)

# neither 
mean(D$control_rating_ordered[D$final_control_choice_conserv=="Neither"], na.rm = TRUE)
sd(D$control_rating_ordered[D$final_control_choice_conserv=="Neither"], na.rm = TRUE)

# follow-up t-tests
both_control_choice_correct = D$control_rating_ordered[D$final_control_choice_conserv=="Both"]
order_control_choice_correct = D$control_rating_ordered[D$final_control_choice_conserv=="Order Consistent"]
inverse_control_choice_correct = D$control_rating_ordered[D$final_control_choice_conserv=="Order Inconsistent"]
neither_control_choice_correct = D$control_rating_ordered[D$final_control_choice_conserv=="Neither"]

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded by choosing the order-consistent test object.
control_both_vs_order_consistent_t_test = t.test(both_control_choice_correct,
                                                 order_control_choice_correct,
                                                 alternative = "two.sided") 
control_both_vs_order_consistent_t_test

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded by choosing the order inconsistent test object
control_both_vs_inverse_t_test = t.test(both_control_choice_correct,
                                        inverse_control_choice_correct,
                                        alternative = "two.sided")
control_both_vs_inverse_t_test

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded "neither.
control_both_vs_neither_t_test = t.test(both_control_choice_correct,
                                        neither_control_choice_correct,
                                        alternative = "two.sided")
control_both_vs_neither_t_test

# compare the confidence ratings for the order consistent test object between those who responded by choosing the
# order consistent object and those who responded by choosing the order inconsistent test object
control_consistent_vs_inconsistent_t_test = t.test(order_control_choice_correct,
                                                   inverse_control_choice_correct,
                                                   alternative = "two.sided")
control_consistent_vs_inconsistent_t_test

# compare the confidence ratings for the order consistent test object between those who responded by choosing the 
# directionally consistent object and those
# who responded "neither.
control_direction_consistent_vs_neither_t_test = t.test(order_control_choice_correct,
                                                        neither_control_choice_correct,
                                                        alternative = "two.sided")
control_direction_consistent_vs_neither_t_test


# CONCLUSION: 


# liberal coding
table(D$control_choice_lib)
aov_control_choice_lib_inverse = aov(D$control_rating_inverse~D$contr_INCONSISTENTchoice,
                                     data = D)
summary(aov_control_choice_lib_inverse)

mean(D$control_rating_inverse[D$control_choice_lib=="Other"], na.rm = TRUE)
sd(D$control_rating_inverse[D$control_choice_lib=="Other"], na.rm = TRUE)

mean(D$control_rating_inverse[D$control_choice_lib=="Order Consistent"], na.rm = TRUE)
sd(D$control_rating_inverse[D$control_choice_lib=="Order Consistent"], na.rm = TRUE)

# CONCLUSION: Participants who chose the order consistent object were much less confident in their
# ratings of the inverse object test than those who chose the inverse object. Makes sense.


table(D$final_control_choice_conserv)
aov_control_choice_conserv_inverse = aov(D$control_rating_inverse~D$final_control_choice_conserv,
                                         data = D)
summary(aov_control_choice_conserv_inverse)

# both
mean(D$control_rating_inverse[D$final_control_choice_conserv=="Both"])
sd(D$control_rating_inverse[D$final_control_choice_conserv=="Both"])

# order consistent
mean(D$control_rating_inverse[D$final_control_choice_conserv=="Order Consistent"], na.rm = TRUE)
sd(D$control_rating_inverse[D$final_control_choice_conserv=="Order Consistent"], na.rm = TRUE)

# inverse 
mean(D$control_rating_inverse[D$final_control_choice_conserv=="Order Inconsistent"], na.rm = TRUE)
sd(D$control_rating_inverse[D$final_control_choice_conserv=="Order Inconsistent"], na.rm = TRUE)

# neither 
mean(D$control_rating_inverse[D$final_control_choice_conserv=="Neither"], na.rm = TRUE)
sd(D$control_rating_inverse[D$final_control_choice_conserv=="Neither"], na.rm = TRUE)

control_rating_inverse_barplot = ggplot(D, aes(final_control_choice_conserv,final_control_rating_inverse, fill = final_control_choice_conserv)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
control_rating_inverse_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Rating") + # change the label of the y-axis
  xlab("Control Test Trial Object Ratings for Order Inconsistent Responses") + 
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 100)) +
  scale_fill_manual(values=c("black","#ffc857", "dark blue", "purple")) +
  theme_bw()+
  theme(legend.position = "none")

# follow-up t-tests
both_control_choice_inverse = D$control_rating_inverse[D$final_control_choice_conserv=="Both"]
order_consistent_control_choice_inverse = D$control_rating_inverse[D$final_control_choice_conserv=="Order Consistent"]
inverse_control_choice_inverse = D$control_rating_inverse[D$final_control_choice_conserv=="Order Inconsistent"]
neither_control_choice_inverse = D$control_rating_inverse[D$final_control_choice_conserv=="Neither"]

# compare the confidence ratings for the order inconsistent test object between those who responded "both" and those
# who responded by choosing the order-consistent test object.
control_both_vs_order_consistent_inverse_t_test = t.test(both_control_choice_inverse,
                                                         order_consistent_control_choice_inverse,
                                                         alternative = "two.sided") 
control_both_vs_order_consistent_inverse_t_test

# compare the confidence ratings for the order inconsitent test object between those who responded "both" and those
# who responded by choosing the order-inconsistent test object
control_both_vs_order_inconsistent_inverse_t_test = t.test(both_control_choice_inverse,
                                                           inverse_control_choice_inverse,
                                                           alternative = "two.sided")
control_both_vs_order_inconsistent_inverse_t_test

# compare the confidence ratings for the order inconsistent test object between those who responded "both" and those
# who responded "neither.
control_both_vs_neither_inverse_t_test = t.test(both_control_choice_inverse,
                                                neither_control_choice_inverse,
                                                alternative = "two.sided")
control_both_vs_neither_inverse_t_test

# compare the confidence ratings for the order inconsistent test object between those who responded 
# "order consistent" and those who responded "order inconsistent"
control_order_consistent_vs_order_inconsistent_inverse_t_test = t.test(order_consistent_control_choice_inverse,
                                                                       inverse_control_choice_inverse,
                                                                       alternative = "two.sided")
control_order_consistent_vs_order_inconsistent_inverse_t_test

# compare the confidence ratings for the order inconsistent test object between those who responded 
# "order consistent" and those who responded "neither.
control_order_consistent_vs_neither_inverse_t_test = t.test(order_consistent_control_choice_inverse,
                                                            neither_control_choice_inverse,
                                                            alternative = "two.sided")
control_order_consistent_vs_neither_inverse_t_test

# CONCLUSION: 

ggplot() +
  geom_density(data = D, aes(x = test_rating_correct), alpha = 0.5) +
  labs(x = "Object Rating", y = "Density", title = "Distribution of SOC-Consistent Object Rating in SOC Trial") +
  scale_fill_manual(values = c("#ffc857")) +
  theme_minimal()

ggplot() +
  geom_density(data = D, aes(x = direction_rating_SOCconsistent), alpha = 0.5) +
  labs(x = "Object Rating", y = "Density", title = "Distribution of Order-Consistent Object Rating in Direction Trial") +
  scale_fill_manual(values = c("#ffc857")) +
  theme_minimal()

ggplot() +
  geom_density(data = D, aes(x = control_rating_ordered), alpha = 0.5) +
  labs(x = "Object Rating", y = "Density", title = "Distribution of Order-Consistent Object Rating in Control Trial") +
  scale_fill_manual(values = c("#ffc857")) +
  theme_minimal()

### compare the distributional difference between those who were not reprompted and reprompted
IntuitSOC <- table(D_HASintuit$final_test_choice)
IntuitSOC
NoIntuitSOC <- table(D_NOintuit$final_test_choice)
NoIntuitSOC
Intuit_SOC_prop_results <- prop.test(IntuitSOC, NoIntuitSOC)
Intuit_SOC_prop_results

IntuitDir <- table(D_HASintuit$final_direction_choice_conserv)
IntuitDir
NoIntuitDir <- table(D_NOintuit$final_direction_choice_conserv)
NoIntuitDir

table(D_HASintuit$final_control_choice_conserv)
table(D_NOintuit$final_control_choice_conserv)

######### REPROMPTED PARTICIPANTS
# test_choice
table(D_HASintuit$test_choice)
baseline_test_sucess_prob = table(D_HASintuit$test_choice)[[1]]/(table(D_HASintuit$test_choice)[[1]]+table(D_HASintuit$test_choice)[[2]]+table(D_HASintuit$test_choice)[[3]])
baseline_test_sucess_prob
baseline_test_success_odds = baseline_test_sucess_prob/(1-baseline_test_sucess_prob) # this is what will be shown
baseline_test_success_odds 

# final_test_choice
table(D_HASintuit$final_test_choice)
baseline_test_sucess_prob = table(D_HASintuit$final_test_choice)[[1]]/(table(D_HASintuit$final_test_choice)[[1]]+table(D_HASintuit$final_test_choice)[[2]])
baseline_test_sucess_prob
baseline_test_success_odds = baseline_test_sucess_prob/(1-baseline_test_sucess_prob) # this is what will be shown
baseline_test_success_odds

# direction_choice
table(D_HASintuit$direction_choice_conserv)
baseline_direction_ordered_conserv_prob = table(D_HASintuit$direction_choice_conserv)[[2]]/(table(D_HASintuit$direction_choice_conserv)[[1]]+
                                                                                    table(D_HASintuit$direction_choice_conserv)[[2]]+
                                                                                    table(D_HASintuit$direction_choice_conserv)[[3]]+
                                                                                    table(D_HASintuit$direction_choice_conserv)[[4]]+
                                                                                    table(D_HASintuit$direction_choice_conserv)[[5]])
baseline_direction_ordered_conserv_prob 
baseline_direction_ordered__conserv_odds = baseline_direction_ordered_conserv_prob/(1-baseline_direction_ordered_conserv_prob) # this is what will be shown
baseline_direction_ordered__conserv_odds

table(D_HASintuit$final_direction_choice_conserv)
final_baseline_direction_ordered_conserv_prob = table(D_HASintuit$final_direction_choice_conserv)[[2]]/(table(D_HASintuit$final_direction_choice_conserv)[[1]]+
                                                                                                table(D_HASintuit$final_direction_choice_conserv)[[2]]+
                                                                                                table(D_HASintuit$final_direction_choice_conserv)[[3]]+
                                                                                                table(D_HASintuit$final_direction_choice_conserv)[[4]])
final_baseline_direction_ordered_conserv_prob 
final_baseline_direction_ordered__conserv_odds = final_baseline_direction_ordered_conserv_prob/(1-final_baseline_direction_ordered_conserv_prob) # this is what will be shown
final_baseline_direction_ordered__conserv_odds

# control_choice
table(D_HASintuit$control_choice_conserv)
baseline_control_ordered_conserv_prob = table(D_HASintuit$control_choice_conserv)[[2]]/(table(D_HASintuit$control_choice_conserv)[[1]]+
                                                                                table(D_HASintuit$control_choice_conserv)[[2]]+
                                                                                table(D_HASintuit$control_choice_conserv)[[3]]+
                                                                                table(D_HASintuit$control_choice_conserv)[[4]]+
                                                                                table(D_HASintuit$control_choice_conserv)[[5]])
baseline_control_ordered_conserv_prob
baseline_control_ordered_conserv_odds = baseline_control_ordered_conserv_prob/(1-baseline_control_ordered_conserv_prob) # this is what will be shown
baseline_control_ordered_conserv_odds

table(D_HASintuit$final_control_choice_conserv)
baseline_final_control_ordered_conserv_prob = table(D_HASintuit$final_control_choice_conserv)[[2]]/(table(D_HASintuit$final_control_choice_conserv)[[1]]+
                                                                                            table(D_HASintuit$final_control_choice_conserv)[[2]]+
                                                                                            table(D_HASintuit$final_control_choice_conserv)[[3]]+
                                                                                            table(D_HASintuit$final_control_choice_conserv)[[4]])
baseline_final_control_ordered_conserv_prob
baseline_final_control_ordered_conserv_odds = baseline_final_control_ordered_conserv_prob/(1-baseline_final_control_ordered_conserv_prob) # this is what will be shown
baseline_final_control_ordered_conserv_odds

## Follow-up t-tests ##

## assess whether participants were more likely to provide higher (or even lower) ##
## ratings for the correct and incorrect objects during the test trial ##

# correct ratings
mean_test_rating_correct = mean(D_HASintuit$test_rating_correct, na.rm = TRUE)
mean_test_rating_correct
sd_test_rating_correct = sd(D_HASintuit$test_rating_correct, na.rm = TRUE)
sd_test_rating_correct

final_mean_test_rating_correct = mean(D_HASintuit$final_test_rating_correct, na.rm = TRUE)
final_mean_test_rating_correct
final_sd_test_rating_correct = sd(D_HASintuit$final_test_rating_correct, na.rm = TRUE)
final_sd_test_rating_correct

# incorrect ratings
mean_test_rating_incorrect = mean(D_HASintuit$test_rating_incorrect, na.rm = TRUE)
mean_test_rating_incorrect
sd_test_rating_incorrect = sd(D_HASintuit$test_rating_incorrect, na.rm = TRUE)
sd_test_rating_incorrect

final_mean_test_rating_incorrect = mean(D_HASintuit$final_test_rating_incorrect, na.rm = TRUE)
final_mean_test_rating_incorrect
final_sd_test_rating_incorrect = sd(D_HASintuit$final_test_rating_incorrect, na.rm = TRUE)
final_sd_test_rating_incorrect

t_test_test_trial_ratings = t.test(D_HASintuit$test_rating_correct,
                                   D_HASintuit$test_rating_incorrect, paired = TRUE,
                                   alternative = "two.sided")
t_test_test_trial_ratings

# Examine whether there is an effect of whether participants chose the SOC object during
# the replication trial (i.e., D_HASintuit$test_choice) on their ratings of the correct (i.e., D_HASintuit$test_rating_correct)
# & incorrect objects.
aov_test_choice_correct = aov(test_rating_correct~final_test_choice,
                              data = D_HASintuit)
summary(aov_test_choice_correct)

# rating of correct object for those who chose correctly
mean(D_HASintuit$test_rating_correct[D_HASintuit$final_test_choice=="Correct"], na.rm = TRUE)
sd(D_HASintuit$test_rating_correct[D_HASintuit$final_test_choice=="Correct"], na.rm = TRUE)

# rating of correct object for those who chose incorrectly
mean(D_HASintuit$test_rating_correct[D_HASintuit$final_test_choice=="Incorrect"], na.rm = TRUE)
sd(D_HASintuit$test_rating_correct[D_HASintuit$final_test_choice=="Incorrect"], na.rm = TRUE)

# CONCLUSION:
# 

aov_test_choice_incorrect = aov(test_rating_incorrect~final_test_choice,
                                data = D_HASintuit)
summary(aov_test_choice_incorrect)

# rating of incorrect object for those who chose correctly
mean(D$test_rating_incorrect[D_HASintuit$final_test_choice=="Correct"], na.rm = TRUE)
sd(D$test_rating_incorrect[D_HASintuit$final_test_choice=="Correct"], na.rm = TRUE)

# rating of incorrect object for those who chose incorrectly
mean(D$test_rating_incorrect[D_HASintuit$final_test_choice=="Incorrect"], na.rm = TRUE)
sd(D$test_rating_incorrect[D_HASintuit$final_test_choice=="Incorrect"], na.rm = TRUE)

# CONCLUSION:


# Examine whether there is an effect of whether participants chose the direction-consistent object during
# the direction trial (i.e., D$direction_choice_lib or D$direction_choice_conserv) for both 
# the liberal and conservative codings
# on their ratings of the direction consistent and inverse objects (D$direction_rating_SOCconsistent or 
# D$direction_rating_inverse)
# & incorrect objects.

table(D_HASintuit$direction_choice_lib)
aov_direction_choice_lib_correct = aov(final_direction_rating_SOCconsistent~final_direction_choice_lib,
                                       data = D_HASintuit)
summary(aov_direction_choice_lib_correct)

# other
mean(D_HASintuit$direction_rating_SOCconsistent[D_HASintuit$direction_choice_lib=="Other"], na.rm = TRUE)
sd(D_HASintuit$direction_rating_SOCconsistent[D_HASintuit$direction_choice_lib=="Other"], na.rm = TRUE)

# order consistent
mean(D_HASintuit$final_direction_rating_SOCconsistent[D_HASintuit$final_direction_choice_lib=="Order Consistent"], na.rm = TRUE)
sd(D_HASintuit$final_direction_rating_SOCconsistent[D_HASintuit$final_direction_choice_lib=="Order Consistent"], na.rm = TRUE)

# CONCLUSION: 


table(D_HASintuit$final_direction_choice_conserv)
aov_direction_choice_conserv_correct = aov(final_direction_rating_SOCconsistent~final_direction_choice_conserv,
                                           data = D_HASintuit)
summary(aov_direction_choice_conserv_correct)

# both
mean(D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Both"], na.rm = TRUE)
sd(D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Both"], na.rm = TRUE)

# order consistent
mean(D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Order Consistent"], na.rm = TRUE)
sd(D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Order Consistent"], na.rm = TRUE)

# inverse 
mean(D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Inverse"], na.rm = TRUE)
sd(D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Inverse"], na.rm = TRUE)

# neither 
mean(D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Neither"], na.rm = TRUE)
sd(D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Neither"], na.rm = TRUE)

# follow-up t-tests
both_direction_choice_correct = D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Both"]
order_consistent_direction_choice_correct = D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Order Consistent"]
inverse_direction_choice_correct = D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Inverse"]
neither_direction_choice_correct = D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Neither"]

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded by choosing the order-consistent test object.
both_vs_order_consistent_t_test = t.test(both_direction_choice_correct,
                                         order_consistent_direction_choice_correct,
                                         alternative = "two.sided") 
both_vs_order_consistent_t_test

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded by choosing the order-inconsistent test object
both_vs_inverse_t_test = t.test(both_direction_choice_correct, 
                                inverse_direction_choice_correct,
                                alternative = "two.sided")
both_vs_inverse_t_test

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


# CONCLUSION: 



# liberal coding
table(D$direction_choice_lib)
aov_direction_choice_lib_inverse = aov(D$direction_rating_inverse~D$direction_choice_lib,
                                       data = D)
summary(aov_direction_choice_lib_inverse)

mean(D$direction_rating_inverse[D$direction_choice_lib=="Other"], na.rm = TRUE)
sd(D$direction_rating_inverse[D$direction_choice_lib=="Other"], na.rm = TRUE)

mean(D$direction_rating_inverse[D$direction_choice_lib=="Order Consistent"], na.rm = TRUE)
sd(D$direction_rating_inverse[D$direction_choice_lib=="Order Consistent"], na.rm = TRUE)

# CONCLUSION: 


table(D$final_direction_choice_conserv)
aov_direction_choice_conserv_inverse = aov(D$direction_rating_inverse~D$final_direction_choice_conserv,
                                           data = D)
summary(aov_direction_choice_conserv_inverse)

# both
mean(D$direction_rating_inverse[D$final_direction_choice_conserv=="Both"], na.rm = TRUE)
sd(D$direction_rating_inverse[D$final_direction_choice_conserv=="Both"], na.rm = TRUE)

# order consistent
mean(D$direction_rating_inverse[D$final_direction_choice_conserv=="Order Consistent"])
sd(D$direction_rating_inverse[D$final_direction_choice_conserv=="Order Consistent"])

# inverse 
mean(D$direction_rating_inverse[D$final_direction_choice_conserv=="Inverse"])
sd(D$direction_rating_inverse[D$final_direction_choice_conserv=="Inverse"])

# neither 
mean(D$direction_rating_inverse[D$final_direction_choice_conserv=="Neither"])
sd(D$direction_rating_inverse[D$final_direction_choice_conserv=="Neither"])

# follow-up t-tests
both_direction_choice_inverse = D$direction_rating_inverse[D$final_direction_choice_conserv=="Both"]
order_consistent_direction_choice_inverse = D$direction_rating_inverse[D$final_direction_choice_conserv=="Order Consistent"]
inverse_direction_choice_inverse = D$direction_rating_inverse[D$final_direction_choice_conserv=="Inverse"]
neither_direction_choice_inverse = D$direction_rating_inverse[D$final_direction_choice_conserv=="Neither"]

# compare the confidence ratings for the order inconsistent test object between those who responded "both" and those
# who responded by choosing the order-consistent test object.
both_vs_order_consistent_inverse_t_test = t.test(both_direction_choice_inverse,
                                                 order_consistent_direction_choice_inverse,
                                                 alternative = "two.sided") 
both_vs_order_consistent_inverse_t_test

# compare the confidence ratings for the order inconsistent test object between those who responded "both" and those
# who responded by choosing the order-inconsistent test object
both_vs_inverse_inverse_t_test = t.test(both_direction_choice_inverse, 
                                        inverse_direction_choice_inverse,
                                        alternative = "two.sided")
both_vs_inverse_inverse_t_test

# compare the confidence ratings for the order inconsistent test object between those who responded "both" and those
# who responded "neither.
both_vs_neither_inverse_t_test = t.test(both_direction_choice_inverse,
                                        neither_direction_choice_inverse,
                                        alternative = "two.sided")
both_vs_neither_inverse_t_test


# compare the confidence ratings for the order consistent test object between those who responded by choosing the 
# directionally consistent object and those
# who responded "neither.
direction_consistent_vs_inverse_inverse_t_test = t.test(order_consistent_direction_choice_inverse,
                                                        inverse_direction_choice_inverse,
                                                        alternative = "two.sided")
direction_consistent_vs_inverse_inverse_t_test

# compare the confidence ratings for the order consistent test object between those who responded by choosing the 
# directionally consistent object and those
# who responded "neither.
direction_consistent_vs_neither_inverse_t_test = t.test(order_consistent_direction_choice_inverse,
                                                        neither_direction_choice_inverse,
                                                        alternative = "two.sided")
direction_consistent_vs_neither_inverse_t_test


# CONCLUSION: 


# Examine whether there is an effect of whether participants chose the direction-consistent object during
# the control trial for both the liberal and conservative codings
# on their ratings of the control_ordered and inverse object.
table(D$control_choice_lib)
aov_control_choice_lib_correct = aov(D$control_rating_ordered~D$control_choice_lib,
                                     data = D)
summary(aov_control_choice_lib_correct)

mean(D$control_rating_ordered[D$control_choice_lib=="Order Consistent"], na.rm = TRUE)
sd(D$control_rating_ordered[D$control_choice_lib=="Order Consistent"], na.rm = TRUE)
mean(D$control_rating_ordered[D$control_choice_lib=="Other"], na.rm = TRUE)
sd(D$control_rating_ordered[D$control_choice_lib=="Other"], na.rm = TRUE)

# CONCLUSION: Participants who chose the order consistent object were more confident in their
# ratings of the direction-consistent test object than those who responded with some other pattern.

table(D$final_control_choice_conserv)
aov_control_choice_conserv_correct = aov(D$control_rating_ordered~D$final_control_choice_conserv,
                                         data = D)
summary(aov_control_choice_conserv_correct)

# both
mean(D$control_rating_ordered[D$final_control_choice_conserv=="Both"])
sd(D$control_rating_ordered[D$final_control_choice_conserv=="Both"])

# order consistent
mean(D$control_rating_ordered[D$final_control_choice_conserv=="Order Consistent"], na.rm = TRUE)
sd(D$control_rating_ordered[D$final_control_choice_conserv=="Order Consistent"], na.rm = TRUE)

# inverse 
mean(D$control_rating_ordered[D$final_control_choice_conserv=="Order Inconsistent"], na.rm = TRUE)
sd(D$control_rating_ordered[D$final_control_choice_conserv=="Order Inconsistent"], na.rm = TRUE)

# neither 
mean(D$control_rating_ordered[D$final_control_choice_conserv=="Neither"], na.rm = TRUE)
sd(D$control_rating_ordered[D$final_control_choice_conserv=="Neither"], na.rm = TRUE)

# follow-up t-tests
both_control_choice_correct = D$control_rating_ordered[D$final_control_choice_conserv=="Both"]
order_control_choice_correct = D$control_rating_ordered[D$final_control_choice_conserv=="Order Consistent"]
inverse_control_choice_correct = D$control_rating_ordered[D$final_control_choice_conserv=="Order Inconsistent"]
neither_control_choice_correct = D$control_rating_ordered[D$final_control_choice_conserv=="Neither"]

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded by choosing the order-consistent test object.
control_both_vs_order_consistent_t_test = t.test(both_control_choice_correct,
                                                 order_control_choice_correct,
                                                 alternative = "two.sided") 
control_both_vs_order_consistent_t_test

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded by choosing the order inconsistent test object
control_both_vs_inverse_t_test = t.test(both_control_choice_correct,
                                        inverse_control_choice_correct,
                                        alternative = "two.sided")
control_both_vs_inverse_t_test

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded "neither.
control_both_vs_neither_t_test = t.test(both_control_choice_correct,
                                        neither_control_choice_correct,
                                        alternative = "two.sided")
control_both_vs_neither_t_test

# compare the confidence ratings for the order consistent test object between those who responded by choosing the
# order consistent object and those who responded by choosing the order inconsistent test object
control_consistent_vs_inconsistent_t_test = t.test(order_control_choice_correct,
                                                   inverse_control_choice_correct,
                                                   alternative = "two.sided")
control_consistent_vs_inconsistent_t_test

# compare the confidence ratings for the order consistent test object between those who responded by choosing the 
# directionally consistent object and those
# who responded "neither.
control_direction_consistent_vs_neither_t_test = t.test(order_control_choice_correct,
                                                        neither_control_choice_correct,
                                                        alternative = "two.sided")
control_direction_consistent_vs_neither_t_test


# CONCLUSION: 


# liberal coding
table(D$control_choice_lib)
aov_control_choice_lib_inverse = aov(D$control_rating_inverse~D$control_choice_lib,
                                     data = D)
summary(aov_control_choice_lib_inverse)

mean(D$control_rating_inverse[D$control_choice_lib=="Other"], na.rm = TRUE)
sd(D$control_rating_inverse[D$control_choice_lib=="Other"], na.rm = TRUE)

mean(D$control_rating_inverse[D$control_choice_lib=="Order Consistent"], na.rm = TRUE)
sd(D$control_rating_inverse[D$control_choice_lib=="Order Consistent"], na.rm = TRUE)

# CONCLUSION: Participants who chose the order consistent object were much less confident in their
# ratings of the inverse object test than those who chose the inverse object. Makes sense.


table(D$final_control_choice_conserv)
aov_control_choice_conserv_inverse = aov(D$control_rating_inverse~D$final_control_choice_conserv,
                                         data = D)
summary(aov_control_choice_conserv_inverse)

# both
mean(D$control_rating_inverse[D$final_control_choice_conserv=="Both"])
sd(D$control_rating_inverse[D$final_control_choice_conserv=="Both"])

# order consistent
mean(D$control_rating_inverse[D$final_control_choice_conserv=="Order Consistent"], na.rm = TRUE)
sd(D$control_rating_inverse[D$final_control_choice_conserv=="Order Consistent"], na.rm = TRUE)

# inverse 
mean(D$control_rating_inverse[D$final_control_choice_conserv=="Order Inconsistent"], na.rm = TRUE)
sd(D$control_rating_inverse[D$final_control_choice_conserv=="Order Inconsistent"], na.rm = TRUE)

# neither 
mean(D$control_rating_inverse[D$final_control_choice_conserv=="Neither"], na.rm = TRUE)
sd(D$control_rating_inverse[D$final_control_choice_conserv=="Neither"], na.rm = TRUE)

# follow-up t-tests
both_control_choice_inverse = D$control_rating_inverse[D$final_control_choice_conserv=="Both"]
order_consistent_control_choice_inverse = D$control_rating_inverse[D$final_control_choice_conserv=="Order Consistent"]
inverse_control_choice_inverse = D$control_rating_inverse[D$final_control_choice_conserv=="Order Inconsistent"]
neither_control_choice_inverse = D$control_rating_inverse[D$final_control_choice_conserv=="Neither"]

# compare the confidence ratings for the order inconsistent test object between those who responded "both" and those
# who responded by choosing the order-consistent test object.
control_both_vs_order_consistent_inverse_t_test = t.test(both_control_choice_inverse,
                                                         order_consistent_control_choice_inverse,
                                                         alternative = "two.sided") 
control_both_vs_order_consistent_inverse_t_test

# compare the confidence ratings for the order inconsitent test object between those who responded "both" and those
# who responded by choosing the order-inconsistent test object
control_both_vs_order_inconsistent_inverse_t_test = t.test(both_control_choice_inverse,
                                                           inverse_control_choice_inverse,
                                                           alternative = "two.sided")
control_both_vs_order_inconsistent_inverse_t_test

# compare the confidence ratings for the order inconsistent test object between those who responded "both" and those
# who responded "neither.
control_both_vs_neither_inverse_t_test = t.test(both_control_choice_inverse,
                                                neither_control_choice_inverse,
                                                alternative = "two.sided")
control_both_vs_neither_inverse_t_test

# compare the confidence ratings for the order inconsistent test object between those who responded 
# "order consistent" and those who responded "order inconsistent"
control_order_consistent_vs_order_inconsistent_inverse_t_test = t.test(order_consistent_control_choice_inverse,
                                                                       inverse_control_choice_inverse,
                                                                       alternative = "two.sided")
control_order_consistent_vs_order_inconsistent_inverse_t_test

# compare the confidence ratings for the order inconsistent test object between those who responded 
# "order consistent" and those who responded "neither.
control_order_consistent_vs_neither_inverse_t_test = t.test(order_consistent_control_choice_inverse,
                                                            neither_control_choice_inverse,
                                                            alternative = "two.sided")
control_order_consistent_vs_neither_inverse_t_test


######### NO REPROMPT PARTICIPANTS
# test_choice
table(D_NOintuit$final_test_choice)
baseline_test_sucess_prob = table(D_NOintuit$final_test_choice)[[1]]/(table(D_NOintuit$final_test_choice)[[1]]+table(D_NOintuit$final_test_choice)[[2]])
baseline_test_sucess_prob
baseline_test_success_odds = baseline_test_sucess_prob/(1-baseline_test_sucess_prob) # this is what will be shown
baseline_test_success_odds

# direction_choice
table(D_NOintuit$final_direction_choice_conserv)
final_baseline_direction_ordered_conserv_prob = table(D_NOintuit$final_direction_choice_conserv)[[2]]/(table(D_NOintuit$final_direction_choice_conserv)[[1]]+
                                                                                                          table(D_NOintuit$final_direction_choice_conserv)[[2]]+
                                                                                                          table(D_NOintuit$final_direction_choice_conserv)[[3]]+
                                                                                                          table(D_NOintuit$final_direction_choice_conserv)[[4]])
final_baseline_direction_ordered_conserv_prob 
final_baseline_direction_ordered__conserv_odds = final_baseline_direction_ordered_conserv_prob/(1-final_baseline_direction_ordered_conserv_prob) # this is what will be shown
final_baseline_direction_ordered__conserv_odds

# control_choice
table(D_NOintuit$final_control_choice_conserv)
baseline_final_control_ordered_conserv_prob = table(D_NOintuit$final_control_choice_conserv)[[2]]/(table(D_NOintuit$final_control_choice_conserv)[[1]]+
                                                                                                      table(D_NOintuit$final_control_choice_conserv)[[2]]+
                                                                                                      table(D_NOintuit$final_control_choice_conserv)[[3]]+
                                                                                                      table(D_NOintuit$final_control_choice_conserv)[[4]])
baseline_final_control_ordered_conserv_prob
baseline_final_control_ordered_conserv_odds = baseline_final_control_ordered_conserv_prob/(1-baseline_final_control_ordered_conserv_prob) # this is what will be shown
baseline_final_control_ordered_conserv_odds

######### REPROMPT PARTICIPANTS
# REPROMPTED ON SOC TEST TRIAL
D_HASintuit_SOC <- D[!is.na(D$intuit_test_rating_correct),]
D_withoutSOCintuit <- anti_join(D, D_HASintuit_SOC, by = "ID")
table(D_HASintuit_SOC$test_choice)
# replication 1: 9 unsure SOC

table(D_HASintuit_SOC$final_test_choice)
baseline_test_sucess_prob = table(D_HASintuit_SOC$final_test_choice)[[2]]/(table(D_HASintuit_SOC$final_test_choice)[[1]]+table(D_HASintuit_SOC$final_test_choice)[[2]])
baseline_test_sucess_prob
baseline_test_success_odds = baseline_test_sucess_prob/(1-baseline_test_sucess_prob) # this is what will be shown
baseline_test_success_odds
# replication 1 SOC odds 1.25
# replication 2 SOC OR 1.2

D_HASintuit_SOC$final_test_choice = relevel(D_HASintuit_SOC$final_test_choice, ref="Incorrect")
glm_final_test_choice = glm(final_test_choice~1, 
                            data = D_HASintuit_SOC, 
                            family = "binomial")
summary(glm_final_test_choice)
exp(glm_final_test_choice$coefficients)
exp(confint(glm_final_test_choice))

table(D_withoutSOCintuit$test_choice)
baseline_test_sucess_prob = table(D_withoutSOCintuit$final_test_choice)[[2]]/(table(D_withoutSOCintuit$final_test_choice)[[1]]+table(D_withoutSOCintuit$final_test_choice)[[2]])
baseline_test_sucess_prob
baseline_test_success_odds = baseline_test_sucess_prob/(1-baseline_test_sucess_prob) # this is what will be shown
baseline_test_success_odds

D_withoutSOCintuit$final_test_choice = relevel(D_withoutSOCintuit$final_test_choice, ref="Incorrect")
glm_final_test_choice = glm(final_test_choice~1, 
                            data = D_withoutSOCintuit, 
                            family = "binomial")
summary(glm_final_test_choice)
exp(glm_final_test_choice$coefficients)
exp(confint(glm_final_test_choice))

# REPROMPTED ON DIRECTION TEST TRIAL
D_HASintuit_DIR <- D_HASintuit[!is.na(D_HASintuit$intuit_direction_rating_SOCconsistent),]
table(D_HASintuit_DIR$direction_choice_conserv)

table(D_HASintuit_DIR$final_direction_choice_conserv)
final_baseline_direction_ordered_conserv_prob = table(D_HASintuit_DIR$final_direction_choice_conserv)[[2]]/(table(D_HASintuit_DIR$final_direction_choice_conserv)[[1]]+
                                                                                                         table(D_HASintuit_DIR$final_direction_choice_conserv)[[2]]+
                                                                                                         table(D_HASintuit_DIR$final_direction_choice_conserv)[[3]]+
                                                                                                         table(D_HASintuit_DIR$final_direction_choice_conserv)[[4]])
final_baseline_direction_ordered_conserv_prob 
final_baseline_direction_ordered__conserv_odds = final_baseline_direction_ordered_conserv_prob/(1-final_baseline_direction_ordered_conserv_prob) # this is what will be shown
final_baseline_direction_ordered__conserv_odds
# replication 1 OR = 0.75
# replication 2 OR = 0.09090909

table(D_HASintuit_DIR$final_direction_choice_lib)
D_HASintuit_DIR$final_direction_choice_lib = relevel(D_HASintuit_DIR$final_direction_choice_lib, ref="Other")
glm_direction_choice_lib = glm(final_direction_choice_lib~1,
                               data = D_HASintuit_DIR, family = "binomial")
summary(glm_direction_choice_lib)
exp(glm_direction_choice_lib$coefficients)
exp(confint(glm_direction_choice_lib))

# REPROMPTED ON CONTROL TEST TRIAL
D_HASintuit_CON <- D_HASintuit[!is.na(D_HASintuit$intuit_control_rating_ordered),]
table(D_HASintuit_CON$control_choice_conserv)

baseline_final_control_ordered_conserv_prob = table(D_HASintuit_CON$final_control_choice_conserv)[[2]]/(table(D_HASintuit_CON$final_control_choice_conserv)[[1]]+
                                                                                                     table(D_HASintuit_CON$final_control_choice_conserv)[[2]]+
                                                                                                     table(D_HASintuit_CON$final_control_choice_conserv)[[3]]+
                                                                                                     table(D_HASintuit_CON$final_control_choice_conserv)[[4]])
baseline_final_control_ordered_conserv_prob
baseline_final_control_ordered_conserv_odds = baseline_final_control_ordered_conserv_prob/(1-baseline_final_control_ordered_conserv_prob) # this is what will be shown
baseline_final_control_ordered_conserv_odds
# replication 1 OR = 0.4285714
table(D_HASintuit_CON$final_control_choice_conserv)

table(D_HASintuit_CON$final_control_choice_lib)
D_HASintuit_CON$final_control_choice_lib = relevel(D_HASintuit_CON$final_control_choice_lib, ref="Other")
glm_control_choice_lib = glm(final_control_choice_lib~1,
                               data = D_HASintuit_CON, family = "binomial")
summary(glm_control_choice_lib)
exp(glm_control_choice_lib$coefficients)
exp(confint(glm_control_choice_lib))

t_test_reprompt_test = t.test(D_HASintuit_SOC$test_rating_correct,
                              D_HASintuit_SOC$intuit_test_rating_correct, paired = TRUE,
                              alternative = "two.sided")
t_test_reprompt_test

t_test_reprompt_test = t.test(D_HASintuit_DIR$direction_rating_SOCconsistent,
                              D_HASintuit_DIR$intuit_direction_rating_SOCconsistent, paired = TRUE,
                              alternative = "two.sided")
t_test_reprompt_test

t_test_reprompt_test = t.test(D_HASintuit_CON$control_rating_ordered,
                              D_HASintuit_CON$intuit_control_rating_ordered, paired = TRUE,
                              alternative = "two.sided")
t_test_reprompt_test
# Create density plots of Correct Object Ratings in SOC Trial
ggplot(D_HASintuit) +
  geom_density(aes(x = test_rating_correct, colour = "Original SOC Object Rating"), lwd = 1.2, linetype = 1) +
  geom_density(aes(x = final_test_rating_correct, colour = "Re-Prompted SOC \nObject Rating"), lwd = 1.2, linetype = 1) +
  scale_color_manual(values = c("#ffc857", "purple")) +
  labs(x = "SOC Object Rating", 
       y = "Density", 
       title = "Density Plot of SOC Consistent Object Ratings \nand Re-Prompted Object Ratings") +
  theme_minimal()

plot(density(D_HASintuit$test_rating_correct), col = "#ffc857", lwd = 2, main = "Density Plot of Correct Object Ratings in SOC Trial")
lines(density(D_HASintuit$final_test_rating_correct), col = "purple", lwd = 2)
legend("topright", c("Test Rating", "Final Test Rating"), col = c("#ffc857", "purple"), lwd = 2, cex = 0.75)

# Create boxplots of Correct Object Ratings in SOC Trial
boxplot(D_HASintuit$test_rating_correct, D_HASintuit$final_test_rating_correct, col = c("#ffc857", "purple"), 
        main = "Boxplot of Correct Object Ratings in SOC Trial", 
        names = c("Test Rating", "Final Test Rating"))

# Create density plots of Incorrect Object Ratings in SOC Trial
plot(density(D_HASintuit$test_rating_incorrect), col = "#ffc857", lwd = 2, main = "Density Plot of Incorrect Object Ratings in SOC Trial")
lines(density(D_HASintuit$final_test_rating_incorrect), col = "purple", lwd = 2)
legend("topright", c("Test Rating", "Final Test Rating"), col = c("#ffc857", "purple"), lwd = 2, cex = 0.75)

# Create boxplots of Incorrect Object Ratings in SOC Trial
boxplot(D_HASintuit$test_rating_incorrect, D_HASintuit$final_test_rating_incorrect, col = c("#ffc857", "purple"), 
        main = "Boxplot of Incorrect Object Ratings in SOC Trial", 
        names = c("Test Rating", "Final Test Rating"))

# Create density plots of Correct Object Ratings in Direction Trial
plot(density(D_HASintuit$direction_rating_SOCconsistent), col = "#ffc857", lwd = 2, main = "Density Plot of Consistent Object Ratings in Direction Trial")
lines(density(D_HASintuit$final_direction_rating_SOCconsistent), col = "purple", lwd = 2)
legend("topright", c("Test Rating", "Final Test Rating"), col = c("#ffc857", "purple"), lwd = 2, cex = 0.75)

# Create boxplots of Correct Object Ratings in Direction Trial
boxplot(D_HASintuit$direction_rating_SOCconsistent, D_HASintuit$final_direction_rating_SOCconsistent, col = c("#ffc857", "purple"), 
        main = "Boxplot of Order Consistent Object Ratings in Direction Trial", 
        names = c("Test Rating", "Final Test Rating"))

# Create density plots of Inverse Object Ratings in Direction Trial
plot(density(D_HASintuit$direction_rating_inverse), col = "#ffc857", lwd = 2, main = "Density Plot of Inverse Object Ratings in Direction Trial")
lines(density(D_HASintuit$final_direction_rating_inverse), col = "purple", lwd = 2)
legend("topright", c("Test Rating", "Final Test Rating"), col = c("#ffc857", "purple"), lwd = 2, cex = 0.75)

# Create boxplots of Inverse Object Ratings in Direction Trial
boxplot(D_HASintuit$direction_rating_inverse, D_HASintuit$final_direction_rating_inverse, col = c("#ffc857", "purple"), 
        main = "Boxplot of Inverse Object Ratings in Direction Trial", 
        names = c("Test Rating", "Final Test Rating"))

# Create density plots of Correct Object Ratings in Control Trial
plot(density(D_HASintuit$control_rating_ordered), col = "#ffc857", lwd = 2, main = "Density Plot of Consistent Object Ratings in Control Trial")
lines(density(D_HASintuit$final_control_rating_ordered), col = "purple", lwd = 2)
legend("topright", c("Test Rating", "Final Test Rating"), col = c("#ffc857", "purple"), lwd = 2, cex = 0.75)

ggplot(D_HASintuit) +
  geom_density(aes(x = control_rating_ordered, colour = "Original Order Consistent \nObject Rating"), lwd = 1.2, linetype = 1) +
  geom_density(aes(x = final_control_rating_ordered, colour = "Re-Prompted Order Consistent \nObject Rating"), lwd = 1.2, linetype = 1) +
  scale_color_manual(values = c("#ffc857", "purple")) +
  labs(x = "Order Consistent Object Rating", 
       y = "Density", 
       title = "Density Plot of Order Consistent Consistent Object Ratings \nand Re-Prompted Object Ratings in Control Trial") +
  theme_minimal()

# Create boxplots of Correct Object Ratings in Control Trial
boxplot(D_HASintuit$control_rating_ordered, D_HASintuit$final_control_rating_ordered, col = c("#ffc857", "purple"), 
        main = "Boxplot of Order Consistent Object Ratings in Control Trial", 
        names = c("Test Rating", "Final Test Rating"))

# Create density plots of Inverse Object Ratings in Control Trial
ggplot(D_HASintuit) +
  geom_density(aes(x = control_rating_inverse, colour = "Original Order Inconsistent \nObject Rating"), lwd = 1.2, linetype = 1) +
  geom_density(aes(x = final_control_rating_inverse, colour = "Re-Prompted Order Inconsistent \nObject Rating"), lwd = 1.2, linetype = 1) +
  scale_color_manual(values = c("#ffc857", "purple")) +
  labs(x = "Order Inconsistent Object Rating", 
       y = "Density", 
       title = "Density Plot of Order Inconsistent Consistent Object Ratings \nand Re-Prompted Object Ratings") +
  theme_minimal()

plot(density(D_HASintuit$control_rating_inverse), col = "#ffc857", lwd = 2, main = "Density Plot of Inverse Object Ratings in Control Trial")
lines(density(D_HASintuit$final_control_rating_inverse), col = "purple", lwd = 2)
legend("topright", c("Test Rating", "Final Test Rating"), col = c("#ffc857", "purple"), lwd = 2, cex = 0.75)

# Create boxplots of Inverse Object Ratings in Control Trial
boxplot(D_HASintuit$control_rating_inverse, D_HASintuit$final_control_rating_inverse, col = c("#ffc857", "purple"), 
        main = "Boxplot of Inverse Object Ratings in Control Trial", 
        names = c("Test Rating", "Final Test Rating"))

# Compare the density distribution between participants who were reprompted and those who were not in SOC Trial
ggplot() +
  geom_density(data = D_HASintuit, aes(x = final_test_rating_correct, fill = "Re-Prompted"), alpha = 0.5) +
  geom_density(data = D_NOintuit, aes(x = final_test_rating_correct, fill = "No Reprompt"), alpha = 0.5) +
  labs(x = "Object Rating", y = "Density", title = "Distributional Difference of Order-Consistent Object Rating \nin SOC Trial for Re-Prompted Participants") +
  scale_fill_manual(values = c("#ffc857", "purple")) +
  theme_minimal()

ggplot() +
  geom_density(data = D_HASintuit, aes(x = final_test_rating_incorrect, fill = "Re-Prompted"), alpha = 0.5) +
  geom_density(data = D_NOintuit, aes(x = final_test_rating_incorrect, fill = "No Reprompt"), alpha = 0.5) +
  labs(x = "Object Rating", y = "Density", title = "Distributional Difference of Order-Inconsistent Object Rating \nin SOC Trial for Re-Prompted Participants") +
  scale_fill_manual(values = c("#ffc857", "purple")) +
  theme_minimal()

ggplot() +
  geom_density(data = D_HASintuit, aes(x = final_direction_rating_SOCconsistent, fill = "Re-Prompted"), alpha = 0.5) +
  geom_density(data = D_NOintuit, aes(x = final_direction_rating_SOCconsistent, fill = "No Reprompt"), alpha = 0.5) +
  labs(x = "Object Rating", y = "Density", title = "Distributional Difference of Order-Consistent Object Rating \nin Direction Trial for Re-Prompted Participants") +
  scale_fill_manual(values = c("#ffc857", "purple")) +
  theme_minimal()

ggplot() +
  geom_density(data = D_HASintuit, aes(x = final_direction_rating_inverse, fill = "Re-Prompted"), alpha = 0.5) +
  geom_density(data = D_NOintuit, aes(x = final_direction_rating_inverse, fill = "No Reprompt"), alpha = 0.5) +
  labs(x = "Object Rating", y = "Density", title = "Distributional Difference of Order-Inconsistent Object Rating \nin Direction Trial for Re-Prompted Participants") +
  scale_fill_manual(values = c("#ffc857", "purple")) +
  theme_minimal()

ggplot() +
  geom_density(data = D_HASintuit, aes(x = final_control_rating_ordered, fill = "Re-Prompted"), alpha = 0.5) +
  geom_density(data = D_NOintuit, aes(x = final_control_rating_ordered, fill = "No Reprompt"), alpha = 0.5) +
  labs(x = "Object Rating", y = "Density", title = "Distributional Difference of Order-Consistent Object Rating \nin Control Trial for Re-Prompted Participants") +
  scale_fill_manual(values = c("#ffc857", "purple")) +
  theme_minimal()

ggplot() +
  geom_density(data = D_HASintuit, aes(x = final_control_rating_inverse, fill = "Re-Prompted"), alpha = 0.5) +
  geom_density(data = D_NOintuit, aes(x = final_control_rating_inverse, fill = "No Reprompt"), alpha = 0.5) +
  labs(x = "Object Rating", y = "Density", title = "Distributional Difference of Order-Inconsistent Object Rating \nin Control Trial for Re-Prompted Participants") +
  scale_fill_manual(values = c("#ffc857", "purple")) +
  theme_minimal()

####### BLICKETNESS OPEN-ENDED QUESTION - only for REPLICATION 2
# 1 = responded by saying blicketness was based on SHAPE and COLOR 
# 2 = some other response
table(D$blicketness) # 11:15
table(D_HASintuit$blicketness) # 6:7
table(D_NOintuit$blicketness) # 5:8

table(D$blicketness, D$final_test_choice)
table(D$blicketness, D$final_direction_choice_conserv)
table(D$blicketness, D$final_control_choice_conserv)

# while limited by the sample size, what we can learn from the open-ended item on blicket-ness is
# that those who responded by saying that a blicket is determined by shape/color combination 
# 1. 100% correct on SOC, and 2. significantly choosing Both in the relational order trials

# it is difficult to interpret the results of those who responded with other descriptions for 
# blicketness, as some responses discussed feature dimensions that were counterbalanced across 
# stimuli presentation during trainig and at test, while others discussed feature dimensions
# that were not counterbalanced but never shown during training.

D_CorrectBlicketness <- D[D$blicketness=="1",]
D_CorrectBlicketness <- subset(D_CorrectBlicketness, !is.na(D_CorrectBlicketness$blicketness))
D_OtherBlicketness <- D[D$blicketness=="2",]
D_OtherBlicketness <- subset(D_OtherBlicketness, !is.na(D_OtherBlicketness$blicketness))

# final_test_choice
table(D_CorrectBlicketness$final_test_choice)
baseline_test_sucess_prob = table(D_CorrectBlicketness$final_test_choice)[[2]]/(table(D_CorrectBlicketness$final_test_choice)[[1]]+table(D_CorrectBlicketness$final_test_choice)[[2]])
baseline_test_sucess_prob
baseline_test_success_odds = baseline_test_sucess_prob/(1-baseline_test_sucess_prob) # this is what will be shown
baseline_test_success_odds

D_CorrectBlicketness$final_test_choice = relevel(D$final_test_choice, ref="Correct")
glm_final_test_choice = glm(final_test_choice~1, 
                            data = D_CorrectBlicketness, 
                            family = "binomial")
summary(glm_final_test_choice)
exp(glm_final_test_choice$coefficients)
exp(confint(glm_final_test_choice))
    
# correct ratings
mean_test_rating_correct = mean(D_CorrectBlicketness$test_rating_correct, na.rm = TRUE)
mean_test_rating_correct
sd_test_rating_correct = sd(D_CorrectBlicketness$test_rating_correct, na.rm = TRUE)
sd_test_rating_correct

final_mean_test_rating_correct = mean(D_CorrectBlicketness$final_test_rating_correct, na.rm = TRUE)
final_mean_test_rating_correct
final_sd_test_rating_correct = sd(D_CorrectBlicketness$final_test_rating_correct, na.rm = TRUE)
final_sd_test_rating_correct

# incorrect ratings
mean_test_rating_incorrect = mean(D_CorrectBlicketness$test_rating_incorrect, na.rm = TRUE)
mean_test_rating_incorrect
sd_test_rating_incorrect = sd(D_CorrectBlicketness$test_rating_incorrect, na.rm = TRUE)
sd_test_rating_incorrect

final_mean_test_rating_incorrect = mean(D_CorrectBlicketness$final_test_rating_incorrect, na.rm = TRUE)
final_mean_test_rating_incorrect
final_sd_test_rating_incorrect = sd(D_CorrectBlicketness$final_test_rating_incorrect, na.rm = TRUE)
final_sd_test_rating_incorrect

t_test_test_trial_ratings = t.test(D_CorrectBlicketness$test_rating_correct,
                                   D_CorrectBlicketness$test_rating_incorrect, paired = TRUE,
                                   alternative = "two.sided")
t_test_test_trial_ratings

t_test_reprompt_test = t.test(D_CorrectBlicketness$test_rating_correct,
                              D_CorrectBlicketness$final_test_rating_correct, paired = TRUE,
                              alternative = "two.sided")
t_test_reprompt_test # not significant effect of reprompt

# Examine whether there is an effect of whether participants chose the SOC object during
# the replication trial (i.e., D_CorrectBlicketness$test_choice) on their ratings of the correct (i.e., D_CorrectBlicketness$test_rating_correct)
# & incorrect objects.
aov_test_choice_correct = aov(final_test_rating_correct~final_test_choice,
                              data = D_CorrectBlicketness)
summary(aov_test_choice_correct)

# rating of correct object for those who chose correctly
mean(D_CorrectBlicketness$test_rating_correct[D_CorrectBlicketness$final_test_choice=="Correct"], na.rm = TRUE)
sd(D_CorrectBlicketness$test_rating_correct[D_CorrectBlicketness$final_test_choice=="Correct"], na.rm = TRUE)

# rating of correct object for those who chose incorrectly
mean(D_CorrectBlicketness$test_rating_correct[D_CorrectBlicketness$final_test_choice=="Incorrect"], na.rm = TRUE)
sd(D_CorrectBlicketness$test_rating_correct[D_CorrectBlicketness$final_test_choice=="Incorrect"], na.rm = TRUE)

# CONCLUSION:
# 

aov_test_choice_incorrect = aov(final_test_rating_incorrect~final_test_choice,
                                data = D_CorrectBlicketness)
summary(aov_test_choice_incorrect)

# rating of incorrect object for those who chose correctly
mean(D$test_rating_incorrect[D$final_test_choice=="Correct"], na.rm = TRUE)
sd(D$test_rating_incorrect[D$final_test_choice=="Correct"], na.rm = TRUE)

# rating of incorrect object for those who chose incorrectly
mean(D$test_rating_incorrect[D$final_test_choice=="Incorrect"], na.rm = TRUE)
sd(D$test_rating_incorrect[D$final_test_choice=="Incorrect"], na.rm = TRUE)

# CONCLUSION:

# direction_choice
table(D_CorrectBlicketness$final_direction_choice_conserv)
final_baseline_direction_ordered_conserv_prob = table(D_CorrectBlicketness$final_direction_choice_conserv)[[2]]/(table(D_CorrectBlicketness$final_direction_choice_conserv)[[1]]+
                                                                                                          table(D_CorrectBlicketness$final_direction_choice_conserv)[[2]]+
                                                                                                          table(D_CorrectBlicketness$final_direction_choice_conserv)[[3]]+
                                                                                                          table(D_CorrectBlicketness$final_direction_choice_conserv)[[4]])
final_baseline_direction_ordered_conserv_prob 
final_baseline_direction_ordered__conserv_odds = final_baseline_direction_ordered_conserv_prob/(1-final_baseline_direction_ordered_conserv_prob) # this is what will be shown
final_baseline_direction_ordered__conserv_odds

### odds of selecting BOTH is 1.75

glm_direction_choice_lib = glm(direction_choice_lib~1,
                               data = D_CorrectBlicketness, family = "binomial")
summary(glm_direction_choice_lib)
exp(glm_direction_choice_lib$coefficients)
exp(confint(glm_direction_choice_lib))

table(D_CorrectBlicketness$direction_choice_lib)

aov_direction_choice_lib_correct = aov(direction_rating_SOCconsistent~direction_choice_lib,
                                       data = D_CorrectBlicketness)
summary(aov_direction_choice_lib_correct)

# other
mean(D_CorrectBlicketness$direction_rating_SOCconsistent[D_CorrectBlicketness$direction_choice_lib=="Other"], na.rm = TRUE)
sd(D_CorrectBlicketness$direction_rating_SOCconsistent[D_CorrectBlicketness$direction_choice_lib=="Other"], na.rm = TRUE)

# order consistent
mean(D_CorrectBlicketness$direction_rating_SOCconsistent[D_CorrectBlicketness$direction_choice_lib=="Order Consistent"], na.rm = TRUE)
sd(D_CorrectBlicketness$direction_rating_SOCconsistent[D_CorrectBlicketness$direction_choice_lib=="Order Consistent"], na.rm = TRUE)

# CONCLUSION: 

table(D_CorrectBlicketness$final_direction_choice_conserv)
aov_direction_choice_conserv_correct = aov(D_CorrectBlicketness$direction_rating_SOCconsistent~D_CorrectBlicketness$final_direction_choice_conserv,
                                           data = D)
summary(aov_direction_choice_conserv_correct)

# both
mean(D_CorrectBlicketness$direction_rating_SOCconsistent[D_CorrectBlicketness$final_direction_choice_conserv=="Both"], na.rm = TRUE)
sd(D_CorrectBlicketness$direction_rating_SOCconsistent[D_CorrectBlicketness$final_direction_choice_conserv=="Both"], na.rm = TRUE)

# order consistent
mean(D_CorrectBlicketness$direction_rating_SOCconsistent[D_CorrectBlicketness$final_direction_choice_conserv=="Order Consistent"], na.rm = TRUE)
sd(D_CorrectBlicketness$direction_rating_SOCconsistent[D_CorrectBlicketness$final_direction_choice_conserv=="Order Consistent"], na.rm = TRUE)

# inverse 
mean(D_CorrectBlicketness$direction_rating_SOCconsistent[D_CorrectBlicketness$final_direction_choice_conserv=="Inverse"], na.rm = TRUE)
sd(D_CorrectBlicketness$direction_rating_SOCconsistent[D_CorrectBlicketness$final_direction_choice_conserv=="Inverse"], na.rm = TRUE)

# neither 
mean(D_CorrectBlicketness$direction_rating_SOCconsistent[D_CorrectBlicketness$final_direction_choice_conserv=="Neither"], na.rm = TRUE)
sd(D_CorrectBlicketness$direction_rating_SOCconsistent[D_CorrectBlicketness$final_direction_choice_conserv=="Neither"], na.rm = TRUE)

# Figure

# follow-up t-tests
both_direction_choice_correct = D_CorrectBlicketness$direction_rating_SOCconsistent[D_CorrectBlicketness$final_direction_choice_conserv=="Both"]
order_consistent_direction_choice_correct = D_CorrectBlicketness$direction_rating_SOCconsistent[D_CorrectBlicketness$final_direction_choice_conserv=="Order Consistent"]
inverse_direction_choice_correct = D_CorrectBlicketness$direction_rating_SOCconsistent[D_CorrectBlicketness$final_direction_choice_conserv=="Inverse"]
neither_direction_choice_correct = D_CorrectBlicketness$direction_rating_SOCconsistent[D_CorrectBlicketness$final_direction_choice_conserv=="Neither"]

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded by choosing the order-consistent test object.
both_vs_order_consistent_t_test = t.test(both_direction_choice_correct,
                                         order_consistent_direction_choice_correct,
                                         alternative = "two.sided") 
both_vs_order_consistent_t_test

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded by choosing the order-inconsistent test object
both_vs_inverse_t_test = t.test(both_direction_choice_correct, 
                                inverse_direction_choice_correct,
                                alternative = "two.sided")
both_vs_inverse_t_test

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


# CONCLUSION: 

# liberal coding
table(D_CorrectBlicketness$direction_choice_lib)
aov_direction_choice_lib_inverse = aov(D_CorrectBlicketness$direction_rating_inverse~D_CorrectBlicketness$direction_choice_lib,
                                       data = D)
summary(aov_direction_choice_lib_inverse)

mean(D_CorrectBlicketness$direction_rating_inverse[D_CorrectBlicketness$direction_choice_lib=="Other"], na.rm = TRUE)
sd(D_CorrectBlicketness$direction_rating_inverse[D_CorrectBlicketness$direction_choice_lib=="Other"], na.rm = TRUE)

mean(D_CorrectBlicketness$direction_rating_inverse[D_CorrectBlicketness$direction_choice_lib=="Order Consistent"], na.rm = TRUE)
sd(D_CorrectBlicketness$direction_rating_inverse[D_CorrectBlicketness$direction_choice_lib=="Order Consistent"], na.rm = TRUE)

# CONCLUSION: 

table(D_CorrectBlicketness$final_direction_choice_conserv)
aov_direction_choice_conserv_inverse = aov(D_CorrectBlicketness$direction_rating_inverse~D_CorrectBlicketness$final_direction_choice_conserv,
                                           data = D)
summary(aov_direction_choice_conserv_inverse)

multinom_direction_choice = multinom(final_direction_choice_conserv ~ 1, data = D_CorrectBlicketness)
summary(multinom_direction_choice)

multinom_direction_choice_z <- summary(multinom_direction_choice)$coefficients/summary(multinom_direction_choice)$standard.errors
multinom_direction_choice_z
multinom_direction_choice_p <- (1 - pnorm(abs(multinom_direction_choice_z), 0, 1)) * 2
multinom_direction_choice_p

# both
mean(D_CorrectBlicketness$direction_rating_inverse[D_CorrectBlicketness$final_direction_choice_conserv=="Both"], na.rm = TRUE)
sd(D_CorrectBlicketness$direction_rating_inverse[D_CorrectBlicketness$final_direction_choice_conserv=="Both"], na.rm = TRUE)

# order consistent
mean(D_CorrectBlicketness$direction_rating_inverse[D_CorrectBlicketness$final_direction_choice_conserv=="Order Consistent"])
sd(D_CorrectBlicketness$direction_rating_inverse[D_CorrectBlicketness$final_direction_choice_conserv=="Order Consistent"])

# inverse 
mean(D_CorrectBlicketness$direction_rating_inverse[D_CorrectBlicketness$final_direction_choice_conserv=="Inverse"])
sd(D_CorrectBlicketness$direction_rating_inverse[D_CorrectBlicketness$final_direction_choice_conserv=="Inverse"])

# neither 
mean(D_CorrectBlicketness$direction_rating_inverse[D_CorrectBlicketness$final_direction_choice_conserv=="Neither"])
sd(D_CorrectBlicketness$direction_rating_inverse[D_CorrectBlicketness$final_direction_choice_conserv=="Neither"])

# follow-up t-tests
both_direction_choice_inverse = D_CorrectBlicketness$direction_rating_inverse[D_CorrectBlicketness$final_direction_choice_conserv=="Both"]
order_consistent_direction_choice_inverse = D_CorrectBlicketness$direction_rating_inverse[D_CorrectBlicketness$final_direction_choice_conserv=="Order Consistent"]
inverse_direction_choice_inverse = D_CorrectBlicketness$direction_rating_inverse[D_CorrectBlicketness$final_direction_choice_conserv=="Inverse"]
neither_direction_choice_inverse = D_CorrectBlicketness$direction_rating_inverse[D_CorrectBlicketness$final_direction_choice_conserv=="Neither"]

# compare the confidence ratings for the order inconsistent test object between those who responded "both" and those
# who responded by choosing the order-consistent test object.
both_vs_order_consistent_inverse_t_test = t.test(both_direction_choice_inverse,
                                                 order_consistent_direction_choice_inverse,
                                                 alternative = "two.sided") 
both_vs_order_consistent_inverse_t_test

# compare the confidence ratings for the order inconsistent test object between those who responded "both" and those
# who responded by choosing the order-inconsistent test object
both_vs_inverse_inverse_t_test = t.test(both_direction_choice_inverse, 
                                        inverse_direction_choice_inverse,
                                        alternative = "two.sided")
both_vs_inverse_inverse_t_test

# compare the confidence ratings for the order inconsistent test object between those who responded "both" and those
# who responded "neither.
both_vs_neither_inverse_t_test = t.test(both_direction_choice_inverse,
                                        neither_direction_choice_inverse,
                                        alternative = "two.sided")
both_vs_neither_inverse_t_test


# compare the confidence ratings for the order consistent test object between those who responded by choosing the 
# directionally consistent object and those
# who responded "neither.
direction_consistent_vs_inverse_inverse_t_test = t.test(order_consistent_direction_choice_inverse,
                                                        inverse_direction_choice_inverse,
                                                        alternative = "two.sided")
direction_consistent_vs_inverse_inverse_t_test

# compare the confidence ratings for the order consistent test object between those who responded by choosing the 
# directionally consistent object and those
# who responded "neither.
direction_consistent_vs_neither_inverse_t_test = t.test(order_consistent_direction_choice_inverse,
                                                        neither_direction_choice_inverse,
                                                        alternative = "two.sided")
direction_consistent_vs_neither_inverse_t_test

# CONCLUSION: 

# control_choice
table(D_CorrectBlicketness$final_control_choice_conserv)
baseline_final_control_ordered_conserv_prob = table(D_CorrectBlicketness$final_control_choice_conserv)[[2]]/(table(D_CorrectBlicketness$final_control_choice_conserv)[[1]]+
                                                                                                      table(D_CorrectBlicketness$final_control_choice_conserv)[[2]]+
                                                                                                      table(D_CorrectBlicketness$final_control_choice_conserv)[[3]]+
                                                                                                      table(D_CorrectBlicketness$final_control_choice_conserv)[[4]])
baseline_final_control_ordered_conserv_prob
baseline_final_control_ordered_conserv_odds = baseline_final_control_ordered_conserv_prob/(1-baseline_final_control_ordered_conserv_prob) # this is what will be shown
baseline_final_control_ordered_conserv_odds

### odds of selecting BOTH is 1.75

D_CorrectBlicketness$final_control_choice_lib = relevel(D_CorrectBlicketness$final_control_choice_lib, ref="Other")
glm_control_choice = glm(control_choice_lib ~ 1,
                         data = D_CorrectBlicketness, family = "binomial")
summary(glm_control_choice)
exp(glm_control_choice$coefficients)
exp(confint(glm_control_choice))

multinom_control_choice = multinom(final_control_choice_conserv ~ 1, data = D_CorrectBlicketness)
summary(multinom_control_choice)

multinom_control_choice_z <- summary(multinom_control_choice)$coefficients/summary(multinom_control_choice)$standard.errors
multinom_control_choice_z
multinom_control_choice_p <- (1 - pnorm(abs(multinom_control_choice_z), 0, 1)) * 2
multinom_control_choice_p

t_test_reprompt_test = t.test(D_CorrectBlicketness$direction_rating_SOCconsistent,
                              D_CorrectBlicketness$final_direction_rating_SOCconsistent, paired = TRUE,
                              alternative = "two.sided")
t_test_reprompt_test

t_test_reprompt_test = t.test(D_CorrectBlicketness$control_rating_ordered,
                              D_CorrectBlicketness$final_control_rating_ordered, paired = TRUE,
                              alternative = "two.sided")
t_test_reprompt_test

####### OTHER Blicketness
# final_test_choice
table(D_OtherBlicketness$final_test_choice)
baseline_test_sucess_prob = table(D_OtherBlicketness$final_test_choice)[[2]]/(table(D_OtherBlicketness$final_test_choice)[[1]]+table(D_OtherBlicketness$final_test_choice)[[2]])
baseline_test_sucess_prob
baseline_test_success_odds = baseline_test_sucess_prob/(1-baseline_test_sucess_prob) # this is what will be shown
baseline_test_success_odds

D_OtherBlicketness$final_test_choice = relevel(D$final_test_choice, ref="Correct")
glm_final_test_choice = glm(final_test_choice~1, 
                            data = D_OtherBlicketness, 
                            family = "binomial")
summary(glm_final_test_choice)
exp(glm_final_test_choice$coefficients)
exp(confint(glm_final_test_choice))

# correct ratings
mean_test_rating_correct = mean(D_OtherBlicketness$test_rating_correct, na.rm = TRUE)
mean_test_rating_correct
sd_test_rating_correct = sd(D_OtherBlicketness$test_rating_correct, na.rm = TRUE)
sd_test_rating_correct

final_mean_test_rating_correct = mean(D_OtherBlicketness$final_test_rating_correct, na.rm = TRUE)
final_mean_test_rating_correct
final_sd_test_rating_correct = sd(D_OtherBlicketness$final_test_rating_correct, na.rm = TRUE)
final_sd_test_rating_correct

# incorrect ratings
mean_test_rating_incorrect = mean(D_OtherBlicketness$test_rating_incorrect, na.rm = TRUE)
mean_test_rating_incorrect
sd_test_rating_incorrect = sd(D_OtherBlicketness$test_rating_incorrect, na.rm = TRUE)
sd_test_rating_incorrect

final_mean_test_rating_incorrect = mean(D_OtherBlicketness$final_test_rating_incorrect, na.rm = TRUE)
final_mean_test_rating_incorrect
final_sd_test_rating_incorrect = sd(D_OtherBlicketness$final_test_rating_incorrect, na.rm = TRUE)
final_sd_test_rating_incorrect

t_test_test_trial_ratings = t.test(D_OtherBlicketness$final_test_rating_correct,
                                   D_OtherBlicketness$final_test_rating_incorrect, paired = TRUE,
                                   alternative = "two.sided")
t_test_test_trial_ratings

t_test_reprompt_test = t.test(D_OtherBlicketness$test_rating_correct,
                              D_OtherBlicketness$final_test_rating_correct, paired = TRUE,
                              alternative = "two.sided")
t_test_reprompt_test # not significant effect of reprompt

# Examine whether there is an effect of whether participants chose the SOC object during
# the replication trial (i.e., D_OtherBlicketness$test_choice) on their ratings of the correct (i.e., D_OtherBlicketness$test_rating_correct)
# & incorrect objects.
aov_test_choice_correct = aov(final_test_rating_correct~final_test_choice,
                              data = D_OtherBlicketness)
summary(aov_test_choice_correct)

# rating of correct object for those who chose correctly
mean(D_OtherBlicketness$test_rating_correct[D_OtherBlicketness$final_test_choice=="Correct"], na.rm = TRUE)
sd(D_OtherBlicketness$test_rating_correct[D_OtherBlicketness$final_test_choice=="Correct"], na.rm = TRUE)

# rating of correct object for those who chose incorrectly
mean(D_OtherBlicketness$test_rating_correct[D_OtherBlicketness$final_test_choice=="Incorrect"], na.rm = TRUE)
sd(D_OtherBlicketness$test_rating_correct[D_OtherBlicketness$final_test_choice=="Incorrect"], na.rm = TRUE)

# CONCLUSION:
# 

aov_test_choice_incorrect = aov(final_test_rating_incorrect~final_test_choice,
                                data = D_OtherBlicketness)
summary(aov_test_choice_incorrect)

# rating of incorrect object for those who chose correctly
mean(D$test_rating_incorrect[D$final_test_choice=="Correct"], na.rm = TRUE)
sd(D$test_rating_incorrect[D$final_test_choice=="Correct"], na.rm = TRUE)

# rating of incorrect object for those who chose incorrectly
mean(D$test_rating_incorrect[D$final_test_choice=="Incorrect"], na.rm = TRUE)
sd(D$test_rating_incorrect[D$final_test_choice=="Incorrect"], na.rm = TRUE)

# CONCLUSION:

# direction_choice
table(D_OtherBlicketness$final_direction_choice_conserv)
final_baseline_direction_ordered_conserv_prob = table(D_OtherBlicketness$final_direction_choice_conserv)[[2]]/(table(D_OtherBlicketness$final_direction_choice_conserv)[[1]]+
                                                                                                                   table(D_OtherBlicketness$final_direction_choice_conserv)[[2]]+
                                                                                                                   table(D_OtherBlicketness$final_direction_choice_conserv)[[3]]+
                                                                                                                   table(D_OtherBlicketness$final_direction_choice_conserv)[[4]])
final_baseline_direction_ordered_conserv_prob 
final_baseline_direction_ordered__conserv_odds = final_baseline_direction_ordered_conserv_prob/(1-final_baseline_direction_ordered_conserv_prob) # this is what will be shown
final_baseline_direction_ordered__conserv_odds

### odds of selecting BOTH is 1.75

glm_direction_choice_lib = glm(direction_choice_lib~1,
                               data = D_OtherBlicketness, family = "binomial")
summary(glm_direction_choice_lib)
exp(glm_direction_choice_lib$coefficients)
exp(confint(glm_direction_choice_lib))

table(D_OtherBlicketness$direction_choice_lib)

aov_direction_choice_lib_correct = aov(direction_rating_SOCconsistent~direction_choice_lib,
                                       data = D_OtherBlicketness)
summary(aov_direction_choice_lib_correct)

# other
mean(D_OtherBlicketness$direction_rating_SOCconsistent[D_OtherBlicketness$direction_choice_lib=="Other"], na.rm = TRUE)
sd(D_OtherBlicketness$direction_rating_SOCconsistent[D_OtherBlicketness$direction_choice_lib=="Other"], na.rm = TRUE)

# order consistent
mean(D_OtherBlicketness$direction_rating_SOCconsistent[D_OtherBlicketness$direction_choice_lib=="Order Consistent"], na.rm = TRUE)
sd(D_OtherBlicketness$direction_rating_SOCconsistent[D_OtherBlicketness$direction_choice_lib=="Order Consistent"], na.rm = TRUE)

# CONCLUSION: 

table(D_OtherBlicketness$final_direction_choice_conserv)
aov_direction_choice_conserv_correct = aov(D_OtherBlicketness$direction_rating_SOCconsistent~D_OtherBlicketness$final_direction_choice_conserv,
                                           data = D)
summary(aov_direction_choice_conserv_correct)

# both
mean(D_OtherBlicketness$direction_rating_SOCconsistent[D_OtherBlicketness$final_direction_choice_conserv=="Both"], na.rm = TRUE)
sd(D_OtherBlicketness$direction_rating_SOCconsistent[D_OtherBlicketness$final_direction_choice_conserv=="Both"], na.rm = TRUE)

# order consistent
mean(D_OtherBlicketness$direction_rating_SOCconsistent[D_OtherBlicketness$final_direction_choice_conserv=="Order Consistent"], na.rm = TRUE)
sd(D_OtherBlicketness$direction_rating_SOCconsistent[D_OtherBlicketness$final_direction_choice_conserv=="Order Consistent"], na.rm = TRUE)

# inverse 
mean(D_OtherBlicketness$direction_rating_SOCconsistent[D_OtherBlicketness$final_direction_choice_conserv=="Inverse"], na.rm = TRUE)
sd(D_OtherBlicketness$direction_rating_SOCconsistent[D_OtherBlicketness$final_direction_choice_conserv=="Inverse"], na.rm = TRUE)

# neither 
mean(D_OtherBlicketness$direction_rating_SOCconsistent[D_OtherBlicketness$final_direction_choice_conserv=="Neither"], na.rm = TRUE)
sd(D_OtherBlicketness$direction_rating_SOCconsistent[D_OtherBlicketness$final_direction_choice_conserv=="Neither"], na.rm = TRUE)

# Figure

# follow-up t-tests
both_direction_choice_correct = D_OtherBlicketness$direction_rating_SOCconsistent[D_OtherBlicketness$final_direction_choice_conserv=="Both"]
order_consistent_direction_choice_correct = D_OtherBlicketness$direction_rating_SOCconsistent[D_OtherBlicketness$final_direction_choice_conserv=="Order Consistent"]
inverse_direction_choice_correct = D_OtherBlicketness$direction_rating_SOCconsistent[D_OtherBlicketness$final_direction_choice_conserv=="Inverse"]
neither_direction_choice_correct = D_OtherBlicketness$direction_rating_SOCconsistent[D_OtherBlicketness$final_direction_choice_conserv=="Neither"]

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded by choosing the order-consistent test object.
both_vs_order_consistent_t_test = t.test(both_direction_choice_correct,
                                         order_consistent_direction_choice_correct,
                                         alternative = "two.sided") 
both_vs_order_consistent_t_test

# compare the confidence ratings for the order consistent test object between those who responded "both" and those
# who responded by choosing the order-inconsistent test object
both_vs_inverse_t_test = t.test(both_direction_choice_correct, 
                                inverse_direction_choice_correct,
                                alternative = "two.sided")
both_vs_inverse_t_test

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


# CONCLUSION: 

# liberal coding
table(D_OtherBlicketness$direction_choice_lib)
aov_direction_choice_lib_inverse = aov(D_OtherBlicketness$direction_rating_inverse~D_OtherBlicketness$direction_choice_lib,
                                       data = D)
summary(aov_direction_choice_lib_inverse)

mean(D_OtherBlicketness$direction_rating_inverse[D_OtherBlicketness$direction_choice_lib=="Other"], na.rm = TRUE)
sd(D_OtherBlicketness$direction_rating_inverse[D_OtherBlicketness$direction_choice_lib=="Other"], na.rm = TRUE)

mean(D_OtherBlicketness$direction_rating_inverse[D_OtherBlicketness$direction_choice_lib=="Order Consistent"], na.rm = TRUE)
sd(D_OtherBlicketness$direction_rating_inverse[D_OtherBlicketness$direction_choice_lib=="Order Consistent"], na.rm = TRUE)

# CONCLUSION: 

table(D_OtherBlicketness$final_direction_choice_conserv)
aov_direction_choice_conserv_inverse = aov(D_OtherBlicketness$direction_rating_inverse~D_OtherBlicketness$final_direction_choice_conserv,
                                           data = D)
summary(aov_direction_choice_conserv_inverse)

multinom_direction_choice = multinom(final_direction_choice_conserv ~ 1, data = D_OtherBlicketness)
summary(multinom_direction_choice)

multinom_direction_choice_z <- summary(multinom_direction_choice)$coefficients/summary(multinom_direction_choice)$standard.errors
multinom_direction_choice_z
multinom_direction_choice_p <- (1 - pnorm(abs(multinom_direction_choice_z), 0, 1)) * 2
multinom_direction_choice_p

# both
mean(D_OtherBlicketness$direction_rating_inverse[D_OtherBlicketness$final_direction_choice_conserv=="Both"], na.rm = TRUE)
sd(D_OtherBlicketness$direction_rating_inverse[D_OtherBlicketness$final_direction_choice_conserv=="Both"], na.rm = TRUE)

# order consistent
mean(D_OtherBlicketness$direction_rating_inverse[D_OtherBlicketness$final_direction_choice_conserv=="Order Consistent"])
sd(D_OtherBlicketness$direction_rating_inverse[D_OtherBlicketness$final_direction_choice_conserv=="Order Consistent"])

# inverse 
mean(D_OtherBlicketness$direction_rating_inverse[D_OtherBlicketness$final_direction_choice_conserv=="Inverse"])
sd(D_OtherBlicketness$direction_rating_inverse[D_OtherBlicketness$final_direction_choice_conserv=="Inverse"])

# neither 
mean(D_OtherBlicketness$direction_rating_inverse[D_OtherBlicketness$final_direction_choice_conserv=="Neither"])
sd(D_OtherBlicketness$direction_rating_inverse[D_OtherBlicketness$final_direction_choice_conserv=="Neither"])

# follow-up t-tests
both_direction_choice_inverse = D_OtherBlicketness$direction_rating_inverse[D_OtherBlicketness$final_direction_choice_conserv=="Both"]
order_consistent_direction_choice_inverse = D_OtherBlicketness$direction_rating_inverse[D_OtherBlicketness$final_direction_choice_conserv=="Order Consistent"]
inverse_direction_choice_inverse = D_OtherBlicketness$direction_rating_inverse[D_OtherBlicketness$final_direction_choice_conserv=="Inverse"]
neither_direction_choice_inverse = D_OtherBlicketness$direction_rating_inverse[D_OtherBlicketness$final_direction_choice_conserv=="Neither"]

# compare the confidence ratings for the order inconsistent test object between those who responded "both" and those
# who responded by choosing the order-consistent test object.
both_vs_order_consistent_inverse_t_test = t.test(both_direction_choice_inverse,
                                                 order_consistent_direction_choice_inverse,
                                                 alternative = "two.sided") 
both_vs_order_consistent_inverse_t_test

# compare the confidence ratings for the order inconsistent test object between those who responded "both" and those
# who responded by choosing the order-inconsistent test object
both_vs_inverse_inverse_t_test = t.test(both_direction_choice_inverse, 
                                        inverse_direction_choice_inverse,
                                        alternative = "two.sided")
both_vs_inverse_inverse_t_test

# compare the confidence ratings for the order inconsistent test object between those who responded "both" and those
# who responded "neither.
both_vs_neither_inverse_t_test = t.test(both_direction_choice_inverse,
                                        neither_direction_choice_inverse,
                                        alternative = "two.sided")
both_vs_neither_inverse_t_test


# compare the confidence ratings for the order consistent test object between those who responded by choosing the 
# directionally consistent object and those
# who responded "neither.
direction_consistent_vs_inverse_inverse_t_test = t.test(order_consistent_direction_choice_inverse,
                                                        inverse_direction_choice_inverse,
                                                        alternative = "two.sided")
direction_consistent_vs_inverse_inverse_t_test

# compare the confidence ratings for the order consistent test object between those who responded by choosing the 
# directionally consistent object and those
# who responded "neither.
direction_consistent_vs_neither_inverse_t_test = t.test(order_consistent_direction_choice_inverse,
                                                        neither_direction_choice_inverse,
                                                        alternative = "two.sided")
direction_consistent_vs_neither_inverse_t_test

# CONCLUSION: 

# control_choice
table(D_OtherBlicketness$final_control_choice_conserv)
baseline_final_control_ordered_conserv_prob = table(D_OtherBlicketness$final_control_choice_conserv)[[2]]/(table(D_OtherBlicketness$final_control_choice_conserv)[[1]]+
                                                                                                               table(D_OtherBlicketness$final_control_choice_conserv)[[2]]+
                                                                                                               table(D_OtherBlicketness$final_control_choice_conserv)[[3]]+
                                                                                                               table(D_OtherBlicketness$final_control_choice_conserv)[[4]])
baseline_final_control_ordered_conserv_prob
baseline_final_control_ordered_conserv_odds = baseline_final_control_ordered_conserv_prob/(1-baseline_final_control_ordered_conserv_prob) # this is what will be shown
baseline_final_control_ordered_conserv_odds

### odds of selecting BOTH is 1.75

D_OtherBlicketness$final_control_choice_lib = relevel(D_OtherBlicketness$final_control_choice_lib, ref="Other")
glm_control_choice = glm(control_choice_lib ~ 1,
                         data = D_OtherBlicketness, family = "binomial")
summary(glm_control_choice)
exp(glm_control_choice$coefficients)
exp(confint(glm_control_choice))

multinom_control_choice = multinom(final_control_choice_conserv ~ 1, data = D_OtherBlicketness)
summary(multinom_control_choice)

multinom_control_choice_z <- summary(multinom_control_choice)$coefficients/summary(multinom_control_choice)$standard.errors
multinom_control_choice_z
multinom_control_choice_p <- (1 - pnorm(abs(multinom_control_choice_z), 0, 1)) * 2
multinom_control_choice_p

t_test_reprompt_test = t.test(D_OtherBlicketness$direction_rating_SOCconsistent,
                              D_OtherBlicketness$final_direction_rating_SOCconsistent, paired = TRUE,
                              alternative = "two.sided")
t_test_reprompt_test

t_test_reprompt_test = t.test(D_OtherBlicketness$control_rating_ordered,
                              D_OtherBlicketness$final_control_rating_ordered, paired = TRUE,
                              alternative = "two.sided")
t_test_reprompt_test

ggplot() +
  geom_density(data = D_CorrectBlicketness, aes(x = final_test_rating_correct, fill = "Correct Blicket"), alpha = 0.5) +
  geom_density(data = D_OtherBlicketness, aes(x = final_test_rating_correct, fill = "Other Blicket"), alpha = 0.5) +
  labs(x = "Object Rating", y = "Density", title = "Distributional Difference of Order-Consistent Object Rating \nin SOC Trial") +
  scale_fill_manual(values = c("#ffc857", "purple")) +
  theme_minimal()

ggplot() +
  geom_density(data = D_CorrectBlicketness, aes(x = final_test_rating_incorrect, fill = "Correct Blicket"), alpha = 0.5) +
  geom_density(data = D_OtherBlicketness, aes(x = final_test_rating_incorrect, fill = "Other Blicket"), alpha = 0.5) +
  labs(x = "Object Rating", y = "Density", title = "Distributional Difference of Order-Inconsistent Object Rating \nin SOC Trial") +
  scale_fill_manual(values = c("#ffc857", "purple")) +
  theme_minimal()

ggplot() +
  geom_density(data = D_CorrectBlicketness, aes(x = final_direction_rating_SOCconsistent, fill = "Correct Blicket"), alpha = 0.5) +
  geom_density(data = D_OtherBlicketness, aes(x = final_direction_rating_SOCconsistent, fill = "Other Blicket"), alpha = 0.5) +
  labs(x = "Object Rating", y = "Density", title = "Distributional Difference of Order-Consistent Object Rating \nin Direction Trial") +
  scale_fill_manual(values = c("#ffc857", "purple")) +
  theme_minimal()

ggplot() +
  geom_density(data = D_CorrectBlicketness, aes(x = final_direction_rating_inverse, fill = "Correct Blicket"), alpha = 0.5) +
  geom_density(data = D_OtherBlicketness, aes(x = final_direction_rating_inverse, fill = "Other Blicket"), alpha = 0.5) +
  labs(x = "Object Rating", y = "Density", title = "Distributional Difference of Order-Inconsistent Object Rating \nin Direction Trial") +
  scale_fill_manual(values = c("#ffc857", "purple")) +
  theme_minimal()

ggplot() +
  geom_density(data = D_CorrectBlicketness, aes(x = final_control_rating_ordered, fill = "Correct Blicket"), alpha = 0.5) +
  geom_density(data = D_OtherBlicketness, aes(x = final_control_rating_ordered, fill = "Other Blicket"), alpha = 0.5) +
  labs(x = "Object Rating", y = "Density", title = "Distributional Difference of Order-Consistent Object Rating \nin Control Trial") +
  scale_fill_manual(values = c("#ffc857", "purple")) +
  theme_minimal()

ggplot() +
  geom_density(data = D_CorrectBlicketness, aes(x = final_control_rating_inverse, fill = "Correct Blicket"), alpha = 0.5) +
  geom_density(data = D_OtherBlicketness, aes(x = final_control_rating_inverse, fill = "Other Blicket"), alpha = 0.5) +
  labs(x = "Object Rating", y = "Density", title = "Distributional Difference of Order-Inconsistent Object Rating \nin Control Trial") +
  scale_fill_manual(values = c("#ffc857", "purple")) +
  theme_minimal()
