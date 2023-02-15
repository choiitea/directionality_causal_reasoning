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
D = read.csv(file.choose(), header = TRUE)

## check to see if there're any "NAs" ##
is.na(D)

## exclude participants who did not complete the tasks
# 1. exclude those who did not complete training or engage in the full range of test trials
D <- D[!is.na(D$static_con) & !is.na(D$causal_con) & !is.na(D$soc_con) & !is.na(D$dir_con) & !is.na(D$contrDir_con), ]

### note -- think about how to deal with empty responses for object ratings.

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

# remove unnecessary columns
D = D[,-c(1,10:21)]
D$ID = c(1:nrow(D))
D$row.names = NULL

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
D$direction_choice_conserv = as.factor(D$direction_choice_conserv)
D$final_direction_choice_conserv = as.factor(D$final_direction_choice_conserv)
D$control_choice_lib = as.factor(D$control_choice_lib)
D$control_choice_conserv = as.factor(D$control_choice_conserv)
D$final_control_choice_conserv = as.factor(D$final_control_choice_conserv)

### cleaned csv export ###
#write.csv(D,"cleaned_replication_data_wRating_wUnsure.csv", row.names = FALSE)

#####################################
# preliminary analyses and plotting #
#####################################
# get distribution of successes and failure 
# test_choice
table(D$test_choice)
baseline_test_sucess_prob = table(D$test_choice)[[1]]/(table(D$test_choice)[[1]]+table(D$test_choice)[[2]])
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
#table(D$direction_choice_lib)
#baseline_direction_ordered_lib_prob = table(D$direction_choice_lib)[[2]]/(table(D$direction_choice_lib)[[1]]+table(D$direction_choice_lib)[[2]])
#baseline_direction_ordered_lib_prob 
#baseline_direction_ordered_lib_odds = baseline_direction_ordered_lib_prob/(1-baseline_direction_ordered_lib_prob) # this is what will be shown
#baseline_direction_ordered_lib_odds

table(D$direction_choice_conserv)
baseline_direction_ordered_conserv_prob = table(D$direction_choice_conserv)[[1]]/(table(D$direction_choice_conserv)[[1]]+
                                                                                    table(D$direction_choice_conserv)[[2]]+
                                                                                    table(D$direction_choice_conserv)[[3]]+
                                                                                    table(D$direction_choice_conserv)[[4]]+
                                                                                    table(D$direction_choice_conserv)[[5]])
baseline_direction_ordered_conserv_prob 
baseline_direction_ordered__conserv_odds = baseline_direction_ordered_conserv_prob/(1-baseline_direction_ordered_conserv_prob) # this is what will be shown
baseline_direction_ordered__conserv_odds

table(D$final_direction_choice_conserv)
final_baseline_direction_ordered_conserv_prob = table(D$final_direction_choice_conserv)[[1]]/(table(D$final_direction_choice_conserv)[[1]]+
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
baseline_control_ordered_conserv_prob = table(D$control_choice_conserv)[[1]]/(table(D$control_choice_conserv)[[1]]+
                                                                                table(D$control_choice_conserv)[[2]]+
                                                                                table(D$control_choice_conserv)[[3]])
baseline_control_ordered_conserv_prob
baseline_control_ordered_conserv_odds = baseline_control_ordered_conserv_prob/(1-baseline_control_ordered_conserv_prob) # this is what will be shown
baseline_control_ordered_conserv_odds


#################
# main analysis #
#################
# test_choice FULL analysis
D$final_test_choice = relevel(D$final_test_choice, ref="Incorrect")
glm_test_choice_full = glm(final_test_choice~static_con+causal_con+soc_con+dir_con+contrDir_con, 
                           data = D, 
                           family = "binomial")
summary(glm_test_choice_full)
# This analysis tell us that there is no effect of our counterbalancing conditions


glm_test_choice = glm(final_test_choice~1, 
                      data = D, 
                      family = "binomial")
summary(glm_test_choice)
exp(glm_test_choice$coefficients)
confint(glm_test_choice)
#estimate = coefficient -- log odds - so if you exponentiate it, you'll get real odds

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

# Conclusion: 


## liberal analyses ##
# direction_choice_lib main analysis
table(D$direction_choice_lib)
glm_direction_choice_full = glm(direction_choice_lib~static_con+causal_con+soc_con+dir_con+contrDir_con, 
                                data = D, family = "binomial")
summary(glm_direction_choice_full)

#### commented out for now - cmd shift c (because I always forget how to comment out block.)
# table(D$direction_choice_lib)
# D$direction_choice_lib = relevel(D$direction_choice_lib, ref="Other")
# glm_direction_choice_lib = glm(direction_choice_lib~1,
#                                data = D, family = "binomial")
# summary(glm_direction_choice_lib)
# exp(glm_direction_choice_lib$coefficients)
# confint(glm_direction_choice_lib)
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
# D$control_choice_lib = relevel(D$control_choice_lib, ref="Other")
# glm_control_choice = glm(control_choice_lib ~ 1, 
#                          data = D, family = "binomial")
# summary(glm_control_choice)
# exp(glm_control_choice$coefficients)
# confint(glm_control_choice)
# 
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


# figure --- not sure why this isn't working. -- same issue for all rating figures.
D_test_rating_tall = reshape(D, varying = 10:11, v.names = "rating", timevar = "test_trial_rating", idvar = "ID", 
                             new.row.names = 1:72, 
                             direction = "long")
test_rating_barplot = ggplot(D_test_rating_tall, aes(test_trial_rating, rating)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
test_rating_barplot + 
  stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Rating") + # change the label of the y-axis  +
  xlab("SOC Test Trial") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 1)) +
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

# Figure
test_rating_correct_barplot = ggplot(D, aes(final_test_choice, test_rating_correct, fill = final_test_choice)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
test_rating_correct_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Rating") + # change the label of the y-axis
  xlab("SOC Test Trial Object Ratings of Correct Responses") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 1)) +
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

# Figure
test_rating_incorrect_barplot = ggplot(D, aes(final_test_choice, test_rating_incorrect, fill = final_test_choice)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
test_rating_incorrect_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Rating") + # change the label of the y-axis
  xlab("SOC Test Trial Object Ratings of Incorrect Responses") +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 1)) +
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
aov_direction_choice_lib_correct = aov(D$direction_rating_SOCconsistent~D$direction_choice_lib,
                                       data = D)
summary(aov_direction_choice_lib_correct)

# other
mean(D$direction_rating_SOCconsistent[D$direction_choice_lib=="Other"], na.rm = TRUE)
sd(D$direction_rating_SOCconsistent[D$direction_choice_lib=="Other"], na.rm = TRUE)

# order consistent
mean(D$direction_rating_SOCconsistent[D$direction_choice_lib=="Order Consistent"], na.rm = TRUE)
sd(D$direction_rating_SOCconsistent[D$direction_choice_lib=="Order Consistent"], na.rm = TRUE)

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

# Figure
direction_rating_SOCconsistent_barplot = ggplot(D, aes(final_direction_choice_conserv,direction_rating_SOCconsistent, fill = final_direction_choice_conserv)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
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
both_direction_choice_correct = D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Both"]
order_consistent_direction_choice_correct = D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Order Consistent"]
neither_direction_choice_correct = D$direction_rating_SOCconsistent[D$final_direction_choice_conserv=="Neither"]

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

# Figure
direction_rating_inverse_barplot = ggplot(D, aes(final_direction_choice_conserv,direction_rating_inverse, fill = final_direction_choice_conserv)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
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
both_direction_choice_inverse = D$direction_rating_inverse[D$final_direction_choice_conserv=="Both"]
order_consistent_direction_choice_inverse = D$direction_rating_inverse[D$final_direction_choice_conserv=="Order Consistent"]
neither_direction_choice_inverse = D$direction_rating_inverse[D$final_direction_choice_conserv=="Neither"]

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

# Figure
control_rating_ordered_barplot = ggplot(D, aes(final_control_choice_conserv,control_rating_ordered, fill = final_control_choice_conserv)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
control_rating_ordered_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Rating") + # change the label of the y-axis
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 1)) +
  scale_fill_manual(values=c("black","#ffc857", "dark blue", "purple")) +
  theme_bw() +
  theme(legend.position = "none")

# follow-up t-tests
both_control_choice_correct = D$control_rating_ordered[D$final_control_choice_conserv=="Both"]
order_control_direction_choice_correct = D$control_rating_ordered[D$final_control_choice_conserv=="Order Consistent"]
neither_control_choice_correct = D$control_rating_ordered[D$final_control_choice_conserv=="Neither"]

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

# Figure
control_rating_inverse_barplot = ggplot(D, aes(final_control_choice_conserv,control_rating_inverse, fill = final_control_choice_conserv)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
control_rating_inverse_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Rating") + # change the label of the y-axis
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 1)) +
  scale_fill_manual(values=c("black","#ffc857", "dark blue", "purple")) +
  theme_bw()+
  theme(legend.position = "none")

# follow-up t-tests
both_control_choice_inverse = D$control_rating_inverse[D$final_control_choice_conserv=="Both"]
order_consistent_control_choice_inverse = D$control_rating_inverse[D$final_control_choice_conserv=="Order Consistent"]
neither_control_choice_inverse = D$control_rating_inverse[D$final_control_choice_conserv=="Neither"]

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


# CONCLUSION: 

