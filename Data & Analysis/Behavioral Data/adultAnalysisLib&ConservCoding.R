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

## look at the data ##
fix(D)

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

# It should be clear that participants overwhelmingly chose
# the correct test object averaged over trials.
# And given that so few participants chose the "incorrect"
# test object (i.e., N = 6), we are justified to lump
# the non-correct responses together; that is, we are justified to lump
# "unsures" (N=4) with "incorrects" (N=2).



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
# compared to "other" choices. Of course, we'll follow this up, below, with formal analyses,
# but frequency distribution at least tells us that adults are *not* representing the SOCs directionally;
# that is, they appear to be insensitive to direction. I intend to test this with children to see
# if the same is true. Interesting stuff.


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

# When participants' choices on the direction test trial are coded this way, it's
# clear that there is much more variability (necessarily). What's most interesting
# is that, ignoring the neither participants, participants were mostly split between
# choosing both objects as a blicket or only the order consistent: 
# binom.test(14, 14+7, p=0.5, alternative="two.sided") > .05


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
  }else{
    D$control_choice_lib[i] = 0 #neither
  }
}

# name the levels of control_choice_lib
D$control_choice_lib = revalue(x = as.factor(D$control_choice_lib), 
                                 c("0" = "Other", "1"="Order Consistent"))
table(D$control_choice_lib)


# The frequency distribution data here shows that during the control trial,
# participants were far more likely not to choose the order consistent object 
# than to respond some other way.


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
                                     c("0" = "Both", "1"="Order Consistent",
                                        "3"= "Neither"))
# get distributional data
table(D$control_choice_conserv)


# The distributional data here are interesting because they indicate that no participant
# chose the inverse object exclusively. Instead, they were either split between choosing 
# both objects are not choosing any of the objects. Only a small number of participants
# chose the "order consistent" object.



# remove unnecessary columns
D = D[,-c(1,10:15)]
fix(D)
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
D$direction_choice = as.factor(D$direction_choice)
D$control_choice = as.factor(D$control_choice)

### cleaned csv export ###
#write.csv(D,"cleaned_adult_data_wRating_wUnsure.csv", row.names = FALSE)

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

# direction_choice
table(D$direction_choice_lib)
baseline_direction_ordered_lib_prob = table(D$direction_choice_lib)[[2]]/(table(D$direction_choice_lib)[[1]]+table(D$direction_choice_lib)[[2]])
baseline_direction_ordered_lib_prob 
baseline_direction_ordered_lib_odds = baseline_direction_ordered__lib_prob/(1-baseline_direction_ordered_lib_prob) # this is what will be shown
baseline_direction_ordered_lib_odds

table(D$direction_choice_conserv)
baseline_direction_ordered_conserv_prob = table(D$direction_choice_conserv)[[1]]/(table(D$direction_choice_conserv)[[1]]+
                                                                                    table(D$direction_choice_conserv)[[2]]+
                                                                                    table(D$direction_choice_conserv)[[3]]+
                                                                                  table(D$direction_choice_conserv)[[4]])
baseline_direction_ordered_conserv_prob 
baseline_direction_ordered__conserv_odds = baseline_direction_ordered_conserv_prob/(1-baseline_direction_ordered_conserv_prob) # this is what will be shown
baseline_direction_ordered__conserv_odds


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
D$test_choice = relevel(D$test_choice, ref="Incorrect")
glm_test_choice_full = glm(test_choice~static_con+causal_con+soc_con+dir_con+contrDir_con, 
                           data = D, 
                      family = "binomial")
summary(glm_test_choice_full)
# This analysis tell us that there is no effect of our counterbalancing conditions,
# which is a *great* thing.


glm_test_choice = glm(test_choice~1, 
                           data = D, 
                           family = "binomial")
summary(glm_test_choice)

# Conclusion: participants were significantly more likely to choose the
# correct object than the incorrect one. This means that adults *do* have
# access to an SOC mechanism. The data indicate that participants are 
# 4.333 times more likely to choose the correct object than the incorrect
# one (exp(glm_test_choice$coefficients)). Note that this data is for the 
# trial that replicates BRS (2021).


## liberal analyses ##
# direction_choice_lib main analysis
glm_direction_choice_full = glm(direction_choice_lib~static_con+causal_con+soc_con+dir_con+contrDir_con, 
                                data = D, family = "binomial")
summary(glm_direction_choice_full)

glm_direction_choice = glm(direction_choice_lib~1, 
                                data = D, family = "binomial")
summary(glm_direction_choice)


## conservative analyses ##
multinom_direction_choice = multinom(direction_choice_conserv ~ 1, data = D)
summary(multinom_direction_choice)

multinom_direction_choice_z <- summary(multinom_direction_choice)$coefficients/summary(multinom_direction_choice)$standard.errors
multinom_direction_choice_z
multinom_direction_choice_p <- (1 - pnorm(abs(multinom_direction_choice_z), 0, 1)) * 2
multinom_direction_choice_p




# Conclusion for the liberal analysis: participants were significantly more likely to choose the
# correct object than the incorrect one. This means that adults *do* have
# access to an SOC mechanism. The data indicate that participants are 
# 4.333 times more likely to choose the correct object than the incorrect
# one (exp(glm_test_choice$coefficients)). Note that this data is for the 
# trial that replicates BRS (2021).

# Conclusion for the conservative analysis: participants were much less likely to
# choose the "order consistent" or "inverse" objects compared to "both" objects.
# However, they were as likely to choose "both" test objects as not to make a choice.



## liberal analyses ## 
# control_choice_main_analysis
table(D$control_choice_lib)
glm_control_choice_full = glm(control_choice_lib ~ static_con+causal_con+soc_con+dir_con+contrDir_con, 
                              data = D, family = "binomial")
summary(glm_control_choice_full)

D$control_choice_lib = relevel(D$control_choice_lib, ref="Order Consistent")
glm_control_choice = glm(control_choice_lib ~ 1, 
                              data = D, family = "binomial")
summary(glm_control_choice)


## conservative analyses ##
table(D$control_choice_conserv)
multinom_control_choice = multinom(control_choice_conserv ~ 1, data = D)
summary(multinom_control_choice)

multinom_control_choice_z <- summary(multinom_control_choice)$coefficients/summary(multinom_control_choice)$standard.errors
multinom_control_choice_z
multinom_control_choice_p <- (1 - pnorm(abs(multinom_control_choice_z), 0, 1)) * 2
multinom_control_choice_p


# Conclusion for the liberal analysis: participants were significantly more likely to show
# some other pattern of responses compared to choosing just the SOC-consistent object.
# Indeed, the data indicate that participants were 
# exactly 7 times more likely to choose the correct object than the incorrect
# one (exp(glm_control_choice$coefficients)). 

# Conclusion for the conservative analysis: participants were much less likely to
# choose the "order consistent" than to choose "both" objects.
# However, they were as likely to choose "both" test objects as not to make a choice. These data 
# more or less the direction data.


# activity level analysis 
D$test_ratingDiff = D$test_rating_correct - D$test_rating_incorrect
D$direction_ratingDiff = D$direction_rating_SOCconsistent - D$direction_rating_inverse
D$control_ratingDiff = D$control_rating_ordered - D$control_rating_inverse

t.test(D$test_ratingDiff[D$control_choice=="Both"], D$test_ratingDiff[D$control_choice=="Order-Consistent"])
t.test(D$direction_ratingDiff[D$control_choice=="Both"], D$direction_ratingDiff[D$control_choice=="Order-Consistent"])
t.test(D$control_ratingDiff[D$control_choice=="Both"], D$control_ratingDiff[D$control_choice=="Order-Consistent"])


## Follow-up t-tests ##

## assess whether participants were more likely to provide higher (or even lower) ##
## ratings for the correct and incorrect objects during the test trial            ##
mean_test_rating_correct = mean(D$test_rating_correct)
mean_test_rating_correct
sd_test_rating_correct = sd(D$test_rating_correct)
sd_test_rating_correct

mean_test_rating_incorrect = mean(D$test_rating_incorrect)
mean_test_rating_incorrect
sd_test_rating_incorrect = sd(D$test_rating_incorrect)
sd_test_rating_incorrect

t_test_test_trial_ratings = t.test(D$test_rating_correct,
                                   D$test_rating_incorrect, paired = TRUE,
                                   alternative = "two.sided")
t_test_test_trial_ratings


# figure
D_test_rating_tall = reshape(D, varying = 10:11, v.names = "rating", timevar = "test_trial_rating", idvar = "ID", 
                             new.row.names = 1:64, 
                             direction = "long")
test_rating_barplot = ggplot(D_test_rating_tall, aes(test_trial_rating, rating)) # create the bar graph with test.trial.2 on the x-axis and measure on the y-axis
test_rating_barplot + stat_summary(fun = mean, geom = "bar", position = "dodge") + # add the bars, which represent the means and the place them side-by-side with 'dodge'
  stat_summary(fun.data=mean_cl_boot, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2) + # add errors bars
  ylab("Rating") + # change the label of the y-axis
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim=c(0, 1)) +
  scale_fill_manual(values = c("black", "azure3")) +
  theme_bw()

# t-test Figure #
# rating difference in Control Test Trial
ggplot(D) +
  geom_bar(aes(x = control_choice, y = control_ratingDiff, fill = control_choice), stat = "summary") +
  theme_bw() +
  scale_fill_manual(values= c("#66545E","#A39193","#AA6F73","#EEA990")) +
  xlab("Test Choice for Order Consistent & Order Inconsistent") +
  ylab("Difference in Control Test Ratings") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none") +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(ylim=c(0, 0.7))

# FIGURES #
D$memory_check = revalue(x = as.factor(D$memory_check), 
                         c("0" = "Memory Check Fail", "1" = "Memory Check Pass"))

test_choice_plot = ggplot(D, aes(test_choice, fill = test_choice)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
test_choice_plot+geom_bar() + theme_bw() + # remove the gray background
  scale_fill_manual(values= c("#66545E","#A39193","#AA6F73","#EEA990")) +
  xlab("SOC Consistent & SOC Inconsistent") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none") +
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 20))

direction_choice_plot = ggplot(D, aes(direction_choice, fill = direction_choice)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
direction_choice_plot+geom_bar() + theme_bw() + # remove the gray background
  scale_fill_manual(values= c("#66545E","#A39193","#AA6F73","#EEA990")) +
  xlab("SOC Consistent & Inverse Construction") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none") +
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 20))

control_choice_plot = ggplot(D, aes(control_choice, fill = control_choice)) # THE FIRST ARGUMENT VALUES AFTER 'AES' CORRESPONDS
control_choice_plot+geom_bar() + theme_bw() + # remove the gray background
  scale_fill_manual(values= c("#66545E","#A39193","#AA6F73","#EEA990")) +
  xlab("Order Consistent & Order Inconsistent") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),
        legend.position="none") +
  scale_x_discrete(drop=FALSE) +
  scale_y_continuous(expand = c(0, 0)) + # ensure that bars hit the x-axis
  coord_cartesian(ylim=c(0, 20))


