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
# load data
D = read.csv(file.choose(), header = TRUE)
D = na.omit(D)

# get the structure of the data
str(D)
  
# create a test_choice variable
D$test_choice = rep(0,nrow(D))
for(i in 1:nrow(D)){
  if(D$correct_soc_choice[i]==D$test_soc_choice[i]){
    D$test_choice[i] = 1
  }else{
    D$test_choice[i] = 0
  }
}
# 0 - incorrect; 1 - correct

# create a direction_choice variable
D$direction_choice = rep(0, nrow(D))
for(i in 1:nrow(D)){
  if(D$test_dir_choice[i]==D$ordered_dir_choice[i]){
    D$direction_choice[i] = 1 #soc-consistent / order-consistent
  }else if(D$test_dir_choice[i]=="both"){
    D$direction_choice[i] = 0
  }else if(D$test_dir_choice[i]=="left"&&D$ordered_dir_choice[i]=="right"){
    D$direction_choice[i] = 2 #chose inverse but not soc_con
  }else if(D$test_dir_choice[i]=="right"&&D$ordered_dir_choice[i]=="left"){
    D$direction_choice[i] = 2 #chose inverse but not soc_con
  }else{
    D$direction_choice[i] = 3 #neither
  }
}
# 0 - both; 1 - SOC_consistent; 2 - inverse but not soc_con; 3 - neither

# create a control_choice variable
D$control_choice = rep(0, nrow(D))
for(i in 1:nrow(D)){
  if(D$ordered_contrDir_choice[i]==D$test_contrDir_choice[i]){
    D$control_choice[i] = 1 #order-consistent 
  }else if(D$test_contrDir_choice[i]=="both"){
    D$control_choice[i] = 0
  }else if(D$test_contrDir_choice[i]=="left"&&D$ordered_contrDir_choice[i]=="right"){
    D$control_choice[i] = 2 #chose inverse but not order_con
  }else if(D$test_contrDir_choice[i]=="right"&&D$ordered_contrDir_choice[i]=="left"){
    D$control_choice[i] = 2 #chose inverse but not order_con
  }else{
    D$control_choice[i] = 3 #neither
  }
}
# 0 - both; 1 - order_consistent; 2 - inverse but not order_consistent; 3 - neither

# remove redundant rows
D = D[,c(1:9,16:25)]
D$ID = c(1:nrow(D))
D = D[,c(20,2:19)]
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
baseline_test_sucess_prob = table(D$test_choice)[[2]]/(table(D$test_choice)[[1]]+table(D$test_choice)[[2]])
baseline_test_sucess_prob
baseline_test_success_odds = baseline_test_sucess_prob/(1-baseline_test_sucess_prob) # this is what will be shown
baseline_test_success_odds 

# direction_choice
table(D$direction_choice)
baseline_direction_ordered_prob = table(D$direction_choice)[[2]]/(table(D$direction_choice)[[1]]+table(D$direction_choice)[[2]]+
                                                                    table(D$direction_choice)[[3]]+table(D$direction_choice)[[4]])
baseline_direction_ordered_prob
baseline_direction_ordered_odds = baseline_direction_ordered_prob/(1-baseline_direction_ordered_prob) # this is what will be shown
baseline_direction_ordered_odds

baseline_direction_both_prob = table(D$direction_choice)[[1]]/(table(D$direction_choice)[[1]]+table(D$direction_choice)[[2]]+
                                                                 table(D$direction_choice)[[3]]+table(D$direction_choice)[[4]])
baseline_direction_both_prob
baseline_direction_both_odds = baseline_direction_both_prob/(1-baseline_direction_both_prob) # this is what will be shown
baseline_direction_both_odds

# control_choice
table(D$control_choice)
baseline_control_ordered_prob = table(D$control_choice)[[2]]/(table(D$control_choice)[[1]]+table(D$control_choice)[[2]]+table(D$control_choice)[[3]])
baseline_control_ordered_prob
baseline_control_ordered_odds = baseline_control_ordered_prob/(1-baseline_control_ordered_prob) # this is what will be shown
baseline_control_ordered_odds

baseline_control_both_prob = table(D$control_choice)[[1]]/(table(D$control_choice)[[1]]+table(D$control_choice)[[2]]+table(D$control_choice)[[3]])
baseline_control_both_prob
baseline_control_both_odds = baseline_control_both_prob/(1-baseline_control_both_prob) # this is what will be shown
baseline_control_both_odds

#################
# main analysis #
#################
# test_choice_main_analysis
D$test_choice = revalue(x = as.factor(D$test_choice), 
                             c("0" = "SOC-Inconsistent", "1"="SOC-Consistent"))
D$test_choice = factor(D$test_choice, levels = c("SOC-Consistent", "SOC-Inconsistent"))
D$test_choice = relevel(D$test_choice, ref="SOC-Consistent")
glm_test_choice = glm(test_choice~1, data = D, 
                           family = "binomial")
summary(glm_test_choice)

# direction_choice_main_analysis
D$direction_choice = revalue(x = as.factor(D$direction_choice), 
                             c("0" = "Both", "1"="SOC-Consistent", "2"="Inverse","3"="Neither"))
D$direction_choice = factor(D$direction_choice, levels = c("Both", "SOC-Consistent", "Inverse","Neither"))

D$direction_choice = relevel(D$direction_choice, ref="SOC-Consistent")
multinom_direction_choice = multinom(direction_choice ~ 1, data = D)
summary(multinom_direction_choice)
multinom_direction_choice_z <- summary(multinom_direction_choice)$coefficients/summary(multinom_direction_choice)$standard.errors
multinom_direction_choice_z
multinom_direction_choice_p <- (1 - pnorm(abs(multinom_direction_choice_z), 0, 1)) * 2
multinom_direction_choice_p
exp(coef(multinom_direction_choice)) #ratio of the probability of test choices over the probability of choosing the SOC-consistent object

# control_choice_main_analysis
D$control_choice = revalue(x = as.factor(D$control_choice), 
                           c("0" = "Both", "1"="Order-Consistent", "2"="Order-Inconsistent","3"="Neither"))
D$control_choice = factor(D$control_choice, levels = c("Both", "Order-Consistent", "Order-Inconsistent","Neither"))
D$control_choice = relevel(D$control_choice, ref="Order-Consistent")
multinom_control_choice = multinom(control_choice ~ 1, data = D)
summary(multinom_control_choice)
multinom_control_choice_z <- summary(multinom_control_choice)$coefficients/summary(multinom_control_choice)$standard.errors
multinom_control_choice_z
multinom_control_choice_p <- (1 - pnorm(abs(multinom_control_choice_z), 0, 1)) * 2
multinom_control_choice_p
exp(coef(multinom_direction_choice)) #ratio of the probability of test choices over the probability of choosing the SOC-consistent object



#glm_direction_choice = glm(direction_choice~1, data = D, 
#                           family = "binomial")
#summary(glm_direction_choice)

#glm_control_choice = glm(control_choice~1, data = D, 
#                      family = "binomial")
#summary(glm_control_choice)

#tab_model(glm_test_choice, glm_direction_choice, glm_control_choice)

# activity level analysis 
D$test_ratingDiff = D$test_rating_correct - D$test_rating_incorrect
D$direction_ratingDiff = D$direction_rating_SOCconsistent - D$direction_rating_inverse
D$control_ratingDiff = D$control_rating_ordered - D$control_rating_inverse

t.test(D$test_ratingDiff[D$control_choice=="Both"], D$test_ratingDiff[D$control_choice=="Order-Consistent"])
t.test(D$direction_ratingDiff[D$control_choice=="Both"], D$direction_ratingDiff[D$control_choice=="Order-Consistent"])
t.test(D$control_ratingDiff[D$control_choice=="Both"], D$control_ratingDiff[D$control_choice=="Order-Consistent"])

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


