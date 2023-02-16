################################
####   Lens Output Script   ####
################################

library(tidyr)
library(dplyr)

D = read.csv(file.choose(), header = FALSE)
colnames(D) <- c("output")
str(D)

# 6 rows make up one simulation
# first, assign each 6 rows of simulation chunk into simulation_id 
sim_len <- 6
D$simulation_id <- 1 + seq(0, nrow(D) - 1) %/% sim_len

# second, transpose the data frame from wide to long format by simulation_id
output_DF <- D %>%
  group_by(simulation_id) %>%
  mutate(col_id = 1:n()) %>%
  spread(col_id, output)
output_DF$row.names = NULL
# now you have an output_DF df in which each row corresponds to each new simulation

# df column names are uninformative
colnames(output_DF) <- c("sim_id","test_activity_correct","test_activity_incorrect",
                         "direction_activity_SOCconsistent","direction_activity_inverse",
                         "control_activity_ordered","control_activity_inverse")

output_DF$model_type = rep(0, nrow(output_DF))
output_DF$model_type = as.character(output_DF$model_type)
output_DF[1:32,]$model_type = "no_inverse"
output_DF[33:64,]$model_type = "split"
#output_DF[1:30,]$model_type = "no_inverse"
#output_DF[31:60,]$model_type = "weak_inverse"
#output_DF[61:90,]$model_type = "split"
#output_DF[91:120,]$model_type = "strong_inverse"

# Save output DF as csv
write.csv(output_DF,"0.01LR_simulation_NI_S.csv", row.names = FALSE)

