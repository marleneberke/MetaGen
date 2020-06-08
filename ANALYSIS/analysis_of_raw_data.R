#Script for processing data and making plots for example runs
library(dplyr)
library(tidyr)
library(readr)
library(tidyboot)
library(ggplot2)
library(tidyverse)
library(Rfast)
library(gam)

source("/Users/marleneberke/Documents/03_Yale/Projects/001_Mask_RCNN/ORB_project3/NEURIPS/ANALYSIS/accuracy_for_NEURIPS.R")
source("/Users/marleneberke/Documents/03_Yale/Projects/001_Mask_RCNN/ORB_project3/NEURIPS/ANALYSIS/visualize_reality_for_NEURIPS.R")
source("/Users/marleneberke/Documents/03_Yale/Projects/001_Mask_RCNN/ORB_project3/NEURIPS/ANALYSIS/MSE_Vs_for_NEURIPS.R")


##################################
## If you are using the csv file output by MetaGen

raw_data <- read_delim("merged.csv",
                        "&", escape_double = FALSE, trim_ws = TRUE, n_max = 20)
names(raw_data)<-str_replace_all(names(raw_data), c(" " = "."))

simID <- 1:dim(raw_data)[1]
raw_data <- cbind(simID, raw_data)

data2 <- raw_data
data2 <- data2 %>% filter_all(all_vars(.!= "gt_R"))

I <- nrow(data2)

combined_data <- vector(mode = "list", length = length(data))
for(i in 1:I){
  print(i)
  combined_data[[i]] <- cbind(simID = i, MSE_Vs(data2[i,]), accuracy(data2[i,]))
}
combined_data <- combined_data %>% bind_rows

write.csv(combined_data,'ProcessedDataTest.csv')

###################################################################
#For visualizing an example run.

#Cherry-pick run to visualize...
#Pick sim with biggest avg difference between retro and threshold models
#m <- max(abs(combined_data$avg_diff_between_retro_and_threshold), na.rm = TRUE)
m <- max(combined_data$avg_diff_between_retro_and_threshold, na.rm = TRUE)
biggest_win <- combined_data %>% filter(avg_diff_between_retro_and_threshold==m)
sim <- biggest_win$simID[1]

data <- raw_data[sim,]

#then step through visualize_reality.R to get plots
visualize_reality(data) #suggest stepping through to show each plot

###################################################################