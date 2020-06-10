# Script for processing the raw data from MetaGen
# Also plots realities from an example run (as in Figure 2 of the NEURIPS paper)
# Change line 16 to read the csv file resulting from merging the csv files from each run of MetaGen
# Change line 35 to save a csv file of the processed data

library(tidyverse)
library(Rfast)

source("accuracy_for_NEURIPS.R")
source("visualize_reality_for_NEURIPS.R")
source("MSE_Vs_for_NEURIPS.R")

##################################
# Read and process the data

raw_data <- read_delim("merged.csv",
                        "&", escape_double = FALSE, trim_ws = TRUE)
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

write.csv(combined_data,'ProcessedData.csv')

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