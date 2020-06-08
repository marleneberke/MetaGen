library(dplyr)
library(tidyr)
library(readr)
library(tidyboot)
library(ggplot2)
library(tidyverse)
library(Rfast)
library(gam)

##################################
## If you are using the pre-processed data

combined_data <- read_delim("ProcessesedData.csv",
                       ",", escape_double = FALSE, trim_ws = TRUE)

##################################
## If you are using the csv file output by MetaGen

# raw_data <- read_delim("merged_all_data.csv",
#                         "&", escape_double = FALSE, trim_ws = TRUE, n_max = 1000)
# names(raw_data)<-str_replace_all(names(raw_data), c(" " = "."))

# simID <- 1:dim(raw_data)[1]
# raw_data <- cbind(simID, raw_data)

# data2 <- raw_data
# data2 <- data2 %>% filter_all(all_vars(.!= "gt_R"))
# 
# I <- nrow(data2)
# 
# combined_data <- vector(mode = "list", length = length(data))
# for(i in 1:I){
#   print(i)
#   combined_data[[i]] <- cbind(simID = i, MSE_Vs(data2[i,]), accuracy(data2[i,]))
# }
# combined_data <- combined_data %>% bind_rows

###################################################################
accuracy_plot <- function(data){
  #drop rows for percept0
  data <- na.omit(data)
  
  df_Accuracy <-
    data %>% gather(
      Model,
      Score,
      A_retrospective_metagen,
      A_lesioned_metagen,
      A_online_metagen,
      #A_naive_reality,
      A_threshold
    )
  
  GetLowerCI <- function(x,y){return(prop.test(x,y)$conf.int[1])}
  GetTopCI <- function(x,y){return(prop.test(x,y)$conf.int[2])}
  
  toPlot_Accuracy <- df_Accuracy %>% group_by(percept_number,Model) %>% summarize(Samples=n(),Hits=sum(Score),Mean=mean(Score),Lower=GetLowerCI(Hits,Samples),Top=GetTopCI(Hits,Samples))
  
  #temp <- toPlot_Accuracy %>% filter(Model == "A_threshold")
  #mean(temp$Mean)
  #temp <- toPlot_Accuracy %>% filter(percept_number==1)
  #group_by(percept_number)
  #MyDataFrame <- df %>% group_by(percept_number, Model) %>% tidyboot_mean(Score)
  
  ggplot(
    toPlot_Accuracy,
    aes(
      x = percept_number,
      y = Mean,
      ymin = Lower,
      ymax = Top,
      fill = Model,
      group = Model
    )
  ) + geom_ribbon() + geom_line() + coord_cartesian(ylim = c(0.70, 1)) + theme(aspect.ratio=1)
}


###################################################################
mse_V_plot <- function(data){
  df_V <-
    data %>% gather(
      V_param,
      MSE,
      MSE_FA,
      MSE_M,
      exp_MSE_FA,
      exp_MSE_M,
    )
  
  GetMean <- function(x){return(t.test(x)$estimate)}
  GetLowerCI <- function(x){return(t.test(x)$conf.int[1])}
  GetTopCI <- function(x){return(t.test(x)$conf.int[2])}
  
  toPlot_V <- df_V %>% group_by(percept_number,V_param) %>% summarize(Mean_MSE=GetMean(MSE),Lower=GetLowerCI(MSE),Top=GetTopCI(MSE))
  
  temp <- toPlot_V %>% filter(percept_number==40)
  
  ggplot(
    toPlot_V,
    aes(
      x = percept_number,
      y = Mean_MSE,
      ymin = Lower,
      ymax = Top,
      fill = V_param,
      group = V_param
    )
  ) + geom_ribbon() + geom_line() + coord_cartesian(ylim = c(0, 0.02)) + theme(aspect.ratio=1)
}

###################################################################
noise_vs_accuracy_plot <- function(data){
  #drop rows for percept0
  data <- na.omit(data)
  
  #fail <- data %>% filter(perceived_noise<0.05 & A_retrospective_metagen == 0)
  
  df_Accuracy <-
    data %>% gather(
      Model,
      Score,
      A_retrospective_metagen,
      #A_lesioned_metagen,
      #A_online_metagen,
      #A_naive_reality,
      A_threshold
    )
  
  #GetLowerCI <- function(x,y){return(prop.test(x,y)$conf.int[1])}
  #GetTopCI <- function(x,y){return(prop.test(x,y)$conf.int[2])}
  
  toPlot_Accuracy <- df_Accuracy %>% group_by(Model)# %>% summarize(Samples=n(),Hits=sum(Score),Mean=mean(Score),Lower=GetLowerCI(Hits,Samples),Top=GetTopCI(Hits,Samples))
  
  b <- ggplot(toPlot_Accuracy, aes(x = perceived_noise, y = Score))
  b + 
    #geom_point(aes(color = Model)) +
    scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
    #ggline(lowess(Score ~ perceived_noise, toPlot_Accuracy))
    geom_smooth(aes(color = Model), method="gam") + theme(aspect.ratio=1)
  
  # #Warning message:
  # Computation failed in `stat_smooth()`:
  #   workspace required (8438306300) is too large probably because of setting 'se = TRUE'.
}


###################################################################
noise_vs_differences <- function(data){
  #drop rows for percept0
  data <- na.omit(data)
  
  #max(data$diff_between_retro_and_threshold)

  b <- ggplot(data, aes(x = perceived_noise, y = diff_between_retro_and_threshold))
  b + 
    #geom_point() +
    scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
    #ggline(lowess(Score ~ perceived_noise, toPlot_Accuracy))
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_smooth(method="gam")
    #geom_smooth(aes(outfit <- method="gam"))
  
  smooth_vals = predict(gam(data$diff_between_retro_and_threshold~sort(data$perceived_noise)), data)
  
}


###################################################################
noise_density_plot <- function(data){
  #drop rows for percept0
  data <- na.omit(data)
  
  p <- ggplot(data, aes(x=perceived_noise)) + 
    geom_density()
  p
}

###################################################################
noise_averaging_window_plot <- function(data){
  data <- na.omit(data)
  df_Accuracy <-
    data %>% gather(
      Model,
      Score,
      A_retrospective_metagen,
      #A_lesioned_metagen,
      #A_online_metagen,
      #A_naive_reality,
      A_threshold
    )
  
  #GetLowerCI <- function(x,y){return(prop.test(x,y)$conf.int[1])}
  #GetTopCI <- function(x,y){return(prop.test(x,y)$conf.int[2])}
  toPlot_Accuracy <- df_Accuracy %>% group_by(Model)# %>% summarize(Samples=n(),Hits=sum(Score),Mean=mean(Score),Lower=GetLowerCI(Hits,Samples),Top=GetTopCI(Hits,Samples))
  GetAccuracy <- function(x){
    return(toPlot_Accuracy %>% filter(perceived_noise<x+0.05,perceived_noise>x-0.05) %>%
             group_by(Model) %>% summarize(Score=mean(Score))) %>% mutate(Fidelity=x)
  }
  Results <- map_df(seq(0,0.68,by=0.01),GetAccuracy)
  Results %>% ggplot(aes(Fidelity,Score,color=Model))+geom_line()+
    theme_grey()+theme(aspect.ratio=1)
}
###################################################################
accuracy_plot(combined_data)
mse_V_plot(combined_data)
#noise_vs_accuracy_plot(combined_data)
noise_vs_differences(combined_data)
#noise_density_plot(combined_data)
noise_averaging_window_plot(combined_data)
###################################################################
