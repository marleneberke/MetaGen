#This is the main script for analyzing the processed data
#Uncomment lines 10-11 to read the csv file saved in analysis_of_raw_data.R
#If you're using the our data, use lines 12-19

library(tidyverse)
library(Rfast)
##################################
## Read the processed data

# combined_data <- read_delim("ProcessedData.csv",
#                        ",", escape_double = FALSE, trim_ws = TRUE)
First_chunk <- read_delim("DATA/First_chunk.csv",
                          ",", escape_double = FALSE, trim_ws = TRUE)
Second_chunk <- read_delim("DATA/Second_chunk.csv",
                          ",", escape_double = FALSE, trim_ws = TRUE)
Third_chunk <- read_delim("DATA/Third_chunk.csv",
                           ",", escape_double = FALSE, trim_ws = TRUE)
Fourth_chunk <- read_delim("DATA/Fourth_chunk.csv",
                          ",", escape_double = FALSE, trim_ws = TRUE)
combined_data <- rbind(First_chunk, Second_chunk, Third_chunk, Fourth_chunk)

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
      A_threshold
    )
  
  GetLowerCI <- function(x,y){return(prop.test(x,y)$conf.int[1])}
  GetTopCI <- function(x,y){return(prop.test(x,y)$conf.int[2])}
  
  toPlot_Accuracy <- df_Accuracy %>% group_by(percept_number,Model) %>% summarize(Samples=n(),Hits=sum(Score),Mean=mean(Score),Lower=GetLowerCI(Hits,Samples),Top=GetTopCI(Hits,Samples))
  
  p <- ggplot(
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
  
  ggsave("accuracy_plot.pdf",p)
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
  
  p <- ggplot(
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
  
  ggsave("mse_V_plot.pdf",p)
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
  p1 <- Results %>% ggplot(aes(Fidelity,Score,color=Model))+geom_line()+
    theme_grey()+theme(aspect.ratio=1)
  
  ggsave("accuracy_vs_noise_plot.pdf", p1)
  
  #Differences plot
  toPlotDiff <- Results %>% spread(Model, Score)
  p2 <- toPlotDiff %>% ggplot(aes(Fidelity,A_retrospective_metagen - A_threshold))+geom_line()+
    theme_grey()+theme(aspect.ratio=1)
  
  ggsave("differences_vs_noise_plot.pdf", p2)
  
}
###################################################################

#These functions produce the plots for Figure 2 in the NEURIPS paper
accuracy_plot(combined_data)
mse_V_plot(combined_data)
noise_averaging_window_plot(combined_data)

###################################################################
