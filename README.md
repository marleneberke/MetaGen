# MetaGen


Overview:
The folder METAGEN contains the code for the MetaGen model. The folder ANALYSIS contains the code for analyzing the data.

METAGEN is implemented in Julia. It requires the following packages:
Gen
FreqTables
Distributions
Distances
TimerOutputs

To run it, the model, run execute_for_NEURIPS.jl. Give the output file "outfile" a name. This will train MetaGen on n_percepts number of observations. The results will be written to a .csv file. If you do this multiple times, with a different name for the output file each time, you can then merge these .csv files into one file called merged.csv and then analyze the data using the code in the ANALYSIS folder.

ANALYSIS is done in R 4.0.0. It requires the following packages:
dplyr
tidyr
readr
tidyboot
ggplot2
tidyverse
Rfast
gam
stringr
cowplot

To analyze the data in the merged.csv file, run the analysis_of_raw_data.R file. This will process the data and write a new .csv file called ProcessedData.csv. This ProcessedData.csv file can then be used in analysis_of_preprocessed_data.R file to generate the graphs in Figure 3.
