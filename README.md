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
* tidyverse
* Rfast
* cowplot

To analyze the processed data that we've provided in the DATA folder file, use the analysis_of_processed_data.R script. This script produces the graphs in Figure 3 of the NEURIPS paper. To analyze raw data output from MetaGen, first merge the .csv files output by each run of run execute_for_NEURIPS.jl. Then analyze the data by running the analysis_of_raw_data.R file. This will process the data and write a new .csv file called ProcessedData.csv. The analysis_of_raw_data.R script will also produce the graphs used in Figure 2 of the NEURIPS paper. This ProcessedData.csv file can then be read in the analysis_of_processed_data.R script.
