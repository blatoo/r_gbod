# r_gbod
This is an grid based anomaly detection algorithm, which suitable for large dataset.

# Installation
The installation need the `devtools` package: https://github.com/hadley/devtools
    library(devtools)
    install_github("blatoo/r_gbod")
    
# Quickstart
Details in file "gbod_usage.html"
Input data is a dataframe without missing values

    library(dplyr)
    library(gbod)
    df = na.omit(read.csv(...)) # input dataframe
    
    # use the basic method
    res = gbod(df, n_partition=10, outlier_percent=1)
        
    # use the scored method
    res = gbod_score(df, partition_range = c(5,8), outlier_percent=1)
