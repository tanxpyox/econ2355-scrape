library(tidyverse)
library(icews)

`%.%` <- paste0
icews_dir <- "labels"

df <- read_csv("dataset_corpus.csv")
range(df$time)

