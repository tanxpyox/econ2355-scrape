library(spacyr)
library(tidyverse)
library(magrittr)
library(future.apply)

plan(multisession)

# spacy_download_langmodel("zh_core_web_sm")
spacy_initialize(model = "zh_core_web_sm")

corpus <- read_csv("dataset_corpus.csv")$corpus
qs <- read_csv("dataset_corpus.csv")$question

extract_locations <- function(txt){
  if(is.na(txt)) return(NA)
  if(str_detect(txt, ":|：")) {
    txt <- str_extract(txt, ":|：(.+)", 1)
  }
  temp = spacy_parse(txt) %>%
    filter(entity %in% c("LOC_B", "GPE_B", "ORG_B"),
           !token %in% c("中国", "中方", "中"))

  out = table(temp$token) %>% sort(T)
  return(names(out))
}

locs <- sapply(1:length(qs), \(i) extract_locations(qs[i]))

# saveRDS(locs, "locations.RDS")
locs <- readRDS("locations.RDS")
