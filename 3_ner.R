library(tidyverse)
library(magrittr)
# library(spacyr)
# library(future.apply)
#
# plan(multisession)
#
# spacy_download_langmodel("zh_core_web_sm")
# spacy_initialize(model = "zh_core_web_sm")
#
# corpus <- read_csv("dataset_corpus.csv")$corpus
# qs <- read_csv("dataset_corpus.csv")$question
#
# extract_locations <- function(txt){
#   if(is.na(txt)) return(NA)
#   if(str_detect(txt, ":|：")) {
#     txt <- str_extract(txt, ":|：(.+)", 1)
#   }
#   temp = spacy_parse(txt) %>%
#     filter(entity %in% c("LOC_B", "GPE_B", "ORG_B"),
#            !token %in% c("中国", "中方", "中"))
#
#   out = table(temp$token) %>% sort(T)
#   return(out)
# }
#
# locs <- sapply(1:length(corpus), \(i) extract_locations(corpus[i]))
#
# saveRDS(locs, "locations.RDS")
locs <- readRDS("locations.RDS")

entities_code <- read_csv("entities_coded.csv")
entities_map <- entities_code$country
names(entities_map) <- entities_code$word

entities_map %<>% na.omit()

process <- function(x){
  names(x) %<>% entities_map[.] %>% unique() %>% na.omit()
  return(x)
}

locs <- sapply(locs, process)

# Label USA, Taiwan and Japan

df <- read_csv("dataset_corpus.csv")
df$usa = sapply(locs, \(x) sum(x[names(x) == "United States"], na.rm = T))

df %<>% filter(usa != 0)

write_csv(df, "dataset_corpus.csv")
