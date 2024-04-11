library(tidyverse)

files = list.files("data", full.names = T)

df <- NULL

for (f in files){
  t <- readRDS(f)
  if(is.null(df)){
    df = t
  } else {
    df <- rbind(df, t)
  }
}

df <- df %>% arrange(time)

# Find speaker
df$speaker <- df$title %>% str_extract("外交部发言人(.+?)(主持|就)", 1)
speaker_list = unique(df$speaker) %>% na.omit()
df[is.na(df$speaker),]$speaker <- str_extract(df[is.na(df$speaker),]$answer,
                                              paste0(speaker_list, collapse = "|"))


write_csv(df, "dataset_corpus.csv", na = "")

# df <- read_csv("dataset_corpus.csv")

