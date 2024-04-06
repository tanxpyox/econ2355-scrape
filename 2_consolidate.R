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

# Find speaker
df$speaker <- df$title %>% str_extract("外交部发言人(.+?)(主持|就)", 1)
speaker_list = unique(df$speaker) %>% na.omit()
df[is.na(df$speaker),]$speaker <- str_extract(df[is.na(df$speaker),]$answer,
                                              paste0(speaker_list, collapse = "|"))

# Add tokens
construct_corpus <- function(x){
  out <- NULL
  if(!is.na(x[1])) {
    out <- paste(out, "[背景]", x[1])
  }
  if(!is.na(x[2])) {
    out <- paste(out, "[问题]", x[2])
  }
  if(!is.na(x[3])) {
    out <- paste(out, "[答案]", x[3])
  }
}

df$corpus <- apply(df %>% select(background_text, question, answer), 1, construct_corpus)


write_csv(df, "dataset_corpus.csv", na = "")

# read_csv("dataset_corpus.csv")

