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

write_csv(df, "corpus.csv")

missed <- df %>% filter(is.na(question), is.na(answer))
links <- readRDS("article_urls.RDS")
redos <- which(links %in% missed$url)
