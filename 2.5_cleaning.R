library(tidyverse)
library(magrittr)

df <- read_csv("dataset_corpus.csv")

working <- df[is.na(df$question) & !is.na(df$answer), ]

first_lines <- str_extract(working$answer, "^(.+?)\\n(.+)", 1)
remainder <- str_extract(working$answer, "^(.+?)\\n(.+)", 2)

conditional = str_detect(first_lines, "(记者|报|问).*(:|：)")

working %<>% mutate(
  question = ifelse(conditional, first_lines, NA),
  answer = ifelse(conditional, remainder, working$answer)
)

df[is.na(df$question) & !is.na(df$answer), ] <- working

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

  if(is.null(out)) {
    return(NA)
  } else {
    return(out)
  }

}

df$corpus <- apply(df %>% select(background_text, question, answer), 1, construct_corpus) %>% unlist()

write_csv(df, "dataset_corpus.csv", na = "")
