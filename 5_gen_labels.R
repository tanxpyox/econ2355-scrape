library(tidyverse)
library(magrittr)

labels <- readRDS("intensities_by_date.RDS")

labels %<>% mutate(
  code = case_when(
    abs(std_intensity) < 0.4 ~ 1, # neutral
    std_intensity <= -0.4 ~ 0, # conflictual
    std_intensity >= 0.4 ~ 2 # cooperative
  )
)

# kernel <- dweibull(1:5, 2, 2*sqrt(2)) %>% `/`(sum(.))

begin <- min(labels$date)
end <- max(labels$date)

# smoothen <- function(d){
#   t <- labels %>% filter(date %in% (d-1):(d+3))
#   m <- t[-1] %>% as.matrix()
#
#   mult <- t(m) %*% kernel
#   names(mult) <- colnames(m)
#
#   return(c("date" = d,
#            mult))
# }
#
# smoothened <- sapply((begin+1):(end-3), smoothen) %>% t() %>% as.data.frame()
#
# derivative <- apply(smoothened, 2, diff) %>% as.data.frame()
#
#
# derivative$date <- as_date(smoothened$date[-1])
#
# to_bin <- function(x){
#   ifelse(is.nan(x), NA,
#          ifelse(x > 0, 1, 0)
#            )
# }
#
# derivative %<>% mutate(
#   usa = to_bin(usa_c),
#   taiwan = to_bin(taiwan_c),
#   japan = to_bin(japan_c)
# )

df <- read_csv("dataset_corpus.csv")

# df %<>% filter(!is.na(target))
df$date <- date(df$time)

get_esc <- function(d){
    return(labels[labels$date == d,]$code)
}

get_intensity <- function(d){
  return(labels[labels$date == d,]$std_intensity)
}

## Select top entry per day

df %<>% group_by(date) %>%
  summarise(
    i = which.max(usa),
    date = date[i],
    usa = usa[i],
    question = question[i],
    answer = answer[i],
    corpus = corpus[i]
  )

df$labels <- map(df$date, get_esc) %>% unlist()
df$lab_intensity <- map(df$date, get_intensity) %>% unlist()
df %<>% filter(!is.na(labels))

write_csv(df, "full_dataset.csv")
