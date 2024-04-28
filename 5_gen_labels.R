library(tidyverse)
library(magrittr)

labels <- readRDS("intensities_by_date.RDS")

labels %<>% mutate(
  tri_code = case_when(
    abs(std_intensity) < 0.4 ~ 1, # neutral
    std_intensity <= -0.4 ~ 0, # conflictual
    std_intensity >= 0.4 ~ 2 # cooperative
  ),
  bin_code = case_when(
    avg_intensity < 0 ~ 0,
    avg_intensity >= 0 ~ 1
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

get_tri_esc <- function(d){
    return(labels[labels$date == d,]$tri_code)
}

get_bin_esc <- function(d){
  return(labels[labels$date == d,]$bin_code)
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

df$tri_labels <- map(df$date, get_tri_esc) %>% unlist()
df$bin_labels <- map(df$date, get_bin_esc) %>% unlist()
df$lab_intensity <- map(df$date, get_intensity) %>% unlist()
df %<>% filter(!is.na(lab_intensity))

n_class <- df$bin_labels %>% table %>% min
df %<>% group_by(bin_labels) %>% group_split() %>% lapply(\(df) sample_n(df, n_class, replace = F)) %>% bind_rows()

write_csv(df, "full_dataset.csv")
