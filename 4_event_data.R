library(tidyverse)
library(magrittr)

`%.%` <- paste0
events_data_dir <- "D:/NGEC"

df <- read_csv("dataset_corpus.csv")
range(df$time)

list_eventsets <- list.files(events_data_dir, full.names = T)

list_eventsets %<>% .[grepl("^[^0-9]+202[0-4]",.)]

events <- NULL

for(f in list_eventsets){
  message(f)
  temp = read_tsv(f)
  temp %<>% filter(`Actor Country` %>% str_detect("China"),
                   `Recipient Country` %>% str_detect("United States|Taiwan|Japan"))

  if(any(names(temp) == "Event Intensity")) {
    temp %<>% rename(
      Intensity = `Event Intensity`
    )
  }

  if(!is.null(events)){
    common_cols = intersect(names(events), names(temp))
    events = rbind(events[common_cols], temp[common_cols])
  } else {
    events = temp %>% select(`Event ID`, `Event Date`, `Event Type`, `Event Mode`, `Intensity`,
                             `Contexts`, `Actor Name`, `Actor Country`, `Recipient Name`, `Recipient Country`)
  }
}

events %<>% mutate(
  usa = str_detect(`Recipient Country`, "United States"),
  taiwan = str_detect(`Recipient Country`, "Taiwan"),
  japan = str_detect(`Recipient Country`, "Japan"),
  coop = case_when(
    Intensity > 0 ~ 1,
    Intensity < 0 ~ 0,
    Intensity == 0 ~ NA
  )
)

# select only observations concerning US, Japan and Taiwan

df %<>% filter(usa + taiwan + japan > 0)
df$date <- date(df$time)

begin <- min(df$date)
end <- max(df$date)

calculate <- function(d) {
  t <- events %>% filter(`Event Date` == d)
  return(c(date = d,
         "usa" = mean(t[t$usa, ]$coop, na.rm = T),
         "japan" = mean(t[t$japan, ]$coop, na.rm = T),
         "taiwan" = mean(t[t$taiwan, ]$coop, na.rm = T),
         "usa_c" = mean(t[t$usa, ]$Intensity, na.rm = T),
         "japan_c" = mean(t[t$japan, ]$Intensity, na.rm = T),
         "taiwan_c" = mean(t[t$taiwan, ]$Intensity, na.rm = T)))
}

labels <- sapply((begin-5):(end+5), calculate) %>% t() %>% as.data.frame()

saveRDS(labels, "intensities_by_date.RDS")

# labels <- readRDS("intensities_by_date.RDS")
