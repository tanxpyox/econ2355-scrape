library(tidyverse)
library(magrittr)

`%.%` <- paste0
events_data_dir <- "D:/NGEC"

df <- read_csv("dataset_corpus.csv")
range(df$time)

# list_eventsets <- list.files(events_data_dir, full.names = T)
#
# list_eventsets %<>% .[grepl("^[^0-9]+202[0-4]",.)]
#
# events <- NULL
#
# for(f in list_eventsets){
#   message(f)
#   temp = read_tsv(f)
#   temp %<>% filter(`Actor Country` %>% str_detect("China"),
#                    `Recipient Country` %>% str_detect("United States"))
#
#   if(any(names(temp) == "Event Intensity")) {
#     temp %<>% rename(
#       Intensity = `Event Intensity`
#     )
#   }
#
#   if(!is.null(events)){
#     common_cols = intersect(names(events), names(temp))
#     events = rbind(events[common_cols], temp[common_cols])
#   } else {
#     events = temp %>% select(`Event ID`, `Event Date`, `Event Type`, `Event Mode`, `Intensity`,
#                              `Contexts`, `Actor Name`, `Actor Country`, `Recipient Name`, `Recipient Country`)
#   }
# }

# saveRDS(events, "full_events.RDS")
events <- readRDS("full_events.RDS")

events$ID <- str_extract(events$`Event ID`, "^[0-9]{8}-[0-9]{4}")
events %<>% group_by(ID) %>%
  summarise(Intensity = mean(Intensity),
            EventType = names(sort(-table(`Event Type`)))[1],
            Date = `Event Date`[1])

events %<>% mutate(
  # usa = str_detect(`Recipient Country`, "United States"),
  # taiwan = str_detect(`Recipient Country`, "Taiwan"),
  # japan = str_detect(`Recipient Country`, "Japan"),
  coop = case_when(
    Intensity > 0 ~ 2, # Cooperative
    Intensity < 0 ~ 0, # Conflictual
    Intensity == 0 ~ 1 # Neutral
  )
)

# select only observations concerning US, Japan and Taiwan

df %<>% filter(usa == 1)
df$date <- date(df$time)

begin <- min(df$date)
end <- max(df$date)

calculate <- function(d) {
  t <- events %>% filter(`Date` == d)
  return(c(date = d,
         "avg_intensity" = mean(t$Intensity, na.rm = T)
  ))
}

labels <- sapply((begin-5):(end+5), calculate) %>% t() %>% as.data.frame()
labels$std_intensity <- (labels$avg_intensity - mean(labels$avg_intensity))/sd(labels$avg_intensity)
labels$date %<>% as_date()

saveRDS(labels, "intensities_by_date.RDS")
saveRDS(events, "events.RDS")
