library(tidyverse)
library(rvest)
library(fs)
library(curl)

`%.%` <- paste0
base_link <- "https://www.mfa.gov.cn/web/wjdt_674879/fyrbt_674889"
index_links =  base_link %.% c("/index", "/index_" %.% 1:28) %.% ".shtml"

#### Get list of links to be scraped

# get_article_links <- function(ind){
#   html_data <- read_html(ind)
#   links = html_data %>% html_nodes(".list1 a") %>% html_attr("href")
#   article_links <- path_norm(path(base_link, links))
#   return(article_links)
# }

# all_art_links <- lapply(index_links, get_article_links)
# all_art_links_vec <- all_art_links %>% unlist()

# saveRDS(all_art_links_vec, "article_urls.RDS")
all_art_links_vec <- readRDS("article_urls.RDS")

#### Scrape and save
# for (i in 1:length(all_art_links_vec)) {
#   article_link = all_art_links_vec[i]
#   page <- rvest::read_html(article_link %>% curl)
#   xml2::write_html(page, "raw/" %.% i)
#   Sys.sleep(0.5)
# }

# Parse webpages
get_page_content <- function(page) {

  # Get page title
  title = page %>% html_nodes("#News_Body_Title") %>% html_text2()
  time = page %>% html_nodes("#News_Body_Time") %>% html_text2()
  if(is_empty(title)){
    title = page %>% html_element("h1") %>% html_text2()
  }
  if(is_empty(time)){
    time = page %>% html_element(".time") %>% html_text2()
  }
  raw = page %>% html_nodes("#News_Body_Txt_A") %>% html_nodes("p")
  text = raw %>% html_text2() %>% as.character(1:length(text))
  filtered_out <- which(!str_detect(text, "\\p{script=Han}"))
  text = text %>% str_subset("\\p{script=Han}") %>% # Keep only paragraphs that contain CJK characters
    str_replace("^\\s+", "")

  q_text <- raw %>% xml2::xml_find_all("strong", flatten = F) %>%
    sapply(\(x) html_text2(x) %>% paste(collapse = "")) %>% unlist() %>%
    {{pos <<- .}} %>% .[. != ""] %>%
    str_subset("\\p{script=Han}") %>%
    str_replace("^\\s+", "")

  if(!is_empty(q_text)){
    q_id <- which(pos != "" & str_detect(pos, "\\p{script=Han}"))
    q_id = q_id - sapply(q_id, \(x) sum(filtered_out < x))
  } else {
    if (length(text) == 1){
      text = strsplit(text, "\\n") %>%
        unlist() %>%
        str_replace("^\\s+", "")
    }
    q_id = str_detect(text, "^\\s*(.+记者|.+报|问)(:|：)") %>% which
    q_text <- text[q_id]
  }

  if(is_empty(q_text)) {
    return(data.frame(title = title,
                      time = time,
                      background_text = NA,
                      question = NA,
                      answer = text %>% paste(collapse = "\n")))
  }

  background_id = if(q_id[1] != 1) 1:(q_id[1]-1) else NULL # find background text IDs
  background_text = if (!is.null(background_id)) paste(text[background_id], collapse = "") else NA
  answer_segments <- cut(1:length(text), breaks = c(q_id,Inf), right = F)
  answer_segments[q_id] <- NA

  answer_text = split(text, answer_segments, drop = T) %>%
    sapply(function(x) paste(x, collapse = "\n"))

  out <- data.frame(title = title,
                    time = time,
                    background_text = background_text,
                    question = text[q_id],
                    answer = answer_text)

  row.names(out) <- NULL
  return(out)
}

list_pages <- list.files("raw", full.names = T)

process_data <- function(i){
  path_to_page = list_pages[i]
  page = xml2::read_html(path_to_page)
  temp = page %>% get_page_content()
  saveRDS(temp, file = "data/text_" %.%
            i %.% "_" %.% temp$time %>% unique() %>% gsub("\\:|\\s","_",.))
  return(0)
}

library(future.apply)
plan(multisession)
future_sapply(1:length(list_pages), process_data)
