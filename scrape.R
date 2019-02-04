
### load required libraries
library(tidyverse)
library(rvest)


### functions to parse speaker's name and party
# text_to_parse has format "[title]. [name] ([position], [party]): [speech]"
title_prefix <- c("Hon.", "Right Hon.", "L'hon.", "L’hon.", "Le très hon.",
                  "M.", "Ms.", "Mrs.", "Mme",
                  "Mr.",
                  "The") %>% 
  paste0("^", ., "[[:space:]]+", collapse = "|") %>% 
  gsub("\\.", "\\\\\\.", .)

extract_name <- function(speaker, prefix_str = title_prefix) {
  nm <- str_squish(strsplit(speaker, "\\:|\\(")[[1]][1])
  nm <- gsub("Member", "member", nm)
  
  if (grepl("member", nm, ignore.case = TRUE)) {
    nm_std <- nm
  } else {
    nm_std <- gsub(prefix_str, "", nm)
  }
  
  return(nm_std)
}

extract_party <- function(speaker) {
  parties <- c("CPC", "Lib.", "NDP", "BQ", "GP", "FD", "Ind.")
  parties_regex <- gsub("$", ")", parties)
  party_index <- str_detect(speaker, coll(parties_regex))
  
  return(ifelse(any(party_index == TRUE),
                paste(parties[party_index], collapse = ", "),
                NA_character_))
}

speakers_lookup <- function(speakers) {
  name <- vapply(speakers, extract_name, "", USE.NAMES = FALSE)
  party <- vapply(speakers, extract_party, "", USE.NAMES = FALSE)
  
  # would be ideal to check for instances of multiple party affil
  data.frame(name, party, stringsAsFactors = FALSE) %>% 
    filter(!is.na(party)) %>% 
    unique()
}




### function to get and parse hansard transcripts of daily parliamentary debates
parse_hansard <- function(url) {

  # url <- "https://www.ourcommons.ca/DocumentViewer/en/42-1/house/sitting-27/hansard"
  
  # get url
  page <- read_html(url)
  
  # get date
  date <- page %>% 
    html_node("#publicationDate") %>% 
    html_text() %>% 
    as.Date(date, format = "%A, %B %d, %Y")
  
  # get headers
  headers <- page %>% 
    html_nodes("h2") %>% 
    html_text() %>% 
    str_trim(side = "left")
  
  # find which header corresponds to question period
  # if no headers correspond to question period, return NULL
  qperiod_index <- grep("oral questions", headers, ignore.case = TRUE)
  if(length(qperiod_index) == 0) return(NULL)

  # get speakers
  speakers <- page %>% 
    html_nodes(xpath = "//span[@class='paratext']//b") %>% 
    html_text() %>% 
    subset(nchar(.) > 1) %>% 
    unique() %>% 
    str_trim(side = "left")
  
  # speaker lookup table
  df_speaker <- speakers_lookup(speakers)
  
  # get language
  lang <- page %>% 
    html_nodes(xpath = "//p[@class='floorlanguage']") %>% 
    html_text() %>% 
    unique()

  # collapse speakers into single regex string; escape parentheses with \\
  speakers_string <- paste0("^", paste(speakers, collapse = "|^"))
  speakers_string <- gsub("\\(", "\\\\\\(", speakers_string)
  speakers_string <- gsub("\\)", "\\\\\\)", speakers_string)

  # get lines of speech, including section and language headers
  lines <- page %>% 
    html_nodes(xpath = "//span[@class='paratext']|//h2|//p[@class='floorlanguage']") %>% 
    html_text() %>% 
    str_trim(side = "left")
  
  # get index corresponding to first line of question period
  lines_start <- grep(headers[qperiod_index], lines) + 1
  
  # get index corresponding to last line of question period
  if(qperiod_index == length(headers)) {
    lines_end <- length(lines)
  } else {
    lines_end <- grep(headers[qperiod_index + 1], lines)
    lines_end <- min(lines_end[lines_end > lines_start]) - 1
  }
  
  # extract subset of lines of speech corresponding to question period
  lines_qperiod <- lines[lines_start:lines_end] 
  
  # identify language of each line of speech
  lang_new_i <- which(lines_qperiod %in% lang)
  lang_new <- lines_qperiod[lang_new_i]
  lang_new <- gsub("\\[|\\]", "", lang_new)
  lang_end_i <- c(lang_new_i[-1] - 1, length(lines_qperiod))
  lang_n_rep <- lang_end_i - lang_new_i
  language <- unlist(mapply(rep, lang_new, lang_n_rep, USE.NAMES = FALSE))
  
  # remove lines that only indicate new language
  lines_qperiod <- lines_qperiod[-lang_new_i]
  
  # identify speaker
  speaker_new_i <- grep(speakers_string, lines_qperiod)
  speaker_new_char_i <- str_locate(lines_qperiod[speaker_new_i], speakers_string)
  speaker_new <- str_sub(lines_qperiod[speaker_new_i], speaker_new_char_i)
  speaker_end_i <- c(speaker_new_i[-1] - 1, length(lines_qperiod))
  speaker_n_rep <- speaker_end_i - speaker_new_i + 1
  speaker <- unlist(mapply(rep, speaker_new, speaker_n_rep, USE.NAMES = FALSE))
  
  # turn of talk
  turn <- unlist(mapply(rep, seq_along(speaker_new), speaker_n_rep, USE.NAMES = FALSE))
  
  # extract speech (remove speaker info) from lines_qperiod
  speech <- gsub(speakers_string, "", lines_qperiod)
  
  # remove whitespace
  speech <- str_squish(speech)
  speaker <- str_squish(speaker)
  
  # extract speaker's name and party 
  name <- vapply(speaker, extract_name, "", USE.NAMES = FALSE)
  party <- vapply(speaker, extract_party, "", USE.NAMES = FALSE)
  
  length(name)
  length(party)
  length(turn)
  length(language)
  
  # assemble output data frame
  out <- data.frame(date, turn, name, party, language, speech,
                    stringsAsFactors = FALSE)

  # match missing party info with known party info in df_speaker
  party_na_i <- which(is.na(out$party))
  name_na <- out$name[party_na_i]
  match_i <- match(name_na, df_speaker$name)
  out$party[party_na_i] <- df_speaker$party[match_i]
  
  # return
  return(out)
}



### get urls for each sittings of the house of commons
url_sitting <- paste0(
  "https://www.ourcommons.ca/DocumentViewer/en/42-1/house/sitting-", 1:338,
  "/hansard"
)

url_sitting <- paste0(
  "https://www.ourcommons.ca/DocumentViewer/en/41-2/house/sitting-", 1:235,
  "/hansard"
)

url_sitting <- paste0(
  "https://www.ourcommons.ca/DocumentViewer/en/41-1/house/sitting-", 1:272,
  "/hansard"
)



### compile text from every question period in the 41st parliament
out_l <- vector("list", length(url_sitting))

for (i in seq_along(url_sitting)) {
  out_l[[i]] <- try(parse_hansard(url_sitting[i]))
  
  # if error, wait 20 seconds and try again
  if (class(out_l[[i]]) == "try-error") {
    Sys.sleep(20)
    out_l[[i]] <- try(parse_hansard(url_sitting[i]))
  }
  
  print(i)
  Sys.sleep(runif(1, 6, 9))  # be kind to the gc.ca servers
}




### unlist and write to file
out_df1 <- do.call(rbind, out_l)
write.csv(out_df1, "data/parl-42-1-raw.txt", row.names = FALSE)

out_df2 <- do.call(rbind, out_l)
write.csv(out_df2, "data/parl-41-2-raw.txt", row.names = FALSE)

out_df3 <- do.call(rbind, out_l)
write.csv(out_df3, "data/parl-41-1-raw.txt", row.names = FALSE)


