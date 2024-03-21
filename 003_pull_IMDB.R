library(rvest)
library(httr)
library(tidyverse)
library(lubridate)
library(glue)
library(zoo)

setwd("/Users/veronica/Documents/Genetics in Media/")

#### pull and clean film and episode data ####

# IMDB <- read_delim("title.basics.tsv")
# IMDB <- IMDB %>%
#     filter(isAdult ==  0) %>%
#     select(-"titleType", -"isAdult")
# IMDB <- IMDB[!grepl("Adult", IMDB$genres),]
# IMDB <- IMDB[!grepl("Short", IMDB$genres),]
# IMDB <- IMDB[!grepl("Documentary", IMDB$genres),]
# IMDB <- IMDB[!grepl("Talk-Show", IMDB$genres),]
# IMDB <- IMDB[!grepl("Music", IMDB$genres),]
# IMDB <- IMDB[!grepl("Sport", IMDB$genres),]
# IMDB <- IMDB[!grepl("Reality-TV", IMDB$genres),]
# IMDB <- IMDB[!grepl("Game-Show", IMDB$genres),]
# IMDB <- IMDB[!grepl("\\N", IMDB$genres),]
# IMDB$runtimeMinutes <- as.integer(IMDB$runtimeMinutes)
# IMDB <- IMDB[(IMDB$runtimeMinutes > 30),]
# IMDB$startYear <- as.integer(IMDB$startYear)
# IMDB <- IMDB[complete.cases(IMDB),]
# IMDB <- IMDB %>%
#     filter(startYear <= 2023)
# 
# write.csv(IMDB, "003_IMDB.csv", row.names = F)

IMDB <- read.csv("003_IMDB.csv")

## for first time, uncomment list initiation and run with 1:nrow(IMDB)
#list_keywords <- list()
load("003_list_IMDB_keywords.RData")
e <- length(list_keywords) + 1

for(e in e:nrow(IMDB)){ # e for later times so restarts where dropped
    keyword_html <- read_html(paste0("https://www.imdb.com/title/",
                                     IMDB[e, "tconst"],
                                     "/keywords/"))
    keywords <- html_nodes(keyword_html, # will pull up to top 50 keywords
                           '[class="ipc-metadata-list-summary-item__t"]') %>%
        html_text()
    
    
    list_keywords[[IMDB[e, "tconst"]]] <- keywords
    if(e %% 1000 == 0){ # save data every 1000 titles
        save(list_keywords, file = "003_list_IMDB_keywords.RData")
        gc()
    }
}

save(list_keywords, file = "003_list_IMDB_keywords.RData")







