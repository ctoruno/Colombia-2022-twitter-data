## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Twitter data analysis
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##                    David Granada Donato        (dagrado@gmail.com)
##
## Creation date:     November 28th, 2021
##
## This version:      November 28th, 2021
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##                1.  
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Working directory set (when working outside project)
# setwd("/Users/carlostorunopaniagua/Documents/MA in Development Economics/Thesis/Data")
# rm(list=ls())

# Required packages
lapply(list("rtweet", "haven", "qdap", "tm", "syuzhet", "SnowballC", "wesanderson",
            "tidytext", "dplyr", "purrr", "readr", "stringr", "magrittr"),
       library, character.only = T)

# Notes: "webshot", "htmlwidgets", "wordcloud2"


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  Frequency Analysis                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Creating a VCorpus
tweets.corpus <- master_data.df %>%
  select(text, created_at, screen_name) %>%
  mutate(text = str_replace_all(tolower(text), c("á" = "a", "é" = "e", "í" = "i", 
                                                 "ó" = "o", "ú|ü" = "u"))) %>%
  relocate(status_id, .before = user_id) %>%
  DataframeSource() %>%
  VCorpus() %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, chartr("áéíóúü", "aeiouu", stopwords("es")))
