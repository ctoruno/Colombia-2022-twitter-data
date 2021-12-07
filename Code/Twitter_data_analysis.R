## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Twitter data analysis
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##                    David Granada Donato        (dagrado@gmail.com)
##
## Creation date:     November 28th, 2021
##
## This version:      December 3rd, 2021
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Working directory set (when working outside project)
# setwd("/Users/carlostorunopaniagua/Documents/GitHub/Political-Observatory-Colombia/")
# rm(list=ls())

# Required packages
lapply(list("rtweet", "haven", "qdap", "tm", "syuzhet", "SnowballC", "wesanderson",
            "tidytext", "dplyr", "purrr", "readr", "stringr", "magrittr", 
            "webshot", "htmlwidgets", "wordcloud2"),
       library, character.only = T)

# Notes: 


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                0.  Loading info from extraction                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Extraction keywords (if not loaded from data image)
parties_query1 <- paste("@MovimientoMAIS OR @PartidoVerdeCoL OR @CeDemocratico OR @NvLiberalismo",
                        "@PartidoLiberal OR @ColombiaHumana_ OR @UP_Colombia OR @LaFuerzaDLaPaz",
                        sep = " OR ")

parties_query2 <- paste("@SoyRenaciente OR @PactoCol OR @PartidoMIRA OR @PoloDemocratico",
                        "@compromisociu OR @PAC_Colombia OR @partidodelaucol OR @PCambioRadical",
                        sep = " OR ")

candidates_query1 <- paste("@petrogustavo OR @FranciaMarquezM OR @RoyBarreras OR @urianaguariyu",
                           "@velascoluisf OR @CamiloRomero OR @ingrodolfohdez OR @JERobledo",
                           "@CarlosAmayaR OR @sergio_fajardo OR @agaviriau OR @juanmanuelgalan",
                           sep = " OR ")

candidates_query2 <- paste("@CristoBustos OR @EnriquePenalosa OR @FicoGutierrez OR @JCecheverryCol",
                           "@AlejandroChar OR @DilianFrancisca OR @davidbarguil OR @JohnMiltonR_",
                           "@aydeelizarazoc OR @OIZuluaga OR @Luis_Perez_G OR @veranodelarosa",
                           sep = " OR ")

# Loading previous version of master data (if not loaded from data image)
prev_master_filepath <- list.files("./Data/Master/",
                                   pattern = "csv$",
                                   full.names = T) %>% extract(which.max(file.mtime(.)))
master_data.df <- read_twitter_csv(prev_master_filepath, unflatten = F) %>%
  mutate(created_at = as.POSIXct(created_at))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  Frequency Analysis                                                                    ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Creating a VCorpus
tweets.corpus <- master_data.df %>%
  select(1:5) %>%
  mutate(doc_id = as.character(row.names(.)),
         text = str_replace_all(tolower(text), c("á" = "a", "é" = "e", "í" = "i", 
                                                 "ó" = "o", "ú|ü" = "u"))) %>%
  relocate(doc_id, .before = user_id) %>%
  DataframeSource() %>%
  VCorpus() %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, chartr("áéíóúü", "aeiouu", stopwords("es")))

# Tokenizing text and removing stop words
twitter_tokenized.df <- master_data.df %>%
  mutate(text = str_replace_all(tolower(text), c("á" = "a", "é" = "e", "í" = "i", 
                                                 "ó" = "o", "ú|ü" = "u"))) %>%
  select(1:5) %>%
  mutate(tweet_id = row_number()) %>%
  unnest_tokens(words, text, token = "tweets", strip_url = T) %>%
  anti_join(data.frame(words = stopwords("es")) %>%
              mutate(words = str_replace_all(words,
                                             c("á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú|ü" = "u"))))

# Removing custom stop words
check <- twitter_tokenized.df %>% 
  count(words) %>% 
  arrange(desc(n))  # just to check remaining stopwords

twitter_tokenized.df <- twitter_tokenized.df %>%      # we remove stopwords in the top-150
  anti_join(tribble(~words, "ahora", "hacer", "hace", "puede", "mismo", "tan", "señor", "ud", "siempre", 
                    "menos","dice", "debe", "ver", "hoy", "sabe", "van", "quiere", "creo", "ustedes", 
                    "decir", "pues", "cabal", "vamos", "nunca", "claro", "ahi", "jajaja", "jajajaja", 
                    "entonces", "gran", "vez", "da", "toda", "d", "favor", "parte", "quieren", "cada", 
                    "hizo", "hecho", "tener", "dijo", "aqui", "cree", "tal", "parece", "hacen", 
                    "despues", "que", "usted", "solo", "ser", "asi", "va", "años", "habla", "tipo", 
                    "misma", "cosas", "5", "necesita", "alguien", "todas", "aun", "sino", "cosa", 
                    "x", "q"))

# Creating word counts
wcount_raw.df <- twitter_tokenized.df %>% count(words) %>% arrange(desc(n))
keywords <- paste(candidates_query1, candidates_query2, parties_query1, parties_query2, sep = " ") %>%
  str_replace_all(" OR ", "|")
wcount_flt.df <- wcount_raw.df %>%    # Removing keywords used to extract tweets
  filter(!(str_detect(words, regex(keywords, ignore_case = T))))

# No. of mentions per candidate
mentions.df <- tweets_opp.df %>%
  select(screen_name, created_at, text) %>%
  mutate(mora = if_else(str_detect(text, regex("mora|miguel", ignore_case = T)), 1, 0),
         cris = if_else(str_detect(text, regex("cristiana", ignore_case = T)), 1, 0),
         jsch = if_else(str_detect(text, regex("jsch|sebastian|sebastián", ignore_case = T)), 1, 0),
         cruz = if_else(str_detect(text, regex("arturo", ignore_case = T)), 1, 0),
         geor = if_else(str_detect(text, regex("george", ignore_case = T)), 1, 0),
         meda = if_else(str_detect(text, regex("medardo", ignore_case = T)), 1, 0),
         mara = if_else(str_detect(text, regex("maradiaga", ignore_case = T)), 1, 0),
         cham = if_else(str_detect(text, regex("chamorro", ignore_case = T)), 1, 0),
         date = as.Date(created_at, format="%Y-%m-%d %H:%M:%S"))

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Word Clouds                                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set.seed(31478)

#Setting wordclouds directory
setwd("/Users/carlostorunopaniagua/Documents/GitHub/Political-Observatory-Colombia/Data/Wordclouds/")
      # For some reason, I cant change or modify the saving directory using saveWidget

# Creating and saving a general Wordcloud
wordcloud.gen <- wordcloud2(wcount_flt.df[1:200,], size = 0.9,
                            color = rep_len(c("DarkRed", "CornflowerBlue", "DarkOrange"),
                                            nrow(wcount_flt.df[1:200,])),
                            ellipticity = 0.2, shuffle = F)
saveWidget(wordcloud.gen, "wcloud-gen.html", selfcontained = F)
webshot::webshot("wcloud-gen.html","wcloud-gen.png",
                 vwidth = 1600, vheight = 1000, delay =10, selector = '#canvas')

#Setting original directory again
setwd("/Users/carlostorunopaniagua/Documents/GitHub/Political-Observatory-Colombia/Data/")

# Saving workspace
save.image(file = "./Data/cleaning_workspace.RData")
