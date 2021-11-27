## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Twitter data tracking
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##                    David Granada Donato        (dagrado@gmail.com)
##
## Creation date:     November 24th, 2021
##
## This version:      November 24th, 2021
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##                1.  
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Working directory set (when working outside project)
# setwd("/Users/carlostorunopaniagua/Documents/MA in Development Economics/Thesis/Data")
rm(list=ls())

# Required packages
lapply(list("rtweet", "dplyr", "purrr", "readr", "stringr"), 
       library, character.only = T)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  Defining extraction algorithm                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Political parties official twitter accounts
parties_query1 <- paste("@MovimientoMAIS OR @PartidoVerdeCoL OR @CeDemocratico OR @NvLiberalismo",
                        "@PartidoLiberal OR @ColombiaHumana_ OR @UP_Colombia OR @LaFuerzaDLaPaz",
                        sep = " OR ")

parties_query2 <- paste("@SoyRenaciente OR @PactoCol OR @PartidoMIRA OR @PoloDemocratico",
                        "@compromisociu OR @PAC_Colombia OR @partidodelaucol OR @PCambioRadical",
                        sep = " OR ")

# Precandidates official twitter accounts
candidates_query1 <- paste("@petrogustavo OR @FranciaMarquezM OR @RoyBarreras OR @urianaguariyu",
                           "@velascoluisf OR @CamiloRomero OR @ingrodolfohdez OR @JERobledo",
                           "@CarlosAmayaR OR @sergio_fajardo OR @agaviriau OR @juanmanuelgalan",
                           sep = " OR ")

candidates_query2 <- paste("@CristoBustos OR @EnriquePenalosa OR @FicoGutierrez OR @JCecheverryCol",
                           "@AlejandroChar OR @DilianFrancisca OR @davidbarguil OR @JohnMiltonR_",
                           "@aydeelizarazoc OR @OIZuluaga OR @Luis_Perez_G OR @veranodelarosa",
                           sep = " OR ")

# Other important Twitter accounts


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Applying extraction                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Retrieving past batches info
batches.df <- read_delim("./Data/batch_log.csv", delim = ";",
                         col_types = cols("parties1_status_id" = "c",
                                          "parties2_status_id" = "c",
                                          "candidates1_status_id" = "c",
                                          "candidates2_status_id" = "c"))
      # I specify col_types for the status_id columns because readr classifies them as double instead of
      # character by default. As a huge number, it gives a scientific notation which can give troubles
      # when matching the identifier.

# Defining input queries and previous batch latest tweet
queries.ls <- list("parties1" = parties_query1,
                   "parties2" = parties_query2,
                   "candidates1" = candidates_query1,
                   "candidates2" = candidates_query2)
prev_batch_max.ls <- list("parties1" = batches.df$parties1_status_id[1],
                          "parties2" = batches.df$parties2_status_id[1],
                          "candidates1" = batches.df$candidates1_status_id[1],
                          "candidates2" = batches.df$candidates2_status_id[1])

# Extracting Twitter info
raw_tweets.ls <- map2(queries.ls, prev_batch_max.ls, 
                      function(query, tweet_id){
                        
                        print(paste0("Searching tweets for: ", query))
                        data.df <- search_tweets(query, n = 4000, 
                                                 retryonratelimit = T, 
                                                 include_rts = F, lang = "es") 
                        tweet_match <- sum(str_detect(data.df$status_id, tweet_id))
                        print(paste0("Result for previous query match is: ", tweet_match))
                        
                        # If we haven't reached the latest tweet from the previous batch, then...
                        while (tweet_match == 0) {
                          
                          print(paste0("Procuring more tweets for: ", query))
                          min.tweet <- data.df %>%
                            slice_min(created_at, n = 1) %>%
                            pull(status_id)
                          add_data.df <- search_tweets(query, n = 2000, 
                                                       retryonratelimit = T, 
                                                       include_rts = F, lang = "es",
                                                       max_id = min.tweet)
                          
                          data.df <- bind_rows(data.df, add_data.df) %>% 
                            distinct(status_id, .keep_all = T)
                          tweet_match <- sum(str_detect(data.df$status_id, tweet_id))
                          print(paste0("Result for previous query is: ", tweet_match))
                        }
                        return(data.df)
                      })

# Updating batch file
batches.df <- bind_rows(tribble(~Date, 
                                Sys.Date()) %>% bind_cols(imap(raw_tweets.ls, 
                                                               function(query, oname){
                                                                 value <- query %>%
                                                                   slice_max(created_at, n = 1) %>%
                                                                   select(created_at, status_id)
                                                                 names(value) <- paste(oname, names(value),
                                                                                       sep = "_")
                                                                 return(value)
                                                               }) %>% bind_cols()),
                        batches.df)

# Saving new batch file
write_delim(batches.df, "./Data/batch_log.csv", delim = ";")

# Saving raw data
raw_tweets.df <- bind_rows(raw_tweets.ls) %>%
  distinct(user_id, screen_name, created_at, .keep_all = T)
write_as_csv(raw_tweets.df, 
             paste0("./Data/RawExtracts/_elections_tweets_col_", format(Sys.Date(), "%Y%m%d"), ".csv"))

  
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                3.  Cleaning extracted data                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tweets.df <- raw_tweets.df %>%
  (1:5, is_quote, favorite_count, retweet_count)

