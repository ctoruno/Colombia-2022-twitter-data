## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Twitter data tracking
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##                    David Granada Donato        (dagrado@gmail.com)
##
## Creation date:     November 24th, 2021
##
## This version:      December 7th, 2021
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Working directory set (when working outside project)
# setwd("/Users/carlostorunopaniagua/Documents/GitHub/Political-Observatory-Colombia/")
# rm(list=ls())

# Notes:
#         

# Required packages
lapply(list("rtweet", "glue", "dplyr", "purrr", "readr", "stringr", "magrittr"), 
       library, character.only = T)

# Loading worspace
load("./Data/extraction_workspace.RData")

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

candidates <- list("Gustavo Petro" = c("@petrogustavo", "petro"), 
                   "Francia Márquez" = c("@FranciaMarquezM", "francia"),
                   "Roy Leonardo Barreras" = c("@RoyBarreras", "roy"), 
                   "Arelis Uriana" = c("@urianaguariyu", "arelis"),
                   "Luis Fernando Velasco" = c("@velascoluisf", "velasco"),
                   "Camilo Romero" = c("@CamiloRomero", "camilo"),
                   "Rodolfo Hernández" = c("@ingrodolfohdez", "hernandez"),
                   "Jorge Enrique Robledo" = c("@JERobledo", "robledo"),
                   "Carlos Amaya" = c("@CarlosAmayaR", "amaya"), 
                   "Sergio Fajardo" = c("@sergio_fajardo", "fajardo"),
                   "Alejandro Gaviria" = c("@agaviriau", "gaviria"),
                   "Juan Manuel Galán" = c("@juanmanuelgalan", "galan"),
                   "Juan Fernando Cristo" = c("@CristoBustos", "cristo"),
                   "Enrique Peñalosa" = c("@EnriquePenalosa", "penalosa"),
                   "Federico Gutiérrez" = c("@FicoGutierrez", "federico"),
                   "Juan Carlos Echeverry" = c("@JCecheverryCol", "echeverry"),
                   "Alejandro Char" = c("@AlejandroChar", "char"),
                   "Dilian Francisca Toro" = c("@DilianFrancisca", "toro"),
                   "David Barguil" = c("@davidbarguil", "barguil"),
                   "John Milton Rodríguez" = c("@JohnMiltonR_", "milton"),
                   "Aydeé Lizarazo" = c("@aydeelizarazoc", "lizarazo"),
                   "Óscar Iván Zuluaga" = c("@OIZuluaga", "zuluaga"),
                   "Luis Pérez"= c("@Luis_Perez_G", "perez"),
                   "Eduardo Verano"= c("@veranodelarosa", "verano"))

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
prev_batch_max.ls <- list("parties1" = batches.df$parties1_created_at[1],
                          "parties2" = batches.df$parties2_created_at[1],
                          "candidates1" = batches.df$candidates1_created_at[1],
                          "candidates2" = batches.df$candidates2_created_at[1])

# Extracting Twitter info
raw_tweets.ls <- map2(queries.ls, prev_batch_max.ls, 
                      function(query, tweet_id){
                        
                        print("STARTING A NEW QUERY")
                        print(paste0("Searching tweets for: ", query))
                        print(paste0("Last tweet from previous batch was tweeted on: ", tweet_id))
                        
                        # Extracting tweets
                        ntweets <- 0
                        data.df <- search_tweets(query, n = 7000, 
                                                 retryonratelimit = F, 
                                                 include_rts = F, lang = "es")
                        
                        # Do we have the last tweet from the previous batch?
                        tweet_match <- min(data.df$created_at) < tweet_id
                        print(paste0("Was previous batch reached? ", tweet_match))
                        
                        # Checking extraction limit
                        ntweets <- ntweets + 7000
                        print(paste0("Current extraction have reached ", ntweets, " tweets."))
                        if (ntweets > 14000) {
                          print("Waiting 15 minutes for limit to reset...")
                          Sys.sleep(900)
                          ntweets <- 0
                        }
                        
                        # If we haven't reached the latest tweet from the previous batch, then...
                        while (tweet_match == FALSE) {
                          
                          print(paste0("Procuring more tweets for: ", query))
                          min.tweet <- data.df %>%
                            slice_min(created_at, n = 1) %>%
                            pull(status_id)
                          print(paste0("Last tweet from current extraction is: ", min.tweet))
                          
                          add_data.df <- search_tweets(query, n = 4000, 
                                                       retryonratelimit = F, 
                                                       include_rts = F, lang = "es",
                                                       max_id = min.tweet[1])
                          
                          data.df <- bind_rows(data.df, add_data.df) %>% 
                            distinct(status_id, .keep_all = T)
                          
                          # Do we have the last tweet from the previous batch?
                          tweet_match <- min(data.df$created_at) < tweet_id
                          print(paste0("Was previous batch reached? ", tweet_match))
                          
                          # Checking extraction limit
                          ntweets <- ntweets + 4000
                          print(paste0("Current extraction have reached ", ntweets, " tweets."))
                          if (ntweets > 14000) {
                            print("Waiting 15 minutes for limit to reset...")
                            Sys.sleep(900)
                            ntweets <- 0
                          }
                        }
                        
                        # Forcing extraction to wait before next query if not done previously
                        if (ntweets != 0) {
                          print("Waiting 15 minutes for limit to reset...")
                          Sys.sleep(900)
                        }
                        
                        print("Final dataset is:")
                        print(head(data.df[2:4], 5))
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

# Loading previous version of master data (if not loaded from data image)
prev_master_filepath <- list.files("./Data/Master/",
                               pattern = "csv$",
                               full.names = T) %>% extract(which.max(file.mtime(.)))
master_data.df <- read_twitter_csv(prev_master_filepath, unflatten = F) %>%
  mutate(created_at = as.POSIXct(created_at))

# Cleaning and adding recently extracted tweets to master dataset
master_data.df <- master_data.df %>%
  bind_rows(raw_tweets.df %>%
              select(1:5, is_quote, favorite_count, retweet_count) %>%
              
              # Creating filters per candidate
              bind_cols(map2_dfc(candidates %>%  map_chr(1),
                                 candidates %>%  map_chr(2),
                                 function(tag, name) {
                                   var_name <- paste0("filter_", name)
                                   master_data.df %>%
                                     mutate(!!var_name := if_else(str_detect(text, tag), 1, 0)) %>%
                                     select(!!var_name)
                                 }))) %>%
  distinct(status_id, .keep_all = T)
  
# Saving master dataset version
write_as_csv(master_data.df, 
             paste0("./Data/Master/_master_data_", format(Sys.Date(), "%Y%m%d"), ".csv"))

# Saving workspace
save.image(file = "./Data/extraction_workspace.RData")
