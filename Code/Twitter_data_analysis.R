## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Twitter data analysis
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##                    David Granada Donato        (dagrado@gmail.com)
##
## Creation date:     January 28th, 2021
##
## This version:      February 25nd, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Required packages
lapply(list("rtweet", "haven", "tm", "topicmodels", "syuzhet", "SnowballC", "wesanderson",
            "plotly", "wordcloud2", "DT", "tidytext", "tidyverse", "magrittr", "qdap"),
       library, character.only = T)

# Loading workspace
load("./Data/twitter_data4dash.RData")

# Specifying dashboard directory to save data to
#dash_directory <- "/Users/carlostorunopaniagua/Documents/GitHub/Colombia-2022-Dashboard/data/"
dash_directory <-"/Users/dagra/OneDrive/Documentos/GitHub/Colombia-2022-Dashboard/data/"

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                0.  Defining cleaning functions                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Defining extraction keywords
keywords <- paste(candidates_query1, candidates_query2, 
                  parties_query1, parties_query2, 
                  sep = " ") %>%
  str_replace_all(" OR ", "|")

# Filtering function
data4filtering <- function(panel, candidate, inputData){
  
  if (panel == "Social Monitoring"| panel=="Sentiment Trends"){
    
    # In order to filter the master data, we need [candidates.ls %>%  map_chr(2)], but we
    # only have [candidates.ls %>%  map_chr(1)]. Therefore, we need to perform a match.
    index <- match(paste0("@", candidate), candidates.ls %>%  map_chr(1))
    selection <- paste0("filter_", candidates.ls[index] %>% map_chr(2))
    
    filteredData <- inputData %>%
      filter(.data[[selection]] == 1)
    
  } else {
    
    filteredData <- inputData %>%
      filter(screen_name == candidate & is_retweet == F)
    
  }  
}



# Tokenizing function for freq. Analysis
data2tokens <- function(data) {
  
  # Tokenizing text and removing stop words
  twitter_tokenized.df <- data %>%
    mutate(text = str_replace_all(tolower(text), c("á" = "a", "é" = "e", "í" = "i",
                                                   "ó" = "o", "ú|ü" = "u"))) %>%
    select(1:5) %>%
    mutate(tweet_id = row_number()) %>%
    unnest_tokens(words, text, token = "tweets", strip_url = T) %>%
    anti_join(data.frame(words = stopwords("es")) %>%
                mutate(words = str_replace_all(words,
                                               c("á" = "a", "é" = "e", "í" = "i", 
                                                 "ó" = "o", "ú|ü" = "u"))))
  
  # Removing custom stop words
  twitter_tokenized.df <- twitter_tokenized.df %>%      # we remove stopwords in the top-150
    anti_join(tribble(~words, "ahora", "hacer", "hace", "puede", "mismo", "tan", "señor", "ud", 
                      "siempre", "menos","dice", "debe", "ver", "hoy", "sabe", "van", "quiere", 
                      "creo", "ustedes", "decir", "pues", "cabal", "vamos", "nunca", "claro", 
                      "ahi", "jajaja", "jajajaja", "entonces", "gran", "vez", "da", "toda", 
                      "d", "favor", "parte", "quieren", "cada","hizo", "hecho", "tener", "dijo", 
                      "aqui", "cree", "tal", "parece", "hacen", "despues", "que", "usted", "solo", 
                      "ser", "asi", "va", "años", "habla", "tipo", "misma", "cosas", "5", 
                      "necesita", "alguien", "todas", "aun", "sino", "cosa", "x", "q", "pais", 
                      "colombia", "|"))
}

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  Overview Panel Data Inputs                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

overview.ls <- list(
  
  "social_ntweets" = formatC(master_data.df %>% 
                               summarise(count = n()) %>% 
                               pull(count),
                             format="f", big.mark = ",", digits=0),
  "social_nusers"  = formatC(master_data.df %>% 
                               distinct(screen_name) %>% 
                               summarise(count = n()) %>% 
                               pull(count),
                             format="f", big.mark = ",", digits=0),
  "social_tlapse"  = paste0(format(as.Date(master_data.df %>% 
                                             slice_min(created_at, n = 1, with_ties = F) %>% 
                                             pull(created_at)), "%b %dth,%Y"),
                            " and ",
                            format(as.Date(master_data.df %>% 
                                             slice_max(created_at, n = 1, with_ties = F) %>% 
                                             pull(created_at)), "%b %dth,%Y")),
  "speech_ntweets" = formatC(timelines.df %>% summarise(count = n()) %>% pull(count), 
                             format="f", big.mark = ",", digits=0),
  "speech_tlapse"  = paste0(format(as.Date(timelines.df %>% 
                                             filter(is_retweet == F) %>%
                                             slice_min(created_at, n = 1, with_ties = F) %>% 
                                             pull(created_at)), "%b %dth,%Y"),
                            " and ",
                            format(as.Date(timelines.df %>% 
                                             filter(is_retweet == F) %>%
                                             slice_max(created_at, n = 1, with_ties = F) %>% 
                                             pull(created_at)), "%b %dth,%Y")),
  "speech_retwts"  = formatC(timelines.df %>%
                               filter(is_retweet == F) %>%
                               summarise(total_retweets = sum(retweet_count)) %>% 
                               pull(total_retweets), 
                             format="f", big.mark = ",", digits=0),
  "daily_log"      =  (master_data.df %>%
                         select(created_at) %>% 
                         mutate(date = as.Date(created_at, format="%Y-%m-%d %H:%M:%S")) %>% 
                         group_by(day = cut(date, breaks = "day")) %>% 
                         count(day) %>%
                         mutate(date = as.Date(day)) %>%
                         filter(date > as.Date("2021-11-22")) %>%
                         ungroup())
  
)



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Frequency Analysis Data Inputs                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


# Obtaining word counts for freq. analysis
word_counts.ls <- 
  imap(list("Social Monitoring" = master_data.df,
            "Speech Analysis"   = timelines.df), 
         function(inputData, panel) {
           
           # Appplying the analysis to each candidate
           lapply(candidates.ls %>% map_chr(1) %>% str_sub(2),
                  function(candidate){
                    
                    # Applying filtering function
                    filteredData <- data4filtering(panel = panel, 
                                                   candidate = candidate,
                                                   inputData = inputData)

                     # Applying the tokenizing function and renaming variables
                     tokens.df <- data2tokens(data = filteredData)
                     
                     
                     # Creating raw word counts
                     word_counts.df <- tokens.df %>% 
                       count(words) %>% 
                       arrange(desc(n)) %>%    
                       filter(!(str_detect(words, regex(keywords, ignore_case = T))) & # Removing keywords
                                !(str_detect(words, "^@"))) # Removing Twitter tags
                     
                     # Renaming variables to identify the candidate
                     names(word_counts.df) <- c(paste0(candidate, "_words"),
                                                paste0(candidate, "_count"))

                     return(list(word_counts.df, tokens.df))

                   })
         })

# Defining data inputs for freq. analysis
freq_analysis.ls <- lapply(word_counts.ls, function(inputData) {
  
  nestedData <- map(inputData, 1)
  
  # Top words related to or used by each candidate
  top_words.df <- map_dfc(nestedData, function(countsDATA){
    selection <- names(countsDATA)[[2]]
    
    # Adding NAs to  data frames that has less than 200 words
    nrows <- nrow(countsDATA)
    if (nrows < 200){
      added_rows <- tibble(x = rep_along(c(1:(200-nrows)), NA_character_),
                           y = rep_along(c(1:(200-nrows)), NA_real_))
      names(added_rows) <- names(countsDATA)
      countsDATA <- countsDATA %>%
        bind_rows(added_rows)
    }
    
    countsDATA %>%
      slice_max(.data[[selection]], n = 200, with_ties = F)
  })
  
  # Top hashtags related to or used by each candidate
  top_hashtags.df <- map_dfc(nestedData, function(countsDATA){
    selection1 <- names(countsDATA)[[1]]
    selection2 <- names(countsDATA)[[2]]
    
    countsDATA <- countsDATA %>%
      filter(str_detect(.data[[selection1]], "^#"))
    
    # Adding NAs to  data frames that has less than 10 hashtags
    nrows <- nrow(countsDATA)
    if (nrows < 10){
      added_rows <- tibble(x = rep_along(c(1:(10-nrows)), NA_character_),
                           y = rep_along(c(1:(10-nrows)), NA_real_))
      names(added_rows) <- names(countsDATA)
      countsDATA <- countsDATA %>%
        bind_rows(added_rows)
    }
    
    countsDATA %>%
      slice_max(.data[[selection2]], n = 10, with_ties = F)
  })
  
  # Return list
  list("Top Words"    = top_words.df,
       "Top Hashtags" = top_hashtags.df)
  
})


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Daily Activity Data Inputs                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Obtaining daily activity log
daily_activity.ls <- 
  imap(list("Social Monitoring" = master_data.df,
            "Speech Analysis"   = timelines.df), 
       function(inputData, panel) {
         
         # Appplying the analysis to each candidate
         data1candidate.ls <- lapply(candidates.ls %>% map_chr(1) %>% str_sub(2),
                                     function(candidate){
                                       
                                       # Creating a daily log
                                       daily_log.df <- tibble(date = seq.Date(as.Date("2021-11-23"), 
                                                                              max(batches.df$Date), 
                                                                              1))
                                       
                                       # Applying filtering function
                                       filteredData <- data4filtering(panel = panel, 
                                                                      candidate = candidate,
                                                                      inputData = inputData)
                                       
                                       # Collapsing data per day
                                       added_info <- filteredData %>%
                                         select(created_at) %>% 
                                         mutate(date = as.Date(created_at, format="%Y-%m-%d %H:%M:%S")) %>% 
                                         group_by(day = cut(date, breaks = "day")) %>% 
                                         count(day) %>% 
                                         mutate(date = as.Date(day)) %>%
                                         rename(mentions = n) %>%
                                         ungroup() %>%
                                         select(date, mentions)
                                       
                                       # Joining info
                                       daily_log.df <- daily_log.df %>%
                                         left_join(added_info) %>%
                                         mutate(mentions = if_else(is.na(mentions), 0, as.double(mentions)))
                                       
                                       # Renaming variables to identify the candidate
                                       names(daily_log.df) <- c("date",
                                                                paste0(candidate, "_tweets"))
                                       
                                       return(daily_log.df)
                                       
                                     })
         
         # Joining all data frames within list
         purrr::reduce(data1candidate.ls, dplyr::left_join, by = "date")
         
       })


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                3.  User Info Data Input                                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

user_info.df <- users_data(lookup_users(candidates.ls %>% map_chr(1) %>% str_sub(2))) %>%
  left_join(timelines.df %>%
              group_by(screen_name) %>%
              filter(is_retweet == F) %>%
              count(screen_name) %>%
              rename(tweets_count = n)) %>%
  left_join(map_dfr(candidates.ls %>% map_chr(1) %>% str_sub(2),
                    function(candidate){
                      data1 <- data4filtering(panel = "Social Monitoring", 
                                     candidate = candidate, 
                                     inputData = master_data.df) %>%
                        mutate(candidate_screen_name = candidate) %>% 
                        group_by(candidate_screen_name) %>% 
                        summarise(total_comm_tweets = n())
                      
                      data2 <- data4filtering(panel = "Social Monitoring", 
                                              candidate = candidate, 
                                              inputData = master_data.df) %>%
                        mutate(candidate_screen_name = candidate) %>%
                        group_by(candidate_screen_name) %>%
                        count(screen_name) %>%
                        summarise(total_users = n())
                      
                      left_join(data1, data2)
                    }),
            by = c("screen_name" = "candidate_screen_name")) %>%
  left_join(daily_activity.ls$`Social Monitoring` %>% 
              pivot_longer(!date, names_to = "user", 
                           names_pattern = "(.*)_tweets", 
                           values_to = "mentions") %>% 
              group_by(user) %>% 
              slice_max(mentions, n = 1, with_ties = F) %>%
              rename(max_mentions_n = mentions,
                     max_mentions_date = date),
            by = c("screen_name" = "user"))


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                4.  Topic Modelling Data Input                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

tmodels.ls <- map(map(word_counts.ls[["Speech Analysis"]], 2),
                  function(tokenized_data){
                    
                    # candidate <- as.character(tokenized_data[1, "screen_name"])
                    
                    # Casting a Document-Term Matrix
                    DTMatrix <- tokenized_data %>% 
                      count(tweet_id, words) %>% 
                      filter(!str_detect(words, "^@")) %>%
                      cast_dtm(document = tweet_id, term = words, value = n)
                    
                    # Fitting a LDA base model with only 2 topics
                    LDAmodel_base <- LDA(x = DTMatrix, 
                                         k = 4, 
                                         method = "Gibbs",
                                         control = list(
                                           alpha = 0.5,
                                           seed = 10005,
                                           iter = 800))
                    
                    # Saving fit values for base model
                    loglik_base <- logLik(LDAmodel_base)
                    perxty_base <- perplexity(object = LDAmodel_base, 
                                              newdata = DTMatrix)
                    
                    # # Best model
                    # LDAmodel <- LDAmodel_base
                    # 
                    # for (ntopics in 3:5) {
                    #   
                    #   # Fitting a LDA model with specified parameters
                    #   LDAmodel_prospect <- LDA(x = DTMatrix, 
                    #                            k = ntopics, 
                    #                            method = "Gibbs",
                    #                            control = list(
                    #                              alpha = 0.5,
                    #                              seed = 10005,
                    #                              iter = 500))
                    #   
                    #   # Saving fit values
                    #   loglik_prospect <- logLik(LDAmodel_prospect)
                    #   perxty_prospect <- perplexity(object = LDAmodel_prospect, 
                    #                                 newdata = DTMatrix)
                    #   
                    #   # Evaluating if there is a better model
                    #   if ((perxty_base-perxty_prospect) > 0) {
                    #     LDAmodel <- LDAmodel
                    #   } else {
                    #     LDAmodel <- LDAmodel_prospect
                    #   }
                    # }
                    
                    TOP_words <- as.data.frame(terms(LDAmodel_base, k = 10))
                    
                  })

names(tmodels.ls) <- candidates.ls %>% map_chr(1) %>% str_sub(2)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                5.  Sentiment trends data                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# sentiment.ls<-
# 
#   imap(list("Social Monitoring" = master_data.df,
#             "Speech Analysis"   = timelines.df), 
#             "Sentiment Trends" = master_data.df,
#        function(inputData, panel) {
#          
#          # Appplying the analysis to each candidate
#          lapply(candidates.ls %>% map_chr(1) %>% str_sub(2),
#                 function(candidate){
#                   
#                   # Applying filtering function
#                   filteredData <- data4filtering(panel = panel, 
#                                                  candidate = candidate,
#                                                  inputData = inputData)
                  

      
      # Applying the tokenizing function and renaming variables

      tokens.df<-master_data.df%>%separate(created_at, into = c("Date", "Hour"), sep = " ")%>%
       select(user_id,status_id,Date,screen_name,text )
      tokens.df <- data2tokens(data = tokens.df)
      
      #Candidate df
      names_candidate<-names(candidates.ls)
      candidate.df<-data.frame(matrix(unlist(candidates.ls), 
                                      nrow=length(candidates.ls), byrow=TRUE))%>%
        rename("twitter_candidate"="X1","nickname"="X2")%>%
        mutate(twitter_candidate1=tolower(twitter_candidate))%>%
        cbind(names_candidate)
      
      #Create a variable candidate per tweet. Keep only tweets with one candidate named
      
      candidates_query<-c(candidates_query1,candidates_query2)
      candidates_query<-candidates.ls %>%  map_chr(1)%>% str_sub(1)
      
      candidates_query<-tolower(candidates_query)
      tokens.df<-tokens.df%>%
        mutate(candidate=ifelse(words %in% candidates_query,words,NA),
               candidate_num=ifelse(is.na(candidate),0,1))       %>%
        arrange (tweet_id,candidate)%>%
        group_by(tweet_id)%>%
        mutate(candidate=first(candidate),
               candidate_num=sum(candidate_num,na.rm=T))%>%ungroup()%>%
        filter(candidate_num==1)
      
      
      
      #Sentiment using NRC dictionary for each sentiment
      
      nrc_sentiments<-get_sentiment_dictionary("nrc", language="spanish")%>%
        mutate(word = str_replace_all(word,
                                      c("á" = "a", "é" = "e", "í" = "i", 
                                        "ó" = "o", "ú|ü" = "u")))
      
  

      #Sentiment by candidate
      sentiment.nrc.df<-tokens.df%>%
        inner_join(nrc_sentiments, by = c("words"="word"))%>%
        group_by(candidate,Date)%>%
        
        count(sentiment)%>%
        group_by(candidate)%>%
        mutate(nrow=sum(n,na.rm=T))%>%ungroup()%>%mutate(Date=as.Date(Date))%>%
        left_join(candidate.df%>%select(twitter_candidate1,twitter_candidate,names_candidate), 
                  by=c("candidate"="twitter_candidate1"))%>%
        mutate(candidate=twitter_candidate)

      sentiment.nrc.ls<-list(sentiment.nrc.df)

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                6.  Twitter Widgets Data Input                                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

twitter_widgets_urls.ls <- 
  imap(list("Social Monitoring" = master_data.df,
            "Speech Analysis"   = timelines.df), 
       function(inputData, panel) {
         
         # Applying the analysis to each candidate
         url_top_tweets.ls <- 
           lapply(candidates.ls %>% map_chr(1) %>% str_sub(2),
                  function(candidate){
                    
                    # Applying filtering function
                    filteredData <- data4filtering(panel = panel, 
                                                   candidate = candidate,
                                                   inputData = inputData)
                    
                    # Identifying most popular tweets
                    statuses <- filteredData %>%
                      slice_max(favorite_count, n = 3, with_ties = F) %>%
                      select(status_id) %>%
                      pull(status_id)

                    # Getting URLs
                    statuses <- lookup_statuses(statuses) %>% pull(status_url)
                    
                    # In case we have a deleted status
                    nrows <- length(statuses)
                    n <- 3
                    while(nrows < 3){
                      n <- n+1
                      statuses <- filteredData %>%
                        slice_max(favorite_count, n = n, with_ties = F) %>%
                        select(status_id) %>%
                        pull(status_id)
                      statuses <- lookup_statuses(statuses) %>% pull(status_url)
                      nrows <- length(statuses)
                    }
                    status_url.df <- tibble(tweet = c("Top 1", "Top 2", "Top 3"),
                                            url = statuses)

                    # Renaming variables to identify the candidate
                    names(status_url.df) <- c("tweet",
                                              paste0(candidate, "_URLs"))

                    return(status_url.df)
                    
                  })
         
         # Joining all data frames within list
         purrr::reduce(url_top_tweets.ls, dplyr::left_join, by = "tweet")
         
       })


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                7.  Saving Data Input                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

write_rds(overview.ls, 
          paste0(dash_directory, "overview.rds"))
write_rds(freq_analysis.ls, 
          paste0(dash_directory, "freq_analysis.rds"))
write_rds(daily_activity.ls, 
          paste0(dash_directory, "daily_activity.rds"))
write_rds(user_info.df, 
          paste0(dash_directory, "user_info.rds"))
write_rds(tmodels.ls, 
          paste0(dash_directory, "tmodels.rds"))
write_rds(twitter_widgets_urls.ls, 
          paste0(dash_directory, "twitter_widgets_urls.rds"))
write_rds(sentiment.nrc.ls, 
          paste0(dash_directory, "sentiment_nrc.rds"))
