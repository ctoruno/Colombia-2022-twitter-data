## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##                    David Granada Donato        (dagrado@gmail.com)
##
## Creation date:     December 5th, 2021
##
## This version:      December 5th, 2021
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
            "shiny", "shinydashboard", "shinythemes", "shinyWidgets",
            "wordcloud2", "ggplot2"),
       library, character.only = T)

# Notes: 


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                0.  Loading data                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Loading Twitter data
load("./Data/twitter_data4dash.RData")

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  Dashboard UI                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Dashboard header
header <- dashboardHeader(
  title = "Colombia 2022: A Twitter companion",
  dropdownMenu(type = "notifications",
               icon = icon("exclamation-triangle"),
               notificationItem(
                 text = paste0("Last successful Twitter extraction: ", batches.df$Date[1]),
                 icon("calendar-alt"))
               )
)

# Dashboard sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Overview", 
             tabName = "overview", icon = icon("dharmachakra")),
    menuItem("Wordclouds",
             tabName = "wordclouds", icon = icon("cloud")),
    menuItem("Mentions per candidate",
             tabName = "mentions", icon = icon("chart-line")),
    menuItem("Top hashtags",
             tabName = "hashtags", icon = icon("clipboard")),
    menuItem("Sentiment trends",
             tabName = "dashboard", icon = icon("comment-dots")),
    menuItem("Methodology",
             tabName = "methodology", icon = icon("book")),
    menuItem("About",
             tabName = "about", icon = icon("chess"))
  )
)

# Dashboard body
body <- dashboardBody(
  tabItems(

    # Wordclouds
    tabItem(
      tabName = "wordclouds",
      fluidRow(
        box(title = "Filters",
            width = 4,
            multiInput(
              inputId = "selected_candidates",
              label = "Candidates:",
              choices = NULL,
              choiceNames = as.list(names(candidates.ls)),
              choiceValues = candidates.ls %>%  map_chr(2)),
            dateRangeInput("date_range",
                           label = h3("Date range"),
                           start = "2021-11-17",
                           end = "2021-12-08")),
        box(wordcloud2Output("wordcloud"),
            width = 8)
      )
    )
  )
)

# Building-up all together
ui <- dashboardPage(header, sidebar, body,
                    skin = "green")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  Dashboard Server                                                                      ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set.seed(31478)

server <- function(input, output){
  
  # Generating wordcloud
  output$wordcloud <- renderWordcloud2({
    
    # Defining filtered data
    filtered_data <- master_data.df %>%
      filter(str_detect(.data$text, regex(paste(.env$input$selected_candidates, collapse = "|"))) &
               between(as.Date(.data$created_at), .env$input$date_range[1], .env$input$date_range[2]))
    
    # Tokenizing text and removing stop words
    twitter_tokenized.df <- filtered_data %>%
      mutate(text = str_replace_all(tolower(text), c("á" = "a", "é" = "e", "í" = "i",
                                                     "ó" = "o", "ú|ü" = "u"))) %>%
      select(1:5) %>%
      mutate(tweet_id = row_number()) %>%
      unnest_tokens(words, text, token = "tweets", strip_url = T) %>%
      anti_join(data.frame(words = stopwords("es")) %>%
                  mutate(words = str_replace_all(.data$words,
                                                 c("á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú|ü" = "u"))))
    
    # Removing custom stop words
    twitter_tokenized.df <- twitter_tokenized.df %>%      # we remove stopwords in the top-150
      anti_join(tribble(~words, "ahora", "hacer", "hace", "puede", "mismo", "tan", "señor", "ud", "siempre",
                        "menos","dice", "debe", "ver", "hoy", "sabe", "van", "quiere", "creo", "ustedes",
                        "decir", "pues", "cabal", "vamos", "nunca", "claro", "ahi", "jajaja", "jajajaja",
                        "entonces", "gran", "vez", "da", "toda", "d", "favor", "parte", "quieren", "cada",
                        "hizo", "hecho", "tener", "dijo", "aqui", "cree", "tal", "parece", "hacen",
                        "despues", "que", "usted", "solo", "ser", "asi", "va", "años", "habla", "tipo",
                        "misma", "cosas", "5", "necesita", "alguien", "todas", "aun", "sino", "cosa",
                        "x", "q"))
    # check <- twitter_tokenized.df %>%
    #   count(words) %>%
    #   arrange(desc(n))  # just to check remaining stopwords
    
    # Creating word counts
    wcount_raw.df <- twitter_tokenized.df %>% count(words) %>% arrange(desc(n))
    keywords <- paste(candidates_query1, candidates_query2, parties_query1, parties_query2, sep = " ") %>%
      str_replace_all(" OR ", "|")
    wcount_flt.df <- wcount_raw.df %>%    # Removing keywords used to extract tweets
      filter(!(str_detect(words, regex(keywords, ignore_case = T))))
    
    # Generating wordcloud
    wordcloud2(wcount_flt.df[1:200,], size = 0.9,
               color = rep_len(c("DarkRed", "CornflowerBlue", "DarkOrange"),
                               nrow(wcount_flt.df[1:200,])),
               ellipticity = 0.2, shuffle = F)
  })
}



shiny::shinyApp(ui, server)
