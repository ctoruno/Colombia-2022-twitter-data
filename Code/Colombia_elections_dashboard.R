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
              choiceNames = as.list(names(candidates)),
              choiceValues = candidates),
            dateRangeInput("date_range", label = h3("Date range"))),
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
  
  # Loading master data
  master_filepath <- list.files("./Data/Master",
                                     pattern = "csv$",
                                     full.names = T) %>% extract(which.max(file.mtime(.)))
  master_data.df <- reactiveFileReader(
    intervalMillis = 10800,
    session = NULL,
    filePath = master_filepath,
    readFunc = read_twitter_csv) %>%
    mutate(created_at = as.POSIXct(created_at))
  
  # output$wordcloud <- wordcloud2(filtered_data[1:200,], size = 0.9,
  #                                color = rep_len(c("DarkRed", "CornflowerBlue", "DarkOrange"),
  #                                                nrow(filtered_data[1:200,])),
  #                                ellipticity = 0.2, shuffle = F)
}




shiny::shinyApp(ui, server)
