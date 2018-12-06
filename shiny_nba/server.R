#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.

# load libraries
library(shiny)
library(tidyverse)
library(plotly)
library(shiny)
library(tidyverse)
library(dplyr)
library(reshape2)
library(tidyquant)

# load data and source functions to make plot of basketball court
source("helpers.R")
# load("nba_shots.RData")

nba_shots  <- read_rds('celtics_data.rds')


#read in summary data
cdata <- read.csv('C:/Users/kariyya/Documents/Training&Admin/Udemy/shiny_nba/cdata.csv', 
                  header = TRUE, stringsAsFactors =  FALSE)
names(cdata)[31] <- 'plusminus'
cdata$plusminus <- as.numeric(cdata$plusminus)
cdata <- cdata %>% mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
# define plot of court
gg_court = make_court()

################################################################################
# Define server logic 
shinyServer(function(input, output) {
   
  # set range of seasons based on player choice
  output$season_choice <- renderUI({
    seasons = nba_shots %>% filter(player_name == input$player_choice) %>%
      distinct(season) %>% pull()

    selectizeInput("season_choice", label = h3("Select season"), choices = seasons,
                selected = seasons[1], multiple = FALSE)

  })
  # plotoutput = courtshots
  output$court_shots <- renderPlot({
  #   subset data by selected player and season(s)
    player_data = filter(nba_shots, player_name == input$player_choice, 
                         shot_type == input$shot_choice,
                        #shot_made_flag == input$shots_made,
                         season %in% input$season_choice)
  # 
  #  create plot
    gg_court + geom_point(data = player_data, alpha = 0.75, size = 2.5,
                          aes(loc_x, loc_y, color = shot_made_flag, shape = season)) +
      scale_color_manual("", values = c(made = "blue", missed = "red"))
  })
  # 
  # 
  # output$shot_distances <- renderPlotly({
  #   nba_shots %>%
  #     filter(if(input$shots_made != 'all')  (shot_made_flag == input$shots_made) else TRUE) %>%
  #     plot_ly(y = ~shot_distance, color = ~player_name, type = "box") %>%
  #     layout(showlegend = FALSE)
  # })
  
  #create table 
  output$table <- renderTable({
    # nba_shots %>% select(player_name, shot_type, shot_type, shot_zone_basic, shot_made_numeric, shot_value) %>%
      # filter(player_name == input$player_choice)
        #create Year variable
    cdata$Date <- as.Date(cdata$Date, format = "%d/%m/%Y")
    cdata <- cdata %>% mutate( Year = format(Date, "%Y"))
    cdata$Year <- as.factor(cdata$Year)
    
    
    #subset data
    df <- cdata %>% select(Name, G, Year, Opp, Venue, MP, FG, FGA, FG., X3P., X3P,FT., TRB, AST, STL, BLK, TOV, PTS, plusminus )
    
    #convert each chr to num
    df[,6:18] <- df %>% select(MP, FG, FGA, FG., X3P., X3P, FT., TRB, AST, STL, BLK, TOV, PTS, plusminus) %>% 
      mutate_all(funs(as.numeric))
    ### NAs to 0s
    df <- df %>% mutate_if(is.numeric, funs(replace(., is.na(.), 0)))
    
    ###PROBLEM WITH MINUTES PLAYED
    ##Summary table comparision 
    df  %>% select(Name, Year, PTS,  AST, TRB, FG., X3P., TOV, plusminus ) %>% 
      group_by(.dots=c("Name","Year"))  %>% summarise_all(funs(mean)) %>% filter(Name == input$player_choice)
    #%>% order_by(.dots=c("Name","Year"))
      })
  
  #create histplot
  # output$histplot <- renderPlot({
  #   hist1 <- df  %>% select(Name, Year, PTS,  AST, TRB, FG, X3P, TOV ) %>%
  #     group_by(.dots=c("Name","Year"))  %>% summarise_all(funs(mean))
  #   hist1 <- hist1 %>% select(Name, Year, PTS,  AST, TRB, FG, X3P, TOV) %>% filter(Name == input$player_choice)
  #   hist2 <- melt(hist1)
  #   names(hist2)[3] <- "var"
  #   ggplot(hist2, aes(x =var, y = value, fill = Year )) + geom_bar(stat = 'identity', width = .5, position = 'dodge')
  # }
  # )
  #create scatter plot
  output$plusminusPlot2017 <- renderPlot({
    first <- df %>%  filter(Name == input$player_choice, Year == 2017) %>% select(G, plusminus)
    ggplot(data = first, aes(x = G, y = plusminus)) + geom_point() + ylim(-15,25) + geom_hline(yintercept = 0) + 
      #geom_ma(ma_fun = SMA, n = 3, na.rm = TRUE)
      geom_line(aes(y=rollmean(plusminus, 3, na.pad=TRUE))) + theme_gray()
    })
  output$plusminusPlot2018 <- renderPlot({
    #2019
    second <- df %>%  filter(Name == input$player_choice, Year == 2018) %>% select(G, plusminus, Year)
    ggplot(data = second, aes(x = G, y = plusminus)) + geom_point() + ylim(-15,25) + geom_hline(yintercept = 0) +
    # geom_ma(ma_fun = SMA, n = 3, na.rm = TRUE)
      geom_line(aes(y=rollmean(plusminus, 3, na.pad=TRUE))) + theme_gray()

      })
  
  # output$image <-renderUI({
  #   tags$img(src = input$image_url)
  # })
  
})

